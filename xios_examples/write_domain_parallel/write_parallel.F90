!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Set up a 2D unstructured domain of arbitrary data and output in parallel to
!> one NetCDF file at the defined frequency

program write_parallel
  use xios
  use mpi

  implicit none

  integer :: comm = -1
  integer :: ierr = 0
  integer :: size, rank
  integer :: n_steps = 10
  integer :: ts = 0

  double precision,allocatable :: levels(:)
  double precision,allocatable :: lon_glo(:)
  double precision,allocatable :: lat_glo(:)
  double precision,allocatable :: bounds_lon_glo(:,:)
  double precision,allocatable :: bounds_lat_glo(:,:)
  double precision,allocatable :: field_1_glo(:,:)
  double precision,allocatable :: lon_lo(:)
  double precision,allocatable :: lat_lo(:)
  double precision,allocatable :: bounds_lon_lo(:,:)
  double precision,allocatable :: bounds_lat_lo(:,:)
  double precision,allocatable :: field_1_lo(:,:)

  call initialise()
  call simulate()
  call finalise()
contains

  subroutine initialise()

    type(xios_date) :: origin
    type(xios_date) :: start
    type(xios_duration) :: tstep
    integer :: mpi_error
    character(len=*),parameter :: id="client"

    integer :: nlon = 100
    integer :: nlat = 100
    integer :: nlevs = 3
    integer :: ncell 
    integer :: ilat, ilon, ilev, ind
    integer :: ni, ibegin
    double precision :: lon1, lon2, lat1, lat2

    ! Initialise MPI and XIOS
    call MPI_INIT(ierr)
    call xios_initialize(id,return_comm=comm)


    !------------------------Set up Vertical levels-------------------!

    allocate(levels(nlevs)) ; levels=(/(ilev,ilev=1,nlevs)/)

    !------------------------Set up Global Horizontal domain-------------------!

    ! Regions around the poles are not included into the grid
    ! The whole grid is rectangular (nvertex=4)

    ncell = nlon * (nlat-1)
    allocate(lon_glo(ncell))
    allocate(lat_glo(ncell))
    allocate(bounds_lon_glo(4,ncell))
    allocate(bounds_lat_glo(4,ncell))


    allocate(field_1_glo(ncell,nlevs))

    ind = 0
    do ilat = 1, nlat-1
     do ilon = 1, nlon

       ind=ind+1

        lon1 = 360./dble(nlon) * dble(ilon-1)
        lon2 = lon1 + 360./DBLE(nlon)

        lat1 = (90. + 90./dble(nlat)) - 180./dble(nlat)*dble(ilat)
        lat2 = lat1 - 180./dble(nlat)

        lon_glo(ind) = (lon1+lon2)*0.5
        lat_glo(ind) = (lat1+lat2)*0.5 

        bounds_lon_glo(1,ind) = lon1
        bounds_lon_glo(2,ind) = lon2
        bounds_lon_glo(3,ind) = lon2
        bounds_lon_glo(4,ind) = lon1

        bounds_lat_glo(1,ind) = lat1
        bounds_lat_glo(2,ind) = lat1
        bounds_lat_glo(3,ind) = lat2      
        bounds_lat_glo(4,ind) = lat2     

        ! Set data field arrays for each level to the level number
 
        do ilev = 1, nlevs
          field_1_glo(ind,ilev) = dble(ilev)
        end do

     enddo
    enddo



    !------------------------Set up Local Partitioned Horizontal domain-------------------!


    call MPI_COMM_RANK(comm,rank,ierr)
    call MPI_COMM_SIZE(comm,size,ierr)

    if (mod(ncell, size) == 0) then
      ni = ncell/size
      ibegin = rank*ni
    else
      if (rank < MOD(ncell, size)) then
        ni = ncell/size + 1
        ibegin = rank*(ncell/size + 1)
      else
        ni = ncell/size
        if (rank == MOD(ncell, size)) then
          ibegin = rank*(ncell/size + 1)
        else
          ibegin = MOD(ncell,size)*(ncell/size + 1) + (rank-MOD(ncell,size))*ncell/size
        end if
      end if
    end if

    allocate(lon_lo(ni))
    allocate(lat_lo(ni))
    allocate(bounds_lon_lo(4,ni))
    allocate(bounds_lat_lo(4,ni))
    allocate(field_1_lo(ni,nlevs)) 

    lon_lo = lon_glo(1+ibegin:ibegin+ni)
    lat_lo = lat_glo(1+ibegin:ibegin+ni)
    bounds_lon_lo(:,:) = bounds_lon_glo(:,1+ibegin:ibegin+ni)
    bounds_lat_lo(:,:) = bounds_lat_glo(:,1+ibegin:ibegin+ni)

    field_1_lo(:,:) = field_1_glo(1+ibegin:ibegin+ni,:)


    ! Arbitrary datetime setup
    origin = xios_date(2022, 2, 2, 12, 0, 0)
    start = xios_date(2022, 12, 13, 12, 0, 0)
    tstep = xios_hour

    call xios_context_initialize('main', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)


    call xios_set_domain_attr("domain_2d", ni_glo=ncell, ibegin=ibegin, ni=ni, type='unstructured')
    call xios_set_domain_attr("domain_2d", lonvalue_1d=lon_lo, latvalue_1d=lat_lo)
    call xios_set_domain_attr("domain_2d", bounds_lon_1d=bounds_lon_lo, bounds_lat_1d=bounds_lat_lo)


    call xios_set_axis_attr("vert_axis", n_glo=nlevs, value=levels)

    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise XIOS and MPI
    call xios_context_finalize()

    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    do ts=1,n_steps

      call xios_update_calendar(ts)
      ! Output field 1 every timestep
      call xios_send_field("global_field_1",field_1_lo)
      ! increment field value
      field_1_lo = field_1_lo + 0.2

    enddo

    ! Clean up
    deallocate(levels)
    deallocate(lon_glo, lat_glo)
    deallocate(bounds_lon_glo, bounds_lat_glo)
    deallocate(field_1_glo)
    deallocate(lon_lo, lat_lo)
    deallocate(bounds_lon_lo, bounds_lat_lo)
    deallocate(field_1_lo)

  end subroutine simulate

end program write_parallel
