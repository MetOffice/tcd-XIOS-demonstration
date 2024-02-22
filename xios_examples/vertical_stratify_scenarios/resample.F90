!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Read 2D data on a domain and resample using the axis_input.nc file
!>
program resample
  use xios
  use mpi

  implicit none

  integer :: comm = -1
  integer :: rank = -1
  integer :: npar = 0

  call initialise()
  call simulate()
  call finalise()
contains

  subroutine initialise()

    type(xios_date) :: origin
    type(xios_date) :: start
    type(xios_duration) :: tstep
    integer :: mpi_error
    integer :: lenx
    integer :: lenrx
    integer :: leny
    integer :: lenry
    integer :: lenz
    integer :: lenrz
    double precision, dimension (:), allocatable :: latvals, lonvals, plvals, mlvals
    double precision, dimension (:,:), allocatable :: latb, lonb

    ! Arbitrary datetime setup, required for XIOS but unused
    origin = xios_date(2022, 2, 2, 12, 0, 0)
    start = xios_date(2022, 12, 13, 12, 0, 0)
    tstep = xios_hour

    ! Initialise MPI and XIOS
    call MPI_INIT(mpi_error)

    call xios_initialize('client', return_comm=comm)

    call MPI_Comm_rank(comm, rank, mpi_error)
    call MPI_Comm_size(comm, npar, mpi_error)

    ! use the axis_check context to obtain sizing information on all arrays
    ! for use in defining the main context interpretation
    call xios_context_initialize('axis_check', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call xios_close_context_definition()

    call xios_get_axis_attr('xm', n_glo=lenx)
    call xios_get_axis_attr('xp', n_glo=lenrx)
    call xios_get_axis_attr('ym', n_glo=leny)
    call xios_get_axis_attr('yp', n_glo=lenry)
    call xios_get_axis_attr('mlev', n_glo=lenz)
    call xios_get_axis_attr('plev', n_glo=lenrz)

    allocate ( mlvals(lenz) )
    allocate ( plvals(lenrz) )
    allocate ( lonvals(lenx) )
    allocate ( latvals(leny) )
    allocate ( lonb(2, lenx) )
    allocate ( latb(2, leny) )

    lonb = reshape((/-6.0, -3.0/), shape(lonb))
    latb = reshape((/50.0, 53.0/), shape(latb))


    call xios_get_axis_attr('xm', value=lonvals)
    call xios_get_axis_attr('ym', value=latvals)
    call xios_get_axis_attr('mlev', value=mlvals)
    call xios_get_axis_attr('plev', value=plvals)


    ! initialize the main context for interacting with the data.
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    !call xios_set_axis_attr('model_levels', n_glo=lenz)
    call xios_set_axis_attr('model_levels', n_glo=lenz, value=mlvals)
    call xios_set_axis_attr('pressure_levels1', n_glo=lenrz, value=plvals)

    call xios_set_domain_attr('latlon_domain', ni_glo=lenx, nj_glo=leny, ni=lenx, nj=leny, ibegin=0, jbegin=0)
    call xios_set_domain_attr('latlon_domain', lonvalue_1d=lonvals, latvalue_1d=latvals, bounds_lon_1d=lonb, bounds_lat_1d=latb)


    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise all XIOS contexts and MPI
    call xios_set_current_context('axis_check')
    call xios_context_finalize()
    call xios_set_current_context('main')
    call xios_context_finalize()
    call MPI_Comm_free(comm, mpi_error)
    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    type(xios_date) :: current
    integer :: ts
    integer :: lenx
    integer :: leny
    integer :: lenmlz
    integer :: lenplz

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:,:,:), allocatable :: inpdata
    double precision, dimension (:,:,:), allocatable :: intdata
    double precision, dimension (:,:,:), allocatable :: intonpdata

    call xios_get_domain_attr('latlon_domain', ni_glo=lenx, nj_glo=leny)
    call xios_get_axis_attr('model_levels', n_glo=lenmlz)
    call xios_get_axis_attr('pressure_levels1', n_glo=lenplz)

    allocate ( inpdata(lenmlz, leny, lenx) )
    allocate ( intdata(lenmlz, leny, lenx) )
    allocate ( intonpdata(lenplz, leny, lenx) )

    call xios_set_current_context('axis_check')


    ! Load data from the input file
    call xios_recv_field('pressure_in', inpdata)
    call xios_recv_field('temperature_in', intdata)
    call xios_recv_field('temponp_in', intonpdata)

    call xios_set_current_context('main')

    do ts=1, 1
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      ! Send the pressure data to the output file.
      call xios_send_field('pressure', inpdata)
      ! Send the pressure data to the output file.
      ! The interpolate_axis and field-ref in main.xml will
      ! also write the interpolated data into the output file.
      call xios_send_field('temperature', intdata)
      call xios_send_field('expected', intonpdata)
    enddo


    deallocate (inpdata)
    deallocate (intdata)
    deallocate (intonpdata)

  end subroutine simulate

end program resample
