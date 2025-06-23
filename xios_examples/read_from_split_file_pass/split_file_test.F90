!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> Split file functionality test to illustrate reading from split file:
!>
!> - Run a sim with (sim_period - sim_start_offset) time steps, one time step = 1 hour.
!>   Here sim_period = 24 and sim_start_offset = 12, the latter indicating the number of time
!>   steps since original sim start date (2022-12-13-13-0-0) to the first time instant in
!    the relevant input split file.
!> - The adjusted simulation start date/time here is: 2022-12-14-01-0-0 which is sim_start_offset = 12
!>   time steps since the original start date.
!> - The updated file splitting intervals are defined in the XML interface. These would need to
!>   different from the original sim settings - i.e. by removing the first sim_start_offset = 12
!>   time steps from the split duration list.
!> - The input data is read into the temperature field from the relevant input split file.
!> - The temperature field is decreased by 1 K each hour up until midnight. And then for the
!>   final 12 hours the temperature field is increased by 1 degree each hour. Note, no increment
!>   is applied at the first iteration of the time step loop here.
!
program split_file_test

  use xios
  use mpi
  use ifile_attr

  implicit none

  integer :: comm = -1
  integer :: wrank = -1
  integer :: wnranks = 0
  integer :: rank = -1
  integer :: nranks = 0
  integer :: ierr

  ! Initialise MPI
  call MPI_INIT(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, wrank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, wnranks, ierr)

  if (wrank == 0) then

    call initialise_xios()
    call simulate()
    call finalise_xios()

  else

    call xios_init_server()

  end if

  call MPI_Finalize(ierr)

contains

  subroutine initialise_xios()

    type(xios_date) :: origin
    type(xios_date) :: start
    type(xios_duration) :: tstep, durations(7)
    type(xios_file) :: file_hdl
    
    double precision, dimension (1) :: lons = [-4.5]
    double precision, dimension (1) :: lats = [51.5]
    double precision, dimension (2,1) :: blons
    double precision, dimension (2,1) :: blats

    blons = reshape((/-6.0, -3.0/), shape(blons))
    blats = reshape((/50.0, 53.0/), shape(blats))

    ! Arbitrary datetime setup, required for XIOS but unused
    ! in this example
    origin = xios_date(2022, 12, 13, 01, 0, 0)
    start = xios_date(2022, 12, 14, 01, 0, 0)
    tstep = xios_hour

    ! Initialise MPI and XIOS
    call xios_initialize('client', return_comm=comm)
    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, nranks, ierr)

    ! Initialise context definition
    call xios_context_initialize('main', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call xios_set_domain_attr('latlon1_domain', lonvalue_1d=lons, latvalue_1d=lats, bounds_lon_1d=blons, bounds_lat_1d=blats)

    ! Close context definition
    call xios_close_context_definition()

  end subroutine initialise_xios

  subroutine finalise_xios()

    integer :: ierr

    call xios_context_finalize()
    call xios_finalize()

  end subroutine finalise_xios

  subroutine simulate()

    type(xios_date) :: current
    integer :: ts, i
    integer :: nlevs = 39

    integer, parameter :: sim_period = 24
    integer, parameter :: sim_start_offset = 12
    
    double precision, dimension (:), allocatable :: field_data, inc_field_data

    allocate(field_data(nlevs))
    allocate(inc_field_data(nlevs))

    ! Read in data from input split file
    call xios_recv_field("read_temperature", field_data)

    do ts = 1, sim_period - sim_start_offset

      call xios_update_calendar(ts)
      call xios_get_current_date(current)

      if (mod(ts + sim_start_offset, 24) <= 12 .and. mod(ts + sim_start_offset, 24) >= 1) then
        inc_field_data = -1.0
      else
        inc_field_data = 1.0
      end if

      if (ts /= 1) then
        field_data = field_data + inc_field_data
      end if

      call xios_send_field('temperature', field_data)

    end do

    deallocate(field_data)
    deallocate(inc_field_data)

  end subroutine simulate

end program split_file_test
