!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Read 3D data on a domain + axis grid
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
    integer :: ni_glo
    integer :: nj_glo
    integer :: ni
    integer :: nj
    integer :: ibegin
    integer :: jbegin
    integer :: nk_glo
    integer :: nk
    integer :: kbegin
    integer :: nl_glo
    integer :: nl
    integer :: lbegin

    ! Datetime setup, required for XIOS & matched to input
    origin = xios_date(2024, 11, 15, 0, 0, 0)
    start = xios_date(2024, 11, 15, 0, 0, 0)
    tstep = xios_hour * 6

    ! Initialise MPI and XIOS
    call MPI_INIT(mpi_error)

    call xios_initialize('client', return_comm=comm)

    call MPI_Comm_rank(comm, rank, mpi_error)
    call MPI_Comm_size(comm, npar, mpi_error)

    call xios_context_initialize('domain_check', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)
    print *, "ready to close context domain_check"
    call xios_close_context_definition()
    print *, "context domain_check closed (read)"

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    print *, "Finalise all XIOS contexts and MPI"
    call xios_set_current_context('domain_check')
    call xios_context_finalize()
    print *, "domain_check context finalised"
    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    type(xios_date) :: current
    integer :: lenx = 127
    integer :: leny = 95
    integer :: lenz = 4
    integer :: lent = 2

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:,:,:,:), allocatable :: inshdata

    allocate ( inshdata(lent, lenz, leny, lenx) )
    print *, "ready to load data from specific humidity array variable"
    ! Load data from the input file
    print *, inshdata(1,1,1,1:5)
    print *, "data is unitialised, values are indeterminate"
    call xios_recv_field('specific_humidity', inshdata)
    print *, "data loaded from specific humidity array variable"
    print *, inshdata(1,1,1,1:5)
    print *, "-1 is the file _FillValue"

    deallocate (inshdata)

  end subroutine simulate

end program resample
