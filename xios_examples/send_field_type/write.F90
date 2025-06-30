!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> write data of different types
!>
program write
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

    ! Arbitrary datetime setup, required for XIOS
    origin = xios_date(2022, 2, 2, 12, 0, 0)
    start = xios_date(2022, 12, 13, 12, 0, 0)
    tstep = xios_hour

    ! Initialise MPI and XIOS
    call MPI_INIT(mpi_error)

    call xios_initialize('client', return_comm=comm)

    call MPI_Comm_rank(comm, rank, mpi_error)
    call MPI_Comm_size(comm, npar, mpi_error)

    ! ! initialize the main context for interacting with the data.
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)


    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    call xios_context_finalize()
    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    type(xios_date) :: current
    integer :: ts, i, j, k

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:,:), allocatable :: indbldata
    real, dimension (:,:), allocatable :: inrealdata

    allocate ( indbldata(5, 4) )
    allocate ( inrealdata(5, 4) )

    jloop: do j = 1, 5
      iloop: do i = 1, 4
        indbldata(j, i) = dble(i*j)
        inrealdata(j, i) = real(i*j)
      end do iloop
    end do jloop
    do ts=1, 2
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      ! Send (copy) the original data to the output file.
      call xios_send_field('floatdata', inrealdata)
      call xios_send_field('doubledata', indbldata)
    end do

  end subroutine simulate

end program write
