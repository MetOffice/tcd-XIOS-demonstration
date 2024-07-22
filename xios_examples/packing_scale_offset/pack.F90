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
    integer :: leny
    double precision, dimension (:), allocatable :: latvals, lonvals
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

    call xios_get_axis_attr('x', n_glo=lenx)
    call xios_get_axis_attr('y', n_glo=leny)

    allocate ( lonvals(lenx) )
    allocate ( latvals(leny) )
    allocate ( lonb(2, lenx) )
    allocate ( latb(2, leny) )

    call xios_get_axis_attr('x', value=lonvals)
    call xios_get_axis_attr('y', value=latvals)

    ! ! initialize the main context for interacting with the data.
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call xios_set_domain_attr("original_domain", ni_glo=lenx, nj_glo=leny, ni=lenx, nj=leny, ibegin=0, jbegin=0)
    call xios_set_domain_attr("original_domain", lonvalue_1d=lonvals, latvalue_1d=latvals)

    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise all XIOS contexts and MPI
    call xios_set_current_context('axis_check')
    call xios_context_finalize()
    call xios_set_current_context('main')
    call xios_context_finalize()
    ! call MPI_Comm_free(comm, mpi_error)
    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    type(xios_date) :: current
    integer :: ts
    integer :: lenx
    integer :: leny
    integer :: i, j

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:,:), allocatable :: inodata

    print *, "simulate"
    call xios_get_domain_attr('original_domain', ni_glo=lenx)
    call xios_get_domain_attr('original_domain', nj_glo=leny)

    allocate ( inodata(leny, lenx) )

    ! Load data from the input file
    call xios_recv_field('odatain', inodata)

    do ts=1, 1
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      ! Send (copy) the original data to the output file.
      call xios_send_field('odata', inodata)
      ! Send the original data to the output file pack target.
      ! The `pdata` field is defined as integer with scale_factor
      ! and `add_offset` defined, so XIOS will pack this data
      ! on write.
      call xios_send_field('pdata', inodata)
    enddo

    deallocate (inodata)

  end subroutine simulate

end program resample
