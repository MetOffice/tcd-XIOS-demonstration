!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Programme to just write data with coordinate system definition metadata.
!>
program write
  use xios
  use mpi
  USE, INTRINSIC :: ISO_C_BINDING
  
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
    integer :: lenz
    double precision, dimension (:), allocatable :: y_vals, x_vals, altvals
    character(len=64) :: t_origin

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

    ! fetch sizes of axes from the input file for allocate
    call xios_get_axis_attr('lon', n_glo=lenx)
    call xios_get_axis_attr('lat', n_glo=leny)
    call xios_get_axis_attr('alt', n_glo=lenz)

    allocate ( x_vals(lenx) )
    allocate ( y_vals(leny) )
    allocate ( altvals(lenz) )

    ! fetch coordinate value arrays from the input file
    call xios_get_axis_attr('lon', value=x_vals)
    call xios_get_axis_attr('lat', value=y_vals)
    call xios_get_axis_attr('alt', value=altvals)

    ! finalise axis_check context, no longer in use
    call xios_set_current_context('axis_check')
    call xios_context_finalize()

    ! initialize the main context for interacting with the data.
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    ! define the horizontal domain and vertical axis using the input file
    ! call xios_set_domain_attr("original_domain", ni_glo=lenx, ni=lenx, nj_glo=leny, nj=leny, ibegin=0, jbegin=0)
    ! call xios_set_domain_attr("original_domain", lonvalue_1d=lonvals, latvalue_1d=latvals)
    
    call xios_set_axis_attr("x", n_glo=lenx, n=lenx, begin=0)
    call xios_set_axis_attr("x", value=x_vals)
    call xios_set_axis_attr("y", n_glo=leny, n=leny, begin=0)
    call xios_set_axis_attr("y", value=y_vals)
    call xios_set_axis_attr("alt", n_glo=lenz, n=lenz, begin=0)
    call xios_set_axis_attr("alt", value=altvals)
    call xios_set_file_attr("data_output", convention_str="CF-1.6, UGRID")
    call xios_set_file_attr("data_output", description="LFRic file format v0.2.0")
    if (.true.) then
      call xios_set_axis_attr("x", standard_name="projection_x_coordinate", &
                              unit="m", long_name="x coordinate of projection")
      call xios_set_axis_attr("y", standard_name="projection_y_coordinate", &
                              unit="m", long_name="y coordinate of projection")
    end if
    call xios_date_convert_to_string(origin, t_origin)
    call xios_set_field_attr("frt", unit="seconds since "//t_origin)

    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise all XIOS contexts and MPI
    call xios_set_current_context('main')
    call xios_context_finalize()

    call xios_finalize()
    call MPI_Finalize(mpi_error)

  end subroutine finalise

  subroutine simulate()

    type(xios_date) :: start
    type(xios_date) :: current
    integer :: ts
    integer :: lenx
    integer :: leny
    integer :: lenz
    double precision :: frtv

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:,:,:), allocatable :: inodata

    ! obtain sizing of the grid for the array allocation

    call xios_get_axis_attr('x', n_glo=lenx)
    call xios_get_axis_attr('y', n_glo=leny)
    call xios_get_axis_attr('alt', n_glo=lenz)

    allocate ( inodata(lenz, leny, lenx) )

    ! Load data from the input file
    call xios_recv_field('odatain', inodata)

    do ts=1, 2
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      ! hardcoded 9
      frtv=9
      ! send frt on ts0 only
      if (ts == 1) then
        call xios_get_start_date(start)
        frtv = dble(xios_date_convert_to_seconds(start))
        call xios_send_field("frt", frtv)
      end if
      
      ! Send (copy) the original data to the output file.
      call xios_send_field('odata', inodata)
    enddo

    deallocate (inodata)

  end subroutine simulate

end program write
