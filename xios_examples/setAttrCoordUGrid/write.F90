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
    integer :: len_node
    integer :: len_face
    integer :: len_edge
    double precision, dimension (:), allocatable :: node_y_vals, node_x_vals, &
                                                    face_y_vals, face_x_vals 
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
    call xios_get_domain_attr('node_domain', ni_glo=len_node)
    call xios_get_domain_attr('face_domain', ni_glo=len_face)
    call xios_get_domain_attr('edge_domain', ni_glo=len_edge)


    allocate ( node_x_vals(len_node) )
    allocate ( node_y_vals(len_node) )
    allocate ( face_x_vals(len_face) )
    allocate ( face_y_vals(len_face) )
    print *, 'len_node= ', len_node

    ! fetch coordinate value arrays from the input file
    ! call xios_get_domain_attr('node_domain', lonvalue_1d=node_x_vals, latvalue_1d=node_y_vals)
    ! call xios_get_domain_attr('face_domain', lonvalue_1d=face_x_vals, latvalue_1d=face_y_vals)
    ! print *, 'node xvals= ', node_x_vals, '\nnode yvals= ', node_y_vals

    ! finalise axis_check context, no longer in use
    call xios_set_current_context('axis_check')
    call xios_context_finalize()
    print *, "axis check finalised successfully"

    ! initialize the main context for interacting with the data.
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    ! define the horizontal domain and vertical axis using the input file
    
    call xios_set_domain_attr("node_domain", ni_glo=len_node, nj_glo=len_node)!, latvalue_1d=node_y_vals, lonvalue_1d=node_x_vals)
    call xios_set_domain_attr("face_domain", ni_glo=len_face, nj_glo=len_face)!, latvalue_1d=face_y_vals, lonvalue_1d=face_x_vals)
    call xios_set_domain_attr("edge_domain", ni_glo=len_edge, nj_glo=len_edge)

    call xios_set_file_attr("data_output", convention_str="CF-1.6, UGRID")
    call xios_set_file_attr("data_output", description="LFRic file format v0.2.0")
    ! if (.true.) then
    !   call xios_set_axis_attr("x", standard_name="projection_x_coordinate", &
    !                           unit="m", long_name="x coordinate of projection")
    !   call xios_set_axis_attr("y", standard_name="projection_y_coordinate", &
    !                           unit="m", long_name="y coordinate of projection")
    ! end if
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
    integer :: i
    integer :: len_node
    integer :: len_edge
    integer :: len_face
    double precision :: frtv

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:), allocatable :: somedata

    ! obtain sizing of the grid for the array allocation

    call xios_get_domain_attr('node_domain', ni_glo=len_node)

    allocate ( somedata(len_node) )

    do ts=1, 2
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      do i=1, len_node
        somedata(i) = ts
      end do
      
      ! send frt on ts0 only
      if (ts == 1) then
        call xios_get_start_date(start)
        frtv = dble(xios_date_convert_to_seconds(start))
        call xios_send_field("frt", frtv)
      end if
      
      ! Send (copy) the original data to the output file.
      call xios_send_field('ndata', somedata)
    enddo

    deallocate (somedata)

  end subroutine simulate

end program write
