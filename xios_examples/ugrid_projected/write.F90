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
  use iso_fortran_env, only : output_unit
  
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
    integer :: len_node, ni_node, ibegin_node
    integer :: len_face, ni_face, ibegin_face
    integer :: len_edge, ni_edge, ibegin_edge
    integer :: fcount, ecount, fvcount, evcount
    integer :: fvertex_len, evertex_len, fvertex_n, evertex_n
    double precision, dimension (:), allocatable :: node_y_vals, node_x_vals, &
                                                    face_y_vals, face_x_vals, &
                                                    edge_y_vals, edge_x_vals
    double precision, dimension (:,:), allocatable :: face_y_bounds, face_x_bounds, &
                                                      edge_y_bounds, edge_x_bounds, &
                                                      mesh_face_nodes_dbl, mesh_edge_nodes_dbl, &
                                                      inbfx, inbfy
    integer, dimension (:,:), allocatable :: mesh_face_nodes, mesh_edge_nodes
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

    print *, 'rank:', rank, '  ', "initialising axis_check"
    flush(output_unit)
    call xios_context_initialize('axis_check', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call xios_close_context_definition()
    print *, 'rank:', rank, '  ', "axis_check initialised"
    flush(output_unit)

    ! fetch sizes of axes from the input file for allocate
    call xios_get_domain_attr('cndata::', ni_glo=len_node, ni=ni_node, ibegin=ibegin_node)
    call xios_get_domain_attr('cfdata::', ni_glo=len_face, ni=ni_face, ibegin=ibegin_face)
    call xios_get_domain_attr('cedata::', ni_glo=len_edge, ni=ni_edge, ibegin=ibegin_edge)
    call xios_get_axis_attr('mesh_face_nodes::[0]', n_glo=fvertex_len, n=fvertex_n)
    print *, 'rank:', rank, '  ', "fvertex length: ", fvertex_len
    call xios_get_axis_attr('mesh_edge_nodes::[0]', n_glo=evertex_len, n=evertex_n)
    print *, 'rank:', rank, '  ', "evertex length: ", evertex_len

    allocate ( node_x_vals(ni_node) )
    allocate ( node_y_vals(ni_node) )
    allocate ( face_x_vals(ni_face) )
    allocate ( face_y_vals(ni_face) )
    allocate ( edge_x_vals(ni_edge) )
    allocate ( edge_y_vals(ni_edge) )
    allocate ( mesh_face_nodes(fvertex_len, ni_face) )
    allocate ( mesh_face_nodes_dbl(fvertex_len, ni_face) )
    allocate ( mesh_edge_nodes(evertex_len, ni_edge) )
    allocate ( mesh_edge_nodes_dbl(evertex_len, ni_edge) )
    allocate ( face_y_bounds(fvertex_len, ni_face) )
    allocate ( face_x_bounds(fvertex_len, ni_face) )
    allocate ( edge_y_bounds(evertex_len, ni_edge) )
    allocate ( edge_x_bounds(evertex_len, ni_edge) )
    print *, 'rank:', rank, '  ', 'ni_node= ', ni_node
    print *, 'rank:', rank, '  ', 'ni_face= ', ni_face
    print *, 'rank:', rank, '  ', 'ni_edge= ', ni_edge
    print *, 'rank:', rank, '  ', 'fvertices: ', fvertex_len
    flush(output_unit)

    allocate ( inbfx(fvertex_len, ni_face) )
    allocate ( inbfy(fvertex_len, ni_face) )
    ! print *, 'get bounds'
    ! flush(output_unit)
    ! call xios_get_domain_attr('cfdata::', bounds_lon_1d=inbfx, bounds_lat_1d=inbfy)
    ! print *, 'inbfx=', inbfx
    ! print *, 'inbfy=', inbfy
    
    print *, 'get coordinate values'
    flush(output_unit)
    ! fetch coordinate value arrays from the input file
    call xios_get_domain_attr('cndata::', lonvalue_1d=node_x_vals, latvalue_1d=node_y_vals)
    call xios_get_domain_attr('cfdata::', lonvalue_1d=face_x_vals, latvalue_1d=face_y_vals)
    !call xios_get_domain_attr('cedata::', lonvalue_1d=edge_x_vals, latvalue_1d=edge_y_vals)
    print *, 'rank:', rank, '  ', 'node xvals= ', node_x_vals
    print *, 'rank:', rank, '  ', 'node yvals= ', node_y_vals
    print *, 'rank:', rank, '  ', 'face xvals= ', face_x_vals
    print *, 'rank:', rank, '  ', 'face yvals= ', face_y_vals
    !print *, 'rank:', rank, '  ', 'edge xvals= ', edge_x_vals, 'edge yvals= ', edge_y_vals
    flush(output_unit)

    call xios_recv_field("mesh_face_nodes", mesh_face_nodes_dbl)
    mesh_face_nodes = int(mesh_face_nodes_dbl)

    print *, 'rank:', rank, '  ', 'mesh_face_nodes= ', mesh_face_nodes
    flush(output_unit)

    do fvcount=1, fvertex_len
      do fcount=1, ni_face
        ! print *, 'rank:', rank, '  ', mesh_face_nodes(fvcount, fcount)
        print *, 'rank:', rank, '  ', 'fvcount=', fvcount, 'fcount=', fcount
        face_x_bounds(fvcount, fcount) = node_x_vals(mesh_face_nodes(fvcount, fcount))
        face_y_bounds(fvcount, fcount) = node_y_vals(mesh_face_nodes(fvcount, fcount))
      end do
      
    end do
    print *, 'rank:', rank, '  ', "face_x_bounds: ", shape(face_x_bounds)
    print *, 'rank:', rank, '  ', face_x_bounds
    print *, 'rank:', rank, '  ', "face_y_bounds: ", shape(face_y_bounds)
    print *, 'rank:', rank, '  ', face_y_bounds

    call xios_recv_field("mesh_edge_nodes", mesh_edge_nodes_dbl)
    mesh_edge_nodes = int(mesh_edge_nodes_dbl)
    print *, 'rank:', rank, '  ', "mesh_edge_nodes", mesh_edge_nodes

    do ecount=1, ni_edge
      do evcount=1, evertex_len
        edge_x_bounds(evcount, ecount) = node_x_vals(mesh_edge_nodes(evcount, ecount))
        edge_y_bounds(evcount, ecount) = node_y_vals(mesh_edge_nodes(evcount, ecount))
      end do
      edge_x_vals(ecount) = edge_x_bounds(1, ecount) - 0.5 * &
                            (edge_x_bounds(1, ecount) - edge_x_bounds(2, ecount))
      edge_y_vals(ecount) = edge_y_bounds(1, ecount) - 0.5 * &
                            (edge_y_bounds(1, ecount) - edge_y_bounds(2, ecount))
    end do
    print *, 'rank:', rank, '  ', "edge_x: ", shape(edge_x_vals), edge_x_vals
    print *, 'rank:', rank, '  ', "edge_x_bounds: ", shape(edge_x_bounds)
    print *, 'rank:', rank, '  ', edge_x_bounds(1, :)
    print *, 'rank:', rank, '  ', edge_x_bounds(2, :)
    print *, 'rank:', rank, '  ', "edge_y: ", shape(edge_y_vals), edge_y_vals
    print *, 'rank:', rank, '  ', "edge_y_bounds: ", shape(edge_y_bounds)
    print *, 'rank:', rank, '  ', edge_y_bounds(1, :)
    print *, 'rank:', rank, '  ', edge_y_bounds(2, :)

    
    ! finalise axis_check context, it no longer in use
    ! call xios_set_current_context('axis_check')
    call xios_context_finalize()
    print *, 'rank:', rank, '  ', "axis check finalised successfully"
    flush(output_unit)

    ! initialize the main context for interacting with the data.
    print *, 'rank:', rank, '  ', "initialising main context"
    flush(output_unit)
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    ! define the horizontal domain and vertical axis using the input file values

    print *, 'rank:', rank, '  ', "now define domain configuration from input mesh"
    flush(output_unit)
    print *, 'rank:', rank, '  ', 'ibegin_node=', ibegin_node, '  ;  ni_node=', ni_node
    print *, 'rank:', rank, '  ', 'nodes: x, y'
    print *, 'rank:', rank, '  ', node_x_vals(ibegin_node+1:(ibegin_node+ni_node))
    print *, 'rank:', rank, '  ', node_y_vals(ibegin_node+1:(ibegin_node+ni_node))
    print *, 'rank:', rank, '  ', 'faces'
    print *, 'rank:', rank, '  ', face_x_vals(ibegin_face+1:(ibegin_face+ni_face))
    print *, 'rank:', rank, '  ', face_y_vals(ibegin_face+1:(ibegin_face+ni_face))
    print *, 'rank:', rank, '  ', 'face bounds'
    print *, 'rank:', rank, '  ', face_x_bounds(:,ibegin_face+1:(ibegin_face+ni_face))
    print *, 'rank:', rank, '  ', face_y_bounds(:,ibegin_face+1:(ibegin_face+ni_face))
    print *, 'rank:', rank, '  ', 'edges'
    print *, 'rank:', rank, '  ', edge_x_vals(ibegin_edge+1:(ibegin_edge+ni_edge))
    print *, 'rank:', rank, '  ', edge_y_vals(ibegin_edge+1:(ibegin_edge+ni_edge))
    print *, 'rank:', rank, '  ', 'edge bounds'
    print *, 'rank:', rank, '  ', edge_x_bounds(:,ibegin_edge+1:(ibegin_edge+ni_edge))
    print *, 'rank:', rank, '  ', edge_y_bounds(:,ibegin_edge+1:(ibegin_edge+ni_edge))
    call xios_set_domain_attr("node_domain", ni_glo=len_node, ni=ni_node, ibegin=ibegin_node, &
                              nj_glo=len_node, nj=ni_node, jbegin=ibegin_node, &
                              latvalue_1d=node_y_vals(ibegin_node+1:(ibegin_node+ni_node)), &
                              lonvalue_1d=node_x_vals(ibegin_node+1:(ibegin_node+ni_node)))
    call xios_set_domain_attr("face_domain", ni_glo=len_face, ni=ni_face, ibegin=ibegin_face, &
                              nj_glo=len_face, nj=ni_face, jbegin=ibegin_face, &
                              latvalue_1d=face_y_vals(ibegin_face+1:(ibegin_face+ni_face)), &
                              lonvalue_1d=face_x_vals(ibegin_face+1:(ibegin_face+ni_face)), &
                              bounds_lat_1d=face_y_bounds(:,ibegin_face+1:(ibegin_face+ni_face)), &
                              bounds_lon_1d=face_x_bounds(:,ibegin_face+1:(ibegin_face+ni_face)))
    call xios_set_domain_attr("edge_domain", ni_glo=len_edge, ni=ni_edge, ibegin=ibegin_edge, &
                              nj_glo=len_edge, nj=ni_edge, jbegin=ibegin_edge, &
                              latvalue_1d=edge_y_vals(ibegin_edge+1:(ibegin_edge+ni_edge)), &
                              lonvalue_1d=edge_x_vals(ibegin_edge+1:(ibegin_edge+ni_edge)), &
                              bounds_lat_1d=edge_y_bounds(:,ibegin_edge+1:(ibegin_edge+ni_edge)), &
                              bounds_lon_1d=edge_x_bounds(:,ibegin_edge+1:(ibegin_edge+ni_edge)))

    print *, 'rank:', rank, '  ', "ready to close main context definition"
    flush(output_unit)


    call xios_close_context_definition()
    print *, 'rank:', rank, '  ', "main context defined successfully"
    flush(output_unit)
    call xios_get_domain_attr('ndata::', lonvalue_1d=node_x_vals, latvalue_1d=node_y_vals)
    print *, 'rank:', rank, '  ', "output node x = ", node_x_vals
    print *, 'rank:', rank, '  ', "output node y = ", node_y_vals
    flush(output_unit)
    call xios_get_domain_attr('fdata::', lonvalue_1d=face_x_vals, latvalue_1d=face_y_vals)
    print *, 'rank:', rank, '  ', "output face x = ", face_x_vals
    print *, 'rank:', rank, '  ', "output face y = ", face_y_vals
    flush(output_unit)

    deallocate (node_x_vals)
    deallocate (node_y_vals)
    deallocate (face_x_vals)
    deallocate (face_y_vals)
    deallocate (edge_x_vals)
    deallocate (edge_y_vals)
    deallocate (face_y_bounds)
    deallocate (face_x_bounds)
    deallocate (edge_y_bounds)
    deallocate (edge_x_bounds)
    deallocate (mesh_face_nodes_dbl)
    deallocate (mesh_face_nodes)
    print *, 'rank:', rank, '  ', "deallocations"
  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise all XIOS contexts and MPI
    print *, 'rank:', rank, '  ', "finalising"
    flush(output_unit)
    call xios_set_current_context('main')
    call xios_context_finalize()
    print *, 'rank:', rank, '  ', "finalised main"
    flush(output_unit)
    call xios_finalize()
    print *, 'rank:', rank, '  ', "finalised xios"
    flush(output_unit)
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
    double precision, dimension (:), allocatable :: node_data, face_data, edge_data

    print *, 'rank:', rank, '  ', "simulation"
    ! obtain sizing of the grid for the array allocation
    call xios_get_domain_attr('ndata::', ni=len_node)
    call xios_get_domain_attr('fdata::', ni=len_face)
    call xios_get_domain_attr('edata::', ni=len_edge)

    allocate ( node_data(len_node) )
    allocate ( face_data(len_face) )
    allocate ( edge_data(len_edge) )
    print *, 'rank:', rank, '  ', 'len_node= ', len_node
    print *, 'rank:', rank, '  ', 'len_face= ', len_face
    print *, 'rank:', rank, '  ', 'len_edge= ', len_edge
    flush(output_unit)

    do ts=1, 2
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      do i=1, len_node
        node_data(i) = ts
      end do
      call xios_send_field('ndata', node_data)
      
      do i=1, len_face
        face_data(i) = 10 * ts
      end do
      call xios_send_field('fdata', face_data)
      
      do i=1, len_edge
        edge_data(i) = 100 * ts
      end do
      call xios_send_field('edata', edge_data)
      
    enddo

    deallocate (node_data)
    deallocate (face_data)
    deallocate (edge_data)
    print *, 'rank:', rank, '  ', "fields sent, exiting simulation"
    flush(output_unit)

  end subroutine simulate

end program write
