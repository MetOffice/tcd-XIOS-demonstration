!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Saves to disk a pressure field read from an input file.
!> This tests that XIOS correctly reads the axis values from from the input
!> file.
!>
!> The client sets the x and y axis values to [2, 4, 6, 8, ... ], however the
!> input file contains x and y axis values of [1, 2, 3, 4, ... ]. XIOS should
!> read these from the input file and correctly overwrite the existing values.
!>
!> While XIOS will check that the dimensions of the axis defined by the user
!> match those defined in the input file, the values in the input file take
!> precedence over any user defined values.

module partition_mod
  use mpi
  implicit none

  type :: partition_info 
    integer :: ni_global, nj_global
    integer :: ni_local, nj_local
    integer :: ibegin_global, jbegin_global
  end type

  contains 

  subroutine create_partition(ni_global, nj_global, comm, info)
    type(partition_info), intent(out) :: info 
    integer, intent(in) :: ni_global, nj_global
    integer, intent(in) :: comm

    integer :: n_ranks
    integer :: rank
    integer :: ierr

    call MPI_Comm_rank(comm, rank, ierr)
    call MPI_Comm_size(comm, n_ranks, ierr)

    info%ni_global = ni_global
    info%nj_global = nj_global
    info%ni_local = ni_global / n_ranks
    info%ibegin_global = rank * info%ni_local
    info%nj_local = nj_global
    info%ibegin_global = 0

  end subroutine

end module


program client
  use xios
  use mpi
  use ifile_attr
  use partition_mod
  implicit none

  integer :: ierr = 0
  type(partition_info) :: pinfo 

  call MPI_INIT(ierr)

  call initialise(pinfo)
  call simulate(pinfo)
  call finalise()

  call MPI_Finalize(ierr)

contains

  subroutine initialise(pinfo)

    integer :: comm = -1
    integer :: i, j
    double precision, dimension (:), allocatable :: x_values
    double precision, dimension (:), allocatable :: y_values
    type(xios_date) :: origin
    type(xios_date) :: start
    type(xios_duration) :: tstep
    type(xios_file) :: file_hdl
    type(xios_axis) :: axis_hdl
    type(partition_info), intent(out) :: pinfo

    ! Datetime setup
    origin = xios_date(2022, 12, 12, 12, 0, 0)
    start = xios_date(2022, 12, 12, 12, 0, 0)
    tstep = xios_hour

    ! Initialise MPI and XIOS
    call xios_initialize('client', return_comm=comm)

    call xios_context_initialize('main', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call create_partition(10, 10, comm, pinfo) ! distribute levels in the field across clients and save distribution info in pinfo

    ! Set x axis values to [2, 4, 6, 8, ... ]
    allocate ( x_values(pinfo%ni_local) )
    do i=1, pinfo%ni_local
      x_values(i) = 2 * (i + pinfo%ibegin_global)
    enddo

    ! Set y axis values to [2, 4, 6, 8, ... ]
    allocate ( y_values(pinfo%nj_local) )
    do j=1, pinfo%nj_local
      y_values(j) = 2 * (j + pinfo%jbegin_global)
    enddo

    ! Set the axis attributes
    call xios_set_axis_attr("x", n_glo=pinfo%ni_global, begin=pinfo%ibegin_global, n=pinfo%ni_local, value=x_values)
    call xios_set_axis_attr("y", n_glo=pinfo%nj_global, begin=pinfo%jbegin_global, n=pinfo%nj_local, value=y_values)

    call xios_close_context_definition()
    
  end subroutine initialise


  subroutine finalise()

    ! Finalise XIOS and MPI
    call xios_context_finalize()

    call xios_finalize()

  end subroutine finalise


  subroutine simulate(pinfo)

    type(partition_info), intent(in) :: pinfo 
    integer :: ts
    integer :: ilevel, jlevel
    double precision, dimension (:, :), allocatable :: inpdata

    allocate ( inpdata(pinfo%ni_local, pinfo%nj_local) )

    ! Read the pressure data from the output file.
    call xios_recv_field('pressure_in', inpdata)

    ! Send the pressure data to the output file.
    do ts=1, 1
      call xios_update_calendar(ts)
      call xios_send_field('pressure', inpdata)
    enddo

    deallocate ( inpdata )

  end subroutine simulate

end program client