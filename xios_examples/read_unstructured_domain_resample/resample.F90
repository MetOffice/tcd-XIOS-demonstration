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
    integer :: ni_glo
    integer :: nj_glo
    integer :: ni
    integer :: nj
    integer :: ibegin
    integer :: jbegin
    integer :: nvertex
    integer :: ni_glo_r
    integer :: nj_glo_r
    integer :: ni_r
    integer :: nj_r
    integer :: ibegin_r
    integer :: jbegin_r

    ! Arbitrary datetime setup, required for XIOS but unused
    origin = xios_date(2022, 2, 2, 12, 0, 0)
    start = xios_date(2022, 12, 13, 12, 0, 0)
    tstep = xios_hour

    ! Initialise MPI and XIOS
    call MPI_INIT(mpi_error)

    call xios_initialize('client', return_comm=comm)

    call MPI_Comm_rank(comm, rank, mpi_error)
    call MPI_Comm_size(comm, npar, mpi_error)

    ! use the domain_check context to obtain sizing information on all arrays and
    ! the domain decomposition for use in defining the main context interpretation
    call xios_context_initialize('domain_check', comm)
    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)
    call xios_close_context_definition()

    call xios_get_domain_attr("original_domain", &
                              ni_glo=ni_glo, nj_glo=nj_glo, &
                              ni=ni, nj=nj, &
                              ibegin=ibegin, jbegin=jbegin, nvertex=nvertex)
    print *, 'original_domain: rank,ni_glo,nj_glo,ni,nj,ibegin,jbegin,nvertex ',rank,ni_glo,nj_glo,ni,nj,ibegin,jbegin,nvertex

    call xios_get_domain_attr("resampled_domain", &
                              ni_glo=ni_glo_r, nj_glo=nj_glo_r, &
                              ni=ni_r, nj=nj_r, &
                              ibegin=ibegin_r, jbegin=jbegin_r)
    print *, 'resampled_domain: rank,ni_glo,nj_glo,ni,nj,ibegin,jbegin ',rank,ni_glo_r,nj_glo_r,ni_r,nj_r,ibegin_r,jbegin_r

    ! initialize the main context for interacting with the data.
    call xios_context_initialize('main', comm)

    call xios_set_time_origin(origin)
    call xios_set_start_date(start)
    call xios_set_timestep(tstep)

    call xios_set_domain_attr("original_domain", &
                              ni_glo=ni_glo, nj_glo=nj_glo, &
                              ni=ni, nj=nj, &
                              ibegin=ibegin, jbegin=jbegin, nvertex=nvertex)
    call xios_set_domain_attr("resampled_domain", &
                              ni_glo=ni_glo_r, nj_glo=nj_glo_r, &
                              ni=ni_r, nj=nj_r, &
                              ibegin=ibegin_r, jbegin=jbegin_r)

    call xios_close_context_definition()

  end subroutine initialise

  subroutine finalise()

    integer :: mpi_error

    ! Finalise all XIOS contexts and MPI
    call xios_set_current_context('domain_check')
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
    integer :: lenrx
    integer :: leny
    integer :: lenry

    ! Allocatable arrays, size is taken from input file
    double precision, dimension (:,:), allocatable :: inodata
    double precision, dimension (:,:), allocatable :: inedata

    call xios_get_domain_attr('original_domain', ni=lenx, nj=leny)
    call xios_get_domain_attr('resampled_domain', ni=lenrx, nj=lenry)

    print *, 'original_domain: rank,lenx,leny ',rank,lenx,leny
    print *, 'resampled_domain: rank,lenx,leny ',rank,lenrx,lenry

    allocate ( inodata(leny, lenx) )
    allocate ( inedata(lenry, lenrx) )

    ! Load data from the input file
    call xios_recv_field('odatain', inodata)
    call xios_recv_field('edatain', inedata)

    do ts=1, 1
      call xios_update_calendar(ts)
      call xios_get_current_date(current)
      ! Send (copy) the original data to the output file.
      call xios_send_field('odata', inodata)
      ! Send (copy) the expected data to the output file.
      call xios_send_field('edata', inedata)
    enddo

    deallocate (inodata)
    deallocate (inedata)

  end subroutine simulate

end program resample
