program context_def_test

use xios
use mpi

use custom_type_mod, only : context_struct
use context_def_mod, only :  number_xios_contexts, xios_contexts, load_contexts

implicit none

integer :: rank, comm, size, ierr
character(len=*), parameter :: id="client"

integer :: i_context

integer :: size_i = 2, i
integer :: size_j = 2, j
integer, parameter :: axis_len = 1

double precision, allocatable :: field_A(:,:,:)
double precision :: lval(axis_len)=1

integer :: ts
type(xios_duration) :: dtime

call MPI_INIT(ierr)

call load_contexts

call xios_initialize(id, return_comm=comm)

!
!
! Allocate and set our data
!
!

allocate(field_A(size_i, size_j, axis_len))
field_A(:,:,:) = 0.0

!
!
! This section iterates over each context, and ensures that they are
! availiable by use by default, and have their handles assigned to them.
! This loop ensures that they are set up consistently
!
!

do i_context=1,number_xios_contexts

  xios_contexts(i_context)%is_available = .true.
  
  call xios_context_initialize( &
     trim(xios_contexts(i_context)%context_name), comm)
  call xios_get_handle( &
     trim(xios_contexts(i_context)%context_name), &
     xios_contexts(i_context)%handle)
  call xios_set_current_context(xios_contexts(i_context)%handle)

  call xios_set_axis_attr("axis_A", n_glo=axis_len, value=lval)
  call xios_set_domain_attr("domain_A", data_dim=2, ni_glo=size_i, &
     nj_glo=size_j, ibegin=0, jbegin=0, ni=size_i, nj=size_j, &
     type='rectilinear')
  dtime%second = 3600
  call xios_set_timestep(dtime)

  call xios_close_context_definition()
end do

!
!
! Start timestepping, and dealing with all contexts in turn
!
!
timestep: do ts=1,15
  ! change our data so we can see clearly which timestep gets written to file
  field_A(:,:,:) = field_A(:,:,:) + 1.0d0
  contexts: do i_context=1,number_xios_contexts
    available: if (xios_contexts(i_context)%is_available) then
      call xios_set_current_context(xios_contexts(i_context)%handle)
      call xios_update_calendar(ts)
      call xios_send_field("field_A", field_A)
      finalize: if (ts == xios_contexts(i_context)%closing_timestep) then
        call xios_context_finalize()
        xios_contexts(i_context)%is_available = .false.
      end if finalize
    end if available
  end do contexts
end do timestep

call MPI_COMM_FREE(comm, ierr)
call xios_finalize()
CALL MPI_FINALIZE(ierr)

end program context_def_test


