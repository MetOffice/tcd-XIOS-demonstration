module context_def_mod
!
! Read in the parameters for contexts that terminate before the end of the
! model run from namelist, and load each into the context_struct derived
! type, and make an array of each context.
!

use custom_type_mod, only : context_struct

implicit none

public :: number_xios_contexts, xios_contexts, load_contexts

integer :: number_xios_contexts
type(context_struct), allocatable :: xios_contexts(:)

contains

subroutine load_contexts()

  integer :: n_contexts

  character(len=20) :: context_names(4) = ''
  integer :: closing_timesteps(4) = 0

  integer :: file_unit, io_stat
  integer :: i

  type(context_struct) :: my_context_struct

  !
  ! Read in the CUSTOM_CONTEXTS namelist from the file context.nml, and
  ! construct a list of context_struct types for each context defined in
  ! the namelist. The namelist contains 3 variables:
  ! integer: n_contexts: the number of contexts to be read in
  ! character(len=20): context_names(4): the name of each custom context
  ! integer: closing_timesteps: the timestep on which the corresponding timestep
  !                             in the context_names array is closed.
  ! There are a maximum of 4 contexts, and only the first n_contexts are read in
  !

  namelist /CUSTOM_CONTEXTS/ n_contexts, context_names, closing_timesteps

  open(action='read', file='context.nml', iostat=io_stat, newunit=file_unit)
  read(nml=CUSTOM_CONTEXTS, iostat=io_stat, unit=file_unit)
  if (io_stat /= 0) write(*, '("Error, invalid namelist format")')
  close(file_unit)

  number_xios_contexts = n_contexts
  allocate(xios_contexts(n_contexts))
  do i=1,n_contexts
    my_context_struct%context_name = context_names(i)
    my_context_struct%closing_timestep = closing_timesteps(i)
    xios_contexts(i) = my_context_struct
  end do

end subroutine load_contexts
    

end module context_def_mod
