module custom_type_mod
!
! A module to contain any derived types required for the test
!

use xios

implicit none

public :: context_struct
!
! The context_struct type holds the context name and handle, along with an
! boolen label is_available to allow an easy check to see if the given
! context is available to the model.
!
type :: context_struct
   type(xios_context) :: handle
   character(len=20) :: context_name
   logical :: is_available
   integer :: closing_timestep
end type context_struct

end module custom_type_mod
