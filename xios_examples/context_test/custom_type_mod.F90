module custom_type_mod

use xios

implicit none

public :: context_struct
type :: context_struct
   type(xios_context) :: handle
   character(len=20) :: context_name
   logical :: is_available
   integer :: closing_timestep
end type context_struct

end module custom_type_mod
