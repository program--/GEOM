module class_geometry
   implicit none

   type, abstract :: geometry
   contains
      procedure(print_interface), deferred :: print
   end type geometry

   interface
      subroutine print_interface(this)
         import geometry
         class(geometry), intent(in) :: this
      end subroutine print_interface
   end interface
end module class_geometry
