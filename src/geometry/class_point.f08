module class_point
   use class_geometry
   implicit none
   private

   type, extends(geometry), public :: point
      real :: x, y
   contains
      procedure, public :: print => print_point
   end type point

contains
   subroutine print_point(this)
      class(point), intent(in) :: this
      print *, 'POINT(', this%x, ', ', this%y, ')'
   end subroutine print_point

   pure function equals_point(a, b) result (res)
      class(point), intent(in) :: a, b
      logical :: res
      res = (a%x == b%x .and. a%y == b%y)
   end function equals_point
end module class_point
