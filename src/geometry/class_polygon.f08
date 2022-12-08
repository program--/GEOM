module class_polygon
   use class_geometry
   implicit none
   private

   type, extends(geometry), public :: polygon
      real, dimension(:), allocatable :: x, y
   contains
      procedure :: print => print_polygon
   end type polygon
contains
   subroutine print_polygon(this)
      class(polygon), intent(in) :: this
      integer :: i
      print *, 'POLYGON('
      do i = 1, size(this%x)
         print *, '(', this%x(i), ', ', this%y(i), ')'
      end do
      print *, ')'
   end subroutine print_polygon
end module class_polygon
