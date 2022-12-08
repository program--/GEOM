module class_linestring
   use class_geometry
   implicit none
   private

   type, extends(geometry), public :: linestring
      real, dimension(:), allocatable :: x, y
   contains
      procedure :: print => print_linestring
   end type linestring
contains
   subroutine print_linestring(this)
      class(linestring), intent(in) :: this
      integer :: i
      print *, 'LINESTRING('
      do i = 1, size(this%x)
         print *, '(', this%x(i), ', ', this%y(i), ')'
      end do
      print *, ')'
   end subroutine print_linestring
end module class_linestring
