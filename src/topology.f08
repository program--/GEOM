module topology
   use class_point
   use class_linestring
   use class_polygon
   implicit none
   private

   interface operator (==)
      module procedure equals_point, equals_line, equals_polygon
   end interface

   interface equals
      module procedure equals_point, equals_line, equals_polygon
   end interface

contains
   ! ---------------------------------------------------- !
   ! Relation: Equals                                     !
   ! ---------------------------------------------------- !
   pure logical function equals_point(a, b) result (res)
      class(point), intent(in) :: a, b
      res = (a%x == b%x .and. a%y == b%y)
   end function equals_point

   pure logical function equals_line(a, b) result(res)
      class(linestring), intent(in) :: a, b
      res = all(a%x == b%x) .and. all(a%y == b%y)
   end function equals_line

   pure logical function equals_polygon(a, b) result(res)
      class(polygon), intent(in) :: a, b
      res = all(a%x == b%x) .and. all(a%y == b%y)
   end function equals_polygon

   ! ---------------------------------------------------- !
   ! Relation: Intersects                                 !
   ! ---------------------------------------------------- !

   ! Intersects (Point - Point)
   pure logical function int_pt_pt(a, b) result (res)
      class(point), intent(in) :: a, b
      res = a == b
   end function int_pt_pt

   ! Intersects (Point - LineString)
   pure logical function int_pt_ls(a, b) result (res)
      class(point), intent(in) :: a
      class(linestring), intent(in) :: b
      logical :: xint, yint
      integer :: i
      do i = 1, size(b%x) - 1
         xint = abs(a%x - b%x(i)) + abs(a%x - b%x(i + 1)) == abs(b%x(i + 1) - b%x(i))
         yint = abs(a%y - b%y(i)) + abs(a%y - b%y(i + 1)) == abs(b%y(i + 1) - b%y(i))
         res = xint .and. yint
         if (res) return
      end do
   end function int_pt_ls

   ! Intersects (Point - Polygon)
   pure logical function int_pt_pg(a, b) result (res)
      ! TODO: Implement Sunday's Winding algorithm
      class(point), intent(in) :: a
      class(polygon), intent(in) :: b
      res = .false.

      ! Check bounding box
      if (a%x > maxval(b%x) .and. a%x < minval(b%x) .and. a%y > maxval(b%y) .and. a%y < minval(b%y)) return
   end function int_pt_pg

   ! Intersects (LineString - Point)
   pure logical function int_ls_pt(a, b) result(res)
      class(linestring), intent(in) :: a
      class(point), intent(in) :: b
      res = int_pt_ls(b, a)
   end function int_ls_pt

end module topology
