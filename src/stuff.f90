module stuffmod
    implicit none
    contains
        elemental function testfunc(a,b) result(x)
            integer, intent(in) :: a,b
            integer :: x
            x = a*b
        end function
end module

program test
    use utilities
    use percolation
    use stuffmod
    implicit none
    character(len=:), allocatable :: string
    integer, dimension(:,:), allocatable :: intarray
    integer :: i,j, fileunit
    integer, dimension(:), allocatable :: a, b
    real(kind=dp), dimension(:), allocatable :: bin_mids, n

    write(*,*) testfunc(3,[4,5,6])
    write(*,*) testfunc([1,2,3],4)
    write(*,*) testfunc([1,2,3],[4,5,6])

    a = [1,2,3,4,5]
    b = [6,7,8,9,10]

    write(*,fmt="(i0,x,i0,*(/,i0,x,i0))") a, b

end program test
