program findk
    use utilities
    use percolation
    use randomwalk
    implicit none

    integer :: L, num_systems, num_steps, num_walkers, i
    integer :: fileunit
    real(kind=dp), dimension(:), allocatable :: displacement
    integer, dimension(:), allocatable :: t
    real(kind=dp) :: k, slope, const

    L = 512
    num_systems = 10
    num_walkers = 100
    num_steps   = 100

    call random_seed()

    allocate(displacement(0:num_steps))
    allocate(t(0:num_steps))

    displacement(:) = random_walkers(pc, L, num_systems, num_walkers, num_steps)
    t(:) = [(i, i = 0, num_steps)]

    open(newunit=fileunit, file="tmp/r2pc.dat", status="replace")
    do i = 0, num_steps
        write(unit=fileunit, fmt="(i0,x,f0.6)") t(i), displacement(i)
    end do
    close(unit=fileunit)



end program findk
