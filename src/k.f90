program findk
    use utilities
    use percolation
    use randomwalk
    implicit none

    integer :: L, num_systems, num_steps, num_walkers, i
    integer :: fileunit
    real(kind=dp), dimension(:), allocatable :: displacement
    integer, dimension(:), allocatable :: t
    real(kind=dp) :: k, slope, const, expconst

    L = 512
    num_systems = 10
    num_walkers = 100
    num_steps   = 100000

    call random_seed()

    allocate(displacement(0:num_steps))
    allocate(t(0:num_steps))

    displacement(:) = random_walkers(pc, L, num_systems, num_walkers, num_steps)
    t(:) = [(i, i = 0, num_steps)]


    call linfit(log(1.0d0*t(1:)), log(displacement(1:)), slope, const)

    k = slope/2
    expconst = exp(const)

    open(newunit=fileunit, file="tmp/r2pc.dat", status="replace")
    do i = 0, num_steps, num_steps/10000
        write(unit=fileunit, fmt="(i0,x,f0.6,x,f0.6)") t(i), displacement(i), &
                                                expconst * t(i)**slope
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/k.dat", status="replace")
    write(unit=fileunit, fmt="(f0.3)") k
    close(unit=fileunit)


end program findk
