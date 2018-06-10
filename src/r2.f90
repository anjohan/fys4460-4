program findk
    use utilities
    use percolation
    use randomwalk
    implicit none

    integer :: L, num_systems, num_steps, num_walkers, num_ps, i, j
    integer :: fileunit
    real(kind=dp), dimension(:,:), allocatable :: displacements
    real(kind=dp), dimension(:), allocatable :: ps, xis
    integer, dimension(:), allocatable :: t, t0s
    real(kind=dp) :: k, slope, const, expconst
    character(len=:), allocatable :: filename

    L = 512
    num_systems = 10
    num_walkers = 100
    num_steps   = 1000000
    num_ps = 10

    ps = linspace(pc, 1.2d0*pc, num_ps)
    allocate(t(0:num_steps))
    allocate(displacements(0:num_steps, num_ps))
    t(:) = [(i, i = 0, num_steps)]

    call random_seed()

    do i = 1, num_ps
        displacements(:,i) = random_walkers(ps(i), L, num_systems, num_walkers, num_steps)
    end do

    allocate(t0s(2:num_ps))
    allocate(xis(2:num_ps))
    t0s(:) = 1
    do i = 2, num_ps
        associate(t0 => t0s(i))
            do
                if(t0 > num_steps) then
                    t0 = num_steps
                    exit
                else if(displacements(t0, i) >= 2*displacements(t0, 1)) then
                    exit
                end if
                t0 = t0 + 1
            end do
            xis(i) = sqrt(displacements(t0, i))
        end associate
    end do

    open(newunit=fileunit, file="tmp/r2.dat", status="replace")
    do i = 1, num_steps, num_steps/1000
        write(unit=fileunit, fmt="(i0,*(x,f0.6))") t(i), &
                            [(displacements(i,j), j = 1, num_ps)]
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_ps.dat", status="replace")
    do i = 1, num_ps
        write(unit=fileunit, fmt="(f0.3)") ps(i)
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_t0s.dat", status="replace")
    do i = 2, num_ps
        write(unit=fileunit, fmt="(i0)") t0s(i)
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_xis.dat", status="replace")
    do i = 2, num_ps
        write(unit=fileunit, fmt="(f0.3)") xis(i)
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_t0xi.dat", status="replace")
    do i = 2, num_ps
        write(unit=fileunit, fmt="(f0.3)") t0s(i), xis(i)
    end do
    close(unit=fileunit)
end program findk
