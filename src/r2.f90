program findk
    use utilities
    use percolation
    use randomwalk
    implicit none

    integer :: L, num_systems, num_steps, num_walkers, num_ps, i, j
    integer :: fileunit
    real(kind=dp), dimension(:,:), allocatable :: displacements
    real(kind=dp), dimension(:), allocatable :: ps, xis, Ds, logt, logr2
    integer, dimension(:), allocatable :: t, t0s
    real(kind=dp) :: k, slope, const, expconst, nu, slope2, const2, tmp_const
    real(kind=dp) :: mu, Dconst, x
    real(kind=dp), dimension(:,:), allocatable :: x_collapse, y_collapse
    real(kind=dp), dimension(:), allocatable :: t2k, tx
    character(len=:), allocatable :: filename

    L = 512
    num_systems = 20
    num_walkers = 50
    num_steps   = 1000000
    num_ps = 10

    ps = linspace(pc, 1.3d0*pc, num_ps)
    allocate(t(0:num_steps))
    allocate(displacements(0:num_steps, num_ps))
    t(:) = [(i, i = 0, num_steps)]

    call random_seed()

    do i = 1, num_ps
        displacements(:,i) = random_walkers(ps(i), L, num_systems, &
                                            num_walkers, num_steps)
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

    call linfit(log(ps(2:) - pc), log(xis), slope, const)
    call linfit(log(ps(2:) - pc), log(1.0d0*t0s), slope2, const2)
    nu = -slope

    allocate(Ds(2:num_ps))
    do i = 2, num_ps
        call linfit(1.0d0*t(t0s(i):), displacements(t0s(i):, i), Ds(i), tmp_const)
    end do

    call linfit(log(ps(2:)-pc), log(Ds), mu, Dconst)
    logt = log(1.0d0*t(1:))
    logr2 = log(displacements(1:,1))
    call linfit(logt, logr2, k, tmp_const)
    k = k/2
    x = (1-2*k)/mu
    t2k = t(1:)**(-2*k)
    tx = t(1:)**x
    write(*,*) t2k(:10)

    allocate(x_collapse(num_steps, 2:num_ps))
    allocate(y_collapse(num_steps, 2:num_ps))

    do i = 2, num_ps
        x_collapse(:,i) = (ps(i) - pc)*tx
        y_collapse(:,i) = t2k * displacements(1:,i)
    end do

    open(newunit=fileunit, file="tmp/r2_collapse.dat", status="replace")
    do i = 1, num_steps, num_steps/1000
        write(unit=fileunit, fmt="(*(f0.12,x))") [([x_collapse(i,j),y_collapse(i,j)], &
                                                  j = 2, num_ps)]
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
        write(unit=fileunit, fmt="(i0,x,f0.3)") t0s(i), xis(i)
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_pxi.dat", status="replace")
    do i = 2, num_ps
        write(unit=fileunit, fmt="(f0.3,x,f0.3,x,f0.3)") ps(i), xis(i), exp(const)*(ps(i)-pc)**slope
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_pt0.dat", status="replace")
    do i = 2, num_ps
        write(unit=fileunit, fmt="(f0.3,x,i0,x,f0.3)") ps(i)-pc, t0s(i), exp(const2)*(ps(i)-pc)**slope2
    end do
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_nu.dat", status="replace")
    write(unit=fileunit, fmt="(f0.3)") nu
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_t0pow.dat", status="replace")
    write(unit=fileunit, fmt="(f0.3)") slope2
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_mu.dat", status="replace")
    write(unit=fileunit, fmt="(f0.3)") mu
    close(unit=fileunit)

    open(newunit=fileunit, file="tmp/r2_D.dat", status="replace")
    do i = 2, num_ps
        write(unit=fileunit, fmt="(f0.3,x,f0.3,x,f0.3)") ps(i)-pc, Ds(i), exp(Dconst)*(ps(i)-pc)**mu
    end do
    close(unit=fileunit)
end program findk
