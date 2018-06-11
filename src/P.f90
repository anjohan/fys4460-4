program probdist
    use utilities
    use randomwalk
    implicit none

    real(kind=dp), dimension(:,:), allocatable :: histograms
    integer :: L, num_steps, num_walkers, num_systems, num_hists, num_bins
    integer :: i, fileunit
    integer, dimension(:), allocatable :: histtimes


    L = 512
    num_systems = 100
    num_walkers = 5000
    num_steps = 5000
    num_hists = 5

    histtimes = [(num_steps/num_hists * i, i = 0, num_hists)]

    histograms = probability_distribution(pc, L, num_steps, num_walkers, &
                                          num_systems, num_hists)

    open(newunit=fileunit, file="tmp/P_pc.dat", status="replace")
    do i = 1, size(histograms,2)
        write(unit=fileunit, fmt="(*(f0.12,x))") histograms(:,i)
    end do

    open(newunit=fileunit, file="tmp/P_t.dat", status="replace")
    do i = 1, num_hists+1
        write(unit=fileunit, fmt="(i0)") histtimes(i)
    end do
end program probdist
