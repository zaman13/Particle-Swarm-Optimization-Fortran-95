program main
    use fitness_example, only: fitness
    use PSOclassicG, only: pso
    use iso_fortran_env, only: pr => real64

    real(pr) :: x(30), xmin(30), xmax(30)

    xmax = 30
    xmin = -30
    call pso(fitness, x, psize=30, verbose=.true., xmin=xmin, xmax=xmax)
end program main
