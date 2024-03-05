module PSOclassicG
   !! Author: Mohammad Asif Zaman
   !! Fortran code (gfortran, G95)
   !! The fitness.f95 and timestamp.f95 files are expected to be in the same directory.
   !! Date: 13th Aug., 2012

   !! VTR = Value to reach. Required fitness function value for termination.
   !! NIR = Number of iterations required to obtain VTR.
   !! pbest = population best solution at each generation/iteration
   !! pbest_val = fitness value at pbset
   !! gbest = global best solution
   !! gbest_val = fitness value at gbest
   !
   !! -> Mathematical parameters
   !!real(pr) , parameter :: pi = 3.141592653589793
   !!complex, parameter :: j = (0,1)
   !!real(pr), parameter :: eps = epsilon(eps)
   !! <- Mathematical parameters

   !! May 2, 2020
   !! version 1.2
   !!   - timestamp subroutine: date bug fixed
   !!   - added Makefile
   !!   - The make file works in both linux terminal and windows powershell as well
   !!   - The code works fine in both linux and windows
   use iso_fortran_env, only: pr => real64
   implicit none

   ! -> File operation parameters
   CHARACTER(len=18) :: str
   character(len=:), allocatable :: command_Make
   character(len=:), allocatable :: command_File
   ! <- File operation parameters


   integer :: loop1, loop2, loopG

   real(pr) :: time_start, time_end
   real(pr) :: temp1, temp2

contains

   subroutine pso(fitness, xopt, verbose, psize, xmax, xmin)
      interface
         subroutine fitness(x, cost)
            import pr
            real(pr), intent(in) :: x(:)
            real(pr), intent(out) :: cost
         end subroutine
      end interface

      real(pr), intent(in out) :: xopt(:) !! Optimal values obtained
      real(pr), intent(in) :: xmax(size(xopt)) !! Maxumim dimensions
      real(pr), intent(in) :: xmin(size(xopt)) !! Minimum dimensions
      integer, intent(in) :: psize !! Number of particles
      logical, intent(in) :: verbose !! Print output

      integer :: dimen
      real(pr) :: vmin(size(xopt)), vmax(size(xopt))
      real(pr) :: x(psize, size(xopt))
      real(pr) :: v(psize, size(xopt))
      real(pr) :: pbest(psize, size(xopt)), gbest(size(xopt))

      ! -> PSO and fitness parameters
      integer, parameter :: max_iter = 5000, c1 = 2, c2 = 2
      real(pr), parameter :: VTR = 1e-6
      integer :: iter = 1, overfly_counter = 0, NIR = 0, NFE = 0
      logical :: overfly_flag = .false.
      real(pr) :: pbest_val(psize)
      real(pr) :: gbest_val
      real(pr) :: w
      real(pr) :: gbest_val_store(max_iter), x11_store(max_iter)
      ! <- PSO and fitness parameters

      pbest_val = 0
      gbest_val = 0
      gbest_val_store = 0

      dimen = size(xopt)
      x = 0
      v = 0

      ! The following loop runs the PSO. Every time this loop starts, a new PSO run is initiated with new seeds.
      loopG = 0
      ! Do while (NIR == 0)    ! Turn this loop on if you want to run PSO multiple times till it satisfies VTR

      ! until NIR reaches a finite value. If VTR is not achieved, this loop will continue.

      loopG = loopG + 1
      print *, loopG
      print *, NIR

      NIR = 0
      NFE = 0
      overfly_counter = 0

      call cpu_time(time_start)

      ! -> Defining solution space
      ! All dimensions are assigned same limit.
      ! But this can easily be changed by employing a loop
      vmin = 0.5*xmin  ! lower limit for velocity
      vmax = 0.5*xmax  ! higher limit for velocity
      ! <- Defining solution space

      call random_seed
      call random_number(x)
      call random_number(v)

      ! -> Intializaing particle position and velocities

      ! The cost of the first particle is evaluated because it is needed
      ! as a stepping stool for evaluating gbest.
      ! fixing the location of the first particle
      x(1, :) = x(1, :)*(xmax - xmin) + xmin
      v(1, :) = v(1, :)*(vmax - vmin) + vmin
      pbest(1, :) = x(1, :)
      gbest = pbest(1, :)

      ! evaluating the fitenss of the first particle
      call fitness(pbest(1, :), pbest_val(1))                
      NFE = NFE + 1
      gbest_val = pbest_val(1)

      ! Composite threaded loop (15th Aug., 2012)
      ! As the population is adjusted, the corresponding fitness parameters of the population is assigned simultaneously.

      do loop1 = 2, psize
         x(loop1, :) = x(loop1, :)*(xmax - xmin) + xmin
         v(loop1, :) = v(loop1, :)*(vmax - vmin) + vmin

         pbest(loop1, :) = x(loop1, :)

         call fitness(x(loop1, :), pbest_val(loop1))
         NFE = NFE + 1
         if (pbest_val(loop1) < gbest_val) then                ! Geared for minimization problem
            gbest_val = pbest_val(loop1)
            gbest = x(loop1, :)
         end if

      end do

      ! <- Intializaing particle position and velocities

      ! Main iteration loop. Creating the generations.
      do iter = 1, max_iter

         w = 0.9 - (0.9 - 0.2)*iter/max_iter

         gbest_val_store(iter) = gbest_val
         x11_store(iter) = x(1, 1)

         do loop1 = 1, psize
            overfly_flag = .false.
            do loop2 = 1, dimen
               call random_number(temp1)
               call random_number(temp2)
               v(loop1, loop2) = w*v(loop1, loop2) + c1*temp1*(pbest(loop1, loop2) - x(loop1, loop2))
               v(loop1, loop2) = v(loop1, loop2) + c2*temp2*(gbest(loop2) - x(loop1, loop2))

               if (v(loop1, loop2) > vmax(loop2)) then
                  v(loop1, loop2) = vmax(loop2)
               end if

               if (v(loop1, loop2) < vmin(loop2)) then
                  v(loop1, loop2) = vmin(loop2)
               end if

               x(loop1, loop2) = x(loop1, loop2) + v(loop1, loop2)

               if (x(loop1, loop2) < xmin(loop2) .or. x(loop1, loop2) > xmax(loop2)) then
                  overfly_flag = .true.
                  overfly_counter = overfly_counter + 1
               end if

            end do

            ! Invisible boundary condition. Can be changed to reflecting or absorbing boundary conditions.
            if (.not. overfly_flag) then

               !print *, "No overfly"
               call fitness(x(loop1, :), temp1)
               if (NIR == 0) then
                  NFE = NFE + 1
               end if

               ! Composite IF statement that adjusts the pbset and gbest parameters (15th Aug., 2012)
               if (temp1 < pbest_val(loop1)) then
                  pbest_val(loop1) = temp1
                  pbest(loop1, :) = x(loop1, :)

                  if (pbest_val(loop1) < gbest_val) then
                     gbest_val = pbest_val(loop1)
                     gbest = pbest(loop1, :)
                     if (NIR == 0 .and. gbest_val < VTR) then
                        NIR = iter
                     end if
                  end if

               end if

            end if

         end do  ! Ending loop over population

      end do  ! Ending main iteration loop

      call cpu_time(time_end)

      xopt = gbest

      ! call timestamp(str)

      if (verbose) then
         print *, "======================================================"
         print *, "Time stamp = ", str
         print *, "======================================================"

         print *, "======================================================"
         print *, "Algorithm parameters"
         print *, "--------------------------------------------------------------------------------------------"
         print *, "Problem dimension = ", dimen
         print *, "Dynamic range = ", xmin(1), "to", xmax(1)
         print *, "Population size = ", psize
         print *, "Maximum allowed iterations = ", max_iter
         print *, "Defined value to reach (VTR) = ", VTR
         print *, "======================================================"

         print *, ""

         print *, "======================================================"
         print *, "Performance parameters"
         print *, "--------------------------------------------------------------------------------------------"
         print *, "Total CPU time = ", time_end - time_start, "seconds"
         print *, "Overfly counter = ", overfly_counter
         print *, "No. of iterations required to reach VTR (NIR) = ", NIR
         print *, "No. of function evaluations (NFE) = ", NFE
         print *, "Global best value achieved after NIR = ", gbest_val_store(NIR)
         print *, "Global best value achieved after max_iter = ", gbest_val
         print *, ""
         print *, "Global best solution ="
         print *, ""
         do loop1 = 1, dimen
            print *, gbest(loop1)
         end do
         print *, ""
         !call fitness(dimen, gbest, temp1)
         !print *, temp1
         print *, "======================================================"
      end if

      !end do  ! Ending NIR loop if active.

      ! -> CSV File operation 17th Aug., 2012. (M -> 18th Aug., 2012)
      ! call make_csv
      ! <- File operation 17th Aug., 2012.

      ! -> Log file operation
      ! call make_log
      ! <- Log file operation
      contains
         subroutine make_csv
            integer :: file_unit
            command_Make = ('mkdir '//'Data_')//str
            call system(command_Make)

            command_File = 'Data_'//str//'/'//'converg.csv'
            open (newunit=file_unit, file=command_File)
            do loop1 = 1, max_iter
               write (file_unit, *) gbest_val_store(loop1), ",", x11_store(loop1)
            end do
            close (file_unit)

            command_File = 'Data_'//str//'/'//'final_x.txt'
            open (newunit=file_unit, file=command_File)
            do loop1 = 1, psize
               write (file_unit, *) x(loop1, :), ","
            end do
            close (file_unit)
         end subroutine

         subroutine make_log
            integer :: file_unit
            command_File = 'Data_'//str//'/'//'logfile.txt'
            open (newunit=file_unit, file=command_File)

            write (file_unit, *) "======================================================"
            write (file_unit, *) "Time stamp = ", str
            write (file_unit, *) "======================================================"

            write (file_unit, *) "======================================================"
            write (file_unit, *) "Algorithm parameters"
            write (file_unit, *) "--------------------------------------------------------------------------------------------"
            write (file_unit, *) "Problem dimension = ", dimen
            write (file_unit, *) "Dynamic range = ", xmin(1), "to", xmax(1)
            write (file_unit, *) "Population size = ", psize
            write (file_unit, *) "Maximum allowed iterations = ", max_iter
            write (file_unit, *) "Defined value to reach (VTR) = ", VTR
            write (file_unit, *) "======================================================"

            write (file_unit, *)

            write (file_unit, *) "======================================================"
            write (file_unit, *) "Performance parameters"
            write (file_unit, *) "---------------------------------------------------------------------------------------------"
            write (file_unit, *) "Total CPU time = ", time_end - time_start, "seconds"
            write (file_unit, *) "Overfly counter = ", overfly_counter
            write (file_unit, *) "No. of iterations required to reach VTR (NIR) = ", NIR
            write (file_unit, *) "No. of function evaluations (NFE) = ", NFE
            write (file_unit, *) "Global best value achieved after NIR = ", gbest_val_store(NIR)
            write (file_unit, *) "Global best value achieved after max_iter = ", gbest_val
            write (file_unit, *)
            write (file_unit, *) "Global best solution ="
            write (file_unit, *)
            do loop1 = 1, dimen
               write (file_unit, *) gbest(loop1)
            end do
            write (file_unit, *)
            write (file_unit, *) "======================================================"

            close (file_unit)
         end subroutine make_log
   end subroutine
end module PSOclassicG
