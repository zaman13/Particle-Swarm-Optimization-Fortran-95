program PSOclassicG

! Author: Mohammad Asif Zaman
! Fortran code (gfortran, G95)
! The fitness.f95 and timestamp.f95 files are expected to be in the same directory.

! gfortran -c PSOclassicG.f95
! gfortran PSOclassicG.f95 -o PSOclassicG.exe
! ./PSOclassicG.exe to run (in Cygwin or powershell terminal)


! Date: 13th Aug., 2012

! VTR = Value to reach. Required fitness function value for termination.
! NIR = Number of iterations required to obtain VTR.
! pbest = population best solution at each generation/iteration
! pbest_val = fitness value at pbset
! gbest = global best solution
! gbest_val = fitness value at gbest



! -> Mathematical parameters
!real(kind(1.0d0)) , parameter :: pi = 3.141592653589793
!complex, parameter :: j = (0,1)
!real(kind(1.0d0)), parameter :: eps = epsilon(eps)
! <- Mathematical parameters



! May 2, 2020
! version 1.2
!   - timestamp subroutine: date bug fixed
!   - added Makefile
!   - The make file works in both linux terminal and windows powershell as well
!   - The code works fine in both linux and windows



! -> File operation parameters
CHARACTER(len=18) :: str
character(len=29) :: command_Make
character(len=35) :: command_File
! <- File operation parameters


! -> PSO and fitness parameters
integer, parameter :: psize = 30, dimen = 30, max_iter = 5000, c1 = 2, c2 = 2
real(kind(1.0d0)), parameter :: VTR = 1e-6
integer :: iter = 1, overfly_counter = 0, NIR = 0, NFE = 0
logical :: overfly_flag = .false.
real(kind(1.0d0)) :: xmin(dimen), xmax(dimen), vmin(dimen), vmax(dimen)
real(kind(1.0d0)) :: x(psize, dimen) = 0, v(psize, dimen) = 0
real(kind(1.0d0)) :: pbest(psize, dimen), gbest(dimen)
real(kind(1.0d0)) :: pbest_val(psize) = 0
real(kind(1.0d0)) :: gbest_val
real(kind(1.0d0)) :: w
real(kind(1.0d0)) :: gbest_val_store(max_iter) = 0, x11_store(max_iter)
! <- PSO and fitness parameters


integer :: loop1, loop2, loopG


real(kind(1.0d0)) :: time_start, time_end
real(kind(1.0d0)) :: temp1, temp2



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
	! All dimensions are assigned same limit. But this can easily be changed by employing a loop
	xmin = -30
	xmax = 30

	vmin = 0.5 * xmin                          ! lower limit for velocity
	vmax = 0.5 * xmax                          ! higher limit for velocity
	! <- Defining solution space



	call random_seed
	call random_number(x)
	call random_number(v)


	! -> Intializaing particle position and velocities

	! The cost of the first particle is evaluated because it is needed as a stepping stool for evaluating gbest.
	x(1,:) = x(1,:) * (xmax - xmin) + xmin			! fixing the location of the first particle
	v(1,:) = v(1,:) * (vmax - vmin) + vmin
	pbest(1,:) = x(1,:)
	gbest = pbest(1,:)
	call fitness(dimen, pbest(1,:), pbest_val(1))		! evaluating the fitenss of the first particle
	NFE = NFE + 1
	gbest_val = pbest_val(1)


	! Composite threaded loop (15th Aug., 2012)
	! As the population is adjusted, the corresponding fitness parameters of the population is assigned simultaneously.

	do loop1 = 2, psize
		x(loop1,:) = x(loop1,:) * (xmax - xmin) + xmin
		v(loop1,:) = v(loop1,:) * (vmax - vmin) + vmin

		pbest(loop1,:) = x(loop1,:)

		call fitness(dimen, x(loop1,:), pbest_val(loop1))
		NFE = NFE + 1
		if ( pbest_val(loop1) < gbest_val ) then		! Geared for minimization problem
			gbest_val = pbest_val(loop1)
			gbest = x(loop1,:)
		end if

	end do


	! <- Intializaing particle position and velocities





	! Main iteration loop. Creating the generations.
	do iter = 1, max_iter

		w = 0.9 - (0.9 - 0.2) * iter /max_iter

		gbest_val_store(iter) = gbest_val
		x11_store(iter) = x(1,1)


		do loop1 = 1, psize
			overfly_flag = .false.
			do loop2 = 1, dimen
				call random_number(temp1)
				call random_number(temp2)
				v(loop1, loop2) = w*v(loop1,  loop2) + c1 * temp1 * (pbest(loop1,loop2) - x(loop1,loop2))
				v(loop1, loop2) = v(loop1, loop2) + c2* temp2 *(gbest(loop2) - x(loop1,loop2))

				if ( v(loop1, loop2) > vmax(loop2) ) then
					v(loop1, loop2) = vmax(loop2)
				end if

				if ( v(loop1, loop2) < vmin(loop2) ) then
					v(loop1, loop2) = vmin(loop2)
				end if


				x(loop1, loop2) = x(loop1, loop2) + v(loop1, loop2)

				if ( x(loop1, loop2) < xmin(loop2) .or. x(loop1, loop2) > xmax(loop2) ) then
					overfly_flag = .true.
					!print *, "fly"
					overfly_counter = overfly_counter + 1
				end if

			end do

			! Invisible boundary condition. Can be changed to reflecting or absorbing boundary conditions.

			if ( .not. overfly_flag ) then

				!print *, "No overfly"
				call fitness(dimen, x(loop1,:), temp1)
				if (NIR == 0) then
					NFE = NFE + 1
				end if


				! Composite IF statement that adjusts the pbset and gbest parameters (15th Aug., 2012)
				if ( temp1 < pbest_val(loop1) ) then
					pbest_val(loop1) = temp1
					pbest(loop1,:) = x(loop1,:)

					if ( pbest_val(loop1) < gbest_val ) then
						gbest_val = pbest_val(loop1)
						gbest = pbest(loop1,:)
						if ( NIR == 0 .and. gbest_val < VTR ) then
							NIR = iter
						end if
					end if

				end if


			end if

		end do  ! Ending loop over population

	end do  ! Ending main iteration loop


	call cpu_time(time_end)





	call timestamp(str)


	!do loop1 = 1, psize
	!print "(4f10.3)",  x(loop1,:), pbest_val(loop1)
	!print *,  x(loop1,:), pbest_val(loop1)

	!print "(1f10.3)", pbest_val(loop1)
	!end do

	!print "(2f10.5)",  x
	!print * , "Best"
	!print *, x(1,:)
	!print "(4f10.6)", gbest, gbest_val

	print *, "======================================================"
	print *, "Time stamp = ", str
	print *, "======================================================"

	print *, "======================================================"
	print *, "Algorithm parameters"
	print *, "--------------------------------------------------------------------------------------------"
	print *, "Problem dimension = ", dimen
	print *, "Dynamic range = ", xmin(1) , "to", xmax(1)
	print *, "Population size = ", psize
	print *, "Maximum allowed iterations = ", max_iter
	print *, "Defined value to reach (VTR) = ", VTR
	print *, "======================================================"

	print *,


	print *, "======================================================"
	print *, "Performance parameters"
	print *, "--------------------------------------------------------------------------------------------"
	print *, "Total CPU time = ", time_end - time_start, "seconds"
	print *, "Overfly counter = ", overfly_counter
	print *, "No. of iterations required to reach VTR (NIR) = ", NIR
	print *, "No. of function evaluations (NFE) = ", NFE
	print *, "Global best value achieved after NIR = ", gbest_val_store(NIR)
	print *, "Global best value achieved after max_iter = ", gbest_val
	print *,
	print *, "Global best solution ="
	print *,
	do loop1 = 1, dimen
		print *, gbest(loop1)
	end do
	print *,
	!call fitness(dimen, gbest, temp1)
	!print *, temp1
	print *, "======================================================"

!end do  ! Ending NIR loop if active.




! -> CSV File operation 17th Aug., 2012. (M -> 18th Aug., 2012)


command_Make = ('mkdir ' // 'Data_') // str

print *, "commandMake = ", command_Make

call system(command_Make)
command_File =   'Data_' // str // '/' // 'converg.csv' 	! the file name must be 11 characters, including . and extension
open  (20, file = command_File)
	do loop1 = 1, max_iter
		write (20,*) gbest_val_store(loop1), ",", x11_store(loop1)
	end do
close (20)

command_File =   'Data_' // str // '/' // 'final_x.txt' 	! the file name must be 11 characters, including . and extension
open  (30, file = command_File)
	do loop1 = 1, psize

		write (30,*) x(loop1,:), ","

	end do
close (30)

! <- File operation 17th Aug., 2012.



! -> Log file operation
command_File =   'Data_' // str // '/' // 'logfile.txt'
open  (11, file = command_File)

write (11,*) "======================================================"
write (11,*) "Time stamp = ", str
write (11,*) "======================================================"

write (11,*) "======================================================"
write (11,*) "Algorithm parameters"
write (11,*) "--------------------------------------------------------------------------------------------"
write (11,*) "Problem dimension = ", dimen
write (11,*) "Dynamic range = ", xmin(1) , "to", xmax(1)
write (11,*) "Population size = ", psize
write (11,*) "Maximum allowed iterations = ", max_iter
write (11,*) "Defined value to reach (VTR) = ", VTR
write (11,*) "======================================================"

write (11,*)


write (11,*) "======================================================"
write (11,*) "Performance parameters"
write (11,*) "---------------------------------------------------------------------------------------------"
write (11,*) "Total CPU time = ", time_end - time_start, "seconds"
write (11,*) "Overfly counter = ", overfly_counter
write (11,*) "No. of iterations required to reach VTR (NIR) = ", NIR
write (11,*) "No. of function evaluations (NFE) = ", NFE
write (11,*) "Global best value achieved after NIR = ", gbest_val_store(NIR)
write (11,*) "Global best value achieved after max_iter = ", gbest_val
write (11,*)
write (11,*) "Global best solution ="
write (11,*)
do loop1 = 1, dimen
	write (11,*) gbest(loop1)
end do
write (11,*)
write (11,*) "======================================================"

close(11)

! <- Log file operation




contains

	include 'fitness.f95'
	include 'timestamp.f95'











end program PSOclassicG
