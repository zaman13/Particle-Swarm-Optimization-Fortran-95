
	subroutine fitness(D, x, cost)

		integer, intent(IN) :: D
		real(kind(1.0d0)), intent(out) :: cost
		real(kind(1.0d0)), dimension(D), intent(in) :: x
		real(kind(1.0d0)) :: temp1
		real(kind(1.0d0)) :: temp2

		integer :: loop1
		!integer :: loop2


		cost = 0
		temp1 = 0
		temp2 = 0

		! -> Sphere function
		!do loop1 = 1, D
		!	cost = cost + x(loop1)**2
		!end do
		! <- Sphere function


		! -> Hyper-Ellipsoid function
		!do loop1 = 1, D
		!	cost = cost + ( 2**(loop1 -1) ) * x(loop1)**2
		!end do
		! <- Hyper-Ellipsoid function

		! -> Generalized Rosenbrock function
		!do loop1 = 1, D-1
		!	cost = cost + 100*( x(loop1+1) - x(loop1)**2)**2 + (x(loop1) - 1)**2
		!end do
		! <- Generalized Rosenbrock function

		! -> Alpine function
		!do loop1 = 1, D
		!	cost = cost + abs( x(loop1)*sin(x(loop1)) + 0.1*x(loop1) )
		!end do
		! <- Alpine function

		! -> Griewangkfunction
		!temp1 = 0
		!temp2 = 1
		!do loop1 = 1, D
		!	temp1 = temp1 + x(loop1)**2
		!	temp2 = temp2 * cos( x(loop1) / ( (loop1)**0.5 ) )
		!end do
		!cost = temp1/4000 - temp2 + 1
		! <- Griewangkfunction



		! -> Schwefel's Ridge
		!do loop1 = 1, D
		!	do loop2 = 1, loop1
		!		temp1 = temp1 + x(loop2)
		!	end do
		!	cost = cost + temp1**2
		!end do
		! <- Schwefel's Ridge


		! -> Ackley
		do loop1 = 1, D
			temp1 = temp1 + x(loop1)**2
			temp2 = temp2 + cos(2*3.141592653589793*x(loop1))
		end do
		temp1 = (temp1 / D)**0.5
		temp2 = temp2/D
		cost = -20 * exp(-0.2 * temp1) - exp(temp2) + 20.0 + exp(1.0)
		! <- Ackley


		! -> Rastrigin
		!do loop1 = 1, D
		!	cost = cost + x(loop1)**2 - 10*cos(2*3.141592653589793*x(loop1)) + 10
		!end do
		! <- Rastrigin

		! -> Salomon
		!do loop1 = 1, D
		!	temp1 = temp1 + x(loop1)**2
		!end do
		!temp1 = temp1 ** 0.5
		!cost = -cos(2*3.141592653589793*temp1) + 0.1*temp1 + 1
		! <- Salomon


		! -> Whitley
		!do loop1 = 1, D
		!	do loop2 = 1, D
		!		temp1 = 100 * (x(loop1) - x(loop2)**2)**2 + (1-x(loop2))**2
		!		temp2 = (temp1**2)/4000 - cos(temp1) + 1
		!		cost = cost + temp2
		!	end do
		!end do
		! <- Whitley
	end subroutine fitness