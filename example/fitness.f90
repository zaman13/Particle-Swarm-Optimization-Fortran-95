module fitness_example
   use iso_fortran_env, only: pr => real64
   implicit none

contains
   subroutine fitness(x, cost)
      real(pr), intent(in) :: x(:)
      real(pr), intent(out) :: cost
      real(pr) :: temp1
      real(pr) :: temp2

      integer :: loop1, D

      D = size(x)

      cost = 0
      temp1 = 0
      temp2 = 0

      ! -> Ackley
      do loop1 = 1, D
         temp1 = temp1 + x(loop1)**2
         temp2 = temp2 + cos(2*3.141592653589793*x(loop1))
      end do

      temp1 = (temp1/D)**0.5
      temp2 = temp2/D
      cost = -20*exp(-0.2*temp1) - exp(temp2) + 20.0 + exp(1.0)
      ! <- Ackley
   end subroutine fitness
end module
