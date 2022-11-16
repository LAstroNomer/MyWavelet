module kernels
use my_prec
implicit none

contains
!----------------------------------------------------------------------

function mhat(t) result(psi)
! Данная функция возвращает MHAT-вейвлет в точке t
! t   - аргумент
! res - результат
use my_prec

real(mp) t, psi

psi = (1 - t**2) * exp(-t**2/2.d0)

end function
!----------------------------------------------------------------------

function mhat_arr(t) result(psi)
! Данная функция возвращает MHAT-вейвлет в точке t
! t   - аргумент массив
! res - результат массив

use my_prec
real(mp) t(:)
real(mp), allocatable, dimension(:) :: psi(:)
allocate(psi(0: size(t)-1))

psi = (1 - t**2) * exp(-t**2/2.d0)

end function 
!----------------------------------------------------------------------

function f_mhat_arr(omega) result(psi)
use my_prec

! Данная функция возвращает фурье образ MHAT-вейвлета
! omega - аргумент массив 
! psi   - результат массив

real(mp) omega(:)
real(mp), allocatable, dimension(:) :: psi(:)
allocate(psi(0: size(omega)-1))

psi = sqrt(2.d0 * pi) * omega**2 * exp(-omega**2/2.d0)
end function
!----------------------------------------------------------------------

function morle_arr(t) result(psi)
! Данная функция возвращает вейвлет Морле
! t   - аргумент массив
! psi - результат массив комплексный
! k0   = 2*pi
!alha  = sqrt(2.d0)

use my_prec
real(mp) t(:), alpha, k0
complex(mp), allocatable, dimension(:) :: psi(:)
allocate(psi(0: size(t)-1))

k0   = 2.d0*pi
alpha = sqrt(2.d0)
psi = exp(-t**2/alpha**2) * (exp(j * k0 * t) - exp(-k0**2 * alpha**2/4.d0))
end function
!----------------------------------------------------------------------

function f_morle_arr(omega, alpha, k0) result(psi)
! Данная функция возвращает фурье образ вейвлета Морле
! t   - аргумент массив
! psi - результат массив комплексный

use my_prec
real(mp) omega(:), alpha, k0
complex(mp), allocatable, dimension(:) :: psi(:)
allocate(psi(0: size(omega)-1))

psi = alpha*sqrt(pi) * (exp(-alpha**2*(k0 - omega)**2/4.d0) - exp(-alpha**2*(k0 + omega)**2/4.d0))
end function
!----------------------------------------------------------------------

function dft(x) result(res)
use my_prec

integer i, n, k
real(mp) x(:)
complex(mp), allocatable, dimension(:) :: res(:)
complex(mp), allocatable, dimension(:,:) :: e
n = size(x)
allocate(e(0:n-1,0:n-1), res(0:n-1))
forall (i=0:n-1, k=0:n-1) e(i,k) = -j * 2 * pi * k * i / n
 
res =  matmul(exp(e), x)

end function
!----------------------------------------------------------------------
end module
