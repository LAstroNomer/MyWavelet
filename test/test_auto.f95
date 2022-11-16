program test
implicit none
integer, parameter :: mp =8
real(mp),parameter :: pi = 4*atan(1.d0)
!-------------------------------------------------------
integer(8) n /100/    ! Число элементо ряда
integer(8) k    ! Счетчики
integer(8) i    !

real(mp) dt /1.0/     ! Шаг по времени
real(mp) u      ! Компонетны шума
real(mp) v      !
real(mp) z      ! Равномерный шум

real(mp),allocatable,dimension(:) :: t, x, noise

!------------------------------------------------------
open(1, file='t.dat')      ! Вывод временного массива
open(2, file='x.dat')      ! Вывод массива значений

!-------------------------------------------------------

! Создаем временную сетку

allocate(t(0:n-1))

do k= 0, n-1
    t(k) = k*dt
enddo

write(1, *) n
write(1, *) t


allocate(x(0:n-1), noise(0:n-1))

! Cоздаем гауссов шум из равномерного
i=0
do while (i < n)
    call random_number(u)
    call random_number(v)
    noise(i)   = sqrt(-2*log(u))*cos(2*pi*v) 
    noise(i+1) = sqrt(-2*log(u))*sin(2*pi*v)
    i = i + 2
enddo

! Создаем ряд
x(0) = 0
do i = 2, n-1
    x(i) = x(i-1) - 0.4*x(i-2) + noise(i)
enddo

write(2, *) n
write(2, *) x
write(*,*) sum(noise)/n
write(*,*) sum((noise - sum(noise)/n)**2)/n-1
end

