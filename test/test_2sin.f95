program test
implicit none
integer, parameter :: mp =8
real(mp),parameter :: pi = 4*atan(1.d0)
!-------------------------------------------------------
integer n    /100/    ! Число элементо ряда
integer k             ! Счетчики
integer i             !

real(mp) dt /1.0/      ! Шаг по времени
real(mp) A  /1.0/      ! Амплитуда
real(mp) nu /0.1/      ! Частота
real(mp) nu2 /0.05/    ! Вторая частота
real(mp) phi /0.0/     ! Фаза
real(mp) u             ! Компонетны шума
real(mp) v             !
real(mp) z             ! Равномерный шум
real(mp) gam /0.5/     ! Соотношение сигнал - шум

real(mp),allocatable,dimension(:) :: t, x, noise

!------------------------------------------------------
open(1, file='t.dat')      ! Вывод временного массива
open(2, file='x.dat')      ! Вывод массива значений

! Создаем временную сетку

allocate(t(0:n-1))

do k= 0, n-1
    t(k) = k*dt
enddo

write(1, *) n
write(1, *) t


! Создаем ряд

x = A*cos(2*pi*nu*t + phi)

do i = n/2, n-1
    x(i) = x(i) +  A*cos(2*pi*nu2*t(i) + phi)
enddo

write(2, *) n
write(2, *) x


write(*,*) 'dt = ', dt
write(*,*) 'n  = ', n 
write(*,*) 'A  = ', A 
write(*,*) 'nu = ', nu 
write(*,*) 'nu2 = ', nu2 
write(*,*) 'phi=', phi 


end

