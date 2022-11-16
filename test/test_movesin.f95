program test
implicit none
integer, parameter :: mp =8
real(mp),parameter :: pi = 4*atan(1.d0)
!-------------------------------------------------------
integer(8) n    ! Число элементо ряда
integer(8) k    ! Счетчики
integer(8) i    !

real(mp) dt     ! Шаг по времени
real(mp) q      ! Уровень значимости
real(mp) A      ! Амплитуда
real(mp) nu     ! Частота
real(mp) phi    ! Фаза
real(mp) u      ! Компонетны шума
real(mp) v      !
real(mp) z      ! Равномерный шум
real(mp) gam    ! Соотношение сигнал - шум

real(mp),allocatable,dimension(:) :: t, x, noise, movenu

!------------------------------------------------------
open(1, file='t.dat')      ! Вывод временного массива
open(2, file='x.dat')      ! Вывод массива значений
open(3, file='test/input')      ! Входные параметры

!------------------------------------------------------

! Задание исходных парметров
read(3,*) dt, n, q, A, nu, phi, gam

write(*,*) 'dt = ', dt
write(*,*) 'n  = ', n 
write(*,*) 'q  = ', q 
write(*,*) 'A  = ', A 
write(*,*) 'nu = ', nu 
write(*,*) 'phi=', phi 
write(*,*) 'gam=', gam

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
    !noise(i+1) = sqrt(-2*log(u))*sin(2*pi*v)
    i = i + 1
enddo

allocate(movenu(0:n-1))


do i = 0, n-1
    movenu(i) = nu + i*nu/n
enddo

! Создаем ряд
x = A*cos(2*pi*movenu*t + phi) !+ noise *  sqrt(A**2/2/gam)

write(2, *) n
write(2, *) x

end

