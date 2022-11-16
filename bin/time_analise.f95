module time_analise
use my_prec
use kernels
implicit none

contains
!----------------------------------------------------------------------
subroutine wavelet(t, x, amin, da, na, bmin, db, nb, func, alpha, B0, wav)
    use my_prec

    interface
        function func(t) result(psi); use my_prec; real(mp) t(:); complex(mp), allocatable, dimension(:) :: psi(:); end function
    end interface


    integer     na
    integer     nb
    integer     i   /0/
    integer     k   /0/
    integer     ier /0/ 

    real(mp)     t(:)           ! Временная сетка
    real(mp)     x(:)           ! Значения ряда
    real(mp)     da             ! Шаг параметра а
    real(mp)     db             ! Шаг параметра b
    real(mp)     ai             ! текущее занчение
    real(mp)     bi             ! текущее значение
    real(mp)     amin           ! 
    real(mp)     bmin           !
    real(mp)     alpha          ! Параметр вейвлета Морле
    real(mp)     B0             ! Параметр вейвлета Морле
    real(mp)     n      /0.d0/  ! Коэф-т вейвлета

    complex(mp),allocatable, dimension(:) :: psi
    complex(mp)  wav(0: na-1, 0:nb-1)


    allocate(psi(0 : size(t)-1), stat=ier)
    psi = 0
    wav = 0

    do i = 1, na-1          ! i=0 сильно шумит ???
        ai = amin + i*da
        do k = 0, nb-1
            bi = bmin + db*k
            psi = func((t-bi)/ai)
            n = sum(exp(-((t-bi)/ai)**2/B0))
            wav(i, k) = sum(x * conjg(psi))/n
        enddo
     enddo
end subroutine
!----------------------------------------------------------------------

function z_arr(t, ai, bi, B0)
    ! Функция расчета порогового значения Z
    use my_prec
    implicit none
    real(mp) t(:), ai, bi, B0, z_arr, n

    n = sum(exp(-((t-bi)/ai)**2/B0))
    z_arr = sum(exp(-((t-bi)/ai)**2))/n**2

end function 
!----------------------------------------------------------------------
end module
