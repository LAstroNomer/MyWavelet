module time_analise
use my_prec
use kernels
implicit none

contains
!----------------------------------------------------------------------
subroutine wavelet(t, x, amin, da, na, bmin, db, nb, func, B0, wav)
    use my_prec

    interface
        function func(t) result(psi); use my_prec; real(mp) t(:); complex(mp), allocatable, dimension(:) :: psi(:); end function
    end interface


    integer     na
    integer     nb
    integer     i
    integer     k

    real(mp)     t(:)   !
    real(mp)     x(:)   ! 
    real(mp)     da     !
    real(mp)     db     !
    real(mp)     B0     !
    real(mp)     ai     !
    real(mp)     bi     !
    real(mp)     amin   !
    real(mp)     bmin   !
    real(mp)     alpha  !
    real(mp)     n      !

    complex(mp),allocatable, dimension(:) :: psi
    complex(mp),allocatable, dimension(:,:) :: wav


    allocate(psi(0 : size(t)-1), wav(0: na-1, 0 : nb-1))
    wav = 0
    alpha = sqrt(2.d0)

    do i = 1, na-1
        ai = amin + i*da
        do k=0, nb-1
            bi = bmin + db*k
            psi = func((t-bi)/ai)
            n = sum(exp(-((t-bi)/ai)**2/B0))
            wav(i, k) = sum(x * conjg(psi))/n
        enddo
     enddo
end subroutine
!----------------------------------------------------------------------

function z_arr(t, ai, bi, B0) 
    use my_prec
    implicit none
    real(mp) t(:), ai, bi, B0, z_arr, n

    n = sum(exp(-((t-bi)/ai)**2/B0))
    z_arr = sum(exp(-((t-bi)/ai)**2))/n**2

end function 
!----------------------------------------------------------------------
end module
