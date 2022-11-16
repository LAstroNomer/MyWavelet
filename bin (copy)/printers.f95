module printer
use my_prec
implicit none

contains
!---------------------------------------------------------
subroutine print_table_xy(t, x, nu, dft, xq, fname)
    use my_prec
    real(mp) t(:), x(:), nu(:), dft(:), xq(:)
    integer i
    character(20) fname
    open(10, file=fname(1:len_trim(fname)))
    do i = 1, size(t)
        write(10,*) t(i), x(i), nu(i), dft(i), xq(i)
    enddo
end subroutine
!--------------------------------------------------------

subroutine print_data(data_, amin, na, da, bmin,  nb, db, fname)
    use my_prec

    character(20)  fname
    integer, parameter :: file_number = 1

    integer na
    integer nb
    integer i


    real(mp) amin
    real(mp) bmin
    real(mp) da
    real(mp) db
    real(mp) data_(0:na-1, 0:nb-1)


    open(file_number, file=fname)
    open(file_number, file=fname)

    write(file_number, fmt='(i10, 1x)', advance='no') max(na, nb)
    do  i = 0, nb-1
        write(file_number, fmt='(f10.2, 1x)', advance='no') bmin + i*db
    enddo

    write(file_number,*)

    do  i = 0, na-1
        write(file_number,*) amin+i*da, real(data_(i,:))
    enddo
end subroutine
end module     
