program wavelet_analise
    ! Данная программа реализует алгоритм вейвлет анализа временного ряда
    ! Пока существует один режим работы программы:
    !       0:: Анализ при помощи вейвлета Морле с alpha^2 = 2; k0 = 2pi
    ! Учёт тренда в данной программе не предуспотрен.
    ! На вход подается два массива: 
    !       1) отсчетов по времени t_k
    !       2) значение x_k=x(t_k)
    ! На выходе в файл wave.dat выгружатся таблица полученных результатов
    !
    ! P.S. Неинициализированные переменные назначаю 0, чтобы избежать заполнение мусором
    !--------------------------------------------------------------------
    use my_prec             ! Константы 
    use kernels             ! Ядра вейвлетов
    use time_analise        ! Модуль функций для анализа временных рядов
    use printer             ! Модуль для отображения результатов
    implicit none

    !--------------------------------------------------------------------

    character(20)   ft  /'t.dat'/      ! Файл с массивом t   
    character(20)   fx  /'x.dat'/      ! Файл с массивом x   

    character(20)   fo  /'tabs/wave.dat'/    ! Файл записи исходных данных  
    character(20)   fs  /'tabs/S.dat' /      ! Файл записи Скалограммы
    character(20)   fg  /'tabs/g.dat' /      ! Файл записи Скейлограммы
    character(20)   fs0 /'tabs/S0.dat'/      ! Файл записи Скаллограммы над шумом
    character(20)   fsc /'tabs/Sc.dat'/      ! Файл записи Скелетона

    integer, parameter ::    num_ft  = 1     ! Номер файла ft
    integer, parameter ::    num_fx  = 2     ! Номер файла fx
    integer, parameter ::    num_fo  = 3     ! Номер файла fo
    integer, parameter ::    num_fs  = 4     ! Номер файла fs
    integer, parameter ::    num_fg  = 5     ! Номер файла fg
    integer, parameter ::    num_fs0 = 6     ! Номер файла fs0
    integer, parameter ::    num_fsc = 7     ! Номер файла fsc

    integer    na   / 100 /         ! Число точек параметра a
    integer    nb   / 100 /         ! Число точек параметра b

    integer    size_t  /0/          ! Берется из файла ft
    integer    size_x  /0/          ! Берется из файла fx

    integer    i, k    /0/          ! Переменные обхода цикла
    integer    ier     /0/          ! Ключ состояния при инициализации массивов

    real(mp)   alpha  /0.d0/         ! Параметры вейвлета Морле
    real(mp)   B0     /0.d0/         ! 
    real(mp)   q     / 0.005d0   /   ! Уровень значимости
    real(mp)   s02     /0.d0/        ! Исправленная дисперсия 
    real(mp)   dt      /0.d0/        ! Шаг по времени
    real(mp)   amin    /0.d0/        ! a --- частотная ось вейвлета
    real(mp)   amax    /0.d0/        ! 
    real(mp)   da      /0.d0/        !
    real(mp)   ai      /0.d0/        !
    real(mp)   bmin    /0.d0/        ! b --- расположение вейвлета 
    real(mp)   bmax    /0.d0/        ! на временной оси
    real(mp)   db      /0.d0/        !       
    real(mp)   bi      /0.d0/        !
    real(mp)   z       /0.d0/        ! Оценка шума 

    real(mp),    allocatable,dimension(:)   :: t      ! Временые отсчеты
    real(mp),    allocatable,dimension(:)   :: x      ! Значение ряда
    real(mp),    allocatable,dimension(:)   :: nu     ! Частоты
    real(mp),    allocatable,dimension(:)   :: xq     ! Порог сигнал/шум
    complex(mp), allocatable,dimension(:)   :: psi    ! Вейвлет
    complex(mp), allocatable,dimension(:)   :: dftx   ! Фурье преобразование
    complex(mp), allocatable,dimension(:)   :: g      ! Скейлограмма
    complex(mp), allocatable,dimension(:,:) :: wav    ! Вейвлет преобразование
    real(mp),    allocatable,dimension(:,:) :: sc     ! Скелетон

    !---------------------------------------------------------------------

    open(num_ft, file=ft)       
    read(num_ft, *) size_t

    open(num_fx, file=fx)
    read(num_fx, *) size_x

    if (size_t .ne. size_x) then
    stop 'Размеры входных массивов не совпадают!'
    endif

    allocate(t(0: (size_t-1)), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив t"
    read(num_ft, *) t

    allocate(x(0: (size_x-1)), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив x"
    read(num_fx, *) x

    allocate(psi(0:size_x-1), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив psi"
    psi = 0

    allocate(dftx(0:size_x-1), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив dftx"
    dftx = 0

    allocate(nu(0:size_x-1), stat=ier) 
    if (ier.ne.0) stop "Не могу выделить массив nu"
    nu = 0

    allocate(xq(0:size_x-1), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив xq"
    xq = 0

    !-----------------------------------------------------------------------

    ! Вычтем среднее
    x = x - sum(x)/size_x

    !Оценка дисперсии ряда, испавленная
    s02 = sum(x**2)/(size_x - 1)

    ! Построеие периодограммы Шустера при помощи
    ! Дискретного Фурье преобразования (не ускоренного)

    dftx = dft(x)
    dftx = (real(dftx(0:size_x-1))**2 + aimag(dftx(0:size_x-1))**2)/size_x**2

    dt = t(1) - t(0) 
    do i=0,size_x-1
    nu(i) = i/real(size_x)/dt
    enddo

    ! Порог шума
    xq = -log(1.d0 - (1.d0 - q)**(2/(real(size_x)-2)))*sqrt(s02)/real(size_x)

    call print_table_xy(t, x, nu, real(dftx), xq, fo)

    deallocate(nu, stat=ier)
    deallocate(dftx, stat=ier)
    deallocate(xq, stat=ier)
    if (ier.ne.0) write(*,*) "Не удалось отчистить память"
    !------------------------------------------------------------------------

    ! Вычисление вейвлет-преобразования
    alpha = sqrt(2.d0)
    B0 = alpha**2

    amin = 2*dt/alpha
    amax = (real(size_x)-1)*dt/alpha
    da = (amax - amin)/(na-1)
    
    bmin = 0
    bmax = (real(size_x)-1)*dt
    db = (bmax-bmin)/(nb-1)

    allocate(wav(0 : na-1, 0 : nb - 1), stat=ier)
    call wavelet(t, x,amin, da, na,bmin, db, nb, morle_arr, alpha, B0, wav)

    ! Скалограмма
    wav = abs(wav)**2
    call print_data(real(wav), amin, na, da, bmin, nb, db, fs)


    ! Скейлограмма
    allocate(g(0:na-1), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив g"

    open(num_fg, file=fg)
    do i = 0, na-1
        ai = amin + i*da
        z = z_arr(t, ai, bmax/2, B0)
        g(i) = sum(real(wav(i, :))) / nb
        write(num_fg,*) ai, real(g(i)), -s02*z*log(q)
    enddo

    deallocate(g, stat=ier)

    ! Скелетон
    allocate(sc(0:na-1, 0:nb-1), stat=ier)
    if (ier.ne.0) stop "Не могу выделить массив sc"

    sc=0
    do i=1,na-2
        do k=1,nb-2
            if (real(wav(i,k)) > max(real(wav(i-1,k)), real(wav(i+1,k)))) then
                sc(i, k) = real(wav(i,k))
            endif
            if (real(wav(i,k)) > max(real(wav(i,k-1)), real(wav(i,k+1)))) then
                sc(i, k) = real(wav(i,k))
            endif
        enddo
    enddo

    call print_data(sc, amin, na, da, bmin, nb, db, fsc)


    ! Выделение сигнала из данных

    do i=0, na-1
        ai = amin + i*da
        do k = 0, nb-1
            bi = bmin + k*db
            z = z_arr(t, ai, bi, B0)
            if (Sc(i,k) < -s02*z*log(q)) then
                Sc(i,k) = 0
            endif
        enddo
    enddo


call print_data(Sc, amin, na, da, bmin, nb, db, fs0)

end
