# Контуры
set term postscript eps size 7,10 font ",20"
set output 'images/conture.eps'
set multiplot layout 3,1

unset key
set grid
set tics out
set ticslevel 0
set hidden3d
set title "Scalogramma"
set xlabel "Time, s"
set ylabel "Period, s"
set view 20,315
splot [0:100][0:30]'tabs/S.dat' nonuniform matrix using 1:2:3 w l

set title "Sceleton"
splot [0:100][0:30]'tabs/Sc.dat' nonuniform matrix using 1:2:3 w l

set title "Above the noise"
splot [0:100][0:30]'tabs/S0.dat' nonuniform matrix using 1:2:3 w l

unset multiplot
