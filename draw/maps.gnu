# Построение карт

set terminal postscript eps size 7,10 font ",20" 
set output 'images/maps.eps'

set multiplot layout 3,1
set view map
unset key
set title 'Scalogram'
set palette
set xlabel "Time, s"
set ylabel "Period, s"
splot  'tabs/S.dat' nonuniform matrix using 1:2:3 w pm3d
set title 'Sceleton'
splot  'tabs/Sc.dat' nonuniform matrix using 1:2:3 w pm3d
set title 'Above the noise'
splot  'tabs/S0.dat' nonuniform matrix using 1:2:3 w pm3d
unset multiplot 


