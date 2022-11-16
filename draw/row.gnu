# Рисуем исходный ряд и его Фурье образ
set terminal postscript eps
set output 'images/row.eps'

set multiplot layout 2, 1
set xlabel 'Time, s'
set ylabel ""
set title 'Time row'
unset key
set grid
plot 'tabs/wave.dat' using 1:2 w l

set key
set title "Schuster's Periodogram"
set xlabel 'Freq, 1/s'
set ylabel ""
plot [0:0.5] 'tabs/wave.dat' using 3:4 w l title 'signal', 'tabs/wave.dat' using 3:5 w l title 'threshold' 
unset multiplot
