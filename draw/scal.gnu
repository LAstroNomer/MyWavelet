set terminal postscript eps
set output 'images/scal.eps'

set multiplot layout 1,1
set title 'Scalogram'
unset key
set xlabel "Period, s"
set ylabel ""
plot  [2:20] [0:1] 'tabs/g.dat' u 1:2 w l, 'tabs/g.dat' u 1:3 w l
unset multiplot
