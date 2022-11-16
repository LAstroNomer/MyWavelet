dir=bin

gf = gfortran
filelist = my_prec.f95 kernels.f95 printers.f95 time_analise.f95 main.f95


files = $(addprefix $(dir)/,$(filelist))

run: built
		./wavelet
	
built: objs
		$(gf) *.o -o wavelet

objs: $(files)
	$(gf) -c $(files) 

clean:
		rm -f *.o *.mod

clean_all: 
		rm -f *.dat *.o *.mod wavelet

plot:
		gnuplot draw/*.gnu

test_sin: built 
		$(gf) test/test.f95 -o test/test_sin
		./test/test_sin
		./wavelet
		gnuplot draw/*.gnu
		
test_movesin: built 
		$(gf) test/test_movesin.f95 -o test/test_move
		./test/test_move
		./wavelet
		gnuplot draw/*.gnu
		

test_2sin: built
		$(gf) test/test_2sin.f95 -o test/test_2sin
		./test/test_2sin
		./wavelet
		gnuplot draw/*.gnu
		
test_switch: built
		$(gf) test/test_switch.f95 -o test/test_switch
		./test/test_switch
		./wavelet
		gnuplot draw/*.gnu
		
test_auto: built 
		$(gf) test/test_auto.f95 -o test/auto
		./test/auto
		./wavelet
		gnuplot draw/*.gnu

ps2pdf:
		ps2pdf images/conture.eps images/conture.pdf
		ps2pdf images/row.eps images/row.pdf
		ps2pdf images/scal.eps images/scal.pdf
		ps2pdf images/maps.eps images/maps.pdf
