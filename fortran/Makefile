all: test

test: test.f90
	gfortran -I/usr/local/include/ -o test test.f90 -lnetcdf

clean: 
	rm -rf test
