all: test

m_smartcarb_oae.o: ./src/m_smartcarb_oae.f90
	gfortran -I/usr/local/include/ -c -o m_smartcarb_oae.o ./src/m_smartcarb_oae.f90 -lnetcdff -lnetcdf

test: test.f90 m_smartcarb_oae.o
	gfortran -I/usr/local/include/ -o test test.f90 m_smartcarb_oae.o -lnetcdff -lnetcdf

clean: 
	rm -rf test
