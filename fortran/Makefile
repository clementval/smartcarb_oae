all: test

data_parameters.o: ./src/data_parameters.f90
	$(FC) -I/usr/local/include/ -c -o data_parameters.o ./src/data_parameters.f90

m_smartcarb_oae.o: ./src/m_smartcarb_oae.f90 data_parameters.o
	$(FC) -I/usr/local/include/ -c -o m_smartcarb_oae.o ./src/m_smartcarb_oae.f90 -lnetcdff -lnetcdf

test: test.f90 m_smartcarb_oae.o data_parameters.o
	$(FC) -I/usr/local/include/ -o test test.f90 m_smartcarb_oae.o data_parameters.o -lnetcdff -lnetcdf

clean:
	rm -rf test *.o *.mod
