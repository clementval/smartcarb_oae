all: test

mo_kind.o: ./src/mo_kind.f90
	$(FC) -I/usr/local/include/ -c -o mo_kind.o ./src/mo_kind.f90

m_smartcarb_oae.o: ./src/m_smartcarb_oae.f90 mo_kind.o
	$(FC) -I/usr/local/include/ -c -o m_smartcarb_oae.o ./src/m_smartcarb_oae.f90 -lnetcdff -lnetcdf

test: test.f90 m_smartcarb_oae.o mo_kind.o
	$(FC) -I/usr/local/include/ -o test test.f90 m_smartcarb_oae.o mo_kind.o -lnetcdff -lnetcdf

clean:
	rm -rf test *.o *.mod
