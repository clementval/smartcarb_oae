#!/usr/bin/env python

from netCDF4 import Dataset
import numpy as np

dataset = Dataset('data/emissions.nc', 'w', format='NETCDF4_CLASSIC')

dim_rlon = dataset.createDimension('rlon', 700) # Dummy size
dim_rlat = dataset.createDimension('rlat', 800) # Dummy size

v_rlon = dataset.createVariable('rlon', np.float64, ('rlon',))
v_rlon.axis = 'X'
v_rlon.units = 'degrees'
v_rlon.standard_name = 'longitude'

v_rlat = dataset.createVariable('rlat', np.float64, ('rlat',))
v_rlat.axis = 'Y'
v_rlat.units = 'degrees'
v_rlat.standard_name = 'latitude'

X1_01_area = dataset.createVariable('X1_01_area', np.float64, ('rlon', 'rlat',))
X1_01_area.units = 'kg h-1 cell-1'

for x in range(0, 700):
    for y in range(0, 800):
        X1_01_area[x,y] = 0.1

X1_01_point = dataset.createVariable('X1_01_point', np.float64, ('rlon', 'rlat',))
X1_01_point.fill_value = -9999.0
X1_01_point.units = 'kg h-1 cell-1'
for x in range(0, 700):
    for y in range(0, 800):
        X1_01_point[x,y] = 0.1

X1_02_area = dataset.createVariable('X1_02_area', np.float64, ('rlon', 'rlat',))
X1_02_area.fill_value = -9999.0
X1_02_area.units = 'kg h-1 cell-1'
for x in range(0, 700):
    for y in range(0, 800):
        X1_02_area[x,y] = 0.1

X1_02_point = dataset.createVariable('X1_02_point', np.float64, ('rlon', 'rlat',))
X1_02_point.fill_value = -9999.0
X1_02_point.units = 'kg h-1 cell-1'
for x in range(0, 700):
    for y in range(0, 800):
        X1_02_point[x,y] = 0.1

dataset.close()
