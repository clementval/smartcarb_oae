#!/usr/bin/env python

import netCDF4
from netCDF4 import Dataset
import numpy as np

dataset = Dataset('data/temporal_profile.nc', 'w', format='NETCDF4_CLASSIC')

dim_tracercat = dataset.createDimension('tracercat', 100) # Dummy size
dim_hourofday = dataset.createDimension('hourofday', 24)
dim_dayofweek = dataset.createDimension('dayofweek', 7)
dim_monthofyear = dataset.createDimension('monthofyear', 12)
dim_hour = dataset.createDimension('hour', 8784)
dim_country = dataset.createDimension('country', 200) # Dummy size for now
dim_nchar = dataset.createDimension('nchar', 20)


v_tracercat = dataset.createVariable('tracercat', 'S1', ('tracercat', 'nchar'))
v_tracercat.long_name = 'tracer name'

# Generate tracer cat name
array_tracercat = np.empty(100, dtype='S20')
for i in range(0,100):
    array_tracercat[i] = 'Tracer Cat No ' + str(i)
array_out = netCDF4.stringtochar(array_tracercat)
v_tracercat[:] = array_out


v_hourofday = dataset.createVariable('hourofday', np.float64, ('hourofday', 'tracercat',))
v_hourofday.units = '1'
v_hourofday.long_name = 'diurnal scaling factor'

# Generate dummy data for hourofday
for h in range(0,24):
    for t in range(0,100):
        v_hourofday[h,t] = (h+1) + ((t+1)/10)

v_dayofweek = dataset.createVariable('dayofweek', np.float64, ('dayofweek', 'tracercat', 'country',))
v_dayofweek.units = '1'
v_dayofweek.long_name = 'day-of-week scaling factor'

# Generate dummy data for dayofweek
for d in range(0,7):
    for t in range(0,100):
        for c in range(0,200):
            v_dayofweek[d,t,c] = (d+1) + ((t+1) * 0.01) + ((c+1) * 0.02)

v_monthofyear = dataset.createVariable('monthofyear', np.float64, ('monthofyear', 'tracercat', 'country',))
v_monthofyear.units = '1'
v_monthofyear.long_name = 'seasonal scaling factor'
# Generate dummy data for monthofyear
for m in range(0,12):
    for t in range(0,100):
        for c in range(0,200):
            v_monthofyear[m,t,c] = (m+1) + ((t+1) * 0.01) + ((c+1) * 0.02)

v_hour = dataset.createVariable('hour', np.float64, ('hour', 'tracercat',))
v_hour.units = '1'
v_hour.long_name = 'hourly scaling factor'
# Generate dummy data for hour
for h in range(0,8784):
    for t in range(0,100):
        v_hour[h,t] = (h+1) + ((t+1) * 0.01)


v_countryID = dataset.createVariable('countryID', np.short, ('country',))
v_countryID.long_name = 'EMEP country code'

# Generate dummy countryID
for i in range(0,200):
    v_countryID[i] = i


dataset.close()
