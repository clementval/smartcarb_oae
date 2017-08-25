#!/usr/bin/env python

from netCDF4 import Dataset
import numpy as np

dataset = Dataset('data/temporal_profile.nc', 'w', format='NETCDF4_CLASSIC')

dim_tracercat = dataset.createDimension('tracercat', None)
dim_hourofday = dataset.createDimension('hourofday', 24)
dim_dayofweek = dataset.createDimension('dayofweek', 7)
dim_monthofyear = dataset.createDimension('monthofyear', 12)
dim_hour = dataset.createDimension('hour', 8784)
dim_country = dataset.createDimension('country', 200) # Dummy size for now
dim_nchar = dataset.createDimension('nchar', 20)


v_tracercat = dataset.createVariable('tracercat', 'S1', ('tracercat', 'nchar',))
v_tracercat.long_name = 'tracer name'

v_hourofday = dataset.createVariable('hourofday', np.float64, ('hourofday', 'tracercat',))
v_hourofday.units = '1'
v_hourofday.long_name = 'diurnal scaling factor'

v_dayofweek = dataset.createVariable('dayofweek', np.float64, ('dayofweek', 'tracercat', 'country',))
v_dayofweek.units = '1'
v_dayofweek.long_name = 'day-of-week scaling factor'

v_monthofyear = dataset.createVariable('monthofyear', np.float64, ('monthofyear', 'tracercat', 'country',))
v_monthofyear.units = '1'
v_monthofyear.long_name = 'seasonal scaling factor'

v_hour = dataset.createVariable('hour', np.float64, ('hour', 'tracercat',))
v_hour.units = '1'
v_hour.long_name = 'hourly scaling factor'

v_countryID = dataset.createVariable('countryID', np.short, ('country',))
v_countryID.long_name = 'EMEP country code'

dataset.close()
