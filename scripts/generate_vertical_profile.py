#!/usr/bin/env python

from netCDF4 import Dataset
import numpy as np

dataset = Dataset('data/vertical_profile.nc', 'w', format='NETCDF4_CLASSIC')

dim_category = dataset.createDimension('category', 10)
dim_level = dataset.createDimension('level', 16)
dim_type = dataset.createDimension('type', 2)

layer_bot = dataset.createVariable('layer_bot', np.float64, ('level',))
layer_bot.units = 'm'
layer_bot.long_name = ''

layer_top = dataset.createVariable('layer_top', np.float64, ('level',))
layer_top.units = 'm'
layer_top.long_name = ''

factor_area = dataset.createVariable('factor_area', np.float64, ('level',))
factor_area.units = '1'
factor_area.long_name = ''

factor_points = dataset.createVariable('factor_point', np.float64, ('level',))
factor_points.units = '1'
factor_points.long_name = ''

dataset.close()

