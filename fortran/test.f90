program test 
  use netcdf
  implicit none

  integer, parameter :: nlevel = 16
  character (len = *), parameter :: vertical_profile_nc = "../data/vertical_profile.nc"
  real, dimension(nlevel) :: layer_bot, layer_top, factor_area, factor_point
  integer :: ncid, varid, err_status

  layer_bot(:) = 0.0
  layer_top(:) = 0.0
  factor_area(:) = 0.0
  factor_point(:) = 0.0


  err_status =  nf90_open(vertical_profile_nc, NF90_NOWRITE, ncid)

  err_status =  nf90_inq_varid(ncid, "layer_bot", varid)
  err_status =  nf90_get_var(ncid, varid, layer_bot)

  err_status =  nf90_inq_varid(ncid, "layer_top", varid)
  err_status =  nf90_get_var(ncid, varid, layer_top)

  err_status =  nf90_inq_varid(ncid, "factor_area", varid)
  err_status =  nf90_get_var(ncid, varid, factor_area)

  err_status =  nf90_inq_varid(ncid, "factor_point", varid)
  err_status =  nf90_get_var(ncid, varid, factor_point)

  err_status =  nf90_close(ncid)

  print*,'SMARTCARB OAE TESTS'
  print*,layer_bot
  print*,layer_top
  print*,factor_area
  print*,factor_point



end program test
