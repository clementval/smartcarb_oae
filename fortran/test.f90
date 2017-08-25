program test 
  use netcdf
  implicit none

  integer, parameter :: nlevel = 16
  character (len = *), parameter :: vertical_profile_nc = "../data/vertical_profile.nc"
  real, dimension(nlevel) :: layer_bot, layer_top, factor_area, factor_point
  integer :: ncid, varid

  call nf90_open(vertical_profile_nc, NF90_NOWRITE, ncid)
  call nf90_inq_varid(ncid, "layer_bot", varid)
  call nf90_get_var(ncid, varid, layer_bot)
  call nf90_inq_varid(ncid, "layer_top", varid)
  call nf90_get_var(ncid, varid, layer_bot)
  call nf90_inq_varid(ncid, "factor_area", varid)
  call nf90_get_var(ncid, varid, layer_bot)
  call nf90_inq_varid(ncid, "factor_point", varid)
  call nf90_get_var(ncid, varid, layer_bot)

  print*,'SMARTCARB OAE TESTS'


end program test
