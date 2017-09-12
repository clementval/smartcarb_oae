program test
  use netcdf
  use m_smartcarb_oae
  implicit none

  call init_vertical_profile_data_fields(vertical_profile_nlevel)
  call read_vertical_profile_from_file()

  print*,'SMARTCARB OAE TESTS'
  print*,layer_bot
  print*,layer_top
  print*,factor_area
  print*,factor_point



end program test
