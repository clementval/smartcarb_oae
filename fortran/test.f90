program test
  use netcdf
  use m_smartcarb_oae
  implicit none

  call read_vertical_profile()

  print*,'SMARTCARB OAE TESTS'
  print*,layer_bot
  print*,layer_top
  print*,factor_area
  print*,factor_point



end program test
