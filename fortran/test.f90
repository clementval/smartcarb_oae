program test
  use netcdf
  use m_smartcarb_oae
  implicit none

  call init_vertical_profile_fields(vertical_profile_nlevel)
  call read_vertical_profile_from_file()

  print*,'SMARTCARB OAE TESTS'
  print*,'-- Vertical profile'
  print*,vp_layer_bot
  print*,vp_layer_top
  print*,vp_factor_area
  print*,vp_factor_point

  ! Tempororal profile
  call init_temporal_profile_fields(100, 100)




end program test
