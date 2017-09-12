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
  print*,'-- Tempororal profile'
  call init_temporal_profile_fields()
  print*,'  tracercat size:', tp_ntracercat
  print*,'  country size:', tp_ncountry
  call read_temporal_profile_from_file()
  print*,'tracercat',tp_tracercat
  print*,'hourofday',sum(tp_hourofday)
  print*,'dayofweek',sum(tp_dayofweek)
  print*,'monthofyear',sum(tp_monthofyear)
  print*,'hour',sum(tp_hour)
  print*,'countryID',sum(tp_countryid)


end program test
