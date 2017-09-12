program test
  use netcdf
  use m_smartcarb_oae
  implicit none


  CALL oae_init()

  print*,'SMARTCARB OAE TESTS'
  print*,'-- Vertical profile'
  print*,vp_layer_bot
  print*,vp_layer_top
  print*,vp_factor_area
  print*,vp_factor_point

  ! Tempororal profile
  print*,'-- Tempororal profile'
  print*,'  tracercat size:', tp_ntracercat
  print*,'  country size:', tp_ncountry
  print*,'tracercat',tp_tracercat
  print*,'hourofday',sum(tp_hourofday)
  print*,'dayofweek',sum(tp_dayofweek)
  print*,'monthofyear',sum(tp_monthofyear)
  print*,'hour',sum(tp_hour)
  print*,'countryID',sum(tp_countryid)


  CALL oae_cleanup()

end program test
