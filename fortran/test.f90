program test
  use netcdf
  use m_smartcarb_oae
  implicit none

  integer :: ierror = 0
  character(len=200) :: errmsg

  CALL oae_init(ierror, errmsg)

  if (ierror /= 0) then
    print*,errmsg
    stop
  end if

  print*,'SMARTCARB OAE TESTS'

  ! Vertical profile
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


  print*,'Index X1_01_area', get_gridded_emissions_idx('X1_01_area')
  print*,'Index X1_01_point', get_gridded_emissions_idx('X1_01_point')
  print*,'Index X1_02_area', get_gridded_emissions_idx('X1_02_area')
  print*,'Index X1_02_point', get_gridded_emissions_idx('X1_02_point')
  print*,'Index Dummy', get_gridded_emissions_idx('Dummy')

  CALL oae_cleanup()

end program test
