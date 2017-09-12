!
!
!


MODULE m_smartcarb_oae
  USE netcdf

  IMPLICIT NONE

  ! Constant variable
  ! May be change to NAMELIST parameter if needed by the implementation
  INTEGER, PARAMETER :: vertical_profile_nlevel = 16

  INTEGER, PARAMETER :: tp_param_hourofday = 24
  INTEGER, PARAMETER :: tp_param_dayofweek = 7
  INTEGER, PARAMETER :: tp_param_monthofyear = 12
  INTEGER, PARAMETER :: tp_param_hour = 8784

  CHARACTER (len = *), PARAMETER :: vertical_profile_nc = "../data/vertical_profile.nc"
  CHARACTER (len = *), PARAMETER :: temporal_profile_nc = "../data/temporal_profile.nc"

  ! Vertical profile array
  REAL, DIMENSION(:), ALLOCATABLE :: &
    vp_layer_bot,                       &
    vp_layer_top,                       &
    vp_factor_area,                     &
    vp_factor_point


  ! Tempororal profile arrays
  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: &
    tp_tracercat

  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
    tp_dayofweek,                        &
    tp_monthofyear

  REAL, DIMENSION(:,:), ALLOCATABLE :: &
    tp_hourofday,                      &
    tp_hour

  INTEGER, DIMENSION(:), ALLOCATABLE :: &
    tp_countryid



  ! Tenporal profile
  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: tracercat

CONTAINS

  ! Allocate the data fields necessary for the vertical profile
  SUBROUTINE init_vertical_profile_fields(nlevel)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nlevel ! Number of level in the vertical profile

    ALLOCATE(vp_layer_bot(nlevel))
    ALLOCATE(vp_layer_top(nlevel))
    ALLOCATE(vp_factor_area(nlevel))
    ALLOCATE(vp_factor_point(nlevel))
  END SUBROUTINE init_vertical_profile_fields

  ! Read the vertical profile from NetCDF file and store them into their
  ! corresponding arrays.
  SUBROUTINE read_vertical_profile_from_file()
    IMPLICIT NONE
    INTEGER :: ncid, varid, err_status

    CALL ncdf_call_and_check_status(nf90_open(vertical_profile_nc, NF90_NOWRITE, ncid))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "layer_bot", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, vp_layer_bot))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "layer_top", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, vp_layer_top))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "factor_area", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, vp_factor_area))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "factor_point", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, vp_factor_point))
    CALL ncdf_call_and_check_status(nf90_close(ncid))

  END SUBROUTINE read_vertical_profile_from_file



  SUBROUTINE ncdf_call_and_check_status(status)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: status

    IF (status /= nf90_noerr) THEN
      print *, trim(nf90_strerror(status))
      stop "NetCDF error"
    END IF
  END SUBROUTINE ncdf_call_and_check_status

  ! Read all the temporal profile for the Nx different tracers

  SUBROUTINE init_temporal_profile_fields(ntracercat, ncountry)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ntracercat, ncountry

    ALLOCATE(tp_tracercat(ntracercat))
    ALLOCATE(tp_hourofday(tp_param_hourofday, ntracercat))
    ALLOCATE(tp_dayofweek(tp_param_dayofweek, ntracercat, ncountry))
    ALLOCATE(tp_monthofyear(tp_param_monthofyear, ntracercat, ncountry))
    ALLOCATE(tp_hour(tp_param_hour, ntracercat))
    ALLOCATE(tp_countryid(ncountry))
  END SUBROUTINE init_temporal_profile_fields

  SUBROUTINE read_temporal_profile_from_file()
    IMPLICIT NONE

  END SUBROUTINE read_temporal_profile_from_file



  SUBROUTINE read_gridded_emissions()
  END SUBROUTINE read_gridded_emissions

END MODULE m_smartcarb_oae
