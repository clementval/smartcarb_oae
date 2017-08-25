!
!
!


MODULE m_smartcarb_oae
  USE netcdf

  IMPLICIT NONE

  ! Constant variable
  INTEGER, PARAMETER :: nlevel = 16

  character (len = *), parameter :: vertical_profile_nc = "../data/vertical_profile.nc"

  ! Vertical profile values
  REAL, DIMENSION(nlevel) :: &
    layer_bot,               &
    layer_top,               &
    factor_area,             &
    factor_point

CONTAINS

  SUBROUTINE read_vertical_profile()
    INTEGER :: ncid, varid, err_status

    CALL ncdf_call_and_check_status(nf90_open(vertical_profile_nc, NF90_NOWRITE, ncid))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "layer_bot", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, layer_bot))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "layer_top", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, layer_top))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "factor_area", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, factor_area))
    CALL ncdf_call_and_check_status(nf90_inq_varid(ncid, "factor_point", varid))
    CALL ncdf_call_and_check_status(nf90_get_var(ncid, varid, factor_point))
    CALL ncdf_call_and_check_status(nf90_close(ncid))
  END SUBROUTINE read_vertical_profile

  SUBROUTINE ncdf_call_and_check_status(status)
    INTEGER, INTENT(IN) :: status

    IF (status /= nf90_noerr) THEN
      print *, trim(nf90_strerror(status))
      stop "NetCDF error"
    END IF
  END SUBROUTINE ncdf_call_and_check_status

  SUBROUTINE read_temporal_profile()

  END SUBROUTINE read_temporal_profile

  SUBROUTINE read_gridded_emissions()

  END SUBROUTINE read_gridded_emissions


END MODULE m_smartcarb_oae
