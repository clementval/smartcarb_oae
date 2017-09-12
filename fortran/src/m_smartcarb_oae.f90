!
!
!


MODULE m_smartcarb_oae
  USE netcdf

  IMPLICIT NONE

  ! Constant variable
  ! May be change to NAMELIST parameter if needed by the implementation
  INTEGER, PARAMETER :: vertical_profile_nlevel = 16


  character (len = *), parameter :: vertical_profile_nc = "../data/vertical_profile.nc"

  ! Vertical profile values
  REAL, DIMENSION(:), ALLOCATABLE :: &
    layer_bot,                       &
    layer_top,                       &
    factor_area,                     &
    factor_point

  ! Tenporal profile
  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: tracercat

CONTAINS

  ! Allocate the data fields necessary for the vertical profile
  SUBROUTINE init_vertical_profile_data_fields(nlevel)
    INTEGER, INTENT(IN) :: nlevel ! Number of level in the vertical profile

    ALLOCATE(layer_bot(nlevel))
    ALLOCATE(layer_top(nlevel))
    ALLOCATE(factor_area(nlevel))
    ALLOCATE(factor_point(nlevel))
  END SUBROUTINE init_vertical_profile_data_fields

  ! Read the vertical profile from NetCDF file and store them into their
  ! corresponding arrays.
  SUBROUTINE read_vertical_profile_from_file()
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

  END SUBROUTINE read_vertical_profile_from_file



  SUBROUTINE ncdf_call_and_check_status(status)
    INTEGER, INTENT(IN) :: status

    IF (status /= nf90_noerr) THEN
      print *, trim(nf90_strerror(status))
      stop "NetCDF error"
    END IF
  END SUBROUTINE ncdf_call_and_check_status

  ! Read all the temporal profile for the Nx different tracers
  SUBROUTINE read_temporal_profile(ntracer)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ntracer ! Number of tracers for temporal profile

    ALLOCATE(tracercat(ntracer))

  END SUBROUTINE read_temporal_profile

  SUBROUTINE read_gridded_emissions()

  END SUBROUTINE read_gridded_emissions


END MODULE m_smartcarb_oae
