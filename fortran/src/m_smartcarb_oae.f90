!
! SMARTCARB - Online Anthropogenic Emissions
!
!

MODULE m_smartcarb_oae
  USE mo_kind
  USE netcdf

  IMPLICIT NONE

  ! Constant variable
  INTEGER, PARAMETER :: vp_nlevel = 16
  INTEGER, PARAMETER :: tp_param_hourofday = 24
  INTEGER, PARAMETER :: tp_param_dayofweek = 7
  INTEGER, PARAMETER :: tp_param_monthofyear = 12
  INTEGER, PARAMETER :: tp_param_hour = 8784

  ! TODO filename specs
  CHARACTER(len=*), PARAMETER :: vertical_profile_nc = "../data/vertical_profile.nc"
  CHARACTER(len=*), PARAMETER :: temporal_profile_nc = "../data/temporal_profile.nc"
  CHARACTER(len=*), PARAMETER :: gridded_emissions_nc = "../data/emissions.nc"

  !
  ! Vertical profile arrays
  !
  REAL(KIND=wp), DIMENSION(:), ALLOCATABLE :: &
    vp_layer_bot,            & ! bottom of layer above ground
    vp_layer_top,            & ! top of layer above ground
    vp_factor_area,          & ! scale factor for area sources
    vp_factor_point            ! scale factor for point sources

  !
  ! Tempororal profile arrays
  !
  INTEGER :: tp_ntracercat ! Number of tracer category in temporal profile
  INTEGER :: tp_ncountry   ! Number of country ID in temporal profile

  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: &
    tp_tracercat             ! Tracer name

  REAL(KIND=wp), DIMENSION(:,:,:), ALLOCATABLE :: &
    tp_dayofweek,          & ! day-of-week scaling factor
    tp_monthofyear           ! seasonal scaling factor

  REAL(KIND=wp), DIMENSION(:,:),   ALLOCATABLE :: &
    tp_hourofday,          & ! diurnal scaling factor
    tp_hour                  ! hourly scaling factor

  INTEGER, DIMENSION(:),  ALLOCATABLE :: &
    tp_countryid             ! EMEP country code

  !
  ! Gridded emissions fields
  !
  CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: &
    gridded_emissions_idx    ! Name of the annual mean emissions
                             ! fields

  REAL, DIMENSION(:,:,:), ALLOCATABLE :: &
    gridded_emissions                      ! Annual mean 2D emissions fields


CONTAINS

  ! Allocate the data structure necessary for the vertical profile
  SUBROUTINE init_vertical_profile_fields()
    IMPLICIT NONE

    ALLOCATE(vp_layer_bot(vp_nlevel))
    ALLOCATE(vp_layer_top(vp_nlevel))
    ALLOCATE(vp_factor_area(vp_nlevel))
    ALLOCATE(vp_factor_point(vp_nlevel))
  END SUBROUTINE init_vertical_profile_fields

  ! Cleanup data structure necessary for the vertical profile
  SUBROUTINE cleanup_vertical_profile_fields()
    IMPLICIT NONE

    DEALLOCATE(vp_layer_bot)
    DEALLOCATE(vp_layer_top)
    DEALLOCATE(vp_factor_area)
    DEALLOCATE(vp_factor_point)
  END SUBROUTINE cleanup_vertical_profile_fields

  ! Read the vertical profile from NetCDF file and store them into their
  ! corresponding arrays.
  SUBROUTINE read_vertical_profile_from_file(ierror, yerrmsg)
    IMPLICIT NONE
    INTEGER :: ncid, varid, err_status
    INTEGER, INTENT(OUT) :: ierror
    CHARACTER (LEN= *), INTENT(OUT) :: yerrmsg

    err_status = nf90_open(vertical_profile_nc, NF90_NOWRITE, ncid)
    err_status = nf90_inq_varid(ncid, "layer_bot", varid)
    err_status = nf90_get_var(ncid, varid, vp_layer_bot)
    err_status = nf90_inq_varid(ncid, "layer_top", varid)
    err_status = nf90_get_var(ncid, varid, vp_layer_top)
    err_status = nf90_inq_varid(ncid, "factor_area", varid)
    err_status = nf90_get_var(ncid, varid, vp_factor_area)
    err_status = nf90_inq_varid(ncid, "factor_point", varid)
    err_status = nf90_get_var(ncid, varid, vp_factor_point)
    err_status = nf90_close(ncid)

    IF (err_status /= nf90_noerr) THEN
      ierror = err_status
      yerrmsg = trim(nf90_strerror(err_status))
    END IF
  END SUBROUTINE read_vertical_profile_from_file

  ! Read all the temporal profile for the Nx different tracers
  SUBROUTINE init_temporal_profile_fields(ierror, yerrmsg)
    IMPLICIT NONE
    INTEGER :: ncid, dimid, err_status
    INTEGER, INTENT(OUT) :: ierror
    CHARACTER (LEN= *), INTENT(OUT) :: yerrmsg

    ! Open the NetCDF file
    err_status = nf90_open(temporal_profile_nc, NF90_NOWRITE, ncid)

    ! Get tracercat dimension information
    err_status = nf90_inq_dimid(ncid, "tracercat", dimid)
    err_status = nf90_inquire_dimension(ncid, dimid, len = tp_ntracercat)

    ! Get countryID dimension information
    err_status = nf90_inq_dimid(ncid, "country", dimid)
    err_status = nf90_inquire_dimension(ncid, dimid, len = tp_ncountry)

    err_status = nf90_close(ncid)

    IF (err_status /= nf90_noerr) THEN
      ierror = err_status
      yerrmsg = trim(nf90_strerror(err_status))
    END IF

    ! Allocate fields to store temporal profile information
    ALLOCATE(tp_tracercat(tp_ntracercat))
    ALLOCATE(tp_hourofday(tp_param_hourofday, tp_ntracercat))
    ALLOCATE(tp_dayofweek(tp_param_dayofweek, tp_ntracercat, tp_ncountry))
    ALLOCATE(tp_monthofyear(tp_param_monthofyear, tp_ntracercat, tp_ncountry))
    ALLOCATE(tp_hour(tp_param_hour, tp_ntracercat))
    ALLOCATE(tp_countryid(tp_ncountry))
  END SUBROUTINE init_temporal_profile_fields

  SUBROUTINE cleanup_temporal_profile_fields()
    IMPLICIT NONE

    DEALLOCATE(tp_tracercat)
    DEALLOCATE(tp_hourofday)
    DEALLOCATE(tp_dayofweek)
    DEALLOCATE(tp_monthofyear)
    DEALLOCATE(tp_hour)
    DEALLOCATE(tp_countryid)
  END SUBROUTINE cleanup_temporal_profile_fields

  SUBROUTINE read_temporal_profile_from_file(ierror, yerrmsg)
    IMPLICIT NONE
    INTEGER :: ncid, varid, err_status
    INTEGER, INTENT(OUT) :: ierror
    CHARACTER (LEN= *), INTENT(OUT) :: yerrmsg

    ! Open the NetCDF file
    err_status = nf90_open(temporal_profile_nc, NF90_NOWRITE, ncid)

    ! Read tracercat variable
    err_status = nf90_inq_varid(ncid, "tracercat", varid)
    err_status = nf90_get_var(ncid, varid, tp_tracercat)

    ! Read hourofday variable
    err_status = nf90_inq_varid(ncid, "hourofday", varid)
    err_status = nf90_get_var(ncid, varid, tp_hourofday, &
      start = (/1, 1/), count = (/tp_ntracercat, tp_param_hourofday/))

    ! Read dayofweek variable
    err_status = nf90_inq_varid(ncid, "dayofweek", varid)
    err_status = nf90_get_var(ncid, varid, tp_dayofweek, &
      start = (/1,1/), count = (/tp_ncountry, tp_ntracercat, tp_param_dayofweek/))

    ! Read monthofyear variable
    err_status = nf90_inq_varid(ncid, "monthofyear", varid)
    err_status = nf90_get_var(ncid, varid, tp_monthofyear, &
      start = (/1,1/), count = (/tp_ncountry, tp_ntracercat, tp_param_monthofyear/))

    ! Read hour variable
    err_status = nf90_inq_varid(ncid, "hour", varid)
    err_status = nf90_get_var(ncid, varid, tp_hour, &
      start = (/1,1/), count = (/tp_ntracercat, tp_param_hour/))

    ! Read countryID variable
    err_status = nf90_inq_varid(ncid, "countryID", varid)
    err_status = nf90_get_var(ncid, varid, tp_countryid)

    ! Close the NetCDF file
    err_status = nf90_close(ncid)

    IF (err_status /= nf90_noerr) THEN
      ierror = err_status
      yerrmsg = trim(nf90_strerror(err_status))
    END IF

  END SUBROUTINE read_temporal_profile_from_file




  ! Allocate the data structure for the gridded emissions fields and read them
  ! from the NetCDF file.
  SUBROUTINE init_and_read_gridded_emissions(ierror, yerrmsg)
    IMPLICIT NONE
    INTEGER :: ncid, nc_nvar, i, gridded_idx, err_status
    INTEGER, DIMENSION(:), ALLOCATABLE :: varids
    CHARACTER(LEN=20) :: var_name
    INTEGER, INTENT(OUT) :: ierror
    CHARACTER (LEN= *), INTENT(OUT) :: yerrmsg

    gridded_idx = 1

    ! Open the NetCDF file
    err_status = nf90_open(gridded_emissions_nc, NF90_NOWRITE, ncid)

    ! Get number of variable in the file
    err_status = nf90_inquire(ncid, nVariables = nc_nvar)
    ALLOCATE(varids(nc_nvar))
    err_status = nf90_inq_varids(ncid, nc_nvar, varids)

    ! Allocate the gridded emissions according to the number of fields
    ALLOCATE(gridded_emissions(nc_nvar - 2, 700, 800))
    ALLOCATE(gridded_emissions_idx(nc_nvar - 2))
    DO i = 1, nc_nvar
      err_status = nf90_inquire_variable(ncid, varids(i), name=var_name)
      IF (var_name /= 'rlon' .AND. var_name /= 'rlat') THEN
        ! Read the actual field
        gridded_emissions_idx(gridded_idx) = var_name
        err_status = nf90_get_var(ncid, varids(i), &
          gridded_emissions(gridded_idx,:,:), start = (/1,1/), count=(/800,700/))
        gridded_idx = gridded_idx + 1
      END IF
    END DO

    ! Close the NetCDF file
    err_status = nf90_close(ncid)

    IF (err_status /= nf90_noerr) THEN
      ierror = err_status
      yerrmsg = trim(nf90_strerror(err_status))
    END IF
  END SUBROUTINE init_and_read_gridded_emissions


  ! Find the index of a gridded emission field based on its name
  FUNCTION get_gridded_emissions_idx(name)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER :: get_gridded_emissions_idx
    INTEGER :: i

    get_gridded_emissions_idx = 0
    DO i = 1, SIZE(gridded_emissions_idx)
      IF(name == gridded_emissions_idx(i)) THEN
        get_gridded_emissions_idx = i
        EXIT
      END IF
    END DO
  END FUNCTION get_gridded_emissions_idx

  ! Clean-up data structure for gridded emission fields
  SUBROUTINE cleanup_gridded_emissions()
    IMPLICIT NONE

    IF (ALLOCATED(gridded_emissions)) DEALLOCATE(gridded_emissions)
    IF (ALLOCATED(gridded_emissions_idx)) DEALLOCATE(gridded_emissions_idx)
  END SUBROUTINE cleanup_gridded_emissions

  SUBROUTINE oae_init(ierror, yerrmsg)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: ierror
    CHARACTER (LEN= *), INTENT(OUT) :: yerrmsg

    CALL init_vertical_profile_fields()
    CALL init_temporal_profile_fields(ierror, yerrmsg)

    CALL read_vertical_profile_from_file(ierror, yerrmsg)
    CALL read_temporal_profile_from_file(ierror, yerrmsg)

    CALL init_and_read_gridded_emissions(ierror, yerrmsg)
  END SUBROUTINE oae_init

  SUBROUTINE oae_cleanup()
    IMPLICIT NONE

    CALL cleanup_vertical_profile_fields()
    CALL cleanup_temporal_profile_fields()
    CALL cleanup_gridded_emissions()
  END SUBROUTINE oae_cleanup

END MODULE m_smartcarb_oae
