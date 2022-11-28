MODULE oas_roms_def

   ! Description
   ! -----------
   ! Module gathering the main public subroutine oas_roms_define as well as
   ! wrappers and helper subroutines for the definition of grids, areas, masks
   ! and exchanged fields.
   ! The module also holds some specific variables.
   
   ! Authors
   ! -------
   ! Matthieu Leclair - ETHZ

   
   ! Used modules
   ! ------------

   USE MPI

   USE netcdf, ONLY: nf90_open, nf90_close, nf90_strerror,  &
      &              nf90_inq_varid, nf90_get_var,   &
      &              NF90_NOWRITE, NF90_NOERR
   
   USE mod_oasis_grid, ONLY: oasis_write_grid,               &
      &                      oasis_write_mask,               &
      &                      oasis_write_corner,             &
      &                      oasis_write_area,               &
      &                      oasis_start_grids_writing,      &
      &                      oasis_terminate_grids_writing
   
   USE mod_oasis_part, ONLY: oasis_def_partition

   USE mod_oasis_var, ONLY: oasis_def_var

   USE mod_oasis_parameters, ONLY: OASIS_Real, OASIS_In, OASIS_Out,          &
      &                            CLIM_strategy, CLIM_offset, CLIM_SizeX,   &
      &                            CLIM_SizeY, CLIM_Ldx, CLIM_Box

   USE mod_oasis_sys, ONLY: oasis_abort

   USE mod_oasis_method, ONLY: oasis_enddef

   USE mod_oasis_kinds, ONLY: ip_intwp_p

   USE oas_roms_data, ONLY: OASIS_Success, ncomp_id, kl_comm,     &
      &                     OAS_GRID, cpl_grd, k_rho, k_u, k_v,   &
      &                     oas_itemp, oas_SSU_U, oas_SSU_V,      &
      &                     oas_SSV_U, oas_SSV_V, oas_UST_U,      &
      &                     oas_VST_U, oas_UST_V, oas_VST_V,      &
      &                     oas_NHF, oas_SWR, oas_TEP,            &
      &                     srcv, ssnd, krcv, ksnd,               &
      &                     alpha_rho, alpha_u, alpha_v,          &
      &                     u_cos_proj_u, v_cos_proj_u,           &
      &                     u_cos_proj_v, v_cos_proj_v,           &
      &                     IOASISDEBUGLVL, l_oas_seq,            &
      &                     l_snd_sst, l_snd_sm

   USE oas_roms_set_cpl_grd, ONLY: oas_roms_set_grd
      

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: oas_roms_define, oas_roms_read_nml

   ! Module variables
   ! ----------------
   CHARACTER(len=256), SAVE :: romsoc_aux_name   ! ROMSOC auxiliary filename
   INTEGER :: mype
   

CONTAINS

   ! *********************************************************************************** !
   !                               PUBLIC MAIN SUBROUTINE                                !
   ! *********************************************************************************** !

   SUBROUTINE oas_roms_define(grdname)
      ! Description
      ! -----------

      ! Arguments
      CHARACTER(len=256), INTENT(IN) :: grdname

      ! Local variables
      INTEGER :: ncid, ierr, il_flag, ierr_code
      LOGICAL :: l_wrt_aux

      CALL MPI_Comm_rank(kl_comm, mype, ierr)
      
      ! ---------------------------------------------------- !
      ! Read in namelilst parameters for coupling with OASIS !  
      ! ---------------------------------------------------- !
      ! - ML - For now, all nodes are reading the namelist. It should
      !        rather be read only by the master node and broadcast.

      CALL oas_roms_read_nml()

      ! ----------------------------- !
      ! Set coupling grid properties  !
      ! ----------------------------- !

      CALL oas_roms_set_grd(cpl_grd(k_rho), k_rho)
      CALL oas_roms_set_grd(cpl_grd(k_u  ), k_u  )
      CALL oas_roms_set_grd(cpl_grd(k_v  ), k_v  )

      ! -------------------------------------------- !
      ! Definition of the OASIS domain partitionning !
      ! -------------------------------------------- !

      CALL oas_roms_def_part(cpl_grd(k_rho))
      CALL oas_roms_def_part(cpl_grd(k_u  ))
      CALL oas_roms_def_part(cpl_grd(k_v  ))

      ! ------------------------------------- !
      ! Initialize ROMSOC auxiliary variables !  
      ! ------------------------------------- !
      
      ! Open ROMSOC auxiliary file
      ierr = nf90_open(TRIM(romsoc_aux_name), NF90_NOWRITE, ncid)
            
      IF ( ierr /= NF90_NOERR) THEN
         WRITE(*,*) 'OAS_ROMS : Error opening file ', TRIM(romsoc_aux_name)
         CALL abort
      ENDIF
      
      ! Read in coupling mask
      CALL romsoc_read_alpha(ncid, romsoc_aux_name, cpl_grd(k_rho), alpha_rho)
      CALL romsoc_read_alpha(ncid, romsoc_aux_name, cpl_grd(k_u  ), alpha_u  )
      CALL romsoc_read_alpha(ncid, romsoc_aux_name, cpl_grd(k_v  ), alpha_v  )
      
      ! Read in velocity directions and compute projection coefficients
      CALL romsoc_get_proj(ncid, romsoc_aux_name, cpl_grd(k_u), u_cos_proj_u, v_cos_proj_u)
      CALL romsoc_get_proj(ncid, romsoc_aux_name, cpl_grd(k_v), u_cos_proj_v, v_cos_proj_v)

      ! Close ROMSOC auxiliary file 
      ierr = nf90_close(ncid)

      ! ---------------------------------------- !
      ! Write OASIS3 auxiliary files (if needed) !
      ! ---------------------------------------- !

      ! Determine if OASIS auxiliary files have to be written based on
      ! the existence of grids.nc
      IF (mype == 0) l_wrt_aux = nf90_open('grids.nc', NF90_NOWRITE, ncid) /= NF90_NOERR
      CALL MPI_BCAST(l_wrt_aux, 1, MPI_LOGICAL, 0, kl_comm, ierr)
      
      IF (l_wrt_aux) THEN
            
         IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
            WRITE(*,*) 'OAS_ROMS : Writing OASIS auxiliary files'
         END IF
         ! Open ROMS grid file
         ierr = nf90_open(TRIM(grdname), NF90_NOWRITE, ncid)
         
         IF ( ierr /= NF90_NOERR) THEN
            WRITE(*,*) 'OAS_ROMS : Error opening file ', TRIM(grdname)
            CALL MPI_ABORT(kl_comm, ierr_code, ierr)
         ENDIF
         
         CALL oasis_start_grids_writing(il_flag)
         
         ! Write grids
         CALL oas_roms_wrt_grd(ncid, grdname, cpl_grd(k_rho))
         CALL oas_roms_wrt_grd(ncid, grdname, cpl_grd(k_u  ))
         CALL oas_roms_wrt_grd(ncid, grdname, cpl_grd(k_v  ))
         
         ! Write masks
         CALL oas_roms_wrt_msk(ncid, grdname, cpl_grd(k_rho))
         CALL oas_roms_wrt_msk(ncid, grdname, cpl_grd(k_u  ))
         CALL oas_roms_wrt_msk(ncid, grdname, cpl_grd(k_v  ))
         
         ! Write corners
         CALL oas_roms_wrt_crn(ncid, grdname, cpl_grd(k_rho))
         CALL oas_roms_wrt_crn(ncid, grdname, cpl_grd(k_u  ))
         CALL oas_roms_wrt_crn(ncid, grdname, cpl_grd(k_v  ))
         
         ! Write areas
         CALL oas_roms_wrt_areas(ncid, grdname)
         
         ! Close ROMS grid file
         ierr = nf90_close(ncid)
         
         CALL oasis_terminate_grids_writing()
         
      ENDIF

      ! --------------------------------- !
      ! Definition of the coupling Fields !
      ! --------------------------------- !

      ! Sent fields
      ! -----------
      ! Sea surface temperature [K]
      IF (l_snd_sm) then
         CALL oas_roms_def_var('snd', k_rho, 'SO_SST_A', oas_itemp, laction=.TRUE.)
      ENDIF
      IF (l_snd_sm) then
         ! Sea surface U-velocity for COSMO U-points [m/s]
         CALL oas_roms_def_var('snd', k_u  , 'SO_SSU_U', oas_SSU_U, laction=.TRUE.)
         ! Sea surface U-velocity for COSMO V-points [m/s]
         CALL oas_roms_def_var('snd', k_u  , 'SO_SSU_V', oas_SSU_V, laction=.TRUE.)
         ! Sea surface V-velocity for COSMO U-points [m/s]
         CALL oas_roms_def_var('snd', k_v  , 'SO_SSV_U', oas_SSV_U, laction=.TRUE.)
         ! Sea surface V-velocity for COSMO V-points [m/s]
         CALL oas_roms_def_var('snd', k_v  , 'SO_SSV_V', oas_SSV_V, laction=.TRUE.)
      ENDIF

      ! Received fields
      ! ---------------
      ! U-momentum flux (surface) [N/m2]
      CALL oas_roms_def_var('rcv', k_u  , 'RO_UST_U', oas_UST_U, laction=.TRUE.)
      ! U-momentum flux (surface) [N/m2]
      CALL oas_roms_def_var('rcv', k_u  , 'RO_VST_U', oas_VST_U, laction=.TRUE.)
      ! U-momentum flux (surface) [N/m2]
      CALL oas_roms_def_var('rcv', k_v  , 'RO_UST_V', oas_UST_V, laction=.TRUE.)
      ! U-momentum flux (surface) [N/m2]
      CALL oas_roms_def_var('rcv', k_v  , 'RO_VST_V', oas_VST_V, laction=.TRUE.)
      ! Net heat flux (surface) [ W/m2]
      CALL oas_roms_def_var('rcv', k_rho, 'RO_NHF_A', oas_NHF  , laction=.TRUE.)
      ! Direct shortwave downward radiation [W/m2]
      CALL oas_roms_def_var('rcv', k_rho, 'RO_SWR_A', oas_SWR  , laction=.TRUE.)
      ! Total evaporation - precipitation rate [kg/m2*s]
      CALL oas_roms_def_var('rcv', k_rho, 'RO_TEP_A', oas_TEP  , laction=.TRUE.)

      ! --------------------------- !
      ! End of the Definition Phase !
      ! --------------------------- !
      ! OASIS3 : only the processes involved in the coupling must call oasis_enddef.
      ! Here all processes are involved in coupling
      ! - ML - We should probably not involve all subdomains in the future

      CALL oasis_enddef(ierr)

   END SUBROUTINE oas_roms_define
   
   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_read_nml()
      ! Description
      ! -----------
      ! Read in romsoc.nml namelist
      
      ! Local variables
      INTEGER, PARAMETER :: nuin=61
      CHARACTER(len=*), PARAMETER :: nml_filename="romsoc.nml"
      INTEGER :: ierr

      IF (mype == 0) THEN
         WRITE(*,*) "OAS_ROMS : Reading namelist romsoc.nml"
      END IF

      ! Describe namelist content
      NAMELIST /romsoc/ romsoc_aux_name, IOASISDEBUGLVL, l_oas_seq, l_snd_sst, l_snd_sm

      ! Initialize with default values
      romsoc_aux_name = "romsoc_aux.nc"
      IOASISDEBUGLVL = 0
      l_oas_seq = .FALSE.

      ! Open namelist file
      OPEN(nuin, FILE=nml_filename, FORM='FORMATTED', STATUS='UNKNOWN',   &
         &       IOSTAT=ierr)
      IF (ierr /= 0) THEN
         WRITE(*,*) 'OAS_ROMS : Error while opening', nml_filename
         CALL abort
      END IF
      
      ! Read namelist
      READ(nuin, romsoc, IOSTAT=ierr)
      IF (ierr /= 0) THEN
         WRITE(*,*) 'OAS_ROMS : mype=', mype, ' Error while reading namelist romsoc from ', nml_filename
         CALL abort
      END IF
      
   END SUBROUTINE oas_roms_read_nml
   
   ! *********************************************************************************** !
   !                                PRIVATE SUBROUTINES                                  !
   ! *********************************************************************************** !

   SUBROUTINE oas_roms_wrt_grd(ncid, ncname, grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_grid subroutine

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: ncname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: lon, lat
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS grid at ", TRIM(grd%pt), "-points"
      END IF

      ALLOCATE(lon(grd%dims_l(1),grd%dims_l(2)),   &
         &     lat(grd%dims_l(1),grd%dims_l(2)))

      CALL oas_roms_read_2d(ncid, 'lon_'//grd%pt, ncname, grd, lon, scope='local', nc_extent='full')
      CALL oas_roms_read_2d(ncid, 'lat_'//grd%pt, ncname, grd, lat, scope='local', nc_extent='full')
      
      CALL oasis_write_grid(grd%grd_name, grd%dims_g(1), grd%dims_g(2), lon, lat, grd%part_id)
      
      DEALLOCATE(lon, lat)

   END SUBROUTINE oas_roms_wrt_grd

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_wrt_msk(ncid, ncname, grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_mask subroutine

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: ncname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: mask
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS mask at ", TRIM(grd%pt), "-points"
      END IF

      ALLOCATE(mask(grd%dims_l(1),grd%dims_l(2)))
      
      CALL oas_roms_read_2d(ncid, 'mask_'//grd%pt, ncname, grd, mask, scope='local', nc_extent='full')
      mask(:,:) = 1.0 - mask(:,:)
      
      CALL oasis_write_mask(grd%grd_name, grd%dims_g(1), grd%dims_g(2), INT(mask), grd%part_id)

      DEALLOCATE(mask)

   END SUBROUTINE oas_roms_wrt_msk

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_wrt_crn(ncid, ncname, grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_corner subroutine

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: ncname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: lon, lat
      INTEGER, DIMENSION(3) :: shp
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS grid corners at ", TRIM(grd%pt), "-points"
      END IF

      shp = (/grd%dims_l(1), grd%dims_l(2), 4/)

      ALLOCATE(lon(grd%dims_l(1), grd%dims_l(2), 4),   &
         &     lat(grd%dims_l(1), grd%dims_l(2), 4))

      CALL oas_roms_read_3d(ncid, 'lon_'//TRIM(grd%pt)//'_crn', ncname, grd, lon, shp(3), scope='local', nc_extent='full')
      CALL oas_roms_read_3d(ncid, 'lat_'//TRIM(grd%pt)//'_crn', ncname, grd, lat, shp(3), scope='local', nc_extent='full')
      
      CALL oasis_write_corner(grd%grd_name, grd%dims_g(1), grd%dims_g(2), 4, lon, lat, grd%part_id)

      DEALLOCATE(lon, lat)

   END SUBROUTINE oas_roms_wrt_crn

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE oas_roms_wrt_areas(ncid, ncname)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_area subroutine
      
      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: ncname
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: e1, e2, a_rho, a_u, a_v
      INTEGER :: n_xi, n_eta, n_xi_rho, n_eta_rho
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS areas"
      END IF

      ! Allocate temporary variables
      ALLOCATE(e1(cpl_grd(k_rho)%dims_l(1), cpl_grd(k_rho)%dims_l(2)),   &
         &     e2(cpl_grd(k_rho)%dims_l(1), cpl_grd(k_rho)%dims_l(2)),   &
         &     a_rho(cpl_grd(k_rho)%dims_l(1), cpl_grd(k_rho)%dims_l(2)),   &
         &     a_u(cpl_grd(k_u)%dims_l(1), cpl_grd(k_u)%dims_l(2)),   &
         &     a_v(cpl_grd(k_v)%dims_l(1), cpl_grd(k_v)%dims_l(2)) )

      ! Read in scale factors
      CALL oas_roms_read_2d(ncid, 'pm', ncname, cpl_grd(k_rho), e1, scope='local', nc_extent='full')
      CALL oas_roms_read_2d(ncid, 'pn', ncname, cpl_grd(k_rho), e2, scope='local', nc_extent='full')
      e1(:,:) = 1.0 / e1(:,:)
      e2(:,:) = 1.0 / e2(:,:)

      ! RHO-grid areas
      a_rho(:,:) =  e1(:,:) * e2(:,:)
      CALL oasis_write_area(cpl_grd(k_rho)%grd_name,    &
         &                  cpl_grd(k_rho)%dims_g(1),   &
         &                  cpl_grd(k_rho)%dims_g(2),   &
         &                  a_rho,                      &
         &                  cpl_grd(k_rho)%part_id       )

      ! U-grid areas
      a_u(:,:) = 0.25 * (e1(1:cpl_grd(k_rho)%dims_l(1)-1,:) + e1(2:cpl_grd(k_rho)%dims_l(1),:))   &
         &            * (e2(1:cpl_grd(k_rho)%dims_l(1)-1,:) + e2(2:cpl_grd(k_rho)%dims_l(1),:))
      CALL oasis_write_area(cpl_grd(k_u)%grd_name,    &
         &                  cpl_grd(k_u)%dims_g(1),   &
         &                  cpl_grd(k_u)%dims_g(2),   &
         &                  a_u,                      &
         &                  cpl_grd(k_u)%part_id       )

      ! V-grid areas
      a_v(:,:) = 0.25 * (e1(:,1:cpl_grd(k_rho)%dims_l(2)-1) + e1(:,2:cpl_grd(k_rho)%dims_l(2)))   &
         &            * (e2(:,1:cpl_grd(k_rho)%dims_l(2)-1) + e2(:,2:cpl_grd(k_rho)%dims_l(2)))
      CALL oasis_write_area(cpl_grd(k_v)%grd_name,    &
         &                  cpl_grd(k_v)%dims_g(1),   &
         &                  cpl_grd(k_v)%dims_g(2),   &
         &                  a_v,                      &
         &                  cpl_grd(k_v)%part_id       )

      ! Deallocate temporary variables
      DEALLOCATE(e1, e2, a_rho, a_u, a_v)

   END SUBROUTINE oas_roms_wrt_areas

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_def_part(grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_def_partition subroutine

      ! Arguments
      TYPE(OAS_GRID) :: grd
      
      ! Local variables
      INTEGER, DIMENSION(5) :: kparal
      INTEGER :: xi_l, eta_l, xi_g, ii_0, jj_0
      INTEGER :: ierr=OASIS_Success   ! error code returned by oasis_def_partition
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : Defining OASIS partition at ", grd%pt, "-points"
      END IF

      xi_l = grd%dims_l(1)   ! local xi dimension
      eta_l = grd%dims_l(2)   ! local eta dimension
      xi_g = grd%dims_g(1)   ! global xi dimension
      ii_0 = grd%start_g(1)   ! global xi index of first local xi point
      jj_0 = grd%start_g(2)   ! global eta index of first local eta point
      
      kparal(CLIM_strategy) = CLIM_Box ! Box Partition
      kparal(CLIM_Ldx) = xi_g
      kparal(CLIM_offset) = ii_0-1 + (jj_0-1) * xi_g   ! local grid offset
      kparal(CLIM_SizeX) = xi_l
      kparal(CLIM_SizeY) = eta_l

      CALL oasis_def_partition(grd%part_id, kparal, ierr)
      IF (ierr /= OASIS_Success)  THEN
         CALL oasis_abort(ncomp_id, 'oas_roms_def_helpers',   &
            &             'Failure in oasis_def_partition for grid type', grd%pt)
      END IF
      
   END SUBROUTINE oas_roms_def_part

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_def_var(drct, k_pt, clname, fld_id, laction, dtype)
      ! Description
      ! -----------
      ! Wrapper to the oasis_def_var subroutine

      ! Arguments
      CHARACTER(len=3), INTENT(IN) :: drct   ! Direction of coupling ('snd' or 'rcv')
      INTEGER, INTENT(IN) :: k_pt   ! Point type on which the coupling field is defined
      CHARACTER(len=8), INTENT(IN) :: clname   ! Name of the coupling field
      INTEGER, INTENT(INOUT) :: fld_id   ! Set id of the excahanged field
      LOGICAL, INTENT(IN), OPTIONAL :: laction   ! Activate coupling for this variable
      INTEGER(KIND=ip_intwp_p), INTENT(IN), OPTIONAL :: dtype   ! Data type

      ! local variables
      INTEGER, DIMENSION(1) :: var_shape   ! dummy argument for oasis_def_var
      INTEGER, DIMENSION(2), PARAMETER :: var_nodims=(/2, 1/) ! rank of exchanged arrays
      !                                                       ! and number of bundles
      !                                                       ! (always 1 for OASIS3)
      INTEGER (KIND=ip_intwp_p) :: oas_type   ! actual data type
      LOGICAL :: oas_act   ! actual laction
      INTEGER :: ierr   ! error code returned by oasis_def_var

      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : Defining OASIS field ", clname
      END IF

      ! Default values
      ! --------------
      IF (PRESENT(dtype)) THEN
         oas_type = dtype
      ELSE
         oas_type = OASIS_Real
      ENDIF
      IF (PRESENT(laction)) THEN
         oas_act = laction
      ELSE
         oas_act = .TRUE.
      ENDIF
      ! give default value in case oas_act is .FALSE.
      ierr = OASIS_Success

      IF (drct == 'snd') THEN   ! Sent fields
         
         ksnd = ksnd + 1
         fld_id = ksnd
         ssnd(fld_id)%clname = clname
         ssnd(fld_id)%laction = oas_act
         ssnd(fld_id)%k_pt = k_pt
         IF (oas_act) THEN
            ALLOCATE(ssnd(fld_id)%pdata(cpl_grd(k_pt)%imin:cpl_grd(k_pt)%imax,   &
               &                        cpl_grd(k_pt)%jmin:cpl_grd(k_pt)%jmax),   &
               &     stat=ierr)
            IF (ierr > 0) THEN
               CALL oasis_abort(ncomp_id, 'oas_roms_def',   &
                  &             'Failure in allocating field', clname)
            ENDIF
            CALL oasis_def_var(ssnd(fld_id)%nid, clname, cpl_grd(k_pt)%part_id,   &
               &               var_nodims, OASIS_Out, var_shape, oas_type, ierr)
         END IF
         
      ELSEIF (drct == 'rcv') THEN   ! Received fields
         
         krcv = krcv + 1
         fld_id = krcv
         srcv(fld_id)%clname = clname
         srcv(fld_id)%laction = oas_act
         srcv(fld_id)%k_pt = k_pt
         IF (oas_act) THEN
            ALLOCATE(srcv(fld_id)%pdata(cpl_grd(k_pt)%imin:cpl_grd(k_pt)%imax,   &
               &                        cpl_grd(k_pt)%jmin:cpl_grd(k_pt)%jmax),   &
               &     stat=ierr)
            IF (ierr > 0) THEN
               CALL oasis_abort(ncomp_id, 'oas_roms_def',   &
                  &             'Failure in allocating field', clname)
            ENDIF
            CALL oasis_def_var(srcv(fld_id)%nid, clname, cpl_grd(k_pt)%part_id,   &
               &               var_nodims, OASIS_In, var_shape, oas_type, ierr)
         END IF
         
      END IF
      
      IF (ierr /= OASIS_Success)  THEN
         CALL oasis_abort(ncomp_id, 'oas_roms_def',   &
            &             'Failure in oasis_def_var for field', clname)
      END IF
      
   END SUBROUTINE oas_roms_def_var

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE romsoc_read_alpha(ncid, ncname, grd, alpha)
      ! Description
      ! -----------
      ! Read in coupling coefficient from ROMSOC auxiliary file

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: ncname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: alpha
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : reading coupling coefficient at ", TRIM(grd%pt), "-points"
      END IF

      ALLOCATE(alpha(grd%imin:grd%imax,grd%jmin:grd%jmax))
      
      CALL oas_roms_read_2d(ncid, 'alpha_'//TRIM(grd%pt), ncname, grd, alpha, scope='local', nc_extent='inner')
      
   END SUBROUTINE romsoc_read_alpha

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE romsoc_get_proj(ncid, ncname, grd, u_proj, v_proj)
      ! Description
      ! -----------
      ! Read in ROMS and COSMO directions and compute projection coefficients
      ! - ML - Maybe consider having the scalar products directly in the ROMSOC
      !        auxiliary file and only read them here.

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: ncname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: u_proj, v_proj
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: cos_u_dir, cos_v_dir, roms_dir
      INTEGER :: k

      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : Compute velocity projection coefficients at ", TRIM(grd%pt), "-points"
      END IF
      
      ALLOCATE(cos_u_dir(grd%imin:grd%imax,grd%jmin:grd%jmax,3),   &
         &     cos_v_dir(grd%imin:grd%imax,grd%jmin:grd%jmax,3),   &
         &     roms_dir (grd%imin:grd%imax,grd%jmin:grd%jmax,3),   &
         &     u_proj   (grd%imin:grd%imax,grd%jmin:grd%jmax  ),   &
         &     v_proj   (grd%imin:grd%imax,grd%jmin:grd%jmax  ))

      ! Read in 3d velocity directions (unit vectors)
      CALL oas_roms_read_3d(ncid, 'cos_u_dir_roms_'//TRIM(grd%pt), ncname, grd, cos_u_dir, 3, scope='local', nc_extent='inner')
      CALL oas_roms_read_3d(ncid, 'cos_v_dir_roms_'//TRIM(grd%pt), ncname, grd, cos_v_dir, 3, scope='local', nc_extent='inner')
      CALL oas_roms_read_3d(ncid, 'roms_'//TRIM(grd%pt)//'_dir'  , ncname, grd, roms_dir , 3, scope='local', nc_extent='inner')

      ! Compute projections
      u_proj(:,:) = cos_u_dir(:,:,1) * roms_dir(:,:,1)
      v_proj(:,:) = cos_v_dir(:,:,1) * roms_dir(:,:,1)
      DO k = 2, 3
         u_proj(:,:) = u_proj(:,:) + cos_u_dir(:,:,k) * roms_dir(:,:,k)
         v_proj(:,:) = v_proj(:,:) + cos_v_dir(:,:,k) * roms_dir(:,:,k)
      END DO

      DEALLOCATE(cos_u_dir, cos_v_dir, roms_dir)
      
   END SUBROUTINE romsoc_get_proj

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE oas_roms_read_2d(ncid, vname, fname, grd, buffer, scope, nc_extent)
      ! Description
      ! -----------
      ! Read in 2d variables from netcdf file

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: vname, fname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:), INTENT(INOUT) :: buffer
      CHARACTER(len=*), INTENT(IN) :: scope, nc_extent
      
      ! local variables
      INTEGER, DIMENSION(2) :: kstart, ncount
      INTEGER :: var_id, ierr

      ierr = nf90_inq_varid(ncid, TRIM(vname), var_id)
      
      IF (ierr == NF90_NOERR) THEN
         
         IF (TRIM(scope) == 'global') THEN
            IF (TRIM(nc_extent) == 'full') THEN
               kstart = (/2, 2/)
            ELSEIF (TRIM(nc_extent) == 'inner') THEN
               kstart = (/1, 1/)
            ELSE
               WRITE(*,*) "OAS_ROMS : ERROR in oas_roms_read_2d : nc_extent has to be either 'full' or 'inner'"
            END IF
            ncount = grd%dims_g
         ELSEIF (TRIM(scope) == 'local') THEN
            IF (TRIM(nc_extent) == 'full') THEN
               kstart = grd%start_g + 1
            ELSEIF (TRIM(nc_extent) == 'inner') THEN
               kstart = grd%start_g
            ELSE
               WRITE(*,*) "OAS_ROMS : ERROR in oas_roms_read_2d : nc_extent has to be either 'full' or 'inner'"
            END IF
            ncount = grd%dims_l
         ELSE
            WRITE(*,*) "OAS_ROMS : ERROR in oas_roms_read_2d : scope has to be either 'local' or 'global'"
            CALL abort
         END IF
         
         ierr = nf90_get_var(ncid, var_id, buffer, start=kstart, count=ncount)
         
         IF (ierr /= NF90_NOERR) THEN
            WRITE(*,*) 'OAS_ROMS : ERROR in oas_roms_read_2d reading var ', TRIM(vname), ' in ', TRIM(fname)
            WRITE(*,*) 'OAS_ROMS : ', nf90_strerror(ierr)
            WRITE(*,*) 'OAS_ROMS : start and count provided to nf90_get_var: ', kstart, ncount
            WRITE(*,*) 'OAS_ROMS : buffer: ', 'LBOUND=', LBOUND(buffer), ' UBOUND=', UBOUND(buffer)
            CALL abort
         END IF
         
      ELSE
         
         WRITE(*,*) 'OAS_ROMS : Error inquiring var ', TRIM(vname), ' in oas_roms_read_2d'
         CALL abort
         
      END IF
      
   END SUBROUTINE oas_roms_read_2d

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_read_3d(ncid, vname, fname, grd, buffer, dim3, scope, nc_extent)
      ! Description
      ! -----------
      ! Read in 3d variables from netcdf file

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: vname, fname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:,:), INTENT(INOUT) :: buffer
      INTEGER, INTENT(IN) :: dim3
      CHARACTER(len=*), INTENT(IN) :: scope, nc_extent
      
      ! local variables
      INTEGER, DIMENSION(3) :: kstart, ncount
      INTEGER :: var_id, ierr

      ierr = nf90_inq_varid(ncid, TRIM(vname), var_id)
      
      IF (ierr == NF90_NOERR) THEN

         IF (TRIM(scope) == 'global') THEN
            IF (TRIM(nc_extent) == 'full') THEN
               kstart = (/2, 2, 1/)
            ELSEIF (TRIM(nc_extent) == 'inner') THEN
               kstart = (/1, 1, 1/)
            ELSE
               WRITE(*,*) "OAS_ROMS : ERROR in oas_roms_read_3d : nc_extent has to be either 'full' or 'inner'"
            END IF
            ncount = (/grd%dims_g(1), grd%dims_g(2), dim3/)
         ELSEIF (TRIM(scope) == 'local') THEN
            IF (TRIM(nc_extent) == 'full') THEN
               kstart = (/grd%start_g(1)+1, grd%start_g(2)+1, 1/)
            ELSEIF (TRIM(nc_extent) == 'inner') THEN
               kstart = (/grd%start_g(1), grd%start_g(2), 1/)
            ELSE
               WRITE(*,*) "OAS_ROMS : ERROR in oas_roms_read_3d : nc_extent has to be either 'full' or 'inner'"
            END IF
            ncount = (/grd%dims_l(1), grd%dims_l(2), dim3/)
         ELSE
            WRITE(*,*) "OAS_ROMS : ERROR in oas_roms_read_3d : scope has to be either 'local' or 'global'"
            CALL abort
         END IF
         
         ierr = nf90_get_var(ncid, var_id, buffer, start=kstart, count=ncount)
         
         IF (ierr /= NF90_NOERR) THEN
            WRITE(*,*) 'OAS_ROMS : ERROR in oas_roms_read_3d reading var ', TRIM(vname), ' in ', TRIM(fname)
            WRITE(*,*) 'OAS_ROMS : ', nf90_strerror(ierr)
            WRITE(*,*) 'OAS_ROMS : start and count provided to nf90_get_var: ', kstart, ncount
            WRITE(*,*) 'OAS_ROMS : buffer: ', 'LBOUND=', LBOUND(buffer), ' UBOUND=', UBOUND(buffer)
            CALL abort
         END IF
      
      ELSE
         
         WRITE(*,*) 'OAS_ROMS : Error inquiring var ', TRIM(vname), ' in oas_roms_read_3d'
         CALL abort
         
      END IF
      
   END SUBROUTINE oas_roms_read_3d
   
   
END MODULE oas_roms_def
