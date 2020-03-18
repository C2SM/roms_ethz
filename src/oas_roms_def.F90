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
      &                     oas_itemp, oas_UST_U, oas_VST_U,      &
      &                     oas_UST_V, oas_VST_V, oas_NHF,        &
      &                     oas_SWR, oas_TEP,                     &
      &                     srcv, ssnd, krcv, ksnd,               &
      &                     alpha_rho, alpha_u, alpha_v,          &
      &                     u_cos_proj_u, v_cos_proj_u,           &
      &                     u_cos_proj_v, v_cos_proj_v,           &
      &                     IOASISDEBUGLVL, l_oas_seq

   USE oas_roms_set_cpl_grd, ONLY: oas_roms_set_grd
      

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: oas_roms_define

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
      INTEGER :: ncid, ierr, il_flag

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

      ! ------------------------------------- !
      ! Initialize ROMSOC auxiliary variables !  
      ! ------------------------------------- !
      
      ! Open ROMSOC auxiliary file
      ierr = nf90_open(TRIM(romsoc_aux_name), NF90_NOWRITE, ncid)
      
      ! Read in coupling mask
      CALL romsoc_read_alpha(ncid, cpl_grd(k_rho), alpha_rho)
      CALL romsoc_read_alpha(ncid, cpl_grd(k_u  ), alpha_u  )
      CALL romsoc_read_alpha(ncid, cpl_grd(k_v  ), alpha_v  )
      
      ! Read in velocity directions and compute projection coefficients
      CALL romsoc_get_proj(ncid, cpl_grd(k_u), u_cos_proj_u, v_cos_proj_u)
      CALL romsoc_get_proj(ncid, cpl_grd(k_v), u_cos_proj_v, v_cos_proj_v)

      ! Close ROMSOC auxiliary file 
      ierr = nf90_close(ncid)

      ! ---------------------------------------------------------------- !
      ! Master process writes info on OASIS3 auxiliary files (if needed) ! 
      ! ---------------------------------------------------------------- !
      
      IF (mype .EQ. 0) THEN   ! Only master node writes grid file

         CALL oasis_start_grids_writing(il_flag)

         IF (il_flag == 1) THEN

            ! Open ROMS grid file
            ierr = nf90_open(TRIM(grdname), NF90_NOWRITE, ncid)

            ! Write grids
            CALL oas_roms_wrt_grd(ncid, cpl_grd(k_rho))
            CALL oas_roms_wrt_grd(ncid, cpl_grd(k_u  ))
            CALL oas_roms_wrt_grd(ncid, cpl_grd(k_v  ))

            ! Write masks
            CALL oas_roms_wrt_msk(ncid, cpl_grd(k_rho))
            CALL oas_roms_wrt_msk(ncid, cpl_grd(k_u  ))
            CALL oas_roms_wrt_msk(ncid, cpl_grd(k_v  ))

            ! Write corners
            CALL oas_roms_wrt_crn(ncid, cpl_grd(k_rho))
            CALL oas_roms_wrt_crn(ncid, cpl_grd(k_u  ))
            CALL oas_roms_wrt_crn(ncid, cpl_grd(k_v  ))

            ! Write areas
            CALL oas_roms_wrt_areas(ncid)

            ! Close ROMS grid file
            ierr = nf90_close(ncid)

            CALL oasis_terminate_grids_writing()

         ENDIF

      ENDIF

      ! -------------------------------------- !
      ! Definition of the Domain Decomposition !
      ! -------------------------------------- !
      ! Allocate the fields sent and received by the model without Halo and
      ! overlaps between subdomains

      CALL oas_roms_def_part(cpl_grd(k_rho))
      CALL oas_roms_def_part(cpl_grd(k_u  ))
      CALL oas_roms_def_part(cpl_grd(k_v  ))

      ! --------------------------------- !
      ! Definition of the coupling Fields !
      ! --------------------------------- !
      ! - ML - coupling of all fields activated by default.
      !        In the final version, it should be defined by the user
      !        (for example through namelist)

      ! Sent fields
      ! -----------
      ! Sea surface temperature [K]
      CALL oas_roms_def_var('snd', k_rho, 'SO_SST_A', oas_itemp, laction=.TRUE.)

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
   

   ! *********************************************************************************** !
   !                                PRIVATE SUBROUTINES                                  !
   ! *********************************************************************************** !

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
      NAMELIST /romsoc/ romsoc_aux_name, IOASISDEBUGLVL, l_oas_seq

      ! Initialize with default values
      romsoc_aux_name = "romsoc_aux.nc"
      IOASISDEBUGLVL = 0
      l_oas_seq = .FALSE.

      ! Open namelist file
      OPEN(nuin, FILE=nml_filename, FORM='FORMATTED', STATUS='UNKNOWN',   &
         &       IOSTAT=ierr)
      IF (ierr /= 0) THEN
         WRITE(*,*) 'Error while opening', nml_filename
         CALL abort
      END IF
      
      ! Read namelist
      READ(nuin, romsoc, IOSTAT=ierr)
      IF (ierr /= 0) THEN
         WRITE(*,*) 'mype=', mype, ' Error while reading namelist romsoc from ', nml_filename
         CALL abort
      END IF
      
   END SUBROUTINE oas_roms_read_nml
   
   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_wrt_grd(ncid, grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_grid subroutine

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      TYPE(OAS_GRID), INTENT(IN) :: grd
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: lon, lat
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS grid at ", TRIM(grd%pt), "-points"
      END IF

      ALLOCATE(lon(grd%dims_g(1),grd%dims_g(2)),   &
         &     lat(grd%dims_g(1),grd%dims_g(2)))

      CALL oas_roms_read_2d(ncid, 'lon_'//grd%pt, grd, lon, 'global')
      CALL oas_roms_read_2d(ncid, 'lat_'//grd%pt, grd, lat, 'global')
      
      CALL oasis_write_grid(grd%grd_name, grd%dims_g(1), grd%dims_g(2), lon, lat)
      
      DEALLOCATE(lon, lat)

   END SUBROUTINE oas_roms_wrt_grd

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_wrt_msk(ncid, grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_mask subroutine

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      TYPE(OAS_GRID), INTENT(IN) :: grd
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: mask
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS mask at ", TRIM(grd%pt), "-points"
      END IF

      ALLOCATE(mask(grd%dims_g(1),grd%dims_g(2)))
      
      CALL oas_roms_read_2d(ncid, 'mask_'//grd%pt, grd, mask, 'global')
      mask(:,:) = 1.0 - mask(:,:)
      
      CALL oasis_write_mask(grd%grd_name, grd%dims_g(1), grd%dims_g(2), INT(mask))

      DEALLOCATE(mask)

   END SUBROUTINE oas_roms_wrt_msk

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_wrt_crn(ncid, grd)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_corner subroutine

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      TYPE(OAS_GRID), INTENT(IN) :: grd
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: lon, lat
      INTEGER, DIMENSION(3) :: shp
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS grid corners at ", TRIM(grd%pt), "-points"
      END IF

      shp = (/grd%dims_g(1), grd%dims_g(2), 4/)

      ALLOCATE(lon(shp(1),shp(2),shp(3)),   &
         &     lat(shp(1),shp(2),shp(3)))

      CALL oas_roms_read_3d(ncid, 'lon_'//TRIM(grd%pt)//'_crn', grd, lon, shp(3), 'global')
      CALL oas_roms_read_3d(ncid, 'lat_'//TRIM(grd%pt)//'_crn', grd, lat, shp(3), 'global')
      
      CALL oasis_write_corner(grd%grd_name, shp(1), shp(2), shp(3), lon, lat)

      DEALLOCATE(lon, lat)

   END SUBROUTINE oas_roms_wrt_crn

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE oas_roms_wrt_areas(ncid)
      ! Description
      ! -----------
      ! Wrapper to the oasis_write_area subroutine
      
      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      
      ! Local variables
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: e1, e2, a_rho, a_u, a_v
      INTEGER :: n_xi, n_eta, n_xi_rho, n_eta_rho
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : writing OASIS areas"
      END IF

      ! Allocate and read scale factors
      n_xi_rho = cpl_grd(k_rho)%dims_g(1)
      n_eta_rho = cpl_grd(k_rho)%dims_g(2)
      ALLOCATE(e1(n_xi_rho,n_eta_rho), e2(n_xi_rho,n_eta_rho))
      CALL oas_roms_read_2d(ncid, 'pm', cpl_grd(k_rho), e1, 'global')
      CALL oas_roms_read_2d(ncid, 'pn', cpl_grd(k_rho), e2, 'global')
      e1(:,:) = 1.0 / e1(:,:)
      e2(:,:) = 1.0 / e2(:,:)

      ! RHO-grid areas
      ALLOCATE(a_rho(n_xi_rho,n_eta_rho))
      a_rho(:,:) =  e1(:,:) * e2(:,:)
      CALL oasis_write_area(cpl_grd(k_rho)%grd_name, n_xi_rho, n_eta_rho, a_rho)
      DEALLOCATE(a_rho)

      ! U-grid areas
      n_xi = cpl_grd(k_u)%dims_g(1)
      n_eta = cpl_grd(k_u)%dims_g(2)
      ALLOCATE(a_u(n_xi,n_eta))
      a_u(:,:) = 0.25 * (e1(1:n_xi_rho-1,:) + e1(2:n_xi_rho,:))   &
         &            * (e2(1:n_xi_rho-1,:) + e2(2:n_xi_rho,:))
      CALL oasis_write_area(cpl_grd(k_u)%grd_name, n_xi, n_eta, a_u)
      DEALLOCATE(a_u)

      ! V-grid areas
      n_xi = cpl_grd(k_v)%dims_g(1)
      n_eta = cpl_grd(k_v)%dims_g(2)
      ALLOCATE(a_v(n_xi,n_eta))
      a_v(:,:) = 0.25 * (e1(:,1:n_eta_rho-1) + e1(:,2:n_eta_rho))   &
         &            * (e2(:,1:n_eta_rho-1) + e2(:,2:n_eta_rho))
      CALL oasis_write_area(cpl_grd(k_v)%grd_name, n_xi, n_eta, a_v)
      DEALLOCATE(a_v)

      ! Deallocate scale factors
      DEALLOCATE(e1, e2)

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
      INTEGER (KIND=ip_intwp_p) :: oas_type=OASIS_Real   ! default data type
      LOGICAL :: oas_act=.TRUE.   ! default laction
      INTEGER :: ierr=OASIS_Success   ! error code returned by oasis_def_var
      !                               ! (give default value in case oas_act is .FALSE.)

      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : Defining OASIS field ", clname
      END IF

      IF (PRESENT(dtype)) oas_type = dtype
      IF (PRESENT(laction)) oas_act = laction

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

   SUBROUTINE romsoc_read_alpha(ncid, grd, alpha)
      ! Description
      ! -----------
      ! Read in coupling coefficient from ROMSOC auxiliary file

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: alpha
      
      IF ((IOASISDEBUGLVL > 0) .AND. (mype == 0)) THEN
         WRITE(*,*) "OAS_ROMS : reading coupling coefficient at ", TRIM(grd%pt), "-points"
      END IF

      ALLOCATE(alpha(grd%imin:grd%imax,grd%jmin:grd%jmax))
      
      CALL oas_roms_read_2d(ncid, 'alpha_'//TRIM(grd%pt), grd, alpha, 'local')
      
   END SUBROUTINE romsoc_read_alpha

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE romsoc_get_proj(ncid, grd, u_proj, v_proj)
      ! Description
      ! -----------
      ! Read in ROMS and COSMO directions and compute projection coefficients
      ! - ML - Maybe consider having the scalar products directly in the ROMSOC
      !        auxiliary file and only read them here.

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
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
      CALL oas_roms_read_3d(ncid, 'cos_u_dir_roms_'//TRIM(grd%pt), grd, cos_u_dir, 3, 'local')
      CALL oas_roms_read_3d(ncid, 'cos_v_dir_roms_'//TRIM(grd%pt), grd, cos_v_dir, 3, 'local')
      CALL oas_roms_read_3d(ncid, 'roms_'//TRIM(grd%pt)//'_dir', grd, roms_dir, 3, 'local')

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
   
   SUBROUTINE oas_roms_read_2d(ncid, vname, grd, A, extent)
      ! Description
      ! -----------
      ! Read in 2d variables from netcdf file

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: vname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:), INTENT(INOUT) :: A
      CHARACTER(len=*) :: extent
      
      ! local variables
      INTEGER, DIMENSION(2) :: start, count
      INTEGER :: var_id, ierr

      ierr = nf90_inq_varid(ncid, TRIM(vname), var_id)
      
      IF (ierr == NF90_NOERR) THEN
         
         IF (TRIM(extent) == 'global') THEN
            start = (/1, 1/)
            count = grd%dims_g
         ELSEIF (TRIM(extent) == 'local') THEN
            start = grd%start_g
            count = grd%dims_l
         ELSE
            WRITE(*,*) "ERROR in oas_roms_read_2d : extent has to be either 'local' or 'global'"
            CALL abort
         END IF
         
         ierr = nf90_get_var(ncid, var_id, A, start=start, count=count)
         
         IF (ierr /= NF90_NOERR) THEN
            WRITE(*,*) TRIM(vname), 'oas_roms_read_2d', nf90_strerror(ierr)
            CALL abort
         END IF
         
      ELSE
         
         WRITE(*,*) TRIM(vname), 'oas_roms_read_2d'
         CALL abort
         
      END IF
      
   END SUBROUTINE oas_roms_read_2d

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE oas_roms_read_3d(ncid, vname, grd, A, dim3, extent)
      ! Description
      ! -----------
      ! Read in 3d variables from netcdf file

      ! Arguments
      INTEGER, INTENT(IN) :: ncid
      CHARACTER(len=*), INTENT(IN) :: vname
      TYPE(OAS_GRID), INTENT(IN) :: grd
      REAL(KIND=8), DIMENSION(:,:,:), INTENT(INOUT) :: A
      INTEGER, INTENT(IN) :: dim3
      CHARACTER(len=*), INTENT(IN) :: extent
      
      ! local variables
      INTEGER, DIMENSION(3) :: start, count
      INTEGER :: var_id, ierr

      ierr = nf90_inq_varid(ncid, TRIM(vname), var_id)
      
      IF (ierr == NF90_NOERR) THEN
         
         IF (TRIM(extent) == 'global') THEN
            start = (/1, 1, 1/)
            count = (/grd%dims_g(1), grd%dims_g(2), dim3/)
         ELSEIF (TRIM(extent) == 'local') THEN
            start = (/grd%start_g(1), grd%start_g(2), 1/)
            count = (/grd%dims_l(1), grd%dims_l(2), dim3/)
         ELSE
            WRITE(*,*) "ERROR in oas_roms_read_3d : extent has to be either 'local' or 'global'"
            CALL abort
         END IF
         
         ierr = nf90_get_var(ncid, var_id, A, start=start, count=count)
         
         IF (ierr /= NF90_NOERR) THEN
            WRITE(*,*) TRIM(vname), 'oas_roms_read_3d', nf90_strerror(ierr)
            CALL abort
         END IF
      
      ELSE
         
         WRITE(*,*) TRIM(vname), 'oas_roms_read_3d'
         CALL abort
         
      END IF
      
   END SUBROUTINE oas_roms_read_3d
   
   
END MODULE oas_roms_def
