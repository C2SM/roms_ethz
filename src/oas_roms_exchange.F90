MODULE oas_roms_exchange
   ! Description
   ! -----------
   ! Module holding subroutines for  exchanging fields with the OASIS coupler.
   
   ! Authors
   ! -------
   ! Matthieu Leclair - ETHZ

   ! Modules used
   ! ------------

   USE MPI

   USE netcdf, ONLY: nf90_create, nf90_enddef,       &
      &              nf90_open, nf90_close,          &
      &              nf90_def_dim, nf90_inq_varid,   &
      &              nf90_def_var, nf90_put_var,     &
      &              NF90_CLOBBER, NF90_UNLIMITED,   &
      &              NF90_WRITE, NF90_DOUBLE
   
   USE mod_oasis_getput_interface, ONLY: oasis_get, oasis_put

   USE mod_oasis_parameters, ONLY: OASIS_Recvd, OASIS_FromRest,        &
      &                            OASIS_RecvOut, OASIS_FromRestOut,   &
      &                            OASIS_Sent, OASIS_ToRest,           &
      &                            OASIS_SentOut, OASIS_ToRestOut

   USE oas_roms_data, ONLY: kl_comm,                             &
      &                     OAS_GRID, cpl_grd, k_rho, k_u, k_v,  &
      &                     oas_itemp, oas_UST_U, oas_VST_U,     &
      &                     oas_UST_V, oas_VST_V, oas_NHF,       &
      &                     oas_SWR, oas_TEP,                    &
      &                     srcv, ssnd, krcv, ksnd,              &
      &                     IOASISDEBUGLVL
   

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: oas_roms_snd,   &
      &      oas_roms_rcv,   &
      &      oas_roms_dbg_rcv,   &
      &      oas_roms_dbg_snd

   ! Module variables
   ! ----------------
   TYPE :: dbg_file
      INTEGER :: ncid   ! netcdf file id
      INTEGER, DIMENSION(3,3) :: dim_ids ! dimension ids
   END TYPE dbg_file

   INTEGER(kind=4), PARAMETER :: NULOUT=6
   
   
CONTAINS

   ! *********************************************************************************** !
   !                                 PUBLIC SUBROUTINES                                  !
   ! *********************************************************************************** !

   SUBROUTINE oas_roms_rcv(kstep)
      ! Description
      ! -----------
      ! Receive Fields from Atmospheric Model

      ! Arguments
      INTEGER, INTENT(IN) :: kstep   ! ocean time-step in seconds
      
      ! Local variables
      INTEGER :: kinfo, jn

      DO jn=1, krcv
         IF (srcv(jn)%laction) THEN
            ! Fill in coupling field data at coupling time steps
            ! - ML - Still don't get exactly why the exfld array is necessary
            CALL oasis_get(srcv(jn)%nid, kstep, cpl_grd(srcv(jn)%k_pt)%exfld, kinfo)
            IF (kinfo == OASIS_Recvd .OR. kinfo == OASIS_FromRest .OR.   &
               kinfo == OASIS_RecvOut .OR. kinfo == OASIS_FromRestOut) THEN
               srcv(jn)%pdata(:,:) = cpl_grd(srcv(jn)%k_pt)%exfld(:,:)
            ENDIF
         ENDIF
      ENDDO

   END SUBROUTINE oas_roms_rcv

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE oas_roms_snd(kstep)
      ! Description
      ! -----------
      ! Send Fields to Atmospheric Model

      ! Arguments
      INTEGER, INTENT(IN) :: kstep   ! ocean time-step in seconds
      
      ! Local variables
      INTEGER :: kinfo, jn

      DO jn=1, ksnd
         IF (ssnd(jn)%laction) THEN

            ! Call OASIS at each time step but field sent to other model only at coupling time step
            ! (accumulation otherwise, if asked in the namcouple configuration file)
            CALL oasis_put(ssnd(jn)%nid, kstep, ssnd(jn)%pdata, kinfo,   &
               & write_restart=(IOASISDEBUGLVL==3))

            ! - ML - Is that really necessary? Comment out for now
            ! IF ( kinfo .EQ. OASIS_Sent .OR. kinfo .EQ. OASIS_ToRest .OR.   &
            !    & kinfo .EQ. OASIS_SentOut  .OR. kinfo .EQ. OASIS_ToRestOut ) THEN
            !    ssnd(jn)%pdata(:,:) = 0.
            ! ENDIF

            ! - ML - Why these dbg prints just for sending, not receiving ??
            IF (IOASISDEBUGLVL > 1) THEN
               WRITE(NULOUT,*) '****************'
               WRITE(NULOUT,*) 'ROMS sent data:'
               WRITE(NULOUT,*) 'oasis_put: ', ssnd(jn)%clname
               WRITE(NULOUT,*) 'oasis_put: ivarid '  , ssnd(jn)%nid
               WRITE(NULOUT,*) 'oasis_put: kstep', kstep
               WRITE(NULOUT,*) 'oasis_put: info ', kinfo
               WRITE(NULOUT,*) '     - Minimum value is ', MINVAL(ssnd(jn)%pdata(:,:))
               WRITE(NULOUT,*) '     - Maximum value is ', MAXVAL(ssnd(jn)%pdata(:,:))
               WRITE(NULOUT,*) '     -     Sum value is ', SUM(ssnd(jn)%pdata(:,:))
               WRITE(NULOUT,*) '****************'
            ENDIF
         END IF
      END DO

   END SUBROUTINE oas_roms_snd

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE oas_roms_dbg_rcv(oas_step, vname1, pdata1, k_pt1, vname2, pdata2, k_pt2,   &
      &                                  vname3, pdata3, k_pt3, vname4, pdata4, k_pt4,   &
      &                                  vname5, pdata5, k_pt5                         )
      ! Description
      ! -----------
      ! Write debug file for received fields

      ! Arguments
      INTEGER, INTENT(IN) :: oas_step
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: vname1, vname2, vname3, vname4, vname5
      REAL(KIND=8), DIMENSION(:,:), INTENT(IN), OPTIONAL :: pdata1, pdata2, pdata3, pdata4, pdata5
      INTEGER, INTENT(IN), OPTIONAL :: k_pt1, k_pt2, k_pt3, k_pt4, k_pt5
      
      
      ! Local varaibles
      INTEGER :: status, ncid, pe, mype, npes, ierr, kv
      TYPE(dbg_file) :: rcv_dbg_file
      CHARACTER(len=*), PARAMETER :: file_name="debugout_roms_rcv.nc"
      
      CALL MPI_Comm_rank(kl_comm, mype, ierr)
      CALL MPI_Comm_size(kl_comm, npes, ierr)

      ! Create and define netcdf file
      IF (mype == 0 .AND. oas_step == 1) THEN
         ! Create netcdf file and dimensions
         CALL dbg_def_file(file_name, rcv_dbg_file)
         ! Create variables
         DO kv = 1, krcv
            CALL dbg_def_var(rcv_dbg_file, srcv(kv)%clname, srcv(kv)%k_pt)
         END DO
         IF (PRESENT(vname1)) CALL dbg_def_var(rcv_dbg_file, TRIM(vname1), k_pt1)
         IF (PRESENT(vname2)) CALL dbg_def_var(rcv_dbg_file, TRIM(vname2), k_pt2)
         IF (PRESENT(vname3)) CALL dbg_def_var(rcv_dbg_file, TRIM(vname3), k_pt3)
         IF (PRESENT(vname4)) CALL dbg_def_var(rcv_dbg_file, TRIM(vname4), k_pt4)
         IF (PRESENT(vname5)) CALL dbg_def_var(rcv_dbg_file, TRIM(vname5), k_pt5)
         ! End netcdf file definition
         status = nf90_enddef(rcv_dbg_file%ncid)
         ! Close netcdf file
         status = nf90_close(rcv_dbg_file%ncid)
      ENDIF

      ! Fill in netcdf file
      DO pe = 0, npes - 1
         ! Each pe writes sequentially
         CALL MPI_Barrier(kl_comm, ierr)
         IF (mype .EQ. pe) THEN
            ! open netcdf file
            status = nf90_open(file_name, NF90_WRITE, ncid)
            ! Fill in variables
            DO kv = 1, krcv
               CALL dbg_fill_var(ncid, srcv(kv)%clname, cpl_grd(srcv(kv)%k_pt), srcv(kv)%pdata, oas_step)
            END DO
            IF (PRESENT(vname1)) CALL dbg_fill_var(ncid, TRIM(vname1), cpl_grd(k_pt1), pdata1, oas_step)
            IF (PRESENT(vname2)) CALL dbg_fill_var(ncid, TRIM(vname2), cpl_grd(k_pt2), pdata2, oas_step)
            IF (PRESENT(vname3)) CALL dbg_fill_var(ncid, TRIM(vname3), cpl_grd(k_pt3), pdata3, oas_step)
            IF (PRESENT(vname4)) CALL dbg_fill_var(ncid, TRIM(vname4), cpl_grd(k_pt4), pdata4, oas_step)
            IF (PRESENT(vname5)) CALL dbg_fill_var(ncid, TRIM(vname5), cpl_grd(k_pt5), pdata5, oas_step)
            ! Close netcdf file
            status = nf90_close(ncid)
         ENDIF
      ENDDO
      
   END SUBROUTINE oas_roms_dbg_rcv

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE oas_roms_dbg_snd(oas_step)
      ! Description
      ! -----------
      ! Write debug file for sent fields

      ! Arguments
      INTEGER, INTENT(IN) :: oas_step
      
      ! Local varaibles
      INTEGER :: status, ncid, pe, mype, npes, ierr, kv
      TYPE(dbg_file) :: snd_dbg_file
      CHARACTER(len=*), PARAMETER :: file_name="RST_roms.nc"
      
      CALL MPI_Comm_rank(kl_comm, mype, ierr)
      CALL MPI_Comm_size(kl_comm, npes, ierr)

      ! Create and define netcdf file
      IF (mype == 0 .AND. oas_step == 1) THEN
         ! Create netcdf file and dimensions
         CALL dbg_def_file(file_name, snd_dbg_file)
         ! Create variables
         DO kv = 1, ksnd
            CALL dbg_def_var(snd_dbg_file, ssnd(kv)%clname, ssnd(kv)%k_pt)
         END DO
         ! End netcdf file definition
         status = nf90_enddef(snd_dbg_file%ncid)
         ! Close netcdf file
         status = nf90_close(snd_dbg_file%ncid)
      ENDIF

      ! Fill in netcdf file
      DO pe = 0, npes - 1
         ! Each pe writes sequentially
         CALL MPI_Barrier(kl_comm, ierr)
         IF (mype .EQ. pe) THEN
            ! open netcdf file
            status = nf90_open(file_name, NF90_WRITE, ncid)
            ! Fill in variables
            DO kv = 1, ksnd
               CALL dbg_fill_var(ncid, ssnd(kv)%clname, cpl_grd(ssnd(kv)%k_pt), ssnd(kv)%pdata, oas_step)
            END DO
            ! Close netcdf file
            status = nf90_close(ncid)
         ENDIF
      ENDDO
      
   END SUBROUTINE oas_roms_dbg_snd
   

   ! *********************************************************************************** !
   !                                 PRIVATE SUBROUTINES                                 !
   ! *********************************************************************************** !

   SUBROUTINE dbg_def_file(file_name, dfile)
      ! Description
      ! -----------
      ! Init netcdf debug file
      
      ! Arguments
      CHARACTER(len=*), INTENT(IN) :: file_name
      TYPE(dbg_file), INTENT(OUT) :: dfile
      
      ! Local variables
      INTEGER :: status, ncid,   &
         &       xi_rho_id, eta_rho_id, xi_u_id, eta_v_id, step_id

      ! Get netcdf file id
      status = nf90_create(TRIM(file_name), NF90_CLOBBER, dfile%ncid)
      ! Decalre dimensions and get ids
      status = nf90_def_dim(ncid, "x", cpl_grd(k_rho)%dims_g(1), xi_rho_id)
      status = nf90_def_dim(ncid, "y", cpl_grd(k_rho)%dims_g(2), eta_rho_id)
      status = nf90_def_dim(ncid, "xu", cpl_grd(k_u)%dims_g(1), xi_u_id)
      status = nf90_def_dim(ncid, "yv", cpl_grd(k_v)%dims_g(2), eta_v_id)
      status = nf90_def_dim(ncid, "step", NF90_UNLIMITED, step_id)
      dfile%dim_ids(:,k_rho) = (/ xi_rho_id, eta_rho_id, step_id /)
      dfile%dim_ids(:,k_u) = (/ xi_u_id, eta_rho_id, step_id /)
      dfile%dim_ids(:,k_v) = (/ xi_rho_id, eta_v_id, step_id /)
      
   END SUBROUTINE dbg_def_file

   ! ----------------------------------------------------------------------------------- !
   
   SUBROUTINE dbg_def_var(dfile, var_name, k_pt)
      ! Description
      ! -----------
      ! Define variable in a debug netcdf file

      ! Arguments
      TYPE(dbg_file), INTENT(IN) :: dfile   ! debug file
      CHARACTER(len=*), INTENT(IN) :: var_name   ! netcdf variable name
      INTEGER, INTENT(IN) :: k_pt   ! grid point type
      
      ! Local varapeles
      INTEGER :: status, var_id

      status = nf90_def_var(dfile%ncid, var_name, NF90_DOUBLE, dfile%dim_ids(:,k_pt), var_id)
      
   END SUBROUTINE dbg_def_var

   ! ----------------------------------------------------------------------------------- !

   SUBROUTINE dbg_fill_var(ncid, var_name, grd, pdata, oas_step)
      ! Description
      ! -----------
      ! Fill in debug netcdf file with data knowing grid point type

      ! Arguments
      INTEGER, INTENT(IN) :: ncid   ! netcdf file id
      CHARACTER(len=*), INTENT(IN) :: var_name   ! netcdf variable name
      TYPE(OAS_GRID), INTENT(IN) :: grd   ! grid type
      REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: pdata   ! data to fill in variable
      INTEGER, INTENT(IN) :: oas_step   ! coupling time step
      
      ! Local variables
      INTEGER :: status, var_id
      INTEGER, DIMENSION(3) :: start, count

      status = nf90_inq_varid(ncid, var_name , var_id)

      status = nf90_put_var(ncid, var_id, pdata(grd%imin:grd%imax, grd%jmin:grd%jmax),   &
         &                  start=(/ grd%start_g(1), grd%start_g(2), (oas_step-1) /),    &
         &                  count=(/ grd%dims_l(1), grd%dims_l(2), 1 /) )
     
   END SUBROUTINE dbg_fill_var


END MODULE oas_roms_exchange
