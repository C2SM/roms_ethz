   SUBROUTINE oas_roms_snd( kid, kstep, pdata, kinfo )

   !!---------------------------------------------------------------------
   !!              ***  ROUTINE oas_roms_snd  ***
   !!
   !! ** Purpose : - At each coupling time-step,this routine sends fields
   !!                to the coupler or remote application.
   !!----------------------------------------------------------------------

   use oas_roms_vardef
   !use mod_prism_put_proto
   use mod_oasis

   IMPLICIT NONE

   !! * Arguments
   !!
   INTEGER(kind=4), INTENT( IN    )   :: kid    ! variable intex in the array
   INTEGER(kind=4), INTENT(   OUT )   :: kinfo  ! OASIS info argument
   INTEGER(kind=4), INTENT( IN    )   :: kstep  ! time-step in seconds
   REAL(kind=8), DIMENSION(oas_imin:oas_imax,oas_jmin:oas_jmax), INTENT( IN    )   :: pdata
   !!
   INTEGER(kind=4) :: NULOUT
   !!
   !!--------------------------------------------------------------------
   !
      !
      NULOUT=6
      !
      ! prepare array (only valid shape, without halos) for OASIS
      !
      exfld = pdata

      ! Call OASIS at each time step but field sent to other model only at coupling time step
      ! (accumulation otherwise, if asked in the namcouple configuration file)
      !
      CALL oasis_put( ssnd(kid)%nid, kstep, exfld(:,:), kinfo )

      IF ( IOASISDEBUGLVL > 1 ) THEN
            WRITE(NULOUT,*) '****************'
            WRITE(NULOUT,*) 'ROMS sent data:'
            WRITE(NULOUT,*) 'prism_put: ', ssnd(kid)%clname
            WRITE(NULOUT,*) 'prism_put: ivarid '  , ssnd(kid)%nid
            WRITE(NULOUT,*) 'prism_put:   kstep', kstep
            WRITE(NULOUT,*) 'prism_put:   info ', kinfo
            WRITE(NULOUT,*) '     - Minimum value is ', MINVAL(pdata(:,:))
            WRITE(NULOUT,*) '     - Maximum value is ', MAXVAL(pdata(:,:))
            WRITE(NULOUT,*) '     -     Sum value is ', SUM(pdata(:,:))
            WRITE(NULOUT,*) '****************'
      ENDIF

   END SUBROUTINE oas_roms_snd
