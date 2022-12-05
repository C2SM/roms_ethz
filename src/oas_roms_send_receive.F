#include "cppdefs.h"
      MODULE oas_roms_send_receive
         ! Description
         ! -----------
         ! Module holding sending and receiving routines called by the original ROMS source

         ! Notes
         ! -----
         ! This is written in an .FF90 file and FIXED FORMAT in order to be able to INCLUDE
         ! ROMS .h files and run mpc on them.

         ! Authors
         ! -------
         ! Matthieu Leclair- ETHZ

         USE oas_roms_exchange, ONLY: oas_roms_snd, oas_roms_dbg_snd,
     &                                oas_roms_rcv, oas_roms_dbg_rcv

         USE oas_roms_data, ONLY: IOASISDEBUGLVL, l_oas_seq,
     &                            ssnd, srcv, cpl_grd,
     &                            k_rho, k_u, k_v,
     &                            oas_itemp, oas_SSU_U, oas_SSU_V,
     &                            oas_SSV_U, oas_SSV_V,
     &                            oas_UST_U, oas_VST_U,
     &                            oas_UST_V, oas_VST_V, oas_NHF,
     &                            oas_SWR, oas_TEP,
     &                            alpha_rho, alpha_u, alpha_v,
     &                            u_cos_proj_u, v_cos_proj_u,
     &                            u_cos_proj_v, v_cos_proj_v,
     &                            l_snd_sst, l_snd_sm

         IMPLICIT NONE

         PRIVATE

         PUBLIC :: oas_roms_send, oas_roms_receive

      CONTAINS


         SUBROUTINE oas_roms_send(oas_step)
            !**** *oas_roms_prepsnd*  - 
            !
            !     Purpose.
            !     --------
            !     Send coupling fields to Atmospheric model
            !
            !**   Interface.
            !     ----------
            !       *CALL*  *oas_roms_send*
            !
            !     Input:
            !     -----
            !
            !     Output:
            !     ------
            !      
            !
            !     Method:
            !     ------
            !       
            !
            !     Externals:
            !     ---------
            !      oas_roms_vardef
            !
            !
            !     Author:
            !     -------
            !       D. Byrne ; ETHZ
            !
            !     Modifications:
            !     --------------
            !       Matthieu Leclair - ETHZ
            !

            INTEGER :: oas_step, oas_dt
            ! => itemp, N, mynode:
#include "param.h"
            ! => nstp, dt, ntimes:
#include "scalars.h"
            ! => t, u, v:
#include "ocean3d.h"

            
            ! Skip if sequential coupling mode and last time step
            ! ---------------------------------------------------
            IF (l_oas_seq .AND. oas_step == ntimes) RETURN

            oas_dt = oas_step * INT(dt)

            ! Update fields for sending
            ! -------------------------
            IF (l_snd_sst) THEN
                ssnd(oas_itemp)%pdata =
     &             t(cpl_grd(k_rho)%imin:cpl_grd(k_rho)%imax,
     &             cpl_grd(k_rho)%jmin:cpl_grd(k_rho)%jmax,N,nstp,itemp)
            ENDIF
            IF (l_snd_sm) THEN
            ssnd(oas_SSU_U)%pdata =
     &           u(cpl_grd(k_u)%imin:cpl_grd(k_u)%imax,
     &             cpl_grd(k_u)%jmin:cpl_grd(k_u)%jmax,N,nstp)
            ssnd(oas_SSU_V)%pdata =
     &           u(cpl_grd(k_u)%imin:cpl_grd(k_u)%imax,
     &             cpl_grd(k_u)%jmin:cpl_grd(k_u)%jmax,N,nstp)
            ssnd(oas_SSV_U)%pdata =
     &           v(cpl_grd(k_v)%imin:cpl_grd(k_v)%imax,
     &             cpl_grd(k_v)%jmin:cpl_grd(k_v)%jmax,N,nstp)
            ssnd(oas_SSV_V)%pdata =
     &           v(cpl_grd(k_v)%imin:cpl_grd(k_v)%imax,
     &             cpl_grd(k_v)%jmin:cpl_grd(k_v)%jmax,N,nstp)
            ENDIF

            ! Write Debug File
            ! ----------------
            ! IF ((IOASISDEBUGLVL == 1) .OR. (IOASISDEBUGLVL == 3)) THEN
            !    IF (mynode==0) WRITE(6,*) 'ROMS:OASSND_DT', oas_dt
            !    CALL oas_roms_dbg_snd(oas_step)
            ! ENDIF

            ! Send the Coupling Fields
            ! ------------------------
            CALL oas_roms_snd(oas_dt)

            ! - ML - Figure out which method is better, through oasis_put directly
            !        with write_restart=.TURE. or with the custom oas_roms_dbg_snd
            !        routine. If we stick to oasis_put we can remove oas_roms_dbg_snd
            !        in the long run.
            ! IF ( IOASISDEBUGLVL == 3 ) THEN
            !    WRITE(6,*) ' RST_roms.nc restart file written'
            !    WRITE(6,*) ' we stop simulation'
            !    STOP
            ! ENDIF

         END SUBROUTINE oas_roms_send

         ! ----------------------------------------------------------------------------------- !

         SUBROUTINE oas_roms_receive(oas_step)                 
            !**** *oas_roms_receive*  - 
            !
            !     Purpose.
            !     --------
            !     Receive coupling fields from Atmospheric model and convert to ocean forcing
            !
            !**   Interface.
            !     ----------
            !       *CALL*  *oas_roms_receive*
            !
            !     Input:
            !     -----
            !
            !     Output:
            !     ------
            !      
            !
            !     Method:
            !     ------
            !       
            !
            !     Externals:
            !     ---------
            !      oas_roms_vardef
            !
            !
            !     Author:
            !     -------
            !       D. Byrne - ETHZ
            !     
            !     Modifications:
            !     --------------
            !       Matthieu Leclair - ETHZ
            !

            ! => itemp, isalt, N, mynode
#include "param.h"
            ! => nstp, dt, tdays, nrhs, rho0, Cp, deg2rad
#include "scalars.h"
            ! => t, Hz
#include "ocean3d.h"
            ! => sustr, svstr, srflx, stflx
#include "forces.h"
            ! => hbls
#include "ncvars.h"
            INTEGER ::  oas_dt, oas_step, jn, i, j
            REAL :: cff_wstr, cff_hflx, cff_fwflx
            REAL :: zflx_1, zflx_2
            ! Skip if sequential coupling mode and first time step
            ! ----------------------------------------------------
            IF (l_oas_seq .AND. oas_step == 0) RETURN
            

            ! Receive fields when necessary
            ! -----------------------------
            oas_dt = oas_step * INT(dt)
            CALL oas_roms_rcv(oas_dt)

            ! Set conversion coefficients
            ! ---------------------------    
            cff_wstr = 1./rho0
            cff_hflx = 1./(rho0*Cp)
            cff_fwflx = 0.01/86400

            ! Momentum Flux
            ! -------------
            DO j = cpl_grd(k_u)%jmin, cpl_grd(k_u)%jmax
               DO i = cpl_grd(k_u)%imin, cpl_grd(k_u)%imax
                  zflx_1 = u_cos_proj_u(i,j)
     &                     * srcv(oas_UST_U)%pdata(i,j)
     &                   + v_cos_proj_u(i,j)
     &                     * srcv(oas_VST_U)%pdata(i,j)
                  sustr(i,j) = (1.0-alpha_u(i,j)) * sustr(i,j)
     &                            + alpha_u(i,j) * cff_wstr * zflx_1
               ENDDO
            ENDDO

            DO j = cpl_grd(k_v)%jmin, cpl_grd(k_v)%jmax
               DO i = cpl_grd(k_v)%imin, cpl_grd(k_v)%imax
                  zflx_1 = u_cos_proj_v(i,j)
     &                     * srcv(oas_UST_V)%pdata(i,j)
     &                   + v_cos_proj_v(i,j)
     &                     * srcv(oas_VST_V)%pdata(i,j)
                  svstr(i,j) = (1.0-alpha_v(i,j)) * svstr(i,j)
     &                            + alpha_v(i,j) * cff_wstr * zflx_1
               ENDDO
            ENDDO

            ! Solar Radiative Flux
            ! --------------------
            DO j = cpl_grd(k_rho)%jmin, cpl_grd(k_rho)%jmax
               DO i = cpl_grd(k_rho)%imin, cpl_grd(k_rho)%imax
                  zflx_1 = cff_hflx * srcv(oas_SWR)%pdata(i,j)
                  srflx(i,j) = (1.0-alpha_rho(i,j)) * srflx(i,j)
     &                            + alpha_rho(i,j) * zflx_1

                  ! Net Heat Flux
                  ! -------------
                  zflx_2 = cff_hflx * srcv(oas_NHF)%pdata(i,j) + zflx_1
                  !
                  ! Restrict stflx to prevent surface temperature to go below -2
                  ! degrees C.
                  stflx(i,j,itemp) =
     &                 (1.0-alpha_rho(i,j)) * stflx(i,j,itemp)
     &                    + alpha_rho(i,j) * zflx_2


                  ! Fresh Water Flux
                  ! ----------------
                  zflx_1 = cff_fwflx * srcv(oas_TEP)%pdata(i,j)
     &                               * t(i,j,N,nrhs,isalt)
                  stflx(i,j,isalt) =
     &                 (1.0-alpha_rho(i,j)) * stflx(i,j,isalt)
     &                    + alpha_rho(i,j) * zflx_1
               ENDDO
            ENDDO

            ! Write Debug File
            ! ----------------
     !        CALL oas_roms_dbg_rcv(oas_step,
     ! &                            "SUSTR", sustr           , k_u  ,
     ! &                            "SVSTR", svstr           , k_v  ,
     ! &                            "NHF"  , stflx(:,:,itemp), k_rho,
     ! &                            "SWR"  , srflx           , k_rho,
     ! &                            "FRESH", stflx(:,:,isalt), k_rho )

         END SUBROUTINE oas_roms_receive

      END MODULE oas_roms_send_receive
