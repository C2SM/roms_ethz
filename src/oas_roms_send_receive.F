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
     &                            l_snd_sst, l_snd_sm,
     &                            sustr_a, svstr_a, srflx_a, stflx_a,
     &                            oas_time

         IMPLICIT NONE

         PRIVATE

         PUBLIC :: oas_roms_send, oas_roms_receive, oas_merge_forces

      CONTAINS


         SUBROUTINE oas_roms_send(oas_step)
            !
            !     Purpose.
            !     --------
            !     Send  fields to OASIS 
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

            ! => itemp, isalt, NT
#include "param.h"
            ! => nstp, dt, tdays, nrhs, rho0, Cp, deg2rad
#include "scalars.h"
            ! => t, Hz
#include "ocean3d.h"
            ! => 
!#include "forces.h"
            ! => hbls
!#include "ncvars.h"
            INTEGER ::  oas_dt
            INTEGER :: oas_step, jn, i, j
            INTEGER, SAVE :: it = 1
            REAL :: cff_wstr, cff_hflx, cff_fwflx
            REAL :: zflx_1, zflx_2
            LOGICAL :: updates
            LOGICAL, SAVE :: initial_call = .TRUE.

            ! Skip if sequential coupling mode and first time step
            ! ----------------------------------------------------
            IF (l_oas_seq .AND. oas_step == 0) RETURN
            

            ! Receive fields when necessary
            ! -----------------------------
            oas_dt = oas_step * dt
            CALL oas_roms_rcv(oas_dt,updates)
            IF ( .NOT. updates ) RETURN
            it = mod(it+1,2)
            oas_time(it) = oas_dt

            ! Set conversion coefficients
            ! ---------------------------    
            cff_wstr = 1./rho0
            cff_hflx = 1./(rho0*Cp)
            cff_fwflx = 0.01/86400

            ! Momentum Flux
            ! -------------
            DO j = cpl_grd(k_u)%jmin, cpl_grd(k_u)%jmax
               DO i = cpl_grd(k_u)%imin, cpl_grd(k_u)%imax
                  sustr_a(i,j,it) = ( u_cos_proj_u(i,j)
     &                     * srcv(oas_UST_U)%pdata(i,j)
     &                   + v_cos_proj_u(i,j)
     &                     * srcv(oas_VST_U)%pdata(i,j) ) * cff_wstr
               ENDDO
            ENDDO

            DO j = cpl_grd(k_v)%jmin, cpl_grd(k_v)%jmax
               DO i = cpl_grd(k_v)%imin, cpl_grd(k_v)%imax
                  svstr_a(i,j,it) = ( u_cos_proj_v(i,j)
     &                     * srcv(oas_UST_V)%pdata(i,j)
     &                   + v_cos_proj_v(i,j)
     &                     * srcv(oas_VST_V)%pdata(i,j) ) * cff_wstr
               ENDDO
            ENDDO

            DO j = cpl_grd(k_rho)%jmin, cpl_grd(k_rho)%jmax
               DO i = cpl_grd(k_rho)%imin, cpl_grd(k_rho)%imax
                  ! Solar Radiative Flux
                  ! --------------------
                  srflx_a(i,j,it) = cff_hflx * srcv(oas_SWR)%pdata(i,j)

                  ! Net Heat Flux (total lat+sens+tot_rad)
                  ! -------------
                  stflx_a(i,j,itemp,it) = cff_hflx * srcv(oas_NHF)%pdata(i,j) 
     &                                    + srflx_a(i,j,it)
                  ! Fresh Water Flux
                  ! ----------------
                  stflx_a(i,j,isalt,it) = cff_fwflx * srcv(oas_TEP)%pdata(i,j)
     &                               * t(i,j,N,nrhs,isalt)
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

         if (initial_call) then
            ! Use Initial forcing data as prior data
            ! This breaks EXACT RESTART!
            initial_call = .FALSE.
            i = it
            it = mod(it+1,2)
            oas_time(it) = oas_time(i) - oas_dt 
            sustr_a(:,:,it) = sustr_a(:,:,i)
            svstr_a(:,:,it) = svstr_a(:,:,i)
            srflx_a(:,:,it) = srflx_a(:,:,i)
            stflx_a(:,:,:,it) = stflx_a(:,:,:,i)
         ENDIF

         END SUBROUTINE oas_roms_receive

         SUBROUTINE oas_merge_forces
            IMPLICIT NONE
            ! also contains mynode
#include "param.h"
            ! istr,iend,jstr,jend:
#include "compute_tile_bounds.h"
            ! sustr, svstr, srflx, stflx are in forces.h
#include "forces.h"
            !  iic, ntstart are in scalars.h
#include "scalars.h"
            INTEGER :: i1, i2, it1, it2
            REAL    :: w1, w2 ! time slice weights
            INTEGER :: imid,jmid
            !
            ! Interpolate the atm forces in time to the current time
            ! and use the alpha_x weights to merge these forces into 
            ! the pre-existing forces outside the coupling region where alpha_? = 0.0 
            !
            if( oas_time(1) > oas_time(0)) then
                    i1=0
                    i2=1
            else
                    i2=0
                    i2=1
            endif
            it2 = oas_time(i2) ! time of current ...
            it1 = oas_time(i1) ! and past atm forces.
 
            ! Weights for linear time interpolation to current time (it)
            w2 = (iic-ntstart - it1)/(it2-it1)
            w1 = 1.0 - w2  
#ifdef COUP_DEBUG
       imid=(istr+iend)/2; jmid=(jstr+jend)/2
       write(unit=107,*) 'era: iic:, ', iic, ', ',sustr(imid,jmid)
     &                               , ', ',svstr(imid,jmid)
     &                               , ', ',srflx(imid,jmid)
     &                               , ', ',stflx(imid,jmid,1)
     &                               , ', ',stflx(imid,jmid,2)
#endif
            ! Remark:  Array operations below
            sustr = (1.0 - alpha_u) * sustr
     &                   + alpha_u  * ( w1 * sustr_a(:,:,i1) 
     &                                + w2 * sustr_a(:,:,i2) ) 
            svstr = (1.0 - alpha_v) * svstr + 
     &                   + alpha_v  * ( w1 * svstr_a(:,:,i1) 
     &                                + w2 * svstr_a(:,:,i2) ) 
            srflx = (1.0 - alpha_rho) * srflx
     &                   + alpha_rho  * ( w1 * srflx_a(:,:,i1) 
     &                                  + w2 * srflx_a(:,:,i2) ) 
            stflx(:,:,itemp) = (1.0 - alpha_rho) * stflx(:,:,itemp) 
     &                   + alpha_rho  * ( w1 * stflx_a(:,:,itemp,i1) 
     &                                  + w2 * stflx_a(:,:,itemp,i2) )
            stflx(:,:,isalt) = (1.0 - alpha_rho) * stflx(:,:,isalt) 
     &                   + alpha_rho  * ( w1 * stflx_a(:,:,isalt,i1) 
     &                                  + w2 * stflx_a(:,:,isalt,i2) )
#ifdef COUP_DEBUG
       if (mynode ==60) then
       imid=(istr+iend)/2; jmid=(jstr+jend)/2
       write(unit=107,*) 'merged: iic:, ', iic, ', ',sustr(imid,jmid)
     &                               , ', ',svstr(imid,jmid)
     &                               , ', ',srflx(imid,jmid)
     &                               , ', ',stflx(imid,jmid,1)
     &                               , ', ',stflx(imid,jmid,2)
       write(unit=107,*) 
       endif
#endif
         
         END SUBROUTINE oas_merge_forces

       END MODULE oas_roms_send_receive
