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
     &                            oas_tnow, oas_tprior,
     &                            oas_inow, oas_iprior

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
            INTEGER :: oas_step, jn, i, j, it
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
            oas_iprior=oas_inow
            it = mod(oas_iprior+1,2)
            oas_inow = it

            oas_tprior = oas_tnow
            ! time extrapolation to future
            ! more accurate but maybe dangeous (unstable)
            ! oas_tnow = oas_dt
            ! Safer to pretent the current time is 
            ! one coupling time step ahead:
            oas_tnow = oas_dt + dt
            write(*,*) 'oas_dt,oas_tnow,dt,',oas_dt,oas_tnow,dt

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
               stflx_a(i,j,itemp,it) = cff_hflx * 
     &                srcv(oas_NHF)%pdata(i,j) + srflx_a(i,j,it)
               ! Fresh Water Flux
               ! ----------------
               stflx_a(i,j,isalt,it) = cff_fwflx * 
     &                srcv(oas_TEP)%pdata(i,j) * t(i,j,N,nrhs,isalt)
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
            ! At start up current forcing data 
            ! also as prior forcing
            ! This breaks EXACT RESTART!
            initial_call = .FALSE.
            i = it
            it = mod(it+1,2)
            oas_tprior = oas_tnow
            sustr_a(:,:,it) = sustr_a(:,:,i)
            svstr_a(:,:,it) = svstr_a(:,:,i)
            srflx_a(:,:,it) = srflx_a(:,:,i)
            stflx_a(:,:,:,it) = stflx_a(:,:,:,i)
         ENDIF

         END SUBROUTINE oas_roms_receive

         SUBROUTINE oas_merge_forces(istr,iend,jstr,jend,ierr)
            IMPLICIT NONE
            ! also contains mynode
#include "param.h"
            ! sustr, svstr, srflx, stflx are in forces.h
#include "forces.h"
            !  iic, ntstart are in scalars.h
#include "scalars.h"
            INTEGER :: istr, iend, jstr, jend, ierr
            INTEGER :: i, j, i1, i2, it1, it2
            REAL    :: w1, w2 ! time slice weights
            INTEGER :: imid, jmid
#include "compute_extended_bounds.h"
            !
            ! Interpolate the atm forces in time to the current time
            ! and use the alpha_x weights to merge these forces into 
            ! the pre-existing forces outside the coupling region where alpha_? = 0.0 
 
            ! Weights for linear time interpolation to current time iic-ntstart
            ! for the 2 forcing time slices at times oas_tnow and oas_tprior
            ! stored in index oas_inow and oas_iprior
            !
            ! Note that this is currently a linear time extrapolation
            ! It would be more stable  to interpolate by pretending that
            ! the latest forcing received from the coupler is actually 
            ! one coupling time step ahead.
            w2 = ((iic-ntstart)*dt - oas_tprior)/(oas_tnow-oas_tprior)
            w1 = 1.0 - w2  
#ifdef COUP_DEBUG
      write(*,*) 'oas_tprior,oas_tnow, ',oas_tprior,oas_tnow
      write(*,*) 'iic,ntstart,dt*(iic-ntstart), ',iic,ntstart,dt*float(iic-ntstart)
      write(*,*) 'w1,w2, ',w1,w2
       imid=(istr+iend)/2; jmid=(jstr+jend)/2
       write(unit=107,*) 'Era5: ', iic, ', ',sustr(imid,jmid)
     &                               , ', ',svstr(imid,jmid)
     &                               , ', ',srflx(imid,jmid)
     &                               , ', ',stflx(imid,jmid,1)
     &                               , ', ',stflx(imid,jmid,2)
#endif

       !  taux, tauy 
       do j = jstrR, jendR
       do i = istrR, iendR
          sustr(i,j) = (1.0 - alpha_u(i,j)) * sustr(i,j)
     &         + alpha_u(i,j)  * ( w1 * sustr_a(i,j,oas_iprior)
     &                           + w2 * sustr_a(i,j,oas_inow) )
          svstr(i,j) = (1.0 - alpha_v(i,j)) * svstr(i,j) +
     &         + alpha_v(i,j) * ( w1 * svstr_a(i,j,oas_iprior)
     &                          + w2 * svstr_a(i,j,oas_inow) )
       enddo
       enddo

       ! solar-, net heat- (or temp-), salt- flux
       do j = jstr, jend
       do i = istr, iend
          srflx(i,j) = (1.0 - alpha_rho(i,j)) * srflx(i,j)
     &         + alpha_rho(i,j)  * ( w1 * srflx_a(i,j,oas_iprior)
     &                             + w2 * srflx_a(i,j,oas_inow) )
          stflx(i,j,itemp) = (1.0 - alpha_rho(i,j)) * stflx(i,j,itemp)
     &         + alpha_rho(i,j)  * ( w1 * stflx_a(i,j,itemp,oas_iprior)
     &                             + w2 * stflx_a(i,j,itemp,oas_inow) )
          stflx(i,j,isalt) = (1.0 - alpha_rho(i,j)) * stflx(i,j,isalt)
     &         + alpha_rho(i,j)  * ( w1 * stflx_a(i,j,isalt,oas_iprior)
     &                             + w2 * stflx_a(i,j,isalt,oas_inow) )
       enddo
       enddo

#ifdef COUP_DEBUG
       if (mynode ==60) then
       imid=(istr+iend)/2; jmid=(jstr+jend)/2
       write(unit=108,*) 'Merged: ', iic, ', ',sustr(imid,jmid)
     &                               , ', ',svstr(imid,jmid)
     &                               , ', ',srflx(imid,jmid)
     &                               , ', ',stflx(imid,jmid,1)
     &                               , ', ',stflx(imid,jmid,2)
     &                               , ', ',alpha_rho(imid,jmid)
       endif
#endif
         
         END SUBROUTINE oas_merge_forces

       END MODULE oas_roms_send_receive
