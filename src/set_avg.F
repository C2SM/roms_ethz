#include "cppdefs.h"
#ifdef AVERAGES
!
! We are using CPP to produce 2 similar routines for regular 3 and 2D averages
! (set_avg) and slice averages (set_slavg) out of a common source files.
!
# ifndef AVRSLICE
#  define set_AVR set_avg
#  define set_AVR_tile set_avg_tile
#  define wrtAVR wrtavg
#  define time_AVR time_avg
#  define ntsAVR ntsavg
#  define nAVR navg
#  define ntsAVR ntsavg
#  define zeta_AVR zeta_avg
#  define ubar_AVR ubar_avg
#  define vbar_AVR vbar_avg
#  define u_AVR(i,j,k) u_avg(i,j,k)
#  define v_AVR(i,j,k) v_avg(i,j,k)
#  define w_AVR(i,j,k) w_avg(i,j,k)
#  define t_AVR(i,j,k,itrc) t_avg(i,j,k,itrc)
#  define hbl_AVR hbl_avg
#  define hbbl_AVR hbbl_avg
#  define rho_AVR(i,j,k) rho_avg(i,j,k)
#  define aks_AVR(i,j,k) aks_avg(i,j,k)
#  define akt_AVR(i,j,k) akt_avg(i,j,k)
#  define akv_AVR(i,j,k) akv_avg(i,j,k)
#  define KLOOP(s)  do k=s,N
#  define ENDKLOOP  enddo
#  define KLEVEL   k
# else  /* AVRSLICE */
#  define set_AVR set_slavg
#  define set_AVR_tile set_slavg_tile
#  define wrtAVR wrtslavg
#  define time_AVR time_slavg
#  define ntsAVR ntsslavg
#  define nAVR nslavg
#  define zeta_AVR zeta_slavg
#  define ubar_AVR ubar_slavg
#  define vbar_AVR vbar_slavg
#  define u_AVR(i,j,k) u_slavg(i,j)
#  define v_AVR(i,j,k) v_slavg(i,j)
#  define w_AVR(i,j,k) w_slavg(i,j)
#  define t_AVR(i,j,k,itrc) t_slavg(i,j,itrc)
#  define hbl_AVR hbl_slavg
#  define hbbl_AVR hbbl_slavg
#  define rho_AVR(i,j,k) rho_slavg(i,j)
#  define aks_AVR(i,j,k) aks_slavg(i,j)
#  define akt_AVR(i,j,k) akt_slavg(i,j)
#  define akv_AVR(i,j,k) akv_slavg(i,j)
#  define KLOOP(s)  
#  define ENDKLOOP  
#  define KLEVEL   k2d
# endif

 
      subroutine set_AVR (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_AVR_tile (istr,iend,jstr,jend,tile)
      return
      end
 
      subroutine set_AVR_tile (istr,iend,jstr,jend,tile)
!
! Compute time-averaged fields within a tile.
! ------- ------------- ------ ------ - -----
! Because of syncronization issues, the delayed mode averaging
! procedure is used. This procedure implies that all fields to be
! averaged are sampled during the next time step, rather than at
! the end of the time step when they were computed.
!
! Thought this algorithm results in somewhat awkward controlling
! logic it has the advantage that that all fields to be sampled
! correspond to exactly the same time, which is time step "n".
! Particularly, this is done this way because vertical velocity
! corresponding to the newly computed horizontal velocities
! becomes available only during the following time step.
! The same applies to the density field.
!
! The algorithm consists of three logical blocks: (1) initialization
! of the averages arrays: when mod(iic-1,navg).eq.1 the target arrays
! are set to the first contribution; (2) accumulation of averaged
! data, when mod(iic-1,navg).gt.1; and (3) adding the last
! contribution and scaling.
!
      implicit none
      integer istr,iend,jstr,jend, i,j, tile
# ifdef SOLVE3D
     &                       , itrc, k
# endif
      real cff
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "eos_vars.h"
# include "mixing.h"
# include "averages.h"
# ifdef BIOLOGY_NPZDOC
#  include "bgcflux.h"
# endif
# ifdef BIOLOGY_BEC
#  include "ecosys_bec.h"
#  include "bec_avg.h"
# endif
# include "work.h"
!
#include "compute_auxiliary_bounds.h"
!
      if (iic.gt.ntsAVR) then
        if (nAVR .eq. 1 .or. mod(iic-ntsAVR,nAVR).eq.1) then
          if (ZEROTH_TILE) then
            time_AVR=time
!      MPI_master_only write(*,*) 'started averaging',iic,ntsAVR,nAVR
          endif
          if (wrtAVR(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                zeta_AVR(i,j)=zeta(i,j,knew)
              enddo
            enddo
          endif
          if (wrtAVR(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                ubar_AVR(i,j)=ubar(i,j,knew)
              enddo
            enddo
          endif
          if (wrtAVR(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                vbar_AVR(i,j)=vbar(i,j,knew)
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtAVR(indxU)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_AVR(i,j,k)=u(i,j,KLEVEL,nstp)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxV)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_AVR(i,j,k)=v(i,j,KLEVEL,nstp)
                enddo
              enddo
            ENDKLOOP
          endif
          do itrc=1,NT
           if (wrtAVR(indxT+itrc-1)) then
             KLOOP(1)
               do j=jstrR,jendR
                 do i=istrR,iendR
                   t_AVR(i,j,k,itrc)=t(i,j,KLEVEL,nstp,itrc)
                 enddo
               enddo
            ENDKLOOP
           endif
          enddo
#  ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtAVR(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                      t_sed_AVR(i,j,itrc)=t_sed(i,j,itrc)
                   enddo
                enddo
             endif
          enddo
#  endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
         KLOOP(1)
          do j=jstrR,jendR
             do i=istrR,iendR
                HCO3d_AVR(i,j,k) = HCO3d_hist(i,j,KLEVEL)
                CO3d_AVR(i,j,k) = CO3d_hist(i,j,KLEVEL)
                CO2STARd_AVR(i,j,k) = CO2STARd_hist(i,j,KLEVEL)
                PHd_AVR(i,j,k) = PHd_hist(i,j,KLEVEL)
             end do
           end do
         ENDKLOOP
#   endif /* CH_CARBON_DEPTH */
          do j=jstrR,jendR
             do i=istrR,iendR
                pH_AVR(i,j) = pH_hist(i,j)
                pCO2_AVR(i,j) = pCO2sw(i,j)
                pCO2air_AVR(i,j) = pCO2air(i,j)
             end do
          end do

          do j=jstrR,jendR
             do i=istrR,iendR
                PARinc_AVR(i,j) = PARinc(i,j)
             end do
          end do
          KLOOP(1)
            do j=jstrR,jendR
               do i=istrR,iendR
                  PAR_AVR(i,j,k) = PAR(i,j,KLEVEL)
               end do
            end do
         ENDKLOOP
#  endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
          if (wrtAVR(indxR)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                  rho_AVR(i,j,k)=rho1(i,j,KLEVEL)+qp1(i,j,KLEVEL)
     &                          *(z_w(i,j,N)-z_r(i,j,KLEVEL))
#  else
                  rho_AVR(i,j,k)=rho(i,j,KLEVEL)
#  endif
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxO)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_AVR(i,j,k)=w(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtAVR(indxW)) then
              call Wvlcty (tile, work) 
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_mean(i,j,k)=work(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtAVR(indxAkt)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  akt_AVR(i,j,k)=Akt(i,j,KLEVEL,itemp)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxAkv)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  akv_AVR(i,j,k)=Akv(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
#  ifdef KPP_DIAGNOSE
          if (wrtAVR(indxRich)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  rich_AVR(i,j,k)=Rich(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxRichN)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  richN_AVR(i,j,k)=RichN(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
#  endif /* KPP_DIAGNOSE */
#  ifdef SALINITY
          if (wrtAVR(indxAks)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  aks_AVR(i,j,k)=Akt(i,j,KLEVEL,isalt)
                enddo
              enddo
            ENDKLOOP
          endif
#  endif
#  ifdef LMD_KPP
          if (wrtAVR(indxHbl)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                hbl_AVR(i,j)=hbls(i,j,nstp)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtAVR(indxHbbl)) then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_AVR(i,j)=hbbls(i,j,nstp)
            enddo
          enddo
        endif
#  endif
# endif /* SOLVE3D */
        elseif (mod(iic-ntsAVR,nAVR).gt.1) then
          if (ZEROTH_TILE) time_AVR=time_AVR+time
          if (wrtAVR(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                zeta_AVR(i,j)=zeta_AVR(i,j)+zeta(i,j,knew)
              enddo
            enddo
          endif
          if (wrtAVR(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                ubar_AVR(i,j)=ubar_AVR(i,j)+ubar(i,j,knew)
              enddo
            enddo
          endif
          if (wrtAVR(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                vbar_AVR(i,j)=vbar_AVR(i,j)+vbar(i,j,knew)
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtAVR(indxU)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_AVR(i,j,k)=u_AVR(i,j,k)+u(i,j,KLEVEL,nstp)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxV)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_AVR(i,j,k)=v_AVR(i,j,k)+v(i,j,KLEVEL,nstp)
                enddo
              enddo
            ENDKLOOP
          endif
          do itrc=1,NT
            if (wrtAVR(indxT+itrc-1)) then
              KLOOP(1)
                do j=jstrR,jendR
                  do i=istrR,iendR
                    t_AVR(i,j,k,itrc)=t_AVR(i,j,k,itrc)
     &                              +t(i,j,KLEVEL,nstp,itrc)
                  enddo
                enddo
              ENDKLOOP
            endif
          enddo
#  ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtAVR(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                      t_sed_AVR(i,j,itrc)=t_sed_AVR(i,j,itrc)
     &                     +t_sed(i,j,itrc)
                   enddo
                enddo
             endif
          enddo
#  endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
        KLOOP(1)
          do j=jstrR,jendR
             do i=istrR,iendR
                HCO3d_AVR(i,j,k) = HCO3d_AVR(i,j,k) + HCO3d_hist(i,j,KLEVEL)
                CO3d_AVR(i,j,k) = CO3d_AVR(i,j,k) + CO3d_hist(i,j,KLEVEL)
                CO2STARd_AVR(i,j,k) = CO2STARd_AVR(i,j,k) + CO2STARd_hist(i,j,KLEVEL)
                PHd_AVR(i,j,k) = PHd_AVR(i,j,k) + PHd_hist(i,j,KLEVEL)
             end do
          end do
        ENDKLOOP
#   endif /* CH_CARBON_DEPTH */
          do j=jstrR,jendR
             do i=istrR,iendR
                pCO2air_AVR(i,j) = pCO2air_AVR(i,j) + pCO2air(i,j)
                pH_AVR(i,j) = pH_AVR(i,j) + pH_hist(i,j)
                pCO2_AVR(i,j) = pCO2_AVR(i,j) + pCO2sw(i,j)
             end do
          end do

          do j=jstrR,jendR
             do i=istrR,iendR
                PARinc_AVR(i,j) = PARinc_AVR(i,j) + PARinc(i,j)
             end do
          end do
          KLOOP(1)
            do j=jstrR,jendR
               do i=istrR,iendR
                  PAR_AVR(i,j,k) = PAR_AVR(i,j,k) + PAR(i,j,KLEVEL)
               end do
            end do
          ENDKLOOP
#  endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
          if (wrtAVR(indxR)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                  rho_AVR(i,j,k)=rho_AVR(i,j,KLEVEL) + rho1(i,j,KLEVEL)
     &                     +qp1(i,j,KLEVEL)*(z_w(i,j,N)-z_r(i,j,KLEVEL))
#  else
                  rho_AVR(i,j,k)=rho_AVR(i,j,k)+rho(i,j,KLEVEL)
#  endif
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxO)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_AVR(i,j,k)=w_AVR(i,j,k)+w(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtAVR(indxW)) then
              call Wvlcty (tile, work) 
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_mean(i,j,k)=w_mean(i,j,k)+work(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtAVR(indxAkt)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  akt_AVR(i,j,k)=akt_AVR(i,j,k)+Akt(i,j,KLEVEL,itemp)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxAkv)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  akv_AVR(i,j,k)=akv_AVR(i,j,k)+Akv(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
#  ifdef KPP_DIAGNOSE
          if (wrtAVR(indxRich)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  rich_AVR(i,j,k)=rich_AVR(i,j,k)+Rich(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxRichN)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  richN_AVR(i,j,k)=richN_AVR(i,j,k)+RichN(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
#  endif /* KPP_DIAGNOSE */
#  ifdef SALINITY
          if (wrtAVR(indxAks)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  aks_AVR(i,j,k)=aks_AVR(i,j,k)+Akt(i,j,KLEVEL,isalt)
                enddo
              enddo
            ENDKLOOP
          endif
#  endif
#  ifdef LMD_KPP
          if (wrtAVR(indxHbl)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                hbl_AVR(i,j)=hbl_AVR(i,j)+hbls(i,j,nstp)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtAVR(indxHbbl)) then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_AVR(i,j)=hbbl_AVR(i,j)+hbbls(i,j,nstp)
            enddo
          enddo
        endif
#  endif
# endif /* SOLVE3D */
        elseif (mod(iic-ntsAVR,nAVR).eq.0) then
          cff=1./float(nAVR)
          if (ZEROTH_TILE) then
             time_AVR=cff*(time_AVR+time)
!      MPI_master_only write(*,*) 'finish averaging',iic,ntsAVR,nAVR
          endif
          if (wrtAVR(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                zeta_AVR(i,j)=cff*( zeta_AVR(i,j)+zeta(i,j,knew))
              enddo
            enddo
          endif
          if (wrtAVR(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                ubar_AVR(i,j)=cff*( ubar_AVR(i,j)+ubar(i,j,knew))
              enddo
            enddo
          endif
          if (wrtAVR(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                vbar_AVR(i,j)=cff*( vbar_AVR(i,j)+vbar(i,j,knew))
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtAVR(indxU))
     &    then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_AVR(i,j,k)=cff*(u_AVR(i,j,k)+u(i,j,KLEVEL,nstp))
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxV))
     &    then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_AVR(i,j,k)=cff*(v_AVR(i,j,k)+v(i,j,KLEVEL,nstp))
                enddo
              enddo
            ENDKLOOP
          endif
          do itrc=1,NT
            if (wrtAVR(indxT+itrc-1))
     &    then
              KLOOP(1)
                do j=jstrR,jendR
                  do i=istrR,iendR
                    t_AVR(i,j,k,itrc)=cff*( t_AVR(i,j,KLEVEL,itrc)
     &                                    +t(i,j,KLEVEL,nstp,itrc))
                  enddo
                enddo
              ENDKLOOP
            endif
          enddo
#ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtAVR(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                      t_sed_AVR(i,j,itrc)=cff*( t_sed_AVR(i,j,itrc)
     &                     +t_sed(i,j,itrc))
                   enddo
                enddo
             endif
          enddo
#endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
       KLOOP(1)
          do j=jstrR,jendR
             do i=istrR,iendR
                HCO3d_AVR(i,j,k) = cff*( HCO3d_AVR(i,j,k) + HCO3d_hist(i,j,KLEVEL) )
                CO3d_AVR(i,j,k) = cff*( CO3d_AVR(i,j,k) + CO3d_hist(i,j,KLEVEL) )
                CO2STARd_AVR(i,j,k) = cff*( CO2STARd_AVR(i,j,k) + CO2STARd_hist(i,j,KLEVEL) )
                PHd_AVR(i,j,k) = cff*( PHd_AVR(i,j,k) + PHd_hist(i,j,KLEVEL) )
             end do
          end do
       ENDKLOOP

#   endif /* CH_CARBON_DEPTH */
          do j=jstrR,jendR
             do i=istrR,iendR
                pH_AVR(i,j) = cff*( pH_AVR(i,j) + pH_hist(i,j) )
                pCO2_AVR(i,j) = cff*( pCO2_AVR(i,j) + pCO2sw(i,j) )
                pCO2air_AVR(i,j) = cff*(pCO2air_AVR(i,j) + pCO2air(i,j))
             end do
          end do

          do j=jstrR,jendR
             do i=istrR,iendR
                PARinc_AVR(i,j) = cff*( PARinc_AVR(i,j) + PARinc(i,j) )
             end do
          end do
          KLOOP(1)
            do j=jstrR,jendR
               do i=istrR,iendR
                  PAR_AVR(i,j,k) = cff*( PAR_AVR(i,j,k) + PAR(i,j,KLEVEL) )
               end do
            end do
          ENDKLOOP
#  endif /* BIOLOGY_NPZDOC */
          if (wrtAVR(indxR))
     &    then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                  rho_AVR(i,j,k)=cff*( rho_AVR(i,j,KLEVEL) + rho1(i,j,KLEVEL)
     &                          +qp1(i,j,KLEVEL)*(z_w(i,j,N)-z_r(i,j,KLEVEL)))
#  else
                  rho_AVR(i,j,k)=cff*(rho_AVR(i,j,k)+rho(i,j,KLEVEL))
#  endif
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxO))
     &    then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_AVR(i,j,k)=cff*pm(i,j)*pn(i,j)*( w_AVR(i,j,k)
     &                                                  +w(i,j,KLEVEL))
                enddo
              enddo
            ENDKLOOP
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtAVR(indxW))
     &    then
              call Wvlcty (tile, work) 
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_mean(i,j,k)=cff*(w_mean(i,j,k)+work(i,j,KLEVEL))
                enddo
              enddo
            ENDKLOOP
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtAVR(indxAkt))
     &    then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  akt_AVR(i,j,k)=cff*(akt_AVR(i,j,k)+Akt(i,j,KLEVEL,itemp))
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxAkv))
     &    then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  akv_AVR(i,j,k)=cff*(akv_AVR(i,j,k)+Akv(i,j,KLEVEL))
                enddo
              enddo
            ENDKLOOP
          endif
#  ifdef KPP_DIAGNOSE
          if (wrtAVR(indxRich))
     &    then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  rich_AVR(i,j,k)=cff*(rich_AVR(i,j,k)+Rich(i,j,KLEVEL))
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxRichN))
     &    then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  richN_AVR(i,j,k)=cff*(richN_AVR(i,j,k)+RichN(i,j,KLEVEL))
                enddo
              enddo
            ENDKLOOP
          endif
#  endif /* KPP_DIAGNOSE */
#  ifdef SALINITY
          if (wrtAVR(indxAks))
     &    then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  aks_AVR(i,j,k)=cff*(aks_AVR(i,j,k)+Akt(i,j,KLEVEL,isalt))
                enddo
              enddo
            ENDKLOOP
          endif
#  endif
#  ifdef LMD_KPP
          if (wrtAVR(indxHbl))
     &    then
            do j=jstrR,jendR
              do i=istrR,iendR
                hbl_AVR(i,j)=cff*(hbl_AVR(i,j)+hbls(i,j,nstp))
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtAVR(indxHbbl))
     &    then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_AVR(i,j)=cff*(hbbl_AVR(i,j)+hbbls(i,j,nstp))
            enddo
          enddo
        endif
#  endif
# endif /* SOLVE3D */
        endif
      endif       !<-- iic.gt.ntsAVR
      return
      end
# undef set_AVR
# undef set_AVR_tile
# undef wrtAVR
# undef time_AVR
# undef ntsAVR
# undef nAVR
# undef ntsAVR
# undef zeta_AVR
# undef ubar_AVR
# undef vbar_AVR
# undef u_AVR
# undef v_AVR
# undef w_AVR
# undef t_AVR
# undef hbl_AVR
# undef hbbl_AVR
# undef rho_AVR
# undef aks_AVR
# undef akt_AVR
# undef akv_AVR
# undef KLOOP
# undef ENDKLOOP
# undef KLEVEL
# ifdef SLICE_AVG
#  ifndef AVRSLICE
#   define AVRSLICE
#   include "set_avg.F"
#  endif
# endif
#else /* AVERAGES not defined */
      subroutine set_AVR_empty
      end
#endif /* AVERAGES */
