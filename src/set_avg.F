#include "cppdefs.h"
#ifdef AVERAGES
 
      subroutine set_avg (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_avg_tile (istr,iend,jstr,jend,tile)
      return
      end
 
      subroutine set_avg_tile (istr,iend,jstr,jend,tile)
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
      if (iic.gt.ntsavg) then
        if (navg .eq. 1 .or. mod(iic-ntsavg,navg).eq.1) then
          if (ZEROTH_TILE) then
            time_avg=time
!      MPI_master_only write(*,*) 'started averaging',iic,ntsavg,navg
          endif
          if (wrtavg(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                zeta_avg(i,j)=zeta(i,j,knew)
              enddo
            enddo
          endif
          if (wrtavg(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                ubar_avg(i,j)=ubar(i,j,knew)
              enddo
            enddo
          endif
          if (wrtavg(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                vbar_avg(i,j)=vbar(i,j,knew)
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtavg(indxU)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_avg(i,j,k)=u(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxV)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_avg(i,j,k)=v(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          do itrc=1,NT
           if (wrtavg(indxT+itrc-1)) then
             do k=1,N
               do j=jstrR,jendR
                 do i=istrR,iendR
                   t_avg(i,j,k,itrc)=t(i,j,k,nstp,itrc)
                 enddo
               enddo
             enddo
           endif
          enddo
#  ifdef SLICE_AVG
          if (wrtslavg(indxU)) then
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_slavg(i,j)=u(i,j,k2d,nstp)
                enddo
              enddo
          endif
          if (wrtslavg(indxV)) then
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_slavg(i,j)=v(i,j,k2d,nstp)
                enddo
              enddo
          endif
          do itrc=1,NT
           if (wrtslavg(indxT+itrc-1)) then
               do j=jstrR,jendR
                 do i=istrR,iendR
                   t_slavg(i,j,itrc)=t(i,j,k2d,nstp,itrc)
                 enddo
               enddo
           endif
          enddo
#  endif
#  ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtavg(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                      t_sed_avg(i,j,itrc)=t_sed(i,j,itrc)
                   enddo
                enddo
             endif
          enddo
#  endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
         do k=1,N
          do j=jstrR,jendR
             do i=istrR,iendR
                HCO3d_avg(i,j,k) = HCO3d_hist(i,j,k)
                CO3d_avg(i,j,k) = CO3d_hist(i,j,k)
                CO2STARd_avg(i,j,k) = CO2STARd_hist(i,j,k)
                PHd_avg(i,j,k) = PHd_hist(i,j,k)
             end do
           end do
         end do
#   endif /* CH_CARBON_DEPTH */
          do j=jstrR,jendR
             do i=istrR,iendR
                pH_avg(i,j) = pH_hist(i,j)
                pCO2_avg(i,j) = pCO2sw(i,j)
                pCO2air_avg(i,j) = pCO2air(i,j)
             end do
          end do

          do j=jstrR,jendR
             do i=istrR,iendR
                PARinc_avg(i,j) = PARinc(i,j)
             end do
          end do
          do k=1,N
            do j=jstrR,jendR
               do i=istrR,iendR
                  PAR_avg(i,j,k) = PAR(i,j,k)
               end do
            end do
          end do
#  endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
          if (wrtavg(indxR)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                  rho_avg(i,j,k)=rho1(i,j,k)+qp1(i,j,k)
     &                          *(z_w(i,j,N)-z_r(i,j,k))
#  else
                  rho_avg(i,j,k)=rho(i,j,k)
#  endif
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxO)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_avg(i,j,k)=w(i,j,k)
                enddo
              enddo
            enddo
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtavg(indxW)) then
              call Wvlcty (tile, work) 
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_mean(i,j,k)=work(i,j,k)
                enddo
              enddo
            enddo
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtavg(indxAkt)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  akt_avg(i,j,k)=Akt(i,j,k,itemp)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxAkv)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  akv_avg(i,j,k)=Akv(i,j,k)
                enddo
              enddo
            enddo
          endif
#  ifdef KPP_DIAGNOSE
          if (wrtavg(indxRich)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  rich_avg(i,j,k)=Rich(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxRichN)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  richN_avg(i,j,k)=RichN(i,j,k)
                enddo
              enddo
            enddo
          endif
#  endif /* KPP_DIAGNOSE */
#  ifdef SALINITY
          if (wrtavg(indxAks)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  aks_avg(i,j,k)=Akt(i,j,k,isalt)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_KPP
          if (wrtavg(indxHbl)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                hbl_avg(i,j)=hbls(i,j,nstp)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtavg(indxHbbl)) then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_avg(i,j)=hbbls(i,j,nstp)
            enddo
          enddo
        endif
#  endif
# endif /* SOLVE3D */
        elseif (mod(iic-ntsavg,navg).gt.1) then
          if (ZEROTH_TILE) time_avg=time_avg+time
          if (wrtavg(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                zeta_avg(i,j)=zeta_avg(i,j)+zeta(i,j,knew)
              enddo
            enddo
          endif
          if (wrtavg(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                ubar_avg(i,j)=ubar_avg(i,j)+ubar(i,j,knew)
              enddo
            enddo
          endif
          if (wrtavg(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                vbar_avg(i,j)=vbar_avg(i,j)+vbar(i,j,knew)
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtavg(indxU)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_avg(i,j,k)=u_avg(i,j,k)+u(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxV)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_avg(i,j,k)=v_avg(i,j,k)+v(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          do itrc=1,NT
            if (wrtavg(indxT+itrc-1)) then
              do k=1,N
                do j=jstrR,jendR
                  do i=istrR,iendR
                    t_avg(i,j,k,itrc)=t_avg(i,j,k,itrc)
     &                              +t(i,j,k,nstp,itrc)
                  enddo
                enddo
              enddo
            endif
          enddo
#  ifdef SLICE_AVG
          if (wrtslavg(indxU)) then
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_slavg(i,j)=u_slavg(i,j)+u(i,j,k2d,nstp)
                enddo
              enddo
          endif
          if (wrtslavg(indxV)) then
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_slavg(i,j)=v_slavg(i,j)+v(i,j,k2d,nstp)
                enddo
              enddo
          endif
          do itrc=1,NT
           if (wrtslavg(indxT+itrc-1)) then
               do j=jstrR,jendR
                 do i=istrR,iendR
                   t_slavg(i,j,itrc)=t_slavg(i,j,itrc)+
     &                                  t(i,j,k2d,nstp,itrc)
                 enddo
               enddo
           endif
          enddo
#  endif
#  ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtavg(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                      t_sed_avg(i,j,itrc)=t_sed_avg(i,j,itrc)
     &                     +t_sed(i,j,itrc)
                   enddo
                enddo
             endif
          enddo
#  endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
        do k=1,N
          do j=jstrR,jendR
             do i=istrR,iendR
                HCO3d_avg(i,j,k) = HCO3d_avg(i,j,k) + HCO3d_hist(i,j,k)
                CO3d_avg(i,j,k) = CO3d_avg(i,j,k) + CO3d_hist(i,j,k)
                CO2STARd_avg(i,j,k) = CO2STARd_avg(i,j,k) + CO2STARd_hist(i,j,k)
                PHd_avg(i,j,k) = PHd_avg(i,j,k) + PHd_hist(i,j,k)
             end do
          end do
        end do
#   endif /* CH_CARBON_DEPTH */
          do j=jstrR,jendR
             do i=istrR,iendR
                pCO2air_avg(i,j) = pCO2air_avg(i,j) + pCO2air(i,j)
                pH_avg(i,j) = pH_avg(i,j) + pH_hist(i,j)
                pCO2_avg(i,j) = pCO2_avg(i,j) + pCO2sw(i,j)
             end do
          end do

          do j=jstrR,jendR
             do i=istrR,iendR
                PARinc_avg(i,j) = PARinc_avg(i,j) + PARinc(i,j)
             end do
          end do
          do k=1,N
            do j=jstrR,jendR
               do i=istrR,iendR
                  PAR_avg(i,j,k) = PAR_avg(i,j,k) + PAR(i,j,k)
               end do
            end do
          end do
#  endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
          if (wrtavg(indxR)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                  rho_avg(i,j,k)=rho_avg(i,j,k) + rho1(i,j,k)
     &                     +qp1(i,j,k)*(z_w(i,j,N)-z_r(i,j,k))
#  else
                  rho_avg(i,j,k)=rho_avg(i,j,k)+rho(i,j,k)
#  endif
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxO)) then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_avg(i,j,k)=w_avg(i,j,k)+w(i,j,k)
                enddo
              enddo
            enddo
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtavg(indxW)) then
              call Wvlcty (tile, work) 
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_mean(i,j,k)=w_mean(i,j,k)+work(i,j,k)
                enddo
              enddo
            enddo
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtavg(indxAkt)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  akt_avg(i,j,k)=akt_avg(i,j,k)+Akt(i,j,k,itemp)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxAkv)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  akv_avg(i,j,k)=akv_avg(i,j,k)+Akv(i,j,k)
                enddo
              enddo
            enddo
          endif
#  ifdef KPP_DIAGNOSE
          if (wrtavg(indxRich)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  rich_avg(i,j,k)=rich_avg(i,j,k)+Rich(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxRichN)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  richN_avg(i,j,k)=richN_avg(i,j,k)+RichN(i,j,k)
                enddo
              enddo
            enddo
          endif
#  endif /* KPP_DIAGNOSE */
#  ifdef SALINITY
          if (wrtavg(indxAks)) then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  aks_avg(i,j,k)=aks_avg(i,j,k)+Akt(i,j,k,isalt)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_KPP
          if (wrtavg(indxHbl)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                hbl_avg(i,j)=hbl_avg(i,j)+hbls(i,j,nstp)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtavg(indxHbbl)) then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_avg(i,j)=hbbl_avg(i,j)+hbbls(i,j,nstp)
            enddo
          enddo
        endif
#  endif
# endif /* SOLVE3D */
        elseif (mod(iic-ntsavg,navg).eq.0) then
          cff=1./float(navg)
          if (ZEROTH_TILE) then
             time_avg=cff*(time_avg+time)
!      MPI_master_only write(*,*) 'finish averaging',iic,ntsavg,navg
          endif
          if (wrtavg(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                zeta_avg(i,j)=cff*( zeta_avg(i,j)+zeta(i,j,knew))
              enddo
            enddo
          endif
          if (wrtavg(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                ubar_avg(i,j)=cff*( ubar_avg(i,j)+ubar(i,j,knew))
              enddo
            enddo
          endif
          if (wrtavg(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                vbar_avg(i,j)=cff*( vbar_avg(i,j)+vbar(i,j,knew))
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtavg(indxU))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  u_avg(i,j,k)=cff*(u_avg(i,j,k)+u(i,j,k,nstp))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxV))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  v_avg(i,j,k)=cff*(v_avg(i,j,k)+v(i,j,k,nstp))
                enddo
              enddo
            enddo
          endif
          do itrc=1,NT
            if (wrtavg(indxT+itrc-1))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
              do k=1,N
                do j=jstrR,jendR
                  do i=istrR,iendR
                    t_avg(i,j,k,itrc)=cff*( t_avg(i,j,k,itrc)
     &                                    +t(i,j,k,nstp,itrc))
                  enddo
                enddo
              enddo
            endif
          enddo
#ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtavg(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                      t_sed_avg(i,j,itrc)=cff*( t_sed_avg(i,j,itrc)
     &                     +t_sed(i,j,itrc))
                   enddo
                enddo
             endif
          enddo
#endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
       do k=1,N
          do j=jstrR,jendR
             do i=istrR,iendR
                HCO3d_avg(i,j,k) = cff*( HCO3d_avg(i,j,k) + HCO3d_hist(i,j,k) )
                CO3d_avg(i,j,k) = cff*( CO3d_avg(i,j,k) + CO3d_hist(i,j,k) )
                CO2STARd_avg(i,j,k) = cff*( CO2STARd_avg(i,j,k) + CO2STARd_hist(i,j,k) )
                PHd_avg(i,j,k) = cff*( PHd_avg(i,j,k) + PHd_hist(i,j,k) )
             end do
          end do
       end do

#   endif /* CH_CARBON_DEPTH */
          do j=jstrR,jendR
             do i=istrR,iendR
                pH_avg(i,j) = cff*( pH_avg(i,j) + pH_hist(i,j) )
                pCO2_avg(i,j) = cff*( pCO2_avg(i,j) + pCO2sw(i,j) )
                pCO2air_avg(i,j) = cff*(pCO2air_avg(i,j) + pCO2air(i,j))
             end do
          end do

          do j=jstrR,jendR
             do i=istrR,iendR
                PARinc_avg(i,j) = cff*( PARinc_avg(i,j) + PARinc(i,j) )
             end do
          end do
          do k=1,N
            do j=jstrR,jendR
               do i=istrR,iendR
                  PAR_avg(i,j,k) = cff*( PAR_avg(i,j,k) + PAR(i,j,k) )
               end do
            end do
          end do
#  endif /* BIOLOGY_NPZDOC */
          if (wrtavg(indxR))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                  rho_avg(i,j,k)=cff*( rho_avg(i,j,k) + rho1(i,j,k)
     &                          +qp1(i,j,k)*(z_w(i,j,N)-z_r(i,j,k)))
#  else
                  rho_avg(i,j,k)=cff*(rho_avg(i,j,k)+rho(i,j,k))
#  endif
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxO))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_avg(i,j,k)=cff*pm(i,j)*pn(i,j)*( w_avg(i,j,k)
     &                                                  +w(i,j,k))
                enddo
              enddo
            enddo
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtavg(indxW))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
              call Wvlcty (tile, work) 
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_mean(i,j,k)=cff*(w_mean(i,j,k)+work(i,j,k))
                enddo
              enddo
            enddo
          endif
! --T.Nagai to put true vertical velocity 6/29/2006
          if (wrtavg(indxAkt))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  akt_avg(i,j,k)=cff*(akt_avg(i,j,k)+Akt(i,j,k,itemp))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxAkv))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  akv_avg(i,j,k)=cff*(akv_avg(i,j,k)+Akv(i,j,k))
                enddo
              enddo
            enddo
          endif
#  ifdef KPP_DIAGNOSE
          if (wrtavg(indxRich))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  rich_avg(i,j,k)=cff*(rich_avg(i,j,k)+Rich(i,j,k))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxRichN))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  richN_avg(i,j,k)=cff*(richN_avg(i,j,k)+RichN(i,j,k))
                enddo
              enddo
            enddo
          endif
#  endif /* KPP_DIAGNOSE */
#  ifdef SALINITY
          if (wrtavg(indxAks))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do k=0,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  aks_avg(i,j,k)=cff*(aks_avg(i,j,k)+Akt(i,j,k,isalt))
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_KPP
          if (wrtavg(indxHbl))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
            do j=jstrR,jendR
              do i=istrR,iendR
                hbl_avg(i,j)=cff*(hbl_avg(i,j)+hbls(i,j,nstp))
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtavg(indxHbbl))
#   ifdef LONGTERM_AVERAGES
     &       .and. (mod(iic-ntsavg,navg*lavg).eq.0)
#   endif
     &    then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_avg(i,j)=cff*(hbbl_avg(i,j)+hbbls(i,j,nstp))
            enddo
          enddo
        endif
#  endif
# endif /* SOLVE3D */
        endif
      endif       !<-- iic.gt.ntsavg
      return
      end
#else
      subroutine set_avg_empty
      end
#endif /* AVERAGES */
