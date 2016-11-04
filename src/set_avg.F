#include "cppdefs.h"
#ifdef AVERAGES

! We are using CPP to produce 2 similar routines for regular 3 and 2D averages
! (set_avg) and slice averages (set_slavg) out of a common source files.

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
#  define wvl_AVR(i,j,k) wvl_avg(i,j,k)
#  define t_AVR(i,j,k,itrc) t_avg(i,j,k,itrc)
#  define hbl_AVR hbl_avg
#  define hbbl_AVR hbbl_avg
#  if defined KPP_DIAGNOSE
#   define kpprichm_AVR(i,j,k) kpprichm_avg(i,j,k)
#   define kppricht_AVR(i,j,k) kppricht_avg(i,j,k)
#   define kppsblsh_AVR(i,j,k) kppsblsh_avg(i,j,k)
#   define kppsblst_AVR(i,j,k) kppsblst_avg(i,j,k)
#   define kppsblrot_AVR(i,j,k) kppsblrot_avg(i,j,k)
#   define kppsblent_AVR(i,j,k) kppsblent_avg(i,j,k)
#  endif
#  define rho_AVR(i,j,k) rho_avg(i,j,k)
#  define aks_AVR(i,j,k) aks_avg(i,j,k)
#  define akt_AVR(i,j,k) akt_avg(i,j,k)
#  define akv_AVR(i,j,k) akv_avg(i,j,k)
#  if defined WRITE_HEATFLX
#   define heatflx_AVR(i,j) heatflx_avg(i,j)
#  endif
#  if defined WRITE_TEMP_REST
#   define restflx_temp_AVR(i,j) restflx_temp_avg(i,j)
#  endif
#  if defined WRITE_SALT_REST
#   define restflx_salt_AVR(i,j) restflx_salt_avg(i,j)
#  endif
#  define rich_AVR(i,j,k) rich_avg(i,j,k)
#  define richN_AVR(i,j,k) richN_avg(i,j,k)
#  define swr_frac_AVR(i,j,k) swr_frac_avg(i,j,k)
#  define KLOOP(s)  do k=s,N
#  define ENDKLOOP  enddo
#  define KLEVEL   k
#  ifdef BIOLOGY_NPZDOC
#   define PAR_AVR(i,j,k) PAR_avg(i,j,k)
#   ifdef OXYGEN 
#    define pH_AVR pH_avg
#    define pCO2_AVR pCO2_avg
#    define pCO2air_AVR pCO2air_avg
#    define PARinc_AVR PARinc_avg
#    define t_sed_AVR t_sed_avg
#    define GasExcFlux_AVR GasExcFlux_avg
#   endif
#  endif /* BIOLOGY_NPZDOC */
#  ifdef BIOLOGY_BEC
#   define PAR_AVR PAR_avg
#   define pH_AVR pH_avg
#   define pCO2_AVR pCO2_avg
#   define pCO2air_AVR pCO2air_avg
#   define PARinc_AVR PARinc_avg
#  endif /* BIOLOGY_BEC */
#  ifdef WRITE_DEPTHS
#    define Hz_AVR Hz_avg
#    define z_r_AVR z_r_avg
#    define z_w_AVR z_w_avg
#  endif /* WRITE_DEPTHS */
#  if defined BIOLOGY_BEC2 && defined BEC2_DIAG
#   define bec2_diag_2d_AVR bec2_diag_2d_avg
#   define bec2_diag_3d_AVR(i,j,k,itrc) bec2_diag_3d_avg(i,j,k,itrc)
#  endif /* BIOLOGY_BEC2 && BEC2_DIAG */
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
#  define wvl_AVR(i,j,k) wvl_slavg(i,j)
#  define t_AVR(i,j,k,itrc) t_slavg(i,j,itrc)
#  define hbl_AVR hbl_slavg
#  define hbbl_AVR hbbl_slavg
#  define rho_AVR(i,j,k) rho_slavg(i,j)
#  define aks_AVR(i,j,k) aks_slavg(i,j)
#  define akt_AVR(i,j,k) akt_slavg(i,j)
#  define akv_AVR(i,j,k) akv_slavg(i,j)
#  define rich_AVR(i,j,k) rich_slavg(i,j)
#  define richN_AVR(i,j,k) richN_slavg(i,j)
#  define swr_frac_AVR(i,j,k) swr_frac_slavg(i,j)
#  define KLOOP(s)  
#  define ENDKLOOP  
#  define KLEVEL   ksl
#  ifdef BIOLOGY_NPZDOC
#   define PAR_AVR(i,j,k) PAR_slavg(i,j)
#   ifdef OXYGEN 
#    define pH_AVR pH_slavg
#    define pCO2_AVR pCO2_slavg
#    define pCO2air_AVR pCO2air_slavg
#    define PARinc_AVR PARinc_slavg
#    define t_sed_AVR t_sed_slavg
#    define GasExcFlux_AVR GasExcFlux_slavg
#   endif
#  endif /* BIOLOGY_NPZDOC */
#  ifdef BIOLOGY_BEC
#   define PAR_AVR PAR_slavg
#   define pH_AVR pH_slavg
#   define pCO2_AVR pCO2_slavg
#   define pCO2air_AVR pCO2air_slavg
#   define PARinc_AVR PARinc_slavg
#  endif /* BIOLOGY_BEC */
#  ifdef WRITE_DEPTHS
#    define Hz_AVR(i,j,k) Hz_slavg(i,j)
#    define z_r_AVR(i,j,k) z_r_slavg(i,j)
#    define z_w_AVR(i,j,k) z_w_slavg(i,j)
#  endif /* WRITE_DEPTHS */
#  if defined BIOLOGY_BEC2 && defined BEC2_DIAG
#   define bec2_diag_2d_AVR bec2_diag_2d_slavg
#   define bec2_diag_3d_AVR(i,j,k,itrc) bec2_diag_3d_slavg(i,j,itrc)
#  endif /* BIOLOGY_BEC2 && BEC2_DIAG */
# endif
 
      subroutine set_AVR (tile)
      implicit none
      integer tile
# include "param.h"
# ifdef SOLVE3D
#  include "ncvars.h"
#  include "work.h"
#  include "private_scratch.h"
# endif
# include "compute_tile_bounds.h"
# ifdef SOLVE3D
      if (wrtavg(indxW)) then
        call wvlcty_tile(istr,iend,jstr,jend, work, A2d(1,1),
     &                                    A2d(1,2), A2d(1,3))
      endif
# endif
      call set_AVR_tile (istr,iend,jstr,jend)
      return
      end
 
      subroutine set_AVR_tile(istr,iend,jstr,jend)

! Compute time-averaged fields within a tile.
! ------- ------------- ------ ------ - -----
! Because of the syncronization issues, a delayed-mode procedure is
! used for averaging.  This implies that all fields to be averaged are
! sampled during the next time step, rather than at the end of the time
! step when they are computed.
!
! Although this algorithm results in somewhat awkward controlling logic
! it has the advantage that that all fields to be sampled correspond to
! exactly the same time, which is time step "n".  Particularly, this is
! done this way because vertical velocity corresponding to the newly
! computed horizontal velocities becomes available only during the
! following time step. The same applies to the density field.
!
! The algorithm consists of three logical blocks: (1) initialization
! of the average arrays: when  mod(iic-1,navg)==1  the target arrays
! are set to the first contribution; (2) accumulation of averaged data,
! when mod(iic-1,navg)>1; and (3) adding the last contribution and
! scaling.

      implicit none
      integer istr,iend,jstr,jend, i,j
# ifdef SOLVE3D
     &                       , itrc
#  ifndef AVRSLICE
     &                       , k
# endif
# endif
      real cff,cff1
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "eos_vars.h"
# include "mixing.h"
# include "averages.h"
# ifdef SOLVE3D
#  include "work.h"
# endif

# if (defined BIOLOGY_NPZDOC && defined PCO2AIR_FORCING) || \
     defined WRITE_TEMP_REST || defined WRITE_SALT_REST || \
     defined WRITE_HEATFLX
#  include "forces.h"
# endif
# ifdef BIOLOGY_NPZDOC
#  include "bgcflux.h"
#  include "gasexc.h"
# endif
# ifdef BIOLOGY_BEC
#  include "ecosys_bec.h"
#  include "bec_avg.h"
# endif
#if defined BIOLOGY_BEC2 && defined BEC2_DIAG
# include "ecosys_bec2.h"
#endif

#include "compute_auxiliary_bounds.h"

      if (iic>ntsAVR) then
        cff=1./dble(nAVR) !<-- scaling
        if (mod(iic-ntsAVR,nAVR)==1) then
          if (ZEROTH_TILE) then
            mpi_master_only write(*,'(7x,A,I8,2x,A,I8,2x,A,I5)')
#ifdef AVRSLICE
     &          'set_slavg :: started slice averaging at iic=', iic,
     &                          'ntsslavg=', ntsAVR, 'nslavg=', nAVR
#else
     &               'set_avg :: started averaging at iic=', iic,
     &                          'ntsavg=', ntsAVR, 'navg=', nAVR
#endif
          endif
          cff1=0.   !--> initialize
        else
          cff1=1.   !--> keep adding
        endif

        if (ZEROTH_TILE) time_AVR=cff1*time_AVR + cff*time

        if (wrtavg(indxZ)) then
            do j=jstrR,jendR
              do i=istrR,iendR
              zeta_AVR(i,j)=cff1*zeta_AVR(i,j) + cff*zeta(i,j,knew)
              enddo
            enddo
          endif
          if (wrtAVR(indxUb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
              ubar_AVR(i,j)=cff1*ubar_AVR(i,j) + cff*ubar(i,j,knew)
              enddo
            enddo
          endif
          if (wrtAVR(indxVb)) then
            do j=jstrR,jendR
              do i=istrR,iendR
              vbar_AVR(i,j)=cff1*vbar_AVR(i,j) +cff*vbar(i,j,knew)
              enddo
            enddo
          endif


# ifdef SOLVE3D
          if (wrtAVR(indxU)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                u_AVR(i,j,k)=cff1*u_AVR(i,j,k) +cff*u(i,j,KLEVEL,nstp)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxV)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                v_AVR(i,j,k)=cff1*v_AVR(i,j,k) + cff*v(i,j,KLEVEL,nstp)
                enddo
              enddo
            ENDKLOOP
          endif
          do itrc=1,NT
           if (wrtAVR(indxT+itrc-1)) then
             KLOOP(1)
               do j=jstrR,jendR
                 do i=istrR,iendR
                  t_AVR(i,j,k,itrc)=cff1*t_AVR(i,j,k,itrc)
     &                              +cff*t(i,j,KLEVEL,nstp,itrc)
                 enddo
               enddo
            ENDKLOOP
           endif
          enddo
          if (wrtAVR(indxR)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
#  ifdef SPLIT_EOS
                rho_AVR(i,j,k)=cff1*rho_AVR(i,j,k) +cff*( rho1(i,j,KLEVEL)
     &                                        -qp1(i,j,KLEVEL)*z_r(i,j,KLEVEL) )
#  else
                rho_AVR(i,j,k)=cff1*rho_AVR(i,j,k) +cff*rho(i,j,KLEVEL)
#  endif
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxO)) then
          if (mod(iic-ntsAVR,nAVR)==1) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_AVR(i,j,k)=We(i,j,KLEVEL)+Wi(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          elseif (mod(iic-ntsAVR,nAVR)>1) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_AVR(i,j,k)=w_AVR(i,j,k) +We(i,j,KLEVEL)+Wi(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          elseif (mod(iic-ntsAVR,nAVR)==0) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  w_AVR(i,j,k)=cff*( w_AVR(i,j,KLEVEL)+We(i,j,KLEVEL)+Wi(i,j,KLEVEL)
     &                                              )*pm(i,j)*pn(i,j)
                enddo
              enddo
            ENDKLOOP
          endif
          endif
          if (wrtAVR(indxW)) then
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                wvl_AVR(i,j,k)=cff1*wvl_AVR(i,j,k) +cff*work(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxAkv)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                akv_AVR(i,j,k)=cff1*akv_AVR(i,j,k) +cff*Akv(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
        if (wrtAVR(indxAkt)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                akt_AVR(i,j,k)=cff1*akt_AVR(i,j,k)+cff*Akt(i,j,KLEVEL,itemp)
                enddo
              enddo
            ENDKLOOP
          endif
#  ifdef SALINITY
          if (wrtAVR(indxAks)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                aks_AVR(i,j,k)=cff1*aks_AVR(i,j,k)+cff*Akt(i,j,KLEVEL,isalt)
                enddo
              enddo
            ENDKLOOP
          endif
#  endif
#  if defined WRITE_HEATFLX
          if (wrtAVR(indxHeatflx)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                heatflx_AVR(i,j)=cff1*heatflx_AVR(i,j) +cff*Heatflx(i,j)
              enddo
            enddo
          endif
#  endif
#  if defined WRITE_TEMP_REST
          if (wrtAVR(indxRestflxTemp)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                restflx_temp_AVR(i,j)=cff1*restflx_temp_AVR(i,j) +cff*RestflxTemp(i,j)
              enddo
            enddo
          endif
#  endif
#  if defined WRITE_SALT_REST
          if (wrtAVR(indxRestflxSalt)) then
            do j=jstrR,jendR
              do i=istrR,iendR
                restflx_salt_AVR(i,j)=cff1*restflx_salt_AVR(i,j) +cff*RestflxSalt(i,j)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_KPP
        if (wrtAVR(indxHbls)) then
            do j=jstrR,jendR
              do i=istrR,iendR
              hbl_AVR(i,j)=cff1*hbl_AVR(i,j) +cff*hbls(i,j,nstp)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
        if (wrtAVR(indxHbbl)) then
          do j=jstrR,jendR
            do i=istrR,iendR
              hbbl_AVR(i,j)=cff1*hbbl_AVR(i,j) +cff*hbbl(i,j,nstp)
            enddo
          enddo
        endif
#  endif
#  ifdef KPP_DIAGNOSE
! KppRichm KppRicht KppSblSh KppSblSt KppSblRot KppSblEnt
          if (wrtAVR(indxKppRichm)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  kpprichm_AVR(i,j,k)=cff1*kpprichm_AVR(i,j,k)+cff*KppRichm(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxKppRicht)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  kppricht_AVR(i,j,k)=cff1*kppricht_AVR(i,j,k)+cff*KppRicht(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxKppSblSh)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  kppsblsh_AVR(i,j,k)=cff1*kppsblsh_AVR(i,j,k)+cff*KppSblSh(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxKppSblSt)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  kppsblst_AVR(i,j,k)=cff1*kppsblst_AVR(i,j,k)+cff*KppSblSt(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxKppSblRot)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  kppsblrot_AVR(i,j,k)=cff1*kppsblrot_AVR(i,j,k)+cff*KppSblRot(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
          if (wrtAVR(indxKppSblEnt)) then
            KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
                  kppsblent_AVR(i,j,k)=cff1*kppsblent_AVR(i,j,k)+cff*KppSblEnt(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP
          endif
#  endif

# ifdef WRITE_DEPTHS
        KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
              z_r_AVR(i,j,k)=cff1*z_r_AVR(i,j,k)+cff*z_r(i,j,KLEVEL)
              Hz_AVR(i,j,k)=cff1*Hz_AVR(i,j,k)+cff*Hz(i,j,KLEVEL)
                enddo
              enddo
        ENDKLOOP
        KLOOP(0)
              do j=jstrR,jendR
                do i=istrR,iendR
              z_w_AVR(i,j,k)=cff1*z_w_AVR(i,j,k)+cff*z_w(i,j,KLEVEL)
                enddo
              enddo
        ENDKLOOP
# endif /* WRITE_DEPTHS */

#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
             do j=jstrR,jendR
                do i=istrR,iendR
                pCO2air_AVR(i,j) = cff1*pCO2air_AVR(i,j) + cff*pCO2air(i,j)
                pH_AVR(i,j) = cff1*pH_AVR(i,j) + cff*pH_hist(i,j)
                pCO2_AVR(i,j) = cff1*pCO2_AVR(i,j) + cff*pCO2sw(i,j)
                end do
             end do

            do j=jstrR,jendR
              do i=istrR,iendR
                PARinc_AVR(i,j) = cff1*PARinc_AVR(i,j) + cff*PARinc(i,j)
              enddo
            enddo
            KLOOP(1)
              do j=jstrR,jendR
                do i=istrR,iendR
                  PAR_AVR(i,j,k) = cff1*PAR_AVR(i,j,k) + cff*PAR(i,j,KLEVEL)
                enddo
              enddo
            ENDKLOOP

#   ifdef OXYGEN
          do itrc=1,NumGasExcTerms
              do j=jstrR,jendR
                do i=istrR,iendR
                   GasExcFlux_AVR(i,j,itrc) = cff1*GasExcFlux_AVR(i,j,itrc)+cff*GasExcFlux(i,j,itrc)
                enddo
              enddo
                  enddo
#   endif /* OXYGEN */

#ifdef SEDIMENT_BIOLOGY
          do itrc=1,NT_sed
             if (wrtAVR(indxSedOrgN+itrc-1)) then
                do j=jstrR,jendR
                   do i=istrR,iendR
                t_sed_AVR(i,j,itrc)=cff1*t_sed_AVR(i,j,itrc) +cff*t_sed(i,j,itrc)
                   enddo
                enddo
             endif
          enddo
#endif /* SEDIMENT_BIOLOGY */
#  endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
#  if defined BIOLOGY_BEC2 && defined BEC2_DIAG
          do itrc=1,nr_bec2_diag_2d
             do j=jstrR,jendR
                do i=istrR,iendR
                   bec2_diag_2d_AVR(i,j,itrc) = bec2_diag_2d_AVR(i,j,itrc) + bec2_diag_2d(i,j,itrc)
                end do
             end do
          end do
          do itrc=1,nr_bec2_diag_3d
             KLOOP(1)
             do j=jstrR,jendR
                do i=istrR,iendR
                   bec2_diag_3d_AVR(i,j,k,itrc) = cff1*bec2_diag_3d_AVR(i,j,KLEVEL,itrc) 
     &                                           + bec2_diag_3d(i,j,KLEVEL,itrc)
                end do
             end do
             ENDKLOOP
          end do
#  endif /* BIOLOGY_BEC2 && BEC2_DIAG */
# endif /* SOLVE3D */

        if (mod(iic-ntsAVR,nAVR)==0) then
          if (ZEROTH_TILE) then
            mpi_master_only write(*,'(7x,2A,I8,2x,A,I8,2x,A,I5)')
# ifdef AVRSLICE
     &              'set_slavg',
# else 
     &              'set_avg',
# endif
     &              ' :: finished averaging at iic=', iic,
     &                         'ntsAVR=', ntsAVR,  'nAVR=', nAVR
          endif
        endif
      endif       !<-- iic>ntsAVR
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
# undef wvl_AVR
# undef t_AVR
# undef hbl_AVR
# undef hbbl_AVR
# if defined KPP_DIAGNOSE
#  undef kpprichm_AVR
#  undef kppricht_AVR
#  undef kppsblsh_AVR
#  undef kppsblst_AVR
#  undef kppsblrot_AVR
#  undef kppsblent_AVR
# endif
# undef rho_AVR
# undef aks_AVR
# undef akt_AVR
# if defined WRITE_HEATFLX
#  undef heatflx_AVR
# endif
# if defined WRITE_TEMP_REST
#  undef restflx_temp_AVR
# endif
# if defined WRITE_SALT_REST
#  undef restflx_salt_AVR
# endif
# undef swr_frac_AVR
# undef akv_AVR
# undef KLOOP
# undef ENDKLOOP
# undef KLEVEL
# ifdef BIOLOGY_NPZDOC
#  undef PAR_AVR
#  ifdef OXYGEN 
#   undef pH_AVR
#   undef pCO2_AVR
#   undef pCO2air_AVR
#   undef PARinc_AVR
#   undef t_sed_AVR
#   undef GasExcFlux_AVR
#  endif
# endif
#  if defined BIOLOGY_BEC2 && defined BEC2_DIAG
#   undef bec2_diag_2d_AVR
#   undef bec2_diag_3d_AVR
#  endif /* BIOLOGY_BEC2 && BEC2_DIAG */
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
