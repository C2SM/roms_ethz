!   $Log: set_bgc_flux_avg.F,v $
!   Revision 1.2  2002/02/06 19:00:04  frenzel
!   Correction: Previously, averaging was not correct if the averaging was
!   performed every time step.
!
!   Revision 1.1  2002/01/16 19:58:53  frenzel
!   New routines related to flux analysis.
!
!
#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC && defined BGC_FLUX_ANALYSIS && defined AVERAGES

      subroutine set_bgc_flux_avg(tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_bgc_flux_avg_tile(Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_bgc_flux_avg_tile(Istr,Iend,Jstr,Jend)
!
! Compute time-averaged fields within a tile.
! ------- ------------- ------ ------ - -----
! Because of syncronization issues, the delayed mode averaging
! procedure is used. This procedure implies that all fields to be
! averaged are sampled during the next time step, rather than at
! the end of the time step when they were computed.
!
! Although this algorithm results in somewhat awkward controlling
! logic it has the advantage that all fields to be sampled
! correspond to exactly the same time, which is time step "n".
! Particularly, this is done this way because vertical velocity
! corresponding to the newly computed horizontal velocities
! becomes available only during the following time step.
! The same applies to the density field.
!
! The algorithm consists of three logical blocks: 
! (1) initialization of the averages arrays: 
! when mod(iic-1,n_bgc_flux_avg).eq.1 the target arrays
! are set to the first contribution; 
! (2) accumulation of averaged data,
! when mod(iic-1,n_bgc_flux_avg).gt.1; and 
! (3) adding the last contribution and scaling.
!

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, iflux, k
      real cff
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "averages.h"
# include "bgcflux.h"
# include "ncroms_bgcflux.h"
!
# include "compute_auxiliary_bounds.h"
!
! calculation of averaged fluxes will only be performed if n_bgc_flux_avg
! is a positive number
      if (n_bgc_flux_avg < 1) return

      if (iic.gt.nts_bgc_flux_avg) then
         if (n_bgc_flux_avg .eq. 1 .or. 
     &        mod(iic-1,n_bgc_flux_avg).eq.1) then
            if (ZEROTH_TILE) time_bgc_flux_avg=time
            do j = JstrR, JendR
               do i = IstrR, IendR
                  zeta_bgc_flux_avg(i,j) = zeta(i,j,nstp)
               end do
            end do
#   ifdef OXYGEN
!          if (wrtavg(indxU10)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   u10_avg(i,j)=u10(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxKvO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_O2_avg(i,j)=Kv_O2(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxO2sat)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   O2satu_avg(i,j)=O2satu(i,j)
                enddo
             enddo
!          endif
#    ifdef CARBON
!          if (wrtavg(indxKvCO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_CO2_avg(i,j)=Kv_CO2(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxCO2sol)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   CO2sol_avg(i,j)=CO2sol(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxPCO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pco2_avg(i,j)=pCO2sw(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxPCO2air)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pCO2air_avg(i,j)=pCO2air(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxPH)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pH_avg(i,j)=pH_hist(i,j)
                enddo
             enddo
!          endif
#    endif /* CARBON */
#   endif /* OXYGEN */
!          if (wrtavg(indxPAR)) then
            do k=1,N
               do j=JstrR,JendR
                  do i=IstrR,IendR
                     PAR_avg(i,j,k)=PAR(i,j,k)
                  enddo
               enddo
            enddo
            do j=JstrR,JendR
               do i=IstrR,IendR
                  PARinc_avg(i,j)=PARinc(i,j)
               enddo
            enddo
!          endif

            do iflux = 1, NumFluxTerms
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        Flux_avg(i,j,k,iflux) = Flux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            
#  ifdef OXYGEN
            do iflux = 1, NumGasExcTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     GasExcFlux_avg(i,j,iflux) = 
     &                    GasExcFlux(i,j,iflux)
                  end do
               end do
            end do
#  endif /* OXYGEN */
            do iflux = 1, NumVSinkTerms
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VSinkFlux_avg(i,j,k,iflux) = 
     &                       VSinkFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
#  ifdef SEDIMENT_BIOLOGY
            do iflux = 1, NumSedFluxTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     SedFlux_avg(i,j,iflux) = 
     &                       SedFlux(i,j,iflux)
                  end do
               end do
            end do
#  endif /* SEDIMENT_BIOLOGY */
         elseif (mod(iic-1,n_bgc_flux_avg).gt.1) then
            if (ZEROTH_TILE) time_bgc_flux_avg=time_bgc_flux_avg+time
            do j = JstrR, JendR
               do i = IstrR, IendR
                  zeta_bgc_flux_avg(i,j) = zeta_bgc_flux_avg(i,j) +
     &                 zeta(i,j,nstp)
               end do
            end do
#   ifdef OXYGEN
!          if (wrtavg(indxU10)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   u10_avg(i,j)=u10_avg(i,j)+u10(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxKvO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_O2_avg(i,j)=Kv_O2_avg(i,j)+Kv_O2(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxO2sat)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   O2satu_avg(i,j)=O2satu_avg(i,j)+O2satu(i,j)
                enddo
             enddo
!          endif
#   ifdef CARBON
!          if (wrtavg(indxKvCO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_CO2_avg(i,j)=Kv_CO2_avg(i,j)+Kv_CO2(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxCO2sol)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   CO2sol_avg(i,j)=CO2sol_avg(i,j)+CO2sol(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxPCO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pco2_avg(i,j)=pco2_avg(i,j)+pCO2sw(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxPCO2air)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pCO2air_avg(i,j)=pCO2air_avg(i,j)+pCO2air(i,j)
                enddo
             enddo
!          endif
!          if (wrtavg(indxPH)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pH_avg(i,j)=pH_avg(i,j)+pH_hist(i,j)
                enddo
             enddo
!          endif
#   endif /* CARBON */
#  endif /* OXYGEN */
!          if (wrtavg(indxPAR)) then
             do k=1,N
                do j=JstrR,JendR
                   do i=IstrR,IendR
                      PAR_avg(i,j,k)=PAR_avg(i,j,k)+PAR(i,j,k)
                   enddo
                enddo
             enddo
             do j=JstrR,JendR
                do i=IstrR,IendR
                   PARinc_avg(i,j)=PARinc_avg(i,j)+PARinc(i,j)
                enddo
             enddo
!          endif

            do iflux = 1, NumFluxTerms
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        Flux_avg(i,j,k,iflux) = Flux_avg(i,j,k,iflux) +
     &                       Flux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
# ifdef OXYGEN
            do iflux = 1, NumGasExcTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     GasExcFlux_avg(i,j,iflux) = 
     &                    GasExcFlux_avg(i,j,iflux) +
     &                    GasExcFlux(i,j,iflux)
                  end do
               end do
            end do
# endif /* OXYGEN */
            do iflux = 1, NumVSinkTerms
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VSinkFlux_avg(i,j,k,iflux) = 
     &                       VSinkFlux_avg(i,j,k,iflux) + 
     &                       VSinkFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
# ifdef SEDIMENT_BIOLOGY
            do iflux = 1, NumSedFluxTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     SedFlux_avg(i,j,iflux) = 
     &                    SedFlux_avg(i,j,iflux) +
     &                    SedFlux(i,j,iflux)
                  end do
               end do
            end do
# endif /* SEDIMENT_BIOLOGY */
         elseif (mod(iic-1,n_bgc_flux_avg).eq.0) then
            cff=1./float(n_bgc_flux_avg)
            if (ZEROTH_TILE) time_bgc_flux_avg=cff*
     &           (time_bgc_flux_avg + time)
            do j = JstrR, JendR
               do i = IstrR, IendR
                  zeta_bgc_flux_avg(i,j) = cff * ( 
     &                 zeta_bgc_flux_avg(i,j) + zeta(i,j,nstp) )
               end do
            end do
#   ifdef OXYGEN
!          if (wrtavg(indxU10)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   u10_avg(i,j)=cff*( u10_avg(i,j)+u10(i,j) )
                enddo
             enddo
!          endif
!          if (wrtavg(indxKvO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_O2_avg(i,j)=cff*( Kv_O2_avg(i,j)+Kv_O2(i,j) )
                enddo
             enddo
!          endif
!          if (wrtavg(indxO2sat)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   O2satu_avg(i,j)=cff*( O2satu_avg(i,j)+O2satu(i,j) )
                enddo
             enddo
!          endif
#    ifdef CARBON
!          if (wrtavg(indxKvCO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_CO2_avg(i,j)=cff*( Kv_CO2_avg(i,j)+Kv_CO2(i,j) )
                enddo
             enddo
!          endif
!          if (wrtavg(indxCO2sol)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   CO2sol_avg(i,j)=cff*( CO2sol_avg(i,j)+CO2sol(i,j) )
                enddo
             enddo
!          endif
!          if (wrtavg(indxPCO2)) then
              do j=JstrR,JendR
                 do i=IstrR,IendR
                    pco2_avg(i,j)=cff*( pco2_avg(i,j) + pCO2sw(i,j) )
                 enddo
              enddo
!          endif
!          if (wrtavg(indxPCO2air)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   pCO2air_avg(i,j)=cff*(pCO2air_avg(i,j)+pCO2air(i,j))
                enddo
             enddo
!          endif
!          if (wrtavg(indxPH)) then
              do j=JstrR,JendR
                 do i=IstrR,IendR
                    pH_avg(i,j)=cff*( pH_avg(i,j) + pH_hist(i,j) )
                 enddo
              enddo
!          endif
#    endif /* CARBON */
#   endif /* OXYGEN */
!          if (wrtavg(indxPAR)) then
             do k=1,N
                do j=JstrR,JendR
                   do i=IstrR,IendR
                      PAR_avg(i,j,k)=cff*( PAR_avg(i,j,k) + PAR(i,j,k))
                   enddo
                enddo
             enddo
             do j=JstrR,JendR
                do i=IstrR,IendR
                   PARinc_avg(i,j)=cff*( PARinc_avg(i,j) + PARinc(i,j))
                enddo
             enddo
!          endif


            do iflux = 1, NumFluxTerms
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        Flux_avg(i,j,k,iflux) = cff * 
     &                       ( Flux_avg(i,j,k,iflux) +
     &                       Flux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
# ifdef OXYGEN
            do iflux = 1, NumGasExcTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     GasExcFlux_avg(i,j,iflux) = cff *
     &                    ( GasExcFlux_avg(i,j,iflux) +
     &                    GasExcFlux(i,j,iflux) )
                  end do
               end do
            end do
# endif /* OXYGEN */
            do iflux = 1, NumVSinkTerms
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VSinkFlux_avg(i,j,k,iflux) = cff *
     &                       ( VSinkFlux_avg(i,j,k,iflux) + 
     &                       VSinkFlux(i,j,k,iflux) )
                        
                     end do
                  end do
               end do
            end do
# ifdef SEDIMENT_BIOLOGY
            do iflux = 1, NumSedFluxTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     SedFlux_avg(i,j,iflux) = cff *
     &                    ( SedFlux_avg(i,j,iflux) +
     &                    SedFlux(i,j,iflux) )
                  end do
               end do
            end do
# endif /* SEDIMENT_BIOLOGY */
         endif
      endif
         
      return
      end
#else /* BGC_FLUX_ANALYSIS && AVERAGES */
      subroutine set_bgc_flux_avg_empty
      end
#endif /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS && AVERAGES */

