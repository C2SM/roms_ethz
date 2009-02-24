!   $Log: set_phys_flux_avg.F,v $
!   Revision 1.3  2002/04/19 00:52:55  frenzel
!   Added code for the analysis of mixing and nudging fluxes.
!
!   Revision 1.2  2002/02/06 19:00:04  frenzel
!   Correction: Previously, averaging was not correct if the averaging was
!   performed every time step.
!
!   Revision 1.1  2002/01/16 19:58:53  frenzel
!   New routines related to flux analysis.
!
!
#include "cppdefs.h"
#if defined PHYS_FLUX_ANALYSIS && defined AVERAGES

      subroutine set_phys_flux_avg(tile)

      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_phys_flux_avg_tile(Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_phys_flux_avg_tile(Istr,Iend,Jstr,Jend)
!
! Compute time-averaged fields within a tile.
! ------- ------------- ------ ------ - -----
! Because of syncronization issues, the delayed mode averaging
! procedure is used. This procedure implies that all fields to be
! averaged are sampled during the next time step, rather than at
! the end of the time step when they were computed.
!
! Thought this algorithm results in somewhat ackwad controlling
! logic it has the advantage that that all fields to be sampled
! correspond to exactly the same time, which is time step "n".
! Particularly, this is done this way because vertical velocity
! corresponding to the newly computed horizontal velocities
! becomes available only during the following time step.
! The same applies to the density field.
!
! The algorithm consists of three logical blocks: (1) initialization
! of the averages arrays: when mod(iic-nts_phys_flux_avg,navg).eq.1 the target arrays
! are set to the first contribution; (2) accumulation of averaged
! data, when mod(iic-nts_phys_flux_avg,navg).gt.1; and (3) adding the last
! contribution and scaling.
!
!      USE scalars
!      USE grid
!      USE averages
!      USE ocean2d
!      USE physflux
!      USE ncscrum

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, iflux, k
      real cff
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
!# include "averages.h"
# include "physflux.h"
# include "ncroms_physflux.h"
!
#include "compute_auxiliary_bounds.h"
!
! calculation of averaged fluxes will only be performed if n_phys_flux_avg
! is a positive number
      if (n_phys_flux_avg < 1) return

      if (iic.gt.nts_phys_flux_avg) then
         if (n_phys_flux_avg .eq. 1 .or. 
     &        mod(iic-nts_phys_flux_avg,n_phys_flux_avg).eq.1) then
            if (ZEROTH_TILE) time_phys_flux_avg=time
            do j = JstrR, JendR
               do i = IstrR, IendR
                  zeta_phys_flux_avg(i,j) = zeta(i,j,knew)
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorXAdvFlux_avg(i,j,k,iflux) = 
     &                       HorXAdvFlux(i,j,k,iflux)  
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorYAdvFlux_avg(i,j,k,iflux) = 
     &                       HorYAdvFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertAdvFlux_avg(i,j,k,iflux) = 
     &                       VertAdvFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
#ifdef VERT_DIFF_ANALYSIS
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertDiffFlux_avg(i,j,k,iflux) = 
     &                       VertDiffFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
#endif
# ifdef FULL_PHYS_FLUX_ANALYSIS
            do iflux = 1, NT_PFA
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     TopFlux_avg(i,j,iflux) = TopFlux(i,j,iflux)  
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     BottomFlux_avg(i,j,iflux) = 
     &                    BottomFlux(i,j,iflux)
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorXMixFlux_avg(i,j,k,iflux) = 
     &                       HorXMixFlux(i,j,k,iflux)  
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorYMixFlux_avg(i,j,k,iflux) = 
     &                       HorYMixFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertMixFlux_avg(i,j,k,iflux) = 
     &                       VertMixFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        NudgingFlux_avg(i,j,k,iflux) = 
     &                       NudgingFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
# endif /* FULL_PHYS_FLUX_ANALYSIS */
         elseif (mod(iic-nts_phys_flux_avg,n_phys_flux_avg).gt.1) then
            if (ZEROTH_TILE) time_phys_flux_avg=
     &           time_phys_flux_avg+time
            do j = JstrR, JendR
               do i = IstrR, IendR
                  zeta_phys_flux_avg(i,j) = zeta_phys_flux_avg(i,j) +
     &                 zeta(i,j,knew)
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorXAdvFlux_avg(i,j,k,iflux) = 
     &                       HorXAdvFlux_avg(i,j,k,iflux) +
     &                       HorXAdvFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorYAdvFlux_avg(i,j,k,iflux) = 
     &                       HorYAdvFlux_avg(i,j,k,iflux) +
     &                       HorYAdvFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertAdvFlux_avg(i,j,k,iflux) = 
     &                       VertAdvFlux_avg(i,j,k,iflux) +
     &                       VertAdvFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
#ifdef VERT_DIFF_ANALYSIS
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertDiffFlux_avg(i,j,k,iflux) = 
     &                       VertDiffFlux_avg(i,j,k,iflux) +
     &                       VertDiffFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
#endif
# ifdef FULL_PHYS_FLUX_ANALYSIS
            do iflux = 1, NT_PFA
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     TopFlux_avg(i,j,iflux) = 
     &                    TopFlux_avg(i,j,iflux) + 
     &                    TopFlux(i,j,iflux)
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     BottomFlux_avg(i,j,iflux) = 
     &                    BottomFlux_avg(i,j,iflux) + 
     &                    BottomFlux(i,j,iflux)  
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorXMixFlux_avg(i,j,k,iflux) = 
     &                       HorXMixFlux_avg(i,j,k,iflux) +
     &                       HorXMixFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorYMixFlux_avg(i,j,k,iflux) = 
     &                       HorYMixFlux_avg(i,j,k,iflux) +
     &                       HorYMixFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertMixFlux_avg(i,j,k,iflux) = 
     &                       VertMixFlux_avg(i,j,k,iflux) +
     &                       VertMixFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        NudgingFlux_avg(i,j,k,iflux) = 
     &                       NudgingFlux_avg(i,j,k,iflux) +
     &                       NudgingFlux(i,j,k,iflux)
                     end do
                  end do
               end do
            end do
# endif /* FULL_PHYS_FLUX_ANALYSIS */
         elseif (mod(iic-nts_phys_flux_avg,n_phys_flux_avg).eq.0) then
            cff=1./float(n_phys_flux_avg)
            if (ZEROTH_TILE) time_phys_flux_avg=cff*
     &           (time_phys_flux_avg+time)
            do j = JstrR, JendR
               do i = IstrR, IendR
                  zeta_phys_flux_avg(i,j) = cff * 
     &                 ( zeta_phys_flux_avg(i,j) + 
     &                 zeta(i,j,knew) )
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorXAdvFlux_avg(i,j,k,iflux) = cff *
     &                       ( HorXAdvFlux_avg(i,j,k,iflux) +
     &                       HorXAdvFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorYAdvFlux_avg(i,j,k,iflux) = cff *
     &                       ( HorYAdvFlux_avg(i,j,k,iflux) +
     &                       HorYAdvFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertAdvFlux_avg(i,j,k,iflux) = cff *
     &                       ( VertAdvFlux_avg(i,j,k,iflux) +
     &                       VertAdvFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
#ifdef VERT_DIFF_ANALYSIS
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertDiffFlux_avg(i,j,k,iflux) = cff *
     &                       ( VertDiffFlux_avg(i,j,k,iflux) +
     &                       VertDiffFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
#endif
# ifdef FULL_PHYS_FLUX_ANALYSIS
            do iflux = 1, NT_PFA
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     TopFlux_avg(i,j,iflux) = cff *
     &                    ( TopFlux_avg(i,j,iflux) + 
     &                    TopFlux(i,j,iflux) )
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do j = JstrR, JendR
                  do i = IstrR, IendR 
                     BottomFlux_avg(i,j,iflux) = cff *
     &                    ( BottomFlux_avg(i,j,iflux) + 
     &                    BottomFlux(i,j,iflux) )
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorXMixFlux_avg(i,j,k,iflux) = cff *
     &                       ( HorXMixFlux_avg(i,j,k,iflux) +
     &                       HorXMixFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        HorYMixFlux_avg(i,j,k,iflux) = cff *
     &                       ( HorYMixFlux_avg(i,j,k,iflux) +
     &                       HorYMixFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 0, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        VertMixFlux_avg(i,j,k,iflux) = cff *
     &                       ( VertMixFlux_avg(i,j,k,iflux) +
     &                       VertMixFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
            do iflux = 1, NT_PFA
               do k = 1, N
                  do j = JstrR, JendR
                     do i = IstrR, IendR 
                        NudgingFlux_avg(i,j,k,iflux) = cff *
     &                       ( NudgingFlux_avg(i,j,k,iflux) +
     &                       NudgingFlux(i,j,k,iflux) )
                     end do
                  end do
               end do
            end do
# endif /* FULL_PHYS_FLUX_ANALYSIS */
         endif
      endif
      return
      end
#else /* PHYS_FLUX_ANALYSIS && AVERAGES */
      subroutine set_phys_flux_avg_empty
      end
#endif /* PHYS_FLUX_ANALYSIS && AVERAGES */

