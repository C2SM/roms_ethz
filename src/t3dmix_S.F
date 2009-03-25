#include "cppdefs.h"
 
      subroutine t3dmix (tile)
      implicit none
      integer tile, itrc
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      do itrc=1,NT
        call t3dmix_tile (Istr,Iend,Jstr,Jend, itrc, A2d(1,1),
     &                                     A2d(1,2), A2d(1,3))
      enddo
      return
      end
 
      subroutine t3dmix_tile (Istr,Iend,Jstr,Jend, itrc, FX,FE,LapT)
!
!  This routine computes horizontal mixing of tracers
!  along S-surfaces.
!
      implicit none
#   include "param.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j,k
      real FX(PRIVATE_2D_SCRATCH_ARRAY),     cff1,
     &     FE(PRIVATE_2D_SCRATCH_ARRAY),     cff2,
     &     LapT(PRIVATE_2D_SCRATCH_ARRAY)
#include "grid.h"
#include "ocean3d.h"
#include "mixing.h"
#include "climat.h"
#include "scalars.h"
!
#include "compute_auxiliary_bounds.h"
!
      do k=1,N
 
#ifdef TS_DIF2
!
!  Add in horizontal Laplacian diffusion along constant S-surfaces.
!--------------------------------------------------------------------
!  Compute XI- and ETA-components of diffusive tracer flux.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
            FX(i,j)=0.25*(diff2(i,j,itrc)+diff2(i-1,j,itrc))
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k,itrc)+tclm(i-1,j,k,itrc)
# endif
     &                                                              )
# ifdef MASKING
     &                                                   *umask(i,j)
# endif
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
            FE(i,j)=0.25*(diff2(i,j,itrc)+diff2(i,j-1,itrc))
     &                     *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                      t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k,itrc)+tclm(i,j-1,k,itrc)
# endif
     &                                                              )
# ifdef MASKING
     &                                                   *vmask(i,j)
# endif
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!  Multiply by mixing coefficient.
!
        do j=Jstr,Jend
          do i=Istr,Iend
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*pm(i,j)*pn(i,j)
     &                         *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                                     /Hz(i,j,k)
          enddo
        enddo
#endif /* TS_DIF2 */
#ifdef TS_DIF4
!
!  Compute/Add in horizontal biharmonic diffusion along constant
!  S-surfaces.
!-----------------------------------------------------------------
!  The biharmonic operator is computed by applying the Laplacian
!  operator twice.
!
!  Compute horizontal tracer flux in the XI-direction at U-points.
!
        do j=Jstr-1,Jend+1
          do i=max(Istr-1,1),min(Iend+2,L)
            FX(i,j)=pmon_u(i,j)*0.5*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                      t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k,itrc)+tclm(i-1,j,k,itrc)
# endif
     &                                                              )
# ifdef MASKING
     &                                                   *umask(i,j)
# endif
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
            FX(0,j)=pmon_u(Lm2,j)*0.5*(Hz(Lm2,j,k)+Hz(L-3,j,k))*(
     &                     t(Lm2,j,k,nrhs,itrc)-t(L-3,j,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                         -tclm(Lm2,j,k,itrc)+tclm(L-3,j,k,itrc)
# endif
     &                                                              )
# ifdef MASKING
     &                                                 *umask(Lm2,j)
# endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
            FX(Lp,j)=pmon_u(3,j)*0.5*(Hz(3,j,k)+Hz(2,j,k))*(
     &                        t(3,j,k,nrhs,itrc)-t(2,j,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                             -tclm(3,j,k,itrc)+tclm(2,j,k,itrc)
# endif
     &                                                              )
# ifdef MASKING
     &                                                   *umask(3,j)
# endif
          enddo
        endif
# endif /* EW_PERIODIC */
!
!  Compute horizontal tracer flux in the ETA-direction at V-points.
!
        do j=max(Jstr-1,1),min(Jend+2,M)
          do i=Istr-1,Iend+1
            FE(i,j)=pnom_v(i,j)*0.5*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k,itrc)+tclm(i,j-1,k,itrc)
# endif
     &                                                              )
# ifdef MASKING
     &                                                   *vmask(i,j)
# endif
          enddo
        enddo
# ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
            FE(i,0)=pnom_v(i,Mm2)*0.5*(Hz(i,Mm2,k)+Hz(i,M-3,k))*(
     &                   t(i,Mm2,k,nrhs,itrc)-t(i,M-3,k,nrhs,itrc)
#  ifdef CLIMAT_TS_MIXH
     &                        -tclm(i,Mm2,k,itrc)+tclm(i,M-3,k,itrc)
#  endif
     &                                                              )
#  ifdef MASKING
     &                                                 *vmask(i,Mm2)
#  endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
            FE(i,Mp)=pnom_v(i,3)*0.5*(Hz(i,3,k)+Hz(i,2,k))*(
     &                       t(i,3,k,nrhs,itrc)-t(i,2,k,nrhs,itrc)
#  ifdef CLIMAT_TS_MIXH
     &                            -tclm(i,3,k,itrc)+tclm(i,2,k,itrc)
#  endif
     &                                                              )
#  ifdef MASKING
     &                                                   *vmask(i,3)
#  endif
          enddo
        endif
# endif /* NS_PERIODIC */
!
!  Compute first Laplacian, without mixing coefficient.
!  Multiply by the metrics of the second Laplacian.
!  Save into work array "LapT".
!
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            LapT(i,j)=(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                          *pm(i,j)*pn(i,j)/Hz(i,j,k)
          enddo
        enddo
!
!  Apply boundary conditions (except periodic; closed or gradient)
!  to the first Laplacian.
!
# ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
#  ifdef WESTERN_WALL
            LapT(Istr-1,j)=0.
#  else
            LapT(Istr-1,j)=LapT(Istr,j)
#  endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
#  ifdef EASTERN_WALL
            LapT(Iend+1,j)=0.
#  else
            LapT(Iend+1,j)=LapT(Iend,j)
#  endif
          enddo
        endif
# endif /* !EW_PERIODIC */
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
#  ifdef SOUTHERN_WALL
            LapT(i,Jstr-1)=0.
#  else
            LapT(i,Jstr-1)=LapT(i,Jstr)
#  endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
#  ifdef NORTHERN_WALL
            LapT(i,Jend+1)=0.
#  else
            LapT(i,Jend+1)=LapT(i,Jend)
#  endif
          enddo
        endif
# endif /* !NS_PERIODIC */
!
!  Compute FX=d(LapT)/d(xi) and FE=d(LapT)/d(eta) terms
!  After that cmpute and add in biharmonic mixing [T m^3/s].
!  Multiply by mixing coefficient.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
            FX(i,j)=0.25*(diff4(i,j,itrc)+diff4(i-1,j,itrc))
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                 *(LapT(i,j)-LapT(i-1,j))
# ifdef MASKING
     &                                           *umask(i,j)
# endif
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
            FE(i,j)=0.25*(diff4(i,j,itrc)+diff4(i,j-1,itrc))
     &                   *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                 *(LapT(i,j)-LapT(i,j-1))
# ifdef MASKING
     &                                           *vmask(i,j)
# endif
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*pm(i,j)*pn(i,j)
     &                         *(FX(i,j)-FX(i+1,j)+FE(i,j)-FE(i,j+1))
     &                                                     /Hz(i,j,k)
          enddo
        enddo
#endif /* TS_DIF4 */
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
# endif
      return
      end
 
