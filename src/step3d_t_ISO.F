#include "cppdefs.h"
#ifdef SOLVE3D

#ifndef ADV_ISONEUTRAL
# define UPSTREAM_TS
#endif
c--# define AKIMA
c--# define CONST_TRACERS

! The following two CPPs activate switching triad and stabilizing
! algorithms. They must always be defined whenever the
! isoneutral hyperdiffusion is selected.

# ifdef ADV_ISONEUTRAL
#  define SW_TRIADS
#  define STABILIZE
# endif
                                  ! Note that arrays A2d(1,1:4) within
      subroutine step3d_t(tile)   ! the list of agruments in call below
      implicit none               ! are repeated to use the same memory
      integer tile                ! for vertical and horizontal scratch
                                  ! arrays.
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"

      call step3d_t_iso_tile( istr,iend,jstr,jend,
     &                       A2d(1,1), A2d(1,2), A2d(1,3), A2d(1,4),
     &                       A2d(1,1), A2d(1,2), A2d(1,3)
# ifdef ADV_ISONEUTRAL
     &                     , A2d(1,4), A2d(1,6), A2d(1,8), A2d(1,10)
     &                                                    , A3d(1,1)
#  ifdef STABILIZE
     &                                                    , A3d(1,2)
#  endif
# endif
     &                                                             )
      end

      subroutine step3d_t_iso_tile( istr,iend,jstr,jend, WC,FC,CF,DC
     &                                                 ,  FX,FE, wrk1
# ifdef ADV_ISONEUTRAL
     &                                           , FSC,dTdz,dTdx,dTde
     &                                                         , LapT
#  ifdef STABILIZE
     &                                                         ,  Akz
#  endif
# endif
     &                                                             )
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, imin,imax,jmin,jmax, i,j,k
      real, dimension(PRIVATE_1D_SCRATCH_ARRAY,0:N) :: WC,FC,CF,DC
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY)     :: FX,FE, wrk1
# ifdef ADV_ISONEUTRAL
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY,2) :: FSC,  dTdz,
     &                                               dTdx, dTde
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY,N) :: LapT
#  ifdef STABILIZE
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY,0:N) :: Akz
#  endif
# endif
      real cff
      real, parameter :: epsil=1.E-33
      integer itrc, iAkt
# ifdef ADV_ISONEUTRAL
      integer k1,k2
      real sumX,sumE
#  ifdef SW_TRIADS
      integer idx,ide
      real, dimension(0:4), parameter :: wgt=(/ 0., 1., 0.5,
     &                           0.3333333333333333, 0.25 /)
#  endif
#  ifdef  STABILIZE
      real cff2, cffX, s2_XLU,s2_XRU, s2_ELU,s2_ERU,
     &           cffE, s2_XLL,s2_XRL, s2_ELL,s2_ERL
#  endif
# endif



# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
# include "eos_vars.h"
#if defined TSOURCE || defined AGE_DYE_TRACER
# include "sources.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
# include "physflux.h"
#endif
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
# include "ncvars.h"
      integer is
      logical source_active
#endif
#ifdef PHYS_FLUX_ANALYSIS
! temporary variable for calculation of vertical diffusive flux
      real ThisVDF(PRIVATE_2D_SCRATCH_ARRAY,0:N)
#endif /* PHYS_FLUX_ANALYSIS */
#ifdef ADV_WENO
 #define T_HADV_WENO
 #define T_VADV_WENO
#endif
#if defined T_HADV_WENO || defined T_VADV_WENO
      REAL    :: vel, flux2
      REAL    :: flx2, flx3
      REAL    :: mask1
      REAL    :: flux3_weno, flux5_weno
#endif



# include "compute_auxiliary_bounds.h"

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        imin=istr
      else
        imin=istr-1
      endif
      if (EASTERN_EDGE) then
        imax=iend
      else
        imax=iend+1
      endif
# else
      imin=istr-1
      imax=iend+1
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        jmin=jstr
      else
        jmin=jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=jend
      else
        jmax=jend+1
      endif
# else
      jmin=jstr-1
      jmax=jend+1
# endif
#  define ntdf nstp
c--#  define ntdf nrhs

! Note that k-loop is recursive in the case of ADV_ISONEUTRAL switch
! active, while and "k1" and "k2" are alternating indices, 1-2-1-2-...
! etc, designed to mimic full 3D arrays, while using only two vertical
! slices. This applies to arrays  dTdx,dTde (both are at vertical RHO-
! points) and  FSC,dTdz (at vertical W-points). The index mapping
! rules for k1,k2 are as follows:
!   (i) k2 is always above k1;
!  (ii) arrays of W-type are computed ahead at k+1 --> k2
! (iii) arrays of RHO-type are computed at k -


      do itrc=1,NT
# ifdef ADV_ISONEUTRAL
        k2=1
        do k=0,N,+1       !--> recursive
          k1=k2
          k2=3-k1
          if (k > 0) then
# else
        do k=1,N
# endif

! Compute and add in contribution due to horizontal advection fluxes.
! Note that at this moment the content of array t(:,:,:,nnew,itrc) is
! concentration of tracer "itrc" taken at time corresponding to "nstp"
! and already MULTIPLIED by Hz(:,:,:) at "nstp" in "pre_step3d_tile".

# include "compute_horiz_tracer_fluxes.h"

            do j=jstr,jend
              do i=istr,iend
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) -dt*pm(i,j)
     &                                *pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                          +FE(i,j+1)-FE(i,j)
     &                                                           )
              enddo
            enddo                 !-->  discard FX,FE

# ifdef ADV_ISONEUTRAL
          endif !<-- k > 0


! Computation of the rotated (isoneutral) biharmonic operator expects
! the following five fields be defined at entry:
!
!         d rho |(ad)           d rho |(ad)           [ d rho|(ad)]^-1
!  dRdx = ------|        dRde = ------|        idRz = [ -----|    ]
!          d xi | s             d eta | s             [  d z |    ]
!
!                    diff3u               diff3v
!
!  WARNING: it is assumed here that diff3u and diff3v are the square
! roots of the hyperdiffusivities
!
!
!                     /  |       Schematic of variable placement on
!                   /    |       the vertical grid and explanation of
!                 /      |       meaning of alternating indices "k1"
!               /        |       and "k2" in the code below:
!             /          |
!           /            |
!         /        dTdx(i+1,j,k2)       ------------------
!       /                |             |                  |
!      |                 |             |                  |
!      |                 |             |                  |
!      |                 |             |                  |
!      |                /|       dTdx(i,j,k2)     dTdx(i+1,j,k2)  k+1
!      |              /  |             |                  |
! dTdx(i,j,k2)      /    |             |                  |
!      |          /      |             |                  |
!      |   dTdz(i,j,k2)  |             |                  |
!      |      /          |              -- dTdz(i,j,k2) --        k+1/2
!      |    /      dTdx(i+1,j,k1)      |                  |
!      |  /              |             |                  |
!      |/                |             |                  |
!      |                 |             |                  |
!      |                 |       dTdx(i,j,k1)     dTdx(i+1,j,k1)  k
!      |                 |             |                  |
!      |                /              |                  |
!      |              /                |                  |
! dTdx(i,j,k1)      /                  |                  |
!      |          /                     -- dTdz(i,j,k1) --        k-1/2
!      |    dTdz(i,j,k1)
!      |      /
!      |    /
!      |  /   Physical grid,                logical grid
!      |/   X-Z cross-section


!  Add in horizontal biharmonic diffusion along rotated surfaces.
!  The biharmonic operator is computed by applying the rotated
!  Laplacian operator twice.
!

! THE FIRST ROTATED LAPLACIAN OPERATOR:
!---- ----- ------- --------- ----------


! Initialize computation of FSC flux appearing due to projection of
! along-isopycnal fluxes onto vertical direction by setting first to
! just metric term FSC=1/[drho/dz]. This metric term will later be
! converted later to the actual flux.

          if (k == 0) then
            do j=jmin-1,jmax+1
              do i=imin-1,imax+1
                FSC(i,j,k2)=0.
                dTdz(i,j,k2)=idRz(i,j,1)*( t(i,j,2,ntdf,itrc)
     &                                   - t(i,j,1,ntdf,itrc) )
              enddo
            enddo
          elseif (k < N) then
            do j=jmin-1,jmax+1
              do i=imin-1,imax+1
                FSC(i,j,k2)=idRz(i,j,k)*(z_r (i,j,k+1)-z_r (i,j,k))
                dTdz(i,j,k2)=idRz(i,j,k)*( t(i,j,k+1,ntdf,itrc)
     &                                       - t(i,j,k,ntdf,itrc) )
              enddo
            enddo
          elseif (k == N) then
            do j=jmin-1,jmax+1
              do i=imin-1,imax+1
                FSC(i,j,k2)=0.
                dTdz(i,j,k2)=dTdz(i,j,k1)
              enddo
            enddo
          endif

! Note that dTdx,dTde are computed one-step-ahead along k-loop before
! they are used to compute FX,FE below.  This migh seem to be off, but
! it should be kept in mind that dTdx,dTde are needed for dual purpose:
! (i) to compute FX,FE, and eventually, to compute slope-contributions
! to FSC further below, where both slices of dTdx,dTde will be used
! simultaneously.

          if (k < N) then
            do j=jmin,jmax
              do i=imin,imax+1
                dTdx(i,j,k2)=0.5*(pm(i,j)+pm(i-1,j))*(
     &            t(i,j,k+1,ntdf,itrc)-t(i-1,j,k+1,ntdf,itrc) )
#  ifdef MASKING
     &                                              *umask(i,j)
#  endif
              enddo
            enddo

            do j=jmin,jmax+1
              do i=imin,imax
                dTde(i,j,k2)=0.5*(pn(i,j)+pn(i,j-1))*(
     &            t(i,j,k+1,ntdf,itrc)-t(i,j-1,k+1,ntdf,itrc) )
#  ifdef MASKING
     &                                              *vmask(i,j)
#  endif
              enddo
            enddo
          endif  !<-- k < N

! Compute horizontal components of isopycnic diffusive flux

          if (k > 0) then
            do j=jmin,jmax
              do i=imin,imax+1
                FX(i,j)=diff3u(i,j,k)*0.5*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                                  *dn_u(i,j)*(
     &                       dTdx(i,j,k1)
#  ifdef SW_TRIADS
     &       -0.5*( min(dRdx(i,j,k),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &             +max(dRdx(i,j,k),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1))
#  else
     &                -0.25*dRdx(i,j,k)*( dTdz(i-1,j,k1)+dTdz(i,j,k2)
     &                                   +dTdz(i-1,j,k2)+dTdz(i,j,k1)
#  endif
     &                                                            ) )
              enddo
            enddo

            do j=jmin,jmax+1
              do i=imin,imax
                FE(i,j)=diff3v(i,j,k)*0.5*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                                  *dm_v(i,j)*(
     &                       dTde(i,j,k1)
#  ifdef SW_TRIADS
     &       -0.5*(min(dRde(i,j,k),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &            +max(dRde(i,j,k),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1)))
#  else
     &                -0.25*dRde(i,j,k)*(dTdz(i,j-1,k1)+dTdz(i,j,k2)
     &                                  +dTdz(i,j-1,k2)+dTdz(i,j,k1))
#  endif
     &                                                             )
              enddo
            enddo  !--> keep dTdx,dTde

! Finalize computation of the vertical component FSC

            if (k < N) then
              do j=jmin,jmax
                do i=imin,imax
#  ifdef SW_TRIADS
                  sumX=0. ; idx=0

                  if (dRdx(i,j,k) < 0.) then
                   sumX=diff3u(i,j,k)*dRdx(i,j,k)*(
     &                    dRdx(i,j,k)*dTdz(i,j,k2)-dTdx(i,j,k1) )
                   idx=1
                  endif

                  if (dRdx(i,j,k+1) > 0.) then
                   sumX=sumX + diff3u(i,j,k+1)*dRdx(i,j,k+1)*(
     &                           dRdx(i,j,k+1)*dTdz(i,j,k2)
     &                                        -dTdx(i,j,k2) )
                   idx=idx+1
                  endif

                  if (dRdx(i+1,j,k+1) < 0.) then
                   sumX=sumX + diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*(
     &                           dRdx(i+1,j,k+1)*dTdz(i  ,j,k2)
     &                                          -dTdx(i+1,j,k2) )
                   idx=idx+1
                  endif

                  if (dRdx(i+1,j,k) > 0.) then
                   sumX=sumX + diff3u(i+1,j,k)*dRdx(i+1,j,k)*(
     &                           dRdx(i+1,j,k)*dTdz(i  ,j,k2)
     &                                        -dTdx(i+1,j,k1) )
                   idx=idx+1
                  endif
#  else
                  sumX=diff3u(i,j,k)*dRdx(i,j,k)*(
     &              dRdx(i,j,k)*dTdz(i,j,k2)-dTdx(i,j,k1) )

     &              +diff3u(i,j,k+1)*dRdx(i,j,k+1)*(
     &                   dRdx(i,j,k+1)*dTdz(i,j,k2)-dTdx(i,j,k2) )

     &              +diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*(
     &               dRdx(i+1,j,k+1)*dTdz(i,j,k2)-dTdx(i+1,j,k2) )

     &              +diff3u(i+1,j,k)*dRdx(i+1,j,k)*(
     &               dRdx(i+1,j,k)*dTdz(i,j,k2)-dTdx(i+1,j,k1) )
#  endif

#  ifdef SW_TRIADS
                  sumE=0. ; ide=0

                  if (dRde(i,j,k) < 0.) then
                    sumE=diff3v(i,j,k)*dRde(i,j,k)*(
     &                     dRde(i,j,k)*dTdz(i,j,k2)
     &                                -dTde(i,j,k1) )
                   ide=1
                  endif

                  if (dRde(i,j,k+1) > 0.) then
                    sumE=sumE + diff3v(i,j,k+1)*dRde(i,j,k+1)*(
     &                            dRde(i,j,k+1)*dTdz(i,j,k2)
     &                                         -dTde(i,j,k2) )
                   ide=ide+1
                  endif

                  if (dRde(i,j+1,k+1) <  0.) then
                    sumE=sumE + diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*(
     &                            dRde(i,j+1,k+1)*dTdz(i,j  ,k2)
     &                                           -dTde(i,j+1,k2) )
                   ide=ide+1
                  endif

                  if (dRde(i,j+1,k) > 0.) then
                    sumE=sumE + diff3v(i,j+1,k)*dRde(i,j+1,k)*(
     &                            dRde(i,j+1,k)*dTdz(i,j  ,k2)
     &                                         -dTde(i,j+1,k1) )
                    ide=ide+1
                  endif

                  FSC(i,j,k2)=FSC(i,j,k2)*(sumX*wgt(idx)+sumE*wgt(ide))
#  else
                  sumE=diff3v(i,j,k)*dRde(i,j,k)*(
     &                  dRde(i,j,k)*dTdz(i,j,k2)-dTde(i,j,k1) )

     &                +diff3v(i,j,k+1)*dRde(i,j,k+1)*(
     &                  dRde(i,j,k+1)*dTdz(i,j,k2)-dTde(i,j,k2) )

     &                +diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*(
     &                dRde(i,j+1,k+1)*dTdz(i,j,k2)-dTde(i,j+1,k2) )

     &              +diff3v(i,j+1,k)*dRde(i,j+1,k)*(
     &                  dRde(i,j+1,k)*dTdz(i,j,k2)-dTde(i,j+1,k1) )

                  FSC(i,j,k2)=FSC(i,j,k2)*0.25*(sumX+sumE)
#  endif
                enddo
              enddo
            endif     !<--   k < N
            do j=jmin,jmax
              do i=imin,imax
                LapT(i,j,k)=( pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                         +FE(i,j+1)-FE(i,j)
     &                      )+FSC(i,j,k2)-FSC(i,j,k1) )/Hz(i,j,k)
              enddo
            enddo
          endif   ! <-- k > 0
        enddo   ! --> k


! Apply lateral boundary conditions to the Laplacian (for the cases
! other than periodic: closed or gradient.

#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do k=1,N
            do j=jmin,jmax
#   ifndef OBC_WEST
              LapT(istr-1,j,k)=0.
#   else
              LapT(istr-1,j,k)=LapT(istr,j,k)
#   endif
            enddo
          enddo
        endif
        if (EASTERN_EDGE) then
          do k=1,N
            do j=jmin,jmax
#   ifndef OBC_EAST
              LapT(iend+1,j,k)=0.
#   else
              LapT(iend+1,j,k)=LapT(iend,j,k)
#   endif
            enddo
          enddo
        endif
#  endif
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do k=1,N
            do i=imin,imax
#   ifndef OBC_SOUTH
              LapT(i,jstr-1,k)=0.
#   else
              LapT(i,jstr-1,k)=LapT(i,jstr,k)
#   endif
            enddo
          enddo
        endif
        if (NORTHERN_EDGE) then
          do k=1,N
            do i=imin,imax
#   ifndef OBC_NORTH
              LapT(i,jend+1,k)=0.
#   else
              LapT(i,jend+1,k)=LapT(i,jend,k)
#   endif
            enddo
          enddo
        endif
#  endif /* !NS_PERIODIC */

! The second rotated Laplacian operator:
!---- ------ ------- --------- ----------

        k2=1
        do k=0,N,+1
          k1=k2
          k2=3-k1

          if (k == 0) then
            do j=jstr-1,jend+1
              do i=istr-1,iend+1
                FSC(i,j,k2)=0.
                dTdz(i,j,k2)=idRz(i,j,1)*(LapT(i,j,2)-LapT(i,j,1))
              enddo
            enddo
          elseif (k < N) then
            do j=jstr-1,jend+1
              do i=istr-1,iend+1
                FSC(i,j,k2)= idRz(i,j,k)*(z_r (i,j,k+1)-z_r (i,j,k))
                dTdz(i,j,k2)=idRz(i,j,k)*(LapT(i,j,k+1)-LapT(i,j,k))
              enddo
            enddo
          elseif (k == N) then
            do j=jmin-1,jmax+1
              do i=imin-1,imax+1
                FSC(i,j,k2)=0.
                dTdz(i,j,k2)=dTdz(i,j,k1)
              enddo
            enddo
          endif

          if (k < N) then
            do j=jstr,jend
              do i=istr,iend+1
                dTdx(i,j,k2)=0.5*(pm(i,j)+pm(i-1,j))*( LapT(i,j,k+1)
     &                                              -LapT(i-1,j,k+1))
#  ifdef MASKING
     &                                                    *umask(i,j)
#  endif
              enddo
            enddo
            do j=jstr,jend+1
              do i=istr,iend
                dTde(i,j,k2)=0.5*(pn(i,j)+pn(i,j-1))*( LapT(i,j,k+1)
     &                                              -LapT(i,j-1,k+1))
#  ifdef MASKING
     &                                                    *vmask(i,j)
#  endif
              enddo
            enddo
          endif

          if (k > 0) then
            do j=jstr,jend
              do i=istr,iend+1
                FX(i,j)=-diff3u(i,j,k)*0.5*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                                 *dn_u(i,j)*(
     &                      dTdx(i,j,k1)
#  ifdef SW_TRIADS
     &     -0.5*(min(dRdx(i,j,k),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &          +max(dRdx(i,j,k),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1)))
#  else
     &              -0.25*dRdx(i,j,k)*(dTdz(i-1,j,k1)+dTdz(i,j,k2)
     &                                +dTdz(i-1,j,k2)+dTdz(i,j,k1))
#  endif
     &                                                           )
              enddo
            enddo
# ifdef PHYS_FLUX_ANALYSIS
            if (itrc >= NT_PFA) then
              do j=Jstr,Jend
                do i=Istr,Iend+1
                                ! unit: mmol / s
                  HorXAdvFlux(i,j,k,itrc) = FX(i,j)
#  ifdef MASKING
     &                  * rmask(i,j)
#  endif
               enddo
             enddo
           endif
# endif /* PHYS_FLUX_ANALYSIS */

            do j=jstr,jend+1
              do i=istr,iend
                FE(i,j)=-diff3v(i,j,k)*0.5*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                                 *dm_v(i,j)*(
     &                      dTde(i,j,k1)
#  ifdef SW_TRIADS
     &     -0.5*(min(dRde(i,j,k),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &          +max(dRde(i,j,k),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1)))
#  else
     &              -0.25*dRde(i,j,k)*(dTdz(i,j-1,k1)+dTdz(i,j,k2)
     &                                +dTdz(i,j-1,k2)+dTdz(i,j,k1))
#  endif
     &                                                           )
              enddo
            enddo
# ifdef PHYS_FLUX_ANALYSIS
           if (itrc >= NT_PFA) then
             do j=Jstr,Jend+1
               do i=Istr,Iend
                                ! unit: mmol / s
                 HorYAdvFlux(i,j,k,itrc) = FE(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
               enddo
             enddo
           endif
# endif /* PHYS_FLUX_ANALYSIS */

            if (k < N) then    ! Compute the vertical component...
              do j=jstr,jend
                do i=istr,iend

#  ifdef STABILIZE

                  s2_XLL=(FSC(i,j,k2)*dRdx(i,j,  k  ))**2
                  s2_XLU=(FSC(i,j,k2)*dRdx(i,j,  k+1))**2
                  s2_XRU=(FSC(i,j,k2)*dRdx(i+1,j,k+1))**2
                  s2_XRL=(FSC(i,j,k2)*dRdx(i+1,j,k  ))**2

                  s2_ELL=(FSC(i,j,k2)*dRde(i,j  ,k  ))**2
                  s2_ELU=(FSC(i,j,k2)*dRde(i,j  ,k+1))**2
                  s2_ERU=(FSC(i,j,k2)*dRde(i,j+1,k+1))**2
                  s2_ERL=(FSC(i,j,k2)*dRde(i,j+1,k  ))**2

c**               cff=1./(z_r(i,j,k+1)-z_r(i,j,k))
                  cff=2./(Hz(i,j,k+1)+Hz(i,j,k))

                  cff2=cff**2
                  cffX=pm(i,j)**2
                  cffE=pn(i,j)**2

#   ifdef SW_TRIADS
                  Akz(i,j,k) = 15.*(  max( diff3u(i  ,j,k  )*s2_XLL,
     &                                     diff3u(i+1,j,k  )*s2_XRL,
     &                                     diff3u(i  ,j,k+1)*s2_XLU,
     &                                     diff3u(i+1,j,k+1)*s2_XRU)

     &                               +max( diff3v(i,j  ,k  )*s2_ELL,
     &                                     diff3v(i,j+1,k  )*s2_ERL,
     &                                     diff3v(i,j  ,k+1)*s2_ELU,
     &                                     diff3v(i,j+1,k+1)*s2_ERU)
     &                 )*(
     &                     max( diff3u(i  ,j,k  )*(cffX + cff2*s2_XLL),
     &                          diff3u(i  ,j,k+1)*(cffX + cff2*s2_XLU),
     &                          diff3u(i+1,j,k+1)*(cffX + cff2*s2_XRU),
     &                          diff3u(i+1,j,k  )*(cffX + cff2*s2_XRL))

     &                    +max( diff3v(i,j  ,k  )*(cffE + cff2*s2_ELL),
     &                          diff3v(i,j  ,k+1)*(cffE + cff2*s2_ELU),
     &                          diff3v(i,j+1,k+1)*(cffE + cff2*s2_ERU),
     &                          diff3v(i,j+1,k  )*(cffE + cff2*s2_ERL))
     &                   )
#   else
                  Akz(i,j,k) = 0.5*(  diff3u(i  ,j,k  )*s2_XLL
     &                               +diff3u(i+1,j,k  )*s2_XRL
     &                               +diff3u(i  ,j,k+1)*s2_XLU
     &                               +diff3u(i+1,j,k+1)*s2_XRU

     &                               +diff3v(i,j,k  )*s2_ELL
     &                               +diff3v(i,j+1,k)*s2_ERL
     &                               +diff3v(i,j,k+1)*s2_ELU
     &                               +diff3v(i,j+1,k+1)*s2_ERU
     &                      )*(
     &                          diff3u(i  ,j,k  )*(cffX + cff2*s2_XLL)
     &                         +diff3u(i  ,j,k+1)*(cffX + cff2*s2_XLU)
     &                         +diff3u(i+1,j,k+1)*(cffX + cff2*s2_XRU)
     &                         +diff3u(i+1,j,k  )*(cffX + cff2*s2_XRL)

     &                         +diff3v(i,j  ,k  )*(cffE + cff2*s2_ELL)
     &                         +diff3v(i,j  ,k+1)*(cffE + cff2*s2_ELU)
     &                         +diff3v(i,j+1,k+1)*(cffE + cff2*s2_ERU)
     &                         +diff3v(i,j+1,k  )*(cffE + cff2*s2_ERL)
     &                       )
#   endif
#  endif  /* STABILIZE */



#  ifdef SW_TRIADS
                  sumX=0. ; idx=0

                  if (dRdx(i,j,k) < 0.) then
                    sumX=diff3u(i,j,k)*dRdx(i,j,k)*(
     &                                 dRdx(i,j,k)*dTdz(i,j,k2)
     &                                            -dTdx(i,j,k1) )
                    idx=1
                  endif

                  if (dRdx(i,j,k+1) > 0.) then
                    sumX=sumX + diff3u(i,j,k+1)*dRdx(i,j,k+1)*(
     &                            dRdx(i,j,k+1)*dTdz(i,j,k2)
     &                                         -dTdx(i,j,k2) )
                    idx=idx+1
                  endif

                  if (dRdx(i+1,j,k+1) < 0.) then
                    sumX=sumX + diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*(
     &                            dRdx(i+1,j,k+1)*dTdz(i  ,j,k2)
     &                                           -dTdx(i+1,j,k2) )
                    idx=idx+1
                  endif

                  if (dRdx(i+1,j,k  ) > 0.) then
                    sumX=sumX + diff3u(i+1,j,k)*dRdx(i+1,j,k)*(
     &                            dRdx(i+1,j,k)*dTdz(i  ,j,k2)
     &                                         -dTdx(i+1,j,k1) )
                    idx=idx+1
                  endif
#  else
                  sumX=diff3u(i  ,j,k  )*dRdx(i  ,j,k  )
     &                 *(dRdx(i  ,j,k  )*dTdz(i  ,j,k2 )-dTdx(i  ,j,k1))
     &                +diff3u(i  ,j,k+1)*dRdx(i  ,j,k+1)
     &                 *(dRdx(i  ,j,k+1)*dTdz(i  ,j,k2 )-dTdx(i  ,j,k2))
     &                +diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)
     &                 *(dRdx(i+1,j,k+1)*dTdz(i  ,j,k2 )-dTdx(i+1,j,k2))
     &                +diff3u(i+1,j,k  )*dRdx(i+1,j,k  )
     &                 *(dRdx(i+1,j,k  )*dTdz(i  ,j,k2 )-dTdx(i+1,j,k1))
#  endif
#  ifdef SW_TRIADS
                  sumE=0. ; ide=0

                  if (dRde(i,j,k) < 0.) then
                   sumE=diff3v(i,j,k)*dRde(i,j,k)*(
     &                                dRde(i,j,k)*dTdz(i,j,k2)
     &                                           -dTde(i,j,k1) )
                   ide=1
                  endif

                  if (dRde(i,j,k+1) > 0.) then
                   sumE=sumE + diff3v(i,j,k+1)*dRde(i,j,k+1)*(
     &                           dRde(i,j,k+1)*dTdz(i,j,k2)
     &                                        -dTde(i,j,k2) )
                   ide=ide+1
                  endif

                  if (dRde(i,j+1,k+1) < 0.) then
                   sumE=sumE + diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*(
     &                           dRde(i,j+1,k+1)*dTdz(i,j  ,k2)
     &                                          -dTde(i,j+1,k2) )
                   ide=ide+1
                  endif

                  if (dRde(i,j+1,k  ) > 0.) then
                   sumE=sumE + diff3v(i,j+1,k)*dRde(i,j+1,k)*(
     &                           dRde(i,j+1,k)*dTdz(i,j  ,k2)
     &                                        -dTde(i,j+1,k1) )
                   ide=ide+1
                  endif
#  else
                  sumE=diff3v(i,j  ,k  )*dRde(i,j  ,k  )
     &                 *(dRde(i,j  ,k  )*dTdz(i,j  ,k2 )-dTde(i,j ,k1))
     &                +diff3v(i,j  ,k+1)*dRde(i,j  ,k+1)
     &                 *(dRde(i,j  ,k+1)*dTdz(i,j  ,k2 )-dTde(i,j ,k2))
     &                +diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)
     &                 *(dRde(i,j+1,k+1)*dTdz(i,j  ,k2)-dTde(i,j+1,k2))
     &                +diff3v(i,j+1,k  )*dRde(i,j+1,k )
     &                 *(dRde(i,j+1,k  )*dTdz(i,j  ,k2)-dTde(i,j+1,k1))
#  endif


#  ifdef SW_TRIADS
                 FSC(i,j,k2)=-FSC(i,j,k2)*(sumX*wgt(idx)+sumE*wgt(ide))
#  else
                 FSC(i,j,k2)=-FSC(i,j,k2)*0.25*(sumX+sumE)
#  endif
#  ifdef STABILIZE
     &                         -cff*Akz(i,j,k)*( t(i,j,k+1,ntdf,itrc)
     &                                            -t(i,j,k,ntdf,itrc))
#  endif

                enddo
              enddo
            endif    !<-- k < N
            do j=jstr,jend
              do i=istr,iend
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) +dt*( pm(i,j)
     &               *pn(i,j)*(FX(i+1,j)-FX(i,j) +FE(i,j+1)-FE(i,j))
     &                                    +FSC(i,j,k2)-FSC(i,j,k1) )
              enddo
            enddo
          endif   !<-- k > 0
# endif /* ADV_ISONEUTRAL */
# ifdef PHYS_FLUX_ANALYSIS
!CN (Dec 2017): Location of writing consistent with that for vertical advective fluxes
          if (itrc .le. NT_PFA) then
! separate do loops (for better efficiency) to store the fluxes
             j_loop2: do j=Jstr,Jend
                do i=Istr,Iend+1
                                ! unit: mmol / s
                   HorXAdvFlux(i,j,k,itrc) = FX(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
                end do
             end do j_loop2
             j_loop3: do j=Jstr,Jend+1
                do i=Istr,Iend
                                ! unit: mmol / s
                   HorYAdvFlux(i,j,k,itrc) = FE(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
                enddo
             end do j_loop3
          end if
# endif /* PHYS_FLUX_ANALYSIS */

        enddo      !<-- k




! Compute and apply vertical advective fluxes.

        do j=jstr,jend

# include "compute_vert_tracer_fluxes.h"

          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) -dt*pm(i,j)*pn(i,j)
     &                                              *(FC(i,k)-FC(i,k-1))
            enddo
          enddo


! Start computation of vertical diffusion: add surface forcing fluxes


# ifdef PHYS_FLUX_ANALYSIS
          if (itrc .le. NT_PFA) then
! separate do loop (for better efficiency) to store the fluxes
             do k=0,N
                do i=Istr,Iend
               ! unit: mmol / (m2 s)
                   VertAdvFlux(i,j,k,itrc) = FC(i,k)
     &                  * pm(i,j) * pn(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
                enddo
             enddo
          end if
# endif /* PHYS_FLUX_ANALYSIS */
# ifdef TSOURCE
! AH17: Add tracer sources
! Add source and dillute all tracers with volume of water 
! added. Note that tracer flux Tflx is computed by set_forces.F. 
! Note1: t at position nnew is multiplied by Hz)
! Note2: If in future there should be a need to implement
! a tracer source that is not associated with a volume
! flux, consider an additional option to add the tracer
! directly without dillution: t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)
! +dt*T_src(i,j,k,itrc)*Hz(i,j,k)
! whereas T_src would need the unit of the tracer per second.
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*Tflx(i,j,k,itrc)
            enddo
          enddo
# endif /* TSOURCE */
! Start computation of vertical diffusion: add surface forcing fluxes
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
!DL: do this except for age tracers. Age tracers are dealt with in pre_step3d
          if (itrc.le.indxT+ntrc_salt .or. itrc.gt.indxT+ntrc_salt+ntrc_pas 
     &        .or. .not.Age(itrc-(indxT+ntrc_salt))) then
# endif
          do i=istr,iend
            t(i,j,N,nnew,itrc)=t(i,j,N,nnew,itrc)+dt*stflx(i,j,itrc)
          enddo
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
          endif ! itrc.le.indxT+ntrc_salt .or. ...
# endif
# ifdef FULL_PHYS_FLUX_ANALYSIS
          if (itrc .le. NT_PFA) then
! separate do loops (for better efficiency) to store the fluxes
             do i=Istr,Iend
             ! unit = mmol / (m2 s)
                TopFlux(i,j,itrc) = stflx(i,j,itrc)
#  ifdef MASKING
     &               * rmask(i,j)
#  endif
             enddo
#  if !defined SELECTED_FLUX_ANALYSIS /* AH */
             do i=Istr,Iend
             ! unit = mmol / (m2 s)
                BottomFlux(i,j,itrc) = 0. !-btflx(i,j,itrc)
#   ifdef MASKING
     &               * rmask(i,j)
#   endif
             enddo
#  endif /* SELECTED_FLUX_ANALYSIS */
          end if
# endif /* FULL_PHYS_FLUX_ANALYSIS */

# if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
! The vertical diffusive flux is not added in a step like the other fluxes,
! so we store the old concentration (depth is already factored in here) first, 
! and then add the new concentration.
          do k=1,N
            do i=Istr,Iend
               ThisVDF(i, j, k) = t(i,j,k,nnew,itrc)
            end do
         end do
# endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */

# if defined LMD_KPP && defined ICEOBS && defined BRINE_PLUMES
! AH15: Distributing salt fluxes due freezing uniformly over
! kbl to mimic brine plume convection (see Duffy et al. 1999)
          if (itrc .eq. isalt) then
            do i=istr,iend
              if (freezing(i,j)>0) then
#  if defined FULL_PHYS_FLUX_ANALYSIS
                TopFlux(i,j,itrc) = TopFlux(i,j,itrc)+freezing(i,j)
#   ifdef MASKING
     &               * rmask(i,j)
#   endif
#  endif /* FULL_PHYS_FLUX_ANALYSIS */
#  if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
                ThisVDF(i, j, N) = ThisVDF(i, j, N)+dt*freezing(i,j)
#  endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */
                do k=N,kbl(i,j),-1 !k=kbl(i,j),N,+1
                  t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+(dt*freezing(i,j)
     &                  *Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kbl(i,j)-1)))
!     &                  *(z_w(i,j,k)-z_w(i,j,k-1))/(z_w(i,j,N)-z_w(i,j,kbl(i,j)-1)))
                enddo
              endif
            enddo
          endif
# endif

# ifdef LMD_KPP
! Add the solar radiation flux in temperature equation. Also compute
! the nonlocal transport flux for unstable (convective) forcing
! conditions into matrix DC when using the Large et al. 1994 KPP
! scheme.

          if (itrc == itemp) then
# ifdef FULL_PHYS_FLUX_ANALYSIS
            do i=istr,iend
             SRAbsFlux(i,j,N)=0.
            enddo
# endif /* FULL_PHYS_FLUX_ANALYSIS */
            do k=N-1,1,-1
              do i=istr,iend
                cff=srflx(i,j)*swr_frac(i,j,k)
#  ifdef LMD_NONLOCAL
     &                 -ghat(i,j,k)*(stflx(i,j,itemp)-srflx(i,j))
#  endif
                t(i,j,k+1,nnew,itemp)=t(i,j,k+1,nnew,itemp) -dt*cff
                t(i,j,k  ,nnew,itemp)=t(i,j,k  ,nnew,itemp) +dt*cff
# ifdef FULL_PHYS_FLUX_ANALYSIS
                SRAbsFlux(i,j,k+1)=SRAbsFlux(i,j,k+1)-cff
                SRAbsFlux(i,j,k  )=cff
# endif /* FULL_PHYS_FLUX_ANALYSIS */
              enddo
            enddo

c?? WARNING: the above implies that light (if any) reaching all the
c?? way to the bottom is entirely absorbed within the botom-most grid
c?? box, rather that reflected/scattered back to the water column. It
c?? is not clear, however, how to deal with this situation correctly
c??
c??         do i=istr,iend
c??           cff=srflx(i,j)*swr_frac(i,j,0)
c??#  ifdef LMD_NONLOCAL
c??  &               -ghat(i,j,0)*(stflx(i,j,itemp)-srflx(i,j))
c??#  endif
c??           t(i,j,1,nnew,itemp)=t(i,j,1,nnew,itemp) -dt*cff
c??         enddo

#  if defined LMD_NONLOCAL && defined SALINITY
          elseif (itrc == isalt) then
            do k=N-1,1,-1
              do i=istr,iend
                cff=-dt*ghat(i,j,k)*stflx(i,j,isalt)
                t(i,j,k+1,nnew,isalt)=t(i,j,k+1,nnew,isalt) -cff
                t(i,j,k  ,nnew,isalt)=t(i,j,k  ,nnew,isalt) +cff
              enddo
            enddo
#  endif
          endif
# endif

! Perform an implicit time step for vertical diffusion
!
!   dq(k)     1     [         q(k+1)-q(k)             q(k)-q(k-1) ]
!  ------ = ----- * [ Akt(k)* ----------- - Akt(k-1)* ----------- ]
!    dt     Hz(k)   [            dz(k)                   dz(k-1)  ]
!
! where q(k) represents tracer field t(:,:,k,:,itrc). Doing so
! implies solution of a tri-diagonal system
!
!     -FC(k-1)*q_new(k-1) +[Hz(k)+FC(k-1)+FC(k)]*q_new(k)
!                       -FC(k)*q_new(k+1) = Hz(k)*q_old(k)
!
!                dt*Akt(k)
! where FC(k) = ----------- is normalized diffusivity coefficient
!                  dz(k)
!
! defined at W-points; q_new(k) is the new-time-step (unknown) tracer
! field; q_old(k) is old-time-step tracer (known).   As long as Akt(k)
! is nonnegative, the tri-diagonal matrix is diagonally dominant which
! guarantees stability of the Gaussian elimination algorithm.  Top and
! bottom boundary conditions are assumed to be no-flux, effectively
! Akt(N)=Akt(0)=0, hence FC(N)=FC(1)=0. This leads to equations for
! top and bottom grid boxes;
!
!   -FC(N-1)*q_new(N-1) +[Hz(N)+FC(N-1)]*q_new(N) = Hz(N)*q_old(N)
!
!          [Hz(1)+FC(1)]*q_new(1) -FC(1)*q_new(2) = Hz(1)*q_old(1)
!
! The FC(N)=FC(0)=0 boundary conditions does not mean that physical
! boundary conditions are no flux: the forcing fluxes have been
! applied explicitly above.  Instead, the no-flux condition should
! be interpreted as that the implicit step merely redistributes the
! tracer concentration throughout the water column. At this moment
! the content of array t(:,:,:,nnew,itrc) has meaning of Hz*tracer.
! After the implicit step it becomes just tracer.

# if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
! The vertical diffusive flux is not added in a step like the other fluxes,
! so we store the old concentration (depth is already factored in here) first,
! and then add the new concentration.
          do k=1,N
            do i=Istr,Iend
               ThisVDF(i, j, k) = t(i,j,k,nnew,itrc)
            end do
         end do
# endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */

# ifdef BIOLOGY
#  ifdef SALINITY
          iAkt=min(itrc,isalt)
#  else
          iAkt=min(itrc,itemp)
#  endif
# else
          iAkt=itrc
# endif
          do i=istr,iend
            DC(i,0)=dt*pm(i,j)*pn(i,j) !<--horizontal metric

# if defined ADV_ISONEUTRAL && defined STABILIZE
            FC(i,1)=2.*dt*(Akt(i,j,1,iAkt)+Akz(i,j,1))
# else
            FC(i,1)=2.*dt* Akt(i,j,1,iAkt)
# endif
     &                           /(Hz(i,j,1)+Hz(i,j,2))
            WC(i,1)=DC(i,0)*Wi(i,j,1)

            cff=1./(Hz(i,j,1) +FC(i,1)+max(WC(i,1),0.))
            CF(i,1)=cff*(      FC(i,1)-min(WC(i,1),0.))
            DC(i,1)=cff*t(i,j,1,nnew,itrc)
          enddo
          do k=2,N-1,+1
            do i=istr,iend
# if defined ADV_ISONEUTRAL && defined STABILIZE
              FC(i,k)=2.*dt*(Akt(i,j,k,iAkt)+Akz(i,j,k))
# else
              FC(i,k)=2.*dt* Akt(i,j,k,iAkt)
# endif
     &                            /(Hz(i,j,k)+Hz(i,j,k+1))

              WC(i,k)=Wi(i,j,k)*DC(i,0)

              cff=1./( Hz(i,j,k) +FC(i,k)+max(WC(i,k),0.)
     &                         +FC(i,k-1)-min(WC(i,k-1),0.)
     &              -CF(i,k-1)*(FC(i,k-1)+max(WC(i,k-1),0.))
     &                                                     )

              CF(i,k)=cff*(       FC(i,k)-min(WC(i,k),0.) )

              DC(i,k)=cff*( t(i,j,k,nnew,itrc) +DC(i,k-1)*(
     &                          FC(i,k-1)+max(WC(i,k-1),0.) ))
            enddo
          enddo          !--> discard DC(:,0)
          do i=istr,iend
            t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc) +DC(i,N-1)*(
     &                               FC(i,N-1)+max(WC(i,N-1),0.) )
     &                   )/( Hz(i,j,N) +FC(i,N-1)-min(WC(i,N-1),0.)
     &                      -CF(i,N-1)*(FC(i,N-1)+max(WC(i,N-1),0.))
     &                                                             )
# ifdef MASKING
     &                                                  *rmask(i,j)
# endif
          enddo
          do k=N-1,1,-1
            do i=istr,iend
              t(i,j,k,nnew,itrc)=(DC(i,k)+CF(i,k)*t(i,j,k+1,nnew,itrc))
# ifdef MASKING
     &                                                      *rmask(i,j)
# endif

            enddo
          enddo           !--> discard FC,WC,CF,DC

# if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
          do k=1,N
            do i=Istr,Iend
               ! ThisVDF is the old concentration times depth
               ThisVDF(i,j,k) = 
     &              (ThisVDF(i,j,k) - t(i,j,k,nnew,itrc) * Hz(i,j,k))
     &              / dt
# ifdef MASKING
     &              * rmask(i,j)
# endif


            enddo
          enddo
          if (itrc .le. NT_PFA) then
             do i=Istr,Iend
             ! unit: mmol / (m2 s)
                VertDiffFlux(i,j,0,itrc) = 0.0 ! Neumann b.c. at bottom
                do k=1,N-1
                   VertDiffFlux(i,j,k,itrc) = ThisVDF(i,j,k) +
     &                  VertDiffFlux(i,j,k-1,itrc)
                end do
                VertDiffFlux(i,j,N,itrc) = 0.0 ! Neumann b.c. at top
             end do
          end if
# endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */

#ifdef CONST_TRACERS
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
            enddo
          enddo
#endif
#  if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
          if (itrc .le. NT_PFA) then
             do k=1,N
                do i=Istr,Iend
                   VertDiffFlux(i,j,k,itrc) = 0.0
                enddo
             enddo
          end if
#  endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */
# ifdef AMAZON
       if (EASTERN_EDGE) then
         if (itrc==itemp .OR. itrc==isalt) then
# endif
# ifdef TCLIMATOLOGY
          do k=1,mxnudg
            do i=istr,iend
              t(i,j,k,nnew,itrc)=( t(i,j,k,nnew,itrc)
     &          +dt*Tnudgcof(i,j,itrc)
#  ifdef TNUDGE_WEIGHTS
     &          *nudgweights(i,j)
#  endif 
     &          *(tclm(i,j,k,itrc)-t(i,j,k,nnew,itrc))
     &                                               )
#  ifdef MASKING
     &                                   *rmask(i,j)
#  endif
#  ifdef FULL_PHYS_FLUX_ANALYSIS
#   if !defined SELECTED_FLUX_ANALYSIS /* AH */
          if (itrc .le. NT_PFA) then
               NudgingFlux(i,j,k,itrc) = Tnudgcof(i,j,itrc) *
     &              ( tclm(i,j,k,itrc) - t(i,j,k,nnew,itrc) )
#    ifdef TNUDGE_WEIGHTS
     &          *nudgweights(i,j)
#    endif 
#    ifdef MASKING
     &              * rmask(i,j)
#    endif
            end if
#   endif /* SELECTED_FLUX_ANALYSIS */
#  endif /* FULL_PHYS_FLUX_ANALYSIS */
            enddo
          enddo
# endif /* TCLIMATOLOGY */
# ifdef AMAZON
         endif
       endif
# endif

        enddo      !<-- j
# ifdef AGE_DYE_TRACER
        is=itrc-itemp-ntrc_salt
        if(is> 0 .and. Age(is)) then
          call t3dbc_closed_tile (istr,iend,jstr,jend, itrc)
        else
          call t3dbc_tile (istr,iend,jstr,jend, itrc, wrk1)
        endif
# else
        call t3dbc_tile (istr,iend,jstr,jend, itrc, wrk1)
# endif
      enddo     !<-- itrc
!DL:
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      do itrc=itemp+ntrc_salt+1,itemp+ntrc_salt+N_agedye ! loop over age/dye tracers
         is=itrc-(itemp+ntrc_salt) ! is = number of age/dye tracer
         if (.not.Age(is).and..not.Restore_ad(is)) then
            do l=1,Nsrc_ad(is)     ! loop over sources of tracer is
               ! Get i, j, k indices of this source:
               i = Isrc_ad(is,l)
               j = Jsrc_ad(is,l)
               k = Ksrc_ad(is,l)
               ! Check whether this source is still active:
               source_active = .false.
               if (switch_is_days_ad(is) .and.
     &             time*sec2day.ge.Switch_on_src_ad(is,l) .and.
     &             time*sec2day.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (.not.switch_is_days_ad(is) .and.
     &             iic.ge.Switch_on_src_ad(is,l) .and.
     &             iic.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (source_active) then
                  ! Now add source term:
                  t(i,j,k,nnew,itrc) = t(i,j,k,nnew,itrc) + dt*Ssrc_ad(is,l)
               endif
            enddo
         endif
         if (.not.Age(is).and.Restore_ad(is)) then
            ! apply restoring bc
            do l=1,Nsrc_ad(is)  ! loop over sources of tracer is
               i = Isrc_ad(is,l)
               j = Jsrc_ad(is,l)
               k = Ksrc_ad(is,l)
               ! Check whether this source is still active:
               source_active = .false.
               if (switch_is_days_ad(is) .and.
     &             time*sec2day.ge.Switch_on_src_ad(is,l) .and.
     &             time*sec2day.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (.not.switch_is_days_ad(is) .and.
     &             iic.ge.Switch_on_src_ad(is,l) .and.
     &             iic.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (source_active) then
                  t(i,j,k,nnew,itrc) = Ssrc_ad(is,l)
               endif
            enddo
         endif ! .not.Age(is).and.Restore_ad(is)
         if (Age(is)) then
            do k=1,N-1          ! exclude surface k=N
               do j=jstrR,jendR
                  do i=istrR,iendR
                     t(i,j,k,nnew,itrc) = t(i,j,k,nnew,itrc) 
#  ifdef USE_REAL_YEAR
     &                    + dt/(day2sec*365)
#  else
     &                    + dt/(day2sec*360)
#  endif
                  enddo
               enddo
            enddo ! k=1,N-1
         endif ! Age(is)
      enddo ! itrc
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */

!
!DL: Set surface fluxes for age tracers such that the new surface tracer
! concentration is zero:
!
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      do is=1,N_agedye
         itrc = itemp+ntrc_salt+is
         if (Age(is)) then
            do i=istrR,iendR
               do j=jstrR,jendR
                 stflx(i,j,itrc) = -t(i,j,N,nnew,itrc)/dt
#  ifdef MASKING
     &                               * rmask(i,j)
#  endif
               enddo
            enddo
            do i=istrR,iendR
               do j=jstrR,jendR
                 t(i,j,N,nnew,itrc) = 0.0
               enddo
            enddo
         endif ! Age(is)
      enddo ! is=1,N_agedye
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */

# if defined BIOLOGY_BEC
      call ecosys_tile (istr,iend,jstr,jend)      ! BEC of about 2006
# elif defined BIOLOGY_BEC2
      call ecosys_bec2_tile (istr,iend,jstr,jend) ! BEC, 2014 
# elif defined BIOLOGY || defined BIOLOGY_NPZDOC
      call biology_tile (istr,iend,jstr,jend)     ! NPZDOC
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do itrc=1,NT-1,2
        call exchange_2_tile(istr,iend,jstr,jend,
     &                       t(START_2D_ARRAY,1,nnew,itrc  ), N,
     &                       t(START_2D_ARRAY,1,nnew,itrc+1), N)
      enddo
      if (mod(NT,2) == 1) then
        call exchange_tile (istr,iend,jstr,jend,
     &                          t(START_2D_ARRAY,1,nnew,NT), N)
      endif
# endif
      end

      subroutine check_step_t_switches(ierr)

! The following routine keeps track of the status of CPP settings in
! "compute_horiz_tracer_fluxes.h" and "compute_horiz_vert_fluxes.h".
! This code must be placed here rather than in a separate file so it
! is exposed to the relevant CPP-settings.   It does not affect any
! model results, other than signature in global attribute "CPPS" in
! the output netCDF files.

      implicit none
      integer ierr, is,ie, lenstr
# include "param.h"
# include "strings.h"
      ie=lenstr(cpps)
      is=ie+2 ; ie=is+15
      if (ie > max_opt_size) goto 99
      cpps(is:ie)='<step3d_t_ISO.F>'

# include "track_advec_switches.h"

      return
  99  mpi_master_only write(*,'(/1x,2A/12x,A/)')      '### ERROR: ',
     &  'Insufficient length of string "cpps" in file "strings.h".',
     &        'Increase parameter "max_opt_size" it and recompile.'
      ierr=ierr+1
      end
      
      
      
    

#else
      subroutine step3d_t_empty
      end
#endif  /* SOLVE3D */
