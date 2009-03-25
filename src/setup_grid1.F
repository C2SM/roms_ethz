#include "cppdefs.h"
                                            ! Setting up curvilinear
      subroutine setup_grid1 (tile)         ! grid: Compute various
      implicit none                         ! combinations of metric
      integer tile                          ! terms.
#include "param.h"
#include "compute_tile_bounds.h"
      call setup_grid1_tile (Istr,Iend,Jstr,Jend)
      return
      end
 
      subroutine setup_grid1_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
      real cff1, cff2
#include "param.h"
#include "scalars.h"
#include "grid.h"
!
#include "compute_extended_bounds.h"
!
!  Set f/mn,at horizontal RHO-points.
!
      do j=JstrR,JendR                             ! This array
        do i=IstrR,IendR                           ! is NOT to be
          fomn(i,j)=f(i,j)/(pm(i,j)*pn(i,j))       ! communicated
        enddo                                      ! in MPI code;
      enddo                                        ! others are...
 
#ifdef EW_PERIODIC
# define IR_RANGE Istr,Iend
# define IU_RANGE Istr,Iend
#else
# define IR_RANGE IstrR,IendR
# define IU_RANGE  Istr,IendR
# ifdef MPI
      if (WEST_INTER) IstrR=Istr          ! Ghost points along
      if (EAST_INTER) IendR=Iend          ! computational boundary...
# endif
#endif
#ifdef NS_PERIODIC
# define JR_RANGE Jstr,Jend
# define JV_RANGE Jstr,Jend
#else
# define JR_RANGE JstrR,JendR
# define JV_RANGE  Jstr,JendR
# ifdef MPI
      if (SOUTH_INTER) JstrR=Jstr         ! are filled during
      if (NORTH_INTER) JendR=Jend         ! subsequent communication
# endif
#endif
!
!  Compute n/m and m/n; all at horizontal RHO-points.
!
      do j=JR_RANGE
        do i=IR_RANGE
          dm_r(i,j)=1./pm(i,j)
          dn_r(i,j)=1./pn(i,j)
        enddo
      enddo
 
#if (defined CURVGRID && defined UV_ADV)
!
!  Compute d(1/n)/d(xi) and d(1/m)/d(eta) tems, both at RHO-points.
!
      do j=Jstr,Jend
        do i=Istr,Iend
          dndx(i,j)=0.5/pn(i+1,j)-0.5/pn(i-1,j)
          dmde(i,j)=0.5/pm(i,j+1)-0.5/pm(i,j-1)
        enddo
      enddo
 
#endif
!
!  Compute m/n at horizontal U-points.
!
      do j=JR_RANGE
        do i=IU_RANGE
           pmon_u(i,j)=(pm(i,j)+pm(i-1,j))
     &                 /(pn(i,j)+pn(i-1,j))
           dm_u(i,j)=2./(pm(i,j)+pm(i-1,j))
           dn_u(i,j)=2./(pn(i,j)+pn(i-1,j))
#ifdef MASKING
           umask(i,j)=rmask(i,j)*rmask(i-1,j)
#endif
        enddo
      enddo
!
!  Compute n/m at horizontal V-points.
!
      do j=JV_RANGE
        do i=IR_RANGE
          pnom_v(i,j)=(pn(i,j)+pn(i,j-1))
     &                /(pm(i,j)+pm(i,j-1))
          dm_v(i,j)=2./(pm(i,j)+pm(i,j-1))
          dn_v(i,j)=2./(pn(i,j)+pn(i,j-1))
#ifdef MASKING
          vmask(i,j)=rmask(i,j)*rmask(i,j-1)
#endif
        enddo
      enddo
!
! Compute n/m and m/n at horizontal PSI-points.
! Set mask according to slipperness parameter gamma.
!
      do j=JV_RANGE
        do i=IU_RANGE
          dm_p(i,j)=4./(pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
          dn_p(i,j)=4./(pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
 
#ifdef MASKING
c          pmask(i,j)=rmask(i,j)*rmask(i-1,j)*rmask(i,j-1)
c     &                                      *rmask(i-1,j-1)
C***      if (gamma2.lt.0.) pmask(i,j)=2.-pmask(i,j)
!
! Set no-slip boundary conditions on land-mask boundaries
! regardless of supplied value of gamma2.
!

          cff1=1.  !<-- computation of off-diagonal nonlinear terms
          cff2=2.

          if (rmask(i-1,j  ).gt.0.5 .and. rmask(i,j  ).gt.0.5 .and.
     &        rmask(i-1,j-1).gt.0.5 .and. rmask(i,j-1).gt.0.5) then
            pmask(i,j)=1.

          elseif(rmask(i-1,j  ).lt.0.5 .and.rmask(i,j  ).gt.0.5 .and.
     &           rmask(i-1,j-1).gt.0.5 .and.rmask(i,j-1).gt.0.5) then
            pmask(i,j)=cff1
          elseif(rmask(i-1,j  ).gt.0.5 .and.rmask(i,j  ).lt.0.5 .and.
     &           rmask(i-1,j-1).gt.0.5 .and.rmask(i,j-1).gt.0.5) then
            pmask(i,j)=cff1
          elseif(rmask(i-1,j  ).gt.0.5 .and.rmask(i,j  ).gt.0.5 .and.
     &           rmask(i-1,j-1).lt.0.5 .and.rmask(i,j-1).gt.0.5) then
            pmask(i,j)=cff1
          elseif(rmask(i-1,j  ).gt.0.5 .and.rmask(i,j  ).gt.0.5 .and.
     &           rmask(i-1,j-1).gt.0.5 .and.rmask(i,j-1).lt.0.5) then
            pmask(i,j)=cff1


          elseif(rmask(i-1,j  ).gt.0.5 .and.rmask(i,j  ).lt.0.5 .and.
     &           rmask(i-1,j-1).gt.0.5 .and.rmask(i,j-1).lt.0.5) then
            pmask(i,j)=cff2
          elseif(rmask(i-1,j  ).lt.0.5 .and.rmask(i,j  ).gt.0.5 .and.
     &           rmask(i-1,j-1).lt.0.5 .and.rmask(i,j-1).gt.0.5) then
            pmask(i,j)=cff2
          elseif(rmask(i-1,j  ).gt.0.5 .and.rmask(i,j  ).gt.0.5 .and.
     &           rmask(i-1,j-1).lt.0.5 .and.rmask(i,j-1).lt.0.5) then
            pmask(i,j)=cff2
          elseif(rmask(i-1,j  ).lt.0.5 .and.rmask(i,j  ).lt.0.5 .and.
     &           rmask(i-1,j-1).gt.0.5 .and.rmask(i,j-1).gt.0.5) then
            pmask(i,j)=cff2

          else
            pmask(i,j)=0.
          endif
#endif
        enddo
      enddo

#undef IR_RANGE
#undef IU_RANGE
#undef JR_RANGE
#undef JV_RANGE
 
#if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, dm_r)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, dn_r)
# if defined CURVGRID && defined UV_ADV
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,   dndx)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,   dmde)
# endif
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend, pmon_u)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,   dm_u)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,   dn_u)
 
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend, pnom_v)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,   dm_v)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,   dn_v)
 
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend, dm_p)
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend, dn_p)
# ifdef MASKING
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,  rmask)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,  umask)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,  vmask)
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend,  pmask)
# endif
#endif
      return
      end
 
 
