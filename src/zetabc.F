#include "cppdefs.h"
 
      subroutine zetabc_tile (Istr,Iend,Jstr,Jend, zeta_new)
!
! Set boundary conditions for free surface zeta_new
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
      real zeta_new(PRIVATE_2D_SCRATCH_ARRAY),
     &                     cff,cx,dft,dfx, eps
      parameter (eps=1.D-20)
#include "param.h"
#include "grid.h"
#include "ocean2d.h"
#include "scalars.h"
#include "boundary.h"
#include "climat.h"
!
#include "compute_auxiliary_bounds.h"
!
#ifndef EW_PERIODIC

      if (WESTERN_EDGE) then
# if defined OBC_WEST && defined OBC_Z2ORLANSKI
        do j=JstrV-1,Jend                             ! Western edge
          dft=zeta(Istr,j,kstp)-zeta_new(Istr,j)      ! Radiation BC
          dfx=zeta_new(Istr,j)-zeta_new(Istr+1,j)
 
          if (dfx*dft .lt. 0.) dft=0.     ! <--  SUPPRESS INFLOW
 
          cx=dft*dfx
          cff=max(dfx*dfx, eps)
 
          zeta_new(Istr-1,j)=( cff*zeta(Istr-1,j,kstp)
     &                  +cx*zeta_new(Istr,j) )/(cff+cx)
           
#  ifdef MASKING
     &                                 *rmask(Istr-1,j)
#  endif
        enddo
# elif defined OBC_WEST && defined OBC_M2FLATHER
        do j=JstrV-1,Jend                              ! Chapman BC
          cx=dtfast*pm(Istr,j)*sqrt(g*h(Istr,j))       ! ======= ==
          zeta_new(Istr-1,j)=( (1.-cx)*zeta(Istr-1,j,kstp)
     &                               +cx*zeta(Istr,j,kstp) )
#  ifdef MASKING
     &                                      *rmask(Istr-1,j)
#  endif
        enddo
# else
        do j=JstrV-1,Jend                              ! Western edge
          zeta_new(Istr-1,j)=zeta_new(Istr,j)          ! gradient BC
#  ifdef MASKING
     &                        *rmask(Istr-1,j)
#  endif
        enddo
# endif
      endif

c*******
c*******
#ifdef ISWAKE
#define OBC_Z2ORLANSKI
#endif
c*******
c*******

      if (EASTERN_EDGE) then
#  if defined OBC_EAST && defined OBC_Z2ORLANSKI

        do j=JstrV-1,Jend                             ! Eastern edge
          dft=zeta(Iend,j,kstp)-zeta_new(Iend,j)      ! Radiation BC
          dfx=zeta_new(Iend,j)-zeta_new(Iend-1,j)
 
          if (dfx*dft .lt. 0.) dft=0.     ! <--  SUPPRESS INFLOW
 
          cx=dft*dfx
          cff=max(dfx*dfx, eps)
 
          zeta_new(Iend+1,j)=( cff*zeta(Iend+1,j,kstp)
     &                  +cx*zeta_new(Iend,j) )/(cff+cx)
#  ifdef MASKING
     &                                 *rmask(Iend+1,j)
#  endif
        enddo
# elif defined OBC_EAST && defined OBC_M2FLATHER
        do j=JstrV-1,Jend
          cx=dtfast*pm(Iend,j)*sqrt(g*h(Iend,j))      ! Eastern edge
          zeta_new(Iend+1,j)=( (1.-cx)*zeta(Iend+1,j,kstp)
     &                               +cx*zeta(Iend,j,kstp) )

#  ifdef MASKING
     &                                      *rmask(Iend+1,j)
#  endif
        enddo
#  else
        do j=JstrV-1,Jend                             ! Eastern edge
          zeta_new(Iend+1,j)=zeta_new(Iend,j)         ! gradient BC
#  ifdef MASKING
     &                        *rmask(Iend+1,j)
#  endif
        enddo
# endif
      endif
#endif        /* !EW_PERIODIC */

 
#ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
# if defined OBC_SOUTH && defined OBC_M2FLATHER
        do i=IstrU-1,Iend
          cx=dtfast*pn(i,Jstr)*sqrt(g*h(i,Jstr))     ! Southern edge
          zeta_new(i,Jstr-1)=( (1.-cx)*zeta(i,Jstr-1,kstp)
     &                               +cx*zeta(i,Jstr,kstp) )
#  ifdef MASKING
     &                                      *rmask(i,Jstr-1)
#  endif
        enddo
#  else
        do i=IstrU-1,Iend                            ! Southern edge
          zeta_new(i,Jstr-1)=zeta_new(i,Jstr)        ! gradient BC
#  ifdef MASKING
     &                       *rmask(i,Jstr-1)
#  endif
        enddo
# endif  /* OBC_SOUTH */
      endif


      if (NORTHERN_EDGE) then
# if defined OBC_NORTH && defined OBC_M2FLATHER
        do i=IstrU-1,Iend
          cx=dtfast*pn(i,Jend)*sqrt(g*h(i,Jend))    ! Northern edge
          zeta_new(i,Jend+1)=( (1.-cx)*zeta(i,Jend+1,kstp)
     &                               +cx*zeta(i,Jend,kstp) )
#  ifdef MASKING
     &                                      *rmask(i,Jend+1)
#  endif
        enddo
# else
        do i=IstrU-1,Iend                           ! Northern edge
          zeta_new(i,Jend+1)=zeta_new(i,Jend)       ! gradient BC
#  ifdef MASKING
     &                       *rmask(i,Jend+1)
#  endif
        enddo
# endif   /* OBC_NORTH */
      endif
#endif       /* !NS_PERIODIC */
!
! Corners between adjacent open boundaries
!
#ifndef EW_PERIODIC
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        zeta_new(Istr-1,Jstr-1)=0.5*( zeta_new(Istr  ,Jstr-1)
     &                               +zeta_new(Istr-1,Jstr  ))
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        zeta_new(Iend+1,Jstr-1)=0.5*( zeta_new(Iend  ,Jstr-1)
     &                               +zeta_new(Iend+1,Jstr  ))
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        zeta_new(Istr-1,Jend+1)=0.5*( zeta_new(Istr  ,Jend+1)
     &                               +zeta_new(Istr-1,Jend  ))
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        zeta_new(Iend+1,Jend+1)=0.5*( zeta_new(Iend  ,Jend+1)
     &                               +zeta_new(Iend+1,Jend  ))
      endif
# endif
#endif
      return
      end
 
