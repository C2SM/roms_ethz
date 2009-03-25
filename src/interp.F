#define GRID_LEVEL 2
#include "cppdefs.h"
#ifdef GRID_LEVEL
# if GRID_LEVEL > 1

      subroutine u2dbc_tile (Istr,Iend,Jstr,Jend, zeta_new,buff)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, jmin,jmax
      real zeta_new(PRIVATE_2D_SCRATCH_ARRAY),
     &         buff(PRIVATE_2D_SCRATCH_ARRAY)
#  include "param.h"
#  include "grid.h"
#  include "scalars.h"
#  include "ocean2d.h"
                                ! Preload broker array "buff" with
      if (SOUTHERN_EDGE) then   ! masked cross-sections of the fine-
        jmin=Jstr-1             ! grid elements, accross which normal
      else                      ! velocity component exports/imports
        jmin=Jstr-2             ! mass in/out of the domain. These
      endif                     ! These crossections are then used
      if (NORTHERN_EDGE) then   ! inside the interpolation routine
        jmax=Jend+1             ! to enforce flux-consistency between
      else                      ! the fine and coarse grids within
        jmax=Jend+2             ! each fine-grid triad. Note that the
      endif                     ! interpolation routines return mass
                                ! fluxes, which are then need to be 
      if (WESTERN_EDGE) then    ! divided by the crossections.
        do j=jmin,jmax
          buff(Istr-1,j)=0.5*( zeta_new(Istr-1,j)+h(Istr-1,j)
     &                        +zeta_new(Istr  ,j)+h(Istr  ,j))
     &                                           *dm_u(Istr,j)
          buff(Istr,j)=buff(Istr-1,j)
#  ifdef MASKING
     &                 *umask(Istr,j)
#  endif
        enddo
#  if GRID_LEVEL == 2
        call int_u2d_west_1 (Istr,Iend,Jstr,Jend, buff)
#  elif GRID_LEVEL == 3
        call int_u2d_west_2 (Istr,Iend,Jstr,Jend, buff)
#  elif GRID_LEVEL == 4
        call int_u2d_west_3 (Istr,Iend,Jstr,Jend, buff)
#  endif
        do j=Jstr,Jend
          ubar(Istr,j,knew)=buff(Istr,j)/buff(Istr-1,j)
        enddo
      endif

      if (EASTERN_EDGE) then
        do j=jmin,jmax
          buff(Iend+2,j)=0.5*( zeta_new(Iend+1,j)+h(Iend+1,j)
     &                        +zeta_new(Iend  ,j)+h(Iend  ,j))
     &                                         *dm_u(Iend+1,j)
          buff(Iend+1,j)=buff(Iend+2,j)
#  ifdef MASKING
     &                 *umask(Iend+1,j)
#  endif
        enddo
#  if GRID_LEVEL == 2
        call int_u2d_east_1 (Istr,Iend,Jstr,Jend, buff)
#  elif GRID_LEVEL == 3
        call int_u2d_east_2 (Istr,Iend,Jstr,Jend, buff)
#  elif GRID_LEVEL == 4
        call int_u2d_east_3 (Istr,Iend,Jstr,Jend, buff)
#  endif
        do j=Jstr,Jend
          ubar(Iend+1,j,knew)=buff(Iend+1,j)/buff(Iend+2,j)
        enddo
      endif





      if (SOUTHERN_EDGE) then
      endif
      if (NORTHERN_EDGE) then
      endif



 
      return
      end 
# endif
# if GRID_LEVEL < MAX_GRID_LEVEL

      subroutine int_u2d_west (Istr,Iend,Jstr,Jend, buff)
      implicit none
      integer Istr,Iend,Jstr,Jend, jmin,jmax, j,jf
      real buff(PRIVATE_2D_SCRATCH_ARRAY)
      real T1,T4,T5, crs,crsR,crsL, uctr,dltR,dltL, cff,cffR,cffL 
      parameter (T1=1./27., T4=4./27., T5=5./27.)
#  include "param.h"
#  include "scalars.h"
#  include "ocean2d.h"
#  include "coupling.h"

      jmin=jmin_child+(Jstr-1)/3
      jmax=jmin_child+(Jend-1)/3
 
      do j=jmin,jmax                  ! At entry "buff" is preloaded
        jf=2+3*(j-jmin_child)         ! with masked crossections from 
        crs=buff(Istr,jf  )           ! the fine grid.  Save them to
        crsR=buff(Istr,jf+1)          ! restore integral flux in each 
        crsL=buff(Istr,jf-1)          ! triade. 

        uctr=ubar(imin_child,j,knew)
        dltR=ubar(imin_child,j+1,knew)-uctr
        dltL=uctr-ubar(imin_child,j-1,knew)

        cffL=4.*dltL
        cffR=4.*dltR
        if (dltL*dltR .lt.0.) then
          dltL=0.
          dltR=0.
        elseif (abs(dltL) .gt. abs(cffR)) then
          dltL=cffR
        elseif (abs(dltR) .gt. abs(cffL)) then
          dltR=cffL
        endif

        buff(Istr,jf  )=crs *( uctr - T1*(dltR-dltL) )
        buff(Istr,jf+1)=crsR*( uctr +T5*dltR +T4*dltL )
        buff(Istr,jf-1)=crsL*( uctr -T4*dltR -T5*dltL )

        cff=crs+crsR+crsL
        if (cff.gt.0.) cff=( DU_avg2(imin_child,j) -buff(Istr,jf)
     &                     -buff(Istr,jf+1)-buff(Istr,jf-1) )/cff 

        buff(Istr,jf  )=buff(Istr,jf  ) +cff*crs
        buff(Istr,jf+1)=buff(Istr,jf+1) +cff*crsR
        buff(Istr,jf-1)=buff(Istr,jf-1) +cff*crsL
      enddo
      return
      end

      subroutine int_u2d_east (Istr,Iend,Jstr,Jend, buff)
      implicit none
      integer Istr,Iend,Jstr,Jend, jmin,jmax, j,jf
      real buff(PRIVATE_2D_SCRATCH_ARRAY)
      real T1,T4,T5, crs,crsR,crsL, uctr,dltR,dltL, cff,cffR,cffL
      parameter (T1=1./27., T4=4./27., T5=5./27.)
#  include "param.h"
#  include "scalars.h"
#  include "ocean2d.h"
#  include "coupling.h"

      jmin=jmin_child+(Jstr-1)/3
      jmax=jmin_child+(Jend-1)/3

      do j=jmin,jmax
        jf=2+3*(j-jmin_child)
        crs=buff(Iend+1,jf  )
        crsR=buff(Iend+1,jf+1)
        crsL=buff(Iend+1,jf-1)

        uctr=ubar(imax_child,j,knew)
        dltR=ubar(imax_child,j+1,knew)-uctr
        dltL=uctr-ubar(imax_child,j-1,knew)

        cffL=4.*dltL
        cffR=4.*dltR
        if (dltL*dltR .lt.0.) then
          dltL=0.
          dltR=0.
        elseif (abs(dltL) .gt. abs(cffR)) then
          dltL=cffR
        elseif (abs(dltR) .gt. abs(cffL)) then
          dltR=cffL
        endif

        buff(Iend+1,jf  )=crs *( uctr- T1*(dltR-dltL) )
        buff(Iend+1,jf+1)=crsR*( uctr+T5*dltR +T4*dltL )
        buff(Iend+1,jf-1)=crsL*( uctr-T4*dltR -T5*dltL )

        cff=crs+crsR+crsL
        if (cff.gt.0.) cff=( DU_avg2(imin_child,j) -buff(Iend+1,jf)
     &                   -buff(Iend+1,jf+1)-buff(Iend+1,jf-1) )/cff  

        buff(Iend+1,jf  )=buff(Iend+1,jf  ) +cff*crs
        buff(Iend+1,jf+1)=buff(Iend+1,jf+1) +cff*crsR
        buff(Iend+1,jf-1)=buff(Iend+1,jf-1) +cff*crsL
      enddo
      return
      end
# endif
#else
      subroutine int_2d_empty
      end
#endif
 
