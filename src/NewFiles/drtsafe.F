!  $Log: drtsafe.F,v $
!  Revision 1.1  2001/10/19 21:22:14  frenzel
!  Routines from the OCMIP project for the ROMS carbonized ecosystem model.
!  Some of these routines were adapted for the use in a parallel code.
!
!  Revision 1.1  1999/04/03 22:00:42  orr
!  Initial revision
! 
!  ---------------------------------------------------------------------
!  
#include "cppdefs.h"

#if defined BIOLOGY_NPZDOC && defined CARBON && defined OCMIP_CARBON
      REAL FUNCTION DRTSAFE(FUNCD,X1,X2,XACC,
     &     k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal,
     &     bt,st,ft,sit,pt,dic,ta)
      implicit none
!
!	File taken from Numerical Recipes. Modified  R.M.Key 4/94
!

      real k12,k12p,k123p
      real k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal
      real ta, dic, pt, sit, ft, st, bt, XACC, X1, X2
      real DF, FL, FH, XL, XH, SWAP, DXOLD, DX, F, TEMP
      
      integer J, MAXIT


      MAXIT=100
      CALL FUNCD(X1,FL,DF,
     &     k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal,
     &     bt,st,ft,sit,pt,dic,ta)
      CALL FUNCD(X2,FH,DF,
     &     k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal,
     &     bt,st,ft,sit,pt,dic,ta)
      IF(FL .LT. 0.0) THEN
        XL=X1
        XH=X2
      ELSE
        XH=X1
        XL=X2
        SWAP=FL
        FL=FH
        FH=SWAP
      END IF
      DRTSAFE=.5*(X1+X2)
      DXOLD=ABS(X2-X1)
      DX=DXOLD
      CALL FUNCD(DRTSAFE,F,DF,
     &     k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal,
     &     bt,st,ft,sit,pt,dic,ta)
      DO J=1,MAXIT
        IF(((DRTSAFE-XH)*DF-F)*((DRTSAFE-XL)*DF-F) .GE. 0.0 .OR.
     &	      ABS(2.0*F) .GT. ABS(DXOLD*DF)) THEN
          DXOLD=DX
          DX=0.5*(XH-XL)
          DRTSAFE=XL+DX
          IF(XL .EQ. DRTSAFE)RETURN
        ELSE
          DXOLD=DX
          DX=F/DF
          TEMP=DRTSAFE
          DRTSAFE=DRTSAFE-DX
          IF(TEMP .EQ. DRTSAFE)RETURN
	END IF
        IF(ABS(DX) .LT. XACC)RETURN
        CALL FUNCD(DRTSAFE,F,DF,
     &     k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal,
     &     bt,st,ft,sit,pt,dic,ta)
        IF(F .LT. 0.0) THEN
          XL=DRTSAFE
          FL=F
        ELSE
          XH=DRTSAFE
          FH=F
        END IF
      end do
      RETURN
      END
#else
      subroutine drtsafe_empty()
      return
      end
#endif /* BIOLOGY_NPZDOC && CARBON && OCMIP_CARBON */
