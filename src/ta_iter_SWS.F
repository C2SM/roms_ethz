!  $Log: ta_iter_1.F,v $
!
!  Latest revision by OCMIP: Use seawater pH scale consistently (June 2004)
!  Modified from ta_iter_1.f (RCS version 1.2, OCMIP-2)
!  - by A. Mouchet, 2004:
!  Fixed Problems w/ version of ta_iter_1.f used in OCMIP-2 (vers. 1.2)
!   1) fixed errors in signs, parenthesis and coefficient c in derivative
!   2) changed from Total to Seawater Scale 
!      * c defined for seawater H scale; 
!      * fn and df adapted to KF on free H scale
!      * comments have been adapted
!
!  Revision 1.1  2001/10/19 21:22:14  frenzel
!  Routines from the OCMIP project for the ROMS carbonized ecosystem model.
!  Some of these routines were adapted for the use in a parallel code.
!
! 
!  Revision 1.2  1999/09/01 17:55:41  orr
!  Fixed sign error in dfn/dx following remarks of C. Voelker (10/Aug/1999)
! 
!  Revision 1.1  1999/04/03 22:00:42  orr
!  Initial revision
! 
!  ---------------------------------------------------------------------
!  
#include "cppdefs.h"

#if defined BIOLOGY_NPZDOC && defined CARBON && defined OCMIP_CARBON
      subroutine ta_iter_SWS(x,fn,df,
     &     k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,hSWS,
     &     bt,st,ft,sit,pt,dic,ta)
      implicit none

      real k12,k12p,k123p
      real k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,hSWS
      real ta, dic, pt, sit, ft, st, bt
      real df, fn, x, x2, x3, c, a, a2, da, b, b2, db
      real cff1, cff2
C
C This routine expresses TA as a function of DIC, htotal and constants.
C It also calculates the derivative of this function with respect to 
C htotal. It is used in the iterative solution for htotal. In the call
C "x" is the input value for htotal, "fn" is the calculated value for TA
C and "df" is the value for dTA/dhtotal
C
      x2=x*x
      x3=x2*x
      k12 = k1*k2
      k12p = k1p*k2p
      k123p = k12p*k3p
      c = 1.0 + st/ks + ft/kf
      a = x3 + k1p*x2 + k12p*x + k123p
      a2=a*a
      da = 3.0*x2 + 2.0*k1p*x + k12p
      b = x2 + k1*x + k12
      b2=b*b
      db = 2.0*x + k1

      cff1 = ks*c/x
      cff2 = kf*c/x

C
C     fn = hco3+co3+borate+oh+hpo4+2*po4+silicate+hfree+hso4+hf+h3po4-ta
C
      fn = k1*x*dic/b +
     &           2.0*dic*k12/b +
     &           bt/(1.0 + x/kb) +
     &           kw/x +
     &           pt*k12p*x/a +
     &           2.0*pt*k123p/a +
     &           sit/(1.0 + x/ksi) -
     &           x/c -
     &           st/(1.0 + cff1) -
     &           ft/(1.0 + cff2) -
     &           pt*x3/a -
     &           ta
C
C     df = dfn/dx
C
      df = ((k1*dic*b) - k1*x*dic*db)/b2 -
     &           2.0*dic*k12*db/b2 -
     &           bt/kb/(1.0+x/kb)**2. -
     &           kw/x2 +
     &           (pt*k12p*(a - x*da))/a2 -
     &           2.0*pt*k123p*da/a2 -
     &           sit/ksi/(1.0+x/ksi)**2. -
     &           1.0/c -
     &           st*(1.0 + cff1)**(-2.)*(cff1/x) -
     &           ft*(1.0 + cff2)**(-2.)*(cff2/x) -
     &           pt*x2*(3.0*a-x*da)/a2
      return
      end
#else
      subroutine ta_iter_SWS_empty()
      return
      end
#endif /* BIOLOGY_NPZDOC && CARBON && OCMIP_CARBON */
