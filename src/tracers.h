!
! Biological and other tracers
!
#if defined BIOLOGY_NPZDOC || defined  LEGACY_NPZD
      integer, parameter :: indxNO3=LAST_I+1
     &           , indxNH4 =indxNO3+1, indxChla=indxNO3+2
     &           , indxPhyt=indxNO3+3, indxZoo =indxNO3+4
     &           , indxSDet=indxNO3+5, indxLDet=indxNO3+6
# undef LAST_I
# define LAST_I indxLDet
# ifdef OXYGEN
     &           , indxO2 = LAST_I+1
#  undef LAST_I
#  define LAST_I indxLDet
#  ifdef CARBON
      integer, parameter :: indxDIC=LAST_I+1
     &           , indxTALK=indxDIC+1
     &           , indxSDetC=indxTALK+1
     &           , indxLDetC=indxSDetC+1
     &           , indxCaCO3=indxLDetC+1
#   undef LAST_I
#   define LAST_I CaCO3
#  endif /* CARBON */
# endif /* OXYGEN */
#endif /* BIOLOGY_NPZDOC || LEGACY_NPZD*/

#ifdef BIOLOGY_BEC2
      integer, parameter :: indxPo4=LAST_I+1
     &          , indxNo3=indxPO4+1, indxSiO3=indxPO4+2
     &          , indxNh4=indxPO4+3, indxFe=indxPO4+4
     &          , indxO2=indxPO4+5,  indxDic=indxPO4+6
     &          , indxAlk=indxPO4+7, indxDOC=indxPO4+8
     &          , indxDon=indxPO4+9, indxDOFe=indxPO4+10
     &          , indxDop=indxPO4+11, indxDOPr=indxPO4+12
     &          , indxDonr=indxPO4+13, indxZooC=indxPO4+14
     &          , indxSpchl=indxPO4+15, indxSpC=indxPO4+16
     &          , indxSpfe=indxPO4+17, indxSpCaCO3=indxPO4+18
     &          , indxDiatchl=indxPO4+19, indxDiatC=indxPO4+20
     &          , indxDiatfe=indxPO4+21, indxDiatSi=indxPO4+22
     &          , indxDiazchl=indxPO4+23, indxDiazC=indxPO4+24
     &          , indxDiazfe=indxPO4+25
# undef LAST_I
# define LAST_I indxDiazfe
# ifdef BEC_COCCO
     &          , indxCoccoc=indxPO4+26, indxCoccochl=indxPO4+27
     &          , indxCoccocal=indxPO4+28, indxCoccofe=indxPO4+29
     &          , indxCal=indxPO4+30
#  undef LAST_I
#  define LAST_I indxCal
# endif
#endif /* BIOLOGY_BEC2 */

