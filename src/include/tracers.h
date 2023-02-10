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
#  define LAST_I indxO2
#  ifdef CARBON
      integer, parameter :: indxDIC=LAST_I+1
     &           , indxTALK=indxDIC+1
     &           , indxSDetC=indxTALK+1
     &           , indxLDetC=indxSDetC+1
     &           , indxCaCO3=indxLDetC+1
#   undef LAST_I
#   define LAST_I indxCaCO3
#  endif /* CARBON */
# endif /* OXYGEN */
# ifdef SEDIMENT_BIOLOGY
      integer, parameter :: indxSedOrgN=LAST_I+1
     &           , indxSedOrgC=indxSedOrgN+1
     &           , indxSedCaCO3=indxSedOrgC+1
#  undef LAST_I
#  define LAST_I indxSedCaCO3
# endif /* SEDIMENT_BIOLOGY */
#endif /* BIOLOGY_NPZDOC || LEGACY_NPZD*/

#ifdef BIOLOGY_BEC2
      integer, parameter :: indxPO4=LAST_I+1
     &          , indxNo3=indxPO4+1, indxSiO3=indxPO4+2
     &          , indxNh4=indxPO4+3, indxFe=indxPO4+4
     &          , indxO2=indxPO4+5,  indxDic=indxPO4+6
     &          , indxAlk=indxPO4+7, indxDOC=indxPO4+8
     &          , indxDon=indxPO4+9, indxDOFe=indxPO4+10
     &          , indxDop=indxPO4+11, indxDOPr=indxPO4+12
     &          , indxDonr=indxPO4+13, indxZooC=indxPO4+14
     &          , indxSpC=indxPO4+15, indxSpchl=indxPO4+16
     &          , indxSpfe=indxPO4+17, indxSpCaCO3=indxPO4+18
     &          , indxDiatC=indxPO4+19, indxDiatchl=indxPO4+20
     &          , indxDiatfe=indxPO4+21, indxDiatSi=indxPO4+22
     &          , indxDiazC=indxPO4+23, indxDiazchl=indxPO4+24
     &          , indxDiazfe=indxPO4+25
# undef LAST_I
# define LAST_I indxDiazfe
# ifdef BEC_COCCO
     &          , indxCoccoc=LAST_I+1, indxCoccochl=LAST_I+2
     &          , indxCoccocal=LAST_I+3, indxCoccofe=LAST_I+4
     &          , indxCal=LAST_I+5
#  undef LAST_I
#  define LAST_I indxCal
# endif
# ifdef BEC_DDA
     &          , indxDdac=LAST_I+1, indxDdachl=LAST_I+2
     &          , indxDdasi=LAST_I+3, indxDdafe=LAST_I+4
#  undef LAST_I
#  define LAST_I indxDdafe
#endif
# ifdef BEC_UCYN
     &          , indxUcync=LAST_I+1, indxUcynchl=LAST_I+2
     &          , indxUcynfe=LAST_I+3
#  undef LAST_I
#  define LAST_I indxUcynfe
#endif
# ifdef BEC_PHAEO
     &          , indxPhaeoc=LAST_I+1, indxPhaeochl=LAST_I+2
     &          , indxPhaeofe=LAST_I+3
#  undef LAST_I
#  define LAST_I indxPhaeofe
# endif
# ifdef USE_EXPLICIT_VSINK
     &          , indxdusthard=LAST_I+1, indxpochard=indxdusthard+1
     &          , indxpcaco3hard=indxdusthard+2, indxpsio2hard=indxdusthard+3
     &          , indxpironhard=indxdusthard+4, indxdustsoft=indxdusthard+5
     &          , indxpocsoft=indxdusthard+6, indxpcaco3soft=indxdusthard+7
     &          , indxpsio2soft=indxdusthard+8, indxpironsoft=indxdusthard+9
#  undef LAST_I
#  define LAST_I indxpironsoft
# endif
#endif /* BIOLOGY_BEC2 */
