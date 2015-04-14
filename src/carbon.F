!  $Log: carbon.F,v $
!  Revision 1.1  2001/10/19 21:22:14  frenzel
!  Routines from the OCMIP project for the ROMS carbonized ecosystem model.
!  Some of these routines were adapted for the use in a parallel code.
!
! 
! 
!  ---------------------------------------------------------------------
#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC && !defined OCMIP_OXYGENSAT
      SUBROUTINE O2SAT(DISSO2,THETA,SALT,RHOST0,O2SATU,SATPC,AOU)
!
!     TITLE:
!     *****
!
!       O2SAT  -- CALCULATE O2 SATURATION, %SATURATION AND APPARENT
!                 O2 UTILIZATION (AOU)
!
!     PARAMETERS:
!     **********
!
!       DISSO2 -> DISSOLVED O2 (mmol m^-3)
!       THETA  -> POTENTIAL TEMPERATURE (CELSIUS DEGREES)
!       SALT   -> SALINITY (0/00)
!       RHOST0 -> INSITU DENSITY (S,T,0)
!       O2SATU <- O2 SATURATION (mmol m^-3)
!       SATPC  <- % SATURATION
!       AOU    <- APPARENT O2 UTILIZATION (mmol m^-3)
!
      implicit none
      REAL DISSO2,THETA,SALT,RHOST0,O2SATU,SATPC,AOU
!
!     VARIABLES:
!     *********
!
      REAL A,KELVIN,K100
!
!     CODE:
!     ****
!
      KELVIN = THETA +273.15
      K100   = KELVIN/100.
      A      = -177.7888+255.5907/K100+146.4813*LOG(K100)-22.204*K100
      A      = A + SALT*(-0.037362+K100*(0.016504-0.0020564*K100))
!hf: old formula (for O2 in ml/l): O2SATU = EXP(A)*RHOST0/1000.0 
      O2SATU = EXP(A)*RHOST0 / 22.3916
!
      SATPC  = DISSO2/O2SATU*100.0
      AOU    = O2SATU-DISSO2
!
      RETURN
      END
#endif /* BIOLOGY_NPZDOC && ! OCMIP_OXYGENSAT */

      REAL FUNCTION EOS80(P1,T,S)
!     modified by jrm  12/10/91
!**************************************
!     EQUATION OF STATE FOR SEAWATER PROPOSED BY JPOTS 1980
!     REFERENCES
!     MILLERD ET AL 1980, DEEP-SEA RES., 27A, 255-264
!     JPOTS NINTH REPORT 1978, TENTH REPORT 1980
!     UNITS:
!     PRESSURE        P        BARS
!     INPUT PRESSURE  P1       DECIBARS
!     TEMPERATURE     T        DEG CELSIUS (IPTS-68)
!     SALINITY        S        NSU (IPSS-78)
!     SPEC. VOL.      SPECV    M**3/KG
!     DENSITY         EOS80    KG/M**3 (RHO-1000.)
!     CHECK VALUE: EOS80=59.82 KG/M**3 FOR S=40 NSU,
!     T=40 DEG C, P=1000 BARS.
!
!     N. FOFONOFF  REVISED OCT 7 1980
!     MODIFIED TO TAKE DB INPUT PRESSURE, AND OUTPUT IN CM**3/GM 28NOV80
!     N. BRAY
      implicit none
      REAL P1,P,T,S,RHO,SR,R1,R2,R3,R4
      REAL A,B,C,D,E,A1,B1,AW,BW,K,K0,KW,ALPHA
! EQUIV
      EQUIVALENCE (E,D,B1,R4),(BW,B,R3),(C,A1,R2)
      EQUIVALENCE (AW,A,R1),(KW,K0,K)
! CONVERT PRESSURE TO BARS AND SQUARE ROOT SALINITY
      P=P1*.1
      SR=SQRT(ABS(S))
! COMPUTE DENSITY PURE WATER AT ATM PRESSURE
      R1=((((6.536332E-9*T-1.120083E-6)*T+1.001685E-4)*T
     &-9.095290E-3)*T+6.793952E-2)*T+999.842594
! SEAWATER DENSITY ATM PRESS.
      R2=(((5.3875E-9*T-8.2467E-7)*T+7.6438E-5)*T-4.0899E-3)*T
     &+8.24493E-1
      R3=(-1.6546E-6*T+1.0227E-4)*T-5.72466E-3
      R4=4.8314E-4
      RHO=(R4*S+R3*SR+R2)*S+R1
! SPECIFIC VOLUME AT ATMOSPHERIC PRESSURE
      ALPHA=1.E+3/RHO
      EOS80=RHO-1000.
      IF(P.EQ.0.0)RETURN
! COMPUTE COMPRESSION TERMS
      E=(9.1697E-10*T+2.0816E-8)*T-9.9348E-7
      BW=(5.2787E-8*T-6.12293E-6)*T+8.50935E-5
      B=BW+E*S
!
      D=1.91075E-4
      C=(-1.6078E-6*T-1.0981E-5)*T+2.2838E-3
      AW=((-5.77905E-7*T+1.16092E-4)*T+1.43713E-3)*T
     &+3.239908
      A=(D*SR+C)*S+AW
!
      B1=(-5.3009E-4*T+1.6483E-2)*T+7.944E-2
      A1=((-6.1670E-5*T+1.09987E-2)*T-0.603459)*T+54.6746
      KW=(((-5.155288E-5*T+1.360477E-2)*T-2.327105)*T
     &+148.4206)*T+19652.21
      K0=(B1*SR+A1)*S+KW
!
      K=(B*P+A)*P+K0
      ALPHA=ALPHA*(1.0-P/K)
!      EOS80=ALPHA
!      EOS80=1./EOS80
      EOS80=(1./(ALPHA*.001))-1000.
      RETURN
      END

#ifndef OCMIP_CARBON
      subroutine CO2SYS(TP,TSi,Sal,TempC,rhosw,Pdbar,TA,DIC,
     &                  pCO2,K0)
!***********************************************************************
!     Front-End Driver for Ernie Lewis' CO_2 System Program
!     Written by: John R. Moisan 12/8/2000
      implicit none
      real TP,TSi,TA,DIC,pH,fH,Sal,TempC,rhosw,Pdbar,Fugfac,VPFac
      real H,K0,K1,K2,HCO3,CO3,CO2,fCO2,pCO2,pCO2sw,CO2sol
      real K(10)  ! These are the equilibrium constants
      real T(5)   ! These are the amounts of the various species
      integer WhichKs, WhoseKSO4,pHScale
!***********************************************************************
!
!     select a pH Scale
!off      pHScale=1 ! ==> "pH" there are only K1, K2, and KW & they should be ok
      pHScale=2 ! ==> "pHsws" they are all on this now
!off      pHScale=3 ! ==> "pHtot"
!off      pHScale=4 ! ==> "pHfree"
!off      pHScale=5 ! ==> "pHNBS"
!
!hf:      WhichKs = 1    ! Roy's K1 and K2
      WhichKs = 4    ! Mehrbach refit
      WhoseKSO4 = 1  ! Dickson's KSO4
!
!     Need to input TP, TSi, Sal, TempC, Pdbar, TA, DIC
!      TP    ==>  ! Total Phosphate [mol/kg-seawater]
!      TSi   ==>  ! Total Silicate [mol/kg-seawater]
!      TempC ==>  ! temperature [deg C]
!      Sal   ==>  ! Salinity [PSU]
!      Pdbar ==>  ! pressure [db]
!      TA    ==>  ! Total Alkalinity [meq/m^3-seawater]
!      DIC   ==>  ! Dissolved Inorganic Carbon [mmol-C/m^3-seawater]
!
!     convert TA from [meq/m^3-seawater] to [eq/kg-seawater]
      TA=TA/(rhosw*1000.)
!
!     convert DIC from [mmol-C/m^3-seawater] to [mol-C/kg-seawater]
      DIC=DIC/(rhosw*1000.)
!
!     convert TP from [mmol-P/m^3-seawater] to [mol-P/kg-seawater]
      TP=TP/(rhosw*1000.)
      T(4)=TP
!
!     convert TSi from [mmol-Si/m^3-seawater] to [mol-Si/kg-seawater]
      TSi=TSi/(rhosw*1000.)
      T(5)=TSi
!
      CALL Constants(pHScale,WhichKs,WhoseKSO4,Sal,
     &               TempC,Pdbar,K0,K,T,fH,FugFac,VPFac)
      CALL CalculatepHfromTADIC(TA,DIC,K,T,pH)
!
      H = 10.**(-pH)
      K1=K(1)
      K2=K(2)
      fCO2 = DIC * H * H / (H * H + K1 * H + K1 * K2) / K0
      pCO2=fCO2/FugFac
      HCO3 = DIC * K1 * H / (K1 * H + H * H + K1 * K2)
      CO3 = DIC * K1 * K2 / (K1 * H + H * H + K1 * K2)
      CO2 = DIC - HCO3 - CO3
!
!     convert from [atm] to [uatm]
      fCO2=fCO2*1000.*1000.
      pCO2=pCO2*1000.*1000.
!
!     convert from [mol-C/kg-seawater/atm] to [mmol-C/m^3/uatm] 
      K0=K0*rhosw/1000.
!
!     convert TP from [mol-P/kg-seawater] to [mmol-P/m^3-seawater]
      TP=TP*rhosw*1000.
!
!     convert TSi from [mol-Si/kg-seawater] to [mmol-Si/m^3-seawater]
      TSi=TSi*rhosw*1000.
!
!     convert TA from [eq/kg-seawater] to [meq/m^3-seawater]
      TA=TA*rhosw*1000.
!
!     convert all below from [mol-C/kg-seawater] to [mmol-C/m^3-seawater]
      CO2=CO2*1000.*rhosw
      HCO3=HCO3*1000.*rhosw
      CO3=CO3*1000.*rhosw
      DIC=DIC*1000.*rhosw
!
!***********************************************************************
      return
      end


      subroutine CalculatepHfromTADIC(TA,DIC,K,T,pH)
!     Version 04.01, 10-13-96, written by Ernie Lewis.
!     converted from Basic to Fortran-77 by John R. Moisan 12/8/2000
!***********************************************************************
!     Inputs: TA,DIC,K(10),T(5)
!     Output: pH
!     This calculates pH from TA and DIC using K1 and K2 by Newton's method.
!     It tries to solve for the pH at which Residual = 0.
!     The starting guess is pH = 8.
!     Though it is coded for H on the total pH scale, for the pH values occuring
!     in seawater (pH > 6) it will be equally valid on any pH scale (H terms
!     negligible) as long as the K constants are on that scale.
!***********************************************************************
      implicit none
      real K(10),T(5)
      real TA,DIC,pH
      real K1,K2,KW,KB,KF,KS,KP1,KP2,KP3,KSi,TB,TF,TS,TSi,TP
      real pHGuess,pHTol,ln10,deltapH,H,Denom,Calk,Balk,OH,
     &     PhosTop,PhosBot,Palk,SiAlk,FREEtoTOT,Hfree,HSO4,
     &     HF,Residual,Slope
!***********************************************************************
!
      K1 = K(1)
      K2 = K(2)
      KW = K(3)
      KB = K(4)
      KF = K(5)
      KS = K(6)
      KP1 = K(7)
      KP2 = K(8)
      KP3 = K(9)
      KSi = K(10)
      TB = T(1)
      TF = T(2)
      TS = T(3)
      TP = T(4)
      TSi = T(5)
!
      pHGuess = 8.     ! This is the first guess for pH
      pHTol = .0001    ! This is .0001 pH units (pH tolerance???)
      ln10 = log(10.)
      pH = pHGuess
!
      deltapH=2.*pHTol  ! set deltapH initially to be larger than pHTol
      dowhile (abs(deltapH) .gt. pHTol)
        H = 10.**(-pH)
        Denom = (H * H + K1 * H + K1 * K2)
        CAlk = DIC * K1 * (H + 2. * K2) / Denom
        BAlk = TB * KB / (KB + H)
        OH = KW / H
        PhosTop = KP1 * KP2 * H + 2. * KP1 * KP2 * KP3 - H * H * H
        PhosBot = H * H * H + KP1 * H * H + KP1 * KP2 * 
     &            H + KP1 * KP2 * KP3
        PAlk = TP * PhosTop / PhosBot
        SiAlk = TSi * KSi / (KSi + H)
        FREEtoTOT = (1. + TS / KS)      ! pH scale conversion factor
        Hfree = H / FREEtoTOT           ! for H on the total scale
        HSO4 = TS / (1. + KS / Hfree)   ! since KS is on the free scale
        HF = TF / (1. + KF / Hfree)     ! since KF is on the free scale
        Residual = TA - CAlk - BAlk - OH - PAlk - SiAlk + 
     &             Hfree + HSO4 + HF
!
!       find Slope dTA/dpH: (this is not exact, but keeps all important terms):
        Slope = ln10 * (DIC * K1 * H * (H * H + K1 * K2 + 4. * H * K2) /
     &          Denom / Denom + BAlk * H / (KB + H) + OH + H)
        deltapH = Residual / Slope   ! this is Newton's method
                                       ! to keep the jump from being too big
!
        dowhile (abs(deltapH) .gt. 1.)
          deltapH = deltapH / 2.
        enddo
!
        pH = pH + deltapH
!
      enddo
!
!***********************************************************************
      return
      end


      subroutine Constants(pHScale,WhichKs,WhoseKSO4,Sal,TempC,Pdbar,K0,
     &                     K,T,fH,FugFac,VPFac)
!     Version 04.01, 10-13-97, written by Ernie Lewis.
!***********************************************************************
!     converted from Basic to Fortran-77 by John R. Moisan 12/8/2000
!     Inputs: pHScale,WhichKs,WhoseKSO4,Sal,TempC,Pdbar
!     Outputs: K0,K(10),T(5),fH,FugFac,VPFac
!     This finds the constants of the CO2 system in seawater or freshwater,
!     corrects them for pressure, and reports them on the chosen pH scale.
!     The process is as follows: the constants (except KS, KF which stay on the
!     free scale - these are only corrected for pressure) are
!     1) evaluated as they are given in the literature
!     2) converted to the SWS scale in mol/kg-SW or to the NBS scale
!     3) corrected for pressure
!     4) converted to the SWS pH scale in mol/kg-SW
!     5) converted to the chosen pH scale
!
!       PROGRAMMER'S NOTE: all logs are log base e
!       PROGRAMMER'S NOTE: all constants are converted to the pH scale
!          pHScale (the chosen one) in units of mol/kg-SW
!          except KS and KF are on the free scale
!          and KW is in units of (mol/kg-SW)**2
!***********************************************************************
      implicit none
      real T(5),K(10)
      real Sal,TempC,Pdbar,K0,fH,Fugfac,VPFac
      integer WhichKs,WhoseSO4,WhoseKSO4,pHScale,i
      real RGasConstant,TempK,RT,sqrtSal,logTempK,Pbar,VPWP,P1atm,B,
     &     delta,pHfactor,KSifac,KP3fac,KP2fac,KP1fac,KSfac,KFfac,KBfac,
     &     KWfac,K2fac,K1fac,lnKSifac,lnKP3fac,lnKP2fac,lnKP1fac,
     &     lnKSfac,lnKFfac,lnKWfac,VPSWWP,VPCorrWP,lnKBfac,lnK1fac,
     &     lnK2fac,Kappa,deltaV,logK1,logK2,pK1,pK2,K2,lnK2,K1,lnK1,
     &     KSi,lnKSiKP3,KP3,lnKP3,KP2,lnKP2,KP1,lnKP1,KW,lnKW,KB,lnKB,
     &     lnKSi,logKB,lnKBtop,FREEtoTOT,SWStoTOT,KF,lnKF,KS,pKS,lnKS,
     &     IonS,TTT,TS,TB,TF,lnK0
!***********************************************************************
!
      RGasConstant = 83.1451  ! bar-cm3/(mol-K) or 8.31451 N-m/(mol-K)
      TempK = TempC + 273.15
      RT = RGasConstant * TempK
      sqrtSal = sqrt(Sal)
      logTempK = log(TempK)
      Pbar = Pdbar/10.
!     deltaVs are in cm3/mole
!     Kappas are in cm3/mole/bar
!
!***********************************************************************
!     CalculateTB:
!
!     SELECT CASE WhichKs
      if (WhichKs .le. 5) then    ! CASE 1, 2, 3, 4, 5
!       Uppstrom, L., Deep-Sea Research 21:161-162, 1974
        TB = (.000232 / 10.811) * (Sal / 1.80655)  ! [mol/kg-SW]
!                 this is .000416 * Sal / 35. = .0000119 * Sal
      elseif (WhichKs .eq. 6 .or. WhichKs .eq. 7) then  !  CASE 6, 7
!       Culkin, F., in Chemical Oceanography, ed. Riley and Skirrow, 1965
!       GEOSECS references this, but this value is not explicitly
!       given here
        TB = .0004106 * Sal / 35. ! [mol/kg-SW]
!                this is .00001173 * Sal
!                this is about 1% lower than Uppstrom's value
      elseif (WhichKs .eq. 8) then ! CASE 8
        TB = 0.
      endif
!
!***********************************************************************
!     CalculateTF:
!     Riley, J. P., Deep-Sea Research 12:219-220, 1965:
      TF = (.000067 / 18.998) * (Sal / 1.80655)     !  in mol/kg-SW
!        this is .000068 * Sal / 35. = .00000195 * Sal
!***********************************************************************
!     CalculateTS:
!     Morris, A. W., and Riley, J. P., Deep-Sea Research 13:699-705, 1966:
      TS = (.14 / 96.062) * (Sal / 1.80655)     !  in mol/kg-SW
!        this is .02824 * Sal / 35. = .0008067 * Sal
!***********************************************************************
!     MakeTMatrix:
      T(1) = TB
      T(2) = TF
      T(3) = TS
!     T(4) = TP     !       These last two were set earlier.
!     T(5) = TSi    !       These last two were set earlier.
!***********************************************************************
!     CalculateK0:
!     Weiss, R. F., Marine Chemistry 2:203-215, 1974.
      TTT = TempK / 100.
      lnK0 = -60.2409 + 93.4517 / TTT + 23.3585 * log(TTT)
      lnK0 = lnK0 + Sal * 
     &       (0.023517 - .023656 * TTT + .0047036 * TTT * TTT)
      K0 = exp(lnK0)     !  this is in mol/kg-SW/atm
!***********************************************************************
!     CalculateIonS:
!     This is from the DOE handbook, Chapter 5, p. 13/22, eq. 7.2.4:
      IonS = 19.924 * Sal / (1000. - 1.005 * Sal)
!***********************************************************************
!     CalculateKS:
!     SELECT CASE WhoseKSO4
      if (WhoseKSO4 .eq. 1) then !  CASE 1     !  Dickson's value
!       Dickson, A. G., J. Chemical Thermodynamics, 22:113-127, 1990
!       The goodness of fit is .021.
!       It was given in mol/kg-H2O. I convert it to mol/kg-SW.
!       TYPO...!!! on p. 121: the constant e9 should be e8.
!
!       This is from eqs 22 and 23 on p. 123, and Table 4 on p 121:
        lnKS = -4276.1 / TempK + 141.328 - 23.093 * logTempK
        lnKS = lnKS + (-13856. / TempK + 324.57 - 47.986 * logTempK) * 
     &         sqrt(IonS)
        lnKS = lnKS + (35474. / TempK - 771.54 + 114.723 * logTempK) * 
     &         IonS
        lnKS = lnKS + (-2698. / TempK) * sqrt(IonS) * IonS
        lnKS = lnKS + (1776. / TempK) * IonS * IonS
        KS = exp(lnKS)     !  this is on the free pH scale in mol/kg-H2O
        KS = KS * (1. - .001005 * Sal)     !  convert to mol/kg-SW
!
      elseif (WhoseKSO4 .eq. 2) then ! CASE 2
!       Khoo, et al, Analytical Chemistry, 49(1):29-34, 1977
!       KS was found by titrations with a hydrogen electrode
!       of artificial seawater containing sulfate (but without F)
!       at 3 salinities from 20 to 45 and artificial seawater NOT
!       containing sulfate (nor F) at 16 salinities from 15 to 45,
!       both at temperatures from 5 to 40 deg C.
!       KS is on the Free pH scale (inherently so).
!       It was given in mol/kg-H2O. I convert it to mol/kg-SW.
!       He finds log(beta) which = my pKS;
!       his beta is an association constant.
!       The rms error is .0021 in pKS, or about .5% in KS.
!
!       This is equation 20 on p. 33:
        pKS = 647.59 / TempK - 6.3451 + .019085 * TempK - .5208 * 
     &        sqrt(IonS)
        KS = 10. ** (-pKS)     !  this is on the free pH scale in mol/kg-H2O
        KS = KS * (1. - .001005 * Sal)     !  convert to mol/kg-SW
      endif
!
!***********************************************************************
!     CalculateKF:
!     Dickson, A. G. and Riley, J. P., Marine Chemistry 7:89-99, 1979:
      lnKF = 1590.2 / TempK - 12.641 + 1.525 * sqrt(IonS)
      KF = exp(lnKF)     !  this is on the free pH scale in mol/kg-H2O
      KF = KF * (1. - .001005 * Sal)     !  convert to mol/kg-SW
!***********************************************************************
!     CalculatepHScaleConversionFactors:
!     These are NOT pressure-corrected.
      SWStoTOT = (1. + TS / KS) / (1. + TS / KS + TF / KF)
      FREEtoTOT = 1. + TS / KS
!***********************************************************************
!     CalculatefH:
!     Use GEOSECS's value for cases 1,2,3,4,5 (and 6) to convert pH scales.
!     SELECT CASE WhichKs
      if (WhichKs .le. 6) then ! CASE 1, 2, 3, 4, 5, 6
!       Takahashi et al, Chapter 3 in GEOSECS Pacific Expedition,
!       v. 3, 1982 (p. 80):
        fH = 1.2948 - .002036 * TempK
        fH = fH + (.0004607 - 1.475E-06 * TempK) * Sal * Sal
      elseif (WhichKs .eq. 7) then ! CASE 7
!       Peng et al, Tellus 39B:439-458, 1987:
!       They reference the GEOSECS report, but round the value
!       given there off so that it is about .008 (1%) lower. It
!       doesn't agree with the check value they give on p. 456.
        fH = 1.29 - .00204 * TempK
        fH = fH + (.00046 - 1.48E-06 * TempK) * Sal * Sal
      elseif (WhichKs .eq. 8) then ! CASE 8
        fH = 1.     !  this shouldn't occur in the program for this case
      endif
!***********************************************************************
!     CalculateKB:
!     SELECT CASE WhichKs
      if (WhichKs .le. 5) then ! CASE 1, 2, 3, 4, 5
!       Dickson, A. G., Deep-Sea Research 37:755-766, 1990:
        lnKBtop = -8966.9 - 2890.53 * sqrtSal - 77.942 * Sal
        lnKBtop = lnKBtop + 1.728 * sqrtSal * Sal - .0996 * Sal * Sal
        lnKB = lnKBtop / TempK
        lnKB = lnKB + 148.0248 + 137.1942 * sqrtSal + 1.62142 * Sal
        lnKB = lnKB + (-24.4344 - 25.085 * sqrtSal - .2474 * Sal) * 
     &         logTempK
        lnKB = lnKB + .053105 * sqrtSal * TempK
        KB = exp(lnKB)     !  this is on the total pH scale in mol/kg-SW
        KB = KB / SWStoTOT     !  convert to SWS pH scale
      elseif (WhichKs .eq. 6 .or. WhichKs .eq. 7) then ! CASE 6, 7
!       This is for GEOSECS and Peng et al.
!       Lyman, John, UCLA Thesis, 1957
!       fit by Li et al, JGR 74:5507-5525, 1969:
        logKB = -9.26 + .00886 * Sal + .01 * TempC
        KB = 10. ** (logKB)     !  this is on the NBS scale
        KB = KB / fH     !  convert to the SWS scale
      elseif (WhichKs .eq. 8) then ! CASE 8
        KB = 0.
      endif
!***********************************************************************
!     CalculateKW:
!     SELECT CASE WhichKs
      if (WhichKs .le. 5) then ! CASE 1, 2, 3, 4, 5
!       Millero, Geochemica et Cosmochemica Acta 59:661-677, 1995.
!       his check value of 1.6 umol/kg-SW should be 6.2
        lnKW = 148.9802 - 13847.26 / TempK - 23.6521 * logTempK
        lnKW = lnKW + (-5.977 + 118.67 / TempK + 1.0495 * logTempK) * 
     &         sqrtSal
        lnKW = lnKW - .01615 * Sal
        KW = exp(lnKW)     !  this is on the SWS pH scale in (mol/kg-SW)**2
      elseif (WhichKs .eq. 6) then ! CASE 6
        KW = 0.     !  GEOSECS doesn't include OH effects
      elseif (WhichKs .eq. 7) then ! CASE 7
!       Millero, Geochemica et Cosmochemica Acta 43:1651-1661, 1979
        lnKW = 148.9802 - 13847.26 / TempK - 23.6521 * logTempK
        lnKW = lnKW + 
     &       (-79.2447 + 3298.72 / TempK + 12.0408 * logTempK) * sqrtSal
        lnKW = lnKW - .019813 * Sal
        KW = exp(lnKW)     !  this is on the SWS pH scale
      elseif (WhichKs .eq. 8) then ! CASE 8
!       Millero, Geochemica et Cosmochemica Acta 43:1651-1661, 1979
!       refit data of Harned and Owen, The Physical Chemistry of
!       Electrolyte Solutions, 1958
        lnKW = 148.9802 - 13847.26 / TempK - 23.6521 * logTempK
        KW = exp(lnKW)
      endif
!***********************************************************************
!     CalculateKP1KP2KP3KSi:
!     SELECT CASE WhichKs
      if (WhichKs .le. 5) then ! CASE 1, 2, 3, 4, 5
!        Yao and Millero, Aquatic Geochemistry 1:53-88, 1995
!        KP1, KP2, KP3 are on the SWS pH scale in mol/kg-SW.
!        KSi was given on the SWS pH scale in molal units.
         lnKP1 = -4576.752 / TempK + 115.54 - 18.453 * logTempK
         lnKP1 = lnKP1 + (-106.736 / TempK + .69171) * sqrtSal
         lnKP1 = lnKP1 + (-.65643 / TempK - .01844) * Sal
         KP1 = exp(lnKP1)
!
         lnKP2 = -8814.715 / TempK + 172.1033 - 27.927 * logTempK
         lnKP2 = lnKP2 + (-160.34 / TempK + 1.3566) * sqrtSal
         lnKP2 = lnKP2 + (.37335 / TempK - .05778) * Sal
         KP2 = exp(lnKP2)
!
         lnKP3 = -3070.75 / TempK - 18.126
         lnKP3 = lnKP3 + (17.27039 / TempK + 2.81197) * sqrtSal
         lnKP3 = lnKP3 + (-44.99486 / TempK - .09984) * Sal
         KP3 = exp(lnKP3)
!
         lnKSi = -8904.2 / TempK + 117.4 - 19.334 * logTempK
         lnKSi = lnKSi + (-458.79 / TempK + 3.5913) * sqrt(IonS)
         lnKSi = lnKSi + (188.74 / TempK - 1.5998) * IonS
         lnKSi = lnKSi + (-12.1652 / TempK + .07871) * IonS * IonS
         KSi = exp(lnKSi)     !  this is on the SWS pH scale in mol/kg-H2O
         KSi = KSi * (1. - .001005 * Sal)     !  convert to mol/kg-SW
!
      elseif (WhichKs .eq. 7) then ! CASE 7
        KP1 = .02
!       Peng et al don't include the contribution from this term,
!       but it is so small it doesn't contribute. It needs to be
!       kept so that the routines work ok.
!       KP2, KP3 from Kester, D. R., and Pytkowicz, R. M.,
!       Limnology and Oceanography 12:243-252, 1967:
!       these are only for sals 33 to 36 and are on the NBS scale
        KP2 = exp(-9.039 - 1450. / TempK)     !  this is on the NBS scale
        KP2 = KP2 / fH     !  convert to SWS scale
        KP3 = exp(4.466 - 7276 / TempK)     !  this is on the NBS scale
        KP3 = KP3 / fH     !  convert to SWS scale
!       Sillen, Martell, and Bjerrum,  Stability constants of metal-ion complexes,
!       The Chemical Society (London), Special Publ. 17:751, 1964:
        KSi = 4E-10        ! this is on the NBS scale
        KSi = KSi / fH     ! convert to SWS scale
      elseif (WhichKs .eq. 6 .or. WhichKs .eq. 8) then ! CASE 6, 8
        KP1 = 0.
        KP2 = 0.
        KP3 = 0.
        KSi = 0.
!       Neither the GEOSECS choice nor the freshwater choice
!       include contributions from phosphate or silicate.
      endif
!***********************************************************************
!     CalculateK1K2:
      if (WhichKs .eq. 1) then ! CASE 1
!       ROY et al, Marine Chemistry, 44:249-267, 1993
!       (see also: Erratum, Marine Chemistry 45:337, 1994
!       and Erratum, Marine Chemistry 52:183, 1996)
!       !!! Typo: in the abstract on p. 249: in the eq. for lnK1* the
!       last term should have S raised to the power 1.5.
!       They claim standard deviations (p. 254) of the fits as
!       .0048 for lnK1 (.5% in K1) and .007 in lnK2 (.7% in K2).
!       They also claim (p. 258) 2s precisions of .004 in pK1 and
!       .006 in pK2. These are consistent, but Andrew Dickson
!       (personal communication) obtained an rms deviation of about
!       .004 in pK1 and .003 in pK2. This would be a 2s precision
!       of about 2% in K1 and 1.5% in K2.
!
!       This is eq. 29 on p. 254 and what they use in their abstract:
        lnK1 = 2.83655 - 2307.1266 / TempK - 1.5529413 * logTempK
        lnK1 = lnK1 + (-.20760841 - 4.0484 / TempK) * sqrtSal
        lnK1 = lnK1 + .08468345 * Sal - .00654208 * sqrtSal * Sal
        K1 = exp(lnK1)     !  this is on the total pH scale in mol/kg-H2O
        K1 = K1 * (1. - .001005 * Sal)     !  convert to mol/kg-SW
        K1 = K1 / SWStoTOT     !  convert to SWS pH scale
!
!       This is eq. 30 on p. 254 and what they use in their abstract:
        lnK2 = -9.226508 - 3351.6106 / TempK - .2005743 * logTempK
        lnK2 = lnK2 + (-.106901773 - 23.9722 / TempK) * sqrtSal
        lnK2 = lnK2 + .1130822 * Sal - .00846934 * sqrtSal * Sal
        K2 = exp(lnK2)     !  this is on the total pH scale in mol/kg-H2O
        K2 = K2 * (1. - .001005 * Sal)     !  convert to mol/kg-SW
        K2 = K2 / SWStoTOT     !  convert to SWS pH scale
      elseif (WhichKs .eq. 2) then ! CASE 2
!       GOYET AND POISSON, Deep-Sea Research, 36(11):1635-1654, 1989
!       The 2s precision in pK1 is .011, or 2.5% in K1.
!       The 2s precision in pK2 is .02, or 4.5% in K2.
!
!       This is in Table 5 on p. 1652 and what they use in the abstract:
        pK1 = 812.27 / TempK + 3.356 - .00171 * Sal * logTempK
        pK1 = pK1 + .000091 * Sal * Sal
        K1 = 10. ** (-pK1)     !  this is on the SWS pH scale in mol/kg-SW
!
!       This is in Table 5 on p. 1652 and what they use in the abstract:
        pK2 = 1450.87 / TempK + 4.604 - .00385 * Sal * logTempK
        pK2 = pK2 + .000182 * Sal * Sal
        K2 = 10. ** (-pK2)     !  this is on the SWS pH scale in mol/kg-SW
      elseif (WhichKs .eq. 3) then ! CASE 3
!       HANSSON refit BY DICKSON AND MILLERO
!       Dickson and Millero, Deep-Sea Research, 34(10):1733-1743, 1987
!       (see also Corrigenda, Deep-Sea Research, 36:983, 1989)
!       refit data of Hansson, Deep-Sea Research, 20:461-478, 1973
!       and Hansson, Acta Chemica Scandanavia, 27:931-944, 1973.
!       on the SWS pH scale in mol/kg-SW.
!       Hansson gave his results on the Total scale (he called it
!               the seawater scale) and in mol/kg-SW.
!       !!! Typo in DM on p. 1739 in Table 4: the equation for pK2*
!               for Hansson should have a .000132 *S**2
!               instead of a .000116 *S**2.
!       The 2s precision in pK1 is .013, or 3% in K1.
!       The 2s precision in pK2 is .017, or 4.1% in K2.
!
!       This is from Table 4 on p. 1739.
        pK1 = 851.4 / TempK + 3.237 - .0106 * Sal + .000105 * Sal * Sal
        K1 = 10. ** (-pK1)   ! this is on the SWS pH scale in mol/kg-SW
!
!       This is from Table 4 on p. 1739.
        pK2 = -3885.4 / TempK + 125.844 - 18.141 * logTempK
        pK2 = pK2 - .0192 * Sal + .000132 * Sal * Sal
        K2 = 10. ** (-pK2)    ! this is on the SWS pH scale in mol/kg-SW
        elseif (WhichKs .eq. 4) then  ! CASE 4
!         MEHRBACH refit BY DICKSON AND MILLERO
!         Dickson and Millero, Deep-Sea Research, 34(10):1733-1743, 1987
!         (see also Corrigenda, Deep-Sea Research, 36:983, 1989)
!         refit data of Mehrbach et al, Limn Oc, 18(6):897-907, 1973
!         on the SWS pH scale in mol/kg-SW.
!         Mehrbach et al gave results on the NBS scale.
!         The 2s precision in pK1 is .011, or 2.6% in K1.
!         The 2s precision in pK2 is .020, or 4.6% in K2.
!
!         This is in Table 4 on p. 1739.
          pK1 = 3670.7 / TempK - 62.008 + 9.7944 * logTempK
          pK1 = pK1 - .0118 * Sal + .000116 * Sal * Sal
          K1 = 10. ** (-pK1)     !  this is on the SWS pH scale in mol/kg-SW
!
!         This is in Table 4 on p. 1739.
          pK2 = 1394.7 / TempK + 4.777 - .0184 * Sal + .000118 * 
     &          Sal * Sal
          K2 = 10. ** (-pK2)     !  this is on the SWS pH scale in mol/kg-SW
        elseif (WhichKs .eq. 5) then ! CASE 5
!         HANSSON and MEHRBACH refit BY DICKSON AND MILLERO
!         Dickson and Millero, Deep-Sea Research,34(10):1733-1743, 1987
!         (see also Corrigenda, Deep-Sea Research, 36:983, 1989)
!         refit data of Hansson, Deep-Sea Research, 20:461-478, 1973,
!         Hansson, Acta Chemica Scandanavia, 27:931-944, 1973,
!         and Mehrbach et al, Limnol. Oceanogr.,18(6):897-907, 1973
!         on the SWS pH scale in mol/kg-SW.
!         !!! Typo in DM on p. 1740 in Table 5: the second equation
!                 should be pK2* =, not pK1* =.
!         The 2s precision in pK1 is .017, or 4% in K1.
!         The 2s precision in pK2 is .026, or 6% in K2.
!
!         This is in Table 5 on p. 1740.
          pK1 = 845. / TempK + 3.248 - .0098 * Sal + .000087 * 
     &          Sal * Sal
          K1 = 10. ** (-pK1)     !  this is on the SWS pH scale in mol/kg-SW
!
!         This is in Table 5 on p. 1740.
          pK2 = 1377.3 / TempK + 4.824 - .0185 * Sal + .000122 * 
     &          Sal * Sal
          K2 = 10.**(-pK2)     !  this is on the SWS pH scale in mol/kg-SW
!
        elseif (WhichKs .eq. 6 .or. WhichKs .eq. 7) then
!         GEOSECS and Peng et al use K1, K2 from Mehrbach et al,
!         Limnology and Oceanography, 18(6):897-907, 1973.
!         The 2s precision in pK1 is .005, or 1.2% in K1.
!         The 2s precision in pK2 is .008, or 2% in K2.
!
          logK1 = 13.7201 - .031334 * TempK - 3235.76 / TempK
          logK1 = logK1 - .000013 * Sal * TempK + .1032 * sqrtSal
          K1 = 10.**(logK1)      ! this is on the NBS scale
          K1 = K1 / fH           ! convert to SWS scale
!
          logK2 = -5371.9645 - 1.671221 * TempK + 128375.28 / TempK
          logK2 = logK2 + 2194.3055 * logTempK / 
     &            log(10.) - .22913 * Sal
          logK2 = logK2 - 18.3802 * log(Sal) / log(10.) + 
     &            0.00080944 * Sal * TempK
          logK2 = logK2 + 5617.11 * log(Sal) / log(10.) / 
     &            TempK - 2.136 * Sal / TempK
          K2 = 10.**(logK2)      ! this is on the NBS scale
          K2 = K2 / fH           ! convert to SWS scale
!
        elseif (WhichKs .eq. 8) then ! CASE 8
!         Millero, F. J., Geochemica et Cosmochemica Acta 43:1651-1661, 1979:
!         K1 from refit data from Harned and Davis,
!                 J American Chemical Society, 65:2030-2037, 1943.
!         K2 from refit data from Harned and Scholes,
!                 J American Chemical Society, 43:1706-1709, 1941.
!         These are the thermodynamic constants:
          lnK1 = 290.9097 - 14554.21 / TempK - 45.0575 * logTempK
          K1 = exp(lnK1)
          lnK2 = 207.6548 - 11843.79 / TempK - 33.6485 * logTempK
          K2 = exp(lnK2)
        endif
!***********************************************************************
!       CorrectKsForPressureNow:
!       Currently: For WhichKs = 1 to 7, all Ks (except KF and KS, which are on
!       the free scale) are on the SWS scale.
!       For WhichKs = 6, KW set to 0, KP1, KP2, KP3, KSi don't matter.
!       For WhichKs = 8, K1, K2, and KW are on the "pH" pH scale
!       (the pH scales are the same in this case); the other Ks don't matter.
!
!       No salinity dependence is given for the pressure coefficients here.
!       It is assumed that the salinity is at or very near Sal = 35.
!       These are valid for the SWS pH scale, but the difference between this and
!       the total only yields a difference of .004 pH units at 1000 bars, much
!       less than the uncertainties in the values.
!***********************************************************************
!       The sources used are:
!       Millero, 1995:
!       Millero, F. J., Thermodynamics of the carbon dioxide system in the
!       oceans, Geochemica et Cosmochemica Acta 59:661-677, 1995.
!       See table 9 and eqs. 90-92, p. 675.
!       TYPO!!!: a factor of 10**3 was left out of the definition of Kappa
!       TYPO!!!: the value of R given is incorrect with the wrong units
!       TYPO!!!: the values of the a's for H2S and H2O are from the 1983
!                values for fresh water
!       TYPO!!!: the value of a1 for B(OH)3 should be +.1622
!       !!! Table 9 on p. 675 has no values for Si.
!       There are a variety of other typos in Table 9 on p. 675.
!       There are other typos in the paper, and most of the check values
!       given don't check.
!       Millero, 1992:
!       Millero, Frank J., and Sohn, Mary L., Chemical Oceanography,
!       CRC Press, 1992. See chapter 6.
!       TYPO!!!: this chapter has numerous typos (eqs. 36, 52, 56, 65, 72,
!               79, and 96 have typos).
!       Millero, 1983:
!       Millero, Frank J., Influence of pressure on chemical processes in
!       the sea. Chapter 43 in Chemical Oceanography, eds. Riley, J. P. and
!       Chester, R., Academic Press, 1983.
!       TYPO!!!: p. 51, eq. 94: the value -26.69 should be -25.59
!       TYPO!!!: p. 51, eq. 95: the term .1700t should be .0800t
!       these two are necessary to match the values given in Table 43.24
!       Millero, 1979:
!       Millero, F. J., The thermodynamics of the carbon dioxide system
!       in seawater, Geochemica et Cosmochemica Acta 43:1651-1661, 1979.
!       See table 5 and eqs. 7, 7a, 7b on pp. 1656-1657.
!       Takahashi et al, in GEOSECS Pacific Expedition, v. 3, 1982.
!       TYPO!!!: the pressure dependence of K2 should have a 16.4, not 26.4
!       This matches the GEOSECS results and is in Edmond and Gieskes.
!       Culberson, C. H. and Pytkowicz, R. M., Effect of pressure on 
!       carbonic acid, boric acid, and the pH of seawater, 
!       Limnology and Oceanography 13:403-417, 1968.
!       Edmond, John M. and Gieskes, J. M. T. M., The calculation of the degree of
!       seawater with respect to calcium carbonate under in situ conditions,
!       Geochemica et Cosmochemica Acta, 34:1261-1291, 1970.
!***********************************************************************
! These references often disagree and give different fits for the same thing.
! They are not always just an update either; that is, Millero, 1995 may agree
!       with Millero, 1979, but differ from Millero, 1983.
! For WhichKs = 7 (Peng choice) I used the same factors for KW, KP1, KP2,
!       KP3, and KSi as for the other cases. Peng et al didn't consider the
!       case of P different from 0. GEOSECS did consider pressure, but didn't
!       include Phos, Si, or OH, so including the factors here won't matter.
! For WhichKs = 8 (freshwater) the values are from Millero, 1983 (for K1, K2,
!       and KW). The other aren't used (TB = TS = TF = TP = TSi = 0.), so
!       including the factors won't matter.
!***********************************************************************
!     CorrectK1K2KBForPressure:
      if (WhichKs .le. 5) then  ! CASE 1, 2, 3, 4, 5
!
!       PressureEffectsOnK1:
!         These are from Millero, 1995.
!         They are the same as Millero, 1979 and Millero, 1992.
!         They are from data of Culberson and Pytkowicz, 1968.
        deltaV = -25.5 + .1271 * TempC
!         deltaV = deltaV - .151 * (Sal - 34.8)   ! Millero, 1979
        Kappa = (-3.08 + .0877 * TempC) / 1000.
!         Kappa = Kappa  - .578 * (Sal - 34.8)/1000.  ! Millero, 1979
        lnK1fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!       The fits given in Millero, 1983 are somewhat different.
!
!       PressureEffectsOnK2:
!         These are from Millero, 1995.
!         They are the same as Millero, 1979 and Millero, 1992.
!         They are from data of Culberson and Pytkowicz, 1968.
        deltaV = -15.82 - .0219 * TempC
!      'deltaV = deltaV + .321 * (Sal - 34.8)     !  Millero, 1979
        Kappa = (1.13 - .1475 * TempC) / 1000.
!      'Kappa = Kappa - .314 * (Sal - 34.8) / 1000.     !  Millero, 1979
        lnK2fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!       The fit given in Millero, 1983 is different.
!       Not by a lot for deltaV, but by much for Kappa.!!. '
!
!       PressureEffectsOnKB:
!       This is from Millero, 1979.
!       It is from data of Culberson and Pytkowicz, 1968.
        deltaV = -29.48 + .1622 * TempC - .002608 * TempC * TempC
!       Millero, 1983 has:
!      'deltaV = -28.56 + .1211 * TempC - .000321 * TempC * TempC
!       Millero, 1992 has:
!      'deltaV = -29.48 + .1622 * TempC + .295 * (Sal - 34.8)
!       Millero, 1995 has:
!      'deltaV = -29.48 - .1622 * TempC - .002608 * TempC * TempC
!      'deltaV = deltaV + .295 * (Sal - 34.8)     !  Millero, 1979
        Kappa = -2.84 / 1000.     !  Millero, 1979
!       Millero, 1992 and Millero, 1995 also have this.
!      'Kappa = Kappa + .354 * (Sal - 34.8) / 1000.     !  Millero,1979
!       Millero, 1983 has:
!      'Kappa = (-3. + .0427 * TempC) / 1000.
        lnKBfac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
      elseif (WhichKs .eq. 6 .or. WhichKs .eq. 7) then ! CASE 6, 7
!
!       GEOSECS Pressure Effects On K1, K2, KB (on the NBS scale)
!       Takahashi et al, GEOSECS Pacific Expedition v. 3, 1982 quotes
!       Culberson and Pytkowicz, L and O 13:403-417, 1968:
!       but the fits are the same as those in
!       Edmond and Gieskes, GCA, 34:1261-1291, 1970
!       who in turn quote Li, personal communication
!
        lnK1fac = (24.2 - .085 * TempC) * Pbar / RT
        lnK2fac = (16.4 - .04 * TempC) * Pbar / RT
!       Takahashi et al had 26.4, but 16.4 is from Edmond and Gieskes
!       and matches the GEOSECS results
        lnKBfac = (27.5 - .095 * TempC) * Pbar / RT
!
      elseif (WhichKS .eq. 8) then ! CASE 8
!
!       PressureEffectsOnK1inFreshWater:
!       This is from Millero, 1983.
        deltaV = -30.54 + .1849 * TempC - .0023366 * TempC * TempC
        Kappa = (-6.22 + .1368 * TempC - .001233 * TempC * TempC) / 
     &           1000.
        lnK1fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!       PressureEffectsOnK2inFreshWater:
!       This is from Millero, 1983.
        deltaV = -29.81 + .115 * TempC - .001816 * TempC * TempC
        Kappa = (-5.74 + .093 * TempC - .001896 * TempC * TempC) / 
     &           1000.
        lnK2fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
        lnKBfac = 0.   ! this doesn't matter since TB = 0 for this case
!
      endif
!
!***********************************************************************
!     CorrectKWForPressure:
!     SELECT CASE WhichKs
      if (WhichKs .le. 7) then ! CASE 1, 2, 3, 4, 5, 6, 7
!       GEOSECS doesn't include OH term, so this won't matter.
!       Peng et al didn't include pressure, but here I assume that 
!       the KW correction is the same as for the other seawater cases.
!       PressureEffectsOnKW:
!       This is from Millero, 1983 and his programs CO2ROY(T).BAS.
        deltaV = -20.02 + .1119 * TempC - .001409 * TempC * TempC
!       Millero, 1992 and Millero, 1995 have:
!      'deltaV = -25.6 + .2324*TempC - .0036246*TempC*TempC
!       This is the freshwater value listed in Millero, 1983.
!       The difference is about 4 to 5 over the range 0 < TempC < 20,
!       which corresponds to a change in KW(P) of 3% at 200 bar,
!       8% at 500 bar, and 18% at 1000 bar.
!       This is probably correct since in Millero, 1983 values of
!       -deltaVs are less in seawater than pure water in all cases.
        Kappa = (-5.13 + .0794 * TempC) / 1000.     !  Millero, 1983
!       Millero, 1995 has this too, but Millero, 1992 is different.
        lnKWfac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!       Millero, 1979 does not list values for these.
!
      elseif (WhichKS .eq. 8) then ! CASE 8
!       PressureEffectsOnKWinFreshWater:
!       This is from Millero, 1983.
        deltaV = -25.6 + .2324 * TempC - .0036246 * TempC * TempC
        Kappa = (-7.33 + .1368 * TempC - .001233 * TempC * TempC) / 
     &           1000.
        lnKWfac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!       !!! NOTE the temperature dependence of KappaK1 and KappaKW
!           for fresh water in Millero, 1983 are the same.
!
      endif
!***********************************************************************
!     PressureEffectsOnKF:
!     This is from Millero, 1995, which is the same as Millero, 1983.
!     It is assumed that KF is on the free pH scale.
      deltaV = -9.78 - .009 * TempC - .000942 * TempC * TempC
      Kappa = (-3.91 + .054 * TempC) / 1000.
      lnKFfac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!     PressureEffectsOnKS:
!     This is from Millero, 1995, which is the same as Millero, 1983.
!     It is assumed that KS is on the free pH scale.
      deltaV = -18.03 + .0466 * TempC + .000316 * TempC * TempC
      Kappa = (-4.53 + .09 * TempC) / 1000.
      lnKSfac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!***********************************************************************
!     CorrectKP1KP2KP3KSiForPressure:
!     These corrections don't matter for the GEOSECS choice (WhichKs = 6) and
!     the freshwater choice (WhichKs = 8). For the Peng choice I assume
!     that they are the same as for the other choices (WhichKs = 1 to 5).
!     The corrections for KP1, KP2, and KP3 are from Millero, 1995, 
!     which are the same as Millero, 1983.
!
!     PressureEffectsOnKP1:
      deltaV = -14.51 + .1211 * TempC - .000321 * TempC * TempC
      Kappa = (-2.67 + .0427 * TempC) / 1000.
      lnKP1fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!     PressureEffectsOnKP2:
      deltaV = -23.12 + .1758 * TempC - .002647 * TempC * TempC
      Kappa = (-5.15 + .09 * TempC) / 1000.
      lnKP2fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!     PressureEffectsOnKP3:
      deltaV = -26.57 + .202 * TempC - .003042 * TempC * TempC
      Kappa = (-4.08 + .0714 * TempC) / 1000.
      lnKP3fac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!     PressureEffectsOnKSi:
! !!! The only mention of this is Millero, 1995 where it is stated that the
!     values have been estimated from the values of boric acid. HOWEVER,
!     there is no listing of the values in the table.
!     I used the values for boric acid from above.
      deltaV = -29.48 + .1622 * TempC - .002608 * TempC * TempC
      Kappa = -2.84 / 1000.
      lnKSifac = (-deltaV + .5 * Kappa * Pbar) * Pbar / RT
!
!***********************************************************************
!     CorrectKsForPressureHere:
      K1fac = exp(lnK1fac)
      K1 = K1 * K1fac
      K2fac = exp(lnK2fac)
      K2 = K2 * K2fac
      KWfac = exp(lnKWfac)
      KW = KW * KWfac
      KBfac = exp(lnKBfac)
      KB = KB * KBfac
      KFfac = exp(lnKFfac)
      KF = KF * KFfac
      KSfac = exp(lnKSfac)
      KS = KS * KSfac
      KP1fac = exp(lnKP1fac)
      KP1 = KP1 * KP1fac
      KP2fac = exp(lnKP2fac)
      KP2 = KP2 * KP2fac
      KP3fac = exp(lnKP3fac)
      KP3 = KP3 * KP3fac
      KSifac = exp(lnKSifac)
      KSi = KSi * KSifac
!***********************************************************************
!     MakeKMatrix:
      K(1) = K1
      K(2) = K2
      K(3) = KW
      K(4) = KB
      K(5) = KF
      K(6) = KS
      K(7) = KP1
      K(8) = KP2
      K(9) = KP3
      K(10) = KSi
!***********************************************************************
!     CorrectpHScaleConversionsForPressure:
!     fH has been assumed to be independent of pressure.
      SWStoTOT = (1. + TS / KS) / (1. + TS / KS + TF / KF)
      FREEtoTOT = 1. + TS / KS
!     The values KS and KF are already pressure-corrected, so the pH scale
!     conversions are now valid at pressure.
!***********************************************************************
!     FindpHScaleConversionFactor:
!     SELECT pHScale     !  this is the scale they will be put on
      if (pHScale .eq. 1) then           ! CASE "pH"
        pHfactor = 1.
      elseif (pHScale .eq. 2) then       ! CASE "pHsws"
        pHfactor = 1.
      elseif (pHScale .eq. 3) then       ! CASE "pHtot"
        pHfactor = SWStoTOT
      elseif (pHScale .eq. 4) then       ! CASE "pHfree"
        pHfactor = SWStoTOT / FREEtoTOT
      elseif (pHScale .eq. 5) then       ! CASE "pHNBS"
        pHfactor = fH
      endif
!
!***********************************************************************
!     ConvertFromSWSpHScaleToChosenScale:
      do i=1,4
        K(i)=K(i)*pHfactor
      enddo
!     KS [K(5)] and KF [K(6)] remain on the free pH scale
      do i=7,10
        K(i) = K(i) * pHfactor
      enddo
!
!     The constants should all be on the chosen pH scale at pressure.
!***********************************************************************
!     CalculateFugacityConstants:
!     !!! This assumes that the pressure is at one atmosphere, 
!          or close to it.
!     Otherwise, the Pres term in the exponent affects the results.
!     Weiss, R. F., Marine Chemistry 2:203-215, 1974.
!     Delta and B in cm3/mol
      Delta = (57.7 - .118 * TempK)
      B = -1636.75 + 12.0408 * TempK - .0327957 * TempK * TempK
      B = B + 3.16528 * .00001 * TempK * TempK * TempK
!
!     For a mixture of CO2 and air at 1 atm (at low CO2 concentrations):
      P1atm = 1.01325     !  in bar
      FugFac = exp((B + 2. * Delta) * P1atm / RT)
!
      if (WhichKs .eq. 6 .or. WhichKs .eq. 7) FugFac = 1.
!     GEOSECS and Peng assume pCO2 = fCO2, or FugFac = 1
!
!***********************************************************************
!     CalculateVPFac:
!     Weiss, R. F., and Price, B. A., Nitrous oxide solubility in water
!     and seawater, Marine Chemistry 8:347-359, 1980.
!     They fit the data of Goff and Gratch (1946) with the vapor pressure
!     lowering by sea salt as given by Robinson (1954).
!     This fits the more complicated Goff and Gratch, and Robinson 
!     equations from 273 to 313 deg K and 0 to 40 Sal with a standard 
!     error of .015%, about 5 uatm over this range.
!     This may be on IPTS-29 since they didn't mention the temperature
!     scale, and the data of Goff and Gratch came before IPTS-48.
!     The references are:
!     Goff, J. A. and Gratch, S., Low pressure properties of water 
!     from -160 deg to 212 deg F, Transactions of the American 
!     Society of Heating and Ventilating Engineers 52:95-122, 1946.
!     Robinson, Journal of the Marine Biological Association of the U. K.
!     33:449-455, 1954.
!
!     This is eq. 10 on p. 350.
!     This is in atmospheres.
      VPWP = exp(24.4543 - 67.4509 * (100. / TempK) - 4.8489 * 
     &  log(TempK / 100.))
      VPCorrWP = exp(-.000544 * Sal)
      VPSWWP = VPWP * VPCorrWP
      VPFac = 1. - VPSWWP     !  this assumes 1 atmosphere
!
!***********************************************************************
      return
      end
#endif /* BIOLOGY_NPZDOC && !OCMIP_CARBON */
