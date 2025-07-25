!
! The macro LAST_I should always be set to the last used index number.
! It must be define in ncvars.h and should be redefined accordingly
! at the end of this include file

!#ifdef WRITE_DEPTHS
!       integer, parameter :: indxz_r=LAST_I+1
!     &            , indxz_w=indxz_r+1
!     &            , indxHz=indxz_w+1
!# undef LAST_I
!# define LAST_I indxHz
!#endif

#ifdef BIOLOGY_NPZDOC
# ifdef CARBON
       integer, parameter :: indxPH_rst=LAST_I+1
     &                   , indxPCO2_rst=indxPH_rst+1
     &                   , indxPCO2air_rst=indxPCO2_rst+1
#  undef LAST_I
#  define LAST_I indxPCO2air_rst
# endif
       integer, parameter :: indxPAR_rst=LAST_I+1
     &                   , indxPARinc_rst=indxPAR_rst+1
# undef LAST_I
# define LAST_I indxPARinc_rst
#endif /* BIOLOGY_NPZDOC */

#ifdef BIOLOGY_BEC2
      integer, parameter :: indxdust=LAST_I+1
     &            , indxiron=LAST_I+2
     &            , indxPH_rst=LAST_I+3
# undef LAST_I
# define LAST_I indxPH_rst
#ifndef BEC2_DIAG
     &            , indxPH=LAST_I+3, indxPCO2=LAST_I+4
     &            , indxPCO2air=LAST_I+5, indxPARinc=LAST_I+6
     &            , indxPAR=LAST_I+7
# undef LAST_I
# define LAST_I indxPAR
#endif /* !BEC2_DIAG */

# ifdef BEC2_DIAG
  ! Indices to be used in vname_bec2_diag_2d only:
      integer, parameter :: indxPCO2air=1,indxPARinc=indxPCO2air+1
     &            , indxFGO2=indxPCO2air+2,indxFGCO2=indxPCO2air+3,indxWS10m=indxPCO2air+4
     &            , indxXKW=indxPCO2air+5,indxATMPRESS=indxPCO2air+6
     &            , indxSCHMIDTO2=indxPCO2air+7
     &            , indxO2SAT=indxPCO2air+8,indxSCHMIDTCO2=indxPCO2air+9
     &            , indxPVO2=indxPCO2air+10
     &            , indxPVCO2=indxPCO2air+11,indxIRONFLUX=indxPCO2air+12
     &            , indxSEDDENITRIF=indxPCO2air+13,indxPH=indxPCO2air+14
     &            , indxPCO2=indxPCO2air+15,indxCO2STAR=indxPCO2air+16
     &            , indxPCO2OC=indxPCO2air+17
     &            , indxDCO2STAR=indxPCO2air+18
# undef LAST_I_BIO2D
# define LAST_I_BIO2D indxDCO2STAR
# ifdef Ncycle_SY
     &            , indxschmidt_n2o=LAST_I_BIO2D+1, indxpvn2o=LAST_I_BIO2D+2, indxn2osat=LAST_I_BIO2D+3
     &		  , indxfgn2o=LAST_I_BIO2D+4,indxschmidt_n2=LAST_I_BIO2D+5, indxpvn2=LAST_I_BIO2D+6
     &            , indxfgn2=LAST_I_BIO2D+7, indxn2sat=LAST_I_BIO2D+8
# undef LAST_I_BIO2D
# define LAST_I_BIO2D indxn2sat
# ifdef N2O_TRACER_DECOMP
     &            , indxfgn2o_ao1=LAST_I_BIO2D+1, indxfgn2o_siden=LAST_I_BIO2D+2
     &            , indxfgn2o_soden=LAST_I_BIO2D+3, indxfgn2o_atm=LAST_I_BIO2D+4
# undef LAST_I_BIO2D
# define LAST_I_BIO2D indxfgn2o_atm
# endif
# endif
# ifdef N2O_NEV
     &            , indxfgn2o_nev=LAST_I_BIO2D+1
# undef LAST_I_BIO2D
# define LAST_I_BIO2D indxfgn2o_nev
# endif
#  ifdef CCHEM_MOCSY
#   if !defined CCHEM_TODEPTH
     &            , indxCO3=LAST_I_BIO2D+1, indxHCO3=LAST_I_BIO2D+2
# undef LAST_I_BIO2D
# define LAST_I_BIO2D indxHCO3
#   endif
#  else /* CCHEM_MOCSY */
!     &            , indxPH=LAST_I_BIO2D+1, indxPCO2=LAST_I_BIO2D+2, indxCO2STAR=LAST_I_BIO2D+3
!# undef LAST_I_BIO2D
!# define LAST_I_BIO2D indxCO2STAR
#  endif /* CCHEM_MOCSY */
     &            , indxFESEDFLUX=LAST_I_BIO2D+1,indxFLUXTOSED=LAST_I_BIO2D+2
     &            , indxCACO3FLUXTOSED=LAST_I_BIO2D+3
     &            , indxSIO2FLUXTOSED=LAST_I_BIO2D+4,indxPIRONFLUXTOSED=LAST_I_BIO2D+5
     &            , indxDUSTFLUXTOSED=LAST_I_BIO2D+6,indxPOCSEDLOSS=LAST_I_BIO2D+7
     &            , indxOTHERREMIN=LAST_I_BIO2D+8,indxCACO3SEDLOSS=LAST_I_BIO2D+9
     &            , indxSIO2SEDLOSS=LAST_I_BIO2D+10
# undef LAST_I_BIO2D
# define LAST_I_BIO2D indxSIO2SEDLOSS



   ! Indices to be used in vname_bec2_diag_3d only:
      integer, parameter :: indxPAR=1,indxPOCFLUXIN=indxPAR+1,indxPOCPROD=indxPAR+2
     &            , indxPOCREMIN=indxPAR+3,indxCACO3FLUXIN=indxPAR+4,indxPCACO3PROD=indxPAR+5
     &            , indxCACO3REMIN=indxPAR+6,indxSIO2FLUXIN=indxPAR+7,indxSIO2PROD=indxPAR+8
     &            , indxSIO2REMIN=indxPAR+9,indxDUSTFLUXIN=indxPAR+10,indxDUSTREMIN=indxPAR+11
     &            , indxPIRONFLUXIN=indxPAR+12,indxPIRONPROD=indxPAR+13,indxPIRONREMIN=indxPAR+14
     &            , indxGRAZESP=indxPAR+15,indxGRAZEDIAT=indxPAR+16,indxGRAZEDIAZ=indxPAR+17
     &            , indxSPLOSS=indxPAR+18,indxDIATLOSS=indxPAR+19,indxZOOLOSS=indxPAR+20
     &            , indxSPAGG=indxPAR+21,indxDIATAGG=indxPAR+22,indxPHOTOCSP=indxPAR+23
     &            , indxPHOTOCDIAT=indxPAR+24,indxTOTPROD=indxPAR+25,indxDOCPROD=indxPAR+26
     &            , indxDOCREMIN=indxPAR+27,indxFESCAVENGE=indxPAR+28,indxSPNLIM=indxPAR+29
     &            , indxSPFEUPTAKE=indxPAR+30,indxSPPO4UPTAKE=indxPAR+31,indxSPLIGHTLIM=indxPAR+32
     &            , indxDIATNLIM=indxPAR+33,indxDIATFEUPTAKE=indxPAR+34,indxDIATPO4UPTAKE=indxPAR+35
     &            , indxDIATSIO3UPTAKE=indxPAR+36,indxDIATLIGHTLIM=indxPAR+37,indxCACO3PROD=indxPAR+38
     &            , indxDIAZNFIX=indxPAR+39,indxDIAZLOSS=indxPAR+40,indxPHOTOCDIAZ=indxPAR+41
     &            , indxDIAZPO4UPTAKE=indxPAR+42,indxDIAZFEUPTAKE=indxPAR+43,indxDIAZLIGHTLIM=indxPAR+44
     &            , indxFESCAVENGERATE=indxPAR+45,indxDONPROD=indxPAR+46,indxDONREMIN=indxPAR+47
     &            , indxDOFEPROD=indxPAR+48,indxDOFEREMIN=indxPAR+49,indxDOPPROD=indxPAR+50
     &            , indxDOPREMIN=indxPAR+51,indxDIATSIUPTAKE=indxPAR+52,indxIRONUPTAKESP=indxPAR+53
     &            , indxIRONUPTAKEDIAT=indxPAR+54,indxIRONUPTAKEDIAZ=indxPAR+55,indxNITRIF=indxPAR+56
     &            , indxDENITRIF=indxPAR+57,indxSPNO3UPTAKE=indxPAR+58,indxDIATNO3UPTAKE=indxPAR+59
     &            , indxDIAZNO3UPTAKE=indxPAR+60,indxSPNH4UPTAKE=indxPAR+61,indxDIATNH4UPTAKE=indxPAR+62
     &            , indxDIAZNH4UPTAKE=indxPAR+63,indxGRAZEDICSP=indxPAR+64,indxGRAZEDICDIAT=indxPAR+65
     &            , indxGRAZEDICDIAZ=indxPAR+66,indxLOSSDICSP=indxPAR+67,indxLOSSDICDIAT=indxPAR+68
     &            , indxLOSSDICDIAZ=indxPAR+69,indxZOOLOSSDIC=indxPAR+70,indxDIAZAGG=indxPAR+71
     &            , indxGRAZESPZOO=indxPAR+72,indxGRAZEDIATZOO=indxPAR+73,indxGRAZEDIAZZOO=indxPAR+74
     &            , indxSPQCACO3=indxPAR+75,indxSPPHOTOACC=indxPAR+76,indxDIATPHOTOACC=indxPAR+77
     &            , indxDIAZPHOTOACC=indxPAR+78,indxSPCZERO=indxPAR+79,indxDIATCZERO=indxPAR+80
     &            , indxDIAZCZERO=indxPAR+81,indxDOCZERO=indxPAR+82,indxZOOCZERO=indxPAR+83
     &            , indxSPCACO3ZERO=indxPAR+84,indxDONRREMIN=indxPAR+85, indxTOTCHL=indxPAR+86
     &            , indxSPPLIM=indxPAR+87,indxDIATPLIM=indxPAR+88,indxDIAZPLIM=indxPAR+89
     &            , indxTOTPHYTOC=indxPAR+90,indxPOCPRODZOOLOSS=indxPAR+91
     &            , indxPOCPRODDIAT=indxPAR+92,indxPOCPRODSP=indxPAR+93,indxPOCPRODDIAZ=indxPAR+94
     &            , indxQSI=indxPAR+95,indxTOTGRAZE=indxPAR+96,indxTOTGRAZEDIC=indxPAR+97
     &            , indxTOTGRAZEZOO=indxPAR+98,indxTOTAGG=indxPAR+99,indxTOTNO3UPTAKE=indxPAR+100
     &            , indxTOTNH4UPTAKE=indxPAR+101,indxTOTLOSS=indxPAR+102,indxTOTLOSSDIC=indxPAR+103
     &            , indxNEXCRETE=indxPAR+104,indxTOTPOC=indxPAR+105,indxNCP=indxPAR+106 
     &            , indxo2cons=indxPAR+107, indxo2prod=indxPAR+108
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxo2prod
#  if defined CCHEM_MOCSY && defined CCHEM_TODEPTH
     &            , indxPH=LAST_I_BIO3D+1, indxPCO2=indxPH+1, indxCO3=indxPH+2
     &            , indxHCO3=indxPH+3, indxCO2STAR=indxPH+4
     &            , indxOMEGACALC=indxPH+5, indxOMEGAARAG=indxPH+6
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxOMEGAARAG
#  endif
# ifdef USE_EXPLICIT_VSINK
     &            , indxPIRONHARDREMIN=LAST_I_BIO3D+1,indxCACO3HARDREMIN=LAST_I_BIO3D+2
     &            , indxSIO2HARDREMIN=LAST_I_BIO3D+3
     &            , indxPOCHARDREMIN=LAST_I_BIO3D+4,indxDUSTHARDREMIN=LAST_I_BIO3D+5
     &            , indxPIRONSOFTREMIN=LAST_I_BIO3D+6,indxCACO3SOFTREMIN=LAST_I_BIO3D+7
     &            , indxSIO2SOFTREMIN=LAST_I_BIO3D+8
     &            , indxPOCSOFTREMIN=LAST_I_BIO3D+9,indxDUSTSOFTREMIN=LAST_I_BIO3D+10
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxDUSTSOFTREMIN
# else /* USE_EXPLICIT_VSINK */
! already defined above
# endif /* USE_EXPLICIT_VSINK */
# ifdef BEC_COCCO
     &            , indxGRAZECOCCO=LAST_I_BIO3D+1,indxCOCCOLOSS=LAST_I_BIO3D+2
     &            , indxCOCCOAGG=LAST_I_BIO3D+3,indxPHOTOCCOCCO=LAST_I_BIO3D+4
     &            , indxCOCCONLIM=LAST_I_BIO3D+5
     &            , indxCOCCOPO4UPTAKE=LAST_I_BIO3D+6,indxCOCCOFEUPTAKE=LAST_I_BIO3D+7
     &            , indxCOCCOLIGHTLIM=LAST_I_BIO3D+8,indxCACO3PRODCOCCO=LAST_I_BIO3D+9
     &            , indxIRONUPTAKECOCCO=LAST_I_BIO3D+10,indxCOCCONO3UPTAKE=LAST_I_BIO3D+11
     &            , indxCOCCONH4UPTAKE=LAST_I_BIO3D+12,indxCOCCOGRAZEDIC=LAST_I_BIO3D+13
     &            , indxCOCCOLOSSDIC=LAST_I_BIO3D+14,indxGRAZECOCCOZOO=LAST_I_BIO3D+15
     &            , indxQCACO3COCCO=LAST_I_BIO3D+16,indxCOCCOPHOTOACC=LAST_I_BIO3D+17
     &            , indxCOCCOPLIM=LAST_I_BIO3D+18,indxPOCPRODCOCCO=LAST_I_BIO3D+19
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxPOCPRODCOCCO
# endif
# ifdef BEC_DDA
     &            , indxGRAZEDDA=LAST_I_BIO3D+1,indxDDALOSS=LAST_I_BIO3D+2
     &            , indxDDAAGG=LAST_I_BIO3D+3,indxPHOTOCDDA=LAST_I_BIO3D+4,indxDDANLIM=LAST_I_BIO3D+5
     &            , indxDDAPO4UPTAKE=LAST_I_BIO3D+6,indxDDAFEUPTAKE=LAST_I_BIO3D+7
     &            , indxDDALIGHTLIM=LAST_I_BIO3D+8
     &            , indxIRONUPTAKEDDA=LAST_I_BIO3D+9,indxDDANO3UPTAKE=LAST_I_BIO3D+10
     &            , indxDDANH4UPTAKE=LAST_I_BIO3D+11,indxDDAGRAZEDIC=LAST_I_BIO3D+12
     &            , indxDDALOSSDIC=LAST_I_BIO3D+13,indxGRAZEDDAZOO=LAST_I_BIO3D+14
     &            , indxDDAPHOTOACC=LAST_I_BIO3D+15
     &            , indxDDAPLIM=LAST_I_BIO3D+16,indxDDANFIX=LAST_I_BIO3D+17
     &            , indxDDASIO3UPTAKE=LAST_I_BIO3D+18
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxDDASIO3UPTAKE
# endif
# ifdef BEC_PHAEO
     &            , indxGRAZEPHAEO=LAST_I_BIO3D+1,indxPHAEOLOSS=LAST_I_BIO3D+2
     &            , indxPHAEOAGG=LAST_I_BIO3D+3,indxPHOTOCPHAEO=LAST_I_BIO3D+4
     &            , indxPHAEONLIM=LAST_I_BIO3D+5
     &            , indxPHAEOPO4UPTAKE=LAST_I_BIO3D+6,indxPHAEOFEUPTAKE=LAST_I_BIO3D+7
     &            , indxPHAEOLIGHTLIM=LAST_I_BIO3D+8
     &            , indxIRONUPTAKEPHAEO=LAST_I_BIO3D+9,indxPHAEONO3UPTAKE=LAST_I_BIO3D+10
     &            , indxPHAEONH4UPTAKE=LAST_I_BIO3D+11,indxPHAEOGRAZEDIC=LAST_I_BIO3D+12
     &            , indxPHAEOLOSSDIC=LAST_I_BIO3D+13,indxGRAZEPHAEOZOO=LAST_I_BIO3D+14
     &            , indxPHAEOPHOTOACC=LAST_I_BIO3D+15,indxPHAEOPLIM=LAST_I_BIO3D+16
     &            , indxPOCPRODPHAEO=LAST_I_BIO3D+17
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxPOCPRODPHAEO
# endif

# ifdef Ncycle_SY
     &            , indxammox=LAST_I_BIO3D+1,indxnitrox=LAST_I_BIO3D+2
     &            , indxanammox=LAST_I_BIO3D+3,indxDENITRIF1=LAST_I_BIO3D+4,indxDENITRIF2=LAST_I_BIO3D+5
     &            , indxDENITRIF3=LAST_I_BIO3D+6, indxSPNO2UPTAKE=LAST_I_BIO3D+7,indxDIATNO2UPTAKE=LAST_I_BIO3D+8
     &            , indxDIAZNO2UPTAKE=LAST_I_BIO3D+9, indxN2OAMMOX=LAST_I_BIO3D+10, indxN2OSODEN_CONS=LAST_I_BIO3D+11
     &            , indxN2OAO1_CONS=LAST_I_BIO3D+12, indxN2OATM_CONS=LAST_I_BIO3D+13
     &            , indxN2OSIDEN_CONS=LAST_I_BIO3D+14
#  undef LAST_I_BIO3D
#  define LAST_I_BIO3D indxN2OSIDEN_CONS
# endif

# ifdef N2O_NEV
     &            , indxn2oprodnev=LAST_I_BIO3D+1,indxn2oconsnev=LAST_I_BIO3D+2
# endif

# endif /* BEC2_DIAG */
#endif /* BIOLOGY_BEC2 */

!
! Integer NetCDF IDs for BIOLOGY variables
!

#if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC2
      integer rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
# ifdef CH_CARBON_DEPTH
     &      , rstHCO3d, rstCO3d, rstCO2STARd, rstPHd
     &      , hisHCO3d, hisCO3d, hisCO2STARd, hisPHd
# endif
# ifdef AVERAGES
     &      , avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
#  ifdef CH_CARBON_DEPTH
     &      , avgHCO3d, avgCO3d, avgCO2STARd, avgPHd
#  endif
# endif /* AVERAGES */
# ifdef SLICE_AVG
     &      , slavgPH, slavgPCO2, slavgPCO2air, slavgPARinc, slavgPAR
# endif
# ifdef BEC2_DIAG
     &      , hisf_graze_CaCO3_remin, hisQ_BEC2, hisDONrefract
#  ifdef AVERAGES
     &      , avgf_graze_CaCO3_remin, avgQ_BEC2, avgDONrefract
#  endif
#  ifdef SLICE_AVG
     &      , slavgf_graze_CaCO3_remin, slavgQ_BEC2, slavgDONrefract
#  endif
# endif
# if defined BGC_FLUX_ANALYSIS && !defined PHYS_FLUX_ANALYSIS
     &      , rstTstepFA
# endif

      common /ncvars/ rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
# ifdef AVERAGES
     &      , avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
# endif /* AVERAGES */
# ifdef SLICE_AVG
     &      , slavgPH, slavgPCO2, slavgPCO2air, slavgPARinc, slavgPAR
# endif
# if defined BGC_FLUX_ANALYSIS && !defined PHYS_FLUX_ANALYSIS
     &      , rstTstepFA
# endif
#endif /* BIOLOGY_NPZDOC or BIOLOGY_BEC2 */


#ifdef SEDIMENT_BIOLOGY
      integer, dimension(NT_sed) :: rstTsed, hisTsed
# ifdef AVERAGES
     &       , avgTsed
# endif
# ifdef SLICE_AVG
     &       , slavgTsed
# endif
      common /ncvars/ rstTsed, hisTsed
# ifdef AVERAGES
     &      , avgTsed
# endif
# ifdef SLICE_AVG
     &      , slavgTsed
# endif
#endif /* SEDIMENT_BIOLOGY */
