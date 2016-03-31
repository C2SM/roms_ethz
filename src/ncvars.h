! This is "ncvars.h":   indices for character array "vname" to keep
!----- -- -----------   names of netCDF variables and their attributes
! indxTime        time
! indxZ           free-surface
! indxUb,Vb     vertically integrated 2D U,V-momentum components
! indxU,V       3D u- and v-velocity components
! indxT,S,...,Zoo tracers (temperature, salinity, biotracers).
! indxO         "omega" vertical velocity (flux)
! indxW         "true" vertical velocity
! indxR           density anomaly
! indxAkv,Akt,Aks vertical viscosity/diffusivity coefficients
! indxHbls      depth of planetary boundary layer in KPP model
! indxHbbl      depth of bottom boundary layer in BKPP model

! indxAi        fraction of cell covered by ice
! indxUi,Vi     U,V-components of sea ice velocity
! indxHi,HS     depth of ice cover and depth of snow cover
! indxTIsrf     temperature of ice surface

! indxBSD,BSS bottom sediment grain Density and Size.
! indxWWA,WWD,WWP  wind induced wave Amplitude, Direction,and Period

! indxSUSTR,indxSVSTR  surface U-, V-momentum stress (wind forcing)
! indxSHFl        net surface heat flux.
! indxSWRad       shortwave radiation flux
! indxSST         sea surface temperature
! indxdQdSST      Q-correction coefficient dQdSST
! indxSSFl        surface fresh water flux

      integer, parameter :: indxTime=1, indxZ=2, indxUb=3, indxVb=4
#ifdef SOLVE3D
     &                    , indxU=5, indxV=6, indxO=7, indxW=8
     &                    , indxR=9, indxT=10
# ifdef SALINITY
     &                    , indxS=indxT+1
# endif
# ifdef BIOLOGY
#  ifdef SALINITY
     &                    , indxNO3=indxS+1
#  else
     &                    , indxNO3=indxT+1
# endif
     &                    , indxNH4 =indxNO3+1, indxChla=indxNO3+2
     &                    , indxPhyt=indxNO3+3, indxZoo =indxNO3+4
     &                    , indxSDet=indxNO3+5, indxLDet=indxNO3+6
# endif
     &                    , indxAkv=indxT+NT,   indxAkt=indxAkv+1

# ifdef SOLVE3D
     &                    , indxSUSTR=indxAkt+3
# else
     &                    , indxSUSTR=indxVb+1
# endif
     &                    , indxSVSTR=indxSUSTR+1, indxSHFl=indxSUSTR+2
     &                    , indxSWRad=indxSHFl+1

# ifdef SALINITY
     &                    , indxSSFl=indxSWRad+1, indxSSS=indxSWRad+2
     &                    , indxSST=indxSWRad+3
# else
     &                    , indxSST=indxSWRad+1
# endif
     &                    , indxdQdSST=indxSST+1

# ifdef WRITE_DEPTHS
     &                    , indxz_r=indxAkt+1,  indxz_w=indxAkt+2
     &                    , indxHz=indxAkt+3
# endif

# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC || defined BIOLOGY_BEC2

      /* Create space for pH, pCO2, pCO2air, PARinc, PAR: */
     &     + 5

#  ifdef SEDIMENT_BIOLOGY
     &     + NT_sed
#  endif
#  ifdef WRITE_DEPTHS
     &     + 3
#  endif 
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC || BIOLOGY_BEC2 */
# ifdef SALINITY
     &                    , indxAks=indxAkt+1
# endif
# ifdef LMD_KPP
#  ifdef SALINITY
     &                    , indxHbls=indxAks+1
#  else
     &                    , indxHbls=indxAkt+1
#  endif
# endif
# ifdef LMD_BKPP
     &                    , indxHbbl=indxHbls+1
#  endif

# if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
     &                    , indxdust=indxSST+3
     &                    , indxiron=indxSST+4
# endif /* BIOLOGY_BEC || BIOLOGY_BEC2 */
# ifdef SG_BBL96
#  ifndef ANA_WWAVE
     &                    , indxWWA=indxSST+5,  indxWWD=indxWWA+1
     &                    , indxWWP=indxWWA+2
#  endif
# endif
#endif
#ifdef ICEOBS
     &                    , indxCi=indxSST+8  ! wastes 3 indices if ANA_WWAVE is not defined!
     &                    , indxFi=indxCi+1
     &                    , indxMi=indxFi+1
#endif
#ifdef ICE
      integer, parameter :: indxAi=????,     indxUi=indxAi+1,
     &                    , indxVi=indxAi+2, indxHi=indxAi+3,
     &                      indxHS=indxAi+4, indxTIsrf=indxAi+5
#endif

#ifdef SOLVE3D
# ifdef BIOLOGY_NPZDOC
      integer indxNO3, indxNH4, indxChla,
     &        indxPhyt, indxZoo, indxSDet, indxLDet
      parameter (indxNO3=indxT+ntrc_salt+ntrc_pas+1)
      parameter (indxNH4 =indxNO3+1, indxChla=indxNO3+2, 
     &           indxPhyt=indxNO3+3, indxZoo=indxNO3+4, 
     &           indxSDet=indxNO3+5, indxLDet=indxNO3+6)
#  ifdef OXYGEN
      integer indxO2
      parameter (indxO2 = indxLDet + 1)
#   ifdef CARBON
      integer indxDIC, indxTALK, indxSDetC, indxLDetC, indxCaCO3
      integer indxPH_rst, indxPCO2_rst, indxPCO2air_rst
      parameter (indxDIC = indxO2 + 1)
      parameter (indxTALK = indxDIC + 1)
      parameter (indxSDetC = indxTALK + 1)
      parameter (indxLDetC = indxSDetC + 1)
      parameter (indxCaCO3 = indxLDetC + 1)
      parameter (indxPH_rst = indxCaCO3 + 1)
      parameter (indxPCO2_rst = indxPH_rst + 1)
      parameter (indxPCO2air_rst = indxPCO2_rst + 1)
#   endif /* CARBON */
#  endif /* OXYGEN */
! PAR and PARinc:
      integer indxPARinc_rst, indxPAR_rst
#  ifdef OXYGEN
#   ifdef CARBON
      parameter (indxPARinc_rst = indxPCO2air_rst + 1)
#   else
      parameter (indxPARinc_rst = indxO2 + 1)
#   endif /* CARBON */
#  else
      parameter (indxPARinc_rst = indxLDet + 1)
#  endif /* OXYGEN */
      parameter (indxPAR_rst = indxPARinc_rst + 1)
! Sediment tracers:
#  ifdef SEDIMENT_BIOLOGY
      integer indxSedOrgN
      parameter (indxSedOrgN = indxPAR_rst + 1)
#   ifdef CARBON
      integer indxSedOrgC, indxSedCaCO3
      parameter (indxSedOrgC = indxSedOrgN + 1)
      parameter (indxSedCaCO3 = indxSedOrgC + 1)
#   endif /* CARBON */
#  endif /* SEDIMENT_BIOLOGY */
#  ifdef WRITE_DEPTHS
       parameter(indxz_r=indxPAR_rst+NT_sed+1, indxz_w=indxz_r+1,
     &           indxHz=indxz_w+1)
#  endif /* WRITE_DEPTHS */
# endif /* BIOLOGY_NPZDOC */

# ifdef BIOLOGY_BEC
#  if defined CH_CARBON_DEPTH
       integer indxPo4,indxNo3,indxSio3,indxNh4,indxFe,indxO2,indxDic,
     &   indxAlk,indxDoc,indxSpc,indxSpchl,indxSpcaco3,indxDiatc,indxDiatchl,
     & indxZooc,indxSpfe,indxDiatsi,indxDiatfe,indxDiazc,indxDiazchl,
     & indxDiazfe,indxDon,indxDofe,indxDop,indxPH_rst,
     & indxPCO2_rst, indxPCO2air_rst, indxPARinc_rst, indxPAR_rst,
     & indxCO2STARd_rst, indxHCO3d_rst, indxCO3d_rst, indxPHd_rst
       parameter ( indxPO4=indxT+ntrc_salt+ntrc_pas+1,
     &           indxNo3 =indxPO4+1, indxSio3=indxPO4+2,
     &           indxNh4 =indxPO4+3, indxFe=indxPO4+4,
     &           indxO2 =indxPO4+5, indxDic=indxPO4+6,
     &           indxAlk =indxPO4+7, indxDoc=indxPO4+8,
     &           indxSpc =indxPO4+9, indxSpchl=indxPO4+10,
     &           indxSpcaco3 =indxPO4+11, indxDiatc=indxPO4+12,
     &           indxDiatchl =indxPO4+13, indxZooc=indxPO4+14,
     &           indxSpfe =indxPO4+15, indxDiatsi=indxPO4+16,
     &           indxDiatfe =indxPO4+17, indxDiazc=indxPO4+18,
     &           indxDiazchl =indxPO4+19, indxDiazfe=indxPO4+20,
     &           indxDon =indxPO4+21, indxDofe=indxPO4+22,
     &           indxDop =indxPO4+23)
       parameter(indxPH_rst = indxDOP+1
     &      )
       parameter(indxPCO2_rst = indxPH_rst+1,
     &      indxPCO2air_rst = indxPCO2_rst+1,
     &      indxPARinc_rst = indxPCO2air_rst+1,
     &      indxCO2STARd_rst = indxPARinc_rst+1,
     &      indxHCO3d_rst = indxCO2STARd_rst+1,
     &      indxCO3d_rst = indxHCO3d_rst+1,
     &      indxPHd_rst = indxCO3d_rst+1,
     &      indxPAR_rst = indxPHd_rst+1)
#  else
       integer indxPo4,indxNo3,indxSio3,indxNh4,indxFe,indxO2,indxDic,
     &   indxAlk,indxDoc,indxSpc,indxSpchl,indxSpcaco3,indxDiatc,indxDiatchl,
     & indxZooc,indxSpfe,indxDiatsi,indxDiatfe,indxDiazc,indxDiazchl,
     & indxDiazfe,indxDon,indxDofe,indxDop,indxPH_rst,
     & indxPCO2_rst, indxPCO2air_rst, indxPARinc_rst, indxPAR_rst
       parameter ( indxPO4=indxT+ntrc_salt+ntrc_pas+1,
     &           indxNo3 =indxPO4+1, indxSio3=indxPO4+2,
     &           indxNh4 =indxPO4+3, indxFe=indxPO4+4,
     &           indxO2 =indxPO4+5, indxDic=indxPO4+6,
     &           indxAlk =indxPO4+7, indxDoc=indxPO4+8,
     &           indxSpc =indxPO4+9, indxSpchl=indxPO4+10,
     &           indxSpcaco3 =indxPO4+11, indxDiatc=indxPO4+12,
     &           indxDiatchl =indxPO4+13, indxZooc=indxPO4+14,
     &           indxSpfe =indxPO4+15, indxDiatsi=indxPO4+16,
     &           indxDiatfe =indxPO4+17, indxDiazc=indxPO4+18,
     &           indxDiazchl =indxPO4+19, indxDiazfe=indxPO4+20,
     &           indxDon =indxPO4+21, indxDofe=indxPO4+22,
     &           indxDop =indxPO4+23)
       parameter(indxPH_rst = indxDOP+1
     &      )
       parameter(indxPCO2_rst = indxPH_rst+1,
     &      indxPCO2air_rst = indxPCO2_rst+1,
     &      indxPARinc_rst = indxPCO2air_rst+1,
     &      indxPAR_rst = indxPARinc_rst+1)
#  endif /* CH_CARBON_DEPTH */
#  ifdef WRITE_DEPTHS
       parameter(indxz_r=indxPAR_rst+1, indxz_w=indxz_r+1,
     &           indxHz=indxz_w+1)
#  endif /* WRITE_DEPTHS */
# endif /* BIOLOGY_BEC */
# ifdef BIOLOGY_BEC2

!  MM Merge remark: This block should be rewritten in 
!      integer, parameter ::   style

       integer indxPo4,indxNo3,indxSio3,indxNh4,indxFe,indxO2,indxDic,
     &   indxAlk,indxDoc,indxDon,indxDofe,indxDop,indxDopr,indxDonr,indxZooc,
     &   indxSpchl,indxSpc,indxSpfe,indxSpcaco3,indxDiatchl,indxDiatc,
     &   indxDiatfe,indxDiatsi,indxDiazchl,indxDiazc,indxDiazfe
       parameter (indxPo4=indxT+ntrc_salt+ntrc_pas+1,
     &            indxNo3=indxPO4+1, indxSio3=indxPO4+2,
     &            indxNh4=indxPO4+3, indxFe=indxPO4+4,
     &            indxO2=indxPO4+5,  indxDic=indxPO4+6,
     &            indxAlk=indxPO4+7, indxDoc=indxPO4+8,
     &            indxDon=indxPO4+9, indxDofe=indxPO4+10,
     &            indxDop=indxPO4+11, indxDopr=indxPO4+12,
     &            indxDonr=indxPO4+13, indxZooc=indxPO4+14,
     &            indxSpchl=indxPO4+15, indxSpc=indxPO4+16,
     &            indxSpfe=indxPO4+17, indxSpcaco3=indxPO4+18,
     &            indxDiatchl=indxPO4+19, indxDiatc=indxPO4+20,
     &            indxDiatfe=indxPO4+21, indxDiatsi=indxPO4+22,
     &            indxDiazchl=indxPO4+23, indxDiazc=indxPO4+24,
     &            indxDiazfe=indxPO4+25)
#  ifdef WRITE_DEPTHS
       parameter(indxz_r=indxDiazfe+1, indxz_w=indxz_r+1,
     &           indxHz=indxz_w+1)
#  endif /* WRITE_DEPTHS */
#  ifdef BEC2_DIAG
       ! Indices to be used in vname_bec2_diag_2d:
       integer indxPH,indxPCO2,indxPCO2air,indxPARinc,indxWS10m,indxXKW,
     &    indxFGO2,indxFGCO2,indxATMPRESS,indxSCHMIDTO2,indxO2SAT,indxSCHMIDTCO2,
     &    indxPVO2,indxPVCO2,indxCO2STAR,indxDCO2STAR,indxIRONFLUX,indxSEDDENITRIF
       parameter( indxPH=1,indxPCO2=indxPH+1,indxPCO2air=indxPH+2,indxPARinc=indxPH+3,
     &            indxFGO2=indxPH+4,indxFGCO2=indxPH+5,indxWS10m=indxPH+6,
     &            indxXKW=indxPH+7,indxATMPRESS=indxPH+8,indxSCHMIDTO2=indxPH+9,
     &            indxO2SAT=indxPH+10,indxSCHMIDTCO2=indxPH+11,indxPVO2=indxPH+12,
     &            indxPVCO2=indxPH+13,indxCO2STAR=indxPH+14,indxDCO2STAR=indxPH+15,
     &            indxIRONFLUX=indxPH+16,indxSEDDENITRIF=indxPH+17 )
       ! Indices to be used in vname_bec2_diag_3d:
       integer indxPAR,indxPOCFLUXIN,indxPOCPROD,indxPOCREMIN,indxCACO3FLUXIN,indxPCACO3PROD,
     &    indxCACO3REMIN,indxSIO2FLUXIN,indxSIO2PROD,indxSIO2REMIN,indxDUSTFLUXIN,
     &    indxDUSTREMIN,indxPIRONFLUXIN,indxPIRONPROD,indxPIRONREMIN,indxGRAZESP,
     &    indxGRAZEDIAT,indxGRAZEDIAZ,indxSPLOSS,indxDIATLOSS,indxZOOLOSS,indxSPAGG,
     &    indxDIATAGG,indxPHOTOCSP,indxPHOTOCDIAT,indxTOTPROD,indxDOCPROD,indxDOCREMIN,
     &    indxFESCAVENGE,indxSPNLIM,indxSPFEUPTAKE,indxSPPO4UPTAKE,indxSPLIGHTLIM,
     &    indxDIATNLIM,indxDIATFEUPTAKE,indxDIATPO4UPTAKE,indxDIATSIO3UPTAKE,
     &    indxDIATLIGHTLIM,indxCACO3PROD,indxDIAZNFIX,indxDIAZLOSS,indxPHOTOCDIAZ,
     &    indxDIAZPO4UPTAKE,indxDIAZFEUPTAKE,indxDIAZLIGHTLIM,indxFESCAVENGERATE,
     &    indxDONPROD,indxDONREMIN,indxDOFEPROD,indxDOFEREMIN,indxDOPPROD,indxDOPREMIN,
     &    indxDIATSIUPTAKE,indxIRONUPTAKESP,indxIRONUPTAKEDIAT,indxIRONUPTAKEDIAZ,indxNITRIF,
     &    indxDENITRIF,indxSPNO3UPTAKE,indxDIATNO3UPTAKE,indxDIAZNO3UPTAKE,indxSPNH4UPTAKE,
     &    indxDIATNH4UPTAKE,indxDIAZNH4UPTAKE,indxGRAZEDICSP,indxGRAZEDICDIAT,indxGRAZEDICDIAZ,
     &    indxLOSSDICSP,indxLOSSDICDIAT,indxLOSSDICDIAZ,indxZOOLOSSDIC,indxDIAZAGG,indxGRAZESPZOO,
     &    indxGRAZEDIATZOO,indxGRAZEDIAZZOO,indxSPQCACO3,indxSPPHOTOACC,indxDIATPHOTOACC,
     &    indxDIAZPHOTOACC,indxSPCZERO,indxDIATCZERO,indxDIAZCZERO,indxDOCZERO,
     &    indxZOOCZERO,indxSPCACO3ZERO,indxDONRREMIN
       parameter( indxPAR=1,indxPOCFLUXIN=indxPAR+1,indxPOCPROD=indxPAR+2,
     &            indxPOCREMIN=indxPAR+3,indxCACO3FLUXIN=indxPAR+4,indxPCACO3PROD=indxPAR+5,
     &            indxCACO3REMIN=indxPAR+6,indxSIO2FLUXIN=indxPAR+7,indxSIO2PROD=indxPAR+8,
     &            indxSIO2REMIN=indxPAR+9,indxDUSTFLUXIN=indxPAR+10,indxDUSTREMIN=indxPAR+11,
     &            indxPIRONFLUXIN=indxPAR+12,indxPIRONPROD=indxPAR+13,indxPIRONREMIN=indxPAR+14,
     &            indxGRAZESP=indxPAR+15,indxGRAZEDIAT=indxPAR+16,indxGRAZEDIAZ=indxPAR+17,
     &            indxSPLOSS=indxPAR+18,indxDIATLOSS=indxPAR+19,indxZOOLOSS=indxPAR+20,
     &            indxSPAGG=indxPAR+21,indxDIATAGG=indxPAR+22,indxPHOTOCSP=indxPAR+23,
     &            indxPHOTOCDIAT=indxPAR+24,indxTOTPROD=indxPAR+25,indxDOCPROD=indxPAR+26,
     &            indxDOCREMIN=indxPAR+27,indxFESCAVENGE=indxPAR+28,indxSPNLIM=indxPAR+29,
     &            indxSPFEUPTAKE=indxPAR+30,indxSPPO4UPTAKE=indxPAR+31,indxSPLIGHTLIM=indxPAR+32,
     &            indxDIATNLIM=indxPAR+33,indxDIATFEUPTAKE=indxPAR+34,indxDIATPO4UPTAKE=indxPAR+35,
     &            indxDIATSIO3UPTAKE=indxPAR+36,indxDIATLIGHTLIM=indxPAR+37,indxCACO3PROD=indxPAR+38,
     &            indxDIAZNFIX=indxPAR+39,indxDIAZLOSS=indxPAR+40,indxPHOTOCDIAZ=indxPAR+41,
     &            indxDIAZPO4UPTAKE=indxPAR+42,indxDIAZFEUPTAKE=indxPAR+43,indxDIAZLIGHTLIM=indxPAR+44,
     &            indxFESCAVENGERATE=indxPAR+45,indxDONPROD=indxPAR+46,indxDONREMIN=indxPAR+47,
     &            indxDOFEPROD=indxPAR+48,indxDOFEREMIN=indxPAR+49,indxDOPPROD=indxPAR+50,
     &            indxDOPREMIN=indxPAR+51,indxDIATSIUPTAKE=indxPAR+52,indxIRONUPTAKESP=indxPAR+53,
     &            indxIRONUPTAKEDIAT=indxPAR+54,indxIRONUPTAKEDIAZ=indxPAR+55,indxNITRIF=indxPAR+56,
     &            indxDENITRIF=indxPAR+57,indxSPNO3UPTAKE=indxPAR+58,indxDIATNO3UPTAKE=indxPAR+59,
     &            indxDIAZNO3UPTAKE=indxPAR+60,indxSPNH4UPTAKE=indxPAR+61,indxDIATNH4UPTAKE=indxPAR+62,
     &            indxDIAZNH4UPTAKE=indxPAR+63,indxGRAZEDICSP=indxPAR+64,indxGRAZEDICDIAT=indxPAR+65,
     &            indxGRAZEDICDIAZ=indxPAR+66,indxLOSSDICSP=indxPAR+67,indxLOSSDICDIAT=indxPAR+68,
     &            indxLOSSDICDIAZ=indxPAR+69,indxZOOLOSSDIC=indxPAR+70,indxDIAZAGG=indxPAR+71,
     &            indxGRAZESPZOO=indxPAR+72,indxGRAZEDIATZOO=indxPAR+73,indxGRAZEDIAZZOO=indxPAR+74,
     &            indxSPQCACO3=indxPAR+75,indxSPPHOTOACC=indxPAR+76,indxDIATPHOTOACC=indxPAR+77,
     &            indxDIAZPHOTOACC=indxPAR+78,indxSPCZERO=indxPAR+79,indxDIATCZERO=indxPAR+80,
     &            indxDIAZCZERO=indxPAR+81,indxDOCZERO=indxPAR+82,indxZOOCZERO=indxPAR+83,
     &            indxSPCACO3ZERO=indxPAR+84,indxDONRREMIN=indxPAR+85 )
#  endif /* BEC2_DIAG */
# endif /* BIOLOGY_BEC2 */
#endif /* SOLVE3D */

! Length of netCDF variable "time_step"

      integer, parameter :: iaux=6


! Naming conventions for indices, variable IDs, etc...
!------- ----------- --- -------- -------- ---- ------
! prefix ncid_  means netCDF ID for netCDF file
!        nrec_  record number in netCDF file since initialization
!        nrpf_  maximum number of records per file  (output netCDF
!                                                       files only)
! prefix/ending rst_/_rst refers to restart  netCDF file
!               his_/_his           history
!               avg_/_avg           averages
!               stn_/_stn           stations
!                    _frc           forcing
!                    _clm           climatology
!
! endings refer to:  ___Time  time [in seconds]
!                    ___Tstep time step numbers and record numbers
! all objects with   ___Z     free-surface
! these endings are  ___Ub    vertically integrated 2D U-momentum
! either:            ___Vb    vertically integrated 2D V-momentum
!
!  netCDF IDs, if    ___U     3D U-momentum
!  occur with prefix ___V     3D V-momentum
!  rst/his/avg/stn   ___T(NT) tracers
!                    ___R     density anomaly
! or                 ___O     omega vertical velocity
!                    ___W     true vertical velocity
!  parameter indices 
!  if combined with  ___Akv   vertical viscosity coefficient
!  prefix "indx"     ___Akt   vertical T-diffusion coefficient
!  (see above).      ___Aks   vertical S-diffusion coefficient
!                    ___Hbl   depth of KPP surface boundary layer
!                    ___Hbbl  thickness of KPP bottom boundary layer
!
! vname    character array for variable names and attributes;

      integer, parameter :: max_frc_files=360
      integer max_frc, ncfrc(max_frc_files), nrst,  ncrst,   nrecrst,
     &      nrrec, nrpfrst, ncidclm, nwrt,  nchis, nrechis, nrpfhis

#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
     &     , ntdust, ntiron
#endif
#ifdef TSOURCE
      integer ncidtsrc(max_frc_files)
#endif
      common /ncvars/       max_frc, ncfrc, nrst,  ncrst,   nrecrst,
     &      nrrec, nrpfrst, ncidclm, nwrt,  nchis, nrechis, nrpfhis
#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
     &     , ntdust, ntiron
#endif
#ifdef TSOURCE
     &     , ncidtsrc
#endif
#ifdef BIOLOGY_BEC2
      integer ntnox, ntnhy, ntdin_river
      common /ncvars/ ntnox, ntnhy, ntdin_river
#endif
#ifdef AVERAGES
      integer ntsavg,  navg
      common /ncvars/ ntsavg,  navg
# ifdef SLICE_AVG
      integer ntsslavg,  nslavg
      common /ncvars/ ntsslavg,  nslavg
# endif
#endif
#ifdef STATIONS
      integer nsta
      common /ncvars/ nsta
#endif
#ifdef FLOATS
      integer nflt
      common /ncvars/ nflt
#endif

! NetCFD IDs for model variables

      integer rstTime, rstTstep,  rstZ, rstUb, rstVb,
     &        hisTime, hisTstep,  hisZ, hisUb, hisVb
      common /ncvars/
     &        rstTime, rstTstep,  rstZ, rstUb, rstVb,
     &        hisTime, hisTstep,  hisZ, hisUb, hisVb
#ifdef SOLVE3D
# ifdef EXACT_RESTART
#  ifdef EXTRAP_BAR_FLUXES
      integer rst_DU_avg2,    rst_DV_avg2,
     &        rst_DU_avg_bak, rst_DV_avg_bak
      common /ncvars/ rst_DU_avg2,    rst_DV_avg2,
     &                rst_DU_avg_bak, rst_DV_avg_bak
#  elif defined PRED_COUPLED_MODE
      integer rst_rufrc, rst_rvfrc
      common /ncvars/ rst_rufrc, rst_rvfrc
#  endif
# endif
      integer rstU, rstV, rstT(NT+1), hisO,   hisW,   hisR,
     &        hisU, hisV, hisT(NT+1), hisAkv, hisAkt, hisAks 
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC || defined BIOLOGY_BEC2
#  if defined CH_CARBON_DEPTH
     &      , rstHCO3d, rstCO3d, rstCO2STARd, rstPHd, rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisHCO3d, hisCO3d, hisCO2STARd, hisPHd, hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
     &      , avgHCO3d, avgCO3d, avgCO2STARd, avgPHd, avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
# else
     &      , rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
     &      , avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
#  ifdef SLICE_AVG
     &      , slavgPH, slavgPCO2, slavgPCO2air, slavgPARinc, slavgPAR
#  endif
#  endif /* CH_CARBON_DEPTH */
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
#if defined BIOLOGY_BEC2 && defined BEC2_DIAG
     &      , hisf_graze_CaCO3_remin, hisQ_BEC2, hisDONrefract
     &      , avgf_graze_CaCO3_remin, avgQ_BEC2, avgDONrefract
     &      , slavgf_graze_CaCO3_remin, slavgQ_BEC2, slavgDONrefract
#endif /* BIOLOGY_BEC2 && BEC2_DIAG */

# ifdef WRITE_DEPTHS
     &      , hisz_r, hisz_w, hisHz
#  ifdef AVERAGES
     &      , avgz_r, avgz_w, avgHz
#  endif
# endif /* WRITE_DEPTHS */
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
     &      , rstTstepFA
#endif
# ifdef SEDIMENT_BIOLOGY
     &      , rstTsed(NT_sed), hisTsed(NT_sed)
# endif /* SEDIMENT_BIOLOGY */
      common /ncvars/
     &        rstU, rstV, rstT,       hisO,   hisW,   hisR,
     &        hisU, hisV, hisT,       hisAkv, hisAkt, hisAks
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#  if defined CH_CARBON_DEPTH
     &      , rstHCO3d, rstCO3d, rstCO2STARd, rstPHd, rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisHCO3d, hisCO3d, hisCO2STARd, hisPHd, hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
     &      , avgHCO3d, avgCO3d, avgCO2STARd, avgPHd, avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
# else
     &      , rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
     &      , avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
#  ifdef SLICE_AVG
     &      , slavgPH, slavgPCO2, slavgPCO2air, slavgPARinc, slavgPAR
#  endif
#  endif /* CH_CARBON_DEPTH */
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */

# ifdef WRITE_DEPTHS
     &      , hisz_r, hisz_w, hisHz
#  ifdef AVERAGES
     &      , avgz_r, avgz_w, avgHz
#  endif
# endif /* WRITE_DEPTHS */
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
     &      , rstTstepFA
#endif
# ifdef SEDIMENT_BIOLOGY
     &      , rstTsed, hisTsed
# endif /* SEDIMENT_BIOLOGY */

# ifdef LMD_KPP
      integer rstHbls, hisHbls
      common /ncvars/ rstHbls, hisHbls
# endif
# ifdef LMD_BKPP
      integer rstHbbl, hisHbbl
      common /ncvars/ rstHbbl, hisHbbl
# endif
#endif /* SOLVE3D */

#ifdef AVERAGES
      integer ncavg, nrecavg, nrpfavg,  avgTime, avgZ, avgUb, avgVb
      common /ncvars/  ncavg, nrecavg,  nrpfavg,
     &                                  avgTime, avgZ, avgUb, avgVb
#  ifdef SOLVE3D
      integer avgU,  avgV,  avgT(NT+1), avgR,    avgO,    avgW,
     &                                  avgAkv,  avgAkt,  avgAks
#   ifdef SEDIMENT_BIOLOGY
     &      , avgTsed(NT_sed)
#   endif /* SEDIMENT_BIOLOGY */
      common /ncvars/ avgU, avgV, avgT, avgR,    avgO,    avgW,
     &                                  avgAkv,  avgAkt,  avgAks
#  ifdef SEDIMENT_BIOLOGY
     &      , avgTsed
#  endif /* SEDIMENT_BIOLOGY */
#  ifdef LMD_KPP
      integer avgHbls
      common /ncvars/ avgHbls
#  endif
#  ifdef LMD_BKPP
      integer avgHbbl
      common /ncvars/ avgHbbl
#  endif
#  ifdef SLICE_AVG
      integer ncslavg, nrecslavg,  nrpfslavg,
     &    slavgTime, slavgTstep, slavgZ, slavgUb, slavgVb
      common /ncvars/  ncslavg,  nrecslavg, nrpfslavg,
     &    slavgTime, slavgTstep, slavgZ, slavgUb, slavgVb
      integer ksl, slavgU, slavgV, slavgT(NT+1), slavgR
     &      , slavgO, slavgW, slavgAkv, slavgAkt, slavgAks
#   ifdef SEDIMENT_BIOLOGY
     &      , slavgTsed(NT_sed)
#   endif /* SEDIMENT_BIOLOGY */
      common /ncvars/ ksl, slavgU, slavgV, slavgT, slavgR
     &      , slavgO, slavgW, slavgAkv, slavgAkt, slavgAks
#  ifdef SEDIMENT_BIOLOGY
     &      , slavgTsed
#  endif /* SEDIMENT_BIOLOGY */
#  ifdef LMD_KPP
      integer slavgHbls
      common /ncvars/ slavgHbls
#  endif
#  ifdef LMD_BKPP
      integer slavgHbbl
      common /ncvars/ slavgHbbl
#  endif
#  endif /* SLICE_AVG */
# endif /* SOLVE3D */
#endif /* AVERAGES */

#ifdef STATIONS
      integer nstation,  ispos(NS), jspos(NS),
     &        ncidstn, nrecstn, nrpfstn, stnTime, stnZ, stnUb, stnVb
      common /ncvars/ nstation, ispos,   jspos,
     &        ncidstn, nrecstn, nrpfstn, stnTime, stnZ, stnUb, stnVb
# ifdef SOLVE3D
      integer stnU,  stnV,  stnT(NT+1),  stnR,    stnO,    stnW,
     &                                   stnAkv,  stnAkt,  stnAks
      common /ncvars/ stnU, stnV, stnT,  stnR,    stnO,    stnW,
     &                                   stnAkv,  stnAkt,  stnAks
#  ifdef LMD_KPP
      integer stnHbls
      common /ncvars/ stnHbls
#  endif
#  ifdef LMD_BKPP
      integer stnHbbl
      common /ncvars/ stnHbbl
#  endif
# endif /* SOLVE3D */
#endif /* STATIONS */

#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      integer ncid_ad(ntrc_pas), ad_tid(ntrc_pas), bcVal_id(ntrc_pas)
      common /ncvars/ ncid_ad, ad_tid,  bcVal_id
#endif /* PASSIVE_TRACER && AGE_DYE_TRACER */

#ifdef SOLVE3D
# define NWRTHIS 100+NT
#else
# define NWRTHIS 14      
#endif
      logical ldefhis, wrthis(NWRTHIS)
      common /ncvars/ ldefhis, wrthis
#ifdef AVERAGES
      logical wrtavg(NWRTHIS)
      common /ncvars/ wrtavg
# ifdef SLICE_AVG
      logical wrtslavg(NWRTHIS)
      common /ncvars/ wrtslavg
# endif
#endif
#ifdef FLOATS
      logical ldefflt
      common /ncvars/ ldefflt
#endif
#ifdef STATIONS
      logical wrtsta(NWRTHIS)
      common /ncvars/ wrtsta
#endif


! Horizontal Grid Type Codes =  0,1,2,3 for RHO-, U-, V-, PSI-points

      integer, parameter :: r_var=0, u_var=1, v_var=2, q_var=3

!            Horizontal array dimensions in netCDF files. In the case
! xi_rho     of MPI code with PARALLEL_FILES activated these dimensions
! xi_u       depend on the corresponding sizes of individual subdomains
! eta_rho    rather than the whole physical grid, therefore they become
! eta_v      live variables placed in common block and assigned values
!            in "mpi_setup" rather tnan be parameters defined here.

#if defined MPI && defined PARALLEL_FILES
      integer xi_rho,xi_u, eta_rho,eta_v
      common /ncvars/ xi_rho,xi_u, eta_rho,eta_v
#else
      integer, parameter :: xi_rho=LLm+2, eta_rho=MMm+2,
     &                     xi_u=xi_rho-1, eta_v=eta_rho-1
#endif

      integer, parameter :: max_name_size=256
      character date_str*44, title*80
      character(len=max_name_size) :: ininame, grdname, rstname,
     &                             hisname, frcfile(max_frc_files)
      common /cncvars/ date_str, title, ininame, grdname, rstname,
     &                                           hisname, frcfile
#ifdef AVERAGES
      character(len=max_name_size) avgname
      common /cncvars/ avgname
# ifdef SLICE_AVG
      character*(max_name_size) slavgname
      common /cncvars/ slavgname
# endif
#endif
#if (defined TCLIMATOLOGY && !defined ANA_TCLIMA) || !defined ANA_SSH
      integer, parameter :: max_clm_files=30
      integer max_clm
      character(len=max_name_size) clm_file(max_clm_files)
      common /cncvars/ max_clm, clm_file
#endif /* (defined TCLIMATOLOGY && ... */
#if defined T_FRC_BRY  || defined M2_FRC_BRY || \
    defined M3_FRC_BRY || defined Z_FRC_BRY
      integer, parameter :: max_bry_files=8
      character(len=max_name_size) bry_file(max_bry_files)
      common /cncvars/ bry_file
#endif

#if defined TSOURCE
      character*(max_name_size) tsrc_file
      common /cncvars/ tsrc_file
#endif

#ifdef STATIONS
      character(len=max_name_size) staname
      common /cncvars/ staname
#endif
#ifdef ASSIMILATION
      character(len=max_name_size) aparnam, assname
      common /cncvars/ aparnam, assname
#endif
      character*42  vname(4,
#if defined BIOLOGY || defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC || defined BIOLOGY_BEC2
     &                       40+NT-2)
#else
     &                       40)
#endif
      common /cncvars/ vname

!DL: array for storing the names of those variables in the climatology
! files which contain the data times:
      character(len=40) tclm_name(NT)
      common /cncvars/ tclm_name
