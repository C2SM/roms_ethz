! This is "ncvars.h":   indices for character array "vname" for
!----- -- -----------   names of netCDF variables and attributes
!
! indxTime        time
! indxZ           free-surface
! indxUb,indxVb   vertically integrated 2D U,V-momentum components
!
! indxU,indxV     3D U- and V-momenta.
! indxT,indxS, ..., indxZoo  tracers (temerature, salinity,
!                 biological tracers.
! indxO,indxW     omega vertical mass flux and true vertical velocity
! indxR           density anomaly
!
! indxAkv,indxAkt,indxAks  vertical viscosity/diffusivity coeffcients
! indxHbl         depth of planetary boundary layer in KPP model
! indxHbbl        depth of bottom boundary layer in (B)KPP model
!
! indxSUSTR,indxSVSTR  surface U-, V-momentum stress (wind forcing)
! indxSHFl        net surface heat flux.
! indxSWRad       shortwave radiation flux
! indxSST         sea surface temperature
! indxdQdSST      Q-correction coefficient dQdSST
! indxSSFl        surface fresh water flux
!
! indxAi          fraction of cell covered by ice
! indxUi,indxVi   U,V-components of sea ice velocity
! indxHi,indxHS   depth of ice cover and depth of snow cover
! indxTIsrf       temperature of ice surface
!
! indxBSD,indxBSS bottom sediment grain Density and Size.
! indxWWA,indxWWD,indxWWP   wind induced wave Amplitude,
!                 Direction and Period
!
      integer indxTime, indxZ, indxUb, indxVb
      parameter (indxTime=1, indxZ=2, indxUb=3, indxVb=4)
#ifdef SOLVE3D
      integer indxU, indxV, indxT
      parameter (indxU=5, indxV=6, indxT=7)
# ifdef SALINITY
      integer indxS
      parameter (indxS=indxT+1)
# endif
# ifdef BIOLOGY
      integer indxNO3, indxNH4, indxChla,
     &        indxPhyt, indxZoo, indxSDet, indxLDet
      parameter (indxNO3=indxT+ntrc_salt+ntrc_pas+1)
      parameter (indxNH4=indxNO3+1,  indxChla=indxNO3+2,
     &           indxPhyt=indxNO3+3, indxZoo=indxNO3+4,
     &           indxSDet=indxNO3+5, indxLDet=indxNO3+6)
# endif /* BIOLOGY */
      integer indxO, indxW, indxR, indxAkv, indxAkt, indxRich
      integer indxRichN
      parameter (indxO=indxT+NT
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
      /* Create space for pH, pCO2, pCO2air, PARinc, and PAR: */
     &     + 5
#  ifdef SEDIMENT_BIOLOGY
     &     + NT_sed
#  endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
     &     )
      parameter (indxW=indxO+1, indxR=indxO+2,
     &     indxAkv=indxR+1, indxRich=indxAkv+1, indxRichN=indxAkv+2,
     &     indxAkt=indxAkv+3)
# ifdef SALINITY
      integer indxAks
      parameter (indxAks=indxAkt+1)
# endif
# ifdef LMD_KPP
      integer indxHbl
#  ifdef SALINITY
      parameter (indxHbl=indxAks+1)
#  else
      parameter (indxHbl=indxAkt+1)
#  endif
# endif
# ifdef LMD_BKPP
      integer indxHbbl
#  ifdef SALINITY
      parameter (indxHbbl=indxAks+2)
#  else
      parameter (indxHbbl=indxAkt+2)
#  endif
# endif
#endif /* SOLVE3D */


      integer indxSUSTR, indxSVSTR
#ifdef SOLVE3D
      parameter (indxSUSTR=indxAkt+4, indxSVSTR=indxAkt+5)
#else
      parameter (indxSUSTR=indxVb+1,  indxSVSTR=indxSUSTR+1)
#endif

#ifdef SOLVE3D
      integer indxSHFl, indxSWRad
      parameter (indxSHFl=indxAkt+6)
# ifdef SALINITY
      integer indxSSFl
      parameter (indxSSFl=indxSHFl+1, indxSWRad=indxSHFl+2)
# else
      parameter (indxSWRad=indxSHFl+1)
# endif
      integer indxSST, indxdQdSST, indxSSS
      parameter (indxSST=indxSWRad+1, indxdQdSST=indxSST+1,
     &                                   indxSSS=indxSST+2)
# ifdef BIOLOGY_BEC
      integer indxdust
      parameter (indxdust=indxSST+3)
      integer indxiron
      parameter (indxiron=indxSST+4)
# endif
# ifdef SG_BBL96
#  ifndef ANA_WWAVE
      integer indxWWA,indxWWD,indxWWP
      parameter (indxWWA=indxSST+5,   indxWWD=indxWWA+1,
     &                                indxWWP=indxWWA+2)
#  endif
# endif
#endif
#ifdef ICE
      integer indxAi
      parameter (indxAi=????)
      integer indxUi, indxVi, indxHi, indxHS, indxTIsrf
      parameter (indxUi=indxAi+1, indxVi=indxAi+2, indxHi=indxAi+3,
     &                         indxHS=indxAi+4, indxTIsrf=indxAi+5)
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
      integer indxPARinc_rst, indxPAR_rst
      parameter (indxDIC = indxO2 + 1)
      parameter (indxTALK = indxDIC + 1)
      parameter (indxSDetC = indxTALK + 1)
      parameter (indxLDetC = indxSDetC + 1)
      parameter (indxCaCO3 = indxLDetC + 1)
      parameter (indxPH_rst = indxCaCO3 + 1)
      parameter (indxPCO2_rst = indxPH_rst + 1)
      parameter (indxPCO2air_rst = indxPCO2_rst + 1)
      parameter (indxPARinc_rst = indxPCO2air_rst + 1)
      parameter (indxPAR_rst = indxPARinc_rst + 1)
#   endif /* CARBON */
#  endif /* OXYGEN */
#  ifdef SEDIMENT_BIOLOGY
      integer indxSedOrgN
#   ifdef CARBON
      parameter (indxSedOrgN = indxPAR_rst + 1)
      integer indxSedOrgC, indxSedCaCO3
      parameter (indxSedOrgC = indxSedOrgN + 1)
      parameter (indxSedCaCO3 = indxSedOrgC + 1)
#   else /* CARBON */
#    ifdef OXYGEN
      parameter (indxSedOrgN = indxO2 + 1)
#    else /* OXYGEN */
      parameter (indxSedOrgN = indxLDet + 1)
#    endif /* OXYGEN */
#   endif /* CARBON */
#  endif /* SEDIMENT_BIOLOGY */
# endif /* BIOLOGY_NPZDOC */
# ifdef BIOLOGY_BEC
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
       parameter (indxPH_rst = indxDOP+1
     &      )
       parameter(indxPCO2_rst = indxPH_rst+1,
     &      indxPCO2air_rst = indxPCO2_rst+1,
     &      indxPARinc_rst = indxPCO2air_rst+1,
     &      indxPAR_rst = indxPARinc_rst+1)
# endif /* BIOLOGY_BEC */
#endif /* SOLVE3D */




!
! Naming conventions for indices, variable IDs, etc...
!
! prefix ncid_  means netCDF ID for netCDF file
!        nrec_  record number in netCDF file since initialization
!        nrpf_  maximum number of records per file  (output netCDF
!                                                       files only)
! prefix/ending rst_/_rst refers to restart  netCDF file
!               his_/_his           history
!               avg_/_avg           averages
!               sta_/_sta           stations
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
!  rst/his/avg/sta   ___T(NT) tracers
!                    ___R     density anomaly
! or                 ___O     omega vertical velocity
!                    ___W     true vertical velocity
!  parameter indices 
!  if combined with  ___Akv   vertical viscosity coefficient
!  prefix "indx"     ___Akt   vertical T-diffusion coefficient
!  (see above).      ___Aks   vertical S-diffusion coefficient
!                    ___Hbl   depth of mixed layer LMD_KPP.
!
! vname    character array for variable names and attributes;
!
      integer max_frc_file
      parameter (max_frc_file=4)
      integer max_frc, ncidfrc(max_frc_file), nrst, ncidrst, nrecrst,
     &      nrrec, nrpfrst, ncidclm, nwrt, ncidhis, nrechis, nrpfhis
#ifdef BIOLOGY_NPZDOC
     &     , ncidclm2, ncidclm3
#endif
#ifdef BIOLOGY_BEC
     &     , ncidclm2, ncidclm3, ncidclm4, ntdust, ntiron
#endif
      common /ncvars/       max_frc, ncidfrc, nrst, ncidrst, nrecrst,
     &      nrrec, nrpfrst, ncidclm, nwrt, ncidhis, nrechis, nrpfhis
#ifdef BIOLOGY_NPZDOC
     &     , ncidclm2, ncidclm3
#endif
#ifdef BIOLOGY_BEC
     &     , ncidclm2, ncidclm3, ncidclm4, ntdust, ntiron
#endif




#ifdef AVERAGES
      integer ntsavg,  navg
      common /ncvars/ ntsavg,  navg
#endif
#ifdef STATIONS
      integer nsta
      common /ncvars/ nsta
#endif
#ifdef FLOATS
      integer nflt
      common /ncvars/ nflt
#endif




      integer rstTime, rstTstep,      rstZ,   rstUb,  rstVb,
     &        hisTime, hisTstep,      hisZ,   hisUb,  hisVb
      common /ncvars/
     &        rstTime, rstTstep,      rstZ,   rstUb,  rstVb,
     &        hisTime, hisTstep,      hisZ,   hisUb,  hisVb
#ifdef SOLVE3D
      integer rstU, rstV, rstT(NT+1), hisO,   hisW,   hisR,
     &        hisU, hisV, hisT(NT+1), hisAkv, hisAkt, hisAks, 
     &        hisRich, hisRichN
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
     &      , rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
     &      , avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
     &      , rstTstepFA
#endif
# ifdef SEDIMENT_BIOLOGY
     &      , rstTsed(NT_sed), hisTsed(NT_sed)
# endif /* SEDIMENT_BIOLOGY */
      common /ncvars/
     &        rstU, rstV, rstT,       hisO,   hisW,   hisR,
     &        hisU, hisV, hisT,       hisAkv, hisAkt, hisAks, 
     &        hisRich, hisRichN
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
     &      , rstPH, rstPCO2, rstPCO2air, rstPAR
     &      , hisPH, hisPCO2, hisPCO2air, hisPARinc, hisPAR
     &      , avgPH, avgPCO2, avgPCO2air, avgPARinc, avgPAR
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
     &      , rstTstepFA
#endif
# ifdef SEDIMENT_BIOLOGY
     &      , rstTsed, hisTsed
# endif /* SEDIMENT_BIOLOGY */

# ifdef LMD_KPP
      integer rstHbl, hisHbl
      common /ncvars/ rstHbl, hisHbl
# endif
# ifdef LMD_BKPP
      integer rstHbbl, hisHbbl
      common /ncvars/ rstHbbl, hisHbbl
# endif
#endif /* SOLVE3D */

#ifdef AVERAGES
      integer ncidavg, nrecavg,  nrpfavg,
     &        avgTime, avgTstep, avgZ,    avgUb, avgVb
      common /ncvars/  ncidavg,  nrecavg, nrpfavg,
     &        avgTime, avgTstep, avgZ,    avgUb, avgVb
# ifdef SOLVE3D
      integer avgU,  avgV,  avgT(NT+1), avgR,
     &        avgO,  avgW,  avgAkv,     avgAkt,  avgAks,
     &        avgRich, avgRichN
#  ifdef SEDIMENT_BIOLOGY
     &      , avgTsed(NT_sed)
#  endif /* SEDIMENT_BIOLOGY */
      common /ncvars/ avgU, avgV,       avgT,    avgR, 
     &        avgO,  avgW,  avgAkv,     avgAkt,  avgAks,
     &        avgRich, avgRichN
#  ifdef SEDIMENT_BIOLOGY
     &      , avgTsed
#  endif /* SEDIMENT_BIOLOGY */
#  ifdef LMD_KPP
      integer avgHbl
      common /ncvars/ avgHbl
#  endif
#  ifdef LMD_BKPP
      integer avgHbbl
      common /ncvars/ avgHbbl
#  endif
# endif /* SOLVE3D */
#endif /* AVERAGES */

#ifdef STATIONS
      integer ncidsta, nrecsta,  nrpfsta,  stadid,    stazid,
     &        staubid, stavbid,  nstation, ispos(NS), jspos(NS)
      common /ncvars/
     &        ncidsta, nrecsta,  nrpfsta,  stadid,    stazid,
     &        staubid, stavbid,  nstation, ispos,     jspos
# ifdef SOLVE3D
      integer stauid,  stavid,   statid(NT+1),        starid,
     &        stawid,  staakvid, staaktid,            staaksid
      common /ncvars/
     &        stauid,  stavid,   statid,              starid,
     &        stawid,  staakvid, staaktid,            staaksid
#  ifdef LMD_KPP
      integer stahblid
      common /ncvars/ stahblid
#  endif
#  ifdef LMD_BKPP
      integer stahbblid
      common /ncvars/ stahbblid
#  endif
# endif /* SOLVE3D */
#endif /* STATIONS */

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
#endif
#ifdef FLOATS
      logical ldefflt
      common /ncvars/ ldefflt
#endif
#ifdef STATIONS
      logical wsta(NWRTHIS)
      common /ncvars/ wsta
#endif

!
! Grid Type Codes:  r2dvar....w3hvar are codes for array types.
! ==== ==== ======  The codes are set according to the rule:
!                     horiz_grid_type+4*vert_grid_type
!    where horiz_grid_type=0,1,2,3 for RHO-,U-,V-,PSI-points
!    respectively and vert_grid_type=0 for 2D fields; 1,2 for
!    3D-RHO- and W-vertical points.
!
      integer r2dvar, u2dvar, v2dvar, p2dvar, r3dvar,
     &                u3dvar, v3dvar, p3dvar, w3dvar
      parameter (r2dvar=0, u2dvar=1, v2dvar=2, p2dvar=3,
     & r3dvar=4, u3dvar=5, v3dvar=6, p3dvar=7, w3dvar=8)
!
!            Horizontal array dimensions in netCDF files.
! xi_rho     NOTE: In MPI mode using PARALLEL_FILES these
! xi_u       depend on corresonding sizes of individual MPI
! eta_rho    subdomains rather than the whole physical grid, 
! eta_v      and therefore become live variables placed in 
!            common block here and set in mpi_setup.
!
      integer xi_rho,xi_u, eta_rho,eta_v
#if defined MPI && defined PARALLEL_FILES
      common /ncvars/ xi_rho,xi_u, eta_rho,eta_v
#else
      parameter (xi_rho=LLm+2,   eta_rho=MMm+2,
     &           xi_u=xi_rho-1,  eta_v=eta_rho-1)
#endif

      integer max_name_size
      parameter (max_name_size=64)
      character date_str*44, title*80
      character*(max_name_size) ininame, grdname,
     &                 hisname, rstname, frcfile(max_frc_file)
      common /cncvars/ date_str, title,  ininame,
     &        grdname, hisname, rstname, frcfile
#ifdef AVERAGES
      character*(max_name_size) avgname
      common /cncvars/ avgname
#endif
#if (defined TCLIMATOLOGY && !defined ANA_TCLIMA) || !defined ANA_SSH
      character*(max_name_size) clm_file
!# ifdef BIOLOGY_NPZDOC
!     & , clm_file2, clm_file3
!# endif
# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
     & , clm_file2, clm_file3, clm_file4
# endif
      common /cncvars/ clm_file
!# ifdef BIOLOGY_NPZDOC
!     & , clm_file2, clm_file3
!# endif
# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
     & , clm_file2, clm_file3, clm_file4
# endif
#endif
#if defined T_FRC_BRY  || defined M2_FRC_BRY || \
    defined M3_FRC_BRY || defined Z_FRC_BRY
      character*(max_name_size) bry_file 
      common /cncvars/ bry_file
#endif

#ifdef STATIONS
      character*(max_name_size) staname, sposnam
      common /cncvars/ staname, sposnam
#endif
#ifdef ASSIMILATION
      character*(max_name_size) aparnam, assname
      common /cncvars/ aparnam, assname
#endif
      character*42  vname(4,40+NT-2)
      common /cncvars/ vname
