! This is "ncvars.h":   indices for character array "vname" to keep
!----- -- -----------   names of netCDF variables and their attributes
! indxTime      time
! indxZ         free-surface
! indxUb,Vb     vertically integrated 2D U,V-momentum components
! indxU,V       3D u- and v-velocity components
! indxT,S,...,Zoo tracers (temperature, salinity, biotracers).
! indxO         "omega" vertical velocity (flux)
! indxW         "true" vertical velocity
! indxR         density anomaly
! indxAkv,Akt,Aks vertical viscosity/diffusivity coefficients
! indxHbls      depth of planetary boundary layer in KPP model
! indxHbbl      depth of bottom boundary layer in BKPP model
! indxRestflx   restoring flux for salinity

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
# undef LAST_I
# define LAST_I indxT
# ifdef SALINITY
     &                    , indxS=indxT+1
#  undef LAST_I
#  define LAST_I indxS
# endif
# ifdef BIOLOGY
#  include "tracers.h"
# endif
     &                    , indxAkv=indxT+NT,   indxAkt=indxAkv+1
# undef LAST_I
# define LAST_I indxAkt
# ifdef SALINITY
     &                    , indxAks=LAST_I+1
#  undef LAST_I
#  define LAST_I indxAks
# endif
# ifdef LMD_KPP
     &                    , indxHbls=LAST_I+1
#  undef LAST_I
#  define LAST_I indxHbls
# endif
# ifdef LMD_BKPP
     &                    , indxHbbl=LAST_I+1
#  undef LAST_I
#  define LAST_I indxHbbl
# endif

# ifdef KPP_DIAGNOSE
      integer, parameter :: indxKppRichm=LAST_I+1, indxKppRicht=LAST_I+2
     &           , indxKppSblSh=LAST_I+3, indxKppSblSt=LAST_I+4
     &           , indxKppSblRot=LAST_I+5, indxKppSblEnt=LAST_I+6
#  undef LAST_I
#  define LAST_I indxKppSblEnt
# endif
# if defined WRITE_HEATFLX
     &                    , indxHeatflx=LAST_I+1
#   undef LAST_I
#   define LAST_I indxHeatflx
# endif
# if defined WRITE_TEMP_REST
     &                    , indxRestflxTemp=LAST_I+1
#   undef LAST_I
#   define LAST_I indxRestflxTemp
# endif
# if defined WRITE_SALT_REST
     &                    , indxRestflxSalt=LAST_I+1
#   undef LAST_I
#   define LAST_I indxRestflxSalt
# endif
# ifdef WRITE_DEPTHS
     &                    , indxz_r=LAST_I+1,  indxz_w=LAST_I+2
     &                    , indxHz=LAST_I+3
#  undef LAST_I
#  define LAST_I indxHz
# endif
     &                    , indxSUSTR=LAST_I+1
     &                    , indxSVSTR=indxSUSTR+1, indxSHFl=indxSUSTR+2
     &                    , indxSWRad=indxSHFl+1
# undef LAST_I
# define LAST_I indxSWRad
# ifdef SALINITY
     &                    , indxSSFl=LAST_I+1, indxSSS=LAST_I+2
#  undef LAST_I
#  define LAST_I indxSSS
# endif
     &                    , indxSST=LAST_I+1
     &                    , indxdQdSST=indxSST+1
# undef LAST_I
# define LAST_I indxdQdSST

# ifdef SG_BBL96
#  ifndef ANA_WWAVE
     &                    , indxWWA=LAST_I+1, indxWWD=indxWWA+1
     &                    , indxWWP=indxWWA+2
#   undef LAST_I
#   define LAST_I indxWWP
#  endif
# endif /* SG_BBL96 */
#endif /* SOLVE3D */
#ifdef ICE
      integer, parameter :: indxAi=LAST_I+1     indxUi=indxAi+1,
     &                    , indxVi=indxAi+2, indxHi=indxAi+3,
     &                      indxHS=indxAi+4, indxTIsrf=indxAi+5
# undef LAST_I
# define LAST_I indxTIsrf
#endif

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
      common /ncvars/       max_frc, ncfrc, nrst,  ncrst,   nrecrst,
     &      nrrec, nrpfrst, ncidclm, nwrt,  nchis, nrechis, nrpfhis
#ifdef AVERAGES
      integer ntsavg,  navg
      common /ncvars/ ntsavg, navg
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
      common /ncvars/
     &        rstU, rstV, rstT,       hisO,   hisW,   hisR,
     &        hisU, hisV, hisT,       hisAkv, hisAkt, hisAks

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
# ifdef SOLVE3D
      integer avgU,  avgV,  avgT(NT+1), avgR,    avgO,    avgW,
     &                                  avgAkv,  avgAkt,  avgAks
      common /ncvars/ avgU, avgV, avgT, avgR,    avgO,    avgW,
     &                                  avgAkv,  avgAkt,  avgAks
#  ifdef LMD_KPP
      integer avgHbls
      common /ncvars/ avgHbls
#  endif
#  ifdef LMD_BKPP
      integer avgHbbl
      common /ncvars/ avgHbbl
#  endif
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

#ifdef SOLVE3D
# define NWRTHIS 16+NT-2+4  /* +4 to accomodate writing of Heatflx, TEMP-, SALT REST, CO2FLX */
!mm old: # define NWRTHIS 100+NT
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
      integer xi_rho, xi_u, eta_rho, eta_v
      common /ncvars/ xi_rho, xi_u, eta_rho, eta_v
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
#endif
#if defined T_FRC_BRY  || defined M2_FRC_BRY || \
    defined M3_FRC_BRY || defined Z_FRC_BRY
      integer, parameter :: max_bry_files=8
      character(len=max_name_size) bry_file(max_bry_files)
      common /cncvars/ bry_file
#endif

#ifdef STATIONS
      character(len=max_name_size) staname
      common /cncvars/ staname
#endif
#ifdef ASSIMILATION
      character(len=max_name_size) aparnam, assname
      common /cncvars/ aparnam, assname
#endif

#ifdef ICEOBS
      integer, parameter :: indxCi=LAST_I+1  ! concentration
     &                    , indxFi= indxCi+1 ! melting
     &                    , indxMi= indxFi+1 ! freezing
# undef LAST_I
# define LAST_I indxMi
#endif
#ifdef BIOLOGY
# include "bio_diag.h"
#endif

      character*42  vname(4,LAST_I)
      common /cncvars/ vname

!---------------------
! ETH Zurich additions
!---------------------

! files which contain the data times:
      character(len=40) tclm_name(NT)
      common /cncvars/ tclm_name

#ifdef TSOURCE
      integer ncidtsrc(max_frc_files)
      common /ncvars/ ncidtsrc
      character*(max_name_size) tsrc_file
      common /cncvars/ tsrc_file
#endif

#ifdef SOLVE3D
# ifdef KPP_DIAGNOSE
      integer hisKppRichm, hisKppRicht
     &      , hisKppSblSh, hisKppSblSt
     &      , hisKppSblRot, hisKppSblEnt
      common /ncvars/ hisKppRichm, hisKppRicht
     &              , hisKppSblSh, hisKppSblSt
     &              , hisKppSblRot, hisKppSblEnt
      integer rstKppRichm, rstKppRicht
     &      , rstKppSblSh, rstKppSblSt
     &      , rstKppSblRot, rstKppSblEnt
      common /ncvars/ rstKppRichm, rstKppRicht
     &              , rstKppSblSh, rstKppSblSt
     &              , rstKppSblRot, rstKppSblEnt
#  ifdef AVERAGES
      integer avgKppRichm, avgKppRicht
     &      , avgKppSblSh, avgKppSblSt
     &      , avgKppSblRot, avgKppSblEnt
      common /ncvars/ avgKppRichm, avgKppRicht
     &              , avgKppSblSh, avgKppSblSt
     &              ,avgKppSblRot, avgKppSblEnt
#  endif
# endif
#endif /* SOLVE3D */

# ifdef WRITE_HEATFLX
      integer hisHeatflx
#  ifdef AVERAGES
     &      , avgHeatflx
#  endif
#ifdef SLICE_AVG
     &      , slavgHeatflx
#  endif
      common /ncvars/ hisHeatflx
#  ifdef AVERAGES
     &      , avgHeatflx
#  endif
# endif /* WRITE_HEATFLX */

# ifdef WRITE_TEMP_REST
      integer hisRestflxTemp
#  ifdef AVERAGES
     &      , avgRestflxTemp
#  endif
#ifdef SLICE_AVG
     &      , slavgRestflxTemp
#  endif
      common /ncvars/ hisRestflxTemp
#  ifdef AVERAGES
     &              , avgRestflxTemp
#  endif
#ifdef SLICE_AVG
     &      , slavgRestflxTemp
# endif /* WRITE_TEMP_REST */

# ifdef WRITE_SALT_REST
      integer hisRestflxSalt
#  ifdef AVERAGES
     &      , avgRestflxSalt
#  endif
#ifdef SLICE_AVG
     &      , slavgRestflxSalt
# endif /* WRITE_SALT_REST */
      common /ncvars/ hisRestflxSalt
#  ifdef AVERAGES
     &              , avgRestflxSalt
#  endif
#ifdef SLICE_AVG
     &      , slavgRestflxSalt
# endif /* WRITE_SALT_REST */

# ifdef WRITE_DEPTHS
      integer hisz_r, hisz_w, hisHz
#  ifdef AVERAGES
     &      , avgz_r, avgz_w, avgHz
#  endif
#  ifdef SLICE_AVG
     &      , slavgz_r, slavgz_w, slavgHz
#  endif
      common /ncvars/ hisz_r, hisz_w, hisHz
#  ifdef AVERAGES
     &              , avgz_r, avgz_w, avgHz
#  endif
#  ifdef SLICE_AVG
     &      , slavgz_r, slavgz_w, slavgHz
#  endif
# endif /* WRITE_DEPTHS */


#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      integer ncid_ad(ntrc_pas), ad_tid(ntrc_pas), bcVal_id(ntrc_pas)
      common /ncvars/ ncid_ad, ad_tid,  bcVal_id
#endif /* PASSIVE_TRACER && AGE_DYE_TRACER */

#ifdef SLICE_AVG
      integer ksl, ntsslavg, nslavg
      common /ncvars/ ksl, ntsslavg, nslavg
      logical wrtslavg(NWRTHIS)
      common /ncvars/ wrtslavg
      character*(max_name_size) slavgname
      common /cncvars/ slavgname
      integer ncslavg, nrecslavg, nrpfslavg, slavgTime, slavgZ
     &      , slavgUb, slavgVb
      common /ncvars/ ncslavg, nrecslavg,  nrpfslavg
     &              , slavgTime, slavgZ, slavgUb, slavgVb
# ifdef SOLVE3D
      integer slavgU,  slavgV,  slavgT(NT+1), slavgR, slavgO, slavgW
     &      , slavgAkv, slavgAkt, slavgAks
      common /ncvars/ slavgU, slavgV, slavgT, slavgR, slavgO, slavgW
     &              , slavgAkv,  slavgAkt,  slavgAks
#  ifdef LMD_KPP
      integer slavgHbls
      common /ncvars/ slavgHbls
#  endif
#  ifdef LMD_BKPP
      integer slavgHbbl
      common /ncvars/ slavgHbbl
#  endif
# endif /* SOLVE3D */
#endif /* SLICE_AVG */

#if (defined TCLIMATOLOGY && !defined ANA_TCLIMA) || !defined ANA_SSH
      integer, parameter :: max_clm_files=30
      integer max_clm
      character(len=max_name_size) clm_file(max_clm_files)
      common /cncvars/ max_clm, clm_file
#endif /* (defined TCLIMATOLOGY && ... */




