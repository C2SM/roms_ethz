! This is include file "scalars.h"
!----- -- ------- ---- -----------
! The following common block contains time variables and indices for
! 2D (k-indices) and 3D (n-indices) computational engines.  Since they
! are changed together, it is advantageous to place them into the same
! cache line despite their mixed type, so that only one cache line is
! being invalidated and has to be propagated accross the multi-CPU
! machine.
! Array "proc" holds thread number and process IDs for individual
! threads; "cpu_init", "cpu_end" to measure CPU time consumed by each
! thread during the whole model run (these are for purely diagnostic
! and performance measurements and do not affect the model results.)
!
! Note that real values are placed first into the common block before
! integers. This is done to prevent misallignment of the 8-byte
! objects in the case when an uneven number of 4-byte integers is
! placed before a 8-byte real (in the case when default real size is
! set to 8 Bytes). Although misallignment is not formally a violation
! of fortran standard, it may cause performance degradation and/or
! make compiler issue a warning message (Sun, DEC Alpha) or even
! crash (Alpha).

      real(kind=4) cpu_init, cpu_net
      real(kind=8) WallClock, time, tdays
      integer proc(2), numthreads, iic, kstp, knew
#ifdef SOLVE3D
     &                           , iif, nstp, nnew, nrhs
#endif
#ifdef FLOATS
     &                     , nfp1, nf,  nfm1, nfm2, nfm3
#endif
     &                           , priv_count(16)
      logical synchro_flag, diag_sync
      common /priv_scalars/ WallClock, cpu_init, cpu_net,
     &   proc, time, tdays, numthreads, iic,  kstp, knew
#ifdef SOLVE3D
     &                           , iif, nstp, nnew, nrhs
#endif
#ifdef FLOATS
     &                      ,nfp1, nf,  nfm1, nfm2, nfm3
#endif
     &       , priv_count, synchro_flag, diag_sync
C$OMP THREADPRIVATE(/priv_scalars/)

! Slowly changing variables: these are typically set in the beginning
! of the run and either remain unchanged, or are changing only in
! association with the I/0.

! dt       Time step for 3D primitive equations [seconds];
! dtfast   Time step for 2D (barotropic) mode [seconds];

! xl,el    Physical size[m] of the domain in XI- and ETA-directions

! rdrg,rdrg2    Linear and quadratic bottom drag coefficients.
! gamma2   Slipperiness parameter, either 1. (free-slip)

! ntstart  Starting timestep in evolving the 3D primitive equations;
!                              usually 1, if not a restart run.
! ntimes   Number of timesteps for the 3D primitive equations in
!                                                    the current run.
! ndtfast  Number of timesteps for 2-D equations between each "dt".

! nrst     Number of timesteps between storage of restart fields.
! nwrt     Number of timesteps between writing of fields into
!                                                     history file.
! ninfo    Number of timesteps between print of single line
!                                   information to standard output.
! nsta     Number of timesteps between storage of station data.
! navg     Number of timesteps between storage of time-averaged
!                                                           fields.
! ntsavg   Starting timestep for accumulation of output time-
!                                                 averaged fields.
! nrrec    Counter of restart time records to read from disk,
!                   the last is used as the initial conditions.
! ldefhis  Logical switch to create a new history file: if .true.
!              create a new file, otherwise append an existing one.
! levsfrc  Deepest and shallowest level to apply surface momentum
! levbfrc                                stress as as bodyforce.

      real(kind=8) start_time, dt, dtfast, time_avg
      real xl,el, rdrg,rdrg2,Zob, visc2,gamma2
#ifdef SLICE_AVG
      real(kind=8) time_slavg
      common /scalars_main/ time_slavg
#endif
      common /scalars_main/ start_time, dt, dtfast, time_avg,
     &                   xl,el, rdrg,rdrg2,Zob, visc2,gamma2
#ifdef SOLVE3D
      real rho0, tnu2(NT)
      common /scalars_main/ rho0, tnu2

# if !defined LMD_MIXING && !defined BVF_MIXING && !defined PP_MIXING\
                         && !defined MY2_MIXING && !defined MY25_MIXING
      real Akv_bak, Akt_bak(NT)
      common /scalars_main/ Akv_bak, Akt_bak
# endif
# ifdef MY25_MIXING
      real Akq_bak,   q2nu2,   q2nu4
      common /scalars_main/ Akq_bak, q2nu2, q2nu4
# endif
# if defined SFLX_CORR && defined SALINITY
      real dSSSdt
      common /scalars_main/ dSSSdt
# endif
#endif
#ifdef SPONGE
      real n_sponge
      real v_sponge
      common /scalars_main/ n_sponge, v_sponge
#endif

!# if defined OBC_M2ORLANSKI && ( defined M2_FRC_BRY \
!                               || defined M2NUDGING )
      real attnM2
      common /scalars_main/ attnM2
!# endif

!# if defined M2_FRC_BRY || defined M2NUDGING
!      real attnM2
!      common /scalars_main/ attnM2
!# endif




#if  defined T_FRC_BRY || defined M2_FRC_BRY || defined TNUDGING \
  || defined Z_FRC_BRY || defined M3_FRC_BRY || defined M2NUDGING \
                                             || defined M3NUDGING
      real ubind
      common /scalars_main/ ubind
#endif


#if defined TCLIMATOLOGY || ( defined TNUDGING &&\
               defined T_FRC_BRY ) || defined SPONGE
      real tauM2_in, tauM2_out
      common /scalars_main/ tauM2_in, tauM2_out
# ifdef SOLVE3D
      real tauM3_in, tauM3_out,  tauT_in, tauT_out
      common /scalars_main/ tauM3_in,tauM3_out, tauT_in,tauT_out
# endif

! MF: define nudging tracers
# if defined TNUDGE_WEIGHTS || defined AMAZON
      integer, parameter :: mxnudg=itemp+ntrc_salt  ! only nudge temp and salts
# else
      integer, parameter :: mxnudg=NT !nudge all tracers including  BIOLOGY variables
# endif
#endif

!DL: variables for varying atm pCO2:
#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2 || defined BIOLOGY_NPZDOC
# ifdef VARIABLE_ANN_ATM_PCO2
      real start_year
      character(len=4) futr_scen
      common /scalars_var_atm_co2/ start_year, futr_scen
# endif
#endif

!DL: gas exchange fluxes:
# ifdef BIOLOGY_NPZDOC
      integer NumGasExcTerms
#  ifdef OXYGEN
      integer OFlux_GasExc
      parameter(OFlux_GasExc = 1)
#   ifdef CARBON
      integer CFlux_GasExc
      parameter(CFlux_GasExc = 2)
      parameter(NumGasExcTerms = 2)
#   else /* CARBON */
      parameter(NumGasExcTerms = 1)
#   endif /* CARBON */
#  else /* OXYGEN */
      parameter(NumGasExcTerms = 0)
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

      integer ntstart, ntimes, ndtfast, nfast, ninfo, may_day_flag,
     &                                                barr_count(16)
      common /scalars_main/ ntstart, ntimes, ndtfast, nfast, ninfo,
     &                               may_day_flag,    barr_count
#ifdef EXACT_RESTART
      integer forw_start
      common /scalars_main/ forw_start
#endif

! Physical constants:  Earth radius [m]; Aceleration of gravity
!--------- ----------  duration of the day in seconds; Specific
! heat [Joules/kg/degC] for seawater (it is approximately 4000,
! and varies only slightly, see Gill, 1982, Appendix 3);  von
! Karman constant.

      real, parameter :: pi=3.14159265358979323, Eradius=6371315.,
     &              deg2rad=pi/180., rad2deg=180./pi, day2sec=86400.,
     &                   sec2day=1./86400., Cp=3985., vonKar=0.41
#ifdef SOLITON
     &                 , g=1.   ! nondimensional
#else
     &                 , g=9.81 ! m/s^2
#endif
#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
      real nmol_cm2_to_mmol_m2
      parameter (nmol_cm2_to_mmol_m2 = 0.01)
#endif

! Flag indicating whether the Netcdf input files are assumed to be
! partitioned:
! true: files are assumed to be partitioned if MPI and PARALLEL_FILES
!     are both activated, otherwise unpartitioned
! false: all input file are assumed to be unpartitioned
      logical, parameter ::  input_partitioned
#ifdef JOINED_INPUT
     &         = .false.
#else
     &         = .true.
#endif
