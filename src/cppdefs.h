/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#undef BASIN           /* Big Bad Basin Example */
#undef CANYON_A        /* Canyon_A Example */
#undef CANYON_B        /* Canyon_B Example */
#undef DAMEE_B         /* North Atlantic DAMEE, Big Domain */
#undef GRAV_ADJ        /* Graviational Adjustment Example */
#undef NJ_BIGHT        /* New Jersey Bight Application */
#undef PACIFIC         /* North-Equatorial Pacific Application */
#undef PACIFIC_2D     /* Pacific Tsunami model */
#undef OVERFLOW        /* Graviational/Overflow Example */
#undef SEAMOUNT        /* Seamount Example */
#undef ISWAKE         /* Island Wake Example */
#undef SOLITON         /* Equatorial Rossby Wave Example */
#undef RIVER          /* River runoff test problem */
#undef UPWELLING       /* Upwelling Example */
#undef USWEST          /* US West Coast Application */
#define USWC_CENTRAL
#undef WAVE_RAD        /* A test for wave radiation boundaries */
#undef ATL360X408    /* Whole Atlantic 360x408 */
!--> #define ATL50S70N    /* Whole Atlantic 50S x 70N (360x468) */
# undef ONE_DIM /*ONE DIM CONFIGURATION (JDS)*/

/*
    Embedded (nested) grid configuration segment
*/

c--#ifndef MAX_GRID_LEVEL
c--# define MAX_GRID_LEVEL 2
c--# include "redefs.X"
c--#endif


/*
   Main switch starts here: model configuration choice.
*/

#if defined ATL50S70N || defined ATL360X408
# define ATLANTIC
#endif

#if defined BASIN    /* Big Bad Basin Configuration */
# define SOLVE3D
 
# define UV_ADV
# define UV_COR
# define MIX_GP_UV
 
# undef  SALINITY
# undef  NONLIN_EOS
 
# undef  TS_DIF2
# undef  TS_DIF4
# define MIX_GP_TS
 
/*  define BODYFORCE */
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# undef  ANA_VMIX
 
#elif defined CANYON_A      /* Canyon A Configuration */
# define SOLVE3D
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define MIX_GP_UV
# define TS_DIF2
# define MIX_GP_TS
# define EW_PERIODIC
/*  define BODYFORCE */
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
 
#elif defined CANYON_B      /* Canyon B Configuration */
# define SOLVE3D
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define MIX_GP_UV
# define TS_DIF2
# define MIX_GP_TS
# define EW_PERIODIC
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_VMIX
 
#elif defined DAMEE_B                      /* North Atlantic     */
# define SOLVE3D                            /* DAMEE configuration */
c--# define AVERAGES
# define UV_COR
# define UV_ADV
!                       Equation of State
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
!                       Forcing and Climatology
# define TCLIMATOLOGY
# define TNUDGING
# define QCORRECTION
# define SFLX_CORR
!                       Lateral Mixing
# define VIS_GRID
# define MIX_GP_UV  /* <-- changed: was previously undef */
# define DIF_GRID
# define MIX_GP_TS
# undef MIX_EN_TS
!                       Vertical Mixing
# define LMD_MIXING
#  define LMD_RIMIX
#  define LMD_CONVEC
c--#  define LMD_DDMIX
 
c--#  define LMD_KPP
c--#  define LMD_NONLOCAL
!
!       Grid Configuration and Boundaries
!
# define CURVGRID
# define SPHERICAL
# define MASKING
# define EASTERN_WALL
# define WESTERN_WALL
# define SOUTHERN_WALL
# define NORTHERN_WALL
 
 
c--# define REST_STATE_TEST    /* Rest-state unforced problem */
# ifdef REST_STATE_TEST     /* (pressure gradient error test) */
#  define ANA_INITIAL
#  define NONLIN_EOS
#  undef SPLIT_EOS
#  define SALINITY
#  define ANA_SMFLUX
#  define ANA_SSFLUX
#  define ANA_STFLUX
#  define ANA_SST
#  define ANA_SRFLUX
#  undef TCLIMATOLOGY
#  undef TNUDGING
#  undef QCORRECTION
#  undef LMD_MIXING
#  define UV_VIS2
#  define MIX_S_UV
#  undef MIX_GP_UV
# endif
!
#elif defined GRAV_ADJ     /* Gravitational Adjustment */
# define SOLVE3D
 
# define UV_ADV
# undef UV_COR
# define UV_VIS2
# define MIX_GP_UV
 
# define TS_DIF2
# define MIX_GP_TS
 
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX

c--# define OBC_WEST
c--# define OBC_EAST 
c--# define OBC_M2ORLANSKI
c--# define OBC_M3ORLANSKI
c--# define OBC_TORLANSKI
!
#elif defined NJ_BIGHT        /* New Jersey Bight Configuration */
# define SOLVE3D
 
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define MIX_GP_UV
 
# define SALINITY
 
# define TS_DIF2
 
# define CURVGRID
# define SPHERICAL
 
# define STATIONS
# define OBC_EAST
# define OBC_NORTH
# define OBC_SOUTH
# define OBC_FSORLANSKI
# define OBC_M2ORLANSKI
# define OBC_M3ORLANSKI
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_KPP
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_SRFLUX
# define ANA_SSFLUX
# define ANA_STFLUX


#elif defined PACIFIC || defined ATLANTIC /* North-Equatorial Pacific Configuration */
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define CURVGRID
# define SPHERICAL
# define MASKING
# define MASK_LAND_DATA

# define EXACT_RESTART
# define AVERAGES
!                       Equation of State
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS

# define QCORRECTION
# define SFLX_CORR
# define SSS_dQdSST   ! dQdSSSt must be read together with
                      ! SSS or not (in this case with SST)

!                       Lateral Mixing
# define UV_VIS2
# define MIX_GP_TS
# undef VIS_GRID

# define TS_DIF2
# define MIX_GP_UV
# undef DIF_GRID
!                       Vertical Mixing
# define LMD_MIXING
# define LMD_KPP
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_NONLOCAL
c--# define DIURNAL_SRFLUX
# undef LMD_DDMIX
!                       Open Boundary Conditions
# define OBC_WEST
# define OBC_SOUTH
# define OBC_M2FLATHER
c--# define OBC_M2ORLANSKI
# define OBC_M3ORLANSKI
# define OBC_TORLANSKI
# define M2NUDGING
# define M3NUDGING

c>>># define TNUDGING
c>>># define TCLIMATOLOGY
c>>># define UCLIMATOLOGY

c--mm:
# ifdef ATLANTIC
#   define TNUDGING
#   define TCLIMATOLOGY
#   define UCLIMATOLOGY

#   define OBC_SOUTH
#   define OBC_EAST
#   define OBC_NORTH
#   undef OBC_WEST
!   mm new switch STFLX_LIM, (implicit with PACIFIC switch)
!   Shut off surface flux if SST<-2C  (ice formation)
#   define STFLX_LIM   
# endif

c--DL:
# ifdef PACIFIC
!# define Z_FRC_BRY
!# define M2_FRC_BRY
!# define M3_FRC_BRY 
!# define T_FRC_BRY
# define TNUDGING
# define TCLIMATOLOGY
# define BIO_1ST_USTREAM_TEST
# define DAILYPAR_BEC
# define DIURNAL_SRFLUX
# ifdef TCLIMATOLOGY
#   define CLIMAT_TS_MIXH
# endif
# define VERT_DIFF_ANALYSIS
# undef OBC_WEST
# undef OBC_SOUTH
# undef OBC_M2FLATHER
# undef OBC_M3ORLANSKI
# undef OBC_TORLANSKI
# undef M2NUDGING
# undef M3NUDGING
# undef LMD_NONLOCAL
# undef EXACT_RESTART
# undef NEW_S_COORD
!# define AN_UNDEFINED_CASE
! Switches related to biology:
!#define BIOLOGY_BEC
#define BIOLOGY_NPZDOC
#define FOUR_CLIM_FILES
#define BGC_FLUX_ANALYSIS
#define PHYS_FLUX_ANALYSIS
! Other switches:
#define VFLX_CORR
#endif

# define SPONGE


#elif defined PACIFIC_2D   /* Pacific Tsynami model */
# define UV_COR
# define UV_ADV
# define CURVGRID
# define SPHERICAL
# define MASKING
# undef VIS_GRID
# define UV_VIS2
# define ANA_SMFLUX
# define ANA_INITIAL

# define OBC_WEST
# define OBC_SOUTH
# define OBC_M2FLATHER
# define ANA_BRY
# define Z_FRC_BRY
# define M2_FRC_BRY
c--# define OBC_M2ORLANSKI
c--# define OBC_VOLCONS
# define SPONGE

#elif defined OVERFLOW      /* Gravitational Overflow */
# define SOLVE3D
 
# define UV_ADV
# define UV_VIS2
# define MIX_GP_UV
 
# define TS_DIF2
# define MIX_GP_TS
 
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
 
#elif defined SEAMOUNT     /* Seamount Configuration */
# define SOLVE3D
 
#define EXACT_RESTART

# define UV_ADV
# define UV_COR
 
# define UV_VIS2
# define MIX_S_UV
c--# define MIX_GP_UV
 
# undef TS_DIF2
# undef  TS_DIF4
c--#define MIX_GP_TS
 
c--# define  NONLIN_EOS
c--# define SALINITY
 
# define EW_PERIODIC
 
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX


#elif defined ISWAKE     /* Island Wake Configureation */
# define SOLVE3D
# define UV_ADV
# define UV_COR

c---# define UV_VIS2
# define MIX_GP_UV
c---# define SPONGE
# define LINEAR_DRAG_ONLY

# define OBC_WEST
# define OBC_EAST
c--# define OBC_SOUTH
c--# define OBC_NORTH

c--# define OBC_M2ORLANSKI
c--# define OBC_M3ORLANSKI
c--# define OBC_TORLANSKI
c--# define OBC_M2SPECIFIED
# define OBC_M3SPECIFIED
# define OBC_TSPECIFIED

#define OBC_M2FLATHER
 
#define EXACT_RESTART

# define ANA_BRY
# define T_FRC_BRY
# define Z_FRC_BRY
# define M2_FRC_BRY
# define M3_FRC_BRY

# undef AVERAGES

# define ANA_GRID
# define MASKING
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX

#define MASK_LAND_DATA

!
! Vertical Mixing: nothing defined here, just use externally
! supplied background value.
!
#elif defined SOLITON    /* Equatorial Rossby Soliton */
# undef  SOLVE3D
 
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
 
# define UV_COR
# define UV_ADV
# undef UV_VIS2
# define EW_PERIODIC
c--# define NS_PERIODIC

c--# define OBC_WEST
c--# define OBC_EAST
c--# define OBC_NORTH
c--# define OBC_SOUTH
c--# define OBC_M2ORLANSKI
 
#elif defined RIVER     /* River run-off test problem */
# define SOLVE3D
# define UV_ADV
# define UV_COR
# define MIX_GP_UV

# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY
# define MIX_GP_TS

# define ANA_GRID
# define MASKING
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX

# define PSOURCE
# define ANA_PSOURCE

# define EASTERN_WALL
# define WESTERN_WALL
# define NORTHERN_WALL
# define OBC_SOUTH
# define OBC_TORLANSKI
# define OBC_M2ORLANSKI
# define OBC_M3ORLANSKI
 
#elif defined UPWELLING     /* Upwelling Configuration */
# define SOLVE3D
# define UV_ADV
# define UV_COR
# define MIX_GP_UV
 
# undef NONLIN_EOS
# define SALINITY
# undef LMD_VMIX
 
# define MIX_GP_TS
 
# define EW_PERIODIC
 
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_VMIX
 
#elif defined USWC_CENTRAL   /* US West Coast Configuration */
# define SOLVE3D
# define UV_COR
# define UV_ADV
!                       Equation of State
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY

# define EXACT_RESTART

!                       Forcing and Climatology
# ifdef SOLVE3D
#   define QCORRECTION
#   define TCLIMATOLOGY
#   define UCLIMATOLOGY
#   define TNUDGING
#   define M3NUDGING
#   define M2NUDGING
#   define SPONGE
#   define SFLX_CORR
#   undef ROBUST_DIAG
 
#   undef  ANA_SSFLUX
#   undef  ANA_SST
#   undef  ANA_SRFLUX
#   undef  ANA_STFLUX
#   define DIURNAL_SRFLUX
# endif
                      /* Lateral Mixing */
# undef VIS_GRID
# define UV_VIS2
# define MIX_GP_TS
 
# ifdef SOLVE3D
#   undef DIF_GRID
#   define TS_DIF2
#   define MIX_GP_UV
#   undef TS_DIF4
#   ifdef TCLIMATOLOGY
#     define CLIMAT_TS_MIXH
#   endif
# endif
                      /* Vertical Mixing */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# undef LMD_DDMIX
# define LMD_KPP
# define LMD_NONLOCAL
                      /* Grid Configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
                      /* Open Boundary Conditions */
# undef OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
c--> # define OBC_VOLCONS
c--> # define OBC_FLUX_CORR
 
# define OBC_TORLANSKI
c--# define OBC_M2ORLANSKI
#define OBC_M2FLATHER
# define OBC_M3ORLANSKI
!
# undef OBC_TSPECIFIED
# undef OBC_M2SPECIFIED
# undef OBC_M3SPECIFIED
!
# define AVERAGES
!                       Biology
# undef BIOLOGY
!# define BIOLOGY_NPZDOC
# define BIOLOGY_BEC2
# define BEC2_DIAG
!!# define BIOLOGY_BEC
!!#define BGC_FLUX_ANALYSIS
!!#define PHYS_FLUX_ANALYSIS
!!#define VERT_DIFF_ANALYSIS
!!#define FULL_PHYS_FLUX_ANALYSIS
!!#define WRITE_DEPTHS
!# define SLICE_AVG
! Switch to calculate carbonsystem
!--> # define CH_CARBON_DEPTH
! Age/dye tracer:
!--> # define PASSIVE_TRACER
!--> # define AGE_DYE_TRACER

# define MULT_CLIM_FILES
# define NEW_S_COORD
# define DIURNAL_SRFLUX
# define VFLX_CORR

# undef FLOATS
# ifdef FLOATS
#   undef FLOATS_GLOBAL_ATTRIBUTES
#   undef RANDOM_WALK

# endif

#elif defined WAVE_RAD
# undef  SOLVE3D
# define UV_COR
# define UV_ADV
# undef UV_VIS2

# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX

# define OBC_WEST
# define OBC_EAST
# define OBC_NORTH
# define OBC_SOUTH
c--# define OBC_M2ORLANSKI
# define OBC_M2FLATHER
# define ANA_BRY
# define Z_FRC_BRY
# define M2_FRC_BRY

#endif

#ifdef ONE_DIM

# define SOLVE3D
# define UV_COR
# define UV_ADV
# define CURVGRID
# define SPHERICAL
# define MASKING
# define MASK_LAND_DATA

# define EXACT_RESTART
# define AVERAGES
!                       Equation of State
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS

# define QCORRECTION
# define SFLX_CORR
# define SSS_dQdSST   ! dQdSSSt must be read together with
                      ! SSS or not (in this case with SST)
	!                       Lateral Mixing
# define UV_VIS2
# define MIX_GP_TS
# define TS_DIF2
# define MIX_GP_UV
	!                       Vertical Mixing
# define LMD_MIXING
# define LMD_KPP
# define LMD_RIMIX
# define LMD_CONVEC
# define DIURNAL_SRFLUX

# define EW_PERIODIC
# define NS_PERIODIC
# define BIO_1ST_USTREAM_TEST
# define DAILYPAR_BEC
# define DIURNAL_SRFLUX
# define NEW_S_COORD
! Switches related to biology:
#define BIOLOGY_NPZDOC
!# define BIOLOGY_BEC
# define CH_CARBON_DEPTH
! Switches related to bottom restore layer
! For 1d configurations and when using bec
# define DBLEPREC
!#define ANA_VMIX
!#define FOUR_CLIM_FILES
!#define BGC_FLUX_ANALYSIS
#define PHYS_FLUX_ANALYSIS
!#define VERT_DIFF_ANALYSIS
!#define FULL_PHYS_FLUX_ANALYSIS
!#define PHYS_FLUX_ALL_COMPS
#define VFLX_CORR

#endif /*ONE_DIM*/
 
#include "set_global_definitions.h"
!#define BIOLOGY_NPZDOC
!#define BIOLOGY_BEC
!#define FOUR_CLIM_FILES
!#define BGC_FLUX_ANALYSIS

#ifdef BIOLOGY_NPZDOC
! HF:
#define VFLX_CORR
!#define TWO_CLIM_FILES
!#define THREE_CLIM_FILES
#define DAILYPAR_PHOTOINHIBITION
#define OXYGEN
#define OCMIP_OXYGENSAT
#define OCMIP_OXYGEN_SC
#define CARBON
#define OCMIP_CARBON
#define VARIABLE_ATM_PCO2
#define CALC_CARBON_ONCE
#define USE_PH_SAVED
#define SEDIMENT_BIOLOGY
!HF#define BGC_FLUX_ANALYSIS
!#define BGC_FLUX_EXT_HIS_OUTPUT
!MM #define PHYS_FLUX_ANALYSIS
!!#define VERT_DIFF_ANALYSIS
!#define FULL_PHYS_FLUX_ANALYSIS
#endif

#ifndef BIOLOGY_BEC
# undef BIO_1ST_USTREAM_TEST
# undef DAILYPAR_BEC
#endif

!DL:
#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
!# define BIO_1ST_USTREAM_TEST
# ifdef DIURNAL_SRFLUX
#  define DAILYPAR_BEC
# endif
#define VARIABLE_ATM_PCO2
#endif /* BIOLOGY_BEC || BIOLOGY_BEC2 */
