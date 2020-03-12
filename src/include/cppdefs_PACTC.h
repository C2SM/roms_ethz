/*
  UP ETH Pacific Telescopic Setup - USWC Focus
  == === ======= ========== ===== = ==== =====
*/

                     /* Include standard CPP switches for UP ETH Zurich */
                     /* *** ACTIVATE BIOLOGY HERE *** */
                     /* (Otherwise standard UP bio switches are not set) */

# define BIOLOGY_BEC2
#include "cppdefs_UP.h"

! CHOOSE SETUP HERE
#define PACTCS60

#ifdef PACTCS60
# define GRID_SIZE LLm=300, MMm=257, N=64      ! pactcs60 8-120km telescopic up to Antarctica
# define DOMAIN_TILING NP_XI=6, NP_ETA=24, NSUB_X=1, NSUB_E=1 ! Mobilis - EL2020
! # define DOMAIN_TILING NP_XI=8, NP_ETA=18, NSUB_X=1, NSUB_E=1 ! Euler
#else 
! default: PACTCS30 
# define PACTCS30
# define GRID_SIZE LLm=602, MMm=516, N=64      ! pactcs30 4.1-65km telescopic up to Antarctica
# define DOMAIN_TILING NP_XI=8, NP_ETA=48, NSUB_X=1, NSUB_E=1 ! Euler
#endif

!mf #define DOMAIN_TILING NP_XI=8, NP_ETA=30, NSUB_X=1, NSUB_E=1 ! Euler

     /* Open Boundaries */
#define OBC_SOUTH

     /* Open Boundary Conditions */
!-- Try to treat inflow/outflow of ACC differently! (not yet tested, so far I used M2FLATHER)
!-- new only indic/pacific, see below #define OBC_M2SPECIFIED /* special for SO */
#define OBC_SOUTH_M2SPECIFIED_TILESTR 000 /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_SOUTH_M2SPECIFIED_TILEEND 004 /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */


     /* Open Boundary Conditions */
!-- #define CLMFORCING


    /*  Use CLM Files */
!-- #ifdef CLMFORCING
!# define TCLIMATOLOGY
/* Partial restoring of TS in user-defined region via 2D field nudg_weights in clm file (MF)*/
!# define TNUDGE_WEIGHTS /* TCLIMATOLOGY must be defined */
!--> # define UCLIMATOLOGY
!--> # define TNUDGING
!--> # define M3NUDGING
!--> # define M2NUDGING
# ifdef TCLIMATOLOGY
#      define CLIMAT_TS_MIXH
# endif
!--   /* undefine default BRY settings */
!# undef T_FRC_BRY
!# undef Z_FRC_BRY
!# undef M3_FRC_BRY
!# undef M2_FRC_BRY
!--> #else                /* Default: Use BRY Files*/
!--> # undef SPONGE /* note: UP ETH before 8/2015 did not use SPONGE with BRY */
!-- #endif

    /* Bottom KPP */
#define LMD_BKPP


!--> #undef OBC_M2FLATHER  /* undefine default in cppdefs_UP.h */
!--> #define OBC_M2SPECIFIED

     /* Seaice SSS masking and read seaice conc */
!--> # define SALINITY_MASK
!# define ICEOBS

     /* Output */
#define AVERAGES
!#define SLICE_AVG
#ifdef PACTCS30
# define CALENDAR '365_day'     /* netCDF CF-convention CALENDAR attribute default: '360_day' */
#endif
!--> #define STARTDATE '0001-01-01' /* part of netCDF CF-convention time units attribute default: '0001-01-01'*/

#define ADV_ISONEUTRAL

     /* Biology */
!--> #define BIOLOGY_NPZDOC

#ifdef BIOLOGY_BEC2
#  define DAILYPAR_BEC
#  define USE_EXPLICIT_VSINK
#  define DEFAULT_BRY_VALUES
!--> # define MULT_CLIM_FILES
!# define BUDGETVARS
# define VFLX_CORR /* MF: make sure this is always on if running with BIOLOGY */
# define BEC2_DIAG
# define RIVER_LOAD_ALK_DIC_SI
# define RIVER_LOAD_N
# define RIVER_LOAD_P
# define PCO2AIR_FORCING
# define NHY_FORCING
# define NOX_FORCING
#endif /* BIOLOGY_BEC2 */


    /* Flux Analysis */
!# define BGC_FLUX_ANALYSIS
!!#define BGC_FLUX_EXT_HIS_OUTPUT
!# define PHYS_FLUX_ANALYSIS
!!# define FULL_PHYS_FLUX_ANALYSIS
!# define VERT_DIFF_ANALYSIS
!# define SELECTED_FLUX_ANALYSIS
!# define WRITE_DEPTHS /* For Budget Analysis Closure */


!#define USE_REAL_YEAR /* Only used by age tracers*/
!--> #define VERBOSE
!#define HIS_DOUBLE
!--> #define DEBUG

#include "set_global_definitions.h"

!--> #undef PARALLEL_FILES
