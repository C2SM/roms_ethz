! UP ETH Southern Ocean Setup
! == === ======== ===== =====


                     /* Include standard UP ETH Zurich switches */
                     /* *** ACTIVATE BIOLOGY HERE *** */
                     /* (Otherwise standard UP bio switches are not set) */
#define BIOLOGY_BEC2
#include "cppdefs_UP.h"

#define SOUTHERNOCEAN
!-- #define SO_CN  /* Config. Cara (SO_CN) */
#define SO_AH  /* Config. Alex (SO_AH)  */


                     /* Resolution */
#define SO_d025 /* SO_d05, SO_d025, SO_d0125, SO_d01 */

#ifdef SO_d05        /*                     - 1/2 degree setup (SO_d05) */
# define GRID_SIZE LLm=720, MMm=216, N=64
# define DOMAIN_TILING NP_XI=8, NP_ETA=36, NSUB_X=1, NSUB_E=1
#endif
#ifdef SO_d025       /*                     - 1/4 degree setup (SO_d025) */
# define GRID_SIZE LLm=1440, MMm=432, N=64
# define DOMAIN_TILING NP_XI=16, NP_ETA=36, NSUB_X=1, NSUB_E=1
#endif
#ifdef SO_d0125      /*                     - 1/8 degree setup (SO_d0125) */
#define GRID_SIZE LLm=2880, MMm=864, N=64
#define DOMAIN_TILING NP_XI=32, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif
#ifdef SO_d01        /*                     - 1/10 degree setup (SO_d01) */
#define GRID_SIZE LLm=3600, MMm=1080, N=64
#define DOMAIN_TILING NP_XI=40, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif


                     /*  Dynamics */
#define ADV_ISONEUTRAL
#define ADV_WENO

                     /*  Forcing */
                     /*          - surface */
#define SALINITY_MASK
#ifdef SO_AH
!-- #define SALINITY_MASKLATSTR -48.0
!-- #define SALINITY_MASKLATEND -53.01
!-- #define SALINITY_MASKLATSTR -40.0
!-- #define SALINITY_MASKLATEND -45.01
#endif
#define ICEOBS
#define VFLX_CORR
                     /*          - lateral */
#define DEFAULT_BRY_VALUES
#define TSOURCE


                     /* Restart */
#define EXACT_RESTART


                     /* Vertical Mixing */
#define SO_KPP     /* note that this option sets important KPP parameters */
#define LMD_DDMIX
#define LMD_BKPP
#ifdef SO_KPP
# undef LMD_CONVEC
# define LMD_MIN_KPP
# define BRINE_PLUMES
# define LMD_LIMIT_STABLE
!-- # define LMD_NEWENTRAIN
#endif


                      /* Open Boundary Conditions */
#define EW_PERIODIC
#define OBC_NORTH
#define OBC_N_M2SPEC_STR (NP_XI*(NP_ETA-1.)) /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_N_M2SPEC_END ((NP_XI*(NP_ETA-1.))+(NP_XI*0.75)-1.) /* OBC_M2SPECIFIED for a certain range of tiles */
#undef SPONGE_WIDTH  /* calculated in set_nudgcof.F */


                      /* Output */
#define AVERAGES
!-- #define SLICE_AVG
#ifdef SO_AH
# define WRITE_HEATFLX
# define WRITE_SALT_REST
# define WRITE_TEMP_REST
# ifdef BIOLOGY_BEC2
#  define WRITE_CO2FLX
# endif
!-- # define KPP_DIAGNOSE
!-- # define COMPUTE_SPEED_DIAGNOSE
#endif


                      /* Flux Analysis */
#ifdef SO_AH
!-- # define PHYS_FLUX_ANALYSIS
!-- # define FULL_PHYS_FLUX_ANALYSIS
!-- # define VERT_DIFF_ANALYSIS
!-- # define SELECTED_FLUX_ANALYSIS
#endif


                      /* Biology (SO specific) */
#ifdef BIOLOGY_BEC2
#  define BIOPAR_R
#  define BEC_COCCO
#  define KILL_THE_WINNER
#  define BEC2_DIAG
#  define PCO2AIR_FORCING
#  define VFLX_CORR
#  define DEFAULT_BRY_VALUES
#  undef DAILYPAR_BEC /* on by default */
# endif

!-- #define RIVER_LOAD_N
!-- #define RIVER_LOAD_P
!-- # define PCO2AIR_FORCING
!-- # define VARIABLE_ATM_PCO2


                      /* Other tracers */
#ifdef SO_AH
!--  # define PASSIVE_TRACER
! -- # define AGE_DYE_TRACER
#endif

#include "set_global_definitions.h"
!
!  PARALLEL_FILES is set in set_global_definitions.h
!  Switch it off, if not desired
!-- #undef PARALLEL_FILES
