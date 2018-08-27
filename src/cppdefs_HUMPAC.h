! UP ETH Southern Ocean Setup 
! == === ======== ===== =====


                     /* Include standard UP ETH Zurich switches */
                     /* *** ACTIVATE BIOLOGY HERE *** */
                     /* (Otherwise standard UP bio switches are not set) */
#define BIOLOGY_BEC2
#include "cppdefs_UP.h"


!-- #define SO_AH  /* Config. Alex (SO_AH) or Cara (SO_CN) */


                     /* Resolution */
#define HUMPAC15 /* HUMPAC30 HUMPAC15 */

#ifdef HUMPAC30        /*                    Humboldt telescopic 8-80km setup */
# define GRID_SIZE   LLm=349, MMm=504, N=42    
# define DOMAIN_TILING NP_XI=12, NP_ETA=16, NSUB_X=1, NSUB_E=1
#endif
#ifdef HUMPAC15        /*                    Humboldt telescopic 4-40km setup */
# define GRID_SIZE    LLm=699, MMm=1007, N=42
# define DOMAIN_TILING NP_XI=8, NP_ETA=48, NSUB_X=1, NSUB_E=1
#endif

/* NOTE: for more or less cores increase NP_ETA and change M2SPECIFIED tile range below */



                     /*  Forcing */
                     /*          - surface */
!-- #define SALINITY_MASK
!-- #define SALINITY_MASKLATSTR -48.0
!-- #define SALINITY_MASKLATEND -53.01

!-- #define ICEOBS
#define VFLX_CORR
                     /*          - lateral */
#define DEFAULT_BRY_VALUES
!-- #define TSOURCE


                     /* Restart */
#define EXACT_RESTART


                     /* Vertical Mixing */
#define LMD_DDMIX
#define LMD_BKPP
# undef LMD_CONVEC

#ifdef SO_AH
# undef LMD_CONVEC
# define LMD_MIN_KPP
# define BRINE_PLUMES
# define LMD_LIMIT_STABLE
#endif


                      /* Open Boundary Conditions */
#define OBC_WEST
#ifdef SO_d05        /*                     - 1/2 degree setup (SO_d05) */
# define OBC_NORTH_M2SPECIFIED_TILESTR 280 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND 285 /* OBC_M2SPECIFIED for a certain range of tiles */
#endif
#ifdef SO_d025       /*                     - 1/4 degree setup (SO_d025) */
# define OBC_NORTH_M2SPECIFIED_TILESTR 560 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND 571 /* OBC_M2SPECIFIED for a certain range of tiles */
#endif


                      /* Output */
#define MASK_LAND_DATA
#define AVERAGES
#define SLICE_AVG

#ifdef SO_AH
# define WRITE_HEATFLX
# define WRITE_SALT_REST
# define WRITE_TEMP_REST
!-- # define WRITE_CO2FLX
!-- # define KPP_DIAGNOSE
#endif


                      /* Flux Analysis */
!-- #define PHYS_FLUX_ANALYSIS
!-- #define FULL_PHYS_FLUX_ANALYSIS
!-- #define VERT_DIFF_ANALYSIS
!-- #define SELECTED_FLUX_ANALYSIS


                      /* Biology (SO specific) */
#ifdef BIOLOGY_BEC2
# define BEC2_DIAG
!-- # define BEC2_DIAG_USER
!-- #  define BEC_COCCO
# define KILL_THE_WINNER
# define LIMIT_MAX_SST
#endif
!-- #define RIVER_LOAD_N
!-- #define RIVER_LOAD_P

# define PCO2AIR_FORCING

                     /*Time and Calendar*/
# define USE_REAL_YEAR
# define STARTDATE '1979-01-01'


#include "set_global_definitions.h"
!
!  PARALLEL_FILES is set in set_global_definitions.h
!  Switch it off, if not desired
!-- #undef PARALLEL_FILES

