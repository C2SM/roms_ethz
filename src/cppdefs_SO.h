! UP ETH Southern Ocean Setup 
! == === ======== ===== =====


                     /* Include standard UP ETH Zurich switches */
                     /* *** ACTIVATE BIOLOGY HERE *** */
                     /* (Otherwise standard UP bio switches are not set) */
!-- #define BIOLOGY_BEC2
#include "cppdefs_UP.h"


                     /* Resolution */
#define SO_d025 /* SO_d05, SO_d025, SO_d0125, SO_d01 */

#define SALINITY_MASK
# define SALINITY_MASKLATSTR -48.0
# define SALINITY_MASKLATEND -53.01


                     /*  Dynamics */
!-- #define ADV_ISONEUTRAL


                     /*  Forcing */
                     /*          - surface */
#define SALINITY_MASK
#define SALINITY_MASKLATSTR -48.0
#define SALINITY_MASKLATEND -53.01
!-- #define SALINITY_MASKLATSTR -40.0
!-- #define SALINITY_MASKLATEND -45.01
#define ICEOBS
#define VFLX_CORR
                     /*          - lateral */
#define DEFAULT_BRY_VALUES
#define TSOURCE

                     /* Restart */
#define EXACT_RESTART

     /* Vertical Mixing */
#define LMD_LIMIT_STABLE
#define LMD_DDMIX
#define LMD_BKPP

#ifdef SO_AH
# undef LMD_CONVEC
# define LMD_MIN_KPP
# define BRINE_PLUMES
# define LMD_LIMIT_STABLE
!-- # define BRINE_PLUMES2
!-- # define LMD_NEWENTRAIN
!-- # define MLCONVEC
#endif

                      /* Open Boundary Conditions */
#define EW_PERIODIC
#define OBC_NORTH
#ifdef SO_d05        /*                     - 1/2 degree setup (SO_d05) */
# define OBC_NORTH_M2SPECIFIED_TILESTR 280 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND 285 /* OBC_M2SPECIFIED for a certain range of tiles */
#endif
#ifdef SO_d025       /*                     - 1/4 degree setup (SO_d025) */
# define OBC_NORTH_M2SPECIFIED_TILESTR 560 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND 571 /* OBC_M2SPECIFIED for a certain range of tiles */
#endif
#ifdef SO_d0125       /*                     - 1/8 degree setup (SO_d0125) */
# define OBC_NORTH_M2SPECIFIED_TILESTR XX /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND XX /* OBC_M2SPECIFIED for a certain range of tiles */
#endif
#ifdef SO_d01       /*                     - 1/10 degree setup (SO_d01) */
# define OBC_NORTH_M2SPECIFIED_TILESTR XX /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND XX /* OBC_M2SPECIFIED for a certain range of tiles */
#endif


                      /* Output */
#define MASK_LAND_DATA
#define AVERAGES
!-- #define SLICE_AVG
#ifdef SO_AH
# define WRITE_HEATFLX
# define WRITE_SALT_REST
# define WRITE_TEMP_REST
/* # define WRITE_CO2FLX */
/* # define KPP_DIAGNOSE */
#endif


                      /* Flux Analysis */
!-- #define PHYS_FLUX_ANALYSIS
!-- #define FULL_PHYS_FLUX_ANALYSIS
!-- #define VERT_DIFF_ANALYSIS
!-- #define SELECTED_FLUX_ANALYSIS


                      /* Biology (SO specific) */
#ifdef BIOLOGY_BEC2
# ifdef SO_CN
#  define BEC_COCCO
#  define KILL_THE_WINNER
!-- #  define BEC2_DIAG
!-- #  undef DAILYPAR_BEC
# endif
#endif
!-- #define RIVER_LOAD_N
!-- #define RIVER_LOAD_P
!-- # define PCO2AIR_FORCING
!-- # define VARIABLE_ATM_PCO2

                      /* Other tracers */
!-- #define PASSIVE_TRACER
!-- #define AGE_DYE_TRACER


#include "set_global_definitions.h"
!
!  PARALLEL_FILES is set in set_global_definitions.h
!  Switch it off, if not desired
!-- #undef PARALLEL_FILES

