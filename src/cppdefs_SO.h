/* 
  UP ETH Southern Ocean Setup 
  == === ======== ===== =====
*/
     /* Include standard CPP switches for UP ETH Zurich */                   
#include "cppdefs_UP.h"

     /* 1/2 degree setup (SO_d05) */
#define GRID_SIZE LLm=720, MMm=216, N=42   ! SO_d05 1/2 deg setup
!--#define GRID_SIZE LLm=720, MMm=216, N=64   ! SO_d05 1/2 deg setup
!-- 288 cpus: 
!-- #define  DOMAIN_TILING NP_XI=8, NP_ETA=36, NSUB_X=1, NSUB_E=1
!-- 24 cpus: 
#define  DOMAIN_TILING NP_XI=4, NP_ETA=6, NSUB_X=1, NSUB_E=1

    /* 1/4 degree setup (SO_d025) */
!-- #define GRID_SIZE LLm=1440, MMm=416, N=42      ! SO_d025 1/4  deg resolution
!-- #define  DOMAIN_TILING NP_XI=16, NP_ETA=24, NSUB_X=1, NSUB_E=1

    /* 1/8 degree setup (SO_d0125) */
!-- #define GRID_SIZE LLm=2880, MMm=864, N=42      ! SO_d0125 1/8   deg resolution
!-- #define  DOMAIN_TILING NP_XI=32, NP_ETA=12, NSUB_X=1, NSUB_E=1
     /* Forcing */

#define SALINITY_MASK
# define SALINITY_MASKLATSTR -48.0
# define SALINITY_MASKLATEND -53.01

#define TSOURCE
#define ICEOBS

     /* Open Boundaries */
#define EW_PERIODIC
#define OBC_NORTH

     /* Open Boundary Conditions */
#define OBC_M2FLATHER  
!-- new only indic/pacific, see below #define OBC_M2SPECIFIED /* special for SO */
#define OBC_NORTH_M2SPECIFIED_TILESTR 280 /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_NORTH_M2SPECIFIED_TILEEND 285 /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */

     /* Vertical Mixing */
#define LMD_LIMIT_STABLE
#define LMD_DDMIX
#define LMD_BKPP
!-- #define SO_AH16
#ifdef SO_AH16
# undef LMD_CONVEC
# define LMD_MIN_KPP
# define BRINE_PLUMES
#endif


     /* Output */
#define AVERAGES
#define SLICE_AVG

     /* Biology */
#define BIOLOGY_BEC2
!-- #define VERBOSE
#ifdef BIOLOGY_BEC2
# define BEC_COCCO
# define KILL_THE_WINNER  /* if defined, use Vallina 2014 parametrization for grazing */
# define BIOLOGY
#endif

!-- #define PASSIVE_TRACER
!-- #define AGE_DYE_TRACER
#define DEFAULT_BRY_VALUES

!-- # define BEC2_DIAG
!-- # define RIVER_LOAD_N
!-- # define RIVER_LOAD_P
!-- # define PCO2AIR_FORCING
#define VFLX_CORR


#include "set_global_definitions.h"
