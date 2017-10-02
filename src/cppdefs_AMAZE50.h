/*
  UP ETH Amazon Ocean Setup
  == === ====== ===== =====
  -- comments marked by SO are optins used in the Southern Ocean setup --
*/
     /* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"

     /* 1st  grid used  for amazon*/
!--#define GRID_SIZE LLm=367, MMm=148, N=32
!--#define  DOMAIN_TILING NP_XI=4, NP_ETA=8, NSUB_X=1, NSUB_E=1

#define GRID_SIZE LLm=345, MMm=331, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1

     /* Forcing */

!--#define SALINITY_MASK
!--SO # define SALINITY_MASKLATSTR -48.0
!--SO # define SALINITY_MASKLATEND -53.01

!--#define TSOURCE
!--SO #define ICEOBS

     /* Open Boundaries */
!--SO #define EW_PERIODIC
#define OBC_NORTH
#define OBC_EAST
#define OBC_SOUTH

     /* Open Boundary Conditions */


     /* Vertical Mixing */
!-- #define LMD_LIMIT_STABLE / KPP fix for shallow mixing layer
#define LMD_DDMIX
#define LMD_BKPP
  
    /* Output */
#define AVERAGES
!--#define SLICE_AVG
!--SO #define WRITE_HEATFLX
#define WRITE_SALT_REST
!#define WRITE_TEMP_REST
!--SO #define WRITE_CO2FLX
!--#define PHYS_FLUX_ANALYSIS

     /* Biology */
#define BIOLOGY_BEC2
#ifdef BIOLOGY_BEC2
# define BEC_DDA
# undef BEC_COCCO
# define KILL_THE_WINNER  /* if defined, use Vallina 2014 parametrization for grazing */
# define BIOLOGY
#endif

!--#define PASSIVE_TRACER
!-- #define AGE_DYE_TRACER
#define DEFAULT_BRY_VALUES

# define BEC2_DIAG
!-- # define RIVER_LOAD_N
!-- # define RIVER_LOAD_P
!-- # define PCO2AIR_FORCING
#define VFLX_CORR

     /*Tides*/
!--#define UV_TIDES
!--#define SSH_TIDES

!-- #define MMDEBUG

#include "set_global_definitions.h"

