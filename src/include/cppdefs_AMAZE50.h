/*
  UP ETH Amazon Ocean Setup
  == === ====== ===== =====
  -- comments marked by SO are optins used in the Southern Ocean setup --
*/

!--#define BIOLOGY_BEC2

/* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"


     /* Resolution*/

#define FULLGRID /*FULLGRID CLIPPED140 CLIPPED210 */

#ifdef CLIPPED140
#define GRID_SIZE LLm=345, MMm=138, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1
#endif
#ifdef CLIPPED210
#define GRID_SIZE LLm=345, MMm=208, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1
#endif
#ifdef FULLGRID
#define GRID_SIZE LLm=345, MMm=331, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif

     /* Forcing */
                     /*          - surface */
#define SALINITY_MASK
!--SO # define SALINITY_MASKLATSTR -48.0
!--SO # define SALINITY_MASKLATEND -53.01
#define VFLX_CORR
!-- # define RIVER_LOAD_N
!-- # define RIVER_LOAD_P
!-- # define PCO2AIR_FORCING
                     /*          - lateral */
!--#define TSOURCE
#define DEFAULT_BRY_VALUES

     /* Open Boundaries */
!--SO #define EW_PERIODIC
#define OBC_NORTH
#define OBC_EAST
#define OBC_SOUTH

     /* Open Boundary Conditions */
#define OBC_S_M2SPEC_STR 1
#define OBC_S_M2SPEC_END 12
#define OBC_E_M2SPEC_STR 1
#define OBC_E_M2SPEC_END 10

     /* Vertical Mixing */
!--#define LMD_LIMIT_STABLE / KPP fix for shallow mixing layer
#define LMD_DDMIX
#define LMD_BKPP

     /* Biology */
#ifdef BIOLOGY_BEC2
!--# define BEC2_DIAG
# define BEC_DDA
!-- # define KILL_THE_WINNER  /* parametrization for grazing, not working for the moment */
#endif

     /* Other tracers */
!-- #define PASSIVE_TRACER
!-- #define AGE_DYE_TRACER

     /* Output */
#define AVERAGES
!--#define SLICE_AVG
!--SO #define WRITE_HEATFLX
#define WRITE_SALT_REST
!--#define WRITE_TEMP_REST
!--SO #define WRITE_CO2FLX

     /* Diagnostics */
!--#define PHYS_FLUX_ANALYSIS
!--# define KPP_DIAGNOSE
!--SO #define COMPUTE_SPEED_DIAGNOSE

     /*Tides*/
!--#define UV_TIDES
!--#define SSH_TIDES

!-- #define MMDEBUG


#include "set_global_definitions.h"
!
!  PARALLEL_FILES is set in set_global_definitions.h
!  Switch it off, if not desired
!--#undef PARALLEL_FILES
