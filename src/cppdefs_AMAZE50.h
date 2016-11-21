/*
  UP ETH Amazon Ocean Setup
  == === ====== ===== =====
  -- comments marked by SO are optins used in the Southern Ocean setup --
*/
     /* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"

     /* 1st  grid used  for amazon*/
#define GRID_SIZE LLm=367, MMm=148, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=4, NSUB_X=1, NSUB_E=1

     /* Forcing */

!--#define SALINITY_MASK
!--SO # define SALINITY_MASKLATSTR -48.0
!--SO # define SALINITY_MASKLATEND -53.01

#define TSOURCE
!--SO #define ICEOBS

     /* Open Boundaries */
!--SO #define EW_PERIODIC
#define OBC_NORTH
!--#define OBC_EAST

     /* Open Boundary Conditions */
#define OBC_M2FLATHER
!--# define OBC_NORTH_M2SPECIFIED_TILESTR 1 /* OBC_M2SPECIFIED for a certain range of tiles */
!--# define OBC_NORTH_M2SPECIFIED_TILEEND 367 /* OBC_M2SPECIFIED for a certain ran
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */

     /* Vertical Mixing */
!-- #define LMD_LIMIT_STABLE / KPP fix for shallow mixing layer
#define LMD_DDMIX
#define LMD_BKPP
  
    /* Output */
#define AVERAGES
#define SLICE_AVG
!--SO #define WRITE_HEATFLX
# define WRITE_SALT_REST
!--SO #define WRITE_TEMP_REST
!--SO #define WRITE_CO2FLX
!--#define FULL_PHYS_FLUX_ANALYSIS

     /* Biology */
!--#define BIOLOGY_BEC2
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

