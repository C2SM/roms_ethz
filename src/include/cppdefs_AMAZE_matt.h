/*
  UP ETH Amazon Ocean Setup
  == === ====== ===== =====
  -- comments marked by SO are optins used in the Southern Ocean setup --
*/
     /* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"

     /* 1st  grid used  for amazon*/
#define GRID_SIZE LLm=367, MMm=338, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1

     /* Forcing */

#define SALINITY_MASK
!--SO # define SALINITY_MASKLATSTR -48.0
!--SO # define SALINITY_MASKLATEND -53.01

#define TSOURCE
!--SO #define ICEOBS

     /* Open Boundaries */
!--SO #define EW_PERIODIC
#define OBC_NORTH
#define OBC_EAST

     /* Open Boundary Conditions */
#define OBC_M2FLATHER
# define OBC_N_M2SPEC_STR 1 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_N_M2SPEC_END 367 /* OBC_M2SPECIFIED for a certain ran
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */

     /* Vertical Mixing */
#define LMD_LIMIT_STABLE
#define LMD_DDMIX
#define LMD_BKPP
!--SO #define SO_AH16
#ifdef SO_AH16
# undef LMD_CONVEC
# define LMD_MIN_KPP
# define BRINE_PLUMES
#endif

     /* Output */
#define AVERAGES
#define SLICE_AVG
!--SO #define WRITE_HEATFLX
!--SO #define WRITE_SALT_REST
!--SO #define WRITE_TEMP_REST
!--SO #define WRITE_CO2FLX

     /* Biology */
#define BIOLOGY_BEC2
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
