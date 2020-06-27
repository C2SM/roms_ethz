/*
  UP ETH Amazon Ocean Setup
  == === ====== ===== =====
  -- comments marked by SO are optins used in the Southern Ocean setup --
*/

#define BIOLOGY_BEC2
!--#define EXACT_RESTART

/* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"


     /* Resolution*/

#define AMC576

#ifdef CLIPPED140
#define GRID_SIZE LLm=345, MMm=138, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1
#endif
#ifdef CLIPPED220
#define GRID_SIZE LLm=345, MMm=218, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1
#endif
#ifdef FULLGRID
#define GRID_SIZE LLm=345, MMm=331, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif
#ifdef AMZ50GRID
#define GRID_SIZE LLm=431, MMm=333, N=32
#define  DOMAIN_TILING NP_XI=12, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif
#ifdef AMC30GRID
#define GRID_SIZE LLm=982, MMm=808, N=32
#define  DOMAIN_TILING NP_XI=12, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif
#ifdef AMC50GRID
#define GRID_SIZE LLm=574, MMm=448, N=32
#define  DOMAIN_TILING NP_XI=12, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif
#ifdef AMC576
#define GRID_SIZE LLm=574, MMm=448, N=42
#define  DOMAIN_TILING NP_XI=16, NP_ETA=36, NSUB_X=1, NSUB_E=1
#endif
#ifdef AMC50SG
#define GRID_SIZE LLm=558, MMm=469, N=32
#define  DOMAIN_TILING NP_XI=12, NP_ETA=24, NSUB_X=1, NSUB_E=1
#endif



     /* Forcing */
                     /*          - surface */
#define SALINITY_MASK
!--SO # define SALINITY_MASKLATSTR -48.0
!--SO # define SALINITY_MASKLATEND -53.01
#define VFLX_CORR
!-- # define PCO2AIR_FORCING
                     /*          - lateral */
!--#define TSOURCE
#define DEFAULT_BRY_VALUES
#define TCLIMATOLOGY
#define UCLIMATOLOGY
#define SPONGE_WIDTH
#define AMAZON     

     /* Open Boundaries */
!--SO #define EW_PERIODIC
!--#define OBC_NORTH
#define OBC_EAST
#define OBC_SOUTH

     /* Open Boundary Conditions */
#define OBC_S_M2SPEC_STR 1
#define OBC_S_M2SPEC_END 70
#define OBC_E_M2SPEC_STR (NP_ETA*(NP_XI-1.))
#define OBC_E_M2SPEC_END ((NP_ETA*(NP_XI-1.))+(NP_ETA*0.6)-1.)

     /* Vertical Mixing */
!--#define LMD_LIMIT_STABLE / KPP fix for shallow mixing layer
!--#define LMD_DDMIX
#define LMD_BKPP

     /* Biology */
#ifdef BIOLOGY_BEC2
# define BEC2_DIAG
# define BEC_DDA
# define BEC_DDA_case2
!# define BEC_DDA_case1
# define AMAZON_PAR
# define RIVER_LOAD_N
# define RIVER_LOAD_P
# define RIVER_LOAD_BIO
!-- # define USE_EXPLICIT_VSINK
!-- # define KILL_THE_WINNER  /* parametrization for grazing, not working for the moment */
#endif
#define NOX_FORCING 
#define NHY_FORCING
#define POX_FORCING

     /* Other tracers */
!#define PASSIVE_TRACER
!#define AGE_DYE_TRACER

     /* Output */
#define AVERAGES
!--#define SLICE_AVG
!--SO #define WRITE_HEATFLX
!--#define WRITE_SALT_REST
!--#define WRITE_TEMP_REST
!--SO #define WRITE_CO2FLX

     /* Diagnostics */
!#define PHYS_FLUX_ANALYSIS
!#define FULL_PHYS_FLUX_ANALYSIS
!#define VERT_DIFF_ANALYSIS
!#define TOP_FLUX_ANALYSIS
!#define WRITE_DEPTHS
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
#define COUP_OAS
