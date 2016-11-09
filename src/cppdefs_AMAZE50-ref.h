/* 
  UP ETH Amazon Setup 
  == === ======== ===== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */                   
#include "cppdefs_UP.h"
#define GRID_SIZE LLm=367, MMm=148, N=32
#define DOMAIN_TILING  NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1



         /* Open Boundary Conditions */
#define OBC_NORTH_M2SPECIFIED_TILESTR
#define OBC_NORTH_M2SPECIFIED_TILESTR2
#define OBC_M3ORLANSKI
#define OBC_TORLANSKI

         /* Forcing */
#define SALINITY_MASK

         /* Open Boundaries */
!#define EW_PERIODIC
#define OBC_NORTH
#define OBC_EAST

         /* Open Boundary Conditions */
#undef SPONGE /* note: UCLA uses SPONGE with BRY */

#define OBC_M2FLATHER
/* Barotop. BC: OBC_M2FLATHER, OBC_M2ORLANSKI, OBC_M2SPECIFIED */
#undef OBC_M2SPECIFIED 

         /* Output */
#define AVERAGES
#define SLICE_AVG

        /* Biology */
!#define BIOLOGY_BEC2
!#define BEC2_DIAG
!#define RIVER_LOAD_N
!#define RIVER_LOAD_P
!#define PCO2AIR_FORCING

/* # define MULT_CLIM_FILES */
# define VFLX_CORR

/* #define SO */ /* used in param.h */

#include "set_global_definitions.h"


