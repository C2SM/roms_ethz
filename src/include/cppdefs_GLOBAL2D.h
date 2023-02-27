/*
  UP ETH GLOBAL TSUNAMI SETUP
  == === ====== ======= =====
  For Testing with the PACTCS30 setup
*/


!----------------------------- PACIFIC2D
# define UV_COR
 # define UV_ADV
                      /* Grid Configuration */
 # define CURVGRID
 # define SPHERICAL
 # define MASKING

 # undef VIS_GRID
 # define UV_VIS2

                      /* Forcing */
 # define ANA_SMFLUX
 # define ANA_INITIAL

                      /* Open Boundary Conditions */
 # define OBC_WEST
 # define OBC_SOUTH
 # define OBC_M2FLATHER
 # define ANA_BRY
 # define Z_FRC_BRY
 # define M2_FRC_BRY
 # define SPONGE


!----------------------------- End PACIFIC2D
!#define SPONGE_WIDTH /* # of sponge points is input parameter */

                      /* Output */
#define AVERAGES
#define MASK_LAND_DATA

                      /* Grid Configuration */
! default: PACTCS30 
# define PACTCS30
# define GRID_SIZE LLm=602, MMm=516, N=64      ! pactcs30 4.1-65km telescopic up to Antarctica
# define DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1 ! Euler




#include "set_global_definitions.h"

#undef PARALLEL_FILES


!------------------------------

