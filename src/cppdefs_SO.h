/* 
  UP ETH Southern Ocean Setup 
  == === ======== ===== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */                   
#include "cppdefs_UP.h"
                 
         /* Forcing */
#define SALINITY_MASK

         /* Open Boundaries */
#define EW_PERIODIC
#define OBC_NORTH

         /* Open Boundary Conditions */
#undef SPONGE /* note: UCLA uses SPONGE with BRY */

         /* Output */
!--# define AVERAGES

#define SO /* used in param.h */

#include "set_global_definitions.h"

