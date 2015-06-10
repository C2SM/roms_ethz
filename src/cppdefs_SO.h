/* 
  UP ETH Southern Ocean Setup 
  == === ======== ===== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */                   
#include "cppdefs_UP.h"
                 
         /* Forcing */
#define SALINITY_MASK

         /* Open Boundary Conditions */
#define EW_PERIODIC
#define OBC_NORTH
#define OBC_M2FLATHER
#define OBC_M3ORLANSKI
#define OBC_TORLANSKI

         /* Output */
!--# define AVERAGES

#define SO


#include "set_global_definitions.h"

