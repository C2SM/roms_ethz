/* 
  UP SAEP (Southern Atlantic Eastern Pacific) Coupled Model Setup
  == ==== ========= ======== ======= ======== ======= ===== =====
*/

      /* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"

      /* Open Boundary */
#undef OBC_EAST
#define OBC_WEST
#define OBC_NORTH
#define OBC_SOUTH

       /* Open Boundary Conditions */
#define TCLIMATOLOGY
#define UCLIMATOLOGY
#define TNUDGING
#define M3NUDGING
#define M2NUDGING

       /* Mixing */ 
#ifdef TCLIMATOLOGY
# define CLIMAT_TS_MIXH
#endif

       /* Forcing */
#define SFLX_CORR
#define DIURNAL_SRFLUX
#define VFLX_CORR

       /* Output */
#define AVERAGES

#include "set_global_definitions.h"
