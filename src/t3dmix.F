#include "cppdefs.h"
#if defined SOLVE3D && defined MIX_EN_TS
# include "t3dmix_EN.F"
#elif defined SOLVE3D && defined MIX_GP_TS
# include "t3dmix_GP.F"
#elif defined SOLVE3D && defined MIX_S_TS
# include "t3dmix_S.F"
#else
      subroutine t3dmix_empty
      end
#endif
 
 
