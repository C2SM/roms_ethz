#include "cppdefs.h"
#if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
 
# define exchange_2d_tile exchange_r2d_tile
# define JSTART jstrR
# define ISTART istrR
# include "exchange_2d_tile.h"
# undef ISTART
# undef JSTART
# undef exchange_2d_tile
 
# define exchange_2d_tile exchange_u2d_tile
# define JSTART jstrR
# define ISTART istr
# include "exchange_2d_tile.h"
# undef ISTART
# undef JSTART
# undef exchange_2d_tile
 
# define exchange_2d_tile exchange_v2d_tile
# define JSTART jstr
# define ISTART istrR
# include "exchange_2d_tile.h"
# undef ISTART
# undef JSTART
# undef exchange_2d_tile
 
# define exchange_2d_tile exchange_p2d_tile
# define JSTART jstr
# define ISTART istr
# include "exchange_2d_tile.h"
# undef ISTART
# undef JSTART
# undef exchange_2d_tile
 
# ifdef SOLVE3D
#  define KSTART 1
#  define exchange_3d_tile exchange_r3d_tile
#  define JSTART jstrR
#  define ISTART istrR
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
 
#  define exchange_3d_tile exchange_u3d_tile
#  define JSTART jstrR
#  define ISTART istr
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
 
#  define exchange_3d_tile exchange_v3d_tile
#  define JSTART jstr
#  define ISTART istrR
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
 
#  define exchange_3d_tile exchange_p3d_tile
#  define JSTART jstr
#  define ISTART istr
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef KSTART
 
#  define KSTART 0
#  define exchange_3d_tile exchange_w3d_tile
#  define JSTART jstrR
#  define ISTART istrR
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef KSTART
# endif
#else
      subroutine exchange_empty
      end
#endif
 
 
 
 
 
