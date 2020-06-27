/*
 *      Diego Santaren's 1D (ONE_DIM) setup for parameter tuning
 *      */
#include "cppdefs_UP.h"

#define GRID_SIZE LLm=2, MMm=2, N=32
#define  DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1

                     /* Lateral Boundary */
# define EW_PERIODIC
# define NS_PERIODIC
                     /* Output */
# define EXACT_RESTART
# define AVERAGES
                     /* Biology */
!# define BIO_1ST_USTREAM_TEST
!# define DAILYPAR_BEC
!#define BIOLOGY_NPZDOC
!# define BIOLOGY_BEC

                     /* Flux Analysis */
!#define BGC_FLUX_ANALYSIS
#define PHYS_FLUX_ANALYSIS

#include "set_global_definitions.h"
