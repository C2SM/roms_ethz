/*
     Diego Santaren's 1D (ONE_DIM) setup for parameter tuning
*/

                     /* Lateral Boundary */
# define EW_PERIODIC
# define NS_PERIODIC
                     /* Output */
# define EXACT_RESTART
# define AVERAGES
                     /* Biology */
# define BIO_1ST_USTREAM_TEST
# define DAILYPAR_BEC
#define BIOLOGY_NPZDOC
!# define BIOLOGY_BEC

                     /* Flux Analysis */
!#define BGC_FLUX_ANALYSIS
#define PHYS_FLUX_ANALYSIS

