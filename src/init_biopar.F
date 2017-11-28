#include "cppdefs.h"
#ifdef BIOLOGY_NPZDOC
      subroutine init_biopar
      implicit none
#include "biopar.h"
#include "param.h" /* We need param.h for 'mynode' mpif.h for MPI_master_only */
#ifdef MPI
# include "mpif.h"
#endif
      integer status
      character*100 filename_biopar_nml, filename_biopar_out_nml

      filename_biopar_nml = 'biopar.nml'
      filename_biopar_out_nml = 'biopar_out.nml'

! first set the default values
      kwater  = 0.04
      kChla   = 0.024
      CN_P    = 6.625
      CN_Z    = 6.625
#ifdef CARBON
      r_CaCO3_orgC = 0.07
      t_dissCaCO3 = 0.0057
#endif /* CARBON */
      theta_m = 0.053478
      palpha = 1.0
      K_NO3   = 1./0.75
      K_NH4   = 1./0.5
      t_Pmort = 0.024
      t_Zgraz = 0.6
      AE_N    = 0.75
#ifdef CARBON
      AE_C    = 0.75
#endif /* CARBON */
      GGE_C   = 0.75
      K_P     = 1.
      t_Zbmet = 0.1
      t_Zmort = 0.1
      Zgamma = 0.33
      Zomega = 0.33
      t_SDremin = 0.03
      t_LDremin = 0.01
#ifdef SEDIMENT_BIOLOGY
      t_SedRemin = 0.003
# ifdef CARBON
      t_SedDissCaCO3 = 0.002
# endif
#endif /* SEDIMENT_BIOLOGY */
      t_coag    = 0.005
      t_nitri = 0.05
      I_thNH4  = 0.0095
      D_p5NH4  = 0.036
#if defined OXYGEN
      ON_NO3 = 9.375
      ON_NH4 = 7.375
#endif /* OXYGEN */
#ifdef OXYLIM
      N_CDET = 0.8
#endif /* OXYLIM */
      wSDet   = 1.0
      wLDet   = 10.0
      wPhyt   = 0.5
#ifdef CARBON
      wCaCO3 = 20.0
      pHeps = 0.25
#endif /* CARBON */
      Vp0 = 1.0

      open(71,file=filename_biopar_nml,status='old',iostat=status)
! Since this file is optional, it's okay to proceed if it doesn't exist or
! can't be read.
      if (status .eq. 0) then
         read(71,NML=biology_param_nml,iostat=status)
         if (status .ne. 0) then
            MPI_master_only print *, 
     &           'Could not read the biology parameter namelist!'
         else
            MPI_master_only print *, 
     &           'Biology parameters were read from ', 
     &           filename_biopar_nml
         end if
         close(71)
      else
         MPI_master_only print *, 
     &      'Biology parameter namelist file was not found'
         MPI_master_only print *, 'Default values will be used.'
      end if

#ifdef WRITE_BIOPAR
      !DL: write biopar file only by the master process:
# ifdef MPI
      if (mynode.eq.0) then
# endif
         open(72,file=filename_biopar_out_nml,status='unknown',
     &        iostat=status)
         if (status .eq. 0) then
            write(72,NML=biology_param_nml,iostat=status)
            if (status .ne. 0) then
               print *,'Could not write the biology parameter namelist!'
            end if
            close(72)
         else
            print *, 'Could not create output file ', 
     &           filename_biopar_out_nml
         end if
# ifdef MPI
      end if ! mynode.eq.0
# endif
#endif

      return
      end
#else /* BIOLOGY_NPZDOC */
      subroutine init_biopar_empty
      return
      end
#endif /* BIOLOGY_NPZDOC */
      
      
