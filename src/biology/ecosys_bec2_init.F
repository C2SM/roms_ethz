#include "cppdefs.h"
#ifdef BIOLOGY_BEC2
      subroutine ecosys2_init()
      implicit none
 
#include "param.h" 
#include "param_bec2.h"
#include "ecosys_bec2.h"
#include "grid.h"

      integer auto_ind
      ! Variables used for namelist parameter input:
      integer status,  lvar,itrc,lenstr
#ifdef BIOPAR_NAMELIST
      character*256 filename_biopar_bec_nml, filename_biopar_out_bec_nml

      ! Define namelist for ecosystem parameters:
      namelist /ecosys_parms_nml/ 
     &    parm_Fe_bioavail,
     &    parm_o2_min,
     &    parm_o2_min_delta,
     &    parm_kappa_nitrif,
     &    parm_nitrif_par_lim,
     &    parm_z_mort_0,
     &    parm_z_mort2_0,
     &    parm_labile_ratio,
     &    parm_POMbury,
     &    parm_BSIbury,
     &    parm_Fe_scavenge_rate0,
     &    parm_f_prod_sp_CaCO3,
     &    parm_POC_diss,
     &    parm_SiO2_diss,
     &    parm_CaCO3_diss,
     &    parm_scalelen_z,
     &    parm_scalelen_vals,
     &    kFe, kPO4, kDOP, kNO3, kNH4, kSiO3, Qp, gQfe_0, gQfe_min, 
     &    alphaPI, PCref,
     &    thetaN_max, loss_thres, loss_thres2, temp_thres, mort, mort2, 
     &    agg_rate_max,
     &    agg_rate_min, z_umax_0, z_grz, graze_zoo, graze_poc, 
     &    graze_doc, loss_poc, f_zoo_detr


      filename_biopar_bec_nml = 'biopar_bec.nml'
      filename_biopar_out_bec_nml = 'biopar_out_bec.nml'
#endif

      !---------------------------------------------------------------------------
      !   default namelist settings
      !---------------------------------------------------------------------------
#ifdef AMACANC
# include "bec2_param_AMACANC.F"
#endif
#ifdef CANAMA
# include "bec2_param_CANAMA.F"
#endif
#ifdef PACTCS
# include "bec2_param_PACTCS.F"
#endif
#ifdef SO
# include "bec2_param_SO.F"
#endif

#ifdef BIOPAR_NAMELIST
      !---------------------------------------------------------------------------
      !   read in namelist
      !---------------------------------------------------------------------------

      open(73,file=filename_biopar_bec_nml,status='old',iostat=status)
      ! Since this file is optional, it is okay to proceed if it does not exist or
      ! cannot be read.
      if (status .eq. 0) then
         read(73,NML=ecosys_parms_nml,iostat=status)
#ifdef MPI
         if (mynode.eq.0) then
#endif
            if (status .ne. 0) then
               print *, 'Could not read the biology parameter namelist!'
               print *, ' --- WARNING: Error reading BEC parameter ',
     &                  'input file.'
            else
               print *, 'Biology parameters were read from ',
     &              filename_biopar_bec_nml
            end if
#ifdef MPI
         end if  ! mynode.eq.0
#endif
         close(73)
      else  ! status .eq. 0
# ifdef MPI
         if (mynode.eq.0) then
# endif
            print *, ' --- WARNING: BEC parameter input file not found.'
            print *, '             ==> Using default values.'
# ifdef MPI
         end if
# endif
      end if  ! status .eq. 0
#ifndef BIOPAR_NOWRITE
# ifdef MPI
      ! Master process writes file with parameters used:
      if (mynode.eq.0) then
# endif
         open(74,file=filename_biopar_out_bec_nml,status='unknown',
     &           iostat=status)
         if (status .eq. 0) then
            write(74,NML=ecosys_parms_nml,iostat=status)
            if (status .ne. 0) then
               print *,'Could not write the biology parameter namelist!'
            end if
            close(74)
         else
            print *, 'Could not create output file ',
     &           filename_biopar_out_bec_nml
         end if
# ifdef MPI
      end if  ! (mynode.eq.0)
# endif
#endif /* BIOPAR_NOWRITE */
#endif /* BIOPAR_NAMELIST */


#ifdef BEC2_DIAG
      !---------------------------------------------------------------------------
      !   Initialize diagnostic variables:
      !---------------------------------------------------------------------------
# ifdef BEC2_DIAG_USER
      ! Allocate arrays for diagnostics:
      allocate(bec2_diag_2d(GLOBAL_2D_ARRAY,nr_bec2_diag_2d_user))
      allocate(bec2_diag_3d(GLOBAL_2D_ARRAY,N,nr_bec2_diag_3d_user))
#  ifdef AVERAGES
      allocate(bec2_diag_2d_avg(GLOBAL_2D_ARRAY,nr_bec2_diag_2d_user))
      allocate(bec2_diag_3d_avg(GLOBAL_2D_ARRAY,N,nr_bec2_diag_3d_user))
#   ifdef SLICE_AVG
      allocate(bec2_diag_2d_slavg(GLOBAL_2D_ARRAY,nr_bec2_diag_2d_user))
      allocate(bec2_diag_3d_slavg(GLOBAL_2D_ARRAY,nr_bec2_diag_3d_user))
#   endif
#  endif /* AVERAGES */
      MPI_master_only write (*,'(a,i3,a)') 'bec2_diag_2d allocated for ',
     &    nr_bec2_diag_2d_user, ' variables'
      MPI_master_only write (*,'(a,i3,a)') 'bec2_diag_3d allocated for ',
     &    nr_bec2_diag_3d_user, ' variables'
# endif /* BEC2_DIAG_USER */
      bec2_diag_2d = c0
      bec2_diag_3d = c0
#else /* BEC2_DIAG */
      ph_hist = c0
#endif /* BEC2_DIAG */

      !---------------------------------------------------------------------------
      !   Initialize ice fraction and atm. pressure field:
      !---------------------------------------------------------------------------
      ifrac = 0.0
      press = 1.

      lflux_gas_o2  = .TRUE.
      lflux_gas_co2 = .TRUE.
      liron_flux = .TRUE.
#ifdef RIVER_LOAD_N
      lriver_load_n = .TRUE.
#endif
#ifdef RIVER_LOAD_P
      lriver_load_p = .TRUE.
#endif
#ifdef RIVER_LOAD_ALK_DIC_SI
      lriver_load_alk = .TRUE.
      lriver_load_dic = .TRUE.
      lriver_load_si = .TRUE.
#endif
      lsource_sink  = .TRUE.  

      where(rmask==1)
         landmask=.TRUE.
      elsewhere 
         landmask=.FALSE.
      endwhere

#ifdef CCHEM_MOCSY
       optcon  = 'mol/m3'   ! input concentrations of DIC, alk, P, Si are in MOL/M3
       optt    = 'Tpot'     ! 'temp' is POTENTIAL temp [C]
       optp    = 'm'        ! input variable z_r is in METERS
       optb    = 'u74'      ! Uppstrom (1974) formulation for total boron
       optk1k2 = 'm95'      ! Millero (1995) formulations for k1 and k2
       optkf   = 'pf'       ! Perez & Fraga (1987) formulation for Kf
       optgas  = 'Pinsitu'  ! compute in situ values for fCO2 and pCO2 (i.e. accounting for pressure)
#endif


      end subroutine ecosys2_init

#else /* BIOLOGY_BEC2 */
      subroutine ecosys2_init()
      return 
      end
#endif /* BIOLOGY_BEC2 */
