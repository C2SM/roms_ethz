#include "cppdefs.h"
        subroutine lmd_kpp_init()
        implicit none

#include "param_lmd.h"
#include "param.h"
#ifdef MPI
# include "mpif.h"
#endif

        character*100 filename_lmd_nml,filename_lmd_out_nml
        integer status

        filename_lmd_nml = 'param_lmd.nml'
        filename_lmd_out_nml = 'param_lmd_out.nml'

        nubl=0.01   ! maximum allowed boundary layer

        nu0c=0.1    ! convective adjustment for viscosity
                    !              and diffusivity [m^2/s].

        Cv=1.8      ! ratio of interior Brunt-Vaisala frequency
                    ! "N" to that at the entrainment depth
                    ! "he".

        Ricr=0.45   ! Critical bulk Richardson number (0.3).

        betaT=-0.2  ! ratio of entrainment flux to surface
                    !         buoyancy flux.

        epssfc=0.1  ! nondimensional extent of the surface layer.

        C_MO=1.     ! constant for computaion Monin-Obukhov depth.

        C_Ek=258.   ! constant for computating stabilization term
                    ! due to Coriolis force (Ekman depth
                    ! limit).

        Cstar=10.   ! proportionality coefficient parameterizing
                    ! nonlocal transport.

        eps=1.E-20

        zeta_m=-0.2 ! Maximum stability parameters "zeta"
        a_m=1.257   ! value of the 1/3 power law regime of
        c_m=8.360   ! flux profile for momentum and tracers
        zeta_s=-1.0 ! and coefficients of flux profile for
        a_s=-28.86  ! momentum and tracers in their 1/3-power
        c_s=98.96   ! law regime;

        alpha_hbl=1.  ! Parameter coefficients introduced by jds 
        offset_hbl=1. ! to optimize the MLD: alpha is a scaling factor
                      ! offset is an offset (JDS)

        ! parameters representing a scaling factor
        !  and an offset in the 
        ! salinities and thermal diffusivities in order to have a non-zero 
        ! thermal diffusivity in the deep ocean
        alpha_temp_diff=1.
        alpha_salt_diff=1.
        offset_temp_diff=1.
        offset_salt_diff=1.

        alpha_input_shflux=1. ! Parameter to optimize the amplitude of the 
                              ! input sensible heat flux

        alpha_input_swflux=1. ! Parameter to optimize the amplitude of the 
                              ! input water flux

        open(75,file=filename_lmd_nml,status='old',iostat=status)

        if (status .eq. 0) then
           read(75,NML=param_lmd_nml,iostat=status)
           if (status .ne. 0) then
               MPI_master_only print *, 
     &             'Could not read the turbulence (lmd_kpp) ',
     &             'parameter namelist!'
           else
               MPI_master_only print *, 
     &               'Turbulence (lmd_kpp) parameters read from ',
     &               filename_lmd_nml
           end if
           close(75)
        else
CR           print *, 'lmd parameter namelist file was not found'
CR           print *, 'Default values will be used.'
        end if

#ifdef WRITE_KPP_PAR
        !DL: write biopar file only by the master process:
# ifdef MPI
        if (mynode.eq.0) then
# endif
           open(76,file=filename_lmd_out_nml,status='unknown'
     &          ,iostat=status)

           if (status .eq. 0) then
              write(76,NML=param_lmd_nml,iostat=status)
              if (status .ne. 0) then
                  print *, 'Could not write the turbulence (lmd_kpp) ',
     &                'parameter namelist!'
              end if
              close(76)
           else
               print *, 'Could not create lmd param output file ',
     &                     filename_lmd_out_nml
           end if 
# ifdef MPI
        end if ! (mynode.eq.0
# endif
#endif

        return
!End of the function
        end
