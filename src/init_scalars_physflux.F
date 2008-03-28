!
!
#include "cppdefs.h"
#ifdef PHYS_FLUX_ANALYSIS

       subroutine init_scalars_physflux(ierr)
!
! Set initial values for  globally accessible (stored in common
! blocks) scalar variables of the NPZDOC model.
!
      implicit none
      integer ierr, i,j
      integer omp_get_num_threads
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "physflux.h"
#include "ncroms_physflux.h"

! Find out how many threads are created and check whether the number
! of threads exceeds maximum allowed, as well as check if the number
! of tiles is divisible by the number of threads, so the job can be
! evenly partitioned, complain and signal to terminate, if something
! is wrong. 
! 
C$OMP PARALLEL
C$OMP CRITICAL (isca_cr_rgn)
      numthreads=1
C$      numthreads=omp_get_num_threads()
C$OMP END CRITICAL (isca_cr_rgn)
C$OMP END PARALLEL
      MPI_master_only write(stdout,'(1x,A,3(1x,A,I3),A)') 'NUMBER',
     &    'OF THREADS:',numthreads,'BLOCKING:',NSUB_X,'x',NSUB_E,'.'

#ifdef MPI
# define NUMPROC NNODES
#else
# define NUMPROC 1
#endif

      if (numthreads.gt.NUMPROC) then
        MPI_master_only write(stdout,'(/1x,A,I3/)')
     &    'ERROR: Requested number of threads exceeds setting: ', NUMPROC
        ierr=ierr+1                                      !--> ERROR
      elseif (mod(NSUB_X*NSUB_E,numthreads).ne.0) then
        MPI_master_only write(stdout,
     &                '(/1x,A,1x,A,I3,4x,A,I3,4x,A,I4,A)') 'ERROR:',
     &                'wrong choice of numthreads =', numthreads,
     &                'NSUB_X =', NSUB_X, 'NSUB_E =', NSUB_E, '.'
        ierr=ierr+1                                      !--> ERROR
      endif
!
!
! Initialize netCDF files IDs to closed status.
!
      ncid_phys_flux_his = -1
      ncid_phys_flux_avg = -1

!
! Define names of variables in NetCDF output files.
! Inner dimension is for variable type and outer is for variable
! attributes (name, long-name, units, field).
!
#ifdef SOLVE3D
! horizontal advective fluxes in xi-direction
      vname_phys(1,indxHorXAdvFlux)=
     &     'HorXAdvFlux_Temp                           '
      vname_phys(2,indxHorXAdvFlux)=
     &     'Horizontal (xi) advective flux of heat     '
      vname_phys(3,indxHorXAdvFlux)=
     &     'degC s-1                                   '
      vname_phys(4,indxHorXAdvFlux)=
     &     'HorXAdvFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxHorXAdvFlux+1)=
     &     'HorXAdvFlux_Salt                           '
      vname_phys(2,indxHorXAdvFlux+1)=
     &     'Horizontal (xi) advective flux of salt     '
      vname_phys(3,indxHorXAdvFlux+1)=
     &     'PSU s-1                                    '
      vname_phys(4,indxHorXAdvFlux+1)=
     &     'HorXAdvFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxHorXAdvFlux+2)=
     &     'HorXAdvFlux_NO3                            '
      vname_phys(2,indxHorXAdvFlux+2)=
     &     'Horizontal (xi) advective flux of NO3      '
      vname_phys(3,indxHorXAdvFlux+2)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+2)=
     &     'HorXAdvFlux_NO3, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+3)=
     &     'HorXAdvFlux_NH4                            '
      vname_phys(2,indxHorXAdvFlux+3)=
     &     'Horizontal (xi) advective flux of NH4      '
      vname_phys(3,indxHorXAdvFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+3)=
     &     'HorXAdvFlux_NH4, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+4)=
     &     'HorXAdvFlux_Chla                           '
      vname_phys(2,indxHorXAdvFlux+4)=
     &     'Horizontal (xi) advective flux of Chla     '
      vname_phys(3,indxHorXAdvFlux+4)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXAdvFlux+4)=
     &     'HorXAdvFlux_Chla, scalar, series           '

      vname_phys(1,indxHorXAdvFlux+5)=
     &     'HorXAdvFlux_Phyto                          '
      vname_phys(2,indxHorXAdvFlux+5)=
     &     'Horizontal (xi) advective flux of Phyto    '
      vname_phys(3,indxHorXAdvFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+5)=
     &     'HorXAdvFlux_Phyto, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+6)=
     &     'HorXAdvFlux_Zoo                            '
      vname_phys(2,indxHorXAdvFlux+6)=
     &     'Horizontal (xi) advective flux of Zoo      '
      vname_phys(3,indxHorXAdvFlux+6)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+6)=
     &     'HorXAdvFlux_Zoo, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+7)=
     &     'HorXAdvFlux_SDetN                          '
      vname_phys(2,indxHorXAdvFlux+7)=
     &     'Horizontal (xi) advective flux of SDetN    '
      vname_phys(3,indxHorXAdvFlux+7)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+7)=
     &     'HorXAdvFlux_SDetN, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+8)=
     &     'HorXAdvFlux_LDetN                          '
      vname_phys(2,indxHorXAdvFlux+8)=
     &     'Horizontal (xi) advective flux of LDetN    '
      vname_phys(3,indxHorXAdvFlux+8)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+8)=
     &     'HorXAdvFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxHorXAdvFlux+9)=
     &     'HorXAdvFlux_O2                             '
      vname_phys(2,indxHorXAdvFlux+9)=
     &     'Horizontal (xi) advective flux of O2       '
      vname_phys(3,indxHorXAdvFlux+9)=
     &     'mmol O2 s-1                                '
      vname_phys(4,indxHorXAdvFlux+9)=
     &     'HorXAdvFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxHorXAdvFlux+10)=
     &     'HorXAdvFlux_DIC                            '
      vname_phys(2,indxHorXAdvFlux+10)=
     &     'Horizontal (xi) advective flux of DIC      '
      vname_phys(3,indxHorXAdvFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+10)=
     &     'HorXAdvFlux_DIC, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+11)=
     &     'HorXAdvFlux_Alk                            '
      vname_phys(2,indxHorXAdvFlux+11)=
     &     'Horizontal (xi) advective flux of Alk      '
      vname_phys(3,indxHorXAdvFlux+11)=
     &     'meq s-1                                    '
      vname_phys(4,indxHorXAdvFlux+11)=
     &     'HorXAdvFlux_Alk, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+12)=
     &     'HorXAdvFlux_SDetC                          '
      vname_phys(2,indxHorXAdvFlux+12)=
     &     'Horizontal (xi) advective flux of SDetC    '
      vname_phys(3,indxHorXAdvFlux+12)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+12)=
     &     'HorXAdvFlux_SDetC, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+13)=
     &     'HorXAdvFlux_LDetC                          '
      vname_phys(2,indxHorXAdvFlux+13)=
     &     'Horizontal (xi) advective flux of LDetC    '
      vname_phys(3,indxHorXAdvFlux+13)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+13)=
     &     'HorXAdvFlux_LDetC, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+14)=
     &     'HorXAdvFlux_CaCO3                          '
      vname_phys(2,indxHorXAdvFlux+14)=
     &     'Horizontal (xi) advective flux of CaCO3    '
      vname_phys(3,indxHorXAdvFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+14)=
     &     'HorXAdvFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

! horizontal advective fluxes in eta-direction
      vname_phys(1,indxHorYAdvFlux)=
     &     'HorYAdvFlux_Temp                           '
      vname_phys(2,indxHorYAdvFlux)=
     &     'Horizontal (eta) advective flux of heat    '
      vname_phys(3,indxHorYAdvFlux)=
     &     'degC s-1                                   '
      vname_phys(4,indxHorYAdvFlux)=
     &     'HorYAdvFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxHorYAdvFlux+1)=
     &     'HorYAdvFlux_Salt                           '
      vname_phys(2,indxHorYAdvFlux+1)=
     &     'Horizontal (eta) advective flux of salt    '
      vname_phys(3,indxHorYAdvFlux+1)=
     &     'PSU s-1                                    '
      vname_phys(4,indxHorYAdvFlux+1)=
     &     'HorYAdvFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxHorYAdvFlux+2)=
     &     'HorYAdvFlux_NO3                            '
      vname_phys(2,indxHorYAdvFlux+2)=
     &     'Horizontal (eta) advective flux of NO3     '
      vname_phys(3,indxHorYAdvFlux+2)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+2)=
     &     'HorYAdvFlux_NO3, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+3)=
     &     'HorYAdvFlux_NH4                            '
      vname_phys(2,indxHorYAdvFlux+3)=
     &     'Horizontal (eta) advective flux of NH4     '
      vname_phys(3,indxHorYAdvFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+3)=
     &     'HorYAdvFlux_NH4, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+4)=
     &     'HorYAdvFlux_Chla                           '
      vname_phys(2,indxHorYAdvFlux+4)=
     &     'Horizontal (eta) advective flux of Chla    '
      vname_phys(3,indxHorYAdvFlux+4)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYAdvFlux+4)=
     &     'HorYAdvFlux_Chla, scalar, series           '

      vname_phys(1,indxHorYAdvFlux+5)=
     &     'HorYAdvFlux_Phyto                          '
      vname_phys(2,indxHorYAdvFlux+5)=
     &     'Horizontal (eta) advective flux of Phyto   '
      vname_phys(3,indxHorYAdvFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+5)=
     &     'HorYAdvFlux_Phyto, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+6)=
     &     'HorYAdvFlux_Zoo                            '
      vname_phys(2,indxHorYAdvFlux+6)=
     &     'Horizontal (eta) advective flux of Zoo     '
      vname_phys(3,indxHorYAdvFlux+6)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+6)=
     &     'HorYAdvFlux_Zoo, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+7)=
     &     'HorYAdvFlux_SDetN                          '
      vname_phys(2,indxHorYAdvFlux+7)=
     &     'Horizontal (eta) advective flux of SDetN   '
      vname_phys(3,indxHorYAdvFlux+7)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+7)=
     &     'HorYAdvFlux_SDetN, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+8)=
     &     'HorYAdvFlux_LDetN                          '
      vname_phys(2,indxHorYAdvFlux+8)=
     &     'Horizontal (eta) advective flux of LDetN   '
      vname_phys(3,indxHorYAdvFlux+8)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+8)=
     &     'HorYAdvFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxHorYAdvFlux+9)=
     &     'HorYAdvFlux_O2                             '
      vname_phys(2,indxHorYAdvFlux+9)=
     &     'Horizontal (eta) advective flux of O2      '
      vname_phys(3,indxHorYAdvFlux+9)=
     &     'mmol O2 s-1                                '
      vname_phys(4,indxHorYAdvFlux+9)=
     &     'HorYAdvFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxHorYAdvFlux+10)=
     &     'HorYAdvFlux_DIC                            '
      vname_phys(2,indxHorYAdvFlux+10)=
     &     'Horizontal (eta) advective flux of DIC     '
      vname_phys(3,indxHorYAdvFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+10)=
     &     'HorYAdvFlux_DIC, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+11)=
     &     'HorYAdvFlux_Alk                            '
      vname_phys(2,indxHorYAdvFlux+11)=
     &     'Horizontal (eta) advective flux of Alk     '
      vname_phys(3,indxHorYAdvFlux+11)=
     &     'meq s-1                                    '
      vname_phys(4,indxHorYAdvFlux+11)=
     &     'HorYAdvFlux_Alk, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+12)=
     &     'HorYAdvFlux_SDetC                          '
      vname_phys(2,indxHorYAdvFlux+12)=
     &     'Horizontal (eta) advective flux of SDetC   '
      vname_phys(3,indxHorYAdvFlux+12)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+12)=
     &     'HorYAdvFlux_SDetC, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+13)=
     &     'HorYAdvFlux_LDetC                          '
      vname_phys(2,indxHorYAdvFlux+13)=
     &     'Horizontal (eta) advective flux of LDetC   '
      vname_phys(3,indxHorYAdvFlux+13)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+13)=
     &     'HorYAdvFlux_LDetC, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+14)=
     &     'HorYAdvFlux_CaCO3                          '
      vname_phys(2,indxHorYAdvFlux+14)=
     &     'Horizontal (eta) advective flux of CaCO3   '
      vname_phys(3,indxHorYAdvFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+14)=
     &     'HorYAdvFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

! vertical advective fluxes
      vname_phys(1,indxVertAdvFlux)=
     &     'VertAdvFlux_Temp                           '
      vname_phys(2,indxVertAdvFlux)=
     &     'Vertical advective flux of heat            '
      vname_phys(3,indxVertAdvFlux)=
     &     'degC m-2 s-1                               '
      vname_phys(4,indxVertAdvFlux)=
     &     'VertAdvFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxVertAdvFlux+1)=
     &     'VertAdvFlux_Salt                           '
      vname_phys(2,indxVertAdvFlux+1)=
     &     'Vertical advective flux of salt            '
      vname_phys(3,indxVertAdvFlux+1)=
     &     'PSU m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+1)=
     &     'VertAdvFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxVertAdvFlux+2)=
     &     'VertAdvFlux_NO3                            '
      vname_phys(2,indxVertAdvFlux+2)=
     &     'Vertical advective flux of NO3             '
      vname_phys(3,indxVertAdvFlux+2)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+2)=
     &     'VertAdvFlux_NO3, scalar, series            '

      vname_phys(1,indxVertAdvFlux+3)=
     &     'VertAdvFlux_NH4                            '
      vname_phys(2,indxVertAdvFlux+3)=
     &     'Vertical advective flux of NH4             '
      vname_phys(3,indxVertAdvFlux+3)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+3)=
     &     'VertAdvFlux_NH4, scalar, series            '

      vname_phys(1,indxVertAdvFlux+4)=
     &     'VertAdvFlux_Chla                           '
      vname_phys(2,indxVertAdvFlux+4)=
     &     'Vertical advective flux of Chla            '
      vname_phys(3,indxVertAdvFlux+4)=
     &     'mg Chl-a m-2 s-1                           '
      vname_phys(4,indxVertAdvFlux+4)=
     &     'VertAdvFlux_Chla, scalar, series           '

      vname_phys(1,indxVertAdvFlux+5)=
     &     'VertAdvFlux_Phyto                          '
      vname_phys(2,indxVertAdvFlux+5)=
     &     'Vertical advective flux of Phyto           '
      vname_phys(3,indxVertAdvFlux+5)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+5)=
     &     'VertAdvFlux_Phyto, scalar, series          '

      vname_phys(1,indxVertAdvFlux+6)=
     &     'VertAdvFlux_Zoo                            '
      vname_phys(2,indxVertAdvFlux+6)=
     &     'Vertical advective flux of Zoo             '
      vname_phys(3,indxVertAdvFlux+6)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+6)=
     &     'VertAdvFlux_Zoo, scalar, series            '

      vname_phys(1,indxVertAdvFlux+7)=
     &     'VertAdvFlux_SDetN                          '
      vname_phys(2,indxVertAdvFlux+7)=
     &     'Vertical advective flux of SDetN           '
      vname_phys(3,indxVertAdvFlux+7)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+7)=
     &     'VertAdvFlux_SDetN, scalar, series          '

      vname_phys(1,indxVertAdvFlux+8)=
     &     'VertAdvFlux_LDetN                          '
      vname_phys(2,indxVertAdvFlux+8)=
     &     'Vertical advective flux of LDetN           '
      vname_phys(3,indxVertAdvFlux+8)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+8)=
     &     'VertAdvFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxVertAdvFlux+9)=
     &     'VertAdvFlux_O2                             '
      vname_phys(2,indxVertAdvFlux+9)=
     &     'Vertical advective flux of O2              '
      vname_phys(3,indxVertAdvFlux+9)=
     &     'mmol O2 m-2 s-1                            '
      vname_phys(4,indxVertAdvFlux+9)=
     &     'VertAdvFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxVertAdvFlux+10)=
     &     'VertAdvFlux_DIC                            '
      vname_phys(2,indxVertAdvFlux+10)=
     &     'Vertical advective flux of DIC             '
      vname_phys(3,indxVertAdvFlux+10)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+10)=
     &     'VertAdvFlux_DIC, scalar, series            '

      vname_phys(1,indxVertAdvFlux+11)=
     &     'VertAdvFlux_Alk                            '
      vname_phys(2,indxVertAdvFlux+11)=
     &     'Vertical advective flux of Alk             '
      vname_phys(3,indxVertAdvFlux+11)=
     &     'meq m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+11)=
     &     'VertAdvFlux_Alk, scalar, series            '

      vname_phys(1,indxVertAdvFlux+12)=
     &     'VertAdvFlux_SDetC                          '
      vname_phys(2,indxVertAdvFlux+12)=
     &     'Vertical advective flux of SDetC           '
      vname_phys(3,indxVertAdvFlux+12)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+12)=
     &     'VertAdvFlux_SDetC, scalar, series          '

      vname_phys(1,indxVertAdvFlux+13)=
     &     'VertAdvFlux_LDetC                          '
      vname_phys(2,indxVertAdvFlux+13)=
     &     'Vertical advective flux of LDetC           '
      vname_phys(3,indxVertAdvFlux+13)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+13)=
     &     'VertAdvFlux_LDetC, scalar, series          '

      vname_phys(1,indxVertAdvFlux+14)=
     &     'VertAdvFlux_CaCO3                          '
      vname_phys(2,indxVertAdvFlux+14)=
     &     'Vertical advective flux of CaCO3           '
      vname_phys(3,indxVertAdvFlux+14)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+14)=
     &     'VertAdvFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

#ifdef VERT_DIFF_ANALYSIS
! vertical diffusive fluxes
      vname_phys(1,indxVertDiffFlux)=
     &     'VertDiffFlux_Temp                          '
      vname_phys(2,indxVertDiffFlux)=
     &     'Vertical diffusive flux of heat            '
      vname_phys(3,indxVertDiffFlux)=
     &     'degC m-2 s-1                               '
      vname_phys(4,indxVertDiffFlux)=
     &     'VertDiffFlux_Temp, scalar, series          '

# ifdef SALINITY
      vname_phys(1,indxVertDiffFlux+1)=
     &     'VertDiffFlux_Salt                          '
      vname_phys(2,indxVertDiffFlux+1)=
     &     'Vertical diffusive flux of salt            '
      vname_phys(3,indxVertDiffFlux+1)=
     &     'PSU m-2 s-1                                '
      vname_phys(4,indxVertDiffFlux+1)=
     &     'VertDiffFlux_Salt, scalar, series          '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxVertDiffFlux+2)=
     &     'VertDiffFlux_NO3                           '
      vname_phys(2,indxVertDiffFlux+2)=
     &     'Vertical diffusive flux of NO3             '
      vname_phys(3,indxVertDiffFlux+2)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+2)=
     &     'VertDiffFlux_NO3, scalar, series           '

      vname_phys(1,indxVertDiffFlux+3)=
     &     'VertDiffFlux_NH4                           '
      vname_phys(2,indxVertDiffFlux+3)=
     &     'Vertical diffusive flux of NH4             '
      vname_phys(3,indxVertDiffFlux+3)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+3)=
     &     'VertDiffFlux_NH4, scalar, series           '

      vname_phys(1,indxVertDiffFlux+4)=
     &     'VertDiffFlux_Chla                          '
      vname_phys(2,indxVertDiffFlux+4)=
     &     'Vertical diffusive flux of Chla            '
      vname_phys(3,indxVertDiffFlux+4)=
     &     'mg Chl-a m-2 s-1                           '
      vname_phys(4,indxVertDiffFlux+4)=
     &     'VertDiffFlux_Chla, scalar, series          '

      vname_phys(1,indxVertDiffFlux+5)=
     &     'VertDiffFlux_Phyto                         '
      vname_phys(2,indxVertDiffFlux+5)=
     &     'Vertical diffusive flux of Phyto           '
      vname_phys(3,indxVertDiffFlux+5)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+5)=
     &     'VertDiffFlux_Phyto, scalar, series         '

      vname_phys(1,indxVertDiffFlux+6)=
     &     'VertDiffFlux_Zoo                           '
      vname_phys(2,indxVertDiffFlux+6)=
     &     'Vertical diffusive flux of Zoo             '
      vname_phys(3,indxVertDiffFlux+6)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+6)=
     &     'VertDiffFlux_Zoo, scalar, series           '

      vname_phys(1,indxVertDiffFlux+7)=
     &     'VertDiffFlux_SDetN                         '
      vname_phys(2,indxVertDiffFlux+7)=
     &     'Vertical diffusive flux of SDetN           '
      vname_phys(3,indxVertDiffFlux+7)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+7)=
     &     'VertDiffFlux_SDetN, scalar, series         '

      vname_phys(1,indxVertDiffFlux+8)=
     &     'VertDiffFlux_LDetN                         '
      vname_phys(2,indxVertDiffFlux+8)=
     &     'Vertical diffusive flux of LDetN           '
      vname_phys(3,indxVertDiffFlux+8)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+8)=
     &     'VertDiffFlux_LDetN, scalar, series         '

#  ifdef OXYGEN
      vname_phys(1,indxVertDiffFlux+9)=
     &     'VertDiffFlux_O2                            '
      vname_phys(2,indxVertDiffFlux+9)=
     &     'Vertical diffusive flux of O2              '
      vname_phys(3,indxVertDiffFlux+9)=
     &     'mmol O2 m-2 s-1                            '
      vname_phys(4,indxVertDiffFlux+9)=
     &     'VertDiffFlux_O2, scalar, series            '

#   ifdef CARBON
      vname_phys(1,indxVertDiffFlux+10)=
     &     'VertDiffFlux_DIC                           '
      vname_phys(2,indxVertDiffFlux+10)=
     &     'Vertical diffusive flux of DIC             '
      vname_phys(3,indxVertDiffFlux+10)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+10)=
     &     'VertDiffFlux_DIC, scalar, series           '

      vname_phys(1,indxVertDiffFlux+11)=
     &     'VertDiffFlux_Alk                           '
      vname_phys(2,indxVertDiffFlux+11)=
     &     'Vertical diffusive flux of Alk             '
      vname_phys(3,indxVertDiffFlux+11)=
     &     'meq m-2 s-1                                '
      vname_phys(4,indxVertDiffFlux+11)=
     &     'VertDiffFlux_Alk, scalar, series           '

      vname_phys(1,indxVertDiffFlux+12)=
     &     'VertDiffFlux_SDetC                         '
      vname_phys(2,indxVertDiffFlux+12)=
     &     'Vertical diffusive flux of SDetC           '
      vname_phys(3,indxVertDiffFlux+12)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+12)=
     &     'VertDiffFlux_SDetC, scalar, series         '

      vname_phys(1,indxVertDiffFlux+13)=
     &     'VertDiffFlux_LDetC                         '
      vname_phys(2,indxVertDiffFlux+13)=
     &     'Vertical diffusive flux of LDetC           '
      vname_phys(3,indxVertDiffFlux+13)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+13)=
     &     'VertDiffFlux_LDetC, scalar, series         '

      vname_phys(1,indxVertDiffFlux+14)=
     &     'VertDiffFlux_CaCO3                         '
      vname_phys(2,indxVertDiffFlux+14)=
     &     'Vertical diffusive flux of CaCO3           '
      vname_phys(3,indxVertDiffFlux+14)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertDiffFlux+14)=
     &     'VertDiffFlux_CaCO3, scalar, series         '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */
#endif /* VERT_DIFF_ANALYSIS */

# ifdef FULL_PHYS_FLUX_ANALYSIS
! top fluxes
      vname_phys(1,indxTopFlux)=
     &     'TopFlux_Temp                               '
      vname_phys(2,indxTopFlux)=
     &     'Top flux of heat                           '
      vname_phys(3,indxTopFlux)=
     &     'degC m-2 s-1                               '
      vname_phys(4,indxTopFlux)=
     &     'TopFlux_Temp, scalar, series               '

# ifdef SALINITY
      vname_phys(1,indxTopFlux+1)=
     &     'TopFlux_Salt                               '
      vname_phys(2,indxTopFlux+1)=
     &     'Top flux of salt                           '
      vname_phys(3,indxTopFlux+1)=
     &     'PSU m-2 s-1                                '
      vname_phys(4,indxTopFlux+1)=
     &     'TopFlux_Salt, scalar, series               '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxTopFlux+2)=
     &     'TopFlux_NO3                                '
      vname_phys(2,indxTopFlux+2)=
     &     'Top flux of NO3                            '
      vname_phys(3,indxTopFlux+2)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxTopFlux+2)=
     &     'TopFlux_NO3, scalar, series                '

      vname_phys(1,indxTopFlux+3)=
     &     'TopFlux_NH4                                '
      vname_phys(2,indxTopFlux+3)=
     &     'Top flux of NH4                            '
      vname_phys(3,indxTopFlux+3)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxTopFlux+3)=
     &     'TopFlux_NH4, scalar, series                '

      vname_phys(1,indxTopFlux+4)=
     &     'TopFlux_Chla                               '
      vname_phys(2,indxTopFlux+4)=
     &     'Top flux of Chla                           '
      vname_phys(3,indxTopFlux+4)=
     &     'mg Chl-a m-2 s-1                           '
      vname_phys(4,indxTopFlux+4)=
     &     'TopFlux_Chla, scalar, series               '

      vname_phys(1,indxTopFlux+5)=
     &     'TopFlux_Phyto                              '
      vname_phys(2,indxTopFlux+5)=
     &     'Top flux of Phyto                          '
      vname_phys(3,indxTopFlux+5)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxTopFlux+5)=
     &     'TopFlux_Phyto, scalar, series              '

      vname_phys(1,indxTopFlux+6)=
     &     'TopFlux_Zoo                                '
      vname_phys(2,indxTopFlux+6)=
     &     'Top flux of Zoo                            '
      vname_phys(3,indxTopFlux+6)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxTopFlux+6)=
     &     'TopFlux_Zoo, scalar, series                '

      vname_phys(1,indxTopFlux+7)=
     &     'TopFlux_SDetN                              '
      vname_phys(2,indxTopFlux+7)=
     &     'Top flux of SDetN                          '
      vname_phys(3,indxTopFlux+7)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxTopFlux+7)=
     &     'TopFlux_SDetN, scalar, series              '

      vname_phys(1,indxTopFlux+8)=
     &     'TopFlux_LDetN                              '
      vname_phys(2,indxTopFlux+8)=
     &     'Top flux of LDetN                          '
      vname_phys(3,indxTopFlux+8)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxTopFlux+8)=
     &     'TopFlux_LDetN, scalar, series              '

#  ifdef OXYGEN
      vname_phys(1,indxTopFlux+9)=
     &     'TopFlux_O2                                 '
      vname_phys(2,indxTopFlux+9)=
     &     'Top flux of O2                             '
      vname_phys(3,indxTopFlux+9)=
     &     'mmol O2 m-2 s-1                            '
      vname_phys(4,indxTopFlux+9)=
     &     'TopFlux_O2, scalar, series                 '

#   ifdef CARBON
      vname_phys(1,indxTopFlux+10)=
     &     'TopFlux_DIC                                '
      vname_phys(2,indxTopFlux+10)=
     &     'Top flux of DIC                            '
      vname_phys(3,indxTopFlux+10)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxTopFlux+10)=
     &     'TopFlux_DIC, scalar, series                '

      vname_phys(1,indxTopFlux+11)=
     &     'TopFlux_Alk                                '
      vname_phys(2,indxTopFlux+11)=
     &     'Top flux of Alk                            '
      vname_phys(3,indxTopFlux+11)=
     &     'meq m-2 s-1                                '
      vname_phys(4,indxTopFlux+11)=
     &     'TopFlux_Alk, scalar, series                '

      vname_phys(1,indxTopFlux+12)=
     &     'TopFlux_SDetC                              '
      vname_phys(2,indxTopFlux+12)=
     &     'Top flux of SDetC                          '
      vname_phys(3,indxTopFlux+12)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxTopFlux+12)=
     &     'TopFlux_SDetC, scalar, series              '

      vname_phys(1,indxTopFlux+13)=
     &     'TopFlux_LDetC                              '
      vname_phys(2,indxTopFlux+13)=
     &     'Top flux of LDetC                          '
      vname_phys(3,indxTopFlux+13)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxTopFlux+13)=
     &     'TopFlux_LDetC, scalar, series              '

      vname_phys(1,indxTopFlux+14)=
     &     'TopFlux_CaCO3                              '
      vname_phys(2,indxTopFlux+14)=
     &     'Top flux of CaCO3                          '
      vname_phys(3,indxTopFlux+14)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxTopFlux+14)=
     &     'TopFlux_CaCO3, scalar, series              '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */
! bottom fluxes
      vname_phys(1,indxBottomFlux)=
     &     'BottomFlux_Temp                            '
      vname_phys(2,indxBottomFlux)=
     &     'Bottom flux of heat                        '
      vname_phys(3,indxBottomFlux)=
     &     'degC m-2 s-1                               '
      vname_phys(4,indxBottomFlux)=
     &     'BottomFlux_Temp, scalar, series            '

# ifdef SALINITY
      vname_phys(1,indxBottomFlux+1)=
     &     'BottomFlux_Salt                            '
      vname_phys(2,indxBottomFlux+1)=
     &     'Bottom flux of salt                        '
      vname_phys(3,indxBottomFlux+1)=
     &     'PSU m-2 s-1                                '
      vname_phys(4,indxBottomFlux+1)=
     &     'BottomFlux_Salt, scalar, series            '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxBottomFlux+2)=
     &     'BottomFlux_NO3                             '
      vname_phys(2,indxBottomFlux+2)=
     &     'Bottom flux of NO3                         '
      vname_phys(3,indxBottomFlux+2)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxBottomFlux+2)=
     &     'BottomFlux_NO3, scalar, series             '

      vname_phys(1,indxBottomFlux+3)=
     &     'BottomFlux_NH4                             '
      vname_phys(2,indxBottomFlux+3)=
     &     'Bottom flux of NH4                         '
      vname_phys(3,indxBottomFlux+3)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxBottomFlux+3)=
     &     'BottomFlux_NH4, scalar, series             '

      vname_phys(1,indxBottomFlux+4)=
     &     'BottomFlux_Chla                            '
      vname_phys(2,indxBottomFlux+4)=
     &     'Bottom flux of Chla                        '
      vname_phys(3,indxBottomFlux+4)=
     &     'mg Chl-a m-2 s-1                           '
      vname_phys(4,indxBottomFlux+4)=
     &     'BottomFlux_Chla, scalar, series            '

      vname_phys(1,indxBottomFlux+5)=
     &     'BottomFlux_Phyto                           '
      vname_phys(2,indxBottomFlux+5)=
     &     'Bottom flux of Phyto                       '
      vname_phys(3,indxBottomFlux+5)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxBottomFlux+5)=
     &     'BottomFlux_Phyto, scalar, series           '

      vname_phys(1,indxBottomFlux+6)=
     &     'BottomFlux_Zoo                             '
      vname_phys(2,indxBottomFlux+6)=
     &     'Bottom flux of Zoo                         '
      vname_phys(3,indxBottomFlux+6)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxBottomFlux+6)=
     &     'BottomFlux_Zoo, scalar, series             '

      vname_phys(1,indxBottomFlux+7)=
     &     'BottomFlux_SDetN                           '
      vname_phys(2,indxBottomFlux+7)=
     &     'Bottom flux of SDetN                       '
      vname_phys(3,indxBottomFlux+7)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxBottomFlux+7)=
     &     'BottomFlux_SDetN, scalar, series           '

      vname_phys(1,indxBottomFlux+8)=
     &     'BottomFlux_LDetN                           '
      vname_phys(2,indxBottomFlux+8)=
     &     'Bottom flux of LDetN                       '
      vname_phys(3,indxBottomFlux+8)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxBottomFlux+8)=
     &     'BottomFlux_LDetN, scalar, series           '

#  ifdef OXYGEN
      vname_phys(1,indxBottomFlux+9)=
     &     'BottomFlux_O2                              '
      vname_phys(2,indxBottomFlux+9)=
     &     'Bottom flux of O2                          '
      vname_phys(3,indxBottomFlux+9)=
     &     'mmol O2 m-2 s-1                            '
      vname_phys(4,indxBottomFlux+9)=
     &     'BottomFlux_O2, scalar, series              '

#   ifdef CARBON
      vname_phys(1,indxBottomFlux+10)=
     &     'BottomFlux_DIC                             '
      vname_phys(2,indxBottomFlux+10)=
     &     'Bottom flux of DIC                         '
      vname_phys(3,indxBottomFlux+10)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxBottomFlux+10)=
     &     'BottomFlux_DIC, scalar, series             '

      vname_phys(1,indxBottomFlux+11)=
     &     'BottomFlux_Alk                             '
      vname_phys(2,indxBottomFlux+11)=
     &     'Bottom flux of Alk                         '
      vname_phys(3,indxBottomFlux+11)=
     &     'meq m-2 s-1                                '
      vname_phys(4,indxBottomFlux+11)=
     &     'BottomFlux_Alk, scalar, series             '

      vname_phys(1,indxBottomFlux+12)=
     &     'BottomFlux_SDetC                           '
      vname_phys(2,indxBottomFlux+12)=
     &     'Bottom flux of SDetC                       '
      vname_phys(3,indxBottomFlux+12)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxBottomFlux+12)=
     &     'BottomFlux_SDetC, scalar, series           '

      vname_phys(1,indxBottomFlux+13)=
     &     'BottomFlux_LDetC                           '
      vname_phys(2,indxBottomFlux+13)=
     &     'Bottom flux of LDetC                       '
      vname_phys(3,indxBottomFlux+13)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxBottomFlux+13)=
     &     'BottomFlux_LDetC, scalar, series           '

      vname_phys(1,indxBottomFlux+14)=
     &     'BottomFlux_CaCO3                           '
      vname_phys(2,indxBottomFlux+14)=
     &     'Bottom flux of CaCO3                       '
      vname_phys(3,indxBottomFlux+14)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxBottomFlux+14)=
     &     'BottomFlux_CaCO3, scalar, series           '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */
! horizontal mixing fluxes in xi-direction
      vname_phys(1,indxHorXMixFlux)=
     &     'HorXMixFlux_Temp                           '
      vname_phys(2,indxHorXMixFlux)=
     &     'Horizontal (xi) mixing flux of heat        '
      vname_phys(3,indxHorXMixFlux)=
     &     'degC s-1                                   '
      vname_phys(4,indxHorXMixFlux)=
     &     'HorXMixFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxHorXMixFlux+1)=
     &     'HorXMixFlux_Salt                           '
      vname_phys(2,indxHorXMixFlux+1)=
     &     'Horizontal (xi) mixing flux of salt        '
      vname_phys(3,indxHorXMixFlux+1)=
     &     'PSU s-1                                    '
      vname_phys(4,indxHorXMixFlux+1)=
     &     'HorXMixFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxHorXMixFlux+2)=
     &     'HorXMixFlux_NO3                            '
      vname_phys(2,indxHorXMixFlux+2)=
     &     'Horizontal (xi) mixing flux of NO3         '
      vname_phys(3,indxHorXMixFlux+2)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+2)=
     &     'HorXMixFlux_NO3, scalar, series            '

      vname_phys(1,indxHorXMixFlux+3)=
     &     'HorXMixFlux_NH4                            '
      vname_phys(2,indxHorXMixFlux+3)=
     &     'Horizontal (xi) mixing flux of NH4         '
      vname_phys(3,indxHorXMixFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+3)=
     &     'HorXMixFlux_NH4, scalar, series            '

      vname_phys(1,indxHorXMixFlux+4)=
     &     'HorXMixFlux_Chla                           '
      vname_phys(2,indxHorXMixFlux+4)=
     &     'Horizontal (xi) mixing flux of Chla        '
      vname_phys(3,indxHorXMixFlux+4)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXMixFlux+4)=
     &     'HorXMixFlux_Chla, scalar, series           '

      vname_phys(1,indxHorXMixFlux+5)=
     &     'HorXMixFlux_Phyto                          '
      vname_phys(2,indxHorXMixFlux+5)=
     &     'Horizontal (xi) mixing flux of Phyto       '
      vname_phys(3,indxHorXMixFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+5)=
     &     'HorXMixFlux_Phyto, scalar, series          '

      vname_phys(1,indxHorXMixFlux+6)=
     &     'HorXMixFlux_Zoo                            '
      vname_phys(2,indxHorXMixFlux+6)=
     &     'Horizontal (xi) mixing flux of Zoo         '
      vname_phys(3,indxHorXMixFlux+6)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+6)=
     &     'HorXMixFlux_Zoo, scalar, series            '

      vname_phys(1,indxHorXMixFlux+7)=
     &     'HorXMixFlux_SDetN                          '
      vname_phys(2,indxHorXMixFlux+7)=
     &     'Horizontal (xi) mixing flux of SDetN       '
      vname_phys(3,indxHorXMixFlux+7)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+7)=
     &     'HorXMixFlux_SDetN, scalar, series          '

      vname_phys(1,indxHorXMixFlux+8)=
     &     'HorXMixFlux_LDetN                          '
      vname_phys(2,indxHorXMixFlux+8)=
     &     'Horizontal (xi) mixing flux of LDetN       '
      vname_phys(3,indxHorXMixFlux+8)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+8)=
     &     'HorXMixFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxHorXMixFlux+9)=
     &     'HorXMixFlux_O2                             '
      vname_phys(2,indxHorXMixFlux+9)=
     &     'Horizontal (xi) mixing flux of O2          '
      vname_phys(3,indxHorXMixFlux+9)=
     &     'mmol O2 s-1                                '
      vname_phys(4,indxHorXMixFlux+9)=
     &     'HorXMixFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxHorXMixFlux+10)=
     &     'HorXMixFlux_DIC                            '
      vname_phys(2,indxHorXMixFlux+10)=
     &     'Horizontal (xi) mixing flux of DIC         '
      vname_phys(3,indxHorXMixFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+10)=
     &     'HorXMixFlux_DIC, scalar, series            '

      vname_phys(1,indxHorXMixFlux+11)=
     &     'HorXMixFlux_Alk                            '
      vname_phys(2,indxHorXMixFlux+11)=
     &     'Horizontal (xi) mixing flux of Alk         '
      vname_phys(3,indxHorXMixFlux+11)=
     &     'meq s-1                                    '
      vname_phys(4,indxHorXMixFlux+11)=
     &     'HorXMixFlux_Alk, scalar, series            '

      vname_phys(1,indxHorXMixFlux+12)=
     &     'HorXMixFlux_SDetC                          '
      vname_phys(2,indxHorXMixFlux+12)=
     &     'Horizontal (xi) mixing flux of SDetC       '
      vname_phys(3,indxHorXMixFlux+12)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+12)=
     &     'HorXMixFlux_SDetC, scalar, series          '

      vname_phys(1,indxHorXMixFlux+13)=
     &     'HorXMixFlux_LDetC                          '
      vname_phys(2,indxHorXMixFlux+13)=
     &     'Horizontal (xi) mixing flux of LDetC       '
      vname_phys(3,indxHorXMixFlux+13)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+13)=
     &     'HorXMixFlux_LDetC, scalar, series          '

      vname_phys(1,indxHorXMixFlux+14)=
     &     'HorXMixFlux_CaCO3                          '
      vname_phys(2,indxHorXMixFlux+14)=
     &     'Horizontal (xi) mixing flux of CaCO3       '
      vname_phys(3,indxHorXMixFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+14)=
     &     'HorXMixFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

! horizontal mixing fluxes in eta-direction
      vname_phys(1,indxHorYMixFlux)=
     &     'HorYMixFlux_Temp                           '
      vname_phys(2,indxHorYMixFlux)=
     &     'Horizontal (eta) mixing flux of heat       '
      vname_phys(3,indxHorYMixFlux)=
     &     'degC s-1                                   '
      vname_phys(4,indxHorYMixFlux)=
     &     'HorYMixFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxHorYMixFlux+1)=
     &     'HorYMixFlux_Salt                           '
      vname_phys(2,indxHorYMixFlux+1)=
     &     'Horizontal (eta) mixing flux of salt       '
      vname_phys(3,indxHorYMixFlux+1)=
     &     'PSU s-1                                    '
      vname_phys(4,indxHorYMixFlux+1)=
     &     'HorYMixFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxHorYMixFlux+2)=
     &     'HorYMixFlux_NO3                            '
      vname_phys(2,indxHorYMixFlux+2)=
     &     'Horizontal (eta) mixing flux of NO3        '
      vname_phys(3,indxHorYMixFlux+2)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+2)=
     &     'HorYMixFlux_NO3, scalar, series            '

      vname_phys(1,indxHorYMixFlux+3)=
     &     'HorYMixFlux_NH4                            '
      vname_phys(2,indxHorYMixFlux+3)=
     &     'Horizontal (eta) mixing flux of NH4        '
      vname_phys(3,indxHorYMixFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+3)=
     &     'HorYMixFlux_NH4, scalar, series            '

      vname_phys(1,indxHorYMixFlux+4)=
     &     'HorYMixFlux_Chla                           '
      vname_phys(2,indxHorYMixFlux+4)=
     &     'Horizontal (eta) mixing flux of Chla       '
      vname_phys(3,indxHorYMixFlux+4)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYMixFlux+4)=
     &     'HorYMixFlux_Chla, scalar, series           '

      vname_phys(1,indxHorYMixFlux+5)=
     &     'HorYMixFlux_Phyto                          '
      vname_phys(2,indxHorYMixFlux+5)=
     &     'Horizontal (eta) mixing flux of Phyto      '
      vname_phys(3,indxHorYMixFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+5)=
     &     'HorYMixFlux_Phyto, scalar, series          '

      vname_phys(1,indxHorYMixFlux+6)=
     &     'HorYMixFlux_Zoo                            '
      vname_phys(2,indxHorYMixFlux+6)=
     &     'Horizontal (eta) mixing flux of Zoo        '
      vname_phys(3,indxHorYMixFlux+6)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+6)=
     &     'HorYMixFlux_Zoo, scalar, series            '

      vname_phys(1,indxHorYMixFlux+7)=
     &     'HorYMixFlux_SDetN                          '
      vname_phys(2,indxHorYMixFlux+7)=
     &     'Horizontal (eta) mixing flux of SDetN      '
      vname_phys(3,indxHorYMixFlux+7)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+7)=
     &     'HorYMixFlux_SDetN, scalar, series          '

      vname_phys(1,indxHorYMixFlux+8)=
     &     'HorYMixFlux_LDetN                          '
      vname_phys(2,indxHorYMixFlux+8)=
     &     'Horizontal (eta) mixing flux of LDetN      '
      vname_phys(3,indxHorYMixFlux+8)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+8)=
     &     'HorYMixFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxHorYMixFlux+9)=
     &     'HorYMixFlux_O2                             '
      vname_phys(2,indxHorYMixFlux+9)=
     &     'Horizontal (eta) mixing flux of O2         '
      vname_phys(3,indxHorYMixFlux+9)=
     &     'mmol O2 s-1                                '
      vname_phys(4,indxHorYMixFlux+9)=
     &     'HorYMixFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxHorYMixFlux+10)=
     &     'HorYMixFlux_DIC                            '
      vname_phys(2,indxHorYMixFlux+10)=
     &     'Horizontal (eta) mixing flux of DIC        '
      vname_phys(3,indxHorYMixFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+10)=
     &     'HorYMixFlux_DIC, scalar, series            '

      vname_phys(1,indxHorYMixFlux+11)=
     &     'HorYMixFlux_Alk                            '
      vname_phys(2,indxHorYMixFlux+11)=
     &     'Horizontal (eta) mixing flux of Alk        '
      vname_phys(3,indxHorYMixFlux+11)=
     &     'meq s-1                                    '
      vname_phys(4,indxHorYMixFlux+11)=
     &     'HorYMixFlux_Alk, scalar, series            '

      vname_phys(1,indxHorYMixFlux+12)=
     &     'HorYMixFlux_SDetC                          '
      vname_phys(2,indxHorYMixFlux+12)=
     &     'Horizontal (eta) mixing flux of SDetC      '
      vname_phys(3,indxHorYMixFlux+12)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+12)=
     &     'HorYMixFlux_SDetC, scalar, series          '

      vname_phys(1,indxHorYMixFlux+13)=
     &     'HorYMixFlux_LDetC                          '
      vname_phys(2,indxHorYMixFlux+13)=
     &     'Horizontal (eta) mixing flux of LDetC      '
      vname_phys(3,indxHorYMixFlux+13)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+13)=
     &     'HorYMixFlux_LDetC, scalar, series          '

      vname_phys(1,indxHorYMixFlux+14)=
     &     'HorYMixFlux_CaCO3                          '
      vname_phys(2,indxHorYMixFlux+14)=
     &     'Horizontal (eta) mixing flux of CaCO3      '
      vname_phys(3,indxHorYMixFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+14)=
     &     'HorYMixFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

! vertical mixing fluxes
      vname_phys(1,indxVertMixFlux)=
     &     'VertMixFlux_Temp                           '
      vname_phys(2,indxVertMixFlux)=
     &     'Vertical mixing flux of heat               '
      vname_phys(3,indxVertMixFlux)=
     &     'degC m-2 s-1                               '
      vname_phys(4,indxVertMixFlux)=
     &     'VertMixFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxVertMixFlux+1)=
     &     'VertMixFlux_Salt                           '
      vname_phys(2,indxVertMixFlux+1)=
     &     'Vertical mixing flux of salt               '
      vname_phys(3,indxVertMixFlux+1)=
     &     'PSU m-2 s-1                                '
      vname_phys(4,indxVertMixFlux+1)=
     &     'VertMixFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxVertMixFlux+2)=
     &     'VertMixFlux_NO3                            '
      vname_phys(2,indxVertMixFlux+2)=
     &     'Vertical mixing flux of NO3                '
      vname_phys(3,indxVertMixFlux+2)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+2)=
     &     'VertMixFlux_NO3, scalar, series            '

      vname_phys(1,indxVertMixFlux+3)=
     &     'VertMixFlux_NH4                            '
      vname_phys(2,indxVertMixFlux+3)=
     &     'Vertical mixing flux of NH4                '
      vname_phys(3,indxVertMixFlux+3)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+3)=
     &     'VertMixFlux_NH4, scalar, series            '

      vname_phys(1,indxVertMixFlux+4)=
     &     'VertMixFlux_Chla                           '
      vname_phys(2,indxVertMixFlux+4)=
     &     'Vertical mixing flux of Chla               '
      vname_phys(3,indxVertMixFlux+4)=
     &     'mg Chl-a m-2 s-1                           '
      vname_phys(4,indxVertMixFlux+4)=
     &     'VertMixFlux_Chla, scalar, series           '

      vname_phys(1,indxVertMixFlux+5)=
     &     'VertMixFlux_Phyto                          '
      vname_phys(2,indxVertMixFlux+5)=
     &     'Vertical mixing flux of Phyto              '
      vname_phys(3,indxVertMixFlux+5)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+5)=
     &     'VertMixFlux_Phyto, scalar, series          '

      vname_phys(1,indxVertMixFlux+6)=
     &     'VertMixFlux_Zoo                            '
      vname_phys(2,indxVertMixFlux+6)=
     &     'Vertical mixing flux of Zoo                '
      vname_phys(3,indxVertMixFlux+6)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+6)=
     &     'VertMixFlux_Zoo, scalar, series            '

      vname_phys(1,indxVertMixFlux+7)=
     &     'VertMixFlux_SDetN                          '
      vname_phys(2,indxVertMixFlux+7)=
     &     'Vertical mixing flux of SDetN              '
      vname_phys(3,indxVertMixFlux+7)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+7)=
     &     'VertMixFlux_SDetN, scalar, series          '

      vname_phys(1,indxVertMixFlux+8)=
     &     'VertMixFlux_LDetN                          '
      vname_phys(2,indxVertMixFlux+8)=
     &     'Vertical mixing flux of LDetN              '
      vname_phys(3,indxVertMixFlux+8)=
     &     'mmol N m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+8)=
     &     'VertMixFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxVertMixFlux+9)=
     &     'VertMixFlux_O2                             '
      vname_phys(2,indxVertMixFlux+9)=
     &     'Vertical mixing flux of O2                 '
      vname_phys(3,indxVertMixFlux+9)=
     &     'mmol O2 m-2 s-1                            '
      vname_phys(4,indxVertMixFlux+9)=
     &     'VertMixFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxVertMixFlux+10)=
     &     'VertMixFlux_DIC                            '
      vname_phys(2,indxVertMixFlux+10)=
     &     'Vertical mixing flux of DIC                '
      vname_phys(3,indxVertMixFlux+10)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+10)=
     &     'VertMixFlux_DIC, scalar, series            '

      vname_phys(1,indxVertMixFlux+11)=
     &     'VertMixFlux_Alk                            '
      vname_phys(2,indxVertMixFlux+11)=
     &     'Vertical mixing flux of Alk                '
      vname_phys(3,indxVertMixFlux+11)=
     &     'meq m-2 s-1                                '
      vname_phys(4,indxVertMixFlux+11)=
     &     'VertMixFlux_Alk, scalar, series            '

      vname_phys(1,indxVertMixFlux+12)=
     &     'VertMixFlux_SDetC                          '
      vname_phys(2,indxVertMixFlux+12)=
     &     'Vertical mixing flux of SDetC              '
      vname_phys(3,indxVertMixFlux+12)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+12)=
     &     'VertMixFlux_SDetC, scalar, series          '

      vname_phys(1,indxVertMixFlux+13)=
     &     'VertMixFlux_LDetC                          '
      vname_phys(2,indxVertMixFlux+13)=
     &     'Vertical mixing flux of LDetC              '
      vname_phys(3,indxVertMixFlux+13)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+13)=
     &     'VertMixFlux_LDetC, scalar, series          '

      vname_phys(1,indxVertMixFlux+14)=
     &     'VertMixFlux_CaCO3                          '
      vname_phys(2,indxVertMixFlux+14)=
     &     'Vertical mixing flux of CaCO3              '
      vname_phys(3,indxVertMixFlux+14)=
     &     'mmol C m-2 s-1                             '
      vname_phys(4,indxVertMixFlux+14)=
     &     'VertMixFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */

! nudging divergences
      vname_phys(1,indxNudgingFlux)=
     &     'NudgingFlux_Temp                           '
      vname_phys(2,indxNudgingFlux)=
     &     'Nudging divergence of heat                 '
      vname_phys(3,indxNudgingFlux)=
     &     'degC m-3 s-1                               '
      vname_phys(4,indxNudgingFlux)=
     &     'NudgingFlux_Temp, scalar, series           '

# ifdef SALINITY
      vname_phys(1,indxNudgingFlux+1)=
     &     'NudgingFlux_Salt                           '
      vname_phys(2,indxNudgingFlux+1)=
     &     'Nudging divergence of salt                 '
      vname_phys(3,indxNudgingFlux+1)=
     &     'PSU m-3 s-1                                '
      vname_phys(4,indxNudgingFlux+1)=
     &     'NudgingFlux_Salt, scalar, series           '
# endif /* SALINITY */
# ifdef BIOLOGY_NPZDOC
      vname_phys(1,indxNudgingFlux+2)=
     &     'NudgingFlux_NO3                            '
      vname_phys(2,indxNudgingFlux+2)=
     &     'Nudging divergence of NO3                  '
      vname_phys(3,indxNudgingFlux+2)=
     &     'mmol N m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+2)=
     &     'NudgingFlux_NO3, scalar, series            '

      vname_phys(1,indxNudgingFlux+3)=
     &     'NudgingFlux_NH4                            '
      vname_phys(2,indxNudgingFlux+3)=
     &     'Nudging divergence of NH4                  '
      vname_phys(3,indxNudgingFlux+3)=
     &     'mmol N m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+3)=
     &     'NudgingFlux_NH4, scalar, series            '

      vname_phys(1,indxNudgingFlux+4)=
     &     'NudgingFlux_Chla                           '
      vname_phys(2,indxNudgingFlux+4)=
     &     'Nudging divergence of Chla                 '
      vname_phys(3,indxNudgingFlux+4)=
     &     'mg Chl-a m-3 s-1                           '
      vname_phys(4,indxNudgingFlux+4)=
     &     'NudgingFlux_Chla, scalar, series           '

      vname_phys(1,indxNudgingFlux+5)=
     &     'NudgingFlux_Phyto                          '
      vname_phys(2,indxNudgingFlux+5)=
     &     'Nudging divergence of Phyto                '
      vname_phys(3,indxNudgingFlux+5)=
     &     'mmol N m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+5)=
     &     'NudgingFlux_Phyto, scalar, series          '

      vname_phys(1,indxNudgingFlux+6)=
     &     'NudgingFlux_Zoo                            '
      vname_phys(2,indxNudgingFlux+6)=
     &     'Nudging divergence of Zoo                  '
      vname_phys(3,indxNudgingFlux+6)=
     &     'mmol N m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+6)=
     &     'NudgingFlux_Zoo, scalar, series            '

      vname_phys(1,indxNudgingFlux+7)=
     &     'NudgingFlux_SDetN                          '
      vname_phys(2,indxNudgingFlux+7)=
     &     'Nudging divergence of SDetN                '
      vname_phys(3,indxNudgingFlux+7)=
     &     'mmol N m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+7)=
     &     'NudgingFlux_SDetN, scalar, series          '

      vname_phys(1,indxNudgingFlux+8)=
     &     'NudgingFlux_LDetN                          '
      vname_phys(2,indxNudgingFlux+8)=
     &     'Nudging divergence of LDetN                '
      vname_phys(3,indxNudgingFlux+8)=
     &     'mmol N m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+8)=
     &     'NudgingFlux_LDetN, scalar, series          '

#  ifdef OXYGEN
      vname_phys(1,indxNudgingFlux+9)=
     &     'NudgingFlux_O2                             '
      vname_phys(2,indxNudgingFlux+9)=
     &     'Nudging divergence of O2                   '
      vname_phys(3,indxNudgingFlux+9)=
     &     'mmol O2 m-3 s-1                            '
      vname_phys(4,indxNudgingFlux+9)=
     &     'NudgingFlux_O2, scalar, series             '

#   ifdef CARBON
      vname_phys(1,indxNudgingFlux+10)=
     &     'NudgingFlux_DIC                            '
      vname_phys(2,indxNudgingFlux+10)=
     &     'Nudging divergence of DIC                  '
      vname_phys(3,indxNudgingFlux+10)=
     &     'mmol C m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+10)=
     &     'NudgingFlux_DIC, scalar, series            '

      vname_phys(1,indxNudgingFlux+11)=
     &     'NudgingFlux_Alk                            '
      vname_phys(2,indxNudgingFlux+11)=
     &     'Nudging divergence of Alk                  '
      vname_phys(3,indxNudgingFlux+11)=
     &     'meq m-3 s-1                                '
      vname_phys(4,indxNudgingFlux+11)=
     &     'NudgingFlux_Alk, scalar, series            '

      vname_phys(1,indxNudgingFlux+12)=
     &     'NudgingFlux_SDetC                          '
      vname_phys(2,indxNudgingFlux+12)=
     &     'Nudging divergence of SDetC                '
      vname_phys(3,indxNudgingFlux+12)=
     &     'mmol C m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+12)=
     &     'NudgingFlux_SDetC, scalar, series          '

      vname_phys(1,indxNudgingFlux+13)=
     &     'NudgingFlux_LDetC                          '
      vname_phys(2,indxNudgingFlux+13)=
     &     'Nudging divergence of LDetC                '
      vname_phys(3,indxNudgingFlux+13)=
     &     'mmol C m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+13)=
     &     'NudgingFlux_LDetC, scalar, series          '

      vname_phys(1,indxNudgingFlux+14)=
     &     'NudgingFlux_CaCO3                          '
      vname_phys(2,indxNudgingFlux+14)=
     &     'Nudging divergence of CaCO3                '
      vname_phys(3,indxNudgingFlux+14)=
     &     'mmol C m-3 s-1                             '
      vname_phys(4,indxNudgingFlux+14)=
     &     'NudgingFlux_CaCO3, scalar, series          '
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */
# endif /* FULL_PHYS_FLUX_ANALYSIS */
#endif /* SOLVE3D */
      return
      end
#else /* PHYS_FLUX_ANALYSIS */
      subroutine init_scalars_physflux_empty()
      return
      end
#endif /* PHYS_FLUX_ANALYSIS */
