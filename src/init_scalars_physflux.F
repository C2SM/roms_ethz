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
#ifdef AVERAGES
      ncid_phys_flux_avg = -1
#endif

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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxHorXAdvFlux+2)=
     &     'HorXAdvFlux_PO4                            '
      vname_phys(2,indxHorXAdvFlux+2)=
     &     'Horizontal (xi) advective flux of PO4      '
      vname_phys(3,indxHorXAdvFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorXAdvFlux+2)=
     &     'HorXAdvFlux_PO4, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+3)=
     &     'HorXAdvFlux_NO3                            '
      vname_phys(2,indxHorXAdvFlux+3)=
     &     'Horizontal (xi) advective flux of NO3      '
      vname_phys(3,indxHorXAdvFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+3)=
     &     'HorXAdvFlux_NO3, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+4)=
     &     'HorXAdvFlux_SiO3                            '
      vname_phys(2,indxHorXAdvFlux+4)=
     &     'Horizontal (xi) advective flux of SiO3      '
      vname_phys(3,indxHorXAdvFlux+4)=
     &     'mmol Si s-1                                 '
      vname_phys(4,indxHorXAdvFlux+4)=
     &     'HorXAdvFlux_SiO3, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+5)=
     &     'HorXAdvFlux_NH4                            '
      vname_phys(2,indxHorXAdvFlux+5)=
     &     'Horizontal (xi) advective flux of NH4      '
      vname_phys(3,indxHorXAdvFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+5)=
     &     'HorXAdvFlux_NH4, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+6)=
     &     'HorXAdvFlux_Fe                            '
      vname_phys(2,indxHorXAdvFlux+6)=
     &     'Horizontal (xi) advective flux of Fe      '
      vname_phys(3,indxHorXAdvFlux+6)=
     &     'mmol Fe s-1                                 '
      vname_phys(4,indxHorXAdvFlux+6)=
     &     'HorXAdvFlux_Fe, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+7)=
     &     'HorXAdvFlux_O2                            '
      vname_phys(2,indxHorXAdvFlux+7)=
     &     'Horizontal (xi) advective flux of O2      '
      vname_phys(3,indxHorXAdvFlux+7)=
     &     'mmol O2 s-1                                 '
      vname_phys(4,indxHorXAdvFlux+7)=
     &     'HorXAdvFlux_O2, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+8)=
     &     'HorXAdvFlux_DIC                            '
      vname_phys(2,indxHorXAdvFlux+8)=
     &     'Horizontal (xi) advective flux of DIC      '
      vname_phys(3,indxHorXAdvFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+8)=
     &     'HorXAdvFlux_DIC, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+9)=
     &     'HorXAdvFlux_Alk                            '
      vname_phys(2,indxHorXAdvFlux+9)=
     &     'Horizontal (xi) advective flux of Alk      '
      vname_phys(3,indxHorXAdvFlux+9)=
     &     'mmol s-1                                    '
      vname_phys(4,indxHorXAdvFlux+9)=
     &     'HorXAdvFlux_Alk, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+10)=
     &     'HorXAdvFlux_DOC                            '
      vname_phys(2,indxHorXAdvFlux+10)=
     &     'Horizontal (xi) advective flux of DOC      '
      vname_phys(3,indxHorXAdvFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+10)=
     &     'HorXAdvFlux_DOC, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+11)=
     &     'HorXAdvFlux_SPC                            '
      vname_phys(2,indxHorXAdvFlux+11)=
     &     'Horizontal (xi) advective flux of SPC      '
      vname_phys(3,indxHorXAdvFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+11)=
     &     'HorXAdvFlux_SPC, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+12)=
     &     'HorXAdvFlux_SPChl                          '
      vname_phys(2,indxHorXAdvFlux+12)=
     &     'Horizontal (xi) advective flux of SPChl    '
      vname_phys(3,indxHorXAdvFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXAdvFlux+12)=
     &     'HorXAdvFlux_SPChl, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+13)=
     &     'HorXAdvFlux_SPCaCO3                        '
      vname_phys(2,indxHorXAdvFlux+13)=
     &     'Horizontal (xi) advective flux of SPCaCO3  '
      vname_phys(3,indxHorXAdvFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxHorXAdvFlux+13)=
     &     'HorXAdvFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxHorXAdvFlux+14)=
     &     'HorXAdvFlux_DiatC                          '
      vname_phys(2,indxHorXAdvFlux+14)=
     &     'Horizontal (xi) advective flux of DiatC    '
      vname_phys(3,indxHorXAdvFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+14)=
     &     'HorXAdvFlux_DiatC, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+15)=
     &     'HorXAdvFlux_DiatChl                        '
      vname_phys(2,indxHorXAdvFlux+15)=
     &     'Horizontal (xi) advective flux of DiatChl  '
      vname_phys(3,indxHorXAdvFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXAdvFlux+15)=
     &     'HorXAdvFlux_DiatChl, scalar, series        '

      vname_phys(1,indxHorXAdvFlux+16)=
     &     'HorXAdvFlux_ZooC                           '
      vname_phys(2,indxHorXAdvFlux+16)=
     &     'Horizontal (xi) advective flux of ZooC     '
      vname_phys(3,indxHorXAdvFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+16)=
     &     'HorXAdvFlux_ZooC, scalar, series           '

      vname_phys(1,indxHorXAdvFlux+17)=
     &     'HorXAdvFlux_SPFe                           '
      vname_phys(2,indxHorXAdvFlux+17)=
     &     'Horizontal (xi) advective flux of SPFe     '
      vname_phys(3,indxHorXAdvFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXAdvFlux+17)=
     &     'HorXAdvFlux_SPFe, scalar, series           '

      vname_phys(1,indxHorXAdvFlux+18)=
     &     'HorXAdvFlux_DiatSi                         '
      vname_phys(2,indxHorXAdvFlux+18)=
     &     'Horizontal (xi) advective flux of DiatSi   '
      vname_phys(3,indxHorXAdvFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxHorXAdvFlux+18)=
     &     'HorXAdvFlux_DiatSi, scalar, series         '

      vname_phys(1,indxHorXAdvFlux+19)=
     &     'HorXAdvFlux_DiatFe                         '
      vname_phys(2,indxHorXAdvFlux+19)=
     &     'Horizontal (xi) advective flux of DiatFe   '
      vname_phys(3,indxHorXAdvFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXAdvFlux+19)=
     &     'HorXAdvFlux_DiatFe, scalar, series         '

      vname_phys(1,indxHorXAdvFlux+20)=
     &     'HorXAdvFlux_DiazC                          '
      vname_phys(2,indxHorXAdvFlux+20)=
     &     'Horizontal (xi) advective flux of DiazC    '
      vname_phys(3,indxHorXAdvFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXAdvFlux+20)=
     &     'HorXAdvFlux_DiazC, scalar, series          '

      vname_phys(1,indxHorXAdvFlux+21)=
     &     'HorXAdvFlux_DiazChl                        '
      vname_phys(2,indxHorXAdvFlux+21)=
     &     'Horizontal (xi) advective flux of DiazChl  '
      vname_phys(3,indxHorXAdvFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXAdvFlux+21)=
     &     'HorXAdvFlux_DiazChl, scalar, series        '
#ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxHorXAdvFlux+22)=
     &     'HorXAdvFlux_DiazFe                         '
      vname_phys(2,indxHorXAdvFlux+22)=
     &     'Horizontal (xi) advective flux of DiazFe   '
      vname_phys(3,indxHorXAdvFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXAdvFlux+22)=
     &     'HorXAdvFlux_DiazFe, scalar, series         '

      vname_phys(1,indxHorXAdvFlux+23)=
     &     'HorXAdvFlux_DON                            '
      vname_phys(2,indxHorXAdvFlux+23)=
     &     'Horizontal (xi) advective flux of DON      '
      vname_phys(3,indxHorXAdvFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXAdvFlux+23)=
     &     'HorXAdvFlux_DON, scalar, series            '

      vname_phys(1,indxHorXAdvFlux+24)=
     &     'HorXAdvFlux_DOFe                           '
      vname_phys(2,indxHorXAdvFlux+24)=
     &     'Horizontal (xi) advective flux of DOFe     '
      vname_phys(3,indxHorXAdvFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXAdvFlux+24)=
     &     'HorXAdvFlux_DOFe, scalar, series           '

      vname_phys(1,indxHorXAdvFlux+25)=
     &     'HorXAdvFlux_DOP                            '
      vname_phys(2,indxHorXAdvFlux+25)=
     &     'Horizontal (xi) advective flux of DOP      '
      vname_phys(3,indxHorXAdvFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorXAdvFlux+25)=
     &     'HorXAdvFlux_DOP, scalar, series            '
#endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */


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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxHorYAdvFlux+2)=
     &     'HorYAdvFlux_PO4                            '
      vname_phys(2,indxHorYAdvFlux+2)=
     &     'Horizontal (eta) advective flux of PO4     '
      vname_phys(3,indxHorYAdvFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorYAdvFlux+2)=
     &     'HorYAdvFlux_PO4, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+3)=
     &     'HorYAdvFlux_NO3                            '
      vname_phys(2,indxHorYAdvFlux+3)=
     &     'Horizontal (eta) advective flux of NO3     '
      vname_phys(3,indxHorYAdvFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+3)=
     &     'HorYAdvFlux_NO3, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+4)=
     &     'HorYAdvFlux_SiO3                            '
      vname_phys(2,indxHorYAdvFlux+4)=
     &     'Horizontal (eta) advective flux of SiO3     '
      vname_phys(3,indxHorYAdvFlux+4)=
     &     'mmol Si s-1                                 '
      vname_phys(4,indxHorYAdvFlux+4)=
     &     'HorYAdvFlux_SiO3, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+5)=
     &     'HorYAdvFlux_NH4                            '
      vname_phys(2,indxHorYAdvFlux+5)=
     &     'Horizontal (eta) advective flux of NH4     '
      vname_phys(3,indxHorYAdvFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+5)=
     &     'HorYAdvFlux_NH4, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+6)=
     &     'HorYAdvFlux_Fe                            '
      vname_phys(2,indxHorYAdvFlux+6)=
     &     'Horizontal (eta) advective flux of Fe     '
      vname_phys(3,indxHorYAdvFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxHorYAdvFlux+6)=
     &     'HorYAdvFlux_Fe, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+7)=
     &     'HorYAdvFlux_O2                            '
      vname_phys(2,indxHorYAdvFlux+7)=
     &     'Horizontal (eta) advective flux of O2     '
      vname_phys(3,indxHorYAdvFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxHorYAdvFlux+7)=
     &     'HorYAdvFlux_O2, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+8)=
     &     'HorYAdvFlux_DIC                            '
      vname_phys(2,indxHorYAdvFlux+8)=
     &     'Horizontal (eta) advective flux of DIC     '
      vname_phys(3,indxHorYAdvFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+8)=
     &     'HorYAdvFlux_DIC, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+9)=
     &     'HorYAdvFlux_Alk                            '
      vname_phys(2,indxHorYAdvFlux+9)=
     &     'Horizontal (eta) advective flux of Alk     '
      vname_phys(3,indxHorYAdvFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxHorYAdvFlux+9)=
     &     'HorYAdvFlux_Alk, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+10)=
     &     'HorYAdvFlux_DOC                            '
      vname_phys(2,indxHorYAdvFlux+10)=
     &     'Horizontal (eta) advective flux of DOC     '
      vname_phys(3,indxHorYAdvFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+10)=
     &     'HorYAdvFlux_DOC, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+11)=
     &     'HorYAdvFlux_SPC                            '
      vname_phys(2,indxHorYAdvFlux+11)=
     &     'Horizontal (eta) advective flux of SPC     '
      vname_phys(3,indxHorYAdvFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+11)=
     &     'HorYAdvFlux_SPC, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+12)=
     &     'HorYAdvFlux_SPChl                          '
      vname_phys(2,indxHorYAdvFlux+12)=
     &     'Horizontal (eta) advective flux of SPChl   '
      vname_phys(3,indxHorYAdvFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYAdvFlux+12)=
     &     'HorYAdvFlux_SPChl, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+13)=
     &     'HorYAdvFlux_SPCaCO3                        '
      vname_phys(2,indxHorYAdvFlux+13)=
     &     'Horizontal (eta) advective flux of SPCaCO3 '
      vname_phys(3,indxHorYAdvFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxHorYAdvFlux+13)=
     &     'HorYAdvFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxHorYAdvFlux+14)=
     &     'HorYAdvFlux_DiatC                          '
      vname_phys(2,indxHorYAdvFlux+14)=
     &     'Horizontal (eta) advective flux of DiatC   '
      vname_phys(3,indxHorYAdvFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+14)=
     &     'HorYAdvFlux_DiatC, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+15)=
     &     'HorYAdvFlux_DiatChl                        '
      vname_phys(2,indxHorYAdvFlux+15)=
     &     'Horizontal (eta) advective flux of DiatChl '
      vname_phys(3,indxHorYAdvFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYAdvFlux+15)=
     &     'HorYAdvFlux_DiatChl, scalar, series        '

      vname_phys(1,indxHorYAdvFlux+16)=
     &     'HorYAdvFlux_ZooC                           '
      vname_phys(2,indxHorYAdvFlux+16)=
     &     'Horizontal (eta) advective flux of ZooC    '
      vname_phys(3,indxHorYAdvFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+16)=
     &     'HorYAdvFlux_ZooC, scalar, series           '

      vname_phys(1,indxHorYAdvFlux+17)=
     &     'HorYAdvFlux_SPFe                           '
      vname_phys(2,indxHorYAdvFlux+17)=
     &     'Horizontal (eta) advective flux of SPFe    '
      vname_phys(3,indxHorYAdvFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYAdvFlux+17)=
     &     'HorYAdvFlux_SPFe, scalar, series           '

      vname_phys(1,indxHorYAdvFlux+18)=
     &     'HorYAdvFlux_DiatSi                         '
      vname_phys(2,indxHorYAdvFlux+18)=
     &     'Horizontal (eta) advective flux of DiatSi  '
      vname_phys(3,indxHorYAdvFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxHorYAdvFlux+18)=
     &     'HorYAdvFlux_DiatSi, scalar, series         '

      vname_phys(1,indxHorYAdvFlux+19)=
     &     'HorYAdvFlux_DiatFe                         '
      vname_phys(2,indxHorYAdvFlux+19)=
     &     'Horizontal (eta) advective flux of DiatFe  '
      vname_phys(3,indxHorYAdvFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYAdvFlux+19)=
     &     'HorYAdvFlux_DiatFe, scalar, series         '

      vname_phys(1,indxHorYAdvFlux+20)=
     &     'HorYAdvFlux_DiazC                          '
      vname_phys(2,indxHorYAdvFlux+20)=
     &     'Horizontal (eta) advective flux of DiazC   '
      vname_phys(3,indxHorYAdvFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYAdvFlux+20)=
     &     'HorYAdvFlux_DiazC, scalar, series          '

      vname_phys(1,indxHorYAdvFlux+21)=
     &     'HorYAdvFlux_DiazChl                        '
      vname_phys(2,indxHorYAdvFlux+21)=
     &     'Horizontal (eta) advective flux of DiazChl '
      vname_phys(3,indxHorYAdvFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYAdvFlux+21)=
     &     'HorYAdvFlux_DiazChl, scalar, series        '
#ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxHorYAdvFlux+22)=
     &     'HorYAdvFlux_DiazFe                         '
      vname_phys(2,indxHorYAdvFlux+22)=
     &     'Horizontal (eta) advective flux of DiazFe  '
      vname_phys(3,indxHorYAdvFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYAdvFlux+22)=
     &     'HorYAdvFlux_DiazFe, scalar, series         '

      vname_phys(1,indxHorYAdvFlux+23)=
     &     'HorYAdvFlux_DON                            '
      vname_phys(2,indxHorYAdvFlux+23)=
     &     'Horizontal (eta) advective flux of DON     '
      vname_phys(3,indxHorYAdvFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYAdvFlux+23)=
     &     'HorYAdvFlux_DON, scalar, series            '

      vname_phys(1,indxHorYAdvFlux+24)=
     &     'HorYAdvFlux_DOFe                           '
      vname_phys(2,indxHorYAdvFlux+24)=
     &     'Horizontal (eta) advective flux of DOFe    '
      vname_phys(3,indxHorYAdvFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYAdvFlux+24)=
     &     'HorYAdvFlux_DOFe, scalar, series           '

      vname_phys(1,indxHorYAdvFlux+25)=
     &     'HorYAdvFlux_DOP                            '
      vname_phys(2,indxHorYAdvFlux+25)=
     &     'Horizontal (eta) advective flux of DOP     '
      vname_phys(3,indxHorYAdvFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorYAdvFlux+25)=
     &     'HorYAdvFlux_DOP, scalar, series            '
#endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */


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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxVertAdvFlux+2)=
     &     'VertAdvFlux_PO4                            '
      vname_phys(2,indxVertAdvFlux+2)=
     &     'Vertical advective flux of PO4             '
      vname_phys(3,indxVertAdvFlux+2)=
     &     'mmol P m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+2)=
     &     'VertAdvFlux_PO4, scalar, series            '

      vname_phys(1,indxVertAdvFlux+3)=
     &     'VertAdvFlux_NO3                            '
      vname_phys(2,indxVertAdvFlux+3)=
     &     'Vertical advective flux of NO3             '
      vname_phys(3,indxVertAdvFlux+3)=
     &     'mmol N m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+3)=
     &     'VertAdvFlux_NO3, scalar, series            '

      vname_phys(1,indxVertAdvFlux+4)=
     &     'VertAdvFlux_SiO3                           '
      vname_phys(2,indxVertAdvFlux+4)=
     &     'Vertical advective flux of SiO3            '
      vname_phys(3,indxVertAdvFlux+4)=
     &     'mmol Si m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+4)=
     &     'VertAdvFlux_SiO3, scalar, series           '

      vname_phys(1,indxVertAdvFlux+5)=
     &     'VertAdvFlux_NH4                            '
      vname_phys(2,indxVertAdvFlux+5)=
     &     'Vertical advective flux of NH4     '
      vname_phys(3,indxVertAdvFlux+5)=
     &     'mmol N m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+5)=
     &     'VertAdvFlux_NH4, scalar, series            '

      vname_phys(1,indxVertAdvFlux+6)=
     &     'VertAdvFlux_Fe                            '
      vname_phys(2,indxVertAdvFlux+6)=
     &     'Vertical advective flux of Fe     '
      vname_phys(3,indxVertAdvFlux+6)=
     &     'mmol Fe m-2 s-1                               '
      vname_phys(4,indxVertAdvFlux+6)=
     &     'VertAdvFlux_Fe, scalar, series            '

      vname_phys(1,indxVertAdvFlux+7)=
     &     'VertAdvFlux_O2                            '
      vname_phys(2,indxVertAdvFlux+7)=
     &     'Vertical advective flux of O2     '
      vname_phys(3,indxVertAdvFlux+7)=
     &     'mmol O2 m-2 s-1                               '
      vname_phys(4,indxVertAdvFlux+7)=
     &     'VertAdvFlux_O2, scalar, series            '

      vname_phys(1,indxVertAdvFlux+8)=
     &     'VertAdvFlux_DIC                            '
      vname_phys(2,indxVertAdvFlux+8)=
     &     'Vertical advective flux of DIC     '
      vname_phys(3,indxVertAdvFlux+8)=
     &     'mmol C m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+8)=
     &     'VertAdvFlux_DIC, scalar, series            '

      vname_phys(1,indxVertAdvFlux+9)=
     &     'VertAdvFlux_Alk                            '
      vname_phys(2,indxVertAdvFlux+9)=
     &     'Vertical advective flux of Alk     '
      vname_phys(3,indxVertAdvFlux+9)=
     &     'mmol m-2 s-1                                   '
      vname_phys(4,indxVertAdvFlux+9)=
     &     'VertAdvFlux_Alk, scalar, series            '

      vname_phys(1,indxVertAdvFlux+10)=
     &     'VertAdvFlux_DOC                            '
      vname_phys(2,indxVertAdvFlux+10)=
     &     'Vertical advective flux of DOC     '
      vname_phys(3,indxVertAdvFlux+10)=
     &     'mmol C m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+10)=
     &     'VertAdvFlux_DOC, scalar, series            '

      vname_phys(1,indxVertAdvFlux+11)=
     &     'VertAdvFlux_SPC                            '
      vname_phys(2,indxVertAdvFlux+11)=
     &     'Vertical advective flux of SPC     '
      vname_phys(3,indxVertAdvFlux+11)=
     &     'mmol C m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+11)=
     &     'VertAdvFlux_SPC, scalar, series            '

      vname_phys(1,indxVertAdvFlux+12)=
     &     'VertAdvFlux_SPChl                          '
      vname_phys(2,indxVertAdvFlux+12)=
     &     'Vertical advective flux of SPChl   '
      vname_phys(3,indxVertAdvFlux+12)=
     &     'mg Chl-a m-2 s-1                               '
      vname_phys(4,indxVertAdvFlux+12)=
     &     'VertAdvFlux_SPChl, scalar, series          '

      vname_phys(1,indxVertAdvFlux+13)=
     &     'VertAdvFlux_SPCaCO3                        '
      vname_phys(2,indxVertAdvFlux+13)=
     &     'Vertical advective flux of SPCaCO3 '
      vname_phys(3,indxVertAdvFlux+13)=
     &     'mmol CaCO3 m-2 s-1                             '
      vname_phys(4,indxVertAdvFlux+13)=
     &     'VertAdvFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxVertAdvFlux+14)=
     &     'VertAdvFlux_DiatC                          '
      vname_phys(2,indxVertAdvFlux+14)=
     &     'Vertical advective flux of DiatC   '
      vname_phys(3,indxVertAdvFlux+14)=
     &     'mmol C m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+14)=
     &     'VertAdvFlux_DiatC, scalar, series          '

      vname_phys(1,indxVertAdvFlux+15)=
     &     'VertAdvFlux_DiatChl                        '
      vname_phys(2,indxVertAdvFlux+15)=
     &     'Vertical advective flux of DiatChl '
      vname_phys(3,indxVertAdvFlux+15)=
     &     'mg Chl-a m-2 s-1                               '
      vname_phys(4,indxVertAdvFlux+15)=
     &     'VertAdvFlux_DiatChl, scalar, series        '

      vname_phys(1,indxVertAdvFlux+16)=
     &     'VertAdvFlux_ZooC                           '
      vname_phys(2,indxVertAdvFlux+16)=
     &     'Vertical advective flux of ZooC    '
      vname_phys(3,indxVertAdvFlux+16)=
     &     'mmol C m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+16)=
     &     'VertAdvFlux_ZooC, scalar, series           '

      vname_phys(1,indxVertAdvFlux+17)=
     &     'VertAdvFlux_SPFe                           '
      vname_phys(2,indxVertAdvFlux+17)=
     &     'Vertical advective flux of SPFe    '
      vname_phys(3,indxVertAdvFlux+17)=
     &     'mmol Fe m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+17)=
     &     'VertAdvFlux_SPFe, scalar, series           '

      vname_phys(1,indxVertAdvFlux+18)=
     &     'VertAdvFlux_DiatSi                         '
      vname_phys(2,indxVertAdvFlux+18)=
     &     'Vertical advective flux of DiatSi  '
      vname_phys(3,indxVertAdvFlux+18)=
     &     'mmol Si m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+18)=
     &     'VertAdvFlux_DiatSi, scalar, series         '

      vname_phys(1,indxVertAdvFlux+19)=
     &     'VertAdvFlux_DiatFe                         '
      vname_phys(2,indxVertAdvFlux+19)=
     &     'Vertical advective flux of DiatFe  '
      vname_phys(3,indxVertAdvFlux+19)=
     &     'mmol Fe m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+19)=
     &     'VertAdvFlux_DiatFe, scalar, series         '

      vname_phys(1,indxVertAdvFlux+20)=
     &     'VertAdvFlux_DiazC                          '
      vname_phys(2,indxVertAdvFlux+20)=
     &     'Vertical advective flux of DiazC   '
      vname_phys(3,indxVertAdvFlux+20)=
     &     'mmol C m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+20)=
     &     'VertAdvFlux_DiazC, scalar, series          '

      vname_phys(1,indxVertAdvFlux+21)=
     &     'VertAdvFlux_DiazChl                        '
      vname_phys(2,indxVertAdvFlux+21)=
     &     'Vertical advective flux of DiazChl '
      vname_phys(3,indxVertAdvFlux+21)=
     &     'mg Chl-a m-2 s-1                               '
      vname_phys(4,indxVertAdvFlux+21)=
     &     'VertAdvFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxVertAdvFlux+22)=
     &     'VertAdvFlux_DiazFe                         '
      vname_phys(2,indxVertAdvFlux+22)=
     &     'Vertical advective flux of DiazFe  '
      vname_phys(3,indxVertAdvFlux+22)=
     &     'mmol Fe m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+22)=
     &     'VertAdvFlux_DiazFe, scalar, series         '

      vname_phys(1,indxVertAdvFlux+23)=
     &     'VertAdvFlux_DON                            '
      vname_phys(2,indxVertAdvFlux+23)=
     &     'Vertical advective flux of DON     '
      vname_phys(3,indxVertAdvFlux+23)=
     &     'mmol N m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+23)=
     &     'VertAdvFlux_DON, scalar, series            '

      vname_phys(1,indxVertAdvFlux+24)=
     &     'VertAdvFlux_DOFe                           '
      vname_phys(2,indxVertAdvFlux+24)=
     &     'Vertical advective flux of DOFe    '
      vname_phys(3,indxVertAdvFlux+24)=
     &     'mmol Fe m-2 s-1                                '
      vname_phys(4,indxVertAdvFlux+24)=
     &     'VertAdvFlux_DOFe, scalar, series           '

      vname_phys(1,indxVertAdvFlux+25)=
     &     'VertAdvFlux_DOP                            '
      vname_phys(2,indxVertAdvFlux+25)=
     &     'Vertical advective flux of DOP     '
      vname_phys(3,indxVertAdvFlux+25)=
     &     'mmol P m-2 s-1                                 '
      vname_phys(4,indxVertAdvFlux+25)=
     &     'VertAdvFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */


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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxVertDiffFlux+2)=
     &     'VertDiffFlux_PO4                            '
      vname_phys(2,indxVertDiffFlux+2)=
     &     'Vertical diffusive flux of PO4             '
      vname_phys(3,indxVertDiffFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxVertDiffFlux+2)=
     &     'VertDiffFlux_PO4, scalar, series            '

      vname_phys(1,indxVertDiffFlux+3)=
     &     'VertDiffFlux_NO3                            '
      vname_phys(2,indxVertDiffFlux+3)=
     &     'Vertical diffusive flux of NO3             '
      vname_phys(3,indxVertDiffFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxVertDiffFlux+3)=
     &     'VertDiffFlux_NO3, scalar, series            '

      vname_phys(1,indxVertDiffFlux+4)=
     &     'VertDiffFlux_SiO3                           '
      vname_phys(2,indxVertDiffFlux+4)=
     &     'Vertical diffusive flux of SiO3            '
      vname_phys(3,indxVertDiffFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxVertDiffFlux+4)=
     &     'VertDiffFlux_SiO3, scalar, series           '

      vname_phys(1,indxVertDiffFlux+5)=
     &     'VertDiffFlux_NH4                            '
      vname_phys(2,indxVertDiffFlux+5)=
     &     'Vertical diffusive flux of NH4     '
      vname_phys(3,indxVertDiffFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxVertDiffFlux+5)=
     &     'VertDiffFlux_NH4, scalar, series            '

      vname_phys(1,indxVertDiffFlux+6)=
     &     'VertDiffFlux_Fe                            '
      vname_phys(2,indxVertDiffFlux+6)=
     &     'Vertical diffusive flux of Fe     '
      vname_phys(3,indxVertDiffFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxVertDiffFlux+6)=
     &     'VertDiffFlux_Fe, scalar, series            '

      vname_phys(1,indxVertDiffFlux+7)=
     &     'VertDiffFlux_O2                            '
      vname_phys(2,indxVertDiffFlux+7)=
     &     'Vertical diffusive flux of O2     '
      vname_phys(3,indxVertDiffFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxVertDiffFlux+7)=
     &     'VertDiffFlux_O2, scalar, series            '

      vname_phys(1,indxVertDiffFlux+8)=
     &     'VertDiffFlux_DIC                            '
      vname_phys(2,indxVertDiffFlux+8)=
     &     'Vertical diffusive flux of DIC     '
      vname_phys(3,indxVertDiffFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertDiffFlux+8)=
     &     'VertDiffFlux_DIC, scalar, series            '

      vname_phys(1,indxVertDiffFlux+9)=
     &     'VertDiffFlux_Alk                            '
      vname_phys(2,indxVertDiffFlux+9)=
     &     'Vertical diffusive flux of Alk     '
      vname_phys(3,indxVertDiffFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxVertDiffFlux+9)=
     &     'VertDiffFlux_Alk, scalar, series            '

      vname_phys(1,indxVertDiffFlux+10)=
     &     'VertDiffFlux_DOC                            '
      vname_phys(2,indxVertDiffFlux+10)=
     &     'Vertical diffusive flux of DOC     '
      vname_phys(3,indxVertDiffFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertDiffFlux+10)=
     &     'VertDiffFlux_DOC, scalar, series            '

      vname_phys(1,indxVertDiffFlux+11)=
     &     'VertDiffFlux_SPC                            '
      vname_phys(2,indxVertDiffFlux+11)=
     &     'Vertical diffusive flux of SPC     '
      vname_phys(3,indxVertDiffFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertDiffFlux+11)=
     &     'VertDiffFlux_SPC, scalar, series            '

      vname_phys(1,indxVertDiffFlux+12)=
     &     'VertDiffFlux_SPChl                          '
      vname_phys(2,indxVertDiffFlux+12)=
     &     'Vertical diffusive flux of SPChl   '
      vname_phys(3,indxVertDiffFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxVertDiffFlux+12)=
     &     'VertDiffFlux_SPChl, scalar, series          '

      vname_phys(1,indxVertDiffFlux+13)=
     &     'VertDiffFlux_SPCaCO3                        '
      vname_phys(2,indxVertDiffFlux+13)=
     &     'Vertical diffusive flux of SPCaCO3 '
      vname_phys(3,indxVertDiffFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxVertDiffFlux+13)=
     &     'VertDiffFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxVertDiffFlux+14)=
     &     'VertDiffFlux_DiatC                          '
      vname_phys(2,indxVertDiffFlux+14)=
     &     'Vertical diffusive flux of DiatC   '
      vname_phys(3,indxVertDiffFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertDiffFlux+14)=
     &     'VertDiffFlux_DiatC, scalar, series          '

      vname_phys(1,indxVertDiffFlux+15)=
     &     'VertDiffFlux_DiatChl                        '
      vname_phys(2,indxVertDiffFlux+15)=
     &     'Vertical diffusive flux of DiatChl '
      vname_phys(3,indxVertDiffFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxVertDiffFlux+15)=
     &     'VertDiffFlux_DiatChl, scalar, series        '

      vname_phys(1,indxVertDiffFlux+16)=
     &     'VertDiffFlux_ZooC                           '
      vname_phys(2,indxVertDiffFlux+16)=
     &     'Vertical diffusive flux of ZooC    '
      vname_phys(3,indxVertDiffFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertDiffFlux+16)=
     &     'VertDiffFlux_ZooC, scalar, series           '

      vname_phys(1,indxVertDiffFlux+17)=
     &     'VertDiffFlux_SPFe                           '
      vname_phys(2,indxVertDiffFlux+17)=
     &     'Vertical diffusive flux of SPFe    '
      vname_phys(3,indxVertDiffFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertDiffFlux+17)=
     &     'VertDiffFlux_SPFe, scalar, series           '

      vname_phys(1,indxVertDiffFlux+18)=
     &     'VertDiffFlux_DiatSi                         '
      vname_phys(2,indxVertDiffFlux+18)=
     &     'Vertical diffusive flux of DiatSi  '
      vname_phys(3,indxVertDiffFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxVertDiffFlux+18)=
     &     'VertDiffFlux_DiatSi, scalar, series         '

      vname_phys(1,indxVertDiffFlux+19)=
     &     'VertDiffFlux_DiatFe                         '
      vname_phys(2,indxVertDiffFlux+19)=
     &     'Vertical diffusive flux of DiatFe  '
      vname_phys(3,indxVertDiffFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertDiffFlux+19)=
     &     'VertDiffFlux_DiatFe, scalar, series         '

      vname_phys(1,indxVertDiffFlux+20)=
     &     'VertDiffFlux_DiazC                          '
      vname_phys(2,indxVertDiffFlux+20)=
     &     'Vertical diffusive flux of DiazC   '
      vname_phys(3,indxVertDiffFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertDiffFlux+20)=
     &     'VertDiffFlux_DiazC, scalar, series          '

      vname_phys(1,indxVertDiffFlux+21)=
     &     'VertDiffFlux_DiazChl                        '
      vname_phys(2,indxVertDiffFlux+21)=
     &     'Vertical diffusive flux of DiazChl '
      vname_phys(3,indxVertDiffFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxVertDiffFlux+21)=
     &     'VertDiffFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxVertDiffFlux+22)=
     &     'VertDiffFlux_DiazFe                         '
      vname_phys(2,indxVertDiffFlux+22)=
     &     'Vertical diffusive flux of DiazFe  '
      vname_phys(3,indxVertDiffFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertDiffFlux+22)=
     &     'VertDiffFlux_DiazFe, scalar, series         '

      vname_phys(1,indxVertDiffFlux+23)=
     &     'VertDiffFlux_DON                            '
      vname_phys(2,indxVertDiffFlux+23)=
     &     'Vertical diffusive flux of DON     '
      vname_phys(3,indxVertDiffFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxVertDiffFlux+23)=
     &     'VertDiffFlux_DON, scalar, series            '

      vname_phys(1,indxVertDiffFlux+24)=
     &     'VertDiffFlux_DOFe                           '
      vname_phys(2,indxVertDiffFlux+24)=
     &     'Vertical diffusive flux of DOFe    '
      vname_phys(3,indxVertDiffFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertDiffFlux+24)=
     &     'VertDiffFlux_DOFe, scalar, series           '

      vname_phys(1,indxVertDiffFlux+25)=
     &     'VertDiffFlux_DOP                            '
      vname_phys(2,indxVertDiffFlux+25)=
     &     'Vertical diffusive flux of DOP     '
      vname_phys(3,indxVertDiffFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxVertDiffFlux+25)=
     &     'VertDiffFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */

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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxTopFlux+2)=
     &     'TopFlux_PO4                            '
      vname_phys(2,indxTopFlux+2)=
     &     'Top diffusive flux of PO4             '
      vname_phys(3,indxTopFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxTopFlux+2)=
     &     'TopFlux_PO4, scalar, series            '

      vname_phys(1,indxTopFlux+3)=
     &     'TopFlux_NO3                            '
      vname_phys(2,indxTopFlux+3)=
     &     'Top diffusive flux of NO3             '
      vname_phys(3,indxTopFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxTopFlux+3)=
     &     'TopFlux_NO3, scalar, series            '

      vname_phys(1,indxTopFlux+4)=
     &     'TopFlux_SiO3                           '
      vname_phys(2,indxTopFlux+4)=
     &     'Top diffusive flux of SiO3            '
      vname_phys(3,indxTopFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxTopFlux+4)=
     &     'TopFlux_SiO3, scalar, series           '

      vname_phys(1,indxTopFlux+5)=
     &     'TopFlux_NH4                            '
      vname_phys(2,indxTopFlux+5)=
     &     'Top diffusive flux of NH4     '
      vname_phys(3,indxTopFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxTopFlux+5)=
     &     'TopFlux_NH4, scalar, series            '

      vname_phys(1,indxTopFlux+6)=
     &     'TopFlux_Fe                            '
      vname_phys(2,indxTopFlux+6)=
     &     'Top diffusive flux of Fe     '
      vname_phys(3,indxTopFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxTopFlux+6)=
     &     'TopFlux_Fe, scalar, series            '

      vname_phys(1,indxTopFlux+7)=
     &     'TopFlux_O2                            '
      vname_phys(2,indxTopFlux+7)=
     &     'Top diffusive flux of O2     '
      vname_phys(3,indxTopFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxTopFlux+7)=
     &     'TopFlux_O2, scalar, series            '

      vname_phys(1,indxTopFlux+8)=
     &     'TopFlux_DIC                            '
      vname_phys(2,indxTopFlux+8)=
     &     'Top diffusive flux of DIC     '
      vname_phys(3,indxTopFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxTopFlux+8)=
     &     'TopFlux_DIC, scalar, series            '

      vname_phys(1,indxTopFlux+9)=
     &     'TopFlux_Alk                            '
      vname_phys(2,indxTopFlux+9)=
     &     'Top diffusive flux of Alk     '
      vname_phys(3,indxTopFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxTopFlux+9)=
     &     'TopFlux_Alk, scalar, series            '

      vname_phys(1,indxTopFlux+10)=
     &     'TopFlux_DOC                            '
      vname_phys(2,indxTopFlux+10)=
     &     'Top diffusive flux of DOC     '
      vname_phys(3,indxTopFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxTopFlux+10)=
     &     'TopFlux_DOC, scalar, series            '

      vname_phys(1,indxTopFlux+11)=
     &     'TopFlux_SPC                            '
      vname_phys(2,indxTopFlux+11)=
     &     'Top diffusive flux of SPC     '
      vname_phys(3,indxTopFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxTopFlux+11)=
     &     'TopFlux_SPC, scalar, series            '

      vname_phys(1,indxTopFlux+12)=
     &     'TopFlux_SPChl                          '
      vname_phys(2,indxTopFlux+12)=
     &     'Top diffusive flux of SPChl   '
      vname_phys(3,indxTopFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxTopFlux+12)=
     &     'TopFlux_SPChl, scalar, series          '

      vname_phys(1,indxTopFlux+13)=
     &     'TopFlux_SPCaCO3                        '
      vname_phys(2,indxTopFlux+13)=
     &     'Top diffusive flux of SPCaCO3 '
      vname_phys(3,indxTopFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxTopFlux+13)=
     &     'TopFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxTopFlux+14)=
     &     'TopFlux_DiatC                          '
      vname_phys(2,indxTopFlux+14)=
     &     'Top diffusive flux of DiatC   '
      vname_phys(3,indxTopFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxTopFlux+14)=
     &     'TopFlux_DiatC, scalar, series          '

      vname_phys(1,indxTopFlux+15)=
     &     'TopFlux_DiatChl                        '
      vname_phys(2,indxTopFlux+15)=
     &     'Top diffusive flux of DiatChl '
      vname_phys(3,indxTopFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxTopFlux+15)=
     &     'TopFlux_DiatChl, scalar, series        '

      vname_phys(1,indxTopFlux+16)=
     &     'TopFlux_ZooC                           '
      vname_phys(2,indxTopFlux+16)=
     &     'Top diffusive flux of ZooC    '
      vname_phys(3,indxTopFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxTopFlux+16)=
     &     'TopFlux_ZooC, scalar, series           '

      vname_phys(1,indxTopFlux+17)=
     &     'TopFlux_SPFe                           '
      vname_phys(2,indxTopFlux+17)=
     &     'Top diffusive flux of SPFe    '
      vname_phys(3,indxTopFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxTopFlux+17)=
     &     'TopFlux_SPFe, scalar, series           '

      vname_phys(1,indxTopFlux+18)=
     &     'TopFlux_DiatSi                         '
      vname_phys(2,indxTopFlux+18)=
     &     'Top diffusive flux of DiatSi  '
      vname_phys(3,indxTopFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxTopFlux+18)=
     &     'TopFlux_DiatSi, scalar, series         '

      vname_phys(1,indxTopFlux+19)=
     &     'TopFlux_DiatFe                         '
      vname_phys(2,indxTopFlux+19)=
     &     'Top diffusive flux of DiatFe  '
      vname_phys(3,indxTopFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxTopFlux+19)=
     &     'TopFlux_DiatFe, scalar, series         '

      vname_phys(1,indxTopFlux+20)=
     &     'TopFlux_DiazC                          '
      vname_phys(2,indxTopFlux+20)=
     &     'Top diffusive flux of DiazC   '
      vname_phys(3,indxTopFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxTopFlux+20)=
     &     'TopFlux_DiazC, scalar, series          '

      vname_phys(1,indxTopFlux+21)=
     &     'TopFlux_DiazChl                        '
      vname_phys(2,indxTopFlux+21)=
     &     'Top diffusive flux of DiazChl '
      vname_phys(3,indxTopFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxTopFlux+21)=
     &     'TopFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxTopFlux+22)=
     &     'TopFlux_DiazFe                         '
      vname_phys(2,indxTopFlux+22)=
     &     'Top diffusive flux of DiazFe  '
      vname_phys(3,indxTopFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxTopFlux+22)=
     &     'TopFlux_DiazFe, scalar, series         '

      vname_phys(1,indxTopFlux+23)=
     &     'TopFlux_DON                            '
      vname_phys(2,indxTopFlux+23)=
     &     'Top diffusive flux of DON     '
      vname_phys(3,indxTopFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxTopFlux+23)=
     &     'TopFlux_DON, scalar, series            '

      vname_phys(1,indxTopFlux+24)=
     &     'TopFlux_DOFe                           '
      vname_phys(2,indxTopFlux+24)=
     &     'Top diffusive flux of DOFe    '
      vname_phys(3,indxTopFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxTopFlux+24)=
     &     'TopFlux_DOFe, scalar, series           '

      vname_phys(1,indxTopFlux+25)=
     &     'TopFlux_DOP                            '
      vname_phys(2,indxTopFlux+25)=
     &     'Top diffusive flux of DOP     '
      vname_phys(3,indxTopFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxTopFlux+25)=
     &     'TopFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */
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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxBottomFlux+2)=
     &     'BottomFlux_PO4                            '
      vname_phys(2,indxBottomFlux+2)=
     &     'Bottom diffusive flux of PO4             '
      vname_phys(3,indxBottomFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxBottomFlux+2)=
     &     'BottomFlux_PO4, scalar, series            '

      vname_phys(1,indxBottomFlux+3)=
     &     'BottomFlux_NO3                            '
      vname_phys(2,indxBottomFlux+3)=
     &     'Bottom diffusive flux of NO3             '
      vname_phys(3,indxBottomFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxBottomFlux+3)=
     &     'BottomFlux_NO3, scalar, series            '

      vname_phys(1,indxBottomFlux+4)=
     &     'BottomFlux_SiO3                           '
      vname_phys(2,indxBottomFlux+4)=
     &     'Bottom diffusive flux of SiO3            '
      vname_phys(3,indxBottomFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxBottomFlux+4)=
     &     'BottomFlux_SiO3, scalar, series           '

      vname_phys(1,indxBottomFlux+5)=
     &     'BottomFlux_NH4                            '
      vname_phys(2,indxBottomFlux+5)=
     &     'Bottom diffusive flux of NH4     '
      vname_phys(3,indxBottomFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxBottomFlux+5)=
     &     'BottomFlux_NH4, scalar, series            '

      vname_phys(1,indxBottomFlux+6)=
     &     'BottomFlux_Fe                            '
      vname_phys(2,indxBottomFlux+6)=
     &     'Bottom diffusive flux of Fe     '
      vname_phys(3,indxBottomFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxBottomFlux+6)=
     &     'BottomFlux_Fe, scalar, series            '

      vname_phys(1,indxBottomFlux+7)=
     &     'BottomFlux_O2                            '
      vname_phys(2,indxBottomFlux+7)=
     &     'Bottom diffusive flux of O2     '
      vname_phys(3,indxBottomFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxBottomFlux+7)=
     &     'BottomFlux_O2, scalar, series            '

      vname_phys(1,indxBottomFlux+8)=
     &     'BottomFlux_DIC                            '
      vname_phys(2,indxBottomFlux+8)=
     &     'Bottom diffusive flux of DIC     '
      vname_phys(3,indxBottomFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxBottomFlux+8)=
     &     'BottomFlux_DIC, scalar, series            '

      vname_phys(1,indxBottomFlux+9)=
     &     'BottomFlux_Alk                            '
      vname_phys(2,indxBottomFlux+9)=
     &     'Bottom diffusive flux of Alk     '
      vname_phys(3,indxBottomFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxBottomFlux+9)=
     &     'BottomFlux_Alk, scalar, series            '

      vname_phys(1,indxBottomFlux+10)=
     &     'BottomFlux_DOC                            '
      vname_phys(2,indxBottomFlux+10)=
     &     'Bottom diffusive flux of DOC     '
      vname_phys(3,indxBottomFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxBottomFlux+10)=
     &     'BottomFlux_DOC, scalar, series            '

      vname_phys(1,indxBottomFlux+11)=
     &     'BottomFlux_SPC                            '
      vname_phys(2,indxBottomFlux+11)=
     &     'Bottom diffusive flux of SPC     '
      vname_phys(3,indxBottomFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxBottomFlux+11)=
     &     'BottomFlux_SPC, scalar, series            '

      vname_phys(1,indxBottomFlux+12)=
     &     'BottomFlux_SPChl                          '
      vname_phys(2,indxBottomFlux+12)=
     &     'Bottom diffusive flux of SPChl   '
      vname_phys(3,indxBottomFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxBottomFlux+12)=
     &     'BottomFlux_SPChl, scalar, series          '

      vname_phys(1,indxBottomFlux+13)=
     &     'BottomFlux_SPCaCO3                        '
      vname_phys(2,indxBottomFlux+13)=
     &     'Bottom diffusive flux of SPCaCO3 '
      vname_phys(3,indxBottomFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxBottomFlux+13)=
     &     'BottomFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxBottomFlux+14)=
     &     'BottomFlux_DiatC                          '
      vname_phys(2,indxBottomFlux+14)=
     &     'Bottom diffusive flux of DiatC   '
      vname_phys(3,indxBottomFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxBottomFlux+14)=
     &     'BottomFlux_DiatC, scalar, series          '

      vname_phys(1,indxBottomFlux+15)=
     &     'BottomFlux_DiatChl                        '
      vname_phys(2,indxBottomFlux+15)=
     &     'Bottom diffusive flux of DiatChl '
      vname_phys(3,indxBottomFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxBottomFlux+15)=
     &     'BottomFlux_DiatChl, scalar, series        '

      vname_phys(1,indxBottomFlux+16)=
     &     'BottomFlux_ZooC                           '
      vname_phys(2,indxBottomFlux+16)=
     &     'Bottom diffusive flux of ZooC    '
      vname_phys(3,indxBottomFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxBottomFlux+16)=
     &     'BottomFlux_ZooC, scalar, series           '

      vname_phys(1,indxBottomFlux+17)=
     &     'BottomFlux_SPFe                           '
      vname_phys(2,indxBottomFlux+17)=
     &     'Bottom diffusive flux of SPFe    '
      vname_phys(3,indxBottomFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxBottomFlux+17)=
     &     'BottomFlux_SPFe, scalar, series           '

      vname_phys(1,indxBottomFlux+18)=
     &     'BottomFlux_DiatSi                         '
      vname_phys(2,indxBottomFlux+18)=
     &     'Bottom diffusive flux of DiatSi  '
      vname_phys(3,indxBottomFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxBottomFlux+18)=
     &     'BottomFlux_DiatSi, scalar, series         '

      vname_phys(1,indxBottomFlux+19)=
     &     'BottomFlux_DiatFe                         '
      vname_phys(2,indxBottomFlux+19)=
     &     'Bottom diffusive flux of DiatFe  '
      vname_phys(3,indxBottomFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxBottomFlux+19)=
     &     'BottomFlux_DiatFe, scalar, series         '

      vname_phys(1,indxBottomFlux+20)=
     &     'BottomFlux_DiazC                          '
      vname_phys(2,indxBottomFlux+20)=
     &     'Bottom diffusive flux of DiazC   '
      vname_phys(3,indxBottomFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxBottomFlux+20)=
     &     'BottomFlux_DiazC, scalar, series          '

      vname_phys(1,indxBottomFlux+21)=
     &     'BottomFlux_DiazChl                        '
      vname_phys(2,indxBottomFlux+21)=
     &     'Bottom diffusive flux of DiazChl '
      vname_phys(3,indxBottomFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxBottomFlux+21)=
     &     'BottomFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxBottomFlux+22)=
     &     'BottomFlux_DiazFe                         '
      vname_phys(2,indxBottomFlux+22)=
     &     'Bottom diffusive flux of DiazFe  '
      vname_phys(3,indxBottomFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxBottomFlux+22)=
     &     'BottomFlux_DiazFe, scalar, series         '

      vname_phys(1,indxBottomFlux+23)=
     &     'BottomFlux_DON                            '
      vname_phys(2,indxBottomFlux+23)=
     &     'Bottom diffusive flux of DON     '
      vname_phys(3,indxBottomFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxBottomFlux+23)=
     &     'BottomFlux_DON, scalar, series            '

      vname_phys(1,indxBottomFlux+24)=
     &     'BottomFlux_DOFe                           '
      vname_phys(2,indxBottomFlux+24)=
     &     'Bottom diffusive flux of DOFe    '
      vname_phys(3,indxBottomFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxBottomFlux+24)=
     &     'BottomFlux_DOFe, scalar, series           '

      vname_phys(1,indxBottomFlux+25)=
     &     'BottomFlux_DOP                            '
      vname_phys(2,indxBottomFlux+25)=
     &     'Bottom diffusive flux of DOP     '
      vname_phys(3,indxBottomFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxBottomFlux+25)=
     &     'BottomFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */

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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxHorXMixFlux+2)=
     &     'HorXMixFlux_PO4                            '
      vname_phys(2,indxHorXMixFlux+2)=
     &     'Horizontal (xi) mixing flux of PO4             '
      vname_phys(3,indxHorXMixFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorXMixFlux+2)=
     &     'HorXMixFlux_PO4, scalar, series            '

      vname_phys(1,indxHorXMixFlux+3)=
     &     'HorXMixFlux_NO3                            '
      vname_phys(2,indxHorXMixFlux+3)=
     &     'Horizontal (xi) mixing flux of NO3             '
      vname_phys(3,indxHorXMixFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+3)=
     &     'HorXMixFlux_NO3, scalar, series            '

      vname_phys(1,indxHorXMixFlux+4)=
     &     'HorXMixFlux_SiO3                           '
      vname_phys(2,indxHorXMixFlux+4)=
     &     'Horizontal (xi) mixing flux of SiO3            '
      vname_phys(3,indxHorXMixFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxHorXMixFlux+4)=
     &     'HorXMixFlux_SiO3, scalar, series           '

      vname_phys(1,indxHorXMixFlux+5)=
     &     'HorXMixFlux_NH4                            '
      vname_phys(2,indxHorXMixFlux+5)=
     &     'Horizontal (xi) mixing flux of NH4     '
      vname_phys(3,indxHorXMixFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+5)=
     &     'HorXMixFlux_NH4, scalar, series            '

      vname_phys(1,indxHorXMixFlux+6)=
     &     'HorXMixFlux_Fe                            '
      vname_phys(2,indxHorXMixFlux+6)=
     &     'Horizontal (xi) mixing flux of Fe     '
      vname_phys(3,indxHorXMixFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxHorXMixFlux+6)=
     &     'HorXMixFlux_Fe, scalar, series            '

      vname_phys(1,indxHorXMixFlux+7)=
     &     'HorXMixFlux_O2                            '
      vname_phys(2,indxHorXMixFlux+7)=
     &     'Horizontal (xi) mixing flux of O2     '
      vname_phys(3,indxHorXMixFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxHorXMixFlux+7)=
     &     'HorXMixFlux_O2, scalar, series            '

      vname_phys(1,indxHorXMixFlux+8)=
     &     'HorXMixFlux_DIC                            '
      vname_phys(2,indxHorXMixFlux+8)=
     &     'Horizontal (xi) mixing flux of DIC     '
      vname_phys(3,indxHorXMixFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+8)=
     &     'HorXMixFlux_DIC, scalar, series            '

      vname_phys(1,indxHorXMixFlux+9)=
     &     'HorXMixFlux_Alk                            '
      vname_phys(2,indxHorXMixFlux+9)=
     &     'Horizontal (xi) mixing flux of Alk     '
      vname_phys(3,indxHorXMixFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxHorXMixFlux+9)=
     &     'HorXMixFlux_Alk, scalar, series            '

      vname_phys(1,indxHorXMixFlux+10)=
     &     'HorXMixFlux_DOC                            '
      vname_phys(2,indxHorXMixFlux+10)=
     &     'Horizontal (xi) mixing flux of DOC     '
      vname_phys(3,indxHorXMixFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+10)=
     &     'HorXMixFlux_DOC, scalar, series            '

      vname_phys(1,indxHorXMixFlux+11)=
     &     'HorXMixFlux_SPC                            '
      vname_phys(2,indxHorXMixFlux+11)=
     &     'Horizontal (xi) mixing flux of SPC     '
      vname_phys(3,indxHorXMixFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+11)=
     &     'HorXMixFlux_SPC, scalar, series            '

      vname_phys(1,indxHorXMixFlux+12)=
     &     'HorXMixFlux_SPChl                          '
      vname_phys(2,indxHorXMixFlux+12)=
     &     'Horizontal (xi) mixing flux of SPChl   '
      vname_phys(3,indxHorXMixFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXMixFlux+12)=
     &     'HorXMixFlux_SPChl, scalar, series          '

      vname_phys(1,indxHorXMixFlux+13)=
     &     'HorXMixFlux_SPCaCO3                        '
      vname_phys(2,indxHorXMixFlux+13)=
     &     'Horizontal (xi) mixing flux of SPCaCO3 '
      vname_phys(3,indxHorXMixFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxHorXMixFlux+13)=
     &     'HorXMixFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxHorXMixFlux+14)=
     &     'HorXMixFlux_DiatC                          '
      vname_phys(2,indxHorXMixFlux+14)=
     &     'Horizontal (xi) mixing flux of DiatC   '
      vname_phys(3,indxHorXMixFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+14)=
     &     'HorXMixFlux_DiatC, scalar, series          '

      vname_phys(1,indxHorXMixFlux+15)=
     &     'HorXMixFlux_DiatChl                        '
      vname_phys(2,indxHorXMixFlux+15)=
     &     'Horizontal (xi) mixing flux of DiatChl '
      vname_phys(3,indxHorXMixFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXMixFlux+15)=
     &     'HorXMixFlux_DiatChl, scalar, series        '

      vname_phys(1,indxHorXMixFlux+16)=
     &     'HorXMixFlux_ZooC                           '
      vname_phys(2,indxHorXMixFlux+16)=
     &     'Horizontal (xi) mixing flux of ZooC    '
      vname_phys(3,indxHorXMixFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+16)=
     &     'HorXMixFlux_ZooC, scalar, series           '

      vname_phys(1,indxHorXMixFlux+17)=
     &     'HorXMixFlux_SPFe                           '
      vname_phys(2,indxHorXMixFlux+17)=
     &     'Horizontal (xi) mixing flux of SPFe    '
      vname_phys(3,indxHorXMixFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXMixFlux+17)=
     &     'HorXMixFlux_SPFe, scalar, series           '

      vname_phys(1,indxHorXMixFlux+18)=
     &     'HorXMixFlux_DiatSi                         '
      vname_phys(2,indxHorXMixFlux+18)=
     &     'Horizontal (xi) mixing flux of DiatSi  '
      vname_phys(3,indxHorXMixFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxHorXMixFlux+18)=
     &     'HorXMixFlux_DiatSi, scalar, series         '

      vname_phys(1,indxHorXMixFlux+19)=
     &     'HorXMixFlux_DiatFe                         '
      vname_phys(2,indxHorXMixFlux+19)=
     &     'Horizontal (xi) mixing flux of DiatFe  '
      vname_phys(3,indxHorXMixFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXMixFlux+19)=
     &     'HorXMixFlux_DiatFe, scalar, series         '

      vname_phys(1,indxHorXMixFlux+20)=
     &     'HorXMixFlux_DiazC                          '
      vname_phys(2,indxHorXMixFlux+20)=
     &     'Horizontal (xi) mixing flux of DiazC   '
      vname_phys(3,indxHorXMixFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorXMixFlux+20)=
     &     'HorXMixFlux_DiazC, scalar, series          '

      vname_phys(1,indxHorXMixFlux+21)=
     &     'HorXMixFlux_DiazChl                        '
      vname_phys(2,indxHorXMixFlux+21)=
     &     'Horizontal (xi) mixing flux of DiazChl '
      vname_phys(3,indxHorXMixFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorXMixFlux+21)=
     &     'HorXMixFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxHorXMixFlux+22)=
     &     'HorXMixFlux_DiazFe                         '
      vname_phys(2,indxHorXMixFlux+22)=
     &     'Horizontal (xi) mixing flux of DiazFe  '
      vname_phys(3,indxHorXMixFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXMixFlux+22)=
     &     'HorXMixFlux_DiazFe, scalar, series         '

      vname_phys(1,indxHorXMixFlux+23)=
     &     'HorXMixFlux_DON                            '
      vname_phys(2,indxHorXMixFlux+23)=
     &     'Horizontal (xi) mixing flux of DON     '
      vname_phys(3,indxHorXMixFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorXMixFlux+23)=
     &     'HorXMixFlux_DON, scalar, series            '

      vname_phys(1,indxHorXMixFlux+24)=
     &     'HorXMixFlux_DOFe                           '
      vname_phys(2,indxHorXMixFlux+24)=
     &     'Horizontal (xi) mixing flux of DOFe    '
      vname_phys(3,indxHorXMixFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorXMixFlux+24)=
     &     'HorXMixFlux_DOFe, scalar, series           '

      vname_phys(1,indxHorXMixFlux+25)=
     &     'HorXMixFlux_DOP                            '
      vname_phys(2,indxHorXMixFlux+25)=
     &     'Horizontal (xi) mixing flux of DOP     '
      vname_phys(3,indxHorXMixFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorXMixFlux+25)=
     &     'HorXMixFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */

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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxHorYMixFlux+2)=
     &     'HorYMixFlux_PO4                            '
      vname_phys(2,indxHorYMixFlux+2)=
     &     'Horizontal (eta) mixing flux of PO4             '
      vname_phys(3,indxHorYMixFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorYMixFlux+2)=
     &     'HorYMixFlux_PO4, scalar, series            '

      vname_phys(1,indxHorYMixFlux+3)=
     &     'HorYMixFlux_NO3                            '
      vname_phys(2,indxHorYMixFlux+3)=
     &     'Horizontal (eta) mixing flux of NO3             '
      vname_phys(3,indxHorYMixFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+3)=
     &     'HorYMixFlux_NO3, scalar, series            '

      vname_phys(1,indxHorYMixFlux+4)=
     &     'HorYMixFlux_SiO3                           '
      vname_phys(2,indxHorYMixFlux+4)=
     &     'Horizontal (eta) mixing flux of SiO3            '
      vname_phys(3,indxHorYMixFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxHorYMixFlux+4)=
     &     'HorYMixFlux_SiO3, scalar, series           '

      vname_phys(1,indxHorYMixFlux+5)=
     &     'HorYMixFlux_NH4                            '
      vname_phys(2,indxHorYMixFlux+5)=
     &     'Horizontal (eta) mixing flux of NH4     '
      vname_phys(3,indxHorYMixFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+5)=
     &     'HorYMixFlux_NH4, scalar, series            '

      vname_phys(1,indxHorYMixFlux+6)=
     &     'HorYMixFlux_Fe                            '
      vname_phys(2,indxHorYMixFlux+6)=
     &     'Horizontal (eta) mixing flux of Fe     '
      vname_phys(3,indxHorYMixFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxHorYMixFlux+6)=
     &     'HorYMixFlux_Fe, scalar, series            '

      vname_phys(1,indxHorYMixFlux+7)=
     &     'HorYMixFlux_O2                            '
      vname_phys(2,indxHorYMixFlux+7)=
     &     'Horizontal (eta) mixing flux of O2     '
      vname_phys(3,indxHorYMixFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxHorYMixFlux+7)=
     &     'HorYMixFlux_O2, scalar, series            '

      vname_phys(1,indxHorYMixFlux+8)=
     &     'HorYMixFlux_DIC                            '
      vname_phys(2,indxHorYMixFlux+8)=
     &     'Horizontal (eta) mixing flux of DIC     '
      vname_phys(3,indxHorYMixFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+8)=
     &     'HorYMixFlux_DIC, scalar, series            '

      vname_phys(1,indxHorYMixFlux+9)=
     &     'HorYMixFlux_Alk                            '
      vname_phys(2,indxHorYMixFlux+9)=
     &     'Horizontal (eta) mixing flux of Alk     '
      vname_phys(3,indxHorYMixFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxHorYMixFlux+9)=
     &     'HorYMixFlux_Alk, scalar, series            '

      vname_phys(1,indxHorYMixFlux+10)=
     &     'HorYMixFlux_DOC                            '
      vname_phys(2,indxHorYMixFlux+10)=
     &     'Horizontal (eta) mixing flux of DOC     '
      vname_phys(3,indxHorYMixFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+10)=
     &     'HorYMixFlux_DOC, scalar, series            '

      vname_phys(1,indxHorYMixFlux+11)=
     &     'HorYMixFlux_SPC                            '
      vname_phys(2,indxHorYMixFlux+11)=
     &     'Horizontal (eta) mixing flux of SPC     '
      vname_phys(3,indxHorYMixFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+11)=
     &     'HorYMixFlux_SPC, scalar, series            '

      vname_phys(1,indxHorYMixFlux+12)=
     &     'HorYMixFlux_SPChl                          '
      vname_phys(2,indxHorYMixFlux+12)=
     &     'Horizontal (eta) mixing flux of SPChl   '
      vname_phys(3,indxHorYMixFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYMixFlux+12)=
     &     'HorYMixFlux_SPChl, scalar, series          '

      vname_phys(1,indxHorYMixFlux+13)=
     &     'HorYMixFlux_SPCaCO3                        '
      vname_phys(2,indxHorYMixFlux+13)=
     &     'Horizontal (eta) mixing flux of SPCaCO3 '
      vname_phys(3,indxHorYMixFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxHorYMixFlux+13)=
     &     'HorYMixFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxHorYMixFlux+14)=
     &     'HorYMixFlux_DiatC                          '
      vname_phys(2,indxHorYMixFlux+14)=
     &     'Horizontal (eta) mixing flux of DiatC   '
      vname_phys(3,indxHorYMixFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+14)=
     &     'HorYMixFlux_DiatC, scalar, series          '

      vname_phys(1,indxHorYMixFlux+15)=
     &     'HorYMixFlux_DiatChl                        '
      vname_phys(2,indxHorYMixFlux+15)=
     &     'Horizontal (eta) mixing flux of DiatChl '
      vname_phys(3,indxHorYMixFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYMixFlux+15)=
     &     'HorYMixFlux_DiatChl, scalar, series        '

      vname_phys(1,indxHorYMixFlux+16)=
     &     'HorYMixFlux_ZooC                           '
      vname_phys(2,indxHorYMixFlux+16)=
     &     'Horizontal (eta) mixing flux of ZooC    '
      vname_phys(3,indxHorYMixFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+16)=
     &     'HorYMixFlux_ZooC, scalar, series           '

      vname_phys(1,indxHorYMixFlux+17)=
     &     'HorYMixFlux_SPFe                           '
      vname_phys(2,indxHorYMixFlux+17)=
     &     'Horizontal (eta) mixing flux of SPFe    '
      vname_phys(3,indxHorYMixFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYMixFlux+17)=
     &     'HorYMixFlux_SPFe, scalar, series           '

      vname_phys(1,indxHorYMixFlux+18)=
     &     'HorYMixFlux_DiatSi                         '
      vname_phys(2,indxHorYMixFlux+18)=
     &     'Horizontal (eta) mixing flux of DiatSi  '
      vname_phys(3,indxHorYMixFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxHorYMixFlux+18)=
     &     'HorYMixFlux_DiatSi, scalar, series         '

      vname_phys(1,indxHorYMixFlux+19)=
     &     'HorYMixFlux_DiatFe                         '
      vname_phys(2,indxHorYMixFlux+19)=
     &     'Horizontal (eta) mixing flux of DiatFe  '
      vname_phys(3,indxHorYMixFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYMixFlux+19)=
     &     'HorYMixFlux_DiatFe, scalar, series         '

      vname_phys(1,indxHorYMixFlux+20)=
     &     'HorYMixFlux_DiazC                          '
      vname_phys(2,indxHorYMixFlux+20)=
     &     'Horizontal (eta) mixing flux of DiazC   '
      vname_phys(3,indxHorYMixFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxHorYMixFlux+20)=
     &     'HorYMixFlux_DiazC, scalar, series          '

      vname_phys(1,indxHorYMixFlux+21)=
     &     'HorYMixFlux_DiazChl                        '
      vname_phys(2,indxHorYMixFlux+21)=
     &     'Horizontal (eta) mixing flux of DiazChl '
      vname_phys(3,indxHorYMixFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxHorYMixFlux+21)=
     &     'HorYMixFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxHorYMixFlux+22)=
     &     'HorYMixFlux_DiazFe                         '
      vname_phys(2,indxHorYMixFlux+22)=
     &     'Horizontal (eta) mixing flux of DiazFe  '
      vname_phys(3,indxHorYMixFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYMixFlux+22)=
     &     'HorYMixFlux_DiazFe, scalar, series         '

      vname_phys(1,indxHorYMixFlux+23)=
     &     'HorYMixFlux_DON                            '
      vname_phys(2,indxHorYMixFlux+23)=
     &     'Horizontal (eta) mixing flux of DON     '
      vname_phys(3,indxHorYMixFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxHorYMixFlux+23)=
     &     'HorYMixFlux_DON, scalar, series            '

      vname_phys(1,indxHorYMixFlux+24)=
     &     'HorYMixFlux_DOFe                           '
      vname_phys(2,indxHorYMixFlux+24)=
     &     'Horizontal (eta) mixing flux of DOFe    '
      vname_phys(3,indxHorYMixFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxHorYMixFlux+24)=
     &     'HorYMixFlux_DOFe, scalar, series           '

      vname_phys(1,indxHorYMixFlux+25)=
     &     'HorYMixFlux_DOP                            '
      vname_phys(2,indxHorYMixFlux+25)=
     &     'Horizontal (eta) mixing flux of DOP     '
      vname_phys(3,indxHorYMixFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxHorYMixFlux+25)=
     &     'HorYMixFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */

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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxVertMixFlux+2)=
     &     'VertMixFlux_PO4                            '
      vname_phys(2,indxVertMixFlux+2)=
     &     'Vertical mixing flux of PO4             '
      vname_phys(3,indxVertMixFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxVertMixFlux+2)=
     &     'VertMixFlux_PO4, scalar, series            '

      vname_phys(1,indxVertMixFlux+3)=
     &     'VertMixFlux_NO3                            '
      vname_phys(2,indxVertMixFlux+3)=
     &     'Vertical mixing flux of NO3             '
      vname_phys(3,indxVertMixFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxVertMixFlux+3)=
     &     'VertMixFlux_NO3, scalar, series            '

      vname_phys(1,indxVertMixFlux+4)=
     &     'VertMixFlux_SiO3                           '
      vname_phys(2,indxVertMixFlux+4)=
     &     'Vertical mixing flux of SiO3            '
      vname_phys(3,indxVertMixFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxVertMixFlux+4)=
     &     'VertMixFlux_SiO3, scalar, series           '

      vname_phys(1,indxVertMixFlux+5)=
     &     'VertMixFlux_NH4                            '
      vname_phys(2,indxVertMixFlux+5)=
     &     'Vertical mixing flux of NH4     '
      vname_phys(3,indxVertMixFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxVertMixFlux+5)=
     &     'VertMixFlux_NH4, scalar, series            '

      vname_phys(1,indxVertMixFlux+6)=
     &     'VertMixFlux_Fe                            '
      vname_phys(2,indxVertMixFlux+6)=
     &     'Vertical mixing flux of Fe     '
      vname_phys(3,indxVertMixFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxVertMixFlux+6)=
     &     'VertMixFlux_Fe, scalar, series            '

      vname_phys(1,indxVertMixFlux+7)=
     &     'VertMixFlux_O2                            '
      vname_phys(2,indxVertMixFlux+7)=
     &     'Vertical mixing flux of O2     '
      vname_phys(3,indxVertMixFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxVertMixFlux+7)=
     &     'VertMixFlux_O2, scalar, series            '

      vname_phys(1,indxVertMixFlux+8)=
     &     'VertMixFlux_DIC                            '
      vname_phys(2,indxVertMixFlux+8)=
     &     'Vertical mixing flux of DIC     '
      vname_phys(3,indxVertMixFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertMixFlux+8)=
     &     'VertMixFlux_DIC, scalar, series            '

      vname_phys(1,indxVertMixFlux+9)=
     &     'VertMixFlux_Alk                            '
      vname_phys(2,indxVertMixFlux+9)=
     &     'Vertical mixing flux of Alk     '
      vname_phys(3,indxVertMixFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxVertMixFlux+9)=
     &     'VertMixFlux_Alk, scalar, series            '

      vname_phys(1,indxVertMixFlux+10)=
     &     'VertMixFlux_DOC                            '
      vname_phys(2,indxVertMixFlux+10)=
     &     'Vertical mixing flux of DOC     '
      vname_phys(3,indxVertMixFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertMixFlux+10)=
     &     'VertMixFlux_DOC, scalar, series            '

      vname_phys(1,indxVertMixFlux+11)=
     &     'VertMixFlux_SPC                            '
      vname_phys(2,indxVertMixFlux+11)=
     &     'Vertical mixing flux of SPC     '
      vname_phys(3,indxVertMixFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertMixFlux+11)=
     &     'VertMixFlux_SPC, scalar, series            '

      vname_phys(1,indxVertMixFlux+12)=
     &     'VertMixFlux_SPChl                          '
      vname_phys(2,indxVertMixFlux+12)=
     &     'Vertical mixing flux of SPChl   '
      vname_phys(3,indxVertMixFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxVertMixFlux+12)=
     &     'VertMixFlux_SPChl, scalar, series          '

      vname_phys(1,indxVertMixFlux+13)=
     &     'VertMixFlux_SPCaCO3                        '
      vname_phys(2,indxVertMixFlux+13)=
     &     'Vertical mixing flux of SPCaCO3 '
      vname_phys(3,indxVertMixFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxVertMixFlux+13)=
     &     'VertMixFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxVertMixFlux+14)=
     &     'VertMixFlux_DiatC                          '
      vname_phys(2,indxVertMixFlux+14)=
     &     'Vertical mixing flux of DiatC   '
      vname_phys(3,indxVertMixFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertMixFlux+14)=
     &     'VertMixFlux_DiatC, scalar, series          '

      vname_phys(1,indxVertMixFlux+15)=
     &     'VertMixFlux_DiatChl                        '
      vname_phys(2,indxVertMixFlux+15)=
     &     'Vertical mixing flux of DiatChl '
      vname_phys(3,indxVertMixFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxVertMixFlux+15)=
     &     'VertMixFlux_DiatChl, scalar, series        '

      vname_phys(1,indxVertMixFlux+16)=
     &     'VertMixFlux_ZooC                           '
      vname_phys(2,indxVertMixFlux+16)=
     &     'Vertical mixing flux of ZooC    '
      vname_phys(3,indxVertMixFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertMixFlux+16)=
     &     'VertMixFlux_ZooC, scalar, series           '

      vname_phys(1,indxVertMixFlux+17)=
     &     'VertMixFlux_SPFe                           '
      vname_phys(2,indxVertMixFlux+17)=
     &     'Vertical mixing flux of SPFe    '
      vname_phys(3,indxVertMixFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertMixFlux+17)=
     &     'VertMixFlux_SPFe, scalar, series           '

      vname_phys(1,indxVertMixFlux+18)=
     &     'VertMixFlux_DiatSi                         '
      vname_phys(2,indxVertMixFlux+18)=
     &     'Vertical mixing flux of DiatSi  '
      vname_phys(3,indxVertMixFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxVertMixFlux+18)=
     &     'VertMixFlux_DiatSi, scalar, series         '

      vname_phys(1,indxVertMixFlux+19)=
     &     'VertMixFlux_DiatFe                         '
      vname_phys(2,indxVertMixFlux+19)=
     &     'Vertical mixing flux of DiatFe  '
      vname_phys(3,indxVertMixFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertMixFlux+19)=
     &     'VertMixFlux_DiatFe, scalar, series         '

      vname_phys(1,indxVertMixFlux+20)=
     &     'VertMixFlux_DiazC                          '
      vname_phys(2,indxVertMixFlux+20)=
     &     'Vertical mixing flux of DiazC   '
      vname_phys(3,indxVertMixFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxVertMixFlux+20)=
     &     'VertMixFlux_DiazC, scalar, series          '

      vname_phys(1,indxVertMixFlux+21)=
     &     'VertMixFlux_DiazChl                        '
      vname_phys(2,indxVertMixFlux+21)=
     &     'Vertical mixing flux of DiazChl '
      vname_phys(3,indxVertMixFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxVertMixFlux+21)=
     &     'VertMixFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxVertMixFlux+22)=
     &     'VertMixFlux_DiazFe                         '
      vname_phys(2,indxVertMixFlux+22)=
     &     'Vertical mixing flux of DiazFe  '
      vname_phys(3,indxVertMixFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertMixFlux+22)=
     &     'VertMixFlux_DiazFe, scalar, series         '

      vname_phys(1,indxVertMixFlux+23)=
     &     'VertMixFlux_DON                            '
      vname_phys(2,indxVertMixFlux+23)=
     &     'Vertical mixing flux of DON     '
      vname_phys(3,indxVertMixFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxVertMixFlux+23)=
     &     'VertMixFlux_DON, scalar, series            '

      vname_phys(1,indxVertMixFlux+24)=
     &     'VertMixFlux_DOFe                           '
      vname_phys(2,indxVertMixFlux+24)=
     &     'Vertical mixing flux of DOFe    '
      vname_phys(3,indxVertMixFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxVertMixFlux+24)=
     &     'VertMixFlux_DOFe, scalar, series           '

      vname_phys(1,indxVertMixFlux+25)=
     &     'VertMixFlux_DOP                            '
      vname_phys(2,indxVertMixFlux+25)=
     &     'Vertical mixing flux of DOP     '
      vname_phys(3,indxVertMixFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxVertMixFlux+25)=
     &     'VertMixFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */

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
# ifdef BIOLOGY_BEC
      vname_phys(1,indxNudgingFlux+2)=
     &     'NudgingFlux_PO4                            '
      vname_phys(2,indxNudgingFlux+2)=
     &     'Nudging divergence of PO4             '
      vname_phys(3,indxNudgingFlux+2)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxNudgingFlux+2)=
     &     'NudgingFlux_PO4, scalar, series            '

      vname_phys(1,indxNudgingFlux+3)=
     &     'NudgingFlux_NO3                            '
      vname_phys(2,indxNudgingFlux+3)=
     &     'Nudging divergence of NO3             '
      vname_phys(3,indxNudgingFlux+3)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxNudgingFlux+3)=
     &     'NudgingFlux_NO3, scalar, series            '

      vname_phys(1,indxNudgingFlux+4)=
     &     'NudgingFlux_SiO3                           '
      vname_phys(2,indxNudgingFlux+4)=
     &     'Nudging divergence of SiO3            '
      vname_phys(3,indxNudgingFlux+4)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxNudgingFlux+4)=
     &     'NudgingFlux_SiO3, scalar, series           '

      vname_phys(1,indxNudgingFlux+5)=
     &     'NudgingFlux_NH4                            '
      vname_phys(2,indxNudgingFlux+5)=
     &     'Nudging divergence of NH4     '
      vname_phys(3,indxNudgingFlux+5)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxNudgingFlux+5)=
     &     'NudgingFlux_NH4, scalar, series            '

      vname_phys(1,indxNudgingFlux+6)=
     &     'NudgingFlux_Fe                            '
      vname_phys(2,indxNudgingFlux+6)=
     &     'Nudging divergence of Fe     '
      vname_phys(3,indxNudgingFlux+6)=
     &     'mmol Fe s-1                               '
      vname_phys(4,indxNudgingFlux+6)=
     &     'NudgingFlux_Fe, scalar, series            '

      vname_phys(1,indxNudgingFlux+7)=
     &     'NudgingFlux_O2                            '
      vname_phys(2,indxNudgingFlux+7)=
     &     'Nudging divergence of O2     '
      vname_phys(3,indxNudgingFlux+7)=
     &     'mmol O2 s-1                               '
      vname_phys(4,indxNudgingFlux+7)=
     &     'NudgingFlux_O2, scalar, series            '

      vname_phys(1,indxNudgingFlux+8)=
     &     'NudgingFlux_DIC                            '
      vname_phys(2,indxNudgingFlux+8)=
     &     'Nudging divergence of DIC     '
      vname_phys(3,indxNudgingFlux+8)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxNudgingFlux+8)=
     &     'NudgingFlux_DIC, scalar, series            '

      vname_phys(1,indxNudgingFlux+9)=
     &     'NudgingFlux_Alk                            '
      vname_phys(2,indxNudgingFlux+9)=
     &     'Nudging divergence of Alk     '
      vname_phys(3,indxNudgingFlux+9)=
     &     'mmol s-1                                   '
      vname_phys(4,indxNudgingFlux+9)=
     &     'NudgingFlux_Alk, scalar, series            '

      vname_phys(1,indxNudgingFlux+10)=
     &     'NudgingFlux_DOC                            '
      vname_phys(2,indxNudgingFlux+10)=
     &     'Nudging divergence of DOC     '
      vname_phys(3,indxNudgingFlux+10)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxNudgingFlux+10)=
     &     'NudgingFlux_DOC, scalar, series            '

      vname_phys(1,indxNudgingFlux+11)=
     &     'NudgingFlux_SPC                            '
      vname_phys(2,indxNudgingFlux+11)=
     &     'Nudging divergence of SPC     '
      vname_phys(3,indxNudgingFlux+11)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxNudgingFlux+11)=
     &     'NudgingFlux_SPC, scalar, series            '

      vname_phys(1,indxNudgingFlux+12)=
     &     'NudgingFlux_SPChl                          '
      vname_phys(2,indxNudgingFlux+12)=
     &     'Nudging divergence of SPChl   '
      vname_phys(3,indxNudgingFlux+12)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxNudgingFlux+12)=
     &     'NudgingFlux_SPChl, scalar, series          '

      vname_phys(1,indxNudgingFlux+13)=
     &     'NudgingFlux_SPCaCO3                        '
      vname_phys(2,indxNudgingFlux+13)=
     &     'Nudging divergence of SPCaCO3 '
      vname_phys(3,indxNudgingFlux+13)=
     &     'mmol CaCO3 s-1                             '
      vname_phys(4,indxNudgingFlux+13)=
     &     'NudgingFlux_SPCaCO3, scalar, series        '

      vname_phys(1,indxNudgingFlux+14)=
     &     'NudgingFlux_DiatC                          '
      vname_phys(2,indxNudgingFlux+14)=
     &     'Nudging divergence of DiatC   '
      vname_phys(3,indxNudgingFlux+14)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxNudgingFlux+14)=
     &     'NudgingFlux_DiatC, scalar, series          '

      vname_phys(1,indxNudgingFlux+15)=
     &     'NudgingFlux_DiatChl                        '
      vname_phys(2,indxNudgingFlux+15)=
     &     'Nudging divergence of DiatChl '
      vname_phys(3,indxNudgingFlux+15)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxNudgingFlux+15)=
     &     'NudgingFlux_DiatChl, scalar, series        '

      vname_phys(1,indxNudgingFlux+16)=
     &     'NudgingFlux_ZooC                           '
      vname_phys(2,indxNudgingFlux+16)=
     &     'Nudging divergence of ZooC    '
      vname_phys(3,indxNudgingFlux+16)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxNudgingFlux+16)=
     &     'NudgingFlux_ZooC, scalar, series           '

      vname_phys(1,indxNudgingFlux+17)=
     &     'NudgingFlux_SPFe                           '
      vname_phys(2,indxNudgingFlux+17)=
     &     'Nudging divergence of SPFe    '
      vname_phys(3,indxNudgingFlux+17)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxNudgingFlux+17)=
     &     'NudgingFlux_SPFe, scalar, series           '

      vname_phys(1,indxNudgingFlux+18)=
     &     'NudgingFlux_DiatSi                         '
      vname_phys(2,indxNudgingFlux+18)=
     &     'Nudging divergence of DiatSi  '
      vname_phys(3,indxNudgingFlux+18)=
     &     'mmol Si s-1                                '
      vname_phys(4,indxNudgingFlux+18)=
     &     'NudgingFlux_DiatSi, scalar, series         '

      vname_phys(1,indxNudgingFlux+19)=
     &     'NudgingFlux_DiatFe                         '
      vname_phys(2,indxNudgingFlux+19)=
     &     'Nudging divergence of DiatFe  '
      vname_phys(3,indxNudgingFlux+19)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxNudgingFlux+19)=
     &     'NudgingFlux_DiatFe, scalar, series         '

      vname_phys(1,indxNudgingFlux+20)=
     &     'NudgingFlux_DiazC                          '
      vname_phys(2,indxNudgingFlux+20)=
     &     'Nudging divergence of DiazC   '
      vname_phys(3,indxNudgingFlux+20)=
     &     'mmol C s-1                                 '
      vname_phys(4,indxNudgingFlux+20)=
     &     'NudgingFlux_DiazC, scalar, series          '

      vname_phys(1,indxNudgingFlux+21)=
     &     'NudgingFlux_DiazChl                        '
      vname_phys(2,indxNudgingFlux+21)=
     &     'Nudging divergence of DiazChl '
      vname_phys(3,indxNudgingFlux+21)=
     &     'mg Chl-a s-1                               '
      vname_phys(4,indxNudgingFlux+21)=
     &     'NudgingFlux_DiazChl, scalar, series        '
#  ifdef PHYS_FLUX_ALL_COMPS
      vname_phys(1,indxNudgingFlux+22)=
     &     'NudgingFlux_DiazFe                         '
      vname_phys(2,indxNudgingFlux+22)=
     &     'Nudging divergence of DiazFe  '
      vname_phys(3,indxNudgingFlux+22)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxNudgingFlux+22)=
     &     'NudgingFlux_DiazFe, scalar, series         '

      vname_phys(1,indxNudgingFlux+23)=
     &     'NudgingFlux_DON                            '
      vname_phys(2,indxNudgingFlux+23)=
     &     'Nudging divergence of DON     '
      vname_phys(3,indxNudgingFlux+23)=
     &     'mmol N s-1                                 '
      vname_phys(4,indxNudgingFlux+23)=
     &     'NudgingFlux_DON, scalar, series            '

      vname_phys(1,indxNudgingFlux+24)=
     &     'NudgingFlux_DOFe                           '
      vname_phys(2,indxNudgingFlux+24)=
     &     'Nudging divergence of DOFe    '
      vname_phys(3,indxNudgingFlux+24)=
     &     'mmol Fe s-1                                '
      vname_phys(4,indxNudgingFlux+24)=
     &     'NudgingFlux_DOFe, scalar, series           '

      vname_phys(1,indxNudgingFlux+25)=
     &     'NudgingFlux_DOP                            '
      vname_phys(2,indxNudgingFlux+25)=
     &     'Nudging divergence of DOP     '
      vname_phys(3,indxNudgingFlux+25)=
     &     'mmol P s-1                                 '
      vname_phys(4,indxNudgingFlux+25)=
     &     'NudgingFlux_DOP, scalar, series            '
#  endif /* PHYS_FLUX_ALL_COMPS */
# endif /* BIOLOGY_BEC */
# endif /* FULL_PHYS_FLUX_ANALYSIS */
#endif /* SOLVE3D */
      return
      end
#else /* PHYS_FLUX_ANALYSIS */
      subroutine init_scalars_physflux_empty()
      return
      end
#endif /* PHYS_FLUX_ANALYSIS */
