!
!
#include "cppdefs.h"
#ifdef BIOLOGY_NPZDOC

       subroutine init_scalars_npzdoc(ierr)
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
#include "bgcflux.h"
#include "ncroms_bgcflux.h"
!
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
# define NUMPROC 48
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
      do i=0,2*NT+1
        global_sum(i)=QuadZero
      enddo

!
! Define names of variables in NetCDF output files.
! Inner dimension is for variable type and outer is for variable
! attributes (name, long-name, units, field).
!
#ifdef SOLVE3D
CR      write(stdout,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='NO3 Nutrient                              '
      vname(3,indxNO3)='mMol N m-3                                '
      vname(4,indxNO3)='NO3, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxNH4 =',indxNH4
      vname(1,indxNH4)='NH4                                       '
      vname(2,indxNH4)='NH4 Nutrient                              '
      vname(3,indxNH4)='mMol N m-3                                '
      vname(4,indxNH4)='NH4, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxChla =',indxChla
      vname(1,indxChla)='CHLA                                     '
      vname(2,indxChla)='Chlorophyll A                            '
      vname(3,indxChla)='mg Chla m-3                              '
      vname(4,indxChla)='Chlorophyll, scalar, series              '

CR      write(stdout,'(1x,A,I3)') 'indxPhyt =',indxPhyt
      vname(1,indxPhyt)='PHYTO                                    '
      vname(2,indxPhyt)='Phytoplankton                            '
      vname(3,indxPhyt)='mMol N m-3                               '
      vname(4,indxPhyt)='Phytoplankton, scalar, series            '

CR      write(stdout,'(1x,A,I3)') 'indxZoo =',indxZoo
      vname(1,indxZoo)='ZOO                                       '
      vname(2,indxZoo)='Zooplankton                               '
      vname(3,indxZoo)='mMol N m-3                                '
      vname(4,indxZoo)='Zooplankton, scalar, series               '

CR      write(stdout,'(1x,A,I3)') 'indxSDet =',indxSDet
      vname(1,indxSDet)='SDET                                     '
      vname(2,indxSDet)='Small Detritus Nutrient                  '
      vname(3,indxSDet)='mMol N m-3                               '
      vname(4,indxSDet)='Small Detritus, scalar, series           '

CR      write(stdout,'(1x,A,I3)') 'indxLDet =',indxLDet
      vname(1,indxLDet)='LDET                                     '
      vname(2,indxLDet)='Large Detritus Nutrient                  '
      vname(3,indxLDet)='mMol N m-3                               '
      vname(4,indxLDet)='Large Detritus, scalar, series           '
#  ifdef OXYGEN
      vname(1,indxO2)='O2                                       '
      vname(2,indxO2)='Oxygen                                   '
      vname(3,indxO2)='mMol O2 m-3                              '
      vname(4,indxO2)='Oxygen, scalar, series                   '
#   ifdef CARBON
      vname(1,indxDIC)='DIC                                      '
      vname(2,indxDIC)='Dissolved Inorganic Carbon               '
      vname(3,indxDIC)='mMol C m-3                               '
      vname(4,indxDIC)='Dissolved Inorganic Carbon, scalar, series'
      vname(1,indxTALK)='Alk                                      '
      vname(2,indxTALK)='Total Alkalinity                         '
      vname(3,indxTALK)='meq m-3                                  '
      vname(4,indxTALK)='Total Alkalinity, scalar, series         '
      vname(1,indxSDetC)='SDetC                                    '
      vname(2,indxSDetC)='Small Detritus Carbon                    '
      vname(3,indxSDetC)='mMol C m-3                               '
      vname(4,indxSDetC)='Small Detritus Carbon, scalar, series    '
      vname(1,indxLDetC)='LDetC                                    '
      vname(2,indxLDetC)='Large Detritus Carbon                    '
      vname(3,indxLDetC)='mMol C m-3                               '
      vname(4,indxLDetC)='Large Detritus Carbon, scalar, series    '
      vname(1,indxCaCO3)='CaCO3                                    '
      vname(2,indxCaCO3)='Calcium carbonate precipitate            '
      vname(3,indxCaCO3)='mMol C m-3                               '
      vname(4,indxCaCO3)='CaCO3, scalar, series                    '
!DL: pH treated here for avg/his/rst files;
!DL: and in init_scalars_bgcflux for flux files:
      vname(1,indxPH_rst)='PH                                       '
      vname(2,indxPH_rst)='pH value                                 '
      vname(3,indxPH_rst)='-                                        '
      vname(4,indxPH_rst)='pH, scalar, series                       '
!DL: some more variables which have been moved from the flux files to
!DL: averages/histrory/restart files:
      vname(1,indxPCO2_rst)='pCO2                                 '
      vname(2,indxPCO2_rst)='surface pCO2                         '
      vname(3,indxPCO2_rst)='ppm                                  '
      vname(4,indxPCO2_rst)='pCO2, scalar, series                 '
      vname(1,indxPCO2air_rst)='pCO2air                           '
      vname(2,indxPCO2air_rst)='air pCO2                          '
      vname(3,indxPCO2air_rst)='ppm                               '
      vname(4,indxPCO2air_rst)='pCO2air, scalar, series           '
#   endif /* CARBON */
#  endif /* OXYGEN */
      vname(1,indxPARinc_rst)='PARinc                                '
      vname(2,indxPARinc_rst)='Incoming photosynth. active radiation '
      vname(3,indxPARinc_rst)='W m-2                                 '
      vname(4,indxPARinc_rst)='PARinc, scalar, series                '
      vname(1,indxPAR_rst)='PAR                                   '
      vname(2,indxPAR_rst)='Photosynthetically active radiation   '
      vname(3,indxPAR_rst)='W m-2                                 '
      vname(4,indxPAR_rst)='PAR, scalar, series                   '
#  ifdef SEDIMENT_BIOLOGY
      vname(1,indxSedOrgN)=
     &     'SedOrgN                                  '
      vname(2,indxSedOrgN)=
     &     'Sedimented Organic Material (N)          '
      vname(3,indxSedOrgN)=
     &     'mMol N m-2                               '
      vname(4,indxSedOrgN)=
     &     'Sedimented Organic N, scalar, series     '
#   ifdef CARBON
      vname(1,indxSedOrgC)=
     &     'SedOrgC                                  '
      vname(2,indxSedOrgC)=
     &     'Sedimented Organic Material (C)          '
      vname(3,indxSedOrgC)=
     &     'mMol C m-2                               '
      vname(4,indxSedOrgC)=
     &     'Sedimented Organic C, scalar, series     '

      vname(1,indxSedCaCO3)=
     &     'SedCaCO3                                 '
      vname(2,indxSedCaCO3)=
     &     'Sedimented Calcium Carbonate             '
      vname(3,indxSedCaCO3)=
     &     'mMol C m-2                               '
      vname(4,indxSedCaCO3)=
     &     'Sedimented CaCO3, scalar, series         '
#   endif /* CARBON */
#  endif /* SEDIMENT_BIOLOGY */

      tclm_name(iNO3_) = 'no3_time'
      tclm_name(iNH4_) = 'nh4_time'
      tclm_name(iChla) = 'chla_time'
      tclm_name(iPhyt) = 'phyto_time'
      tclm_name(iZoo_) = 'zoo_time'
      tclm_name(iSDet) = 'sdet_time'
      tclm_name(iLDet) = 'ldet_time'
#  ifdef OXYGEN
      tclm_name(iO2)   = 'o2_time'
#   ifdef CARBON
      tclm_name(iDIC)  = 'dic_time'
      tclm_name(iTALK) = 'alk_time'
      tclm_name(iSDetC) = 'sdetc_time'
      tclm_name(iLDetC) = 'ldetc_time'
      tclm_name(iCaCO3) = 'caco3_time'
#   endif /* CARBON */
#  endif /* OXYGEN */


#endif /* SOLVE3D */
      return
      end
#else /* BIOLOGY_NPZDOC */
      subroutine init_scalars_npzdoc_empty()
      return
      end
#endif /* BIOLOGY_NPZDOC */
