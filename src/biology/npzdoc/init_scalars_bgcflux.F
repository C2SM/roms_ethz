!
!
#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC && defined BGC_FLUX_ANALYSIS

       subroutine init_scalars_bgcflux(ierr)

! Set initial values for  globally accessible (stored in common
! blocks) scalar variables of the NPZDOC model related to the
! biogeochemical flux analysis.
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
!
! Initialize netCDF files IDs to closed status.
!
      ncid_bgc_flux_his = -1
#ifdef AVERAGES
      ncid_bgc_flux_avg = -1
#endif
!
! Define names of variables in NetCDF output files.
! Inner dimension is for variable type and outer is for variable
! attributes (name, long-name, units, field).
!
#  ifdef OXYGEN
CR      write(stdout,'(1x,A,I3)') 'indxU10 =',indxU10
      vname_bgcflux(1,indxU10)='u10                                   '
      vname_bgcflux(2,indxU10)='wind speed at 10 m                    '
      vname_bgcflux(3,indxU10)='m s-1                                 '
      vname_bgcflux(4,indxU10)='u10, scalar, series                   '
CR      write(stdout,'(1x,A,I3)') 'indxKvO2 =',indxKvO2
      vname_bgcflux(1,indxKvO2)='KvO2                                 '
      vname_bgcflux(2,indxKvO2)='gas transfer velocity of O2          '
      vname_bgcflux(3,indxKvO2)='m s-1                                '
      vname_bgcflux(4,indxKvO2)='KvO2, scalar, series                 '
CR      write(stdout,'(1x,A,I3)') 'indxO2sat =',indxO2sat
      vname_bgcflux(1,indxO2sat)='O2sat                               '
      vname_bgcflux(2,indxO2sat)='saturation concentration of O2      '
      vname_bgcflux(3,indxO2sat)='mmol m-3                            '
      vname_bgcflux(4,indxO2sat)='O2sat, scalar, series               '
#   ifdef CARBON
CR      write(stdout,'(1x,A,I3)') 'indxKvCO2 =',indxKvCO2
      vname_bgcflux(1,indxKvCO2)='KvCO2                               '
      vname_bgcflux(2,indxKvCO2)='gas transfer velocity of CO2        '
      vname_bgcflux(3,indxKvCO2)='m s-1                               '
      vname_bgcflux(4,indxKvCO2)='KvCO2, scalar, series               '
CR      write(stdout,'(1x,A,I3)') 'indxCO2sol =',indxCO2sol
      vname_bgcflux(1,indxCO2sol)='CO2sol                             '
      vname_bgcflux(2,indxCO2sol)='solubility of CO2                  '
      vname_bgcflux(3,indxCO2sol)='mmol m-3 atm-1                     '
      vname_bgcflux(4,indxCO2sol)='CO2sol, scalar, series             '
CR      write(stdout,'(1x,A,I3)') 'indxPCO2 =',indxPCO2
      vname_bgcflux(1,indxPCO2)='pCO2                                 '
      vname_bgcflux(2,indxPCO2)='surface pCO2                         '
      vname_bgcflux(3,indxPCO2)='ppm                                  '
      vname_bgcflux(4,indxPCO2)='pCO2, scalar, series                 '
CR      write(stdout,'(1x,A,I3)') 'indxPCO2air =',indxPCO2air
      vname_bgcflux(1,indxPCO2air)='pCO2air                           '
      vname_bgcflux(2,indxPCO2air)='air pCO2                          '
      vname_bgcflux(3,indxPCO2air)='ppm                               '
      vname_bgcflux(4,indxPCO2air)='pCO2air, scalar, series           '
CR      write(stdout,'(1x,A,I3)') 'indxPH =',indxPH
      vname_bgcflux(1,indxPH)='pH                                     '
      vname_bgcflux(2,indxPH)='surface pH                             '
      vname_bgcflux(3,indxPH)='-                                      '
      vname_bgcflux(4,indxPH)='pH, scalar, series                     '
#   endif /* CARBON */
#  endif /* OXYGEN */
CR      write(stdout,'(1x,A,I3)') 'indxPAR =',indxPAR
      vname_bgcflux(1,indxPAR)='PAR                                   '
      vname_bgcflux(2,indxPAR)='Photosynthetically active radiation   '
      vname_bgcflux(3,indxPAR)='W m-2                                 '
      vname_bgcflux(4,indxPAR)='PAR, scalar, series                   '
CR      write(stdout,'(1x,A,I3)') 'indxPARinc =',indxPARinc
      vname_bgcflux(1,indxPARinc)='PARincoming                        '
      vname_bgcflux(2,indxPARinc)=
     &     'Inc. Photosynthetically active radiation  '
      vname_bgcflux(3,indxPARinc)='W m-2                              '
      vname_bgcflux(4,indxPARinc)='PARinc, scalar, series             '
      vname_bgcflux(1,indxFlux+NFlux_NewProd-1)=
     &     'NFlux_NewProd                             '
      vname_bgcflux(2,indxFlux+NFlux_NewProd-1)=
     &     'Flux of N due to new production           '
      vname_bgcflux(3,indxFlux+NFlux_NewProd-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_NewProd-1)=
     &     'NFlux_NewProd, scalar, series             '

      vname_bgcflux(1,indxFlux+NFlux_RegProd-1)=
     &     'NFlux_RegProd                             '
      vname_bgcflux(2,indxFlux+NFlux_RegProd-1)=
     &     'Flux of N due to regenerated production   '
      vname_bgcflux(3,indxFlux+NFlux_RegProd-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_RegProd-1)=
     &     'NFlux_RegProd, scalar, series             '

      vname_bgcflux(1,indxFlux+NFlux_Grazing-1)=
     &     'NFlux_Grazing                             '
      vname_bgcflux(2,indxFlux+NFlux_Grazing-1)=
     &     'Flux of N due to grazing                  '
      vname_bgcflux(3,indxFlux+NFlux_Grazing-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_Grazing-1)=
     &     'NFlux_Grazing, scalar, series             '

      vname_bgcflux(1,indxFlux+NFlux_SlopFeed-1)=
     &     'NFlux_SlopFeed                            '
      vname_bgcflux(2,indxFlux+NFlux_SlopFeed-1)=
     &     'Flux of N due to sloppy feeding           '
      vname_bgcflux(3,indxFlux+NFlux_SlopFeed-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_SlopFeed-1)=
     &     'NFlux_SlopFeed, scalar, series            '

      vname_bgcflux(1,indxFlux+NFlux_Zfecp-1)=
     &     'NFlux_Zfecp                               '
      vname_bgcflux(2,indxFlux+NFlux_Zfecp-1)=
     &     'Flux of N due to fecal pellet production  '
      vname_bgcflux(3,indxFlux+NFlux_Zfecp-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_Zfecp-1)=
     &     'NFlux_Zfecp, scalar, series               '

      vname_bgcflux(1,indxFlux+NFlux_Pmort-1)=
     &     'NFlux_Pmort                               '
      vname_bgcflux(2,indxFlux+NFlux_Pmort-1)=
     &     'Flux of N due to phytoplankton mortality  '
      vname_bgcflux(3,indxFlux+NFlux_Pmort-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_Pmort-1)=
     &     'NFlux_Pmort, scalar, series               '

      vname_bgcflux(1,indxFlux+NFlux_Zmetab-1)=
     &     'NFlux_Zmetab                              '
      vname_bgcflux(2,indxFlux+NFlux_Zmetab-1)=
     &     'Flux of N due to zooplankton metabolism   '
      vname_bgcflux(3,indxFlux+NFlux_Zmetab-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_Zmetab-1)=
     &     'NFlux_Zmetab, scalar, series              '

      vname_bgcflux(1,indxFlux+NFlux_Zexcr-1)=
     &     'NFlux_Zexcr                               '
      vname_bgcflux(2,indxFlux+NFlux_Zexcr-1)=
     &     'Flux of N due to zooplankton excretion    '
      vname_bgcflux(3,indxFlux+NFlux_Zexcr-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_Zexcr-1)=
     &     'NFlux_Zexcr, scalar, series               '

      vname_bgcflux(1,indxFlux+NFlux_ZmortS-1)=
     &     'NFlux_ZmortS                              '
      vname_bgcflux(2,indxFlux+NFlux_ZmortS-1)=
     &     'Flux of N due to zooplankton mort. to SDet'
      vname_bgcflux(3,indxFlux+NFlux_ZmortS-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_ZmortS-1)=
     &     'NFlux_ZmortS, scalar, series              '

      vname_bgcflux(1,indxFlux+NFlux_ZmortL-1)=
     &     'NFlux_ZmortL                              '
      vname_bgcflux(2,indxFlux+NFlux_ZmortL-1)=
     &     'Flux of N due to zooplankton mort. to LDet'
      vname_bgcflux(3,indxFlux+NFlux_ZmortL-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_ZmortL-1)=
     &     'NFlux_ZmortL, scalar, series              '

      vname_bgcflux(1,indxFlux+NFlux_DetCoagP-1)=
     &     'NFlux_DetCoagP                            '
      vname_bgcflux(2,indxFlux+NFlux_DetCoagP-1)=
     &     'Flux of N due to coagulation of phytopl.  '
      vname_bgcflux(3,indxFlux+NFlux_DetCoagP-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_DetCoagP-1)=
     &     'NFlux_DetCoagP, scalar, series            '

      vname_bgcflux(1,indxFlux+NFlux_DetCoagD-1)=
     &     'NFlux_DetCoagD                            '
      vname_bgcflux(2,indxFlux+NFlux_DetCoagD-1)=
     &     'Flux of N due to coagulation of small det.'
      vname_bgcflux(3,indxFlux+NFlux_DetCoagD-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_DetCoagD-1)=
     &     'NFlux_DetCoagD, scalar, series            '

      vname_bgcflux(1,indxFlux+NFlux_ReminS-1)=
     &     'NFlux_ReminS                              '
      vname_bgcflux(2,indxFlux+NFlux_ReminS-1)=
     &     'Flux of N due to remin. of small detritus '
      vname_bgcflux(3,indxFlux+NFlux_ReminS-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_ReminS-1)=
     &     'NFlux_ReminS, scalar, series              '

      vname_bgcflux(1,indxFlux+NFlux_ReminL-1)=
     &     'NFlux_ReminL                              '
      vname_bgcflux(2,indxFlux+NFlux_ReminL-1)=
     &     'Flux of N due to remin. of large detritus '
      vname_bgcflux(3,indxFlux+NFlux_ReminL-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_ReminL-1)=
     &     'NFlux_ReminL, scalar, series              '

      vname_bgcflux(1,indxFlux+NFlux_Nitrif-1)=
     &     'NFlux_Nitrif                              '
      vname_bgcflux(2,indxFlux+NFlux_Nitrif-1)=
     &     'Flux of N due to nitrification of NH4     '
      vname_bgcflux(3,indxFlux+NFlux_Nitrif-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_Nitrif-1)=
     &     'NFlux_Nitrif, scalar, series              '

#  ifdef OXYLIM
      vname_bgcflux(1,indxFlux+NFlux_DenitrS-1)=
     &     'NFlux_DenitrS                             '
      vname_bgcflux(2,indxFlux+NFlux_DenitrS-1)=
     &     'Flux of N due to denitr of NO3 from SDetC '
      vname_bgcflux(3,indxFlux+NFlux_DenitrS-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_DenitrS-1)=
     &     'NFlux_DenitrS, scalar, series             '

      vname_bgcflux(1,indxFlux+NFlux_DenitrL-1)=
     &     'NFlux_DenitrL                             '
      vname_bgcflux(2,indxFlux+NFlux_DenitrL-1)=
     &     'Flux of N due to denitr of NO3 from LDetC '
      vname_bgcflux(3,indxFlux+NFlux_DenitrL-1)=
     &     'mmol N m-2 s-1                            '
      vname_bgcflux(4,indxFlux+NFlux_DenitrL-1)=
     &     'NFlux_DenitrL, scalar, series             '

#  endif

#  ifdef CARBON
      vname_bgcflux(1,indxFlux+CFlux_Zresp-1)=
     &     'CFlux_Zresp                                '
      vname_bgcflux(2,indxFlux+CFlux_Zresp-1)=
     &     'Flux of C due to zooplankton respiration   '
      vname_bgcflux(3,indxFlux+CFlux_Zresp-1)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxFlux+CFlux_Zresp-1)=
     &     'CFlux_Zresp, scalar, series                '

      vname_bgcflux(1,indxFlux+CFlux_DetCoagD-1)=
     &     'CFlux_DetCoagD                             '
      vname_bgcflux(2,indxFlux+CFlux_DetCoagD-1)=
     &     'Flux of C due to coagulation of small det. '
      vname_bgcflux(3,indxFlux+CFlux_DetCoagD-1)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxFlux+CFlux_DetCoagD-1)=
     &     'CFlux_DetCoagD, scalar, series             '

      vname_bgcflux(1,indxFlux+CFlux_ReminS-1)=
     &     'CFlux_ReminS                               '
      vname_bgcflux(2,indxFlux+CFlux_ReminS-1)=
     &     'Flux of C due to remin. of small detritus  '
      vname_bgcflux(3,indxFlux+CFlux_ReminS-1)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxFlux+CFlux_ReminS-1)=
     &     'CFlux_ReminS, scalar, series               '

      vname_bgcflux(1,indxFlux+CFlux_ReminL-1)=
     &     'CFlux_ReminL                               '
      vname_bgcflux(2,indxFlux+CFlux_ReminL-1)=
     &     'Flux of C due to remin. of large detritus  '
      vname_bgcflux(3,indxFlux+CFlux_ReminL-1)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxFlux+CFlux_ReminL-1)=
     &     'CFlux_ReminL, scalar, series               '

      vname_bgcflux(1,indxFlux+CFlux_Dissolv-1)=
     &     'CFlux_Dissolv                              '
      vname_bgcflux(2,indxFlux+CFlux_Dissolv-1)=
     &     'Flux of C due to CaCO3 dissolution         '
      vname_bgcflux(3,indxFlux+CFlux_Dissolv-1)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxFlux+CFlux_Dissolv-1)=
     &     'CFlux_Dissolv, scalar, series              '
#   endif /* CARBON */

!DL: some diagnostic variables related to phyto growth:
      vname_bgcflux(1,indxFlux+NFlux_Theta-1)=
     &     'Theta                               '
      vname_bgcflux(2,indxFlux+NFlux_Theta-1)=
     &     'Phytoplankton Chl/C ratio                 '
      vname_bgcflux(3,indxFlux+NFlux_Theta-1)=
     &     'mg Chla (mg C)-1                          '
      vname_bgcflux(4,indxFlux+NFlux_Theta-1)= ' '

      vname_bgcflux(1,indxFlux+NFlux_PhytoLimTemp-1)=
     &     'PhytoLimTemp                        '
      vname_bgcflux(2,indxFlux+NFlux_PhytoLimTemp-1)=
     &     'Temperature-dependent growth rate of P'
      vname_bgcflux(3,indxFlux+NFlux_PhytoLimTemp-1)=
     &     'day-1                                     '
      vname_bgcflux(4,indxFlux+NFlux_PhytoLimTemp-1)= ' '

      vname_bgcflux(1,indxFlux+NFlux_PhytoLimNO3-1)=
     &     'PhytoLimNO3                         '
      vname_bgcflux(2,indxFlux+NFlux_PhytoLimNO3-1)=
     &     'NO3 limitation factor of P        '
      vname_bgcflux(3,indxFlux+NFlux_PhytoLimNO3-1)=
     &     '                                          '
      vname_bgcflux(4,indxFlux+NFlux_PhytoLimNO3-1)= ' '

      vname_bgcflux(1,indxFlux+NFlux_PhytoLimNH4-1)=
     &     'PhytoLimNH4                         '
      vname_bgcflux(2,indxFlux+NFlux_PhytoLimNH4-1)=
     &     'NH4 limitation factor of P        '
      vname_bgcflux(3,indxFlux+NFlux_PhytoLimNH4-1)=
     &     '                                          '
      vname_bgcflux(4,indxFlux+NFlux_PhytoLimNH4-1)= ' '

      vname_bgcflux(1,indxFlux+NFlux_PhytoLimTempLight-1)=
     &     'PhytoLimTempLight                   '
      vname_bgcflux(2,indxFlux+NFlux_PhytoLimTempLight-1)=
     &     'Temperature-dependent, light-limited growth rate of P'
      vname_bgcflux(3,indxFlux+NFlux_PhytoLimTempLight-1)=
     &     'day-1                                     '
      vname_bgcflux(4,indxFlux+NFlux_PhytoLimTempLight-1)= ' '

!DL: moved to init_scalars_npzdoc:
! gas exchange terms
!#   ifdef OXYGEN
!      vname_bgcflux(1,indxGasExcFlux)=
!     &     'GasExchangeO2                              '
!      vname_bgcflux(2,indxGasExcFlux)=
!     &     'Gas exchange of oxygen at the surface      '
!      vname_bgcflux(3,indxGasExcFlux)=
!     &     'mmol O2 m-2 s-1                            '
!      vname_bgcflux(4,indxGasExcFlux)=
!     &     'GasExchangeO2, scalar, series              '
!#  endif /* OXYGEN */
!
!# ifdef CARBON
!      vname_bgcflux(1,indxGasExcFlux+1)=
!     &     'GasExchangeCO2                             '
!      vname_bgcflux(2,indxGasExcFlux+1)=
!     &     'Gas exchange of CO2 at the surface         '
!      vname_bgcflux(3,indxGasExcFlux+1)=
!     &     'mmol C m-2 s-1                             '
!      vname_bgcflux(4,indxGasExcFlux+1)=
!     &     'GasExchangeCO2, scalar, series             '
!#  endif /* CARBON */

! vertical sinking terms
      vname_bgcflux(1,indxVSinkFlux)=
     &     'VSinkFlux_Phyto                            '
      vname_bgcflux(2,indxVSinkFlux)=
     &     'Vertical sinking flux of phytoplankton     '
      vname_bgcflux(3,indxVSinkFlux)=
     &     'mmol N m-2 s-1                             '
      vname_bgcflux(4,indxVSinkFlux)=
     &     'VSinkFlux_Phyto, scalar, series            '

      vname_bgcflux(1,indxVSinkFlux+1)=
     &     'VSinkFlux_SDetN                            '
      vname_bgcflux(2,indxVSinkFlux+1)=
     &     'Vertical sinking flux of small detritus N  '
      vname_bgcflux(3,indxVSinkFlux+1)=
     &     'mmol N m-2 s-1                             '
      vname_bgcflux(4,indxVSinkFlux+1)=
     &     'VSinkFlux_SmallDetN, scalar, series        '

      vname_bgcflux(1,indxVSinkFlux+2)=
     &     'VSinkFlux_LDetN                            '
      vname_bgcflux(2,indxVSinkFlux+2)=
     &     'Vertical sinking flux of large detritus N  '
      vname_bgcflux(3,indxVSinkFlux+2)=
     &     'mmol N m-2 s-1                             '
      vname_bgcflux(4,indxVSinkFlux+2)=
     &     'VSinkFlux_LargeDetN, scalar, series        '

#  ifdef CARBON
      vname_bgcflux(1,indxVSinkFlux+3)=
     &     'VSinkFlux_SDetC                            '
      vname_bgcflux(2,indxVSinkFlux+3)=
     &     'Vertical sinking flux of small detritus C  '
      vname_bgcflux(3,indxVSinkFlux+3)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxVSinkFlux+3)=
     &     'VSinkFlux_SmallDetC, scalar, series        '

      vname_bgcflux(1,indxVSinkFlux+4)=
     &     'VSinkFlux_LDetC                            '
      vname_bgcflux(2,indxVSinkFlux+4)=
     &     'Vertical sinking flux of large detritus C  '
      vname_bgcflux(3,indxVSinkFlux+4)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxVSinkFlux+4)=
     &     'VSinkFlux_LargeDetC, scalar, series        '

      vname_bgcflux(1,indxVSinkFlux+5)=
     &     'VSinkFlux_CaCO3                            '
      vname_bgcflux(2,indxVSinkFlux+5)=
     &     'Vertical sinking flux of CaCO3             '
      vname_bgcflux(3,indxVSinkFlux+5)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxVSinkFlux+5)=
     &     'VSinkFlux_CaCO3, scalar, series            '
#  endif /* CARBON */
#  ifdef SEDIMENT_BIOLOGY
      vname_bgcflux(1,indxSedFlux)=
     &     'NFlux_ReminSed                             '
      vname_bgcflux(2,indxSedFlux)=
     &     'Flux of N due to remin. of sedimented mat. '
      vname_bgcflux(3,indxSedFlux)=
     &     'mmol N m-2 s-1                             '
      vname_bgcflux(4,indxSedFlux)=
     &     'NFlux_ReminSed, scalar, series             '
#   ifdef CARBON
#   ifdef OXYLIM_SED
      vname_bgcflux(1,indxSedFlux+1)=
     &     'NFlux_DenitrSed                             '
      vname_bgcflux(2,indxSedFlux+1)=
     &     'Flux of NO3 due to benthic denitrification  '
      vname_bgcflux(3,indxSedFlux+1)=
     &     'mmol N m-2 s-1                              '
      vname_bgcflux(4,indxSedFlux+1)=
     &     'NFlux_DenitrSed, scalar, series             '

      vname_bgcflux(1,indxSedFlux+2)=
     &     'CFlux_ReminSed                             '
      vname_bgcflux(2,indxSedFlux+2)=
     &     'Flux of C due to remin. of sedimented mat. '
      vname_bgcflux(3,indxSedFlux+2)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxSedFlux+2)=
     &     'CFlux_ReminSed, scalar, series             '

      vname_bgcflux(1,indxSedFlux+3)=
     &     'CFlux_DissolvSed                           '
      vname_bgcflux(2,indxSedFlux+3)=
     &     'Flux of C due to diss. of sedimented CaCO3 '
      vname_bgcflux(3,indxSedFlux+3)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxSedFlux+3)=
     &     'CFlux_DissolvSed, scalar, series           '
#  else /* OXYLIM_SED */
      vname_bgcflux(1,indxSedFlux+1)=
     &     'CFlux_ReminSed                             '
      vname_bgcflux(2,indxSedFlux+1)=
     &     'Flux of C due to remin. of sedimented mat. '
      vname_bgcflux(3,indxSedFlux+1)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxSedFlux+1)=
     &     'CFlux_ReminSed, scalar, series             '

      vname_bgcflux(1,indxSedFlux+2)=
     &     'CFlux_DissolvSed                           '
      vname_bgcflux(2,indxSedFlux+2)=
     &     'Flux of C due to diss. of sedimented CaCO3 '
      vname_bgcflux(3,indxSedFlux+2)=
     &     'mmol C m-2 s-1                             '
      vname_bgcflux(4,indxSedFlux+2)=
     &     'CFlux_DissolvSed, scalar, series           '
# endif /* OXYLIM_SED */
#   endif /* CARBON */
#  endif /* SEDIMENT_BIOLOGY */
      return
      end
#else /* BIOLOGY_NPZDOC */
      subroutine init_scalars_bgcflux_empty()
      return
      end
#endif /* BIOLOGY_NPZDOC */
