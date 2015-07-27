!
!
#include "cppdefs.h"
#ifdef BIOLOGY_BEC2

       subroutine init_scalars_bec2(ierr)
!
! Set initial values for  globally accessible (stored in common
! blocks) scalar variables of the BEC model.
!
      implicit none
      integer ierr, i,j, itrc ,lvar,lenstr
      integer omp_get_num_threads
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "comm_vars.h"
#include "ncroms_bgcflux.h"
#ifdef BEC2_DIAG
# include "ecosys_bec2.h"
#endif
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
      MPI_master_only write(*,'(1x,A,3(1x,A,I3),A)') 'NUMBER',
     &    'OF THREADS:',numthreads,'BLOCKING:',NSUB_X,'x',NSUB_E,'.'

#ifdef MPI
# define NUMPROC NNODES
#else
# define NUMPROC 1
#endif

      if (numthreads.gt.NUMPROC) then
        MPI_master_only write(*,'(/1x,A,I3/)')
     &    'ERROR: Requested number of threads exceeds setting: ',NUMPROC
        ierr=ierr+1                                      !--> ERROR
      elseif (mod(NSUB_X*NSUB_E,numthreads).ne.0) then
        MPI_master_only write(*,
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
      vname(1,indxPO4)='PO4                                       '
      vname(2,indxpO4)='Phosphate                                 '
      vname(3,indxpO4)='mMol P m-3                                '
      vname(4,indxpO4)='  '

CR      write(*,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='Nitrate                                   '
      vname(3,indxNO3)='mMol N m-3                                '
      vname(4,indxNO3)='  '

      vname(1,indxSIO3)='SiO3                                      '
      vname(2,indxSIO3)='Silicate                                  '
      vname(3,indxSIO3)='mMol Si m-3                               '
      vname(4,indxSIO3)='  '

CR      write(*,'(1x,A,I3)') 'indxNH4 =',indxNH4
      vname(1,indxNH4)='NH4                                       '
      vname(2,indxNH4)='Ammonium                                  '
      vname(3,indxNH4)='mMol N m-3                                '
      vname(4,indxNH4)='  '


      vname(1,indxFE)='Fe                                        '
      vname(2,indxFE)='Iron                                      '
      vname(3,indxFE)='mMol Fe m-3                               '
      vname(4,indxFE)='  '

      vname(1,indxO2)='O2                                        '
      vname(2,indxO2)='Oxygen                                    '
      vname(3,indxO2)='mMol O2 m-3                               '
      vname(4,indxO2)='  '

      vname(1,indxDIC)='DIC                                       '
      vname(2,indxDIC)='Dissolved inorganic carbon                '
      vname(3,indxDIC)='mMol C m-3                                '
      vname(4,indxDIC)='  '

      vname(1,indxALK)='Alk                                       '
      vname(2,indxALK)='Alkalinity                                '
      vname(3,indxALK)='mMol m-3                                '
      vname(4,indxALK)='  '

      vname(1,indxDOC)='DOC                                       '
      vname(2,indxDOC)='Dissolved organic carbon                  '
      vname(3,indxDOC)='mMol C m-3                                '
      vname(4,indxDOC)='  '

      vname(1,indxDon)='DON                                      '
      vname(2,indxDon)='Dissolved organic nitrogen               '
      vname(3,indxDon)='mMol N m-3                               '
      vname(4,indxDon)='  '

      vname(1,indxDofe)='DOFE                                     '
      vname(2,indxDofe)='Dissolved organic iron                   '
      vname(3,indxDofe)='mMol Fe m-3                              '
      vname(4,indxDofe)='  '

      vname(1,indxDop)='DOP                                      '
      vname(2,indxDop)='Dissolved organic phosphorus             '
      vname(3,indxDop)='mMol P m-3                               '
      vname(4,indxDop)='  '

      vname(1,indxDopr)='DOPR                                     '
      vname(2,indxDopr)='Refractory dissolved organic phosphorus  '
      vname(3,indxDopr)='mMol P m-3                               '
      vname(4,indxDopr)='  '

      vname(1,indxDonr)='DONR                                     '
      vname(2,indxDonr)='Refractory dissolved organic nitrogen    '
      vname(3,indxDonr)='mMol N m-3                               '
      vname(4,indxDonr)='  '

      vname(1,indxZOOC)='ZOOC                                     '
      vname(2,indxZOOC)='Zooplankton                              '
      vname(3,indxZOOC)='mMol C m-3                               '
      vname(4,indxZOOC)='  '

      vname(1,indxSPCHL)='SPCHL                                     '
      vname(2,indxSPCHL)='Small phytoplankton chlorophyll           '
      vname(3,indxSPCHL)='mg Chl-a m-3                              '
      vname(4,indxSPCHL)='  '

      vname(1,indxSPC)='SPC                                       '
      vname(2,indxSPC)='Small phytoplankton carbon                '
      vname(3,indxSPC)='mMol C m-3                                '
      vname(4,indxSPC)='  '

      vname(1,indxSPFE)='SPFE                                     '
      vname(2,indxSPFE)='Small phytoplankton iron                 '
      vname(3,indxSPFE)='mMol Fe m-3                              '
      vname(4,indxSPFE)='  '

      vname(1,indxSPCACO3)='SPCACO3                                   '
      vname(2,indxSPCACO3)='Small phytoplankton CaCO3                 '
      vname(3,indxSPCACO3)='mMol CaCO3 m-3                            '
      vname(4,indxSPCACO3)='  '

      vname(1,indxDIATCHL)='DIATCHL                                   '
      vname(2,indxDIATCHL)='Diatom chlorophyll                        '
      vname(3,indxDIATCHL)='mg Chl-a m-3                              '
      vname(4,indxDIATCHL)='  '

      vname(1,indxDIATC)='DIATC                                     '
      vname(2,indxDIATC)='Diatom carbon                             '
      vname(3,indxDIATC)='mMol C m-3                                '
      vname(4,indxDIATC)='  '

      vname(1,indxDIATFE)='DIATFE                                    '
      vname(2,indxDIATFE)='Diatom Iron                               '
      vname(3,indxDIATFE)='mMol Fe m-3                               '
      vname(4,indxDIATFE)='  '

      vname(1,indxDIATSI)='DIATSI                                    '
      vname(2,indxDIATSI)='Diatom silicon                            '
      vname(3,indxDIATSI)='mMol Si m-3                               '
      vname(4,indxDIATSI)='  '

      vname(1,indxDiazchl)='DIAZCHL                                  '
      vname(2,indxDiazchl)='Diazotroph chlorophyll                   '
      vname(3,indxDiazchl)='mg Chl-a m-3                             '
      vname(4,indxDiazchl)='  '

      vname(1,indxdiazc)='DIAZC                                    '
      vname(2,indxDiazc)='Diazotroph carbon                        '
      vname(3,indxDiazc)='mMol C m-3                               '
      vname(4,indxDiazc)='  '

      vname(1,indxDiazfe)='DIAZFE                                   '
      vname(2,indxDiazfe)='Diazotroph iron                          '
      vname(3,indxDiazfe)='mMol Fe m-3                              '
      vname(4,indxDiazfe)='  '

      vname(1,indxdust)='dust                                       '
      vname(2,indxdust)='dust flux                      '
      vname(3,indxdust)='kg m-2 s-1                                '
      vname(4,indxdust)='  '

      vname(1,indxiron)='iron                                    '
      vname(2,indxiron)='iron flux                      '
      vname(3,indxiron)='nmol cm-2 s-1                             '
      vname(4,indxiron)='  '

# ifdef BEC2_DIAG
      !
      ! 2d diagnotics:
      !
      vname_bec2_diag_2d(1,indxPH)='PH'
      vname_bec2_diag_2d(2,indxPH)='surf pH value'
      vname_bec2_diag_2d(3,indxPH)=' '
      vname_bec2_diag_2d(4,indxPH)=' '
      vname_bec2_diag_2d(1,indxPCO2)='pCO2                                 '
      vname_bec2_diag_2d(2,indxPCO2)='Surface water pCO2                   '
      vname_bec2_diag_2d(3,indxPCO2)='ppm                                  '
      vname_bec2_diag_2d(4,indxPCO2)='  '
      vname_bec2_diag_2d(1,indxPCO2air)='pCO2air                           '
      vname_bec2_diag_2d(2,indxPCO2air)='Atmospheric pCO2                  '
      vname_bec2_diag_2d(3,indxPCO2air)='ppm                               '
      vname_bec2_diag_2d(4,indxPCO2air)='           '
      vname_bec2_diag_2d(1,indxPARinc)='PARinc                                '
      vname_bec2_diag_2d(2,indxPARinc)=
     &  'Incoming photosynth. active radiation '
      vname_bec2_diag_2d(3,indxPARinc)='W m-2                                 '
      vname_bec2_diag_2d(4,indxPARinc)='  '
      vname_bec2_diag_2d(1,indxFGO2)='FG_O2'
      vname_bec2_diag_2d(2,indxFGO2)='Air-sea flux of O2'
      vname_bec2_diag_2d(3,indxFGO2)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxFGO2)=' '
      vname_bec2_diag_2d(1,indxFGCO2)='FG_CO2'
      vname_bec2_diag_2d(2,indxFGCO2)='Air-sea flux of CO2'
      vname_bec2_diag_2d(3,indxFGCO2)='mmol/m2/s '
      vname_bec2_diag_2d(4,indxFGCO2)=' '
      vname_bec2_diag_2d(1,indxWS10m)='WS10m'
      vname_bec2_diag_2d(2,indxWS10m)='Wind speed at 10m'
      vname_bec2_diag_2d(3,indxWS10m)='m/s '
      vname_bec2_diag_2d(4,indxWS10m)=' '
      vname_bec2_diag_2d(1,indxXKW)='XKW'
      vname_bec2_diag_2d(2,indxXKW)='XKW'
      vname_bec2_diag_2d(3,indxXKW)='m/s '
      vname_bec2_diag_2d(4,indxXKW)=' '
      vname_bec2_diag_2d(1,indxATMPRESS)='ATM_PRESS'
      vname_bec2_diag_2d(2,indxATMPRESS)='Atmospheric pressure'
      vname_bec2_diag_2d(3,indxATMPRESS)='atm '
      vname_bec2_diag_2d(4,indxATMPRESS)=' '

      vname_bec2_diag_2d(1,indxSCHMIDTO2)='SCHMIDT_O2'
      vname_bec2_diag_2d(2,indxSCHMIDTO2)='Schmidt number for O2'
      vname_bec2_diag_2d(3,indxSCHMIDTO2)=' '
      vname_bec2_diag_2d(4,indxSCHMIDTO2)=' '

      vname_bec2_diag_2d(1,indxO2SAT)='O2SAT'
      vname_bec2_diag_2d(2,indxO2SAT)='O2 saturation concentration'
      vname_bec2_diag_2d(3,indxO2SAT)='mmol/m3 '
      vname_bec2_diag_2d(4,indxO2SAT)=' '
      vname_bec2_diag_2d(1,indxSCHMIDTCO2)='SCHMIDT_CO2'
      vname_bec2_diag_2d(2,indxSCHMIDTCO2)='Schmidt number for CO2'
      vname_bec2_diag_2d(3,indxSCHMIDTCO2)=' '
      vname_bec2_diag_2d(4,indxSCHMIDTCO2)=' '

      vname_bec2_diag_2d(1,indxPVO2)='PV_O2'
      vname_bec2_diag_2d(2,indxPVO2)='Piston velocity for O2'
      vname_bec2_diag_2d(3,indxPVO2)='m/s '
      vname_bec2_diag_2d(4,indxPVO2)=' '
      vname_bec2_diag_2d(1,indxPVCO2)='PV_CO2'
      vname_bec2_diag_2d(2,indxPVCO2)='Piston velocity for CO2'
      vname_bec2_diag_2d(3,indxPVCO2)='m/s '
      vname_bec2_diag_2d(4,indxPVCO2)=' '

      vname_bec2_diag_2d(1,indxCO2STAR)='CO2STAR'
      vname_bec2_diag_2d(2,indxCO2STAR)='CO2STAR'
      vname_bec2_diag_2d(3,indxCO2STAR)='mmol/m3 '
      vname_bec2_diag_2d(4,indxCO2STAR)=' '
      vname_bec2_diag_2d(1,indxDCO2STAR)='DCO2STAR'
      vname_bec2_diag_2d(2,indxDCO2STAR)='DCO2STAR'
      vname_bec2_diag_2d(3,indxDCO2STAR)='mmol/m3'
      vname_bec2_diag_2d(4,indxDCO2STAR)=' '

      vname_bec2_diag_2d(1,indxIRONFLUX)='IRON_FLUX'
      vname_bec2_diag_2d(2,indxIRONFLUX)='Iron surface flux'
      vname_bec2_diag_2d(3,indxIRONFLUX)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxIRONFLUX)=' '
      vname_bec2_diag_2d(1,indxSEDDENITRIF)='SED_DENITRIF'
      vname_bec2_diag_2d(2,indxSEDDENITRIF)='Sediment denitrification'
      vname_bec2_diag_2d(3,indxSEDDENITRIF)='mmol/m3/s'
      vname_bec2_diag_2d(4,indxSEDDENITRIF)=' '

      !
      ! 3d diagnostics:
      !
      vname_bec2_diag_3d(1,indxPAR)='PAR'
      vname_bec2_diag_3d(2,indxPAR)=
     &  'Photosynthetically active radiation'
      vname_bec2_diag_3d(3,indxPAR)='W m-2'
      vname_bec2_diag_3d(4,indxPAR)='  '

      vname_bec2_diag_3d(1,indxPOCFLUXIN)='POC_FLUX_IN'
      vname_bec2_diag_3d(2,indxPOCFLUXIN)='POC flux into cell'
      vname_bec2_diag_3d(3,indxPOCFLUXIN)='mmol C/m2/s '
      vname_bec2_diag_3d(4,indxPOCFLUXIN)=' '

      vname_bec2_diag_3d(1,indxPOCPROD)='POC_PROD'
      vname_bec2_diag_3d(2,indxPOCPROD)='POC production'
      vname_bec2_diag_3d(3,indxPOCPROD)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPOCPROD)=' '
      vname_bec2_diag_3d(1,indxPOCREMIN)='POC_REMIN'
      vname_bec2_diag_3d(2,indxPOCREMIN)='POC remineralization'
      vname_bec2_diag_3d(3,indxPOCREMIN)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxPOCREMIN)=' '

      vname_bec2_diag_3d(1,indxCACO3FLUXIN)='CACO3_FLUX_IN'
      vname_bec2_diag_3d(2,indxCACO3FLUXIN)=
     &  'Incoming flux of large detrital CaCO3'
      vname_bec2_diag_3d(3,indxCACO3FLUXIN)='mmol CaCO3/m2/s '
      vname_bec2_diag_3d(4,indxCACO3FLUXIN)=' '
      vname_bec2_diag_3d(1,indxPCACO3PROD)='PCACO3_PROD'
      vname_bec2_diag_3d(2,indxPCACO3PROD)=
     &  'Production of large detrital CaCO3'
      vname_bec2_diag_3d(3,indxPCACO3PROD)='mmol CaCO3/m3/s '
      vname_bec2_diag_3d(4,indxPCACO3PROD)=' '
      vname_bec2_diag_3d(1,indxCACO3REMIN)='CACO3_REMIN'
      vname_bec2_diag_3d(2,indxCACO3REMIN)=
     &  'Remineralization of large detrital CaCO3'
      vname_bec2_diag_3d(3,indxCACO3REMIN)='mmol CaCO3/m3/s '
      vname_bec2_diag_3d(4,indxCACO3REMIN)=' '

      vname_bec2_diag_3d(1,indxSIO2FLUXIN)='SIO2_FLUX_IN'
      vname_bec2_diag_3d(2,indxSIO2FLUXIN)=
     &  'Incoming flux of large detritus SiO2'
      vname_bec2_diag_3d(3,indxSIO2FLUXIN)='mmol Si/m2/s '
      vname_bec2_diag_3d(4,indxSIO2FLUXIN)=' '
      vname_bec2_diag_3d(1,indxSIO2PROD)='SIO2_PROD'
      vname_bec2_diag_3d(2,indxSIO2PROD)=
     &  'Production of large detritus SiO2'
      vname_bec2_diag_3d(3,indxSIO2PROD)='mmol Si/m3/s '
      vname_bec2_diag_3d(4,indxSIO2PROD)=' '
      vname_bec2_diag_3d(1,indxSIO2REMIN)='SIO2_REMIN'
      vname_bec2_diag_3d(2,indxSIO2REMIN)=
     &  'Remineralization of large detritus SiO2'
      vname_bec2_diag_3d(3,indxSIO2REMIN)='mmol Si/m3/s'
      vname_bec2_diag_3d(4,indxSIO2REMIN)=' '

      vname_bec2_diag_3d(1,indxDUSTFLUXIN)='DUST_FLUX_IN'
      vname_bec2_diag_3d(2,indxDUSTFLUXIN)=
     &  'Incoming dust flux'
      vname_bec2_diag_3d(3,indxDUSTFLUXIN)='kg dust/m2/s '
      vname_bec2_diag_3d(4,indxDUSTFLUXIN)=' '
      vname_bec2_diag_3d(1,indxDUSTREMIN)='DUST_REMIN'
      vname_bec2_diag_3d(2,indxDUSTREMIN)=
     &  'Remineralization of dust'
      vname_bec2_diag_3d(3,indxDUSTREMIN)='kg dust/m3/s '
      vname_bec2_diag_3d(4,indxDUSTREMIN)=' '

      vname_bec2_diag_3d(1,indxPIRONFLUXIN)='P_IRON_FLUX_IN'
      vname_bec2_diag_3d(2,indxPIRONFLUXIN)=
     &  'Incoming flux of large detritus iron'
      vname_bec2_diag_3d(3,indxPIRONFLUXIN)='mmol Fe/m2/s '
      vname_bec2_diag_3d(4,indxPIRONFLUXIN)=' '
      vname_bec2_diag_3d(1,indxPIRONPROD)='P_IRON_PROD'
      vname_bec2_diag_3d(2,indxPIRONPROD)=
     &  'Production of large detritus iron'
      vname_bec2_diag_3d(3,indxPIRONPROD)='mmol Fe/m3/s '
      vname_bec2_diag_3d(4,indxPIRONPROD)=' '
      vname_bec2_diag_3d(1,indxPIRONREMIN)='P_IRON_REMIN'
      vname_bec2_diag_3d(2,indxPIRONREMIN)=
     &  'Remineralization of large detritus iron'
      vname_bec2_diag_3d(3,indxPIRONREMIN)='mmol Fe/m3/s '
      vname_bec2_diag_3d(4,indxPIRONREMIN)=' '

      vname_bec2_diag_3d(1,indxGRAZESP)='GRAZE_SP'
      vname_bec2_diag_3d(2,indxGRAZESP)=
     &  'Grazing rate on small phytoplankton'
      vname_bec2_diag_3d(3,indxGRAZESP)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxGRAZESP)=' '
      vname_bec2_diag_3d(1,indxGRAZEDIAT)='GRAZE_DIAT'
      vname_bec2_diag_3d(2,indxGRAZEDIAT)=
     &  'Grazing rate on diatoms'
      vname_bec2_diag_3d(3,indxGRAZEDIAT)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZEDIAT)=' '
      vname_bec2_diag_3d(1,indxGRAZEDIAZ)='GRAZE_DIAZ'
      vname_bec2_diag_3d(2,indxGRAZEDIAZ)=
     &  'Grazing rate on diazotrophs'
      vname_bec2_diag_3d(3,indxGRAZEDIAZ)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZEDIAZ)=' '

      vname_bec2_diag_3d(1,indxSPLOSS)='SP_LOSS'
      vname_bec2_diag_3d(2,indxSPLOSS)=
     &  'Small phytoplankton non-grazing mortality'
      vname_bec2_diag_3d(3,indxSPLOSS)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxSPLOSS)=' '
      vname_bec2_diag_3d(1,indxDIATLOSS)='DIAT_LOSS'
      vname_bec2_diag_3d(2,indxDIATLOSS)=
     &  'Diatom non-grazing mortality'
      vname_bec2_diag_3d(3,indxDIATLOSS)='mmol/m3/s '
      vname_bec2_diag_3d(4,indxDIATLOSS)=' '
      vname_bec2_diag_3d(1,indxZOOLOSS)='ZOO_LOSS'
      vname_bec2_diag_3d(2,indxZOOLOSS)=
     &  'Mortality due to higher trophic grazing on zooplankton'
      vname_bec2_diag_3d(3,indxZOOLOSS)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxZOOLOSS)=' '

      vname_bec2_diag_3d(1,indxSPAGG)='SP_AGG'
      vname_bec2_diag_3d(2,indxSPAGG)=
     &  'Aggregation of small phytoplankton'
      vname_bec2_diag_3d(3,indxSPAGG)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxSPAGG)=' '
      vname_bec2_diag_3d(1,indxDIATAGG)='DIAT_AGG'
      vname_bec2_diag_3d(2,indxDIATAGG)=
     &  'Aggregation of diatoms'
      vname_bec2_diag_3d(3,indxDIATAGG)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxDIATAGG)=' '

      vname_bec2_diag_3d(1,indxPHOTOCSP)='PHOTOC_SP'
      vname_bec2_diag_3d(2,indxPHOTOCSP)=
     &  'C fixation rate by small phytoplankton'
      vname_bec2_diag_3d(3,indxPHOTOCSP)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHOTOCSP)=' '
      vname_bec2_diag_3d(1,indxPHOTOCDIAT)='PHOTOC_DIAT'
      vname_bec2_diag_3d(2,indxPHOTOCDIAT)=
     &  'C fixation rate by diatoms'
      vname_bec2_diag_3d(3,indxPHOTOCDIAT)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHOTOCDIAT)=' '

      vname_bec2_diag_3d(1,indxTOTPROD)='TOT_PROD'
      vname_bec2_diag_3d(2,indxTOTPROD)=
     &  'Total autotroph production'
      vname_bec2_diag_3d(3,indxTOTPROD)='mmol/m3/s '
      vname_bec2_diag_3d(4,indxTOTPROD)=' '
      vname_bec2_diag_3d(1,indxDOCPROD)='DOC_PROD'
      vname_bec2_diag_3d(2,indxDOCPROD)='DOC production'
      vname_bec2_diag_3d(3,indxDOCPROD)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxDOCPROD)=' '

      vname_bec2_diag_3d(1,indxDOCREMIN)='DOC_REMIN'
      vname_bec2_diag_3d(2,indxDOCREMIN)=
     &  'Remineralization rate of DOC'
      vname_bec2_diag_3d(3,indxDOCREMIN)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxDOCREMIN)=' '
      vname_bec2_diag_3d(1,indxFESCAVENGE)='FE_SCAVENGE'
      vname_bec2_diag_3d(2,indxFESCAVENGE)=
     &  'Loss of dissolved iron, scavenging'
      vname_bec2_diag_3d(3,indxFESCAVENGE)='mmol/m3/s '
      vname_bec2_diag_3d(4,indxFESCAVENGE)=' '

      vname_bec2_diag_3d(1,indxSPNLIM)='SP_N_LIM'
      vname_bec2_diag_3d(2,indxSPNLIM)=
     &  'N limitation factor of small phytoplankton'
      vname_bec2_diag_3d(3,indxSPNLIM)=' '
      vname_bec2_diag_3d(4,indxSPNLIM)=' '
      vname_bec2_diag_3d(1,indxSPFEUPTAKE)='SP_FE_LIM'
      vname_bec2_diag_3d(2,indxSPFEUPTAKE)=
     &  'Fe limitation factor of small phytoplankton'
      vname_bec2_diag_3d(3,indxSPFEUPTAKE)=' '
      vname_bec2_diag_3d(4,indxSPFEUPTAKE)=' '
      vname_bec2_diag_3d(1,indxSPPO4UPTAKE)='SP_PO4_LIM'
      vname_bec2_diag_3d(2,indxSPPO4UPTAKE)=
     &  'PO4 limitation factor of small phytoplankton'
      vname_bec2_diag_3d(3,indxSPPO4UPTAKE)=' '
      vname_bec2_diag_3d(4,indxSPPO4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxSPLIGHTLIM)='SP_LIGHT_LIM'
      vname_bec2_diag_3d(2,indxSPLIGHTLIM)=
     &  'Small phytoplankton light limitation factor'
      vname_bec2_diag_3d(3,indxSPLIGHTLIM)=' '
      vname_bec2_diag_3d(4,indxSPLIGHTLIM)=' '

      vname_bec2_diag_3d(1,indxDIATNLIM)='DIAT_N_LIM'
      vname_bec2_diag_3d(2,indxDIATNLIM)=
     &  'N limitation factor of diatoms'
      vname_bec2_diag_3d(3,indxDIATNLIM)=' '
      vname_bec2_diag_3d(4,indxDIATNLIM)=' '
      vname_bec2_diag_3d(1,indxDIATFEUPTAKE)='DIAT_FE_LIM'
      vname_bec2_diag_3d(2,indxDIATFEUPTAKE)=
     &  'Fe limitation factor of diatoms'
      vname_bec2_diag_3d(3,indxDIATFEUPTAKE)=' '
      vname_bec2_diag_3d(4,indxDIATFEUPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIATPO4UPTAKE)='DIAT_PO4_LIM'
      vname_bec2_diag_3d(2,indxDIATPO4UPTAKE)=
     &  'PO4 limitation factor of diatoms'
      vname_bec2_diag_3d(3,indxDIATPO4UPTAKE)=' '
      vname_bec2_diag_3d(4,indxDIATPO4UPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIATSIO3UPTAKE)='DIAT_SIO3_LIM'
      vname_bec2_diag_3d(2,indxDIATSIO3UPTAKE)=
     &  'SiO3 limitation factor of diatoms'
      vname_bec2_diag_3d(3,indxDIATSIO3UPTAKE)=' '
      vname_bec2_diag_3d(4,indxDIATSIO3UPTAKE)=' '

      vname_bec2_diag_3d(1,indxDIATLIGHTLIM)='DIAT_LIGHT_LIM'
      vname_bec2_diag_3d(2,indxDIATLIGHTLIM)=
     &  'Diatom light limitation factor'
      vname_bec2_diag_3d(3,indxDIATLIGHTLIM)=' '
      vname_bec2_diag_3d(4,indxDIATLIGHTLIM)=' '

      vname_bec2_diag_3d(1,indxCACO3PROD)='CACO3_PROD'
      vname_bec2_diag_3d(2,indxCACO3PROD)=
     &  'Production of CaCO3 by small phytoplankton'
      vname_bec2_diag_3d(3,indxCACO3PROD)='mmol CaCO3/m3/s '
      vname_bec2_diag_3d(4,indxCACO3PROD)=' '
      vname_bec2_diag_3d(1,indxDIAZNFIX)='DIAZ_NFIX'
      vname_bec2_diag_3d(2,indxDIAZNFIX)=
     &  'Total nitrogen fixation by diazotrophs'
      vname_bec2_diag_3d(3,indxDIAZNFIX)='mmol/m3/s   '
      vname_bec2_diag_3d(4,indxDIAZNFIX)=' '

      vname_bec2_diag_3d(1,indxDIAZLOSS)='DIAZ_LOSS'
      vname_bec2_diag_3d(2,indxDIAZLOSS)=
     &  'Diazotroph non-grazing mortality'
      vname_bec2_diag_3d(3,indxDIAZLOSS)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxDIAZLOSS)=' '
      vname_bec2_diag_3d(1,indxPHOTOCDIAZ)='PHOTOC_DIAZ'
      vname_bec2_diag_3d(2,indxPHOTOCDIAZ)=
     &   'Diazotroph C-fixation'
      vname_bec2_diag_3d(3,indxPHOTOCDIAZ)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxPHOTOCDIAZ)=' '

      vname_bec2_diag_3d(1,indxDIAZPO4UPTAKE)='DIAZ_PO4_LIM'
      vname_bec2_diag_3d(2,indxDIAZPO4UPTAKE)=
     &  'PO4 limitation factor of diazotrophs'
      vname_bec2_diag_3d(3,indxDIAZPO4UPTAKE)=' '
      vname_bec2_diag_3d(4,indxDIAZPO4UPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIAZFEUPTAKE)='DIAZ_FE_LIM'
      vname_bec2_diag_3d(2,indxDIAZFEUPTAKE)=
     &  'Fe limitation factor of diazotrophs'
      vname_bec2_diag_3d(3,indxDIAZFEUPTAKE)=' '
      vname_bec2_diag_3d(4,indxDIAZFEUPTAKE)=' '

      vname_bec2_diag_3d(1,indxDIAZLIGHTLIM)='DIAZ_LIGHT_LIM'
      vname_bec2_diag_3d(2,indxDIAZLIGHTLIM)=
     &   'Diazotroph light limitation'
      vname_bec2_diag_3d(3,indxDIAZLIGHTLIM)=' '
      vname_bec2_diag_3d(4,indxDIAZLIGHTLIM)=' '
      vname_bec2_diag_3d(1,indxFESCAVENGERATE)='FE_SCAVENGE_RATE'
      vname_bec2_diag_3d(2,indxFESCAVENGERATE)=
     &  'Annual scavenging rate of iron as % of ambient'
      vname_bec2_diag_3d(3,indxFESCAVENGERATE)=' '
      vname_bec2_diag_3d(4,indxFESCAVENGERATE)=' '

      vname_bec2_diag_3d(1,indxDONPROD)='DON_PROD'
      vname_bec2_diag_3d(2,indxDONPROD)=
     &  'Production of dissolved organic N'
      vname_bec2_diag_3d(3,indxDONPROD)=' '
      vname_bec2_diag_3d(4,indxDONPROD)=' '
      vname_bec2_diag_3d(1,indxDONREMIN)='DON_REMIN'
      vname_bec2_diag_3d(2,indxDONREMIN)=
     &  'Remineralization rate of DON'
      vname_bec2_diag_3d(3,indxDONREMIN)='mmol N/m3/s  '
      vname_bec2_diag_3d(4,indxDONREMIN)=' '

      vname_bec2_diag_3d(1,indxDOFEPROD)='DOFE_PROD'
      vname_bec2_diag_3d(2,indxDOFEPROD)=
     &  'Production of dissolved organic iron'
      vname_bec2_diag_3d(3,indxDOFEPROD)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDOFEPROD)=' '
      vname_bec2_diag_3d(1,indxDOFEREMIN)='DOFE_REMIN'
      vname_bec2_diag_3d(2,indxDOFEREMIN)=
     &  'Remineralization rate of DOFE'
      vname_bec2_diag_3d(3,indxDOFEREMIN)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDOFEREMIN)=' '

      vname_bec2_diag_3d(1,indxDOPPROD)='DOP_PROD'
      vname_bec2_diag_3d(2,indxDOPPROD)=
     &  'Production of dissolved organic P'
      vname_bec2_diag_3d(3,indxDOPPROD)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDOPPROD)=' '
      vname_bec2_diag_3d(1,indxDOPREMIN)='DOP_REMIN'
      vname_bec2_diag_3d(2,indxDOPREMIN)=
     &  'Remineralization rate of DOP'
      vname_bec2_diag_3d(3,indxDOPREMIN)='mmol/m3/s '
      vname_bec2_diag_3d(4,indxDOPREMIN)=' '

      vname_bec2_diag_3d(1,indxDIATSIUPTAKE)='DIAT_SI_UPTAKE'
      vname_bec2_diag_3d(2,indxDIATSIUPTAKE)=
     &  'Silicon uptake rate by diatoms'
      vname_bec2_diag_3d(3,indxDIATSIUPTAKE)='mmol Si/m3/s  '
      vname_bec2_diag_3d(4,indxDIATSIUPTAKE)=' '

      vname_bec2_diag_3d(1,indxIRONUPTAKESP)='PHOTOFE_SP'
      vname_bec2_diag_3d(2,indxIRONUPTAKESP)=
     &  'Iron uptake rate by small phytoplankton'
      vname_bec2_diag_3d(3,indxIRONUPTAKESP)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxIRONUPTAKESP)=' '
      vname_bec2_diag_3d(1,indxIRONUPTAKEDIAT)='PHOTOFE_DIAT'
      vname_bec2_diag_3d(2,indxIRONUPTAKEDIAT)=
     &  'Iron uptake rate by diatoms'
      vname_bec2_diag_3d(3,indxIRONUPTAKEDIAT)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxIRONUPTAKEDIAT)=' '
      vname_bec2_diag_3d(1,indxIRONUPTAKEDIAZ)='PHOTOFE_DIAZ'
      vname_bec2_diag_3d(2,indxIRONUPTAKEDIAZ)=
     &  'Iron uptake rate by diatotrophs'
      vname_bec2_diag_3d(3,indxIRONUPTAKEDIAZ)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxIRONUPTAKEDIAZ)=' '

      vname_bec2_diag_3d(1,indxNITRIF)='NITRIF'
      vname_bec2_diag_3d(2,indxNITRIF)='Nitrification'
      vname_bec2_diag_3d(3,indxNITRIF)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxNITRIF)=' '
      vname_bec2_diag_3d(1,indxDENITRIF)='DENITRIF'
      vname_bec2_diag_3d(2,indxDENITRIF)='Denitrification'
      vname_bec2_diag_3d(3,indxDENITRIF)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDENITRIF)=' '

      vname_bec2_diag_3d(1,indxSPNO3UPTAKE)='SP_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxSPNO3UPTAKE)=
     &   'NO3 uptake rate of small phyto'
      vname_bec2_diag_3d(3,indxSPNO3UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxSPNO3UPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIATNO3UPTAKE)='DIAT_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxDIATNO3UPTAKE)=
     &   'NO3 uptake rate of diatoms'
      vname_bec2_diag_3d(3,indxDIATNO3UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDIATNO3UPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIAZNO3UPTAKE)='DIAZ_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxDIAZNO3UPTAKE)=
     &   'NO3 uptake rate of diazotrophs'
      vname_bec2_diag_3d(3,indxDIAZNO3UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDIAZNO3UPTAKE)=' '

      vname_bec2_diag_3d(1,indxSPNH4UPTAKE)='SP_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxSPNH4UPTAKE)=
     &   'NH4 uptake rate of small phyto'
      vname_bec2_diag_3d(3,indxSPNH4UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxSPNH4UPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIATNH4UPTAKE)='DIAT_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxDIATNH4UPTAKE)=
     &   'NH4 uptake rate of diatoms'
      vname_bec2_diag_3d(3,indxDIATNH4UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDIATNH4UPTAKE)=' '
      vname_bec2_diag_3d(1,indxDIAZNH4UPTAKE)='DIAZ_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxDIAZNH4UPTAKE)=
     &   'NH4 uptake rate of diazotrophs'
      vname_bec2_diag_3d(3,indxDIAZNH4UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDIAZNH4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxGRAZEDICSP)='SP_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxGRAZEDICSP)=
     &   'Small phyto grazing rate routed to DIC'
      vname_bec2_diag_3d(3,indxGRAZEDICSP)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxGRAZEDICSP)=' '
      vname_bec2_diag_3d(1,indxGRAZEDICDIAT)='DIAT_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxGRAZEDICDIAT)=
     &   'Diatom grazing rate routed to DIC'
      vname_bec2_diag_3d(3,indxGRAZEDICDIAT)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxGRAZEDICDIAT)=' '
      vname_bec2_diag_3d(1,indxGRAZEDICDIAZ)='DIAZ_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxGRAZEDICDIAZ)=
     &   'Diazotroph grazing rate routed to DIC'
      vname_bec2_diag_3d(3,indxGRAZEDICDIAZ)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxGRAZEDICDIAZ)=' '

      vname_bec2_diag_3d(1,indxLOSSDICSP)='SP_LOSS_DIC'
      vname_bec2_diag_3d(2,indxLOSSDICSP)=
     &   'Small phyto non-grazing mortality routed to DIC'
      vname_bec2_diag_3d(3,indxLOSSDICSP)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxLOSSDICSP)=' '
      vname_bec2_diag_3d(1,indxLOSSDICDIAT)='DIAT_LOSS_DIC'
      vname_bec2_diag_3d(2,indxLOSSDICDIAT)=
     &   'Diatom non-grazing mortality routed to DIC'
      vname_bec2_diag_3d(3,indxLOSSDICDIAT)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxLOSSDICDIAT)=' '
      vname_bec2_diag_3d(1,indxLOSSDICDIAZ)='DIAZ_LOSS_DIC'
      vname_bec2_diag_3d(2,indxLOSSDICDIAZ)=
     &   'Diazotroph non-grazing mortality routed to DIC'
      vname_bec2_diag_3d(3,indxLOSSDICDIAZ)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxLOSSDICDIAZ)=' '

      vname_bec2_diag_3d(1,indxZOOLOSSDIC)='ZOO_LOSS_DIC'
      vname_bec2_diag_3d(2,indxZOOLOSSDIC)=
     &   'Zooplankton loss routed to DIC'
      vname_bec2_diag_3d(3,indxZOOLOSSDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxZOOLOSSDIC)=' '

      vname_bec2_diag_3d(1,indxDIAZAGG)='DIAZ_AGG'
      vname_bec2_diag_3d(2,indxDIAZAGG)=
     &  'Aggregation of diatoms'
      vname_bec2_diag_3d(3,indxDIAZAGG)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxDIAZAGG)=' '

      vname_bec2_diag_3d(1,indxGRAZESPZOO)='GRAZE_SP_ZOO'
      vname_bec2_diag_3d(2,indxGRAZESPZOO)=
     &  'Grazing rate on small phytoplankton routed to zoo'
      vname_bec2_diag_3d(3,indxGRAZESPZOO)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxGRAZESPZOO)=' '
      vname_bec2_diag_3d(1,indxGRAZEDIATZOO)='GRAZE_DIAT_ZOO'
      vname_bec2_diag_3d(2,indxGRAZEDIATZOO)=
     &  'Grazing rate on diatoms routed to zoo'
      vname_bec2_diag_3d(3,indxGRAZEDIATZOO)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZEDIATZOO)=' '
      vname_bec2_diag_3d(1,indxGRAZEDIAZZOO)='GRAZE_DIAZ_ZOO'
      vname_bec2_diag_3d(2,indxGRAZEDIAZZOO)=
     &  'Grazing rate on diazotrophs routed to zoo'
      vname_bec2_diag_3d(3,indxGRAZEDIAZZOO)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZEDIAZZOO)=' '

      vname_bec2_diag_3d(1,indxSPQCACO3)='QCACO3'
      vname_bec2_diag_3d(2,indxSPQCACO3)=
     &  'Small phyto CaCO3/C ratio'
      vname_bec2_diag_3d(3,indxSPQCACO3)='mmol CaCO3/mmol C'
      vname_bec2_diag_3d(4,indxSPQCACO3)=' '

      vname_bec2_diag_3d(1,indxSPPHOTOACC)='PHOTOACC_SP'
      vname_bec2_diag_3d(2,indxSPPHOTOACC)=
     &  'Chl synthesis in photoadaptation for small phyto'
      vname_bec2_diag_3d(3,indxSPPHOTOACC)='mg Chl/m3/s'
      vname_bec2_diag_3d(4,indxSPPHOTOACC)=' '
      vname_bec2_diag_3d(1,indxDIATPHOTOACC)='PHOTOACC_DIAT'
      vname_bec2_diag_3d(2,indxDIATPHOTOACC)=
     &  'Chl synthesis in photoadaptation for diatoms'
      vname_bec2_diag_3d(3,indxDIATPHOTOACC)='mg Chl/m3/s'
      vname_bec2_diag_3d(4,indxDIATPHOTOACC)=' '
      vname_bec2_diag_3d(1,indxDIAZPHOTOACC)='PHOTOACC_DIAZ'
      vname_bec2_diag_3d(2,indxDIAZPHOTOACC)=
     &  'Chl synthesis in photoadaptation for diazotrophs'
      vname_bec2_diag_3d(3,indxDIAZPHOTOACC)='mg Chl/m3/s'
      vname_bec2_diag_3d(4,indxDIAZPHOTOACC)=' '

      vname_bec2_diag_3d(1,indxSPCZERO)='SPC_ZERO'
      vname_bec2_diag_3d(2,indxSPCZERO)=
     &  'Change caused by setting negative SPC values to zero'
      vname_bec2_diag_3d(3,indxSPCZERO)='mMol C/m3/s'
      vname_bec2_diag_3d(4,indxSPCZERO)=' '
      vname_bec2_diag_3d(1,indxDIATCZERO)='DIATC_ZERO'
      vname_bec2_diag_3d(2,indxDIATCZERO)=
     &  'Change caused by setting negative DIATC values to zero'
      vname_bec2_diag_3d(3,indxDIATCZERO)='mMol C/m3/s'
      vname_bec2_diag_3d(4,indxDIATCZERO)=' '
      vname_bec2_diag_3d(1,indxDIAZCZERO)='DIAZC_ZERO'
      vname_bec2_diag_3d(2,indxDIAZCZERO)=
     &  'Change caused by setting negative DIAZC values to zero'
      vname_bec2_diag_3d(3,indxDIAZCZERO)='mMol C/m3/s'
      vname_bec2_diag_3d(4,indxDIAZCZERO)=' '
      vname_bec2_diag_3d(1,indxDOCZERO)='DOC_ZERO'
      vname_bec2_diag_3d(2,indxDOCZERO)=
     &  'Change caused by setting negative DOC values to zero'
      vname_bec2_diag_3d(3,indxDOCZERO)='mMol C/m3/s'
      vname_bec2_diag_3d(4,indxDOCZERO)=' '
      vname_bec2_diag_3d(1,indxZOOCZERO)='ZOOC_ZERO'
      vname_bec2_diag_3d(2,indxZOOCZERO)=
     &  'Change caused by setting negative ZOOC values to zero'
      vname_bec2_diag_3d(3,indxZOOCZERO)='mMol C/m3/s'
      vname_bec2_diag_3d(4,indxZOOCZERO)=' '
      vname_bec2_diag_3d(1,indxSPCACO3ZERO)='SPCACO3_ZERO'
      vname_bec2_diag_3d(2,indxSPCACO3ZERO)=
     &  'Change caused by setting negative SPCACO3 values to zero'
      vname_bec2_diag_3d(3,indxSPCACO3ZERO)='mMol CaCO3/m3/s'
      vname_bec2_diag_3d(4,indxSPCACO3ZERO)=' '

      vname_bec2_diag_3d(1,indxDONRREMIN) = 'DONr_REMIN'
      vname_bec2_diag_3d(2,indxDONRREMIN)=
     &  'Portion of refractory DON remineralized'
      vname_bec2_diag_3d(3,indxDONRREMIN)='mMol N/m3/s'
      vname_bec2_diag_3d(4,indxDONRREMIN)=' '

# endif /* BEC2_DIAG */

      tclm_name(iPO4) = 'po4_time'
      tclm_name(iNO3) = 'no3_time'
      tclm_name(iSIO3) = 'sio3_time'
      tclm_name(iO2) = 'o2_time'
      tclm_name(iFE) = 'fe_time'
      tclm_name(iDIC) = 'dic_time'
      tclm_name(iALK) = 'alk_time'
      tclm_name(iNH4) = 'nh4_time'
      tclm_name(iDOC) = 'doc_time'
      tclm_name(iDOP) = 'dop_time'
      tclm_name(iDOP) = 'dop_time'
      tclm_name(iDOPR) = 'dop_time'
      tclm_name(iDON) = 'don_time'
      tclm_name(iDONR) = 'don_time'
      tclm_name(iDOFE) = 'dofe_time'
      tclm_name(iSPC) = 'spc_time'
      tclm_name(iSPCHL) = 'spchl_time'
      tclm_name(iSPCACO3) = 'spcaco3_time'
      tclm_name(iSPFE) = 'spfe_time'
      tclm_name(iDIATC) = 'diatc_time'
      tclm_name(iDIATCHL) = 'diatchl_time'
      tclm_name(iDIATSI) = 'diatsi_time'
      tclm_name(iDIATFE) = 'diatfe_time'
      tclm_name(iZOOC) = 'zooc_time'
      tclm_name(iDIAZC) = 'diazc_time'
      tclm_name(iDIAZCHL) = 'diazchl_time'
      tclm_name(iDIAZFE) = 'diazfe_time'
#endif /* SOLVE3D */
      return
      end
#else /* BIOLOGY_BEC2 */
      subroutine init_scalars_bec2_empty()
      return
      end
#endif /* BIOLOGY_BEC2 */
