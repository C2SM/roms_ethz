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

! CN: take this part out of the NEVER_DEF part
#ifdef BEC2_DIAG
# include "ecosys_bec2.h"
#endif

#ifdef NEVER_DEF

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
#endif /* NEVER_DEF */

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

      vname(1,indxDopr)='DOPR'
      vname(2,indxDopr)='Refractory dissolved organic phosphorus  '
      vname(3,indxDopr)='mMol P m-3'
      vname(4,indxDopr)='  '

      vname(1,indxDonr)='DONR'
      vname(2,indxDonr)='Refractory dissolved organic nitrogen    '
      vname(3,indxDonr)='mMol N m-3'
      vname(4,indxDonr)='  '

      vname(1,indxZOOC)='ZOOC'
      vname(2,indxZOOC)='Zooplankton'
      vname(3,indxZOOC)='mMol C m-3'
      vname(4,indxZOOC)='  '

      vname(1,indxSPCHL)='SPCHL'
      vname(2,indxSPCHL)='Small phytoplankton chlorophyll'
      vname(3,indxSPCHL)='mg Chl-a m-3'
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

# ifdef BEC_COCCO
      vname(1,indxCoccoc)='COCCOC                                   '
      vname(2,indxCoccoc)='Coccolithophore Carbon                   '
      vname(3,indxCoccoc)='mMol C m-3                              '
      vname(4,indxCoccoc)='  '

      vname(1,indxCoccochl)='COCCOCHL                                 '
      vname(2,indxCoccochl)='Coccolithophore Chlorophyll              '
      vname(3,indxCoccochl)='mg Chl-a m-3                              '
      vname(4,indxCoccochl)='  '

      vname(1,indxCoccocal)='COCCOCAL                                '
      vname(2,indxCoccocal)='Coccolithophore CaCO3                   '
      vname(3,indxCoccocal)='mMol CaCO3 m-3                          '
      vname(4,indxCoccocal)='  '

      vname(1,indxCoccofe)='COCCOFE                                   '
      vname(2,indxCoccofe)='Coccolithophore iron                    '
      vname(3,indxCoccofe)='mMol Fe m-3                              '
      vname(4,indxCoccofe)='  '

      vname(1,indxCal)='CAL                                   '
      vname(2,indxCal)='CaCO3                                   '
      vname(3,indxCal)='mMol CaCO3 m-3                         '
      vname(4,indxCal)='  '
# endif 
# ifdef BEC_DDA
      vname(1,indxDdac)='DDAC                                   '
      vname(2,indxDdac)='Diatom-diazotroph assemblages Carbon        '
      vname(3,indxDdac)='mMol C m-3                              '
      vname(4,indxDdac)='  '

      vname(1,indxDdachl)='DDACHL                                 '
      vname(2,indxDdachl)='Diatom-diazotroph assemblages Chlorophyll '
      vname(3,indxDdachl)='mg Chl-a m-3                              '
      vname(4,indxDdachl)='  '

      vname(1,indxDdasi)='DDASI                                '
      vname(2,indxDdasi)='Diatom-diazotroph assemblages Silicon    '
      vname(3,indxDdasi)='mMol Si m-3                          '
      vname(4,indxDdasi)='  '

      vname(1,indxDdafe)='DDAFE                                   '
      vname(2,indxDdafe)='Diatom-diazotroph assemblages iron       '
      vname(3,indxDdafe)='mMol Fe m-3                              '
      vname(4,indxDdafe)='  '
# endif
# ifdef BEC_PHAEO
      vname(1,indxPhaeoc)='PHAEOC                                   '
      vname(2,indxPhaeoc)='Phaeocystis Carbon                   '
      vname(3,indxPhaeoc)='mMol C m-3                              '
      vname(4,indxPhaeoc)='  '

      vname(1,indxPhaeochl)='PHAEOCHL                                 '
      vname(2,indxPhaeochl)='Phaeocystis Chlorophyll              '
      vname(3,indxPhaeochl)='mg Chl-a m-3                              '
      vname(4,indxPhaeochl)='  '

      vname(1,indxPhaeofe)='PHAEOFE                                   '
      vname(2,indxPhaeofe)='Phaeocystis iron                    '
      vname(3,indxPhaeofe)='mMol Fe m-3                              '
      vname(4,indxPhaeofe)='  '
# endif 

      vname(1,indxdust)='dust                                       '
      vname(2,indxdust)='dust flux                      '
      vname(3,indxdust)='kg m-2 s-1                                '
      vname(4,indxdust)='  '

      vname(1,indxiron)='iron                                    '
      vname(2,indxiron)='iron flux                      '
      vname(3,indxiron)='nmol cm-2 s-1                             '
      vname(4,indxiron)='  '

# ifndef BEC2_DIAG
      vname(1,indxPH)='PH'
      vname(2,indxPH)='surf pH value'
      vname(3,indxPH)=' '
      vname(4,indxPH)=' '
      vname(1,indxPCO2)='pCO2                                 '
      vname(2,indxPCO2)='Surface water pCO2                   '
      vname(3,indxPCO2)='ppm                                  '
      vname(4,indxPCO2)='  '
      vname(1,indxPCO2air)='pCO2air                           '
      vname(2,indxPCO2air)='Atmospheric pCO2                  '
      vname(3,indxPCO2air)='ppm                               '
      vname(4,indxPCO2air)='           '
      vname(1,indxPARinc)='PARinc                                '
      vname(2,indxPARinc)=
     &  'Incoming photosynth. active radiation '
      vname(3,indxPARinc)='W m-2                                 '
      vname(4,indxPARinc)='  '
      vname(1,indxPAR)='PAR'
      vname(2,indxPAR)=
     &  'Photosynthetically active radiation'
      vname(3,indxPAR)='W m-2'
      vname(4,indxPAR)='  '
# endif /* !BEC2_DIAG */



# ifdef BEC2_DIAG
      !
      ! 2d diagnotics:
      !
#  if defined CCHEM_MOCSY && defined CCHEM_TODEPTH
      vname_bec2_diag_3d(1,indxPH)='PH'
      vname_bec2_diag_3d(2,indxPH)='surf pH value'
      vname_bec2_diag_3d(3,indxPH)=' '
      vname_bec2_diag_3d(4,indxPH)=' '
      vname_bec2_diag_3d(1,indxPCO2)='pCO2'
      vname_bec2_diag_3d(2,indxPCO2)='Surface water pCO2'
      vname_bec2_diag_3d(3,indxPCO2)='ppm'
      vname_bec2_diag_3d(4,indxPCO2)='  '
#  else
      vname_bec2_diag_2d(1,indxPH)='PH'
      vname_bec2_diag_2d(2,indxPH)='surf pH value'
      vname_bec2_diag_2d(3,indxPH)=' '
      vname_bec2_diag_2d(4,indxPH)=' '
      vname_bec2_diag_2d(1,indxPCO2)='pCO2'
      vname_bec2_diag_2d(2,indxPCO2)='Surface water pCO2'
      vname_bec2_diag_2d(3,indxPCO2)='ppm'
      vname_bec2_diag_2d(4,indxPCO2)='  '
#  endif
      vname_bec2_diag_2d(1,indxPCO2air)='pCO2air'
      vname_bec2_diag_2d(2,indxPCO2air)='Atmospheric pCO2'
      vname_bec2_diag_2d(3,indxPCO2air)='ppm'
      vname_bec2_diag_2d(4,indxPCO2air)='           '
      vname_bec2_diag_2d(1,indxPARinc)='PARinc'
      vname_bec2_diag_2d(2,indxPARinc)=
     &  'Incoming photosynth. active radiation '
      vname_bec2_diag_2d(3,indxPARinc)='W m-2'
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

#  if defined CCHEM_MOCSY && defined CCHEM_TODEPTH
      vname_bec2_diag_3d(1,indxCO2STAR)='CO2STAR'
      vname_bec2_diag_3d(2,indxCO2STAR)='CO2STAR'
      vname_bec2_diag_3d(3,indxCO2STAR)='mmol/m3 '
      vname_bec2_diag_3d(4,indxCO2STAR)=' '
#  else
      vname_bec2_diag_2d(1,indxCO2STAR)='CO2STAR'
      vname_bec2_diag_2d(2,indxCO2STAR)='CO2STAR'
      vname_bec2_diag_2d(3,indxCO2STAR)='mmol/m3 '
      vname_bec2_diag_2d(4,indxCO2STAR)=' '
#  endif
      vname_bec2_diag_2d(1,indxDCO2STAR)='DCO2STAR'
      vname_bec2_diag_2d(2,indxDCO2STAR)='DCO2STAR'
      vname_bec2_diag_2d(3,indxDCO2STAR)='mmol/m3'
      vname_bec2_diag_2d(4,indxDCO2STAR)=' '

      vname_bec2_diag_2d(1,indxPCO2OC)='PCO2OC'
      vname_bec2_diag_2d(2,indxPCO2OC)='PCO2OC'
      vname_bec2_diag_2d(3,indxPCO2OC)='not looked up yet '
      vname_bec2_diag_2d(4,indxPCO2OC)=' '

      vname_bec2_diag_2d(1,indxIRONFLUX)='IRON_FLUX'
      vname_bec2_diag_2d(2,indxIRONFLUX)='Iron surface flux'
      vname_bec2_diag_2d(3,indxIRONFLUX)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxIRONFLUX)=' '

      vname_bec2_diag_2d(1,indxSEDDENITRIF)='SED_DENITRIF'
      vname_bec2_diag_2d(2,indxSEDDENITRIF)='Sediment denitrification'
      vname_bec2_diag_2d(3,indxSEDDENITRIF)='mmol/m3/s'
      vname_bec2_diag_2d(4,indxSEDDENITRIF)=' '

#  ifdef CCHEM_MOCSY
#   ifdef CCHEM_TODEPTH
      vname_bec2_diag_3d(1,indxCO3)='CO3'
      vname_bec2_diag_3d(2,indxCO3)='carbonate ion (CO3-) concentration'
      vname_bec2_diag_3d(3,indxCO3)='mmol/m3'
      vname_bec2_diag_3d(4,indxCO3)=' '
      vname_bec2_diag_3d(1,indxHCO3)='HCO3'
      vname_bec2_diag_3d(2,indxHCO3)=
     &  'bicarbonate ion (HCO3-) concentration'
      vname_bec2_diag_3d(3,indxHCO3)='mmol/m3'
      vname_bec2_diag_3d(4,indxHCO3)=' '
      vname_bec2_diag_3d(1,indxOMEGACALC)='OMEGA_CALC'
      vname_bec2_diag_3d(2,indxOMEGACALC)=
     &  'Calcite saturation state'
      vname_bec2_diag_3d(3,indxOMEGACALC)=' '
      vname_bec2_diag_3d(4,indxOMEGACALC)=' '
      vname_bec2_diag_3d(1,indxOMEGAARAG)='OMEGA_ARAG'
      vname_bec2_diag_3d(2,indxOMEGAARAG)=
     &  'Aragonite saturation state'
      vname_bec2_diag_3d(3,indxOMEGAARAG)=' '
      vname_bec2_diag_3d(4,indxOMEGAARAG)=' '
#   else
      vname_bec2_diag_2d(1,indxCO3)='CO3'
      vname_bec2_diag_2d(2,indxCO3)='carbonate ion (CO3-) concentration'
      vname_bec2_diag_2d(3,indxCO3)='mmol/m3'
      vname_bec2_diag_2d(4,indxCO3)=' '
      vname_bec2_diag_2d(1,indxHCO3)='HCO3'
      vname_bec2_diag_2d(2,indxHCO3)=
     &  'bicarbonate ion (HCO3-) concentration'
      vname_bec2_diag_2d(3,indxHCO3)='mmol/m3'
      vname_bec2_diag_2d(4,indxHCO3)=' '
#   endif /* CCHEM_TODEPTH */
#  endif /* CCHEM_MOCSY */

      vname_bec2_diag_2d(1,indxFESEDFLUX)='SED_FE_FLUX'
      vname_bec2_diag_2d(2,indxFESEDFLUX)='Benthic Iron Flux'
      vname_bec2_diag_2d(3,indxFESEDFLUX)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxFESEDFLUX)=' '

      vname_bec2_diag_2d(1,indxFLUXTOSED)='FLUX_TO_SED'
      vname_bec2_diag_2d(2,indxFLUXTOSED)='POC reaching the sediments'
      vname_bec2_diag_2d(3,indxFLUXTOSED)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxFLUXTOSED)=' '

      vname_bec2_diag_2d(1,indxCACO3FLUXTOSED)='CACO3_FLUX_TO_SED'
      vname_bec2_diag_2d(2,indxCACO3FLUXTOSED)='CaCO3 reaching the sed'
      vname_bec2_diag_2d(3,indxCACO3FLUXTOSED)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxCACO3FLUXTOSED)=' '

      vname_bec2_diag_2d(1,indxSIO2FLUXTOSED)='SIO2_FLUX_TO_SED'
      vname_bec2_diag_2d(2,indxSIO2FLUXTOSED)='Opal reaching the sed'
      vname_bec2_diag_2d(3,indxSIO2FLUXTOSED)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxSIO2FLUXTOSED)=' '

      vname_bec2_diag_2d(1,indxPIRONFLUXTOSED)='PIRON_FLUX_TO_SED'
      vname_bec2_diag_2d(2,indxPIRONFLUXTOSED)='P iron reaching the sed'
      vname_bec2_diag_2d(3,indxPIRONFLUXTOSED)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxPIRONFLUXTOSED)=' '

      vname_bec2_diag_2d(1,indxDUSTFLUXTOSED)='DUST_FLUX_TO_SED'
      vname_bec2_diag_2d(2,indxDUSTFLUXTOSED)='Dust reaching the sed'
      vname_bec2_diag_2d(3,indxDUSTFLUXTOSED)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxDUSTFLUXTOSED)=' '

      vname_bec2_diag_2d(1,indxPOCSEDLOSS)='POC_SED_LOSS'
      vname_bec2_diag_2d(2,indxPOCSEDLOSS)='POC lost to sed at bottom'
      vname_bec2_diag_2d(3,indxPOCSEDLOSS)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxPOCSEDLOSS)=' '

      vname_bec2_diag_2d(1,indxOTHERREMIN)='OTHER_REMIN'
      vname_bec2_diag_2d(2,indxOTHERREMIN)='other remin pathways in sed'
      vname_bec2_diag_2d(3,indxOTHERREMIN)='mmol/m3/s'
      vname_bec2_diag_2d(4,indxOTHERREMIN)=' '

      vname_bec2_diag_2d(1,indxCACO3SEDLOSS)='P_CACO3_SED_LOSS'
      vname_bec2_diag_2d(2,indxCACO3SEDLOSS)='CaCO3 lost to sed'
      vname_bec2_diag_2d(3,indxCACO3SEDLOSS)='mmol/m2/s'
      vname_bec2_diag_2d(4,indxCACO3SEDLOSS)=' '

      vname_bec2_diag_2d(1,indxSIO2SEDLOSS)='SIO2_SED_LOSS'
      vname_bec2_diag_2d(2,indxSIO2SEDLOSS)='SiO2 lost to sed'
      vname_bec2_diag_2d(3,indxSIO2SEDLOSS)='mmol Si/m2/s'
      vname_bec2_diag_2d(4,indxSIO2SEDLOSS)=' '


      ! MF: Control BEC2_DIAG Output vars
      wrtavg_bec2_diag_2d=(/.false.,.false.,.false.,.true.,.false.,  ! pCO2air, PARinc,FG_O2,FG_CO2, WS10m
     &                     .false.,.false.,.false.,.false.,.false.,   ! XKW, ATM_PRESS,SCHMIDT_O2,O2SAT, SCHMIDT_CO2
     &                     .false.,.false.,.false.,.false.,.false.,   ! PV_O2,PV_CO2,IRON_FLUX, SED_DENITRI, PH
     &                     .true.,.false.,.false.,.false.,          ! PCO2, CO2STAR, PCO2OC, DCO2STAR
     &                     .false.,.false.,.false.,.false.,.false.,   ! FE_SED_FLUX,FLUX_TO_SED,CACO3_FLUX_TO_SED,SIO2_FLUX_TO_SED,PIRON_FLUX_TO_SED
     &                     .false.,.false.,.false.,.false.,.false./)  ! DUST_FLUX_SED, POC_SED_LOSS, OTHER_REMIN,CaCO3_sed_loss,P_SiO2_sed_loss

      !
      ! More 3d diagnostics:
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

      vname_bec2_diag_3d(1,indxTOTCHL)='TOT_CHL'
      vname_bec2_diag_3d(2,indxTOTCHL)=
     &  'Total Chlorophyll'
      vname_bec2_diag_3d(3,indxTOTCHL)='mg Chl/m3'
      vname_bec2_diag_3d(4,indxTOTCHL)=' '

      vname_bec2_diag_3d(1,indxTOTPHYTOC)='TOT_PHYTOC'
      vname_bec2_diag_3d(2,indxTOTPHYTOC)=
     &  'Total Phytoplankton Carbon'
      vname_bec2_diag_3d(3,indxTOTPHYTOC)='mMol C/m3'
      vname_bec2_diag_3d(4,indxTOTPHYTOC)=' '

      vname_bec2_diag_3d(1,indxSPPLIM)='SP_P_LIM'
      vname_bec2_diag_3d(2,indxSPPLIM)=
     &  'Small phytoplankton P limitation (PO4 + DOP)'
      vname_bec2_diag_3d(3,indxSPPLIM)=' '
      vname_bec2_diag_3d(4,indxSPPLIM)=' '

      vname_bec2_diag_3d(1,indxDIATPLIM)='DIAT_P_LIM'
      vname_bec2_diag_3d(2,indxDIATPLIM)=
     &  'Diatom P limitation (PO4 + DOP)'
      vname_bec2_diag_3d(3,indxDIATPLIM)=' '
      vname_bec2_diag_3d(4,indxDIATPLIM)=' '

      vname_bec2_diag_3d(1,indxDIAZPLIM)='DIAZ_P_LIM'
      vname_bec2_diag_3d(2,indxDIAZPLIM)=
     &  'Diazotroph P limitation (PO4 + DOP)'
      vname_bec2_diag_3d(3,indxDIAZPLIM)=' '
      vname_bec2_diag_3d(4,indxDIAZPLIM)=' '

      vname_bec2_diag_3d(1,indxPOCPRODZOOLOSS)='POC_PROD_ZOO_LOSS'
      vname_bec2_diag_3d(2,indxPOCPRODZOOLOSS)=
     &  'POC PROD due to Zoo loss'
      vname_bec2_diag_3d(3,indxPOCPRODZOOLOSS)='mMol C m-3 s-1'

      vname_bec2_diag_3d(1,indxPOCPRODDIAT)='POC_PROD_DIAT'
      vname_bec2_diag_3d(2,indxPOCPRODDIAT)=
     &  'POC PROD due to Diat grz/agg/loss'
      vname_bec2_diag_3d(3,indxPOCPRODDIAT)='mMol C m-3 s-1'

      vname_bec2_diag_3d(1,indxPOCPRODSP)='POC_PROD_SP'
      vname_bec2_diag_3d(2,indxPOCPRODSP)=
     &  'POC PROD due to SP grz/agg/loss'
      vname_bec2_diag_3d(3,indxPOCPRODSP)='mMol C m-3 s-1'

      vname_bec2_diag_3d(1,indxPOCPRODDIAZ)='POC_PROD_DIAZ'
      vname_bec2_diag_3d(2,indxPOCPRODDIAZ)=
     &  'POC PROD due to Diaz grz/agg/loss'
      vname_bec2_diag_3d(3,indxPOCPRODDIAZ)='mMol C m-3 s-1'

      vname_bec2_diag_3d(1,indxQSI)='QSI_DIAT'
      vname_bec2_diag_3d(2,indxQSI)=
     &  'Initial Si/C ratio of diatoms'
      vname_bec2_diag_3d(3,indxQSI)=' '

      vname_bec2_diag_3d(1,indxTOTGRAZE)='TOT_GRAZE'
      vname_bec2_diag_3d(2,indxTOTGRAZE)=
     &  'Total Phytoplankton Grazing'
      vname_bec2_diag_3d(3,indxTOTGRAZE)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTGRAZE)=' '

      vname_bec2_diag_3d(1,indxTOTGRAZEDIC)='TOT_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxTOTGRAZEDIC)=
     &  'Total Phytoplankton Grazing routed to DIC'
      vname_bec2_diag_3d(3,indxTOTGRAZEDIC)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTGRAZEDIC)=' '

      vname_bec2_diag_3d(1,indxTOTGRAZEZOO)='TOT_GRAZE_ZOO'
      vname_bec2_diag_3d(2,indxTOTGRAZEZOO)=
     &  'Total Phytoplankton Grazing routed to Zoo'
      vname_bec2_diag_3d(3,indxTOTGRAZEZOO)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTGRAZEZOO)=' '

      vname_bec2_diag_3d(1,indxTOTAGG)='TOT_AGG'
      vname_bec2_diag_3d(2,indxTOTAGG)=
     &  'Total Phytoplankton Agrregation'
      vname_bec2_diag_3d(3,indxTOTAGG)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTAGG)=' '

      vname_bec2_diag_3d(1,indxTOTNO3UPTAKE)='TOT_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxTOTNO3UPTAKE)=
     &  'Total Phytoplankton NO3 Uptake'
      vname_bec2_diag_3d(3,indxTOTNO3UPTAKE)='mMol m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTNO3UPTAKE)=' '

      vname_bec2_diag_3d(1,indxTOTNH4UPTAKE)='TOT_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxTOTNH4UPTAKE)=
     &  'Total Phytoplankton NH4 Uptake'
      vname_bec2_diag_3d(3,indxTOTNH4UPTAKE)='mMol m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTNH4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxTOTLOSS)='TOT_LOSS'
      vname_bec2_diag_3d(2,indxTOTLOSS)=
     &  'Total Phytoplankton Loss'
      vname_bec2_diag_3d(3,indxTOTLOSS)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTLOSS)=' '

      vname_bec2_diag_3d(1,indxTOTLOSSDIC)='TOT_LOSS_DIC'
      vname_bec2_diag_3d(2,indxTOTLOSSDIC)=
     &  'Total Phytoplankton Loss routed to DIC'
      vname_bec2_diag_3d(3,indxTOTLOSSDIC)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxTOTLOSSDIC)=' '

      vname_bec2_diag_3d(1,indxNEXCRETE)='NEXCRETE'
      vname_bec2_diag_3d(2,indxNEXCRETE)=
     &  'DIAZ N excretion'
      vname_bec2_diag_3d(3,indxNEXCRETE)='mMol N m-3 s-1'
      vname_bec2_diag_3d(4,indxNEXCRETE)=' '

      vname_bec2_diag_3d(1,indxNCP)='NCP'
      vname_bec2_diag_3d(2,indxNCP)=
     &  'Net Community Production'
      vname_bec2_diag_3d(3,indxNCP)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxNCP)=' '

      vname_bec2_diag_3d(1,indxTOTPOC)='TOT_POC'
      vname_bec2_diag_3d(2,indxTOTPOC)=
     &  'Total (free + QA) POC'
      vname_bec2_diag_3d(3,indxTOTPOC)='mMol m-3'
      vname_bec2_diag_3d(4,indxTOTPOC)=' '

# ifdef BEC_COCCO
      vname_bec2_diag_3d(1,indxGRAZECOCCO)='GRAZE_COCCO'
      vname_bec2_diag_3d(2,indxGRAZECOCCO)=
     &  'Grazing rate on Coccos'
      vname_bec2_diag_3d(3,indxGRAZECOCCO)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZECOCCO)=' '

      vname_bec2_diag_3d(1,indxCOCCOLOSS)='COCCO_LOSS'
      vname_bec2_diag_3d(2,indxCOCCOLOSS)=
     &  'Cocco non-grazing mortality'
      vname_bec2_diag_3d(3,indxCOCCOLOSS)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxCOCCOLOSS)=' '

      vname_bec2_diag_3d(1,indxCOCCOAGG)='COCCO_AGG'
      vname_bec2_diag_3d(2,indxCOCCOAGG)=
     &  'Aggregation of coccos'
      vname_bec2_diag_3d(3,indxCOCCOAGG)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxCOCCOAGG)=' '

      vname_bec2_diag_3d(1,indxPHOTOCCOCCO)='PHOTOC_COCCO'
      vname_bec2_diag_3d(2,indxPHOTOCCOCCO)=
     &  'C fixation rate by Coccos'
      vname_bec2_diag_3d(3,indxPHOTOCCOCCO)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHOTOCCOCCO)=' '

      vname_bec2_diag_3d(1,indxCOCCONLIM)='COCCO_N_LIM'
      vname_bec2_diag_3d(2,indxCOCCONLIM)=
     &  'N limitation factor of Coccos'
      vname_bec2_diag_3d(3,indxCOCCONLIM)=' '
      vname_bec2_diag_3d(4,indxCOCCONLIM)=' '
      vname_bec2_diag_3d(1,indxCOCCOFEUPTAKE)='COCCO_FE_LIM'
      vname_bec2_diag_3d(2,indxCOCCOFEUPTAKE)=
     &  'Fe limitation factor of Coccos'
      vname_bec2_diag_3d(3,indxCOCCOFEUPTAKE)=' '
      vname_bec2_diag_3d(4,indxCOCCOFEUPTAKE)=' '
      vname_bec2_diag_3d(1,indxCOCCOPO4UPTAKE)='COCCO_PO4_LIM'
      vname_bec2_diag_3d(2,indxCOCCOPO4UPTAKE)=
     &  'PO4 limitation factor of Coccos'
      vname_bec2_diag_3d(3,indxCOCCOPO4UPTAKE)=' '
      vname_bec2_diag_3d(4,indxCOCCOPO4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxCOCCOLIGHTLIM)='COCCO_LIGHT_LIM'
      vname_bec2_diag_3d(2,indxCOCCOLIGHTLIM)=
     &  'Cocco light limitation factor'
      vname_bec2_diag_3d(3,indxCOCCOLIGHTLIM)=' '
      vname_bec2_diag_3d(4,indxCOCCOLIGHTLIM)=' '

      vname_bec2_diag_3d(1,indxCACO3PRODCOCCO)='CACO3_PROD_COCCO'
      vname_bec2_diag_3d(2,indxCACO3PRODCOCCO)=
     &  'Production of CaCO3 by Coccos'
      vname_bec2_diag_3d(3,indxCACO3PRODCOCCO)='mmol CaCO3/m3/s '
      vname_bec2_diag_3d(4,indxCACO3PRODCOCCO)=' '

      vname_bec2_diag_3d(1,indxIRONUPTAKECOCCO)='PHOTOFE_COCCO'
      vname_bec2_diag_3d(2,indxIRONUPTAKECOCCO)=
     &  'Iron uptake rate by Coccos'
      vname_bec2_diag_3d(3,indxIRONUPTAKECOCCO)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxIRONUPTAKECOCCO)=' '

      vname_bec2_diag_3d(1,indxCOCCONO3UPTAKE)='COCCO_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxCOCCONO3UPTAKE)=
     &   'NO3 uptake rate of Coccos'
      vname_bec2_diag_3d(3,indxCOCCONO3UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxCOCCONO3UPTAKE)=' '

      vname_bec2_diag_3d(1,indxCOCCONH4UPTAKE)='COCCO_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxCOCCONH4UPTAKE)=
     &   'NH4 uptake rate of Coccos'
      vname_bec2_diag_3d(3,indxCOCCONH4UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxCOCCONH4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxCOCCOGRAZEDIC)='COCCO_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxCOCCOGRAZEDIC)=
     &   'Cocco grazing rate routed to DIC'
      vname_bec2_diag_3d(3,indxCOCCOGRAZEDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxCOCCOGRAZEDIC)=' '

      vname_bec2_diag_3d(1,indxCOCCOLOSSDIC)='COCCO_LOSS_DIC'
      vname_bec2_diag_3d(2,indxCOCCOLOSSDIC)=
     &   'Cocco non-grazing mortality routed to DIC'
      vname_bec2_diag_3d(3,indxCOCCOLOSSDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxCOCCOLOSSDIC)=' '

      vname_bec2_diag_3d(1,indxGRAZECOCCOZOO)='GRAZE_COCCO_ZOO'
      vname_bec2_diag_3d(2,indxGRAZECOCCOZOO)=
     &  'Grazing rate on Coccos routed to zoo'
      vname_bec2_diag_3d(3,indxGRAZECOCCOZOO)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxGRAZECOCCOZOO)=' '

      vname_bec2_diag_3d(1,indxQCACO3COCCO)='QCACO3_COCCO'
      vname_bec2_diag_3d(2,indxQCACO3COCCO)=
     &  'Cocco CaCO3/C ratio'
      vname_bec2_diag_3d(3,indxQCACO3COCCO)='mmol CaCO3/mmol C'
      vname_bec2_diag_3d(4,indxQCACO3COCCO)=' '

      vname_bec2_diag_3d(1,indxCOCCOPHOTOACC)='PHOTOACC_COCCO'
      vname_bec2_diag_3d(2,indxCOCCOPHOTOACC)=
     &  'Chl synthesis in photoadaptation for Coccos'
      vname_bec2_diag_3d(3,indxCOCCOPHOTOACC)='mg Chl/m3/s'
      vname_bec2_diag_3d(4,indxCOCCOPHOTOACC)=' '

      vname_bec2_diag_3d(1,indxCOCCOPLIM)='COCCO_P_LIM'
      vname_bec2_diag_3d(2,indxCOCCOPLIM)=
     &  'Cocco P limitation (PO4 + DOP)'
      vname_bec2_diag_3d(3,indxCOCCOPLIM)=' '
      vname_bec2_diag_3d(4,indxCOCCOPLIM)=' '

      vname_bec2_diag_3d(1,indxPOCPRODCOCCO)='POC_PROD_COCCO'
      vname_bec2_diag_3d(2,indxPOCPRODCOCCO)=
     &  'POC PROD due to Cocco grz/agg/loss'
      vname_bec2_diag_3d(3,indxPOCPRODCOCCO)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxPOCPRODCOCCO)=' '

# endif BEC_COCCO
# ifdef BEC_DDA
      vname_bec2_diag_3d(1,indxGRAZEDDA)='GRAZE_DDA'
      vname_bec2_diag_3d(2,indxGRAZEDDA)=
     &  'Grazing rate on Phaeo'
      vname_bec2_diag_3d(3,indxGRAZEDDA)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZEDDA)=' '

      vname_bec2_diag_3d(1,indxDDALOSS)='DDA_LOSS'
      vname_bec2_diag_3d(2,indxDDALOSS)=
     &  'Phaeo non-grazing mortality'
      vname_bec2_diag_3d(3,indxDDALOSS)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxDDALOSS)=' '

      vname_bec2_diag_3d(1,indxDDAAGG)='DDA_AGG'
      vname_bec2_diag_3d(2,indxDDAAGG)=
     &  'Aggregation of coccos'
      vname_bec2_diag_3d(3,indxDDAAGG)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxDDAAGG)=' '

      vname_bec2_diag_3d(1,indxPHOTOCDDA)='PHOTOC_DDA'
      vname_bec2_diag_3d(2,indxPHOTOCDDA)=
     &  'C fixation rate by Phaeo'
      vname_bec2_diag_3d(3,indxPHOTOCDDA)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHOTOCDDA)=' '

      vname_bec2_diag_3d(1,indxDDANLIM)='DDA_N_LIM'
      vname_bec2_diag_3d(2,indxDDANLIM)=
     &  'N limitation factor of Phaeo'
      vname_bec2_diag_3d(3,indxDDANLIM)=' '
      vname_bec2_diag_3d(4,indxDDANLIM)=' '
      vname_bec2_diag_3d(1,indxDDAFEUPTAKE)='DDA_FE_LIM'
      vname_bec2_diag_3d(2,indxDDAFEUPTAKE)=
     &  'Fe limitation factor of Phaeo'
      vname_bec2_diag_3d(3,indxDDAFEUPTAKE)=' '
      vname_bec2_diag_3d(4,indxDDAFEUPTAKE)=' '
      vname_bec2_diag_3d(1,indxDDAPO4UPTAKE)='DDA_PO4_LIM'
      vname_bec2_diag_3d(2,indxDDAPO4UPTAKE)=
     &  'PO4 limitation factor of Phaeo'
      vname_bec2_diag_3d(3,indxDDAPO4UPTAKE)=' '
      vname_bec2_diag_3d(4,indxDDAPO4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxDDALIGHTLIM)='DDA_LIGHT_LIM'
      vname_bec2_diag_3d(2,indxDDALIGHTLIM)=
     &  'Phaeo light limitation factor'
      vname_bec2_diag_3d(3,indxDDALIGHTLIM)=' '
      vname_bec2_diag_3d(4,indxDDALIGHTLIM)=' '

      vname_bec2_diag_3d(1,indxIRONUPTAKEDDA)='PHOTOFE_DDA'
      vname_bec2_diag_3d(2,indxIRONUPTAKEDDA)=
     &  'Iron uptake rate by Phaeo'
      vname_bec2_diag_3d(3,indxIRONUPTAKEDDA)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxIRONUPTAKEDDA)=' '

      vname_bec2_diag_3d(1,indxDDANO3UPTAKE)='DDA_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxDDANO3UPTAKE)=
     &   'NO3 uptake rate of Phaeo'
      vname_bec2_diag_3d(3,indxDDANO3UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDDANO3UPTAKE)=' '

      vname_bec2_diag_3d(1,indxDDANH4UPTAKE)='DDA_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxDDANH4UPTAKE)=
     &   'NH4 uptake rate of Phaeo'
      vname_bec2_diag_3d(3,indxDDANH4UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxDDANH4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxDDAGRAZEDIC)='DDA_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxDDAGRAZEDIC)=
     &   'Phaeo grazing rate routed to DIC'
      vname_bec2_diag_3d(3,indxDDAGRAZEDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxDDAGRAZEDIC)=' '

      vname_bec2_diag_3d(1,indxDDALOSSDIC)='DDA_LOSS_DIC'
      vname_bec2_diag_3d(2,indxDDALOSSDIC)=
     &   'Phaeo non-grazing mortality routed to DIC'
      vname_bec2_diag_3d(3,indxDDALOSSDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxDDALOSSDIC)=' '

      vname_bec2_diag_3d(1,indxGRAZEDDAZOO)='GRAZE_DDA_ZOO'
      vname_bec2_diag_3d(2,indxGRAZEDDAZOO)=
     &  'Grazing rate on Phaeo routed to zoo'
      vname_bec2_diag_3d(3,indxGRAZEDDAZOO)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxGRAZEDDAZOO)=' '

      vname_bec2_diag_3d(1,indxDDAPHOTOACC)='PHOTOACC_DDA'
      vname_bec2_diag_3d(2,indxDDAPHOTOACC)=
     &  'Chl synthesis in photoadaptation for Phaeo'
      vname_bec2_diag_3d(3,indxDDAPHOTOACC)='mg Chl/m3/s'
      vname_bec2_diag_3d(4,indxDDAPHOTOACC)=' '

      vname_bec2_diag_3d(1,indxDDAPLIM)='DDA_P_LIM'
      vname_bec2_diag_3d(2,indxDDAPLIM)=
     &  'Phaeo P limitation (PO4 + DOP)'
      vname_bec2_diag_3d(3,indxDDAPLIM)=' '
      vname_bec2_diag_3d(4,indxDDAPLIM)=' '
# endif /* BEC_DDA */

# ifdef BEC_PHAEO
      vname_bec2_diag_3d(1,indxGRAZEPHAEO)='GRAZE_PHAEO'
      vname_bec2_diag_3d(2,indxGRAZEPHAEO)=
     &  'Grazing rate on Phaeo'
      vname_bec2_diag_3d(3,indxGRAZEPHAEO)='mmol C/m3/s'
      vname_bec2_diag_3d(4,indxGRAZEPHAEO)=' '

      vname_bec2_diag_3d(1,indxPHAEOLOSS)='PHAEO_LOSS'
      vname_bec2_diag_3d(2,indxPHAEOLOSS)=
     &  'Phaeo non-grazing mortality'
      vname_bec2_diag_3d(3,indxPHAEOLOSS)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHAEOLOSS)=' '

      vname_bec2_diag_3d(1,indxPHAEOAGG)='PHAEO_AGG'
      vname_bec2_diag_3d(2,indxPHAEOAGG)=
     &  'Aggregation of coccos'
      vname_bec2_diag_3d(3,indxPHAEOAGG)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHAEOAGG)=' '

      vname_bec2_diag_3d(1,indxPHOTOCPHAEO)='PHOTOC_PHAEO'
      vname_bec2_diag_3d(2,indxPHOTOCPHAEO)=
     &  'C fixation rate by Phaeo'
      vname_bec2_diag_3d(3,indxPHOTOCPHAEO)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxPHOTOCPHAEO)=' '

      vname_bec2_diag_3d(1,indxPHAEONLIM)='PHAEO_N_LIM'
      vname_bec2_diag_3d(2,indxPHAEONLIM)=
     &  'N limitation factor of Phaeo'
      vname_bec2_diag_3d(3,indxPHAEONLIM)=' '
      vname_bec2_diag_3d(4,indxPHAEONLIM)=' '
      vname_bec2_diag_3d(1,indxPHAEOFEUPTAKE)='PHAEO_FE_LIM'
      vname_bec2_diag_3d(2,indxPHAEOFEUPTAKE)=
     &  'Fe limitation factor of Phaeo'
      vname_bec2_diag_3d(3,indxPHAEOFEUPTAKE)=' '
      vname_bec2_diag_3d(4,indxPHAEOFEUPTAKE)=' '
      vname_bec2_diag_3d(1,indxPHAEOPO4UPTAKE)='PHAEO_PO4_LIM'
      vname_bec2_diag_3d(2,indxPHAEOPO4UPTAKE)=
     &  'PO4 limitation factor of Phaeo'
      vname_bec2_diag_3d(3,indxPHAEOPO4UPTAKE)=' '
      vname_bec2_diag_3d(4,indxPHAEOPO4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxPHAEOLIGHTLIM)='PHAEO_LIGHT_LIM'
      vname_bec2_diag_3d(2,indxPHAEOLIGHTLIM)=
     &  'Phaeo light limitation factor'
      vname_bec2_diag_3d(3,indxPHAEOLIGHTLIM)=' '
      vname_bec2_diag_3d(4,indxPHAEOLIGHTLIM)=' '

      vname_bec2_diag_3d(1,indxIRONUPTAKEPHAEO)='PHOTOFE_PHAEO'
      vname_bec2_diag_3d(2,indxIRONUPTAKEPHAEO)=
     &  'Iron uptake rate by Phaeo'
      vname_bec2_diag_3d(3,indxIRONUPTAKEPHAEO)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxIRONUPTAKEPHAEO)=' '

      vname_bec2_diag_3d(1,indxPHAEONO3UPTAKE)='PHAEO_NO3_UPTAKE'
      vname_bec2_diag_3d(2,indxPHAEONO3UPTAKE)=
     &   'NO3 uptake rate of Phaeo'
      vname_bec2_diag_3d(3,indxPHAEONO3UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxPHAEONO3UPTAKE)=' '

      vname_bec2_diag_3d(1,indxPHAEONH4UPTAKE)='PHAEO_NH4_UPTAKE'
      vname_bec2_diag_3d(2,indxPHAEONH4UPTAKE)=
     &   'NH4 uptake rate of Phaeo'
      vname_bec2_diag_3d(3,indxPHAEONH4UPTAKE)='mmol/m3/s  '
      vname_bec2_diag_3d(4,indxPHAEONH4UPTAKE)=' '

      vname_bec2_diag_3d(1,indxPHAEOGRAZEDIC)='PHAEO_GRAZE_DIC'
      vname_bec2_diag_3d(2,indxPHAEOGRAZEDIC)=
     &   'Phaeo grazing rate routed to DIC'
      vname_bec2_diag_3d(3,indxPHAEOGRAZEDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxPHAEOGRAZEDIC)=' '

      vname_bec2_diag_3d(1,indxPHAEOLOSSDIC)='PHAEO_LOSS_DIC'
      vname_bec2_diag_3d(2,indxPHAEOLOSSDIC)=
     &   'Phaeo non-grazing mortality routed to DIC'
      vname_bec2_diag_3d(3,indxPHAEOLOSSDIC)='mmol C/m3/s  '
      vname_bec2_diag_3d(4,indxPHAEOLOSSDIC)=' '

      vname_bec2_diag_3d(1,indxGRAZEPHAEOZOO)='GRAZE_PHAEO_ZOO'
      vname_bec2_diag_3d(2,indxGRAZEPHAEOZOO)=
     &  'Grazing rate on Phaeo routed to zoo'
      vname_bec2_diag_3d(3,indxGRAZEPHAEOZOO)='mmol C/m3/s '
      vname_bec2_diag_3d(4,indxGRAZEPHAEOZOO)=' '

      vname_bec2_diag_3d(1,indxPHAEOPHOTOACC)='PHOTOACC_PHAEO'
      vname_bec2_diag_3d(2,indxPHAEOPHOTOACC)=
     &  'Chl synthesis in photoadaptation for Phaeo'
      vname_bec2_diag_3d(3,indxPHAEOPHOTOACC)='mg Chl/m3/s'
      vname_bec2_diag_3d(4,indxPHAEOPHOTOACC)=' '

      vname_bec2_diag_3d(1,indxPHAEOPLIM)='PHAEO_P_LIM'
      vname_bec2_diag_3d(2,indxPHAEOPLIM)=
     &  'Phaeo P limitation (PO4 + DOP)'
      vname_bec2_diag_3d(3,indxPHAEOPLIM)=' '
      vname_bec2_diag_3d(4,indxPHAEOPLIM)=' '

      vname_bec2_diag_3d(1,indxPOCPRODPHAEO)='POC_PROD_PHAEO'
      vname_bec2_diag_3d(2,indxPOCPRODPHAEO)=
     &  'POC PROD due to Phaeo grz/agg/loss'
      vname_bec2_diag_3d(3,indxPOCPRODPHAEO)='mMol C m-3 s-1'
      vname_bec2_diag_3d(4,indxPOCPRODPHAEO)=' '

# endif /* BEC_PHAEO */
# endif /* BEC2_DIAG */

# ifdef USE_EXPLICIT_VSINK
! For output files:
      vname(1,indxdusthard)='dust_hard                             '
      vname(2,indxdusthard)='Hard component of dust                '
      vname(3,indxdusthard)='kg m-3                                '
      vname(4,indxdusthard)='  '
      vname(1,indxdustsoft)='dust_soft                             '
      vname(2,indxdustsoft)='Soft component of dust                '
      vname(3,indxdustsoft)='kg m-3                                '
      vname(4,indxdustsoft)='  '

      vname(1,indxpochard)='POC_hard'
      vname(2,indxpochard)='Hard component of POC'
      vname(3,indxpochard)='mmol C/m^3'
      vname(4,indxpochard)='  '
      vname(1,indxpocsoft)='POC_soft'
      vname(2,indxpocsoft)='Soft component of POC'
      vname(3,indxpocsoft)='mmol C/m^3'
      vname(4,indxpocsoft)='  '

      vname(1,indxpcaco3hard)='PCaCO3_hard'
      vname(2,indxpcaco3hard)='Hard component of particulate CaCO3'
      vname(3,indxpcaco3hard)='mmol CaCO3/m^3'
      vname(4,indxpcaco3hard)='  '
      vname(1,indxpcaco3soft)='PCaCO3_soft'
      vname(2,indxpcaco3soft)='Soft component of particulate CaCO3'
      vname(3,indxpcaco3soft)='mmol CaCO3/m^3'
      vname(4,indxpcaco3soft)='  '

      vname(1,indxpsio2hard)='PSiO2_hard'
      vname(2,indxpsio2hard)='Hard component of particulate SiO2'
      vname(3,indxpsio2hard)='mmol Si/m^3'
      vname(4,indxpsio2hard)='  '
      vname(1,indxpsio2soft)='PSiO2_soft'
      vname(2,indxpsio2soft)='Soft component of particulate SiO2'
      vname(3,indxpsio2soft)='mmol Si/m^3'
      vname(4,indxpsio2soft)='  '

      vname(1,indxpironhard)='PFe_hard'
      vname(2,indxpironhard)='Hard component of particulate iron'
      vname(3,indxpironhard)='mmol Fe/m^3'
      vname(4,indxpironhard)='  '
      vname(1,indxpironsoft)='PFe_soft'
      vname(2,indxpironsoft)='Soft component of particulate iron'
      vname(3,indxpironsoft)='mmol Fe/m^3'
      vname(4,indxpironsoft)='  '
      
# if defined BEC2_DIAG
      vname_bec2_diag_3d(1,indxDUSTHARDREMIN)='dust_hard_remin       '
      vname_bec2_diag_3d(2,indxDUSTHARDREMIN)=
     &       'Remin of Hard component of dust'
      vname_bec2_diag_3d(3,indxDUSTHARDREMIN)='kg m-3 s-1 (check) '
      vname_bec2_diag_3d(4,indxDUSTHARDREMIN)='  '

      vname_bec2_diag_3d(1,indxDUSTSOFTREMIN)='dust_soft_remin        '
      vname_bec2_diag_3d(2,indxDUSTSOFTREMIN)=
     &       'Remin of Soft component of dust'
      vname_bec2_diag_3d(3,indxDUSTSOFTREMIN)='kg m-3 s-1 (check)   '
      vname_bec2_diag_3d(4,indxDUSTSOFTREMIN)='  '

      vname_bec2_diag_3d(1,indxPOCHARDREMIN)='POC_hard_remin'
      vname_bec2_diag_3d(2,indxPOCHARDREMIN)=
     &        'Remin of Hard component of POC'
      vname_bec2_diag_3d(3,indxPOCHARDREMIN)='mmol C m-3 s-1'
      vname_bec2_diag_3d(4,indxPOCHARDREMIN)='  '

      vname_bec2_diag_3d(1,indxPOCSOFTREMIN)='POC_soft_remin'
      vname_bec2_diag_3d(2,indxPOCSOFTREMIN)=
     &        'Remin of Soft component of POC'
      vname_bec2_diag_3d(3,indxPOCSOFTREMIN)='mmol C m-3 s-1'
      vname_bec2_diag_3d(4,indxPOCSOFTREMIN)='  '

      vname_bec2_diag_3d(1,indxCACO3HARDREMIN)='CaCO3_hard_remin'
      vname_bec2_diag_3d(2,indxCACO3HARDREMIN)=
     &       'Remin of Hard component of CaCO3'
      vname_bec2_diag_3d(3,indxCACO3HARDREMIN)='mmol CaCO3 m-3 s-1'
      vname_bec2_diag_3d(4,indxCACO3HARDREMIN)='  '

      vname_bec2_diag_3d(1,indxCACO3SOFTREMIN)='CaCO3_soft_remin'
      vname_bec2_diag_3d(2,indxCACO3SOFTREMIN)=
     &        'Remin of Soft component of CaCO3'
      vname_bec2_diag_3d(3,indxCACO3SOFTREMIN)='mmol CaCO3 m-3 s-1'
      vname_bec2_diag_3d(4,indxCACO3SOFTREMIN)='  '

      vname_bec2_diag_3d(1,indxSIO2HARDREMIN)='SiO2_hard_remin'
      vname_bec2_diag_3d(2,indxSIO2HARDREMIN)=
     &         'Remin of Hard component of SiO2'
      vname_bec2_diag_3d(3,indxSIO2HARDREMIN)='mmol Si m-3 s-1'
      vname_bec2_diag_3d(4,indxSIO2HARDREMIN)='  '

      vname_bec2_diag_3d(1,indxSIO2SOFTREMIN)='SiO2_soft_remin'
      vname_bec2_diag_3d(2,indxSIO2SOFTREMIN)=
     &       'Remin of Soft component of SiO2'
      vname_bec2_diag_3d(3,indxSIO2SOFTREMIN)='mmol Si m-3 s-1'
      vname_bec2_diag_3d(4,indxSIO2SOFTREMIN)='  '

      vname_bec2_diag_3d(1,indxPIRONHARDREMIN)='PFe_hard_remin'
      vname_bec2_diag_3d(2,indxPIRONHARDREMIN)=
     &        'Remin of Hard component of piron'
      vname_bec2_diag_3d(3,indxPIRONHARDREMIN)='mmol Fe m-3 s-1'
      vname_bec2_diag_3d(4,indxPIRONHARDREMIN)='  '

      vname_bec2_diag_3d(1,indxPIRONSOFTREMIN)='PFe_soft_remin'
      vname_bec2_diag_3d(2,indxPIRONSOFTREMIN)=
     &        'Remin of Soft component of piron'
      vname_bec2_diag_3d(3,indxPIRONSOFTREMIN)='mmol Fe m-3 s-1'
      vname_bec2_diag_3d(4,indxPIRONSOFTREMIN)='  '

# endif /* BEC2_DIAG */
# endif /* USE_EXPLICIT_VSINK */


# ifdef BEC2_DIAG

        ! MF: Control BEC2_DIAG Output vars
      wrtavg_bec2_diag_3d=(/.false.,.false.,.false.,.false.,.false.,      !PAR, POC_FLUX_IN, POC_Prod,POC_REMIN, CACO3_FLUX_IN,
     &                     .false.,.false.,.false.,.false.,.false.,       !PCACO3_PROD, CACO3_REMIN,SIO2_FLUX_IN, SIO2_PROD, SIO2_REMIN,
     &                     .false.,.false.,.false.,.false.,.false.,       !DUST_FLUX_IN, DUST_REMIN,P_IRON_FLUX_IN, P_IRON_PROD, P_IRON_REMIN,
     &                     .false.,.false.,.false.,.false.,.false.,       !GRAZE_SP, GRAZE_DIAT,GRAZE_DIAZ, SP_LOSS, DIAT_LOSS,
     &                     .false.,.false.,.false.,.false.,.false.,       !ZOO_LOSS, SP_AGG, DIAT_AGG,PHOTOC_SP, PHOTOC_DIA,
     &                     .true.,.false.,.false.,.false.,.false.,       !TOT_PROD, DOC_PROD,DOC_REMIN, FE_SCAVENGE, SP_N_LIM, 
     &                     .false.,.false.,.false.,.false.,.false.,       !SP_FE_LIM, SP_PO4_LIM,SP_LIGHT_LIM, DIAT_N_LIM, DIAT_FE_LIM,
     &                     .false.,.false.,.false.,.false.,.false.,       !DIAT_PO4_LIM, DIAT_SIO3_LIM,DIAT_LIGHT_LIM, CACO3_PROD, DIAZ_NFIX,
     &                     .false.,.false.,.false.,.false.,.false.,       !DIAZ_LOSS, PHOTOC_DIAZ,DIAZ_PO4_LIM, DIAZ_FE_LIM, DIAZ_LIGHT_LIM,
     &                     .false.,.false.,.false.,.false.,.false.,       !FE_SCAVENGE_RATE, DON_PROD,DON_REMIN, DOFE_PROD, DOFE_REMIN,
     &                     .false.,.false.,.false.,.false.,.false.,     !DOP_PROD, DOP_REMIN,DIAT_SI_UPTAKE, PHOTOFE_SP, PHOTOFE_DIA,  
     &                     .false.,.false.,.false.,.false.,.false.,    !PHOTOFE_DIAZ, NITRI,DENITRIF,SP_NO3_UPTAKE,DIAT_NO3_UPTAKE,  
     &                     .false.,.false.,.false.,.false.,.false.,  !DIAZ_NO3_UPTAKE,SP_NH4_UPTAKE, DIAT_NH4_UPTAKE,DIAZ_NH4_UPTAKE,SP_GRAZE_DIC,  
     &                     .false.,.false.,.false.,.false.,.false.,  !DIAT_GRAZE_DIC,DIAZ_GRAZE_DIC, SP_LOSS_DIC,DIAT_LOSS_DIC,DIAZ_LOSS_DIC,  
     &                     .false.,.false.,.false.,.false.,.false.,  !ZOO_LOSS_DIC, DIAZ_AGG,GRAZE_SP_ZOO, GRAZE_DIAT_ZOO, GRAZE_DIAZ_ZOO, 
     &                     .false.,.false.,.false.,.false.,          !SPQCACO3,PHOTOACC_SP,PHOTOACC_DIAT, PHOTOACC_DIAZ,
     &                     .false.,.false.,.false.,.false.,.false.,  !SPC_ZERO, DIATC_ZERO, DIAZC_ZERO, DOC_ZERO, ZOOC_ZERO
     &                     .false.,.false.,.false.,.false.,           !SPCACO3_ZERO, DONR_REMIN, TOTCHL, SP_P_LIM 
     &                     .false.,.false.,.true.,                     !DIAT_P_LIM,DIAZ_P_LIM, TOT_PHYTO_C
     &                     .false.,.false.,.false.,.false.,     !POC_PROD_ZOO_LOSS,POC_PROD_DIAT,POC_PROD_SP,POC_PROD_DIAZ
     &                     .false.,.false.,.false.,.false.,.false.,  ! QSI_DIAT,TOT_GRAZE,TOT_GRAZE_DIC,TOT_GRAZE_ZOO,TOT_AGG
     &                     .false.,.false.,.false.,.false.,.false., ! TOT_NO3_UPTAKE,TOT_NH4_UPTAKE,TOT_LOSS,TOT_LOSS_DIC, NEXCRETE
     &                     .false.,.false.                              ! TOT_POC, NCP
     ! 107 tracer
# ifdef USE_EXPLICIT_VSINK
     ! +10
     &                     ,.false.,.true.,.false.,.false.,.false.,  ! PIRON_HARD_REMIN, CACO3_HARD_REMIN, SIO2_HARD_REMIN, POC_HARD_REMIN,DUST_HARD_REMIN
     &                     .false.,.true.,.false.,.false.,.false.    ! PIRON_SOFT_REMIN, CACO3_SOFT_REMIN, SIO2_SOFT_REMIN, POC_SOFT_REMIN,DUST_SOFT_REMIN
# endif
# ifdef BEC_COCCO 
     ! + 19 
     &                     ,.false., ! GRAZE_COCCO
     &                     .false.,.false.,.true.,.false.,     ! COCCO_LOSS,COCCO_AGG, PHOTOC_COCCO, COCCO_N_LIM
     &                     .false.,.true.,.false.,.false.,     ! COCCO_PO4_LIM, COCCO_FE_LIM, COCCO_LIGHT_LIM, CACO3_PROD_COCCO 
     &                     .true.,.false.,.false.,.false., ! IRON_UPTAKE_COCCO, COCCO_NO3_UPTAKE, COCCO_NH4_UPTAKE, COCCO_GRAZE_DIC 
     &                     .false.,.false.,.false.,.false.,    ! COCCO_LOSS_DIC, GRAZE_COCCO_ZOO, QCACO3_COCCO, COCCO_PHOTOACC 
     &                     .false.,.false.   ! COCCO_P_LIM,POC_PROD_COCCO
# endif
# ifdef BEC_DDA
     ! + 16
     &                     ,.false., ! GRAZE_DDA 
     &                     .false.,.false.,.true.,.true., !DDA_LOSS,DDA_AGG, PHOTOC_DDA, DDA_N_LIM
     &                     .false.,.true.,.true.,            ! DDA_PO4_LIM,DDA_FE_LIM, DDA_LIGHT_LIM
     &                     .false.,.false.,.false.,.false., !IRON_UPTAKE_DDA, DDA_NO3_UPTAKE, DDA_NH4_UPTAKE, DDA_GRAZE_DIC 
     &                     .false.,.false.,.false.,          ! DDA_LOSS_DIC,GRAZE_DDA_ZOO, DDA_PHOTOACC 
     &                     .true.                     !DDA_P_LIM,
# endif
# ifdef BEC_PHAEO
     ! + 17
     &                     ,.false., ! GRAZE_PHAEO
     &                     .false.,.false.,.true.,.true.,    !PHAEO_LOSS,PHAEO_AGG, PHOTOC_PHAEO, PHAEO_N_LIM
     &                     .false.,.true.,.true.,            ! PHAEO_PO4_LIM,PHAEO_FE_LIM, PHAEO_LIGHT_LIM
     &                     .false.,.false.,.false.,.false.,  !IRON_UPTAKE_PHAEO, PHAEO_NO3_UPTAKE, PHAEO_NH4_UPTAKE, PHAEO_GRAZE_DIC 
     &                     .false.,.false.,.false.,          ! PHAEO_LOSS_DIC,GRAZE_PHAEO_ZOO, PHAEO_PHOTOACC 
     &                     .true.,.true.                     ! PHAEO_P_LIM,POCPRODPHAEO
# endif
     &                     /)

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
# ifdef BEC_COCCO
      tclm_name(iCOCCOC) = 'coccoc_time'
      tclm_name(iCOCCOCHL) = 'coccochl_time'
      tclm_name(iCOCCOCAL) = 'coccocal_time'
      tclm_name(iCOCCOFE) = 'coccofe_time'
      tclm_name(iCAL) = 'cal_time'
# endif
# ifdef BEC_PHAEO
      tclm_name(iPHAEOC) = 'phaeoc_time'
      tclm_name(iPHAEOCHL) = 'phaeochl_time'
      tclm_name(iPHAEOFE) = 'phaeofe_time'
# endif
#endif /* SOLVE3D */
      return
      end
#else /* BIOLOGY_BEC2 */
      subroutine init_scalars_bec2_empty()
      return
      end
#endif /* BIOLOGY_BEC2 */
