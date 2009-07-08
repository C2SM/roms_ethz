!
!
#include "cppdefs.h"
#ifdef BIOLOGY_BEC

       subroutine init_scalars_bec(ierr)
!
! Set initial values for  globally accessible (stored in common
! blocks) scalar variables of the BEC model.
!
      implicit none
      integer ierr, i,j
      integer omp_get_num_threads
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "comm_vars.h"
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
# define NUMPROC 1
#endif

      if (numthreads.gt.NUMPROC) then
        MPI_master_only write(stdout,'(/1x,A,I3/)')
     &    'ERROR: Requested number of threads exceeds setting: ',NUMPROC
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
      vname(1,indxPO4)='PO4                                       '
      vname(2,indxpO4)='Phosphate                                 '
      vname(3,indxpO4)='mMol P m-3                                '
      vname(4,indxpO4)='PO4, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='Nitrate                                   '
      vname(3,indxNO3)='mMol N m-3                                '
      vname(4,indxNO3)='NO3, scalar, series                       '

      vname(1,indxSIO3)='SiO3                                      '
      vname(2,indxSIO3)='Silicate                                  '
      vname(3,indxSIO3)='mMol Si m-3                               '
      vname(4,indxSIO3)='SiO3, scalar, series                      '

CR      write(stdout,'(1x,A,I3)') 'indxNH4 =',indxNH4
      vname(1,indxNH4)='NH4                                       '
      vname(2,indxNH4)='Ammonium                                  '
      vname(3,indxNH4)='mMol N m-3                                '
      vname(4,indxNH4)='NH4, scalar, series                       '


      vname(1,indxFE)='Fe                                        '
      vname(2,indxFE)='Iron                                      '
      vname(3,indxFE)='mMol Fe m-3                               '
      vname(4,indxFE)='Fe, scalar, series                        '

      vname(1,indxO2)='O2                                        '
      vname(2,indxO2)='Oxygen                                    '
      vname(3,indxO2)='mMol O2 m-3                               '
      vname(4,indxO2)='O2, scalar, series                        '

      vname(1,indxDIC)='DIC                                       '
      vname(2,indxDIC)='Dissolved inorganic carbon                '
      vname(3,indxDIC)='mMol C m-3                                '
      vname(4,indxDIC)='DIC, scalar, series                       '

      vname(1,indxALK)='Alk                                       '
      vname(2,indxALK)='Alkalinity                                '
      vname(3,indxALK)='mMol m-3                                '
      vname(4,indxALK)='Alk, scalar, series                       '

      vname(1,indxDOC)='DOC                                       '
      vname(2,indxDOC)='Dissolved organic carbon                  '
      vname(3,indxDOC)='mMol C m-3                                '
      vname(4,indxDOC)='DOC, scalar, series                       '

      vname(1,indxSPC)='SPC                                       '
      vname(2,indxSPC)='Small phytoplankton carbon                '
      vname(3,indxSPC)='mMol C m-3                                '
      vname(4,indxSPC)='SPC, scalar, series                       '

      vname(1,indxSPCHL)='SPCHL                                     '
      vname(2,indxSPCHL)='Small phytoplankton chlorophyll           '
      vname(3,indxSPCHL)='mg Chl-a m-3                              '
      vname(4,indxSPCHL)='SPCHL, scalar, series                     '


      vname(1,indxSPCACO3)='SPCACO3                                   '
      vname(2,indxSPCACO3)='Small phytoplankton CaCO3                 '
      vname(3,indxSPCACO3)='mMol CaCO3 m-3                            '
      vname(4,indxSPCACO3)='SPCaCO3, scalar, series                   '

      vname(1,indxDIATC)='DIATC                                     '
      vname(2,indxDIATC)='Diatom carbon                             '
      vname(3,indxDIATC)='mMol C m-3                                '
      vname(4,indxDIATC)='DIATC, scalar, series                     '

      vname(1,indxDIATCHL)='DIATCHL                                   '
      vname(2,indxDIATCHL)='Diatom chlorophyll                        '
      vname(3,indxDIATCHL)='mg Chl-a m-3                              '
      vname(4,indxDIATCHL)='DIATCHL, scalar, series                   '

      vname(1,indxZOOC)='ZOOC                                     '
      vname(2,indxZOOC)='Zooplankton                              '
      vname(3,indxZOOC)='mMol C m-3                               '
      vname(4,indxZOOC)='ZOOC, scalar, series                     '

      vname(1,indxSPFE)='SPFE                                     '
      vname(2,indxSPFE)='Small phytoplankton iron                 '
      vname(3,indxSPFE)='mMol Fe m-3                              '
      vname(4,indxSPFE)='SPFE, scalar, series                     '

      vname(1,indxDIATSI)='DIATSI                                    '
      vname(2,indxDIATSI)='Diatom silicon                            '
      vname(3,indxDIATSI)='mMol Si m-3                               '
      vname(4,indxDIATSI)='DIATSI, scalar, series                    '

      vname(1,indxDIATFE)='DIATFE                                    '
      vname(2,indxDIATFE)='Diatom Iron                               '
      vname(3,indxDIATFE)='mMol Fe m-3                               '
      vname(4,indxDIATFE)='DIATFE, scalar, series                    '

      vname(1,indxdiazc)='DIAZC                                    '
      vname(2,indxDiazc)='Diazotroph carbon                        '
      vname(3,indxDiazc)='mMol C m-3                               '
      vname(4,indxDiazc)='DIAZC, scalar, series                    '

      vname(1,indxDiazchl)='DIAZCHL                                  '
      vname(2,indxDiazchl)='Diazotroph chlorophyll                   '
      vname(3,indxDiazchl)='mg Chl-a m-3                             '
      vname(4,indxDiazchl)='DIAZCHL, scalar, series                  '

      vname(1,indxDiazfe)='DIAZFE                                   '
      vname(2,indxDiazfe)='Diazotroph iron                          '
      vname(3,indxDiazfe)='mMol Fe m-3                              '
      vname(4,indxDiazfe)='DIAZFE, scalar, series                   '

      vname(1,indxDon)='DON                                      '
      vname(2,indxDon)='Dissolved organic nitrogen               '
      vname(3,indxDon)='mMol N m-3                               '
      vname(4,indxDon)='DON, scalar, series                      '

      vname(1,indxDofe)='DOFE                                     '
      vname(2,indxDofe)='Dissolved organic iron                   '
      vname(3,indxDofe)='mMol Fe m-3                              '
      vname(4,indxDofe)='DOFE, scalar, series                     '

      vname(1,indxDop)='DOP                                      '
      vname(2,indxDop)='Dissolved organic phosphorus             '
      vname(3,indxDop)='mMol P m-3                               '
      vname(4,indxDop)='DOP, scalar, series                      '


      vname(1,indxdust)='dust                                       '
      vname(2,indxdust)='dust flux                      '
      vname(3,indxdust)='kg m-2 s-1                                '
      vname(4,indxdust)='dust, scalar, series                       '

      vname(1,indxiron)='iron                                    '
      vname(2,indxiron)='iron flux                      '
      vname(3,indxiron)='nmol cm-2 s-1                             '
      vname(4,indxiron)='iron, scalar, series                       '

CDL: pH is treated here for rst files;
CDL: and in init_scalars_becflux for flux files:
      vname(1,indxPH_rst)='PH'
      vname(2,indxPH_rst)='pH value'
      vname(3,indxPH_rst)=' '
      vname(4,indxPH_rst)=' '
!DL: some more variables which are to be output also to
!DL: averages/history/restart files:
      vname(1,indxPCO2_rst)='pCO2                                 '
      vname(2,indxPCO2_rst)='Surface water pCO2                   '
      vname(3,indxPCO2_rst)='ppm                                  '
      vname(4,indxPCO2_rst)='pCO2, scalar, series                 '
      vname(1,indxPCO2air_rst)='pCO2air                           '
      vname(2,indxPCO2air_rst)='Atmospheric pCO2                  '
      vname(3,indxPCO2air_rst)='ppm                               '
      vname(4,indxPCO2air_rst)='           '
      vname(1,indxPARinc_rst)='PARinc                                '
      vname(2,indxPARinc_rst)='Incoming photosynth. active radiation '
      vname(3,indxPARinc_rst)='W m-2                                 '
      vname(4,indxPARinc_rst)='PARinc, scalar, series                '
      vname(1,indxPAR_rst)='PAR                                   '
      vname(2,indxPAR_rst)='Photosynthetically active radiation   '
      vname(3,indxPAR_rst)='W m-2                                 '
      vname(4,indxPAR_rst)='PAR, scalar, series                   '

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
      tclm_name(iDON) = 'don_time'
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
#else /* BIOLOGY_BEC */
      subroutine init_scalars_bec_empty()
      return
      end
#endif /* BIOLOGY_BEC */
