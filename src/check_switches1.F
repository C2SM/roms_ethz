#include "cppdefs.h"


      subroutine check_switches1 (ierr)

!!!!!! WARNING: THIS IS A MACHINE GENERATED CODE, DO NOT EDIT! !!!!!!
!!!!!! This file needs to be updated only if new CPP-switches  !!!!!!
!!!!!! were introduced into "cppdefs.h".  NO ACTION IS NEEDED  !!!!!!
!!!!!! if changes in "cppdefs.h" are limited to activation or  !!!!!!
!!!!!! deactivation of previously known switches.              !!!!!!
!!!!!! To refresh this file compile and execute "cppcheck.F"   !!!!!!
!!!!!! as an independent program, or use commands              !!!!!!
!!!!!! "make checkdefs" or "make depend".                      !!!!!!
!!!!!! Number of Configuration Choices:                     16 !!!!!!
!!!!!! Total number of CPP-switches:                        94 !!!!!!

      implicit none
      integer ierr, is,ie, iexample
#include "param.h"
#include "strings.h"
#ifdef MPI
# include "scalars.h"
#endif
      MPI_master_only write(stdout,'(/1x,A/)')
     &      'Activated C-preprocessing Options:'
      do is=1,max_opt_size
        cpps(is:is)=' '
      enddo
      iexample=0
      is=1
!
! cppdefs.h                       
!
      MPI_master_only write(stdout,'(1x,3A)') '<cppdefs.h>'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='<cppdefs.h>'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#ifdef BASIN
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'BASIN'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='BASIN'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANYON_A
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'CANYON_A'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CANYON_A'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANYON_B
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'CANYON_B'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CANYON_B'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DAMEE_B
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'DAMEE_B'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='DAMEE_B'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GRAV_ADJ
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'GRAV_ADJ'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='GRAV_ADJ'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NJ_BIGHT
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'NJ_BIGHT'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='NJ_BIGHT'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PACIFIC
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'PACIFIC'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PACIFIC'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PACIFIC_2D
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'PACIFIC_2D'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PACIFIC_2D'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OVERFLOW
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'OVERFLOW'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OVERFLOW'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SEAMOUNT
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'SEAMOUNT'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SEAMOUNT'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ISWAKE
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'ISWAKE'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ISWAKE'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOLITON
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'SOLITON'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SOLITON'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RIVER
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'RIVER'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='RIVER'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UPWELLING
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'UPWELLING'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UPWELLING'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef USWEST
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'USWEST'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='USWEST'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_RAD
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_RAD'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='WAVE_RAD'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOLVE3D
      MPI_master_only write(stdout,'(10x,A)') 'SOLVE3D'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SOLVE3D'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_ADV
      MPI_master_only write(stdout,'(10x,A)') 'UV_ADV'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UV_ADV'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_COR
      MPI_master_only write(stdout,'(10x,A)') 'UV_COR'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UV_COR'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_GP_UV
      MPI_master_only write(stdout,'(10x,A)') 'MIX_GP_UV'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_GP_UV'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SALINITY
      MPI_master_only write(stdout,'(10x,A)') 'SALINITY'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SALINITY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NONLIN_EOS
      MPI_master_only write(stdout,'(10x,A)') 'NONLIN_EOS'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='NONLIN_EOS'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF2
      MPI_master_only write(stdout,'(10x,A)') 'TS_DIF2'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TS_DIF2'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF4
      MPI_master_only write(stdout,'(10x,A)') 'TS_DIF4'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TS_DIF4'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_GP_TS
      MPI_master_only write(stdout,'(10x,A)') 'MIX_GP_TS'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_GP_TS'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_GRID
      MPI_master_only write(stdout,'(10x,A)') 'ANA_GRID'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_GRID'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_INITIAL
      MPI_master_only write(stdout,'(10x,A)') 'ANA_INITIAL'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_INITIAL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SMFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SMFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SMFLUX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_STFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_STFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_STFLUX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_VMIX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_VMIX'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_VMIX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VIS2
      MPI_master_only write(stdout,'(10x,A)') 'UV_VIS2'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UV_VIS2'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EW_PERIODIC
      MPI_master_only write(stdout,'(10x,A)') 'EW_PERIODIC'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='EW_PERIODIC'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPLIT_EOS
      MPI_master_only write(stdout,'(10x,A)') 'SPLIT_EOS'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPLIT_EOS'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TCLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'TCLIMATOLOGY'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TCLIMATOLOGY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TNUDGING
      MPI_master_only write(stdout,'(10x,A)') 'TNUDGING'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TNUDGING'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef QCORRECTION
      MPI_master_only write(stdout,'(10x,A)') 'QCORRECTION'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='QCORRECTION'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SFLX_CORR
      MPI_master_only write(stdout,'(10x,A)') 'SFLX_CORR'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SFLX_CORR'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef VIS_GRID
      MPI_master_only write(stdout,'(10x,A)') 'VIS_GRID'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='VIS_GRID'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIF_GRID
      MPI_master_only write(stdout,'(10x,A)') 'DIF_GRID'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='DIF_GRID'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_EN_TS
      MPI_master_only write(stdout,'(10x,A)') 'MIX_EN_TS'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_EN_TS'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_MIXING
      MPI_master_only write(stdout,'(10x,A)') 'LMD_MIXING'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_MIXING'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_RIMIX
      MPI_master_only write(stdout,'(10x,A)') 'LMD_RIMIX'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_RIMIX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_CONVEC
      MPI_master_only write(stdout,'(10x,A)') 'LMD_CONVEC'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_CONVEC'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CURVGRID
      MPI_master_only write(stdout,'(10x,A)') 'CURVGRID'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CURVGRID'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPHERICAL
      MPI_master_only write(stdout,'(10x,A)') 'SPHERICAL'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPHERICAL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MASKING
      MPI_master_only write(stdout,'(10x,A)') 'MASKING'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MASKING'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EASTERN_WALL
      MPI_master_only write(stdout,'(10x,A)') 'EASTERN_WALL'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='EASTERN_WALL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WESTERN_WALL
      MPI_master_only write(stdout,'(10x,A)') 'WESTERN_WALL'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='WESTERN_WALL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOUTHERN_WALL
      MPI_master_only write(stdout,'(10x,A)') 'SOUTHERN_WALL'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SOUTHERN_WALL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NORTHERN_WALL
      MPI_master_only write(stdout,'(10x,A)') 'NORTHERN_WALL'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='NORTHERN_WALL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef REST_STATE_TEST
      MPI_master_only write(stdout,'(10x,A)') 'REST_STATE_TEST'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='REST_STATE_TEST'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SSFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SSFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SSFLUX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SST
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SST'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SST'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SRFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SRFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SRFLUX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_S_UV
      MPI_master_only write(stdout,'(10x,A)') 'MIX_S_UV'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_S_UV'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef STATIONS
      MPI_master_only write(stdout,'(10x,A)') 'STATIONS'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='STATIONS'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_EAST
      MPI_master_only write(stdout,'(10x,A)') 'OBC_EAST'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_EAST'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_NORTH
      MPI_master_only write(stdout,'(10x,A)') 'OBC_NORTH'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_NORTH'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_SOUTH
      MPI_master_only write(stdout,'(10x,A)') 'OBC_SOUTH'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_SOUTH'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_FSORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_FSORLANSKI'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_FSORLANSKI'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2ORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2ORLANSKI'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M2ORLANSKI'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M3ORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M3ORLANSKI'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M3ORLANSKI'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_KPP
      MPI_master_only write(stdout,'(10x,A)') 'LMD_KPP'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_KPP'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MASK_LAND_DATA
      MPI_master_only write(stdout,'(10x,A)') 'MASK_LAND_DATA'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MASK_LAND_DATA'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EXACT_RESTART
      MPI_master_only write(stdout,'(10x,A)') 'EXACT_RESTART'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='EXACT_RESTART'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AVERAGES
      MPI_master_only write(stdout,'(10x,A)') 'AVERAGES'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='AVERAGES'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SSS_dQdSST
      MPI_master_only write(stdout,'(10x,A)') 'SSS_dQdSST'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SSS_dQdSST'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_NONLOCAL
      MPI_master_only write(stdout,'(10x,A)') 'LMD_NONLOCAL'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_NONLOCAL'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_DDMIX
      MPI_master_only write(stdout,'(10x,A)') 'LMD_DDMIX'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_DDMIX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_WEST
      MPI_master_only write(stdout,'(10x,A)') 'OBC_WEST'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_WEST'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2FLATHER
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2FLATHER'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M2FLATHER'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_TORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_TORLANSKI'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_TORLANSKI'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2NUDGING
      MPI_master_only write(stdout,'(10x,A)') 'M2NUDGING'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M2NUDGING'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3NUDGING
      MPI_master_only write(stdout,'(10x,A)') 'M3NUDGING'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M3NUDGING'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Z_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'Z_FRC_BRY'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='Z_FRC_BRY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'M2_FRC_BRY'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M2_FRC_BRY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'M3_FRC_BRY'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M3_FRC_BRY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef T_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'T_FRC_BRY'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='T_FRC_BRY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPONGE
      MPI_master_only write(stdout,'(10x,A)') 'SPONGE'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPONGE'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BRY
      MPI_master_only write(stdout,'(10x,A)') 'ANA_BRY'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_BRY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LINEAR_DRAG_ONLY
      MPI_master_only write(stdout,'(10x,A)') 'LINEAR_DRAG_ONLY'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LINEAR_DRAG_ONLY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M3SPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M3SPECIFIED'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M3SPECIFIED'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_TSPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'OBC_TSPECIFIED'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_TSPECIFIED'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PSOURCE
      MPI_master_only write(stdout,'(10x,A)') 'PSOURCE'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PSOURCE'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_PSOURCE
      MPI_master_only write(stdout,'(10x,A)') 'ANA_PSOURCE'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_PSOURCE'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_VMIX
      MPI_master_only write(stdout,'(10x,A)') 'LMD_VMIX'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_VMIX'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UCLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'UCLIMATOLOGY'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UCLIMATOLOGY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ROBUST_DIAG
      MPI_master_only write(stdout,'(10x,A)') 'ROBUST_DIAG'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ROBUST_DIAG'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CLIMAT_TS_MIXH
      MPI_master_only write(stdout,'(10x,A)') 'CLIMAT_TS_MIXH'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CLIMAT_TS_MIXH'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2SPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2SPECIFIED'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M2SPECIFIED'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BIOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'BIOLOGY'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='BIOLOGY'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FLOATS
      MPI_master_only write(stdout,'(10x,A)') 'FLOATS'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='FLOATS'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FLOATS_GLOBAL_ATTRIBUTES
      MPI_master_only write(stdout,'(10x,A)') 'FLOATS_GLOBAL_ATTRIBUTES'
      ie=is +23
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='FLOATS_GLOBAL_ATTRIBUTES'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RANDOM_WALK
      MPI_master_only write(stdout,'(10x,A)') 'RANDOM_WALK'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='RANDOM_WALK'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
      if (iexample.eq.0) then
        MPI_master_only write(stdout,'(/1x,A)')
     & 'ERROR in "cppdefs.h": no configuration is specified.'
        ierr=ierr+1
      elseif (iexample.gt.1) then
        MPI_master_only write(stdout,'(/1x,A/12x,A)')
     &  '*** ERROR: More than one main configurational ',
     &              'switch is defined in "cppdefs.h".'
        ierr=ierr+1
      endif
      return
  99  MPI_master_only write(stdout,'(/1x,A,A/14x,A)')
     &  'CHECKDEFS -- ERROR: Unsufficient size of string cpps',
     &  'in file "strings.h".', 'Increase the size it and recompile.'
      ierr=ierr+1
      return
      end
