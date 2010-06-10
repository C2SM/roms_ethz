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
!!!!!! Number of Configuration Choices:                     20 !!!!!!
!!!!!! Total number of CPP-switches:                        95 !!!!!!

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
      MPI_master_only write(stdout,'(1x,A)') '<cppdefs.h>'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='<cppdefs.h>'
      cpps(ie+1:ie+1)=' '
      is=ie+2
#ifdef BASIN
      iexample=iexample+1
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='BASIN'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANYON_A
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CANYON_A'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANYON_B
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CANYON_B'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DAMEE_B
      iexample=iexample+1
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='DAMEE_B'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GRAV_ADJ
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='GRAV_ADJ'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NJ_BIGHT
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='NJ_BIGHT'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PACIFIC
      iexample=iexample+1
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PACIFIC'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PACIFIC_2D
      iexample=iexample+1
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PACIFIC_2D'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OVERFLOW
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OVERFLOW'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SEAMOUNT
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SEAMOUNT'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ISWAKE
      iexample=iexample+1
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ISWAKE'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOLITON
      iexample=iexample+1
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SOLITON'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RIVER
      iexample=iexample+1
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='RIVER'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UPWELLING
      iexample=iexample+1
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UPWELLING'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef USWEST
      iexample=iexample+1
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='USWEST'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PACBIG
      iexample=iexample+1
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PACBIG'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ATLSMA
      iexample=iexample+1
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ATLSMA'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANBAS2
      iexample=iexample+1
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CANBAS2'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPIRAL
      iexample=iexample+1
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPIRAL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_RAD
      iexample=iexample+1
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='WAVE_RAD'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOLVE3D
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SOLVE3D'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_ADV
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UV_ADV'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_COR
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UV_COR'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_GP_UV
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_GP_UV'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SALINITY
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SALINITY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NONLIN_EOS
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='NONLIN_EOS'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF2
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TS_DIF2'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF4
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TS_DIF4'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_GP_TS
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_GP_TS'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_GRID
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_GRID'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_INITIAL
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_INITIAL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SMFLUX
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SMFLUX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_STFLUX
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_STFLUX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_VMIX
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_VMIX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VIS2
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='UV_VIS2'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EW_PERIODIC
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='EW_PERIODIC'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPLIT_EOS
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPLIT_EOS'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TCLIMATOLOGY
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TCLIMATOLOGY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TNUDGING
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='TNUDGING'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef QCORRECTION
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='QCORRECTION'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SFLX_CORR
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SFLX_CORR'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef VIS_GRID
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='VIS_GRID'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIF_GRID
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='DIF_GRID'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_EN_TS
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_EN_TS'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_MIXING
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_MIXING'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_RIMIX
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_RIMIX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_CONVEC
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_CONVEC'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CURVGRID
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CURVGRID'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPHERICAL
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPHERICAL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MASKING
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MASKING'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EASTERN_WALL
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='EASTERN_WALL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WESTERN_WALL
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='WESTERN_WALL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOUTHERN_WALL
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SOUTHERN_WALL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NORTHERN_WALL
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='NORTHERN_WALL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef REST_STATE_TEST
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='REST_STATE_TEST'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SSFLUX
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SSFLUX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SST
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SST'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SRFLUX
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_SRFLUX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MIX_S_UV
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MIX_S_UV'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef STATIONS
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='STATIONS'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_EAST
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_EAST'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_NORTH
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_NORTH'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_SOUTH
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_SOUTH'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_FSORLANSKI
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_FSORLANSKI'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2ORLANSKI
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M2ORLANSKI'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M3ORLANSKI
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M3ORLANSKI'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_KPP
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_KPP'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MASK_LAND_DATA
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='MASK_LAND_DATA'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EXACT_RESTART
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='EXACT_RESTART'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SSS_dQdSST
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SSS_dQdSST'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_NONLOCAL
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_NONLOCAL'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_DDMIX
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_DDMIX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_WEST
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_WEST'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2FLATHER
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M2FLATHER'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_TORLANSKI
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_TORLANSKI'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2NUDGING
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M2NUDGING'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3NUDGING
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M3NUDGING'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Z_FRC_BRY
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='Z_FRC_BRY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2_FRC_BRY
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M2_FRC_BRY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3_FRC_BRY
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='M3_FRC_BRY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef T_FRC_BRY
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='T_FRC_BRY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPONGE
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='SPONGE'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BRY
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_BRY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LINEAR_DRAG_ONLY
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LINEAR_DRAG_ONLY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M3SPECIFIED
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M3SPECIFIED'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_TSPECIFIED
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_TSPECIFIED'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AVERAGES
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='AVERAGES'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PSOURCE
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='PSOURCE'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_PSOURCE
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='ANA_PSOURCE'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_VMIX
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='LMD_VMIX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef STFLX_LIM
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='STFLX_LIM'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIURNAL_SRFLUX
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='DIURNAL_SRFLUX'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BRY
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='BRY'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CLIMAT_TS_MIXH
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='CLIMAT_TS_MIXH'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2SPECIFIED
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      cpps(is:ie)='OBC_M2SPECIFIED'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      cpps(ie+1:ie+1)=' '
      is=ie+2
#endif
      if (iexample.eq.0) then
        MPI_master_only write(stdout,'(/1x,A)')
     & 'ERROR in "cppdefs.h": no configuration is specified.'
        ierr=ierr+1
      elseif (iexample.gt.1) then
        MPI_master_only write(stdout,'(/1x,A/12x,A)')
     &  '### ERROR: More than one main configurational ',
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
