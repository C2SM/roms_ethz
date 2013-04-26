#include "cppdefs.h"
 
       subroutine init_scalars (ierr)
!
! Set initial values for  globally accessable (stored in common
! blocks) scalar variables. Typically this is associated with
! model utility switches, i/o contlol, time indices, global
! summation counters, etc; NONE of the physical parameters of
! the model is set here.
!
      implicit none
      integer ierr, i,j
#ifdef PASSIVE_TRACER
      integer itrc
      character*3 c_indx
#endif
#define ALL_DATA
#include "param.h"
#include "scalars.h"
#include "comm_vars.h"
#include "forces.h"
#include "sources.h"
#include "mixing.h"
#include "ncvars.h"
#include "boundary.h"

c      integer size, kmp_get_stacksize_s
c      size=kmp_get_stacksize_s()
      

cc      write(*,*) 'kmp_stacksize =', size

      start_time=0.                  ! Set time and evolving
      nfast=1                        ! time indices to their
      iic=0                          ! default initial values.
      kstp=1
      knew=1
      ntstart=1
#ifdef SOLVE3D
      iif=1
      nstp=1
      nrhs=1
      nnew=1
#endif
#ifdef FLOATS
      nfp1=0     ! set time step indices for
      nf=3       ! 5th order AB4AM4 corrected
      nfm1=2     ! scheme
      nfm2=1
      nfm3=0
#endif
                                     ! Reset intra-thread
      may_day_flag=0                 ! communication signal
      trd_count=0                    ! variables and counters.
      trd_count=0
      do i=1,4
        CPU_time_ALL(i)=0.
      enddo
      do i=1,16
        barr_count(i)=0
      enddo

 
      mcheck=0
      first_time=0
      tile_count=0                   ! Reset global sumation
      bc_count=0                     ! variables and counters
      volume=0.                      ! used for volume/energy
      avke=0.                        ! diagnostics and volume
      avpe=0.                        ! conservation constraint.
      prev_ke=0.
      avke2b=0.
      avke3bc=0.
      avkp=0.
      v2d_max=0.
      v3d_max=0.
      v3bc_max=0.
 
      hmin=+1.E+20                   ! Set extremal values to
      hmax=-1.E+20                   ! large positive or negative
      grdmin=+1.E+20                 ! unrealistic values.
      grdmax=-1.E+20
      Cu_min=+1.E+20
      Cu_max=-1.E+20
#ifdef SOLVE3D
      rx0=-1.E+20
      rx1=-1.E+20
#endif
 
      bc_crss=QuadZero
#ifdef OBC_VOLCONS
      bc_flux=QuadZero
      ubar_xs=QuadZero
#endif
#ifdef BIOLOGY
      do i=0,2*NT+1
        global_sum(i)=QuadZero
      enddo
#endif
 
      ncidrst=-1                     ! Set netCDF file IDs to
      nrecrst=0                      ! unrealistic negative values
      ncidhis=-1                     ! to indicate their initial
      nrechis=0                      ! closed status.
#ifdef AVERAGES
      ncidavg=-1                     ! Also reset record counters for
      nrecavg=0                      ! restart, history, average, and
# ifdef SLICE_AVG
      ncidslavg=-1                     ! Also reset record counters for
      nrecslavg=0                      ! restart, history, average, and
# endif
#endif
#ifdef STATIONS
      ncidsta=-1                     ! stations netCDF output files.
      nrecsta=0
#endif
      do i=1,max_frc_file
        ncidfrc(i)=-1
      enddo
#ifndef ANA_SMFLUX
      sms_file_id=0
#endif
#ifdef SOLVE3D
# if !defined ANA_STFLUX || !defined ANA_SSFLUX
      do i=1,NT
        stf_file_id(i)=0
      enddo
# endif
# ifndef ANA_SRFLUX
      srf_file_id=0
# endif
# if defined QCORRECTION && !defined ANA_SST
      sst_file_id=0
      sss_file_id=0
      dqdt_file_id=0
# endif
#endif
      ncidclm=-1
#ifndef ANA_BRY
       bry_id=-1
#endif
#ifdef PSOURCE
       do j=1,NT                     ! Initialize switch
         do i=1,Msrc                 ! to apply tracer
           Lsrc(i,j)=.false.         ! sources and sinks.
         enddo
       enddo
#endif
 
      call get_date (date_str)
!
! Define names of variables in NetCDF output files.
! Inner dimension is for variable type and outer is for variable
! attributes (name, long-name, units, field).
!
c--#define CR
CR      write(stdout,'(1x,A,I3)') 'indxTime =',indxTime
#ifdef CALENDAR
      vname(1,indxTime)='time                                      '
      vname(3,indxTime)='second since 0001-01-01 00:00:00          '
#else
      vname(1,indxTime)='ocean_time                                '
      vname(3,indxTime)='second                                    '
#endif
      vname(2,indxTime)='time since initialization                 '
 
CR      write(stdout,'(1x,A,I3)') 'indxZ =',indxZ
      vname(1,indxZ)='zeta                                      '
      vname(2,indxZ)='free-surface elevation                    '
      vname(3,indxZ)='meter                                     '
 
CR      write(stdout,'(1x,A,I3)') 'indxUb =',indxUb
      vname(1,indxUb)='ubar                                      '
      vname(2,indxUb)='vertically integrated u-momentum component'
      vname(3,indxUb)='meter second-1                            '
 
CR      write(stdout,'(1x,A,I3)') 'indxVb = ',indxVb
      vname(1,indxVb)='vbar                                      '
      vname(2,indxVb)='vertically integrated v-momentum component'
      vname(3,indxVb)='meter second-1                            '
 
#ifdef SOLVE3D
CR      write(stdout,'(1x,A,I3)') 'indxU = ',indxU
      vname(1,indxU)='u                                         '
      vname(2,indxU)='u-momentum component                      '
      vname(3,indxU)='meter second-1                            '
 
CR      write(stdout,'(1x,A,I3)') 'indxV = ',indxV
      vname(1,indxV)='v                                         '
      vname(2,indxV)='v-momentum component                      '
      vname(3,indxV)='meter second-1                            '
 
CR      write(stdout,'(1x,A,I3)') 'indxT = ',indxT
      vname(1,indxT)='temp                                      '
      vname(2,indxT)='potential temperature                     '
      vname(3,indxT)='Celsius                                   '
 
# ifdef SALINITY
CR      write(stdout,'(1x,A,I3)') 'indxS = ',indxS
      vname(1,indxS)='salt                                      '
      vname(2,indxS)='salinity                                  '
      vname(3,indxS)='PSU                                       '
# endif

# ifdef PASSIVE_TRACER
      do itrc = 1, ntrc_pas
         itpas(itrc) = itemp + ntrc_salt + itrc
      end do
!
      do itrc = indxT+ntrc_salt+1,indxT+ntrc_salt+ntrc_pas
         if (itrc-indxT-ntrc_salt .lt. 10) then
            write (c_indx, '(i1)') itrc-indxT-ntrc_salt
         else if (itrc-indxT-ntrc_salt .lt. 100) then
            write (c_indx, '(i2)') itrc-indxT-ntrc_salt
         else
            write (c_indx, '(i3)') itrc-indxT-ntrc_salt
         end if
         vname(1,itrc) = 'tpas'/ /trim(c_indx)
         vname(2,itrc) = 'passive tracer '/ /c_indx
         vname(3,itrc)='no unit'
         vname(4,itrc)='passive tracer, scalar, series'
      end do
#  ifdef AGE_DYE_TRACER
      N_agedye = 0
      do itrc=1,ntrc_pas
         ncid_ad(itrc) = -1
      enddo
#  endif
# endif /* PASSIVE_TRACER */

# ifdef BIOLOGY
CR      write(stdout,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='NO3 Nutrient                              '
      vname(3,indxNO3)='mMol N m-3                                '
 
CR      write(stdout,'(1x,A,I3)') 'indxNH4 =',indxNH4
      vname(1,indxNH4='NH4                                        '
      vname(2,indxNH4)='NH4 Nutrient                              '
      vname(3,indxNH4)='mMol N m-3                                '
 
CR      write(stdout,'(1x,A,I3)') 'indxDet =',indxDet
      vname(1,indxDet)='DET                                       '
      vname(2,indxDet)='Detritus Nutrient                         '
      vname(3,indxDet)='mMol N m-3                                '
 
CR      write(stdout,'(1x,A,I3)') 'indxPhyt =',indxPhyt
      vname(1,indxPhyt)='PHYTO                                     '
      vname(2,indxPhyt)='Phytoplankton                             '
      vname(3,indxPhyt)='mMol N m-3                                '
 
CR      write(stdout,'(1x,A,I3)') 'indxZoo =',indxZoo
      vname(1,indxZoo)='ZOO                                       '
      vname(2,indxZoo)='Zooplankton                               '
      vname(3,indxZoo)='mMol N m-3                                '
# endif
 
CR      write(stdout,'(1x,A,I3)') 'indxO =',indxO
      vname(1,indxO)='omega'
      vname(2,indxO)='S-coordinate vertical velocity'
      vname(3,indxO)='meter second-1'
 
CR      write(stdout,'(1x,A,I3)') 'indxW =',indxW
      vname(1,indxW)='w                                         '
      vname(2,indxW)='vertical momentum component               '
      vname(3,indxW)='meter second-1                            '
 
CR      write(stdout,'(1x,A,I3)') 'indxR =',indxR
      vname(1,indxR)='rho                                       '
      vname(2,indxR)='density anomaly                           '
      vname(3,indxR)='kilogram meter-3                          '
 
CR      write(stdout,'(1x,A,I3)') 'indxAkv =',indxAkv
      vname(1,indxAkv)='AKv                                       '
      vname(2,indxAkv)='vertical viscosity coefficient            '
      vname(3,indxAkv)='meter2 second-1                           '

# ifdef KPP_DIAGNOS
CR      write(stdout,'(1x,A,I3)') 'indxRich =',indxRich
      vname(1,indxRich)='Rich                                      '
      vname(2,indxRich)='Diffusivity due to vert. shear instability'
      vname(3,indxRich)='meter2 second-1                           '

      vname(1,indxRichN)='RichN                                    '
      vname(2,indxRichN)='Local gradient Richardson number          '
      vname(3,indxRichN)='                                          '
# endif

CR      write(stdout,'(1x,A,I3)') 'indxAkt =',indxAkt
      vname(1,indxAkt)='AKt                                       '
      vname(2,indxAkt)='temperature vertical diffusion coefficient'
      vname(3,indxAkt)='meter2 second-1                           '
# ifdef SALINITY
CR      write(stdout,'(1x,A,I3)') 'indxAkt =',indxAks
      vname(1,indxAks)='AKs                                       '
      vname(2,indxAks)='salinity vertical diffusion coefficient   '
      vname(3,indxAks)='meter2 second-1                           '
# endif
# ifdef LMD_KPP
CR      write(stdout,'(1x,A,I3)') 'indxHbl =',indxHbl
      vname(1,indxHbl)='hbls'
      vname(2,indxHbl)='Thickness of KPP surface boundary layer'
      vname(3,indxHbl)='meter'
# endif
# ifdef LMD_BKPP
      vname(1,indxHbbl)='hbbl                                     '
      vname(2,indxHbbl)='Thickness of KPP bottom boundary layer   '
      vname(3,indxHbbl)='meter                                    '
# endif
#endif /* SOLVE3D */
CR      write(stdout,'(1x,A,I3)') 'indxSUSTR =',indxSUSTR
      vname(1,indxSUSTR)='sustr                                     '
      vname(2,indxSUSTR)='surface u-momentum stress                 '
      vname(3,indxSUSTR)='Newton meter-2                            '
 
CR      write(stdout,'(1x,A,I3)') 'indxSVSTR =',indxSVSTR
      vname(1,indxSVSTR)='svstr                                     '
      vname(2,indxSVSTR)='surface v-momentum stress                 '
      vname(3,indxSVSTR)='Newton meter-2                            '
 
#ifdef SOLVE3D
CR      write(stdout,'(1x,A,I3)') 'indxSHFl =',indxSHFl
      vname(1,indxSHFl)='shflux                                    '
      vname(2,indxSHFl)='surface net heat flux                     '
      vname(3,indxSHFl)='Watts meter-2                             '
# ifdef SALINITY
CR      write(stdout,'(1x,A,I3)') 'indxSSFl=',indxSSFl
      vname(1,indxSSFl)='swflux                                    '
      vname(2,indxSSFl)='surface freshwater flux (E-P)             '
      vname(3,indxSSFl)='centimeter day-1                          '
# endif
CR      write(stdout,'(1x,A,I3)') 'indxSWRad =',indxSWRad
      vname(1,indxSWRad)='swrad                                     '
      vname(2,indxSWRad)='solar shortwave radiation                 '
      vname(3,indxSWRad)='Watts meter-2                             '
 
CR      write(stdout,'(1x,A,I3)') 'indxSST =', indxSST
      vname(1,indxSST)='SST                                       '
      vname(2,indxSST)='sea surface temperature                   '
      vname(3,indxSST)='Celsius                                   '
 
CR      write(stdout,'(1x,A,I3)') 'indxdQdSST =', indxdQdSST
      vname(1,indxdQdSST)='dQdSST                                    '
      vname(2,indxdQdSST)='surface net heat flux sensitivity to SST  '
      vname(3,indxdQdSST)='Watts meter-2 Celsius-1                   '

CR      write(stdout,'(1x,A,I3)') 'indxSSS =', indxSSS
      vname(1,indxSSS)='SSS                                       '
      vname(2,indxSSS)='sea surface salinity                      '
      vname(3,indxSSS)='PSU                                       '
 
# ifdef SG_BBL96
#  ifndef ANA_WWAVE
      vname(1,indxWWA)='Awave                                     '
      vname(2,indxWWA)='wind induced wave amplitude               '
      vname(3,indxWWA)='meter                                     '
 
      vname(1,indxWWD)='Dwave                                     '
      vname(2,indxWWD)='wind induced wave direction               '
      vname(3,indxWWD)='degrees                                   '
 
      vname(1,indxWWP)='Pwave                                     '
      vname(2,indxWWP)='wind induced wave Period                  '
      vname(3,indxWWP)='second                                    '
#  endif
#  ifndef ANA_BSEDIM
      vname(1,indxBSS)='Ssize                                     '
      vname(2,indxBSS)='bottom sediment grain diameter size       '
      vname(3,indxBSS)='meter                                     '
 
      vname(1,indxBSD)='Sdens                                     '
      vname(2,indxBSD)='bottom sediment grain density             '
      vname(3,indxBSD)='kilogram meter-3                          '
#  endif
# endif
#endif
#ifdef ICE
      vname(1,indxAi)='aice                                      '
      vname(2,indxAi)='fraction of cell covered by ice           '
      vname(3,indxAi)='nondimensional                            '
 
      vname(1,indxUi)='uice                                      '
      vname(2,indxUi)='u-component of ice velocity               '
      vname(3,indxUi)='meter sec-1                               '
 
      vname(1,indxVi)='vice                                      '
      vname(2,indxVi)='v-component of ice velocity               '
      vname(3,indxVi)='meter sec-1                               '
 
      vname(1,indxHi)='hice                                      '
      vname(2,indxHi)='depth of ice cover                        '
      vname(3,indxHi)='meter                                     '
 
      vname(1,indxHS)='snow_thick                                '
      vname(2,indxHS)='depth of snow cover                       '
      vname(3,indxHS)='meter                                     '
 
      vname(1,indxTIsrf)='tisrf                                     '
      vname(2,indxTIsrf)='temperature of ice surface                '
      vname(3,indxTIsrf)='Kelvin                                    '
#endif
!DL:
#ifdef WRITE_DEPTHS
      vname(1,indxz_r)='z_r                                       '
      vname(2,indxz_r)='depths of rho points                      '
      vname(3,indxz_r)='meter                                     '
      vname(1,indxz_w)='z_w                                       '
      vname(2,indxz_w)='depths of w points                        '
      vname(3,indxz_w)='meter                                     '
      vname(1,indxHz)='Hz                                        '
      vname(2,indxHz)='thicknesses of sigma layers               '
      vname(3,indxHz)='meter                                     '
#endif /* WRITE_DEPTHS */

      tclm_name(itemp) = 'tclm_time'
#ifdef SALINITY
      tclm_name(isalt) = 'sclm_time'
#endif
 
      ierr=0
      call read_inp (ierr)
      if (ierr.eq.0) then
#ifdef SOLVE3D
        call set_weights
#endif
#ifdef GRID_LEVEL
# if GRID_LEVEL < MAX_GRID_LEVEL
#  if GRID_LEVEL == 1
        call init_scalars_2 (ierr)
#  elif GRID_LEVEL == 2
        call init_scalars_3 (ierr)
#  elif GRID_LEVEL == 3
        call init_scalars_4 (ierr)
#  endif
# endif
#endif
      endif
      return
      end
 
