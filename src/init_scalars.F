#include "cppdefs.h"

       subroutine init_scalars (ierr)

! Set initial values for globally accessable (stored in common blocks)
! scalar variables.  Typically these are associated with time indices,
! global summation counters, model utility switches, i/o contlol, etc.
! Note that NONE of the model physical parameters is initialized here.

      implicit none
      integer ierr, i
#ifdef PASSIVE_TRACER
     &        , j
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
#include "ncvars_floats.h"
#include "boundary.h"

c      integer size, kmp_get_stacksize_s
c      size=kmp_get_stacksize_s()
c      write(*,*) 'kmp_stacksize =', size

      start_time=0. ; iic=0          ! Set time and evolving time
      nfast=1       ; kstp=1         ! stepping indices to their
      ntstart=1     ; knew=1         ! initial values needed for

#ifdef SOLVE3D
      iif=1   ; nstp=1               ! proper logical initialization
      nrhs=1  ; nnew=1               ! of model fields.
#endif
#ifdef FLOATS
      nfp1=0                         ! set time step indices for
      nf=3    ; nfm1=2               ! 5th order AB4AM4 corrected
      nfm2=1  ; nfm3=0               ! scheme
#endif
                                     ! Reset intra-thread
      may_day_flag=0                 ! communication signal
      trd_count=0                    ! variables and counters.
      cpu_all=0.   !<-- array
      barr_count=0 !<-- array

      mcheck=0
      first_time=0
      tile_count=0                   ! Reset global sumation variables
      bc_count=0                     ! and counters needed for volume
      volume=0.                      ! and energy diagnostics, and for
c      avke=0.                       ! volume conservation constraint.
c      avpe=0.
c      prev_ke=0.
c      avke2b=0.
c      avke3bc=0.
c      v2d_max=0.
c      v3d_max=0.
c      v3bc_max=0.

      hmin=+1.E+20   ; hmax=-1.E+20     ! Set extreme values to large
      grdmin=+1.E+20 ; grdmax=-1.E+20   ! unrealistic numbers, positive
      Cg_min=+1.E+20 ; Cg_max=-1.E+20   ! or negative, depending on the
#ifdef SOLVE3D
      rx0=-1.E+20    ; rx1=-1.E+20      ! context.
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
                             ! Set slipperness parameter to "free slip"
      gamma2=1.              ! along the straight boundaries: this may
                             ! be overwritten later by "read_inp".

      ncrst=-1   ; nrecrst=0       ! Set netCDF file IDs to negative
      nchis=-1   ; nrechis=0       ! values to indicate their initially
#ifdef AVERAGES
      ncavg=-1   ; nrecavg=0       ! closed status. Also reset record
# ifdef SLICE_AVG
      ncslavg=-1; nrecslavg=0
# endif
#endif
#ifdef STATIONS
      ncidstn=-1 ; nrecstn=0       ! counters for restart, history,
#endif
#ifdef FLOATS
      ncidflt=-1 ; nrecflt=0       ! averages, stations, and floats.
#endif
      do i=1,max_frc_files
        ncfrc(i)=-1
      enddo
#ifdef TSOURCE
      do i=1,max_frc_files
        ncidtsrc(i)=-1
      enddo
#endif
#ifndef ANA_SMFLUX
      wnd_file_id=-1
#endif
#ifdef SOLVE3D
# if !defined ANA_STFLUX || !defined ANA_SSFLUX
      do i=1,NT
        stf_file_id(i)=-1
      enddo
# endif
# ifndef ANA_SRFLUX
      srf_file_id=-1
# endif
# if defined ICEOBS
      ice_file_id=-1
# endif
# if defined TSOURCE
      tsrc_file_id=-1
# endif
# if defined QCORRECTION && !defined ANA_SST
      sst_file_id=-1
      dqdt_file_id=-1
# endif
# if defined SFLX_CORR && defined SALINITY
      sss_file_id=-1
# endif
! MF: initialize ncid macro for (get_forces/get_smth)
# ifdef SSH_DATA
      ssh_file_id=-1
# endif
! MF: initialize ncid macro for (get_forces/get_smth)
# ifdef TNUDG_WEIGHTS_DATA
      nudg_file_id=-1
# endif
#endif
! MF: initialize array of clm files
      do i=1,max_clm_files
        ncidclm(i)=-1
      enddo
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

! Names of variables in NetCDF output files. The first element
! is the name of the variable; the other two are are attributes.

c--#define CR
CR      write(*,'(1x,A,I3)') 'indxTime =',indxTime
!#ifdef CALENDAR
      vname(1,indxTime)='time'
!#else
!      vname(1,indxTime)='ocean_time'
!#endif
      vname(2,indxTime)='Time since initialization'
#ifndef STARTDATE
# define STARTDATE '0001-01-01' 
#endif
      vname(3,indxTime)='second since '/ /STARTDATE/ /' 00:00:00'

CR      write(*,'(1x,A,I3)') 'indxZ =',indxZ
      vname(1,indxZ)='zeta'
      vname(2,indxZ)='free-surface elevation'
      vname(3,indxZ)='meter'

CR      write(*,'(1x,A,I3)') 'indxUb =',indxUb
      vname(1,indxUb)='ubar'
      vname(2,indxUb)='vertically averaged u-momentum component'
      vname(3,indxUb)='meter second-1'

CR      write(*,'(1x,A,I3)') 'indxVb = ',indxVb
      vname(1,indxVb)='vbar'
      vname(2,indxVb)='vertically averaged v-momentum component'
      vname(3,indxVb)='meter second-1'

#ifdef SOLVE3D
CR      write(*,'(1x,A,I3)') 'indxU = ', indxU
      vname(1,indxU)='u'
      vname(2,indxU)='u-momentum component'
      vname(3,indxU)='meter second-1'

CR      write(*,'(1x,A,I3)') 'indxV = ', indxV
      vname(1,indxV)='v'
      vname(2,indxV)='v-momentum component'
      vname(3,indxV)='meter second-1'

CR      write(*,'(1x,A,I3)') 'indxO =',indxO
      vname(1,indxO)='omega'
      vname(2,indxO)='S-coordinate vertical velocity'
      vname(3,indxO)='meter second-1'

CR      write(*,'(1x,A,I3)') 'indxW =', indxW
      vname(1,indxW)='w'
      vname(2,indxW)='vertical velocity'
      vname(3,indxW)='meter second-1'

CR      write(*,'(1x,A,I3)') 'indxR =', indxR
      vname(1,indxR)='rho'
      vname(2,indxR)='density anomaly'
      vname(3,indxR)='kilogram meter-3'

CR      write(*,'(1x,A,I3)') 'indxT = ', indxT
      vname(1,indxT)='temp'
      vname(2,indxT)='potential temperature'
      vname(3,indxT)='Celsius'

# ifdef SALINITY
CR      write(*,'(1x,A,I3)') 'indxS = ', indxS
      vname(1,indxS)='salt'
      vname(2,indxS)='salinity'
      vname(3,indxS)='PSU'
# endif
# ifdef PASSIVE_TRACER
!      do itrc = 1, ntrc_pas
!         itpas(itrc) = itemp + ntrc_salt + itrc
!      end do
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

# ifdef LEGACY_NPZD
CR      write(*,'(1x,A,I3)') 'indxNO3 =', indxNO3
      vname(1,indxNO3)='NO3'
      vname(2,indxNO3)='NO3 Nutrient'
      vname(3,indxNO3)='mMol N m-3'

CR      write(*,'(1x,A,I3)') 'indxNH4 =', indxNH4
      vname(1,indxNH4='NH4'
      vname(2,indxNH4)='NH4 Nutrient'
      vname(3,indxNH4)='mMol N m-3'

CR      write(*,'(1x,A,I3)') 'indxDet =', indxDet
      vname(1,indxDet)='DET'
      vname(2,indxDet)='Detritus'
      vname(3,indxDet)='mMol N m-3'

CR      write(*,'(1x,A,I3)') 'indxPhyt =', indxPhyt
      vname(1,indxPhyt)='PHYTO'
      vname(2,indxPhyt)='Phytoplankton'
      vname(3,indxPhyt)='mMol N m-3'

CR      write(*,'(1x,A,I3)') 'indxZoo =', indxZoo
      vname(1,indxZoo)='ZOO'
      vname(2,indxZoo)='Zooplankton'
      vname(3,indxZoo)='mMol N m-3'
# endif

CR      write(*,'(1x,A,I3)') 'indxAkv =', indxAkv
      vname(1,indxAkv)='Akv'
      vname(2,indxAkv)='vertical viscosity coefficient'
      vname(3,indxAkv)='meter2 second-1'

CR      write(*,'(1x,A,I3)') 'indxAkt =',indxAkt
      vname(1,indxAkt)='Akt'
      vname(2,indxAkt)='vertical thermal conductivity coefficient'
      vname(3,indxAkt)='meter2 second-1'
# ifdef SALINITY
CR      write(*,'(1x,A,I3)') 'indxAks =',indxAks
      vname(1,indxAks)='AKs'
      vname(2,indxAks)='salinity vertical diffusion coefficient'
      vname(3,indxAks)='meter2 second-1'
# endif
!AH16:
# ifdef WRITE_CO2FLX
      vname(1,indxCO2flx)='CO2flx'
      vname(2,indxCO2flx)='Net surface CO2 flux'
      vname(3,indxCO2flx)='mmol m-2'
# endif
# ifdef WRITE_N2OFLX
      vname(1,indxN2Oflx)='N2Oflx'
      vname(2,indxN2Oflx)='Net surface N2O flux'
      vname(3,indxN2Oflx)='mmol m-2'
# endif
# if defined WRITE_HEATFLX
      vname(1,indxHeatflx)='Heatflx'
      vname(2,indxHeatflx)='Net surface heat flux'
      vname(3,indxHeatflx)='W m-2'
# endif
# if defined WRITE_TEMP_REST
      vname(1,indxRestflxTemp)='RestflxTemp'
      vname(2,indxRestflxTemp)='Restoring flux for temperature'
      vname(3,indxRestflxTemp)='W m-2'
# endif
# if defined WRITE_SALT_REST
      vname(1,indxRestflxSalt)='RestflxSalt'
      vname(2,indxRestflxSalt)='Restoring flux for salinity'
      vname(3,indxRestflxSalt)='centimeter day-1'
# endif
# ifdef LMD_KPP
CR      write(*,'(1x,A,I3)') 'indxHbls =',indxHbls
      vname(1,indxHbls)='hbls'
      vname(2,indxHbls)='Thickness of KPP surface boundary layer'
      vname(3,indxHbls)='meter'
# endif
# ifdef LMD_BKPP
CR      write(*,'(1x,A,I3)') 'indxHbbl =',indxHbbl
      vname(1,indxHbbl)='hbbl'
      vname(2,indxHbbl)='Thickness of KPP bottom boundary layer'
      vname(3,indxHbbl)='meter'
# endif
# ifdef KPP_DIAGNOSE
      vname(1,indxKppRichm)='KppRichm'
      vname(2,indxKppRichm)='Viscosity due to vert. shear instability'
      vname(3,indxKppRichm)='meter second-2'

      vname(1,indxKppRicht)='KppRicht'
      vname(2,indxKppRicht)='Diffusivity due to vert. shear instability'
      vname(3,indxKppRicht)='meter second-2'

      vname(1,indxKppSblSh)='KppSblSh'
      vname(2,indxKppSblSh)='Destabilizing mean shear component of HBLS'
      vname(3,indxKppSblSh)='meter second-2'

      vname(1,indxKppSblSt)='KppSblSt'
      vname(2,indxKppSblSt)='Stabilizing stratification component of HBLS'
      vname(3,indxKppSblSt)='meter second-2'

      vname(1,indxKppSblRot)='KppSblRot'
      vname(2,indxKppSblRot)='Stabilizing rotation component of HBLS'
      vname(3,indxKppSblRot)='meter second-2'

      vname(1,indxKppSblEnt)='KppSblEnt'
      vname(2,indxKppSblEnt)='Destabilizing turb. entr. component of HBLS'
      vname(3,indxKppSblEnt)='meter second-2'
# endif
#endif

#ifdef WRITE_FORCING
CR      write(*,'(1x,A,I3)') 'indxSUSTR =',indxSUSTR
      vname(1,indxSUSTR)='sustr                                     '
      vname(2,indxSUSTR)='surface u-momentum stress                 '
      vname(3,indxSUSTR)='Newton meter-2                            '
 
CR      write(*,'(1x,A,I3)') 'indxSVSTR =',indxSVSTR
      vname(1,indxSVSTR)='svstr                                     '
      vname(2,indxSVSTR)='surface v-momentum stress                 '
      vname(3,indxSVSTR)='Newton meter-2                            '
#endif
 
#ifdef SOLVE3D
# ifdef WRITE_FORCING
CR      write(*,'(1x,A,I3)') 'indxSHFl =',indxSHFl
      vname(1,indxSHFl)='shflux                                    '
      vname(2,indxSHFl)='surface net heat flux                     '
      vname(3,indxSHFl)='Watts meter-2                             '
#  ifdef SALINITY
CR      write(*,'(1x,A,I3)') 'indxSSFl=',indxSSFl
      vname(1,indxSSFl)='swflux                                    '
      vname(2,indxSSFl)='surface freshwater flux (E-P)             '
      vname(3,indxSSFl)='centimeter day-1                          '
#  endif
CR      write(*,'(1x,A,I3)') 'indxSWRad =',indxSWRad
      vname(1,indxSWRad)='swrad                                     '
      vname(2,indxSWRad)='solar shortwave radiation                 '
      vname(3,indxSWRad)='Watts meter-2                             '
 
CR      write(*,'(1x,A,I3)') 'indxSST =', indxSST
      vname(1,indxSST)='SST                                       '
      vname(2,indxSST)='sea surface temperature                   '
      vname(3,indxSST)='Celsius                                   '
 
CR      write(*,'(1x,A,I3)') 'indxdQdSST =', indxdQdSST
      vname(1,indxdQdSST)='dQdSST                                    '
      vname(2,indxdQdSST)='surface net heat flux sensitivity to SST  '
      vname(3,indxdQdSST)='Watts meter-2 Celsius-1                   '

CR      write(*,'(1x,A,I3)') 'indxSSS =', indxSSS
      vname(1,indxSSS)='SSS                                       '
      vname(2,indxSSS)='sea surface salinity                      '
      vname(3,indxSSS)='PSU                                       '
# endif /* WRITE_FORTING */
 
# ifdef SG_BBL96
#  ifndef ANA_WWAVE
      vname(1,indxWWA)='Awave'
      vname(2,indxWWA)='wind induced wave amplitude'
      vname(3,indxWWA)='meter'

      vname(1,indxWWD)='Dwave'
      vname(2,indxWWD)='wind induced wave direction'
      vname(3,indxWWD)='degrees'

      vname(1,indxWWP)='Pwave'
      vname(2,indxWWP)='wind induced wave Period'
      vname(3,indxWWP)='second'
#  endif
#  ifndef ANA_BSEDIM
      vname(1,indxBSS)='Ssize'
      vname(2,indxBSS)='bottom sediment grain diameter size'
      vname(3,indxBSS)='meter'

      vname(1,indxBSD)='Sdens'
      vname(2,indxBSD)='bottom sediment grain density'
      vname(3,indxBSD)='kilogram meter-3'
#  endif
# endif
#endif
#ifdef ICEOBS
      vname(1,indxCi)='seaice                                    '
      vname(2,indxCi)='fraction of cell covered by ice           '
      vname(3,indxCi)='nondimensional                            '
      vname(1,indxFi)='freezing                                  '
      vname(2,indxFi)='surface freshwater flux (sea-ice freezing)'
      vname(3,indxFi)='centimeter day-1                          '
      vname(1,indxMi)='melting                                   '
      vname(2,indxMi)='surface freshwater flux (sea-ice melting) '
      vname(3,indxMi)='centimeter day-1                          '
#endif
#ifdef ICE
      vname(1,indxAi)='aice'
      vname(2,indxAi)='fraction of cell covered by ice'
      vname(3,indxAi)='nondimensional'

      vname(1,indxUi)='uice'
      vname(2,indxUi)='u-component of ice velocity'
      vname(3,indxUi)='meter sec-1'

      vname(1,indxVi)='vice'
      vname(2,indxVi)='v-component of ice velocity'
      vname(3,indxVi)='meter sec-1'

      vname(1,indxHi)='hice'
      vname(2,indxHi)='depth of ice cover'
      vname(3,indxHi)='meter'

      vname(1,indxHS)='snow_thick'
      vname(2,indxHS)='depth of snow cover'
      vname(3,indxHS)='meter'

      vname(1,indxTIsrf)='tisrf'
      vname(2,indxTIsrf)='temperature of ice surface'
      vname(3,indxTIsrf)='Kelvin'
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
# ifndef OBSOLETE_SUB_INIT_SCALARS_IN_MAIN
#  ifdef BIOLOGY_NPZDOC
      if (ierr == 0) then
      call init_scalars_npzdoc(ierr)
      endif
#   ifdef BGC_FLUX_ANALYSIS
      if (ierr == 0) then
      call init_scalars_bgcflux(ierr)
      endif
#   endif
#  endif
#  ifdef BIOLOGY_BEC
      if (ierr == 0) then
        call init_scalars_bec(ierr)
      endif
#  endif
#  ifdef BIOLOGY_BEC2
      if (ierr == 0) then
        call init_scalars_bec2(ierr)
      endif
#  endif
#  ifdef PHYS_FLUX_ANALYSIS
      if (ierr == 0) then
      call init_scalars_physflux(ierr)
      endif
#  endif
# endif /* OBSOLETE_SUB_INIT_SCALARS_IN_MAIN */

 
      ierr=0
      call read_inp (ierr)
      if (ierr==0) then
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
      end
#undef CR
