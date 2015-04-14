#include "cppdefs.h"

      subroutine get_forces

! Read various forcing and climatological fields from their netCDF
! files and save them as globally accessible arrays in common blocks
! "forces.h" and "climat.h". NOTE that NO EFFORT has been made to
! convert units of these fluxes at this stage (i.e., convert them
! into kinematic fluxes): typically data is assumed to have dynamical
! physical units, thus wind stress is in [Newton/m^2]; surface heat
! and short wave radiation fluxes are in [Watts/m^2]; SST sensitivity
! to heat flux dQdSST is in [Watts/(deg C * m^2)]; surface fresh
! water flux is in [mm/day].

      implicit none
      integer ierr
#include "param.h"
#include "scalars.h"
c--#define VERBOSE
#ifdef VERBOSE
      write(*,*) 'enter get_forces' MYID
#endif
      ierr=0
#if defined M2NUDGING && !defined M2_FRC_BRY && !defined ANA_SSH
      call get_ssh (ierr)
#endif
#if defined UCLIMATOLOGY && !defined ANA_UCLIMA
      if (ierr==0) call get_uclima (ierr)
#endif
#ifdef SOLVE3D
# if ( defined TCLIMATOLOGY ||( defined TNUDGING && \
       defined T_FRC_BRY )) && !defined ANA_TCLIMA
      if (ierr==0) call get_tclima (ierr)
# endif
#endif
# if  defined T_FRC_BRY  || defined M2_FRC_BRY || \
      defined M3_FRC_BRY || defined Z_FRC_BRY
#  ifndef ANA_BRY
      if (ierr==0) call get_bry_all (ierr)
#  endif
# endif
#if defined SSH_TIDES || defined UV_TIDES
      if (ierr==0) call get_tides (ierr)
#endif


#ifndef ANA_SMFLUX
      if (ierr==0) call get_smflux (ierr)
# ifdef WIND_STRESS_AMP
      if (ierr==0) call get_stress_magn (ierr)
# endif
#endif
#ifdef SOLVE3D
# ifndef ANA_STFLUX
      if (ierr==0) call get_stflux (itemp, ierr)
# endif
# if defined SALINITY && !defined ANA_SSFLUX
      if (ierr==0) call get_stflux (isalt, ierr)
# endif
# if defined QCORRECTION && !defined ANA_SST
      if (ierr==0) call get_sst (ierr)
      if (ierr==0) call get_dqdt (ierr)
      if (ierr==0) call get_sss (ierr)
# endif
# if defined LMD_KPP && !defined ANA_SRFLUX
      if (ierr==0) call get_srflux (ierr)
# endif
#endif
#if defined SG_BBL96
# ifndef ANA_WWAVE
      if (ierr==0) call get_wwave (ierr)
# endif
# ifndef ANA_BSEDIM
      if (ierr==0) call get_bsedim (ierr)
# endif
#endif
      if (ierr/=0) then
        may_day_flag=2
        write(*,'(1x,2A,2x,A,I4)') '### ERROR: get_forces :: ',
     &          'set may_day_flag = 2' MYID
      endif

#ifdef VERBOSE
      write(*,*) 'return get_forces' MYID
#endif
      return
      end



      subroutine set_forces (tile)


! Using either data read from netCDF files or created analytically,
! prepare surface and bottom boundary fluxes as well as relevant
! climatological fields, so they can be applied to the model. This
! procedure essentially interpolates the available data to current
! model time and converts units to make all fluxes be kinematic
! fluxes, i.e,
!                      input data       conversion      kinematic
!                      units            factor          flux units
!
!  wind stress         [Newton/m^2]      1/rho0          [m^2/s^2]
!
!  heat, SWR fluxes    [Watts/m^2]       1/(rho*Cp)     [deg C * m/s]
!
!  fresh water flux     [cm/day]     S_surf*0.01/86400  [PSU *  m/s]
!
!     dQdSST       [Watts/(m^2*deg C)]   1/(rho*Cp)        [m/s]
!
! where S_surf is current model salinity near surface (i.e., fresh
! water precipitation/evaporation flux is converted into equivalent
! "salinity" flux. Units of climatological data: ssh[m], sst[deg C],
! tclima[deg C], and uclima[deg C] remain unchanged and only temporal
! interpolation has to be performed for them.
!
! Bottom drag is computed using either Styles and Glenn(1996) bottom
! boundary layer formulation, or linear/quadratic friction law..
!
! NOTE: Since some of the surface flux formulation may require
! climatological data (e.g., salinity at surface) update tclima
! first.

      implicit none
      integer tile, ierr
#include "param.h"
!
#include "compute_tile_bounds.h"
!
      ierr=0                           !--> Climatological data

# if defined M2NUDGING && !defined M2_FRC_BRY
#  ifdef ANA_SSH
      call ana_ssh_tile (istr,iend,jstr,jend)
#  else
      call set_ssh_tile (istr,iend,jstr,jend, ierr)
#  endif
# endif
# ifdef UCLIMATOLOGY
#  ifdef ANA_UCLIMA
      call ana_uclima_tile (istr,iend,jstr,jend)
#  else
      call set_uclima_tile (istr,iend,jstr,jend, ierr)
#  endif
# endif
# if defined SOLVE3D && defined TCLIMATOLOGY
#  ifdef ANA_TCLIMA
      call ana_tclima_tile (istr,iend,jstr,jend)
#  else
      call set_tclima_tile (istr,iend,jstr,jend, ierr)
#  endif
# endif
# if  defined T_FRC_BRY  || defined M2_FRC_BRY || \
      defined M3_FRC_BRY || defined Z_FRC_BRY
#  ifdef ANA_BRY
c***              no code here
#  else
      call set_bry_all_tile (istr,iend,jstr,jend, ierr)
#  endif
# endif

                                        !--> Surface fluxes
#ifdef ANA_SMFLUX
      call ana_smflux_tile (istr,iend,jstr,jend)
#else
      call set_smflux_tile (istr,iend,jstr,jend, ierr)
#endif
#ifdef SOLVE3D
# ifdef ANA_STFLUX
      call ana_stflux_tile (istr,iend,jstr,jend, itemp)
# else
      call set_stflux_tile (istr,iend,jstr,jend, itemp, ierr)
# endif
# ifdef SALINITY
#  ifdef ANA_SSFLUX
      call ana_stflux_tile (istr,iend,jstr,jend, isalt)
#  else
      call set_stflux_tile (istr,iend,jstr,jend, isalt, ierr)
#  endif
# endif
# ifdef LMD_KPP
#  ifdef ANA_SRFLUX
      call ana_srflux_tile (istr,iend,jstr,jend)
#  else
      call set_srflux_tile (istr,iend,jstr,jend, ierr)
#  endif
# endif
#endif
                                 !--> Bottom boundary fluxes
                                 !    [Styles and Glenn (1996) bottom
                                 !    boundary layer formulation.
                                 !    Not implemented in this code]
#if defined ANA_BMFLUX
      call ana_bmflux ILLEGAL
#elif defined SG_BBL96
# ifdef ANA_WWAVE
      call ana_wwave ILLEGAL
# else
      call set_wwave_tile (istr,iend,jstr,jend)
# endif
      call sg_bbl96 ILLEGAL
#endif


c>>>      if (ierr /= 0) may_day_flag=2
#ifdef ANA_PSOURCE
      if (ZEROTH_TILE) call ana_psource
#endif
      return
      end

