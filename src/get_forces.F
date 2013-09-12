#include "cppdefs.h"
 
      subroutine get_forces
!
! Read various forcing and climatological fields from their netCDF
! files and save them as globally accessible arrays in common blocks
! "forces.h" and "climat.h". NOTE that NO EFFORT has been made to
! convert units of these fluxes at this stage (i.e., convert them
! into kinematic fluxes): typically data is assumed to have dynamical
! physical units, thus wind stress is in [Newton/m^2]; surface heat
! and short wave radiation fluxes are in [Watts/m^2]; SST sensitivity
! to heat flux dQdSST is in [Watts/(deg C * m^2)]; surface fresh
! water flux is in [mm/day].
!
      implicit none
      integer ierr
#include "param.h"
#include "scalars.h"
      ierr=0
#if defined M2NUDGING && !defined M2_FRC_BRY && !defined ANA_SSH
      call get_ssh (ierr)
#endif
#if defined UCLIMATOLOGY && !defined ANA_UCLIMA
      if (ierr.eq.0) call get_uclima (ierr)
#endif
#ifdef SOLVE3D
# if ( defined TCLIMATOLOGY ||( defined TNUDGING && \
       defined T_FRC_BRY )) && !defined ANA_TCLIMA
      if (ierr.eq.0) call get_tclima (ierr)
# endif
#endif
# if  defined T_FRC_BRY  || defined M2_FRC_BRY || \
      defined M3_FRC_BRY || defined Z_FRC_BRY 
#  ifndef ANA_BRY
      if (ierr.eq.0) call get_all_bry (ierr)
#  endif
# endif
#if defined SSH_TIDES || defined UV_TIDES
      if (ierr.eq.0) call get_tides (ierr)
#endif


#ifndef ANA_SMFLUX
      if (ierr.eq.0) call get_smflux (ierr)
#endif
#ifdef SOLVE3D
# ifndef ANA_STFLUX
      if (ierr.eq.0) call get_stflux (itemp, ierr)
# endif
# if defined SALINITY && !defined ANA_SSFLUX
      if (ierr.eq.0) call get_stflux (isalt, ierr)
# endif
# if defined QCORRECTION && !defined ANA_SST
      if (ierr.eq.0) call get_sst (ierr)
      if (ierr.eq.0) call get_dqdt (ierr)

      if (ierr.eq.0) call get_sss (ierr)

# endif
# ifdef BIOLOGY_BEC
        if (ierr.eq.0)  call get_dust(ierr)
        if (ierr.eq.0)  call get_iron(ierr)
# endif
# ifdef PCO2AIR_FORCING
        if (ierr.eq.0)  call get_pCO2air(ierr)
# endif
# if (defined LMD_KPP || defined LMD_BKPP) && !defined ANA_SRFLUX
      if (ierr.eq.0) call get_srflux (ierr)
# endif
#endif
#if defined SG_BBL96
# ifndef ANA_WWAVE
      if (ierr.eq.0) call get_wwave (ierr)
# endif
# ifndef ANA_BSEDIM
      if (ierr.eq.0) call get_bsedim (ierr)
# endif
#endif
! DL:
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      if (ierr.eq.0) call get_agedye(ierr)
#endif
      if (ierr.ne.0) may_day_flag=2
      return
      end
 
 
 
      subroutine set_forces (tile)
      
!
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
!
      implicit none
      integer tile, ierr, itrc, i, j
      real cff
#include "param.h"
#if defined VFLX_CORR && (defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC)
# include "comm_vars.h"
# include "forces.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef MASKING
#  include "grid.h"
# endif
#endif /* VFLX_CORR && (defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC) */
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
      call set_all_bry_tile (istr,iend,jstr,jend, ierr)
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
#   if defined VFLX_CORR && (defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC)
      ! Flux correction for all biological/chemical tracers
#   ifdef USE_GLOBAL_SRF_SUM
      cff = 1. / global_srf_sum(isalt)
#   endif
      do itrc=isalt+1,NT
         do j=jstr,jend
            do i=istr,iend
#   ifdef MASKING
               if (rmask(i,j) .eq. 1) then
#   endif
               stflx(i,j,itrc)=stflx(i,j,isalt)*
#   ifdef USE_GLOBAL_SRF_SUM
     &              global_srf_sum(itrc) * cff
#   else
     &         t(i,j,N,nrhs,itrc)/t(i,j,N,nrhs,isalt)
#   endif
#   ifdef MASKING
               else ! rmask(i,j) .eq. 1
                  stflx(i,j,itrc)=0.0
               endif
#   endif
            enddo
         enddo
      enddo
#   endif /* VFLX_CORR && (BIOLOGY_NPZDOC || BIOLOGY_BEC) */
#  endif /* ANA_SSFLUX */
# endif /* SALINITY */
# ifdef BIOLOGY_BEC
      call set_iron_tile (Istr,Iend,Jstr,Jend,ierr) 
      call set_dust_tile (Istr,Iend,Jstr,Jend,ierr) 
# endif
# if (defined LMD_KPP || defined LMD_BKPP)
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


c>>>      if (ierr.ne.0) may_day_flag=2
#ifdef ANA_PSOURCE
      if (ZEROTH_TILE) call ana_psource
#endif

! DL:
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      if (ierr.eq.0) call set_agedye_tile (istr,iend,jstr,jend,ierr)
#endif

      return
      end
