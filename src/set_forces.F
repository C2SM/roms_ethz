#include "cppdefs.h"

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
#if defined VFLX_CORR || defined TSOURCE
      integer itrc, i, j, k
# include "comm_vars.h"
# include "forces.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef MASKING
#  include "grid.h"
# endif
#endif
#if defined TSOURCE
# include "sources.h"
#endif

#include "compute_tile_bounds.h"

      ierr=0

! External data to supply at open boundaries. Note that there are
! two mutually exclusive mechanisms for each variable: either _BRY
! or climatological data. 

#if defined T_FRC_BRY  || defined M2_FRC_BRY || \
    defined M3_FRC_BRY || defined Z_FRC_BRY
# ifdef ANA_BRY
c***              no code here
# else
      call set_bry_all_tile(istr,iend,jstr,jend, ierr)
# endif
#endif

!--> Climatological data

#if defined M2NUDGING && !defined M2_FRC_BRY
# ifdef ANA_SSH
      call ana_ssh_tile(istr,iend,jstr,jend)
# else
      call set_ssh_tile(istr,iend,jstr,jend, ierr)
# endif
#endif
#ifdef UCLIMATOLOGY
# ifdef ANA_UCLIMA
      call ana_uclima_tile(istr,iend,jstr,jend)
# else
      call set_uclima_tile (istr,iend,jstr,jend, ierr)
# endif
#endif
#if defined SOLVE3D && defined TCLIMATOLOGY
# ifdef ANA_TCLIMA
      call ana_tclima_tile(istr,iend,jstr,jend)
# else
      call set_tclima_tile(istr,iend,jstr,jend, ierr)
#  ifdef TNUDGE_WEIGHTS
      call set_nudg_weights_tile(istr,iend,jstr,jend, ierr)
#  endif
# endif
#endif

# ifdef PCO2AIR_FORCING
      call set_pco2air_tile(istr,iend,jstr,jend,ierr)
# endif
# ifdef RIVER_LOAD_N
      call set_din_river_tile(istr,iend,jstr,jend,ierr)
# endif
# ifdef RIVER_LOAD_P
      call set_dip_river_tile(istr,iend,jstr,jend,ierr)
# endif

# ifdef NHY_FORCING
      call set_nhy_tile(istr,iend,jstr,jend,ierr)
# endif
# ifdef NOX_FORCING
      call set_nox_tile(istr,iend,jstr,jend,ierr)
# endif
# ifdef POX_FORCING
      call set_pox_tile(istr,iend,jstr,jend,ierr)
# endif


#ifdef ANA_SMFLUX
      call ana_smflux_tile(istr,iend,jstr,jend)
#elif !defined BULK_SM_UPDATE
      call set_smflux_tile(istr,iend,jstr,jend, ierr)
#endif

! Thermodynamic forcing: Note that BULK_FLUX requires computing the
! short-wave radiation flux first because bulk flux routine performs
! the final assembly of everything. Conversely if model is forced by
! precomputed total flux (which includes daily averaged short-wave
! radiation interpolated in time), then to introduce DIURNAL CYCLE
! modulation set_srflux routine must interpolate short-wave flux in
! time first, then subtract it from total, then modulate short-wave,
! and, finally, add it back to total -- hence it must be called after.


#ifdef SOLVE3D
# ifdef BULK_FLUX
#  ifdef LMD_KPP
#   ifdef ANA_SRFLUX
      call ana_srflux_tile(istr,iend,jstr,jend)
#   else
      call set_srflux_tile(istr,iend,jstr,jend, ierr)
#   endif
#  endif

      call bulk_flux_tile(istr,iend,jstr,jend)

# else
#  if defined SALINITY && defined ICEOBS
      call set_seaice_tile(istr,iend,jstr,jend,ierr)
#  endif
#  ifdef ANA_STFLUX
      call ana_stflux_tile(istr,iend,jstr,jend, itemp)
#  else
      call set_stflux_tile(istr,iend,jstr,jend, itemp, ierr)
#  endif
#  ifdef LMD_KPP
#   ifdef ANA_SRFLUX
      call ana_srflux_tile(istr,iend,jstr,jend)
#   else
      call set_srflux_tile(istr,iend,jstr,jend, ierr)
#   endif
#  endif
#  ifdef SALINITY
#   ifdef ANA_SSFLUX
      call ana_stflux_tile(istr,iend,jstr,jend, isalt)
#   else
      call set_stflux_tile(istr,iend,jstr,jend, isalt, ierr)
#   endif
#   if defined VFLX_CORR
      ! Flux correction for all tracers
#   ifdef USE_GLOBAL_SRF_SUM
      cff = 1. / global_srf_sum(isalt)
#   endif
      do itrc=isalt+1,NT
         do j=jstr,jend
            do i=istr,iend
#   ifdef MASKING
               if (rmask(i,j) .eq. 1) then
#   endif
               stflx(i,j,itrc)=       (stflx(i,j,isalt)
#   if defined LMD_KPP && defined ICEOBS && defined BRINE_PLUMES
     &                                   +freezing(i,j)
#   endif
     &                                               )*
#   ifdef USE_GLOBAL_SRF_SUM
     &                       global_srf_sum(itrc) * cff
#   else
     &           t(i,j,N,nrhs,itrc)/t(i,j,N,nrhs,isalt)
#   endif
#   ifdef MASKING
               else ! rmask(i,j) .eq. 1
                  stflx(i,j,itrc)=0.0
               endif
#   endif
            enddo
         enddo
      enddo
#   endif /* VFLX_CORR */
#  endif /* SALINITY */
# endif
# ifdef TSOURCE
! AH17: Compute tracer flux Tflx from 3D source
! The tracer flux of the source is computed from 
! a certain volume flux (T_src(i,j,k,0) in [m3 * sec-1])
! with a predefined tracer concentration (T_src(i,j,k,itrc)
! in [tracer unit]) and accounting for dillution of the tracers,
! which corresponds to the way that dillution occurs
! due to the surface flux for all tracers.
! Tflx is added to the tracer in step3d_t_ISO.F.
      do itrc=1,NT
        do j=jstr,jend
          do i=istr,iend
            do k=1,N
              Tflx(i,j,k,itrc)=pm(i,j)*pn(i,j)*T_src(i,j,k,0)
     &                *(T_src(i,j,k,itrc)-t(i,j,k,nrhs,itrc))
            enddo
          enddo
        enddo
      enddo
# endif /* TSOURCE */
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      call set_agedye_tile(istr,iend,jstr,jend,ierr)
# endif
# if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
      call set_iron_tile (Istr,Iend,Jstr,Jend,ierr) 
      call set_dust_tile (Istr,Iend,Jstr,Jend,ierr) 
# endif
#endif  /* SOLVE3D */

                                 
!--> Bottom boundary fluxes [Styles and Glenn (1996) bottom
!    boundary layer formulation.  Not implemented in this code]

#if defined ANA_BMFLUX
      call ana_bmflux ILLEGAL
#elif defined SG_BBL96
# ifdef ANA_WWAVE
      call ana_wwave ILLEGAL
# else
      call set_wwave_tile(istr,iend,jstr,jend)
# endif
      call sg_bbl96 ILLEGAL
#endif

c>>>      if (ierr /= 0) may_day_flag=2
#ifdef ANA_PSOURCE
      if (ZEROTH_TILE) call ana_psource
#endif
      end

