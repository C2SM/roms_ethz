#include "cppdefs.h"
#if defined SOLVE3D && (!defined ANA_STFLUX || !defined ANA_SSFLUX)

      subroutine set_stflux_tile(istr,iend,jstr,jend, itrc, ierr)

! Interpolate and assemble surface tracer flux for current tile.

      implicit none
      integer istr,iend,jstr,jend, itrc, ierr, i,j, it1,it2
      real tmid, cff1,cff2, cff

# include "param.h"
# include "scalars.h"
# include "ocean3d.h"
# define STFLUX_DATA
# if defined QCORRECTION && !defined ANA_SST
      integer it3,it4, it5,it6
      real  cff3,cff4, cff5,cff6
#  define SST_DATA
#  define DQDT_DATA
# endif
# if defined SFLX_CORR && defined  SALINITY
      integer  it7,it8
      real cff7,cff8, dSdt
#  define SSS_DATA
# endif
# ifdef ICEOBS
      integer  k,k1,k2
# endif
# include "forces.h"

# ifndef ICE_MODEL
! hbls (in mixing.h) is needed for surface flux limitation if SST falls below freezing
#  include "mixing.h"
# endif
# if defined ICEOBS || (defined SALINITY_MASKLATSTR && defined SALINITY_MASKLATEND) || defined WRITE_SALT_REST
#  include "grid.h"
# endif
# ifdef WRITE_SALT_REST
      real, parameter :: eps=1.e-33
# endif
# include "compute_extended_bounds.h"

! Set coefficients for interpolation.  Also check that for the next
! time step [when time=time+dt] both weights will still be positive --
! if not set synchro_flag to signal that a fresh timeslice of data
! should be read from an appropriate netCDF input file (master thread
! only). After that either load time-invariant data, or interpolate in
! time or complain about error and signal to quit, if interpolation
! is needed, but not possible.  Note unit conversion factors:

! for heat flux: 1./(rho0*Cp) to convert [W/(m^2*deg C)] into [m/sec]
! thus into units of kinematic flux; same applies to (T-SST)*dQdSST;

! for salinity(freshwater) flux: 0.01/day2sec to convert from [cm/day]
! into [PSU m/s]

      tmid=time+0.5*dt
      it1=3-itstf(itrc)            ; it2=itstf(itrc)
      cff1=stf_time(it2,itrc)-tmid ; cff2=tmid-stf_time(it1,itrc)
      if (cff1 < dt .and. ntstf(itrc) > 1) synchro_flag=.true.
      if (itrc == itemp) then
        cff=1./(rho0*Cp*(cff1+cff2))
      elseif (itrc == isalt) then
        cff=0.01/(day2sec*(cff1+cff2))
      else
        cff=1./(cff1+cff2)
      endif
      cff1=cff1*cff ; cff2=cff2*cff

# if defined QCORRECTION && !defined ANA_SST
      it3=3-itsst              ; it4=itsst
      cff3=sst_time(it4)-tmid  ; cff4=tmid-sst_time(it3)
      if (cff3 < dt .and. ntstf(itrc) > 1) synchro_flag=.true.
      cff=1./(cff3+cff4)
      cff3=cff3*cff ; cff4=cff4*cff

      it5=3-itdqdt             ; it6=itdqdt
      cff5=dqdt_time(it6)-tmid ; cff6=tmid-dqdt_time(it5)
      if (cff5 < dt .and. ntstf(itrc) > 1) synchro_flag=.true.
      cff=1./(rho0*Cp*(cff5+cff6))
      cff5=cff5*cff ; cff6=cff6*cff
# endif

# if defined SFLX_CORR && defined SALINITY
      it7=3-itsss              ; it8=itsss
      cff7=sss_time(it8)-tmid  ; cff8=tmid-sss_time(it7)
      if (cff7 < dt .and. ntstf(itrc) > 1) synchro_flag=.true.
      cff=1./(cff7+cff8)
      cff7=cff7*cff ; cff8=cff8*cff
!      dSdt=1./(90.*day2sec) ! relaxation time for surface salinity
      dSdt=1./(45.*day2sec) ! relaxation time for surface salinity
!      dSdt=1./(9000.*day2sec) ! relaxation time for surface salinity
# endif

      if (ntstf(itrc) == 1) then       ! Load time-invariant
        if (itrc == itemp) then        ! surface tracer flux
          cff=1./(rho0*Cp)
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,itemp)=cff*stflxg(i,j,itstf(itemp),itemp)
            enddo
          enddo
# ifdef SALINITY
        elseif (itrc == isalt) then
          cff=0.01/day2sec
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,isalt)=(cff*stflxg(i,j,itstf(isalt),isalt)
     &                                       *t(i,j,N,nrhs,isalt))
#  ifdef ICEOBS
     &                                               +melting(i,j)
#   if !defined BRINE_PLUMES
!AH16: Note that if BRINE_PLUMES is defined salt from freezing is
!      added later to the subsurface to mimic brine convection through 
!      the mixed layer, else it is added here
     &                                               +freezing(i,j)
#   endif
#  endif
            enddo
          enddo
# endif
        endif                          ! Interpolate surface
      else                             ! tracer fluxes in time
        if (itrc == itemp) then
# if defined WRITE_TEMP_REST
          cff=rho0*Cp
# endif
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,itemp)=cff1*stflxg(i,j,it1,itemp)
     &                        +cff2*stflxg(i,j,it2,itemp)
# if !defined ICE_MODEL
#  ifdef ICEOBS
! If there is net sea-ice freezing in the grid box we assume a freezing point
! skin temperature for the entire grid box and recalculate heat fluxes
! based on Markus (1999). Note that this overwrites any surface forcing
! heat flux in grid cells with net freezing. !AH16
              stflx(i,j,itemp)=stflx(i,j,itemp) !Note: stflx(i,j,itemp)*(1.-sic(i,j)) this is done in the forcing
     &           +min(0.,
     &           0.0055        ! Stanton number under ice, range 0.005-0.006 (McPhee 1992)
     &                                 *sqrt( sqrt( 0.333333333333*(
     &      sustr(i,j)**2 + sustr(i+1,j)**2 + sustr(i,j)*sustr(i+1,j)
     &     +svstr(i,j)**2 + svstr(i,j+1)**2 + svstr(i,j)*svstr(i,j+1)
     &                                                            )))
     &           *(-0.0543*t(i,j,N,nrhs,isalt)-t(i,j,N,nrhs,itemp))
     &           *sic(i,j))
#  endif  /* ICEOBS */
# endif  /* !ICE_MODEL */
# if defined QCORRECTION && !defined ANA_SST
!#  if defined PACIFIC || defined STFLX_LIM
              stflx(i,j,itemp)=stflx(i,j,itemp)
     &         +(cff5*dqdtg(i,j,it5)+cff6*dqdtg(i,j,it6))
     &         *( t(i,j,N,nrhs,itemp) -cff3*sstg(i,j,it3)
     &                                -cff4*sstg(i,j,it4)
     &                                                  )
#  ifdef WRITE_TEMP_REST
              RestFlxTemp(i,j)=cff
     &         *((cff5*dqdtg(i,j,it5)+cff6*dqdtg(i,j,it6))
     &          *( t(i,j,N,nrhs,itemp) -cff3*sstg(i,j,it3)
     &                                 -cff4*sstg(i,j,it4)
     &                                                  ))
#   ifdef MASKING
     &                                 *rmask(i,j)
#   endif
#  endif
# endif
# if !defined ICE_MODEL
! Restrict stflx to prevent surface temperature to go below surface 
! freezing point t_freez=-0.0543*salinity [C] (Steele et al. 1989)
              stflx(i,j,itemp)=max(stflx(i,j,itemp)
     &            ,Hz(i,j,N)*(-0.0543*t(i,j,N,nrhs,isalt)
     &                          -t(i,j,N,nrhs,itemp))/dt)
# endif
# ifdef WRITE_HEATFLX
              Heatflx(i,j)=cff*stflx(i,j,itemp)
#   ifdef MASKING
     &                                 *rmask(i,j)
#   endif
# endif
            enddo
          enddo
# ifdef SALINITY
        elseif (itrc == isalt) then      ! convert fresh-water flux
#  if defined WRITE_SALT_REST
          cff=day2sec/0.01
#  endif
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,isalt)=(t(i,j,N,nrhs,isalt)*(
     &                     cff1*stflxg(i,j,it1,isalt)
     &                  +cff2*stflxg(i,j,it2,isalt)))
#  ifdef ICEOBS
     &                                  +melting(i,j)
#   if !defined BRINE_PLUMES
!AH16: Note that if BRINE_PLUMES is defined salt from freezing is
!      added later to the subsurface to mimic brine convection through 
!      the mixed layer, else it is added here
     &                                 +freezing(i,j)
#   endif
#  endif
! Add relaxation of surface salinity back to climatological value
! to avoid long-term drift
# endif
# if defined SFLX_CORR && defined SALINITY
#  ifdef SALINITY_MASK
              if (sssg(i,j,it7) > 0. .and. sssg(i,j,it8) > 0.) then
#  endif
#  ifdef WRITE_SALT_REST
                RestFlxSalt(i,j)=stflx(i,j,isalt)
#  endif
                stflx(i,j,isalt)=stflx(i,j,isalt)-(dSdt*Hz(i,j,N)*(
     &                                       t(i,j,N,nrhs,isalt)
c**  &                                        -tclm(i,j,N,isalt)
     &                  -(cff7*sssg(i,j,it7)+cff8*sssg(i,j,it8))
     &                                                         )
#  if defined SALINITY_MASKLATSTR && defined SALINITY_MASKLATEND
     &          *max(max(min(abs(latr(i,j)-(SALINITY_MASKLATEND))
     & /abs((SALINITY_MASKLATSTR)-(SALINITY_MASKLATEND)),1.),0.0)
     &   ,min(max(real(floor(abs(latr(i,j)-(SALINITY_MASKLATEND))
     &        /abs((SALINITY_MASKLATSTR)-(SALINITY_MASKLATEND))))
     &           ,real(floor(abs(latr(i,j)-(SALINITY_MASKLATSTR))
     & /abs((SALINITY_MASKLATSTR)-(SALINITY_MASKLATEND))))),1.0))
#  endif
     &                                                         )
#  ifdef WRITE_SALT_REST
                RestFlxSalt(i,j)=( cff*(stflx(i,j,isalt)
     &              -RestFlxSalt(i,j))/max(t(i,j,N,nrhs,isalt),eps) )
#   ifdef MASKING
     &                                 *rmask(i,j)
#   endif
#  endif
#  ifdef SALINITY_MASK
              else
                RestFlxSalt(i,j)=0.
              endif
#  endif
# endif
            enddo
          enddo
        endif  !<-- itemp or isalt


        if ( cff1 < 0. .or. cff2 < 0.
# if defined QCORRECTION && !defined ANA_SST
     &       .or. cff3 < 0. .or. cff4 < 0.
     &       .or. cff5 < 0. .or. cff6 < 0.
#  if defined SFLX_CORR && defined  SALINITY
     &       .or. cff7 < 0. .or. cff8 < 0.
#  endif
# endif
     &                             ) then
          if (ZEROTH_TILE) then
            write(*,'(/1x,3A/3(1x,A,F16.10)/)')       '### WARNING: ',
     &  'set_stflux_tile :: Current model time is outside bounds of ',
     &  '''stf_time''.',  'stf_tstart =', stf_time(it1,itrc)*sec2day,
     &                    'tdays =',      tdays,
     &                    'stf_tend =',   stf_time(it2,itrc)*sec2day
          endif
          ierr=ierr+1
        endif
      endif
      end
#else
      subroutine set_stflux_empty
      end
#endif /* SOLVE3D  && !ANA_STFLUX || (SALINITY && !ANA_SSFLUX) */
