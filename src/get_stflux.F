#include "cppdefs.h"
#if defined SOLVE3D && (!defined ANA_STFLUX || !defined ANA_SSFLUX)

                                          ! Read surface flux for
      subroutine get_stflux (itrc, ierr)  ! tracer itrc from forcing
      implicit none                       ! netCDF file.
# define STFLUX_DATA
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "ncvars.h"
# include "netcdf.inc"
      integer itrc,ierr, ncid,ifrc, lfrc,lvar,ltvar,lenstr, nf_fread
      character(len=max_name_size) frcname
      character(len=8) stf_var, stf_time_var
      real cff

# ifdef VERBOSE
      write(*,*) 'enter get_stflux' MYID
# endif
                                      ! Specify netCDF variable name
      if (itrc==itemp) then         ! of the surface forcing flux for
        stf_var='shflux'              ! each tracer and name for the
        stf_time_var='shf_time'       ! corresponding time variable.
# ifdef SALINITY
      elseif (itrc==isalt) then     ! Currently it is done for T and
        stf_var='swflux'              ! S only, but in principle it can
        stf_time_var='swf_time'       ! be expanded to any number of
# endif
      endif                           ! tracers right here without
      lvar=lenstr(stf_var)            ! changing anything throughout
      ltvar=lenstr(stf_time_var)      ! any other parts of the code.


! Initialization:  Check, whether forcing netCDF file is already
!----------------  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables. Determine whether there is cycling to
! reuse the input data and what is cycling period "stf_cycle", find
! initial cycling index "stf_ncycle", and record index "stf_rec".
! Set initial value for time index "itstf" and set both time record
! bounds to large artificial negative values, to trigger the logic
! in reading part below.

      ifrc=max(stf_file_id(itrc),1)
 100  ncid=ncfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic==ntstart) then
        if (ncid==-1) then
          ierr=nf_open (frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr==nf_noerr) then
            ncfrc(ifrc)=ncid
          else
            write(*,'(/1x,4A,1x,2A,I4 /)') '### ERROR: get_stflux :: ',
     &            'Cannot open netCDF file ''', frcname(1:lfrc), '''.',
     &             nf_strerror(ierr) MYID
          endif
        endif
        if (ierr==nf_noerr) then
          ierr=nf_inq_varid (ncid, stf_var(1:lvar), stf_id(itrc))
          if (ierr == nf_noerr) then
            stf_file_id(itrc)=ifrc
            mpi_nonexit_warn write(*,'(1x,5A,I3)')    'Found ''',
     &              stf_var(1:lvar), ''' in ''', frcname(1:lfrc),
     &                                       ''', ifrc =', ifrc

            ierr=nf_inq_varid (ncid, stf_time_var(1:ltvar),
     &                                       stf_tid(itrc))
            if (ierr==nf_noerr) then
              call set_cycle (ncid, stf_tid(itrc), ntstf(itrc),
     &                          stf_cycle(itrc), stf_ncycle(itrc),
     &                                       stf_rec(itrc), ierr)
              itstf(itrc)=1
              stf_time(1,itrc)=-1.E+20
              stf_time(2,itrc)=-1.E+20
            else
              write(*,2) stf_time_var(1:ltvar), itrc, frcname(1:lfrc)
            endif
          elseif (ifrc < max_frc) then
            mpi_nonexit_warn write(*,1) stf_var(1:lvar), itrc,
     &                                         frcname(1:lfrc)
            ifrc=ifrc+1
            goto 100
          else
            write(*,2)  stf_var(1:lvar), itrc, frcname(1:lfrc)
          endif
        endif
      endif
  1   format(1x,  'WARNING: get_stflux :: Cannot find variable ''',
     &             A, ''', itrc =', I3, 1x, 'in file ''', A, '''.')
  2   format(1x, '### ERROR: get_stflux :: Cannot find variable ''',
     &             A, ''', itrc =', I3, 1x, 'in file ''', A, '''.')


! Read data from the file:   Check if model time is bounded by past
!----- ---- ---- --- -----   and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.

      do while (stf_time(itstf(itrc),itrc)<time+0.5*dt .and.
     &                                         ierr==nf_noerr)
        call advance_cycle (stf_cycle(itrc),  ntstf(itrc),
     &                  stf_ncycle(itrc), stf_rec(itrc), ierr)
        if (ierr==nf_noerr) then
          ierr=nf_get_var1_FTYPE(ncid, stf_tid(itrc),
     &                               stf_rec(itrc), cff)
          if (ierr==nf_noerr) then
            itstf(itrc)=min(3-itstf(itrc),ntstf(itrc))
            stf_time(itstf(itrc),itrc)=cff*day2sec
     &                     + stf_cycle(itrc)*stf_ncycle(itrc)

            ierr=nf_fread (stflxg(START_2D_ARRAY,itstf(itrc),itrc),
     &                 ncid, stf_id(itrc), stf_rec(itrc), r2dvar)
            if (ierr == nf_noerr) then
# ifdef MPI_SILENT_MODE
              if (mynode==0) then
# endif
                write(*,'(4x,A,I2,1x,A,F12.4,2(1x,A,I4))')
     &         'get_stflux :: read surface flux for tracer', itrc,
     &         'stf_time =', cff, 'rec =', stf_rec(itrc) MYID
# ifdef MPI_SILENT_MODE
                endif
# endif
              if (ntstf(itrc)==1) return
            else
              write(*,3) 'stflux', itrc, stf_rec, nf_strerror(ierr)
            endif
          else
            write(*,3) 'stf_time', itrc, stf_rec(itstf(itrc)),
     &                                            nf_strerror(ierr)
          endif
        else
          write(*,'(/1x,2A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')  '### ',
     &   'ERROR: get_stflux :: requested time record ', stf_rec(itrc),
     &   'exeeds the last record',    ntstf(itrc),    'available in ',
     &   'netCDF file ''',    frcname(1:lfrc),    '''',    'tdays = ',
     &    tdays, '  but the last available  stf_time =',
     &                           stf_time(itstf(itrc),itrc)*sec2day
        endif
      enddo
  3   format(/1x,'### ERROR: get_stflux :: cannot read variable ''',
     &                    A, ''', itrc =', I2, ' rec =', I4, 12x,A/)
# ifdef VERBOSE
      write(*,*) '...leaving get_stflux' MYID
# endif
      end




      subroutine set_stflux_tile (istr,iend,jstr,jend, itrc, ierr)

! Interpolate and assemble surface tracer flux for current tile.

      implicit none
      integer istr,iend,jstr,jend, itrc, ierr, i,j, it1,it2
      real cff, cff1,cff2
# if defined QCORRECTION && !defined ANA_SST
      integer it3,it4, it5,it6
      real  cff3,cff4, cff5,cff6
#  ifdef SALINITY
      integer  it7,it8
      real cff7,cff8, dSdt
#  endif
# endif

# define STFLUX_DATA
# define SST_DATA

# include "param.h"
# include "scalars.h"
# include "ocean3d.h"
# include "forces.h"
# ifdef PACIFIC
#  include "mixing.h"
# endif
# ifdef ICEOBS
#  include "grid.h"
# endif
# include "compute_extended_bounds.h"

! Set coefficients for interpolation.  Also check that for the next
! time step [when time=time+dt] both weights will still be positive --
! if not set synchro_flag to signal that a fresh timeslice of data
! should be read from an appropriate netCDF input file (master thread
! only). After that either load time-invariant data, or interpolate in
! time or complain about error and signal to quit, if interpolation
! is needed, but not possible.

      cff=time+0.5*dt

      it1=3-itstf(itrc)
      it2=itstf(itrc)
      cff1=stf_time(it2,itrc)-cff
      cff2=cff-stf_time(it1,itrc)

# if defined QCORRECTION && !defined ANA_SST
      it3=3-itsst
      it4=itsst
      cff3=sst_time(it4)-cff
      cff4=cff-sst_time(it3)

      it5=3-itdqdt
      it6=itdqdt
      cff5=dqdt_time(it6)-cff
      cff6=cff-dqdt_time(it5)

#  ifdef SALINITY
      it7=3-itsss
      it8=itsss
      cff7=sss_time(it8)-cff
      cff8=cff-sss_time(it7)

      dSdt=1./(90.*day2sec) ! relaxation time for surface salinity

c**      dSdt=0.

#  endif
# endif


      if ( ( cff1<dt
# if defined QCORRECTION && !defined ANA_SST
     &          .or. cff3<dt .or. cff5<dt
#  ifdef SALINITY
     &                          .or. cff7<dt
#  endif
# endif
     &   ) .and. ntstf(itrc)>1 ) synchro_flag=.true.



      if (ntstf(itrc)==1) then       ! Load time-invariant
        if (itrc==itemp) then        ! surface tracer flux
          cff=1./(rho0*Cp)
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,itemp)=cff*stflxg(i,j,itstf(itemp),itemp)
            enddo
          enddo
# ifdef SALINITY
        elseif (itrc==isalt) then
          cff=0.01/86400.
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,isalt)=cff*stflxg(i,j,itstf(isalt),isalt)
     &                                       *t(i,j,N,nrhs,isalt)
            enddo
          enddo
# endif
        endif                          ! Interpolate surface
      else                             ! tracer fluxes in time
        if (itrc == itemp) then
          cff=1./(rho0*Cp*(cff1+cff2))
          cff1=cff1*cff
          cff2=cff2*cff

# if defined QCORRECTION && !defined ANA_SST
          cff=1./(cff3+cff4)
          cff3=cff3*cff                ! Interpolate SST and dQdSST
          cff4=cff4*cff                ! data to model time. Convert
                                       ! units of dQdSST from
          cff=1./(rho0*Cp*(cff5+cff6)) ! [Watts/(m^2*deg C)] to [m/sec]
          cff5=cff5*cff                ! so the product (T-SST)*dQdSST
          cff6=cff6*cff                ! has units of kinematic flux.
# endif


          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,itemp)=cff1*stflxg(i,j,it1,itemp)
     &                          +cff2*stflxg(i,j,it2,itemp)

# if defined QCORRECTION && !defined ANA_SST
     &           +(cff5*dqdtg(i,j,it5)+cff6*dqdtg(i,j,it6))
     &           *( t(i,j,N,nrhs,itemp)-( cff3*sstg(i,j,it3)
     &                                   +cff4*sstg(i,j,it4)
     &                                                    ))

!
! Restrict stflx to prevent surface temperature to go below
! freezing point (degrees C) when not using the sea ice model
!
!#  if defined PACIFIC || defined STFLX_LIM
#  if !defined ICE_MODEL
#   ifdef ICEOBS

! If there is sea-ice freezing in the grid box we assume a freezing point
! skin temperature for the entire grid box and recalculate heat fluxes
! based on Markus (1999). Note that this overwrites any surface forcing
! heat flux in grid cells with sea ice. !AH15
!
              stflx(i,j,itemp)=(stflx(i,j,itemp)
     &           *abs(min(0.,(real(ceiling(freezing(i,j)))-1.)))
     &           *(1.-sic(i,j)))
     &           +(0.006        ! Stanton number under ice
#    ifdef MASKING
     &            *(sqrt(sqrt( 
     &            ( (sustr(i,j)*umask(i,j)+sustr(i+1,j)*umask(i+1,j))
     &                         *(1.0-0.5*umask(i,j)*umask(i+1,j)) )**2
     &           +( (svstr(i,j)*vmask(i,j)+svstr(i,j+1)*vmask(i,j+1))
     &                         *(1.0-0.5*vmask(i,j)*vmask(i,j+1)) )**2 
     &                                                        )))
#    else
     &            *(sqrt(0.5*sqrt( (sustr(i,j)+sustr(i+1,j))**2
     &                             +(svstr(i,j)+svstr(i,j+1))**2)))
#    endif
     &           *(-0.0543*t(i,j,N,nrhs,isalt)-t(i,j,N,nrhs,itemp))
     &           *max(min(1.,real(ceiling(freezing(i,j)))),sic(i,j)))
!     &           *sic(i,j))
#   endif  /* ICEOBS */
              stflx(i,j,itemp)=max( stflx(i,j,itemp),
#   ifdef LMD_KPP
     &                          max(hbls(i,j,nstp), Hz(i,j,N))
#   else
     &                                               Hz(i,j,N)
#   endif
!     &                         *(-2.D0-t(i,j,N,nrhs,itemp))/dt )
! Steele et al. 1989 surface freezing temp: t_freez=-0.0543*salinity !AH14
     &             *(-0.0543*t(i,j,N,nrhs,isalt)-t(i,j,N,nrhs,itemp))/dt
     &                                                       )
#  endif
# endif

            enddo
          enddo
# ifdef SALINITY
        elseif (itrc == isalt) then      ! fresh-water flux: convert
          cff=0.01/(day2sec*(cff1+cff2)) ! from [cm/day] to [PSU m/s]
          cff1=cff1*cff                  ! and multiply by surface
          cff2=cff2*cff                  ! salinity.

#  if defined QCORRECTION && !defined ANA_SST
          cff=1./(cff7+cff8)
          cff7=cff7*cff
          cff8=cff8*cff
#  endif
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,isalt)=t(i,j,N,nrhs,isalt)*(
     &                     cff1*stflxg(i,j,it1,isalt)
     &                    +cff2*stflxg(i,j,it2,isalt)
     &                                              )

! Add relaxation of surface salinity back to climatological value
! to avoid long-term drift

#  if defined QCORRECTION && !defined ANA_SST

#   ifdef SALINITY_MASK
              if (sssg(i,j,it7) > 0.) then
#   endif
             stflx(i,j,isalt)=stflx(i,j,isalt) -dSdt*Hz(i,j,N)*(
     &                                   t(i,j,N,nrhs,isalt)
c**  &                                         -tclm(i,j,N,isalt)

     &                   -(cff7*sssg(i,j,it7)+cff8*sssg(i,j,it8))
     &                                                          )
#   ifdef SALINITY_MASK
              endif
#   endif
#  endif
     &                                                             )
# endif
            enddo
          enddo
# endif
        endif  !<-- itemp or isalt


        if (  cff1<0. .or. cff2<0.
# if defined QCORRECTION && !defined ANA_SST
     &         .or. cff3<0. .or. cff4<0.
     &         .or. cff5<0. .or. cff6<0.
#  ifdef SALINITY
     &         .or. cff7<0. .or. cff8<0.
#  endif
# endif
     &                                 ) then
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
      subroutine get_stflux_empty
      end
#endif /* SOLVE3D  && !ANA_STFLUX || (SALINITY && !ANA_SSFLUX) */
