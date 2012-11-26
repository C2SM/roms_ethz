#include "cppdefs.h"
#if defined SOLVE3D && (!defined ANA_STFLUX || !defined ANA_SSFLUX)
 
                                          ! Read surface flux for
      subroutine get_stflux (itrc, ierr)  ! tracer itrc from forcing
      implicit none                       ! netCDF file.
# define STFLUX_DATA
# include "param.h"
# ifdef MPI
#  include "mpif.h"
# endif
# include "forces.h"
# include "scalars.h"
# include "ncvars.h"
# include "netcdf.inc"
      real cff
      integer itrc, ncid,ifrc, ierr, lfrc,lvar,lenstr, nf_fread
      character*(max_name_size) frcname
!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables. Determine whether there is cycling to
! reuse the input data and what is cycling period "stf_cycle", find
! initial cycling index "stf_ncycle", and record index "stf_rec".
! Set initial value for time index "itstf" and set both time record
! bounds to large artificial negative values, to trigger the logic
! in reading part below.
!
      ifrc=max(stf_file_id(itrc),1)
 100  ncid=ncidfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        if (ncid.eq.-1) then
          ierr=nf_open (frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfrc(ifrc)=ncid
          else
            write(stdout,'(/1x,4A/)') '### ERROR in get_stflux: ',
     &        'cannot open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr.eq.nf_noerr) then
          lvar=lenstr(vname(1,indxSHFl+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxSHFl+itrc-1),
     &                                              stf_id(itrc))
          if (ierr .eq. nf_noerr) then
            stf_file_id(itrc)=ifrc
CR            MPI_master_only write(*,*) 'Found ''', 
CR     &              vname(1,indxSHFl+itrc-1)(1:lvar), 
CR     &              ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc 
            if (itrc.eq.itemp) then
              ierr=nf_inq_varid (ncid, 'shf_time', stf_tid(itrc))
# ifdef SALINITY
            elseif (itrc.eq.isalt) then
              ierr=nf_inq_varid (ncid, 'swf_time', stf_tid(itrc))
# endif
            endif
            if (ierr.eq.nf_noerr) then
              call set_cycle (ncid, stf_tid(itrc), ntstf(itrc),
     &                          stf_cycle(itrc), stf_ncycle(itrc),
     &                                       stf_rec(itrc), ierr)
              itstf(itrc)=1
              stf_time(1,itrc)=-1.E+20
              stf_time(2,itrc)=-1.E+20
            else
              if (itrc.eq.itemp) then
                write(stdout,1) 'shf_time', itrc, frcname(1:lfrc)
# ifdef SALINITY
              elseif (itrc.eq.isalt) then
                write(stdout,1) 'swf_time', itrc, frcname(1:lfrc)
# endif
              endif
            endif
          else
            write(stdout,1) vname(1,indxSHFl+itrc-1)(1:lvar),
     &                                   itrc, frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_smflux cannot find variable ''', A,
     &  ''', itrc =', I3/7x, 'in forcing netCDF file ''', A, '''.')
 
!
! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (stf_time(itstf(itrc),itrc).lt.time+0.5*dt .and.
     &                                         ierr.eq.nf_noerr)
        call advance_cycle (stf_cycle(itrc),  ntstf(itrc),
     &                  stf_ncycle(itrc), stf_rec(itrc), ierr)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE(ncid, stf_tid(itrc),
     &                               stf_rec(itrc), cff)
          if (ierr.eq.nf_noerr) then
            itstf(itrc)=min(3-itstf(itrc),ntstf(itrc))
            stf_time(itstf(itrc),itrc)=cff*day2sec
     &                     + stf_cycle(itrc)*stf_ncycle(itrc)
 
            ierr=nf_fread (stflxg(START_2D_ARRAY,itstf(itrc),itrc),
     &                 ncid, stf_id(itrc), stf_rec(itrc), r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &        '(6x,A,I2,1x,A,F12.4,2(1x,A,I4))')
     &           'get_stflux -- read surface flux for tracer', itrc,
     &           'stf_time =', cff, 'rec =', 
     &           stf_rec(itrc) MYID
              if (ntstf(itrc).eq.1) return
            else
              write(stdout,2) 'stflux', itrc, stf_rec
            endif
          else
            write(stdout,2) 'stf_time', itrc, stf_rec(itstf(itrc))
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &  'ERROR in get_stflux: requested time record ', stf_rec(itrc),
     &  'exeeds the last record',    ntstf(itrc),    'available in ',
     &  'netCDF file ''',    frcname(1:lfrc),    '''',    'tdays = ',
     &   tdays, '  but the last available  stf_time =',
     &                          stf_time(itstf(itrc),itrc)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_stflux: cannot read variable ''',
     &                 A, ''', itrc =', I2, ' for record ', I4/)
      return
      end
 
      subroutine set_stflux_tile (istr,iend,jstr,jend, itrc, ierr)
!
! Set-up surface tracer flux for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, itrc, ierr, i,j, it1,it2,
     &                                    it3,it4, it5,it6, it7,it8
      real cff, cff1,cff2, cff3,cff4, cff5,cff6, cff7,cff8, dSdt

# define STFLUX_DATA
# define SST_DATA

# include "param.h"
# include "scalars.h"
# include "ocean3d.h"
# include "forces.h"
# if defined PACIFIC || defined STFLX_LIM
#  include "mixing.h"
# endif
!
# include "compute_extended_bounds.h"
!
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
      it1=3-itstf(itrc)
      it2=itstf(itrc)
      cff1=stf_time(it2,itrc)-time
      cff2=time-stf_time(it1,itrc)

# if defined QCORRECTION && !defined ANA_SST
      it3=3-itsst
      it4=itsst
      cff3=sst_time(it4)-time
      cff4=time-sst_time(it3)

      it5=3-itdqdt
      it6=itdqdt
      cff5=dqdt_time(it6)-time
      cff6=time-dqdt_time(it5)

#  ifdef SALINITY
      it7=3-itsss
      it8=itsss
      cff7=sss_time(it8)-time
      cff8=time-sss_time(it7)

      dSdt=1./(90.*day2sec) ! relaxation time for surface salinity

c**      dSdt=0.

#  endif
# endif


      if ( cff1.lt.dt
# if defined QCORRECTION && !defined ANA_SST
     &             .or. cff3.lt.dt .or. cff5.lt.dt
#  ifdef SALINITY
     &                             .or. cff7.lt.dt
#  endif
# endif
     &                     ) synchro_flag=.true.


 
      if (ntstf(itrc).eq.1) then           ! Load time-invariant
        if (itrc.eq.itemp) then            ! surface tracer flux
          cff=1./(rho0*Cp)
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,itemp)=cff*stflxg(i,j,itstf(itemp),itemp)
            enddo
          enddo
# ifdef SALINITY
        elseif (itrc.eq.isalt) then
          cff=0.01/86400.
          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,isalt)=cff*stflxg(i,j,itstf(isalt),isalt)
     &                                       *t(i,j,N,nrhs,isalt)
            enddo
          enddo
# endif
        endif                                 ! Interpolate surface
      elseif ( cff1.ge.0. .and. cff2.ge.0.    ! tracer fluxes in time
# if defined QCORRECTION && !defined ANA_SST
     &        .and. cff3.ge.0. .and. cff4.ge.0. 
     &        .and. cff5.ge.0. .and. cff6.ge.0.
#  ifdef SALINITY
     &        .and. cff7.ge.0. .and. cff8.ge.0.
#  endif
# endif
     &                                 ) then
        if (itrc.eq.itemp) then
          cff=1./(rho0*Cp*(cff1+cff2))
          cff1=cff1*cff
          cff2=cff2*cff

# if defined QCORRECTION && !defined ANA_SST
          cff=1./(cff3+cff4)
          cff3=cff3*cff                ! Interpolate SST and dQdSST
          cff4=cff4*cff                ! data to model time.  Convert
                                       ! units of dQdSST from
          cff=1./(rho0*Cp*(cff5+cff6)) ! [Watts/(m^2*deg C)] to [m/sec]
          cff5=cff5*cff                ! so that product (T-SST)*dQdSST
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
! Restrict stflx to prevent surface temperature to go below -2
! degrees C.
!
#  if defined PACIFIC || defined STFLX_LIM
              stflx(i,j,itemp)=max( stflx(i,j,itemp),
#   ifdef LMD_KPP
     &                          max(hbls(i,j,nstp), Hz(i,j,N))
#   else
     &                                               Hz(i,j,N)
#   endif
     &                         *(-2.D0-t(i,j,N,nrhs,itemp))/dt
     &                                                       )
#  endif
# endif

            enddo
          enddo
# ifdef SALINITY
        elseif (itrc.eq.isalt) then      ! fresh water flux: convert
          cff=0.01/(86400.*(cff1+cff2))  ! from [cm/day] to [PSU m/s]
          cff1=cff1*cff                  ! and multiply by surface
          cff2=cff2*cff                  ! salinity.

          cff=1./(cff7+cff8)
          cff7=cff7*cff
          cff8=cff8*cff

          do j=jstrR,jendR
            do i=istrR,iendR
              stflx(i,j,isalt)=t(i,j,N,nrhs,isalt)*(
     &                     cff1*stflxg(i,j,it1,isalt)
     &                    +cff2*stflxg(i,j,it2,isalt)
     &                                             )


!
! Add relaxation of surface salinity back to climatological value
! to avoid long-term drift
!
     &                 -dSdt*Hz(i,j,N)*( t(i,j,N,nrhs,isalt)
c**  &                                         -tclm(i,j,N,isalt)
# if defined QCORRECTION && !defined ANA_SST
     &                   -(cff7*sssg(i,j,it7)+cff8*sssg(i,j,it8))
#endif
     &                                                          )



            enddo
          enddo
# endif
        endif
      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &            'SET_STFLUX_TILE - current model time is outside ',
     &            'bounds of ''stf_time''.',
     &                    'STF_TSTART=',  stf_time(it1,itrc)*sec2day,
     &                    'TDAYS=',       tdays,
     &                    'STF_TEND=',    stf_time(it2,itrc)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else
      subroutine get_stflux_empty
      end
#endif /* SOLVE3D  && !ANA_STFLUX || (SALINITY && !ANA_SSFLUX) */
