#include "cppdefs.h"
#if defined SOLVE3D && defined ICEOBS
 
                                      ! Read sea-ice concentration
      subroutine get_seaice (ierr)    ! at the appropriate time from
      implicit none                   ! forcing netCDF file.
# include "param.h"
# include "scalars.h"
# include "forces.h"
# include "ncvars.h"
# include "netcdf.inc"
      real cff
      integer ncid,ifrc, ierr, lfrc,lvar, lenstr, nf_fread
      character*(max_name_size) frcname
!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables. After that determine whether there is
! cycling to reuse the input data, find cycling period "ice_cycle",
! set initial cycling index "ice_ncycle" and record index "ice_rec".
! Set initial value for time index "itice" and both time record
! bounds to large negative artificial values to trigger logic in
! reading part below.
!
      ifrc=max(ice_file_id,1)
 100  ncid=ncidfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        if (ncid.eq.-1) then
          ierr=nf_open(frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfrc(ifrc)=ncid
          else
            write(stdout,'(/1x,4A/)') '### ERROR  in get_seaice: can ',
     &             'not open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(vname(1,indxCi))
          ierr=nf_inq_varid (ncid, vname(1,indxCi), ice_id)
          if (ierr .eq. nf_noerr) then
            ice_file_id=ifrc
CR            MPI_master_only write(*,*) 'Found ''', 
CR     &           vname(1,indxCi)(1:lvar),
CR     &           ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc
            ierr=nf_inq_varid (ncid, vname(1,indxFi), ice_Fid)
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxFi)(1:lvar),
     &                                 frcname(1:lfrc)
            endif
            ierr=nf_inq_varid (ncid, vname(1,indxMi), ice_Mid)
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxMi)(1:lvar),
     &                                 frcname(1:lfrc)
            endif
            ierr=nf_inq_varid (ncid, 'ice_time', ice_tid)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, ice_tid, ntice, ice_cycle,
     &                                 ice_ncycle, ice_rec, ierr)
              itice=1
              ice_time(1)=-1.E+20
              ice_time(2)=-1.E+20
            else
              write(stdout,1) 'ice_time', frcname(1:lfrc)
            endif
          else
            write(stdout,1) vname(1,indxCi)(1:lvar),
     &                                 frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_seaice cannot find variable ''',
     &       A, ''''/7x, 'in forcing netCDF file ''', A, '''.')
 
!
! Read data from the forcing file:  Get out, if model time is already
!===== ==== ==== === ======= =====  within the interval set by past
! and future data times. Otherwise increment record, flip the time
! index, and read a new portion of data. Repeat it until model time
! is between the two times from data.
!
      do while (ice_time(itice).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (ice_cycle, ntice, ice_ncycle, ice_rec,
     &                                                       ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE(ncid, ice_tid, ice_rec, cff)
          if (ierr .eq. nf_noerr) then
            itice=min(3-itice,ntice)
            ice_time(itice)=cff*day2sec + ice_cycle*ice_ncycle
            ierr=nf_fread (sicg(START_2D_ARRAY,itice), ncid,
     &                                   ice_id, ice_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &        '(6x,2A,F12.4,2(1x,A,I4))') 'get_seaice ',
     &                '-- read sea ice concentration ice_time =',
     &                 cff, 'rec =', ice_rec MYID
              if (ntice.eq.1) return
            else
              write(stdout,2) 'seaice', ice_rec
            endif
            ierr=nf_fread (freezingg(START_2D_ARRAY,itice), ncid,
     &                                   ice_Fid, ice_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &        '(6x,2A,F12.4,2(1x,A,I4))') 'get_seaice ',
     &                '-- read sea ice freezing ice_time =',
     &                 cff, 'rec =', ice_rec MYID
              if (ntice.eq.1) return
            else
              write(stdout,2) 'freezing', ice_rec
            endif
            ierr=nf_fread (meltingg(START_2D_ARRAY,itice), ncid,
     &                                   ice_Mid, ice_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &        '(6x,2A,F12.4,2(1x,A,I4))') 'get_seaice ',
     &                '-- read sea ice melting ice_time =',
     &                 cff, 'rec =', ice_rec MYID
              if (ntice.eq.1) return
            else
              write(stdout,2) 'melting', ice_rec
            endif
          else
            write(stdout,2) 'ice_time', ice_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &    'ERROR in get_seaice: requested time record ',   ice_rec,
     &    'exeeds the last record ',  ntice, 'available in forcing ',
     &    'netCDF file ''',   frcname(1:lfrc),   '''',    'tdays = ',
     &     tdays,             '  but the last available ice_time = ',
     &                                     ice_time(itice)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_seaice: cannot read variable ''',
     &                                 A, ''' for record ', I4/)
      return
      end
 
 
 
      subroutine set_seaice_tile (istr,iend,jstr,jend, ierr)
!
! Set-up sea-ice concentration for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff, cff1, cff2, cff3, cff4
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "forces.h"
# include "ocean3d.h"
!
# include "compute_extended_bounds.h"
!

! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data or interpolate in time.
! Complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
      it1=3-itice
      it2=itice
      cff1=ice_time(it2)-time
      cff2=time-ice_time(it1)
      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntice.eq.1) then                  ! Load time-invariant
        if (iic.eq.ntstart) then            ! sea-ice concentration
          cff=0.01/86400.                   ! fresh water flux: convert
          do j=jstrR,jendR                  ! from [cm/day] to [m/s]
            do i=istrR,iendR
              sic(i,j)=sicg(i,j,itice)
              freezing(i,j)=cff*freezingg(i,j,itice)*t(i,j,N,nrhs,isalt)
              melting(i,j)=cff*meltingg(i,j,itice)*t(i,j,N,nrhs,isalt)
            enddo
          enddo
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then     ! Interpolate in time.
        cff=0.01/(86400.*(cff1+cff2))               ! Fresh water flux: convert
        cff3=cff1*cff                               ! from [cm/day] to [m/s]
        cff4=cff2*cff
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        do j=jstrR,jendR
          do i=istrR,iendR
            sic(i,j)=cff1*sicg(i,j,it1)+cff2*sicg(i,j,it2)
            freezing(i,j)=cff3*freezingg(i,j,it1)+cff4*freezingg(i,j,it2)*t(i,j,N,nrhs,isalt)
            melting(i,j)=cff3*meltingg(i,j,it1)+cff4*meltingg(i,j,it2)*t(i,j,N,nrhs,isalt)
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &     'SET_ICELUX_TILE - current model time is outside bounds ',
     &     'of ''ice_time''.',  'ICE_TSTART=', ice_time(it1)*sec2day,
     &                          'TDAYS=',      tdays,
     &                          'ICE_TEND=',   ice_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else
      subroutine get_seaice_empty
      end
#endif /* SOLVE3D && ICEOBS */
 
