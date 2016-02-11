#include "cppdefs.h"
#if defined SOLVE3D && defined FLDSOURCE
 
      subroutine get_fldsource (ierr)
!
!  Read in data related to field sources
!  from NetCDF files.
!
      implicit none
# include "param.h"
# ifdef MPI
#  include "mpif.h"
# endif
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
# include "sources.h"
      real cff
      integer ierr, ifldsrc, lstr, lenstr, nf_fread, varid, ncid, itrc, lvar
      character units*64

!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether fldsrc data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "fldsrc_cycle", set initial cycling index "fldsrc_ncycle", record index
! "fldsrc_rec", and time to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      ifldsrc=max(fldsrc_file_id,1)
 100  ncid=ncidfldsrc(ifldsrc)
      lstr=lenstr(fldsrc_file)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        if (ncid.eq.-1) then
          ierr=nf_open(fldsrc_file(1:lstr), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfldsrc(ifldsrc)=ncid
          else
            write(stdout,'(/1x,4A/)') '### ERROR in get_fldsource: can ',
     &           'not open netCDF file ''', fldsrc_file(1:lstr), '''.'
          endif
        endif
        do itrc=1,NT
          lvar=lenstr(vname(1,indxT+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                    fldsrc_id(itrc))
          if (ierr .ne. nf_noerr) then
            MPI_master_only write(*,1) vname(1,indxT+itrc-1)(1:lvar), 
     &                           fldsrc_file(1:lstr)!, nf_strerror(ierr)
     &            ,' => Initialized to 0.' 
          endif
        enddo
        ierr=nf_inq_varid (ncid,'qu', fldsrc_quid)
        if (ierr .ne. nf_noerr) then
          MPI_master_only write(*,1) 'qu', 
     &                           fldsrc_file(1:lstr)!, nf_strerror(ierr)
     &            ,' => Initialized to 0.' 
        endif
        ierr=nf_inq_varid (ncid,'qv', fldsrc_qvid)
        if (ierr .ne. nf_noerr) then
          MPI_master_only write(*,1) 'qv', 
     &                           fldsrc_file(1:lstr)!, nf_strerror(ierr)
     &            ,' => Initialized to 0.' 
        endif
        ierr=nf_inq_varid (ncid,'qubar', fldsrc_qubarid)
        if (ierr .ne. nf_noerr) then
          MPI_master_only write(*,1) 'qubar', 
     &                           fldsrc_file(1:lstr)!, nf_strerror(ierr)
     &            ,' => Initialized to 0.' 
        endif
        ierr=nf_inq_varid (ncid,'qvbar', fldsrc_qvbarid)
        if (ierr .ne. nf_noerr) then
          MPI_master_only write(*,1) 'qvbar', 
     &                           fldsrc_file(1:lstr)!, nf_strerror(ierr)
     &            ,' => Initialized to 0.' 
        endif
        fldsrc_file_id=ifldsrc
        ierr=nf_inq_varid (ncid, 'fldsrc_time', fldsrc_tid)
        if (ierr .eq. nf_noerr) then
          call set_cycle (ncid, fldsrc_tid, ntfldsrc, fldsrc_cycle,
     &                                 fldsrc_ncycle, fldsrc_rec, ierr)
          fldsrc_time=-1.E+20
        else
          write(*,3) 'fldsrc_time', fldsrc_file(1:lstr),
     &                                   nf_strerror(ierr)
          goto 99
        endif
      endif
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (fldsrc_time.lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (fldsrc_cycle, ntfldsrc, fldsrc_ncycle,
     &                                 fldsrc_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, fldsrc_tid, fldsrc_rec, cff)
          if (ierr .eq. nf_noerr) then
            fldsrc_time=cff*day2sec + fldsrc_cycle*fldsrc_ncycle
            ierr=nf_fread (Qu_fldsrc(START_2D_ARRAY,1), ncid, fldsrc_quid,
     &                                         fldsrc_rec, u3dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &        '(6x,2A,F12.4,2(1x,A,I4))') 'get_fldsource    ',
     &                '-- read field sources                      for time =',
     &                   cff, 'rec =', fldsrc_rec MYID
            else
              write(*,2) 'qu',   fldsrc_rec,
     &               fldsrc_file(1:lstr), nf_strerror(ierr)
              goto 99                                         !--> ERROR
            endif
            ierr=nf_fread (Qubar_fldsrc(START_2D_ARRAY), ncid, fldsrc_qubarid,
     &                                         fldsrc_rec, u2dvar)
            if (ierr .ne. nf_noerr) then
              write(*,2) 'qubar',   fldsrc_rec,
     &               fldsrc_file(1:lstr), nf_strerror(ierr)
              goto 99                                         !--> ERROR
            endif
            ierr=nf_fread (Qv_fldsrc(START_2D_ARRAY,1), ncid, fldsrc_qvid,
     &                                         fldsrc_rec, v3dvar)
            if (ierr .ne. nf_noerr) then
              write(*,2) 'qv',   fldsrc_rec,
     &               fldsrc_file(1:lstr), nf_strerror(ierr)
              goto 99                                         !--> ERROR
            endif
            ierr=nf_fread (Qvbar_fldsrc(START_2D_ARRAY), ncid, fldsrc_qvbarid,
     &                                         fldsrc_rec, v2dvar)
            if (ierr .ne. nf_noerr) then
              write(*,2) 'qvbar',   fldsrc_rec,
     &               fldsrc_file(1:lstr), nf_strerror(ierr)
              goto 99                                         !--> ERROR
            endif
            do itrc=1,NT
              lvar=lenstr(vname(1,indxT+itrc-1))
              ierr=nf_fread (T_fldsrc(START_2D_ARRAY,1,itrc), ncid, fldsrc_id(itrc),
     &                                         fldsrc_rec, r3dvar)
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(*,1) vname(1,indxT+itrc-1)(1:lvar), 
     &                           fldsrc_file(1:lstr)!, nf_strerror(ierr)
     &            ,' => Kept constant (potentially 0)' 
              endif
            enddo
          else
            write(stdout,2) 'fldsrc_time', fldsrc_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_fldsource: requested time record ',      fldsrc_rec,
     &   'exeeds the last record', ntfldsrc, 'available in forcing ',
     &   'netCDF file ''',   fldsrc_file(1:lstr),   '''',  'tdays = ',
     &    tdays,        '  but the last available  fldsrc_time =',
     &                                    fldsrc_time*sec2day
        endif
      enddo
  1   format(/1x,'### NOTE: get_fldsource :: netCDF variable ''',
     &               A,'''' , ' not found in file ''', A, ''''1x,A)
  2   format(/1x,'### ERROR in get_fldsource :: Cannot read variable ''',
     &             A,'''' /12x, 'from netCDF file ''', A, ''''/12x,A)
  3   format(/1x,'### ERROR in get_fldsource :: Cannot find variable ''',
     &             A,'''' /12x, 'in netCDF file ''', A, ''''/12x,A)
      return
  99  may_day_flag=2
      return
      end
#else
      subroutine get_fldsource_empty
      end
#endif /* FLDSOURCE */
