#include "cppdefs.h"
#if defined SOLVE3D && defined TSOURCE
 
      subroutine get_tsource (ierr)
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
      integer ierr, ierr_all, itsrc, lstr, lenstr, ncdf_read, varid, ncid, itrc, lvar
      character units*64
      logical tsrc_set

!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether tsrc data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "tsrc_cycle", set initial cycling index "tsrc_ncycle", record index
! "tsrc_rec", and time to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      itsrc=max(tsrc_file_id,1)
 100  ncid=ncidtsrc(itsrc)
      lstr=lenstr(tsrc_file)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        if (ncid .eq. -1) then
          ierr=nf_open (tsrc_file(1:lstr), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidtsrc(itsrc)=ncid
          else
            MPI_master_only write(*,'(/1x,4A/)')
     &           '### ERROR in get_tsource: can ',
     &           'not open netCDF file ''', tsrc_file(1:lstr), '''.'
          endif
        endif
        if (ierr.eq.nf_noerr) then
          ierr_all=0 
          ierr=nf_inq_varid (ncid, 'tsrc_time',  tsrc_tid)
          if (ierr.ne.nf_noerr) then
            MPI_master_only write(*,1) 'tsrc_time'
          endif
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (ncid, 'volflux', tsrc_id(0))
          if (ierr.ne.nf_noerr) then
            MPI_master_only write(*,1) 'volflux'
          endif
          ierr_all=ierr_all+ierr
          do itrc = 1, NT
             lvar=lenstr(vname(1,indxT+itrc-1))
             ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &            tsrc_id(itrc))
             if (ierr.ne.nf_noerr) then
                MPI_master_only write(*,11) vname(1,indxT+itrc-1)(1:lvar)
                tsrc_id(itrc) = 0
             endif
          enddo
          ierr=ierr_all
          if (ierr.eq.nf_noerr) then
            call set_cycle (ncid, tsrc_tid, nttsrc,
     &           tsrc_cycle, tsrc_ncycle, tsrc_rec, ierr)
            itsrc=1
            tsrc_time=-1.E+20
          else
            MPI_master_only write(*,'(8x,4A)') '### ERROR(s) occur while ',
     &                   'examining content of netCDF file ''',
     &                    tsrc_file(1:lstr), '''.'
          endif
        endif
      endif
  1   format(/1x,'### ERROR in get_tsource: cannot find variable ''',A,'''')
  11   format(/1x,'   NOTE in get_tsource: cannot find variable ''',A,
     &             ''' => Initialized to 0.')
!
! Read data from the file:  Check if model time is bounded by past
!----- ---- ---- --- -----  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (tsrc_time.lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (tsrc_cycle,nttsrc,tsrc_ncycle,tsrc_rec,ierr)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, tsrc_tid, tsrc_rec, cff)
          if (ierr.eq.nf_noerr) then
            tsrc_time=cff*day2sec + tsrc_cycle*tsrc_ncycle
            ierr_all=0
            ierr=ncdf_read(ncid, tsrc_id(0), tsrc_rec, r_var,
     &                    T_src(START_2D_ARRAY,1,0), N)
            if (ierr.ne.nf_noerr)  then
              MPI_master_only write(*,2)
     &                       vname(1,indxT+itrc-1)(1:lvar)
              ierr_all=ierr_all+ierr
            end if
            do itrc=1, NT
               tsrc_set = .false.
               lvar=lenstr(vname(1,indxT+itrc-1))
               if (tsrc_id(itrc) .gt. 0) then
                  ierr=ncdf_read(ncid, tsrc_id(itrc), tsrc_rec, r_var,
     &                    T_src(START_2D_ARRAY,1,itrc), N)
                  if (ierr.ne.nf_noerr)  then
                     if (itrc .le. 1 + ntrc_salt) then
                        MPI_master_only write(*,2)
     &                       vname(1,indxT+itrc-1)(1:lvar)
                        ierr_all=ierr_all+ierr
                     end if
                  else
                     tsrc_set = .true.
                  endif
               endif
               if (.not. tsrc_set .and. itrc .gt. 1 + ntrc_salt) then
                  ierr = 0
                  MPI_master_only write(*,3)
     &                 vname(1,indxT+itrc-1)(1:lvar)
                  T_src(:,:,:,itrc) = 0.0
               endif
            enddo
            ierr=ierr_all
            if (ierr.eq.0) then
              MPI_master_only write(*,'(6x,A,2x,A,F12.4,2(1x,A,I4))')
     &            'get_tsource -- read tracer sources for all fields',
     &            'tsrc_time =', cff, 'rec =', tsrc_rec MYID
            else
              MPI_master_only write(*,'(1x,2A,I4/8x,3A)')
     &           '### ERROR(s) occur while trying to read record ',
     &             tsrc_rec,  'in file ''',tsrc_file(1:lstr),'''.'
            endif 
          else
            MPI_master_only write(*,2) 'tsrc_time'
          endif
        else
           MPI_master_only write(*,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &    'ERROR in get_tsource: requested time record ',  tsrc_rec,
     &    'exeeds the last record', nttsrc,  'available in netCDF ',
     &    'file ''',   tsrc_file(1:lstr),  '''',  'tdays = ', tdays,
     &    '  but the last available tsrc_time =',
     &                                   tsrc_time*sec2day
        endif
      enddo
  2   format(/1x,'### ERROR in get_tsource: Cannot read variable ''',
     &                                                     A, '''.')
  3   format('   NOTE in get_tsource: cannot read variable ''',
     &                           A,''' => set to 0.')
      return
  99  may_day_flag=2
      return
      end
#else
      subroutine get_tsource_empty
      end
#endif /* TSOURCE */

