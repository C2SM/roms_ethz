#include "cppdefs.h"
#if defined SOLVE3D && defined QCORRECTION && !defined ANA_SST
 
      subroutine get_dqdt (ierr)
      implicit none
# define SST_DATA
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
      real cff
      integer ncid,ifrc, ierr, lfrc,lvar,lenstr, nf_fread
      character*(max_name_size) frcname
!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether dqdt data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "dqdt_cycle", set initial cycling index "dqdt_ncycle", record index
! "dqdt_rec", and time index "itdqdt" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      ifrc=max(dqdt_file_id,1)
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
            write(stdout,'(/1x,4A/)') '### ERROR in get_dqdt: can ',
     &             'not open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(vname(1,indxdQdSST))
          ierr=nf_inq_varid (ncid,vname(1, indxdQdSST), dqdt_id)
          if (ierr .eq. nf_noerr) then
            dqdt_file_id=ifrc
            write(*,*) 'Found ''', vname(1,indxdQdSST)(1:lvar),
     &         ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc

!
! WARNING: Here dQdSST is presumed to have its own time variable,
! or, if not, found it is slaved to one of the existing fields, such
! as "sss" or "sst", in this priority. This policy, however is unsafe
! because it does not rule out possibility of wrong clocking of
! dQdSST, if SST does from dayly input, while dQdSST is computed
! from climatology and placed into the same file.   
!
            ierr=nf_inq_varid (ncid, 'dqdt_time', dqdt_tid)
            if (ierr .ne. nf_noerr) then
              ierr=nf_inq_varid (ncid, 'sss_time', dqdt_tid)
              if (ierr .eq. nf_noerr) then
                write(*,*) 'WARNING: ', vname(1,indxdQdSST)(1:lvar),
     &                            ' timing is slaved to SSS timing.' 
              else
                ierr=nf_inq_varid (ncid, 'sst_time', dqdt_tid)
                if (ierr .eq. nf_noerr) then
                  write(*,*) 'WARNING: ',vname(1,indxdQdSST)(1:lvar),
     &                             ' timing is slaved to SST timing.'
                endif
              endif
            endif
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, dqdt_tid, ntdqdt,dqdt_cycle,
     &                              dqdt_ncycle, dqdt_rec, ierr)
              itdqdt=1
              dqdt_time(1)=-1.E+20
              dqdt_time(2)=-1.E+20
            else
              write(stdout,1) 'dqdt_time', frcname(1:lfrc)
            endif
          else
            write(stdout,1) vname(1,indxdQdSST)(1:lvar),
     &                                   frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_dqdt cannot find variable ''',
     &     A, ''''/7x, 'in forcing netCDF file ''', A, '''.')
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cycling indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (dqdt_time(itdqdt).lt.time+0.5*dt .and.
     &                                   ierr.eq.nf_noerr)
        call advance_cycle (dqdt_cycle, ntdqdt, dqdt_ncycle,
     &                                 dqdt_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, dqdt_tid, dqdt_rec, cff)
          if (ierr .eq. nf_noerr) then
            itdqdt=3-itdqdt
            dqdt_time(itdqdt)=cff*day2sec + dqdt_cycle*dqdt_ncycle
            ierr=nf_fread (dqdtg(START_2D_ARRAY,itdqdt), ncid,
     &                                 dqdt_id, dqdt_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              write(stdout,'(6x,2A,F12.4,2(1x,A,I4))') 'get_dqdt   ',
     &                '-- read dQdT                      for time =',
     &                 cff, 'rec =', dqdt_rec MYID
            else
              write(stdout,2) 'dQdT', dqdt_rec
            endif
          else
            write(stdout,2) 'dqdt_time', dqdt_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_dqdt: requested time record ',     dqdt_rec,
     &   'exeeds the last record', ntdqdt, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lfrc),   '''',   'tdays = ',
     &    tdays,          '  but the last available  dqdt_time =',
     &                                 dqdt_time(itdqdt)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_dqdt: cannot read variable ''', A,
     &                                    ''' for record ', I4/)
      return
      end
#else
      subroutine get_dqdt_empty
      end
#endif /* QCORRECTION && !ANA_SST */
 
