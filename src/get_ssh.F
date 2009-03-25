#include "cppdefs.h"
#if defined M2NUDGING && !defined M2_FRC_BRY && !defined ANA_SSH


                                         ! Read sea surface height
      subroutine get_ssh (ierr)          ! at appropriate time from
      implicit none                      ! climatological file.
# define SSH_DATA
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "netcdf.inc"
# include "climat.h"
      real cff
      integer i, lstr, lsvar, lenstr, ierr, nf_fread
!
! Initialization: Check, whether climatology netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables. Determine whether there is cycling
! to reuse the input data and find cycling period "ssh_cycle", set
! initial cycling index "ssh_ncycle" and record index "ssh_rec".
! Set initial value for time index "itssh" and both time record
! bounds to large negative artificial values to trigger logic in
! reading part below.
!
      ierr=nf_noerr
      if (iic.eq.ntstart) then
        lstr=lenstr(clm_file)
        if (ncidclm.eq.-1) then
          ierr=nf_open(clm_file(1:lstr), nf_nowrite, ncidclm)
          if (ierr.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
     &                   'in get_ssh: can not open netCDF file ''',
     &                                       clm_file(1:lstr), '''.'
        endif
        if (ierr.eq.nf_noerr) then
          ierr=nf_inq_varid (ncidclm, 'ssh_time', ssh_tid)
          if (ierr.eq.nf_noerr) then
            ierr=nf_inq_varid (ncidclm, 'zeta', ssh_id)
            if (ierr .ne. nf_noerr) then
              ierr=nf_inq_varid (ncidclm, 'SSH', ssh_id)
            endif
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncidclm, ssh_tid, ntssh, ssh_cycle,
     &                                 ssh_ncycle, ssh_rec, ierr)
              itssh=1
              ssh_time(1)=-1.E+20
              ssh_time(2)=-1.E+20
            else
              write(*,'(1x,5A/12x,A/)')     '### ERROR: get_ssh :: ',
     &          'Cannot get netCDF ID for variable "zeta" or "SSH" ',
     &          'from ''', clm_file(1:lstr),'''.', nf_strerror(ierr)
            endif
          else
            write(*,'(1x,5A/12x,A/)')       '### ERROR: get_ssh :: ',
     &        'Cannot get netCDF ID for variable "ssh_time" from ''',
     &                    clm_file(1:lstr), '''.', nf_strerror(ierr)
          endif
        endif
      endif
!
! Read data from the file: Check if model time is bounded by past
!===== ==== ==== === ===== and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (ssh_time(itssh).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (ssh_cycle, ntssh, ssh_ncycle,
     &                                     ssh_rec, ierr)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE(ncidclm, ssh_tid, ssh_rec, cff)
          if (ierr.eq.nf_noerr) then
            itssh=min(3-itssh,ntssh)
            ssh_time(itssh)=cff*day2sec+ssh_cycle*ssh_ncycle
            ierr=nf_fread(sshg(START_2D_ARRAY,itssh), ncidclm,
     &                                ssh_id, ssh_rec, r2dvar)
            if (ierr.eq.nf_noerr) then
              write(stdout,'(6x,A,4x,A,23x,A,F12.4,2(1x,A,I4))')
     &             'get_ssh', '-- read SSH', 'ssh_time =', cff,
     &             'rec =', ssh_rec MYID
              if (ntssh.eq.1) return
            else
              write(stdout,2) 'SSH', ssh_rec
            endif
          else
            write(stdout,2) 'ssh_time', ssh_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_ssh: requested time record ',      ssh_rec,
     &   'exeeds the last record',  ntssh, 'available in netCDF ',
     &   'file ''',  clm_file(1:lstr),  '''',   'tdays = ',  tdays,
     &   '  but the last available ssh_time =',
     &                                    ssh_time(itssh)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_ssh: cannot read variable ''',
     &                              A, ''' for record ', I4/)
      return
      end
 
 
      subroutine set_ssh_tile (istr,iend,jstr,jend, ierr)
!
! Set-up sea surface height climatology for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff1, cff2, cff
# define SSH_DATA
# include "param.h"
# include "climat.h"
# include "scalars.h"
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
      it1=3-itssh
      it2=itssh
      cff1=ssh_time(it2)-time
      cff2=time-ssh_time(it1)
      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntssh.eq.1) then                   ! Load time-invariant
        if (iic.eq.ntstart) then             ! sea-surface height.
          do j=jstrR,jendR
            do i=istrR,iendR
              ssh(i,j)=sshg(i,j,itssh)
            enddo
          enddo
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then        ! Interpolate
        cff=1./(cff1+cff2)                             ! sea-surface
        cff1=cff1*cff                                  ! height in
        cff2=cff2*cff                                  ! in time.
        do j=jstrR,jendR
          do i=istrR,iendR
            ssh(i,j)=cff1*sshg(i,j,it1)+cff2*sshg(i,j,it2)
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &      'SET_SSH_TILE - current model time is outside bounds of',
     &      '''ssh_time''.',   'SSH_TSTART=',  ssh_time(it2)*sec2day,
     &                         'TDAYS=',       tdays,
     &                         'SSH_TEND=',    ssh_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else
      subroutine get_ssh_empty
      end
#endif
 
 
