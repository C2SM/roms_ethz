#include "cppdefs.h"
c--#define VERBOSE

      subroutine set_cycle (ncid, varid, ntime, cycle_length,
     &                                   icycle, irec,  ierr)

! Purpose: Set the initial time record index "irec" and, if applicable,
! cycling index "icycle", in such a way that the subsequent reading of
! two consecutive records of timing variable (identified by netCDF ID
! "varid") results in bounding the current model time as
!
!             X_time(irec-1)  <=  time  <  X_time(irec)
!
! where, in the case of cycling X_time my be incremented by
! icycle*cycle_length relatively to data from the file.
!
! This routine also automatically detects and assigns values to the
! associated variables "ntime" and "cycle_length".
!
!
! Arguments:
!-------------
! input:  ncid          netCDF file ID
!         varid         netCDF ID for field time variable
!
! output: ntime         size of time dimension
!         cycle_length  length of field time cycle [seconds]
!         icycle        cycling index (integer)
!         irec          time record index (integer)
!         ierr          error status, normally nf_noerr

      implicit none
      real cycle_length,  tstart, tend
      integer ncid, varid, ntime, icycle, irec, ierr, nvdims, nvatts,
     &        vartype,  size, vdims(8), i, lvar, ldim, latt, lenstr
      logical found
      character(len=16) varname, dimname, attname

#include "param.h"
#include "scalars.h"
#include "netcdf.inc"

      ntime=1         ! Initialize: if not found later otherwise,
      cycle_length=0. ! it is assumed that the file contains only one
      icycle=0        ! record for the requested variable, and no
      irec=1          ! temporal interpolation needed/can be performed.

! Make general inquiry into about variable with netCDF ID "varid" and
! find whether there is a single dimension dimension which name has the
! suffix "_time" (i.e. temporal record dimension). If found, retrieve
! its size and save it as "ntime".

      ierr=nf_inq_var (ncid, varid, varname, vartype,
     &                       nvdims,  vdims,  nvatts)
      if (ierr==nf_noerr) then
        lvar=lenstr(varname)
        found=.false.
        i=0
        do while (i<nvdims .and. ierr==nf_noerr)
          i=i+1
          ierr=nf_inq_dim (ncid, vdims(i), dimname, size)
          if (ierr==nf_noerr) then
            ldim=lenstr(dimname)
            if (dimname(ldim-4:ldim) == '_time') then
              if (.not.found) then
                found=.true.
                ntime=size
              else
                write(*,'(/1x,5A)')         '### ERROR: set_cycle :: ',
     &                  'variable ''', varname(1:lvar), ''' has more ',
     &                                 'than one ''_time'' dimension.'
                ierr=-9999
              endif
            endif
          else
            write(*,'(/1x,4A/12x,A/)')      '### ERROR: set_cycle :: ',
     &                     'Cannot inquire dimensions for variable ''',
     &                      varname(1:lvar), ''':', nf_strerror(ierr)
          endif
        enddo

! If there is more than one record available check whether the variable
! has cycling attribute, and if so, retrieve cycling period (leaving it
! set to zero means that no recycling will be performed).

        if (ntime>1 .and. ierr==nf_noerr) then
          i=0
          do while (i<nvatts .and. ierr==nf_noerr)
            i=i+1
            ierr=nf_inq_attname (ncid, varid, i, attname)
            if (ierr==nf_noerr) then
              latt=lenstr(attname)
              if (attname(1:latt) == 'cycle_length') then
                ierr=nf_get_att_FTYPE (ncid, varid, attname(1:latt),
     &                                                 cycle_length)
                if (ierr /= nf_noerr) then
                  write(*,'(/1x,4A/12x,A)') '### ERROR: set_cycle :: ',
     &                     'Cannot read attribute ''', attname(1:latt),
     &                                       '''.',  nf_strerror(ierr)
                endif
              endif   !<-- cycle_length  is/is not present
            else
              write(*,'(/1x,2A,I3,1x,2A/12x,A)')        '### ERROR: ',
     &           'set_cycle :: Cannot inquire name of attribute #', i,
     &           'for ''', varname(1:lvar), '''.',  nf_strerror(ierr)
            endif
          enddo

! If cycle_length is found, scan all records available within in the
! file and check whether all of them lie within the declared cycling
! interval, i.e.,
!
!     time(rec=1)  <  time(rec)  <=  time(rec=1)+cycle_length
!          for all   1  <  rec  <=  ntime
!
! If not restrict the last record to be read "ntime" to those which
! are within (this means that all the remaining records in the file
! will be ignored).

          if (cycle_length>0.) then
             i=0
             do while (i<ntime)
               i=i+1
               ierr=nf_get_var1_FTYPE (ncid, varid, i, tend)
               if (ierr==nf_noerr) then
                 if (i==1) then
                   tstart=tend           !<-- initial time
                 elseif (i==2) then
                   tstart=2.*tstart-tend !<-- retard by 1/2-step
                 elseif (tend > tstart+cycle_length) then
                    write(*,'(8x,A,2(A,I4,1x),A)')   'WARNING: ',
     &               'set_cycle :: Adjusted "ntime" from ', ntime,
     &               'to', i-1,  'restricted by "cycle_length".'
                      ntime=i-1
                 endif
               else
                 write(*,1) varname(1:lvar), i, nf_strerror(ierr)
               endif
             enddo

! Convert cycle_length from days to seconds.

             cycle_length=cycle_length*day2sec
          endif
        endif   !<-- ntime>1

#ifdef VERBOSE
        write(*,'(1x,3A,1x,A,I5,2x,A,ES16.9)')         'set_cycle: ',
     &          'varname = ''', varname(1:lvar), ''' ntime =', ntime,
     &                      'cycle_length =', cycle_length*sec2day
#endif
      else
        write(*,'(/1x,2A,I4,1x,A/12x,A/)')   '### ERROR: set_cycle ',
     &     ':: Cannot make general inquiry about variable with ID =',
     &             varid, 'in input netCDF file.', nf_strerror(ierr)
      endif

! Search for starting time record field and time-index. First look
! at the first record and, in the case of cycling, compare its value
! "tstart" with current model time to set initial cycling index
! "icycle" in such a way, that current model time is held within the
! bounds:
!
!     tstart+icycle*cycle_length ; tstart+(icycle+1)*cycle_length
!
! Once it is set, increment "irec" forward in time (in a manner similar
! to the actual time stepping done by get_ routines), to find two
! consecutive time records which bound current model time,
!
!           X_time(irec-1) <= time < X_time(irec)
!
! (hence "irec" corresponds to future time). WARNING: in the case when
! input netCDF file has corrupt or missing data, it is still possible
! that nf_get_var1_FTYPE returns non-error status and bad values for
! tstart, tend, and cycle_length (if any).  This is known to put the
! search algorithm into indefinite spin, even is situations when, cycle
! length is not specified, i.e., data is not designed to be cycled
! [the exact cause of it is due to the fact that missing value in
! netCDF files is interpreted still as a number, which results in
! tstart,tend be the same and equal to whatever missing value flag is
! set to.  Consequently model time is always outside time the window
! and search goes on indefinitely. To prevent this from happening, an
! alternative termination algorithm counts the number of search
! iterations and  terminates the search after scanning all of the
! records available in the file.

      if (ntime>1 .and. ierr==nf_noerr) then
        ierr=nf_get_var1_FTYPE (ncid, varid, irec, tstart)
        if (ierr==nf_noerr) then
          tstart=tstart*day2sec
          if (cycle_length > 0.) then
            icycle=int((time-tstart)/cycle_length)
            if (time<tstart) icycle=icycle-1
            tstart=tstart + icycle*cycle_length
          endif

          found=.false.
          i=0
          do while (.not.found .and. ierr==nf_noerr)
            i=i+1
            call advance_cycle (cycle_length,ntime, icycle,irec, ierr)
            if (ierr==nf_noerr) then
              ierr=nf_get_var1_FTYPE (ncid, varid, irec, tend)
              if (ierr==nf_noerr) then
                tend=tend*day2sec + icycle*cycle_length
                if (tstart<=time .and. time<tend) then
                  found=.true.
                elseif (i>ntime) then

        write(*,2)  '### ERROR: set_cycle :: Cannot find appropriate ',
     & 'time record after scanning all', 'available records: icycle =',
     &  icycle,   'cycle_length =', cycle_length,   'tstart =', tstart,
     & 'tend =', tend,    'Check integrity of netCDF input file for ',
     &                                     'missing or corrupt data.'
                  ierr=-9999
                else
                  tstart=tend  !--> continue search
                endif
              else
                write(*,1) varname(1:lvar), irec, nf_strerror(ierr)
              endif
            endif                     ! Because no actual data has been
          enddo                       ! read thus far, backstep "irec"
                                      ! twice, so the subsequient call
          if (ierr == nf_noerr) then  ! to "get_" routine will retrieve
            do i=1,2                  ! two time levels of data.
              irec=irec-1
              if (irec<1 .and. cycle_length>0) then
                irec=ntime
                icycle=icycle-1
              elseif (irec<0) then
                write(*,'(/1x,2A/)')      '### ERROR: set_cycle :: ',
     &            'run out of time records in non-recycling regime.'
                ierr=-9999
              endif
            enddo
          endif
        endif
      endif

  1   format(/1x,'### ERROR: set_cycle :: Cannot read variable ''',
     &                               A, ''' for record ', I4/12x,A/)
  2   format(/1x,2A/12x,A,I8,2x,A,ES16.10/8x,2(4x,A,ES16.10)/12x,2A)
      end


      subroutine advance_cycle (cycle_length, ntime, icycle,irec, ierr)
      implicit none
      real cycle_length
      integer ntime, icycle, irec, ierr
      irec=irec+1
      if (irec>ntime) then
        if (cycle_length>0.) then
          irec=1
          icycle=icycle+1
        else
          write(*,'(/1x,2A/)') '### ERROR: advance_cycle :: run ',
     &              'out of time records in non-recycling regime.'
          ierr=-9999
        endif
      endif
      end
