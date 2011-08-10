#include "cppdefs.h"
 
      subroutine set_cycle (ncid, varid, ntime, cycle_length,
     &                                   icycle, irec,  ierr)
!
! Determine how many time records "ntime" are available for variable
! with netCDF ID "varid", check whether there is time periodicity
! (cycling) for this variable and set initial cycling index "icycle"
! and time record index "irec" depending on the current model in such
! a way that subsequent reading of two records of time variable
! results in bounding of current model time according to
!
!               X_time(irec-1) =< time < X_time(irec)
!
! where, in the case of cycling X_time my be incremented by
! icycle*cycle_length relatively to data from the file.
!
! input:  ncid          netCDF file ID
!         varid         netCDF ID for field time variable
!
! output: ntime         size of time dimension.
!         cycle_length  length of field time cycle [seconds].
!         icycle        cycling index (integer)
!         irec          time record index (integer)
!
      implicit none
      real cycle_length,  tstart, tend
      integer ncid, varid, ntime, icycle, irec, ierr,  itry, nvdims,
     &  vdims(8), vartype, nvatts, size, i, lvar, ldim, latt, lenstr
      logical found
      character*16 varname, dimname, attname, testname
 
#include "param.h"
#include "scalars.h"
#include "netcdf.inc"
!                       ! Default settings: if not discovered
      cycle_length=0.   ! otherwise, it is assumed that file
      icycle=0          ! contains only one record for requested
      irec=1            ! variable, and no temporal interpolation
      ntime=1           ! needed/can be to be further performed.
!
! Make general inquiry into about variable with netCDF ID "varid"
! and find whether one (and only one) of its dimension names has
! suffix "_time" (i.e. temporal record dimension). If found,
! retrieve its size "ntime".
!
      ierr=nf_inq_var (ncid, varid, varname, vartype,
     &                       nvdims,  vdims,  nvatts)
      lvar=lenstr(varname)
      found=.false.
      i=0
      do while (i.lt.nvdims .and. ierr.eq.nf_noerr)
        i=i+1
        ierr=nf_inq_dim (ncid, vdims(i), dimname, size)
        if (ierr.eq.nf_noerr) then
          ldim=lenstr(dimname)
          if (dimname(ldim-4:ldim) .eq. '_time') then
            if (.not.found) then
              found=.true.
              ntime=size
            else
              write(stdout,'(/1x,4A/18x,A)')         '### ERROR: ',
     &               'set_cycle :: variable ''',  varname(1:lvar),
     &               ''' has', 'more than one ''_time'' dimension.'
              ierr=-9999
            endif
          endif
        else
          write(stdout,'(/1x,4A/10x,A/)')  '### ERROR: set_cycle ',
     &              ':: Cannot inquire dimensions for variable ''',
     &               varname(1:lvar),   '''.',   nf_strerror(ierr) 
        endif
      enddo
!
! Check, if there is more than one record available in netCDF file:
! In the case of multiple records, check whether variable has cycling
! attribute, if so, retrieve cycling period (leaving it set to zero
! means that no recycling will be performed).
!
      if (ierr.eq.nf_noerr) then
        if (ntime.gt.1) then
          i=0
          do while (i.lt.nvatts .and. ierr.eq.nf_noerr)
            i=i+1
            ierr=nf_inq_attname (ncid, varid, i, attname)
            if (ierr.eq.nf_noerr) then
              latt=lenstr(attname)
              if (attname(1:latt) .eq. 'cycle_length') then
                ierr=nf_get_att_FTYPE (ncid, varid, attname(1:latt),
     &                                                 cycle_length)
                if (ierr.eq.nf_noerr) then
                  cycle_length=cycle_length*day2sec
                else
                  write(stdout,'(/1x,4A/)') 'SET_CYCLE ERROR while ',
     &                 'reading attribute ''', attname(1:latt), '''.'
                endif
              endif
            else
              write(stdout,'(/1x,A,1x,3A/)') 'SET_CYCLE ERROR while',
     &          'inquiring attributes for ''', varname(1:lvar), '''.'
            endif
          enddo
!
! Search for starting time record field and time-index. First look
! at the first record and, in the case of cycling, compare its value
! "tstart" with current model time to set initial cycling index
! "icycle" in such a way, that current model time is held within the
! bouds:
!
!     tstart+icycle*cycle_length ; tstart+(icycle+1)*cycle_length
!
! Once it is set, increment "irec" forward in time (in a manner
! similar to the actual time stepping done by get_ routines), to
! find two consecutive time records which bound corrent model time,
!
!           X_time(irec-1) <= time < X_time(irec)
!
! (hence "irec" corresponds to future time). NOTE: in the case when
! input netCDF file has corrupt or missing data, it is still possible
! that nf_get_var1_FTYPE returns non-error status and bad values for
! tstart, tend, and cycle_length (if any).  This is known to put the 
! search algorithm into indefinite spin, even is situations when,
! cycle length is not specified, i.e., data is not designed to be
! cycled [the exact cause of it is due to the fact that missing
! value in netCDF files is interpreted stil as a number, which
! results in tstart,tend be the same and equal to whatever missing
! value flag is set to.  Consequently model time is always outside
! time the window and search goes on indefinitely. To prevent this
! from happening, an alternative termination algoritm counts the
! number of search iterations ("itry") and  terminates the search
! after scanning all of the records avalable in the file.  
!
          if (ierr.eq.nf_noerr) then
            found=.false.
            ierr=nf_get_var1_FTYPE (ncid, varid, irec, tstart)
            if (ierr.eq.nf_noerr) then
              tstart=tstart*day2sec
              if (cycle_length.gt.0.) then
                icycle=int((time-tstart)/cycle_length)
                if (time.lt.tstart) icycle=icycle-1
                tstart=tstart + icycle*cycle_length
              endif
              itry=0
              do while (.not.found .and. ierr.eq.nf_noerr)
                itry=itry+1
                call advance_cycle (cycle_length, ntime, icycle,
     &                                              irec, ierr)
                  if (ierr.eq.nf_noerr) then
                  ierr=nf_get_var1_FTYPE (ncid, varid, irec, tend)
                  if (ierr.eq.nf_noerr) then

c           write(*,*) irec, ntime, icycle, cycle_length, tend 
                     
                    tend=tend*day2sec + icycle*cycle_length
                    if (tstart.le.time .and. time.lt.tend) then
                      found=.true.
                    elseif (itry.gt.ntime) then

         write(stdout,97) '### ERROR: set_cycle :: Cannot find ',
     &  'appropriate record after scanning all records ',
     &  'available:  icycle =', icycle,   'cycle_length =',
     &   cycle_length, ' tstart =', tstart,  'tend =', tend,
     &  'Possible cause: Check integrity of netCDF input ',
     &  'file for missing/corrupt data.' 
  97     format(/1x,2A/12x,A,I16,3x,A,E16.9/7x,2(4x,A,E16.9)/12x,2A)

                      ierr=1000 
                    else
                      tstart=tend  !--> continue search
                    endif
                  else
                    write(stdout,1) varname(1:lvar), irec, ierr
                  endif
                endif
              enddo
!
! Since no actual data has been read so far, retard "irec" twice,
! so that calling get_ routine will retrieve two time levels of data.
!
              if (ierr.eq.nf_noerr) then
                do i=1,2
                  irec=irec-1
                  if (irec.lt.1 .and. cycle_length.gt.0) then
                    irec=ntime
                    icycle=icycle-1
                  elseif (irec.lt.0) then
                    write(*,'(/1x,2A/)') '### ERROR: set_cycle :: ',
     &            'run out of time records in non-recycling regime.'
                    ierr=-9999
                  endif
                enddo
              endif
            endif
          endif
        else
          write(stdout,1) varname(1:lvar), irec, ierr
        endif
      else
        write(stdout,'(/1x,2A,I4/18x,A,I4/)') 'SET_CYCLE ERROR: ',
     &     'Cannot make general inquiry about variable with ID =',
     &      varid, 'in input file; netCDF error code =', ierr
      endif
  1   format(/1x,'### ERROR: set_cycle :: cannot read variable ''',
     &    A, ''' for record ', I4/10x, 'netCDF error code =', I5/)

      if (ierr .ne. nf_noerr) then
        i=nf_inq_varname (ncid, varid, testname)
        if (i .eq. nf_noerr) then
          lvar=lenstr(testname)
          write(*,'(12x,A,I4,2x,3A,2z,A,1PE16.7/)')   'varid =',
     &      varid, 'variable in question: ''', testname(1:lvar),
     &      '''.', 'cycle_length=',  cycle_length

        else

        endif
      endif
      return
      end
 
 
      subroutine advance_cycle (cycle_length, ntime,
     &                           icycle, irec, ierr)
      implicit none
      real cycle_length
      integer ntime, icycle, irec, ierr
      irec=irec+1
      if (irec.gt.ntime) then
        if (cycle_length.gt.0.) then
          irec=1
          icycle=icycle+1
        else
          write(*,'(/1x,2A/)') '### ERROR: advance_cycle :: run ',
     &              'out of time records in non-recycling regime.'
          ierr=-9999
        endif
      endif
      return
      end
 
 
