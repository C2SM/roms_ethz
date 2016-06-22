#include "cppdefs.h"
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
 
      subroutine get_agedye (ierr)
!
!  Read in data related to restoring and flux bc of age/dye tracers
!  from NetCDF files.
!
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
# include "sources.h"
      real cff
      integer is, nsrc, ierr, lstr, lvar, lenstr, ncdf_read
      character*80 fname
      character*3 tnum
!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables.
!
! After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "ad_cycle", set initial cycling index "ad_ncycle", record index
! "ad_rec", and time index "ad_time" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        do is=1,N_agedye
           ! Go to next age/dye tracer if this is an age tracer:
           if (Age(is)) cycle
           ! construct forcing file name for this tracer:
           write(tnum,'(i3.3)') is
           fname='ad_tracer_'/ /trim(tnum)/ /'.nc'
           lstr=lenstr(fname)
           ! If not opened yet, open NetCDF file related to a/d tracer is for reading.
           ! Find and save IDs for relevant variables.
           if (ncid_ad(is).eq.-1) then
              ierr=nf_open(fname(1:lstr), nf_nowrite, ncid_ad(is))
              if (ierr.ne.nf_noerr) then
                 write(*,'(/1x,4A/)') 
     &               '### ERROR in get_agedye: can ',
     &               'not open netCDF file ''', fname(1:lstr), '''.'
                 goto 99
              endif
           endif ! ncid_ad(is).eq.-1
! time id
           ierr=nf_inq_varid (ncid_ad(is), 'ad_time', ad_tid(is))
           if (ierr .ne. nf_noerr) then
              write(*,1) 'ad_time', fname(1:lstr)
              goto 99
           endif
! bc_val:
           ierr=nf_inq_varid (ncid_ad(is), 'bc_val', bcVal_id(is))
           if (ierr .ne. nf_noerr) then
              write(*,1) 'bc_val', fname(1:lstr)
              goto 99          !--> ERROR
           endif
           call set_cycle (ncid_ad(is), ad_tid(is), nt_ad(is), 
     &                     ad_cycle(is), ad_ncycle(is), ad_rec(is), ierr)
           it_ad(is)=1
           ad_time(is,1)=-1.E+20
           ad_time(is,2)=-1.E+20
        enddo  ! is=1,N_agedye
      endif ! iic.eq.ntstart
  1   format(/1x,'### ERROR: get_agedye cannot find variable ''',
     &     A, ''''/7x, 'in netCDF file ''', A, '''.')
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do is=1,N_agedye
      ! Go to next age/dye tracer if this is an age tracer:
      if (Age(is)) cycle
      do while (ad_time(is,it_ad(is)).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (ad_cycle(is), nt_ad(is), ad_ncycle(is),
     &                      ad_rec(is), ierr)
        
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid_ad(is), ad_tid(is), ad_rec(is), cff)
          if (ierr .eq. nf_noerr) then
            it_ad(is)=3-it_ad(is)
            ad_time(is,it_ad(is))=cff*day2sec + ad_cycle(is)*ad_ncycle(is)
            ierr=ncdf_read( ncid_ad(is),   bcVal_id(is),   ad_rec(is),   r_var,
     &                    bc_valg(START_2D_ARRAY,1,it_ad(is),is), N)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(*,
     &        '(6x,2A,I2,A,F12.4,2(1x,A,I4))') 'get_agedye ',
     &        '-- read climatology for age/dye tracer ', is,
     &        ' for time =',
     &        cff, 'rec =', ad_rec(is) MYID
            else
              write(*,2) 'AD tracer', is, ad_rec(is)
            endif
          else
            write(*,2) 'ad_time', is, ad_rec(is)
          endif
        else
          write(*,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_agedye: requested time record ',   ad_rec(is),
     &   'exeeds the last record', nt_ad(is), 'available in forcing ',
     &   'netCDF file ''',   fname(1:lstr),   '''',  'tdays = ',
     &    tdays,        '  but the last available  ad_time =',
     &                                    ad_time(is,it_ad(is))*sec2day
        endif
      enddo ! while ...
      enddo ! is=1,N_agedye
  2   format(/1x,'ERROR in get_agedye: cannot read variable ''', A,
     &           ''' for age/dye tracer ', I3,' and record ', I4/)
 99   continue
      return
      end


! Subroutine for temporal interpolations of boundary conditions:

      subroutine set_agedye_tile (Istr,Iend,Jstr,Jend,ierr)
!
! Set-up sea age/dye data for current tile.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k,it1,it2,ierr,is,nsrc
      real cff, cff1,cff2, cff3,cff4, val1

# include "param.h"
# include "forces.h"
# include "ncvars.h"
# include "scalars.h"
# include "sources.h"
!
# include "compute_extended_bounds.h"
 
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.
!

      do is=1,N_agedye
         ! Go to next age/dye tracer if this is an age tracer:
         if (Age(is)) cycle
         it1=3-it_ad(is)
         it2=it_ad(is)
         cff1=ad_time(is,it2)-time
         cff2=time-ad_time(is,it1)

         ! Looks like synchro_flag is currently not used, but still:
         if (ZEROTH_TILE  .and. ( cff1.lt.dt )) synchro_flag=.true.
 
         if (cff1.ge.0. .and. cff2.ge.0. ) then
            ! Interpolate boundary condition to model time. No conversion of
            ! units here.
            cff=1./(cff1+cff2)
            cff1=cff1*cff
            cff2=cff2*cff

            do nsrc=1,Nsrc_ad(is)
               Ssrc_ad(is,nsrc) = cff1*bc_valg(Isrc_ad(is,nsrc),Jsrc_ad(is,nsrc),Ksrc_ad(is,nsrc),it1,is)
     &                           +cff2*bc_valg(Isrc_ad(is,nsrc),Jsrc_ad(is,nsrc),Ksrc_ad(is,nsrc),it2,is)
            enddo
         elseif (ZEROTH_TILE) then
            write(*,'(1x,2A,i3,3(1x,A,F16.10)/)')
     &       'set_agedye_tile - current model time is outside bounds ',
     &       'for age/dye tracer.', is, 'TSTART=',ad_time(is,it1)*sec2day,
     &                           'TDAYS=',     tdays,
     &                           'TEND=',  ad_time(is,it2)*sec2day
            ierr=ierr+1
         endif ! cff1.ge.0. .and. cff2.ge.0.
      enddo ! is=1,N_agedye

      return
      end
#else
      subroutine get_agedye_empty
      end
#endif /* PASSIVE_TRACER && AGE_DYE_TRACER */
