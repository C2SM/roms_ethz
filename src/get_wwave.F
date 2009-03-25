#include "cppdefs.h"
#if defined SG_BBL96 && !defined ANA_WWAVE
 
      subroutine get_wwave (ierr)
!
!--------------------------------------------------------------------
!  Read wind induced wave amplitude, direction and period from
!  forcing netCDF file.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "scalars.h"
# include "pconst.h"
# include "bblm.h"
# include "forces.h"
# include "ncvars.h"
# include "netcdf.inc"
# include "ocean.h"
      logical gotwwa, gotwwd, gotwwp, gotwwt
      integer i, lstr, lsvar, lvar, status, lenstr, nf_fread
      real tdelta, tend, tstart
 
      if (may_day_flag.ne.0) return      !-->  EXIT
 
      if (iic.eq.0) then
!
!  Initialize time variables.
!
        tww(1)=0.
        tww(2)=0.
        ww_tintrp(1)=0.
        ww_ww_tintrp(2)=0.
!
!  Intialize logical flags.
!
        ww_onerec=.false.
        gotwwa=.false.
        gotwwd=.false.
        gotwwp=.false.
        gotwwt=.false.
!
!  Inquire about the contents of forcing NetCDF file.
!  Inquire about the dimensions and variables. Check for consistency.
!--------------------------------------------------------------------
!
        lstr=lenstr(frcname)
        call opencdf (frcname,N)
        if (may_day_flag.ne.0) return    !-->  EXIT
 
!
!  Scan variable list from input NetCDF and check for wind induced
!  wave variables.
!
        do i=1,nvars
          lvar=lenstr(varnam(i))
          lsvar=lenstr(vname(1,indxWWA))
          if (varnam(i)(1:lvar).eq.vname(1,indxWWA)(1:lsvar)) then
            wwaid=i
            gotwwa=.true.
            if (nvdims(i).gt.1) lwwgrd=.true.
          endif
          lsvar=lenstr(vname(1,indxWWD))
          if (varnam(i)(1:lvar).eq.vname(1,indxWWD)(1:lsvar)) then
            wwdid=i
            gotwwd=.true.
            if (nvdims(i).gt.1) lwwgrd=.true.
          endif
          lsvar=lenstr(vname(1,indxWWP))
          if (varnam(i)(1:lvar).eq.vname(1,indxWWP)(1:lsvar)) then
            wwpid=i
            gotwwp=.true.
            if (nvdims(i).gt.1) lwwgrd=.true.
          endif
          if (varnam(i)(1:lvar).eq.'wave_time') then
            wwtid=i
            gotwwt=.true.
          endif
        enddo
!
!  Terminate execution if wind induced variables are not found.
!
        if (.not.gotwwa) then
          lsvar=lenstr(vname(1,indxWWA))
          write(stdout,10) vname(1,indxWWA)(1:lsvar), frcname(1:lstr)
          may_day_flag=2
          return                         !-->  EXIT
        endif
        if (.not.gotwwd) then
          lsvar=lenstr(vname(1,indxWWD))
          write(stdout,10) vname(1,indxWWD)(1:lsvar), frcname(1:lstr)
          may_day_flag=2
          return                         !-->  EXIT
        endif
        if (.not.gotwwp) then
          lsvar=lenstr(vname(1,indxWWP))
          write(stdout,10) vname(1,indxWWP)(1:lsvar), frcname(1:lstr)
          may_day_flag=2
          return                         !-->  EXIT
        endif
        if (.not.gotwwt) then
          write(stdout,10) 'wave_time', frcname(1:lstr)
          may_day_flag=2
          return                         !-->  EXIT
        endif
!
!  Open forcing NetCDF file for reading.
!
        ncid=ncidfrc(ifrc)
        if (ncid.eq.-1) then
          status=nf_open(frcname(1:lstr), nf_nowrite, ncid)
          if (status.eq.NF_NOERR) then
            ncidfrc(ifrc)=ncid
          else
            write(stdout,20) frcname(1:lstr)
            may_day_flag=2
            return                       !-->  EXIT
          endif
        endif
!
!  Determine initial time index to read and cycling switch.
!
        call get_cycle (ncid, wwtid,   ntww,   wwcycle,
     &                  wwclen,  twwindx, tstart, tend)
        if (may_day_flag.ne.0) return    !-->  EXIT
 
!
!  The strategy here is to create a local, monotonically increasing
!  time variable so the interpolation between snapshots is trivial
!  when cycling forcing fields.  Subtract one to time index "twwindx"
!  to avoid doing special case at initialization.
!
        itww=2
        ww_tstart=tstart
        ww_tend=tend
        tww(itww)=tstart
        if (wwcycle) then
          if (twwindx.eq.ntww) then
            if (tdays.lt.tend) then
              tsww=tstart-wwclen
            else
              tsww=tdays+(tstart-wwclen)
              tsww=tsww+(tstart-mod(tdays+tstart,wwclen))
            endif
            tsww=tsww*day2sec
          else
            if (tdays.gt.wwclen) then
              tsww=time-mod(tdays-tstart,wwclen)*day2sec
            else
              tsww=tstart*day2sec
            endif
          endif
        else
          tsww=time
        endif
        twwindx=twwindx-1
!
!  Set scale factor to convert input wind induced wave data to model
!  units: convert from degrees to radieans.
!
        sclwwa=1.
        sclwwd=deg2rad
        sclwwp=1.
!
!  Set switch for one time record dataset. In this case, the wind
!  induced wave data is always the same and no time interpolation
!  is performed.
!
        if (ntww.eq.1) ww_onerec=.true.
      endif
!
!
      if ((tsww.lt.time).or.(iic.eq.0)) then
        if (wwcycle) then
          twwindx=mod(twwindx,ntww)+1
        else
          twwindx=twwindx+1
        endif
        if (twwindx.le.ntww) then
          itww=3-itww
!
!  Read time coordinate.
!
          status=nf_get_var1_FTYPE(ncid,wwtid,twwindx,tww(itww))
          if (status.ne.NF_NOERR) then
            write(stdout,30) 'wave_time', twwindx
            may_day_flag=2
            return                       !-->  EXIT
          endif
!
!  Read in 2D-grided or point wind induced wave amplitude, direction
!  and period.
!
          if (lwwgrd) then
            status=nf_fread(wwag(START_2D_ARRAY,itww),ncid,wwaid,
     &                      twwindx,r2dvar)
            if (status.ne.NF_NOERR) then
              write(stdout,30) 'Awave', twwindx
              may_day_flag=2
              return                     !-->  EXIT
            endif
            status=nf_fread(wwdg(START_2D_ARRAY,itww),ncid,wwdid,
     &                      twwindx,r2dvar)
            if (status.ne.NF_NOERR) then
              write(stdout,30) 'Dwave', twwindx
              may_day_flag=2
              return                     !-->  EXIT
            endif
            status=nf_fread(wwpg(START_2D_ARRAY,itww),ncid,wwpid,
     &                      twwindx,r2dvar)
            if (status.ne.NF_NOERR) then
              write(stdout,30) 'Pwave', twwindx
              may_day_flag=2
              return                     !-->  EXIT
            endif
          else
            status=nf_get_var1_FTYPE(ncid,wwaid,twwindx,wwap(itww))
            if (status.ne.NF_NOERR) then
              write(stdout,30) 'Awave', twwindx
              may_day_flag=2
              return                     !-->  EXIT
            endif
            status=nf_get_var1_FTYPE(ncid,wwdid,twwindx,wwdp(itww))
            if (status.ne.NF_NOERR) then
              write(stdout,30) 'Dwave', twwindx
              may_day_flag=2
              return                     !-->  EXIT
            endif
            status=nf_get_var1_FTYPE(ncid,wwpid,twwindx,wwpp(itww))
            if (status.ne.NF_NOERR) then
              write(stdout,30) 'Pwave', twwindx
              may_day_flag=2
              return                     !-->  EXIT
            endif
          endif
          write(stdout,40) tww(itww)
        endif
!
!  Increment the local time variable "tsww" by the interval between
!  snapshots. If the interval is negative, indicating cycling, add
!  in cycle length. Load values (in seconds) to time variable
!  "ww_tintrp" which used during interpolation between snapshots.
!
        if (.not.ww_onerec) then
          tdelta=tww(itww)-tww(3-itww)
          if (wwcycle.and.(tdelta.lt.0.)) then
            tdelta=tdelta+wwclen
          endif
          tsww=tsww+tdelta*day2sec
          ww_tintrp(itww)=tsww
        endif
      endif
!
  10  format(/,' GET_WWAVE - unable to find forcing variable: ',a,
     &       /,15x,'in forcing NetCDF file: ',a)
  20  format(/,' GET_WWAVE - unable to open forcing NetCDF file: ',a)
  30  format(/,' GET_WWAVE - error while reading variable: ',a,2x,
     &       ' at TIME index = ',i4)
  40  format(6x,'GET_WWAVE   - Read wind induced wave fields ',
     &                  'for time = ', g12.4)
      return
      end
      subroutine set_wwave_tile (istr,iend,jstr,jend)
!
!--------------------------------------------------------------------
!  Set-up wind-induced wave data for current tile.
!--------------------------------------------------------------------
!
      implicit none
      integer istr,iend,jstr,jend, i,j, i1,it2
      real cff, cff1,cff2, cff3,cff4, cff5,cff6, wwa,wwd,wwp
# include "param.h"
# include "pconst.h"
# include "grid.h"
# include "ocean.h"
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      it1=3-itww
      it2=itww
      fac1=ww_tintrp(it2)-time
      fac2=time-ww_tintrp(it1)
!
! Load time invariant wind-induced wave data.
!--------------------------------------------
!
      if (ww_onerec) then
        if (iic.eq.ntstart) then
          if (lwwgrd) then
            do j=jstrR,jendR
              do i=istrR,jendR
                Awave(i,j)=sclwwa*wwag(i,j,itww)
                Dwave(i,j)=sclwwd*wwdg(i,j,itww)
                Pwave(i,j)=sclwwp*wwpg(i,j,itww)
              enddo
            enddo
          else
            wwa=sclwwa*wwap(itww)
            wwd=sclwwd*wwdp(itww)
            wwp=sclwwp*wwpp(itww)
            do j=jstrR,jendR
              do i=istrR,jendR
                Awave(i,j)=wwa
                Dwave(i,j)=wwd
                Pwave(i,j)=wwp
              enddo
            enddo
          endif
        endif
!
! Time-interpolate wind induced wave amplitude, direction and period
! from gridded or point data. Check that for the next time step [when
! time=time+dt] time+dt is still between ww_tintrp(it1) and
! ww_tintrp(it2); and if not, set synchro_flag top signal that the
! new forcing data should be read from the netCDF input file (ONE
! THREAD ONLY).
!
      elseif (fac1.ge.0. .and. fac2.ge.0.) then
        if (fac1.lt.dt) synchro_flag=.true.
        fac=sclwwd/(fac1+fac2)
        fac3=fac*fac1
        fac4=fac*fac2
        fac=sclwwp/(fac1+fac2)
        fac5=fac*fac1
        fac6=fac*fac2
        fac=sclwwa/(fac1+fac2)
        fac1=fac*fac1
        fac2=fac*fac2
        if (lwwgrd) then
          do j=jstrR,jendR
            do i=istrR,iendR
              Awave(i,j)=fac1*wwag(i,j,it1)+fac2*wwag(i,j,it2)
              Dwave(i,j)=fac3*wwdg(i,j,it1)+fac4*wwdg(i,j,it2)
              Pwave(i,j)=fac5*wwpg(i,j,it1)+fac6*wwpg(i,j,it2)
            enddo
          enddo
        else
          wwa=fac1*wwap(it1)+fac2*wwap(it2)
          wwd=fac3*wwdp(it1)+fac4*wwdp(it2)
          wwp=fac5*wwpp(it1)+fac6*wwpp(it2)
          do j=jstrR,jendR
            do i=istrR,jendR
              Awave(i,j)=wwa
              Dwave(i,j)=wwd
              Pwave(i,j)=wwp
            enddo
          enddo
        endif
!
! Unable to set-up wind-induced wave data.
! Complain about the error and signal to quit (ONE THREAD ONLY).
!
      else
        if (ZEROTH_TILE) then
          write(stdout,10) 'wave_time', tdays, tend
  10      format(/,' SET_WWAVE_TILE - current model time',
     &           1x,'exceeds ending value for variable: ',
     &           a,/,13x,'TDAYS = ',g12.4,2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif
#else
      subroutine get_wwave_empty
#endif /* SG_BBL96 && !ANA_WWAVE */
      return
      end
 
