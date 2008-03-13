#include "cppdefs.h"
#ifdef STATIONS
 
      subroutine wrt_station
!
!--------------------------------------------------------------------
!  Write out data into stations NetCDF file.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "scalars.h"
 
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "mixing.h"
 
# include "ncvars.h"
# include "netcdf.inc"
 
# include "work.h"
!
      real psta(NS)
      integer lenstr, i,j, lsvar,nst, status, tile, start(3),count(3)
# ifdef SOLVE3D
     &                                                       , itrc, k
      real rsta(N,NS)
# endif
!
      if (may_day_flag.ne.0) return      !-->  EXIT
!
!  Set time record index.
!
      tstaindx=tstaindx+1
      nrecsta=nrecsta+1
!
!  Write out SCRUM time (s).
!
      start(1)=tstaindx
      count(1)=1
      status=nf_put_vara_FTYPE(ncstaid,stadid,start,count,time)
      if (status.ne.NF_NOERR) then
        lsvar=lenstr(vname(1,indexd))
        write(stdout,10) vname(1,indexd)(1:lsvar), tstaindx
        may_day_flag=3
        return                           !-->  EXIT
      endif
!
!  Write out station data at RHO-points.
!--------------------------------------------------------------------
!
      start(1)=1
      count(1)=N
      start(2)=1
      count(2)=nstation
      start(3)=tstaindx
      count(3)=1
# ifdef SOLVE3D
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      if (wstaU) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=0.5*(u(i,j,k,nstp)+u(i+1,j,k,nstp))
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,stauid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexu))
          write(stdout,10) vname(1,indexu)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
!
!  Write out 3D momentum component (m/s) in the ETA-direction.
!
      if (wstaV) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=0.5*(v(i,j,k,nstp)+v(i,j+1,k,nstp))
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,stavid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexv))
          write(stdout,10) vname(1,indexv)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
!
!  Write out vertical velocity (m/s).
!
      if (wstaW) then
        do tile=0,NSUB_X*NSUB_E-1
          call wvelocity (work,tile)
        enddo
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=work(i,j,k)
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,stawid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexw))
          write(stdout,10) vname(1,indexw)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
# endif /* SOLVE3D */
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      if (wstaUBAR) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          psta(nst)=0.5*(ubar(i,j,knew)+ubar(i+1,j,knew))
        enddo
        status=nf_put_vara_FTYPE(ncstaid,staubid,start(2),
     &                           count(2),psta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexub))
          write(stdout,10) vname(1,indexub)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      if (wstaVBAR) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          psta(nst)=0.5*(vbar(i,j,knew)+vbar(i,j+1,knew))
        enddo
        status=nf_put_vara_FTYPE(ncstaid,stavbid,start(2),
     &                           count(2),psta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexvb))
          write(stdout,10) vname(1,indexvb)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
!
!  Write out free-surface (m).
!
      if (wstaZ) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          psta(nst)=zeta(i,j,knew)
        enddo
        status=nf_put_vara_FTYPE(ncstaid,stazid,start(2),
     &                           count(2),psta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexz))
          write(stdout,10) vname(1,indexz)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
# ifdef SOLVE3D
!
!  Write out tracer type variables.
!
      do itrc=1,NT
        if (wstaT(itrc)) then
          do nst=1,nstation
            i=ispos(nst)
            j=jspos(nst)
            do k=1,N
              rsta(k,nst)=t(i,j,k,nstp,itrc)
            enddo
          enddo
          status=nf_put_vara_FTYPE(ncstaid,statid(itrc),start,count,
     &                             rsta)
          if (status.ne.NF_NOERR) then
            lsvar=lenstr(vname(1,indext(itrc)))
            write(stdout,10) vname(1,indext(itrc))(1:lsvar), tstaindx
            may_day_flag=3
            return                       !-->  EXIT
          endif
        endif
      enddo
!
!  Write out density anomaly.
!
      if (wstaRHO) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=rho(i,j,k)
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,starid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexr))
          write(stdout,10) vname(1,indexr)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
#  ifdef LMD_KPP
!
!  Write out depth of planetary boundary layer (m).
!
      if (wstaHBL) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          psta(nst)=hbl(i,j)
        enddo
        status=nf_put_vara_FTYPE(ncstaid,stahblid,start(2),
     &                           count(2),psta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexhbl))
          write(stdout,10) vname(1,indexhbl)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
#  endif /* LMD_KPP */
!
!  Write out vertical viscosity coefficient.
!
      if (wstaAKV) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=Akv(i,j,k)
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,staakvid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexakv))
          write(stdout,10) vname(1,indexakv)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      if (wstaAKT) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=Akt(i,j,k,itemp)
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,staaktid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexakt))
          write(stdout,10) vname(1,indexakt)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
#  ifdef SALINITY
!
!  Write out vertical diffusion coefficient for salinity.
!
      if (wstaAKS) then
        do nst=1,nstation
          i=ispos(nst)
          j=jspos(nst)
          do k=1,N
            rsta(k,nst)=Akt(i,j,k,isalt)
          enddo
        enddo
        status=nf_put_vara_FTYPE(ncstaid,staaksid,start,count,rsta)
        if (status.ne.NF_NOERR) then
          lsvar=lenstr(vname(1,indexaks))
          write(stdout,10) vname(1,indexaks)(1:lsvar), tstaindx
          may_day_flag=3
          return                         !-->  EXIT
        endif
      endif
#  endif /* SALINITY */
# endif /* SOLVE3D */
!
!  Synchronize restart NetCDF file to disk.
!
      status=nf_sync(ncstaid)
      if (status.ne.NF_NOERR) then
        write(stdout,20)
        may_day_flag=3
        return                           !-->  EXIT
      endif
 
  10  format(/,' WRT_STATION - error while writing variable: ',a,/,
     &       15x,'into stations NetCDF file for time record: ',i4)
  20  format(/,' WRT_STATION - unable to synchronize stations',
     &       1x,'NetCDF file to disk.')
      return
      end
#else
      subroutine wrt_station_empty
      return
      end
#endif /* STATIONS */
 
