#include "cppdefs.h"
#if defined UCLIMATOLOGY && !defined ANA_UCLIMA
 
                                        ! Read momentum climatology
      subroutine get_uclima (ierr)      ! fields from climatological
      implicit none                     ! file at appropriate time.
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "netcdf.inc"
# include "climat.h"
      real cff
      integer i, lstr,lvar,lenstr, ierr, nf_fread
!
! Initialization: Check, whether climatological data file is already
!================ opened, and if not, open it.  Find and save netCDF
! IDs for relevant variables.  Determine whether there is cycling to
! reuse the input data and find cycling period "uclm_cycle", set
! initial cycling index "uclm_ncycle" and record index "uclm_rec",
! time index "iuclm" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.
!
      ierr=nf_noerr
      lstr=lenstr(clm_file)
      if (iic.eq.ntstart) then
        if (ncidclm.eq.-1) then
          ierr=nf_open (clm_file(1:lstr), nf_nowrite, ncidclm)
          if (ierr .ne. nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
     &                  'in get_uclima: can not open netCDF file ''',
     &                                      clm_file(1:lstr),   '''.'
        endif
        if (ierr.eq.nf_noerr) then
          ierr=nf_inq_varid (ncidclm, 'uclm_time', uclm_tid)
          if (ierr.eq.nf_noerr) then
            lvar=lenstr(vname(1,indxUb))
            ierr=nf_inq_varid (ncidclm, vname(1,indxUb)(1:lvar),
     &                                                 ubclm_id)
            if (ierr.eq.nf_noerr) then
              lvar=lenstr(vname(1,indxVb))
              ierr=nf_inq_varid (ncidclm, vname(1,indxVb)(1:lvar),
     &                                                  vbclm_id)
              if (ierr.eq.nf_noerr) then
# ifdef SOLVE3D
                lvar=lenstr(vname(1,indxU))
                ierr=nf_inq_varid (ncidclm, vname(1,indxU)(1:lvar),
     &                                                     uclm_id)
                if (ierr.eq.nf_noerr) then
                  lvar=lenstr(vname(1,indxV))
                  ierr=nf_inq_varid (ncidclm, vname(1,indxV)(1:lvar),
     &                                                      vclm_id)
                  if (ierr.eq.nf_noerr) then
# endif
                    call set_cycle (ncidclm, uclm_tid, ntuclm,
     &                     uclm_cycle, uclm_ncycle, uclm_rec, ierr)
                    ituclm=1
                    uclm_time(1)=-1.E+20
                    uclm_time(2)=-1.E+20
# ifdef SOLVE3D
                  else
                    write(stdout,1) vname(1,indxV)(1:lvar),
     &                                     clm_file(1:lstr)
                  endif
                else
                  write(stdout,1) vname(1,indxU)(1:lvar),
     &                                     clm_file(1:lstr)
                endif
# endif
              else
                write(stdout,1) vname(1,indxVb)(1:lvar),
     &                                    clm_file(1:lstr)
              endif
            else
              write(stdout,1) vname(1,indxUb)(1:lvar),
     &                                    clm_file(1:lstr)
            endif
          else
            write(stdout,1) 'uclm_time', clm_file(1:lstr)
          endif
        endif
      endif
  1   format(/1x,'ERROR in get_uclima: cannot find variable ''',
     &                A, ''''/7x, 'in netCDF file ''', A, '''.')
!
! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (uclm_time(ituclm).lt.time+0.5*dt .and.
     &                                     ierr.eq.nf_noerr)
        call advance_cycle (uclm_cycle,  ntuclm, uclm_ncycle,
     &                                       uclm_rec, ierr)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncidclm, uclm_tid, uclm_rec, cff)
          if (ierr.eq.nf_noerr) then
            ituclm=min(3-ituclm,ntuclm)
            uclm_time(ituclm)=cff*day2sec + uclm_cycle*uclm_ncycle
            ierr=nf_fread (ubclima(START_2D_ARRAY,ituclm), ncidclm,
     &                                   ubclm_id, uclm_rec, u2dvar)
            if (ierr.eq.nf_noerr) then
              ierr=nf_fread (vbclima(START_2D_ARRAY,ituclm), ncidclm,
     &                                    vbclm_id, uclm_rec, v2dvar)
              if (ierr.eq.nf_noerr) then
# ifdef SOLVE3D
                ierr=nf_fread (uclima(START_2D_ARRAY,1,ituclm),
     &                            ncidclm, uclm_id, uclm_rec, u3dvar)
                if (ierr.eq.nf_noerr) then
                  ierr=nf_fread (vclima(START_2D_ARRAY,1,ituclm),
     &                            ncidclm, vclm_id, uclm_rec, v3dvar)
                  if (ierr.eq.nf_noerr) then
# endif
                    write(stdout,'(6x,A,5x,A,G12.4,2(1x,A,I4))')
     &                 'get_uclima -- read momentum climatology',
     &                 'uclm_time =', cff, 'rec =', uclm_rec MYID
                    if (ntuclm.eq.1) return
# ifdef SOLVE3D
                  else
                    lvar=lenstr(vname(1,indxV))
                    write(stdout,2) vname(1,indxV)(1:lvar), uclm_rec
                  endif
                else
                  lvar=lenstr(vname(1,indxU))
                  write(stdout,2) vname(1,indxU)(1:lvar), uclm_rec
                endif
# endif
              else
                lvar=lenstr(vname(1,indxVb))
                write(stdout,2) vname(1,indxVb)(1:lvar), uclm_rec
              endif
            else
              lvar=lenstr(vname(1,indxUb))
              write(stdout,2) vname(1,indxUb)(1:lvar), uclm_rec
            endif
          else
            write(stdout,2) 'Xclm_time', uclm_rec
          endif
        else
           write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &    'ERROR in get_uclima: requested time record ',  uclm_rec,
     &    'exeeds the last record', ntuclm, 'available in netCDF ',
     &    'file ''',   clm_file(1:lstr),  '''',  'tdays = ', tdays,
     &    '  but the last available  uclm_time =',
     &                                   uclm_time(ituclm)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_uclima: cannot read variable ''',
     &                                 A, ''' for record ', I4/)
      return
      end
 
      subroutine set_uclima_tile (istr,iend,jstr,jend, ierr)
!
!  Set-up momentum climatology for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j,k, it1,it2
      real cff, cff1, cff2
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
      it1=3-ituclm
      it2=ituclm
      cff1=uclm_time(it2)-time
      cff2=time-uclm_time(it1)

      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntuclm.eq.1) then                 ! load time-invariant
        if (iic.eq.0) then                  ! momentum climatology.
          do j=jstrR,jendR
            do i=istrR,iendR
              ubclm(i,j)=ubclima(i,j,ituclm)
              vbclm(i,j)=vbclima(i,j,ituclm)
            enddo
          enddo
# ifdef SOLVE3D
          do k=1,N
            do j=jstrR,jendR
              do i=istrR,iendR
                uclm(i,j,k)=uclima(i,j,k,ituclm)
                vclm(i,j,k)=vclima(i,j,k,ituclm)
              enddo
            enddo
          enddo
# endif
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        cff=1./(cff1+cff2)                        ! interpolate
        cff1=cff1*cff                             ! tracer
        cff2=cff2*cff                             ! climatology
        do j=jstrR,jendR                          ! in time.
          do i=istrR,iendR
            ubclm(i,j)=cff1*ubclima(i,j,it1)
     &                +cff2*ubclima(i,j,it2)
            vbclm(i,j)=cff1*vbclima(i,j,it1)
     &                +cff2*vbclima(i,j,it2)
          enddo
        enddo
# ifdef SOLVE3D
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
              uclm(i,j,k)=cff1*uclima(i,j,k,it1)
     &                   +cff2*uclima(i,j,k,it2)
              vclm(i,j,k)=cff1*vclima(i,j,k,it1)
     &                   +cff2*vclima(i,j,k,it2)
            enddo
          enddo
        enddo
# endif
      elseif (ZEROTH_TILE) then
        write(stdout,'(/2(1x,A)/3(1x,A,F16.10)/)')
     &   'SET_UCLIMA_TILE - current model time is out of bounds of',
     &   '''uclm_time''.',   'UCLM_TSTART=',  uclm_time(it1)*sec2day,
     &   'TDAYS=',  tdays,   'UCLM_TEND=',    uclm_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else
      subroutine get_uclima_empty
      end
#endif /* UCLIMATOLOGY && !ANA_UCLIMA */
 
