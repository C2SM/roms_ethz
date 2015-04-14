#include "cppdefs.h"
                               ! Writes requested model
      subroutine wrt_his       ! fields at requested levels
                               ! into history netCDF file.
      implicit none
#include "param.h"
#include "ncvars.h"
      integer ierr,  record, i, lstr, lvar, lenstr
     &   , start(2), count(2), ibuff(iaux), nf_fwrite
      character(len=18) tstring
#include "scalars.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "eos_vars.h"
#include "mixing.h"
#include "netcdf.inc"
#ifdef SOLVE3D
      integer tile, itrc, j,k
# include "work.h"
#endif
#if defined MPI && !defined PARALLEL_FILES
# include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode>0) call MPI_Recv (blank, 1, MPI_INTEGER,
     &            mynode-1, 2, ocean_grid_comm, status, ierr)
#endif

! Create/open history file; write grid arrays, if so needed.

      call def_his (nrechis, ierr)
      if (ierr /= nf_noerr) goto 99
      lstr=lenstr(hisname)
                                        !!! WARNING: Once time
! Set record within the file.           !!! stepping has been
                                        !!! started, it is assumed
      nrechis=max(nrechis,1)            !!! that the global history
      if (nrpfhis==0) then              !!! record index "nrechis"
        record=nrechis                  !!! is advanced by main.
      else
        record=1+mod(nrechis-1, nrpfhis)
      endif


c-#define CR
CR      write(*,*) 'wrt_his: Entry  ' MYID

! Write out evolving model variables:
! ----- --- -------- ----- ----------

      ibuff(1)=iic-1               ! Time step and
      ibuff(2)=nrecrst             ! record numbers.
      ibuff(3)=nrechis
      ibuff(4:iaux)=0
#ifdef AVERAGES
      ibuff(4)=nrecavg
#endif
#ifdef STATIONS
      ibuff(5)=nrecstn
#endif
#ifdef FLOATS
      ibuff(6)=nrecflt
#endif
      start(1)=1
      start(2)=record
      count(1)=iaux
      count(2)=1
      ierr=nf_put_vara_int (nchis, hisTstep, start, count, ibuff)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_his :: ',
     &       'Cannot write variable ''time_step'' into history file, ',
     &       'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

! Time

      ierr=nf_put_var1_FTYPE (nchis, hisTime, record, time)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(*,'(/1x,4A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_his :: ',
     &        'Cannot write variable ''',    vname(1,indxTime)(1:lvar),
     &        ''' into history file, rec =', record, nf_strerror(ierr)
     &            MYID
        goto 99                                           !--> ERROR
      endif
CR      write(*,*) 'wrt_his:  time  ' MYID

! Barotropic mode variables: free-surface and 2D momentum
! components in XI-,ETA-directions.

      if (wrthis(indxZ)) then
        ierr=nf_fwrite (zeta(START_2D_ARRAY,knew), nchis, hisZ,
     &                                              record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(*,1) vname(1,indxZ)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtZ  ' MYID
      endif
      if (wrthis(indxUb)) then
        ierr=nf_fwrite (ubar(START_2D_ARRAY,knew), nchis, hisUb,
     &                                              record, u2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(*,1) vname(1,indxUb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his: wrtUBAR' MYID
      endif
      if (wrthis(indxVb)) then
        ierr=nf_fwrite (vbar(START_2D_ARRAY,knew), nchis, hisVb,
     &                                            record, v2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(*,1) vname(1,indxVb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his: wrtVBAR' MYID
      endif

#ifdef SOLVE3D

! 3D momentum components in XI- and ETA-directions.

      if (wrthis(indxU)) then
        ierr=nf_fwrite (u(START_2D_ARRAY,1,nstp), nchis,
     &                             hisU,  record,  u3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(*,1) vname(1,indxU)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtU  ' MYID
      endif
      if (wrthis(indxV)) then
        ierr=nf_fwrite (v(START_2D_ARRAY,1,nstp), nchis,
     &                             hisV,  record,  v3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxV))
          write(*,1) vname(1,indxV)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtV  ' MYID
      endif

! Tracer variables.

      do itrc=1,NT
        if (wrthis(indxT+itrc-1)) then
          ierr=nf_fwrite (t(START_2D_ARRAY,1,nstp,itrc), nchis,
     &                                hisT(itrc), record, r3dvar)


c          write(*,*) 'Replacing T,S with rho1,qp1.'
c          if (itrc==1) then
c            ierr=nf_fwrite (rho1, nchis, hisT(itrc), record,r3dvar)
c          elseif (itrc==2) then
c            ierr=nf_fwrite  (qp1, nchis, hisT(itrc), record,r3dvar)
c          endif

          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(*,1) vname(1,indxT+itrc-1)(1:lvar),
     &                 record MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_his:  wrtT  ' MYID
        endif
      enddo

! Density anomaly.

      if (wrthis(indxR)) then
# ifdef SPLIT_EOS
cxxxxxxxxxx
cxxxxxxxxxx
cxxxxxxxxxx
        ierr=nf_fwrite (rho1, nchis, hisR, record, r3dvar)
# else
        ierr=nf_fwrite (rho, nchis, hisR, record, r3dvar)
# endif
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(*,1) vname(1,indxR)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtRHO' MYID
      endif

! S-coordinate omega vertical velocity (m/s).

      if (wrthis(indxO)) then
        do k=0,N
          do j=0,Mm+1
            do i=0,Lm+1
              work(i,j,k)=pm(i,j)*pn(i,j)*(We(i,j,k)+Wi(i,j,k))
            enddo
          enddo
        enddo
        ierr=nf_fwrite (work, nchis, hisO, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(*,1) vname(1,indxO)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtO  ' MYID
      endif

! True vertical velocity (m/s).

      if (wrthis(indxW)) then
        do tile=0,NSUB_X*NSUB_E-1
          call wvlcty (tile, work)
        enddo
        ierr=nf_fwrite (work, nchis, hisW, record, r3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(*,1) vname(1,indxW)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtW  ' MYID
      endif

! Vertical viscosity coefficient.

      if (wrthis(indxAkv)) then
        ierr=nf_fwrite (Akv, nchis, hisAkv, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(*,1) vname(1,indxAkv)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAkv' MYID
      endif

! Vertical diffusion coefficient for potential temperature.

      if (wrthis(indxAkt)) then
        ierr=nf_fwrite (Akt(START_2D_ARRAY,0,itemp), nchis,
     &                                hisAkt, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(*,1) vname(1,indxAkt)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAkt' MYID
      endif
# ifdef SALINITY

! Vertical diffusion coefficient for salinity.

      if (wrthis(indxAks)) then
        ierr=nf_fwrite (Akt(START_2D_ARRAY,0,isalt), nchis,
     &                                hisAks, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(*,1) vname(1,indxAks)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAks' MYID
      endif
# endif
# ifdef LMD_KPP
! Depth of planetary boundary layer [m].

      if (wrthis(indxHbls)) then
        ierr=nf_fwrite (hbls(START_2D_ARRAY,nstp), nchis,
     &                             hisHbls, record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbls))
          write(*,1) vname(1,indxHbls)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtHBL' MYID
      endif
# endif
# ifdef LMD_BKPP
! Thickness of KPP bottom boundary layer [m].

      if (wrthis(indxHbbl)) then
        ierr=nf_fwrite (hbbl(START_2D_ARRAY,nstp), nchis,
     &                              hisHbbl, record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(*,1) vname(1,indxHbbl)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtHbBL' MYID
      endif
# endif
#endif

  1   format(/1x, '### ERROR: wrt_his :: Cannot write variable ''',
     &              A, ''' into history file, rec =', i6, 3x,A,i4)
      goto 100
  99  if (may_day_flag==0) may_day_flag=3
 100  continue

! Close or synchronize netCDF file to the disk to allow other
! processes to access data immediately after it is written.

#if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close (nchis)
      if (nrpfhis>0 .and. record>=nrpfhis) nchis=-1
#else
      if (nrpfhis>0 .and. record>=nrpfhis) then
        ierr=nf_close (nchis)
        nchis=-1
      else
        ierr=nf_sync(nchis)
      endif
#endif
      if (ierr == nf_noerr) then
#ifdef MPI_SILENT_MODE
        if (mynode==0) then
#endif
          write(tstring,'(F18.8)') tdays
          i=1
          do while (i<18 .and. tstring(i:i)==' ')
            i=i+1
          enddo
          write(*,'(7x,A,1x,A,2x,A,I7,1x,A,I4,A,I4,1x,A,I3)')
     &      'wrt_his :: wrote history, tdays =', tstring(i:i+8),
     &      'step =', ibuff(1),  'rec =', record, '/',  nrechis
     &       MYID
#ifdef MPI_SILENT_MODE
        endif
#endif
      else
        write(*,'(/1x,2A/)')      '### ERROR: wrt_his :: Cannot ',
     &                       'synchronize or close history file.'
        if (may_day_flag==0) may_day_flag=3
      endif
#if defined MPI && !defined PARALLEL_FILES
      if (mynode < NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER,
     &                           mynode+1, 2, ocean_grid_comm, ierr)
#endif
      end
