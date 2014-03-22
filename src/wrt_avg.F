#include "cppdefs.h"
#ifdef AVERAGES

      subroutine wrt_avg             ! Write time-averaged fields
                                     ! into averages netCDF file.
      implicit none
      integer record, i, ierr, lstr, lvar, lenstr, nf_fwrite
# ifdef SOLVE3D
     &             , itrc
# endif
      character(len=18) tstring
# include "param.h"
# include "scalars.h"
# include "averages.h"
# include "ncvars.h"
# include "netcdf.inc"
# if defined MPI && !defined PARALLEL_FILES
#  include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode>0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,  3,
     &                       ocean_grid_comm, status, ierr)
      endif
# endif

! Create/open averages file; write grid arrays, if so needed,
! check for errors, then set record within the file "record".

      call def_avg (nrecavg, ierr)
      lstr=lenstr(avgname)
      if (ierr /= nf_noerr) goto 99       !!! WARNING: Here it is
      nrecavg=max(nrecavg,1)                !!! assumed that global
      if (nrpfavg==0) then                !!! averages record index
        record=nrecavg                      !!! "nrecavg" is already
      else                                  !!! advanced by main.
        record=1+mod(nrecavg-1, nrpfavg)
      endif

! Write out time-averaged variables:
! ----- --- ------------- ----------
! Averaged time

      ierr=nf_put_var1_FTYPE (ncidavg, avgTime, record, time_avg)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(*,1) vname(1,indxTime)(1:lvar), record MYID
        goto 99                                           !--> ERROR
      endif

! Barotropic mode variables: free-surface and 2D momentum components
! in XI-,ETA-directions.

      if (wrtavg(indxZ)) then
        ierr=nf_fwrite (zeta_avg(START_2D_ARRAY), ncidavg, avgZ,
     &                                            record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(*,1) vname(1,indxTime)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavg(indxUb)) then
        ierr=nf_fwrite (ubar_avg(START_2D_ARRAY), ncidavg, avgUb,
     &                                            record, u2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(*,1) vname(1,indxUb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavg(indxVb)) then
        ierr=nf_fwrite (vbar_avg(START_2D_ARRAY), ncidavg, avgVb,
     &                                            record, v2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(*,1) vname(1,indxVb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# ifdef SOLVE3D

! 3D momentum components in XI- and ETA-directions.

      if (wrtavg(indxU)) then
        ierr=nf_fwrite (u_avg(START_2D_ARRAY,1), ncidavg, avgU,
     &                                           record, u3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(*,1) vname(1,indxU)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavg(indxV)) then
        ierr=nf_fwrite (v_avg(START_2D_ARRAY,1), ncidavg, avgV,
     &                                          record, v3dvar)
        if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxV))
          write(*,1) vname(1,indxV)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

! Tracer variables.

      do itrc=1,NT
        if (wrtavg(indxT+itrc-1)) then
          ierr=nf_fwrite (t_avg(START_2D_ARRAY,1,itrc), ncidavg,
     &                              avgT(itrc), record, r3dvar)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(*,1) vname(1,indxT+itrc-1)(1:lvar),record MYID
            goto 99                                       !--> ERROR
          endif
        endif
      enddo

! Density anomaly.

      if (wrtavg(indxR)) then
        ierr=nf_fwrite (rho_avg(START_2D_ARRAY,1), ncidavg, avgR,
     &                                             record, r3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(*,1) vname(1,indxR)(1:lvar), record  MYID
          goto 99                                         !--> ERROR
        endif
      endif

! S-coordinate omega vertical velocity (m/s).

      if (wrtavg(indxO)) then
        ierr=nf_fwrite (w_avg, ncidavg, avgO, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(*,1) vname(1,indxO)(1:lvar), record  MYID
          goto 99                                         !--> ERROR
        endif
      endif

! True vertical velocity (m/s).

      if (wrtavg(indxW)) then
        ierr=nf_fwrite (wvl_avg, ncidavg, avgW, record, r3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(*,1) vname(1,indxW)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

! Vertical eddy viscosity coefficient for momentum.

      if (wrtavg(indxAkv)) then
        ierr=nf_fwrite (akv_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgAkv, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(*,1) vname(1,indxAkv)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

! Vertical heat conductivity

      if (wrtavg(indxAkt)) then
        ierr=nf_fwrite (akt_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgAkt, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(*,1) vname(1,indxAkt)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  ifdef SALINITY

! Vertical diffusion coefficient for salinity.

      if (wrtavg(indxAks)) then
        ierr=nf_fwrite (aks_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgAks, record, w3dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(*,1) vname(1,indxAks)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef LMD_KPP

! Depth of planetary boundary layer [m].

      if (wrtavg(indxHbls)) then
        ierr=nf_fwrite (hbl_avg(START_2D_ARRAY), ncidavg,
     &                            avgHbls, record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbls))
          write(*,1) vname(1,indxHbls)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef LMD_BKPP

! Thickness of KPP bottom boundary layer [m].

      if (wrtavg(indxHbbl)) then
        ierr=nf_fwrite (hbbl_avg(START_2D_ARRAY), ncidavg,
     &                              avgHbbl, record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(*,1) vname(1,indxHbbl)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
# endif /* SOLVE3D */

  1   format(/1x, '### ERROR: wrt_avg :: Cannot write variable ''',
     &             A, ''' into averages file, rec =', i6, 3x,A,i4)
      goto 100
  99  if (may_day_flag==0) may_day_flag=3
 100  continue

! Close or synchronize netCDF file to disk to allow other
! processes to access data immediately after it is written.

# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(ncidavg)
      if (nrpfavg>0 .and. record>=nrpfavg) ncidavg=-1
# else
      if (nrpfavg>0 .and. record>=nrpfavg) then
        ierr=nf_close(ncidavg)
        ncidavg=-1
      else
        ierr=nf_sync(ncidavg)
      endif
# endif
      if (ierr == nf_noerr) then
# ifdef MPI_SILENT_MODE
        if (mynode==0) then
# endif
          write(tstring,'(F18.8)') time_avg*sec2day
          i=1
          do while (i<18 .and. tstring(i:i)==' ')
            i=i+1
          enddo
          write(*,'(7x,2A,1x,A,1x,A,I4,A,I4,1x,A,I3)') 'wrt_avg :: ',
     &      'wrote averages for time_avg =', tstring(i:i+8), 'rec =',
     &       record, '/', nrecavg  MYID
# ifdef MPI_SILENT_MODE
        endif
# endif
      else
        write(*,'(/1x,2A/)')      '### ERROR: wrt_avg :: Cannot ',
     &                         'synchronize/close averages file.'
        if (may_day_flag==0) may_day_flag=3
      endif
# if defined MPI && !defined PARALLEL_FILES
      if (mynode < NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER,
     &                           mynode+1, 3, ocean_grid_comm, ierr)
# endif
      end
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
