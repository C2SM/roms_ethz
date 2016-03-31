#include "cppdefs.h"
                              ! Write model prognostic
      subroutine wrt_rst      ! variables into restart
                              ! netCDF file.
      implicit none
#include "param.h"
#include "ncvars.h"
      integer record, start(2), count(2), ibuff(iaux),
     &        i, ierr, lstr, lvar, lenstr, ncdf_write
#ifdef SOLVE3D
     &      , itrc
#endif
      character(len=18) tstring
#include "scalars.h"
#include "ocean2d.h"
#ifdef EXACT_RESTART
# include "coupling.h"
#endif
#include "ocean3d.h"
#if defined LMD_KPP || defined LMD_BKPP
# include "mixing.h"
#endif

#include "netcdf.inc"
#if defined BGC_FLUX_ANALYSIS || defined SEDIMENT_BIOLOGY
# include "bgcflux.h"
# include "ncroms_bgcflux.h"
#endif
#ifdef BIOLOGY_BEC
# include "ecosys_bec.h"
# include "bec_avg.h"
#endif
#ifdef BIOLOGY_BEC2
# include "ecosys_bec2.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
# include "physflux.h"
# include "ncroms_physflux.h"
#endif

#if defined FLOATS
# include "ncvars_floats.h"
# include "floats.h"
      integer startTinfo(3), starttrack(4), i,j,
     &        countTinfo(3), counttrack(4), level
      integer tmp(NFT+1,-1:0)
      real bufftrack(1:6,NFT+1,nfloats)
#endif
#if defined MPI && !defined PARALLEL_FILES
# include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode > 0) call MPI_Recv (blank, 1, MPI_INTEGER,
     &            mynode-1, 1, ocean_grid_comm, status, ierr)
#endif

! Create/open restart and set record index within the file.

      call def_rst(nrecrst, ierr)
      if (ierr /= nf_noerr) goto 99
      lstr=lenstr(rstname)
      nrecrst=max(nrecrst,1)                  !!! WARNING: Here it
      if (nrpfrst == 0) then                  !!! is assumed that
        record=nrecrst                        !!! global restart
      else                                    !!! record index
        record=1+mod(nrecrst-1, abs(nrpfrst)) !!! nrecrst is already
      endif                                   !!! advanced by main.

! Write out evolving model variables:
! ----- --- -------- ----- ----------

      ibuff(1)=iic-1  ; ibuff(2)=nrecrst        ! Time step and
      ibuff(4:iaux)=0 ; ibuff(3)=nrechis        ! record numbers.
#ifdef AVERAGES
      ibuff(4)=nrecavg
#endif
#ifdef STATIONS
      ibuff(5)=nrecstn
#endif
#ifdef FLOATS
      ibuff(6)=nrecflt
#endif
      start(1)=1      ; count(1)=iaux
      start(2)=record ; count(2)=1

      ierr=nf_put_vara_int (ncrst, rstTstep, start, count, ibuff)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_rst :: ',
     &       'Cannot write variable ''time_step'' into restart file, ',
     &       'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
! Time record numbers for flux analysis:
      ! default values: 0; replace where appropriate
      ibuff = (/0, 0, 0, 0/)
# ifdef BGC_FLUX_ANALYSIS
      ibuff(1) = nrec_bgc_flux_his
#  ifdef AVERAGES
      ibuff(2) = nrec_bgc_flux_avg
#  endif
# endif
# ifdef PHYS_FLUX_ANALYSIS
      ibuff(3) = nrec_phys_flux_his
#  ifdef AVERAGES
      ibuff(4) = nrec_phys_flux_avg
#  endif
# endif
! start and count values are the same as for time_step
      ierr=nf_put_vara_int (ncrst, rstTstepFA, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(*,1) 'fa_time_step', record, ierr
     &                   MYID
        goto 99                                           !--> ERROR
      endif
#endif /* BGC_FLUX_ANALYSIS || PHYS_FLUX_ANALYSIS */

! Time.

      ierr=nf_put_var1_FTYPE (ncrst, rstTime, record, time)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(*,'(/1x,4A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_rst :: ',
     &        'Cannot write variable ''',    vname(1,indxTime)(1:lvar),
     &        ''' into restart file, rec =', record, nf_strerror(ierr)
     &            MYID
        goto 99                                           !--> ERROR
      endif

! Free-surface and 2D momentum components in XI- and ETA-directions.

      ierr=ncdf_write(ncrst,  rstZ,  record,  r_var,
     &                zeta(START_2D_ARRAY,knew), 1)
      if (ierr == nf_noerr) then
        ierr=ncdf_write(ncrst,  rstUb,  record, u_var,
     &                  ubar(START_2D_ARRAY,knew), 1)
        if (ierr == nf_noerr) then
          ierr=ncdf_write(ncrst,  rstVb,  record, v_var,
     &                    vbar(START_2D_ARRAY,knew), 1)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxVb))
            write(*,1) vname(1,indxVb)(1:lvar), record MYID
          endif
        else
          lvar=lenstr(vname(1,indxUb))
          write(*,1) vname(1,indxUb)(1:lvar), record MYID
        endif
      else
        lvar=lenstr(vname(1,indxZ))
        write(*,1) vname(1,indxZ)(1:lvar), record MYID
      endif
      if (ierr /= nf_noerr) goto 99                !--> ERROR

#ifdef SOLVE3D
# ifdef EXACT_RESTART
#  ifdef EXTRAP_BAR_FLUXES
      ierr=ncdf_write(ncrst, rst_DU_avg2, record, u_var, DU_avg2, 1)
      if (ierr == nf_noerr) then
        ierr=ncdf_write(ncrst, rst_DV_avg2, record, v_var, DV_avg2, 1)
        if (ierr == nf_noerr) then
          ierr=ncdf_write(ncrst, rst_DU_avg_bak, record, u_var,
     &                                                  DU_avg_bak, 1)
          if (ierr == nf_noerr) then
            ierr=ncdf_write(ncrst, rst_DV_avg_bak, record, v_var,
     &                                                  DV_avg_bak, 1)
            if (ierr /= nf_noerr) then
              write(*,1) 'DV_avg_bak', record MYID
            endif
          else
            write(*,1) 'DU_avg_bak', record MYID
          endif
        else
          write(*,1) 'DV_avg2', record MYID
        endif
      else
        write(*,1) 'DU_avg2', record MYID
      endif
#  elif defined PRED_COUPLED_MODE
      ierr=ncdf_write(ncrst,   rst_rufrc,  record,  u_var,
     &                  rufrc_bak(START_2D_ARRAY,nstp), 1)
      if (ierr == nf_noerr) then
        ierr=ncdf_write(ncrst, rst_rvfrc,  record,  v_var,
     &                  rvfrc_bak(START_2D_ARRAY,nstp), 1)
        if (ierr /= nf_noerr) then
          write(*,1) 'rvfrc_bak', record MYID
        endif
      else
        write(*,1) 'rufrc_bak', record MYID
      endif
#  endif
      if (ierr /= nf_noerr) goto 99                !--> ERROR
# endif

! 3D momentum components in XI- and ETA-directions.

      ierr=ncdf_write(ncrst, rstU,  record,  u_var,
     &                 u(START_2D_ARRAY,1,nstp), N)
      if (ierr == nf_noerr) then
        ierr=ncdf_write(ncrst, rstV, record, v_var,
     &                 v(START_2D_ARRAY,1,nstp), N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxV))
          write(*,1) vname(1,indxV)(1:lvar), record MYID
        endif
      else
        lvar=lenstr(vname(1,indxU))
        write(*,1) vname(1,indxU)(1:lvar), record MYID
      endif
      if (ierr /= nf_noerr) goto 99                !--> ERROR

! Tracer variables.

      do itrc=1,NT
        ierr=ncdf_write(ncrst,  rstT(itrc), record, r_var,
     &                  t(START_2D_ARRAY,1,nstp,itrc), N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxT+itrc-1))
          write(*,1) vname(1,indxT+itrc-1)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      enddo

#  ifdef SEDIMENT_BIOLOGY

! Sediment tracer variables.

      do itrc=1,NT_sed
        ierr=nf_fwrite (t_sed(START_2D_ARRAY,itrc), ncrst,
     &                              rstTsed(itrc), record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
          write(*,1) vname(1,indxSedOrgN+itrc-1)(1:lvar), 
     &         record, ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo
#  endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_BEC || define BIOLOGY_BEC2 || (defined BIOLOGY_NPZDOC && \
    defined OXYGEN && defined CARBON)

! pH:

      ierr=nf_fwrite (ph_hist, ncrst, rstPH, record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPH_rst))
         write(*,1) vname(1,indxPH_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif

! pCO2:

      ierr=nf_fwrite (pCO2sw, ncrst, rstPCO2, record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPCO2_rst))
         write(*,1) vname(1,indxPCO2_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
#  ifdef OUTPUT_PCO2AIR_RST

! pCO2air:

      ierr=nf_fwrite (pCO2air, ncrst, rstPCO2air, record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPCO2air_rst))
         write(*,1) vname(1,indxPCO2air_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
#  endif /* OUTPUT_PCO2AIR_RST */
# endif /* BIOLOGY_BEC || (BIOLOGY_NPZDOC && OXYGEN && CARBON) */

# if defined BIOLOGY_BEC2 && defined BEC2_DIAG

! 2d diagnostics:

      do itrc=1,nr_bec2_diag_2d
         ierr=nf_fwrite (bec2_diag_2d(START_2D_ARRAY,itrc), ncrst,
     &                              rstT_bec2_diag(itrc), record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bec2_diag_2d(1,itrc))
          write(*,1) vname_bec2_diag_2d(1,itrc)(1:lvar), record,
     &              ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo

! 3d diagnostics:

      do itrc=1,nr_bec2_diag_3d
         ierr=nf_fwrite (bec2_diag_3d(START_2D_ARRAY,1,itrc), ncrst,
     &                   rstT_bec2_diag(nr_bec2_diag_2d+itrc), record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bec2_diag_3d(1,itrc))
          write(*,1) vname_bec2_diag_3d(1,itrc)(1:lvar), record,
     &              ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo
# endif /* BIOLOGY_BEC2 && BEC2_DIAG */

# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC

! PAR:

      ierr=nf_fwrite (PAR, ncrst, rstPAR, record, r3dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPAR_rst))
         write(*,1) vname(1,indxPAR_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
#  endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */

# ifdef LMD_KPP
! Depth of planetary boundary layer [m].

      ierr=ncdf_write(ncrst, rstHbls, record, r_var,
     &                hbls(START_2D_ARRAY,nstp), 1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxHbls))
        write(*,1) vname(1,indxHbls)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif
# endif
# ifdef LMD_BKPP
! Thickness of KPP bottom boundary layer [m].

      ierr=ncdf_write(ncrst, rstHbbl, record, r_var,
     &                hbbl(START_2D_ARRAY,nstp), 1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxHbbl))
        write(*,1) vname(1,indxHbbl)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif
# endif
#endif /* SOLVE3D */

#ifdef FLOATS
! write nfloats
      ierr=nf_put_vara_int (ncrst, rstnfloats, record, 1, nfloats)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_rst :: ',
     &         'Cannot write variable ''nfloats'' into restart file, ',
     &         'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

! write Tinfo

      startTinfo(1)=1      ; countTinfo(1)=5
      startTinfo(2)=1      ; countTinfo(2)=nfloats
      startTinfo(3)=record ; countTinfo(3)=1

      ierr=nf_put_vara_FTYPE (ncrst, rstTinfo, startTinfo,
     &                        countTinfo, Tinfo)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_rst :: ',
     &           'Cannot write variable ''Tinfo'' into restart file, ',
     &           'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

! write grid level

      start(1)=1      ; count(1)=nfloats
      start(2)=record ; count(2)=1

      ierr=nf_put_vara_int(ncrst, rstfltgrd, start,count, fltgrd)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_rst :: ',
     &      'Cannot write variable ''grid level'' into restart file, ',
     &      'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

! write track

      starttrack(1)=1      ; counttrack(1)=6
      starttrack(2)=1      ; counttrack(2)=NFT+1
      starttrack(3)=1      ; counttrack(3)=nfloats
      starttrack(4)=record ; counttrack(4)=1


      do level=-1,0
        tmp(1,level)=nf
        tmp(2,level)=nfm1
        tmp(3,level)=nfm2
        tmp(4,level)=nfm3
      enddo

      do i=1,NFT+1
        do j=1,nfloats
          bufftrack(1,i,j)=track(ixgrd,tmp(i,fltgrd(j)),j)
          bufftrack(2,i,j)=track(iygrd,tmp(i,fltgrd(j)),j)
          bufftrack(3,i,j)=track(izgrd,tmp(i,fltgrd(j)),j)
          bufftrack(4,i,j)=track(ixrhs,tmp(i,fltgrd(j)),j)
          bufftrack(5,i,j)=track(iyrhs,tmp(i,fltgrd(j)),j)
          bufftrack(6,i,j)=track(izrhs,tmp(i,fltgrd(j)),j)
        enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncrst,  rsttrack,   starttrack,
     &                               counttrack, bufftrack)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_rst :: ',
     &           'Cannot write variable ''track'' into restart file, ',
     &           'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
#endif

  1   format(/1x, '### ERROR: wrt_rst :: Cannot write variable ''',
     &             A, ''' into restart file, rec =', i6, 3x,A,i4/)
      goto 100
  99  if (may_day_flag == 0) may_day_flag=3
 100  continue

! Close or synchronize netCDF file to disk to the allow other
! processes to access data immediately after it is written.

#if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close (ncrst)
      if (nrpfrst > 0 .and. record >= nrpfrst) ncrst=-1
#else
      if (nrpfrst > 0 .and. record >= nrpfrst) then
        ierr=nf_close (ncrst)
        ncrst=-1
      else
        ierr=nf_sync(ncrst)
      endif
#endif
      if (ierr == nf_noerr) then
#ifdef MPI_SILENT_MODE
        if (mynode == 0) then
#endif
          write(tstring,'(F18.8)') tdays
          i=1
          do while (i<18 .and. tstring(i:i)==' ')
            i=i+1
          enddo
          write(*,'(7x,A,1x,A,2x,A,I7,1x,A,I4,A,I4,1x,A,I3)')
     &      'wrt_rst :: wrote restart, tdays =', tstring(i:i+8),
     &      'step =', ibuff(1),  'rec =', record, '/',  nrechis
     &       MYID
#ifdef MPI_SILENT_MODE
        endif
#endif
      else
        write(*,'(/1x,2A/)')    '### ERROR: wrt_rst :: Cannot ',
     &                          'synchronize/close restart file.'
        if (may_day_flag == 0) may_day_flag=3
      endif

#if defined MPI && !defined PARALLEL_FILES
      if (mynode < NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER,
     &                        mynode+1, 1, ocean_grid_comm, ierr)
#endif
      end
