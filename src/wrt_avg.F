#include "cppdefs.h"
#ifdef AVERAGES
                                     ! Write time-averaged
      subroutine wrt_avg             ! fields into averages
                                     ! netCDF file.
      implicit none
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite
# ifdef SOLVE3D
     &            , itrc
# endif
# include "param.h"
# include "scalars.h"
# include "averages.h"
# include "ncvars.h"
# include "netcdf.inc"
#ifdef SEDIMENT_BIOLOGY
# include "bgcflux.h"
#endif
#ifdef BIOLOGY_BEC
# include "ecosys_bec.h"
# include "bec_avg.h"
#endif
# if defined MPI && !defined PARALLEL_FILES
#  include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 3, ocean_grid_comm, status, ierr)
      endif
# endif
!
! Create/open averages file; write grid arrays, if so needed,
! check for errors, then set record within the file "record".
!
      call def_avg (ncidavg, nrecavg, ierr)
      lstr=lenstr(avgname)
      if (ierr .ne. nf_noerr) goto 99       !!! WARNING: Here it is
      nrecavg=max(nrecavg,1)                !!! assumed that global
      if (nrpfavg.eq.0) then                !!! averages record index
        record=nrecavg                      !!! "nrecavg" is already
      else                                  !!! advanced by main.
        record=1+mod(nrecavg-1, nrpfavg)
      endif
!
! Write out time-averaged variables:
! ----- --- ------------- ----------
!
! Time step and record indices.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecavg
 
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidavg, avgTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged time
!
      ierr=nf_put_var1_FTYPE (ncidavg, avgTime, record, time_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Barotropic mode variables: free-surface and 2D momentum
! components in XI-,ETA-directions.
!
      if (wrtavg(indxZ)) then
        ierr=nf_fwrite (zeta_avg(START_2D_ARRAY), ncidavg, avgZ,
     &                                            record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
 
      if (wrtavg(indxUb)) then
        ierr=nf_fwrite (ubar_avg(START_2D_ARRAY), ncidavg, avgUb,
     &                                            record, u2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(stdout,1) vname(1,indxUb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
 
      if (wrtavg(indxVb)) then
        ierr=nf_fwrite (vbar_avg(START_2D_ARRAY), ncidavg, avgVb,
     &                                            record, v2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(stdout,1) vname(1,indxVb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
# ifdef SOLVE3D
!
! 3D momentum components in XI- and ETA-directions.
!
      if (wrtavg(indxU)) then
        ierr=nf_fwrite (u_avg(START_2D_ARRAY,1), ncidavg, avgU,
     &                                           record, u3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(stdout,1) vname(1,indxU)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
 
      if (wrtavg(indxV)) then
        ierr=nf_fwrite (v_avg(START_2D_ARRAY,1), ncidavg, avgV,
     &                                           record, v3dvar)
        if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxV))
          write(stdout,1) vname(1,indxV)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        if (wrtavg(indxT+itrc-1)) then
          ierr=nf_fwrite (t_avg(START_2D_ARRAY,1,itrc), ncidavg,
     &                              avgT(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                      record, ierr MYID
            goto 99                                       !--> ERROR
          endif
        endif
      enddo
#ifdef SEDIMENT_BIOLOGY
!
! Sediment concentrations
!
      do itrc=1,NT_sed
        if (wrtavg(indxSedOrgN+itrc-1)) then
          ierr=nf_fwrite (t_sed_avg(START_2D_ARRAY,itrc), ncidavg,
     &                                avgTsed(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            write(stdout,1) vname(1,indxSedOrgN+itrc-1)(1:lvar), record,
     &                      ierr, nf_strerror(ierr) MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_avg:  wrtT_sed  ' MYID
        endif
      enddo
#endif /* SEDIMENT_BIOLOGY */
!
! Density anomaly.
!
      if (wrtavg(indxR)) then
        ierr=nf_fwrite (rho_avg(START_2D_ARRAY,1), ncidavg, avgR,
     &                                             record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(stdout,1) vname(1,indxR)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      if (wrtavg(indxO)) then
        ierr=nf_fwrite (w_avg, ncidavg, avgO, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(stdout,1) vname(1,indxO)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
! Write out true vertical velocity (m/s).
!
      if (wrtavg(indxW)) then
        ierr=nf_fwrite(w_mean, ncidavg, avgW, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(stdout,1) vname(1,indxW)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out vertical eddy viscosity coefficient for momentum.
!
      if (wrtavg(indxAkv)) then
        ierr=nf_fwrite (akv_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgAkv, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(stdout,1) vname(1,indxAkv)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      if (wrtavg(indxAkt)) then
        ierr=nf_fwrite (akt_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgAkt, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(stdout,1) vname(1,indxAkt)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
! Write out diffusion coefficient due to vertical velocity shear:
!
      if (wrtavg(indxRich)) then
        ierr=nf_fwrite (rich_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgRich, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxRich))
          write(stdout,1) vname(1,indxRich)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
! Write out local gradient Richardson number:
!
      if (wrtavg(indxRichN)) then
        ierr=nf_fwrite (richN_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgRichN, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxRichN))
          write(stdout,1) vname(1,indxRichN)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
# ifdef SALINITY
!
!  Write out vertical diffusion coefficient for salinity.
!
      if (wrtavg(indxAks)) then
        ierr=nf_fwrite (aks_avg(START_2D_ARRAY,0), ncidavg,
     &                                avgAks, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(stdout,1) vname(1,indxAks)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# ifdef LMD_KPP
!
!  Write out depth of planetary boundary layer (m).
!
      if (wrtavg(indxHbl)) then
        ierr=nf_fwrite (hbl_avg(START_2D_ARRAY), ncidavg,
     &                              avgHbl, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHbl))
          write(stdout,1) vname(1,indxHbl)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# ifdef LMD_BKPP
!
!  Write out depth of bottom boundary layer (m).
!
      if (wrtavg(indxHbbl)) then
        ierr=nf_fwrite (hbbl_avg(START_2D_ARRAY), ncidavg,
     &                              avgHbbl, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(stdout,1) vname(1,indxHbbl)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#  if defined CH_CARBON_DEPTH

!
! Write out averaged CO2STAR_DEPTH:
!
      ierr=nf_fwrite (CO2STARd_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgCO2STARd, record, r3dvar)
      write(*,*) 'ierr=' , ierr
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxCO2STARd_rst))
        write(stdout,1) vname(1,indxCO2STARd_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

!
! Write out averaged HCO3_DEPTH:
!
      ierr=nf_fwrite (HCO3d_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgHCO3d, record, r3dvar) 
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxHCO3d_rst))
        write(stdout,1) vname(1,indxHCO3d_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

!
! Write out averaged CO3_DEPTH:
!
      ierr=nf_fwrite (CO3d_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgCO3d, record, r3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxCO3d_rst))
        write(stdout,1) vname(1,indxCO3d_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif


!
! Write out averaged PH_DEPTH:
!
      ierr=nf_fwrite (PHd_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgPHd, record, r3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPHd_rst))
        write(stdout,1) vname(1,indxPHd_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

#  endif /* CH_CARBON_DEPTH */

!
! Write out averaged pH:
!
      ierr=nf_fwrite (pH_avg(START_2D_ARRAY), ncidavg,
     &                              avgPH, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPH_rst))
        write(stdout,1) vname(1,indxPH_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
!
! Write out averaged pCO2:
!
      ierr=nf_fwrite (pCO2_avg(START_2D_ARRAY), ncidavg,
     &                              avgPCO2, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2_rst))
        write(stdout,1) vname(1,indxPCO2_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

!
! Write out averaged pCO2air:
!
      ierr=nf_fwrite (pCO2air_avg(START_2D_ARRAY), ncidavg,
     &                              avgPCO2air, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2air_rst))
        write(stdout,1) vname(1,indxPCO2air_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
!
! incoming PAR:
!
      ierr=nf_fwrite (PARinc_avg(START_2D_ARRAY), ncidavg,
     &                              avgPARinc, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPARinc_rst))
        write(stdout,1) vname(1,indxPARinc_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
!
! PAR:
!
      ierr=nf_fwrite (PAR_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgPAR, record, r3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPAR_rst))
        write(stdout,1) vname(1,indxPAR_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
!DL:
# ifdef WRITE_DEPTHS
!
! depths of rho points:
!
      ierr=nf_fwrite (z_r_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgz_r, record, r3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxz_r))
        write(stdout,1) vname(1,indxz_r)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
!
! depths at w points:
!
      ierr=nf_fwrite (z_w_avg(START_2D_ARRAY,0), ncidavg,
     &                              avgz_w, record, w3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxz_w))
        write(stdout,1) vname(1,indxz_w)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
!
! thicknesses of sigma layers:
!
      ierr=nf_fwrite (Hz_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgHz, record, r3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxHz))
        write(stdout,1) vname(1,indxHz)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif /* WRITE_DEPTHS */
# endif /* SOLVE3D */
  1   format(/' WRT_AVG - ERROR while writing variable(',1x,a,1x,
     &               ')into averages file.',/,11x,'Time record:',
     &                      i6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100
  99  may_day_flag=3
 100  continue
!
! Close or synchronize netCDF file to disk to allow other
! processes to access data immediately after it is written.
!
# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(ncidavg)
      if (nrpfavg.gt.0 .and. record.ge.nrpfavg) ncidavg=-1
# else
      if (nrpfavg.gt.0 .and. record.ge.nrpfavg) then
        ierr=nf_close(ncidavg)
        ncidavg=-1
      else
        ierr=nf_sync(ncidavg)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        !JDS for very long steady states simulations record is written
        !in I10 format instead of I4      
        !write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_AVG -- wrote ',
        write(stdout,'(6x,A,2(A,I10,1x),A,I3)') 'WRT_AVG -- wrote ',
     &            'averaged fields into time record =', record, '/',
     &             nrecavg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_AVG ERROR: Cannot ',
     &             'synchronize/close averages netCDF file.'
        may_day_flag=3
      endif
 
# if defined MPI && !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER, 
     &                            mynode+1, 3, ocean_grid_comm, ierr)
# endif
      return
      end
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
 
