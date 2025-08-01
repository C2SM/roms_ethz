#include "cppdefs.h"
                               ! Writes requested model
      subroutine wrt_his       ! fields at requested levels
                               ! into history netCDF file.
      implicit none
#include "param.h"
#include "ncvars.h"
#include "scalars.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "eos_vars.h"
#include "mixing.h"
#include "netcdf.inc"
#ifdef SEDIMENT_BIOLOGY
# include "bgcflux.h"
#endif
#if defined WRITE_TEMP_REST || defined WRITE_SALT_REST || \
        defined WRITE_HEATFLX || defined WRITE_CO2FLX ||\
        defined WRITE_N2OFLX
# include "forces.h"
#endif
#ifdef BIOLOGY_BEC
# include "ecosys_bec.h"
#endif
#ifdef BIOLOGY_BEC2
# include "ecosys_bec2.h"
#endif
#ifdef BIOLOGY_NPZDOC
# include "gasexc.h"
#endif
      integer ierr,  record, i, lstr, lvar, lenstr
     &   , start(2), count(2), ibuff(iaux), ncdf_write
      character(len=18) tstring
#ifdef SOLVE3D
      integer tile, itrc, j,k
# include "work.h"
#endif
#if defined MPI && !defined PARALLEL_FILES
# include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode > 0) call MPI_Recv (blank, 1, MPI_INTEGER,
     &            mynode-1, 2, ocean_grid_comm, status, ierr)
#endif


! Create/open history and set record index within the file.

      call def_his(nrechis, ierr)
      if (ierr /= nf_noerr) goto 99
      lstr=lenstr(hisname)
      nrechis=max(nrechis,1)              !!! WARNING: Once time
      if (nrpfhis == 0) then              !!! stepping has been
        record=nrechis                    !!! started, it is assumed
      else                                !!! that the global history
        record=1+mod(nrechis-1, nrpfhis)  !!! record index "nrechis"
      endif                               !!! is advanced by main.


c-#define CR
CR      write(*,*) 'wrt_his: Entry  ' MYID

! Write out evolving model variables:
! ----- --- -------- ----- ----------

      ibuff(1)=iic-1  ; ibuff(2)=nrecrst    ! Time step and
      ibuff(4:iaux)=0 ; ibuff(3)=nrechis    ! record numbers.
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

      ierr=nf_put_vara_int(nchis, hisTstep, start, count, ibuff)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,3A,i6/11x,A,3x,A,i4/)') '### ERROR: wrt_his :: ',
     &       'Cannot write variable ''time_step'' into history file, ',
     &       'rec =', record, nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

! Time: Note that (1) time is always double precision; and (2) which
! one to write into the file, time or tdays, is decided by attribute
! "units" stored as vname(3,indxTime).  When computng time is always
! in seconds, however it is more convenient to have it in days in all
! the files.


      if (vname(3,indxTime)(1:6) == 'second') then
        ierr=nf_put_var1_double(nchis, hisTime, record, time)
      else
        ierr=nf_put_var1_double(nchis, hisTime, record, tdays)
      endif
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
        ierr=ncdf_write(nchis, hisZ,  record,  r_var,
     &                  zeta(START_2D_ARRAY,knew), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(*,1) vname(1,indxZ)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtZ  ' MYID
      endif
      if (wrthis(indxUb)) then
        ierr=ncdf_write(nchis, hisUb,  record,  u_var,
     &                  ubar(START_2D_ARRAY,knew), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(*,1) vname(1,indxUb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his: wrtUBAR' MYID
      endif
      if (wrthis(indxVb)) then
        ierr=ncdf_write(nchis, hisVb,  record,  v_var,
     &                  vbar(START_2D_ARRAY,knew), 1)
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
        ierr=ncdf_write(nchis, hisU,  record,  u_var,
     &                  u(START_2D_ARRAY,1,nstp), N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(*,1) vname(1,indxU)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtU  ' MYID
      endif
      if (wrthis(indxV)) then
        ierr=ncdf_write(nchis, hisV,  record,  v_var,
     &                  v(START_2D_ARRAY,1,nstp), N)
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
          ierr=ncdf_write(nchis,  hisT(itrc), record, r_var,
     &                    t(START_2D_ARRAY,1,nstp,itrc), N)

          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(*,1) vname(1,indxT+itrc-1)(1:lvar),
     &                 record MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_his:  wrtT  ' MYID
        endif
      enddo
#ifdef SEDIMENT_BIOLOGY
!
! Sediment concentrations

      do itrc=1,NT_sed
        if (wrthis(indxSedOrgN+itrc-1)) then
          ierr=ncdf_write(nchis,  hisTsed(itrc), record, r_var,
     &                    t_sed(START_2D_ARRAY,itrc), 1)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            write(*,1) vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &                      record MYID
            goto 99                                       !--> ERROR
          endif
        endif
      enddo
#endif /* SEDIMENT_BIOLOGY */

! Density anomaly.

      if (wrthis(indxR)) then
# ifdef SPLIT_EOS
        ierr=ncdf_write(nchis, hisR, record, r_var, rho1, N)
# else
        ierr=ncdf_write(nchis, hisR, record, r_var, rho,  N)
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
        ierr=ncdf_write(nchis, hisO, record, r_var, work, N+1)
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
        ierr=ncdf_write(nchis, hisW, record, r_var, work, N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(*,1) vname(1,indxW)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtW  ' MYID
      endif

! Vertical viscosity coefficient.

      if (wrthis(indxAkv)) then
        ierr=ncdf_write(nchis, hisAkv, record, r_var, Akv, N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(*,1) vname(1,indxAkv)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAkv' MYID
      endif

! Vertical diffusion coefficient for potential temperature.

      if (wrthis(indxAkt)) then
        ierr=ncdf_write(nchis,   hisAkt,   record,  r_var,
     &                  Akt(START_2D_ARRAY,0,itemp), N+1)
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
        ierr=ncdf_write(nchis,  hisAks,   record,   r_var,
     &                  Akt(START_2D_ARRAY,0,isalt), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(*,1) vname(1,indxAks)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAks' MYID
      endif
# endif /* SALINITY */
# ifdef LMD_KPP
! Depth of planetary boundary layer [m].

      if (wrthis(indxHbls)) then
        ierr=ncdf_write(nchis, hisHbls, record, r_var,
     &                  hbls(START_2D_ARRAY,nstp), 1)
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
        ierr=ncdf_write(nchis, hisHbbl, record, r_var,
     &                  hbbl(START_2D_ARRAY,nstp), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(*,1) vname(1,indxHbbl)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtHbBL' MYID
      endif
# endif
! AH16:
# if defined WRITE_CO2FLX

! Surface CO2 flux

      if (wrthis(indxCO2flx)) then
        ierr=ncdf_write(nchis, hisCO2flx, record, r_var,
     &                  CO2flx(START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxCO2flx))
          write(*,1) vname(1,indxCO2flx)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
! AH16:
# if defined WRITE_HEATFLX

! Surface heat flux

      if (wrthis(indxHeatflx)) then
        ierr=ncdf_write(nchis, hisHeatflx, record, r_var,
     &                  Heatflx(START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHeatflx))
          write(*,1) vname(1,indxHeatflx)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# if defined WRITE_TEMP_REST

! Restoring flux for temperature

      if (wrthis(indxRestflxTemp)) then
        ierr=ncdf_write(nchis, hisRestflxTemp, record, r_var,
     &                  RestflxTemp(START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxRestflxTemp))
          write(*,1) vname(1,indxRestflxTemp)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# if defined WRITE_SALT_REST

! Restoring flux for salinity

      if (wrthis(indxRestflxSalt)) then
        ierr=ncdf_write(nchis, hisRestflxSalt, record, r_var,
     &                  RestflxSalt(START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxRestflxSalt))
          write(*,1) vname(1,indxRestflxSalt)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# if defined BIOLOGY_BEC || (defined BIOLOGY_NPZDOC && \
    defined OXYGEN && defined CARBON)

! Write out averaged pH:

      ierr=ncdf_write(nchis, hisPH, record, r_var,
     &                  ph_hist(START_2D_ARRAY), 1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPH_rst))
        write(*,1) vname(1,indxPH_rst)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif

! Write out averaged pCO2:

      ierr=ncdf_write(nchis, hisPCO2, record, r_var,
     &                  pCO2sw(START_2D_ARRAY), 1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2_rst))
        write(*,1) vname(1,indxPCO2_rst)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif


! Write out averaged pCO2air:

      ierr=ncdf_write(nchis, hisPCO2air, record, r_var,
     &                  pCO2air(START_2D_ARRAY), 1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2air_rst))
        write(*,1) vname(1,indxPCO2air_rst)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif
# endif /* BIOLOGY_BEC || (BIOLOGY_NPZDOC && OXYGEN && CARBON) */
# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC

! PARinc:

      ierr=ncdf_write(nchis, hisPARinc, record, r_var,
     &                  PARinc(START_2D_ARRAY), 1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPARinc_rst))
        write(*,1) vname(1,indxPARinc_rst)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif

! PAR:

      ierr=ncdf_write(nchis, hisPAR, record, r_var,
     &                  PAR(START_2D_ARRAY,1), N)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPAR_rst))
        write(*,1) vname(1,indxPAR_rst)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif
# endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */

# if defined BIOLOGY_BEC2 && defined BEC2_DIAG

! 2d diagnostics:

      do itrc=1,nr_bec2_diag_2d
         if (wrtavg_bec2_diag_2d(itrc)) then
           ierr=ncdf_write(nchis, hisT_bec2_diag(itrc), record, r_var,
     &                  bec2_diag_2d(START_2D_ARRAY,itrc), 1)   ! Pbec2 before
           if (ierr /= nf_noerr) then
            lvar=lenstr(vname_bec2_diag_2d(1,itrc))
            write(*,1) vname_bec2_diag_2d(1,itrc)(1:lvar), record MYID
            goto 99                                         !--> ERROR
           endif
         endif
      end do

! 3d diagnostics:

      do itrc=1,nr_bec2_diag_3d
         if (wrtavg_bec2_diag_3d(itrc)) then
           ierr=ncdf_write(nchis, hisT_bec2_diag(nr_bec2_diag_2d+itrc), 
     &                  record, r_var, bec2_diag_3d(START_2D_ARRAY,1,itrc), N)
           if (ierr /= nf_noerr) then
            lvar=lenstr(vname_bec2_diag_3d(1,itrc))
            write(*,1) vname_bec2_diag_3d(1,itrc)(1:lvar), record MYID
            goto 99                                         !--> ERROR
           endif
         endif
      end do
# endif /* BIOLOGY_BEC2 && BEC2_DIAG */

!DL:
# ifdef WRITE_DEPTHS

! depths of rho points:

      ierr=ncdf_write(nchis, hisz_r, record, r_var,
     &                  z_r(START_2D_ARRAY,1), N)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxz_r))
        write(*,1) vname(1,indxz_r)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif

! depths at w points:

      ierr=ncdf_write(nchis, hisz_w, record, r_var,
     &                  z_w(START_2D_ARRAY,0), N+1)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxz_w))
        write(*,1) vname(1,indxz_w)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif

! thicknesses of sigma layers:

      ierr=ncdf_write(nchis, hisHz, record, r_var,
     &                  Hz(START_2D_ARRAY,1), N)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxHz))
        write(*,1) vname(1,indxHz)(1:lvar), record MYID
        goto 99                                         !--> ERROR
      endif
# endif /* WRITE_DEPTHS */

! gas exchange fluxes:

# if defined BIOLOGY_NPZDOC && defined OXYGEN
      do itrc = 1, NumGasExcTerms
         ierr=ncdf_write(nchis, hisGasExcFlux(itrc), record, r_var,
     &                  GasExcFlux(START_2D_ARRAY,itrc), 1)
         if (ierr /= nf_noerr) then
            lvar=lenstr(vname_GasExcFlux(1,itrc))
            write(*,1) 
     &           vname_GasExcFlux(1,itrc)(1:lvar), 
     &           record MYID 
            goto 99
         end if
      end do
# endif /* BIOLOGY_NPZDOC && OXYGEN */

#endif /* SOLVE3D */
  1   format(/1x, '### ERROR: wrt_his :: Cannot write variable ''',
     &              A, ''' into history file, rec =', i6, 3x,A,i4)
      goto 100
  99  if (may_day_flag == 0) may_day_flag=3
 100  continue

! Close or synchronize netCDF file to the disk to allow other
! processes to access data immediately after it is written.

#if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close (nchis)
      if (nrpfhis > 0 .and. record >= nrpfhis) nchis=-1
#else
      if (nrpfhis > 0 .and. record >= nrpfhis) then
        ierr=nf_close (nchis)
        nchis=-1
      else
        ierr=nf_sync(nchis)
      endif
#endif
      if (ierr == nf_noerr) then
#ifdef MPI_SILENT_MODE
        if (mynode == 0) then
#endif
          write(tstring,'(F18.8)') tdays
          i=1
          do while (i < 18 .and. tstring(i:i) == ' ')
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
        if (may_day_flag == 0) may_day_flag=3
      endif
#if defined MPI && !defined PARALLEL_FILES
      if (mynode < NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER,
     &                           mynode+1, 2, ocean_grid_comm, ierr)
#endif
      end
