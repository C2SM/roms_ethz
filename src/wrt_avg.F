#include "cppdefs.h"
#ifdef AVERAGES
# ifndef SLICE_AVR
#  define START_3D_ARRAY_K0 START_2D_ARRAY,0
#  define START_3D_ARRAY_K1 START_2D_ARRAY,1
! Defining strings for regular avg
#  define avr avg
#  define SWRT_AVR 'WRT_AVG'
#  if defined BIOLOGY_BEC2 && defined BEC2_DIAG
#   define bec2_diag_2d_avr bec2_diag_2d_avg
#   define bec2_diag_3d_avr bec2_diag_3d_avg
#   define avrT_bec2_diag avgT_bec2_diag
#  endif
# else
!  We are playing the same redefinition preprocessor game as in def_his.F here...
#  define START_3D_ARRAY_K0 START_2D_ARRAY
#  define START_3D_ARRAY_K1 START_2D_ARRAY
! Redefining strings for slices
# define avr slavg
#  define R3DVAR r2dvar
#  define U3DVAR u2dvar
#  define V3DVAR v2dvar
#  define W3DVAR r2dvar
#  define SWRT_AVR 'WRT_SLAVG'
#  ifdef __IFC
!
!        BRUTUS CLUSTER CLUTCH
!  With the Intel compiler slice averages of PAR are
!  contain INF and other weired values.  It appears to be
!  a tiling/optimisation problem.  Other output appears 
!  not to be affected. It is a bit unnerving but as it's 
!  just a further diagnostic output we just to not output
!  PAR in slice averages when using the Intel compiler.
!
#-->   define NOPARWRT
#  endif
# endif /* ifndef SLICE_AVG */

      subroutine wrt_avr             ! Write time-averaged fields
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
#ifdef SEDIMENT_BIOLOGY
# include "bgcflux.h"
#endif
#ifdef BIOLOGY_BEC
# include "ecosys_bec.h"
# include "bec_avg.h"
#endif
#if defined BIOLOGY_BEC2 && defined BEC2_DIAG
# include "ecosys_bec2.h"
!!# include "bec2_avg.h"
#endif
!DL: for gas exchange fluxes:
#if defined BIOLOGY_NPZDOC
# include "gasexc.h"
#endif
# if defined MPI && !defined PARALLEL_FILES
#  include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode>0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,  3,
     &                       ocean_grid_comm, status, ierr)
      endif
# endif
CR      mpi_master_only print *, '--- NOTE: Executing ', SWRT_AVR

! Create/open averages file; write grid arrays, if so needed,
! check for errors, then set record within the file "record".

      call def_avr (nrecavr, ierr)
      lstr=lenstr(avrname)
      if (ierr /= nf_noerr) goto 99       !!! WARNING: Here it is
      nrecavr=max(nrecavr,1)                !!! assumed that global
      if (nrpfavr==0) then                !!! averages record index
        record=nrecavr                      !!! "nrecavr" is already
      else                                  !!! advanced by main.
        record=1+mod(nrecavr-1, nrpfavr)
      endif

! Write out time-averaged variables:
! ----- --- ------------- ----------
! Time step and record indices.

      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecavr
 
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncavr, avrTstep, start, count, ibuff)
      if (ierr /= nf_noerr) then
        write(*,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged time

      ierr=nf_put_var1_FTYPE (ncavr, avrTime, record, time_avr)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(*,1) vname(1,indxTime)(1:lvar), record MYID
        goto 99                                           !--> ERROR
      endif

! Barotropic mode variables: free-surface and 2D momentum components
! in XI-,ETA-directions.

      if (wrtavr(indxZ)) then
        ierr=nf_fwrite (zeta_avr(START_2D_ARRAY), ncavr, avrZ,
     &                                            record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(*,1) vname(1,indxTime)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavr(indxUb)) then
        ierr=nf_fwrite (ubar_avr(START_2D_ARRAY), ncavr, avrUb,
     &                                            record, u2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(*,1) vname(1,indxUb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavr(indxVb)) then
        ierr=nf_fwrite (vbar_avr(START_2D_ARRAY), ncavr, avrVb,
     &                                            record, v2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(*,1) vname(1,indxVb)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# ifdef SOLVE3D

! 3D momentum components in XI- and ETA-directions.

      if (wrtavr(indxU)) then
        ierr=nf_fwrite (u_avr(START_3D_ARRAY_K1), ncavr, avrU,
     &                                           record, U3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(*,1) vname(1,indxU)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavr(indxV)) then
        ierr=nf_fwrite (v_avr(START_3D_ARRAY_K1), ncavr, avrV,
     &                                           record, V3DVAR)
        if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxV))
          write(*,1) vname(1,indxV)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

! Tracer variables.

      do itrc=1,NT
        if (wrtavr(indxT+itrc-1)) then
          ierr=nf_fwrite (t_avr(START_3D_ARRAY_K1,itrc), ncavr,
     &                              avrT(itrc), record, R3DVAR)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(*,1) vname(1,indxT+itrc-1)(1:lvar),record MYID
            goto 99                                       !--> ERROR
          endif
        endif
      enddo
#ifdef SEDIMENT_BIOLOGY

! Sediment concentrations

      do itrc=1,NT_sed
        if (wrtavr(indxSedOrgN+itrc-1)) then
          ierr=nf_fwrite (t_sed_avr(START_2D_ARRAY,itrc), ncavr,
     &                                avrTsed(itrc), record, r2dvar)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            write(*,1) vname(1,indxSedOrgN+itrc-1)(1:lvar), record,
     &                      ierr, nf_strerror(ierr) MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_avr:  wrtT_sed  ' MYID
        endif
      enddo
#endif /* SEDIMENT_BIOLOGY */

! Density anomaly.

      if (wrtavr(indxR)) then
        ierr=nf_fwrite (rho_avr(START_3D_ARRAY_K1), ncavr, avrR,
     &                                             record, R3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(*,1) vname(1,indxR)(1:lvar), record  MYID
          goto 99                                         !--> ERROR
        endif
      endif

! S-coordinate omega vertical velocity (m/s).

      if (wrtavr(indxO)) then
        ierr=nf_fwrite (w_avr, ncavr, avrO, record, W3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(*,1) vname(1,indxO)(1:lvar), record  MYID
          goto 99                                         !--> ERROR
        endif
      endif

! True vertical velocity (m/s).

      if (wrtavr(indxW)) then
        ierr=nf_fwrite (wvl_avr, ncavr, avrW, record, R3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(*,1) vname(1,indxW)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

! Vertical eddy viscosity coefficient for momentum.

      if (wrtavr(indxAkv)) then
        ierr=nf_fwrite (akv_avr(START_3D_ARRAY_K0), ncavr,
     &                                avrAkv, record, W3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(*,1) vname(1,indxAkv)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif

! Vertical heat conductivity

      if (wrtavr(indxAkt)) then
        ierr=nf_fwrite (akt_avr(START_3D_ARRAY_K0), ncavr,
     &                                avrAkt, record, W3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(*,1) vname(1,indxAkt)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  ifdef SALINITY

! Vertical diffusion coefficient for salinity.

      if (wrtavr(indxAks)) then
        ierr=nf_fwrite (aks_avr(START_3D_ARRAY_K0), ncavr,
     &                                avrAks, record, W3DVAR)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(*,1) vname(1,indxAks)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef LMD_KPP

! Depth of planetary boundary layer [m].

      if (wrtavr(indxHbls)) then
        ierr=nf_fwrite (hbl_avr(START_2D_ARRAY), ncavr,
     &                            avrHbls, record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbls))
          write(*,1) vname(1,indxHbls)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef LMD_BKPP

! Thickness of KPP bottom boundary layer [m].

      if (wrtavr(indxHbbl)) then
        ierr=nf_fwrite (hbbl_avr(START_2D_ARRAY), ncavr,
     &                              avrHbbl, record, r2dvar)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(*,1) vname(1,indxHbbl)(1:lvar), record MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC


! pH:

      ierr=nf_fwrite (pH_avr(START_2D_ARRAY), ncavr,
     &                              avrPH, record, r2dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPH_rst))
        write(*,1) vname(1,indxPH_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

! pCO2:

      ierr=nf_fwrite (pCO2_avr(START_2D_ARRAY), ncavr,
     &                              avrPCO2, record, r2dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2_rst))
        write(*,1) vname(1,indxPCO2_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif


! pCO2air:

      ierr=nf_fwrite (pCO2air_avr(START_2D_ARRAY), ncavr,
     &                              avrPCO2air, record, r2dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2air_rst))
        write(*,1) vname(1,indxPCO2air_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

! incoming PAR:

      ierr=nf_fwrite (PARinc_avr(START_2D_ARRAY), ncavr,
     &                              avrPARinc, record, r2dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPARinc_rst))
        write(*,1) vname(1,indxPARinc_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
#  ifndef NOPARWRT

! PAR:

      ierr=nf_fwrite (PAR_avr(START_3D_ARRAY_K1), ncavr,
     &                              avrPAR, record, R3DVAR)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxPAR_rst))
        write(*,1) vname(1,indxPAR_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
#  endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
# if defined BIOLOGY_BEC2 && defined BEC2_DIAG

! 2d diagnostics:

      do itrc=1,nr_bec2_diag_2d
         ierr=nf_fwrite (bec2_diag_2d_avr(START_2D_ARRAY,itrc), ncavr,
     &                                avrT_bec2_diag(itrc), record, r2dvar)
         if (ierr /= nf_noerr) then
            lvar=lenstr(vname_bec2_diag_2d(1,itrc))
            write(*,1) vname_bec2_diag_2d(1,itrc)(1:lvar),
     &                      record, ierr MYID
            goto 99                                       !--> ERROR
         endif
      enddo

! 3d diagnostics:

      do itrc=1,nr_bec2_diag_3d
         ierr=nf_fwrite (bec2_diag_3d_avr(START_3D_ARRAY_K1,itrc), ncavr,
     &                                avrT_bec2_diag(nr_bec2_diag_2d+itrc), record, R3DVAR)
         if (ierr /= nf_noerr) then
            lvar=lenstr(vname_bec2_diag_3d(1,itrc))
            write(*,1) vname_bec2_diag_3d(1,itrc)(1:lvar),
     &                      record, ierr MYID
            goto 99                                       !--> ERROR
         endif
      enddo
# endif /* BIOLOGY_BEC2 */

# if defined WRITE_DEPTHS && !defined SLICE_AVR

! depths of rho points:

      ierr=nf_fwrite (z_r_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgz_r, record, r3dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxz_r))
        write(*,1) vname(1,indxz_r)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

! depths at w points:

      ierr=nf_fwrite (z_w_avg(START_2D_ARRAY,0), ncidavg,
     &                              avgz_w, record, w3dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxz_w))
        write(*,1) vname(1,indxz_w)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

! thicknesses of sigma layers:

      ierr=nf_fwrite (Hz_avg(START_2D_ARRAY,1), ncidavg,
     &                              avgHz, record, r3dvar)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxHz))
        write(*,1) vname(1,indxHz)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif /* WRITE_DEPTHS */

! Gas exchange fluxes:

# if defined BIOLOGY_NPZDOC && defined OXYGEN
      do itrc = 1, NumGasExcTerms
         ierr = nf_fwrite(GasExcFlux_avr(START_2D_ARRAY,itrc), 
     &        ncavr, avrGasExcFlux(itrc), 
     &        record, r2dvar)
         if (ierr /= nf_noerr) then
            lvar=lenstr(vname_GasExcFlux(1,itrc))
            write(*,1) 
     &           vname_GasExcFlux(1,itrc)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
# endif /* BIOLOGY_NPZDOC && OXYGEN */
# endif /* SOLVE3D */

  1   format(/1x, '### ERROR: wrt_avr :: Cannot write variable ''',
     &             A, ''' into averages file, rec =', i6, 3x,A,i4)
      goto 100
  99  if (may_day_flag==0) may_day_flag=3
 100  continue

! Close or synchronize netCDF file to disk to allow other
! processes to access data immediately after it is written.

# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(ncavr)
      if (nrpfavr>0 .and. record>=nrpfavr) ncavr=-1
# else
      if (nrpfavr>0 .and. record>=nrpfavr) then
        ierr=nf_close(ncavr)
        ncavr=-1
      else
        ierr=nf_sync(ncavr)
      endif
# endif
      if (ierr == nf_noerr) then
# ifdef MPI_SILENT_MODE
        if (mynode==0) then
# endif
          write(tstring,'(F18.8)') time_avr*sec2day
          i=1
          do while (i<18 .and. tstring(i:i)==' ')
            i=i+1
          enddo
          write(*,'(7x,2A,1x,A,1x,A,I4,A,I4,1x,A,I3)') 'wrt_avr :: ',
     &      'wrote averages for time_avr =', tstring(i:i+8), 'rec =',
     &       record, '/', nrecavr  MYID
# ifdef MPI_SILENT_MODE
        endif
# endif
      else
        write(*,'(/1x,2A/)')      '### ERROR: SWRT_AVR :: Cannot ',
     &                         'synchronize/close averages file.'
        if (may_day_flag==0) may_day_flag=3
      endif
# if defined MPI && !defined PARALLEL_FILES
      if (mynode < NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER,
     &                           mynode+1, 3, ocean_grid_comm, ierr)
# endif
      end
# ifndef SLICE_AVR
#  undef START_3D_ARRAY_K0
#  undef START_3D_ARRAY_K1
#  undef avr
#  undef SWRT_AVR
#  undef WRTROUTINE
#  undef NOPARWRT
#  if defined  SLICE_AVG && !defined SLICE_AVR /* also create wrt_slice_avg */
#   define SLICE_AVR
#   include "wrt_avg.F"
#  endif
# endif
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
