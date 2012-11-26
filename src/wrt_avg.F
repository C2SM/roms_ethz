#include "cppdefs.h"
#ifdef AVERAGES
# ifndef SLICE_AVR
#  define START_3D_ARRAY_K0 START_2D_ARRAY,0
#  define START_3D_ARRAY_K1 START_2D_ARRAY,1
! Defining strings for regular avg
#  define wrt_avr wrt_avg
#  define def_avr def_avg
#  define avrTstep avgTstep 
#  define time_avr time_avg
#  define avrTime avgTime 
#  define u_avr u_avg
#  define avrU avgU
#  define v_avr v_avg
#  define avrV avgV
#  define rho_avr rho_avg
#  define avrR avgR
#  define t_avr t_avg
#  define avrT avgT
#  define w_avr w_avg
#  define wt_avr wt_avg
#  define avrO avgO
#  define avrW avgW
#  define Akv_avr Akv_avg
#  define Akt_avr Akt_avg
#  define Aks_avr Aks_avg
#  define avrAkv avgAkv
#  define avrAkt avgAkt
#  define avrAks avgAks
#  define rich_avr rich_avg
#  define avrRich avgRich
#  define richN_avr richN_avg
#  define avrRichN avgRichN
#  define ncidavr ncidavg
#  define ncrecavr ncrecavg 
#  define nrpfavr nrpfavg 
#  define nrecavr nrecavg 
#  define avrname avgname
#  define wrtavr wrtavg
#  define SWRT_AVR 'WRT_AVG'
# else
!  We are playing the same redefinition CPP game as in def_his.F here...
#  define START_3D_ARRAY_K0 START_2D_ARRAY
#  define START_3D_ARRAY_K1 START_2D_ARRAY
! Redefining strings for slices
#  define wrt_avr wrt_slice_avg
#  define def_avr def_slavg
#  define avrTstep slavgTstep 
#  define time_avr time_slavg
#  define avrTime slavgTime 
#  define u_avr u_slavg
#  define avrU slavgU
#  define v_avr v_slavg
#  define avrV slavgV
#  define rho_avr rho_slavg
#  define avrR slavgR
#  define t_avr t_slavg
#  define avrT slavgT
#  define w_avr w_slavg
#  define wt_avr wt_slavg
#  define avrO slavgO
#  define avrW slavgW
#  define Akv_avr Akv_slavg
#  define Akt_avr Akt_slavg
#  define Aks_avr Aks_slavg
#  define avrAkv slavgAkv
#  define avrAkt slavgAkt
#  define avrAks slavgAks
#  define rich_avr rich_slavg
#  define avrRich slavgRich
#  define richN_avr richN_slavg
#  define avrRichN slavgRichN
#  define ncidavr ncidslavg
#  define ncrecavr ncrecslavg 
#  define nrpfavr nrpfslavg 
#  define nrecavr nrecslavg 
#  define avrname slavgname
#  define wrtavr wrtslavg
#  define R3DVAR r2dvar
#  define U3DVAR u2dvar
#  define V3DVAR v2dvar
#  define W3DVAR r2dvar
#  define SWRT_AVR 'WRT_SLAVG'
# endif /* ifndef SLICE_AVG */
                                     ! Write time-averaged
      subroutine wrt_avr             ! fields into averages
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
CR      MPI_master_only print *, '--- NOTE: Executing ', SWRT_AVR
!
! Create/open averages file; write grid arrays, if so needed,
! check for errors, then set record within the file "record".
!
      call def_avr (ncidavr, nrecavr, ierr)
      lstr=lenstr(avrname)
      if (ierr .ne. nf_noerr) goto 99       !!! WARNING: Here it is
      nrecavr=max(nrecavr,1)                !!! assumed that global
      if (nrpfavr.eq.0) then                !!! averages record index
        record=nrecavr                      !!! "nrecavg" is already
      else                                  !!! advanced by main.
        record=1+mod(nrecavr-1, nrpfavr)
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
      ibuff(4)=nrecavr
 
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidavr, avrTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged time
!
      ierr=nf_put_var1_FTYPE (ncidavr, avrTime, record, time_avr)
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
      if (wrtavr(indxZ)) then
        ierr=nf_fwrite (zeta_avg(START_2D_ARRAY), ncidavr, avgZ,
     &                                            record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
 
      if (wrtavr(indxUb)) then
        ierr=nf_fwrite (ubar_avg(START_2D_ARRAY), ncidavr, avgUb,
     &                                            record, u2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(stdout,1) vname(1,indxUb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
 
      if (wrtavr(indxVb)) then
        ierr=nf_fwrite (vbar_avg(START_2D_ARRAY), ncidavr, avgVb,
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
      if (wrtavr(indxU)) then
        ierr=nf_fwrite (u_avr(START_3D_ARRAY_K1), ncidavr, avrU,
     &                                           record, U3DVAR)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(stdout,1) vname(1,indxU)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
 
      if (wrtavr(indxV)) then
        ierr=nf_fwrite (v_avr(START_3D_ARRAY_K1), ncidavr, avrV,
     &                                           record, V3DVAR)
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
        if (wrtavr(indxT+itrc-1)) then
          ierr=nf_fwrite (t_avr(START_3D_ARRAY_K1,itrc), ncidavr,
     &                              avrT(itrc), record, R3DVAR)
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
        if (wrtavr(indxSedOrgN+itrc-1)) then
          ierr=nf_fwrite (t_sed_avg(START_2D_ARRAY,itrc), ncidavr,
     &                                avgTsed(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            write(stdout,1) vname(1,indxSedOrgN+itrc-1)(1:lvar), record,
     &                      ierr, nf_strerror(ierr) MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_avr:  wrtT_sed  ' MYID
        endif
      enddo
#endif /* SEDIMENT_BIOLOGY */
!
! Density anomaly.
!
      if (wrtavr(indxR)) then
        ierr=nf_fwrite (rho_avr(START_3D_ARRAY_K1), ncidavr, avrR,
     &                                             record, R3DVAR)
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
      if (wrtavr(indxO)) then
        ierr=nf_fwrite (w_avr, ncidavr, avrO, record, W3DVAR)
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
      if (wrtavr(indxW)) then
        ierr=nf_fwrite(wt_avr, ncidavr, avrW, record, R3DVAR)
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
      if (wrtavr(indxAkv)) then
        ierr=nf_fwrite (Akv_avr(START_3D_ARRAY_K0), ncidavr,
     &                                avrAkv, record, W3DVAR)
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
      if (wrtavr(indxAkt)) then
        ierr=nf_fwrite (Akt_avr(START_3D_ARRAY_K0), ncidavr,
     &                                avrAkt, record, W3DVAR)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(stdout,1) vname(1,indxAkt)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
# ifdef KPP_DIAGNOSE
!
! Write out diffusion coefficient due to vertical velocity shear:
!
      if (wrtavr(indxRich)) then
        ierr=nf_fwrite (rich_avr(START_2D_ARRAY,0), ncidavr,
     &                                avrRich, record, W3DVAR)
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
      if (wrtavr(indxRichN)) then
        ierr=nf_fwrite (richN_avr(START_2D_ARRAY,0), ncidavr,
     &                                avrRichN, record, W3DVAR)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxRichN))
          write(stdout,1) vname(1,indxRichN)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
# endif /* KPP_DIAGNOSE */
# ifdef SALINITY
!
!  Write out vertical diffusion coefficient for salinity.
!
      if (wrtavr(indxAks)) then
        ierr=nf_fwrite (Aks_avr(START_3D_ARRAY_K0), ncidavr,
     &                                avrAks, record, W3DVAR)
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
      if (wrtavr(indxHbl)) then
        ierr=nf_fwrite (hbl_avg(START_2D_ARRAY), ncidavr,
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
      if (wrtavr(indxHbbl)) then
        ierr=nf_fwrite (hbbl_avg(START_2D_ARRAY), ncidavr,
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
      ierr=nf_fwrite (CO2STARd_avg(START_2D_ARRAY,1), ncidavr,
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
      ierr=nf_fwrite (HCO3d_avg(START_2D_ARRAY,1), ncidavr,
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
      ierr=nf_fwrite (CO3d_avg(START_2D_ARRAY,1), ncidavr,
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
      ierr=nf_fwrite (PHd_avg(START_2D_ARRAY,1), ncidavr,
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
      ierr=nf_fwrite (pH_avg(START_2D_ARRAY), ncidavr,
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
      ierr=nf_fwrite (pCO2_avg(START_2D_ARRAY), ncidavr,
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
      ierr=nf_fwrite (pCO2air_avg(START_2D_ARRAY), ncidavr,
     &                              avgPCO2air, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPCO2air_rst))
        write(stdout,1) vname(1,indxPCO2air_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
!
! PAR:
!
      ierr=nf_fwrite (PARinc_avg(START_2D_ARRAY), ncidavr,
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
      ierr=nf_fwrite (PAR_avg(START_2D_ARRAY,1), ncidavr,
     &                              avgPAR, record, r3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxPAR_rst))
        write(stdout,1) vname(1,indxPAR_rst)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
# endif /* SOLVE3D */
  1   format(/SWRT_AVR,' - ERROR while writing variable(',1x,a,1x,
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
      ierr=nf_close(ncidavr)
      if (nrpfavr.gt.0 .and. record.ge.nrpfavr) ncidavr=-1
# else
      if (nrpfavr.gt.0 .and. record.ge.nrpfavr) then
        ierr=nf_close(ncidavr)
        ncidavr=-1
      else
        ierr=nf_sync(ncidavr)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        if (nrecavr .lt.10000) then
             MPI_master_only write(stdout,
     &                '(I8,1x,A,2(A,I4,1x),A,I3)') iic-1, 
     &          SWRT_AVR
     &            ,': output fields written to record ='
     &            , record, '/', nrecavr  
CR     &              MYID
        else
             MPI_master_only write(stdout,
     &          '(I8,1x,A,2(A,I10,1x),A,I3)') iic-1, 
     &          SWRT_AVR
     &            ,': output fields written to record ='
     &            , record, '/', nrecavr  
CR     &              MYID
        endif
      else
        write(stdout,'(/1x,3A/)') SWRT_AVR, ' ERROR: Cannot ',
     &             'synchronize/close averages netCDF file.'
        may_day_flag=3
      endif
 
# if defined MPI && !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER, 
     &                            mynode+1, 3, ocean_grid_comm, ierr)
# endif
      return
      end
# ifndef SLICE_AVR
#  undef START_3D_ARRAY_K0
#  undef START_3D_ARRAY_K1
#  undef wrt_avr
#  undef def_avr
#  undef avrTstep
#  undef time_avr
#  undef avrTime
#  undef u_avr
#  undef avrU
#  undef v_avr
#  undef avrV
#  undef avrO
#  undef w_avr
#  undef avrW
#  undef rho_avr
#  undef avrR
#  undef t_avr
#  undef avrT
#  undef w_avr
#  undef avrW
#  undef avrO
#  undef Akv_avr
#  undef Akt_avr
#  undef Aks_avr
#  undef avrAkv
#  undef avrAkt
#  undef avrAks
#  undef rich_avr
#  undef avrRich
#  undef richN_avr
#  undef avrRichN
#  undef ncidavr
#  undef ncrecavr
#  undef nrpfavr
#  undef nrecavr
#  undef ncidavr
#  undef avrname
#  undef wrtavr
#  undef SWRT_AVR
#  undef WRTROUTINE
#  if defined  SLICE_AVG && !defined SLICE_AVR /* also create wrt_slice_avg */
#   define SLICE_AVR
#   include "wrt_avg.F"
#  endif
# endif
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
 
