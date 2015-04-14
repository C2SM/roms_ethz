!   $Log: wrt_bgc_flux_avg.F,v $
!   Revision 1.1  2002/01/16 19:58:53  frenzel
!   New routines related to flux analysis.
!
!
#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC && defined BGC_FLUX_ANALYSIS && defined AVERAGES
! Write time-averaged biogeochemical flux fields into averages netCDF file
      subroutine wrt_bgc_flux_avg
      implicit none
#include "netcdf.inc"
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite, iflux

# if defined MPI && !defined PARALLEL_FILES
#     include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
# endif
# include "param.h"
# include "scalars.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "ncvars.h"
# include "bgcflux.h"
# include "ncroms_bgcflux.h"

#if defined MPI && !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 1, MPI_COMM_WORLD, status, ierr)
      endif
#endif
!
! Create/open averages file; write grid arrays, if so needed.
!
      call def_bgc_flux_avg(ncid_bgc_flux_avg, nrec_bgc_flux_avg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(bgc_flux_avg_name)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index
      nrec_bgc_flux_avg=max(nrec_bgc_flux_avg,1) !!! nrec_bgc_flux_avg 
      if (nrpf_bgc_flux_avg.eq.0) then       !!! is already advanced by main.
        record=nrec_bgc_flux_avg
      else
        record=1+mod(nrec_bgc_flux_avg-1, nrpf_bgc_flux_avg)
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
      ibuff(4)=nrec_bgc_flux_avg

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int(ncid_bgc_flux_avg, bgc_flux_avgTstep, 
     &     start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged time
!
      ierr=nf_put_var1_double (ncid_bgc_flux_avg, bgc_flux_avgTime,  
     &     record, time_bgc_flux_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
! Barotropic mode variable: free-surface
! Always include this in the output so that depths of sigma levels
! can be computed
      ierr=nf_fwrite (zeta_bgc_flux_avg(START_2D_ARRAY), 
     &     ncid_bgc_flux_avg, bgc_flux_avgZ,record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxZ))
         write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif
#   ifdef OXYGEN
!      if (wrtavg(indxU10)) then
        ierr=nf_fwrite (u10_avg, ncid_bgc_flux_avg, avgU10, record, 
     &     r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxU10))
          write(stdout,1) vname_bgcflux(1,indxU10)(1:lvar), record, 
     &                   ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_avg:  wrtU10' MYID
!      endif
!      if (wrtavg(indxKvO2)) then
        ierr=nf_fwrite (Kv_O2_avg, ncid_bgc_flux_avg, avgKvO2, record, 
     &       r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxKvO2))
          write(stdout,1) vname_bgcflux(1,indxKvO2)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_avg:  wrtKvO2' MYID
!      endif
!      if (wrtavg(indxO2sat)) then
        ierr=nf_fwrite (O2satu_avg, ncid_bgc_flux_avg, avgO2sat, record, 
     &       r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxO2sat))
          write(stdout,1) vname_bgcflux(1,indxO2sat)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_avg:  wrtO2sat' MYID
!      endif
#    ifdef CARBON
!      if (wrtavg(indxKvCO2)) then
        ierr=nf_fwrite (Kv_CO2_avg, ncid_bgc_flux_avg, avgKvCO2, record, 
     &       r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxKvCO2))
          write(stdout,1) vname_bgcflux(1,indxKvCO2)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_avg:  wrtKvO2' MYID
!      endif
!      if (wrtavg(indxCO2sol)) then
        ierr=nf_fwrite (CO2sol_avg, ncid_bgc_flux_avg, avgCO2sol, 
     &       record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxCO2sol))
          write(stdout,1) vname_bgcflux(1,indxCO2sol)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_avg:  wrtKvO2' MYID
!      endif
!
!  Write out surface pCO2 (ppm)
!
!      if (wrtavg(indxPCO2)) then
!DL        ierr=nf_fwrite (pco2_avg(START_2D_ARRAY), ncid_bgc_flux_avg, 
!DL     &       avgPCO2, record, r2dvar)
!DL        if (ierr .ne. nf_noerr) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2))
!DL          write(stdout,1) vname_bgcflux(1,indxPCO2)(1:lvar), record,
!DL     &         ierr, nf_strerror(ierr) MYID
!DL          goto 99                                         !--> ERROR
!DL        endif
!DL!      endif
!DL!      if (wrtavg(indxPCO2air)) then
!DL        ierr=nf_fwrite(pCO2air_avg, ncid_bgc_flux_avg, avgPCO2air, 
!DL     &       record,r2dvar)
!DL        if (ierr .ne. nf_noerr) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2air))
!DL          write(stdout,1) vname_bgcflux(1,indxPCO2air)(1:lvar), record,
!DL     &                    ierr, nf_strerror(ierr) MYID
!DL          goto 99                                         !--> ERROR
!DL        endif
!DLCR      write(*,*) 'wrt_avg:  wrtPCO2air' MYID
!      endif
!
!  Write out surface pH
!
!      if (wrtavg(indxPH)) then
!DL        ierr=nf_fwrite (pH_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
!DL     &                                           avgPH, record, r2dvar)
!DL        if (ierr .ne. nf_noerr) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPH))
!DL          write(stdout,1) vname_bgcflux(1,indxPH)(1:lvar), record, ierr,
!DL     &         nf_strerror(ierr) MYID
!DL          goto 99                                         !--> ERROR
!DL        endif
!      endif
#    endif /* CARBON */
#   endif /* OXYGEN */
!
!  Write out PAR (W m-2)
!
      if (wrtavg(indxPAR)) then
        ierr=nf_fwrite (PAR_flux_avg(START_2D_ARRAY,1),
     &        ncid_bgc_flux_avg, avgPAR_flux, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxPAR))
          write(stdout,1) vname_bgcflux(1,indxPAR)(1:lvar), record,
     &         ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
        ierr=nf_fwrite (PARinc_flux_avg(START_2D_ARRAY), 
     &        ncid_bgc_flux_avg, avgPARinc_flux, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxPARinc))
          write(stdout,1) vname_bgcflux(1,indxPARinc)(1:lvar), record,
     &         ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif


! Flux terms
      do iflux = 1, NumFluxTerms
         ierr = nf_fwrite(Flux_avg(START_2D_ARRAY,1,iflux), 
     &        ncid_bgc_flux_avg, avgFlux(iflux), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,indxFlux+iflux-1))
            write(stdout,1) vname_bgcflux(1,indxFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
!DL: now in tracer avg files:
!# ifdef OXYGEN
!! gas exchange fluxes
!      do iflux = 1, NumGasExcTerms
!         ierr = nf_fwrite(GasExcFlux_avg(START_2D_ARRAY,iflux), 
!     &        ncid_bgc_flux_avg, avgGasExcFlux(iflux), 
!     &        record, r2dvar)
!         if (ierr .ne. nf_noerr) then
!            lvar=lenstr(vname_bgcflux(1,indxGasExcFlux+iflux-1))
!            write(stdout,1) 
!     &           vname_bgcflux(1,indxGasExcFlux+iflux-1)(1:lvar), 
!     &           record, ierr, nf_strerror(ierr) MYID 
!            goto 99
!         end if
!      end do
!# endif /* OXYGEN */
! vertical sinking fluxes
      do iflux = 1, NumVSinkTerms
         ierr = nf_fwrite(VSinkFlux_avg(START_2D_ARRAY,0,iflux), 
     &        ncid_bgc_flux_avg, avgVSinkFlux(iflux), record, w3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,indxVSinkFlux+iflux-1))
            write(stdout,1) 
     &           vname_bgcflux(1,indxVSinkFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
# ifdef SEDIMENT_BIOLOGY
! sediment-related fluxes
      do iflux = 1, NumSedFluxTerms
         ierr = nf_fwrite(SedFlux_avg(START_2D_ARRAY,iflux), 
     &        ncid_bgc_flux_avg, avgSedFlux(iflux), record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,indxSedFlux+iflux-1))
            write(stdout,1)vname_bgcflux(1,indxSedFlux+iflux-1)(1:lvar),
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
# endif /* SEDIMENT_BIOLOGY */
  1   format(/' WRT_BGC_FLUX_AVG - ERROR while writing variable(',
     &     1x,a,1x,
     &               ')into averages file.',/,11x,'Time record:',
     &                      i6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100
  99  may_day_flag=3
 100  continue

!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(ncid_bgc_flux_avg)
      if (nrpf_bgc_flux_avg.gt.0 .and. record.ge.nrpf_bgc_flux_avg) 
     &     ncid_bgc_flux_avg=-1
# else
      if (nrpf_bgc_flux_avg.gt.0 .and. record.ge.nrpf_bgc_flux_avg) then
        ierr=nf_close(ncid_bgc_flux_avg)
        ncid_bgc_flux_avg=-1
      else
        ierr=nf_sync(ncid_bgc_flux_avg)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &        'WRT_BGC_FLUX_AVG -- wrote ',
     &            'averaged fields into time record =', record, '/',
     &             nrec_bgc_flux_avg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_BGC_FLUX_AVG ERROR: Cannot ',
     &             'synchronize/close averages netCDF file.'
        may_day_flag=3
      endif

# if defined MPI && !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
# endif
      return
      end
#else /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS && AVERAGES */
      subroutine wrt_bgc_flux_avg_empty
      end
#endif /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS && AVERAGES */
