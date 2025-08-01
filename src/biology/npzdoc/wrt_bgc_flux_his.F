!   $Log: wrt_bgc_flux_his.F,v $
!   Revision 1.1  2002/04/19 00:59:10  frenzel
!   New routine: Output of biogeochemical flux histories.
!
!
!
#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC && defined BGC_FLUX_ANALYSIS
! Write biogeochemical flux fields into netCDF file
      subroutine wrt_bgc_flux_his
      implicit none
#include "netcdf.inc"
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), ncf_fwrite, iflux

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
! Create/open histories file; write grid arrays, if so needed.
!
      call def_bgc_flux_his(ncid_bgc_flux_his, nrec_bgc_flux_his, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(bgc_flux_his_name)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index
      nrec_bgc_flux_his=max(nrec_bgc_flux_his,1) !!! nrec_bgc_flux_his
      if (nrpf_bgc_flux_his.eq.0) then       !!! is already advanced by main.
        record=nrec_bgc_flux_his
      else
        record=1+mod(nrec_bgc_flux_his-1, nrpf_bgc_flux_his)
      endif
!
! Write out variables:
! ----- --- ------------- ----------
!
! Time step and record indices.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrec_bgc_flux_his

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int(ncid_bgc_flux_his, bgc_flux_hisTstep,
     &     start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!
      ierr=nf_put_var1_double (ncid_bgc_flux_his, bgc_flux_hisTime,
     &     record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

! Barotropic mode variable: free-surface
! Always include this in the output so that depths of sigma levels
! can be computed
      ierr=ncf_fwrite (zeta(START_2D_ARRAY,knew),
     &     ncid_bgc_flux_his, bgc_flux_hisZ,record, r_var,1)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxZ))
         write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

#ifdef OXYGEN
!      if (wrthis(indxU10)) then
        ierr=ncf_fwrite (u10,ncid_bgc_flux_his,hisU10,record,r_var,1)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxU10))
          write(stdout,1) vname_bgcflux(1,indxU10)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtU10' MYID
!      endif
!      if (wrthis(indxKvO2)) then
        ierr=ncf_fwrite (Kv_O2, ncid_bgc_flux_his, hisKvO2, record,
     &       r_var,1)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxKvO2))
          write(stdout,1) vname_bgcflux(1,indxKvO2)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtKvO2' MYID
!      endif
!      if (wrthis(indxO2sat)) then
        ierr=ncf_fwrite (O2satu, ncid_bgc_flux_his, hisO2sat, record,
     &       r_var,1)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxO2sat))
          write(stdout,1) vname_bgcflux(1,indxO2sat)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtO2sat' MYID
!      endif
#  ifdef CARBON
!      if (wrthis(indxKvCO2)) then
        ierr=ncf_fwrite (Kv_CO2, ncid_bgc_flux_his, hisKvCO2, record,
     &       r_var,1)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxKvCO2))
          write(stdout,1) vname_bgcflux(1,indxKvCO2)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtKvO2' MYID
!      endif
!      if (wrthis(indxCO2sol)) then
        ierr=ncf_fwrite (CO2sol, ncid_bgc_flux_his, hisCO2sol, record,
     &       r_var,1)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxCO2sol))
          write(stdout,1) vname_bgcflux(1,indxCO2sol)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtKvO2' MYID
!      endif
!      if (wrthis(indxPCO2)) then
!DL        ierr=ncf_fwrite (pCO2sw, ncid_bgc_flux_his, hisPCO2, record,
!DL     &       r_var,1)
!DL        if (ierr .ne. nf_noerr) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2))
!DL          write(stdout,1) vname_bgcflux(1,indxPCO2)(1:lvar), record,
!DL     &                    ierr, nf_strerror(ierr) MYID
!DL          goto 99                                         !--> ERROR
!DL        endif
!DLCR      write(*,*) 'wrt_his:  wrtPCO2' MYID
!      endif
!      if (wrthis(indxPCO2air)) then
!DL        ierr=ncf_fwrite (pCO2air, ncid_bgc_flux_his, hisPCO2air, record,
!DL     &       r_var,1)
!DL        if (ierr .ne. nf_noerr) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2air))
!DL          write(stdout,1) vname_bgcflux(1,indxPCO2air)(1:lvar), record,
!DL     &                    ierr, nf_strerror(ierr) MYID
!DL          goto 99                                         !--> ERROR
!DL        endif
!DLCR      write(*,*) 'wrt_his:  wrtPCO2air' MYID
!      endif
!      if (wrthis(indxPH)) then
!DL        ierr=ncf_fwrite (pH_hist, ncid_bgc_flux_his, hisPH, record, r_var,1)
!DL        if (ierr .ne. nf_noerr) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPH))
!DL          write(stdout,1) vname_bgcflux(1,indxPH)(1:lvar), record, ierr,
!DL     &                    nf_strerror(ierr) MYID
!DL          goto 99                                         !--> ERROR
!DL        endif
!DLCR      write(*,*) 'wrt_his:  wrtPH' MYID
!      endif
#  endif /* CARBON */
# endif /* OXYGEN */
!      if (wrthis(indxPAR)) then
        ierr=ncf_fwrite(PAR,ncid_bgc_flux_his,hisPAR_flux,record,r_var,N)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxPAR))
          write(stdout,1) vname_bgcflux(1,indxPAR)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
        ierr=ncf_fwrite(PARinc,ncid_bgc_flux_his,hisPARinc_flux,record,
     &       r_var,1)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,indxPARinc))
          write(stdout,1) vname_bgcflux(1,indxPARinc)(1:lvar), record,
     &                    ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
!      endif



! Flux terms
# ifdef BGC_FLUX_EXT_HIS_OUTPUT
      do iflux = 1, NumFluxTerms
# else /* BGC_FLUX_EXT_HIS_OUTPUT */
      do iflux = 1, 2
# endif /* BGC_FLUX_EXT_HIS_OUTPUT */
         ierr = ncf_fwrite(Flux(START_2D_ARRAY,1,iflux),
     &        ncid_bgc_flux_his, hisFlux(iflux), record, r_var, N)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,indxFlux+iflux-1))
            write(stdout,1) vname_bgcflux(1,indxFlux+iflux-1)(1:lvar),
     &           record, ierr, nf_strerror(ierr) MYID
            goto 99
         end if
      end do
!DL: moved to wrt_his.F:
!# ifdef OXYGEN
!! gas exchange fluxes
!      do iflux = 1, NumGasExcTerms
!         ierr = ncf_fwrite(GasExcFlux(START_2D_ARRAY,iflux),
!     &        ncid_bgc_flux_his, hisGasExcFlux(iflux),
!     &        record, r_var,1)
!         if (ierr .ne. nf_noerr) then
!            lvar=lenstr(vname_bgcflux(1,indxGasExcFlux+iflux-1))
!            write(stdout,1)
!     &           vname_bgcflux(1,indxGasExcFlux+iflux-1)(1:lvar),
!     &           record, ierr, nf_strerror(ierr) MYID
!            goto 99
!         end if
!      end do
!# endif /* OXYGEN */
# ifdef BGC_FLUX_EXT_HIS_OUTPUT
! vertical sinking fluxes
      do iflux = 1, NumVSinkTerms
         ierr = ncf_fwrite(VSinkFlux(START_2D_ARRAY,0,iflux),
     &        ncid_bgc_flux_his, hisVSinkFlux(iflux), record, r_var, N+1)
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
         ierr = ncf_fwrite(SedFlux(START_2D_ARRAY,iflux),
     &        ncid_bgc_flux_his, hisSedFlux(iflux),
     &        record, r_var,1)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,indxSedFlux+iflux-1))
            write(stdout,1)vname_bgcflux(1,indxSedFlux+iflux-1)(1:lvar),
     &           record, ierr, nf_strerror(ierr) MYID
            goto 99
         end if
      end do
# endif /* SEDIMENT_BIOLOGY */
# endif /* BGC_FLUX_EXT_HIS_OUTPUT */

  1   format(/' WRT_BGC_FLUX_HIS - ERROR while writing variable(',
     &     1x,a,1x,
     &               ')into histories file.',/,11x,'Time record:',
     &                      i6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100
  99  may_day_flag=3
 100  continue

!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(ncid_bgc_flux_his)
      if (nrpf_bgc_flux_his.gt.0 .and. record.ge.nrpf_bgc_flux_his)
     &     ncid_bgc_flux_his=-1
# else
      if (nrpf_bgc_flux_his.gt.0 .and. record.ge.nrpf_bgc_flux_his) then
        ierr=nf_close(ncid_bgc_flux_his)
        ncid_bgc_flux_his=-1
      else
        ierr=nf_sync(ncid_bgc_flux_his)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        MPI_master_only write(stdout,'(6x,A,2(A,I4,1x),A,I3)')
     &        'WRT_BGC_FLUX_HIS -- wrote ',
     &            'bgc flux fields into time record =', record, '/',
     &             nrec_bgc_flux_his  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_BGC_FLUX_HIS ERROR: Cannot ',
     &             'synchronize/close bgc fluxes netCDF file.'
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
#else /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS */
      subroutine wrt_bgc_flux_his_empty
      end
#endif /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS */
