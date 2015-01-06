#include "cppdefs.h"
                              ! Write model prognostic
      subroutine wrt_rst      ! variables into restart
                              ! netCDF file.
      implicit none
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite
# ifdef SOLVE3D
     &            , itrc
# endif
#include "param.h"
#include "scalars.h"
#include "ocean2d.h"
#include "ocean3d.h"
#if defined LMD_KPP || defined LMD_BKPP
# include "mixing.h"
#endif
#include "ncvars.h"
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
      integer tmp(NFT+1,1)
      real bufftrack(1:6,NFT+1,nfloats)
# endif

 
#if defined MPI & !defined PARALLEL_FILES
# include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode.gt.0) call MPI_Recv (blank, 1, MPI_INTEGER,
     &            mynode-1, 1, ocean_grid_comm, status, ierr)
#endif

!hf initialization needed in case of error during call to def_rst
      record = 0

!
! Create/open restart file; write grid arrays, if so requested,
! then set record index within the file..
!
      call def_rst (ncidrst, nrecrst, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(rstname)
      nrecrst=max(nrecrst,1)                  !!! WARNING: Here it
      if (nrpfrst.eq.0) then                  !!! is assumed that
        record=nrecrst                        !!! global restart
      else                                    !!! record index
        record=1+mod(nrecrst-1, abs(nrpfrst)) !!! nrecrst is already
      endif                                   !!! advanced by main.
!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record indices.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
#ifdef AVERAGES
      ibuff(4)=nrecavg
#else
      ibuff(4)=0
#endif
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidrst, rstTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr, nf_strerror(ierr)
     &                   MYID
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
      ierr=nf_put_vara_int (ncidrst, rstTstepFA, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'fa_time_step', record, ierr
     &                   MYID
        goto 99                                           !--> ERROR
      endif
#endif /* BGC_FLUX_ANALYSIS || PHYS_FLUX_ANALYSIS */
!
! Time.
!
      ierr=nf_put_var1_FTYPE (ncidrst, rstTime, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
!
! Free-surface.
!
      ierr=nf_fwrite (zeta(START_2D_ARRAY,knew), ncidrst, rstZ,
     &                                            record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxZ))
        write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in XI-direction.
!
      ierr=nf_fwrite (ubar(START_2D_ARRAY,knew), ncidrst, rstUb,
     &                                            record, u2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxUb))
        write(stdout,1) vname(1,indxUb)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in ETA-direction.
!
      ierr=nf_fwrite (vbar(START_2D_ARRAY,knew), ncidrst, rstVb,
     &                                            record, v2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxVb))
        write(stdout,1) vname(1,indxVb)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
#ifdef SOLVE3D
!
! 3D momentum component in XI-direction.
!
      ierr=nf_fwrite (u(START_2D_ARRAY,1,nstp), ncidrst, rstU,
     &                                         record, u3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxU))
        write(stdout,1) vname(1,indxU)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
!
! 3D momentum component in ETA-direction.
!
      ierr=nf_fwrite (v(START_2D_ARRAY,1,nstp), ncidrst, rstV,
     &                                         record, v3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxV))
        write(stdout,1) vname(1,indxV)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        ierr=nf_fwrite (t(START_2D_ARRAY,1,nstp,itrc), ncidrst,
     &                              rstT(itrc), record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxT+itrc-1))
          write(stdout,1) vname(1,indxT+itrc-1)(1:lvar), record,
     &              ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo
#  ifdef SEDIMENT_BIOLOGY
!
! Sediment tracer variables.
!
      do itrc=1,NT_sed
        ierr=nf_fwrite (t_sed(START_2D_ARRAY,itrc), ncidrst,
     &                              rstTsed(itrc), record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
          write(stdout,1) vname(1,indxSedOrgN+itrc-1)(1:lvar), 
     &         record, ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo
#  endif /* SEDIMENT_BIOLOGY */
#  if defined BIOLOGY_BEC || (defined BIOLOGY_NPZDOC && \
    defined OXYGEN && defined CARBON)

#  if defined CH_CARBON_DEPTH
!
! pH_DEPTH:
!
      ierr=nf_fwrite (PHd_hist, ncidrst, rstPHd, record, r3dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPHd_rst))
         write(stdout,1) vname(1,indxPHd_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif

#else /*CH_CARBON_DEPTH*/

!
! pH:
!
      ierr=nf_fwrite (ph_hist, ncidrst, rstPH, record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPH_rst))
         write(stdout,1) vname(1,indxPH_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
!
! pCO2:
!
      ierr=nf_fwrite (pCO2sw, ncidrst, rstPCO2, record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPCO2_rst))
         write(stdout,1) vname(1,indxPCO2_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
#endif /*CH_CARBON_DEPTH*/
#  ifdef OUTPUT_PCO2AIR_RST
!
! pCO2air:
!
      ierr=nf_fwrite (pCO2air, ncidrst, rstPCO2air, record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPCO2air_rst))
         write(stdout,1) vname(1,indxPCO2air_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
#  endif /* OUTPUT_PCO2AIR_RST */
# endif /* BIOLOGY_BEC || (BIOLOGY_NPZDOC && OXYGEN && CARBON) */

# if defined BIOLOGY_BEC2 && defined BEC2_DIAG
!
! 2d diagnostics:
!
      do itrc=1,nr_bec2_diag_2d
         ierr=nf_fwrite (bec2_diag_2d(START_2D_ARRAY,itrc), ncidrst,
     &                              rstT_bec2_diag(itrc), record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bec2_diag_2d(1,itrc))
          write(stdout,1) vname_bec2_diag_2d(1,itrc)(1:lvar), record,
     &              ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo
!
! 3d diagnostics:
!
      do itrc=1,nr_bec2_diag_3d
         ierr=nf_fwrite (bec2_diag_3d(START_2D_ARRAY,1,itrc), ncidrst,
     &                   rstT_bec2_diag(nr_bec2_diag_2d+itrc), record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bec2_diag_3d(1,itrc))
          write(stdout,1) vname_bec2_diag_3d(1,itrc)(1:lvar), record,
     &              ierr, nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      enddo
# endif /* BIOLOGY_BEC2 && BEC2_DIAG */

# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
!
! PAR:
!
      ierr=nf_fwrite (PAR, ncidrst, rstPAR, record, r3dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxPAR_rst))
         write(stdout,1) vname(1,indxPAR_rst)(1:lvar), record,
     &        ierr, nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
#  endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */

# ifdef LMD_KPP
!
! Thickness of planetary boundary layer (m).
!
      ierr=nf_fwrite (hbls(START_2D_ARRAY,nstp), ncidrst,
     &                              rstHbl, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxHbl))
        write(stdout,1) vname(1,indxHbl)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif
#endif

# ifdef LMD_BKPP
!
! Thickness of bottom boundary layer (m).
!
      ierr=nf_fwrite (hbbls(START_2D_ARRAY,nstp), ncidrst,
     &                              rstHbbl, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxHbbl))
        write(stdout,1) vname(1,indxHbbl)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif

#ifdef FLOATS
! write nfloats
      ierr=nf_put_vara_int (ncidrst, rstnfloats, record, 1, nfloats)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'nfloats', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif

! write Tinfo

      startTinfo(1)=1
      startTinfo(2)=1
      startTinfo(3)=record
      countTinfo(1)=5
      countTinfo(2)=nfloats
      countTinfo(3)=1

      ierr=nf_put_vara_FTYPE (ncidrst, rstTinfo, startTinfo,
     &   countTinfo, Tinfo)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'Tinfo', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif

! write grid level

      start(1)=1
      start(2)=record
      count(1)=nfloats
      count(2)=1

      ierr=nf_put_vara_int (ncidrst, rstfltgrd, start, count, fltgrd)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'grid level', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif

! write track

      starttrack(1)=1
      starttrack(2)=1
      starttrack(3)=1
      starttrack(4)=record
      counttrack(1)=6
      counttrack(2)=NFT+1
      counttrack(3)=nfloats
      counttrack(4)=1

      level=-1
      tmp(1,level)=nf
      tmp(2,level)=nfm1
      tmp(3,level)=nfm2
      tmp(4,level)=nfm3
      level=0
      tmp(1,level)=nf
      tmp(2,level)=nfm1
      tmp(3,level)=nfm2
      tmp(4,level)=nfm3

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
      ierr=nf_put_vara_FTYPE (ncidrst, rsttrack, starttrack,
     &   counttrack, bufftrack)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'track', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif
#endif

  1   format(/1x, '### ERROR: wrt_rst :: Cannot write variable ''',
     &        A, ''' into restart file.'  /11x, 'Time record:', I6,
     &        3x,  'netCDF error code', i4/11x, 'Cause of error: ',
     &                                              A, 3x, A, i4/)
      goto 100
  99  may_day_flag=3
 100  continue
 
!
! Synchronize restart netCDF file to disk to allow other
! processes to access data immediately after it is written.
!
#if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close (ncidrst)
      if (nrpfrst.gt.0 .and. record.ge.nrpfrst) ncidrst=-1
#else
      if (nrpfrst.gt.0 .and. record.ge.nrpfrst) then
        ierr=nf_close (ncidrst)
        ncidrst=-1
      else
        ierr=nf_sync(ncidrst)
      endif
#endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_RST -- wrote ',
     &            'restart fields into time record =', record, '/',
     &             nrecrst  MYID
      else
        write(stdout,'(/1x,2A/)') '### ERROR: wrt_rst :: Cannot ',
     &                   'synchronize/close restart netCDF file.'
        may_day_flag=3
      endif
 
#if defined MPI && !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, ocean_grid_comm, ierr)
      endif
#endif
      return
      end
 
