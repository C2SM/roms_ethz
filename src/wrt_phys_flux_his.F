!   $Log: wrt_phys_flux_his.F,v $
!   Revision 1.1  2002/04/19 00:59:56  frenzel
!   New routine: Output of physical flux histories.
!
!
!
#include "cppdefs.h"
#ifdef PHYS_FLUX_ANALYSIS
! Write physical flux fields into histories netCDF file
      subroutine wrt_phys_flux_his
      implicit none
#include "netcdf.inc"
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite, iflux            

# include "param.h"
# include "scalars.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "ncvars.h"
# include "physflux.h"
# include "ncroms_physflux.h"

#if defined MPI && !defined PARALLEL_FILES
#include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank

      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 1, MPI_COMM_WORLD, status, ierr)
      endif
#endif
!
! Create/open histories file; write grid arrays, if so needed.
!
      call def_phys_flux_his(ncid_phys_flux_his, nrec_phys_flux_his, 
     &     ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(phys_flux_his_name)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index
      nrec_phys_flux_his=max(nrec_phys_flux_his,1)!!! nrec_phys_flux_his 
      if (nrpf_phys_flux_his.eq.0) then      !!! is already advanced by main.
        record=nrec_phys_flux_his
      else
        record=1+mod(nrec_phys_flux_his-1, nrpf_phys_flux_his)
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
      ibuff(4)=nrec_phys_flux_his

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int(ncid_phys_flux_his, phys_flux_hisTstep,  
     &     start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr, nf_strerror(ierr)
     &        MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!
      ierr=nf_put_var1_double (ncid_phys_flux_his, phys_flux_hisTime, 
     &     record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr, 
     &       nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
! Barotropic mode variable: free-surface
! Always include this in the output so that depths of sigma levels
! can be computed
      ierr=nf_fwrite (zeta(START_2D_ARRAY,fast_indx_out), 
     &     ncid_phys_flux_his, phys_flux_hisZ,record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxZ))
         write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr,
     &        nf_strerror(ierr) MYID
         goto 99                !--> ERROR
      endif
! horizontal advective fluxes (xi-direction)
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(HorXAdvFlux(START_2D_ARRAY,1,iflux), 
     &        ncid_phys_flux_his, hisHorXAdvFlux(iflux), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_phys(1,indxHorXAdvFlux+iflux-1))
            write(stdout,1) 
     &           vname_phys(1,indxHorXAdvFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! horizontal advective fluxes (eta-direction)
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(HorYAdvFlux(START_2D_ARRAY,1,iflux), 
     &        ncid_phys_flux_his, hisHorYAdvFlux(iflux), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_phys(1,indxHorYAdvFlux+iflux-1))
            write(stdout,1) 
     &           vname_phys(1,indxHorYAdvFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! vertical advective fluxes
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(VertAdvFlux(START_2D_ARRAY,0,iflux), 
     &        ncid_phys_flux_his, hisVertAdvFlux(iflux), record, w3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_phys(1,indxVertAdvFlux+iflux-1))
            write(stdout,1) 
     &           vname_phys(1,indxVertAdvFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
#ifdef VERT_DIFF_ANALYSIS
! vertical diffusive fluxes
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(VertDiffFlux(START_2D_ARRAY,0,iflux), 
     &        ncid_phys_flux_his, hisVertDiffFlux(iflux), 
     &        record, w3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_phys(1,indxVertDiffFlux+iflux-1))
            write(stdout,1) 
     &           vname_phys(1,indxVertDiffFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
#endif /* VERT_DIFF_ANALYSIS */
#ifdef FULL_PHYS_FLUX_ANALYSIS
! top fluxes
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(TopFlux(START_2D_ARRAY,iflux), 
     &        ncid_phys_flux_his, hisTopFlux(iflux), record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTopFlux+iflux-1))
            write(stdout,1) vname(1,indxTopFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! bottom fluxes
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(BottomFlux(START_2D_ARRAY,iflux), 
     &        ncid_phys_flux_his, hisBottomFlux(iflux), record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxBottomFlux+iflux-1))
            write(stdout,1) vname(1,indxBottomFlux+iflux-1)(1:lvar),
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! horizontal mixing fluxes (xi-direction)
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(HorXMixFlux(START_2D_ARRAY,1,iflux), 
     &        ncid_phys_flux_his, hisHorXMixFlux(iflux), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxHorXMixFlux+iflux-1))
            write(stdout,1) vname(1,indxHorXMixFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! horizontal mixing fluxes (eta-direction)
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(HorYMixFlux(START_2D_ARRAY,1,iflux), 
     &        ncid_phys_flux_his, hisHorYMixFlux(iflux), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxHorYMixFlux+iflux-1))
            write(stdout,1) vname(1,indxHorYMixFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! vertical mixing fluxes
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(VertMixFlux(START_2D_ARRAY,0,iflux), 
     &        ncid_phys_flux_his, hisVertMixFlux(iflux), record, w3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxVertMixFlux+iflux-1))
            write(stdout,1) vname(1,indxVertMixFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
! nudging divergences
      do iflux = 1, NT_PFA
         ierr = nf_fwrite(NudgingFlux(START_2D_ARRAY,1,iflux), 
     &        ncid_phys_flux_his, hisNudgingFlux(iflux), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxNudgingFlux+iflux-1))
            write(stdout,1) vname(1,indxNudgingFlux+iflux-1)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
# endif /* FULL_PHYS_FLUX_ANALYSIS */
  1   format(/' WRT_PHYS_FLUX_HIS - ERROR while writing variable(',
     &     1x,a,1x,
     &     ')into phys flux histories file.',/,11x,
     &     'Time record:',
     &     i6,3x,'netCDF error code',i4,3x,a,1x,a,i4)
      goto 100
  99  may_day_flag=3
 100  continue

!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(ncid_phys_flux_his)
      if (nrpf_phys_flux_his.gt.0 .and. record.ge.nrpf_phys_flux_his) 
     &     ncid_phys_flux_his=-1
# else
      if (nrpf_phys_flux_his.gt.0 .and. 
     &     record.ge.nrpf_phys_flux_his) then
        ierr=nf_close(ncid_phys_flux_his)
        ncid_phys_flux_his=-1
      else
        ierr=nf_sync(ncid_phys_flux_his)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        MPI_master_only write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &        'WRT_PHYS_FLUX_HIS -- wrote ',
     &        'phys flux fields into time record =', record, '/',
     &        nrec_phys_flux_his  MYID
      else
        write(stdout,'(/1x,2A/)') 
     &        'WRT_PHYS_FLUX_HIS ERROR: Cannot ',
     &        'synchronize/close phys fluxes netCDF file.'
        may_day_flag=3
      endif

# if defined MPI && !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
         blank=mynode
         call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
# endif
      return
      end
#else /* PHYS_FLUX_ANALYSIS */
      subroutine wrt_phys_flux_his_empty
      end
#endif /* PHYS_FLUX_ANALYSIS */
