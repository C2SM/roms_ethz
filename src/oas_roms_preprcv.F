#include "cppdefs.h"
      subroutine oas_roms_preprcv(oas_step)                 
      !**** *oas_roms_preprcv*  - 
!
!     Purpose.
!     --------
!     Prepare fields for receiving from Atmospheric model
!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_preprcv*
!
!     Input:
!     -----
!
!     Output:
!     ------
!      
!
!     Method:
!     ------
!       
!
!     Externals:
!     ---------
!      oas_roms_vardef
!
!
!     Author:
!     -------
!       D. Byrne ; ETHZ
!
!     Modifications.
!     --------------
!

       USE oas_roms_vardef
       USE netcdf

      implicit none
#include "param.h"
#include "scalars.h"
#include "forces.h"
#include "ncvars.h"
#include "netcdf.inc"
#include "ocean3d.h"
      integer info, oas_dt, oas_id, oas_step, jn, nrcvinfo(krcv),i,j, ierr
      integer il_file_id, il_var_id(krcv), dimids(3), ib, mype, npes, status
      real*8 ztmpl(oas_xi,oas_eta), ztmplu(oas_xi_u,oas_eta_u)
      real*8 ztmplv(oas_xi_v,oas_eta_v)
      real cff

      oas_dt=int((oas_step)*dt)
      
      
      nrcvinfo = OASIS_idle
      ztmpl=0.
      ztmplu=0.
      ztmplv=0.

      ! frcv modified only at coupling time step
      ! frcv keep values from previous coupling (array from module) at other time step
      call MPI_Comm_rank(kl_comm, mype, ierr)
      call MPI_Comm_size(kl_comm, npes, ierr)
      
      do jn=1, krcv
        if( srcv(jn)%laction ) then
          if(jn .eq. oas_UST) then
            call oas_roms_rcv(jn, oas_dt, ztmplu(:,:), nrcvinfo(jn)) 
            print *,'ztemplu',ztmplu(10,10)
            if( nrcvinfo(jn) .eq. OASIS_Rcv ) frcvu(:,:)=ztmplu(:,:)
          endif
          if(jn .eq. oas_VST) then
            call oas_roms_rcv(jn, oas_dt, ztmplv(:,:), nrcvinfo(jn))
            if( nrcvinfo(jn) .eq. OASIS_Rcv ) frcvv(:,:)=ztmplv(:,:)
          endif
          if(jn .ge. 3) then
            call oas_roms_rcv(jn, oas_dt, ztmpl(:,:), nrcvinfo(jn))   
            if( nrcvinfo(jn) .eq. OASIS_Rcv ) frcv(:,:,jn)=ztmpl(:,:)
          endif
        endif
      enddo

      cff=1./rho0
      do j=oas_jmin_u, oas_jmax_u
        do i=oas_imin_u, oas_imax_u
          if (frcvu(i,j) .ne. 0) sustr(i,j)=cff*frcvu(i,j)
        enddo
      enddo

      do j=oas_jmin_v, oas_jmax_v
        do i=oas_imin_v, oas_imax_v
          if (frcvv(i,j) .ne. 0) svstr(i,j)=cff*frcvv(i,j)
        enddo           
      enddo

      cff=1./(rho0*Cp)
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
          if (frcv(i,j,oas_nhf) .ne. 0) stflx(i,j,itemp)=cff*frcv(i,j, oas_nhf)
!
! Restrict stflx to prevent surface temperature to go below -2
! degrees C.
!
#  if defined PACIFIC || defined STFLX_LIM
          stflx(i,j,itemp)=max( stflx(i,j,itemp),
#   ifdef LMD_KPP
     &                          max(hbls(i,j,nstp), Hz(i,j,N))
#   else
     &                                               Hz(i,j,N)
#   endif
     &                         *(-2.D0-t(i,j,N,nrhs,itemp))/dt
     &                                                       )
#  endif
        enddo
      enddo

      cff=1/rho0
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
         if (frcv(i,j,oas_TEP) .ne. 0) stflx(i,j,isalt)=cff*frcv(i,j, oas_TEP)
     &                       *t(i,j,N,nrhs,isalt)
        enddo
      enddo

      IF (IOASISDEBUGLVL .eq. 1) then
      print *,'Frcvu',frcvu(10,10)
      print *,'Frcvv',frcvv(10,10)
      print *,'Frcv',frcv(10,10,3)
      ENDIF

      !Write Debug File
      IF (IOASISDEBUGLVL .eq. 1) then
        IF (mype .eq. 0 .and. oas_step .eq. 1) then
           status = nf90_create("debugout_roms_rcv.nc", NF90_CLOBBER, il_file_id)
           status = nf90_def_dim(il_file_id, "x", xi_rho, dimids(1))
           status = nf90_def_dim(il_file_id, "y", eta_rho, dimids(2))
           status = nf90_def_dim(il_file_id, "step", NF90_UNLIMITED, dimids(3))
           status = nf90_def_var(il_file_id, "RO_UST_A", NF90_DOUBLE, dimids, il_var_id(1))
           status = nf90_def_var(il_file_id, "RO_VST_A", NF90_DOUBLE, dimids, il_var_id(2))
           status = nf90_def_var(il_file_id, "RO_NHF_A", NF90_DOUBLE, dimids, il_var_id(3))
           status = nf90_def_var(il_file_id, "RO_SWR_A", NF90_DOUBLE, dimids, il_var_id(4))
           status = nf90_def_var(il_file_id, "RO_TEP_A", NF90_DOUBLE, dimids, il_var_id(5))
           status = nf90_enddef(il_file_id)
           status = nf90_close(il_file_id)
        ENDIF

        DO ib = 0, npes - 1

           call MPI_Barrier(kl_comm, ierr)
           IF (mype .eq. ib) THEN
              status = nf90_open("debugout_roms_rcv.nc", NF90_WRITE, il_file_id)
              status = nf90_inq_varid(il_file_id, "RO_UST_A" , il_var_id(1))
              status = nf90_inq_varid(il_file_id, "RO_VST_A" , il_var_id(2))
              status = nf90_inq_varid(il_file_id, "RO_NHF_A" , il_var_id(3))
              status = nf90_inq_varid(il_file_id, "RO_SWR_A",  il_var_id(4))
              status = nf90_inq_varid(il_file_id, "RO_TEP_A",  il_var_id(5))
              DO jn = 1, krcv
               if(jn .eq. 1) then
                 status = nf90_put_var(il_file_id, il_var_id(jn), frcvu(:,:),
     &                                start = (/ oas_start_u(1),
     &                                           oas_start_u(2), (oas_step-1) /),
     &                                count = (/ oas_xi_u, oas_eta_u, 1 /) )
               endif
               if(jn .eq. 2) then
                 status = nf90_put_var(il_file_id, il_var_id(jn), frcvv(:,:),
     &                                start = (/ oas_start_v(1),
     &                                           oas_start_v(2), (oas_step-1) /),
     &                                count = (/ oas_xi_v, oas_eta_v, 1 /) )
               endif
               if(jn .ge. 3) then
                 status = nf90_put_var(il_file_id, il_var_id(jn), frcv(:,:,jn),
     &                                start = (/ oas_start(1),
     &                                           oas_start(2), (oas_step-1) /),
     &                                count = (/ oas_xi, oas_eta, 1 /) )
               endif
              ENDDO              
              status = nf90_close(il_file_id)
           ENDIF
        ENDDO
      ENDIF !Debug



      end
