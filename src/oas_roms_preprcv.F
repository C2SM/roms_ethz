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
# include "grid.h"
#include "netcdf.inc"
#include "ocean3d.h"
      integer info, oas_dt, oas_id, oas_step, jn, nrcvinfo(krcv),i,j, ierr
      integer il_file_id, il_var_id(10), dimids(3),uid,vid, ib, mype, npes, status
      real*8 ztmpl(oas_imin:oas_imax, oas_jmin:oas_jmax)
      real cff

# ifdef DIURNAL_SRFLUX
      real phase, year2day, day2year, cff1,cff2
      real cos_h, dec, cos_d, sin_d, tan_d, phi, h0, utc, utc_cor
      parameter (year2day=365.25, day2year=1.D0/year2day,
     &           utc_cor=1.D0/360.D0)
# endif

      oas_dt=int((oas_step)*dt)
      
      
      nrcvinfo = OASIS_idle
      ztmpl(:,:)=0.

      ! frcv modified only at coupling time step
      ! frcv keep values from previous coupling (array from module) at other time step
      call MPI_Comm_rank(kl_comm, mype, ierr)
      call MPI_Comm_size(kl_comm, npes, ierr)

      do jn=1, krcv
        if( srcv(jn)%laction ) then
          if(jn .eq. oas_UST) then
            call oas_roms_rcv(jn, oas_dt, ztmpl(:,:), nrcvinfo(jn)) 
            if( nrcvinfo(jn) .eq. OASIS_Rcv ) frcv(:,:,oas_UST)=ztmpl(:,:)
          endif
          if(jn .eq. oas_VST) then
            call oas_roms_rcv(jn, oas_dt, ztmpl(:,:), nrcvinfo(jn))
            if( nrcvinfo(jn) .eq. OASIS_Rcv ) frcv(:,:,oas_VST)=ztmpl(:,:)
          endif
          if(jn .ge. 3) then
            call oas_roms_rcv(jn, oas_dt, ztmpl(:,:), nrcvinfo(jn))   
            if( nrcvinfo(jn) .eq. OASIS_Rcv ) frcv(:,:,jn)=ztmpl(:,:)
          endif
        endif
      enddo

!Momentum Flux
      cff=1./rho0
      do j=oas_jmin_u, oas_jmax_u
        do i=oas_imin_u, oas_imax_u
          if (frcv(i,j,oas_UST) .ne. 0) sustr(i,j)=cff*frcv(i,j,oas_UST)
        enddo
      enddo

      do j=oas_jmin_v, oas_jmax_v
        do i=oas_imin_v, oas_imax_v
          if (frcv(i,j,oas_VST) .ne. 0) svstr(i,j)=cff*frcv(i,j,oas_VST)
        enddo           
      enddo


! Solar Radiative Flux 
# ifdef DIURNAL_SRFLUX
! DIURNAL CYCLE - USED IN BOTH PHYSICAL AND ECOSYSTEM MODELS
! Patrick Marchesiello - 1999: Modulate average dayly insolation
! to get diurnal cycle by:
!
!              cos(h)*cos(d)*cos(phi) + sin(d)*sin(phi)
!       pi * ---------------------------------------------
!             sin(h0)*cos(d)*cos(phi) + h0*sin(d)sin(phi)
!
! where: h, d, phi -- are hour, declination, latitude angles;
!        h0 is hour angle at sunset and sunrise
!
! Yusuke Uchiyama, 2009: UTC correction based on lonr is added.
!                        (ocean_)time should be in UTC.
!
#  define UTC_CORRECTION
!
#  ifndef UTC_CORRECTION
      cos_h=cos(2.*pi*(tdays+.5-int(tdays+.5)))
#  endif
      dec=-0.406*cos(deg2rad*(tdays-
     &               int(tdays*day2year)*year2day))
      cos_d=cos(dec)
      sin_d=sin(dec)
      tan_d=tan(dec)
# endif
      cff=1./(rho0*Cp)
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
          if (frcv(i,j,oas_SWR) .ne. 0) srflx(i,j)=cff*frcv(i,j, oas_SWR)
# ifdef DIURNAL_SRFLUX
#  ifdef UTC_CORRECTION
          utc=lonr(i,j)*utc_cor
          cos_h=cos(2.*pi*(tdays+.5-int(tdays+.5)+utc))
#  endif
          phi=latr(i,j)*deg2rad
          h0=acos(-tan(phi)*tan_d)
          cff1=cos_d*cos(phi)
          cff2=sin_d*sin(phi)
          cff=pi*(cos_h*cff1+cff2)/(sin(h0)*cff1+h0*cff2)
          srflx(i,j)=max(0.,cff*srflx(i,j))
# endif
        enddo
      enddo


!Net Heat Flux
      cff=1./(rho0*Cp)
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
          if (frcv(i,j,oas_nhf) .ne. 0) stflx(i,j,itemp)=cff*frcv(i,j, oas_nhf)
     &                                                    + srflx(i,j)
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


! Fresh Water Flux
      cff=0.01/86400 !from [cm/day] to [PSU m/s]
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
         if (frcv(i,j,oas_TEP) .ne. 0) stflx(i,j,isalt)=cff*frcv(i,j, oas_TEP)
     &                       *t(i,j,N,nrhs,isalt)
        enddo
      enddo



       !Write Debug File
      IF (IOASISDEBUGLVL .eq. 1) then
        IF (mype .eq. 0 .and. oas_step .eq. 1) then
           status = nf90_create("debugout_roms_rcv.nc", NF90_CLOBBER, il_file_id)
           status = nf90_def_dim(il_file_id, "x", xi_rho, dimids(1))
           status = nf90_def_dim(il_file_id, "xu", xi_u, uid)
           status = nf90_def_dim(il_file_id, "yv", eta_v, vid)
           status = nf90_def_dim(il_file_id, "y", eta_rho, dimids(2))
           status = nf90_def_dim(il_file_id, "step", NF90_UNLIMITED, dimids(3))
           status = nf90_def_var(il_file_id, "RO_UST_A", NF90_DOUBLE, (/ uid, dimids(2), dimids(3) /), il_var_id(1))
           status = nf90_def_var(il_file_id, "RO_VST_A", NF90_DOUBLE, (/ dimids(1), vid, dimids(3) /), il_var_id(2))
           status = nf90_def_var(il_file_id, "RO_NHF_A", NF90_DOUBLE, dimids, il_var_id(3))
           status = nf90_def_var(il_file_id, "RO_SWR_A", NF90_DOUBLE, dimids, il_var_id(4))
           status = nf90_def_var(il_file_id, "RO_TEP_A", NF90_DOUBLE, dimids, il_var_id(5))
           status = nf90_def_var(il_file_id, "SUSTR", NF90_DOUBLE, (/ uid, dimids(2), dimids(3) /), il_var_id(6))
           status = nf90_def_var(il_file_id, "SVSTR", NF90_DOUBLE, (/ dimids(1), vid, dimids(3) /), il_var_id(7))
           status = nf90_def_var(il_file_id, "NHF", NF90_DOUBLE, dimids, il_var_id(8))
           status = nf90_def_var(il_file_id, "FRESH", NF90_DOUBLE, dimids, il_var_id(9))
           status = nf90_def_var(il_file_id, "SWR", NF90_DOUBLE, dimids, il_var_id(10))
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
              status = nf90_inq_varid(il_file_id, "SUSTR",  il_var_id(6))
              status = nf90_inq_varid(il_file_id, "SVSTR",  il_var_id(7))
              status = nf90_inq_varid(il_file_id, "NHF",  il_var_id(8))
              status = nf90_inq_varid(il_file_id, "FRESH",  il_var_id(9))
              status = nf90_inq_varid(il_file_id, "SWR",  il_var_id(10))
              DO jn = 1, krcv
               if(jn .eq. 1) then
                 status = nf90_put_var(il_file_id, il_var_id(jn), frcv(oas_imin_u:oas_imax_u, oas_jmin_u:oas_jmax_u,oas_UST),
     &                                start = (/ oas_start_u(1),
     &                                           oas_start_u(2), (oas_step-1) /),
     &                                count = (/ oas_xi_u, oas_eta_u, 1 /) )
                 status = nf90_put_var(il_file_id, il_var_id(6), sustr(oas_imin_u:oas_imax_u, oas_jmin_u:oas_jmax_u),
     &                                start = (/ oas_start_u(1),
     &                                           oas_start_u(2), (oas_step-1) /),
     &                                count = (/ oas_xi_u, oas_eta_u, 1 /) )
               endif
               if(jn .eq. 2) then
                 status = nf90_put_var(il_file_id, il_var_id(jn), frcv(oas_imin_v:oas_imax_v, oas_jmin_v:oas_jmax_v,oas_VST),
     &                                start = (/ oas_start_v(1),
     &                                           oas_start_v(2), (oas_step-1) /),
     &                                count = (/ oas_xi_v, oas_eta_v, 1 /) )
                 status = nf90_put_var(il_file_id, il_var_id(7), svstr(oas_imin_v:oas_imax_v, oas_jmin_v:oas_jmax_v),
     &                                start = (/ oas_start_v(1),
     &                                           oas_start_v(2), (oas_step-1) /),
     &                                count = (/ oas_xi_v, oas_eta_v, 1 /) )
               endif
               if(jn .ge. 3) then
                 status = nf90_put_var(il_file_id, il_var_id(jn), frcv(oas_imin:oas_imax, oas_jmin:oas_jmax,jn),
     &                                start = (/ oas_start(1),
     &                                           oas_start(2), (oas_step-1) /),
     &                                count = (/ oas_xi, oas_eta, 1 /) )
               endif
              ENDDO 
              status = nf90_put_var(il_file_id, il_var_id(8), stflx(oas_imin:oas_imax, oas_jmin:oas_jmax,itemp),
     &                                start = (/ oas_start(1),
     &                                           oas_start(2), (oas_step-1) /),
     &                                count = (/ oas_xi, oas_eta, 1 /) )             
              status = nf90_put_var(il_file_id, il_var_id(9), stflx(oas_imin:oas_imax, oas_jmin:oas_jmax,isalt),
     &                                start = (/ oas_start(1),
     &                                           oas_start(2), (oas_step-1) /),
     &                                count = (/ oas_xi, oas_eta, 1 /) )             
              status = nf90_close(il_file_id)
              status = nf90_put_var(il_file_id, il_var_id(10), srflx(oas_imin:oas_imax, oas_jmin:oas_jmax),
     &                                start = (/ oas_start(1),
     &                                           oas_start(2), (oas_step-1) /),
     &                                count = (/ oas_xi, oas_eta, 1 /) )                         
              status = nf90_close(il_file_id)

           ENDIF
        ENDDO
      ENDIF !Debug



      end
