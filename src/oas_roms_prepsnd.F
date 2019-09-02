#include "cppdefs.h"
      subroutine oas_roms_prepsnd(oas_step)
      !**** *oas_roms_prepsnd*  - 
!
!     Purpose.
!     --------
!     Prepare fields for sending to Atmospheric model
!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_prepsnd*
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
      integer oas_dt, oas_step, info
#include "scalars.h"
#include "ocean3d.h"
#include "ncvars.h"
#include "netcdf.inc"

      integer  start(4), count(4),i,j, ierr 
      integer il_file_id, il_var_id(1), dimids(3), ib, mype, npes, status,jn
      
      !Current Time
      oas_dt=int((oas_step)*dt)
      
                    
      !Update fields for sending.
      fsndar(:,:,oas_itemp) = t(oas_imin:oas_imax,
     &                      oas_jmin:oas_jmax,N,nstp,itemp)
     
    
      cplcount(:) = cplcount(:) + 1
      
      
      !Write Debug File
      if (IOASISDEBUGLVL .EQ. 1) then
        call MPI_Comm_rank(kl_comm, mype, ierr)
        call MPI_Comm_size(kl_comm, npes, ierr)
        if (mype .eq. 0) print *, 'ROMS:OASSND_DT', oas_dt

        if (mype .eq. 0 .and. oas_step .eq. 1) then
           status = nf90_create("RST_roms.nc", NF90_CLOBBER, il_file_id)
           status = nf90_def_dim(il_file_id, "x", xi_rho, dimids(1))
           status = nf90_def_dim(il_file_id, "y", eta_rho, dimids(2))
           status = nf90_def_dim(il_file_id, "step", NF90_UNLIMITED, dimids(3))
           status = nf90_def_var(il_file_id, "SO_SST_A", NF90_DOUBLE, dimids, il_var_id(1))
           status = nf90_enddef(il_file_id)
           status = nf90_close(il_file_id)
        endif

        do ib = 0, npes - 1

           call MPI_Barrier(kl_comm, ierr)
           if (mype .eq. ib) then
              status = nf90_open("RST_roms.nc", NF90_WRITE, il_file_id)
              status = nf90_inq_varid(il_file_id, "SO_SST_A" , il_var_id(1))
              do jn = 1, ksnd
                 status = nf90_put_var(il_file_id, il_var_id(jn),
     &              t(oas_imin:oas_imax,oas_jmin:oas_jmax,N,nstp,itemp),
     &                                start = (/ oas_start(1),
     &                                           oas_start(2), (oas_step-1) /),
     &                                count = (/ oas_xi, oas_eta /) )
              enddo
              status = nf90_close(il_file_id)
           endif
    
        enddo

      if ( IOASISDEBUGLVL .EQ. 3 ) then
        ! End write coupling fields
          write(6,*) ' RST_roms.nc restart file written'
          write(6,*) ' we stop simulation'
          stop
        endif
      endif !Debug


      !Send the Coupling Fields


      do jn = 1,ksnd
       !Send average field. Average over coupling time step.
       if( ssnd(jn)%laction ) call oas_roms_snd( jn, oas_dt, fsndar(:,:,jn), info )
       !If coupling time step, Reset coupling count and flds to zero.
       if ( info .EQ. PRISM_Sent .OR. info .EQ. PRISM_ToRest .OR.   
     & info .EQ. PRISM_SentOut  .OR. info .EQ. PRISM_ToRestOut ) then
           fsndar(:,:,jn) = 0.
           cplcount(jn) = 0
       endif
      enddo

      end
