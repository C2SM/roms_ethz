#include "cppdefs.h"
      subroutine oas_roms_send(oas_step)
      !**** *oas_roms_prepsnd*  - 
!
!     Purpose.
!     --------
!     Send coupling fields to Atmospheric model
!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_send*
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
!     Modifications:
!     --------------
!       Matthieu Leclair - ETHZ
!

      USE oas_roms_def, ONLY: IOASISDEBUGLVL,
     &                        ssnd, oas_itemp,
     &                        cpl_grd, k_rho

      USE oas_roms_exchange, ONLY: oas_roms_snd,
     &                             oas_roms_dbg_snd

      implicit none
#include "param.h"
      integer oas_step, oas_dt
#include "scalars.h"
#include "ocean3d.h"
#include "ncvars.h"
#include "netcdf.inc"
      
! Update fields for sending
! -------------------------
      ssnd(oas_itemp)%pdata = t(cpl_grd(k_rho)%imin:cpl_grd(k_rho)%imax,
     &     cpl_grd(k_rho)%jmin:cpl_grd(k_rho)%jmax,N,nstp,itemp)
      
! Write Debug File
! ----------------
      if ((IOASISDEBUGLVL == 1) .OR. (IOASISDEBUGLVL == 3)) then
        call oas_roms_dbg_snd(oas_step)
      endif

      if ( IOASISDEBUGLVL == 3 ) then
          write(6,*) ' RST_roms.nc restart file written'
          write(6,*) ' we stop simulation'
          stop
        endif
      endif

! Send the Coupling Fields
! ------------------------
      oas_dt=int((oas_step)*dt)
      call oas_roms_snd(oas_dt)

      end
