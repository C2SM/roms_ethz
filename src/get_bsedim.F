#include "cppdefs.h"
#if defined SG_BBL96 && !defined  ANA_BSEDIM
 
      subroutine get_bsedim          ! Read initial sediment grain
                                     ! size and density from forcing
      implicit none                  ! NetCDF file.
# include "param.h"
# include "bblm.h"
# include "ncvars.h"
# include "netcdf.inc"
      logical gotbsd, gotbss
      integer bsdid, bssid, i, lstr, lsvar, lvar, status
      integer lenstr, nf_fread
 
      if (may_day_flag.ne.0) return      !-->  EXIT
!
!  Inquire about the contents of forcing NetCDF file.
!  Inquire about the dimensions and variables. Check for consistency.
!
      lstr=lenstr(frcname)
      call opencdf (frcname,N)
      if (may_day_flag.ne.0) return      !-->  EXIT
!
!  Scan variable list from input NetCDF and check for bottom sediment
!  grain size and density variables.
!
      do i=1,nvars
        lvar=lenstr(varnam(i))
        lsvar=lenstr(vname(1,indxBSD))
        if (varnam(i)(1:lvar).eq.vname(1,indxBSD)(1:lsvar)) then
          bsdid=i
          gotbsd=.true.
        endif
        lsvar=lenstr(vname(1,indxBSS))
        if (varnam(i)(1:lvar).eq.vname(1,indxBSS)(1:lsvar)) then
          bssid=i
          gotbss=.true.
        endif
      enddo
!
!  Terminate execution if bottom sediment variables are not found.
!
      if (.not.gotbsd) then
        lsvar=lenstr(vname(1,indxBSD))
        write(stdout,10) vname(1,indxBSD)(1:lsvar), frcname(1:lstr)
        may_day_flag=2
        return                           !-->  EXIT
      endif
      if (.not.gotbss) then
        lsvar=lenstr(vname(1,indxBSS))
        write(stdout,10) vname(1,indxBSS)(1:lsvar), frcname(1:lstr)
        may_day_flag=2
        return                           !-->  EXIT
      endif
!
!  Open forcing NetCDF file for reading.
!
      ncid=ncidfrc(ifrc)
      if (ncid.eq.-1) then
        status=nf_open(frcname(1:lstr), nf_nowrite, ncid)
        if (status.eq.nf_noerr) then
          ncidfrc(ifrc)=ncid
        else
          write(stdout,20) frcname(1:lstr)
          may_day_flag=2
          return                         !-->  EXIT
        endif
      endif
!
!  Read in initial sediment grain size and density.
!---------------------------------------------------
!  Read in sediment grain diameter size (m).
!
      status=nf_fread(Ssize(START_2D_ARRAY),ncid,bssid,0,r2dvar)
      if (status.ne.NF_NOERR) then
        write(stdout,30) 'Ssize'
        may_day_flag=2
        return                           !-->  EXIT
      endif
!
!  Read in sediment grain density (kg/m^3).
!
      status=nf_fread(Sdens(START_2D_ARRAY),ncid,bsdid,0,r2dvar)
      if (status.ne.NF_NOERR) then
        write(stdout,30) 'Sdens'
        may_day_flag=2
        return                           !-->  EXIT
      endif
!
  10  format(/' GET_BSEDIM - unable to find forcing variable: ',a,
     &       /15x,'in forcing NetCDF file: ',a)
  20  format(/' GET_BSEDIM - unable to open forcing NetCDF file: ',a)
  30  format(/' GET_BSEDIM - error while reading variable: ',a)
#else
      subroutine get_bsedim_empty
#endif /* SG_BBL96 && !ANA_BSEDIM */
      return
      end
 
