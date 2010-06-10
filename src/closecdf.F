#include "cppdefs.h"
                                !
      subroutine closecdf       ! Close output netCDF files.
      implicit none             !
      integer ierr
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "netcdf.inc"
#ifdef FLOATS
# include "ncvars_floats.h"
#endif
 
      if (ncidrst.ne.-1) ierr=nf_close(ncidrst)
      if (ncidhis.ne.-1) ierr=nf_close(ncidhis)
#ifdef AVERAGES
      if (ncidavg.ne.-1) ierr=nf_close(ncidavg)
#endif
#ifdef FLOATS
      if (ncidflt.ne.-1) ierr=nf_close(ncidflt)
#endif
 
      MPI_master_only write(stdout,'(/1x,2A,I5,4(/8x,A,I5)/)')
     &  'MAIN - ',
     &  'number of records written into restart  file(s):', nrecrst,
     &  'number of records written into history  file(s):', nrechis
#ifdef AVERAGES
     &  ,'number of records written into averages file(s):', nrecavg
#endif
#ifdef FLOATS
     &  ,'number of records written into floats file(s):', nrecflt
#endif
#ifdef STATIONS
     &  ,'number of records written into averages file(s):', nrecsta
#endif
 
      if (may_day_flag.eq.0) then
        MPI_master_only write(stdout,'(/1x,A/)') 'MAIN: DONE'
      elseif (may_day_flag.eq.1) then
        write(stdout,'(/1x,A/)') 'MAIN: Abnormal termination: BLOWUP'
      elseif (may_day_flag.eq.2) then
        write(stdout,'(/1x,A/)')
     &               '### ERROR: Cause of termination: netCDF input.'
      elseif (may_day_flag.eq.3) then
        write(stdout,'(/1x,A/)')
     &              '### ERROR: Cause of termination: netCDF output.'
      elseif (may_day_flag.eq.4) then
          write(stdout,'(/1x,A/)')
     &                          '### ERROR: Cannot open netCDF file.'
      elseif (may_day_flag.eq.5) then
        write(stdout,'(/1x,A/)')
     &                    '### ERROR: read_inp: Error in input file.'
      elseif (may_day_flag.eq.6) then
        write(stdout,'(/1x,A/)')
     &         '### ERROR: read_inp: An input file is not available.'
      elseif (may_day_flag.eq.7) then
          write(stdout,'(/1x,A/)') '### ERROR: KPP algoritm failure.'
      elseif (may_day_flag.eq.8) then
        write(stdout,'(/1x,A/)') '### ERROR: Wrong choice of tiling.'
      endif

      MPI_master_only call system('echo End time  `date`')

      return
      end
 
