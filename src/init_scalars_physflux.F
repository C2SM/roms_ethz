!
!
#include "cppdefs.h"
#ifdef PHYS_FLUX_ANALYSIS

       subroutine init_scalars_physflux(ierr)
!
! Set initial values for  globally accessible (stored in common
! blocks) scalar variables of the NPZDOC model.
!
      implicit none
      integer ierr, i, j, itrc, lvar, lenstr
C$    integer omp_get_num_threads
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "physflux.h"
#include "ncroms_physflux.h"

! Find out how many threads are created and check whether the number
! of threads exceeds maximum allowed, as well as check if the number
! of tiles is divisible by the number of threads, so the job can be
! evenly partitioned, complain and signal to terminate, if something
! is wrong. 
! 
C$OMP PARALLEL
C$OMP CRITICAL (isca_cr_rgn)
      numthreads=1
C$    numthreads=omp_get_num_threads()
C$OMP END CRITICAL (isca_cr_rgn)
C$OMP END PARALLEL
      MPI_master_only write(*,'(1x,A,3(1x,A,I3),A)') 'NUMBER',
     &    'OF THREADS:',numthreads,'BLOCKING:',NSUB_X,'x',NSUB_E,'.'

#ifdef MPI
# define NUMPROC NNODES
#else
# define NUMPROC 1
#endif
      if (numthreads.gt.NUMPROC) then
        MPI_master_only write(*,'(/1x,A,I3/)')
     &    'ERROR: Requested number of threads exceeds setting: ',NUMPROC
        ierr=ierr+1                                      !--> ERROR
      elseif (mod(NSUB_X*NSUB_E,numthreads).ne.0) then
        MPI_master_only write(*,
     &                '(/1x,A,1x,A,I3,4x,A,I3,4x,A,I4,A)') 'ERROR:',
     &                'wrong choice of numthreads =', numthreads,
     &                'NSUB_X =', NSUB_X, 'NSUB_E =', NSUB_E, '.'
        ierr=ierr+1                                      !--> ERROR
      endif
!
!
! Initialize netCDF files IDs to closed status.
!
      ncid_phys_flux_his = -1
#ifdef AVERAGES
      ncid_phys_flux_avg = -1
#endif

! Reset record counters for phys flux analysis averages/history output
!
      nrec_phys_flux_his=0
#ifdef AVERAGES
      nrec_phys_flux_avg=0
#endif
!
! Define names of variables in NetCDF output files.
! Inner dimension is for variable type and outer is for variable
! attributes (name, long-name, units, field).
!
#ifdef SOLVE3D
      do itrc = 1, NT_PFA
         lvar = lenstr(vname(1,indxT+itrc-1))
! horizontal advective fluxes in xi direction
         vname_phys(1,indxHorXAdvFlux+itrc-1) = 
     &        'HorXAdvFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxHorXAdvFlux+itrc-1) = 
     &        'HorXAdvFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxHorXAdvFlux+itrc-1) = 
     &           'Horizontal (xi) advective flux of heat'
         else
            vname_phys(2,indxHorXAdvFlux+itrc-1) = 
     &           'Horizontal (xi) advective flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         if (itrc .eq. 1) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'degC m3 s-1'
# ifdef SALINITY
         else if (itrc .eq. 2) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'PSU m3 s-1'
# endif
# ifdef PASSIVE_TRACER
         else if (itrc .ge. 2+ntrc_salt .and. 
     &           itrc .le. 1+ntrc_salt+ntrc_pas) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'm3 s-1'
# endif
         else
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'mmol s-1'
         end if
#ifdef BIOLOGY_NPZDOC
         if (itrc .eq. iChla) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'mg Chl-a s-1'
         end if
#endif
#ifdef BIOLOGY_BEC
         if (itrc .eq. iSPCHL .or. itrc .eq. iDIATCHL .or.
     &        itrc .eq. iDIAZCHL) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'mg Chl-a s-1'
         end if
#endif

! horizontal advective fluxes in eta direction
         vname_phys(1,indxHorYAdvFlux+itrc-1) = 
     &        'HorYAdvFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxHorYAdvFlux+itrc-1) = 
     &        'HorYAdvFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxHorYAdvFlux+itrc-1) = 
     &           'Horizontal (eta) advective flux of heat'
         else
            vname_phys(2,indxHorYAdvFlux+itrc-1) = 
     &           'Horizontal (eta) advective flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxHorYAdvFlux+itrc-1) = 
     &        vname_phys(3,indxHorXAdvFlux+itrc-1)

! vertical advective fluxes
         vname_phys(1,indxVertAdvFlux+itrc-1) = 
     &        'VertAdvFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxVertAdvFlux+itrc-1) = 
     &        'VertAdvFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxVertAdvFlux+itrc-1) = 
     &           'Vertical advective flux of heat'
         else
            vname_phys(2,indxVertAdvFlux+itrc-1) = 
     &           'Vertical advective flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         if (itrc .eq. 1) then
            vname_phys(3,indxVertAdvFlux+itrc-1) = 
     &           'degC m s-1'
# ifdef SALINITY
         else if (itrc .eq. 2) then
            vname_phys(3,indxVertAdvFlux+itrc-1) = 
     &           'PSU m s-1'
# endif
# ifdef PASSIVE_TRACER
         else if (itrc .ge. 2+ntrc_salt .and. 
     &           itrc .le. 1+ntrc_salt+ntrc_pas) then
            vname_phys(3,indxVertAdvFlux+itrc-1) = 
     &           'm s-1'
# endif
         else
            vname_phys(3,indxVertAdvFlux+itrc-1) = 
     &           'mmol m-2 s-1'
         end if
#ifdef BIOLOGY_NPZDOC
         if (itrc .eq. iChla) then
            vname_phys(3,indxVertAdvFlux+itrc-1) = 
     &           'mg Chl-a m-2 s-1'
         end if
#endif
#ifdef BIOLOGY_BEC
         if (itrc .eq. iSPCHL .or. itrc .eq. iDIATCHL .or.
     &        itrc .eq. iDIAZCHL) then
            vname_phys(3,indxVertAdvFlux+itrc-1) = 
     &           'mg Chl-a m-2 s-1'
         end if
#endif

#ifdef VERT_DIFF_ANALYSIS
! vertical diffusive fluxes
         vname_phys(1,indxVertDiffFlux+itrc-1) = 
     &        'VertDiffFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxVertDiffFlux+itrc-1) = 
     &        'VertDiffFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxVertDiffFlux+itrc-1) = 
     &           'Vertical diffusive flux of heat'
         else
            vname_phys(2,indxVertDiffFlux+itrc-1) = 
     &           'Vertical diffusive flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxVertDiffFlux+itrc-1) = 
     &        vname_phys(3,indxVertAdvFlux+itrc-1)

#endif /* VERT_DIFF_ANALYSIS */
# if defined FULL_PHYS_FLUX_ANALYSIS || defined TOP_FLUX_ANALYSIS
! top fluxes
         vname_phys(1,indxTopFlux+itrc-1) = 
     &        'TopFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxTopFlux+itrc-1) = 
     &        'TopFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxTopFlux+itrc-1) = 
     &           'Top flux of heat'
         else
            vname_phys(2,indxTopFlux+itrc-1) = 
     &           'Top flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxTopFlux+itrc-1) = 
     &        vname_phys(3,indxVertAdvFlux+itrc-1)
#endif
# if defined FULL_PHYS_FLUX_ANALYSIS && !defined SELECTED_FLUX_ANALYSIS /* AH */
! bottom fluxes
         vname_phys(1,indxBottomFlux+itrc-1) = 
     &        'BottomFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxBottomFlux+itrc-1) = 
     &        'BottomFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxBottomFlux+itrc-1) = 
     &           'Bottom flux of heat'
         else
            vname_phys(2,indxBottomFlux+itrc-1) = 
     &           'Bottom flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxBottomFlux+itrc-1) = 
     &        vname_phys(3,indxVertAdvFlux+itrc-1)

! horizontal mixing fluxes in xi direction
         vname_phys(1,indxHorXMixFlux+itrc-1) = 
     &        'HorXMixFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxHorXMixFlux+itrc-1) = 
     &        'HorXMixFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxHorXMixFlux+itrc-1) = 
     &           'Horizontal (xi) mixing flux of heat'
         else
            vname_phys(2,indxHorXMixFlux+itrc-1) = 
     &           'Horizontal (xi) mixing flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxHorXMixFlux+itrc-1) = 
     &        vname_phys(3,indxHorXAdvFlux+itrc-1)

! horizontal mixing fluxes in eta direction
         vname_phys(1,indxHorYMixFlux+itrc-1) = 
     &        'HorYMixFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxHorYMixFlux+itrc-1) = 
     &        'HorYMixFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxHorYMixFlux+itrc-1) = 
     &           'Horizontal (eta) mixing flux of heat'
         else
            vname_phys(2,indxHorYMixFlux+itrc-1) = 
     &           'Horizontal (eta) mixing flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxHorYMixFlux+itrc-1) = 
     &        vname_phys(3,indxHorYAdvFlux+itrc-1)

! vertical mixing fluxes
         vname_phys(1,indxVertMixFlux+itrc-1) = 
     &        'VertMixFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxVertMixFlux+itrc-1) = 
     &        'VertMixFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxVertMixFlux+itrc-1) = 
     &           'Vertical mixing flux of heat'
         else
            vname_phys(2,indxVertMixFlux+itrc-1) = 
     &           'Vertical mixing flux of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         vname_phys(3,indxVertMixFlux+itrc-1) = 
     &        vname_phys(3,indxVertAdvFlux+itrc-1)

! nudging fluxes
         vname_phys(1,indxNudgingFlux+itrc-1) = 
     &        'NudgingFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)
         vname_phys(4,indxNudgingFlux+itrc-1) = 
     &        'NudgingFlux_'/ /vname(1,indxT+itrc-1)(1:lvar)/ /
     &        ', scalar, series'
         if (itrc .eq. 1) then
            vname_phys(2,indxNudgingFlux+itrc-1) = 
     &           'Nudging divergence of heat'
         else
            vname_phys(2,indxNudgingFlux+itrc-1) = 
     &           'Nudging divergence of '/ /
     &           vname(1,indxT+itrc-1)(1:lvar)
         end if
         if (itrc .eq. 1) then
            vname_phys(3,indxNudgingFlux+itrc-1) = 
     &           'degC m3 s-1'
# ifdef SALINITY
         else if (itrc .eq. 2) then
            vname_phys(3,indxNudgingFlux+itrc-1) = 
     &           'PSU m3 s-1'
# endif
# ifdef PASSIVE_TRACER
         else if (itrc .ge. 2+ntrc_salt .and. 
     &           itrc .le. 1+ntrc_salt+ntrc_pas) then
            vname_phys(3,indxNudgingFlux+itrc-1) = 
     &           'm3 s-1'
# endif
         else
            vname_phys(3,indxNudgingFlux+itrc-1) = 
     &           'mmol m-3 s-1'
         end if
#ifdef BIOLOGY_NPZDOC
         if (itrc .eq. iChla) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'mg Chl-a m-3 s-1'
         end if
#endif
#ifdef BIOLOGY_BEC
         if (itrc .eq. iSPCHL .or. itrc .eq. iDIATCHL .or.
     &        itrc .eq. iDIAZCHL) then
            vname_phys(3,indxHorXAdvFlux+itrc-1) = 
     &           'mg Chl-a m-3 s-1'
         end if
#endif
# endif /* FULL_PHYS_FLUX_ANALYSIS */
      end do
# if defined FULL_PHYS_FLUX_ANALYSIS || defined TOP_FLUX_ANALYSIS
! shortwave radiation absorption fluxes !AH15
      vname_phys(1,indxSRAbsFlux) = 'SRAbsFlux'
      vname_phys(4,indxSRAbsFlux) = 'SRAbsFlux'/ /
     &        ', scalar, series'
      vname_phys(2,indxSRAbsFlux) = 
     &           'Shortwave radiation absorption flux'
      vname_phys(3,indxSRAbsFlux) = 
     &           'degC m s-1'
# endif
#endif /* SOLVE3D */
      return
      end
#else /* PHYS_FLUX_ANALYSIS */
      subroutine init_scalars_physflux_empty()
      return
      end
#endif /* PHYS_FLUX_ANALYSIS */
