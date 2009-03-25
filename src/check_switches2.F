#include "cppdefs.h"
                                         ! Checking compatibility of
      subroutine check_tiling (ierr)     ! dimensions of the grid and 
      implicit none                      ! partitioning parameters: 
      integer ierr, tile                 ! logic of the code requires
#include "param.h"
      do tile=0,NSUB_X*NSUB_E-1            ! tiles to be at least
        call check_tile_bounds (tile,ierr) ! two-point wide.
      enddo                                ! Incorrect settings of
      return                               ! NSUB_X and NSUB_E may
      end                                  ! result in smaller size.

      subroutine check_tile_bounds (tile, ierr)
      implicit none
      integer tile, ierr, isize,jsize
#include "param.h"
#include "compute_tile_bounds.h"

      isize=iend-istr+1                  ! NOTE: run terminates, if
      jsize=jend-jstr+1                  ! violation occurs here.

      if (isize.lt.2) then
        ierr=ierr+1
        write(*,'(1x,2A,I3,1x,A,I4,3x,A,I3,1x,A,2I4)') 'ERROR: ',
     &           'NSUB_X =', NSUB_X, 'is incompatible with Lm =',
     &           Lm, 'i_X =', i_X,  '==> istr,iend =', istr,iend
      endif
      if (jsize.lt.2) then
        ierr=ierr+1
        write(*,'(1x,2A,I3,1x,A,I4,3x,A,I3,1x,A,2I4)') 'ERROR: ',
     &           'NSUB_E =', NSUB_E, 'is incompatible with Mm =',
     &           Mm, 'j_E =', j_E,  '==> jstr,jend =', jstr,jend
      endif
      return
      end





      subroutine check_switches2 (ierr)
      implicit none
      integer ierr, ncoupl, nobc_west, nobc_east,
     &              nobc_south, nobc_north, nAk
#include "param.h"
#include "scalars.h"

!
! Checking the correctness of coupling flags:
!
      ncoupl=0 
#ifdef PRED_COUPLED_MODE
      ncoupl=ncoupl+1
#endif
#ifdef CORR_COUPLED_MODE 
      ncoupl=ncoupl+1
#endif
      if (ncoupl.eq.0) then
        write(stdout,'(/1x,3A/)') 'ERROR:  neither ',
     &            'PRED_COUPLED_MODE nor CORR_COUPLED_MODE ',
     &            'are defined in "set_global_definitions.h".'
        ierr=ierr+1
      elseif (ncoupl.gt.1) then
        write(stdout,'(/1x,2A/8x,2A/)')  'ERROR: misconfigured ',
     &       'switches PRED_COUPLED_MODE and CORR_COUPLED_MODE ',
     &       'in "set_global_definitions.h": only one of them ',
     &       'must be defined.'
        ierr=ierr+1
      endif





!
! Checking the number of selections of boundary conditions.
!
#ifdef EW_PERIODIC
      nobc_west=1     ! here "nobc_west=1" means that it is already 
      nobc_east=1     ! chosen and no further selection is possible
#else
      nobc_west=0
      nobc_east=0
#endif
#ifdef OBC_WEST
      nobc_west=nobc_west+1
#endif
#ifdef OBC_EAST
      nobc_east=nobc_east+1
#endif
      if (nobc_west.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &        'one boundary condition is chosen on the WESTERN EGGE.'
        ierr=ierr+1
      endif
      if (nobc_east.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &        'one boundary condition is chosen on the EASTERN EGGE.'
        ierr=ierr+1
      endif
 
#ifdef NS_PERIODIC
      nobc_south=1
      nobc_north=1
#else
      nobc_south=0
      nobc_north=0
#endif
#ifdef OBC_SOUTH
      nobc_south=nobc_south+1
#endif
#ifdef OBC_NORTH
      nobc_north=nobc_north+1
#endif
      if (nobc_south.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &       'one boundary condition is chosen on the SOUTHERN EGGE.'
        ierr=ierr+1
      endif
      if (nobc_north.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &       'one boundary condition is chosen on the NORTHERN EGGE.'
        ierr=ierr+1
      endif
!
! Checking the number of selections vertical mixing schemes.
!
      nAk=0
#ifdef ANA_VMIX
      nAk=nAk+1
#endif
#ifdef BVF_MIXING
      nAk=nAk+1
#endif
#ifdef LMD_MIXING
      nAk=nAk+1
#endif
#ifdef MY2_MIXING
      nAk=nAk+1
#endif
#ifdef MY25_MIXING
      nAk=nAk+1
#endif
#ifdef PP_MIXING
      nAk=nAk+1
#endif
      if (nAk.gt.1) then
        write(stdout,'(1x,A,1x,A)') 'ERROR in "cppdefs.h":',
     &       'more than one vertical mixing scheme is chosen.'
        ierr=ierr+1
      endif
 
#ifdef SPLIT_EOS
# ifndef NONLIN_EOS
      write(stdout,'(1x,A,2(/8x,A))') 'ERROR in "cppdefs.h":',
     &       'illegal combination of CPP-switches, SPLIT_EOS'
     &       'is defined, but NONLIN_EOS is not.'
      ierr=ierr+1
# endif
#endif
 
!
! Stop if explicit time-step splitting on shallow water set-up.
!
#ifndef SOLVE3D
      if (ndtfast.gt.1) then
        write(stdout,'(1x,A,I3,1x,A/8x,A,6x,A)') 'ERROR: NDTFAST =',
     &         ndtfast, 'is greater than unity for a shallow water',
     &        'configuration.','Change it to unity in startup file.'
     &
        ierr=ierr+1
      endif
#endif
      return
      end
 
