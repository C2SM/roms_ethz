#include "cppdefs.h"
#ifdef SOLVE3D
 
c--#define CONST_TRACERS
 
      subroutine step3d_t (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call step3d_t_tile (istr,iend,jstr,jend, A2d(1,1), A2d(1,2),
     &                                         A2d(1,3), A2d(1,4),
     &                               A2d(1,1), A2d(1,2), A2d(1,3))
      return
      end
 
      subroutine step3d_t_tile (istr,iend,jstr,jend,
     &                   FX,FE,WORK,dTdz,  FC,CF,DC)
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, itrc,
     &        imin,imax,jmin,jmax, i,j,k, indx
      real FX(PRIVATE_2D_SCRATCH_ARRAY),
     &     FE(PRIVATE_2D_SCRATCH_ARRAY),  cff,
     &   WORK(PRIVATE_2D_SCRATCH_ARRAY),  epsil,
     &   dTdz(PRIVATE_2D_SCRATCH_ARRAY,2),
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N)
      parameter (epsil=1.E-16)
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
# include "sources.h"
#ifdef PHYS_FLUX_ANALYSIS
# include "physflux.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
! temporary variable for calculation of vertical diffusive flux
      real ThisVDF(PRIVATE_2D_SCRATCH_ARRAY,0:N)
# endif /* PHYS_FLUX_ANALYSIS */
!
# include "compute_auxiliary_bounds.h"
!
      do itrc=1,NT
        do k=1,N
 
# include "compute_horiz_tracer_fluxes.h"
 
          do j=jstr,jend
            do i=istr,iend
              t(i,j,k,nnew,itrc)=Hz_bak(i,j,k)*t(i,j,k,nstp,itrc)
     &                     -dt*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                          +FE(i,j+1)-FE(i,j)
     &                                                           )
            enddo
          enddo
# ifdef PHYS_FLUX_ANALYSIS
          if (itrc .le. NT_PFA) then
! separate do loops (for better efficiency) to store the fluxes
             j_loop2: do j=Jstr,Jend
                do i=Istr,Iend+1
                                ! unit: mmol / s
                   HorXAdvFlux(i,j,k,itrc) = FX(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
                end do
             end do j_loop2
             j_loop3: do j=Jstr,Jend+1
                do i=Istr,Iend
                                ! unit: mmol / s
                   HorYAdvFlux(i,j,k,itrc) = FE(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
                enddo
             end do j_loop3
          end if
# endif /* PHYS_FLUX_ANALYSIS */
        enddo
      enddo
 
      do j=jstr,jend
        do itrc=1,NT
 
# include "compute_vert_tracer_fluxes.h"
 
          do k=1,N            ! Apply vertical advective fluxes.
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-dt*pm(i,j)
     &                             *pn(i,j)*(FC(i,k)-FC(i,k-1))
            enddo
          enddo
# ifdef PHYS_FLUX_ANALYSIS
          if (itrc .le. NT_PFA) then
! separate do loop (for better efficiency) to store the fluxes
             do k=0,N
                do i=Istr,Iend
               ! unit: mmol / (m2 s)
                   VertAdvFlux(i,j,k,itrc) = FC(i,k)
     &                  * pm(i,j) * pn(i,j)
# ifdef MASKING
     &                  * rmask(i,j)
# endif
                enddo
             enddo
          end if
# endif /* PHYS_FLUX_ANALYSIS */
!
! Add surface and bottom fluxes
!
          do i=istr,iend
            t(i,j,N,nnew,itrc)=t(i,j,N,nnew,itrc)+dt*stflx(i,j,itrc)
          enddo
# ifdef FULL_PHYS_FLUX_ANALYSIS
          if (itrc .le. NT_PFA) then
! separate do loops (for better efficiency) to store the fluxes
             do i=Istr,Iend
             ! unit = mmol / (m2 s)
                TopFlux(i,j,itrc) = stflx(i,j,itrc)
#  ifdef MASKING
     &               * rmask(i,j)
#  endif
             enddo
             do i=Istr,Iend
             ! unit = mmol / (m2 s)
                BottomFlux(i,j,itrc) = 0 !-btflx(i,j,itrc)
#   ifdef MASKING
     &               * rmask(i,j)
#   endif
             enddo
          end if
# endif /* FULL_PHYS_FLUX_ANALYSIS */
# ifdef LMD_KPP
!
! Add the solar radiation flux in temperature equation. Also compute
! the nonlocal transport flux for unstable (convective) forcing
! conditions into matrix DC when using the Large et al. 1994 KPP
! scheme.
!
          if (itrc.eq.itemp) then
            do k=N-1,1,-1
              do i=istr,iend
                cff=srflx(i,j)*swr_frac(i,j,k)
#  ifdef LMD_NONLOCAL
     &                 -ghat(i,j,k)*(stflx(i,j,itemp)-srflx(i,j))
#  endif
                t(i,j,k+1,nnew,itemp)=t(i,j,k+1,nnew,itemp) -dt*cff
                t(i,j,k  ,nnew,itemp)=t(i,j,k  ,nnew,itemp) +dt*cff
              enddo
            enddo

c??
c?? WARNING: the above implies that light (if any) reaching all the
c?? way to the bottom is entirely absorbed within the botom-most grid
c?? box, rather that reflected/scatered back to the water column. It 
c?? is not clear, however, how to deal with this situation correctly
c??
c??         do i=istr,iend
c??           cff=srflx(i,j)*swr_frac(i,j,0)
c??#  ifdef LMD_NONLOCAL
c??  &               -ghat(i,j,0)*(stflx(i,j,itemp)-srflx(i,j))
c??#  endif
c??           t(i,j,1,nnew,itemp)=t(i,j,1,nnew,itemp) -dt*cff
c??         enddo


#  if defined LMD_NONLOCAL && defined SALINITY
          elseif (itrc.eq.isalt) then
            do k=N-1,1,-1
              do i=istr,iend
                cff=-dt*ghat(i,j,k)*stflx(i,j,isalt)
                t(i,j,k+1,nnew,isalt)=t(i,j,k+1,nnew,isalt) -cff
                t(i,j,k  ,nnew,isalt)=t(i,j,k  ,nnew,isalt) +cff
              enddo
            enddo
#  endif
          endif
# endif

!
! Perform implicit time step for vertical diffusion,
!
!   dq(k)     1     [         q(k+1)-q(k)             q(k)-q(k-1) ]
!  ------ = ----- * [ Akt(k)* ----------- - Akt(k-1)* ----------- ]
!    dt     Hz(k)   [            dz(k)                   dz(k-1)  ]
!
! where q(k) represents tracer field t(:,:,k,:,itrc). Doing so
! implies solution of a tri-diagonal system
!
!     -FC(k-1)*q_new(k-1) +[Hz(k)+FC(k-1)+FC(k)]*q_new(k)
!                       -FC(k)*q_new(k+1) = Hz(k)*q_old(k)
!
!                dt*Akt(k)
! where FC(k) = ----------- is normalized diffusivity coefficient 
!                  dz(k)
!
! defined at W-points; q_new(k) is the new-time-step (unknown) tracer
! field; q_old(k) is old-time-step tracer (known).  As long as
! vertical diffusivity Akt(k) is nonnegative, the tri-diagonal matrix
! is diagonally dominant which guarantees stability of a Gaussian
! elimination procedure, (e.g., Richtmeyer annd  Morton, 1967).
! Top and bottom boundary conditions are assumed to be no-flux,
! effectively Akt(N)=Akt(0)=0, hence FC(N)=FC(1)=0. This leads to
! equations for top and bottom grid boxes; 
!
!   -FC(N-1)*q_new(N-1) +[Hz(N)+FC(N-1)]*q_new(N) = Hz(N)*q_old(N)
!
!          [Hz(1)+FC(1)]*q_new(1) -FC(1)*q_new(2) = Hz(1)*q_old(1)
!
! The FC(N)=FC(0)=0 boundary conditions does not mean that physical
! boundary conditions are no flux: the forcing fluxes have been
! applied explicitly above.  Instead, the no-flux condition should
! be interpreted as that the implicit step merely redistributes the
! tracer concentration throughout the water column. At this moment
! the content of array t(:,:,:,nnew,itrc) has meaning of Hz*tracer.
! After the implicit step it becomes just tracer. 
!
# if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
! The vertical diffusive flux is not added in a step like the other fluxes,
! so we store the old concentration (depth is already factored in here) first, 
! and then add the new concentration.
          do k=1,N
            do i=Istr,Iend
               ThisVDF(i, j, k) = t(i,j,k,nnew,itrc)
            end do
         end do
# endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */

# ifdef AN_UNDEFINED_CASE
          indx=itrc
# else
#  ifdef SALINITY
          indx=min(itrc,isalt)
#  else
          indx=min(itrc,itemp)
#  endif
# endif
          do i=istr,iend
            FC(i,1)=dt*Akt(i,j,1,indx)/(z_r(i,j,2)-z_r(i,j,1))
            cff=1./(Hz(i,j,1)+FC(i,1))
            CF(i,1)=cff*FC(i,1)
            DC(i,1)=cff*t(i,j,1,nnew,itrc)
          enddo
          do k=2,N-1,+1
            do i=istr,iend
              FC(i,k)=dt*Akt(i,j,k,indx)/(z_r(i,j,k+1)-z_r(i,j,k))
              cff=1./( Hz(i,j,k) +FC(i,k)+FC(i,k-1)*(1.-CF(i,k-1)) )
              CF(i,k)=cff*FC(i,k)
              DC(i,k)=cff*(t(i,j,k,nnew,itrc)+FC(i,k-1)*DC(i,k-1))
            enddo
          enddo
          do i=istr,iend
             t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc) +FC(i,N-1)
     &         *DC(i,N-1) )/(Hz(i,j,N)+FC(i,N-1)*(1.-CF(i,N-1)))
          enddo
          do k=N-1,1,-1
            do i=istr,iend
              t(i,j,k,nnew,itrc)=DC(i,k)+CF(i,k)*t(i,j,k+1,nnew,itrc)
            enddo
          enddo           !--> discard FC,CF,DC

# if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
          do k=1,N
            do i=Istr,Iend
               ! ThisVDF is the old concentration times depth
               ThisVDF(i,j,k) = 
     &              (ThisVDF(i,j,k) - t(i,j,k,nnew,itrc) * Hz(i,j,k))
     &              / dt
# ifdef MASKING
     &              * rmask(i,j)
# endif
            enddo
          enddo
          if (itrc .le. NT_PFA) then
             do i=Istr,Iend
             ! unit: mmol / (m2 s)
                VertDiffFlux(i,j,0,itrc) = 0.0 ! Neumann b.c. at bottom
                do k=1,N-1
                   VertDiffFlux(i,j,k,itrc) = ThisVDF(i,j,k) +
     &                  VertDiffFlux(i,j,k-1,itrc)
                end do
                VertDiffFlux(i,j,N,itrc) = 0.0 ! Neumann b.c. at top
             end do
          end if
# endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */

#ifdef CONST_TRACERS
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
            enddo
          enddo
# if defined PHYS_FLUX_ANALYSIS && defined VERT_DIFF_ANALYSIS
          if (itrc .le. NT_PFA) then
             do k=1,N
                do i=Istr,Iend
                   VertDiffFlux(i,j,k,itrc) = 0.0
                enddo
             enddo
          end if
# endif /* PHYS_FLUX_ANALYSIS & VERT_DIFF_ANALYSIS */
#endif
        enddo      ! <-- itrc
      enddo      ! <-- j
!
! Set lateral boundary conditions; nudge toward tracer climatology;
! apply land-sea mask and exchange periodic boundary conditions.
!
      do itrc=1,NT
        call t3dbc_tile (istr,iend,jstr,jend, itrc, WORK)

# if defined MASKING || defined TCLIMATOLOGY
#  ifdef EW_PERIODIC
#   define I_RANGE istr,iend
#  else
#   define I_RANGE istrR,iendR
#  endif
#  ifdef NS_PERIODIC
#   define J_RANGE jstr,jend
#  else
#   define J_RANGE jstrR,jendR
#  endif

# if defined BIOLOGY_BEC
      enddo                                    ! Biogeochemical
                                               ! Elemental Cycles
      call ecosys_tile (I_RANGE,J_RANGE)       ! (BEC) model 

      do itrc=1,NT  
# elif defined BIOLOGY || defined BIOLOGY_NPZDOC
      enddo                                    ! Insert interactive
      call biology_tile (istr,iend,jstr,jend)  ! part of the
      do itrc=1,NT                             ! biological model
# endif
 
        do k=1,N
          do j=J_RANGE
            do i=I_RANGE
#  ifdef TCLIMATOLOGY
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)
     &       +dt*Tnudgcof(i,j,itrc)*( tclm(i,j,k,itrc)
     &                             -t(i,j,k,nnew,itrc))
#  ifdef FULL_PHYS_FLUX_ANALYSIS
          if (itrc .le. NT_PFA) then
               NudgingFlux(i,j,k,itrc) = Tnudgcof(i,j,itrc) *
     &              ( tclm(i,j,k,itrc) - t(i,j,k,nnew,itrc) )
#   ifdef MASKING
     &              * rmask(i,j)
#   endif
            end if
#  endif /* FULL_PHYS_FLUX_ANALYSIS */
#  endif
#  ifdef MASKING
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*rmask(i,j)
#  endif
            enddo
          enddo
        enddo
#  undef I_RANGE
#  undef J_RANGE
# endif
      enddo  ! <-- itrc
!
! Exchange computational margines and/or periodic boundaries:
!--------- ------------- -------- ------ -------- ----------- 
! This is done only here if the code is configured without explicit
! lateral diffusion. If there is one, then the exchange routine is
! called from the diffusive routine which immediately follows this
! step, therefore there is no need to call it here.
!
# if !defined TS_DIF2 && !defined TS_DIF4
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do itrc=1,NT
        call exchange_r3d_tile (istr,iend,jstr,jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
      enddo
#  endif
# endif
      return
      end


      subroutine check_trc_adv_switches (ierr)
!
! This routine keeps track of the status of CPP-switch settings in
! "compute_horiz_tracer_fluxes.h" and "compute_horiz_vert_fluxes.h".
! This code must be placed here rather than in a separate file in
! order to be exposed to the relevant CPP-settings.   It does not
! affect any model results, other than signature in global attribute
! "CPPS" in output netCDF files.
!
      implicit none
      integer ierr, is,ie, lenstr
# include "param.h"
# include "strings.h"
      ie=lenstr(cpps)
      is=ie+2
      ie=is+30
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='<compute_horiz_tracer_fluxes.h>'
      MPI_master_only write(stdout,'(1x,A)') cpps(is:ie)
      is=ie+2

# ifdef UPSTREAM
      ie=is+7
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='UPSTREAM'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      is=ie+2
# endif
# ifdef AKIMA
      ie=is+4
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='AKIMA'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      is=ie+2
# endif
# ifdef CONST_TRACERS
      ie=is+7
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='CONST_TRACERS'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      is=ie+2
# endif

      ie=is+29
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='<compute_vert_tracer_fluxes.h>'
      MPI_master_only write(stdout,'(1x,A)') cpps(is:ie)
      is=ie+2
# ifdef SPLINES
      ie=is+6
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='SPLINES'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      is=ie+2
# endif
# ifdef AKIMA_V
      ie=is+6
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='AKIMA_V'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      is=ie+2
# endif
      return
  99  MPI_master_only write(stdout,'(/1x,2A/12x,A/)') '### ERROR: ',
     &  'Insufficient length of string "cpps" in file "strings.h".',
     &        'Increase parameter "max_opt_size" it and recompile.'
      ierr=ierr+1
      return
      end
#else
      subroutine step3d_t_empty
      end
#endif  /* SOLVE3D */
 
