#include "cppdefs.h"
#ifdef SOLVE3D
 
# define UPSTREAM_TS
c--# define AKIMA
 
      subroutine step3d_t (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call step3d_t_tile( istr,iend,jstr,jend,
     &                    A2d(1,1), A2d(1,2), A2d(1,3), A2d(1,4),
     &                               A2d(1,1), A2d(1,2), A2d(1,3))
      end
 
      subroutine step3d_t_tile( istr,iend,jstr,jend, WC,FC,CF,DC,
     &                                                FX,FE,wrk1)
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, imin,imax,jmin,jmax, i,j,k
      real, dimension(PRIVATE_1D_SCRATCH_ARRAY,0:N) :: WC,FC,CF,DC
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY)     :: FX,FE,wrk1

      real cff
      real, parameter :: epsil=1.E-33
      integer itrc, iAkt

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
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
# include "ncvars.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
! temporary variable for calculation of vertical diffusive flux
      real ThisVDF(PRIVATE_2D_SCRATCH_ARRAY,0:N)
# endif /* PHYS_FLUX_ANALYSIS */
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      integer is
      logical source_active
#endif

# include "compute_auxiliary_bounds.h"

! Compute and add in contribution due to horizontal advection fluxes.
! Note that at this moment the content of array t(:,:,:,nnew,itrc) is
! concentration of tracer "itrc" taken at time corresponding to "nstp"
! and already MULTIPLIED by Hz(:,:,:) at "nstp" in "pre_step3d_tile".

      do k=1,N
      do itrc=1,NT
 
# include "compute_horiz_tracer_fluxes.h"
 
          do j=jstr,jend
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) -dt*pm(i,j)
     &                             *pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                      +FE(i,j+1)-FE(i,j))
            enddo
          enddo                 !-->  discard FX,FE
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
 
      do j=jstr,jend                  ! Compute and add explicit part
        do itrc=1,NT                  ! of vertical advective fluxes.
 
# include "compute_vert_tracer_fluxes.h"
 
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) -dt*pm(i,j)*pn(i,j)
     &                                             *(FC(i,k)-FC(i,k-1))
            enddo
          enddo               !--> discard FC

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
! Add 3D sources
! AH16
# ifdef TSOURCE
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) +dt*pm(i,j)*pn(i,j)
     &                                               *T_src(i,j,k,itrc)
            enddo
          enddo
        enddo
      enddo
# endif /* TSOURCE */
! Add surface fluxes
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
!DL: do this except for age tracers. Age tracers are dealt with in pre_step3d
          if (itrc.le.indxT+ntrc_salt .or. itrc.gt.indxT+ntrc_salt+ntrc_pas 
     &        .or. .not.Age(itrc-(indxT+ntrc_salt))) then
# endif
          do i=istr,iend
            t(i,j,N,nnew,itrc)=t(i,j,N,nnew,itrc)+dt*stflx(i,j,itrc)
          enddo
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
          endif ! itrc.le.indxT+ntrc_salt .or. ...
# endif
!# if defined LMD_KPP && defined ICEOBS && defined BRINE_PLUMES2
!          if (itrc .eq. isalt) then
!            do i=istr,iend
!              if (freezing(i,j).gt.0) then
!                t(i,j,N,nnew,itrc)=t(i,j,N,nnew,itrc)+dt*freezing(i,j,itrc)
!              endif
!            enddo
!          endif
!# endif
# if defined LMD_KPP && defined ICEOBS && defined BRINE_PLUMES
! AH15: Distributing salt fluxes due freezing uniformly over
! kbl to mimic brine plume convection (see Duffy et al. 1999)
          if (itrc .eq. isalt) then
            do i=istr,iend
              if (freezing(i,j)>0) then
                do k=N,kbl(i,j),-1
                  t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)
     &                  +(dt*freezing(i,j)
     &                  *Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kbl(i,j)-1)))
                enddo
              endif
            enddo
          endif
# endif
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
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
             do i=Istr,Iend
             ! unit = mmol / (m2 s)
                BottomFlux(i,j,itrc) = 0. !-btflx(i,j,itrc)
#   ifdef MASKING
     &               * rmask(i,j)
#   endif
             enddo
#endif /* SELECTED_FLUX_ANALYSIS */
          end if
# endif /* FULL_PHYS_FLUX_ANALYSIS */

# ifdef LMD_KPP
! Add the solar radiation flux in temperature equation. Also compute
! the nonlocal transport flux for unstable (convective) forcing
! conditions into matrix DC when using the Large et al. 1994 KPP
! scheme.

          if (itrc == itemp) then
# ifdef FULL_PHYS_FLUX_ANALYSIS
            do i=istr,iend
             SRAbsFlux(i,j,N)=0.
            enddo
# endif /* FULL_PHYS_FLUX_ANALYSIS */
            do k=N-1,1,-1
              do i=istr,iend
                cff=srflx(i,j)*swr_frac(i,j,k)
#  ifdef LMD_NONLOCAL
     &                 -ghat(i,j,k)*(stflx(i,j,itemp)-srflx(i,j))
#  endif
                t(i,j,k+1,nnew,itemp)=t(i,j,k+1,nnew,itemp) -dt*cff
                t(i,j,k  ,nnew,itemp)=t(i,j,k  ,nnew,itemp) +dt*cff
# ifdef FULL_PHYS_FLUX_ANALYSIS
                SRAbsFlux(i,j,k+1)=SRAbsFlux(i,j,k+1)-cff
                SRAbsFlux(i,j,k  )=cff
# endif /* FULL_PHYS_FLUX_ANALYSIS */
              enddo
            enddo

c?? WARNING: the above implies that light (if any) reaching all the
c?? way to the bottom is entirely absorbed within the botom-most grid
c?? box, rather that reflected/scattered back to the water column. It
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
          elseif (itrc==isalt) then
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

! Perform an implicit time step for vertical diffusion
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
! field; q_old(k) is old-time-step tracer (known).   As long as Akt(k)
! is nonnegative, the tri-diagonal matrix is diagonally dominant which
! guarantees stability of the Gaussian elimination algorithm.  Top and
! bottom boundary conditions are assumed to be no-flux, effectively
! Akt(N)=Akt(0)=0, hence FC(N)=FC(1)=0. This leads to equations for
! top and bottom grid boxes;
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

# ifdef BIOLOGY
#  ifdef SALINITY
          iAkt=min(itrc,isalt)
#  else
          iAkt=min(itrc,itemp)
#  endif
# else
          iAkt=itrc
# endif
          do i=istr,iend
            DC(i,0)=dt*pm(i,j)*pn(i,j) !<--horizontal metric

            FC(i,1)=2.*dt*Akt(i,j,1,iAkt)/(Hz(i,j,1)+Hz(i,j,2))
            WC(i,1)=DC(i,0)*Wi(i,j,1)

            cff=1./(Hz(i,j,1) +FC(i,1)+max(WC(i,1),0.))
            CF(i,1)=cff*(      FC(i,1)-min(WC(i,1),0.))
            DC(i,1)=cff*t(i,j,1,nnew,itrc)
          enddo
          do k=2,N-1,+1
            do i=istr,iend
              FC(i,k)=2.*dt*Akt(i,j,k,iAkt)/(Hz(i,j,k)+Hz(i,j,k+1))
              WC(i,k)=Wi(i,j,k)*DC(i,0)

              cff=1./( Hz(i,j,k) +FC(i,k)+max(WC(i,k),0.)
     &                         +FC(i,k-1)-min(WC(i,k-1),0.)
     &              -CF(i,k-1)*(FC(i,k-1)+max(WC(i,k-1),0.))
     &                                                     )

              CF(i,k)=cff*(       FC(i,k)-min(WC(i,k),0.) )

              DC(i,k)=cff*( t(i,j,k,nnew,itrc) +DC(i,k-1)*(
     &                          FC(i,k-1)+max(WC(i,k-1),0.) ))
            enddo
          enddo          !--> discard DC(:,0)
          do i=istr,iend
            t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc) +DC(i,N-1)*(
     &                               FC(i,N-1)+max(WC(i,N-1),0.) )
     &                   )/( Hz(i,j,N) +FC(i,N-1)-min(WC(i,N-1),0.)
     &                      -CF(i,N-1)*(FC(i,N-1)+max(WC(i,N-1),0.))
     &                                                             )
          enddo
          do k=N-1,1,-1
            do i=istr,iend
              t(i,j,k,nnew,itrc)=DC(i,k)+CF(i,k)*t(i,j,k+1,nnew,itrc)
            enddo
          enddo           !--> discard FC,WC,CF,DC

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

!DL:
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      do itrc=itemp+ntrc_salt+1,itemp+ntrc_salt+N_agedye ! loop over age/dye tracers
         is=itrc-(itemp+ntrc_salt) ! is = number of age/dye tracer
         if (.not.Age(is).and..not.Restore_ad(is)) then
            do l=1,Nsrc_ad(is)     ! loop over sources of tracer is
               ! Get i, j, k indices of this source:
               i = Isrc_ad(is,l)
               j = Jsrc_ad(is,l)
               k = Ksrc_ad(is,l)
               ! Check whether this source is still active:
               source_active = .false.
               if (switch_is_days_ad(is) .and.
     &             time*sec2day.ge.Switch_on_src_ad(is,l) .and.
     &             time*sec2day.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (.not.switch_is_days_ad(is) .and.
     &             iic.ge.Switch_on_src_ad(is,l) .and.
     &             iic.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (source_active) then
                  ! Now add source term:
                  t(i,j,k,nnew,itrc) = t(i,j,k,nnew,itrc) + dt*Ssrc_ad(is,l)
               endif
            enddo
         endif
         if (.not.Age(is).and.Restore_ad(is)) then
            ! apply restoring bc
            do l=1,Nsrc_ad(is)  ! loop over sources of tracer is
               i = Isrc_ad(is,l)
               j = Jsrc_ad(is,l)
               k = Ksrc_ad(is,l)
               ! Check whether this source is still active:
               source_active = .false.
               if (switch_is_days_ad(is) .and.
     &             time*sec2day.ge.Switch_on_src_ad(is,l) .and.
     &             time*sec2day.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (.not.switch_is_days_ad(is) .and.
     &             iic.ge.Switch_on_src_ad(is,l) .and.
     &             iic.lt.Switch_off_src_ad(is,l)) then
                  source_active = .true.
               endif
               if (source_active) then
                  t(i,j,k,nnew,itrc) = Ssrc_ad(is,l)
               endif
            enddo
         endif ! .not.Age(is).and.Restore_ad(is)
         if (Age(is)) then
            do k=1,N-1          ! exclude surface k=N
               do j=jstrR,jendR
                  do i=istrR,iendR
                     t(i,j,k,nnew,itrc) = t(i,j,k,nnew,itrc) 
#  ifdef USE_REAL_YEAR
     &                    + dt/(day2sec*365)
#  else
     &                    + dt/(day2sec*360)
#  endif
                  enddo
               enddo
            enddo ! k=1,N-1
         endif ! Age(is)
      enddo ! itrc
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */

!
!DL: Set surface fluxes for age tracers such that the new surface tracer
! concentration is zero:
!
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      do is=1,N_agedye
         itrc = itemp+ntrc_salt+is
         if (Age(is)) then
            do i=istrR,iendR
               do j=jstrR,jendR
                 stflx(i,j,itrc) = -t(i,j,N,nnew,itrc)/dt
#  ifdef MASKING
     &                               * rmask(i,j)
#  endif
               enddo
            enddo
            do i=istrR,iendR
               do j=jstrR,jendR
                 t(i,j,N,nnew,itrc) = 0.0
               enddo
            enddo
         endif ! Age(is)
      enddo ! is=1,N_agedye
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */



! Set lateral boundary conditions; nudge toward tracer climatology;
! apply land-sea mask and exchange periodic boundary conditions.
!
      do itrc=1,NT
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
! Age tracer should not be dilluted at the open boundary.
! So, treat as closed boundary for this tracer only !AH15
        is = itrc-itemp-ntrc_salt
        if ((itrc.gt.itemp+ntrc_salt)
     &             .and.(itrc.le.itemp+ntrc_salt+N_agedye)
     &             .and.(Age(is))) then
          call t3dbc_closed_tile (istr,iend,jstr,jend, itrc)
        else
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */
          call t3dbc_tile (istr,iend,jstr,jend, itrc, wrk1)
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
        endif
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */

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
# endif

# if defined BIOLOGY_BEC
      enddo                                    ! Biogeochemical
                                               ! Elemental Cycles
      call ecosys_tile (I_RANGE,J_RANGE)       ! (BEC) model 

      do itrc=1,NT
# elif defined BIOLOGY_BEC2
      enddo

      call ecosys_bec2_tile (I_RANGE,J_RANGE)       ! BEC, 2014 version

      do itrc=1,NT
# elif defined BIOLOGY || defined BIOLOGY_NPZDOC
      enddo                                    ! Insert interactive
      call biology_tile (istr,iend,jstr,jend)  ! part of the biological
      do itrc=1,NT                             ! model
# endif

# if defined MASKING || defined TCLIMATOLOGY
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
#  ifdef TCLIMATOLOGY
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)
     &       +dt*Tnudgcof(i,j,itrc)*( tclm(i,j,k,itrc)
     &                             -t(i,j,k,nnew,itrc))
#  ifdef FULL_PHYS_FLUX_ANALYSIS
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
          if (itrc .le. NT_PFA) then
               NudgingFlux(i,j,k,itrc) = Tnudgcof(i,j,itrc) *
     &              ( tclm(i,j,k,itrc) - t(i,j,k,nnew,itrc) )
#   ifdef MASKING
     &              * rmask(i,j)
#   endif
            end if
#endif /* SELECTED_FLUX_ANALYSIS */
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

! Exchange computational margins and/or periodic boundaries:
!--------- ------------- -------- ------ -------- ----------- 
! This is done only here if the code is configured without explicit
! lateral diffusion. If there is one, then the exchange routine is
! called from the diffusive routine which immediately follows this
! step, therefore there is no need to call it here.

# if !defined TS_DIF2 && !defined TS_DIF4
#  ifdef EXCHANGE
      do itrc=1,NT
        call exchange_tile(istr,iend,jstr,jend,
     &        t(START_2D_ARRAY,1,nnew,itrc), N)
      enddo
#  endif
# endif
      end

      subroutine check_step_t_switches(ierr)

! The following routine keeps track of the status of CPP settings in
! "compute_horiz_tracer_fluxes.h" and "compute_horiz_vert_fluxes.h".
! This code must be placed here rather than in a separate file so it
! is exposed to the relevant CPP-settings.   It does not affect any
! model results, other than signature in global attribute "CPPS" in
! the output netCDF files.

      implicit none
      integer ierr, is,ie, lenstr
# include "param.h"
# include "strings.h"
      ie=lenstr(cpps)
      is=ie+2
      ie=is+13
      if (ie>max_opt_size) goto 99
      cpps(is:ie)='<step3d_t3S.F>'

# include "track_advec_switches.h"

      return
  99  mpi_master_only write(*,'(/1x,2A/12x,A/)')      '### ERROR: ',
     &  'Insufficient length of string "cpps" in file "strings.h".',
     &        'Increase parameter "max_opt_size" it and recompile.'
      ierr=ierr+1
      return
      end
#else
      subroutine step3d_t_empty
      end
#endif  /* SOLVE3D */
