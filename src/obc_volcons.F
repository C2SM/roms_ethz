#include "cppdefs.h"
#ifdef OBC_VOLCONS

      subroutine obc_flux_tile (Istr,Iend,Jstr,Jend, dFlx)
!
! Compute integral crossection of open all boundaries "bc_crss" and
! integral mass flux "bc_flux" incoming into model domain (hence
! POSITIVE "bc_flux" means GAIN in mass) in order to enforce global
! mass conservation constraint. This is done by first computing the
! integral crossection of- and fluxes across-  the segments of open
! boundaries within subdomains [tiles], then making global summation
! over the subdomains. At the end, global integrated flux is divided
! by the global cross-section to compute the correction velocity
! "ubar_xs" needed to cancel exactly global mass gain.  
!
      implicit none
      integer Istr,Iend,Jstr,Jend,  i,j, inc,jnc, nsubs 
      real*QUAD dFlx(PRIVATE_2D_SCRATCH_ARRAY), my_crss, my_flux
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# ifdef MPI
#  include "mpif.h"
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(2)
# endif
                          ! Reset partial sums over the segments
      my_crss=QuadZero    ! of open boundaries, then compute partial 
      my_flux=QuadZero    ! cross-sections and mass fluxes over the
                          ! subdomains 
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          dFlx(Istr,j)=0.5*( zeta(Istr-1,j,knew)+zeta(Istr,j,knew)
     &                        +h(Istr-1,j)+h(Istr,j) )*dn_u(Istr,j)
#  ifdef MASKING
     &                                               *umask(Istr,j)
#  endif
          dFlx(Istr-1,j)=dFlx(Istr,j)*ubar(Istr,j,knew)
c**       my_crss=my_crss + dFlx(Istr  ,j)
c**       my_flux=my_flux + dFlx(Istr-1,j)
        enddo
        jnc=1
        do while(Jstr.le.Jend-jnc)
          do j=Jstr,Jend-jnc,2*jnc
            dFlx(Istr  ,j) = dFlx(Istr  ,j) + dFlx(Istr  ,j+jnc)
            dFlx(Istr-1,j) = dFlx(Istr-1,j) + dFlx(Istr-1,j+jnc)
          enddo
          jnc=2*jnc
        enddo
        my_crss=my_crss + dFlx(Istr  ,Jstr)
        my_flux=my_flux + dFlx(Istr-1,Jstr)
      endif
# endif
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
          dFlx(Iend,j)=0.5*( zeta(Iend,j,knew)+zeta(Iend+1,j,knew)
     &                      +h(Iend,j)+h(Iend+1,j) )*dn_u(Iend+1,j)
#  ifdef MASKING
     &                                             *umask(Iend+1,j)
#  endif
          dFlx(Iend+1,j)=dFlx(Iend,j)*ubar(Iend+1,j,knew)
c**       my_crss=my_crss + dFlx(Iend  ,j)
c**       my_flux=my_flux - dFlx(Iend+1,j)
        enddo
        jnc=1
        do while(Jstr.le.Jend-jnc)
          do j=Jstr,Jend-jnc,2*jnc
            dFlx(Iend  ,j) = dFlx(Iend  ,j) + dFlx(Iend  ,j+jnc)
            dFlx(Iend+1,j) = dFlx(Iend+1,j) + dFlx(Iend+1,j+jnc)
          enddo
          jnc=2*jnc
        enddo
        my_crss=my_crss + dFlx(Iend  ,Jstr)
        my_flux=my_flux - dFlx(Iend+1,Jstr)
      endif
# endif
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          dFlx(i,Jstr)=0.5*( zeta(i,Jstr,knew)+zeta(i,Jstr-1,knew)
     &                        +h(i,Jstr)+h(i,Jstr-1) )*dm_v(i,Jstr)
#  ifdef MASKING
     &                                               *vmask(i,Jstr)
#  endif
          dFlx(i,Jstr-1)=dFlx(i,Jstr)*vbar(i,Jstr,knew)
c**       my_crss=my_crss + dFlx(i,Jstr  )
c**       my_flux=my_flux + dFlx(i,Jstr-1)
        enddo
        inc=1
        do while(Istr.le.Iend-inc)
          do i=Istr,Iend-inc,2*inc
            dFlx(i,Jstr  ) = dFlx(i,Jstr  ) + dFlx(i+inc,Jstr  )
            dFlx(i,Jstr-1) = dFlx(i,Jstr-1) + dFlx(i+inc,Jstr-1)
          enddo
          inc=2*inc
        enddo
        my_crss=my_crss + dFlx(Istr,Jstr  )
        my_flux=my_flux + dFlx(Istr,Jstr-1)
      endif
# endif
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          dFlx(i,Jend)=0.5*( zeta(i,Jend,knew)+zeta(i,Jend+1,knew)
     &                      +h(i,Jend)+h(i,Jend+1) )*dm_v(i,Jend+1)
#  ifdef MASKING
     &                                             *vmask(i,Jend+1)
#  endif
          dFlx(i,Jend+1)=dFlx(i,Jend)*vbar(i,Jend+1,knew)

c**       my_crss=my_crss + dFlx(i,Jend  )
c**       my_flux=my_flux - dFlx(i,Jend+1)
        enddo
        inc=1
        do while(Istr.le.Iend-inc)
          do i=Istr,Iend-inc,2*inc
             dFlx(i,Jend  ) = dFlx(i,Jend  ) + dFlx(i+inc,Jend  )
             dFlx(i,Jend+1) = dFlx(i,Jend+1) + dFlx(i+inc,Jend+1)
          enddo
          inc=2*inc
        enddo
        my_crss=my_crss + dFlx(Istr,Jend  )
        my_flux=my_flux - dFlx(Istr,Jend+1)
      endif
# endif

      if (SINGLE_TILE_MODE) then    ! Global summation is performed
        nsubs=1                     ! in two stages, first within
      else                          ! each shared memory (MPI-nodes), 
        nsubs=NSUB_X*NSUB_E         ! then across  MPI nodes (Reduce
      endif                         ! -- Broadcast sequence).

C$OMP CRITICAL (obc_flx_cr_rgn)
        if (bc_count.eq.0) then     ! Counter tile_count identifies
          bc_flux=QuadZero          ! the last thread (within each
          bc_crss=QuadZero          ! MPI-process) leaving critical
        endif                       ! region. This thread (which is
        bc_count=bc_count+1         ! not necessarily master thread
        bc_crss=bc_crss+my_crss     ! within its MPI-process) is
        bc_flux=bc_flux+my_flux     ! responsible for communication
        if (bc_count.eq.nsubs) then ! exchange between MPI-nodes.
          bc_count=0
# ifdef MPI
          size=NNODES               ! Reduce-broadcast sequence...
  1        step=(size+1)/2
            if (mynode.ge.step .and. mynode.lt.size) then
              buff(1)=bc_crss
              buff(2)=bc_flux
              call MPI_Send (buff,  4, MPI_DOUBLE_PRECISION,
     &             mynode-step, 17, ocean_grid_comm,      ierr)
            elseif (mynode .lt. size-step) then
              call MPI_Recv (buff,  4, MPI_DOUBLE_PRECISION,
     &             mynode+step, 17, ocean_grid_comm, status, ierr)
     
              bc_crss=bc_crss+buff(1)
              bc_flux=bc_flux+buff(2)
            endif
           size=step
          if (size.gt.1) goto 1

          buff(1)=bc_crss
          buff(2)=bc_flux
          call MPI_Bcast(buff, 4, MPI_DOUBLE_PRECISION,
     &                         0, ocean_grid_comm, ierr)
          bc_crss=buff(1)
          bc_flux=buff(2)
# endif
          ubar_xs=bc_flux/bc_crss
        endif
C$OMP END CRITICAL (obc_flx_cr_rgn)
      return
      end
#endif
#if defined OBC_FLUX_CORR || defined OBC_VOLCONS

      subroutine set_DUV_bc_tile (Istr,Iend,Jstr,Jend, Drhs, DUon,
     &                                                       DVom)
!
! Correct vertically integrated mass fluxes DUon and DVom along
! the open boundaries in such a way that the integral volume is
! conserved. This is done by applying ubar_xs correction to the
! velocities.
!  
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real Drhs(PRIVATE_2D_SCRATCH_ARRAY),
     &     DUon(PRIVATE_2D_SCRATCH_ARRAY),
     &     DVom(PRIVATE_2D_SCRATCH_ARRAY)
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr-1,JendR
          DUon(Istr,j)=DUon(Istr,j) - ubar_xs*dn_u(Istr,j)
     &                         *0.5*(Drhs(Istr,j)+Drhs(Istr-1,j))
#  ifdef MASKING
     &                                             *umask(Istr,j)
#  endif
        enddo
      endif
# endif
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr-1,JendR
          DUon(Iend+1,j)=DUon(Iend+1,j) + ubar_xs*dn_u(Iend+1,j)
     &                         *0.5*(Drhs(Iend+1,j)+Drhs(Iend,j))
#  ifdef MASKING
     &                                           *umask(Iend+1,j)
#  endif
        enddo
      endif
# endif
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=Istr-1,IendR
          DVom(i,Jstr)=DVom(i,Jstr) - ubar_xs*dm_v(i,Jstr)
     &                         *0.5*(Drhs(i,Jstr)+Drhs(i,Jstr-1))
#  ifdef MASKING
     &                                             *vmask(i,Jstr)
#  endif
        enddo
      endif
# endif
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr-1,IendR
          DVom(i,Jend+1)=DVom(i,Jend+1) + ubar_xs*dm_v(i,Jend+1)
     &                         *0.5*(Drhs(i,Jend+1)+Drhs(i,Jend))
#  ifdef MASKING
     &                                           *vmask(i,Jend+1)
#  endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine obc_volcons_empty
      end
#endif


