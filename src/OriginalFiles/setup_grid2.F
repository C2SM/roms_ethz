#include "cppdefs.h"
 
      subroutine setup_grid2 (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call setup_grid2_tile (istr,iend,jstr,jend, A2d(1,1))
      return
      end
 
      subroutine setup_grid2_tile (istr,iend,jstr,jend, dA)
!
! Find minimum and maximum depth of model topography; minimum and
! maximum horizontal XI- and ETA-grid intervals around h-points;
! Check minimum and maximum values of Courant number for barotropic
! mode (based on surface gravity wave speed, barortopic time step
! and local grid intervals); combined crossection of all open
! boundaries.
!
      implicit none
      integer istr,iend,jstr,jend, i,j, nsub, inc,jnc
      real*QUAD dA(2,PRIVATE_2D_SCRATCH_ARRAY),
     &          my_area,  my_volume,  my_crss
      real my_hmax, my_grdmax, my_Cu_max, cff,
     &     my_hmin, my_grdmin, my_Cu_min, my_Cu_Cor
      logical lsumX, lsumY
#include "param.h"
#include "scalars.h"
#include "comm_vars.h"
#include "grid.h"
#ifdef MPI
# include "mpif.h"
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(16)
#endif
!
#include "compute_auxiliary_bounds.h"
!
      my_hmin=+1.E+20              ! Here two-dimensional Courant
      my_hmax=-1.E+20              ! number is defined as
      my_grdmin=+1.E+20            !
      my_grdmax=-1.E+20            ! Cu = c*dt * sqrt(1/dx^2+1/dy^2)
      my_Cu_min=+1.E+20            !
      my_Cu_max=-1.E+20            ! where c=sqrt(g*h) is phase
      my_Cu_Cor=-1.E+20            ! speed for barotropic mode,
                                   ! and dx,dy are grid spacings
      do j=jstrR,jendR             ! in each direction.
        do i=istrR,iendR
#ifdef MASKING
          if (rmask(i,j) .gt. 0.5) then
#endif
            my_hmin=min(my_hmin,  h(i,j))
            my_hmax=max(my_hmax,  h(i,j))
 
            cff=1./sqrt(pm(i,j)*pn(i,j))
#if defined VIS_GRID || defined DIF_GRID
            grdscl(i,j)=cff
#endif
            my_grdmin=min(my_grdmin, cff)
            my_grdmax=max(my_grdmax, cff)
 
            cff=dtfast*sqrt( g*h(i,j)*( pm(i,j)*pm(i,j)
     &                                 +pn(i,j)*pn(i,j) ))
            my_Cu_min=min(my_Cu_min, cff)
            my_Cu_max=max(my_Cu_max, cff)
 
            cff=dt*abs(f(i,j))
            my_Cu_Cor=max(my_Cu_Cor, cff)
#ifdef MASKING
          endif
#endif
        enddo                      ! Compute unmasked total area
      enddo                        ! and unperturbed volume of model
                                   ! grid (assuming zero free-surface
      do j=jstr,jend               ! disturbance everywhere.
        do i=istr,iend             !
#ifdef MASKING
          dA(1,i,j)=rmask(i,j)/(pm(i,j)*pn(i,j))
#else
          dA(1,i,j)=1./(pm(i,j)*pn(i,j))
#endif
          dA(2,i,j)=dA(1,i,j)*h(i,j)
        enddo
      enddo                        ! The following segment computes
                                   ! sums of dA within the subdomain
      lsumX=.true.                 ! tile using split-directional
      lsumY=.true.                 ! reduction-by-pairs algorithm, to
      inc=1                        ! avoid accumulation of roundoff
      jnc=1                        ! errors.
 
      do while (lsumX .or. lsumY)
        if (istr.le.iend-inc) then
          do j=jstr,jend
            do i=istr,iend-inc,2*inc
              dA(1,i,j) = dA(1,i,j) + dA(1,i+inc,j)
              dA(2,i,j) = dA(2,i,j) + dA(2,i+inc,j)
            enddo
          enddo
          inc=2*inc
        else
          lsumX=.false.
        endif
        if (jstr.le.jend-jnc) then
          do j=jstr,jend-jnc,2*jnc
            do i=istr,iend
              dA(1,i,j) = dA(1,i,j) + dA(1,i,j+jnc)
              dA(2,i,j) = dA(2,i,j) + dA(2,i,j+jnc)
            enddo
          enddo
          jnc=2*jnc
        else
          lsumY=.false.
        endif
      enddo
 
      my_area   = dA(1,istr,jstr)
      my_volume = dA(2,istr,jstr)
 
c**   my_area=QuadZero             ! Straight summation: this code
c**   my_volume=QuadZero           ! produces result which differs
c**   do j=jstr,jend               ! above only by roundoff errors.
c**     do i=istr,iend
c**       my_area   = my_area   + dA(1,i,j)
c**       my_volume = my_volume + dA(2,i,j)
c**     enddo
c**   enddo
                                   ! Compute integral crossection
      my_crss=QuadZero             ! of all open boundaries...
#ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=jstr,jend
          dA(1,istr,j)=0.5*(h(istr-1,j)+h(istr,j))*dn_u(istr,j)
# ifdef MASKING
     &                                            *umask(istr,j)
# endif
        enddo
        jnc=1
        do while(jstr.le.jend-jnc)
          do j=jstr,jend-jnc,2*jnc
            dA(1,istr,j) = dA(1,istr,j) + dA(1,istr,j+jnc)
          enddo
          jnc=2*jnc
        enddo
        my_crss=my_crss + dA(1,istr,jstr)
      endif
#endif
#ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=jstr,jend
          dA(1,iend,j)=0.5*(h(iend,j)+h(iend+1,j))*dn_u(iend+1,j)
# ifdef MASKING
     &                                            *umask(iend+1,j)
# endif
        enddo
        jnc=1
        do while(jstr.le.jend-jnc)
          do j=jstr,jend-jnc,2*jnc
            dA(1,iend,j) = dA(1,iend,j) + dA(1,iend,j+jnc)
          enddo
          jnc=2*jnc
        enddo
        my_crss=my_crss + dA(1,iend,jstr)
      endif
#endif
#ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=istr,iend
          dA(1,i,jstr)=0.5*(h(i,jstr)+h(i,jstr-1))*dm_v(i,jstr)
# ifdef MASKING
     &                                            *vmask(i,jstr)
# endif
        enddo
        inc=1
        do while(istr.le.iend-inc)
          do i=istr,iend-inc,2*inc
            dA(1,i,jstr) = dA(1,i,jstr) + dA(1,i+inc,jstr)
          enddo
          inc=2*inc
        enddo
        my_crss=my_crss + dA(1,istr,jstr)
      endif
#endif
#ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=istr,iend
          dA(1,i,jend)=0.5*(h(i,jend)+h(i,jend+1))*dm_v(i,jend+1)
# ifdef MASKING
     &                                            *vmask(i,jend+1)
# endif
        enddo
        inc=1
        do while(istr.le.iend-inc)
          do i=istr,iend-inc,2*inc
            dA(1,i,jend) = dA(1,i,jend) + dA(1,i+inc,jend)
          enddo
          inc=2*inc
        enddo
        my_crss=my_crss + dA(1,istr,jend)
      endif
#endif
!
! Thus far everything has ben computed within the tile. Next: combine
! the results to get global min/max and summations. This needs to be
! done in two stages, first among all tiles which belong to the same
! shared memory group (MPI-node); within each MPI-node, then across
! MPI nodes (Reduce--Broadcast sequence).
!
      if (SINGLE_TILE_MODE) then
        nsub=1
      else
        nsub=NSUB_X*NSUB_E
      endif
 
C$OMP CRITICAL (grd2_cr_rgn)
        hmin=min(hmin, my_hmin)       ! Global MIN/MAX operations
        hmax=max(hmax, my_hmax)       ! within each shared memory
        grdmin=min(grdmin, my_grdmin) ! group (an MPI-node).
        grdmax=max(grdmax, my_grdmax) ! Counter tile_count identifies
        Cu_min=min(Cu_min, my_Cu_min) ! the last thread (within each
        Cu_max=max(Cu_max, my_Cu_max)
        Cu_Cor=max(Cu_Cor, my_Cu_Cor)
        area=area+my_area
        volume=volume+my_volume
        bc_crss=bc_crss+my_crss       ! MPI-process) leaving critical
                                      ! region. This thread (which is
        tile_count=tile_count+1       ! not necessarily master thread
        if (tile_count.eq.nsub) then  ! within its MPI-process) is
          tile_count=0                ! responsible for communication
# ifdef MPI
          size=NNODES
  1        step=(size+1)/2
            if (mynode.ge.step .and. mynode.lt.size) then
              buff(1)=hmin
              buff(2)=hmax             ! This is similar MPI_Reduce
              buff(3)=grdmin           ! MIN or MAX operation, except
              buff(4)=grdmax           ! that MIN/MAX and QUAD
              buff(5)=Cu_min           ! summations come together.
              buff(6)=Cu_max
              buff(7)=Cu_Cor
              buff(8)=area
              buff(9)=volume
              buff(10)=bc_crss
              call MPI_Send (buff,  20, MPI_DOUBLE_PRECISION,
     &             mynode-step, 17, ocean_grid_comm,      ierr)
            elseif (mynode .lt. size-step) then
              call MPI_Recv (buff,  20, MPI_DOUBLE_PRECISION,
     &             mynode+step, 17, ocean_grid_comm, status, ierr)
 
              cff=buff(1)
              hmin=  min(hmin,   cff)
              cff=buff(2)                 ! Copying buff() into cff
              hmax=  max(hmax,   cff)     ! is needed here to strip
              cff=buff(3)                 ! down QUAD precision in
              grdmin=min(grdmin, cff)     ! case is some compiler
              cff=buff(4)                 ! intrinsic min/max
              grdmax=max(grdmax, cff)     ! operations cannot
              cff=buff(5)                 ! handle QUADs.
              Cu_min=min(Cu_min, cff)
              cff=buff(6)
              Cu_max=max(Cu_max, cff)
              cff= buff(7)
              Cu_Cor=max(Cu_Cor, cff)
              area=area + buff(8)
              volume=volume + buff(9)
              bc_crss=bc_crss + buff(10)
            endif
           size=step
          if (size.gt.1) goto 1
 
          buff(1)=hmin
          buff(2)=hmax
          buff(3)=grdmin
          buff(4)=grdmax
          buff(5)=Cu_min
          buff(6)=Cu_max
          buff(7)=Cu_Cor
          buff(8)=area
          buff(9)=volume
          buff(10)=bc_crss
          call MPI_Bcast(buff, 20, MPI_DOUBLE_PRECISION,
     &                          0, ocean_grid_comm, ierr)
          hmin=  buff(1)
          hmax=  buff(2)
          grdmin=buff(3)
          grdmax=buff(4)
          Cu_min=buff(5)
          Cu_max=buff(6)
          Cu_Cor=buff(7)
          area=buff(8)
          volume=buff(9)
          bc_crss=buff(10)
# endif
# ifdef MPI
          if (mynode.eq.0) then
# endif
            write(stdout,'(1x,A,F12.6,3x,A,1PE14.7,5x,A,1PE23.16)')
     &       'hmin =' ,hmin,  'grdmin =', grdmin, 'area =',   area
            write(stdout,'(1x,A,F12.6,3x,A,1PE14.7,3x,A,1PE23.16)')
     &       'hmax =' ,hmax,  'grdmax =', grdmax, 'volume =', volume
            write(stdout,'(43x,A,1PE23.16)') 'open_cross =', bc_crss
            write(stdout,'(1x,A,F10.7,3x,A,F10.7,3x,A,F10.7)')
     &       'Cu_min =',Cu_min, 'Cu_max =',Cu_max, 'Cu_Cor =',Cu_Cor
 
# ifdef USWEST
            my_area = area     - 2.9359806235808631E+12
            my_volume = volume - 1.1257583746469292E+16
            my_crss = bc_crss  - 2.1590394137797680E+10
 
            write(*,'(/1x,2A,3(/20x,A,2(1PE20.12))/)') 'Checking ',
     &         'accuracy:', 'errArea =',  my_area, my_area/area,
     &                      ' errVol =',my_volume, my_volume/volume,
     &                      'errOpen =',  my_crss,  my_crss/bc_crss
# endif
# ifdef MPI
          endif
# endif
        endif
C$OMP END CRITICAL (grd2_cr_rgn)
 
#if defined VIS_GRID || defined DIF_GRID
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,  grdscl)
# endif
#endif
      return
      end
 
 
