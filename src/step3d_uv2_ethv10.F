#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine step3d_uv2 (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call step3d_uv2_tile (istr,iend,jstr,jend, A2d(1,1), A2d(1,2),
     &                                                     A2d(1,3))
      return
      end
 
      subroutine step3d_uv2_tile (istr,iend,jstr,jend, CF,FC,DC)
      implicit none
      integer istr,iend,jstr,jend, i,j,k
# include "param.h"
      real FC(PRIVATE_1D_SCRATCH_ARRAY,0:N), cff,
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N)
# ifdef PSOURCE
      integer is
# endif
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "coupling.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# include "sources.h"
!
# include "compute_auxiliary_bounds.h"
!
! Apply implicit time step for vertical viscosty term to horizontal
! velocity components,
!
!   du(k)     1     [         u(k+1)-u(k)             u(k)-u(k-1) ]
!  ------ = ----- * [ Akv(k)* ----------- - Akv(k-1)* ----------- ]
!    dt     Hz(k)   [           dz(k)                   dz(k-1)   ]
!
! where u(k) represents u,v(:,:,:,nnew) velocity components; Hz(k)
! and dz(k) are grid spacing between vertical W- and RHO-points
! respectively, interpolated to horizontal U,V-points (whichever
! applies). Backward Euler implicit time step requires solution of
! a tri-diagonal system,
!
!     -FC(k-1)*u_new(k-1) +[Hz(k)+FC(k-1)+FC(k)]*u_new(k)
!                       -FC(k)*u_new(k+1) = Hz(k)*u_old(k)
!
!                dt*Akv(k)
! where FC(k) = ----------- is normalized vertical viscosity defined
!                  dz(k)
!
! at W-points; u_new(k) is the new-time-step (unknown) velocity;
! u_old(k) is old-time-step tracer (known).  As long as vertical
! viscosity Ak(k) is nonnegative, the tri-diagonal matrix is
! diagonally dominant which guarantees stability of Gaussian
! elimination procedure, (e.g., Richtmeyer annd  Morton,  1967).
! Since top (wind) and bottom (drag) boundary stresses are applied
! explicitly, for the purpose of solving the tri-diagonal system
! boundary conditions are effectively no-stress Akv(N)=Akt(0)=0, so
! that the corresponding FC(N)=FC(1)=0. This leads to equations for
! top and bottom grid boxes,
!
!   -FC(N-1)*u_new(N-1) +[Hz(N)+FC(N-1)]*u_new(N) = Hz(N)*u_old(N)
!
!          [Hz(1)+FC(1)]*u_new(1) -FC(1)*u_new(2) = Hz(1)*u_old(1)
!
! Basically FC(N)=FC(1)=0 conditions should be interpreted as the
! implicit step merely redistributes the horizontal momentum
! components u,v throughout the vertical column, while conserving
! its vertical integral. At this moment the content of arrays
! u,v(:,:,:,nnew) has meaning of Hz*velocity. After the implicit
! step they becomes just velocity, which is temporarily loaded into
! array DC(:,1:N)
!>
! Also perform coupling between the barotropic and baroclinic modes.
! For the purpose of the computational efficiency (cache utilization)
! the implicit time step and coupling are partially overlapped. To
! distinguish the two operations in the code below we place separator
! (!>): lines above the separator (!>) belong to the tri-diagonal
! solver, while operations below it belong to the coupling.
! The coupling procedure makes vertical integration of the newly
! computed velocities [which ends up being stored in the scratch
! array DC(:,1:N) after the tri-diagonal problem is resolved] to
! compute its vertical mean [stored as DC(:,0)], and replace it with
! that from the 2D barotropic sub-model ubar=DU_avg1/(D*dn_u), where
! D=CF(:,0) is the total depth of water column.
!
!
! Time step momentum equation in the XI-direction
!----- ---- -------- -------- -- --- ------------
!
      do j=jstr,jend
        do i=istrU,iend                                 ! Apply wind
          u(i,j,N,nnew)=u(i,j,N,nnew)+dt*sustr(i,j)     ! forcing and
c**       u(i,j,1,nnew)=u(i,j,1,nnew)-dt*bustr(i,j)     ! bottom drag
c**                           already added in rhs3d

          FC(i,1)=dt*(Akv(i,j,1)+Akv(i-1,j,1))
     &                     /( z_r(i,j,2)+z_r(i-1,j,2)
     &                       -z_r(i,j,1)-z_r(i-1,j,1))

          cff=1./(0.5*(Hz(i,j,1)+Hz(i-1,j,1))+FC(i,1))
          CF(i,1)=cff*FC(i,1)
          DC(i,1)=cff*u(i,j,1,nnew)
        enddo
        do k=2,N-1,+1                        !--> forward elimination
          do i=istrU,iend
            FC(i,k)=dt*(Akv(i,j,k)+Akv(i-1,j,k))
     &                       /( z_r(i,j,k+1)+z_r(i-1,j,k+1)
     &                         -z_r(i,j,k  )-z_r(i-1,j,k ))

            cff=1./( 0.5*(Hz(i,j,k)+Hz(i-1,j,k))  +FC(i,k)
     &                          +FC(i,k-1)*(1.-CF(i,k-1)) )
            CF(i,k)=cff*FC(i,k)
            DC(i,k)=cff*(u(i,j,k,nnew)+FC(i,k-1)*DC(i,k-1))
          enddo
        enddo
        do i=istrU,iend
          DC(i,N)=(u(i,j,N,nnew)+FC(i,N-1)*DC(i,N-1))
     &                  /( 0.5*(Hz(i,j,N)+Hz(i-1,j,N))
     &                     +FC(i,N-1)*(1.-CF(i,N-1)) )
!>
          CF(i,0)=0.5*(Hz(i,j,N)+Hz(i-1,j,N))
          DC(i,0)=CF(i,0)*DC(i,N)
        enddo                           !--> discard FC, DC,CF(:,1:N)
        do k=N-1,1,-1                   !             keep DC,CF(:,0)
          do i=istrU,iend
            DC(i,k)=DC(i,k)+CF(i,k)*DC(i,k+1)     !<-- backward sweep
!>
            cff=0.5*(Hz(i,j,k)+Hz(i-1,j,k))
            CF(i,0)=CF(i,0)+cff
            DC(i,0)=DC(i,0)+cff*DC(i,k)
          enddo
        enddo
        do i=istrU,iend
          DC(i,0)=(DC(i,0)*dn_u(i,j)-DU_avg1(i,j))
     &                         /(CF(i,0)*dn_u(i,j))
        enddo
        do k=1,N
          do i=istrU,iend
            u(i,j,k,nnew)=(DC(i,k)-DC(i,0))
# ifdef MASKING
     &                          *umask(i,j)
# endif
          enddo
        enddo        !--> discard DC,CF
!
! Time step momentum equation in the ETA-direction.
!----- ---- -------- -------- -- --- --------------
! (All steps are similar to XI-component above.)
!
        if (j.ge.jstrV) then
          do i=istr,iend                                ! Apply wind
            v(i,j,N,nnew)=v(i,j,N,nnew)+dt*svstr(i,j)   ! forcing and
c**         v(i,j,1,nnew)=v(i,j,1,nnew)-dt*bvstr(i,j)   ! bottom drag
c**                  already added in rhs3d

            FC(i,1)=dt*(Akv(i,j,1)+Akv(i,j-1,1))
     &                       /( z_r(i,j,2)+z_r(i,j-1,2)
     &                         -z_r(i,j,1)-z_r(i,j-1,1))

            cff=1./(0.5*(Hz(i,j,1)+Hz(i,j-1,1))+FC(i,1))
            CF(i,1)=cff*FC(i,1)
            DC(i,1)=cff*v(i,j,1,nnew)
          enddo
          do k=2,N-1,+1                      !--> forward elimination
            do i=istr,iend
              FC(i,k)=dt*(Akv(i,j,k)+Akv(i,j-1,k))
     &                        /( z_r(i,j,k+1)+z_r(i,j-1,k+1)
     &                          -z_r(i,j,k  )-z_r(i,j-1,k  ))

              cff=1./( 0.5*(Hz(i,j,k)+Hz(i,j-1,k)) +FC(i,k)
     &                            +FC(i,k-1)*(1.-CF(i,k-1)) )
              CF(i,k)=cff*FC(i,k)
              DC(i,k)=cff*(v(i,j,k,nnew)+FC(i,k-1)*DC(i,k-1))
            enddo
          enddo
          do i=istr,iend
            DC(i,N)=(v(i,j,N,nnew)+FC(i,N-1)*DC(i,N-1))
     &                    /( 0.5*(Hz(i,j,N)+Hz(i,j-1,N))
     &                       +FC(i,N-1)*(1.-CF(i,N-1)) )
!>
            CF(i,0)=0.5*(Hz(i,j,N)+Hz(i,j-1,N))
            DC(i,0)=CF(i,0)*DC(i,N)
          enddo
          do k=N-1,1,-1
            do i=istr,iend
              DC(i,k)=DC(i,k)+CF(i,k)*DC(i,k+1)   !<-- backward sweep
!>
              cff=0.5*(Hz(i,j,k)+Hz(i,j-1,k))
              CF(i,0)=CF(i,0)+cff
              DC(i,0)=DC(i,0)+cff*DC(i,k)
            enddo
          enddo      !--> discard FC, keep DC,CF(:,0)
 
          do i=istr,iend
            DC(i,0)=(DC(i,0)*dm_v(i,j)-DV_avg1(i,j))
     &                           /(CF(i,0)*dm_v(i,j))
          enddo
          do k=1,N
            do i=istr,iend
              v(i,j,k,nnew)=(DC(i,k)-DC(i,0))
# ifdef MASKING
     &                              *vmask(i,j)
# endif
            enddo
          enddo       !--> discard DC,CF(:,0)
        endif
      enddo      ! <-- j
!
! Set PHYSICAL lateral boundary conditions.
!
      call u3dbc_tile (istr,iend,jstr,jend, DC)
      call v3dbc_tile (istr,iend,jstr,jend, DC)

# ifdef PSOURCE
      do is=1,Nsrc           ! Apply point mass sources associated
        i=Isrc(is)           ! with river runoff simulations.
        j=Jsrc(is)
        if (istrR.le.i .and. i.le.iendR .and.
     &      jstrR.le.j .and. j.le.jendR) then
          if (Dsrc(is).eq.0) then
            do k=1,N
              u(i,j,k,nnew)=2.*Qsrc(is,k)/( dn_u(i,j)*(
     &                       z_w(i-1,j,k)-z_w(i-1,j,k-1)
     &                      +z_w(i  ,j,k)-z_w(i  ,j,k-1)
     &                                                ))
            enddo
          else
            do k=1,N
              v(i,j,k,nnew)=2.*Qsrc(is,k)/( dm_v(i,j)*(
     &                       z_w(i,j-1,k)-z_w(i,j-1,k-1)
     &                      +z_w(i,j  ,k)-z_w(i,j  ,k-1)
     &                                                ))
            enddo
          endif
        endif
      enddo
# endif
!
! Coupling 2D and 3D momentum equations:
!--------- -- --- -- -------- ----------
! Compute inaccurate vertical mean of the three-dimensional
! velocity field, then subtract it and replace it with the vertically
! integrated (barotropic) velocity field computed from the two-
! dimensional submodel. After that compute mass fluxes through grid
! box faces.
!
! Meaning of scratch variables below:
!
! DC(i,k) [where k=1,N]  height of grid box for U- or V-cell.
! DC(i,0) total depth of water column at horizontal U- or V-points.
! FC(i,0) vertically integrated mass flux, error, and correction for
!                       the new time step velocity u,v(:,:,:,nnew)
! CF(i,0) vertically integrated mass flux/error/correction for the
!             intermediate time step [n+1/2] mass fluxes FlxU,FlxV.
!
! This procedure also replaces instantaneous (with respect to the
! fast time step) vertically integrated (barotropic) velocities with
! their values based on fast-time-averaged mass fluxes. These are to
! be used as initial conditions for the barotropic mode at the new
! time step.
!
! Explanation of horizontal loop indices: in the case of periodic
! boundaries (in either direction) the coupling and computation of
! mass fluxes FlxU,FlxV is performed within the internal range of
! indices (excluding ghost zones), after that the ghost points for
! the newly computed arrays are exchanged; in the case of nonperiodic
! boundaries the coupling is done over the extended range of indices
! (that is including boundary points).
!
# ifdef EW_PERIODIC
#  define IU_RANGE istr,iend
#  define IV_RANGE istr,iend
# else
#  define IU_RANGE istr,iendR
#  define IV_RANGE istrR,iendR
# endif
 
# ifdef NS_PERIODIC
#  define JU_RANGE jstr,jend
# else
#  define JU_RANGE jstrR,jendR
# endif
 
      do j=JU_RANGE                  !--> XI-component
        do i=IU_RANGE
          DC(i,N)=0.5*(Hz(i,j,N)+Hz(i-1,j,N))*dn_u(i,j)
          DC(i,0)=DC(i,N)
          FC(i,0)=DC(i,N)*u(i,j,N,nnew)
        enddo
        do k=N-1,1,-1
          do i=IU_RANGE
            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)
            DC(i,0)=DC(i,0)+DC(i,k)
            FC(i,0)=FC(i,0)+DC(i,k)*u(i,j,k,nnew)
          enddo
        enddo
        do i=IU_RANGE
          DC(i,0)=1./DC(i,0)
          ubar(i,j,knew)=DC(i,0)*DU_avg1(i,j)
          FC(i,0)=DC(i,0)*(FC(i,0)-DU_avg1(i,j))
!>
          CF(i,0)=0.
        enddo          !--> keep DC(:,0) for later
        do k=N,1,-1
          do i=IU_RANGE
            u(i,j,k,nnew)=(u(i,j,k,nnew)-FC(i,0))
# ifdef MASKING
     &                                  *umask(i,j)
# endif
c--# define TR
# ifdef TR
            CF(i,k)=0.5*DC(i,k)*(u(i,j,k,nstp)+u(i,j,k,nnew))
#else
#define EPSIL 0.125
#define DELTA 0.75
 
            CF(i,k)=DELTA*FlxU(i,j,k) + EPSIL*DC(i,k)
     &                         *(u(i,j,k,nstp)+u(i,j,k,nnew))
#endif
            CF(i,0)=CF(i,0)+CF(i,k)
          enddo
        enddo
        do i=IU_RANGE
          CF(i,0)=DC(i,0)*(CF(i,0)-DU_avg2(i,j))
        enddo
        do k=1,N,+1
          do i=IU_RANGE
            FlxU(i,j,k)=CF(i,k)-DC(i,k)*CF(i,0)
          enddo
        enddo
 
        if (j.ge.jstr) then         !--> ETA-component
          do i=IV_RANGE
            DC(i,N)=0.5*(Hz(i,j,N)+Hz(i,j-1,N))*dm_v(i,j)
            DC(i,0)=DC(i,N)
            FC(i,0)=DC(i,N)*v(i,j,N,nnew)
          enddo
          do k=N-1,1,-1
            do i=IV_RANGE
              DC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)
              DC(i,0)=DC(i,0)+DC(i,k)
              FC(i,0)=FC(i,0)+DC(i,k)*v(i,j,k,nnew)
            enddo
          enddo
          do i=IV_RANGE
            DC(i,0)=1./DC(i,0)
            vbar(i,j,knew)=DC(i,0)*DV_avg1(i,j)
            FC(i,0)=DC(i,0)*(FC(i,0)-DV_avg1(i,j))
!>
            CF(i,0)=0.
          enddo        !--> keep DC(:,0) for later
          do k=N,1,-1
            do i=IV_RANGE
              v(i,j,k,nnew)=(v(i,j,k,nnew)-FC(i,0))
# ifdef MASKING
     &                                    *vmask(i,j)
# endif
# ifdef TR
              CF(i,k)=0.5*DC(i,k)*(v(i,j,k,nstp)+v(i,j,k,nnew))
# else
              CF(i,k)=DELTA*FlxV(i,j,k) + EPSIL*DC(i,k)
     &                           *(v(i,j,k,nstp)+v(i,j,k,nnew))
#  undef DELTA
#  undef EPSIL
# endif
              CF(i,0)=CF(i,0)+CF(i,k)
            enddo
          enddo
          do i=IV_RANGE
            CF(i,0)=DC(i,0)*(CF(i,0)-DV_avg2(i,j))
          enddo
          do k=1,N,+1
            do i=IV_RANGE
              FlxV(i,j,k)=CF(i,k)-DC(i,k)*CF(i,0)
            enddo
          enddo
        endif       !<-- j.ge.jstr
      enddo       ! <-- j
# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
!
! Exchange periodic boundaries and computational margins.
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (istr,iend,jstr,jend,
     &                                 u(START_2D_ARRAY,1,nnew))
      call exchange_v3d_tile (istr,iend,jstr,jend,
     &                                 v(START_2D_ARRAY,1,nnew))
 
      call exchange_u3d_tile (istr,iend,jstr,jend, FlxU)
      call exchange_v3d_tile (istr,iend,jstr,jend, FlxV)
 
      call exchange_u2d_tile (istr,iend,jstr,jend,
     &                                ubar(START_2D_ARRAY,knew))
      call exchange_v2d_tile (istr,iend,jstr,jend,
     &                                vbar(START_2D_ARRAY,knew))
# endif
      return
      end
#else
      subroutine step3d_uv_empty
      return
      end
#endif /* SOLVE3D */
 
