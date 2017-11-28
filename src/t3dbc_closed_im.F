#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine t3dbc_closed_tile (istr,iend,jstr,jend, itrc)
!
! Set closed lateral boundary conditions for tracer field t(:,:,:,itrc)
!
      implicit none
      integer istr,iend,jstr,jend, itrc, i,j,k
      real cff
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=1,N
          do j=jstr,jend
            t(istr-1,j,k,nnew,itrc)=t(istr,j,k,nnew,itrc)
#   ifdef MASKING
     &                                   *rmask(istr-1,j)
#   endif
          enddo
        enddo
      endif     ! <-- WESTERN_EDGE

      if (EASTERN_EDGE) then
        do k=1,N
          do j=jstr,jend
            t(iend+1,j,k,nnew,itrc)=t(iend,j,k,nnew,itrc)
#   ifdef MASKING
     &                                   *rmask(iend+1,j)
#   endif
          enddo
        enddo
      endif    ! <-- EASTERN_EDGE
# endif        /* !EW_PERIODIC */

# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=istr,iend
            t(i,jstr-1,k,nnew,itrc)=t(i,jstr,k,nnew,itrc)
#   ifdef MASKING
     &                                   *rmask(i,jstr-1)
#   endif
          enddo
        enddo
      endif    ! <-- SOUTHERN_EDGE

      if (NORTHERN_EDGE) then
        do k=1,N
          do i=istr,iend
            t(i,jend+1,k,nnew,itrc)=t(i,jend,k,nnew,itrc)
#   ifdef MASKING
     &                                   *rmask(i,jend+1)
#   endif
          enddo
        enddo
      endif    ! <-- NORTHERN_EDGE
# endif /* !NS_PERIODIC */
!
! Corner points between adjacent boundaries. Note that because
! boundary conditions for tracers are of gradient nature -- either
! Neumann, or radiation, the corner points have to be set in all
! cases, even if the adjacent boundaries are closed. This differs
! from setting boundaries for velocities, where there is anisotropy
! associated with normal and tangential components. In the case when 
! one/or both points near the corner is/are masked, the use of masked
! points is avoided. 

# ifndef EW_PERIODIC
#  ifndef NS_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(istr,jstr-1)+rmask(istr-1,jstr)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(istr-1,jstr-1,k,nnew,itrc)=cff*(
     &              rmask(istr,jstr-1)*t(istr,jstr-1,k,nnew,itrc)
     &             +rmask(istr-1,jstr)*t(istr-1,jstr,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(istr-1,jstr-1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(istr-1,jstr-1,k,nnew,itrc)=0.5*( t(istr,jstr-1,k,nnew,
     &                           itrc)+t(istr-1,jstr,k,nnew,itrc))
        enddo
#   endif
      endif

      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(iend,jstr-1)+rmask(iend+1,jstr)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(iend+1,jstr-1,k,nnew,itrc)=cff*(
     &              rmask(iend,jstr-1)*t(iend,jstr-1,k,nnew,itrc)
     &             +rmask(iend+1,jstr)*t(iend+1,jstr,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(iend+1,jstr-1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(iend+1,jstr-1,k,nnew,itrc)=0.5*(t(iend,jstr-1,k,nnew,
     &                           itrc)+t(iend+1,jstr,k,nnew,itrc))
        enddo
#   endif
      endif

      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(istr,jend+1)+rmask(istr-1,jend)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(istr-1,jend+1,k,nnew,itrc)=cff*(
     &              rmask(istr,jend+1)*t(istr,jend+1,k,nnew,itrc)
     &             +rmask(istr-1,jend)*t(istr-1,jend,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(istr-1,jend+1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(istr-1,jend+1,k,nnew,itrc)=0.5*( t(istr,jend+1,k,nnew,
     &                           itrc)+t(istr-1,jend,k,nnew,itrc))
        enddo
#   endif
      endif

      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(iend,jend+1)+rmask(iend+1,jend)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(iend+1,jend+1,k,nnew,itrc)=cff*(
     &              rmask(iend,jend+1)*t(iend,jend+1,k,nnew,itrc)
     &             +rmask(iend+1,jend)*t(iend+1,jend,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(iend+1,jend+1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(iend+1,jend+1,k,nnew,itrc)=0.5*( t(iend,jend+1,k,nnew,
     &                           itrc)+t(iend+1,jend,k,nnew,itrc))
        enddo
#   endif
      endif
#  endif /* !NS_PERIODIC */
# endif /* !EW_PERIODIC */
      return
      end
#else
      subroutine t3dbc_closed_empty
      end
#endif /* SOLVE3D */
