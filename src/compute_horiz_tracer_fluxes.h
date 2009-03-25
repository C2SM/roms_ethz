! This code segment computes horizontal fluxes for tracer variables.
! Basically it interpolates tracer values from their native locations
! on C grid to horizontal velocity points. Curently three options are
! supported: 4-point symmetric fourth-order method (default); 3-point
! upstream-biased parabolic interpolation (UPSTREAM); and 4-point
! scheme where arithmetic averaging of elementary differences is
! replaced by harmonic averaging (AKIMA), resulting in mid-point
! values bounded by two nearest values at native location, regardless
! of grid-scale roughness of the interpolated field, while still
! retaining asymptotic fourth-order behavior for smooth fields.
! This code is extracted into a special module ibecause it is used
! twice, in predictor and corrector substeps for tracer variables. 
! 


#ifdef BIO_1ST_USTREAM_TEST
        if (itrc.gt.isalt) then   !<-- biological components only
          if (nrhs.eq.3) then     !<-- compute fluxes during corrector
            do j=Jstr,Jend        !    stage only
              do i=Istr,Iend+1
                FX(i,j)=t(i-1,j,k,nstp,itrc)*max(FlxU(i,j,k),0.)
     &                 +t(i  ,j,k,nstp,itrc)*min(FlxU(i,j,k),0.)
              enddo
            enddo
            do j=Jstr,Jend+1
              do i=Istr,Iend
                FE(i,j)=t(i,j-1,k,nstp,itrc)*max(FlxV(i,j,k),0.)
     &                 +t(i,j  ,k,nstp,itrc)*min(FlxV(i,j,k),0.)  
              enddo
            enddo
          else                    ! there is no need to compute
            do j=Jstr,Jend+1      ! fluxes during predictor stage 
              do i=Istr,Iend+1    ! because there is no use for 
                FX(i,j)=0.        ! t(:,:,:,:,n+1/2) in the case 
                FE(i,j)=0.        ! of 1st-order upsteam (note 
              enddo               ! index "nstp" instead of "nrhs"
            enddo                 ! above. 
          endif
        else       !--> standard code applies for T,S
#endif

# define UPSTREAM
c--# define AKIMA
c--# define CONST_TRACERS


 
# ifdef UPSTREAM
#  define curv WORK
# else
#  define grad WORK
# endif
# ifdef EW_PERIODIC
          imin=Istr-1                  ! Determine extended index
          imax=Iend+2                  ! range for computation of
# else
          if (WESTERN_EDGE) then       ! elementary differences:
            imin=Istr                  ! it needs to be restricted 
          else                         ! because in the vicinity of  
            imin=Istr-1                ! physical boundary the extra
          endif                        ! point may be not available,
          if (EASTERN_EDGE) then       ! and extrapolation of slope
            imax=Iend+1                ! is used instead.
          else
            imax=Iend+2
          endif
# endif
          do j=Jstr,Jend
            do i=imin,imax
              FX(i,j)=(t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc))
# ifdef MASKING
     &                                               *umask(i,j)
# endif
            enddo
          enddo            !--> discard imin,imax
# ifndef EW_PERIODIC
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              FX(Istr-1,j)=FX(Istr,j)
            enddo
          endif
          if (EASTERN_EDGE) then
            do j=Jstr,Jend
              FX(Iend+2,j)=FX(Iend+1,j)
            enddo
          endif
# endif
          do j=Jstr,Jend
            do i=Istr-1,Iend+1
# if defined UPSTREAM
              curv(i,j)=FX(i+1,j)-FX(i,j)
# elif defined AKIMA
              cff=2.*FX(i+1,j)*FX(i,j)
              if (cff.gt.epsil) then
                grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
              else
                grad(i,j)=0.
              endif
# else
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
# endif
            enddo
          enddo             !--> discard FX
          do j=Jstr,Jend
            do i=Istr,Iend+1
# ifdef UPSTREAM
              FX(i,j)=0.5*(t(i,j,k,nrhs,itrc)+t(i-1,j,k,nrhs,itrc))
     &                                                  *FlxU(i,j,k)
     &            -0.166666666666*( curv(i-1,j)*max(FlxU(i,j,k),0.)
     &                             +curv(i  ,j)*min(FlxU(i,j,k),0.))
#  ifdef DIAGNOSTICS_TS
              TruncFX(i,j)=0.0416666666667*(curv(i,j)-curv(i-1,j))
     &                                           *abs(FlxU(i,j,k))
#  endif
# else
              FX(i,j)=0.5*( t(i,j,k,nrhs,itrc)+t(i-1,j,k,nrhs,itrc)
     &                      -0.333333333333*( grad(i,j)-grad(i-1,j))
     &                                                 )*FlxU(i,j,k)
# endif
            enddo           !--> discard curv,grad, keep FX
          enddo
 
# ifdef NS_PERIODIC
          jmin=Jstr-1
          jmax=Jend+2
# else
          if (SOUTHERN_EDGE) then
            jmin=Jstr
          else
            jmin=Jstr-1
          endif
          if (NORTHERN_EDGE) then
            jmax=Jend+1
          else
            jmax=Jend+2
          endif
# endif
          do j=jmin,jmax
            do i=Istr,Iend
              FE(i,j)=(t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc))
# ifdef MASKING
     &                                               *vmask(i,j)
# endif
            enddo
          enddo         !--> discard jmin,jmax
# ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,Jstr-1)=FE(i,Jstr)
            enddo
          endif
          if (NORTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,Jend+2)=FE(i,Jend+1)
            enddo
          endif
# endif
          do j=Jstr-1,Jend+1
            do i=Istr,Iend
# if defined UPSTREAM
              curv(i,j)=FE(i,j+1)-FE(i,j)
# elif defined AKIMA
              cff=2.*FE(i,j+1)*FE(i,j)
              if (cff.gt.epsil) then
                grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
              else
                grad(i,j)=0.
              endif
# else
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
# endif
            enddo
          enddo            !--> discard FE
 
          do j=Jstr,Jend+1
            do i=Istr,Iend
# ifdef UPSTREAM
              FE(i,j)=0.5*(t(i,j,k,nrhs,itrc)+t(i,j-1,k,nrhs,itrc))
     &                                                  *FlxV(i,j,k)
     &            -0.166666666666*( curv(i,j-1)*max(FlxV(i,j,k),0.)
     &                             +curv(i,j  )*min(FlxV(i,j,k),0.))
#  ifdef DIAGNOSTICS_TS
              TruncFE(i,j)=0.0416666666667*(curv(i,j)-curv(i-1,j))
     &                                           *abs(FlxV(i,j,k))
#  endif
# else
              FE(i,j)=0.5*( t(i,j,k,nrhs,itrc)+t(i,j-1,k,nrhs,itrc)
     &                       -0.333333333333*(grad(i,j)-grad(i,j-1))
     &                                                 )*FlxV(i,j,k)
# endif
            enddo
          enddo            !--> discard curv,grad, keep FE

# ifdef PSOURCE
                         ! Set tracer flux due to point source to
          do is=1,Nsrc   ! simulate different water properties due
            i=Isrc(is)   ! to river run off.
            j=Jsrc(is)
            if (Lsrc(is,itrc) .and. Istr.le.i .and. i.le.Iend+1
     &                   .and. Jstr.le.j .and. j.le.Jend+1) then
              if (Dsrc(is).eq.0) then
                FX(i,j)=FlxU(i,j,k)*Tsrc(is,k,itrc)
              else
                FE(i,j)=FlxV(i,j,k)*Tsrc(is,k,itrc)
              endif
            endif
          enddo
# endif
 
#ifdef BIO_1ST_USTREAM_TEST
        endif  !<-- itrc.gt.isalt, bio-components only.
#endif
 
