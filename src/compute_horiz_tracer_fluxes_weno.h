! Compute horizontal fluxes for tracers.  Essentially interpolate
! tracer values from their native locations on C grid to horizontal
! velocity points with simultaneous translation from grid-box-averages
! to instantaneous values at interface location.  Three options an be
! selected: 3-point upstream-biased parabolic interpolation (UPSTREAM_TS);
!  4-point symmetric fourth-order method (undefined state of both CPP
! switches); 4-point scheme where arithmetic averaging of elementary
! differences is replaced by harmonic averaging (AKIMA), resulting in
! mid-point values be bounded by two nearest values at native location,
! regardless of grid-scale roughness of the interpolated field, while
! still retaining asymptotic fourth-order behavior for smooth fields.
! This code is extracted into a special module because it is used
! twice, in predictor and corrector substeps for tracer variables.





#ifdef BIO_1ST_USTREAM_TEST
        if (itrc>isalt) then       ! biological tracer components:
          if (nrhs==3) then         ! compute fluxes during corrector
            do j=jstr,jend            ! stage only
              do i=istr,iend+1
                FX(i,j)=t(i-1,j,k,nstp,itrc)*max(FlxU(i,j,k),0.)
     &                 +t(i  ,j,k,nstp,itrc)*min(FlxU(i,j,k),0.)
              enddo
            enddo
            do j=jstr,jend+1
              do i=istr,iend
                FE(i,j)=t(i,j-1,k,nstp,itrc)*max(FlxV(i,j,k),0.)
     &                 +t(i,j  ,k,nstp,itrc)*min(FlxV(i,j,k),0.)
              enddo
            enddo
          else                         ! there is no need to compute
            do j=jstr,jend+1           ! fluxes during predictor stage
              do i=istr,iend+1         ! because there is no use for
                FX(i,j)=0.             ! t(:,:,:,:,n+1/2) in the case
                FE(i,j)=0.             ! of 1st-order upsteam (note
              enddo                    ! index "nstp" instead of
            enddo                      ! "nrhs" above.
          endif
        else       !--> standard code applies for T,S
#endif


#ifdef UPSTREAM_TS
# define curv wrk1
#else
# define grad wrk1
#endif
#ifndef EW_PERIODIC
          if (WESTERN_EDGE) then       ! Determine extended index
            imin=istr                  ! range for computation of
          else                         ! elementary differences: it
            imin=istr-1                ! needs to be restricted
          endif                        ! because in the vicinity of
          if (EASTERN_EDGE) then       ! physical boundary the extra
            imax=iend                  ! point may be not available,
          else                         ! and extrapolation of slope
            imax=iend+1                ! is used instead.
          endif
#else
          imin=istr-1
          imax=iend+1
#endif

#if defined T_HADV_WENO 
# if defined ITRC_START_WENO
          if (itrc >= is_weno) then   !<- WENO for high index tracers  only
# endif
!-------------------------------------------------------------------!
!!!!!!!!!!!!! COMPUTATION OF FX (loop on i indices) !!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------!

          DO i = istr,iend+1  !i_loop_x_flux_
                                                  !
            IF ( i.eq.imin-2 ) THEN   ! 2nd order flux next to west
                                           ! boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                FX(i,j) = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel)
              ENDDO
              
              FX(i,j)=FX(i,j)*umask(i,j)

                                                             !
            ELSE IF ( i.ge.imin-1 .and. i.le.imax+1 ) THEN       ! 3rd of 2nd order flux
                                                             ! from west boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
                FX(i,j)=flx3
#  ifdef MASKING
                flx2 = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel)
                
                mask1=umask(i-2,j)*umask(i+1,j)
                
                FX(i,j)=mask1*flx3+(1-mask1)*flx2
                
                FX(i,j)=FX(i,j)*umask(i,j)
                

#  else
                FX(i,j)=flx3
#  endif /* MASKING */
              ENDDO
                                          !
            ELSE IF ( i.eq.imax+2 ) THEN  ! 2nd order flux next to east
                                          ! boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                FX(i,j) = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel)
              ENDDO
              
              FX(i,j)=FX(i,j)*umask(i,j)
            
            ENDIF
          ENDDO ! i_loop_x_flux_5


# ifdef ITRC_START_WENO
          else   !<-- itrc < is_weno (no WENO for low index tracers).
# endif  
#elif  ITRC_START_WENO || ! defined T_HADV_WENO  


          do j=jstr,jend
            do i=imin,imax+1
              FX(i,j)=(t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc))
#ifdef MASKING
     &                                               *umask(i,j)
#endif
            enddo
          enddo
#ifndef EW_PERIODIC
          if (WESTERN_EDGE) then
            do j=jstr,jend
              FX(istr-1,j)=FX(istr,j)
            enddo
          endif
          if (EASTERN_EDGE) then
            do j=jstr,jend
              FX(iend+2,j)=FX(iend+1,j)
            enddo
          endif
#endif
          do j=jstr,jend
            do i=istr-1,iend+1
#if defined UPSTREAM_TS
              curv(i,j)=FX(i+1,j)-FX(i,j)
#elif defined AKIMA
              cff=2.*FX(i+1,j)*FX(i,j)
              if (cff>epsil) then
                grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
              else
                grad(i,j)=0.
              endif
#else
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
#endif
            enddo
          enddo             !--> discard FX
          do j=jstr,jend
            do i=istr,iend+1
#ifdef UPSTREAM_TS
              FX(i,j)=0.5*(t(i,j,k,nrhs,itrc)+t(i-1,j,k,nrhs,itrc))
     &                                                  *FlxU(i,j,k)
     &          -0.1666666666666666*( curv(i-1,j)*max(FlxU(i,j,k),0.)
     &                               +curv(i  ,j)*min(FlxU(i,j,k),0.))
# ifdef DIAGNOSTICS_TS
              TruncFX(i,j)=0.04166666666666667*(curv(i,j)-curv(i-1,j))
     &                                               *abs(FlxU(i,j,k))
# endif
#else
              FX(i,j)=0.5*( t(i,j,k,nrhs,itrc)+t(i-1,j,k,nrhs,itrc)
     &                   -0.3333333333333333*(grad(i,j)-grad(i-1,j))
     &                                                 )*FlxU(i,j,k)
#endif
            enddo           !--> discard curv,grad, keep FX
          enddo

#endif
#ifdef ITRC_START_WENO
          endif   !<-- itrc >= is_weno (WENO for high index tracers  only)
#endif
!endif weno i loop
          
#ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            jmin=jstr
          else
            jmin=jstr-1
          endif
          if (NORTHERN_EDGE) then
            jmax=jend
          else
            jmax=jend+1
          endif
#else
          jmin=jstr-1
          jmax=jend+1
#endif

#ifdef T_HADV_WENO
# if defined ITRC_START_WENO
          if (itrc >= is_weno) then   !<- WENO for high index tracers
# endif
!-------------------------------------------------------------------!
!!!!!!!!!!!!! COMPUTATION OF FE (loop on j indices) !!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------!

         DO j = jstr,jend+1  !j_loop_y_flux
                                                  !
            IF ( j.eq.jmin-2 ) THEN   ! 2nd order flux next to south
                                           ! boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                FE(i,j) = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel)
              ENDDO
              
              FE(i,j)=FE(i,j)*vmask(i,j)
                                                             !
            ELSE IF ( j.ge.jmin-1 .and. j.le.jmax+1 ) THEN  ! 3rd of 2nd order flux 2 in
                                                             ! from south boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
                FE(i,j)=flx3
                
#  ifdef MASKING
                flx2 = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel)
                
                mask1=vmask(i,j-2)*vmask(i,j+1)
                
                FE(i,j)=mask1*flx3+(1-mask1)*flx2
                
                FE(i,j)=FE(i,j)*vmask(i,j)
       

#  else
                FE(i,j)=flx3
#  endif /* MASKING */
                
              ENDDO
                                          !
            ELSE IF ( j.eq.jmax+2 ) THEN  ! 2nd order flux next to north
                                          ! boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                FE(i,j) = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel)
              ENDDO
              
              FE(i,j)=FE(i,j)*vmask(i,j)

            
            ENDIF
          ENDDO ! j_loop_y_flux_5

# ifdef ITRC_START_WENO
          else   !<-- itrc < is_weno (no WENO for low index tracers)
# endif  
#elif  ITRC_START_WENO || ! defined T_HADV_WENO  

          do j=jmin,jmax+1
            do i=istr,iend
              FE(i,j)=(t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc))
#ifdef MASKING
     &                                               *vmask(i,j)
#endif
            enddo
          enddo
#ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=istr,iend
              FE(i,jstr-1)=FE(i,jstr)
            enddo
          endif
          if (NORTHERN_EDGE) then
            do i=istr,iend
              FE(i,jend+2)=FE(i,jend+1)
            enddo
          endif
#endif
          do j=jstr-1,jend+1
            do i=istr,iend
#if defined UPSTREAM_TS
              curv(i,j)=FE(i,j+1)-FE(i,j)
#elif defined AKIMA
              cff=2.*FE(i,j+1)*FE(i,j)
              if (cff>epsil) then
                grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
              else
                grad(i,j)=0.
              endif
#else
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
#endif
            enddo
          enddo            !--> discard FE

          do j=jstr,jend+1
            do i=istr,iend
#ifdef UPSTREAM_TS
              FE(i,j)=0.5*(t(i,j,k,nrhs,itrc)+t(i,j-1,k,nrhs,itrc))
     &                                                  *FlxV(i,j,k)
     &          -0.1666666666666666*( curv(i,j-1)*max(FlxV(i,j,k),0.)
     &                               +curv(i,j  )*min(FlxV(i,j,k),0.))
# ifdef DIAGNOSTICS_TS
              TruncFE(i,j)=0.04166666666666667*(curv(i,j)-curv(i-1,j))
     &                                               *abs(FlxV(i,j,k))
# endif
#else
              FE(i,j)=0.5*( t(i,j,k,nrhs,itrc)+t(i,j-1,k,nrhs,itrc)
     &                   -0.3333333333333333*(grad(i,j)-grad(i,j-1))
     &                                                 )*FlxV(i,j,k)
#endif
            enddo
          enddo             !--> discard curv,grad, keep FE
          
#endif 
#ifdef ITRC_START_WENO
          endif   !<-- itrc <= is_weno (no WENO for low index tracers)
#endif
!endif weno j loop

#ifdef PSOURCE
          do is=1,Nsrc             ! Set tracer fluxes due to point
            i=Isrc(is)             ! sources to simulate river run off
            j=Jsrc(is)
            if ( istr<=i .and. i<=iend+1 .and.
     &           jstr<=j .and. j<=jend+1 ) then
              if (Dsrc(is)==0) then
                FX(i,j)=FlxU(i,j,k)*Tsrc(is,k,itrc)
              else
                FE(i,j)=FlxV(i,j,k)*Tsrc(is,k,itrc)
              endif
            endif
          enddo
#endif
#ifdef BIO_1ST_USTREAM_TEST
        endif  !<-- itrc>isalt, bio-components only.
#endif
