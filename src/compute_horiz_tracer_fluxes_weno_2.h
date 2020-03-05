!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! COPY FROM t3dadv_order5.h



 


! #  ifdef EW_PERIODIC
!           imin=1
!           imax=LOCALLM+1
! #  else
!           imin=3
!           imax=Lm-1
! #   endif
! #  endif

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

          DO i = istr,iend+1  !i_loop_x_flux_5
                                                  !
            IF ( i.ge.imin .and. i.le.imax ) THEN ! use full stencil
                                                  !
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                flx5 = vel*flux5_weno(
     &             t(i-3,j,k,nrhs,itrc), t(i-2,j,k,nrhs,itrc),
     &             t(i-1,j,k,nrhs,itrc), t(i  ,j,k,nrhs,itrc),
     &             t(i+1,j,k,nrhs,itrc), t(i+2,j,k,nrhs,itrc),  vel )
                FX(i,j)=flx5
              ENDDO
                                           !
            ELSE IF ( i.eq.imin-2 ) THEN   ! 2nd order flux next to south
                                           ! boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                FX(i,j) = vel*flux1(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                                             !
            ELSE IF ( i.eq.imin-1 .and. imax.ge.imin ) THEN  ! 3rd of 4th order flux 2 in
                                                             ! from south boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
                FX(i,j)=flx3
              ENDDO
                                          !
            ELSE IF ( i.eq.imax+2 ) THEN  ! 2nd order flux next to north
                                          ! boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                FX(i,j) = vel*flux1(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                          !
            ELSE IF ( i.eq.imax+1 ) THEN  ! 3rd or 4th order flux 2 in from
                                          ! north boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
                FX(i,j)=flx3
              ENDDO
            ENDIF
          ENDDO ! i_loop_x_flux_5
          
          
! #  ifdef NS_PERIODIC
!           jmin=1
!           jmax=LOCALMM+1
! #  else
!           jmin=3
!           jmax=Mm-1
! #  endif
! 

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

         DO j = jstr,jend+1  !j_loop_y_flux_5
                                                  !
            IF ( j.ge.jmin .and. j.le.jmax ) THEN ! use full stencil
                                                  !
              DO i = istr,iend
                vel = Flxv(i,j,k)
                flx5 = vel*flux5_weno(
     &             t(i,j-3,k,nrhs,itrc), t(i,j-2,k,nrhs,itrc), 
     &             t(i,j-1,k,nrhs,itrc), t(i,j  ,k,nrhs,itrc),
     &             t(i,j+1,k,nrhs,itrc), t(i,j+2,k,nrhs,itrc),  vel )
                FE(i,j)=flx5
              ENDDO
                                           !
            ELSE IF ( j.eq.jmin-2 ) THEN   ! 2nd order flux next to south
                                           ! boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                FE(i,j) = vel*flux1(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                                             !
            ELSE IF ( j.eq.jmin-1 .and. jmax.ge.jmin ) THEN  ! 3rd of 4th order flux 2 in
                                                             ! from south boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
                FE(i,j)=flx3
              ENDDO
                                          !
            ELSE IF ( j.eq.jmax+2 ) THEN  ! 2nd order flux next to north
                                          ! boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                FE(i,j) = vel*flux1(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                          !
            ELSE IF ( j.eq.jmax+1 ) THEN  ! 3rd or 4th order flux 2 in from
                                          ! north boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
                FE(i,j)=flx3
              ENDDO
            ENDIF
          ENDDO ! j_loop_y_flux_5


          
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!            EEENNNDDD !!!!!!!!!!!!          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
!           
!           
!           
!           
!           
! #ifdef UPSTREAM_TS
! # define curv wrk1
! #else
! # define grad wrk1
! #endif
! #ifndef EW_PERIODIC
!           if (WESTERN_EDGE) then       ! Determine extended index
!             imin=istr                  ! range for computation of
!           else                         ! elementary differences: it
!             imin=istr-1                ! needs to be restricted
!           endif                        ! because in the vicinity of
!           if (EASTERN_EDGE) then       ! physical boundary the extra
!             imax=iend                  ! point may be not available,
!           else                         ! and extrapolation of slope
!             imax=iend+1                ! is used instead.
!           endif
! #else
!           imin=istr-1
!           imax=iend+1
! #endif
!           do j=jstr,jend
!             do i=imin,imax+1
!               FX(i,j)=(t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc))
! #ifdef MASKING
!      &                                               *umask(i,j)
! #endif
!             enddo
!           enddo
! #ifndef EW_PERIODIC
!           if (WESTERN_EDGE) then
!             do j=jstr,jend
!               FX(istr-1,j)=FX(istr,j)
!             enddo
!           endif
!           if (EASTERN_EDGE) then
!             do j=jstr,jend
!               FX(iend+2,j)=FX(iend+1,j)
!             enddo
!           endif
! #endif
!           do j=jstr,jend
!             do i=istr-1,iend+1
! #if defined UPSTREAM_TS
!               curv(i,j)=FX(i+1,j)-FX(i,j)
! #elif defined AKIMA
!               cff=2.*FX(i+1,j)*FX(i,j)
!               if (cff>epsil) then
!                 grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
!               else
!                 grad(i,j)=0.
!               endif
! #else
!               grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
! #endif
!             enddo
!           enddo             !--> discard FX
!           do j=jstr,jend
!             do i=istr,iend+1
! #ifdef UPSTREAM_TS
!               FX(i,j)=0.5*(t(i,j,k,nrhs,itrc)+t(i-1,j,k,nrhs,itrc))
!      &                                                  *FlxU(i,j,k)
!      &          -0.1666666666666666*( curv(i-1,j)*max(FlxU(i,j,k),0.)
!      &                               +curv(i  ,j)*min(FlxU(i,j,k),0.))
! # ifdef DIAGNOSTICS_TS
!               TruncFX(i,j)=0.04166666666666667*(curv(i,j)-curv(i-1,j))
!      &                                               *abs(FlxU(i,j,k))
! # endif
! #else
!               FX(i,j)=0.5*( t(i,j,k,nrhs,itrc)+t(i-1,j,k,nrhs,itrc)
!      &                   -0.3333333333333333*(grad(i,j)-grad(i-1,j))
!      &                                                 )*FlxU(i,j,k)
! #endif
!             enddo           !--> discard curv,grad, keep FX
!           enddo
! 
! #ifndef NS_PERIODIC
!           if (SOUTHERN_EDGE) then
!             jmin=jstr
!           else
!             jmin=jstr-1
!           endif
!           if (NORTHERN_EDGE) then
!             jmax=jend
!           else
!             jmax=jend+1
!           endif
! #else
!           jmin=jstr-1
!           jmax=jend+1
! #endif
!           do j=jmin,jmax+1
!             do i=istr,iend
!               FE(i,j)=(t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc))
! #ifdef MASKING
!      &                                               *vmask(i,j)
! #endif
!             enddo
!           enddo
! #ifndef NS_PERIODIC
!           if (SOUTHERN_EDGE) then
!             do i=istr,iend
!               FE(i,jstr-1)=FE(i,jstr)
!             enddo
!           endif
!           if (NORTHERN_EDGE) then
!             do i=istr,iend
!               FE(i,jend+2)=FE(i,jend+1)
!             enddo
!           endif
! #endif
!           do j=jstr-1,jend+1
!             do i=istr,iend
! #if defined UPSTREAM_TS
!               curv(i,j)=FE(i,j+1)-FE(i,j)
! #elif defined AKIMA
!               cff=2.*FE(i,j+1)*FE(i,j)
!               if (cff>epsil) then
!                 grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
!               else
!                 grad(i,j)=0.
!               endif
! #else
!               grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
! #endif
!             enddo
!           enddo            !--> discard FE
! 
!           do j=jstr,jend+1
!             do i=istr,iend
! #ifdef UPSTREAM_TS
!               FE(i,j)=0.5*(t(i,j,k,nrhs,itrc)+t(i,j-1,k,nrhs,itrc))
!      &                                                  *FlxV(i,j,k)
!      &          -0.1666666666666666*( curv(i,j-1)*max(FlxV(i,j,k),0.)
!      &                               +curv(i,j  )*min(FlxV(i,j,k),0.))
! # ifdef DIAGNOSTICS_TS
!               TruncFE(i,j)=0.04166666666666667*(curv(i,j)-curv(i-1,j))
!      &                                               *abs(FlxV(i,j,k))
! # endif
! #else
!               FE(i,j)=0.5*( t(i,j,k,nrhs,itrc)+t(i,j-1,k,nrhs,itrc)
!      &                   -0.3333333333333333*(grad(i,j)-grad(i,j-1))
!      &                                                 )*FlxV(i,j,k)
! #endif
!             enddo
!           enddo             !--> discard curv,grad, keep FE
! 
! #ifdef PSOURCE
!           do is=1,Nsrc             ! Set tracer fluxes due to point
!             i=Isrc(is)             ! sources to simulate river run off
!             j=Jsrc(is)
!             if ( istr<=i .and. i<=iend+1 .and.
!      &           jstr<=j .and. j<=jend+1 ) then
!               if (Dsrc(is)==0) then
!                 FX(i,j)=FlxU(i,j,k)*Tsrc(is,k,itrc)
!               else
!                 FE(i,j)=FlxV(i,j,k)*Tsrc(is,k,itrc)
!               endif
!             endif
!           enddo
! #endif
! #ifdef BIO_1ST_USTREAM_TEST
!         endif  !<-- itrc>isalt, bio-components only.
! #endif
