!
!===============================================================
!
! Compute 5th order horizontal advection
!
!===============================================================
!

! To compute the flux, we will use 5th order weno scheme inside domain,
! but the order of accuracy of the method will be lower close to boundary 
! according to the number of neighbour points available. For example,
! WENO 5 needs information of points from i-3 to i+2, but WENO 3 only need 
! i-2 to i+1, therefore it will be used close to the south boundary for example.
! The same logic apply to point in the domain but close to land, the order 
! will be reduced in order to use only information of sea cell.
!

#ifndef EW_PERIODIC
          if (WESTERN_EDGE) then       ! Determine extended index
            imin=istr+2                ! range for computation of
          else                         ! elementary differences: it
            imin=istr+1                ! needs to be restricted
          endif                        ! because in the vicinity of
          if (EASTERN_EDGE) then       ! physical boundary the extra
            imax=iend-1                ! point may be not available,
          else                         ! and extrapolation of slope
            imax=iend                  ! is used instead.
          endif
#else
          imin=istr-1
          imax=iend+1
#endif

#ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            jmin=jstr+2
          else
            jmin=jstr+1
          endif
          if (NORTHERN_EDGE) then
            jmax=jend-1
          else
            jmax=jend
          endif
#else
          jmin=jstr-1
          jmax=jend+1
#endif

!-------------------------------------------------------------------!
!!!!!!!!!!!!! COMPUTATION OF FX (loop on i indices) !!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------!

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
#  ifdef MASKING 
                flx3 = vel*flux3_weno(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel ) 
                flx2 = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)

                mask1=umask(i-2,j)*umask(i+1,j)
                 
                mask2=umask(i-3,j)*umask(i+2,j)
                 
                mask0=mask1*mask2
          
                FX(i,j)=mask0*flx5+(1-mask0)*mask1*flx3+(1-mask0)*(1-mask1)*flx2
                  
                FX(i,j)=FX(i,j)*umask(i,j)
                  

#  else
                FE(i,j)=flx5
                
#  endif /* MASKING */
              ENDDO
                                           !
            ELSE IF ( i.eq.imin-2 ) THEN   ! 2nd order flux next to west
                                           ! boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                FX(i,j) = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
              
              FX(i,j)=FX(i,j)*umask(i,j)

                                                             !
            ELSE IF ( i.eq.imin-1 .and. imax.ge.imin ) THEN  ! 3rd of 2nd order flux
                                                             ! from west boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
                FX(i,j)=flx3
#  ifdef MASKING
                flx2 = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                
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
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
              
              FX(i,j)=FX(i,j)*umask(i,j)
              

                                          !
            ELSE IF ( i.eq.imax+1 ) THEN  ! 3rd or 2nd order flux from
                                          ! east boundary
              DO j = jstr,jend
                vel = Flxu(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
                FX(i,j)=flx3
#  ifdef MASKING
                flx2 = vel*flux2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                
                mask1=umask(i-2,j)*umask(i+1,j)
                
                FX(i,j)=mask1*flx3+(1-mask1)*flx2
                
                FX(i,j)=FX(i,j)*umask(i,j)

#  else
                FX(i,j)=flx3
#  endif /* MASKING */
              ENDDO
            ENDIF
          ENDDO ! i_loop_x_flux_5
          
   
!-------------------------------------------------------------------!
!!!!!!!!!!!!! COMPUTATION OF FE (loop on j indices) !!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------!

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
                
#  ifdef MASKING
                flx3 = vel*flux3_weno(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
                flx2 = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)

                 mask1=vmask(i,j-2)*vmask(i,j+1)
                 mask2=vmask(i,j-3)*vmask(i,j+2)
                 mask0=mask1*mask2
                 
             
                 FE(i,j)=mask0*flx5+(1-mask0)*mask1*flx3+(1-mask0)*(1-mask1)*flx2
                 
                 FE(i,j)=FE(i,j)*vmask(i,j)
                 
#  else
                FE(i,j)=flx5
#  endif /* MASKING */
                
              ENDDO
                                           !
            ELSE IF ( j.eq.jmin-2 ) THEN   ! 2nd order flux next to south
                                           ! boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                FE(i,j) = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
              
              FE(i,j)=FE(i,j)*vmask(i,j)
                                                             !
            ELSE IF ( j.eq.jmin-1 .and. jmax.ge.jmin ) THEN  ! 3rd of 2nd order flux 2 in
                                                             ! from south boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
                FE(i,j)=flx3
                
#  ifdef MASKING
                flx2 = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                
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
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
              
              FE(i,j)=FE(i,j)*vmask(i,j)

                                          !
            ELSE IF ( j.eq.jmax+1 ) THEN  ! 3rd or 2nd order flux 2 in from
                                          ! north boundary
              DO i = istr,iend
                vel = Flxv(i,j,k)
                flx3 = vel*flux3_weno(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
                FE(i,j)=flx3
                
#  ifdef MASKING
                flx2 = vel*flux2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                
                mask1=vmask(i,j-2)*vmask(i,j+1)
                
                FE(i,j)=mask1*flx3+(1-mask1)*flx2
                
                FE(i,j)=FE(i,j)*vmask(i,j)
                
#  else
                FE(i,j)=flx3
#  endif /* MASKING */
                
              ENDDO
            ENDIF
          ENDDO ! j_loop_y_flux_5

