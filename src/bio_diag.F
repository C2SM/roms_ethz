#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
      subroutine bio_diag (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call bio_diag_tile  (Istr,Iend,Jstr,Jend)
      return
      end
 
      subroutine bio_diag_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k,itrc, iocheck
#include "param.h"
      real cff1, NSUB
      real*QUAD cff, my_sum(0:2*NT+1)
      character ses(8)*1, echar*8
      pointer (i_s,ses)
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#ifdef MPI
# include "mpif.h"
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(0:2*NT+1)
      common /xyz/ buff
#endif
#include "comm_vars.h"
 
!#define  ETALON_CHECK
#undef ETALON_CHECK
#ifdef ETALON_CHECK
      integer ncheck, nparam
      parameter (ncheck=10, nparam=4)
      integer icheck, check_point(ncheck)
      character*57 check_line, etalon_line(ncheck)
      character*1 symbol(57)
      pointer (isym,symbol)
      real    A0(nparam), A1(nparam)
      integer P0(nparam), P1(nparam)

#include "etalon_data.h"
#endif
 
      if (mod(iic-1,ninfo).eq.0) then
        do itrc=0,NT
          my_sum(2*itrc  )=0.          ! <-- Reset local sums
          my_sum(2*itrc+1)=0.
        enddo
        do j=Jstr,Jend
         do i=Istr,Iend
#ifdef MASKING
           !cff1=ds*rmask(i,j)/(pm(i,j)*pn(i,j))  ! <-- grid box area
           cff1=rmask(i,j)/(pm(i,j)*pn(i,j))  ! <-- grid box area
#else
           !cff1=ds/(pm(i,j)*pn(i,j))
           cff1=1./(pm(i,j)*pn(i,j))
#endif
           do k=1,N
            cff=cff1*Hz(i,j,k)       ! <-- volume of grid box(i,j,k)
            my_sum(1)=my_sum(1)+cff  ! <-- accumulate volume
             do itrc=1,NT
              my_sum(2*itrc  )=my_sum(2*itrc  )+cff*t(i,j,k,nstp,itrc)
              my_sum(2*itrc+1)=my_sum(2*itrc+1)+cff*t(i,j,k,nstp,itrc)
     &                                             *t(i,j,k,nstp,itrc)
             enddo
           enddo
         enddo
        enddo
!
! Perform global summation: whoever gets first to the critical region
! resets global sums before global summation starts; after the global
! summation is completed, thread, which is the last one to enter the
! critical region, finalizes the computation of diagnostics and
! prints them out.
!
        if (SINGLE_TILE_MODE) then
          NSUB=1
        else
          NSUB=NSUB_X*NSUB_E
        endif
 
C$OMP CRITICAL (bio_cr_rgn)
          if (bc_count.eq.0) then
            do itrc=1,2*NT+1
              global_sum(itrc)=QuadZero   ! <-- Reset global sums
            enddo
          endif
                                       !-->  Perform global summation
          do itrc=1,2*NT+1
            global_sum(itrc)=global_sum(itrc)+my_sum(itrc)
          enddo
 
          bc_count=bc_count+1             ! This counter identifies
          if (bc_count.eq.NSUB) then      ! the last thread, whoever
            bc_count=0                    ! it is, not always master.
#ifdef MPI
! Perform global summation among MPI processes:
            if (NNODES.gt.1) then
               size=NNODES
 3             step=(size+1)/2
               if (mynode.ge.step .and. mynode.lt.size) then
                  ! global_sum may be real*16, so the size of the message
                  ! in the following send is doubled:
                  call MPI_Send (global_sum, 4*NT+4, MPI_DOUBLE_PRECISION,
     &               mynode-step, 17, MPI_COMM_WORLD, ierr)
               elseif (mynode .lt. size-step) then
                  call MPI_Recv (buff, 4*NT+4 , MPI_DOUBLE_PRECISION,
     &               mynode+step, 17, MPI_COMM_WORLD, status, ierr)
                  do itrc=1,2*NT+1
                     global_sum(itrc)=global_sum(itrc)+buff(itrc)
                  enddo
               end if ! <-- mynode.ge.step .and. mynode.lt.size
               size=step
               if (size.gt.1) goto 3
            end if ! <-- NNODES.gt.1
            if (mynode.eq.0) then
#endif
 
            cff=1./global_sum(1)
            do itrc=1,NT
              global_sum(2*itrc  )=cff*global_sum(2*itrc )
              global_sum(2*itrc+1)=cff*global_sum(2*itrc+1)
     &                                -global_sum(2*itrc)**2
            enddo
            if (first_time.eq.0) then
              first_time=1
              write(stdout,1) 'STEP','time[DAYS]','rms_PHYT+ZOO',
     &                  'rms____NO3','rms___NH4','rms__DETRIT','trd'
   1          format(1x,A4,3x,A10,1x,A12,4x,A10,4x,A10,4x,A10,3x,A3)
            endif
 
            do itrc=1,NT
              global_sum(2*itrc+1)=global_sum(2*itrc+1)**0.5
            enddo
 
            cff=global_sum(13) + global_sum(15)

c JDS These lines were commented in order to increase  simulations
c   speed            
c            write(stdout,2) iic-1,tdays, cff, global_sum(7),
c     &                    global_sum(9), global_sum(11), proc(2)
c   2        format(I6, F12.5, 1PE16.9, 3(1PE14.7), I3)
 
#ifdef  ETALON_CHECK
            do icheck=1,ncheck
              if (iic-1.eq.check_point(icheck)) then
                write(check_line,10,iostat=iocheck) cff,global_sum(7),
     &                                  global_sum( 9),global_sum(11)
  10            format(1PE15.9,3(1pe14.7))
                if (check_line .eq. etalon_line(icheck)) then
                  write(stdout,*) 'PASSED_ETALON_CHECK'
                else
                  read(check_line         ,11,iostat=iocheck) (A1(i),
     &                                             P1(i), i=1,nparam)
                  read(etalon_line(icheck),11,iostat=iocheck) (A0(i),
     &                                             P0(i), i=1,nparam)
  11              format(f11.9,1x,i3,3(f10.7,1x,i3))
                  do i=1,nparam
                    A1(i)=A1(i)-A0(i)*10.**float(P0(i)-P1(i))
                  enddo
                  write(check_line        ,12,iostat=iocheck) (A1(i)
     &                                                  ,i=1,nparam)
  12              format(f14.9,3(1x,f13.7))
                  isym=loc(check_line)
                  j=0
                  do i=2,56
                    if (symbol(i).eq.'.'.and.symbol(i-1).eq.'0') then
                      symbol(i)=':'
                      j=1
                    else if (j.eq.1 .and. symbol(i).eq.'0') then
                      symbol(i)='.'
                    else
                      j=0
                    endif
                  enddo
                  write(stdout,15) ' difference:    ',check_line
  15              format(A16,A56)
                  write(stdout,*) 'ETALON_CHECK: SOMETHING_IS_WRONG'
                endif
              endif
            enddo
#endif /* ETALON_CHECK */
#ifdef MPI
          endif    ! <-- mynode.eq.0
#endif
          endif  ! <-- bc_count.eq.NSUB
C$OMP END CRITICAL (bio_cr_rgn)
      endif  ! <-- mod(iic-1,ninfo).eq.0
      return
      end


#if defined SALINITY && defined VFLX_CORR
! resets all global surface sums (volume and tracer concentrations) to 0
      subroutine reset_global_srf_sum()

!      USE param
!      USE scalars

      implicit none
#include "param.h"
#include "scalars.h"
#include "comm_vars.h"   
      integer itrc
      do itrc = 0, NT
         global_srf_sum(itrc) = 0.Q0
      enddo

      return
      end

! the wrapper routine for calc_global_srf_sum_tile
      subroutine calc_global_srf_sum(tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call calc_global_srf_sum_tile(Istr,Iend,Jstr,Jend)

      return 
      end

! calculates the global surface volume and concentrations for one tile
      subroutine calc_global_srf_sum_tile(Istr,Iend,Jstr,Jend)
      
!      USE param
!      USE scalars
!      USE grid
!      USE ocean3d

      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#include "comm_vars.h"
      integer i,j,Istr,Iend,Jstr,Jend,itrc,NSUB
      real cff1
      real*QUAD my_srf_sum(0:NT)

      do itrc = 0, NT
         my_srf_sum(itrc) = 0.Q0
      enddo
      do j=Jstr,Jend
         do i=Istr,Iend
#ifdef MASKING
           cff1=rmask(i,j)/(pm(i,j)*pn(i,j))  ! <-- grid box area
#else 
           cff1=1./(pm(i,j)*pn(i,j))
#endif
           my_srf_sum(0)=my_srf_sum(0) + cff1*Hz(i,j,N) ! accumulate volume
           do itrc=1,NT
              my_srf_sum(itrc) = my_srf_sum(itrc) +
     &             cff1 * Hz(i,j,N) * t(i,j,N,nstp,itrc) 
           enddo

           if (SINGLE_TILE_MODE) then
              NSUB=1
           else
              NSUB=NSUB_X*NSUB_E
           endif
           
C$OMP CRITICAL (bio_diag_rgn)
           do itrc = 0, NT
              global_srf_sum(itrc) = global_srf_sum(itrc) + 
     &             my_srf_sum(itrc)
           enddo
C$OMP END CRITICAL (bio_diag_rgn)
        enddo
      enddo

      return 
      end

! computes actual concentrations by dividing the total masses by volume
      subroutine scale_global_srf_sum()

!      USE param
!      USE scalars

      implicit none
#include "param.h"
#include "scalars.h"
#include "comm_vars.h"
      integer itrc
      real*QUAD factor
      if (global_srf_sum(0) .gt. 0) then
         factor = 1.Q0 / global_srf_sum(0)
      else
         ! global_srf_sum(0) may be zero (if the whole MPI domain happens
         ! to be on the land)
         factor = 0.0
      end if
      do itrc = 1, NT
         global_srf_sum(itrc) = global_srf_sum(itrc) * factor
      enddo

      return
      end
#endif /* SALINITY && VFLX_CORR */
 
#else /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
      subroutine bio_diag_empty()
      return
      end
#endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
