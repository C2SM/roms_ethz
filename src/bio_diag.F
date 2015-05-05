#include "cppdefs.h"
#if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
      subroutine bio_diag (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call bio_diag_tile  (istr,iend,jstr,jend)
      end

      subroutine bio_diag_tile (istr,iend,jstr,jend)
      implicit none
      integer istr,iend,jstr,jend, i,j,k,itrc, iocheck
# include "param.h"
      real cff1, NSUB
      real*QUAD cff, my_sum(0:2*NT+1)
      character ses(8)*1, echar*8
      pointer (i_s,ses)


# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "comm_vars.h"

# define  ETALON_CHECK
#undef ETALON_CHECK
# ifdef ETALON_CHECK
      integer ncheck, nparam
      parameter (ncheck=10, nparam=4)
      integer icheck, check_point(ncheck)
      character*57 check_line, etalon_line(ncheck)
      character*1 symbol(57)
      pointer (isym,symbol)
      real    A0(nparam), A1(nparam)
      integer P0(nparam), P1(nparam)

# include "etalon_data.h"
# endif

      if (mod(iic-1,ninfo)==0) then
        do itrc=0,NT
          my_sum(2*itrc  )=0.          ! <-- Reset local sums
          my_sum(2*itrc+1)=0.
        enddo
        do j=jstr,jend
         do i=istr,iend
# ifdef MASKING
           cff1=rmask(i,j)/(pm(i,j)*pn(i,j))  ! <-- grid box area
# else
           cff1=1./(pm(i,j)*pn(i,j))
# endif
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

! Perform global summation: whoever gets first to the critical region
! resets global sums before global summation starts; after the global
! summation is completed, thread, which is the last one to enter the
! critical region, finalizes the computation of diagnostics and
! prints them out.

        if (SINGLE_TILE_MODE) then
          NSUB=1
        else
          NSUB=NSUB_X*NSUB_E
        endif

C$OMP CRITICAL (bio_cr_rgn)
          if (bc_count==0) then
            do itrc=1,2*NT+1
              global_sum(itrc)=QuadZero   ! <-- Reset global sums
            enddo
          endif
                                       !-->  Perform global summation
          do itrc=1,2*NT+1
            global_sum(itrc)=global_sum(itrc)+my_sum(itrc)
          enddo

          bc_count=bc_count+1             ! This counter identifies
          if (bc_count==NSUB) then      ! the last thread, whoever
            bc_count=0                    ! it is, not always master.
# ifdef MPI
! Perform global summation among MPI processes:
            if (NNODES > 1) then
               size=NNODES
 10             step=(size+1)/2
               if (mynode >= step .and. mynode <= size) then
                  ! global_sum may be real*16, so the size of the message
                  ! in the following send is doubled:
                  call MPI_Send (global_sum, 4*NT+4, MPI_DOUBLE_PRECISION,
     &               mynode-step, 17, MPI_COMM_WORLD, ierr)
               elseif (mynode < size-step) then
                  call MPI_Recv (buff, 4*NT+4 , MPI_DOUBLE_PRECISION,
     &               mynode+step, 17, MPI_COMM_WORLD, status, ierr)
                  do itrc=1,2*NT+1
                     global_sum(itrc)=global_sum(itrc)+buff(itrc)
                  enddo
               endif ! <-- mynode.ge.step .and. mynode.lt.size
               size=step
               if (size.gt.1) goto 10
            endif ! <-- NNODES.gt.1
            if (mynode == 0) then
# endif

            cff=1./global_sum(1)
            do itrc=1,NT
              global_sum(2*itrc  )=cff*global_sum(2*itrc )
              global_sum(2*itrc+1)=cff*global_sum(2*itrc+1)
     &                                -global_sum(2*itrc)**2
            enddo
            if (first_time==0) then
              first_time=1
              write(*,1) 'STEP','time[DAYS]','rms_PHYT+ZOO',
     &                  'rms____NO3','rms___NH4','rms__DETRIT','trd'
   1          format(1x,A4,3x,A10,1x,A12,4x,A10,4x,A10,4x,A10,3x,A3)
            endif

            do itrc=1,NT
              global_sum(2*itrc+1)=global_sum(2*itrc+1)**0.5
            enddo

            cff=global_sum(13) + global_sum(15)

            write(*,2) iic-1,tdays, cff, global_sum(7),
     &                    global_sum(9), global_sum(11), proc(2)
   2        format(I6, F12.5, ES16.9, 3(ES14.7), I3)

# ifdef ETALON_CHECK
            do icheck=1,ncheck
              if (iic-1==check_point(icheck)) then
                write(check_line,3,iostat=iocheck) cff,global_sum(7),
     &                                  global_sum( 9),global_sum(11)
   3            format(1PE15.9,3(1pe14.7))
                if (check_line == etalon_line(icheck)) then
                  write(*,*) 'PASSED_ETALON_CHECK'
                else
                  read(check_line         ,4,iostat=iocheck) (A1(i),
     &                                             P1(i), i=1,nparam)
                  read(etalon_line(icheck),4,iostat=iocheck) (A0(i),
     &                                             P0(i), i=1,nparam)
   4              format(f11.9,1x,i3,3(f10.7,1x,i3))
                  do i=1,nparam
                    A1(i)=A1(i)-A0(i)*10.**float(P0(i)-P1(i))
                  enddo
                  write(check_line,5,iostat=iocheck) (A1(i)
     &                                            ,i=1,nparam)
   5              format(f14.9,3(1x,f13.7))
                  isym=loc(check_line)
                  j=0
                  do i=2,56
                    if (symbol(i)=='.'.and.symbol(i-1)=='0') then
                      symbol(i)=':'
                      j=1
                    else if (j==1 .and. symbol(i)=='0') then
                      symbol(i)='.'
                    else
                      j=0
                    endif
                  enddo
                  write(*,6) ' difference:    ',check_line
   6              format(A16,A56)
                  write(*,*) 'ETALON_CHECK: DIFFERENCE DISCOVERED'
                endif
              endif
            enddo
# endif
#ifdef MPI
          endif    ! <-- mynode==0
#endif
          endif  ! <-- bc_count==NSUB
C$OMP END CRITICAL (bio_cr_rgn)
      endif  ! <-- mod(iic-1,ninfo)==0
      end
#else
! the wrapper routine for calc_global_srf_sum_tile
      subroutine calc_global_srf_sum(tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call calc_global_srf_sum_tile(Istr,Iend,Jstr,Jend)
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
         my_srf_sum(itrc) = QuadZero
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
         enddo
      enddo

! Summation over OpenMP threads:
C$OMP CRITICAL (bio_diag_rgn)
      do itrc = 0, NT
         global_srf_sum(itrc) = global_srf_sum(itrc) + 
     &             my_srf_sum(itrc)
      enddo
C$OMP END CRITICAL (bio_diag_rgn)

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
#ifdef MPI
# include "mpif.h"
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(0:2*NT+1)
#endif

#ifdef MPI
!DL: at this point, each MPI process has an array global_srf_sum with
! values for its own tile only. Now we sum over the MPI domains:
      if (NNODES.gt.1) then
         size=NNODES
 33       step=(size+1)/2
         if (mynode.ge.step .and. mynode.lt.size) then
            ! global_srf_sum may be real*16, so the size of the message
            ! in the following send is doubled:
            call MPI_Send (global_srf_sum, 2*NT+2, MPI_DOUBLE_PRECISION,
     &           mynode-step, 18, MPI_COMM_WORLD, ierr)
         elseif (mynode .lt. size-step) then
            call MPI_Recv (buff, 2*NT+2 , MPI_DOUBLE_PRECISION,
     &           mynode+step, 18, MPI_COMM_WORLD, status, ierr)
            do itrc=0,NT
               global_srf_sum(itrc)=global_srf_sum(itrc)+buff(itrc)
            enddo
         end if   ! <-- mynode.ge.step .and. mynode.lt.size
         size=step
         if (size.gt.1) goto 33

         call MPI_Barrier (ocean_grid_comm, ierr)
         call MPI_Bcast(global_srf_sum, 2*NT+2, MPI_DOUBLE_PRECISION,
     &               0, ocean_grid_comm, ierr)
      end if   ! <-- NNODES.gt.1
#endif

      ! global_srf_sum(0) is now the volume of the whole model domain:
      factor = 1.0 / global_srf_sum(0)
      do itrc = 1, NT
         global_srf_sum(itrc) = global_srf_sum(itrc) * factor
      enddo

      return
      end
#endif /* SALINITY && VFLX_CORR */
 
#else /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
      subroutine bio_diag_empty
      end
#endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
