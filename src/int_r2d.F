#define GRID_LEVEL 1
#define MAX_GRID_LEVEL 2
#include "cppdefs.h"
#if GRID_LEVEL < MAX_GRID_LEVEL

      subroutine int_r2d_west (Istr,Iend,Jstr,Jend, q,buff)
      implicit none
# include "param.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, jmin,jmax, i,j,jc
      real q(GLOBAL_2D_ARRAY)
      real buff(PRIVATE_2D_SCRATCH_ARRAY)
      real qC,qxR,qxL, qyR,qyL, qxyRR,qxyLR,qxyRL,qxyLL,
     &     cff, OneSixth, OneFourth, OneThird, TwoThird
      parameter (OneSixth=1./6., OneFourth=0.25,
     &           OneThird=1./3., TwoThird=2./3.)

      jmin=jmin_child+(Jstr-1)/3
      jmax=jmin_child+(Jend-1)/3
      i=imin_child-1

      do j=jmin,jmax
        if (rmask(i,j).gt.0.) then
          qC=q(i,j)
        else
          cff=rmask(i+1,j)+rmask(i+1,j)+rmask(i,j+1)+rmask(i,j-1)
     &                     +0.7071*( rmask(i+1,j+1)+rmask(i-1,j+1)
     &                             +rmask(i+1,j-1)+rmask(i-1,j-1))
          if (cff.gt.0.) then
            qC=(  rmask(i+1,j)*q(i+1,j) +rmask(i+1,j)*q(i+1,j)
     &           +rmask(i,j+1)*q(i,j+1) +rmask(i,j-1)*q(i,j-1)
     &     +0.7071*(
     &        rmask(i+1,j+1)*q(i+1,j+1) +rmask(i-1,j+1)*q(i-1,j+1)
     &       +rmask(i+1,j-1)*q(i+1,j-1) +rmask(i-1,j-1)*q(i-1,j-1)
     &                                                     ))/cff
          else
            qC=0.
          endif
        endif

        qxR=(q(i+1,j)-qC)*umask(i+1,j)
        qxL=(qC-q(i-1,j))*umask(i  ,j)
        qyR=(q(i,j+1)-qC)*vmask(i,j+1)
        qyL=(qC-q(i,j-1))*vmask(i,j  )
        qxyRR=(q(i+1,j+1)-q(i,j+1))*umask(i+1,j+1)-qxR
     &       +(q(i+1,j+1)-q(i+1,j))*vmask(i+1,j+1)-qyR
        qxyLR=(q(i,j+1)-q(i-1,j+1))*umask(i,j+1)-qxL
     &       +qyR-(q(i-1,j+1)-q(i-1,j))*vmask(i-1,j+1)
        qxyRL=qxR-(q(i+1,j-1)-q(i,j-1))*umask(i+1,j-1)
     &       +(q(i+1,j)-q(i+1,j-1))*vmask(i+1,j)-qyL
        qxyLL=qxL-(q(i,j-1)-q(i-1,j-1))*umask(i,j-1)
     &       +qyL-(q(i-1,j)-q(i-1,j-1))*vmask(i-1,j)

        jc=2+3*(j-jmin_child)
        buff(Istr-2,jc  )=qC
        buff(Istr-2,jc+1)=qC +OneSixth*(qyR+qyL+OneThird*(qyR-qyL))
        buff(Istr-2,jc-1)=qC -OneSixth*(qyR+qyL-OneThird*(qyR-qyL))

        buff(Istr-1,jc  )=qC +OneSixth*(qxR+qxL+OneThird*(qxR-qxL))
        buff(Istr-1,jc+1)=qC +OneSixth*( qxR+qxL +qyR+qyL
     &                        +OneThird*( qxR-qxL +qyR-qyL
     &                        +OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          +TwoThird*( qxyRR     -qxyLL
     &                           +OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
        buff(Istr-1,jc-1)=qC +OneSixth*( qxR+qxL -qyR-qyL
     &                        +OneThird*( qxR-qxL +qyR-qyL
     &                        -OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          +TwoThird*(      -qxyLR+qxyRL
     &                           -OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
      enddo
      return
      end



      subroutine int_r2d_east (Istr,Iend,Jstr,Jend, q,buff)
      implicit none
# include "param.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, jmin,jmax, i,j,jc
      real q(GLOBAL_2D_ARRAY)
      real buff(PRIVATE_2D_SCRATCH_ARRAY)
      real qC,qxR,qxL, qyR,qyL, qxyRR,qxyLR,qxyRL,qxyLL,
     &     cff, OneSixth, OneFourth, OneThird, TwoThird
      parameter (OneSixth=1./6., OneFourth=0.25,
     &           OneThird=1./3., TwoThird=2./3.)

      jmin=jmin_child+(Jstr-1)/3
      jmax=jmin_child+(Jend-1)/3
      i=imax_child

      do j=jmin,jmax
        if (rmask(i,j).gt.0.) then
          qC=q(i,j)
        else
          cff=rmask(i+1,j)+rmask(i+1,j)+rmask(i,j+1)+rmask(i,j-1)
     &                     +0.7071*( rmask(i+1,j+1)+rmask(i-1,j+1)
     &                             +rmask(i+1,j-1)+rmask(i-1,j-1))
          if (cff.gt.0.) then
            qC=(  rmask(i+1,j)*q(i+1,j) +rmask(i+1,j)*q(i+1,j)
     &           +rmask(i,j+1)*q(i,j+1) +rmask(i,j-1)*q(i,j-1)
     &     +0.7071*(
     &        rmask(i+1,j+1)*q(i+1,j+1) +rmask(i-1,j+1)*q(i-1,j+1)
     &       +rmask(i+1,j-1)*q(i+1,j-1) +rmask(i-1,j-1)*q(i-1,j-1)
     &                                                     ))/cff
          else
            qC=0.
          endif
        endif

        qxR=(q(i+1,j)-qC)*umask(i+1,j)
        qxL=(qC-q(i-1,j))*umask(i  ,j)
        qyR=(q(i,j+1)-qC)*vmask(i,j+1)
        qyL=(qC-q(i,j-1))*vmask(i,j  )
        qxyRR=(q(i+1,j+1)-q(i,j+1))*umask(i+1,j+1)-qxR
     &       +(q(i+1,j+1)-q(i+1,j))*vmask(i+1,j+1)-qyR
        qxyLR=(q(i,j+1)-q(i-1,j+1))*umask(i,j+1)-qxL
     &       +qyR-(q(i-1,j+1)-q(i-1,j))*vmask(i-1,j+1)
        qxyRL=qxR-(q(i+1,j-1)-q(i,j-1))*umask(i+1,j-1)
     &       +(q(i+1,j)-q(i+1,j-1))*vmask(i+1,j)-qyL
        qxyLL=qxL-(q(i,j-1)-q(i-1,j-1))*umask(i,j-1)
     &       +qyL-(q(i-1,j)-q(i-1,j-1))*vmask(i-1,j)

        jc=2+3*(j-jmin_child)
        buff(Iend+2,jc  )=qC
        buff(Iend+2,jc+1)=qC +OneSixth*(qyR+qyL+OneThird*(qyR-qyL))
        buff(Iend+2,jc-1)=qC -OneSixth*(qyR+qyL-OneThird*(qyR-qyL))

        buff(Iend+1,jc  )=qC -OneSixth*(qxR+qxL-OneThird*(qxR-qxL))
        buff(Iend+1,jc+1)=qC +OneSixth*( -qxR-qxL +qyR+qyL
     &                        +OneThird*( qxR-qxL  +qyR-qyL
     &                        -OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          +TwoThird*(      qxyLR-qxyRL
     &                           -OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
        buff(Iend+1,jc-1)=qC -OneSixth*( qxR+qxL +qyR+qyL
     &                        -OneThird*( qxR-qxL +qyR-qyL
     &                        +OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          -TwoThird*( qxyRR-qxyLL
     &                           -OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
      enddo
      return
      end



      subroutine int_r2d_south (Istr,Iend,Jstr,Jend, q,buff)
      implicit none
# include "param.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, imin,imax, i,j, ic
      real q(GLOBAL_2D_ARRAY)
      real buff(PRIVATE_2D_SCRATCH_ARRAY)
      real qC,qxR,qxL, qyR,qyL, qxyRR,qxyLR,qxyRL,qxyLL,
     &     cff, OneSixth, OneFourth, OneThird, TwoThird
      parameter (OneSixth=1./6., OneFourth=0.25,
     &           OneThird=1./3., TwoThird=2./3.)

      imin=imin_child+(Jstr-1)/3
      imax=imin_child+(Jend-1)/3
      j=jmin_child-1

      do i=imin,imax
        if (rmask(i,j).gt.0.) then
          qC=q(i,j)
        else
          cff=rmask(i+1,j)+rmask(i+1,j)+rmask(i,j+1)+rmask(i,j-1)
     &                     +0.7071*( rmask(i+1,j+1)+rmask(i-1,j+1)
     &                             +rmask(i+1,j-1)+rmask(i-1,j-1))
          if (cff.gt.0.) then
            qC=(  rmask(i+1,j)*q(i+1,j) +rmask(i+1,j)*q(i+1,j)
     &           +rmask(i,j+1)*q(i,j+1) +rmask(i,j-1)*q(i,j-1)
     &     +0.7071*(
     &        rmask(i+1,j+1)*q(i+1,j+1) +rmask(i-1,j+1)*q(i-1,j+1)
     &       +rmask(i+1,j-1)*q(i+1,j-1) +rmask(i-1,j-1)*q(i-1,j-1)
     &                                                     ))/cff
          else
            qC=0.
          endif
        endif

        qxR=(q(i+1,j)-qC)*umask(i+1,j)
        qxL=(qC-q(i-1,j))*umask(i  ,j)
        qyR=(q(i,j+1)-qC)*vmask(i,j+1)
        qyL=(qC-q(i,j-1))*vmask(i,j  )
        qxyRR=(q(i+1,j+1)-q(i,j+1))*umask(i+1,j+1)-qxR
     &       +(q(i+1,j+1)-q(i+1,j))*vmask(i+1,j+1)-qyR
        qxyLR=(q(i,j+1)-q(i-1,j+1))*umask(i,j+1)-qxL
     &       +qyR-(q(i-1,j+1)-q(i-1,j))*vmask(i-1,j+1)
        qxyRL=qxR-(q(i+1,j-1)-q(i,j-1))*umask(i+1,j-1)
     &       +(q(i+1,j)-q(i+1,j-1))*vmask(i+1,j)-qyL
        qxyLL=qxL-(q(i,j-1)-q(i-1,j-1))*umask(i,j-1)
     &       +qyL-(q(i-1,j)-q(i-1,j-1))*vmask(i-1,j)

        ic=2+3*(i-imin_child)
        buff(ic  ,Jstr-2)=qC
        buff(ic+1,Jstr-2)=qC +OneSixth*(qxR+qxL+OneThird*(qxR-qxL))
        buff(ic-1,Jstr-2)=qC -OneSixth*(qxR+qxL-OneThird*(qxR-qxL))

        buff(ic  ,Jstr-1)=qC +OneSixth*(qyR+qyL+OneThird*(qyR-qyL))
        buff(ic+1,Jstr-1)=qC +OneSixth*( qxR+qxL +qyR+qyL
     &                        +OneThird*( qxR-qxL +qyR-qyL
     &                        +OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          +TwoThird*( qxyRR     -qxyLL
     &                           +OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
        buff(ic-1,Jstr-1)=qC +OneSixth*( -qxR-qxL +qyR+qyL
     &                        +OneThird*( qxR-qxL  +qyR-qyL
     &                        -OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          +TwoThird*(      qxyLR-qxyRL
     &                           -OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
      enddo
      return
      end



      subroutine int_r2d_north (Istr,Iend,Jstr,Jend, q,buff)
      implicit none
# include "param.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, imin,imax, i,j, ic
      real q(GLOBAL_2D_ARRAY)
      real buff(PRIVATE_2D_SCRATCH_ARRAY)
      real qC,qxR,qxL, qyR,qyL, qxyRR,qxyLR,qxyRL,qxyLL,
     &     cff, OneSixth, OneFourth, OneThird, TwoThird
      parameter (OneSixth=1./6., OneFourth=0.25,
     &           OneThird=1./3., TwoThird=2./3.)

      imin=imin_child+(Jstr-1)/3
      imax=imin_child+(Jend-1)/3
      j=jmax_child

      do i=imin,imax
        if (rmask(i,j).gt.0.) then
          qC=q(i,j)
        else
          cff=rmask(i+1,j)+rmask(i+1,j)+rmask(i,j+1)+rmask(i,j-1)
     &                     +0.7071*( rmask(i+1,j+1)+rmask(i-1,j+1)
     &                             +rmask(i+1,j-1)+rmask(i-1,j-1))
          if (cff.gt.0.) then
            qC=(  rmask(i+1,j)*q(i+1,j) +rmask(i+1,j)*q(i+1,j)
     &           +rmask(i,j+1)*q(i,j+1) +rmask(i,j-1)*q(i,j-1)
     &     +0.7071*(
     &        rmask(i+1,j+1)*q(i+1,j+1) +rmask(i-1,j+1)*q(i-1,j+1)
     &       +rmask(i+1,j-1)*q(i+1,j-1) +rmask(i-1,j-1)*q(i-1,j-1)
     &                                                     ))/cff
          else
            qC=0.
          endif
        endif

        qxR=(q(i+1,j)-qC)*umask(i+1,j)
        qxL=(qC-q(i-1,j))*umask(i  ,j)
        qyR=(q(i,j+1)-qC)*vmask(i,j+1)
        qyL=(qC-q(i,j-1))*vmask(i,j  )
        qxyRR=(q(i+1,j+1)-q(i,j+1))*umask(i+1,j+1)-qxR
     &       +(q(i+1,j+1)-q(i+1,j))*vmask(i+1,j+1)-qyR
        qxyLR=(q(i,j+1)-q(i-1,j+1))*umask(i,j+1)-qxL
     &       +qyR-(q(i-1,j+1)-q(i-1,j))*vmask(i-1,j+1)
        qxyRL=qxR-(q(i+1,j-1)-q(i,j-1))*umask(i+1,j-1)
     &       +(q(i+1,j)-q(i+1,j-1))*vmask(i+1,j)-qyL
        qxyLL=qxL-(q(i,j-1)-q(i-1,j-1))*umask(i,j-1)
     &       +qyL-(q(i-1,j)-q(i-1,j-1))*vmask(i-1,j)

        ic=2+3*(i-imin_child)
        buff(ic  ,Jend+2)=qC
        buff(ic+1,Jend+2)=qC +OneSixth*(qxR+qxL+OneThird*(qxR-qxL))
        buff(ic-1,Jend+2)=qC -OneSixth*(qxR+qxL-OneThird*(qxR-qxL))

        buff(ic  ,Jend+1)=qC -OneSixth*(qyR+qyL-OneThird*(qyR-qyL))
        buff(ic+1,Jend+1)=qC +OneSixth*( qxR+qxL -qyR-qyL
     &                        +OneThird*( qxR-qxL +qyR-qyL
     &                        -OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          +TwoThird*(      -qxyLR+qxyRL
     &                           -OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
        buff(ic-1,Jend+1)=qC -OneSixth*( qxR+qxL +qyR+qyL
     &                        -OneThird*( qxR-qxL +qyR-qyL
     &                        +OneFourth*( qxyRR+qxyLR+qxyRL+qxyLL
     &                          -TwoThird*( qxyRR-qxyLL
     &                           -OneSixth*( qxyRR-qxyLR-qxyRL+qxyLL
     &                                                         )))))
      enddo
      return
      end
#endif




