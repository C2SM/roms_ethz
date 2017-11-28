#include "cppdefs.h"
 
#define SOLVE3D
#define STATIONS
#define LMD_KPP
 
#ifdef STATIONS
 
      subroutine sta_par
!
! Read in station positions: and associated switches from stations
! input file.
!
      implicit none
      integer iunit, icard, itrc, k, lstr, lenstr
      parameter (iunit=40)
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "mixing.h"
# include "ncvars.h"
#ifdef MPI
# include "mpif.h"
#endif
      lstr=lenstr(sposnam)
      open(iunit,file=sposnam(1:lstr),form='formatted',status='old')
      MPI_master_only write(stdout,'(/A/)') ' STATION parameters:'
   1  continue
       read(iunit,*,err=97) icard
        if (icard.eq.99) goto 99
!
!  Read in number of stations.
!
        if (icard.eq.1) then
          read(iunit,*,err=97) nstation
          if (NS.lt.nstation) then
            write(stdout,2) NS, nstation
   2        format(/,' INP_PAR - too small dimension parameter, NS: '
     &                   ,2i4,' change file  param.h and recompile.')
            may_day_flag=5
            return
          endif
          MPI_master_only write(stdout,3) nstation
   3      format(4x,i6,2x,'nstation    Number of stations to write',
     &                                    ' out into stations file.')
!
!  Writing switches for fields associated with momentum equations.
!
        elseif (icard.eq.2) then
          read(iunit,*,err=97) wsta(indxU), wsta(indxV), wsta(indxW),
     &          wsta(indxO), wsta(indxUb), wsta(indxVb), wsta(indxZ)
# ifdef SOLVE3D
          MPI_master_only write(stdout,4) wsta(indxU), wsta(indxV),
     &                                    wsta(indxW), wsta(indxO)
   4      format(9x,l1,2x,'wstaU       ',
     &                 'Write out 3D U-momentum component (T/F).',/,
     &           9x,l1,2x,'wstaV       ',
     &                 'Write out 3D V-momentum component (T/F).',/,
     &           9x,l1,2x,'wstaW       ',
     &                    'Write out W-momentum component (T/F).',/,
     &           9x,l1,2x,'wstaO       ',
     &                   'Write out omega vertical velocity (T/F).')
# endif
          MPI_master_only write(stdout,5) wsta(indxUb),
     &                             wsta(indxVb), wsta(indxZ)
   5      format(9x,l1,2x,'wstaUBAR    ',
     &                 'Write out 2D U-momentum component (T/F).',/,
     &           9x,l1,2x,'wstaVBAR    ',
     &                 'Write out 2D V-momentum component (T/F).',/,
     &           9x,l1,2x,'wstaZ       ',
     &                              'Write out free-surface (T/F).')
!
!  Writing switches for fields associated with tracers equations.
!
        elseif (icard.eq.3) then
          read(iunit,*,err=97) (wsta(itrc+indxT-1), itrc=1,NT)
 
# ifdef SOLVE3D
          do itrc=1,NT
            MPI_master_only write(stdout,6) wsta(itrc+indxT-1),
     &                  itrc,itrc
   6        format(9x,l1,2x,'wstaT(',i1,')    Write out tracer ',
     &                                              i1,' (T/F).')
          enddo
# endif
!
!  Writing switches for other fields.
!
        elseif (icard.eq.4) then
          read(iunit,*,err=97) wsta(indxR), wsta(indxAkv)
     &                                    , wsta(indxAkt)
#ifdef SALINITY
     &                                    , wsta(indxAks)
# ifdef LMD_KPP
     &                                    , wsta(indxHbl)
# endif
#endif
# ifdef SOLVE3D
          MPI_master_only write(stdout,7) wsta(indxR),
     &                       wsta(indxAkv), wsta(indxAkt)
#ifdef SALINITY
     &                                    , wsta(indxAks)
# ifdef LMD_KPP
     &                                    , wsta(indxHbl)
# endif
#endif
 
   7      format(9x,l1,2x,'wstaRHO     ',
     &          'Write out density anomaly (T/F).'
     &          /9x,l1,2x,'wstaAKV     ',
     &          'Write out vertical viscosity coefficient (T/F).'
     &          /9x,l1,2x,'wstaAKT     ',
     &          'Write out vertical T-diffusion coefficient (T/F).'
#ifdef SALINITY
     &          /,9x,l1,2x,'wstaAKS     ',
     &          'Write out vertical S-diffusion coefficient (T/F).'
# ifdef LMD_KPP
     &          /9x,l1,2x,'wstaHBL      ',
     &          'Write out depth of mixed layer (T/F).'
#   endif
#  endif
# endif
     &                  )
        elseif (icard.eq.5) then
          MPI_master_only write(stdout,*) ' '
          do k=1,nstation
            read(iunit,*,err=97) ispos(k), jspos(k)
            if (ispos(k).lt.2 .or. ispos(k).gt.Lm) then
              MPI_master_only write(stdout,9) ' ISPOS = ', ispos(k)
              may_day_flag=5
              return
            endif
            if (jspos(k).lt.2 .or. jspos(k).gt.Mm) then
              MPI_master_only write(stdout,9) ' JSPOS = ', jspos(k)
   9          format(/,' INP_PAR - out of range station index',
     &                                        ' position',a,i4)
              may_day_flag=5
              return
            endif
            MPI_master_only write(stdout,10) k, ispos(k), jspos(k)
  10        format(12x,'(I,J) for station ',i3.3,':',2i6)
          enddo
        endif
      goto 1
!
!  Error while reading input parameters.
!
  97  MPI_master_only write(stdout,98) icard, sposnam(1:lstr)
  98  format(/,' STA_PAR - error while reading input card: ',i2,
     &       /,12x,'from stations file: ',a)
      may_day_flag=5
  99  continue
      return
      end
#else
      subroutine sta_par_empty
      return
      end
#endif /* STATIONS */
 
 
