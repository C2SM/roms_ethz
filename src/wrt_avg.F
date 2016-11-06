#include "cppdefs.h"
!  We are playing the same redefinition preprocessor game as in def_his.F here...
#ifdef AVERAGES
# ifndef SLICE_AVR
#  define START_3D_ARRAY_K0 START_2D_ARRAY,0
#  define START_3D_ARRAY_K1 START_2D_ARRAY,1
#  define AVR avg 
#  define CAVR 'avg'
! By defining AVR as avg we make use of the fact that
! compiler will ignore spaced e.g., wrt_ avg -> wrt_avg
! This makes that *.f a bit less readable but saves a lot of #define
# else
#  define START_3D_ARRAY_K0 START_2D_ARRAY
#  define START_3D_ARRAY_K1 START_2D_ARRAY
#  define AVR slavg
#  define CAVR 'slavg'
#  define R3DVAR r2dvar
#  define U3DVAR u2dvar
#  define V3DVAR v2dvar
#  define W3DVAR r2dvar
# endif /* ifndef SLICE_AVG */

      subroutine wrt_ AVR              ! Write time-averaged fields
                                     ! into averages netCDF file.
      implicit none
      integer record, i, ierr, lstr, lvar, lenstr, ncdf_write
# ifdef SOLVE3D
     &             , itrc
# endif
      character(len=18) tstring
# include "param.h"
# include "scalars.h"
# include "averages.h"
# include "ncvars.h"
# include "netcdf.inc"
# ifdef SEDIMENT_BIOLOGY
#  include "bgcflux.h"
# endif
# if defined WRITE_TEMP_REST || defined WRITE_SALT_REST || \
        defined WRITE_HEATFLX || defined WRITE_CO2FLX
#  include "forces.h"
# endif
# ifdef BIOLOGY_BEC
#  include "ecosys_bec.h"
#  include "bec_avg.h"
# endif
# if defined BIOLOGY_BEC2 && defined BEC2_DIAG
#  include "ecosys_bec2.h"
!!# include "bec2_avg.h"
# endif
!DL: for gas exchange fluxes:
# if defined BIOLOGY_NPZDOC
#  include "gasexc.h"
# endif
# if defined MPI && !defined PARALLEL_FILES
#  include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode > 0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,  3,
     &                       ocean_grid_comm, status, ierr)
      endif
# endif
CR      mpi_master_only print *, '--- NOTE: Executing ', wrt_ AVR

! Create/open averages file; write grid arrays, if so needed,
! check for errors, then set record within the file "record".

      call def_ AVR  (nrec AVR , ierr)
      lstr=lenstr( AVR name)
      if (ierr /= nf_noerr) goto 99         !!! WARNING: Here it is
      nrec AVR =max(nrec AVR ,1)            !!! assumed that global
      if (nrpf AVR ==0) then                !!! averages record index
        record=nrec AVR                     !!! "nrec AVR " is already
      else                                  !!! advanced by main.
        record=1+mod(nrec AVR -1, nrpf AVR )
      endif

! Write out time-averaged variables:
! ----- --- ------------- ----------
# if 0  /* Only needed if averages would be used as restart */
! Time step and record indices.

      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrec AVR 
 
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nc AVR ,  AVR Tstep, start, count, ibuff)
      if (ierr /= nf_noerr) then
        write(*,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
# endif
!
! Averaged time

      ierr=nf_put_var1_FTYPE (nc AVR ,  AVR Time, record, time_ AVR )
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(*,1) vname(1,indxTime)(1:lvar), record MYID
      endif

! Barotropic mode variables: free-surface and 2D momentum components
! in XI-,ETA-directions.

      if (wrt AVR (indxZ)) then
        ierr=ncdf_write(nc AVR, AVR Z, record,  r_var,
     &                  zeta_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(*,1) vname(1,indxTime)(1:lvar), record MYID
        endif
      endif

      if (wrt AVR (indxUb) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR Ub, record, u_var,
     &                  ubar_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(*,1) vname(1,indxUb)(1:lvar), record MYID
        endif
      endif

      if (wrt AVR (indxVb) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR Vb, record, v_var,
     &                  vbar_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(*,1) vname(1,indxVb)(1:lvar), record MYID
        endif
      endif
# ifdef SOLVE3D

! 3D momentum components in XI- and ETA-directions.

      if (wrt AVR (indxU) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR U, record, u_var,
     &                  u_ AVR (START_3D_ARRAY_K1), N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(*,1) vname(1,indxU)(1:lvar), record MYID
        endif
      endif

      if (wrt AVR (indxV) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR V, record, v_var,
     &                  v_ AVR (START_3D_ARRAY_K1), N)
        if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxV))
          write(*,1) vname(1,indxV)(1:lvar), record MYID
        endif
      endif

! Tracer variables.

      do itrc=1,NT
        if (wrt AVR (indxT+itrc-1) .and. ierr == nf_noerr) then
          ierr=ncdf_write(nc AVR ,  AVR T(itrc), record, r_var,
     &                    t_ AVR (START_3D_ARRAY_K1,itrc), N)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(*,1) vname(1,indxT+itrc-1)(1:lvar),record MYID
          endif
        endif
      enddo
# ifdef SEDIMENT_BIOLOGY

! Sediment concentrations

      do itrc=1,NT_sed
        if (wrt AVR (indxSedOrgN+itrc-1)) then
          ierr=ncdf_write(nc AVR ,  AVR T(itrc), record, r_var,
     &                    t_sed_ AVR (START_3D_ARRAY_K1,itrc), N)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            write(*,1) vname(1,indxSedOrgN+itrc-1)(1:lvar), record,
     &                      ierr, nf_strerror(ierr) MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_ AVR :  wrtT_sed  ' MYID
        endif
      enddo
# endif /* SEDIMENT_BIOLOGY */

! Density anomaly.

      if (wrt AVR (indxR) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR R, record, r_var,
     &                  rho_ AVR (START_3D_ARRAY_K1), N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(*,1) vname(1,indxR)(1:lvar), record  MYID
        endif
      endif

! S-coordinate omega vertical velocity (m/s).

      if (wrt AVR (indxO) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR O, record, r_var, w_ AVR , N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(*,1) vname(1,indxO)(1:lvar), record  MYID
        endif
      endif

! True vertical velocity (m/s).

      if (wrt AVR (indxW) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR W, record, r_var, wvl_ AVR , N)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(*,1) vname(1,indxW)(1:lvar), record MYID
        endif
      endif

! Vertical eddy viscosity coefficient for momentum.

      if (wrt AVR (indxAkv) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR Akv,  record,  r_var,
     &                  akv_ AVR (START_3D_ARRAY_K0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(*,1) vname(1,indxAkv)(1:lvar), record MYID
        endif
      endif

! Vertical heat conductivity

      if (wrt AVR (indxAkt) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR Akt,  record,  r_var,
     &                  akt_ AVR (START_3D_ARRAY_K0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(*,1) vname(1,indxAkt)(1:lvar), record MYID
        endif
      endif
#  ifdef SALINITY

! Vertical diffusion coefficient for salinity.

      if (wrt AVR (indxAks) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR Aks,  record,  r_var,
     &                  Aks_ AVR (START_3D_ARRAY_K0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(*,1) vname(1,indxAks)(1:lvar), record MYID
        endif
      endif
#  endif /* SALINITY */
#  ifdef LMD_KPP

! Depth of planetary boundary layer [m].

      if (wrt AVR (indxHbls) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR Hbls, record, r_var,
     &                  hbl_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbls))
          write(*,1) vname(1,indxHbls)(1:lvar), record MYID
        endif
      endif
#  endif
#  ifdef LMD_BKPP

! Thickness of KPP bottom boundary layer [m].

      if (wrt AVR (indxHbbl) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR Hbbl, record, r_var,
     &                  hbbl_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(*,1) vname(1,indxHbbl)(1:lvar), record MYID
        endif
      endif
# endif
#  ifdef KPP_DIAGNOSE
! KppRichm KppRicht KppSblSh KppSblSt KppSblRot KppSblEnt
      if (wrt AVR (indxKppRichm) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR kpprichm,  record,  r_var,
     &                  kpprichm_ AVR (START_2D_ARRAY,0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxKppRichm))
          write(*,1) vname(1,indxKppRichm)(1:lvar), record MYID
        endif
      endif
      if (wrt AVR (indxKppRicht) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR kppricht,  record,  r_var,
     &                  kppricht_ AVR (START_2D_ARRAY,0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxKppRicht))
          write(*,1) vname(1,indxKppRicht)(1:lvar), record MYID
        endif
      endif
      if (wrt AVR (indxKppSblSh) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR kppsblsh,  record,  r_var,
     &                  kppsblsh_ AVR (START_2D_ARRAY,0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxKppSblSh))
          write(*,1) vname(1,indxKppSblSh)(1:lvar), record MYID
        endif
      endif
      if (wrt AVR (indxKppSblSt) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR kppsblst,  record,  r_var,
     &                  kppsblst_ AVR (START_2D_ARRAY,0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxKppSblSt))
          write(*,1) vname(1,indxKppSblSt)(1:lvar), record MYID
        endif
      endif
      if (wrt AVR (indxKppSblRot) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR kppsblrot,  record,  r_var,
     &                  kppsblrot_ AVR (START_2D_ARRAY,0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxKppSblRot))
          write(*,1) vname(1,indxKppSblRot)(1:lvar), record MYID
        endif
      endif
      if (wrt AVR (indxKppSblEnt) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR kppsblent,  record,  r_var,
     &                  kppsblent_ AVR (START_2D_ARRAY,0), N+1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxKppSblEnt))
          write(*,1) vname(1,indxKppSblEnt)(1:lvar), record MYID
        endif
      endif
#  endif /* KPP_DIAGNOSE */
! AH16:
# if defined WRITE_CO2FLX

! Surface CO2 flux

      if (wrt AVR (indxCO2flx) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR CO2flx, record, r_var,
     &                  co2flx_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxCO2flx))
          write(*,1) vname(1,indxCO2flx)(1:lvar), record MYID
        endif
      endif
# endif
! AH16:
# if defined WRITE_HEATFLX

! Surface heat flux

      if (wrt AVR (indxHeatflx) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR Heatflx, record, r_var,
     &                  heatflx_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxHeatflx))
          write(*,1) vname(1,indxHeatflx)(1:lvar), record MYID
        endif
      endif
# endif
# if defined WRITE_TEMP_REST

! Restoring flux for temperature

      if (wrt AVR (indxRestflxTemp) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR RestflxTemp, record, r_var,
     &                  restflx_temp_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxRestflxTemp))
          write(*,1) vname(1,indxRestflxTemp)(1:lvar), record MYID
        endif
      endif
# endif
# if defined WRITE_SALT_REST

! Restoring flux for salinity

      if (wrt AVR (indxRestflxSalt) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,  AVR RestflxSalt, record, r_var,
     &                  restflx_salt_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxRestflxSalt))
          write(*,1) vname(1,indxRestflxSalt)(1:lvar), record MYID
        endif
      endif
# endif
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC


! pH:

      if (wrt AVR (indxpH) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR PH,  record,  r_var,
     &                  pH_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxPH))
          write(*,1) vname(1,indxPH)(1:lvar), record MYID
        endif
      endif

! pCO2:

      if (wrt AVR (indxpCO2) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR pCO2,  record,  r_var,
     &                  pCO2_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxpCO2))
          write(*,1) vname(1,indxpCO2)(1:lvar), record MYID
        endif
      endif

! pCO2air:

      if (wrt AVR (indxpCO2air) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR pCO2air,  record,  r_var,
     &                  pCO2air_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxpCO2air))
          write(*,1) vname(1,indxpCO2air)(1:lvar), record MYID
        endif
      endif

! incoming PAR:

      if (wrt AVR (indxPARinc) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR PARinc,  record,  r_var,
     &                  PARinc_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxPARinc))
          write(*,1) vname(1,indxPARinc)(1:lvar), record MYID
        endif
      endif

#  ifndef NOPARWRT
! PAR:

      if (wrt AVR (indxPAR) .and. ierr == nf_noerr) then
        ierr=ncdf_write(nc AVR ,   AVR PAR,  record,  r_var,
     &                  PAR_ AVR (START_2D_ARRAY), 1)
        if (ierr /= nf_noerr) then
          lvar=lenstr(vname(1,indxPAR))
          write(*,1) vname(1,indxPAR)(1:lvar), record MYID
        endif
      endif
#  endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
# if defined BIOLOGY_BEC2 && defined BEC2_DIAG

! 2d diagnostics:

      do itrc=1,nr_bec2_diag_2d
!        if (wrt AVR T_bec2_diag(itrc)) .and. ierr == nf_noerr) then
          ierr=ncdf_write(nc AVR ,   AVR T_bec2_diag(itrc),  record,  r_var,
     &                  bec2_diag_2d_ AVR (START_2D_ARRAY), 1)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname_bec2_diag_2d(1,itrc))
            write(*,1) vname_bec2_diag_2d(1,itrc)(1:lvar), record MYID
          endif
!        endif
      enddo

! 3d diagnostics:

      do itrc=1,nr_bec2_diag_3d
!        if (wrt AVR T_bec2_diag(itrc)) .and. ierr == nf_noerr) then
          ierr=ncdf_write(nc AVR ,   AVR T_bec2_diag(nr_bec2_diag_2d+itrc),  record,  r_var,
     &                  bec2_diag_3d_ AVR (START_3D_ARRAY_K1), N)
          if (ierr /= nf_noerr) then
            lvar=lenstr(vname_bec2_diag_3d(1,itrc))
            write(*,1) vname_bec2_diag_3d(1,itrc)(1:lvar), record MYID
          endif
!        endif
      enddo
# endif /* BIOLOGY_BEC2 */

# if defined WRITE_DEPTHS && !defined SLICE_AVR

! depths of rho points:

      ierr=ncdf_write(nc AVR ,   AVR z_r,  record,  r_var,
     &                  z_r_ AVR (START_3D_ARRAY_K1), N)
      if (ierr /= nf_noerr) then
        lvar=lenstr(vname(1,indxz_r))
        write(*,1) vname(1,indxz_r)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

! depths at w points:

      ierr=ncdf_write(nc AVR, AVR z_w,  record,  r_var,
     &                  z_w_ AVR (START_3D_ARRAY_K0), N)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxz_w))
        write(*,1) vname(1,indxz_w)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif

! thicknesses of sigma layers:

      ierr=ncdf_write(nc AVR,   AVR Hz,  record,  r_var,
     &                  Hz_ AVR (START_3D_ARRAY_K1), N)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxHz))
        write(*,1) vname(1,indxHz)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                         !--> ERROR
      endif
# endif /* WRITE_DEPTHS */

! Gas exchange fluxes:

# if defined BIOLOGY_NPZDOC && defined OXYGEN
      do itrc = 1, NumGasExcTerms
        ierr=ncdf_write(nc AVR, AVR GasExcFlux(itrc), record, r_var,
     &                  Hz_ AVR(START_2D_ARRAY,itrc), 1)
         if (ierr /= nf_noerr) then
            lvar=lenstr(vname_GasExcFlux(1,itrc))
            write(*,1) 
     &           vname_GasExcFlux(1,itrc)(1:lvar), 
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         end if
      end do
# endif /* BIOLOGY_NPZDOC && OXYGEN */
# endif /* SOLVE3D */

    1 format(/1x, '### ERROR: wrt_',CAVR,' :: Cannot write variable ''',
     &             A, ''' into averages file, rec =', i6, 3x,A,i4)
      goto 100
   99 if (may_day_flag == 0) may_day_flag=3
  100 continue

! Close or synchronize netCDF file to disk to allow other
! processes to access data immediately after it is written.

# if defined MPI && !defined PARALLEL_FILES
      ierr=nf_close(nc AVR )
      if (nrpf AVR >0 .and. record>=nrpf AVR ) nc AVR =-1
# else
      if (nrpf AVR >0 .and. record>=nrpf AVR ) then
        ierr=nf_close(nc AVR )
        nc AVR =-1
      else
        ierr=nf_sync(nc AVR )
      endif
# endif
      if (ierr == nf_noerr) then
# ifdef MPI_SILENT_MODE
        if (mynode==0) then
# endif
          write(tstring,'(F18.8)') time_ AVR *sec2day
          i=1
          do while (i<18 .and. tstring(i:i)==' ')
            i=i+1
          enddo
          write(*,'(7x,4A,1x,A,1x,A,I4,A,I4,1x,A,I3)') 
     &      'wrt_',CAVR,' :: ',
     &      'wrote averages for time_avg =', tstring(i:i+8), 'rec =',
     &       record, '/', nrec AVR   MYID
# ifdef MPI_SILENT_MODE
        endif
# endif
      else
        write(*,'(/1x,4A/)')   '### ERROR: wrt_',CAVR,' :: Cannot ',
     &                         'synchronize/close averages file.'
        if (may_day_flag==0) may_day_flag=3
      endif
# if defined MPI && !defined PARALLEL_FILES
      if (mynode < NNODES-1) call MPI_Send (blank, 1, MPI_INTEGER,
     &                           mynode+1, 3, ocean_grid_comm, ierr)
# endif
      end
# ifndef SLICE_AVR
#  undef START_3D_ARRAY_K0
#  undef START_3D_ARRAY_K1
#  undef  AVR 
#  undef CAVR
#  undef WRTROUTINE
#  undef NOPARWRT
#  if defined  SLICE_AVG && !defined SLICE_AVR /* also create wrt_slice_avg */
#   define SLICE_AVR
#   include "wrt_avg.F"
#  endif
# endif
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
