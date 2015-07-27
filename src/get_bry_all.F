#include "cppdefs.h"
#if !defined ANA_BRY && ( defined OBC_WEST   || defined OBC_EAST  || \
                          defined OBC_SOUTH  || defined OBC_NORTH )  \
                     && ( defined M2_FRC_BRY || defined Z_FRC_BRY || \
                          defined M3_FRC_BRY || defined T_FRC_BRY )

      subroutine get_bry_all (ierr)     ! Read side boundary forcing
      implicit none                     ! fields from boundary file
# include "param.h"
# include "scalars.h"
# include "ncvars.h"
# include "netcdf.inc"
# include "boundary.h"
      real cff
      character*15  vname_bry
      integer lstr, lenstr, ierr, ierr_all, itrc
# if defined OBC_WEST  || defined OBC_EAST
     &                          , nf_read_bry_EW
# endif
# if defined OBC_SOUTH || defined OBC_NORTH
     &                          , nf_read_bry_NS
# endif
      logical need_bry
# ifdef OBC_WEST
     &      , west_bry_active
# endif
# ifdef OBC_EAST
     &      , east_bry_active
# endif
# ifdef OBC_SOUTH
     &      , south_bry_active
# endif
# ifdef OBC_NORTH
     &      , north_bry_active
# endif

! Determine which side(s) require boundary conditions and check
! whether it is needed at all.  Note that "need_bry" is used here
! as a trigger variable: if it remains .false., then this MPI
! subdomain does not have edge adjacent to an open boundary.

      need_bry=.false.
# ifdef OBC_WEST
#  ifdef MPI
      west_bry_active=.not.west_exchng
#  else
      west_bry_active=.true.
#  endif
      need_bry=need_bry .or. west_bry_active
# endif
# ifdef OBC_EAST
#  ifdef MPI
      east_bry_active=.not.east_exchng
#  else
      east_bry_active=.true.
#  endif
      need_bry=need_bry .or. east_bry_active
# endif
# ifdef OBC_SOUTH
#  ifdef MPI
      south_bry_active=.not.south_exchng
#  else
      south_bry_active=.true.
#  endif
      need_bry=need_bry .or. south_bry_active
# endif
# ifdef OBC_NORTH
#  ifdef MPI
      north_bry_active=.not.north_exchng
#  else
      north_bry_active=.true.
#  endif
      need_bry=need_bry .or. north_bry_active
# endif

! Initialization: Check, whether boundary forcing file is already
!---------------- opened, and if not, open it.  Find and save netCDF
! IDs for relevant variables.  Determine whether there is cycling to
! reuse the input data and find cycling period "bry_cycle", set
! initial cycling index "bry_ncycle" and record index "bry_rec",
! time index "itbry" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.
# ifdef VERBOSE
      write(*,*) 'enter get_bry_all' MYID
# endif

      ierr_all=0
      ierr=nf_noerr

      lstr=lenstr(bry_file)
      if (iic==ntstart) then
        if (bry_id == -1) then
          ierr=nf_open (bry_file(1:lstr), nf_nowrite, bry_id)
          if (ierr/=nf_noerr) write(*,'(/1x,4A/12x,A/)')
     &             '### ERROR: get_bry_all :: Cannot open file ''',
     &              bry_file(1:lstr),   '''.',  nf_strerror(ierr)

        endif
        if (ierr==nf_noerr) then
          ierr=nf_inq_varid (bry_id, 'bry_time',  bry_time_id)
          if (ierr/=nf_noerr) write(*,1) 'bry_time'
          if (ierr==nf_noerr) then

# ifdef OBC_WEST
            if (west_bry_active) then
#  ifdef Z_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'zeta_west', zeta_west_id)
              if (ierr/=nf_noerr) write(*,1) 'zeta_west',
     &                                             bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'ubar_west', ubar_west_id)
              if (ierr/=nf_noerr) write(*,1) 'ubar_west',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id, 'vbar_west', vbar_west_id)
              if (ierr/=nf_noerr) write(*,1) 'vbar_west',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
              ierr=nf_inq_varid (bry_id,    'u_west', u_west_id)
              if (ierr/=nf_noerr)  write(*,1) 'u_west',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id,    'v_west', v_west_id)
              if (ierr/=nf_noerr)  write(*,1) 'v_west',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
              ierr=nf_inq_varid (bry_id,'temp_west', t_west_id(itemp))
              if (ierr/=nf_noerr) write(*,1) 'temp_west',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    ifdef SALINITY
              ierr=nf_inq_varid (bry_id,'salt_west',t_west_id(isalt))
              if (ierr/=nf_noerr) write(*,1) 'salt_west',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    endif
          do itrc = itemp + ntrc_salt + 1, NT
             vname_bry = trim(vname(1,itrc+indxT-itemp))/ /'_west'

             ierr=nf_inq_varid (bry_id, vname_bry,
     &            t_west_id(itrc))
             if (ierr/=nf_noerr) then
#    ifdef DEFAULT_BGC_BRY_VALUES
                write(*,11) trim(vname_bry)
                t_west_id(itrc) = 0
#    else
                write(*,1) trim(vname_bry),bry_file(1:lstr)
                ierr_all=ierr_all+ierr
#    endif
             end if
          end do
#   endif
#  endif
            endif
# endif

# ifdef OBC_EAST
            if (east_bry_active) then
#  ifdef Z_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'zeta_east', zeta_east_id)
              if (ierr/=nf_noerr) write(*,1) 'zeta_east',
     &                                             bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'ubar_east', ubar_east_id)
              if (ierr/=nf_noerr) write(*,1) 'ubar_east',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id, 'vbar_east', vbar_east_id)
              if (ierr/=nf_noerr) write(*,1) 'vbar_east',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
              ierr=nf_inq_varid (bry_id,    'u_east',   u_east_id)
              if (ierr/=nf_noerr)  write(*,1)   'u_east',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id,    'v_east',   v_east_id)
              if (ierr/=nf_noerr)  write(*,1)   'v_east',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
              ierr=nf_inq_varid (bry_id,'temp_east',t_east_id(itemp))
              if (ierr/=nf_noerr) write(*,1) 'temp_east',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    ifdef SALINITY
              ierr=nf_inq_varid (bry_id,'salt_east',t_east_id(isalt))
              if (ierr/=nf_noerr) write(*,1) 'salt_east',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    endif
          do itrc = itemp + ntrc_salt + 1, NT
             vname_bry = trim(vname(1,itrc+indxT-itemp))/ /'_east'

             ierr=nf_inq_varid (bry_id, vname_bry,
     &            t_east_id(itrc))
             if (ierr/=nf_noerr) then
#    ifdef DEFAULT_BGC_BRY_VALUES
                write(*,11) trim(vname_bry)
                t_east_id(itrc) = 0
#    else
                write(*,1) trim(vname_bry),bry_file(1:lstr)
                ierr_all=ierr_all+ierr
#    endif
             end if
          end do
#   endif
#  endif
            endif
# endif

# ifdef OBC_SOUTH
            if (south_bry_active) then
#  ifdef Z_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'zeta_south', zeta_south_id)
              if (ierr/=nf_noerr) write(*,1) 'zeta_south',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'ubar_south', ubar_south_id)
              if (ierr/=nf_noerr) write(*,1) 'ubar_south',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id, 'vbar_south', vbar_south_id)
              if (ierr/=nf_noerr) write(*,1) 'vbar_south',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
              ierr=nf_inq_varid (bry_id,    'u_south',  u_south_id)
              if (ierr/=nf_noerr) write(*,1)    'u_south',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id,    'v_south',  v_south_id)
              if (ierr/=nf_noerr) write(*,1)    'v_south',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
              ierr=nf_inq_varid (bry_id,'temp_south',t_south_id(itemp))
              if (ierr/=nf_noerr) write(*,1) 'temp_south',
     &                                                 bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    ifdef SALINITY
              ierr=nf_inq_varid (bry_id,'salt_south',t_south_id(isalt))
              if (ierr/=nf_noerr) write(*,1) 'salt_south',
     &                                                bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    endif
          do itrc = itemp + ntrc_salt + 1, NT
             vname_bry = trim(vname(1,itrc+indxT-itemp))/ /'_south'

             ierr=nf_inq_varid (bry_id, vname_bry,
     &            t_south_id(itrc))
             if (ierr/=nf_noerr) then
#    ifdef DEFAULT_BGC_BRY_VALUES
                write(*,11) trim(vname_bry)
                t_south_id(itrc) = 0
#    else
                write(*,1) trim(vname_bry),bry_file(1:lstr)
                ierr_all=ierr_all+ierr
#    endif
             end if
          end do
#   endif
#  endif
            endif
# endif

# ifdef OBC_NORTH
            if (north_bry_active) then
#  ifdef Z_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'zeta_north', zeta_north_id)
              if (ierr/=nf_noerr) write(*,1) 'zeta_north',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
              ierr=nf_inq_varid (bry_id, 'ubar_north', ubar_north_id)
              if (ierr/=nf_noerr) write(*,1) 'ubar_north',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id, 'vbar_north', vbar_north_id)
              if (ierr/=nf_noerr) write(*,1) 'vbar_north',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
              ierr=nf_inq_varid (bry_id,    'u_north',  u_north_id)
              if (ierr/=nf_noerr) write(*,1)    'u_north',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
              ierr=nf_inq_varid (bry_id,    'v_north',  v_north_id)
              if (ierr/=nf_noerr) write(*,1)  'v_north',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
              ierr=nf_inq_varid (bry_id,'temp_north',t_north_id(itemp))
              if (ierr/=nf_noerr) write(*,1) 'temp_north',
     &                                               bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    ifdef SALINITY
              ierr=nf_inq_varid (bry_id,'salt_north',t_north_id(isalt))
              if (ierr/=nf_noerr) write(*,1) 'salt_north',
     &                                              bry_file(1:lstr)
              ierr_all=ierr_all+ierr
#    endif
          do itrc = itemp + ntrc_salt + 1, NT
             vname_bry = trim(vname(1,itrc+indxT-itemp))/ /'_north'

             ierr=nf_inq_varid (bry_id, vname_bry,
     &            t_north_id(itrc))
             if (ierr/=nf_noerr) then
#    ifdef DEFAULT_BGC_BRY_VALUES
                write(*,11) trim(vname_bry)
                t_north_id(itrc) = 0
#    else
                write(*,1) trim(vname_bry),bry_file(1:lstr)
                ierr_all=ierr_all+ierr
#    endif
             end if
          end do
#   endif
#  endif
            endif
# endif

            ierr=ierr_all
            if (ierr==nf_noerr) then
              call set_cycle (bry_id, bry_time_id, ntbry,
     &             bry_cycle, bry_ncycle, bry_rec, ierr)
              itbry=1
              bry_time(1)=-1.E+20
              bry_time(2)=-1.E+20
            else
              write(*,'(8x,4A)') '### ERROR(s) occur while ',
     &                   'examining content of netCDF file ''',
     &                    bry_file(1:lstr), '''.'
            endif
          else
             write(*,1) 'bry_time', bry_file(1:lstr)
          endif
        endif
      endif
  1   format(1x,'### ERROR: get_bry_all :: Cannot find variable ''',
     &                                A, ''' in file ''', A, '''.')
  11   format(' NOTE in get_all_bry: cannot find variable ''',A,'''')

! Read data from the file:  Check if model time is bounded by past and
!----- ---- ---- --- -----  future data times: if not, increment record
! and cycling indices, flip time index and read a new portion of data.
! Repeat until model time falls between the two data times.

! IMPORTANT! Always read "bry_time" even if no actual data needs to be
! read: this is necessary to make sure that running out of data records
! condition is the same for all MPI nodes.

      do while (bry_time(itbry)<time+0.5*dt .and.ierr==nf_noerr)
        call advance_cycle (bry_cycle,ntbry,bry_ncycle,bry_rec,ierr)
        if (ierr==nf_noerr) then
          ierr=nf_get_var1_FTYPE (bry_id, bry_time_id, bry_rec, cff)
          if (ierr==nf_noerr) then
            itbry=min(3-itbry,ntbry)
            bry_time(itbry)=cff*day2sec + bry_cycle*bry_ncycle

            if (need_bry) then
# ifdef OBC_WEST
              if (west_bry_active) then
#  ifdef Z_FRC_BRY
                ierr=nf_read_bry_EW (zeta_west_dt(0,itbry), bry_id,
     &                               zeta_west_id, bry_rec, r2dvar)
                if (ierr.ne.nf_noerr) write(*,2) 'zeta_west'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
                ierr=nf_read_bry_EW (ubar_west_dt(0,itbry), bry_id,
     &                               ubar_west_id, bry_rec, u2dvar)
                if (ierr.ne.nf_noerr) write(*,2) 'ubar_west'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_EW (vbar_west_dt(0,itbry), bry_id,
     &                               vbar_west_id, bry_rec, v2dvar)
                if (ierr.ne.nf_noerr) write(*,2) 'vbar_west'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
                ierr=nf_read_bry_EW (u_west_dt(0,1,itbry), bry_id,
     &                               u_west_id,  bry_rec,  u3dvar)
                if (ierr.ne.nf_noerr)  write(*,2)  'u_west'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_EW (v_west_dt(0,1,itbry), bry_id,
     &                               v_west_id,  bry_rec,  v3dvar)
                if (ierr.ne.nf_noerr)  write(*,2)  'v_west'
                ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
                do itrc=1,NT
                  ierr=nf_read_bry_EW (t_west_dt(0,1,itbry,itrc),
     &                   bry_id, t_west_id(itrc),  bry_rec, r3dvar)
#     ifdef DEFAULT_BGC_BRY_VALUES
                  if (ierr /= nf_noerr)  then
#      ifdef BIOLOGY_NPZDOC
                    if (itrc >= iNO3_ .and. 
     &                   itrc <= iNO3_ + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_west_dt, itbry, Mm+1)
#      endif
#      if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
                    if (itrc >= iPO4 .and. 
     &                  itrc <= iPO4 + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_west_dt, itbry, Mm+1)
#      endif
#     endif /* DEFAULT_BGC_BRY_VALUES */
                  ierr_all=ierr_all+ierr
                enddo
#   endif /* T_FRC_BRY */
#  endif /* SOLVE3D */
      endif ! east_bry_active
# endif /* OBC_WEST */

# ifdef OBC_EAST
              if (east_bry_active) then
#  ifdef Z_FRC_BRY
                ierr=nf_read_bry_EW (zeta_east_dt(0,itbry), bry_id,
     &                             zeta_east_id, bry_rec, r2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'zeta_east'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
                ierr=nf_read_bry_EW (ubar_east_dt(0,itbry), bry_id,
     &                               ubar_east_id, bry_rec, u2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'ubar_east'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_EW (vbar_east_dt(0,itbry), bry_id,
     &                               vbar_east_id, bry_rec, v2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'vbar_east'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
                ierr=nf_read_bry_EW (u_east_dt(0,1,itbry), bry_id,
     &                               u_east_id,  bry_rec,  u3dvar)
              if (ierr.ne.nf_noerr)  write(*,2)  'u_east'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_EW (v_east_dt(0,1,itbry), bry_id,
     &                               v_east_id,  bry_rec,  v3dvar)
              if (ierr.ne.nf_noerr)  write(*,2)  'v_east'
                ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
                do itrc=1,NT
                  ierr=nf_read_bry_EW (t_east_dt(0,1,itbry,itrc),
     &                bry_id, t_east_id(itrc),  bry_rec, r3dvar)
#     ifdef DEFAULT_BGC_BRY_VALUES
                  if (ierr /= nf_noerr)  then
#      ifdef BIOLOGY_NPZDOC
                    if (itrc >= iNO3_ .and. 
     &                   itrc <= iNO3_ + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_east_dt, itbry, Mm+1)
#      endif
#      if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
                    if (itrc >= iPO4 .and. 
     &                  itrc <= iPO4 + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_east_dt, itbry, Mm+1)
#      endif
                  
#     endif /* DEFAULT_BGC_BRY_VALUES */
                  ierr_all=ierr_all+ierr
                enddo
#   endif
#  endif
              endif
# endif

# ifdef OBC_SOUTH
              if (south_bry_active) then
#  ifdef Z_FRC_BRY
                ierr=nf_read_bry_NS (zeta_south_dt(0,itbry), bry_id,
     &                              zeta_south_id, bry_rec,  r2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'zeta_south'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
                ierr=nf_read_bry_NS (ubar_south_dt(0,itbry), bry_id,
     &                               ubar_south_id, bry_rec, u2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'ubar_south'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_NS (vbar_south_dt(0,itbry), bry_id,
     &                               vbar_south_id, bry_rec, v2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'vbar_south'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
                ierr=nf_read_bry_NS (u_south_dt(0,1,itbry), bry_id,
     &                               u_south_id,  bry_rec,  u3dvar)
              if (ierr.ne.nf_noerr)  write(*,2)  'u_south'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_NS (v_south_dt(0,1,itbry), bry_id,
     &                               v_south_id,  bry_rec,  v3dvar)
              if (ierr.ne.nf_noerr)  write(*,2)  'v_south'
                ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
                do itrc=1,NT
                  ierr=nf_read_bry_NS (t_south_dt(0,1,itbry,itrc),
     &                bry_id, t_south_id(itrc),  bry_rec, r3dvar)
#     ifdef DEFAULT_BGC_BRY_VALUES
                  if (ierr /= nf_noerr)  then
#      ifdef BIOLOGY_NPZDOC
                    if (itrc >= iNO3_ .and. 
     &                   itrc <= iNO3_ + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_south_dt, itbry, Lm+1)
#      endif
#      if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
                    if (itrc >= iPO4 .and. 
     &                  itrc <= iPO4 + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_south_dt, itbry, Lm+1)
#      endif
                  
#     endif /* DEFAULT_BGC_BRY_VALUES */
                  ierr_all=ierr_all+ierr
                enddo
#   endif
#  endif
              endif
# endif

# ifdef OBC_NORTH
              if (north_bry_active) then
#  ifdef Z_FRC_BRY
                ierr=nf_read_bry_NS (zeta_north_dt(0,itbry), bry_id,
     &                               zeta_north_id, bry_rec, r2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'zeta_north'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
                ierr=nf_read_bry_NS (ubar_north_dt(0,itbry), bry_id,
     &                               ubar_north_id, bry_rec, u2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'ubar_north'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_NS (vbar_north_dt(0,itbry), bry_id,
     &                               vbar_north_id, bry_rec, v2dvar)
              if (ierr.ne.nf_noerr) write(*,2) 'vbar_north'
                ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
                ierr=nf_read_bry_NS (u_north_dt(0,1,itbry), bry_id,
     &                               u_north_id,  bry_rec,  u3dvar)
              if (ierr.ne.nf_noerr)  write(*,2)  'u_north'
                ierr_all=ierr_all+ierr
                ierr=nf_read_bry_NS (v_north_dt(0,1,itbry), bry_id,
     &                               v_north_id,  bry_rec,  v3dvar)
              if (ierr.ne.nf_noerr)  write(*,2)  'v_north'
                ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
                do itrc=1,NT
                  ierr=nf_read_bry_NS (t_north_dt(0,1,itbry,itrc),
     &                 bry_id, t_north_id(itrc),  bry_rec, r3dvar)
#     ifdef DEFAULT_BGC_BRY_VALUES
                  if (ierr /= nf_noerr)  then
#      ifdef BIOLOGY_NPZDOC
                    if (itrc >= iNO3_ .and. 
     &                   itrc <= iNO3_ + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_north_dt, itbry, Lm+1)
#      endif
#      if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
                    if (itrc >= iPO4 .and. 
     &                  itrc <= iPO4 + ntrc_bio - 1) then
                      write(*,3)  trim(vname_bry)
                      ierr=0
                      call set_bry_defaults (t_north_dt, itbry, Lm+1)
#      endif
                  
#     endif /* DEFAULT_BGC_BRY_VALUES */
                  ierr_all=ierr_all+ierr
                enddo
#   endif
#  endif
              endif
# endif
              ierr=ierr_all
              if (ierr==0) then
# ifdef MPI_SILENT_MODE
                if (mynode==0) then
# endif
                  write(*,'(3x,A,3x,A,F12.4,2(1x,A,I4))')
     &           'get_bry_all :: read boundary for all fields',
     &           'bry_time =', cff, 'rec =', bry_rec MYID
# ifdef MPI_SILENT_MODE
                endif
# endif
              else
                write(*,'(1x,2A,I4,1x,3A,1x,A,I4)') '### ERROR: ',
     &             'get_bry_all :: Cannot read rec =',  bry_rec,
     &             'of ''',  bry_file(1:lstr),  '''.'  MYID
              endif
            endif !<-- need_bry
          else
            write(*,'(8x,2A,I4)')  '### ERROR: get_bry_all :: ',
     &             'Cannot read variable ''bry_time''' MYID
          endif
        else
           write(*,'(/1x,A,I4,1x,A,I4/7x,3A/7x,2(A,G12.4)/)')
     &   '### ERROR: get_bry_all :: requested time record ', bry_rec,
     &   'exeeds the last record',   ntbry,   'available in file ''',
     &    bry_file(1:lstr),  '''',  'tdays = ', tdays,
     &   '  but the last available  bry_time =',
     &                                   bry_time(itbry)*sec2day
        endif
      enddo
  2   format(/1x,'### ERROR in get_bry_all: Cannot read variable ''',
     &                                                     A, '''.')
 3    format('###  NOTE in get_bry_all: cannot read variable ''',
     &                           A,'''/',' - using default values')
# ifdef VERBOSE
      write(*,*) 'return from get_bry_all' MYID
# endif
      return
      end




      subroutine set_bry_all_tile (istr,iend,jstr,jend, ierr)
      implicit none
      integer istr,iend,jstr,jend, ierr,
     &        imin,imax,jmin,jmax, it1,it2
# ifdef SOLVE3D
     &                           , k, itrc
# endif
# if defined OBC_WEST  || defined OBC_EAST
     &                           , j
# endif
# if defined OBC_SOUTH || defined OBC_NORTH
     &                           , i
# endif

      logical need_bry
      real cff, cff1,cff2
# include "param.h"
# include "scalars.h"
# include  "boundary.h"

      need_bry=.false.
                                  ! NOTE: Temporal interpolation of
      imin=istr                   ! BRY forcing variables is done over
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then      ! non-standard ranges of spatial
#  ifdef OBC_WEST
        need_bry=.true.           ! indices which is motivated by the
#  endif
        imin=istr-1               ! desire to avoid using MPI calls
      endif                       ! to exchange ghost points for BRY
# elif defined EXCH_EAST_WEST
      if (WEST_EXCHNG) then       ! arrays, while, at the same time,
        imin=istr-1               ! having only a single row of ghost
      endif                       ! points (so the standard "extended"
# endif
                                  ! bounds are not applicable here).
      imax=iend
# ifndef EW_PERIODIC
      if (EASTERN_EDGE) then
#  ifdef OBC_EAST
        need_bry=.true.
#  endif
        imax=iend+1
      endif
# elif defined EXCH_EAST_WEST
      if (EAST_EXCHNG) then
        imax=iend+1
      endif
# endif

      jmin=jstr
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  ifdef OBC_SOUTH
        need_bry=.true.
#  endif
        jmin=jstr-1
      endif
# elif defined EXCH_NORTH_SOUTH
      if (SOUTH_EXCHNG) then
        jmin=jstr-1
      endif
# endif

      jmax=jend
# ifndef NS_PERIODIC
      if (NORTHERN_EDGE) then
#  ifdef OBC_NORTH
        need_bry=.true.
#  endif
        jmax=jend+1
      endif
# elif defined EXCH_NORTH_SOUTH
      if (NORTH_EXCHNG) then
        jmax=jend+1
      endif
# endif

      it1=3-itbry              ! Always set synchro_flag, regardless
      it2=itbry                ! whether the boundary is needed or not:
      cff=time+0.5*dt          ! this is necessary to make sure that
      cff1=bry_time(it2)-cff   ! running out of data records condition
      cff2=cff-bry_time(it1)   ! is the same for all MPI nodes.

      if (cff1<dt) synchro_flag=.true.

      if (need_bry) then
        cff=1./(cff1+cff2)                      ! interpolate boundary
        cff1=cff1*cff                           ! values in time
        cff2=cff2*cff

# ifdef OBC_WEST
        if (WESTERN_EDGE) then
#  ifdef Z_FRC_BRY
          do j=jmin,jmax
            zeta_west(j)=cff1*zeta_west_dt(j,it1)
     &                  +cff2*zeta_west_dt(j,it2)
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do j=jmin,jmax
            ubar_west(j)=cff1*ubar_west_dt(j,it1)
     &                  +cff2*ubar_west_dt(j,it2)
            vbar_west(j)=cff1*vbar_west_dt(j,it1)
     &                  +cff2*vbar_west_dt(j,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do j=jmin,jmax
              u_west(j,k)=cff1*u_west_dt(j,k,it1)
     &                   +cff2*u_west_dt(j,k,it2)
              v_west(j,k)=cff1*v_west_dt(j,k,it1)
     &                   +cff2*v_west_dt(j,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,NT
            do k=1,N
              do j=jmin,jmax
                t_west(j,k,itrc)=cff1*t_west_dt(j,k,it1,itrc)
     &                          +cff2*t_west_dt(j,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif

# ifdef OBC_EAST
        if (EASTERN_EDGE) then
#  ifdef Z_FRC_BRY
          do j=jmin,jmax
            zeta_east(j)=cff1*zeta_east_dt(j,it1)
     &                  +cff2*zeta_east_dt(j,it2)
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do j=jmin,jmax
            ubar_east(j)=cff1*ubar_east_dt(j,it1)
     &                  +cff2*ubar_east_dt(j,it2)
            vbar_east(j)=cff1*vbar_east_dt(j,it1)
     &                  +cff2*vbar_east_dt(j,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do j=jmin,jmax
              u_east(j,k)=cff1*u_east_dt(j,k,it1)
     &                   +cff2*u_east_dt(j,k,it2)
              v_east(j,k)=cff1*v_east_dt(j,k,it1)
     &                   +cff2*v_east_dt(j,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,NT
            do k=1,N
              do j=jmin,jmax
                t_east(j,k,itrc)=cff1*t_east_dt(j,k,it1,itrc)
     &                          +cff2*t_east_dt(j,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif

# ifdef OBC_SOUTH
        if (SOUTHERN_EDGE) then
#  ifdef Z_FRC_BRY
          do i=imin,imax
            zeta_south(i)=cff1*zeta_south_dt(i,it1)
     &                   +cff2*zeta_south_dt(i,it2)
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do i=imin,imax
            ubar_south(i)=cff1*ubar_south_dt(i,it1)
     &                   +cff2*ubar_south_dt(i,it2)
            vbar_south(i)=cff1*vbar_south_dt(i,it1)
     &                   +cff2*vbar_south_dt(i,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do i=imin,imax
              u_south(i,k)=cff1*u_south_dt(i,k,it1)
     &                    +cff2*u_south_dt(i,k,it2)
              v_south(i,k)=cff1*v_south_dt(i,k,it1)
     &                    +cff2*v_south_dt(i,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,NT
            do k=1,N
              do i=imin,imax
                t_south(i,k,itrc)=cff1*t_south_dt(i,k,it1,itrc)
     &                           +cff2*t_south_dt(i,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif

# ifdef OBC_NORTH
        if (NORTHERN_EDGE) then
#  ifdef Z_FRC_BRY
          do i=imin,imax
            zeta_north(i)=cff1*zeta_north_dt(i,it1)
     &                   +cff2*zeta_north_dt(i,it2)
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do i=imin,imax
            ubar_north(i)=cff1*ubar_north_dt(i,it1)
     &                   +cff2*ubar_north_dt(i,it2)
            vbar_north(i)=cff1*vbar_north_dt(i,it1)
     &                   +cff2*vbar_north_dt(i,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do i=imin,imax
              u_north(i,k)=cff1*u_north_dt(i,k,it1)
     &                    +cff2*u_north_dt(i,k,it2)
              v_north(i,k)=cff1*v_north_dt(i,k,it1)
     &                    +cff2*v_north_dt(i,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,NT
            do k=1,N
              do i=imin,imax
                t_north(i,k,itrc)=cff1*t_north_dt(i,k,it1,itrc)
     &                           +cff2*t_north_dt(i,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif
        if (cff1<0. .or. cff2<0.) then
           write(*,'(/3A/3(1x,A,F16.10)/)')            '### WARNING: ',
     &      'set_bry_all_tile :: current model time is out of bounds ',
     &      'of ''bry_time''.', 'bry_tstart =', bry_time(it1)*sec2day,
     &      'tdays =',  tdays,  'bry_tend =',   bry_time(it2)*sec2day
          ierr=ierr+1
        endif
      endif  ! <-- need_bry
      return
      end


# ifdef DEFAULT_BGC_BRY_VALUES
      subroutine set_bry_defaults (t_bry_dt, itbry, ierr, LMmpp)
      implicit none
#  include "param.h"
      real t_dt(0:LMmpp,N,2), temp, SiO4, cff
      integer :: LMmpp, ij, k


#  ifdef BIOLOGY_NPZDOC
               if (.not. bry_set .and. itrc >= iNO3_ .and. 
     &              itrc <= iNO3_ + ntrc_bio - 1) then

                     ierr = 0
                     if (itrc == iNO3_) then
                       cff=20./3.
                       cff1=2./3.
                        do k=1,N
                          do ij=0,Mm+1
                             temp=t_bry_dt(j,k,itbry,itemp)
                             if (temp.lt.8.) then
                               SiO4=30.
                             elseif (temp.le.11.) then
                               SiO4=30.-((temp-8.)*cff)
                             elseif (temp.le.13.) then
                               SiO4=10.-((temp-11.)*4.)
                             elseif (temp.le.16.) then
                               SiO4=2.-((temp-13.)*cff1)
                             else
                               SiO4=0.
                             endif
                             t_bry_dt(j,k,itbry,itrc)=1.67+0.5873*SiO4
     &                             +0.0144*SiO4**2
     &                             +0.0003099*SiO4**3
#     ifdef NO3_SFC_CUTOFF
                             t_bry_dt(j,k,itbry,itrc)=t_bry_dt(j,k,itbry,itrc)*
     &                             (1-.5*(tanh((z_r(iwest,j,k)+100.)/8.)+1))
#     endif /* NO3_SFC_CUTOFF */
                          enddo
                        enddo
                     else if (itrc .eq. iNH4_) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
                     else if (itrc .eq. iChla) then
                        t_bry_dt(:,:,itbry,itrc) = 0.10
                     else if (itrc .eq. iPhyt) then
                        t_bry_dt(:,:,itbry,itrc) = 0.05
                     else if (itrc .eq. iZoo_) then
                        t_bry_dt(:,:,itbry,itrc) = 0.02
                     else if (itrc .eq. iSDet) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
                     else if (itrc .eq. iLDet) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
#     ifdef OXYGEN
                     else if (itrc .eq. iO2) then
                        t_bry_dt(:,:,itbry,itrc) = 177.9  ! mmol/m3
#      ifdef CARBON
                     else if (itrc .eq. iDIC) then
                        t_bry_dt(:,:,itbry,itrc) = 2200.
                     else if (itrc .eq. iTALK) then
                        t_bry_dt(:,:,itbry,itrc) = 2300.
                     else if (itrc .eq. iSDetC) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
                     else if (itrc .eq. iLDetC) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
                     else if (itrc .eq. iCaCO3) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
#   endif
#     endif /* OXYGEN */
                     end if  ! itrc .eq. iNO3_
               end if  ! .not. bry_set .and. itrc .ge. iNO3_ ...
#    endif /* BIOLOGY_NPZDOC */

#    ifdef BIOLOGY_BEC
! do not provide defaults for selected biogeochemical variables
               if (.not. bry_set .and. itrc .ge. iPO4 .and. 
     &              itrc .le. iPO4 + ntrc_bio - 1) then
                     ierr = 0
                     write(*,3)  vname_bry(1:lvar+5)
                     if (itrc .eq. iNH4 .or. itrc .eq. iDOC .or.
     &                    itrc .eq. iDON .or. itrc. eq. iDOFE
     &                    .or. itrc .eq. iDOP) then
                        t_bry_dt(:,:,itbry,itrc) = 0.0
                     else if (itrc .eq. iPO4) then
#  define temp cff
#  define SiO4 cff1
                        do k=1,N
                          do ij=0,Mm+1
                             temp=t_bry_dt(j,k,itbry,itemp)
                             if (temp.lt.8.) then
                               SiO4=30.
                             elseif (temp.ge.8. .and. temp.le.11.) then
                               SiO4=30.-((temp-8.)*(20./3.))
                             elseif (temp.gt.11. .and. temp.le.13.) then
                               SiO4=10.-((temp-11.)*(8./2.))
                             elseif (temp.gt.13. .and. temp.le.16.) then
                               SiO4=2.-((temp-13.)*(2./3.))
                             elseif (temp.gt.16.) then
                               SiO4=0.
                             endif
                             t_bry_dt(j,k,itbry,itrc)=(1.67+0.5873*SiO4
     &                             +0.0144*SiO4**2
     &                             +0.0003099*SiO4**3)/16
#   ifdef NO3_SFC_CUTOFF 
                             t_bry_dt(j,k,itbry,itrc)=t_bry_dt(j,k,itbry,itrc)*
     &                             (1-.5*(tanh((z_r(iwest,j,k)+100.)/8.)+1))
#   endif /* NO3_SFC_CUTOFF */
                          end do
                        end do
                     else if (itrc .eq. iNO3) then
                        t_bry_dt(:,:,itbry,itrc) = 16*t_bry_dt(:,:,itbry,iPO4)
                     else if (itrc .eq. iSiO3) then
                        t_bry_dt(:,:,itbry,itrc) = t_bry_dt(:,:,itbry,iNO3)
                     else if (itrc .eq. iFE) then
                        t_bry_dt(:,:,itbry,itrc) = 5.395e-4  ! mmol/m3
                     else if (itrc .eq. iO2) then
                        t_bry_dt(:,:,itbry,itrc) = 177.9  ! mmol/m3
                     else if (itrc .eq. iDIC) then
                        t_bry_dt(:,:,itbry,itrc) = 2255  ! mmol/m3
                     else if (itrc .eq. iALK) then
                        t_bry_dt(:,:,itbry,itrc) = 2363  ! mmol/m3
                     else if (itrc .eq. iSPC) then
                        t_bry_dt(:,:,itbry,itrc) = 0.10
                     else if (itrc .eq. iSPCHL) then
                        t_bry_dt(:,:,itbry,itrc) = 0.1 *
     &                       t_bry_dt(:,:,itbry,iSPC)  
                     else if (itrc .eq. iSPFE) then
                        t_bry_dt(:,:,itbry,itrc) = 4.d-5 *
     &                     t_bry_dt(:,:,itbry,iSPC)  
                     else if (itrc .eq. iSPCACO3) then
                        t_bry_dt(:,:,itbry,itrc) = 0.03 *
     &                     t_bry_dt(:,:,itbry,iSPC)  
                     else if (itrc .eq. iDIATC) then
                        t_bry_dt(:,:,itbry,itrc) = 0.10
                     else if (itrc .eq. iDIATCHL) then
                        t_bry_dt(:,:,itbry,itrc) = 0.1 *
     &                       t_bry_dt(:,:,itbry,iDIATC)  
                     else if (itrc .eq. iDIATSI) then
                        t_bry_dt(:,:,itbry,itrc) = 0.2 *
     &                       t_bry_dt(:,:,itbry,iDIATC)  
                     else if (itrc .eq. iDIATFE) then
                        t_bry_dt(:,:,itbry,itrc) = 3.d-5 *
     &                       t_bry_dt(:,:,itbry,iDIATC)  
                     else if (itrc .eq. iDIAZC) then
                        t_bry_dt(:,:,itbry,itrc) = 0.01
                     else if (itrc .eq. iDIAZCHL) then
                        t_bry_dt(:,:,itbry,itrc) = 0.1 *
     &                       t_bry_dt(:,:,itbry,iDIAZC)  
                     else if (itrc .eq. iDIAZFE) then
                        t_bry_dt(:,:,itbry,itrc) = 3.d-5 *
     &                       t_bry_dt(:,:,itbry,iDIAZC)  
                     else if (itrc .eq. iZOOC) then
                        t_bry_dt(:,:,itbry,itrc) = 0.05
                     end if
               end if
#    endif /* BIOLOGY_BEC */
      return
      end
# endif /* DEFAULT_BGC_BRY_VALUES */

#else
      subroutine get_bry_empty
      end
#endif

