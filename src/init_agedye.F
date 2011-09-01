#include "cppdefs.h"
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER

      subroutine init_agedye(tile)
      implicit none
      integer ierr, tile
# include "param.h"
# include "scalars.h"
# include "compute_tile_bounds.h"
      ierr = 0
      call init_agedye_tile (Istr,Iend,Jstr,Jend,ierr)
      ! Now if ierr is nonzero, there was an error related to Netcdf input
      if (ierr.ne.0) then
         may_day_flag = 2
      endif
      return
      end


      subroutine init_agedye_tile (Istr,Iend,Jstr,Jend,ierr)
!
! Set up arrays related to age/dye tracers.
!
      implicit none
!
# include "netcdf.inc"
# include "param.h"
# include "grid.h"
# include "ncvars.h"
# include "sources.h"
!
      integer Istr,Iend,Jstr,Jend,ierr, lenstr, i, j, k, is, lstr
      integer type_bc_ad_id, switch_on_ad_id, switch_off_ad_id, nf_fread
      real type_bc_ad(GLOBAL_2D_ARRAY,N), switch_off_ad(GLOBAL_2D_ARRAY,N)
      real switch_on_ad(GLOBAL_2D_ARRAY,N)
      character fname*80, tracer_type*10, tracer_bc*10, tnum*3, units_att*20
      logical global_type_att_found
# include "compute_extended_bounds.h"
!
      do is = 1, N_agedye
         ! If not opened yet, open NetCDF file related to a/d tracer is for reading.
         ! Find and save IDs for relevant variables.
         if (ncid_ad(is).eq.-1) then
            ! construct forcing file name for this tracer:
            write(tnum,'(i3.3)') is
            fname='ad_tracer_'//trim(tnum)//'.nc'
            lstr=lenstr(fname)
            ierr=nf_open(fname(1:lstr), nf_nowrite, ncid_ad(is))
            if (ierr.ne.nf_noerr) then
               write(stdout,'(/1x,4A/)')
     &             '### ERROR in init_agedye: can ',
     &             'not open netCDF file ''', fname(1:lstr), '''.'
               goto 99
            endif
         endif ! ncid_ad(is).eq.-1
! First check for global attribute 'tracer_type'. If it is found and set to 'age',
! then we continue with the next age/dye tracer:
         global_type_att_found = .false.
         ierr = nf_get_att_text(ncid_ad(is),nf_global,'tracer_type',tracer_type)
         if (ierr.eq.nf_noerr) then
            global_type_att_found = .true.
            if (tracer_type(1:3).eq.'age') then
               Age(is) = .true.
!               MPI_master_only write(stdout,'(1x,a)') 'Age tracer found.'
               cycle
            else if (tracer_type(1:3).eq.'dye') then
               Age(is) = .false.
            else
               write(stdout, '(1x,2a,i3,2a)') 
     &         '### ERROR in init_agedye: found global attribute ',
     &         '''tracer_type'' for tracer ',is,
     &         ', but with an invalid value: ', tracer_type(1:lstr)
               ierr = ierr+1
               return
            endif ! tracer_type.eq.'age'
         endif ! ierr.eq.nf_noerr
! type_bc_ad:
         ierr=nf_inq_varid (ncid_ad(is), 'type_bc_ad', type_bc_ad_id)
         if (ierr .ne. nf_noerr) then
            write(stdout,1) 'type_bc_ad', fname(1:lstr)
            return
         end if
         ierr = nf_fread(type_bc_ad(START_2D_ARRAY,1),ncid_ad(is),type_bc_ad_id,1,r3dvar)
         if (ierr .ne. nf_noerr) then
            write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'cannot read type_bc_ad from file', fname(1:lstr)
            goto 99
         endif
! Read switch_on variable, if present:
         ierr=nf_inq_varid (ncid_ad(is), 'switch_on', switch_on_ad_id)
         if (ierr .ne. nf_noerr) then
            ! not present -> set switch on mask to -1
            do k=1,N
               do j=jstrR,jendR
                  do i=istrR,iendR
                     switch_on_ad(i,j,k) = -1.0
                  enddo
               enddo
            enddo
         else
            ierr = nf_fread(switch_on_ad(START_2D_ARRAY,1),ncid_ad(is),switch_on_ad_id,1,r3dvar)
            if (ierr .ne. nf_noerr) then
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'cannot read switch_on_ad from file', fname(1:lstr)
               goto 99
            endif
            ! Check units (days or time steps):
            ierr=nf_get_att_text(ncid_ad(is), switch_on_ad_id, 'units', units_att)
            if (ierr .ne. nf_noerr) then
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'cannot read units attribute of switch_on_ad from file', fname(1:lstr)
               goto 99
            endif
            if (units_att(1:3).eq.'day') then
               switch_is_days_ad(is) = .true.
            else if (units_att(1:9).eq.'time step') then
               switch_is_days_ad(is) = .false.
            else
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'unknown unit of of switch_on_ad in file', fname(1:lstr)
               goto 99
            end if
         endif
! Read switch_off variable, if present:
         ierr=nf_inq_varid (ncid_ad(is), 'switch_off', switch_off_ad_id)
         if (ierr .ne. nf_noerr) then
            ! not present -> set switch off mask to zero (so all sources will be 
            ! active forever once they are switched on):
            do k=1,N
               do j=jstrR,jendR
                  do i=istrR,iendR
                     switch_off_ad(i,j,k) = 0.0
                  enddo
               enddo
            enddo
         else
            ierr = nf_fread(switch_off_ad(START_2D_ARRAY,1),ncid_ad(is),switch_off_ad_id,1,r3dvar)
            if (ierr .ne. nf_noerr) then
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'cannot read switch_off_ad from file', fname(1:lstr)
               goto 99
            endif
            ! Check units (days or time steps): they have to be the same as for switch_on_ad
            ierr=nf_get_att_text(ncid_ad(is), switch_off_ad_id, 'units', units_att)
            if (ierr .ne. nf_noerr) then
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'cannot read units attribute of switch_on_ad from file', fname(1:lstr)
               goto 99
            endif
            ! Check if units att is consistent with the one for switch_on_ad:
            if (switch_is_days_ad(is) .and. units_att(1:3).ne.'day') then
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'switch_on_ad and switch_off_ad must use the same units in file ', fname(1:lstr)
               goto 99
            endif
            if (.not.switch_is_days_ad(is) .and. units_att(1:9).ne.'time step') then
               write(stdout,'(1x,3a)') '### ERROR in init_agedye: ',
     &           'switch_on_ad and switch_off_ad must use the same units in file ', fname(1:lstr)
               goto 99
            endif
         endif
! Extract the number of sources and their locations:
         Nsrc_ad(is) = 0
         do k=1,N
            do j=jstrR,jendR
               do i=istrR,iendR
# ifdef MASKING
                  if (rmask(i,j).eq.1) then
# endif
                     if (type_bc_ad(i,j,k).ne.0) then
                        ! A source is present in box (i,j,k):
                        Nsrc_ad(is) = Nsrc_ad(is)+1
                        if (Nsrc_ad(is).gt.Max_src_ad) then
                           write(stdout, '(1x,2a,i3)') '### ERROR in init_agedye: ',
     &                          'too many sources for tracer ', is
                           write(stdout, '(1x,a)') 'Increase parameter Max_src_ad in sources.h.'
                           ierr = ierr+1
                           return
                        endif
                        Isrc_ad(is,Nsrc_ad(is)) = i
                        Jsrc_ad(is,Nsrc_ad(is)) = j
                        Ksrc_ad(is,Nsrc_ad(is)) = k
                        Switch_on_src_ad(is,Nsrc_ad(is)) = switch_on_ad(i,j,k)
                        Switch_off_src_ad(is,Nsrc_ad(is)) = switch_off_ad(i,j,k)
                     end if ! type_bc_ad_id(i,j,k).ne.0
# ifdef MASKING
                  end if ! rmask(i,j).eq.1
# endif
               enddo ! i=istrR,iendR
            enddo
         enddo ! k = 1,N
! Determine whether it is actually an age tracer: the global attribute 'tracer_type' has already 
! been checked above. Now we check whether there is a variable attribute belonging to type_bc_ad:
         ierr = nf_get_att_text(ncid_ad(is),type_bc_ad_id,'tracer_type',tracer_type)
         if (ierr.eq.nf_noerr) then
            if (tracer_type(1:3).eq.'age') then
               Age(is) = .true.
            else if (tracer_type(1:3).eq.'dye') then
               Age(is) = .false.
            else
               write(stdout, '(1x,2a,i3,2a)') '### ERROR in init_agedye: found attribute ',
     &              '''tracer_type'' for tracer ',is,', but with an invalid value: ', tracer_type
               ierr = ierr+1
               return
            endif ! tracer_type.eq.'age'
         else if (.not.global_type_att_found) then
            ! Attribute not found anywhere:
            write(stdout, '(1x,2a,i3)') '### ERROR in init_agedye: attribute ',
     &             '''tracer_type'' not found for tracer ', is
            return
         end if ! ierr.eq.nf_noerr
! Determine the type of boundary conditions to be used for tracer is (flux/resoring). This info is
! given by attribute 'tracer_bc', which can either be a global attribute or belong to variable
! type_bc_ad:
         ierr = nf_get_att_text(ncid_ad(is),nf_global,'tracer_bc',tracer_bc)
         if (ierr.eq.nf_noerr) then
            if (tracer_bc(1:4).eq.'flux') then
               Restore_ad(is) = .false.
            elseif (tracer_bc(1:7).eq.'restore') then
               Restore_ad(is) = .true.
            else
               write(stdout, '(1x,2a,i3,2a)') '### ERROR in init_agedye: found global attribute ',
     &              '''tracer_bc'' for tracer ',is,', but with an invalid value: ', tracer_bc
               ierr = ierr+1
               return
            endif ! tracer_bc.eq.'flux'
         else
            ! No global attribute 'tracer_bc' found, so we look for one belonging to 
            ! variable type_bc_ad:
            ierr = nf_get_att_text(ncid_ad(is),type_bc_ad_id,'tracer_bc',tracer_bc)
            if (ierr.eq.nf_noerr) then
               if (tracer_bc(1:4).eq.'flux') then
                  Restore_ad(is) = .false.
               elseif (tracer_bc(1:7).eq.'restore') then
                  Restore_ad(is) = .true.
               else
                  write(stdout, '(1x,2a,i3,2a)') '### ERROR in init_agedye: found attribute ',
     &              '''tracer_bc'' for tracer ',is,', but with an invalid value: ', tracer_bc
                  ierr = ierr+1
                  return
               endif ! tracer_bc.eq.'flux'
            else
               ! Attribute not found:
               write(stdout, '(1x,2a,i3)') '### ERROR in init_agedye: attribute ',
     &              '''tracer_bc'' not found for tracer ', is
               return
            end if ! ierr.eq.nf_noerr
         endif
!         if (Age(is)) then
!            MPI_master_only write(stdout,'(1x,a)') 'Age tracer found.'
!         else
!            MPI_master_only write(stdout,'(1x,a)') 'Dye tracer found.'
!         endif
      end do ! is = 1,N_agedye

1     format(/1x,'### ERROR in init_agedye: cannot find variable ''',
     &     A, ''''/7x, 'in netCDF file ''', A, '''.')

 99   continue
      return
      end

#endif /* PASSIVE_TRACER && AGE_DYE_TRACER */
