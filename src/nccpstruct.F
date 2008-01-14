      program nccpstruct
!
! A generic program to copy skeleton structure of netCDF file
! without filling its content. This is a stand-alone, command line
! operator designed to be used as
!
!          nccpstruct source.nc target.nc
!
! Basically it makes a general inquiry into "source" netCDF file and
! creates "target" netCDF file with exactly the same dimensions,
! variables and attributes.
!

      implicit none
      character*80 src_file, targ_file
      character*32 dimname, attname, varname
      integer, allocatable, dimension(:) :: dimid, vardimids
      integer ncsrc, nctarg, ndims, nvars, ngatts, unlimdimid,
     &        varid, vardims, vartype, varatts, size, i,j, ierr,
     &        nargs, lsrc, ltarg, lvar, lenstr

#include "netcdf.inc"


      nargs=iargc()
      if (nargs .eq. 2) then
        call getarg(1,src_file)
        call getarg(2,targ_file)

        lsrc=lenstr(src_file)
        ltarg=lenstr(targ_file)

        ierr=nf_open (src_file(1:lsrc), nf_nowrite, ncsrc)
        if (ierr .eq. nf_noerr) then
          ierr=nf_create(targ_file(1:ltarg), nf_clobber, nctarg)
          if (ierr .eq. nf_noerr) then
            write(*,'(4x,3A)') 'Created netCDF file ''',
     &                                targ_file(1:ltarg), '''.'
            ierr=nf_inq (ncsrc, ndims, nvars, ngatts, unlimdimid)
            if (ierr .eq. nf_noerr) then
!
! Sort out dimensions:
!
              allocate (dimid(1:ndims))
              allocate (vardimids(1:ndims))

              do i=1,ndims
                ierr=nf_inq_dimname (ncsrc, i, dimname)
                if (ierr .eq. nf_noerr) then
                  lvar=lenstr(dimname)
                  if (i .eq. unlimdimid) then
                    size=nf_unlimited
                  else 
                    ierr=nf_inq_dimlen  (ncsrc, i, size)
                  endif
                  if (ierr .eq. nf_noerr) then
                    ierr=nf_def_dim (nctarg, dimname(1:lvar), size,
     &                                                    dimid(i))
                    if (ierr .eq. nf_noerr) then
                      write(*,'(6x,3A,I4)') 'Created dimension ''',
     &                                dimname(1:lvar), '''=', size 
                    endif
                  else
                    write(*,'(/1x,4A/12x,3A/12x,A/)')  '### ERROR: ',
     &                     'Cannot determine length of dimension ''',
     &                      dimname(1:lvar),'''','in netCDF file ''',
     &                      src_file(1:lsrc),'''.',nf_strerror(ierr)
                  endif
                else
                  write(*,'(/1x,2A,I3/12x,3A/12x,A/)') '### ERROR: ',
     &                 'Cannot determine name for dimension ID =', i,
     &                 'in netCDF file ''', src_file(1:lsrc), '''.',
     &                  nf_strerror(ierr)  
                endif
              enddo
!
! Copy global attributes
!
              do i=1,ngatts
                ierr=nf_inq_attname (ncsrc, nf_global, i, attname)
                if (ierr .eq. nf_noerr) then
                  lvar=lenstr(attname)
                  ierr=nf_copy_att (ncsrc,nf_global,
     &                        attname(1:lvar), nctarg, nf_global)
                  if (ierr .eq. nf_noerr) then
                    write(*,'(8x,3A)')  'Copied global attribute ''',
     &                                        attname(1:lvar), '''.'
                  else
                    write(*,'(/1x,4A/12x,3A/12x,A/)')  '### ERROR: ',
     &             'Cannot copy global attribute ''',attname(1:lvar),
     &             '''', 'into netCDF file ''',   targ_file(1:ltarg),
     &                                      '''.', nf_strerror(ierr)
                  endif
                else
                  write(*,'(/1x,3A,I3/12x,3A/12x,A/)') '### ERROR: ',
     &             'Cannot determine name of global attribute with ',
     &             'ID =', i, 'from netCDF file ''',src_file(1:lsrc),
     &                                      '''.', nf_strerror(ierr)
                endif
              enddo
!
! Define variables and their attributes.
!
              do i=1,nvars
                ierr=nf_inq_var (ncsrc, i, varname,  vartype,
     &                           vardims, vardimids, varatts)
                if (ierr .eq. nf_noerr) then
                  lvar=lenstr(varname)
                  ierr=nf_def_var (nctarg, varname(1:lvar), vartype,
     &                                    vardims, vardimids, varid)
                  if (ierr .eq. nf_noerr) then
                    write(*,'(6x,3A)') 'Created variable ''',
     &                              varname(1:lvar), '''.'
                    do j=1,varatts
                      ierr=nf_inq_attname (ncsrc, i, j, attname)
                      lvar=lenstr(attname)
                      ierr=nf_copy_att (ncsrc, i,  attname(1:lvar),
     &                                              nctarg, varid)
                      if (ierr .eq. nf_noerr) then
                        write(*,'(8x,3A)') 'Copied attribute ''',
     &                                      attname(1:lvar), '''.'
                      else
                        write(*,'(/1x,5A/12x,3A/12x,A/)')
     &                         '### ERROR: Cannot copy attribute ''',
     &                          attname(1:lvar),''' for variable ''',
     &                          varname(1:lenstr(varname)), '''',
     &                         'in file ''', targ_file(1:ltarg),
     &                                    '''.',   nf_strerror(ierr)
                      endif
                    enddo
                  else
                    write(*,'(/1x,4A/12x,3A/12x,A/)')  '### ERROR: ',
     &                  'Cannot create variable ''', varname(1:lvar),
     &                  '''','in netCDF file ''', targ_file(1:ltarg),
     &                                      '''.', nf_strerror(ierr) 
                  endif
                else
                  write(*,'(/1x,2A,I3/12x,4A/12x,A/)') '### ERROR: ',
     &            'Cannot make general inquiry for variable ID =', i,
     &            'in ', 'netCDF file ''', src_file(1:lsrc),  '''.',
     &                                            nf_strerror(ierr)
                endif
              enddo

              ierr=nf_enddef(nctarg)
              write(*,'(4x,3A)')  'Left definition mode for ''',
     &                                 targ_file(1:ltarg), '''.'

#ifdef COPY_TIMELESS_VARS
!
! Copy variables which do not have unlimited dimension (these are
! most likely to be the environmental variables)
!
              do i=1,nvars
                ierr=nf_inq_var (ncsrc, i, varname,  vartype,
     &                           vardims, vardimids, varatts)
                if (ierr .eq. nf_noerr) then
                  size=1
                  do j=1,vardims
                    if (vardimids(j) .eq. unlimdimid) then
                      size=0
                    endif
                  enddo
                  if (size.gt.0) then
                    lvar=lenstr(varname)
                    ierr=nf_copy_var (ncsrc, i, nctarg)
                    if (ierr .eq. nf_noerr) then
                      write(*,'(6x,5A)') 'Copied ''',varname(1:lvar),
     &                        ''' into ''', targ_file(1:ltarg), '''.'
                    else
                      write(*,'(/1x,4A/12x,3A/12x,A)') '### ERROR: ',
     &                'Cannot copy variable ''',     varname(1:lvar),
     &                '''', 'into netCDF file''', targ_file(1:ltarg),
     &                                      '''.', nf_strerror(ierr) 
                    endif
                  endif
                else
                  write(*,'(/1x,2A,I3/12x,4A/12x,A/)') '### ERROR: ',
     &            'Cannot inquire variable ID =',   i,  'in netCDF ',
     &            'file ''',src_file(1:lsrc),'''.',nf_strerror(ierr)
                endif
              enddo
#endif

              deallocate (vardimids)
              deallocate (dimid)

              ierr=nf_close(nctarg)
              write(*,'(4x,3A)')  'Closed netCDF file ''',
     &                           targ_file(1:ltarg), '''.'
              ierr=nf_close(ncsrc)
            else
               write(*,'(/1x,4A/12x,A/)')  '### ERROR: Cannot make ',
     &                         'general inquiry into netCDF file ''',
     &                   src_file(1:lsrc), '''.',  nf_strerror(ierr)
            endif
          else
            write(*,'(/1x,4A/12x,A/)')   '### ERROR: Cannot create ',
     &                    'netCDF file ''', targ_file(1:lsrc), '''.',
     &                                             nf_strerror(ierr)
          endif
        else
          write(*,'(/1x,4A/12x,A/)')      '### ERROR: Cannot open ',
     &         'netCDF file ''', src_file(1:lsrc),''' for reading.',
     &                                            nf_strerror(ierr)
        endif
      else
        write(*,'(/1x,A/32x,A/)') 'Usage of nccpstruct should be: ',
     &                    'nccpstruct source_file.nc target_file.nc'
      endif
      stop 
      end


