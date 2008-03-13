      program nccp
!
! Generic tool to copy content of a netCDF variable from one file
! to another, where the variable is assumed to be apriori defined
! in both source and target files (hence this tool makes no attempt
! to redefine the structure of target netCDF file).
!
! Usage: nccp needs 3 arguments:
!-------
!               nccp source.nc target.nc varname
! where
!          soure.nc   is name of source netCDF file
!          target.nc             target netCDF file
!          varname               variable to be copied
!
! This code is organized as follows: The outer code, program nccp,
! is the driver which reads the source netCDF file. At first it
! checks, whether it is used correctly, i.e., all three arguments
! are present (if not, it simply prints instruction how to use it
! and quits).  Then it opens source netCDF file, makes general
! inquiry about the netCDF variable to be copied, determines its
! type (only real, double, integer types are allowed in the present
! version) and shape (i.e., number of dimensions and sizes of all
! of them), after which the total size of the variable is determined
! and a sufficient buffer array is allocated.  Then the whole
! variable is read into the  buffer, the source file is closed.
! Once the reading is complete, control is passed the the inner code,
! subroutine nccp_write, which does the writing of variable into the
! target file.  Before an attempt to write is done, it makes sure
! that the variable defined in the target file has the same type
! and shape, and if not it complains about the missmatch and quits. 
!
      implicit none
#include "netcdf.inc"
      character*80 ncsource, nctarget, varname 
      integer i, ierr, ncid, varid, vartype, vardims, varatts,
     &        dimids(8), dimsize(8), size, lstr, lvar, lenstr, iargc
      character*16 dimname(8)
      real*8,dimension(:),allocatable :: bffr 

      dimsize=1 ! <-- default 

      i=iargc()
      if (i.eq.3) then
        call getarg(1,ncsource)
        call getarg(2,nctarget)
        call getarg(3, varname)

        lstr=lenstr(ncsource)
        ierr=nf_open(ncsource, nf_nowrite, ncid)
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(varname)
          ierr=nf_inq_varid (ncid, varname, varid)
          if (ierr .eq. nf_noerr) then
            ierr=nf_inq_var (ncid, varid, varname, vartype,
     &                           vardims,  dimids, varatts)
            if (ierr .eq. nf_noerr) then
c**           write(*,'(A,3I4/A,8I4)') varname(1:lvar), vartype,
c**  &        vardims, varatts, 'dimids=', (dimids(i), i=1,vardims)
              size=1
              do i=1,vardims
                ierr=nf_inq_dimname (ncid, dimids(i), dimname(i))
                if (ierr .eq. nf_noerr) then
                  ierr=nf_inq_dimlen  (ncid, dimids(i), dimsize(i))
                  if (ierr .eq. nf_noerr) then
c**                 write(*,*)  dimname(i), dimsize(i)
                    size=size*dimsize(i)
                  else
                    write(*,'(/1x,2A,I3,A/12x,4A/)')   '### ERROR: ',
     &                'Cannot determine size of dimension with ID =',
     &                 dimids(i),     ' within netCDF',    'file ''',
     &                 ncsource(1:lstr), ''': ', nf_strerror(ierr) 
                    size=0
                  endif
                else
                  write(*,'(/1x,2A,I3,A/12x,4A/)')     '### ERROR: ',
     &                'Cannot determine name of dimension with ID =',
     &                 dimids(i),     ' within netCDF',    'file ''',
     &                 ncsource(1:lstr), ''': ', nf_strerror(ierr)
                  size=0
                endif
              enddo
c**           write(*,*) 'size =', size
              if (size.gt.0) then
                allocate(bffr(size)) 
                if (vartype .eq. nf_double) then
                  ierr=nf_get_var_double (ncid, varid, bffr)
                elseif (vartype .eq. nf_float) then
                  ierr=nf_get_var_real   (ncid, varid, bffr)
                elseif (vartype .eq. nf_int) then
                  ierr=nf_get_var_int    (ncid, varid, bffr)
                elseif (vartype .eq. nf_char) then
                  ierr=nf_get_var_text   (ncid, varid, bffr)
                else
                  write(*,'(/1x,2A)') '### ERROR: Cannot read: ',
     &                                'Variable of unknown type.'
                  ierr=-1000
                endif
                if (ierr .eq. nf_noerr) then
                  ierr=nf_close(ncid)
                  if (ierr .eq. nf_noerr) then
                    call nccp_write (ncsource, nctarget, varname,
     &                               bffr, size, vartype, vardims,
     &                               dimsize, dimname, varatts)
                  else
                    write(*,'(/1x,4A,1x,A/)')   '### ERROR: Cannot ',
     &                      'close netCDF file ''', ncsource(1:lstr),
     &                      ''':', nf_strerror(ierr)
                  endif
                else
                  write(*,'(/1x,6A/12x,A/)')    '### ERROR: Cannot ',
     &                   'read netCDF variable ''',  varname(1:lvar),
     &                   ''' from ''',    ncsource(1:lstr),    ''':', 
     &                    nf_strerror(ierr)
                endif
                deallocate(bffr)
              endif
            else
              write(*,'(/1x,2A,I3,A/12x,4A/)') '### ERROR: Cannot ',
     &                'make general inquiry for variable with ID =',
     &                 varid,     ' within',       'netCDF file ''',
     &                 ncsource(1:lstr), ''': ', nf_strerror(ierr)
            endif
          else
            write(*,'(/1x,4A/12x,4A/)')    '### ERROR: Cannot get ',
     &              'netCDF ID for variable ''',    varname(1:lvar),
     &              ''' within netCDF', 'file ''', ncsource(1:lstr),
     &              ''': ', nf_strerror(ierr)
          endif
        else
          write(*,'(/1x,4A,1x,A/)') '### ERROR: Cannot open netCDF ',
     &         'file ''', ncsource(1:lstr), ''':', nf_strerror(ierr)
        endif
      else
        write(*,'(/1x,A,1x,A/32x,A/)') 'The usage of ''nccp''',
     &        'should be:', 'nccp source.nc target.nc varname'
      endif
      stop
      end


      subroutine nccp_write (ncsource, nctarget, varname, bffr, size,
     &                vartype, vardims, dimsize,  dimname,  varatts)
      implicit none
      character*(*) ncsource, nctarget, varname
      integer size, vartype, vardims, varatts
      integer dimsize(vardims)
      character*16 dimname(8), dimname1(8) 
      real*8 bffr(size)
#include "netcdf.inc"
      character varname1*32
      integer ncid, varid, vartype1, varatts1, vardims1, dimids1(8),
     &        dimsize1(8), size1, i, ierr, lstr, lstr1, lvar, lenstr

      dimsize1=1 !<-- default size

      lstr=lenstr(ncsource)
      lstr1=lenstr(nctarget)
      ierr=nf_open(nctarget, nf_write, ncid)
      if (ierr .eq. nf_noerr) then
        lvar=lenstr(varname)
        ierr=nf_inq_varid (ncid, varname, varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_var (ncid, varid, varname1, vartype1,
     &                        vardims1,  dimids1, varatts1)
          if (ierr .eq. nf_noerr) then
            size1=1
            do i=1,vardims1
              ierr=nf_inq_dimname (ncid, dimids1(i), dimname1(i))
              if (ierr .eq. nf_noerr) then
                ierr=nf_inq_dimlen  (ncid, dimids1(i), dimsize1(i))
                if (ierr .eq. nf_noerr) then
                  if (dimsize1(i).eq.dimsize(i)) then
                    size1=size1*dimsize1(i)
                  else
                    write(*,'(/1x,A,I4,1x,6A,I5/33x,4A,I5/)')
     &             '### ERROR: Dimension #',   i,  'of variable ''',
     &              varname(1:lenstr(varname)),     ''' in source ',
     &             'file, ',       dimname(i)(1:lenstr(dimname(i))),
     &             ' =', dimsize(i), 'differs from that of target ',
     &             'file, ',    dimname1(i)(1:lenstr(dimname1(i))),
     &             ' =', dimsize1(i)
                    size1=0
                  endif
                else
                  write(*,'(/1x,3A,I3,1x,A/12x,4A/)')  '### ERROR: ',
     &                    'Cannot determine size of dimension with ',
     &                    'ID =',   dimids1(i),  'in target netCDF ',
     &                    'file ''',     nctarget(1:lstr1),   ''': ',
     &                     nf_strerror(ierr)
                  size1=0
                endif 
              else
                write(*,'(/1x,3A,I3,1x,A/12x,4A/)')  '### ERROR: ',
     &                  'Cannot determine name of dimension with ',
     &                  'ID =',   dimids1(i),   'in target netCDF',
     &                  'file ''',    nctarget(1:lstr1),   ''': ',
     &                   nf_strerror(ierr)
                size1=0
              endif
            enddo
            write(*,*) 'size of variable, source:',
     &                       size,'   target:',size1
            if (size1.gt.0) then
              if (vartype1.eq.vartype) then
                if (vartype .eq. nf_double) then
                  ierr=nf_put_var_double (ncid, varid, bffr)
                elseif (vartype .eq. nf_float) then
                  ierr=nf_put_var_real   (ncid, varid, bffr)
                elseif (vartype .eq. nf_int) then
                  ierr=nf_put_var_int    (ncid, varid, bffr)
                elseif (vartype .eq. nf_char) then
                  ierr=nf_put_var_text   (ncid, varid, bffr)
                else
                  write(*,'(/1x,2A)') '### ERROR: Cannot write: ',
     &                                'Variable of unknown type.' 
                  ierr=-1000
                endif
                if (ierr .eq. nf_noerr) then
                  ierr=nf_close(ncid)
                  if (ierr .eq. nf_noerr) then
                    write(*,'(/1x,7A/)') 'Copied netCDF variable ''',
     &               varname(1:lvar), ''' from ''', ncsource(1:lstr),
     &                  ''' into ''',   nctarget(1:lstr1),     '''.'
                  else
                    write(*,'(/1x,4A,1x,A/)')   '### ERROR: Cannot ',
     &                      'close netCDF file ''',nctarget(1:lstr1),
     &                      ''':',  nf_strerror(ierr)
                  endif
                else
                  write(*,'(/1x,6A/12x,A/)')    '### ERROR: Cannot ',
     &                   'write netCDF variable ''', varname(1:lvar),
     &                   ''' into ''',    nctarget(1:lstr1),   ''':',
     &                    nf_strerror(ierr)
                endif
              else
                write(*,'(/1x,4A,I4,A/25x,2A,I4,A/)')  '### ERROR: ',
     &          'Type of of variable ''', varname(1:lenstr(varname)),
     &          ''' in source file,', vartype, ',',  'differs from ',
     &          'that in target file,', vartype1, '.'
              endif
            endif
          else
            write(*,'(/1x,3A,I3/12x,3A,1x,A/)') '### ERROR: Cannot ',
     &              'make general inquiry for variable with netCDF ',
     &              'ID =',           varid,     'in target file ''',
     &               nctarget(1:lstr1), ''':',     nf_strerror(ierr)
          endif
        else
          write(*,'(/1x,4A/12x,3A,1x,A/)') '### ERROR: Cannot get ',
     &            'netCDF ID for variable ''',      varname(1:lvar),
     &            ''' within', 'target file ''',  nctarget(1:lstr1),
     &            ''':',  nf_strerror(ierr)
        endif
      else
        write(*,'(/1x,3A,1x,A/)') 'Cannot open netCDF file ''',
     &   nctarget(1:lstr1), ''' for writing:', nf_strerror(ierr)
      endif
      return
      end
