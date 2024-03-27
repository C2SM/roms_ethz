      program ncjoin

! Reads ROMS partial netCDF files and combines them into a file for
! the whole physical grid.  This operation is the inverse to partit.
! Basic usage:
!
!           ncjoin [-3 -d -h -c=<n> -o=<dir>] files.*.nc
!
! where files.*.nc matches a complete set of partial files (or
! several complete sets).
! "ncjoin -h" prints  more help on its usage.

! CPP-switch "FAST_MODE" activates mode in which partial arrays are
! assembled into a big array in operating memory, and the whole data
! is written into the disk as a one long record.  If not defined, the
! code relies on the netCDF capability to write rectangular sub-arrays
! via nf_put_vara_TYPE routines.  Because in hardware terms the latter
! results in writing multiple small records (with record length equal
! to the first fortran dimension of sub-array), it is considerably
! slower.

! CPP-switch "DEL_PART_FILES" enables the user-activated option to
! delete partial files just after they are joined.  This becomes
! the default. Only with the option "-k" the partial files are kept.
! The deletion occurs one-set-at-at-
! a-time and ONLY if the joining is successful, i.e., if an error of
! any kind occurs, so the set cannot be joined for whatever reason (a
! missing file, or one file has fewer records than others, or reading
! error; etc), the files stay intact.  If ncjoin is interrupted, the
! partial files stay intact.

! CPP-switch "ZLIB_COMPRESSION" (i) changes from 64-bit to netCDF-4
! (HDF5) format and (ii) activates z-lib compression.    In this case
! conversion is enabled at level 6 (default), but user can also supply
! command-line argument via --complevel=X or shorthand -c=X to change
! from the default, where X=0,...,9 (0 means no compression at all,
! 9 is the highest level).  This results in some savings in size of
! the resultant files (without loss of quality of data), however this
! comes at the expense of significant penalty in execution speed.

! CPP-switch "KEEP_CLOSED" forces regime in which to keep no more
! than one partial file for MPI-nodes 1:nnnodes-1 be opened at any
! given time (file belonging to MPI-node=0 is kept permanently open,
! so does the target file). This regime is needed to avoid netCDF
! internal memory limitation associated with opening too many files
! simultaneously.

! CPP-switch "AUTORENICE" forces automatic lowering priority of ncjoin
! to 19 (the lowest possible) to run it in "background" regime.

#define FAST_MODE
#define DEL_PART_FILES
#define ZLIB_COMPRESSION
!-- #define KEEP_CLOSED
!-- #define TIMING
!-- #define AUTORENICE
#define NCFILEFORMAT nf_netcdf4
#define CLOSE_NETCDF_AFTER_TREC


      implicit none
#include "netcdf.inc"
      integer, parameter :: maxdims=32, maxvars=256
      integer, parameter :: part_XI=1,  part_ETA=2, part_2D=3

      logical complete, clean_set,  digit,  var_mask,  lnewvar, is_in_list
      logical :: selective = .false., time_partition = .false.

      logical :: timing = .false., renice = .false.
     &         , fast = .true., quite = .false.
      integer nargs, nnodes,   size_XI,  XI_rho,  id_xi_rho,  id_xi_u,
     &        arg,   node,     size_ETA, ETA_rho, id_eta_rho, id_eta_v,
     &        ierr,  maxnodes, size_S,   tsize,   unlimdimid, rec,
     &        ntest, nctarg,   ndims,    code_size,           is,ie,
     &        nvars, ngatts,   varatts,  code_size_bak,        i,j,k,
     &        lsrc,  ltrg,     lvar,     lstr,lbak, lenstr, chk_alloc
      integer  ::  ldir=0, verbose = 0

      character(len=8) sffx, sffx_bak
      character(len=32) vname(maxvars), dimname(maxdims), suffix
      character(len=64) root, root_bak, string
      character(len=256) nctestname, nctargname, odir, inc_var_list
     &                    , ex_var_list

      character(len=128), allocatable, dimension(:) :: srcname

      integer, allocatable, dimension(:) :: ncsrc, xi_start, eta_start
      integer, allocatable, dimension(:,:) :: vid, dimsize
      logical, allocatable, dimension(:) :: western_edge, eastern_edge,
     &                                     southern_edge, northern_edge

      logical series(maxvars)
      integer, dimension(maxvars) :: varid,   vnode,  vdims,
     &                               vtype, part_type
      integer, dimension(maxdims) :: dimid,   ldim,   ibuff,
     &                               start,   count,  start1
      integer, dimension(maxdims,maxvars) :: dimids

! Buffer arrays for reading and writing, and their allocated sizes

      integer size, size_cbfr,  size_ibfr1, size_ibfr2,
     &              size_ibffr, size_bffr4, size_bffr8

      integer :: alloc_cbfr=0,  alloc_ibfr1=0, alloc_ibfr2=0,
     &           alloc_ibffr=0, alloc_bffr4=0, alloc_bffr8=0

      character(len=1), allocatable, dimension(:) :: cbfr
      integer(kind=1),  allocatable, dimension(:) :: ibfr1
      integer(kind=2),  allocatable, dimension(:) :: ibfr2
      integer(kind=4),  allocatable, dimension(:) :: ibffr
      real(kind=4), allocatable, dimension(:) :: bffr4
      real(kind=8), allocatable, dimension(:) :: bffr8

#ifdef FAST_MODE
      integer, dimension(maxdims) :: count1
      integer size1, size_cbf_out,  size_ibf1_out, size_ibf2_out,
     &               size_ibff_out, size_bff4_out, size_bff8_out

      integer :: allc_cbf_out=0,  allc_ibf1_out=0, allc_ibf2_out=0,
     &           allc_ibff_out=0, allc_bff4_out=0, allc_bff8_out=0

      character(len=1), allocatable, dimension(:) :: cbf_out
      integer(kind=1),  allocatable, dimension(:) :: ibf1_out
      integer(kind=2),  allocatable, dimension(:) :: ibf2_out
      integer(kind=4),  allocatable, dimension(:) :: ibff_out
      real(kind=4), allocatable, dimension(:) :: bff4_out
      real(kind=8), allocatable, dimension(:) :: bff8_out
#endif
      integer :: minrec=0 ! minrec>0 do not enforce same record number
                          ! in each partial file but use minimum.
      integer :: nstart = 1, nend=0 

      character(len=9), parameter :: signame='nc_format'
#ifdef ZLIB_COMPRESSION
      character(len=25), parameter :: signature =
     &                       'netCDF-4, zlib-compressed'
      integer :: shuffle=1, deflate=1, deflate_level=1 ! <-- default
     &           , ncVersion=4 
#endif
#ifdef DEL_PART_FILES
      logical del_part_files
      character(len=128) rmcmd
#endif
#ifdef CLOSE_NETCDF_AFTER_TREC
      integer :: rec_close = 10
#endif
#ifdef TIMING
      real(kind=4) tstart, RUN_time, CPU_time(2)
      real(kind=8) InitTime, ReadSize, ReadTime, WrtSize,  WrtTime,
     &                                           SyncTime, GrayTime
      integer(kind=4) iclk(2), nclk, clk_rate, clk_max, vtypsize
      integer(kind=8) inc_clk
      integer(kind=8) :: net_clk=0, net_read_size=0, net_wrt_size=0,
     &              net_init_clk=0,  net_read_clk=0,  net_wrt_clk=0,
     &                              net_sync_clk=0,  net_gray_clk=0
# ifdef DEL_PART_FILES
     &                                            , net_rmcmd_clk=0
# endif
#ifdef FAST_MODE
      real(kind=8)  AssmTime
      integer(kind=8) :: net_assm_clk=0
#endif
# ifdef __IFC
      real(kind=4) etime
# endif
#endif
                    /* Function "iargc" is intrinsic in most modern */
#ifdef XLF          /* compilers and does not need to be declared.  */
      integer iargc /* IBM xlf95 is a notable exclusion. So do 7.x  */
#endif              /* and earlier versions of Intel compiler, but  */
                    /* it is intrinsic in 9.x and later versions.   */
!-- #ifdef AUTORENICE
      integer getpid, pid            ! Sometimes it makes sense to
      character(len=32) cmd            ! run ncjoin in the background
#if 0
      if (renice) then
        pid=getpid()                   ! mode with lowered priority to
        write(cmd,'(I8)') pid          ! not interfere with running MPI
        lstr=lenstr(cmd)               ! job. This code segment catches
        cmd(11:lstr+10)=cmd(1:lstr)    ! pid of its own process and
        lstr=lstr+10                   ! executes re-nice command.
        cmd(1:10)='renice 19'
        if (verbose)  
     &  write(*,'(/3A//)') 'Autorenice: executing ''',cmd(1:lstr),'''.'
        call system(cmd(1:lstr))
      endif
#endif
!-- #endif
#ifdef TIMING
      if (timing) then
# ifdef __IFC
      tstart = etime(CPU_time)
# else
      call etime(CPU_time, tstart)
# endif
      nclk=1
      call system_clock(iclk(nclk), clk_rate, clk_max)
      endif 
#endif
#ifdef DEL_PART_FILES
      !del_part_files=.false.
      del_part_files=.true.
#endif

      ntest=-1 ; maxnodes=-1

      nargs=iargc() ; arg=0
      if ( nargs < 1 ) then
        call usage
        call exit(-1)
      endif

! Extract a set of files which cover the whole physical grid.

  1   nnodes=-1                        ! Reset variables which
      root_bak(1:1)=' '                ! define the file set.
      sffx_bak(1:1)=' '
      code_size_bak=-1

  2    arg=arg+1
        call getarg(arg,nctestname)
        lsrc=lenstr(nctestname)
#ifdef DEL_PART_FILES
        if ( nctestname(1:1) == '-' .and. (
     &       lsrc == 2 .and. nctestname(1:2) == '-k' .or.
     &       lsrc == 6 .and. nctestname(1:6) == '--keep' )) then
          print'(1x,A)', '---> Keeping partial files'
          ! write(*,'(2A/)') '>>>> Flag to delete partial files ',
          ! &                                                 'is raised.'
          ! del_part_files=.true.
          del_part_files=.false.
          goto 2
        endif
#endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc == 2  .and. nctestname(1:2) == '-n' )) then
          renice=.true.
          goto 2
        endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc == 2  .and. nctestname(1:2) == '-p' )) then
          minrec=1
          goto 2
        endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 3  .and. nctestname(1:3) == '-s=' )) then
          read(nctestname(4:LEN_TRIM(nctestname)),*) nstart
          print*, 'First record: ', nstart
          time_partition=.true.
          del_part_files=.false.
          goto 2
        endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 3  .and. nctestname(1:3) == '-e=' )) then
          read(nctestname(4:LEN_TRIM(nctestname)),*) nend
          print*, 'Last record: ', nend
          time_partition=.true.
          del_part_files=.false.
          goto 2
        endif

        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 3  .and. nctestname(1:3) == '-i=' )) then
        selective=.true.
          inc_var_list = nctestname(4:LEN_TRIM(nctestname))
          print'(2A)', 
     &       ' ---> List of variables requested in output: '
     &        , trim(inc_var_list)
          inc_var_list = trim(inc_var_list) / / ','
          goto 2
        endif

#ifdef CLOSE_NETCDF_AFTER_TREC
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 10 .and. nctestname(1:10) == '--ncclose=' )) then
          read(nctestname(11:LEN_TRIM(nctestname)),*) rec_close
	  print *, 'Frequency of closing netcdf files [time records]: ', rec_close
	  goto 2
	endif
#endif

# if 1 == 0
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 3  .and. nctestname(1:3) == '-x=' )) then
          ex_var_list = nctestname(4:LEN_TRIM(nctestname))
          print'(2A)', 
     &        ' ---> List of variables to exclude in output: '
     &        , trim(ex_var_list)
          ex_var_list = trim(ex_var_list) / / ','
          goto 2
        endif
#endif
           
#ifdef ZLIB_COMPRESSION
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc == 2  .and. nctestname(1:2) == '-3' )) then
          ncVersion=3
        endif
        if (ncVersion < 4 ) deflate_level=0

        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 3  .and. nctestname(1:3) == '-c=' .or.
     &       lsrc > 12 .and. nctestname(1:12) == '--complevel=' )) then
          i=ichar(nctestname(lsrc:lsrc))-48
          if (i == 0) then
            shuffle=1 ; deflate=0 ; deflate_level=0
            write(*,'(1x,A,I2)') '---> No compression. level =', i
          elseif (0 < i .and. i <= 9) then
            shuffle=1 ; deflate=1 ; deflate_level=i
            write(*,'(1x,A,I2)') '---> Set compression level =', i
          else
            write(*,'(1x,2A)') '### ERROR: invalid argument: ',
     &                   'compression level must be from 0 to 9.'
            stop
        endif
          goto 2
        endif
#endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 3  .and. nctestname(1:3) == '-o=' )) then
          ldir=lenstr(nctestname)
          odir=nctestname(4:ldir)
          ldir=ldir-3
          print*, 'Output dir: ', odir
          goto 2
        endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 1  .and. nctestname(1:2) == '-h') ) then
          call usage
          call exit(0)
        endif
        if ( nctestname(1:1)=='-' .and. (
     &       lsrc > 1  .and. nctestname(1:2) == '-v') ) then
           verbose = 1
           if (trim(nctestname) == '-vv') verbose = 2
           if (trim(nctestname) == '-vvv') verbose = 3
           print*,' Verbosity = ',verbose
           goto 2
        endif
        if ( nctestname(1:1)=='-' .and. 
     &      (lsrc > 1  .and. index('khsenpqvcix',nctestname(2:2))==0)
     &     )  then
          print'(/3A/)', '### ERROR Unknown option >'
     &                   , nctestname(1:2), '<'
          call usage
          call exit(1)
        endif

      if (renice) then
        pid=getpid()                   ! mode with lowered priority to
        write(cmd,'(I8)') pid          ! not interfere with running MPI
        lstr=lenstr(cmd)               ! job. This code segment catches
        cmd(11:lstr+10)=cmd(1:lstr)    ! pid of its own process and
        lstr=lstr+10                   ! executes re-nice command.
        cmd(1:10)='renice 19'
        if (verbose>0)  
     &  write(*,'(/3A//)') 'Autorenice: executing ''',cmd(1:lstr),'''.'
        call system(cmd(1:lstr))
      endif

        if (ntest /= -1) then
          ierr=nf_close(ntest) ; ntest=-1
        endif
        ierr=nf_open(nctestname, nf_nowrite, ntest)
        if (ierr == nf_noerr) then
          ierr=nf_inq_att (ntest, nf_global, 'partition', i, lvar)
          if (ierr == nf_noerr) then
            if (i == nf_int .and. lvar == 4) then
              ierr=nf_get_att_int (ntest,nf_global,'partition',ibuff)
              if (ierr == nf_noerr) then

                if (nnodes == -1) then
                  nnodes=ibuff(2)
                  if (nnodes > maxnodes) then
                    maxnodes=nnodes
                    if (allocated(ncsrc)) then
                      deallocate(dimsize)
                      deallocate(vid)
                      deallocate(northern_edge)
                      deallocate(southern_edge)
                      deallocate(eastern_edge)
                      deallocate(western_edge)
                      deallocate(eta_start)
                      deallocate(xi_start)
                      deallocate(srcname)
                      deallocate(ncsrc)
                    endif
                    allocate(ncsrc(0:nnodes-1))
                    allocate(srcname(0:nnodes-1))
                    allocate(xi_start(0:nnodes-1))
                    allocate(eta_start(0:nnodes-1))
                    allocate(western_edge(0:nnodes-1))
                    allocate(eastern_edge(0:nnodes-1))
                    allocate(southern_edge(0:nnodes-1))
                    allocate(northern_edge(0:nnodes-1))
                    allocate(vid(maxvars,0:nnodes-1))
                    allocate(dimsize(maxdims,0:nnodes))
                  endif
                                          ! Reset variables defining
                  complete=.false.        ! a complete set of partitial
                  do node=0,nnodes-1      ! files. These variables will
                    ncsrc(node)=-1        ! receive meaningful values
                    xi_start(node)=-1     ! from data read from netCDF
                    eta_start(node)=-1    ! file headers, subsequently
                  enddo                   ! be used to verify the set
                                          ! completeness.

                elseif (nnodes /= ibuff(2)) then
                  write(*,'(/1x,2A,I4/14x,3A/14x,A,I4,4x,A/)')
     &                 '### WARNING: Number of MPI nodes in global ',
     &                 'attribute ''partition'', nnodes =', ibuff(2),
     &                 'in netCDF file ''',       nctestname(1:lsrc),
     &                 ''' contradicts that from the initial',
     &                 'file in the sequence, nnodes =',      nnodes,
     &                                   ' ==> The file is ignored.'
                  arg=arg-1
                  goto 5
                endif

                node=ibuff(1)
                if (ncsrc(node) /= -1) then
                  write(*,'(/1x,2A,I4,1x,A)') '### ERROR: netCDF ID ',
     &                         'for file corresponding to MPI-node =',
     &                                   node,  'is already in use.'
                    stop
                  endif

                if (ncsrc(node) == -1 .and. xi_start(node) == -1 .and.
     &                                     eta_start(node) == -1) then
                  ncsrc(node)=ntest       ; srcname(node)=nctestname
                  xi_start(node)=ibuff(3) ; eta_start(node)=ibuff(4)

#define ntest illegal


! Lexical analysis of the file name: it is expected that the name
! consists of root name (eg.: "history"), a segment storing an integer
! number (eg., 03) which is  MPI rank written this file, and a suffix
! (eg.: ".nc").  Files which belonging to the same set normally have
! the same root and suffix, as well as the same number of digits in
! the MPI rank segment in the filename.  In addition to that MPI rank
! determined from its segment in file name should match the similar
! number in global attribute "partition".  Sometimes file names may
! have more than one digital segment (e.g., his.1234.08.nc) containing
! digits. In these cases MPI-rank is always the last segment.

                                               ! Determine positions
                  digit=.false.                ! of starting and ending
                  is=0 ; ie=0                  ! characters of MPI node
                  i=lsrc+1                     ! segment (is:ie)
                  do while (is==0 .and. i>1)
                    i=i-1
                    if (nctestname(i:i) >= '0' .and.
     &                  nctestname(i:i) <= '9') then
                      if (.not.digit) then
                        if (i < lsrc) then
                          if (nctestname(i+1:i+1) == '.') then
                            ie=i ; digit=.true.
                          endif                  ! node segment and
                        else                     ! suffix are separated
                          ie=i  ; digit=.true.   ! by dot (.) except if
                        endif                    ! there is no-suffix
                        endif
                    elseif (digit .and. nctestname(i:i)=='.') then
                      is=i+1 ; digit=.false.
                    endif
                  enddo

                  if (is > 0 .and. ie >= is) then
                    root=nctestname(1:is-1)
                    if (ie < lsrc) then          ! Extract common
                      sffx=nctestname(ie+1:lsrc) ! part of file names,
                    else                         ! MPI node number
                      sffx(1:1)=' '              ! and suffix (if any)
                    endif
                    k=0
                    do i=is,ie
                      k=10*k + ichar(nctestname(i:i))-48
                    enddo
                    code_size=ie-is+1
                  else
                    write(*,'(/1x,3A/)')        '### ERROR: Cannot ',
     &                 'determine MPI node number from file name ''',
     &                                     nctestname(1:lsrc), '''.'
                  endif
      if (verbose>1)  write(*,'(1x,3A,I3,1x,A,I4,1x,A,I4,3x,A,2I4)')
     &           'fname = ''', nctestname(1:lsrc),  ''' code_size =',
     &            code_size,  'code =', k, 'node =', node, 'i,jSW =',
     &                              xi_start(node), eta_start(node)

! Checking consistency of root name with previously found.

                  ierr=nf_noerr
                  if (root_bak(1:1) == ' ') then
                    root_bak=root
                  else
                    lvar=lenstr(root) ; lbak=lenstr(root_bak)
                    if (lvar /= lbak .or. root /= root_bak) then
                      ierr=ierr+1
                      if (.not. quite)
     &                 write(*,'(/8x,6A/17x,3A/)') 'WARNING: file ''',
     &                     nctestname(1:lsrc),   ''' has different ',
     &                    'root name ''',  root(1:lvar),   ''' than',
     &                   'previously found root name ''',
     &                   root_bak(1:lbak),   ''' from the same set.'
                    endif
                  endif

! Checking consistency of suffix with previously found..

                  if (sffx_bak(1:1) == ' ') then
                    sffx_bak=sffx
                  else
                    lvar=lenstr(sffx) ; lbak=lenstr(sffx_bak)
                    if (lvar /= lbak .or. sffx /= sffx_bak) then
                      ierr=ierr+1
                      write(*,'(/8x,7A/17x,3A/)') 'WARNING: ',
     &                  'file ''',  nctestname(1:lsrc),   ''' has ',
     &                  'different suffix name ''',    sffx(1:lvar),
     &                  ''' than','previously found suffix name ''',
     &                  sffx_bak(1:lbak),   ''' from the same set.'
                    endif
                  endif

! Checking consistency of length of node number segment

                  if (code_size_bak == -1) then
                    code_size_bak=code_size
                  elseif (code_size /= code_size_bak) then
                    ierr=ierr+1
                    write(*,'(/8x,A,I2,1x,3A/17x,A,I2,A/)')
     &              'WARNING: Number of digits in MPI node segment',
     &                 code_size, 'in ''', nctestname(1:lsrc), '''',
     &                    'is different than previously determined',
     &              code_size_bak, '.'
                  endif
 
! Checking consistency of node number with the file name.
 
                  if (k /= node) then
                    ierr=ierr+1
                    write(*,'(/8x,3A,I3/17x,2A/17x,A,I3,A/)')
     &                   'WARNING: file ''', nctestname(1:lsrc),
     &                   ''' belongs to different MPI node',   node,
     &                   '(as determined from its global attribute',
     &                   '''partition'')', 'than node', k,
     &                   ' determined from to the file name.'
                  endif

! Stop, if something is wrong.

                  if (ierr /= nf_noerr) goto 97
                else
                  arg=arg-1
                  goto 5
                endif
              else
                write(*,'(/1x,2A/14x,3A/)')     '### WARNING: Cannot ',
     &             'aquire global attribute ''partition'' from netCDF',
     &                                   'file ''', nctestname(1:lsrc),
     &                                 '''. ==> This file is ignored.'
              endif
            else
              write(*,'(/1x,2A/14x,3A/)')   '### WARNING: Wrong type ',
     &                 'or size of global attribute ''partition'' in ',
     &                            'netCDF file ''', nctestname(1:lsrc),
     &                                 '''. ==> This file is ignored.'
            endif
          else
            write(*,'(/1x,3A/)') '### WARNING: ''', nctestname(1:lsrc),
     &      ''' is not a partial netCDF file: ==> The file is ignored.'
          endif
        else
          write(*,'(/1x,4A/14x,A/)')     '### WARNING: Cannot open ''',
     &                     nctestname(1:lsrc), ''' as a netCDF file: ',
     &                  nf_strerror(ierr), ' ==> The file is ignored.'
        endif

#define nctestname illegal

! Verify, whether srcname(0:nnodes-1) and ncsrc(0:nnodes-1) > 0 (i.e.,
! successfully opened for reading) comprise a complete set of partial
! files.  Keep searching, if not.

   5    continue
        if (nnodes > 0) then
          complete=.true.
        do node=0,nnodes-1
            if (ncsrc(node) < 0) complete=.false.
        enddo
        endif
      if (.not.complete .and. arg < nargs) goto 2  !--> next file


! Once a complete set is identified, print the finenames.

        if (complete) then
        lsrc=lenstr(srcname(0))
          write(*,'(2(1x,A,I4),1x,A,2x,A,2I5)') 'Processing set of ',
     &                         nnodes, 'files', 0, srcname(0)(1:lsrc),
     &                          'i,jSW =', xi_start(0), eta_start(0) 
          do node=1,nnodes-1
          if (node < 8 .or. (nnodes > 8 .and. node > nnodes-8)) then
              write(*,'(29x,I4,1x,A,2x,A,2I5)') node,
     &                  srcname(node)(1:lsrc), 'i,jSW =',
     &                              xi_start(node), eta_start(node)
          elseif (nnodes>8 .and. node<10) then
              write(*,'(24x,A)') '.................................'
            endif
          enddo

#undef ntest
        if (ntest /= -1) then       ! Thus far netCDF file id array
          ierr=nf_close(ntest)      ! "ncsrc(0:nnodes-1)" was used just
          ntest=-1                  ! to signal that a complete set of
        endif                       ! partitioned files has been
        do node=0,nnodes-1          ! identified, but all the files are
          ncsrc(node)=-1            ! actually closed at this moment.
        enddo                       ! Reset the ids accordingly.
      elseif (arg < nargs) then
          goto 1
        else
        write(*,*) 'stop at 497'
          stop
        endif



!      *****    *********    ******   *******    *********
!    ***   ***  *  ***  *   **  ***   ***   ***  *  ***  *
!    ***           ***     **   ***   ***   ***     *** 
!      *****       ***    ***   ***   ***   **      ***
!          ***     ***    *********   ******        ***
!    ***   ***     ***    ***   ***   ***  **       ***
!      *****       ***    ***   ***   ***   ***     ***

  
! At this moment a set of files recorded as srcname(0:nnodes-1),
! xi_start(0:nnodes-1), eta_start(0:nnodes-1) comprise a complete
! set, but the files are actually in "closed" state and all netCDF
! IDs are reset to -1.

! Verify that ndims, ngatts, unlimdimid are the same for all nodes,
! however, note that different files may store different composition
! of variables, and netCDF variable IDs for the same variable (with
! the same name) may be different across the set of files.

      do node=0,nnodes-1
        lsrc=lenstr(srcname(node))
        if (ncsrc(node) == -1) ierr=nf_open(srcname(node),nf_nowrite,
     &                                                   ncsrc(node))
        if (ierr == nf_noerr) then
          ierr=nf_inq(ncsrc(node), ibuff(1), ibuff(2),
     &                             ibuff(3), ibuff(4))
          if (ierr /= nf_noerr) then
            write(*,'(/1x,4A/12x,A/)')     '### ERROR: Cannot make ',
     &                         'general inquiry into netCDF file ''',
     &             srcname(node)(1:lsrc), '''.', nf_strerror(ierr)
            goto 97
          elseif (ibuff(1) > maxdims) then
            write(*,'(/1x,2A,I4,1x,3A/12x,2A/)')     '### ERROR: ',
     &       'number of dimensions', ibuff(1), 'in netCDF file ''',
     &        srcname(node)(1:lsrc),    '''',   'exceeds limit.  ',
     &        'Increase parameter maxdims in file "ncjoin.F".'
            goto 97
          elseif (ibuff(2) > maxvars) then
            write(*,'(/1x,2A,I4,1x,3A/12x,2A/)')     '### ERROR: ',
     &       'number of variables',  ibuff(2), 'in netCDF file ''',
     &        srcname(node)(1:lsrc),  '''',      'exceeds limit. ',
     &        'Increase parameter maxvars in file "ncjoin.F".'
            goto 97
          elseif (node == 0) then
            ndims=ibuff(1)
c**         nvars=ibuff(2)
            ngatts=ibuff(3)
            unlimdimid=ibuff(4)
          else
            if (ibuff(1) /= ndims) then
              write(*,'(/4x,4A/15x,3A/)')     '### ERROR: netCDF ',
     &                  'file ''', srcname(node)(1:lsrc), ''' has ',
     &                  'different number of dimensions than ''',
     &                                     srcname(0)(1:lstr), '''.'
              ierr=ierr+1
          endif
c**         if (ibuff(2) /= nvars) then
c**           write(*,'(/4x,4A/15x,3A/)')     '### ERROR: netCDF ',
c**  &                 'file ''', srcname(node)(1:lsrc), ''' has ',
c**  &                     'different number of variables than ''',
c**  &                                    srcname(0)(1:lstr), '''.'
c**           ierr=ierr+1
c**         endif
            if (ibuff(3) /= ngatts) then
              write(*,'(/4x,4A/15x,3A/)')     '### ERROR: netCDF ',
     &                'file ''',  srcname(node)(1:lsrc), ''' has ',
     &                'different number of global attributes than ''',
     &                                     srcname(0)(1:lstr),'''.'
              ierr=ierr+1
            endif
            if (ibuff(4) /= unlimdimid) then
              write(*,'(/4x,4A/15x,3A/)')     '### ERROR: netCDF ',
     &                 'file ''', srcname(node)(1:lsrc), ''' has ',
     &              'different ID for unlimited dimension than ''',
     &                                    srcname(0)(1:lstr), '''.'
              ierr=ierr+1
            endif
            if (ierr /= nf_noerr) goto 97
          endif
   

! Verify that the sequence of dimension names is consistent throughout
! the entire set of variables.

#define i illegal
          do j=1,ibuff(1)
            ierr=nf_inq_dim(ncsrc(node), j, string, dimsize(j,node))
            if (ierr == nf_noerr) then
              lstr=lenstr(string)
              if (node==0) then
                ldim(j)=lstr
                dimname(j)=string(1:lstr)
              elseif (lstr/=ldim(j) .or. string(1:lstr)/=
     &                               dimname(j)(1:ldim(j)) )then
                write(*,'(/1x,2A,I3,3A/12x,6A/12x,3A/)')    '### ',
     &               'ERROR: Name of dimension #', j, ', named ''',
     &                string(1:lstr),   ''' in netCDF file',  '''',
     &                srcname(node)(1:lsrc),  ''' does not match ',
     &               'name ''',  dimname(j)(1:ldim(j)), ''' with ',
     &                'the corresponding name from netCDF file ''',
     &                                srcname(0)(1:lsrc),    '''.'
              goto 97
              endif
            else
              write(*,'(/1x,2A,I3/12x,3A/12x,A)')    '### ERROR: ',
     &          'Cannot determine name and size of dimension #', j,
     &                 'in netCDF file ''',  srcname(node)(1:lsrc),
     &                                    '''.', nf_strerror(ierr)
              goto 97
            endif
          enddo
#undef i

! Create catalog of variables:   names, IDs, and ranks (number of
! dimensions) throughout the entire set. The meaning of the arrays
! defined here is as follows:
!
!     nvars    -- total number of variables discovered;
!     vname(i), where i=1:nvars -- variable name;
!     vid(i,node) -- netCDF ID for that variable in netCDF file for
!                      MPI node "node", where node=0:nnodes-1;
!     vdims(i) -- "rank", i.e. number of dimensions of that variable;
!     vnode(i) -- the file index for the lowest MPI node where the
!                 variable has been found, or has achieved first time
!                 its full rank (this is needed to differentiate
!                 between the true variable and its proxy "dummy
!                 scalar", if partit creates one, e.g., in the case
!                 of boundary forcing variable in inner MPI node,
!                 where it is not needed).
          if (node == 0) then 
            nvars=0
            print*,'Building variable catalog...'
          endif
          do i=1,ibuff(2)
            ierr=nf_inq_varname(ncsrc(node), i, string)
            if (ierr == nf_noerr) then
              lstr=lenstr(string)
              ierr=nf_inq_varndims(ncsrc(node), i, k)
              if (ierr == nf_noerr) then
                lnewvar=.true.
                do j=1,nvars
                  lvar=lenstr(vname(j))
                  if (lstr == lvar .and.
     &                string(1:lstr) == vname(j)(1:lvar)) then
                    lnewvar=.false.
                    vid(j,node)=i
                    if (k > vdims(j)) then
                      vdims(j)=k ; vnode(j)=node
                    endif
                  endif
                enddo
                if (selective) then
                  if (lnewvar .and. (LEN_TRIM(inc_var_list) > 0) ) then
                   lnewvar = is_in_list(inc_var_list,string)
                  endif
!                if (lnewvar .and. (LEN_TRIM(ex_var_list) > 0) )
!     &             lnewvar = ( .not. is_in_list(ex_var_list,string) )
                endif
                if (lnewvar) then
                  nvars=nvars+1  ; vid(nvars,node)=i
                  vname(nvars)=string(1:lstr)
                  print*,'Added ',trim(string),' to output vars'
                  vnode(nvars)=node ; vdims(nvars)=k
                endif
              else
                write(*,'(/1x,3A,I3/12x,5A/12x,A)')     '### ERROR: ',
     &                    'Cannot determine number of dimensions for ',
     &                    'variable #', i, 'named ''',  string(1:lstr),
     &                   ''' in netCDF file ''', srcname(node)(1:lsrc),
     &                                      '''.', nf_strerror(ierr)
                goto 97
              endif
            else
              write(*,'(/1x,2A,I3/12x,3A/12x,A)') '### ERROR: Cannot ',
     &           'determine name of variable #', i,'in netCDF file ''',
     &                 srcname(node)(1:lsrc), '''.', nf_strerror(ierr)
              goto 97
            endif
            enddo
#ifdef KEEP_CLOSED
          if (node > 0) then                   ! Close all the files,
            ierr=nf_close(ncsrc(node))         ! except for MPI rank=0
            ncsrc(node)=-1
              endif
#endif
            else
          write(*,'(/1x,A,1x,3A/14x,A)' )    '### ERROR: Cannot open ',
     &                         'netCDF file ''', srcname(node)(1:lsrc),
     &                            ''' for reading.', nf_strerror(ierr)
              goto 97
            endif
      enddo  !<-- node=0,nnodes-1

      if (verbose)  then
        write(*,'(/1x,A,I3)') 'Inventory of variables: nvars =', nvars
        do i=1,nvars
          lvar=lenstr(vname(i))
          write(*,'(4I4,2x,3A)') i, vid(i,vnode(i)), vnode(i), vdims(i),
     &                                   '''', vname(i)(1:lvar), ''''
        enddo
        write(*,*) '...............................'
      endif

! Determine sizes of dimensions for combined file: For partitionable
! dimensions 'xi_rho', 'xi_u', 'eta_rho' and 'eta_v' determine the
! extent of the physical grid in each direction as the maximum over all
! subdomains of the dimension of each partial file combined with its
! starting index, "xi_start" or "eta_start".   This is straightforward
! for RHO-points, but for U- and V-dimensions it requires to take into
! account the fact that the subdomains adjacent to eastern or southern
! edge have one point less than the corresponding RHO-dimension.
! Consequently, all subsequent subdomains receive one-point shift.
! For all other dimensions, verify that the sizes are the same for
! all nodes.  Also find size of unlimited dimension, if it is present.
! Note that variable "tsize" is set to its default value 1 (meaning
! one record), and may or may not be overwritten by the actual size of
! unlimited dimension (if it exists). If the unlimited dimension does
! not exist, it retains its value of 1 so that the loop over records
! is still executed, but only once.

      XI_rho=0  ;  id_xi_rho=0  ; id_xi_u=0  ; size_XI=1  ; size_S=1
      ETA_rho=0 ;  id_eta_rho=0 ; id_eta_v=0 ; size_ETA=1 ; tsize=1

          do i=1,ndims
            dimsize(i,nnodes)=0
            lvar=lenstr(dimname(i))
        if (lvar == 6 .and. dimname(i)(1:lvar) == 'xi_rho') then
              id_xi_rho=i
              do node=0,nnodes-1
                dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                 dimsize(i,node) +xi_start(node)-1 )
                size_XI=max(size_XI,dimsize(i,node))
                XI_rho=max(XI_rho, dimsize(i,nnodes))
              enddo
           
        elseif (lvar == 4 .and.dimname(i)(1:lvar) == 'xi_u') then
              id_xi_u=i
              do node=0,nnodes-1
            if (xi_start(node) > 1) then
                  dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                   dimsize(i,node) +xi_start(node)-2 )
                else
                  dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                                    dimsize(i,node) )
                endif
                size_XI=max(size_XI,dimsize(i,node))
                XI_rho=max(XI_rho, dimsize(i,nnodes)+1)
              enddo

        elseif (lvar == 7.and. dimname(i)(1:lvar) == 'eta_rho') then
              id_eta_rho=i
              do node=0,nnodes-1
                dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                dimsize(i,node) +eta_start(node)-1 )
                size_ETA=max(size_ETA,dimsize(i,node))
                ETA_rho=max(ETA_rho, dimsize(i,nnodes))
              enddo

        elseif (lvar == 5 .and. dimname(i)(1:lvar) == 'eta_v') then
              id_eta_v=i
              do node=0,nnodes-1
            if (eta_start(node) > 1) then
              dimsize(i,nnodes)=max( dimsize(i,nnodes), dimsize(i,node)
     &                                             +eta_start(node)-2 )
                else
              dimsize(i,nnodes)=max(dimsize(i,nnodes), dimsize(i,node))
                endif
                size_ETA=max(size_ETA,dimsize(i,node))
                ETA_rho=max(ETA_rho, dimsize(i,nnodes)+1)
              enddo

            else
              dimsize(i,nnodes)=dimsize(i,0)
              do node=1,nnodes-1
                if (minrec >0 ) then
                  tsize=max(1,min(dimsize(i,node),tsize))
                else
                if (dimsize(i,0)/=dimsize(i,node)) then
                  lsrc=lenstr(srcname(node))
                  write(*,'(/1x,A,I3,3A,I4,1x,A/12x,4A/12x,3A,I4,A/)')
     &               '### ERROR: Nonpartitionable dimension #',  i,
     &               ' named ''', dimname(i)(1:lvar), ''', size =',
     &                dimsize(i,node),   'in netCDF',    'file ''',
     &                srcname(node)(1:lsrc),   ''' has different ',
     &               'size than the corresponding',
     &               'dimension from file ''',  srcname(0)(1:lsrc),
     &                   ''', which has size =', dimsize(i,0), '.'
                  write(*,*) ' Possible remedy: option "-p"'
                    goto 97
                endif
                endif
              enddo
          if (lvar == 5 .and. dimname(i)(1:lvar) == 's_rho') then
                size_S=max(size_S, dimsize(i,0))
          elseif (lvar == 3.and.dimname(i)(1:lvar) == 's_w') then
                size_S=max(size_S, dimsize(i,0))
              endif
            endif
        if (i == unlimdimid) then
              tsize=dimsize(i,nnodes)
              dimsize(i,nnodes)=nf_unlimited
            endif
          enddo ! <-- i loop over dimensions


      if (verbose) 
     &  write(*,'(1x,A)') 'Identifying presence of boundary edges:'
          do node=0,nnodes-1
        western_edge(node)=.true.  ;  eastern_edge(node)=.true.
        southern_edge(node)=.true. ;  northern_edge(node)=.true.

        if (xi_start(node) > 1) western_edge(node)=.false.
        if (id_xi_rho > 0) then
          if ( xi_start(node)+dimsize(id_xi_rho,node) < XI_rho
     &                             ) eastern_edge(node)=.false.
            endif
        if (id_xi_u > 0) then
          if ( xi_start(node)+dimsize(id_xi_u,node) < XI_rho
     &                             ) eastern_edge(node)=.false.
            endif

        if (eta_start(node) > 1) southern_edge(node)=.false.
        if (id_eta_rho > 0) then
          if ( eta_start(node)+dimsize(id_eta_rho,node) < ETA_rho
     &                             ) northern_edge(node)=.false.
            endif
        if (id_eta_v > 0) then
          if ( eta_start(node)+dimsize(id_eta_v,node) < ETA_rho
     &                             ) northern_edge(node)=.false.
            endif

        if (verbose) then
            if (node == 0) then
              write(*,'(8x,A,I4,4(2x,A,L1))') 'node =', node,
     &        'WST=',western_edge(node),  'EST=',eastern_edge(node),
     &        'SOU=',southern_edge(node), 'NOR=',northern_edge(node)
            else
              write(*,'(14x,I4,4(6x,L1))') node,
     &                     western_edge(node),  eastern_edge(node),
     &                     southern_edge(node), northern_edge(node)
            endif
        endif !<-- verbose
      enddo  !<-- node=0,nnodes-1

! Create combined netCDF file:   Once the completeness of the set of
!------- -------- ------ -----   partial files have been established
! and dimensions survive consistency check, create the combined file,
! define its dimensions and copy global attributes.

          i=lenstr(root_bak)
          do while (root_bak(i:i) == '.')  ! remove dots from end
             root_bak(i:i) = ' '
             i=i-1
          enddo
          if (time_partition) then! Add starting time to output filename
            if (nend == 0) nend = tsize
            !if (nend>0) then
               write(suffix,'(a,i0.3,a,i0.3,a)') '_t',nstart , '-',nend,'.nc'
            !else
            !   write(suffix,'(a,i0.3,a)') '_t',nstart , ':.nc'
            !endif
          else
            suffix = '.nc'
          endif
          print*,'suffix = ',suffix
          nctargname=trim(root_bak)/ /trim(suffix)
      ltrg=lenstr(nctargname)
                                                ! Check whether the
      j=0                                       ! file name contains
      do i=1,ltrg                               ! leading directories
        if (nctargname(i:i) == '/') j=i+1       ! and if it does, take
      enddo                                     ! them out: "ncjoin"
      if (j > 0) then                           ! is designed to place
        nctargname=nctargname(j:ltrg)           ! the combined files by 
        ltrg=ltrg-j+1                           ! default into the current
      endif                                     ! working directory.
      if (ldir > 0) then
        nctargname= odir(1:ldir)/ /'/'/ /nctargname(j:ltrg)
        ltrg=lenstr(nctargname)
      endif

#ifdef ZLIB_COMPRESSION
      if (ncVersion < 4) then
        ierr=nf_create(nctargname(1:ltrg),
     &          nf_clobber+nf_64bit_offset, nctarg)
          else
        ierr=nf_create(nctargname(1:ltrg), nf_netcdf4, nctarg)
#endif
#ifdef ZLIB_COMPRESSION
          endif
#endif
      if (ierr == nf_noerr) then
        write(*,'(/1x,3A)')  'Created netCDF file ''',
     &                        nctargname(1:ltrg), '''.'
        ierr=nf_set_fill(nctarg, nf_nofill, i)
        if (ierr /= nf_noerr) then
          write(*,'(/8x,A,1x,3A/8x,A)')       '### ERROR: cannot set ',
     &       '"nf_nofill" mode for netCDF file ''', nctargname(1:ltrg),
     &                                       '''.',  nf_strerror(ierr)
        endif
      else
        write(*,'(/1x,4A/12x,A/)')  '### ERROR: Cannot create netCDF ',
     &         'file ''', nctargname(1:ltrg), '''.', nf_strerror(ierr)
      endif
      if (ierr /= nf_noerr) stop

! Define dimensions: also compute the size of buffer needed to
! accommodate the largest array.

      if (verbose)
     & write(*,'(/1x,A,3x,A,1x,A,1x,A)') 'Dimensions:', 'id', 'size',
     &                                                       'name'
      size_XI=1 ; size_ETA=1 ; size_S=1

          do i=1,ndims
            lvar=lenstr(dimname(i))
            ierr=nf_def_dim (nctarg, dimname(i)(1:lvar),
     &                     dimsize(i,nnodes), dimid(i))
        if (ierr == nf_noerr) then
          if (dimid(i) == i) then
      if (verbose)
     &      write(*,'(14x,I3,I5,1x,3A)') dimid(i), dimsize(i,nnodes),
     &                                '''', dimname(i)(1:lvar), ''''
            if (dimname(i)(1:3) == 'xi_') then
                  size_XI=max(size_XI, dimsize(i,nnodes))
            elseif (dimname(i)(1:4) == 'eta_') then
                  size_ETA=max(size_ETA, dimsize(i,nnodes))
            elseif (dimname(i)(1:5) == 's_rho' .or.
     &              dimname(i)(1:3) == 's_w') then
                  size_S=max(size_S, dimsize(i,nnodes))
                endif
              else
            write(*,'(/1x,2A,I3,1x,5A/12x,2A,I3,A/)')  '### ERROR: ',
     &      'id =', dimid(i), 'for dimension ''', dimname(i)(1:lvar),
     &      ''' from netCDF file ''',    nctargname(1:ltrg),    '''',
     &                  'differs from ', 'the original id =', i, '.'
                goto 97
              endif
            else
         write(*,'(/1x,4A/12x,A/)')      '### ERROR: Cannot define ',
     &  'dimension ''', dimname(i)(1:lvar), '''.', nf_strerror(ierr)
              goto 97
            endif 
          enddo ! <-- i loop over dimensions

! Copy all global attributes, except 'partition'.

      if (verbose)
     &    write(*,'(1x,A)') 'Copying global attributes:'
      lsrc=lenstr(srcname(0))
      if (ncsrc(0) == -1) ierr=nf_open(srcname(0),nf_nowrite,ncsrc(0))
      if (ierr == nf_noerr) then
          do i=1,ngatts
          ierr=nf_inq_attname(ncsrc(0), nf_global, i, string)
            if (ierr. eq. nf_noerr) then
              lvar=lenstr(string)
            if (string(1:lvar) /= 'partition'            .and.
     &          string(1:lvar) /= signame(1:lenstr(signame))) then

              ierr=nf_copy_att(ncsrc(0), nf_global, string(1:lvar),
     &                                            nctarg, nf_global)
              if (ierr /= nf_noerr) then
                write(*,'(/1x,4A/12x,3A/12x,A)')      '### ERROR: ',
     &            'Cannot copy global attribute ''', string(1:lvar),
     &            ''' into netCDF',  'file ''',  nctargname(1:ltrg),
     &                                    '''.',  nf_strerror(ierr)
                  goto 97
                endif
      if (verbose)
     &          write(*,'(20x,3A)') '''', string(1:lvar), ''''
              endif
            else
            write(*,'(/1x,2A,I3/12x,3A/12x,A/)')     '### ERROR: ',
     &                'Cannot determine name of global attribute #',
     &                i, 'from netCDF file ''',  srcname(0)(1:lsrc),
     &                                    '''.',  nf_strerror(ierr)
              goto 97
            endif
          enddo

#ifdef ZLIB_COMPRESSION                         /* Put format label */
        if (ncVersion > 3 .and. deflate_level > 0) then
          lvar=lenstr(signature)                    ! so the file can be
          ierr=nf_put_att_text(nctarg, nf_global,   ! later identified as
     &                 signame, lvar, signature)    ! compressed netCDF4.
       endif
#endif

      else
        write(*,'(/1x,A,1x,3A/12x,A)')      '### ERROR: Cannot open ',
     & 'netCDF file ''', srcname(0)(1:lsrc), '''.', nf_strerror(ierr)
        goto 97
      endif

! Define variables and copy their attributes.

      if (verbose)
     &write(*,'(1x,A)') 'Variables, their dimensions and attributes:'
          do i=1,nvars
        node=vnode(i)
        lsrc=lenstr(srcname(node))
#ifdef KEEP_CLOSED
        if (ncsrc(node)==-1) ierr=nf_open(srcname(node), nf_nowrite,
     &                                                  ncsrc(node))
        if (ierr == nf_noerr) then
#endif
          ierr=nf_inq_var(ncsrc(node), vid(i,node),  vname(i),
     &             vtype(i), vdims(i), dimids(1,i),  varatts)
          if (ierr == nf_noerr) then
            lvar=lenstr(vname(i))
            ierr=nf_def_var(nctarg, vname(i)(1:lvar),vtype(i),
     &                        vdims(i), dimids(1,i), varid(i))
            if (ierr == nf_noerr) then
#ifdef ZLIB_COMPRESSION
              ! if (vdims(i) > 1 .and. deflate_level > 0 ) then
              if (vdims(i) > 1) then
                ierr=nf_def_var_deflate(nctarg, varid(i), shuffle,
     &                                     deflate, deflate_level)
              endif
              if (ierr == nf_noerr) then
#endif
      if (verbose)
     &         write(*,'(8x,3A,8I3)')  '''', vname(i)(1:lvar)
     &        , '''         dimids =', (dimids(j,i), j=1,vdims(i))
                do j=1,varatts
                  ierr=nf_inq_attname(ncsrc(node), vid(i,node), j,
     &                                                      string)
                  if (ierr == nf_noerr) then
                    lstr=lenstr(string)
                    ierr=nf_copy_att(ncsrc(node), vid(i,node),
     &                      string(1:lstr), nctarg, varid(i))
                    if (ierr /= nf_noerr) then
                      write(*,'(/1x,2A,I3,3A/12x,4A)') '### ERROR: ',
     &               'Cannot copy attribute #', j,' for variable ''',
     &                vname(i)(1:lvar),  ''' into netCDF', 'file ''',
     &                nctargname(1:ltrg), '''.  ', nf_strerror(ierr)
                      goto 97
                    endif
      if (verbose)
     &               write(*,'(16x,3A)') '''', string(1:lstr), ''''
                  else
                    write(*,'(/1x,A,I3/12x,3A/12x,A/)')
     &                 '### ERROR: Cannot get name of attribute #', j,
     &                    'for variable ''', vname(i)(1:lvar), '''.',
     &                                             nf_strerror(ierr)
                    goto 97
                  endif
                enddo
#ifdef ZLIB_COMPRESSION
              else
                write(*,'(/1x,4A/12x,A)')   '### ERROR: Cannot set ',
     &                 'deflation for variable ''', vname(i)(1:lvar),
     &                                      '''.', nf_strerror(ierr)
                goto 97
              endif
#endif
            else
              write(*,'(/8x,4A/)') '### ERROR: Cannot define ',
     &                   'variable ''', vname(i)(1:lvar), '''.'
              goto 97
            endif
          else
            write(*,'(/8x,2A/15x,A,I3,1x,3A/)')      '### ERROR: ',
     &           'Cannot make general inquiry about variable #', i,
     &           'in netCDF file ''',  srcname(node)(1:lsrc), '''.'
            goto 97
          endif

! Determine whether partitionable dimensions or unlimited dimension
! are present for this variable: the convention adopted here is:
!         part_type = 0 -- non-partitionable array;
!                   = 1 -- has partitionable XI-dimension only;
!                   = 2 -- has partitionable ETA-dimension only;
!                   = 3 -- partitionable in both XI and ETA.

            series(i)=.false.
            part_type(i)=0
          do j=1,vdims(i)
            if ( dimids(j,i) == id_xi_rho .or.
     &           dimids(j,i)== id_xi_u  ) then
                part_type(i)=part_type(i)+1
            elseif ( dimids(j,i) == id_eta_rho .or.
     &               dimids(j,i) == id_eta_v ) then
                part_type(i)=part_type(i)+2
            elseif (dimids(j,i)==unlimdimid) then
                series(i)=.true.
              endif
            enddo

#ifdef KEEP_CLOSED
          if (node > 0) then
            ierr=nf_close(ncsrc(node))
            ncsrc(node)=-1
          endif
        else
          write(*,'(/1x,A,1x,3A/12x,A)')    '### ERROR: Cannot open ',
     &     'netCDF file ''', srcname(node)(1:lsrc), ''' for reading.',
     &                                            nf_strerror(ierr)
          goto 97
        endif
#endif
      enddo       ! <-- i=1,nvars, variable IDs.

! Leave definition mode

          ierr=nf_enddef (nctarg)
      if (verbose)
     & write(*,'(/1x,A)') 'Leaving definition mode.'

! Allocate buffer arrays: Scan all the variables to find the largest
!--------- ------ ------- memory requirement within each variable type.

      size_cbfr=0     ; size_ibfr1=0    ; size_ibfr2=0
      size_ibffr=0    ; size_bffr4=0    ; size_bffr8=0
#ifdef FAST_MODE
      size_cbf_out=0  ; size_ibf1_out=0 ; size_ibf2_out=0
      size_ibff_out=0 ; size_bff4_out=0 ; size_bff8_out=0
#endif
      do i=1,nvars                   ! Here "var_mask" is to account
        lvar=lenstr(vname(i))        ! for the fact that some variables
        do node=0,nnodes-1           ! (e.g., boundary forcing arrays)
          var_mask=.false.           ! may be present in some partial
          if (part_type(i)==0) then  ! files, but absent in others.
            var_mask=.true.
          elseif (part_type(i) == 1 .and. lvar > 6) then
            if (vname(i)(lvar-5:lvar) == '_south' .and.
     &                           southern_edge(node)) then
              var_mask=.true.
            elseif (vname(i)(lvar-5:lvar) == '_north' .and.
     &                           northern_edge(node)) then
              var_mask=.true.
            endif
          elseif (part_type(i) == 2 .and. lvar > 5) then
            if (vname(i)(lvar-4:lvar) == '_west' .and.
     &                           western_edge(node)) then
              var_mask=.true.
            elseif (vname(i)(lvar-4:lvar) == '_east' .and.
     &                           eastern_edge(node)) then
              var_mask=.true.
            endif
          elseif (part_type(i)==3) then
            var_mask=.true.
          endif
          if (var_mask) then
            size=1
            do j=1,vdims(i)
              k=dimids(j,i)
              if (k /= unlimdimid) then
                size=size*dimsize(k,node)
              endif
            enddo
            if (vtype(i) == nf_char) then
              size_cbfr=max(size_cbfr,size)
            elseif (vtype(i) == nf_byte) then
              size_ibfr1=max(size_ibfr1,size)
            elseif (vtype(i) == nf_short) then
              size_ibfr2=max(size_ibfr2,size)
            elseif (vtype(i) == nf_int) then
              size_ibffr=max(size_ibffr,size)
            elseif (vtype(i) == nf_float) then
              size_bffr4=max(size_bffr4,size)
            elseif (vtype(i) == nf_double) then
              size_bffr8=max(size_bffr8,size)
        else
              write(*,'(/8x,3A/)')  '### ERROR: Variable ''',
     &            vname(i)(1:lvar), ''' is of unknown type.'
              stop
        endif
          endif
        enddo  !<-- nnodes

#ifdef FAST_MODE
        if (part_type(i) > 0) then
          size1=1
          do j=1,vdims(i)
            k=dimids(j,i)
            if (k == id_xi_rho) then
              size1=size1*XI_rho
            elseif (k == id_xi_u) then
              size1=size1*(XI_rho-1)
            elseif (k == id_eta_rho) then
              size1=size1*ETA_rho
            elseif ( k== id_eta_v) then
              size1=size1*(ETA_rho-1)
            elseif (k /= unlimdimid) then
              size1=size1*dimsize(k,nnodes)
            endif
          enddo

          if (vtype(i) == nf_char) then
            size_cbf_out=max(size_cbf_out,size1)
          elseif (vtype(i) == nf_byte) then
            size_ibf1_out=max(size_ibf1_out,size1)
          elseif (vtype(i) == nf_short) then
            size_ibf2_out=max(size_ibf2_out,size1)
          elseif (vtype(i) == nf_int) then
            size_ibff_out=max(size_ibff_out,size1)
          elseif (vtype(i) == nf_float) then
            size_bff4_out=max(size_bff4_out,size1)
          elseif (vtype(i) == nf_double) then
            size_bff8_out=max(size_bff8_out,size1)
          endif
        endif  !<-- part_type(i) > 0
#endif
      enddo !<-- nvars

      if (size_cbfr > alloc_cbfr) then
        if (allocated(cbfr)) deallocate(cbfr)
        allocate(cbfr(size_cbfr)) ;  alloc_cbfr=size_cbfr
        write(*,*) 'allocated nf_char workspace', size_cbfr
      endif
      if (size_ibfr1 > alloc_ibfr1) then
        if (allocated(ibfr1)) deallocate(ibfr1)
        allocate(ibfr1(size_ibfr1)) ; alloc_ibfr1=size_ibfr1
        write(*,*) 'allocated nf_byte workspace', size_ibfr1
      endif
      if (size_ibfr2 > alloc_ibfr2) then
        if (allocated(ibfr2)) deallocate(ibfr2)
        allocate(ibfr2(size_ibfr2)) ; alloc_ibfr2=size_ibfr2
        write(*,*) 'allocated nf_int2 workspace', size_ibfr2
      endif
      if (size_ibffr > alloc_ibffr) then
        if (allocated(ibffr)) deallocate(ibffr)
        allocate(ibffr(size_ibffr)) ; alloc_ibffr=size_ibffr
        write(*,*)  'allocated nf_int workspace', size_ibffr
      endif
      if (size_bffr4 > alloc_bffr4) then
        if (allocated(bffr4)) deallocate(bffr4)
        allocate(bffr4(size_bffr4)) ; alloc_bffr4=size_bffr4
        write(*,*) 'allocated nf_real workspace', size_bffr4
      endif
      if (size_bffr8 > alloc_bffr8) then
        if (allocated(bffr8)) deallocate(bffr8)
        allocate(bffr8(size_bffr8)) ; alloc_bffr8=size_bffr8
        write(*,*)'allocated nf_double workspace',size_bffr8
      endif

#ifdef FAST_MODE
      if (size_cbf_out > allc_cbf_out) then
        if (allocated(cbf_out)) deallocate(cbf_out)
        allocate(cbf_out(size_cbf_out)) ; allc_cbf_out=size_cbf_out
        write(*,*) 'allocated nf_char workspace', size_cbf_out
      endif
      if (size_ibf1_out > allc_ibf1_out) then
        if (allocated(ibf1_out)) deallocate(ibf1_out)
        allocate(ibf1_out(size_ibf1_out)) ; allc_ibf1_out=size_ibf1_out
        write(*,*) 'allocated nf_byte workspace',size_ibf1_out
      endif
      if (size_ibf2_out > allc_ibf2_out) then
        if (allocated(ibf2_out)) deallocate(ibf2_out)
        allocate(ibf2_out(size_ibf2_out)) ; allc_ibf2_out=size_ibf2_out
        write(*,*) 'allocated nf_int2 workspace', size_ibf2_out
      endif
      if (size_ibff_out > allc_ibff_out) then
        if (allocated(ibff_out)) deallocate(ibff_out)
        allocate(ibff_out(size_ibff_out)) ; allc_ibff_out=size_ibff_out
        write(*,*) 'allocated nf_int workspace',size_ibff_out
      endif
      if (size_bff4_out > allc_bff4_out) then
        if (allocated(bff4_out)) deallocate(bff4_out)
        allocate(bff4_out(size_bff4_out)) ; allc_bff4_out=size_bff4_out
        write(*,*) 'allocated nf_real workspace',size_bff4_out
      endif
      if (size_bff8_out > allc_bff8_out) then
        if (allocated(bff8_out)) deallocate(bff8_out)
        allocate(bff8_out(size_bff8_out)) ; allc_bff8_out=size_bff8_out
          write(*,*) 'allocated nf_double workspace', size_bff8_out
      endif
#endif

#ifdef TIMING
      if (timing) then
! Note: there is a possibility that "iclk" goes beyond "clk_max" and
! is folded back into the range of [0 ... clk_max].  The logic needs to
! account for this. Recall that all the arguments of system_clock() are
! 4-byte integers, while all integers computed below system_clock()
! call are 8-byte integers.

      nclk=3-nclk
      call system_clock(iclk(nclk), clk_rate,clk_max)
      inc_clk=iclk(nclk)-iclk(3-nclk)
      if (inc_clk < 0) inc_clk=inc_clk+clk_max
      net_clk=net_clk+inc_clk
      net_init_clk=net_init_clk+inc_clk
      endif
#endif

!     **     *     ***  *******    ***  *********  ********
!      *    ***   ***   ***   ***  ***  *  ***  *  ***    *
!       *   ***   ***   ***   ***  ***     ***     ***
!       *  *** * ***    ***   **   ***     ***     ******
!        * **  * **     ******     ***     ***     ***
!        ***    ***     ***  **    ***     ***     ***    *
!         *     **      ***   ***  ***     ***     ********


! Transfer variables into combined file.
!========= ========= ==== ======== =====

        if (nend <= 0) nend = tsize
        do rec=nstart,min(tsize,nend)
        if (tsize > 1) then
#ifdef TIMING
      if (timing) then
          nclk=3-nclk
          call system_clock(iclk(nclk), clk_rate,clk_max)
          inc_clk=iclk(nclk)-iclk(3-nclk)
          if (inc_clk < 0) inc_clk=inc_clk+clk_max
          net_clk=net_clk+inc_clk
          net_gray_clk=net_gray_clk+inc_clk

          write(*,'(F8.1,1x,2(A,I8,1x),A)')
     &                         dble(net_clk)/dble(clk_rate),
      endif
#else
          write(*,'(4x,2(A,I8,1x),A)')
#endif
     &     'Processing record', rec, 'out of', tsize,  '...'
        endif

          do i=1,nvars
          lvar=lenstr(vname(i))
          if (rec==1 .or. series(i)) then
#ifdef TIMING
      if (timing) then
            if (vtype(i) == nf_char .or.        ! Determine the size
     &          vtype(i) == nf_byte) then       ! of one element of
              vtypsize=1                        ! the variable in
            elseif (vtype(i) == nf_short) then  ! bytes (needed only
              vtypsize=2                        ! to report the total
            elseif (vtype(i) == nf_int .or.     ! amount of data in
     &              vtype(i) == nf_float) then  ! MBytes).
              vtypsize=4
            elseif (vtype(i) == nf_double) then
              vtypsize=8
            endif
      endif
#endif
            if (part_type(i)==0 .and. .not.series(i)) then

! Scalar variable (actually it may be an array, but having neither
! partitionable spatial dimension, nor record dimension, i.e., it is
! treated as a whole and only during rec=1):

              write(*,'(18x,I4,1x,A,I4,1x,3A)') i, 'out of', nvars,
     &            'non-part.  non-rec. ''', vname(i)(1:lvar), '''.'

              size=1
              do j=1,vdims(i)
                size=size * dimsize(dimids(j,i),0)
              enddo
              if (vtype(i) == nf_char) then
                ierr=chk_alloc(size, alloc_cbfr, 'nf_char')
              elseif (vtype(i) == nf_byte) then
                ierr=chk_alloc(size, alloc_ibfr1, 'nf_byte')
              elseif (vtype(i) == nf_short) then
                ierr=chk_alloc(size, alloc_ibfr2, 'nf_short')
              elseif (vtype(i) == nf_int) then
                ierr=chk_alloc(size, alloc_ibffr,   'nf_int')
              elseif (vtype(i) == nf_float) then
                ierr=chk_alloc(size, alloc_bffr4, 'nf_float')
              elseif (vtype(i) == nf_double) then
                ierr=chk_alloc(size,alloc_bffr8, 'nf_double')
                else
                write(*,'(/1x,4A/)')  '### ERROR: Unknown type ',
     &                 'of variable ''', vname(i)(1:lvar), '''.'
                stop
              endif
              if (ierr /= 0) stop

              if (vtype(i) == nf_char) then
                ierr=nf_get_var_text (ncsrc(0), vid(i,0), cbfr)
              elseif (vtype(i) == nf_byte) then
                ierr=nf_get_var_int1  (ncsrc(0), vid(i,0), ibfr1)
              elseif (vtype(i) == nf_short) then
                ierr=nf_get_var_int2  (ncsrc(0), vid(i,0), ibfr2)
              elseif (vtype(i) == nf_int) then
                ierr=nf_get_var_int   (ncsrc(0), vid(i,0), ibffr)
              elseif (vtype(i) == nf_float) then
                ierr=nf_get_var_real  (ncsrc(0), vid(i,0), bffr4)
              elseif (vtype(i) == nf_double) then
                ierr=nf_get_var_double(ncsrc(0), vid(i,0), bffr8)
              endif

              if (ierr /= nf_noerr) then
                write(*,'(/1x,4A/12x,A/)')  '### ERROR: Cannot ',
     &                      'read variable ''', vname(i)(1:lvar),
     &                                  '''.', nf_strerror(ierr)
                  goto 97
#ifdef TIMING
              else
                nclk=3-nclk
                call system_clock(iclk(nclk),clk_rate,clk_max)
                inc_clk=iclk(nclk)-iclk(3-nclk)
                if (inc_clk < 0) inc_clk=inc_clk+clk_max
                net_clk=net_clk+inc_clk
                net_read_clk=net_read_clk+inc_clk
                net_read_size=net_read_size+size*vtypsize
#endif
                endif

              if (vtype(i) == nf_char) then
                ierr=nf_put_var_text (nctarg, varid(i), cbfr)
              elseif (vtype(i) == nf_byte) then
                ierr=nf_put_var_int1  (nctarg, varid(i), ibfr1)
              elseif (vtype(i) == nf_short) then
                ierr=nf_put_var_int2  (nctarg, varid(i), ibfr2)
              elseif (vtype(i) == nf_int) then
                ierr=nf_put_var_int   (nctarg, varid(i), ibffr)
              elseif (vtype(i) == nf_float) then
                ierr=nf_put_var_real  (nctarg ,varid(i), bffr4)
              elseif (vtype(i) == nf_double) then
                ierr=nf_put_var_double(nctarg, varid(i), bffr8)
                  endif
              if (ierr /= nf_noerr) then
                    write(*,'(/1x,4A/12x,3A/12x,A)') '### ERROR: ',
     &              'Cannot write variable ''', vname(i)(1:lvar),
     &           ''' into netCDF', 'file ''', nctargname(1:ltrg),
     &                                 '''.',  nf_strerror(ierr)
                    goto 97
#ifdef TIMING
                else
                nclk=3-nclk
                call system_clock(iclk(nclk), clk_rate,clk_max)
                inc_clk=iclk(nclk)-iclk(3-nclk)
                if (inc_clk < 0) inc_clk=inc_clk+clk_max
                net_clk=net_clk+inc_clk
                net_wrt_clk=net_wrt_clk+inc_clk
                net_wrt_size=net_wrt_size+size*vtypsize
#endif
                endif
            elseif (part_type(i) == 0) then

! Non-partitionable array (but the one with record dimension).

              write(*,'(16x,3A)') 'Copy non-partitioned array: ''',
     &                                   vname(i)(1:lvar), '''...'
                size=1
              do j=1,vdims(i)
                if (dimids(j,i)==unlimdimid) then
                  start(j)=rec ; count(j)=1
                  else
                  start(j)=1 ; count(j)=dimsize(dimids(j,i),0)
                  endif
                  size=size*count(j)
                enddo

              if (vtype(i) == nf_char) then
                ierr=chk_alloc(size, alloc_cbfr,  'nf_char')
              elseif (vtype(i) == nf_byte) then
                ierr=chk_alloc(size, alloc_ibfr1,  'nf_byte')
              elseif (vtype(i) == nf_short) then
                ierr=chk_alloc(size, alloc_ibfr2,  'nf_short')
              elseif (vtype(i) == nf_int) then
                ierr=chk_alloc(size, alloc_ibffr,    'nf_int')
              elseif (vtype(i) == nf_float) then
                ierr=chk_alloc(size, alloc_bffr4,  'nf_float')
              elseif (vtype(i) == nf_double) then
                ierr=chk_alloc(size, alloc_bffr8, 'nf_double')
                else
                write(*,'(/1x,4A/)')   '### ERROR: Unknown ',
     &          'type of variable ''',vname(i)(1:lvar),'''.'
                stop
                endif
              if (ierr /= 0) stop

              if (vtype(i) == nf_char) then
                ierr=nf_get_vara_text (ncsrc(0), vid(i,0), start,
     &                                               count, cbfr)
              elseif (vtype(i) == nf_byte) then
                ierr=nf_get_vara_int1  (ncsrc(0), vid(i,0), start,
     &                                               count, ibfr1)
              elseif (vtype(i) == nf_short) then
                ierr=nf_get_vara_int2  (ncsrc(0), vid(i,0), start,
     &                                               count, ibfr2)
              elseif (vtype(i) == nf_int) then
                ierr=nf_get_vara_int   (ncsrc(0), vid(i,0), start,
     &                                               count, ibffr)
              elseif (vtype(i) == nf_float) then
                ierr=nf_get_vara_real  (ncsrc(0), vid(i,0), start,
     &                                               count, bffr4)
              elseif (vtype(i) == nf_double) then
                ierr=nf_get_vara_double(ncsrc(0), vid(i,0), start,
     &                                               count, bffr8)
                endif
 
              if (ierr /= nf_noerr) then
                write(*,'(/8x,4A,I3,A/15x,A/)')  '### ERROR: Cannot ',
     &                       'read variable ''',     vname(i)(1:lvar),
     &                ''' for time record',rec,'.', nf_strerror(ierr)
                goto 97
#ifdef TIMING
              else
                nclk=3-nclk
                call system_clock(iclk(nclk),clk_rate,clk_max)
                inc_clk=iclk(nclk)-iclk(3-nclk)
                if (inc_clk < 0) inc_clk=inc_clk+clk_max
                net_clk=net_clk+inc_clk
                net_read_clk=net_read_clk+inc_clk
                net_read_size=net_read_size+size*vtypsize
#endif
                endif

              if (vtype(i) == nf_char) then
                ierr=nf_put_vara_text (nctarg, varid(i), start,
     &                                            count,  cbfr)
              elseif (vtype(i) == nf_byte) then
                ierr=nf_put_vara_int1  (nctarg, varid(i), start,
     &                                             count, ibfr1)
              elseif (vtype(i) == nf_short) then
                ierr=nf_put_vara_int2  (nctarg, varid(i), start,
     &                                             count, ibfr2)
              elseif (vtype(i) == nf_int) then
                ierr=nf_put_vara_int   (nctarg, varid(i), start,
     &                                             count, ibffr)
              elseif (vtype(i) == nf_float) then
                ierr=nf_put_vara_real  (nctarg, varid(i), start,
     &                                             count, bffr4)
              elseif (vtype(i) == nf_double) then
                ierr=nf_put_vara_double(nctarg, varid(i), start,
     &                                             count, bffr8)
                  endif

              if (ierr /= nf_noerr) then
                write(*,'(/8x,4A,I3/15x,3A/)')   '### ERROR: Cannot ',
     &                          'write variable ''', vname(i)(1:lvar),
     &               ''' for time record',rec, 'into netCDF file ''',
     &                    nctargname(1:ltrg),'''.', nf_strerror(ierr)
                    goto 97
#ifdef TIMING
                else
                nclk=3-nclk
                call system_clock(iclk(nclk), clk_rate,clk_max)
                inc_clk=iclk(nclk)-iclk(3-nclk)
                if (inc_clk < 0) inc_clk=inc_clk+clk_max
                net_clk=net_clk+inc_clk
                net_wrt_clk=net_wrt_clk+inc_clk
                net_wrt_size=net_wrt_size+size*vtypsize
#endif
                endif
            elseif (part_type(i) > 0) then

! Partitioned array:

              write(*,'(16x,2A,I3,1x,3A)')  'Assemble partitioned ',
     &                     'array type', part_type(i),  'name = ''',
     &                                       vname(i)(1:lvar), ''''
                do node=0,nnodes-1
                var_mask=.false.
                if (part_type(i) == 1 .and. lvar > 6) then
                  if (vname(i)(lvar-5:lvar) == '_south' .and.
     &                              southern_edge(node)) then
                      var_mask=.true.
      if (verbose)
     &                write(*,'(3x,A,I4,1x,4A)')  'node =', node,
     &               'identified XI-partitioned southern ',
     &               'boundary array ''', vname(i)(1:lvar), ''''
                  elseif (vname(i)(lvar-5:lvar) == '_north' .and.
     &                                 northern_edge(node)) then
                      var_mask=.true.
      if (verbose)
     &                write(*,'(3x,A,I4,1x,4A)')   'node =', node,
     &               'identified XI-partitioned northern ',
     &               'boundary array ''', vname(i)(1:lvar), ''''
                    endif
                elseif (part_type(i) == 2 .and. lvar > 5) then
                  if (vname(i)(lvar-4:lvar) =='_west' .and.
     &                                 western_edge(node)) then
                      var_mask=.true.
      if (verbose)
     &                write(*,'(3x,A,I4,1x,4A)')     'node =', node,
     &               'identified ETA-partitioned western ',
     &               'boundary array ''', vname(i)(1:lvar), ''''
                  elseif (vname(i)(lvar-4:lvar)=='_east'
     &                          .and. eastern_edge(node)) then
                      var_mask=.true.
      if (verbose)
     &                write(*,'(3x,A,I4,1x,4A)')     'node =', node,
     &               'identified ETA-partitioned eastern ',
     &               'boundary array ''', vname(i)(1:lvar), ''''
                    endif
                elseif (part_type(i) == 3) then
                    var_mask=.true.
      if (verbose)
     &              write(*,'(3x,A,I4,1x,4A)')   'node =', node,
     &                     'identified 2D-partitioned array ''',
     &                                  vname(i)(1:lvar), ''''
                  endif

                  if (var_mask) then
                    size=1
#ifdef FAST_MODE
                    size1=1
#endif
                  do j=1,vdims(i)                 ! Pair "start,count"
                    k=dimids(j,i)                 ! is for reading data
                    if (k == unlimdimid) then     ! from partial files.
                      start(j)=rec ; count(j)=1
                    else
                      start(j)=1 ; count(j)=dimsize(k,node)
                    endif
                    size=size*count(j)
                                                  ! Pair "start1,count" 
                    if (k == id_xi_rho) then      ! is for writting 
                        start1(j)=xi_start(node)  ! block of data into 
#ifdef FAST_MODE
                        count1(j)=XI_rho          ! joined file. 
#endif
                    elseif (k == id_xi_u) then
                          start1(j)=max(xi_start(node)-1,1)
#ifdef FAST_MODE
                          count1(j)=XI_rho-1
#endif
                    elseif (k == id_eta_rho) then
                          start1(j)=eta_start(node)
#ifdef FAST_MODE
                          count1(j)=ETA_rho
#endif
                    elseif ( k== id_eta_v) then
                          start1(j)=max(eta_start(node)-1,1)
#ifdef FAST_MODE
                          count1(j)=ETA_rho-1
#endif
                      else
                      start1(j)=start(j)   !<-- the same
#ifdef FAST_MODE
                      count1(j)=count(j)   !<-- the same
#endif
                      endif
#ifdef FAST_MODE
                      size1=size1*count1(j)
#endif
                    enddo

#ifdef KEEP_CLOSED
                  if (ncsrc(node) == -1) ierr=nf_open(srcname(node),
     &                                      nf_nowrite, ncsrc(node))
                  if (ierr == nf_noerr) then
#endif
      if (verbose)
     &               write(*,'(3x,A,I4,2x,A,I7,2x,A,I4,1x,A,I4,1x,A)')
     &                     'node =', node, 'ncsrc=',ncsrc(node),
     &                     'xi_start =', xi_start(node),
     &                     'eta_start =', eta_start(node), 'reading...'
#ifdef FAST_MODE
                    if (vtype(i) == nf_char) then
                      ierr=chk_alloc(size1, allc_cbf_out, 'nf_char')
                    elseif (vtype(i) == nf_byte) then
                      ierr=chk_alloc(size1, allc_ibf1_out,  'nf_byte')
                    elseif (vtype(i) == nf_short) then
                      ierr=chk_alloc(size1, allc_ibf2_out,  'nf_short')
                    elseif (vtype(i) == nf_int) then
                      ierr=chk_alloc(size1, allc_ibff_out,    'nf_int')
                    elseif (vtype(i) == nf_float) then
                      ierr=chk_alloc(size1, allc_bff4_out,  'nf_float')
                    elseif (vtype(i) == nf_double) then
                      ierr=chk_alloc(size1, allc_bff8_out, 'nf_double')
                    endif
                    if (ierr /= 0) stop
#endif
                    if (vtype(i) == nf_char) then
                      ierr=chk_alloc(size, alloc_cbfr,  'nf_char')
                    elseif (vtype(i) == nf_byte) then
                      ierr=chk_alloc(size, alloc_ibfr1,  'nf_byte')
                    elseif (vtype(i) == nf_short) then
                      ierr=chk_alloc(size, alloc_ibfr2,  'nf_short')
                    elseif (vtype(i) == nf_int) then
                      ierr=chk_alloc(size, alloc_ibffr,    'nf_int')
                    elseif (vtype(i) == nf_float) then
                      ierr=chk_alloc(size, alloc_bffr4,  'nf_float')
                    elseif (vtype(i) == nf_double) then
                      ierr=chk_alloc(size, alloc_bffr8, 'nf_double')
                    else
                      write(*,'(/1x,4A/)')   '### ERROR: Unknown ',
     &                'type of variable ''',vname(i)(1:lvar),'''.'
                      stop
                    endif
                    if (ierr /= 0) stop

                    if (vtype(i) == nf_char) then
                      ierr=nf_get_vara_text (ncsrc(node),vid(i,node),
     &                                            start, count, cbfr)
                    elseif (vtype(i) == nf_byte) then
                      ierr=nf_get_vara_int1 (ncsrc(node),vid(i,node),
     &                                           start, count, ibfr1)
                    elseif (vtype(i) == nf_short) then
                      ierr=nf_get_vara_int2 (ncsrc(node),vid(i,node),
     &                                           start, count, ibfr2)
                    elseif (vtype(i) == nf_int) then
                      ierr=nf_get_vara_int  (ncsrc(node),vid(i,node),
     &                                           start, count, ibffr)
                    elseif (vtype(i) == nf_float) then
                      ierr=nf_get_vara_real (ncsrc(node),vid(i,node),
     &                                           start, count, bffr4)
                    elseif (vtype(i) == nf_double) then
                     ierr=nf_get_vara_double(ncsrc(node),vid(i,node),
     &                                          start, count, bffr8)
                    endif

                    if (ierr /= nf_noerr) then
                      lsrc=lenstr(srcname(node))
                      write(*,'(/1x,4A,I3/15x,3A/15x,A/)')  '### ',
     &                              'ERROR: Cannot read variable ''',
     &                   vname(i)(1:lvar), ''' for time record', rec,
     &                  'from netCDF file ''', srcname(node)(1:lsrc),
     &                                      '''.', nf_strerror(ierr)
                      goto 97
#ifdef TIMING
                    else
                      nclk=3-nclk
                      call system_clock(iclk(nclk),clk_rate,clk_max)
                      inc_clk=iclk(nclk)-iclk(3-nclk)
                      if (inc_clk < 0) inc_clk=inc_clk+clk_max
                      net_clk=net_clk+inc_clk
                      net_read_clk=net_read_clk+inc_clk
                      net_read_size=net_read_size+size*vtypsize
#endif
                    endif

#ifdef KEEP_CLOSED
                    if (node > 0) then
                      ierr=nf_close(ncsrc(node))
                      ncsrc(node)=-1
                    endif
                  else
                    lsrc=lenstr(srcname(node))
                    write(*,'(/1x,A,1x,3A/14x,A)')     '### ERROR: ',
     &                                  'Cannot open netCDF file ''',
     &              srcname(node)(1:lsrc), '''.', nf_strerror(ierr)
                    goto 97
                  endif
#endif
      if (verbose)
     &               write(*,'(1x,A)')  'copying ...'

! In the code segment below there are two strategies, resulting in
! equivalent outcome, but different performance: CPP-switch FAST_MODE
! activates procedure in which the content of  partitioned data arrays
! is first assembled into an intermediate buffer array with horizontal
! dimensions corresponding to the whole physical grid.   Once all the
! data is copied in, the array is written once as a single record
! [this occurs immediately after the reading of the last node]. Note
! that all values of "count1(k)" [except for "k" corresponding to
! unlimited dimension] are equal to the actual dimension of the
! variable in netCDF file, while all all "start(k)" [except unlimited
! dimension] are equal to 1.

! Alternatively, partial rectangular blocks are written immediately
! after they read from the partial netCDF files, relying on sub-array
! writing capability of "nf_put_vara_TYPE". This results physical
! write into a large number of small records of size count(1).


#ifdef FAST_MODE
                  if (vtype(i) == nf_char) then
                    call assemble_text (cbfr, cbf_out, start1,
     &                                  count, count1, vdims(i))
                  elseif (vtype(i) == nf_byte) then
                    call assemble_byte  (ibfr1, ibf1_out, start1,
     &                                   count, count1, vdims(i))
                  elseif (vtype(i) == nf_short) then
                    call assemble_int2  (ibfr2, ibf2_out, start1,
     &                                   count, count1, vdims(i))
                  elseif (vtype(i) == nf_int) then
                    call assemble_int   (ibffr, ibff_out, start1,
     &                                   count, count1, vdims(i))
                  elseif (vtype(i) == nf_float) then
                    call assemble_real  (bffr4, bff4_out, start1,
     &                                   count, count1, vdims(i))
                  elseif (vtype(i) == nf_double) then
                    call assemble_double (bffr8, bff8_out, start1,
     &                                   count, count1, vdims(i))
                      endif
# ifdef TIMING
                  nclk=3-nclk
                  call system_clock(iclk(nclk), clk_rate, clk_max)
                  inc_clk=iclk(nclk)-iclk(3-nclk)
                  if (inc_clk < 0) inc_clk=inc_clk+clk_max
                  net_clk=net_clk+inc_clk
                  net_assm_clk=net_assm_clk+inc_clk
# endif
# else
                  if (vtype(i) == nf_char) then
                    ierr=nf_put_vara_text   (nctarg, varid(i), start1,
     &                                                   count, cbfr)
                  elseif (vtype(i) == nf_byte) then
                    ierr=nf_put_vara_int1   (nctarg, varid(i), start1,
     &                                                  count, ibfr1)
                  elseif (vtype(i) == nf_short) then
                    ierr=nf_put_vara_int2   (nctarg, varid(i), start1,
     &                                                  count, ibfr2)
                  elseif (vtype(i) == nf_int) then
                    ierr=nf_put_vara_int    (nctarg, varid(i), start1,
     &                                                  count, ibffr)
                  elseif (vtype(i) == nf_float) then
                    ierr=nf_put_vara_real   (nctarg, varid(i), start1,
     &                                                  count, bffr4)
                  elseif (vtype(i) == nf_double) then
                    ierr=nf_put_vara_double (nctarg, varid(i), start1,
     &                                                  count, bffr8)
                        endif

                  if (ierr /= nf_noerr) then
                    write(*,'(/1x,3A,I3/12x,3A/12x,A/)')
     &                      '### ERROR: Cannot write variable ''',
     &                     vname(i)(1:lvar),''' for time record',rec,
     &                     'into netCDF file ''', nctargname(1:ltrg),
     &                                     '''.',  nf_strerror(ierr)
                          goto 97
# ifdef TIMING
                  else
                    nclk=3-nclk
                    call system_clock(iclk(nclk), clk_rate,clk_max)
                    inc_clk=iclk(nclk)-iclk(3-nclk)
                    if (inc_clk < 0) inc_clk=inc_clk+clk_max
                    net_clk=net_clk+inc_clk
                    net_wrt_clk=net_wrt_clk+inc_clk
                    net_wrt_size=net_wrt_size+size*vtypsize
# endif
                         endif
#endif /* ! FAST_MODE */
                endif  ! <-- var_mask

# ifdef FAST_MODE
                if (node == nnodes-1) then
                  if (vtype(i) == nf_char) then
                    ierr=nf_put_vara_text  (nctarg, varid(i), start,
     &                                              count1, cbf_out)
                  elseif (vtype(i) == nf_byte) then
                    ierr=nf_put_vara_int1  (nctarg, varid(i), start,
     &                                             count1, ibf1_out)
                  elseif (vtype(i) == nf_short) then
                    ierr=nf_put_vara_int2  (nctarg, varid(i), start,
     &                                             count1, ibf2_out)
                  elseif (vtype(i) == nf_int) then
                    ierr=nf_put_vara_int   (nctarg, varid(i), start,
     &                                             count1, ibff_out)
                  elseif (vtype(i) == nf_float) then
                    ierr=nf_put_vara_real  (nctarg, varid(i), start,
     &                                             count1, bff4_out)
                  elseif (vtype(i) == nf_double) then
                    ierr=nf_put_vara_double(nctarg, varid(i), start,
     &                                             count1, bff8_out)
                  endif

                  if (ierr /= nf_noerr) then
                    write(*,'(/1x,4A,I3/12x,3A/12x,A/)') '### ERROR: ',
     &                   'Cannot write variable ''',  vname(i)(1:lvar),
     &                ''' for time record', rec, 'into netCDF file ''',
     &                   nctargname(1:ltrg), '''.',  nf_strerror(ierr)
                    goto 97
# ifdef TIMING
                  else
                    nclk=3-nclk
                    call system_clock(iclk(nclk), clk_rate,clk_max)
                    inc_clk=iclk(nclk)-iclk(3-nclk)
                    if (inc_clk < 0) inc_clk=inc_clk+clk_max
                    net_clk=net_clk+inc_clk
                    net_wrt_clk=net_wrt_clk+inc_clk
                    net_wrt_size=net_wrt_size+size1*vtypsize
# endif
                    endif
                endif    !<--  node == nnodes-1
#endif
                enddo       ! <-- node=0,nnodes-1
            endif    !<-- part_type =,=/, > 0 switch
          endif    !<-- rec==1 .or. series(i) switch
        enddo    !<-- i=1,nvars, loop over variables

! Use intermediate one-per-record nf_sync of the target file only in
! verbose mode because it slows down the execution speed.   Note that
! time spent by nf_sync is counted as writing time, and so does time
! spent to close the target file.

#if defined TIMING
        nclk=3-nclk
        call system_clock(iclk(nclk), clk_rate, clk_max)
        inc_clk=iclk(nclk)-iclk(3-nclk)
        if (inc_clk < 0) inc_clk=inc_clk+clk_max
        net_clk=net_clk+inc_clk
        net_gray_clk=net_gray_clk+inc_clk
#endif
        ierr=nf_sync (nctarg)

#ifdef CLOSE_NETCDF_AFTER_TREC
        if (rec .lt. min(tsize,nend) .and. mod(rec,rec_close)==0) then
           write(*,*) 'close netcdf files'
           do node=0,nnodes-1
             if (ncsrc(node) /= -1) then
               ierr=nf_close(ncsrc(node))
             endif
           enddo
           ierr=nf_close(nctarg)
           write(*,*) 'reopen netcdf files'
           ierr=nf_open(nctargname(1:ltrg), nf_write, nctarg)
           do node=0,nnodes-1
             if (ncsrc(node) /= -1) then
               ierr=nf_open(srcname(node),nf_nowrite,ncsrc(node))
             endif
           enddo
        endif
#endif

#if defined TIMING
        nclk=3-nclk
        call system_clock(iclk(nclk), clk_rate, clk_max)
        inc_clk=iclk(nclk)-iclk(3-nclk)
        if (inc_clk < 0) inc_clk=inc_clk+clk_max
        net_clk=net_clk+inc_clk
        net_sync_clk=net_sync_clk+inc_clk
#endif
      enddo  !<-- rec=1,tsize, loop over records

      if (ierr == nf_noerr) then
        clean_set=.true.
        goto 98
      endif
  97  clean_set=.false.

! Close all files                        ! At this moment open/closed
                                         ! status of partial files
  98  write(*,*) 'closing files...'      ! depends on the state of CPP
      do node=0,nnodes-1                 ! switch KEEP_CLOSED.   If the
        if (ncsrc(node) /= -1) then      ! switch is defined, then only
          ierr=nf_close(ncsrc(node))     ! node=0 file is expected to
          ncsrc(node)=-1                 ! be opened here.   Otherwise
        endif                            ! the entire set is opened and
      enddo                              ! needs to be closed.   Either
      write(*,*) '...........input'      ! way, as ncsrc(node).eq/ne.-1
#if defined TIMING
      nclk=3-nclk
      call system_clock(iclk(nclk), clk_rate, clk_max)
      inc_clk=iclk(nclk)-iclk(3-nclk)
      if (inc_clk < 0) inc_clk=inc_clk+clk_max
      net_clk=net_clk+inc_clk
      net_gray_clk=net_gray_clk+inc_clk
#endif
      ierr=nf_close(nctarg)            ! is used as flag indicating
#if defined TIMING
      nclk=3-nclk
      call system_clock(iclk(nclk), clk_rate, clk_max)
      inc_clk=iclk(nclk)-iclk(3-nclk)
      if (inc_clk < 0) inc_clk=inc_clk+clk_max
      net_clk=net_clk+inc_clk
      net_sync_clk=net_sync_clk+inc_clk
#endif
      write(*,*) '...........output'   ! status of each file.

#ifdef DEL_PART_FILES
      if (del_part_files) then
        if (clean_set) then
# ifdef TIMING
          nclk=3-nclk
          call system_clock(iclk(nclk), clk_rate, clk_max)
          inc_clk=iclk(nclk)-iclk(3-nclk)
          if (inc_clk < 0) inc_clk=inc_clk+clk_max
          net_clk=net_clk+inc_clk
          net_gray_clk=net_gray_clk+inc_clk
# endif
          write(*,'(/1x,A)') 'Deleting partial files...'
          do node=0,nnodes-1
            rmcmd='/bin/rm -f '/ /srcname(node)
            lstr=lenstr(rmcmd)
            if (node < 16 .or. (nnodes > 16 .and.
     &                       node==nnodes-1 )) then
              write(*,'(27x,3A)')  '''', rmcmd(1:lstr), ''''

            elseif (nnodes>16 .and. node<18) then
              write(*,'(24x,A)') '.................................'
            endif
            call system (rmcmd(1:lstr))
          enddo
          write(*,*)
# ifdef TIMING
          nclk=3-nclk
          call system_clock(iclk(nclk), clk_rate, clk_max)
          inc_clk=iclk(nclk)-iclk(3-nclk)
          if (inc_clk < 0) inc_clk=inc_clk+clk_max
          net_clk=net_clk+inc_clk
          net_rmcmd_clk=net_rmcmd_clk+inc_clk
# endif
      else
          write(*,'(/1x,2A/)')  '### ERROR: Not removing partial ',
     &                                  'files because of errors.'
      endif
      endif
#endif   /* DEL_PART_FILES */

      if (arg < nargs)  goto 1     !--> next set of partial files.

#ifdef TIMING
# ifdef __IFC
      RUN_time=etime(CPU_time)
# else
      call etime(CPU_time, RUN_time)
# endif
      RUN_time=RUN_time-tstart

      write(*,'(/3(1x,A,F11.2,1x))') 'CPU_time:  run =', RUN_time,
     &                 'usr =', CPU_time(1),  'sys =', CPU_time(2)

      if (clk_rate > 0) then
        ReadSize=1.0D-6*net_read_size
        WrtSize=1.0D-6*net_wrt_size
        ReadTime=net_read_clk/dble(clk_rate)
        WrtTime = net_wrt_clk/dble(clk_rate)
        InitTime=net_init_clk/dble(clk_rate)
        SyncTime=net_sync_clk/dble(clk_rate)

        write(*,'(/1x,A,22x,F12.2,1x,A)') 'Analysis/file creation :',
     &                                               InitTime, 'sec'
        write(*,'(8x,A,F12.2,1x,A,F12.2,1x,A,F8.2,1x,A)')
     &         'Total data read :', ReadSize, 'MBytes in',  ReadTime,
     &                          'sec (', ReadSize/ReadTime, 'MB/sec)'
        write(*,'(5x,A,F12.2,1x,A,F12.2,1x,A,F8.2,1x,A)')
     &      'Total data written :', WrtSize,  'MBytes in',   WrtTime,
     &                          'sec (',  WrtSize/WrtTime,  'MB/sec)'
# ifdef FAST_MODE
        AssmTime=net_assm_clk/dble(clk_rate)
        write(*,'(5x,A,22x,F12.2,1x,A)')      'Data assembly time :',
     &                                               AssmTime, 'sec'
# endif
        write(*,'(2x,A,22x,F12.2,1x,A)')   'Output file sync time :',
     &                                               SyncTime, 'sec'

# ifdef DEL_PART_FILES
        if (del_part_files) then
          write(*,'(1x,A,22x,F12.2,1x,A)') 'Removing partial files :',
     &                          net_rmcmd_clk/dble(clk_rate), 'sec'
      endif
# endif
        nclk=3-nclk
        call system_clock(iclk(nclk), clk_rate, clk_max)
        inc_clk=iclk(nclk)-iclk(3-nclk)
        if (inc_clk < 0) inc_clk=inc_clk+clk_max
        net_clk=net_clk+inc_clk
        net_gray_clk=net_gray_clk+inc_clk
        GrayTime=dble(net_gray_clk)/dble(clk_rate)
        write(*,'(14x,A,22x,F12.2,1x,A)') 'Gray time :',GrayTime,'sec'
        write(*,'(47x,A/12x,A,11x,F12.2,1x,A/)') '------------------',
     &   'Elapsed wall-clock time:',dble(net_clk)/dble(clk_rate),'sec'
      endif
#endif
      end

      function chk_alloc(size, alloc_size, type_str)
      implicit none
      integer chk_alloc, size, alloc_size
      character(len=*) type_str
      if (size > alloc_size) then
        write(*,'(/1x,4A/12x,A,I10,2x,A,I10/)')          '### ERROR: ',
     & 'Insufficient size of allocated workspace of type ''', type_str,
     &     ''':', 'needed', size, 'but actually allocated', alloc_size
        chk_alloc=1
      else
       chk_alloc=0
      endif
      end

! The following six routines copy rectangular block of data "buff"
! into whole-grid array "bfr_out"; all the routines have common core
! and differ only by type declarations. 
  
#ifdef FAST_MODE
# define NCJOIN

      subroutine assemble_text (buff, bfr_out, start, count,
     &                                        count1, vdims)
      implicit none
      character(len=1) buff(*), bfr_out(*)
# include "copy_subarray.h"
      end

      subroutine assemble_byte (buff, bfr_out, start, count,
     &                                        count1, vdims)
      implicit none
      integer(kind=1) ::  buff(*), bfr_out(*)
# include "copy_subarray.h"
      end

      subroutine assemble_int2 (buff, bfr_out, start, count,
     &                                        count1, vdims)
      implicit none
      integer(kind=2) :: buff(*), bfr_out(*)
# include "copy_subarray.h"
      end

      subroutine assemble_int  (buff, bfr_out, start, count,
     &                                        count1, vdims)
      implicit none
      integer(kind=4) :: buff(*), bfr_out(*)
# include "copy_subarray.h"
      end

      subroutine assemble_real (buff, bfr_out, start, count,
     &                                        count1, vdims)
      implicit none
      real(kind=4) :: buff(*), bfr_out(*)
# include "copy_subarray.h"
      end

      subroutine assemble_double(buff,bfr_out, start, count,
     &                                        count1, vdims)
      implicit none
      real(kind=8) :: buff(*), bfr_out(*)
# include "copy_subarray.h"
      end
# endif
      
      logical function is_in_list(vlist, vname)
      !
      ! Returns true if name is in list 
      ! list must be comma separated without spaces

      CHARACTER(len=*) :: vname
      CHARACTER(len=*) :: vlist
      integer ::len_v
      
      vname = trim(vname) / / ',' ! Mark end of var name by a ','
      len_v = LEN_TRIM(vname)
      is_in_list = (INDEX(trim(vlist),trim(vname)) > 0)
      vname(len_v:len_v) = ' '
      !print*,'Reset vname >',trim(vname),'<'
      !print*, trim(vname),' requested? ', is_in_list
      !print*,' Is_in_list = ', is_in_list

      end function is_in_list

      subroutine usage 
      print'(/(A))'
     &   , 'NCJOIN'
     &   , '     Reads ROMS partial netCDF files and combines them'
     &   , '     into a file for the whole physical grid.'
     &   , '     By default partial files are deleted upon'
     &   , '     successful joining. The output files are'
     &   , '     written in the current working directory'
     &   , '     (NCJOIN  is the inverse of PARTIT.)'
     &   , ' '
     &   , 'USAGE:'
     &   , '     ncjoin [-3hknpqv -c=<n> -i=<list> -x=<list>'
#ifdef CLOSE_NETCDF_AFTER_TREC
     &   / / ' --ncclose=<num>'
#endif
     &   / / '] files.*.nc'
     &   , ' '
     &   , '     where files.*.nc matches one or more complete set of' 
     &   , '     partial files'
     &   , ' '
     &   , 'OPTIONS:'
     &   , '    -3           netCDF3 output (default: netCDF4)'
     &   , '    -h           print this help'
     &   , '    -c=<0-9>     output compression level (default: 1)'
     &   , '                 -c=0 contiguous storage, no compression'
     &   , '    -i=<var_list> list of variables (comma-separated '
     &   , '                 without spaces) to include'
     &   , '    -k           keep partial files'
     &   , '    -s=<n>       output start with record n (time step)'
     &   , '    -e=<m>       output ends with record m (time step)'
!     &   , '    -m           reduce memory usage (slow mode)'
     &   , '    -n           Use low job priority'
     &   , '    -p           if record dim differs, use its minimum'
!     &   , '    -q           quite operation'
!     &   , '    -t           report operation time'
     &   , '    -v           increase verbosity'
!     &   , '    -x=<var_list> list of variables (comma-separated'
!     &   , '                 without spaces) to exclude'
     &   , '    -o=<dir>     output directory (default: working dir)'
#ifdef CLOSE_NETCDF_AFTER_TREC
     &   , '    --ncclose=<num> close and reopen Netcdf files every'
     &   , '                    <num> time records (default: 10)'
#endif
     &   , 'NOTE:'
     &   , '     The variable lists are case sensitive.'
      end subroutine usage

