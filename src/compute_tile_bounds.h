! Auxiliary module "compute_tile_bounds.h":
!---------- ------ ------------------------
! Compute bounds designed to cover interior points of an array
! for shared memory subdomain partitioning (tiling.)
!
! input: tile -- usually from 0 to NSUB_X*NSUB_E-1 -- indicates
!                the specified subdomain;  tile=NSUB_X*NSUB_E
!                corresponds to the whole domain of RHO points
!                treated as a single block.
! outputs: Istr,Iend -- starting and ending indices of subdomain
!          Jstr,Jend    tile in XI- and ETA-directions.
!
      integer Istr,Iend, Jstr,Jend, i_X,j_E,
     &        size_X,margin_X, size_E,margin_E
#ifdef MPI 
# include "hidden_mpi_vars.h"
#else
      parameter (size_X=(Lm+NSUB_X-1)/NSUB_X,
     &           margin_X=(NSUB_X*size_X-Lm)/2,
     &           size_E=(Mm+NSUB_E-1)/NSUB_E,
     &           margin_E=(NSUB_E*size_E-Mm)/2)
#endif


#ifdef ALLOW_SINGLE_BLOCK_MODE
C$    integer trd, omp_get_thread_num
      if (tile.eq.NSUB_X*NSUB_E) then
C$      trd=omp_get_thread_num()
C$      if (trd.gt.0) return !--> just return, if not master thread
# ifdef MPI    
        Istr=iwest      ! MONOBLOCK VERSION:
        Iend=ieast      ! Do not divide grid
        Jstr=jsouth     ! into tiles.
        Jend=jnorth 
  else
        Istr=1
        Iend=Lm
        Jstr=1
        Jend=Mm
      else
# endif
#endif
 
        j_E=tile/NSUB_X
        i_X=tile-j_E*NSUB_X
        if (mod(j_E,2).eq.1) i_X=NSUB_X-1 -i_X

#ifdef MPI
        if (mod(inode,2).gt.0) then
          i_X=NSUB_X-1 -i_X
        endif
        if (mod(jnode,2).gt.0) then
          j_E=NSUB_E-1 -j_E
        endif

        size_X=(ieast-iwest+NSUB_X)/NSUB_X
        margin_X=(NSUB_X*size_X - ieast+iwest-1)/2
        Istr=iwest-margin_X + i_X*size_X
        Iend=min( Istr + size_X-1 ,ieast)
        Istr=max(Istr,iwest)
#else
        Istr=1-margin_X + i_X*size_X
        Iend=min( Istr + size_X-1, Lm)
        Istr=max(Istr,1)
#endif


#ifdef MPI
        size_E=(jnorth-jsouth +NSUB_E)/NSUB_E
        margin_E=(NSUB_E*size_E -jnorth+jsouth-1)/2
        Jstr=jsouth-margin_E + j_E*size_E
        Jend=min( Jstr + size_E-1 ,jnorth)
        Jstr=max(Jstr,jsouth)
#else
        Jstr=1-margin_E + j_E*size_E
        Jend=min( Jstr + size_E-1, Mm)
        Jstr=max(Jstr,1)
#endif

 
#ifdef ALLOW_SINGLE_BLOCK_MODE
      endif
#endif
 
