!
! MPI setup rlated variables:
!---- ----- ------ -----------
!


#ifdef MPI
      integer abs_node, inode, jnode,
     &        p_W, p_E, p_S, p_N, p_SW, p_SE, p_NW, p_NE
      common /hidden_mpi_vars/ abs_node, inode, jnode,
     &        p_W, p_E, p_S, p_N, p_SW, p_SE, p_NW, p_NE
#endif

