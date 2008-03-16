      integer size_X, size_E, size_Z
      parameter (size_Z=4*(N+1),   size_X=2*(N+1)*(Lm+4),
     &                             size_E=2*(N+1)*(Mm+4))

      real  sn_NW(size_Z),   sendN(0:size_X),   sn_NE(size_Z),
     &      rv_NW(size_Z),   recvN(0:size_X),   rv_NE(size_Z),
     &    sendW(0:size_E),                      sendE(0:size_E),
     &    recvW(0:size_E),                      recvE(0:size_E),
     &      sn_SW(size_Z),   sendS(0:size_X),   sn_SE(size_Z),
     &      rv_SW(size_Z),   recvS(0:size_X),   rv_SE(size_Z)

      common /mess_buffers/ sn_NW,rv_NW, sendN,recvN, sn_NE,rv_NE,
     &                    sendW,recvW,                sendE,recvE,
     &                     sn_SW,rv_SW, sendS,recvS, sn_SE,rv_SE
C$OMP THREADPRIVATE(/mess_buffers/)
