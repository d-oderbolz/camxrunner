      SUBROUTINE AVERGE(NX,NY,NZ)
      INCLUDE "nampar.cmd"
      INCLUDE "winds.cmd"
      HIY=-9999.
      HIX=-9999.
      AVXW=0.
      AVXE=0.
      AVYS=0.
      AVYN=0.
      WXLOW = 999999.
      WYLOW = 999999.
      SPMIN = 999999.
      WXMIN = 999999.
      WYMIN = 999999.
      WXHI = -999999.
      WYHI = -999999.
      SPMAX = -999999.
      IMAX = 0
      JMAX = 0
      KMAX = 0
      IMIN = 0
      JMIN = 0
      KMIN = 0
      NC5 = 0
      NC10 = 0
      NC20 = 0
      NC30 = 0
      DO 3 K=1,NZ
         DO 2 J=1,NY
            AVXW=AVXW+U(1,J,K)
            AVXE=AVXE+U(NX,J,K)
            DO 1 I=1,NX
               IF(J.EQ.1) AVYS=AVYS+V(I,1,K)
               IF(J.EQ.NY) AVYN=AVYN+V(I,NY,K)
               IF(ABS(U(I,J,K)).GT.HIX) HIX=ABS(U(I,J,K))
               IF(ABS(V(I,J,K)).GT.HIY) HIY=ABS(V(I,J,K))
               SPD = SQRT(U(I,J,K)**2 + V(I,J,K)**2)
               IF (SPD .GT. SPMAX) THEN
                  SPMAX = SPD
                  IMAX = I
                  JMAX = J
                  KMAX = K
               ENDIF
               IF (SPD .LT. SPMIN) THEN
                  SPMIN = SPD
                  IMIN = I
                  JMIN = J
                  KMIN = K
               ENDIF
               IF (SPD .GT. 18000.) NC5 = NC5 + 1
               IF (SPD .GT. 36000.) NC10 = NC10 + 1
               IF (SPD .GT. 72000.) NC20 = NC20 + 1
               IF (SPD .GT. 108000.) NC30 = NC30 + 1
               IF (U(I,J,K) .LT. WXLOW) WXLOW = U(I,J,K)
               IF (U(I,J,K) .GT. WXHI) WXHI = U(I,J,K)
               IF (ABS(U(I,J,K)) .LT. WXMIN) WXMIN = ABS(U(I,J,K))
               IF (V(I,J,K) .LT. WYLOW) WYLOW = V(I,J,K)
               IF (V(I,J,K) .GT. WYHI) WYHI = V(I,J,K)
               IF (ABS(V(I,J,K)) .LT. WYMIN) WYMIN = ABS(V(I,J,K))
1           CONTINUE
2        CONTINUE
3     CONTINUE
      AVXW=AVXW/FLOAT(NY*NZ)
      AVXE=AVXE/FLOAT(NY*NZ)
      AVYS=AVYS/FLOAT(NX*NZ)
      AVYN=AVYN/FLOAT(NX*NZ)
      REF=10.
      RETURN
      END
