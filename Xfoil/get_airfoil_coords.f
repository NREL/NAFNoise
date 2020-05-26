      SUBROUTINE get_airfoil_coords

      USE AirfoilParams
      INCLUDE 'XFOIL.INC'

C
C---- max panel angle threshold for warning
      DATA ANGTOL / 40.0 /
C     Transform variables
c XFOIL RE = Re_c/chord, but then again it doesn't matter
C      Re = Re/a_chord
      alpha = aofa
      CALL INIT
C
C===============================================
      IF(ISNACA) THEN
       READ(airfoil,*) IINPUT
       CALL NACA(IINPUT)
C===============================================
      ELSE
       CALL LOAD(airfoil,ITYPE)
       IF(ITYPE.GT.0 .AND. NB.GT.0) THEN
ccc       CALL PANGEN(.TRUE.)
        CALL ABCOPY(.TRUE.)
C
        CALL CANG(X,Y,N,0, IMAX,AMAX)
        IF(ABS(AMAX).GT.ANGTOL) THEN
         WRITE(*,1081) AMAX, IMAX
c         CALL PANPLT
        ENDIF
       ENDIF
      ENDIF
1081    FORMAT(
     &  /' WARNING: Poor input coordinate distribution'
     &  /'          Excessive panel angle', F7.1,'  at i =', I4
     &  /'          Repaneling with PANE and/or PPAR suggested'
     &  /'           (doing GDES,CADD before repaneling _may_'
     &  /'            improve excessively coarse LE spacing' )
C
C===============================================
C      ELSEIF(COMAND.EQ.'INTE') THEN
C       CALL INTE
C

C===============================================
C      ELSEIF(COMAND.EQ.'PANE') THEN
       CALL PANGEN(.TRUE.)
ccc       CALL PANPLT
C
C      call GDES_Noise to scale the chord of the airfoil, but don't have to anymore
C       CALL GDES_Noise(a_chord)
       END Subroutine get_airfoil_coords
       
       
      SUBROUTINE get_ti_coords
c==================================================
      
      USE TICoords
      INCLUDE 'XFOIL.INC'
      
      x_ti(1:N) = X(1:N)
      y_ti(1:N) = Y(1:N)
      n_in = N
      
      END Subroutine get_ti_coords