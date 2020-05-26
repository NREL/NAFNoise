      PROGRAM PREDICT
      

C                  --------------------------------
C                  ***** VARIABLE DEFINITIONS *****
C                  --------------------------------

C       VARIABLE NAME               DEFINITION                  UNITS
C       -------------               ----------                  -----

C       ALPHTIP            TIP ANGLE OF ATTACK                DEGREES
C       ALPSTAR            SEGMENT ANGLE OF ATTACK            DEGREES
C       ALPRAT             TIP LIFT CURVE SLOPE               ---
C       C                  SEGMENT CHORDLENGTH                METERS
C       C0                 SPEED OF SOUND                     METERS/SEC
C       FRCEN              1/3 OCTAVE CENTERED FREQUENCIES    HERTZ
C       H                  SEGMENT TRAILING EDGE THICKNESS    METERS
C       IBLUNT             FLAG TO COMPUTE BLUNTNESS NOISE    ---
C       ILAM               FLAG TO COMPUTE LBL NOISE          ---
C       ITIP               FLAG TO COMPUTE TIP NOISE          ---
C       ITRIP              FLAG TO TRIP BOUNDARY LAYER        ---
C       ITURB              FLAG TO COMPUTE TBLTE NOISE        ---
C       L                  SEGMENT SPAN LENGTH                METERS
C       MAXSEG             MAXIMUM NUMBER OF SEGMENTS         ---
C       NFREQ              NUMBER OF 1/3 OCTAVE FREQUENCIES   ---
C       NSEG               NUMBER OF SEGMENTS                 ---
C       P1                 PRESSURE ASSOCIATED WITH 
C                            TBLTE PREDICTION                 NT/M2
C       P2                 PRESSURE ASSOCIATED WITH
C                            TBLTE PREDICTION                 NT/M2
C       P3                 PRESSURE ASSOCIATED WITH
C                            TBLTE PREDICTION                 NT/M2
C       P4                 PRESSURE ASSOCIATED WITH
C                            TOTAL PREDICTION                 NT/M2
C       P5                 PRESSURE ASSOCIATED WITH 
C                            LBLVS PREDICTION                 NT/M2
C       P6                 PRESSURE ASSOCIATED WITH 
C                            BLUNTNESS PREDICTION             NT/M2
C       P7                 PRESSURE ASSOCIATED WITH
C                            TIP NOISE PREDICTION             NT/M2
C       PHI                DIRECTIVITY ANGLE                  DEGREES
C       PSI                BLUNTNESS ANGLE                    DEGREES
C       R                  SEGMENT TO OBSERVER DISTANCE       METERS
C       ROUND              LOGICAL INDICATING ROUNDED TIP     ---
C       SPL                TOTAL SOUND PRESSURE LEVEL         DB
C       SPLALPH            SOUND PRESSURE LEVEL ASSOCIATED
C                            WITH TBLTE PREDICTION            DB
C       SPLBLNT            SOUND PRESSURE LEVEL ASSOCIATED
C                            WITH BLUNTNESS PREDICTION        DB
C       SPLLBL             SOUND PRESSURE LEVEL ASSOCIATED
C                            WITH LBL PREDICTION              DB
C       SPLP               SOUND PRESSURE LEVEL ASSOCIATED
C                            WITH TBLTE PREDICTION            DB
C       SPLS               SOUND PRESSURE LEVEL ASSOCIATED
C                            WITH TBLTE PREDICTION            DB
C       SPLTBL             TOTAL PRESSURE LEVEL ASSOCIATED
C                            WITH TBLTE PREDICTION            DB
C       SPLTIP             SOUND PRESSURE LEVEL ASSOCIATED
C                            WITH TIP NOISE PREDICTION        DB
C       ST                 STROUHAL NUMBER                    --
C       THETA              DIRECTIVITY ANGLE                  DEGREES
C       U                  SEGMENT FREESTREAM VELOCITY        METERS/SEC
C       VISC               KINEMATIC VISCOSITY                M2/SEC

      USE AirfoilParams
      USE Atmosphere
      USE BLParams
      USE Noise_Inputs
      USE NWTC_Subs
      USE ProgMod
      USE Third_Octave_Bands
      USE TINoiseInput
      USE TI_Guidati


      DIMENSION    FRCEN(NumBands)  ,
     1             ST(NumBands)    ,SPLLBL(NumBands) ,SPLTBL(NumBands),
     2             SPLP(NumBands)   ,SPLS(NumBands)  ,
     3             SPLALPH(NumBands) ,SPL(8,NumBands),
     5             SPLBLNT(NumBands),SPLTIP(NumBands),
     8             P1(NumBands)     ,P2(NumBands)    ,
     9             P3(NumBands)   ,P4(NumBands)     ,P5(NumBands)    ,
     1             P6(NumBands)      ,P7(NumBands)  ,P8(NumBands)    ,
     2             SPLti(NumBands)   

   
C     Begin
C     --------------------------------------------------  
      CALL Begin
        
      NFREQ = NumBands

C     SET UP VALUES OF 1/3 OCTAVE CENTERED FREQUENCIES 
C     ------------------------------------------------

      FRCEN  = Third_Octave

C      READ INPUT DATA AND ECHO INPUT TO OUTPUT FILE
C      ---------------------------------------------------
      CALL Read_Noise_Input
      
      CALL Write_Out_Header
      
C     Assign variables to mods for xfoil
                 
                 
C      INITIALIZE ALL PREDICTED PRESSURES AND SOUND
C      PRESSURE LEVELS TO ZERO
C      --------------------------------------------
                 
      DO 6001 I=1,NFREQ
        P1(I) = 0.0
        P2(I) = 0.0
        P3(I) = 0.0
        P4(I) = 0.0
        P5(I) = 0.0
        P6(I) = 0.0
        P7(I) = 0.0
        P8(I) = 0.0

        DO 6002 J=1,8
          SPL(J,I) = 0.0
 6002   CONTINUE
 6001 CONTINUE

   
C     MAKE A NOISE PREDICTION ACCORDING
C     TO THE MECHANISMS SELECTED. 
C     ---------------------------------------------------------
      
C        Assign variables to mods for xfoil
         IF( ITRIP .GT. 0) THEN
            ISTRIPPED = .TRUE.
         ELSE
            ISTRIPPED = .FALSE.
         ENDIF
         Mach = U/co
         Re = U*a_chord/nu

         IF ((X_BLMethod .EQ. 2) .OR. (TBL_Method .EQ. 2) .OR. 
     1       (TI_Method .EQ. 2)) THEN
             CALL WrScr1('Reading airfoil coordinates')
             write(*,*) '==========================='
             CALL get_airfoil_coords
             IF (TI_Method .EQ. 2) CALL get_ti_coords
         ENDIF
         
         IF ((X_BLMethod .EQ. 2) .OR. (TBL_Method .EQ. 2)) THEN
            CALL WrScr1('Calculating boundary layer parameters using
     1 Xfoil')
            write(*,*) '===========================================
     1======'
            CALL xfoil_noise
!   get the boundary layer parameters
            d99 = d99*a_chord
            d_star = d_star*a_chord
         ENDIF
      
        IF ((ILAM .EQ. 1) .AND. (ITRIP .EQ. 0)) then
          CALL WrScr1('Computing laminar vortex-shedding noise')
          write(*,*) '======================================='
          CALL LBLVS(aofa,a_chord,U,FRCEN,SPLLBL,
     2            THETA,PHI,L,R,NFREQ,
     3            nu,co,X_BLMethod)
        Endif

        IF ((TBL_Method .EQ. 1).OR.(TBL_Method .EQ. 2)) then
         IF (TBL_Method .EQ. 1) then
             CALL WrScr1('Computing turbulent boundary layer noise using BPM
     1')
             write(*,*) '==================================================
     1'
         endif
         CALL TBLTE(aofa,a_chord,U,FRCEN,ITRIP,SPLP,
     1           SPLS,SPLALPH,SPLTBL,THETA,PHI,L,R,
     2           NFREQ,nu,co,X_BLMethod)
        endif
     
        IF ((TBL_Method .EQ. 2)) then
         CALL WrScr1('Computing turbulent boundary layer noise using TNO
     1')
          write(*,*) '==================================================
     1'
         CALL TNO_TBLTE(SPLP,SPLS,SPLALPH,SPLTBL,
     2                  THETA,PHI,L,R)
        endif

        IF (IBLUNT .EQ. 1) then
          CALL WrScr1('Computing trailing edge bluntness noise')
          write(*,*) '======================================='
          CALL BLUNT(aofa,a_chord,U,FRCEN,ITRIP,SPLBLNT,
     1             THETA,PHI,L,R,H,PSI,
     2             NFREQ,nu,co,X_BLMethod)
        endif

C        IF ((ITIP .EQ. 1) .AND. (III .EQ. NSEG))
C     1    CALL TIPNOIS(ALPHTIP,ALPRAT,C(III),U(III),FRCEN,SPLTIP,
C     2                 THETA,PHI,R(III),NFREQ,VISC,C0,ROUND)
        IF (TI_Method .EQ. 1 .OR. TI_Method .EQ. 2 .OR. 
     1      TI_Method .EQ. 3) then
           CALL WrScr1('Computing turbulent inflow noise')
           write(*,*) '================================'
           CALL InflowNoise(aofa,U,TINoise,TurbLength,
     2                      rho,a_chord,L,R,THETA,
     3                      PHI,NFREQ,FRCEN,co,SPLti)
        endif
        IF (TI_Method .EQ. 2)
     1     CALL TI_Noise
        IF (TI_Method .EQ. 3) 
     1     CALL Simple_TI (DSPL_TI,thick_1p,thick_10p,a_chord,U)


C      ADD IN THIS SEGMENT'S CONTRIBUTION ON A MEAN-SQUARE
C      PRESSURE BASIS
C      ---------------------------------------------------

        DO 989 I=1,NFREQ

          IF ((ILAM .EQ. 1) .AND. (ITRIP .EQ. 0)) THEN
            P5(I) = P5(I) + 10.**(SPLLBL(I)/10.)
          ENDIF

          IF (TBL_Method .EQ. 1 .OR. TBL_Method .EQ. 2) THEN
            P1(I) = P1(I) + 10.**(SPLP(I)/10.)
            P2(I) = P2(I) + 10.**(SPLS(I)/10.)
            P3(I) = P3(I) + 10.**(SPLALPH(I)/10.)
          ENDIF

          IF (IBLUNT .EQ. 1) THEN
            P6(I) = P6(I) + 10.**(SPLBLNT(I)/10.)
          ENDIF

C          IF ((ITIP .EQ. 1) .AND. (III .EQ. NSEG)) THEN
C            P7(I) = P7(I) + 10.**(SPLTIP(I)/10.)
C          ENDIF
          
          IF (TI_Method .EQ. 1) THEN
            P8(I) = P8(I) + 10.**(SPLti(I)/10.)
          ENDIF
          IF (TI_Method .EQ. 2 .OR. TI_Method .EQ. 3) THEN
            SPLti(I)= SPLti(I) + DSPL_TI(I) + 10  ! +10 is fudge factor to match NLR data
            P8(I) = P8(I) + 10.**(SPLti(I)/10.)
          ENDIF


C      COMPUTE TOTAL PRESSURE FOR THE SEGMENT FOR ALL MECHANISMS
C      ---------------------------------------------------------

          P4(I) = P1(I) + P2(I) + P3(I) + P5(I) + P6(I) + P7(I) + P8(I)

  989   CONTINUE

C      CONTRIBUTIONS FROM ALL SEGMENTS ARE NOW ACCOUNTED FOR.
C      COMPUTE SOUND PRESSURE LEVELS FOR EACH MECHANISM AND
C      FOR THE TOTAL
C      -----------------------------------------------------

      DO 6003 I=1,NFREQ
        IF (P1(I) .NE. 0.) SPL(1,I) = 10.*ALOG10(P1(I))
        IF (P2(I) .NE. 0.) SPL(2,I) = 10.*ALOG10(P2(I))
        IF (P3(I) .NE. 0.) SPL(3,I) = 10.*ALOG10(P3(I))
        IF (P4(I) .NE. 0.) SPL(4,I) = 10.*ALOG10(P4(I))
        IF (P5(I) .NE. 0.) SPL(5,I) = 10.*ALOG10(P5(I))
        IF (P6(I) .NE. 0.) SPL(6,I) = 10.*ALOG10(P6(I))
C        IF (P7(I) .NE. 0.) SPL(7,I) = 10.*ALOG10(P7(I))
        IF (P8(I) .NE. 0.) SPL(8,I) = 10.*ALOG10(P8(I))
 6003 CONTINUE


C      WRITE OUTPUT FILE
C      -----------------

      WRITE(5,7000)

      DO 6005 I=1,NFREQ

        WRITE(5,7100) FRCEN(I),(SPL(J,I),J=1,3),(SPL(J,I),J=5,6),
     1                          SPL(8,I),SPL(4,I)
c        IF (MOD(I,5) .EQ. 0) WRITE(5,7200)
 6005 CONTINUE
      Close (5)

 7000 FORMAT(52X,'ONE-THIRD OCTAVE',/,50X,'SOUND PRESSURE LEVELS',
     1       ////,5X,'               ','   PRESSURE    ',
     2                '    SUCTION    ','  SEPARATION   '/,
     3             5X,' FREQUENCY(HZ) ','   SIDE TBL    ',
     4                '   SIDE TBL    ','   SIDE TBL    ',
     5                '    LAMINAR    ','  BLUNTNESS    ',
     6                '    INFLOW     ','    TOTAL      ',
     7                /,5X,8('-------------- '),/)
     
 7100 FORMAT(9F15.3)
 7200 FORMAT(' ')


      STOP
      END
      SUBROUTINE LBLVS(ALPSTAR,C,U ,FRCEN,SPLLAM,THETA,PHI,L,R,
     1                 NFREQ,VISC,C0,X_BLMethod)

      USE Third_Octave_Bands



C                  --------------------------------
C                  ***** VARIABLE DEFINITIONS *****
C                  --------------------------------

C       VARIABLE NAME               DEFINITION                  UNITS
C       -------------               ----------                  -----


C       ALPSTAR             ANGLE OF ATTACK                   DEGREES
C       C                  CHORD LENGTH                       METERS
C       C0                 SPEED OF SOUND                     METERS/SEC
C       D                  REYNOLDS NUMBER RATIO              ---
C       DBARH              HIGH FREQUENCY DIRECTIVITY         ---
C       DELTAP             PRESSURE SIDE BOUNDARY LAYER 
C                            THICKNESS                        METERS
C       DSTRP              PRESSURE SIDE BOUNDARY LAYER
C                            DISPLACEMENT THICKNESS           METERS
C       DSTRS              SUCTION SIDE BOUNDARY LAYER
C                            DISPLACEMENT THICKNESS           METERS
C       E                  STROUHAL NUMBER RATIO              ---
C       FRCEN              1/3 OCTAVE FREQUENCIES             HERTZ
C       G1                 SOUND PRESSURE LEVEL FUNCTION      DB
C       G2                 OVERALL SOUND PRESSURE LEVEL
C                            FUNCTION                         DB
C       G3                 OVERALL SOUND PRESSURE LEVEL
C                            FUNCTION                         DB
C       ITRIP              FLAG TO TRIP BOUNDARY LAYER        ---
C       L                  SPAN                               METERS
C       M                  MACH NUMBER                        ---
C       NFREQ              NUMBER OF FREQUENCIES              ---
C       OASPL              OVERALL SOUND PRESSURE LEVEL       DB
C       PHI                DIRECTIVITY ANGLE                  DEGREES
C       R                  OBSERVER DISTANCE FROM SEGMENT     METERS
C       RC                 REYNOLDS NUMBER BASED ON CHORD     ---
C       RC0                REFERENCE REYNOLDS NUMBER          ---
C       SCALE              GEOMETRIC SCALING TERM
C       SPLLAM             SOUND PRESSURE LEVEL DUE TO
C                            LAMINAR MECHANISM                DB
C       STPRIM             STROUHAL NUMBER BASED ON PRESSURE
C                            SIDE BOUNDARY LAYER THICKNESS    ---
C       ST1PRIM            REFERENCE STROUHAL NUMBER          ---
C       STPKPRM            PEAK STROUHAL NUMBER               ---
C       THETA              DIRECTIVITY ANGLE                  DEGREES
C       U                  FREESTREAM VELOCITY                METERS/SEC
C       VISC               KINEMATIC VISCOSITY                M2/SEC



      DIMENSION STPRIM(NumBands)  ,SPLLAM(NumBands)    ,FRCEN(NumBands)

      REAL      L                   ,M
	INTEGER X_BLMethod
 

C      COMPUTE REYNOLDS NUMBER AND MACH NUMBER
C      ---------------------------------------

      M        = U  / C0
      RC       = U  * C/VISC


C      COMPUTE BOUNDARY LAYER THICKNESSES
C      ----------------------------------
      SELECT CASE (X_BLMethod) 
      CASE (2)
	   CALL XTHICK_CALC(DELTAP,DSTRS,DSTRP)
	CASE DEFAULT 
         CALL THICK(C,U ,ALPSTAR,ITRIP,DELTAP,DSTRS,DSTRP,C0,VISC)
	END SELECT



C      COMPUTE DIRECTIVITY FUNCTION
C      ----------------------------

      CALL DIRECTH(M,THETA,PHI,DBARH)



C      COMPUTE REFERENCE STROUHAL NUMBER
C      ---------------------------------

      IF (RC .LE. 1.3E+05) ST1PRIM = .18
      IF((RC .GT. 1.3E+05).AND.(RC.LE.4.0E+05))ST1PRIM=.001756*RC**.3931
      IF (RC .GT. 4.0E+05) ST1PRIM = .28

      STPKPRM  = 10.**(-.04*ALPSTAR) * ST1PRIM



C      COMPUTE REFERENCE REYNOLDS NUMBER
C      ---------------------------------

      IF (ALPSTAR .LE. 3.0) RC0=10.**(.215*ALPSTAR+4.978)
      IF (ALPSTAR .GT. 3.0) RC0=10.**(.120*ALPSTAR+5.263)




C      COMPUTE PEAK SCALED SPECTRUM LEVEL
C      ----------------------------------

      D   = RC / RC0

      IF (D .LE. .3237) G2=77.852*ALOG10(D)+15.328
      IF ((D .GT. .3237).AND.(D .LE. .5689))
     1  G2 = 65.188*ALOG10(D) + 9.125
      IF ((D .GT. .5689).AND.(D .LE. 1.7579))
     1  G2 = -114.052 * ALOG10(D)**2.
      IF ((D .GT. 1.7579).AND.(D .LE. 3.0889))
     1  G2 = -65.188*ALOG10(D)+9.125
      IF (D .GT. 3.0889) G2 =-77.852*ALOG10(D)+15.328


      G3      = 171.04 - 3.03 * ALPSTAR

      SCALE   = 10. * ALOG10(DELTAP*M**5*DBARH*L/R**2)



C      COMPUTE SCALED SOUND PRESSURE LEVELS FOR EACH STROUHAL NUMBER
C      -------------------------------------------------------------

      DO 100 I=1,NFREQ

         STPRIM(I)  = FRCEN(I) * DELTAP / U 
         
         E          = STPRIM(I) / STPKPRM

         IF (E .LT. .5974) G1=39.8*ALOG10(E)-11.12
         IF ((E .GE. .5974).AND.(E .LE. .8545))
     1     G1 = 98.409 * ALOG10(E) + 2.0
         IF ((E .GE. .8545).AND.(E .LT. 1.17))
     1     G1 = -5.076+SQRT(2.484-506.25*(ALOG10(E))**2.)
         IF ((E .GE. 1.17).AND.(E .LT. 1.674))
     1     G1 = -98.409 * ALOG10(E) + 2.0
         IF (E .GE. 1.674) G1=-39.80*ALOG10(E)-11.12

         SPLLAM(I) = G1 + G2 + G3 + SCALE

  100 CONTINUE

      RETURN
      END
      SUBROUTINE TBLTE(ALPSTAR,C,U ,FRCEN,ITRIP,SPLP,SPLS,
     1              SPLALPH,SPLTBL,THETA,PHI,L,R,NFREQ,VISC,C0,
     2              X_BLMethod)



C                  --------------------------------
C                  ***** VARIABLE DEFINITIONS *****
C                  --------------------------------



C       VARIABLE NAME               DEFINITION                  UNITS
C       -------------               ----------                  -----

C       A                  STROUHAL NUMBER RATIO                 ---
C       A0                 FUNCTION USED IN 'A' CALCULATION      ---
C       A02                FUNCTION USED IN 'A' CALCULATION      ---
C       AA                 'A' SPECTRUM SHAPE EVALUATED AT
C                             STROUHAL NUMBER RATIO              DB
C       ALPSTAR            ANGLE OF ATTACK                     DEGREES
C       AMAXA              MAXIMUM 'A' CURVE EVALUATED AT
C                            STROUHAL NUMBER RATIO                DB
C       AMAXA0             MAXIMUM 'A' CURVE EVALUATED AT A0      DB
C       AMAXA02            MAXIMUM 'A' CURVE EVALUATED AT A02     DB
C       AMAXB              MAXIMUM 'A' CURVE EVALUATED AT B       DB
C       AMINA              MINIMUM 'A' CURVE EVALUATED AT 
C                            STROUHAL NUMBER RATIO                DB
C       AMINA0             MINIMUM 'A' CURVE EVALUATED AT A0      DB
C       AMINA02            MINIMUM 'A' CURVE EVALUATED AT A02     DB
C       AMINB              MINIMUM 'A' CURVE EVALUATED AT B       DB
C       ARA0               INTERPOLATION FACTOR                  ---
C       ARA02              INTERPOLATION FACTOR                  ---
C       B                  STROUHAL NUMBER RATIO                 ---
C       B0                 FUNCTION USED IN 'B' CALCULATION      ---
C       BB                 'B' SPECTRUM SHAPE EVALUATED AT
C                            STROUHAL NUMBER RATIO                DB
C       BETA               USED IN 'B' COMPUTATION               ---
C       BETA0              USED IN 'B' COMPUTATION               ---
C       BMAXB              MAXIMUM 'B' EVALUATED AT B             DB
C       BMAXB0             MAXIMUM 'B' EVALUATED AT B0            DB
C       BMINB              MINIMUM 'B' EVALUATED AT B             DB
C       BMINB0             MINIMUM 'B' EVALUATED AT B0            DB
C       BRB0               INTERPOLATION FACTOR                   DB
C       C                  CHORD LENGTH                          METERS
C       C0                 SPEED OF SOUND                      METERS/SEC
C       DBARH              HIGH FREQUENCY DIRECTIVITY             ---
C       DBARL              LOW FREQUENCY DIRECTIVITY              ---
C       DELK1              CORRECTION TO AMPLITUDE FUNCTION       DB
C       DELTAP             PRESSURE SIDE BOUNDARY LAYER THICKNESS METERS
C       DSTRP              PRESSURE SIDE DISPLACEMENT THICKNESS  METERS
C       DSTRS              SUCTION SIDE DISPLACEMENT THICKNESS   METERS
C       FRCEN              ARRAY OF CENTERED FREQUENCIES         HERTZ
C       GAMMA              USED IN 'B' COMPUTATION                ---
C       GAMMA0             USED IN 'B' COMPUTATION                ---
C       ITRIP              TRIGGER TO TRIP BOUNDARY LAYER         ---
C       K1                 AMPLITUDE FUNCTION                     DB
C       K2                 AMPLITUDE FUNCTION                     DB
C       L                  SPAN                                  METERS
C       M                  MACH NUMBER                            ---
C       NFREQ              NUMBER OF CENTERED FREQUENCIES         ---
C       PHI                DIRECTIVITY ANGLE                    DEGREES
C       P1                 PRESSURE SIDE PRESSURE               NT/M2
C       P2                 SUCTION SIDE PRESSURE                NT/M2
C       P4                 PRESSURE FROM ANGLE OF ATTACK
C                            CONTRIBUTION                       NT/M2
C       R                  SOURCE TO OBSERVER DISTANCE           METERS
C       RC                 REYNOLDS NUMBER BASED ON  CHORD        ---
C       RDSTRP             REYNOLDS NUMBER BASED ON PRESSURE
C                            SIDE DISPLACEMENT THICKNESS          ---
C       RDSTRS             REYNOLDS NUMBER BASED ON SUCTION
C                            SIDE DISPLACEMENT THICKNESS          ---
C       SPLALPH            SOUND PRESSURE LEVEL DUE TO ANGLE OF 
C                            ATTACK CONTRIBUTION                  DB
C       SPLP               SOUND PRESSURE LEVEL DUE TO PRESSURE
C                            SIDE OF AIRFOIL                      DB
C       SPLS               SOUND PRESSURE LEVEL DUE TO SUCTION
C                            SIDE OF AIRFOIL                      DB
C       SPLTBL             TOTAL SOUND PRESSURE LEVEL DUE TO 
C                            TBLTE MECHANISM                      DB
C       STP                PRESSURE SIDE STROUHAL NUMBER          ---
C       STS                SUCTION SIDE STROUHAL NUMBER           ---
C       ST1                PEAK STROUHAL NUMBER                   ---
C       ST1PRIM            PEAK STROUHAL NUMBER                   ---
C       ST2                PEAK STROUHAL NUMBER                   ---
C       STPEAK             PEAK STROUHAL NUMBER                   ---
C       SWITCH             LOGICAL FOR COMPUTATION OF ANGLE 
C                            OF ATTACK CONTRIBUTION               ---
C       THETA              DIRECTIVITY ANGLE                     DEGREES
C       U                  VELOCITY                             METERS/SEC
C       VISC               KINEMATIC VISCOSITY                   M2/SEC
C       XCHECK             USED TO CHECK FOR ANGLE OF ATTACK
C                            CONTRIBUTION                         ---
C       

      USE Third_OCtave_Bands


      DIMENSION SPLTBL(NumBands)  ,SPLP(NumBands)    ,SPLS(NumBands)  ,
     1          SPLALPH(NumBands) ,STP(NumBands)     ,
     1          STS(NumBands)     ,FRCEN(NumBands)

      LOGICAL SWITCH
      INTEGER X_BLMethod
      REAL    L,M,K1,K2

      RC       = U  * C / VISC
      M        = U  / C0


C      COMPUTE BOUNDARY LAYER THICKNESSES
C      ----------------------------------

      SELECT CASE (X_BLMethod) 
      CASE (2)
	   CALL XTHICK_CALC(DELTAP,DSTRS,DSTRP)
	CASE DEFAULT 
         CALL THICK(C,U ,ALPSTAR,ITRIP,DELTAP,DSTRS,DSTRP,C0,VISC)
	END SELECT
c	write (5,*)"Delta*_Suction Delta*_Pressure"
c	write(5,*) dstrs, dstrp

C     COMPUTE DIRECTIVITY FUNCTION
C     ----------------------------

      CALL DIRECTL(M,THETA,PHI,DBARL)
      CALL DIRECTH(M,THETA,PHI,DBARH)


C     CALCULATE THE REYNOLDS NUMBERS BASED ON PRESSURE AND
C     SUCTION DISPLACEMENT THICKNESS
C     ---------------------------------------------------

      RDSTRS = DSTRS * U  / VISC
      RDSTRP = DSTRP * U  / VISC

C      DETERMINE PEAK STROUHAL NUMBERS TO BE USED FOR
C      'A' AND 'B' CURVE CALCULATIONS
C      ----------------------------------------------

      ST1    = .02 * M ** (-.6)

      IF (ALPSTAR .LE. 1.333) ST2 = ST1
      IF ((ALPSTAR .GT. 1.333).AND.(ALPSTAR .LE. 12.5))
     1   ST2 = ST1*10.**(.0054*(ALPSTAR-1.333)**2.)
      IF (ALPSTAR .GT. 12.5) ST2 = 4.72 * ST1


      ST1PRIM = (ST1+ST2)/2.


      CALL A0COMP(RC,A0)
      CALL A0COMP(3.*RC,A02)

C      EVALUATE MINIMUM AND MAXIMUM 'A' CURVES AT A0
C      ----------------------------------------------

      CALL AMIN(A0,AMINA0)
      CALL AMAX(A0,AMAXA0)

      CALL AMIN(A02,AMINA02)
      CALL AMAX(A02,AMAXA02)

C      COMPUTE 'A' MAX/MIN RATIO
C      -------------------------

      ARA0  = (20. + AMINA0) / (AMINA0 - AMAXA0)
      ARA02 = (20. + AMINA02)/ (AMINA02- AMAXA02)

C      COMPUTE B0 TO BE USED IN 'B' CURVE CALCULATIONS
C      -----------------------------------------------

      IF (RC .LT. 9.52E+04) B0 = .30
      IF ((RC .GE. 9.52E+04).AND.(RC .LT. 8.57E+05))
     1    B0 = (-4.48E-13)*(RC-8.57E+05)**2. + .56
      IF (RC .GE. 8.57E+05) B0 = .56

C      EVALUATE MINIMUM AND MAXIMUM 'B' CURVES AT B0
C      ----------------------------------------------

      CALL BMIN(B0,BMINB0)
      CALL BMAX(B0,BMAXB0)

C      COMPUTE 'B' MAX/MIN RATIO
C      -------------------------

      BRB0  = (20. + BMINB0) / (BMINB0 - BMAXB0)

C      FOR EACH CENTER FREQUENCY, COMPUTE AN
C      'A' PREDICTION FOR THE PRESSURE SIDE
C      -------------------------------------

      STPEAK = ST1

      DO 100 I=1,NFREQ
        STP(I) = FRCEN(I) * DSTRP / U 
        A      = ALOG10( STP(I) / STPEAK )
        CALL AMIN(A,AMINA)
        CALL AMAX(A,AMAXA)
        AA     = AMINA + ARA0 * (AMAXA - AMINA)

        IF (RC .LT. 2.47E+05) K1 = -4.31 * ALOG10(RC) + 156.3
        IF((RC .GE. 2.47E+05).AND.(RC .LT. 8.0E+05))
     1    K1 = -9.0 * ALOG10(RC) + 181.6
        IF (RC .GT. 8.0E+05) K1 = 128.5

        IF (RDSTRP .LE. 5000.) DELK1 = -ALPSTAR*(5.29-1.43*
     1    ALOG10(RDSTRP))
        IF (RDSTRP .GT. 5000.) DELK1 = 0.0

        SPLP(I)=AA+K1-3.+10.*ALOG10(DSTRP*M**5.*DBARH*L/R**2.)+DELK1




      GAMMA   = 27.094 * M +  3.31
      BETA    = 72.650 * M + 10.74
      GAMMA0  = 23.430 * M +  4.651
      BETA0   =-34.190 * M - 13.820

      IF (ALPSTAR .LE. (GAMMA0-GAMMA)) K2 = -1000.0
      IF ((ALPSTAR.GT.(GAMMA0-GAMMA)).AND.(ALPSTAR.LE.(GAMMA0+GAMMA)))
     1 K2=SQRT(BETA**2.-(BETA/GAMMA)**2.*(ALPSTAR-GAMMA0)**2.)+BETA0
      IF (ALPSTAR .GT. (GAMMA0+GAMMA)) K2 = -12.0

      K2 = K2 + K1



      STS(I) = FRCEN(I) * DSTRS / U 

C      CHECK FOR 'A' COMPUTATION FOR SUCTION SIDE
C      ------------------------------------------

      XCHECK = GAMMA0
      SWITCH = .FALSE.
      IF ((ALPSTAR .GE. XCHECK).OR.(ALPSTAR .GT. 12.5))SWITCH=.TRUE.
      IF (.NOT. SWITCH) THEN
        A      = ALOG10( STS(I) / ST1PRIM )
        CALL AMIN(A,AMINA)
        CALL AMAX(A,AMAXA)
        AA = AMINA + ARA0 * (AMAXA - AMINA)

        SPLS(I) = AA+K1-3.+10.*ALOG10(DSTRS*M**5.*DBARH*
     1            L/R**2.) 

C      'B' CURVE COMPUTATION
C       --------------------

        B = ABS(ALOG10(STS(I) / ST2))
        CALL BMIN(B,BMINB)
        CALL BMAX(B,BMAXB)
        BB = BMINB + BRB0 * (BMAXB-BMINB)
        SPLALPH(I)=BB+K2+10.*ALOG10(DSTRS*M**5.*DBARH*L/R**2.) 

      ELSE

C       THE 'A' COMPUTATION IS DROPPED IF 'SWITCH' IS TRUE
C       --------------------------------------------------


        SPLS(I) = 0.0 + 10.*ALOG10(DSTRS*M**5.*DBARL*
     1              L/R**2.) 
        SPLP(I) = 0.0 + 10.*ALOG10(DSTRS*M**5.*DBARL*
     1              L/R**2.) 
        B = ABS(ALOG10(STS(I) / ST2))
        CALL AMIN(B,AMINB)
        CALL AMAX(B,AMAXB)
        BB = AMINB + ARA02 * (AMAXB-AMINB)
        SPLALPH(I)=BB+K2+10.*ALOG10(DSTRS*M**5.*DBARL*
     1           L/R**2.)  
      ENDIF


C      SUM ALL CONTRIBUTIONS FROM 'A' AND 'B' ON BOTH 
C      PRESSURE AND SUCTION SIDE ON A MEAN-SQUARE PRESSURE
C      BASIS
C      ---------------------------------------------------

      IF (SPLP(I)    .LT. -100.) SPLP(I)    = -100.
      IF (SPLS(I)    .LT. -100.) SPLS(I)    = -100.
      IF (SPLALPH(I) .LT. -100.) SPLALPH(I) = -100.

      P1  = 10.**(SPLP(I) / 10.)
      P2  = 10.**(SPLS(I) / 10.)
      P4  = 10.**(SPLALPH(I) / 10.)

      SPLTBL(I) = 10. * ALOG10(P1 + P2 + P4)

  100 CONTINUE

      RETURN
      END
      
      SUBROUTINE AMIN(A,AMINA)

C     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
C     TO THE A-CURVE FOR THE MINIMUM ALLOWED REYNOLDS NUMBER.
C     

      X1 = ABS(A)
    
      IF (X1 .LE. .204) AMINA=SQRT(67.552-886.788*X1**2.)-8.219
      IF((X1 .GT. .204).AND.(X1 .LE. .244))AMINA=-32.665*X1+3.981
      IF (X1 .GT. .244)AMINA=-142.795*X1**3.+103.656*X1**2.-57.757*X1+6.006

      RETURN
      END
      SUBROUTINE AMAX(A,AMAXA)

C     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
C     TO THE A-CURVE FOR THE MAXIMUM ALLOWED REYNOLDS NUMBER.

      X1 = ABS(A)

      IF (X1 .LE. .13)AMAXA=SQRT(67.552-886.788*X1**2.)-8.219
      IF((X1 .GT. .13).AND.(X1 .LE. .321))AMAXA=-15.901*X1+1.098
      IF (X1 .GT. .321)AMAXA=-4.669*X1**3.+3.491*X1**2.-16.699*X1+1.149

      RETURN
      END
      SUBROUTINE BMIN(B,BMINB)

C     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING 
C     TO THE B-CURVE FOR THE MINIMUM ALLOWED REYNOLDS NUMBER.

      X1 = ABS(B)
   
      IF (X1 .LE. .13)BMINB=SQRT(16.888-886.788*X1**2.)-4.109
      IF((X1 .GT. .13).AND.(X1 .LE. .145))BMINB=-83.607*X1+8.138
      IF (X1.GT..145)BMINB=-817.81*X1**3.+355.21*X1**2.-135.024*X1+10.619

      RETURN
      END
      SUBROUTINE BMAX(B,BMAXB)

C     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
C     TO THE B-CURVE FOR THE MAXIMUM ALLOWED REYNOLDS NUMBER.

      X1 = ABS(B)

      IF (X1 .LE. .1) BMAXB=SQRT(16.888-886.788*X1**2.)-4.109
      IF((X1 .GT. .1).AND.(X1 .LE. .187))BMAXB=-31.313*X1+1.854
      IF (X1.GT..187)BMAXB=-80.541*X1**3.+44.174*X1**2.-39.381*X1+2.344

      RETURN
      END
      SUBROUTINE A0COMP(RC,A0) 

C     THIS SUBROUTINE DETERMINES WHERE THE A-CURVE 
C     TAKES ON A VALUE OF -20 dB.

      IF (RC .LT. 9.52E+04) A0 = .57
      IF ((RC .GE. 9.52E+04).AND.(RC .LT. 8.57E+05))
     1   A0 = (-9.57E-13)*(RC-8.57E+05)**2. + 1.13
      IF (RC .GE. 8.57E+05) A0 = 1.13
      RETURN
      END
      SUBROUTINE DIRECTH(M,THETA,PHI,DBAR)

C     THIS SUBROUTINE COMPUTES THE HIGH FREQUENCY
C     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION

      REAL M,MC

      DEGRAD  = .017453

      MC     = .8 * M
      THETAR = THETA * DEGRAD
      PHIR   = PHI * DEGRAD

      DBAR=2.*SIN(THETAR/2.)**2.*SIN(PHIR)**2./((1.+M*COS(THETAR))*
     1      (1.+(M-MC)*COS(THETAR))**2.)
      RETURN
      END
      SUBROUTINE DIRECTL(M,THETA,PHI,DBAR)

C     THIS SUBROUTINE COMPUTES THE LOW FREQUENCY
C     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION

      REAL M,MC

      DEGRAD  = .017453

      MC     = .8 * M
      THETAR = THETA * DEGRAD
      PHIR   = PHI * DEGRAD

      DBAR = (SIN(THETAR)*SIN(PHIR))**2/(1.+M*COS(THETAR))**4

      RETURN
      END
      SUBROUTINE BLUNT(ALPSTAR,C,U ,FRCEN,ITRIP,SPLBLNT,THETA,PHI,
     1                 L,R,H,PSI,NFREQ,VISC,C0,X_BLMethod)


C                  --------------------------------
C                  ***** VARIABLE DEFINITIONS *****
C                  --------------------------------

C       VARIABLE NAME               DEFINITION                  UNITS
C       -------------               ----------                  -----

C       ALPSTAR            ANGLE OF ATTACK                     DEGREES
C       ATERM              USED TO COMPUTE PEAK STROUHAL NO.    ---
C       C                  CHORD LENGTH                        METERS
C       C0                 SPEED OF SOUND                      METERS/SEC
C       DBARH              HIGH FREQUENCY DIRECTIVITY           ---
C       DELTAP             PRESSURE SIDE BOUNDARY LAYER 
C                            THICKNESS                          METERS
C       DSTARH             AVERAGE DISPLACEMENT THICKNESS
C                            OVER TRAILING EDGE BLUNTNESS       ---
C       DSTRAVG            AVERAGE DISPLACEMENT THICKNESS       METERS
C       DSTRP              PRESSURE SIDE DISPLACEMENT THICKNESS METERS
C       DSTRS              SUCTION SIDE DISPLACEMENT THICKNESS  METERS
C       ETA                RATIO OF STROUHAL NUMBERS             ---
C       FRCEN              ARRAY OF 1/3 OCTAVE CENTERED FREQ.   HERTZ
C       F4TEMP             G5 EVALUATED AT MINIMUM HDSTARP       DB
C       G4                 SCALED SPECTRUM LEVEL                 DB
C       G5                 SPECTRUM SHAPE FUNCTION               DB
C       G50                G5 EVALUATED AT PSI=0.0               DB
C       G514               G5 EVALUATED AT PSI=14.0              DB
C       H                  TRAILING EDGE BLUNTNESS              METERS
C       HDSTAR             BLUNTNESS OVER AVERAGE DISPLACEMENT 
C                            THICKNESS                           ---
C       HDSTARL            MINIMUM ALLOWED VALUE OF HDSTAR       ---
C       HDSTARP            MODIFIED VALUE OF HDSTAR              ---
C       ITRIP              TRIGGER FOR BOUNDARY LAYER TRIPPING    ---
C       L                  SPAN                                  METERS
C       M                  MACH NUMBER                           ---
C       NFREQ              NUMBER OF CENTERED FREQUENCIES        ---
C       PHI                DIRECTIVITY ANGLE                    DEGREES
C       PSI                TRAILING EDGE ANGLE                  DEGREES
C       R                  SOURCE TO OBSERVER DISTANCE           METERS
C       RC                 REYNOLDS NUMBER BASED ON CHORD        ---
C       SCALE              SCALING FACTOR                        ---
C       SPLBLNT            SOUND PRESSURE LEVELS DUE TO 
C                            BLUNTNESS                            DB
C       STPEAK             PEAK STROUHAL NUMBER                  ---
C       STPPP              STROUHAL NUMBER                       ---
C       THETA              DIRECTIVITY ANGLE                     ---
C       U                  FREESTREAM VELOCITY                 METERS/SEC
C       VISC               KINEMATIC VISCOSITY                 M2/SEC

      
      USE Third_OCtave_Bands
       
      DIMENSION SPLBLNT(NumBands)  ,FRCEN(NumBands)   ,STPPP(NumBands)

      REAL M,L
	INTEGER X_BLMethod

C      COMPUTE NECESSARY QUANTITIES
C      ----------------------------

      M  = U /C0
      RC = U  * C / VISC


C      COMPUTE BOUNDARY LAYER THICKNESSES
C      ----------------------------------

      SELECT CASE (X_BLMethod) 
      CASE (2)
	   CALL XTHICK_CALC(DELTAP,DSTRS,DSTRP)
	CASE DEFAULT 
         CALL THICK(C,U ,ALPSTAR,ITRIP,DELTAP,DSTRS,DSTRP,C0,VISC)
	END SELECT

C      COMPUTE AVERAGE DISPLACEMENT THICKNESS
C      --------------------------------------

      DSTRAVG = (DSTRS + DSTRP) / 2.
      HDSTAR  = H / DSTRAVG
 
      DSTARH = 1. /HDSTAR

C      COMPUTE DIRECTIVITY FUNCTION
C      ----------------------------

      CALL DIRECTH(M,THETA,PHI,DBARH)


C      COMPUTE PEAK STROUHAL NUMBER
C      ----------------------------

      ATERM  = .212 - .0045 * PSI

      IF (HDSTAR .GE. .2)
     1   STPEAK    = ATERM / (1.+.235*DSTARH-.0132*DSTARH**2.)
      IF (HDSTAR .LT. .2) 
     1   STPEAK    = .1 * HDSTAR + .095 - .00243 * PSI

C      COMPUTE SCALED SPECTRUM LEVEL
C      -----------------------------

      IF (HDSTAR .LE. 5.) G4=17.5*ALOG10(HDSTAR)+157.5-1.114*PSI
      IF (HDSTAR .GT. 5.) G4=169.7 - 1.114 * PSI


C      FOR EACH FREQUENCY, COMPUTE SPECTRUM SHAPE REFERENCED TO 0 DB
C      -------------------------------------------------------------

      DO 1000 I=1,NFREQ

        STPPP(I) = FRCEN(I) * H / U 
        ETA      = ALOG10(STPPP(I)/STPEAK)

        HDSTARL = HDSTAR

        CALL G5COMP(HDSTARL,ETA,G514)

        HDSTARP = 6.724 * HDSTAR **2.-4.019*HDSTAR+1.107

        CALL G5COMP(HDSTARP,ETA,G50)


        G5 = G50 + .0714 * PSI * (G514-G50)
        IF (G5 .GT. 0.) G5 = 0.
        CALL G5COMP(.25,ETA,F4TEMP)
        IF (G5 .GT. F4TEMP) G5 = F4TEMP


        SCALE = 10. * ALOG10(M**5.5*H*DBARH*L/R**2.)

        SPLBLNT(I) = G4 + G5 + SCALE


 1000 CONTINUE

      RETURN  
      END
      SUBROUTINE G5COMP(HDSTAR,ETA,G5)


      REAL M,K,MU

      
      IF (HDSTAR .LT. .25) MU = .1211
      IF ((HDSTAR .GT. .25).AND.(HDSTAR .LE. .62))
     1     MU=-.2175*HDSTAR + .1755
      IF ((HDSTAR .GT. .62).AND.(HDSTAR .LT. 1.15))
     1 MU = -.0308 * HDSTAR + .0596
      IF (HDSTAR .GE. 1.15)MU = .0242

      IF (HDSTAR .LE. .02) M = 0.0
      IF ((HDSTAR .GE. .02).AND.(HDSTAR .LT. .5))
     1    M=68.724*HDSTAR - 1.35     
      IF ((HDSTAR .GT. .5).AND.(HDSTAR .LE. .62))
     1  M = 308.475 * HDSTAR - 121.23
      IF ((HDSTAR .GT. .62).AND.(HDSTAR .LE. 1.15))
     1  M = 224.811 * HDSTAR - 69.354
      IF ((HDSTAR .GT. 1.15) .AND. (HDSTAR .LT. 1.2))
     1  M = 1583.28 * HDSTAR - 1631.592
      IF (HDSTAR .GT. 1.2) M = 268.344
      IF (M .LT. 0.0) M = 0.0

      ETA0 = -SQRT((M*M*MU**4)/(6.25+M*M*MU*MU))

      K    = 2.5*SQRT(1.-(ETA0/MU)**2.)-2.5-M*ETA0

      IF (ETA .LE. ETA0) G5 = M * ETA + K
      IF ((ETA .GT. ETA0).AND.(ETA .LE. 0.))G5=2.5*SQRT(1.-(ETA/MU)**2.)-2.5
      IF((ETA.GT.0.).AND.(ETA.LE..03616))G5=SQRT(1.5625-1194.99*ETA**2.)-1.25
      IF (ETA .GT. .03616) G5=-155.543 * ETA + 4.375

      RETURN
      END
      SUBROUTINE TIPNOIS(ALPHTIP,ALPRAT,C,U ,FRCEN,SPLTIP,THETA,PHI,
     1                   R,NFREQ,VISC,C0,ROUND)

C                  --------------------------------
C                  ***** VARIABLE DEFINITIONS *****
C                  --------------------------------

C       VARIABLE NAME               DEFINITION                  UNITS
C       -------------               ----------                  -----

C       ALPHTIP            TIP ANGLE OF ATTACK                DEGREES
C       ALPRAT             TIP LIFT CURVE SLOPE                 ---
C       ALPTIPP            CORRECTED TIP ANGLE OF ATTACK      DEGREES
C       C                  CHORD LENGTH                         METERS
C       C0                 SPEED OF SOUND                    METERS/SEC
C       DBARH              DIRECTIVITY                         ---
C       FRCEN              CENTERED FREQUENCIES              HERTZ
C       L                  CHARACTERISTIC LENGTH FOR TIP      METERS
C       M                  MACH NUMBER                         ---
C       MM                 MAXIMUM MACH NUMBER                 ---
C       NFREQ              NUMBER OF CENTERED FREQUENCIES      ---
C       PHI                DIRECTIVITY ANGLE                  DEGREES
C       R                  SOURCE TO OBSERVER DISTANCE        METERS
C       ROUND              LOGICAL SET TRUE IF TIP IS ROUNDED  ---
C       SCALE              SCALING TERM                        ---
C       SPLTIP             SOUND PRESSURE LEVEL DUE TO TIP
C                            MECHANISM                         DB
C       STPP               STROUHAL NUMBER                     ---
C       TERM               SCALING TERM                        ---
C       THETA              DIRECTIVITY ANGLE                  DEGREES
C       U                  FREESTREAM VELOCITY               METERS/SEC
C       UM                 MAXIMUM VELOCITY                  METERS/SEC
C       VISC               KINEMATIC VISCOSITY               M2/SEC

      USE Third_Octave_Bands

      DIMENSION SPLTIP(NumBands),FRCEN(NumBands)
      REAL L,M,MM
      LOGICAL ROUND


      ALPTIPP = ALPHTIP * ALPRAT
      M       = U  / C0

      CALL DIRECTH(M,THETA,PHI,DBARH)

      IF (ROUND) THEN
        L = .008 * ALPTIPP * C
      ELSE
        IF (ABS(ALPTIPP) .LE. 2.) THEN
          L = (.023 + .0169*ALPTIPP) * C
        ELSE
          L = (.0378 + .0095*ALPTIPP) * C
        ENDIF
      ENDIF
        

      MM     = (1. + .036*ALPTIPP) * M

      UM     = MM * C0

      TERM  = M*M*MM**3.*L**2.*DBARH/R**2.
      IF (TERM .NE. 0.0) THEN
        SCALE = 10.*ALOG10(TERM)
      ELSE
        SCALE = 0.0
      ENDIF

      DO 100 I=1,NFREQ
        STPP      = FRCEN(I) * L / UM
        SPLTIP(I) = 126.-30.5*(ALOG10(STPP)+.3)**2. + SCALE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE THICK(C,U ,ALPSTAR,ITRIP,DELTAP,DSTRS,DSTRP,C0,VISC)

C                  --------------------------------
C                  ***** VARIABLE DEFINITIONS *****
C                  --------------------------------

C       VARIABLE NAME               DEFINITION                  UNITS
C       -------------               ----------                  -----

C       ALPSTAR            ANGLE OF ATTACK                    DEGREES
C       C                  CHORD LENGTH                        METERS
C       C0                 SPEED OF SOUND                    METERS/SEC
C       DELTA0             BOUNDARY LAYER THICKNESS AT
C                            ZERO ANGLE OF ATTACK              METERS
C       DELTAP             PRESSURE SIDE BOUNDARY LAYER
C                            THICKNESS                         METERS
C       DSTR0              DISPLACEMENT THICKNESS AT ZERO
C                            ANGLE OF ATTACK                   METERS
C       DSTRP              PRESSURE SIDE DISPLACEMENT 
C                            THICKNESS                         METERS
C       DSTRS              SUCTION SIDE DISPLACEMENT 
C                            THICKNESS                         METERS
C       ITRIP              TRIGGER FOR BOUNDARY LAYER TRIPPING  ---
C       M                  MACH NUMBER                          ---
C       RC                 REYNOLDS NUMBER BASED ON CHORD       ---
C       U                  FREESTREAM VELOCITY                METERS/SEC
C       VISC               KINEMATIC VISCOSITY                M2/SEC


C      COMPUTE ZERO ANGLE OF ATTACK BOUNDARY LAYER
C      THICKNESS (METERS) AND REYNOLDS NUMBER
C      -------------------------------------------

      M        = U  / C0

      RC       = U  * C/VISC

      DELTA0   = 10.**(1.6569-.9045*ALOG10(RC)+
     1           .0596*ALOG10(RC)**2.)*C
      IF (ITRIP .EQ. 2) DELTA0 = .6 * DELTA0


C      COMPUTE PRESSURE SIDE BOUNDARY LAYER THICKNESS
C      ----------------------------------------------

      DELTAP   = 10.**(-.04175*ALPSTAR+.00106*ALPSTAR**2.)*DELTA0


C      COMPUTE ZERO ANGLE OF ATTACK DISPLACEMENT THICKNESS
C      ---------------------------------------------------

      IF ((ITRIP .EQ. 1) .OR. (ITRIP .EQ. 2)) THEN
        IF (RC .LE. .3E+06) DSTR0 = .0601 * RC **(-.114)*C
        IF (RC .GT. .3E+06) 
     1    DSTR0=10.**(3.411-1.5397*ALOG10(RC)+.1059*ALOG10(RC)**2.)*C
        IF (ITRIP .EQ. 2) DSTR0 = DSTR0 * .6
      ELSE
        DSTR0=10.**(3.0187-1.5397*ALOG10(RC)+.1059*ALOG10(RC)**2.)*C
      ENDIF

C      PRESSURE SIDE DISPLACEMENT THICKNESS
C      ------------------------------------

      DSTRP   = 10.**(-.0432*ALPSTAR+.00113*ALPSTAR**2.)*DSTR0
      IF (ITRIP .EQ. 3) DSTRP = DSTRP * 1.48

C      SUCTION SIDE DISPLACEMENT THICKNESS
C      -----------------------------------

      IF (ITRIP .EQ. 1) THEN
        IF (ALPSTAR .LE. 5.) DSTRS=10.**(.0679*ALPSTAR)*DSTR0
        IF((ALPSTAR .GT. 5.).AND.(ALPSTAR .LE. 12.5))
     1   DSTRS = .381*10.**(.1516*ALPSTAR)*DSTR0
        IF (ALPSTAR .GT. 12.5)DSTRS=14.296*10.**(.0258*ALPSTAR)*DSTR0
      ELSE
        IF (ALPSTAR .LE. 7.5)DSTRS =10.**(.0679*ALPSTAR)*DSTR0
        IF((ALPSTAR .GT. 7.5).AND.(ALPSTAR .LE. 12.5))
     1   DSTRS = .0162*10.**(.3066*ALPSTAR)*DSTR0
        IF (ALPSTAR .GT. 12.5) DSTRS = 52.42*10.**(.0258*ALPSTAR)*DSTR0
      ENDIF
      
      RETURN
      END
      
      !=======================================================================
      SUBROUTINE InflowNoise(ALPSTAR,U,TINoise,LTurb,AirDens,Chord,d,
     1                       RObs,THETA,PHI,NFrequency,FrequencyCenter,
     2                       C0,SPLti)
      
      !IMPLICIT NONE
      
      INTEGER(4) :: I       ! A generic index for DO loops.
      
	REAL(4) :: ALPSTAR    ! Local angle of attack
      REAL(4) :: Mach       ! local mach number
      REAL(4) :: WaveNumber ! wave number - non-dimensional frequency
      REAL(4) :: U          ! local total velocity
      REAL(4) :: Chord       ! chord length
      REAL(4) :: Beta2       ! Prandtl-Glauert correction factor
      REAL(4) :: SPLhigh       ! predicted high frequency sound pressure level
      REAL(4) :: d       ! element semi-span
      REAL(4) :: Ke        ! Wavenumber range of energy-containing eddies
      REAL(4) :: Kbar        ! Normalized Wavenumber (K*b)
      REAL(4) :: Khat        ! Normalized Wavenumber (K/Ke)
      REAL(4) :: LTurb       ! turbulence length scale (isotropic integral scale parameter)
	REAL(4) :: mu          ! Parameter for determining frequency range
      REAL(4) :: RObs       ! distance to observer
      REAL(4) :: Ums       ! mean square turbulence level
      REAL(4) :: Sears       ! Sears function 
      REAL(4) :: LFC       ! low-frequency correction factor
      REAL(4) :: THETA     ! Chordwise directivity angle
      REAL(4) :: PHI       ! Spanwise directivity angle
      REAL(4) :: DBARL     ! Low-frequency directivity correction factor
      REAL(4) :: DBARH     ! High-frequency directivity correction factor
      REAL(4) :: Frequency_cutoff     ! Cutoff frequency between high & low directivity 
      REAL(4) :: Directivity     ! Directivity correction factor
      
      REAL(4) :: SPLti (NFrequency)       ! total predicted turbulence inflow noise
      REAL(4) :: FrequencyCenter (NFrequency)       ! total predicted turbulence inflow noise
      
      Mach = U/C0
      MeanVNoise = U
	TINoise=TINoise/100
      IF (TINoise > 0) THEN
          Ums = (TINoise*MeanVNoise)**2
      ELSE
          SPLti = 0.
          RETURN
      ENDIF
      
      !IF (RefHH < 30) THEN
      !    LTurb = 3.5*0.7*RefHH   ! Prediction sensitive to this parameter!
      !ELSE
      !    LTurb = 3.2e-2  !3.5*21
      !ENDIF
      
      ! Calculate directivity...?
      CALL DIRECTL(Mach,THETA,PHI,DBARL)
      CALL DIRECTH(Mach,THETA,PHI,DBARH)
      Frequency_cutoff = 10*U/PI/Chord
      
      Ke = 3.0/(4.0*LTurb) 
      PI = 3.141592654
      Beta2 = 1-Mach*Mach
      ALPSTAR = ALPSTAR*PI/180.

      DO I=1,NFrequency
         IF (FrequencyCenter(I) <= Frequency_cutoff) THEN
            Directivity = DBARL
         ELSE
            Directivity = DBARH 
         ENDIF
         WaveNumber = 2.0*PI*FrequencyCenter(I)/U
!	   WaveNumber = PI*FrequencyCenter(I)*Chord/U
         Kbar = WaveNumber*Chord/2.0
         Khat = WaveNumber/Ke
	   mu = Mach*WaveNumber*Chord/2.0/Beta2
         
         SPLhigh = 10.*ALOG10(AirDens*AirDens*C0**4*LTurb*(d/2.)/
     1             (RObs*RObs)*(Mach**5)*TINoise*TINoise*(Khat**3)*
     2             (1+Khat**2)**(-7./3.)*Directivity) + 78.4
!	   SPLhigh = 10.*ALOG10(LTurb*(d/2.)/
!     1             (RObs*RObs)*(Mach**5)*TINoise*TINoise*(WaveNumber**3)
!     2             *(1+WaveNumber**2)**(-7./3.)*Directivity) + 181.3
         SPLhigh = SPLhigh + 10.*ALOG10(1+ 9.0*ALPSTAR*ALPSTAR)
         Sears = 1/(2.*PI*Kbar/Beta2+1/(1+2.4*Kbar/Beta2))
!	   Sears = 1/(2.*PI*WaveNumber/Beta2+1/(1+2.4*WaveNumber/Beta2))
         LFC = 10*Sears*Mach*Kbar*Kbar/Beta2
!	   LFC = 10*Sears*Mach*WaveNumber*WaveNumber/Beta2
!         IF (mu<(PI/4.0)) THEN
!	      SPLti(I) = SPLhigh + 10.*ALOG10(LFC)
!	   ELSE
!	      SPLti(I) = SPLhigh
!	   ENDIF
	   SPLti(I) = SPLhigh + 10.*ALOG10(LFC/(1+LFC))
      ENDDO
      ALPSTAR = ALPSTAR*180./PI
      RETURN
      END SUBROUTINE InflowNoise
           
      !=======================================================================
      SUBROUTINE XTHICK_CALC (DELTAP,DSTRS,DSTRP)
      ! Assigns xfoil results to bl parameters  
      
      USE BLParams
      
      IMPLICIT NONE
      
      REAL(4) :: DELTAP,DSTRS,DSTRP
      
      DELTAP = d99(2)
      DSTRS = d_star(1)
      DSTRP = d_star(2)      
      
      RETURN
      
      END SUBROUTINE XTHICK_CALC      
      
      !=======================================================================
      SUBROUTINE TNO_TBLTE(SPLP,SPLS,SPLALPH,SPLTBL,THETA,PHI,D,R)  
      
      USE PRECISION
      USE Atmosphere
      USE TNOConstants
      USE AirfoilParams
      USE BLParams
      USE Third_Octave_Bands

      implicit none

      ! Variables
      REAL (ReKi):: bound,a,b
      REAL (ReKi):: epsabs,epsrel
      REAL (ReKi):: answer
      REAL (ReKi):: abserr,resabs,resasc
      REAL (ReKi):: alist (limit),blist (limit),rlist (limit)
      REAL (ReKi):: elist (limit)
      
      REAL (ReKi):: D
      REAL (ReKi):: R
      REAL (ReKi):: freq(NumBands)
      REAL (ReKi):: SPLP(NumBands),SPLS(NumBands)
      REAL (ReKi):: SPLALPH(NumBands),SPLTBL(NumBands)
      REAL (ReKi):: SPL_press,SPL_suction
      REAL (ReKi):: band_width,band_ratio
      REAL (ReKi):: Spectrum
      REAL (ReKi):: THETA,PHI,DBARH
      REAL (ReKi):: P1,P2,P4
    
      INTEGER (4) :: neval
      INTEGER (4) :: ier
      INTEGER (4) :: inf
      INTEGER (4) :: iord (limit)
      INTEGER (4) :: last
      INTEGER (4) :: n_freq,i_low,i_hi
     
      REAL (ReKi), EXTERNAL :: int2
    
      ! Body of TNO 
      
      bound  = 0.0     !lower bound of integration
      inf    = 2       !-infinity to +infinity
      epsabs = 1e-10     !absolute accuracy
      epsrel = 1e-10     !relative accuracy
    
      n_freq = NumBands
      freq = Third_Octave
      band_ratio = 2.**(1./3.)
    
      allocate(omega(n_freq))
      
C      COMPUTE DIRECTIVITY FUNCTION
C      ----------------------------

      CALL DIRECTH(Mach,THETA,PHI,DBARH)    

    
      do i_omega = 1,n_freq
    
         omega(i_omega) = 2.*pi*freq(i_omega)
         
         !integration limits
         a = 0.
         b = 10*omega(i_omega)/(Mach*co)
         ! Convert to third octave
         band_width = freq(i_omega)*(sqrt(band_ratio)
     1                -1./sqrt(band_ratio))
         
         ISSUCTION = .TRUE.
         
         IF (Cf(1) .LT. 0.) THEN
            write(*,*) 'Suction Cf is less than zero, Cf = ',Cf(1)
            write(*,*) 'Using BPM'
         ELSE
            CALL qk61(int2,a,b,answer,abserr,resabs,resasc)
                 
            Spectrum = D/(4.*pi*R**2.)*answer
            SPL_suction = 10*log10(Spectrum*DBARH/2.e-5/2.e-5)
            SPLS(i_omega) = SPL_suction + 10*log10(band_width)
         ENDIF
         
         ISSUCTION = .FALSE.
         
         IF (Cf(2) .LT. 0.) THEN
            write(*,*) 'Pressure Cf is less than zero, Cf = ',Cf(1)
            write(*,*) 'Using BPM'
         ELSE
            CALL qk61(int2,a,b,answer,abserr,resabs,resasc)
                 
            Spectrum = D/(4.*pi*R**2.)*answer
            SPL_press = 10*log10(Spectrum*DBARH/2.e-5/2.e-5)
            SPLP(i_omega) = SPL_press + 10*log10(band_width)
         ENDIF
         
         ! Sum the noise sources SPLALPH is BPM value
         IF (SPLP(i_omega)    .LT. -100.) SPLP(i_omega)    = -100.
         IF (SPLS(i_omega)    .LT. -100.) SPLS(i_omega)    = -100.

         P1  = 10.**(SPLP(i_omega) / 10.)
         P2  = 10.**(SPLS(i_omega) / 10.)
         P4  = 10.**(SPLALPH(i_omega) / 10.)

         SPLTBL(i_omega) = 10. * ALOG10(P1 + P2 + P4)
               
      enddo
      
      
      RETURN
      
      END SUBROUTINE TNO_TBLTE  

C========================================================================      
      SUBROUTINE Simple_TI (DSPL_TI,thick_1p,thick_10p,chord,U)

C     Uses simple correction for turbulent inflow noise from Moriarty et. al 2005
      
      USE Precision
      USE Third_Octave_Bands
      USE TNOConstants

      implicit none
      
      REAL (ReKi):: DSPL_TI(NumBands),thick_1p,thick_10p,chord,U
      REAL (ReKi):: slope,TI_param
      INTEGER (4):: I
      
      TI_Param = thick_1p + thick_10p
      slope = 1.123*TI_Param + 5.317*TI_Param*TI_Param
      
      do I =1,NumBands
         DSPL_TI(I) = -slope*(2*pi*Third_Octave(I)*chord/U + 5)
      enddo
      
      
      RETURN
      END SUBROUTINE Simple_TI           
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                              