MODULE Noise_Inputs

      USE AirfoilParams
      USE Atmosphere
      USE BLParams
      USE Third_Octave_Bands
      USE TINoiseInput
      USE TI_Guidati

      REAL    U, H, PHI, PSI, R, THETA, Thick_1p, Thick_10p
      REAL    TINoise, TurbLength, L
      INTEGER ITRIP,IBLUNT,ILAM,X_BLMethod, TBL_Method, TI_Method     
   
END MODULE Noise_Inputs

MODULE Noise_General


CHARACTER(99)                :: OutFile                                         ! The name of the output file.
CHARACTER(99)                :: PriFile   = 'nafnoise.ipt'                       ! The name of the primary input file.  Can be overwritten on command line.
CHARACTER(99)                :: RootName                                        ! The root name of the input and output files.

END MODULE Noise_General