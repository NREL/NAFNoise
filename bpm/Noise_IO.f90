SUBROUTINE Begin


   ! This subroutine Prints out the name and version of the program, checks for
   !  command-line arguments, and creates the names of the output files.


USE                             Noise_General
USE                             SysSubs
USE                             NWTC_Subs

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: Arg_Num                                         ! number of command line arguements
INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: LastChar                                        ! The location of the last non-blank character in the primary file name.

CHARACTER(1024)              :: DirName                                         ! A CHARACTER string containing the path of the current working directory.

LOGICAL                      :: Error

   ! Check for command line arguments.  Maybe the user specified an input file name.

CALL Get_Arg_Num (Arg_Num)

IF (Arg_Num .GT. 0) Then
   CALL Get_Arg ( 1 , PriFile , Error )
Endif


   ! Let's parse the root file name from the name of the primary input file.
   ! We'll count everything after the last period as the extension.

LastChar = LEN_TRIM( PriFile )

DO I=LastChar,1,-1
   IF ( PriFile(I:I) == '.' )  THEN
      RootName = PriFile(:I-1)
   ENDIF
ENDDO ! I

IF ( LEN_TRIM( RootName ) == 0 )  RootName = PriFile

   ! Let's create the name of the output file.

OutFile = TRIM( RootName )//'.out'


   ! Disply program and version
  
CALL DispNVD 

RETURN
END SUBROUTINE Begin

!=======================================================================
SUBROUTINE Read_Noise_Input

   USE AirfoilParams
   USE Atmosphere
   USE NWTC_Subs
   USE Noise_Inputs
   USE Noise_General
   
   implicit none
   
   CALL OpenFInpFile(4,PriFile)
   CALL READCom (4,PriFile,"")
   CALL ReadCom (4,PriFile,"") 
   CALL ReadCom (4,PriFile,"")    
   CALL READRVar (4,PriFile, co, "co", "Speed of Sound")      
   CALL READRVar (4,PriFile, nu, "", "")  
   CALL READRVar (4,PriFile, rho, "", "") 
   CALL ReadCom (4,PriFile,"") 
   CALL READIVar (4,PriFile, ITRIP, "", "")
   CALL READIVar (4,PriFile, X_BLMethod, "", "") 
   CALL READIVar (4,PriFile, TBL_Method, "", "") 
   CALL READIVar (4,PriFile, TI_Method, "", "")                
   CALL READIVar (4,PriFile, IBLUNT , "", "")         
   CALL READIVar (4,PriFile, ILAM, "", "")                                  
   CALL ReadCom (4,PriFile,"")
   CALL READRVar (4,PriFile, a_chord, "", "")
   CALL READRVar (4,PriFile, L, "", "")
   CALL READRVar (4,PriFile, U, "", "")
   CALL READRVar (4,PriFile, aofa, "", "")
   CALL READRVar (4,PriFile, H, "", "")
   CALL READRVar (4,PriFile, PSI  , "", "")
   CALL ReadCom (4,PriFile,"")     
   CALL READRVar (4,PriFile, xtrup, "", "")
   CALL READRVar (4,PriFile, xtrlo, "", "")
   CALL READLVar (4,PriFile, ISNACA, "","")
   CALL ReadCVar (4, PriFile, airfoil, "airfoil", "Airfoil Name") 
   CALL ReadCom (4,PriFile,"")
   CALL READRVar (4,PriFile, TINoise, "", "")
   CALL READRVar (4,PriFile, TurbLength, "", "") 
   CALL READRVar (4,PriFile, thick_1p, "", "")
   CALL READRVar (4,PriFile, thick_10p , "", "") 
   CALL READIVar (4,PriFile, npath, "", "")  
   CALL READRVar (4,PriFile, dpath, "", "")      
   CALL ReadCom (4,PriFile,"") 
   CALL READRVar (4,PriFile, R, "", "")
   CALL READRVar (4,PriFile, PHI, "", "")
   CALL READRVar (4,PriFile, THETA, "", "")

   Close (4)
   
RETURN
END SUBROUTINE Read_Noise_Input

!========================================================================
       
SUBROUTINE Write_Out_Header
   
      USE ProgMod
      USE SysSubs
      USE NWTC_Subs
      USE Noise_General
      USE Noise_Inputs

      CALL OpenFOutFile(5,OutFile)  
      
      WRITE(5,*)'Output file of '//TRIM( ProgName )//' v'//Trim( ProgVer )// &
                 ' for ', TRIM( airfoil )
      WRITE(5,*) 'Executed ',CurDate(), ' at ',CurTime()
      WRITE(5,*) '====================================================='
      WRITE(5,*)
      
      RETURN
END SUBROUTINE Write_Out_Header