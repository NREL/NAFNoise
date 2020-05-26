MODULE SysSubs


   ! This module contains routines with system-specific logic and references.
   ! It also contains standard (but not system-specific) routines it uses.


CONTAINS

!=======================================================================
   SUBROUTINE FileSize ( Unit, Size )


   !  This routine calls the routine FSTAT to obtain the file size
   !  corresponding to a file unit number or returns -1 on error.


   USE             DFPORT

   IMPLICIT        NONE

   INTEGER         Size
   INTEGER         StatArray(12)
   INTEGER         Status
   INTEGER         Unit



   Status = FSTAT( Unit , StatArray )

   IF ( Status /= 0 ) THEN
     Size = -1
   ELSE
     Size = StatArray(8)
   ENDIF


   RETURN
   END SUBROUTINE FileSize ! ( Unit, Size )
!=======================================================================
   SUBROUTINE FindLine ( Str , MaxLen , StrEnd )


      ! This routine finds one line of text with a maximum length of
      ! MaxLen from the Str.  It tries to break the line at a blank.


   USE                             NWTC_Gen

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: MaxLen                                       ! The maximum length of the string.
   INTEGER, INTENT(OUT)         :: StrEnd                                       ! The location of the end of the string.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to search.


      ! Local declarations:

   INTEGER         IC



   StrEnd = MaxLen

   IF ( LEN_TRIM( Str ) > MaxLen )  THEN

      DO IC=MaxLen,1,-1

         IF ( Str(IC:IC) == ' ' )  THEN
            StrEnd = IC-1
            DO WHILE ( Str(StrEnd:StrEnd) == ' ' )
               StrEnd = StrEnd - 1
            ENDDO
            EXIT
         ENDIF

      ENDDO ! IC

   ENDIF


   RETURN
   END SUBROUTINE FindLine ! ( Str , MaxLen , StrEnd )
!=======================================================================
   SUBROUTINE FlushOut ( Unit )


      ! This subroutine flushes the buffer on the specified Unit.
      ! It is especially useful when printing "running..." type
      ! messages.


   USE                             DFPORT
   !USE                             IFLPORT
   !USE                             IFPORT

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: Unit                                         ! The maximum length of the string.



   CALL FLUSH ( Unit )


   RETURN
   END SUBROUTINE FlushOut ! ( Unit )
!=======================================================================
   SUBROUTINE Get_Arg ( Arg_Num , Arg , Error )


      ! This routine gets Arg_Num'th argument from the command line.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Arg_Num                                      ! The argument number to get.

   LOGICAL, INTENT(OUT)         :: Error                                        ! The Error flag returned to the calling program.

   CHARACTER(*), INTENT(OUT)    :: Arg                                          ! The argument string returned to the calling program.


      ! Local declarations.

   INTEGER                      :: Status                                       ! The status of the attempt to get an argument.



   CALL GETARG ( Arg_Num, Arg, Status )

   IF ( LEN_TRIM( Arg ) > 0 )  THEN
      Error = .FALSE.
   ELSE
      Error = .TRUE.
   ENDIF


   RETURN
   END SUBROUTINE Get_Arg ! ( Arg_Num , Arg , Error )
!=======================================================================
   SUBROUTINE Get_Arg_Num ( Arg_Num )


      ! This routine gets the number of command line arguments.


   USE                             DFPORT

   IMPLICIT                      NONE


      ! Argument declarations.

   INTEGER, INTENT(OUT)         :: Arg_Num                                      ! The argument to get from the command line.



   Arg_Num = IARGC()


   RETURN
   END SUBROUTINE Get_Arg_Num ! ( Arg_Num )
!=======================================================================
   FUNCTION Is_NaN ( DblNum )


   !  This routine determines if a REAL(8) variable holds a proper number.


   IMPLICIT        NONE

   REAL(8)         DblNum

   LOGICAL         Is_Nan



   Is_NaN = IsNaN( DblNum )


   RETURN
   END FUNCTION Is_NaN ! ( DblNum )
!=======================================================================
   SUBROUTINE OpenBin ( Un, OutFile, RecLen )


      ! This routine opens a binary output file.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the output file.
   INTEGER, INTENT(IN)          :: RecLen                                       ! Length of binary record

   CHARACTER(*), INTENT(IN)     :: OutFile                                      ! Name of the output file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.



      ! Open output file.  Make sure it worked.

   OPEN( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM='BINARY' , ACCESS='SEQUENTIAL', RECL=RecLen , IOSTAT=IOS )

   IF ( IOS /= 0 )  CALL Abort ( ' Cannot open file "'//TRIM( OutFile ) &
                               //'".  Another program may have locked it for writing.' )


   RETURN
   END SUBROUTINE OpenBin ! ( Un, OutFile, RecLen )
!=======================================================================
   SUBROUTINE OpenCon


      ! This routine opens the console for standard output.


   USE                             SysMod

   IMPLICIT                        NONE



   OPEN ( CU , FILE='CON' , STATUS='UNKNOWN' , CARRIAGECONTROL='FORTRAN' )

   CALL FlushOut ( CU )


   RETURN
   END SUBROUTINE OpenCon
!=======================================================================
   SUBROUTINE OpenUnf ( Un, OutFile )


      !  This routine opens an unformatted file.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)         ::  Un                                           ! Logical unit for the output file

   CHARACTER(*), INTENT(IN)    ::  OutFile                                      ! Name of the output file


      ! Local declarations.

   INTEGER                     ::  IOS                                          ! Returned input/output status.



   OPEN ( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM='UNFORMATTED', ACCESS='SEQUENTIAL', IOSTAT=IOS )

   IF ( IOS /= 0 )  CALL Abort( ' Cannot open file "'//TRIM( OutFile ) &
                              //'".  Another program may have locked it for writing.' )

   RETURN
   END SUBROUTINE OpenUnf ! ( Un, OutFile )
!=======================================================================
   SUBROUTINE ProgExit ( StatCode )


      ! This routine stops the program.  If the compiler supports the EXIT routine,
      ! pass the program status to it.  Otherwise, do a STOP.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: StatCode                                      ! The status code to pass to the OS.



   CALL EXIT ( StatCode )


   RETURN
   END SUBROUTINE ProgExit ! ( StatCode )
!=======================================================================
   SUBROUTINE UsrAlarm


      ! This routine generates an alarm to warn the user that something went wrong.


   IMPLICIT                        NONE



   CALL WrOver ( CHAR( 7 ) )


   RETURN
   END SUBROUTINE UsrAlarm
!=======================================================================
   FUNCTION UserTime()


      ! This function returns the user CPU time.


   USE                             DFPORT                                          ! Use the portability library.

   IMPLICIT                        NONE


      ! Passed variables.

   REAL(4)                      :: UserTime                                        ! User CPU time.


      ! Local variables.

   REAL(4)                      :: TimeAry (2)                                     ! TimeAry(1): User CPU time, TimeAry(2): System CPU time.
   REAL(4)                      :: TotTime                                         ! User CPU time plus system CPU time.




   TotTime  = DTIME( TimeAry )
   UserTime = TimeAry(1)


   RETURN
   END FUNCTION UserTime
!=======================================================================
   SUBROUTINE WrFileNR ( Unit, Str )


      ! This routine writes out a string to the file connected to Unit without following it with a new line.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Unit                                         ! I/O unit for input file.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! String to be written without a newline at the end.



   WRITE (Unit,'(A,$)')  Str


   RETURN
   END SUBROUTINE WrFileNR ! ( Unit, Str )
!=======================================================================
   SUBROUTINE WrML ( Str )


      ! This routine writes out a string in the middle of a line.


   USE                             SysMod

   IMPLICIT                        NONE

   CHARACTER(*)                 :: Str



   CALL WrNR ( Str )


   RETURN
   END SUBROUTINE WrML ! ( Str )
!=======================================================================
   SUBROUTINE WrNR ( Str )


      ! This routine writes out a string to the screen without following it with a new line.


   USE                             SysMod

   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.



   WRITE (CU,'(1X,A,$)')  Str


   RETURN
   END SUBROUTINE WrNR ! ( Str )
!=======================================================================
   SUBROUTINE WrOver ( Str )


      ! This routine writes out a string that overwrites the previous line


   USE                             SysMod

   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.



   WRITE (CU,'(''+'',A)')  Str


   RETURN
   END SUBROUTINE WrOver ! ( Str )
!=======================================================================
   SUBROUTINE WrScr ( Str )


      ! This routine writes out a string to the screen.


   USE                             SysMod

   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.


      ! Local declarations.

   INTEGER                      :: Beg                                          ! The beginning of the next line of text.
   INTEGER                      :: Indent                                       ! The amunt to be indented.
   INTEGER                      :: LStr                                         ! The length of the remaining portion of the string.
   INTEGER                      :: MaxLen                                       ! Maximum number of columns to be written to the screen.

   CHARACTER(10)                :: Frm                                          ! Format specifier for the output.



   !  Find the amount of indent.  Create format.

  MaxLen = 98
  Indent = LEN_TRIM( Str ) - LEN_TRIM( ADJUSTL( Str ) )
  MaxLen = MaxLen - Indent
  Frm    = '(1X,  X,A)'

  WRITE (Frm(5:6),'(I2)')  Indent


   !  Break long messages into multiple lines.

   Beg  = Indent + 1
   LStr = LEN_TRIM( Str(Beg:) )

   DO WHILE ( Lstr > MaxLen )

       CALL FindLine ( Str(Beg:) , MaxLen , LStr )

       WRITE (CU,Frm)  TRIM( ADJUSTL( Str(Beg:Beg+LStr-1) ) )

       Beg  = Beg + LStr + 1
       LStr = LEN_TRIM( Str(Beg:) )

   ENDDO

   WRITE (CU,Frm)  TRIM( ADJUSTL( Str(Beg:Beg+LStr-1) ) )


   RETURN
   END SUBROUTINE WrScr ! ( Str )
!=======================================================================

END MODULE SysSubs
