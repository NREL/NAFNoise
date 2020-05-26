MODULE NWTC_Subs


   ! This module contains general-use routines with non-system-specific logic and references.


CONTAINS

!=======================================================================
   SUBROUTINE Abort (  Message )


      ! This routine outputs fatal Error messages and stops the program.


   USE                             ProgMod
   USE                             SysSubs

   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Message                                      ! Error message.



   CALL UsrAlarm
   CALL WrScr    ( Message )
!Start of proposed change.  v1.00a-mlb  02-Jul-2004
!v1.00a   CALL WrScr1   ( ' Aborting '//ProgName//'.' )
   CALL WrScr1   ( ' Aborting '//TRIM( ProgName )//'.' )
!End of proposed change.  v1.00a-mlb  02-Jul-2004
   CALL WrScr    ( ' ' )
   CALL ProgExit ( 1 )


   END SUBROUTINE Abort ! ( Message )
!=======================================================================
   SUBROUTINE CheckIOS ( IOS, Fil, Variable, VarType )


      ! This routine checks the I/O status and prints either an end-of-file or
      ! an invalid-input message, and then aborts the program.


   USE                             NWTC_Gen

   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: IOS                                          ! I/O status.
   INTEGER, INTENT(IN)          :: VarType                                      ! Type of variable.

   CHARACTER(*), INTENT(IN)     :: Fil                                          ! Name of input file.
   CHARACTER(*), INTENT(IN)     :: Variable                                     ! Variable name.



   IF ( IOS < 0 )  THEN

      CALL PremEOF ( TRIM( Fil ), Variable )

   ELSEIF ( IOS > 0 )  THEN

      SELECTCASE ( VarType )

      CASE ( NumType )
         CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( Fil )//'".' )
      CASE ( FlgType )
         CALL WrScr1 ( ' Invalid logical input for file "'//TRIM( Fil )//'".' )
      CASE ( StrType )
         CALL WrScr1 ( ' Invalid character input for file "'//TRIM( Fil )//'".' )
      ENDSELECT

      CALL Abort  ( ' The error occurred while trying to read '//TRIM( Variable )//'.' )

   ENDIF


   RETURN
   END SUBROUTINE CheckIOS ! ( IOS, Fil, Variable, VarType )
!=======================================================================
   SUBROUTINE CompDR ( NumSeg, RLoc, HubRad, RotorRad, DimenInp, DelRLoc )


      ! This routine computes the segment lengths from the local radii and the rotor radius.
      ! It prints and error if the list of radii is not realizable.


   USE                             Precision
!Start of proposed change.  v1.00a-mlb  02-Jul-2004
   USE                             SysSubs
!End of proposed change.  v1.00a-mlb  02-Jul-2004

   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: NumSeg                                       ! Number of blade segments.

   REAL(ReKi), INTENT(OUT)      :: DelRLoc (NumSeg)                             ! The array of segment lengths.
   REAL(ReKi), INTENT(IN)       :: HubRad                                       ! The hub radius.
   REAL(ReKi), INTENT(IN)       :: RLoc    (NumSeg)                             ! The array of radii (segment centers).
   REAL(ReKi), INTENT(IN)       :: RotorRad                                     ! The rotor radius.

   LOGICAL, INTENT(IN)          :: DimenInp                                     ! Flag that tells if input is dimensional or not.


      ! Local declarations.

   REAL(ReKi)                   :: CompRad                                      ! The computed radius of the rotor.
   REAL(ReKi)                   :: ErrFact                                      ! The conversion to non-dimensional form if needed.
   REAL(ReKi)                   :: SegBeg                                       ! The beginning of the current segment.

   INTEGER                      :: ISeg                                         ! Segment index



      ! Determine the correct units for error messages.

   IF ( DimenInp )  THEN
      ErrFact = 1.0
   ELSE
      ErrFact = RotorRad
   ENDIF


      ! We will work our way from the root to the tip.

   SegBeg = HubRad

   DO ISeg=1,NumSeg

      IF ( RLoc(ISeg) <= SegBeg )  THEN
         CALL Abort ( ' The radius for blade segment #'//Trim( Int2LStr( NumSeg ) )//' is too far inboard for a physically' &
                    //' realizable blade.  It must be greater than '//Trim( Flt2LStr( SegBeg/ErrFact ) )//'.' )
      ENDIF

      DelRLoc(ISeg) = 2.0*( RLoc(ISeg) - SegBeg )
      SegBeg        = SegBeg + DelRLoc(ISeg)

   ENDDO ! ISeg


      ! Ensure that the segments (almost) exactly fill the blade.

   CompRad = RLoc(NumSeg) + 0.5*DelRLoc(NumSeg)

!Start of proposed change.  v1.00a-mlb  02-Jul-2004
!v1.00a   IF ( ABS( CompRad - RotorRad )/RotorRad > 0.001 )  THEN
!v1.00a         CALL Abort ( ' The sum of the lengths of the blade segments does not match the rotor radius.  The segments add up to a' &
!v1.00a                    //' rotor radius of '//Trim( Flt2LStr( CompRad ) )//' instead of the specified radius of ' &
!v1.00a                    //Trim( Flt2LStr( RotorRad ) )//'.  They must agree within 0.1%' )
!v1.00a   ENDIF
   IF ( ABS( CompRad - RotorRad )/RotorRad > 0.005 )  THEN
         CALL Abort ( ' The sum of the lengths of the blade segments does not match the rotor radius.  The segments add up to a' &
                    //' rotor radius of '//Trim( Flt2LStr( CompRad ) )//' instead of the specified radius of ' &
                    //Trim( Flt2LStr( RotorRad ) )//'.  They must agree within 0.5%' )
   ELSEIF ( ABS( CompRad - RotorRad )/RotorRad > 0.001 )  THEN
         CALL WrScr1 ( ' The sum of the lengths of the blade segments does not match the rotor radius.  The segments add up to a' &
                    //' rotor radius of '//Trim( Flt2LStr( CompRad ) )//' instead of the specified radius of ' &
                    //Trim( Flt2LStr( RotorRad ) )//'.  They really should agree within 0.1%, but I''ll let you slide.' )
         CALL UsrAlarm
   ENDIF
!End of proposed change.  v1.00a-mlb  02-Jul-2004


   RETURN
   END SUBROUTINE CompDR ! ( NumSeg, RLoc, RotorRad, DimenInp, DelRLoc )
!=======================================================================
   SUBROUTINE Conv2UC ( Str )


      ! This routine converts all the text in a string to upper case.


   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(INOUT)  :: Str                                          ! The string to be converted to UC.


      ! Local declarations.

   INTEGER                      :: IC                                           ! Character index



   DO IC=1,LEN_TRIM( Str )

      IF ( ( Str(IC:IC) >= 'a' ).AND.( Str(IC:IC) <= 'z' ) )  THEN
         Str(IC:IC) = CHAR( ICHAR( Str(IC:IC) ) - 32 )
      ELSE
         Str(IC:IC) = Str(IC:IC)
      ENDIF

   ENDDO ! IC


   RETURN
   END SUBROUTINE Conv2UC !  ( Str )
!=======================================================================
   FUNCTION CountWords ( Line )


      ! This subroutine is used to count the number of "words" in a line of text.


   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Function declaration.

   INTEGER                      :: CountWords                                   ! This function.


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Line                                         ! Count the words in this text string.


      ! Local declarations.

   INTEGER                      :: Ch                                           ! Character position.
   INTEGER                      :: NextWhite                                    ! Position of the next white space.



      ! Let's initialize the number of columns and the character pointer.

   CountWords = 0


      ! Let's make sure we have text on this line.

   IF ( LEN_TRIM( Line ) == 0 )  RETURN


      ! Count words separated by any combination of spaces, tabs, commas,
      ! semicolons, single quotes, and double quotes ("whitespace").

   Ch = 0

   DO

      NextWhite = SCAN( Line(Ch+1:) , ' ,;''"'//Tab )
      Ch        = Ch + NextWhite

      IF ( NextWhite > 1 )  THEN
         CountWords = CountWords + 1
      ELSEIF ( NextWhite == 1 )  THEN
         CYCLE
      ELSE
         EXIT
      ENDIF

   ENDDO


   RETURN
   END FUNCTION CountWords ! ( Line )
!=======================================================================
   FUNCTION CurDate( )


      ! This function returns a character string encoded with the date in the form dd-mmm-ccyy.


   IMPLICIT                        NONE


      ! Function declaration.

   CHARACTER(11)                :: CurDate                                      ! This function


      ! Local declarations.

   CHARACTER(8)                 :: CDate                                        ! String to hold the returned value from the DATE_AND_TIME subroutine call.



   !  Call the system date function.

   CALL DATE_AND_TIME ( CDate )


   !  Parse out the day.

   CurDate(1:3) = CDate(7:8)//'-'


   !  Parse out the month.

   SELECT CASE ( CDate(5:6) )
      CASE ( '01' )
         CurDate(4:6) = 'Jan'
      CASE ( '02' )
         CurDate(4:6) = 'Feb'
      CASE ( '03' )
         CurDate(4:6) = 'Mar'
      CASE ( '04' )
         CurDate(4:6) = 'Apr'
      CASE ( '05' )
         CurDate(4:6) = 'May'
      CASE ( '06' )
         CurDate(4:6) = 'Jun'
      CASE ( '07' )
         CurDate(4:6) = 'Jul'
      CASE ( '08' )
         CurDate(4:6) = 'Aug'
      CASE ( '09' )
         CurDate(4:6) = 'Sep'
      CASE ( '10' )
         CurDate(4:6) = 'Oct'
      CASE ( '11' )
         CurDate(4:6) = 'Nov'
      CASE ( '12' )
         CurDate(4:6) = 'Dec'
   END SELECT


   !  Parse out the year.

   CurDate(7:11) = '-'//CDate(1:4)


   RETURN
   END FUNCTION CurDate ! ()
!=======================================================================
   FUNCTION CurTime( )


      ! This function returns a character string encoded with the time in the form "hh:mm:ss".


   IMPLICIT                        NONE


      ! Function declaration.

   CHARACTER(8)                 :: CurTime                                      ! This function.


      ! Local declarations.

   CHARACTER(10)                :: CTime                                        ! String to hold the returned value from the DATE_AND_TIME subroutine call.



   CALL DATE_AND_TIME ( TIME=CTime )

   CurTime = CTime(1:2)//':'//CTime(3:4)//':'//CTime(5:6)


   RETURN
   END FUNCTION CurTime ! ()
!=======================================================================
   SUBROUTINE DispNVD


      ! This routine displays the name of the program, it's version, and it's release date.


   USE                             ProgMod
   USE                             SysSubs

   IMPLICIT                        NONE



      ! Print out program name, version, and date.

   CALL WrScr1 ( ' Running '//TRIM( ProgName )//Trim( ProgVer )//'.' )
!Start of proposed change.  v1.00a-mlb  02-Jul-2004
   CALL WrScr  ( ' Linked with the NWTC Subroutine Library '//Trim( NWTCVer )//'.' )
!End of proposed change.  v1.00a-mlb  02-Jul-2004


   RETURN
   END SUBROUTINE DispNVD
!=======================================================================
   FUNCTION Flt2LStr ( FltNum )


      ! This function converts a floating point number to a left-aligned
      ! string.  It eliminates trailing zeroes and even the decimal
      ! point if it is not a fraction.


   USE                             Precision

   IMPLICIT                        NONE


      ! Function declaration.

   CHARACTER(15)                :: Flt2LStr                                        ! This function.


      ! Argument declarations.

   REAL(ReKi), INTENT(IN)       :: FltNum                                          ! The floating-point number to convert.


      ! Local declarations.

   INTEGER                      :: IC                                              ! Character index.



      ! Return a 0 if that's what we have.

   IF ( FltNum == 0.0 )  THEN
      Flt2LStr = '0'
      RETURN
   ENDIF


      ! Write the number into the string using G format and left justify it.

   WRITE (Flt2LStr,'(1PG15.5)')  FltNum

   Flt2LStr = ADJUSTL( Flt2LStr )


      ! Replace trailing zeros and possibly the decimal point with blanks.
      ! Stop trimming once we find the decimal point or a nonzero.

   DO IC=LEN_TRIM( Flt2LStr ),1,-1

      IF ( Flt2LStr(IC:IC) == '.' )  THEN
         Flt2LStr(IC:IC) = ' '
         RETURN
      ELSEIF ( Flt2LStr(IC:IC) /= '0' )  THEN
         RETURN
      ENDIF

      Flt2LStr(IC:IC) = ' '

   ENDDO ! IC


   RETURN
   END FUNCTION Flt2LStr !  ( FltNum )
!=======================================================================
   SUBROUTINE GetAF ( AF_File, AF_Table, ISeg )


      ! Routine to get airfoil data from either a new NWTC-style or an old AeroDyn-style airfoil file.



   USE                             NWTC_Aero
   USE                             NWTC_Gen
   USE                             NWTC_Output
   USE                             Precision
   USE                             SysMod !temporary

   IMPLICIT                        NONE


      ! Argument declarations.

!   TYPE (ElmTable), INTENT(INOUT)  :: AF_Table                                  ! The table of airfoil data for the current segment.
   TYPE (ElmTable), INTENT(OUT)    :: AF_Table                                  ! The table of airfoil data for the current segment.

   INTEGER, INTENT(IN)             :: ISeg                                      ! The segment number.

   CHARACTER(*), INTENT(IN)        :: AF_File                                   ! Name of file containing AeroDyn-style airfoil data.


      ! Local declarations.

      ! Because of what seems to be a compiler bug, we cannot dynamically allocate the data arrays for the new-style
      ! airfoil files.  We really need to do it for the old-style files because there is no limit on the number of points.

!   TYPE                            :: DataRowO                                  ! Declare new type that is an allocatable table of data using a linked list.
!      REAL(ReKi), ALLOCATABLE      :: Data      (:)
!      TYPE(DataRowO), POINTER      :: Next            => NULL()
!   ENDTYPE DataRowO

   REAL(ReKi)                      :: AF_Data   (4)                             ! The values from one line of airfol data.
   REAL(ReKi), ALLOCATABLE         :: AF_DataO  (:)                             ! The values from one line of airfol data.
   REAL(ReKi)                      :: AlfaStal                                  ! The stall AoA for all tables in old airfoil files.
   REAL(ReKi), ALLOCATABLE         :: RnAry     (:)                             ! The Re values for the current element.

   INTEGER                         :: IAlf                                      ! A generic array index for angle of attack.
   INTEGER                         :: Ind                                       ! A generic array index.
   INTEGER                         :: IOS                                       ! The status of an I/O operation.
   INTEGER                         :: ITab                                      ! The table index.
   INTEGER                         :: NumAlf                                    ! The number of lines in an old-style airfoil table.
   INTEGER                         :: NumAlpha                                  ! The number of non--blank lines in an old-style airfoil table.
   INTEGER                         :: NumCoef                                   ! The number of coefficiants in an airfoil table.
   INTEGER                         :: NumVals                                   ! The total number of values on one line of airfoil data.
   INTEGER                         :: Sttus                                     ! The status returned from the allocation.
   INTEGER                         :: UnAF     = 20                             ! I/O unit number for the airfoil file.

   CHARACTER( 15)                  :: Frmt = "(1000(F11.4,:))"                  ! Output format for a line of airfoil data.
   CHARACTER(999)                  :: Line                                      ! A line of text.
   CHARACTER(  3)                  :: Line3                                     ! The first three characters of a line of text.



      ! Open the airfoil data file.

   CALL OpenFInpFile ( UnAF, AF_File )


      ! Read the header block of the airfoil file.  Look to see if this is a new-format file.

   READ (UnAF,'(A)',IOSTAT=IOS)  Line

   CALL CheckIOS ( IOS, AF_File, 'FirstHead', StrType )

   IF ( Echo )  THEN
      WRITE (UnEc,"(15X,A,T27,' - ',A,/,2X,A)")  'FirstHead', 'First line in the airfoil file.', TRIM( Line )
   ENDIF

   CALL Conv2UC  ( Line )

   IF ( Line(:21) == 'AERODYN AIRFOIL FILE.' )  THEN


         ! This is new style of AeroDyn file.

      CALL ReadCom  ( UnAF, AF_File, 'the first title' )
      CALL ReadCom  ( UnAF, AF_File, 'the second title' )
      CALL ReadIVar ( UnAF, AF_File, AF_Table%NumTabs, 'NumTabs', 'Number of airfoil tables for segment #' &
                                                                 //TRIM( Int2LStr( ISeg ) )//'.' )

      IF ( AF_Table%NumTabs < 1 )  CALL Abort ( ' Number of tables in airfoil file, "'//TRIM( AF_File ) &
                                              //'", must be > 0 for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )


         ! Are we expecting Cm data in the file?  Allocate the temporary data array.

      IF ( UseCm )  THEN
         NumVals = 4
      ELSE
         NumVals = 3
      ENDIF


         ! Allocate the AF_Table of pointers for this element.

      ALLOCATE ( AF_Table%Tab(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort ( ' Error allocating memory for the Tab subtable of the AF_Table of pointers for segment #' &
                    //TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      ENDIF


         ! Read the NumTabs airfoil tables.

      DO ITab=1,AF_Table%NumTabs


            ! Read in the Table ID (Re), convert from millions, and then read stall parameters for this table.

         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%Re      , 'AF_Table%Tab('//TRIM( Int2LStr( ITab ) ) &
                                                                   //')%Re      ',  'Reynolds number for this airfoil table.' )
         AF_Table%Tab(ITab)%Re = 1.0e6*AF_Table%Tab(ITab)%Re
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%AlfaStal, 'AF_Table%Tab('//TRIM( Int2LStr( ITab ) ) &
                                                                   //')%AlfaStal',  'Stall AoA for this airfoil table.' )
            ! These need to be changed for AeroDyn use.
         CALL ReadCom  ( UnAF, AF_File, 'the unused zero-lift angle of attack' )
         CALL ReadCom  ( UnAF, AF_File, 'the unused Cn slope for zero lift' )
         CALL ReadCom  ( UnAF, AF_File, 'the unused Cn at stall value for positive angle of attack' )
         CALL ReadCom  ( UnAF, AF_File, 'the unused Cn at stall value for negative angle of attack' )
         CALL ReadCom  ( UnAF, AF_File, 'the unused angle of attack for minimum Cd' )
         CALL ReadCom  ( UnAF, AF_File, 'the unused minimum Cd value' )


            ! Find the length of this table.

         AF_Table%Tab(ITab)%NumAlf = 0

         DO

            READ (UnAF,'(A)',IOSTAT=IOS)  Line3

            IF ( IOS < 0 )  THEN
               CALL PremEOF ( AF_File , 'the "EOT" end-of-table mark for airfoil table #'//TRIM( Int2LStr( ITab ) ) &
                                      //' and segment #'//TRIM( Int2LStr( ISeg ) ) )
            ELSEIF ( IOS > 0 )  THEN
               CALL WrScr1 ( ' Invalid character input for file "'//TRIM( AF_File )//'.' )
               CALL Abort  ( ' The error occurred while trying to read line #'//TRIM( Int2LStr( AF_Table%Tab(ITab)%NumAlf+1 ) ) &
                           //' of airfoil table #'//TRIM( Int2LStr( ITab ) )//' for segment #'//TRIM( Int2LStr( ISeg ) )//'.' )
            ENDIF

            CALL Conv2UC ( Line3 )
            IF ( Line3 == 'EOT' )  EXIT
            AF_Table%Tab(ITab)%NumAlf = AF_Table%Tab(ITab)%NumAlf + 1

         ENDDO


            ! Rewind the file to the beginning of this table.

         DO IAlf=1,AF_Table%Tab(ITab)%NumAlf+1
            BACKSPACE UnAF
         ENDDO ! IAlf


            ! Let's allocate the permanent table.

         ALLOCATE ( AF_Table%Tab(ITab)%Alpha(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL Abort ( ' Error allocating memory for the Alpha subtable for segment #'//TRIM( Int2LStr( ISeg) )//' and table #' &
                       //TRIM( Int2LStr( ITab) )//').' )
         ENDIF

         ALLOCATE ( AF_Table%Tab(ITab)%Cl(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL Abort ( ' Error allocating memory for the Cl subtable for segment #'//TRIM( Int2LStr( ISeg) )//' and table #' &
                       //TRIM( Int2LStr( ITab) )//').' )
         ENDIF

         ALLOCATE ( AF_Table%Tab(ITab)%Cd(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL Abort ( ' Error allocating memory for the Cd subtable for segment #'//TRIM( Int2LStr( ISeg) )//' and table #' &
                       //TRIM( Int2LStr( ITab) )//').' )
         ENDIF

         IF ( UseCm )  THEN
            ALLOCATE ( AF_Table%Tab(ITab)%Cm(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
            IF ( Sttus /= 0 )  THEN
               CALL Abort ( ' Error allocating memory for the Cm subtable for segment #'//TRIM( Int2LStr( ISeg) )//' and table #' &
                       //TRIM( Int2LStr( ITab) )//').' )
            ENDIF
         ENDIF


            ! Read in the airfoil data for this table.

         DO IAlf=1,AF_Table%Tab(ITab)%NumAlf

            READ (UnAF,*,IOSTAT=IOS)  ( AF_Data(Ind), Ind=1,NumVals )

            CALL CheckIOS ( IOS, AF_File, 'AF_Data', NumType )

            IF ( Echo )  WRITE (UnEc,Frmt)  ( AF_Data(Ind), Ind=1,NumVals )

            AF_Table%Tab(ITab)%Alpha(IAlf) = AF_Data(1)
            AF_Table%Tab(ITab)%Cl   (IAlf) = AF_Data(2)
            AF_Table%Tab(ITab)%Cd   (IAlf) = AF_Data(3)

            IF ( UseCm )  THEN
               AF_Table%Tab(ITab)%Cm(IAlf) = AF_Data(4)
            ENDIF

         ENDDO ! IAlf


            ! Skip this EOT mark.

         READ (UnAF,'()')

      ENDDO ! ITab

   ELSE


         ! This is old style of AeroDyn file.

      CALL ReadCom  ( UnAF, AF_File, 'the second title' )
      CALL ReadIVar ( UnAF, AF_File, AF_Table%NumTabs, 'NumTabs', 'Number of airfoil tables for segment #' &
                                                                //TRIM( Int2LStr( ISeg ) )//'.' )

      ALLOCATE ( RnAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort ( ' Error allocating memory for the RnAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      ENDIF

      CALL ReadRAry ( UnAF, AF_File, RnAry, AF_Table%NumTabs, 'RnAry',  'Reynolds number values for the airfoil tables.' )
      CALL ReadRVar ( UnAF, AF_File, AlfaStal, 'AlfaStal', 'Stall AoA for this airfoil table.' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused first obsolete stall parameter' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused second obsolete stall parameter' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused third obsolete stall parameter' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused zero-lift angle of attack' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused Cn slope for zero lift' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused Cn at stall value for positive angle of attack' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused Cn at stall value for negative angle of attack' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused angle of attack for minimum Cd' )
      CALL ReadCom  ( UnAF, AF_File, 'the unused minimum Cd value' )


         ! Are we expecting Cm data in the file?  Allocate the temporary data array.

      IF ( UseCm )  THEN
         NumCoef = 3
      ELSE
         NumCoef = 2
      ENDIF

      NumVals = 1 + NumCoef*AF_Table%NumTabs

      ALLOCATE ( AF_DataO(NumVals) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort ( ' Error allocating memory for the AF_DataO array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      ENDIF


         ! Allocate the AF_Table of pointers for this element.

      ALLOCATE ( AF_Table%Tab(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort ( ' Error allocating memory for the AF_Table of pointers in GetAF.' )
      ENDIF


            ! Find the length of this table.

      NumAlf = 0

      DO
         READ (UnAF,'()',IOSTAT=IOS)
         IF ( IOS < 0 )  EXIT
         NumAlf = NumAlf + 1
      ENDDO


         ! Rewind the file to the beginning of this table.

      DO IAlf=1,NumAlf+1
         BACKSPACE UnAF
      ENDDO ! IAlf


         ! Let's allocate the tables.

      DO ITab=1,AF_Table%NumTabs

         ALLOCATE ( AF_Table%Tab(ITab)%Alpha(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL Abort ( ' Error allocating memory for the Alpha vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                       //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
         ENDIF

         ALLOCATE ( AF_Table%Tab(ITab)%Cl(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL Abort ( ' Error allocating memory for the Cl vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                       //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
         ENDIF

         ALLOCATE ( AF_Table%Tab(ITab)%Cd(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL Abort ( ' Error allocating memory for the Cd vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                       //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
         ENDIF

         IF ( UseCm )  THEN
            ALLOCATE ( AF_Table%Tab(ITab)%Cm(NumAlf) , STAT=Sttus )
            IF ( Sttus /= 0 )  THEN
               CALL Abort ( ' Error allocating memory for the Cm vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                          //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
            ENDIF
         ENDIF

      ENDDO ! ITab


         ! Let's read the data this time through.

      NumAlpha = NumAlf

      DO IAlf=1,NumAlf


            ! Let's skip blank lines.  Decrement the number of alphas when we find them.

         READ (UnAF,'(A)')  Line

         IF ( LEN_TRIM( Line ) == 0 )  THEN
            NumAlpha = NumAlpha - 1
            CYCLE
         ENDIF


            ! Let's get the data from the non-blank line.

         READ (Line,*,IOSTAT=IOS)  ( AF_DataO(Ind), Ind=1,NumVals )

         CALL CheckIOS ( IOS, AF_File, 'AF_DataO', NumType )

         IF ( Echo )  THEN
            WRITE (UnEc,Frmt)  ( AF_DataO(Ind), Ind=1,NumVals )
         ENDIF


            ! Let's move this good data into permanent storage.

         DO ITab=1,AF_Table%NumTabs

            AF_Table%Tab(ITab)%Alpha(IAlf) = AF_DataO(1)
            AF_Table%Tab(ITab)%Cl   (IAlf) = AF_DataO(NumCoef*(ITab-1)+2)
            AF_Table%Tab(ITab)%Cd   (IAlf) = AF_DataO(NumCoef*(ITab-1)+3)

            IF ( UseCm )  THEN
               AF_Table%Tab(ITab)%Cm(IAlf) = AF_DataO(NumCoef*(ITab-1)+4)
            ENDIF
         ENDDO ! ITab

      ENDDO ! IAlf


           ! Store some of the header data in the permanent structure.

      DO ITab=1,AF_Table%NumTabs
         AF_Table%Tab(ITab)%AlfaStal = AlfaStal
         AF_Table%Tab(ITab)%Re       = 1.0e6*RnAry(ITab)
         AF_Table%Tab(ITab)%NumAlf   = NumAlpha
      ENDDO ! ITab


         ! Deallocate the temporary Re array.

      DEALLOCATE ( RnAry, STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort ( ' Error deallocating memory for the RnAry array in GetAF.' )
      ENDIF


         ! Deallocate the temporary data array.

      DEALLOCATE ( AF_DataO, STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort ( ' Error deallocating memory for the AF_DataO array in GetAF.' )
      ENDIF

   ENDIF

   CLOSE ( UnAF )


   RETURN
   END SUBROUTINE GetAF !  ( AF_File, AF_Table, ISeg )
!=======================================================================
   FUNCTION GetCoef( ISeg, Alpha, AlfaTab, CoefTab, NumRows, Ind )


      ! Interpolation routine for airfoil section coefficients.



   USE                                  NWTC_Aero
   USE                                  Precision
   USE                                  ProgMod

   IMPLICIT                             NONE


      ! Function declaration.

   REAL(ReKi)                        :: GetCoef                                 ! The value returned by this function.


      ! Argument declarations.

   INTEGER, INTENT(INOUT)            :: Ind                                     ! The starting/resulting index into the tables.
   INTEGER, INTENT(IN)               :: ISeg                                    ! The current segment.
   INTEGER, INTENT(IN)               :: NumRows                                 ! The length of the arrays.

   REAL(ReKi), INTENT(IN)            :: AlfaTab   (NumRows)                     ! Table of AoAs.
   REAL(ReKi), INTENT(IN)            :: Alpha                                   ! Angle of attack to get the coefficient for.
   REAL(ReKi), INTENT(IN)            :: CoefTab   (NumRows)                     ! Table of coefficients.



      ! If Alpha is to the outside the table, the user needs to make up some data.  Warn the user and stop the program.

   IF ( ( Alpha < AlfaTab(1) ) .OR. ( AlfaTab(NumRows) < Alpha ) )  THEN

      CALL Abort ( ' For segment '//TRIM( Int2LStr( ISeg ) )//', the current angle of attack ('//TRIM( Flt2LStr( Alpha ) ) &
                 //' degrees) is outside the domain of your data table (' //TRIM( Flt2LStr( AlfaTab(1) ) )//' to ' &
                 //TRIM( Flt2LStr( AlfaTab(NumRows) ) )//' degrees).  Please extend your data table.' )

   ENDIF


      ! Alpha is in range.  Interpolate.  Use binary interpolation if this is the first time to access this table.

   IF ( Ind == 0 )  THEN
      GetCoef = InterpBin( Alpha, AlfaTab, CoefTab, Ind, NumRows )
   ELSE
      GetCoef = InterpStp( Alpha, AlfaTab, CoefTab, Ind, NumRows )
   ENDIF


   RETURN
   END FUNCTION GetCoef ! ( ISeg, Alpha, AlfaTab, CoefTab, NumRows, Ind )
!=======================================================================
   SUBROUTINE  GetCoefs ( ISeg, Alpha, Re, AF_Table, ClInt, CdInt, CmInt, DoCl, DoCd, DoCm )


      ! This routine finds the Re-bounding tables and then calls GetCoef() to get the
      ! desired coefficients for the two tables and then interpolates between them.


   USE                                  NWTC_Aero
   USE                                  Precision

   IMPLICIT                             NONE


      ! Argument declarations.

   TYPE (ElmTable), INTENT(INOUT)    :: AF_Table                                ! The table of airfoil data for the current segment.

   INTEGER, INTENT(IN)               :: ISeg                                    ! The current segment.

   LOGICAL, INTENT(IN)               :: DoCd                                    ! Get Cd.
   LOGICAL, INTENT(IN)               :: DoCl                                    ! Get Cl.
   LOGICAL, INTENT(IN)               :: DoCm                                    ! Get Cm.

   REAL(ReKi), INTENT(IN)            :: Alpha                                   ! Angle of attack to get the coefficient for.
   REAL(ReKi), INTENT(OUT)           :: CdInt                                   ! Interpolated drag coefficient.
   REAL(ReKi), INTENT(OUT)           :: ClInt                                   ! Interpolated lift coefficient.
   REAL(ReKi), INTENT(OUT)           :: CmInt                                   ! Interpolated pitching-moment coefficient.
   REAL(ReKi), INTENT(IN)            :: Re                                      ! Reynolds number.


      ! Local declarations.

   REAL(ReKi)                        :: CdHi                                    ! The drag coefficient for the higher Re.
   REAL(ReKi)                        :: ClHi                                    ! The lift coefficient for the higher Re.
   REAL(ReKi)                        :: CmHi                                    ! The pitching-moment coefficient for the higher Re.
   REAL(ReKi)                        :: Fract                                   ! The fractional distance between tables.

   INTEGER                           :: ITab                                    ! An index for table number.
   INTEGER                           :: ITabLo                                  ! The table number that is the lower bound for Re.
   INTEGER                           :: ITabHi                                  ! The table number that is the lower bound for Re.

   LOGICAL                           :: OneTable                                ! Flag that tells if we need to read only one table (no interpolation).



      ! Find the bounding tables (if multiple) for this Re.  If there is only one table
      ! or if we are outside the range of tables, we won't need to interpolate.

   IF ( Re <= AF_Table%Tab(1)%Re )  THEN
      ITabLo   = 1
      OneTable = .TRUE.
   ELSEIF ( Re >= AF_Table%Tab(AF_Table%NumTabs)%Re )  THEN
      ITabLo   = AF_Table%NumTabs
      OneTable = .TRUE.
   ELSEIF ( AF_Table%NumTabs > 1 )  THEN
      DO ITab=1,AF_Table%NumTabs-1
         IF ( Re <= AF_Table%Tab(ITab+1)%Re )  THEN
            ITabLo = ITab
            ITabHi = ITab + 1
            EXIT
         ENDIF
      ENDDO
      OneTable = .FALSE.
   ELSE
      ITabLo   = 1
      OneTable = .TRUE.
   ENDIF


      ! Get the coefficients for ITabLo.

   IF ( DoCl )  THEN
      ClInt    = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cl, AF_Table%Tab(ITabLo)%NumAlf, AF_Table%Tab(ITabLo)%Ind )
   ENDIF

   IF ( DoCd )  THEN
      CdInt    = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cd, AF_Table%Tab(ITabLo)%NumAlf, AF_Table%Tab(ITabLo)%Ind )
   ENDIF

   IF ( DoCm )  THEN
      CmInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cm, AF_Table%Tab(ITabLo)%NumAlf, AF_Table%Tab(ITabLo)%Ind )
   ENDIF


      ! If we don't need to interpolate, we don't need to make a second call and we are done.

   IF ( OneTable )  RETURN


      ! Get the coefficients for ITabHi.  Use step-wise interpolation for all but the first coefficient called.

   Fract    = ( Re - AF_Table%Tab(ITabLo)%Re )/( AF_Table%Tab(ITabHi)%Re - AF_Table%Tab(ITabLo)%Re )

   IF ( DoCl )  THEN
      ClHi     = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cl, AF_Table%Tab(ITabHi)%NumAlf, AF_Table%Tab(ITabLo)%Ind )
      ClInt    = ClInt + Fract*( ClHi - ClInt )
   ENDIF

   IF ( DoCd )  THEN
      CdHi     = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cd, AF_Table%Tab(ITabHi)%NumAlf, AF_Table%Tab(ITabLo)%Ind )
      CdInt    = CdInt + Fract*( CdHi - CdInt )
   ENDIF

   IF ( DoCm )  THEN
      CmHi  = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cm, AF_Table%Tab(ITabHi)%NumAlf, AF_Table%Tab(ITabLo)%Ind )
      CmInt = CmInt + Fract*( CmHi - CmInt )
   ENDIF


   RETURN
   END SUBROUTINE GetCoefs ! ( ISeg, Alpha, Re, AF_Table, ClInt, CdInt, CmInt )
!=======================================================================
   SUBROUTINE GetRoot ( GivenFil, RootName )


      ! Let's parse the root file name from the name of the given file.
      ! We'll count everything after the last period as the extension.


   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: GivenFil                                     ! The name of the given file.
   CHARACTER(*), INTENT(OUT)    :: RootName                                     ! The parsed root name of the given file.


      ! Local declarations.

   INTEGER                      :: I                                            ! DO index for character position.



   DO I=LEN_TRIM( GivenFil ),1,-1
      IF ( GivenFil(I:I) == '.' )  THEN
         RootName = GivenFil(:I-1)
         RETURN
      ENDIF
   ENDDO ! I

   RootName =  GivenFil


   RETURN
   END SUBROUTINE GetRoot ! ( GivenFil, RootName )
!=======================================================================
   SUBROUTINE GetTokens ( Line , NumTok , Tokens , Error )


      ! This routine will parse Line for NumTok "words" and return them in the
      ! Tokens array.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: NumTok                                       ! The number of "words" to look for.

   LOGICAL, INTENT(OUT)         :: Error                                        ! Error flag to indicate an insuffient number of tokens were found.

   CHARACTER(*), INTENT(INOUT)  :: Line                                         ! The string to search.
   CHARACTER(*), INTENT(OUT)    :: Tokens  (NumTok)                             ! The tokens that were found.


      ! Local declarations.

   INTEGER                      :: IT                                           ! Token index.
   INTEGER                      :: NextBlank                                    ! The location of the next blank character.



   NextBlank = 0

   DO IT=1,NumTok

      Line      = ADJUSTL( Line(NextBlank+1:) )
      NextBlank = INDEX  ( Line , ' ' )

      IF ( NextBlank == 0 )  THEN
        Error = .TRUE.
        RETURN
      ENDIF

      Tokens(IT) = Line(1:NextBlank-1)

   ENDDO ! IT


   RETURN
   END SUBROUTINE GetTokens ! ( Line , NumTok , Tokens , Error )
!=======================================================================
   SUBROUTINE GetWords ( Line, Words, NumWords )


      ! This subroutine is to count the number of "words" in a line of text.


   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: NumWords                                     ! The number of words to look for.

   CHARACTER(*), INTENT(IN)     :: Line                                         ! The string to search.
   CHARACTER(*), INTENT(OUT)    :: Words(NumWords)                              ! The array of found words.


      ! Local declarations.

   INTEGER                      :: Ch                                           ! Character position within the string.
   INTEGER                      :: IW                                           ! Word index.
   INTEGER                      :: NextWhite                                    ! The location of the next whitespace in the string.



      ! Let's prefill the array with blanks.

   DO IW=1,NumWords
      Words(IW) = ' '
   ENDDO ! IW


      ! Let's make sure we have text on this line.

   IF ( LEN_TRIM( Line ) == 0 )  RETURN


      ! Parse words separated by any combination of spaces, tabs, commas,
      ! semicolons, single quotes, and double quotes ("whitespace").

   Ch = 0
   IW = 0

   DO

      NextWhite = SCAN( Line(Ch+1:) , ' ,;''"'//Tab )

      IF ( NextWhite > 1 )  THEN

         IW        = IW + 1
         Words(IW) = Line(Ch+1:Ch+NextWhite-1)

         IF ( IW == NumWords )  EXIT

         Ch = Ch + NextWhite

      ELSEIF ( NextWhite == 1 )  THEN

         Ch = Ch + 1

         CYCLE

      ELSE

         EXIT

      ENDIF

   ENDDO


   RETURN
   END SUBROUTINE GetWords ! ( Line, Words, NumWords )
!=======================================================================
   FUNCTION Int2LStr ( Intgr )


      ! This function returns a left-adjusted string representing the passed integer.


   IMPLICIT                        NONE


      ! Function declaration.

   CHARACTER(11)                :: Int2LStr                                     ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Intgr                                        ! The integer to convert to a left-justified string.



   WRITE (Int2LStr,'(I11)')  Intgr

   Int2Lstr = ADJUSTL( Int2LStr )


   RETURN
   END FUNCTION Int2LStr ! ( Intgr )
!=======================================================================
   FUNCTION InterpBin( XVal, XAry, YAry, ILo, AryLen )


      ! This funtion returns a y-value that corresponds to an input x-value by interpolating into the arrays.
      ! It uses a binary interpolation scheme that takes about log(AryLen)/log(2) steps to converge.
      ! It returns the first or last YAry() value if XVal is outside the limits of XAry().


   USE                             Precision

   IMPLICIT                        NONE


      ! Function declaration.


   REAL(ReKi)                   :: InterpBin                                       ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the arrays.
   INTEGER, INTENT(INOUT)       :: ILo                                             ! The low index into the arrays.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.
   REAL(ReKi), INTENT(IN)       :: YAry    (AryLen)                                ! Array of Y values to be interpolated.


      ! Local declarations.

   INTEGER                      :: IHi                                             ! The high index into the arrays.
   INTEGER                      :: IMid                                            ! The mid-point index between IHi and ILo.



      ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      InterpBin = YAry(1)
      ILo       = 1
      RETURN
   ELSEIF ( XVal >= XAry(AryLen) )  THEN
      InterpBin = YAry(AryLen)
      ILo       = AryLen - 1
      RETURN
   ENDIF


      ! Let's interpolate!

   ILo  = 1
   IHi  = AryLen

   DO WHILE ( IHi-ILo > 1 )

      IMid = ( IHi + ILo )/2

      IF ( XVal >= XAry(IMid) ) THEN
         ILo = IMid
      ELSE
         IHi = IMid
      ENDIF

   END DO

   InterpBin = YAry(ILo) + ( YAry(IHi) - YAry(ILo) )*( XVal - XAry(ILo) )/( XAry(IHi) - XAry(ILo) )


   RETURN
   END FUNCTION InterpBin ! ( XVal, XAry, YAry, ILo, AryLen )
!=======================================================================
   FUNCTION InterpStp( XVal, XAry, YAry, Ind, AryLen )


      ! This funtion returns a y-value that corresponds to an input x-value by interpolating into the arrays.
      ! It uses the passed index as the starting point and does a stepwise interpolation from there.  This is
      ! especially useful when the calling routines save the value from the last time this routine was called
      ! for a given case where XVal does not change much from call to call.  When there is no correlation
      ! from one interpolation to another, InterpBin() may be a better choice.
      ! It returns the first or last YAry() value if XVal is outside the limits of XAry().


   USE                             NWTC_Gen
   USE                             Precision

   IMPLICIT                        NONE


      ! Function declaration.


   REAL(ReKi)                   :: InterpStp                                       ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the arrays.
   INTEGER, INTENT(INOUT)       :: Ind                                             ! Initial and final index into the arrays.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.
   REAL(ReKi), INTENT(IN)       :: YAry    (AryLen)                                ! Array of Y values to be interpolated.



      ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      InterpStp = YAry(1)
      Ind       = 1
      RETURN
   ELSEIF ( XVal >= XAry(AryLen) )  THEN
      InterpStp = YAry(AryLen)
      Ind       = AryLen - 1
      RETURN
   ENDIF


     ! Let's interpolate!

   Ind = MAX( MIN( Ind, AryLen-1 ), 1 )

   DO

      IF ( XVal < XAry(Ind) )  THEN

         Ind = Ind - 1

      ELSEIF ( XVal >= XAry(Ind+1) )  THEN

         Ind = Ind + 1

      ELSE

         InterpStp = ( YAry(Ind+1) - YAry(Ind) )*( XVal - XAry(Ind) )/( XAry(Ind+1) - XAry(Ind) ) + YAry(Ind)
         RETURN

      ENDIF

   ENDDO


   RETURN
   END FUNCTION InterpStp ! ( XVal, XAry, YAry, Ind, AryLen )
!=======================================================================
   SUBROUTINE NormStop


      ! This routine performs a normall termination of the program.


   USE                             ProgMod
   USE                             SysSubs

   IMPLICIT                        NONE



   CALL WrScr1   ( TRIM( ProgName )//' terminated normally.' )
   CALL ProgExit ( 0 )


   END SUBROUTINE NormStop
!=======================================================================
   SUBROUTINE OpenFInpFile ( Un, InFile )


      ! This routine opens a formatted input file.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the input file.

   CHARACTER(*), INTENT(IN)     :: InFile                                       ! Name of the input file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.

      ! NOTE: Do not explicitly declare the precision of this variable [as in
      !       LOGICAL(1)] so that the statements using this variable work with
      !       any compiler:
   LOGICAL                      :: Exists                                       ! Flag indicating whether or not a file Exists.



      ! See if input file Exists.

   INQUIRE ( FILE=TRIM( InFile ) , EXIST=Exists )

   IF ( .NOT. Exists )  Call Abort ( ' The input file, "'//TRIM( InFile )//'", was not found.')


      ! Open input file.  Make sure it worked.

   OPEN( Un, FILE=TRIM( InFile ), STATUS='OLD', FORM='FORMATTED', IOSTAT=IOS, ACTION='READ' )

   IF ( IOS /= 0 )  CALL Abort ( ' Cannot open file "'//TRIM( InFile ) &
                               //'".  Another program like MS Excel may have locked it for writing.' )


   RETURN
   END SUBROUTINE OpenFInpFile ! ( Un, InFile )
!=======================================================================
   SUBROUTINE OpenFOutFile ( Un, OutFile )


      ! This routine opens a formatted output file.


   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the output file.

   CHARACTER(*), INTENT(IN)     :: OutFile                                      ! Name of the output file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.



      ! Open output file.  Make sure it worked.

   OPEN( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM='FORMATTED', IOSTAT=IOS )

   IF ( IOS /= 0 )  CALL Abort ( ' Cannot open file "'//TRIM( OutFile ) &
                               //'".  Another program like MS Excel may have locked it for writing.' )


   RETURN
   END SUBROUTINE OpenFOutFile ! ( Un, OutFile )
!=======================================================================
   SUBROUTINE PremEOF ( Fil , Variable )


      ! This routine prints out an EOF message and aborts the program.


   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Fil                                          ! The name of the file that ran out of data.
   CHARACTER(*), INTENT(IN)     :: Variable                                     ! The name of the variable we were trying to read at the time.



   CALL WrScr1 ( ' Premature EOF for file "'//TRIM( Fil )//'".' )
   CALL Abort  ( 'The error occurred while trying to read '//TRIM( Variable )//'.' )


   RETURN
   END SUBROUTINE PremEOF ! ( Fil , Variable )
!=======================================================================
   SUBROUTINE ReadCAry ( UnIn, Fil, CharAry, AryLen, AryName, AryDescr )


      ! This routine reads a AryLen values into a character array from the next AryLen lines of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER(*), INTENT(OUT)    :: CharAry(AryLen)                                 ! Real variable being read.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.
   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the string array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(35)                :: Frmt = "( 15X, A, T27, ' - ', A, /, 2X, A )"    ! Output format for string parameters.



   DO Ind=1,AryLen

      READ (UnIn,*,IOSTAT=IOS)  CharAry(Ind)

      CALL CheckIOS ( IOS, Fil, TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', StrType )

      IF ( Echo )  THEN
         WRITE (UnEc,Frmt)  TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', AryDescr, CharAry(Ind)
      ENDIF

   ENDDO ! Ind


   RETURN
   END SUBROUTINE ReadCAry ! ( UnIn, Fil, CharAry, AryLen, AryName, AryDescr )
!=======================================================================
   SUBROUTINE ReadCom ( UnIn, Fil, ComName )


      ! This routine reads a comment from the next line of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER( * ), INTENT(IN)   :: Fil                                             ! Name of the input file.
   CHARACTER( * ), INTENT(IN)   :: ComName                                         ! Text string containing the comment name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(200)               :: Comment                                         ! Text string containing the comment.




   READ (UnIn,'(A)',IOSTAT=IOS)  Comment

   CALL CheckIOS ( IOS, Fil, ComName, StrType )

   IF ( Echo )  THEN
      WRITE (UnEc,'(A)')  Comment
   ENDIF


   RETURN
   END SUBROUTINE ReadCom ! ( UnIn, Fil, ComName )
!=======================================================================
   SUBROUTINE ReadCVar ( UnIn, Fil, CharVar, VarName, VarDescr )


      ! This routine reads a single character variable from the next line of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER(*), INTENT(OUT)    :: CharVar                                         ! Character variable being read.
   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(35)                :: Frmt = "( 15X, A, T27, ' - ', A, /, 2X, A )"    ! Output format for string parameters.




   READ (UnIn,*,IOSTAT=IOS)  CharVar

   CALL CheckIOS ( IOS, Fil, VarName, StrType )

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  VarName, VarDescr, TRIM( CharVar )
   ENDIF


   RETURN
   END SUBROUTINE ReadCVar ! ( UnIn, Fil, CharVar, VarName, VarDescr )
!=======================================================================
   SUBROUTINE ReadIAry ( UnIn, Fil, IntAry, AryLen, AryName, AryDescr )


      ! This routine reads a AryLen values into an integer array from the next AryLen lines of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output
   USE                             Precision

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(OUT)         :: IntAry(AryLen)                                  ! Integer array being read.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the integer array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, I11, 2X, A, T27, ' - ', A )"      ! Output format for integer array parameters



   READ (UnIn,*,IOSTAT=IOS)  ( IntAry(Ind), Ind=1,AryLen )

   CALL CheckIOS ( IOS, Fil, TRIM( AryName ), NumType )

   IF ( Echo )  THEN
      DO Ind=1,AryLen
         WRITE (UnEc,Frmt)  IntAry(Ind), TRIM( AryName ), AryDescr
      ENDDO ! Ind
   ENDIF


   RETURN
   END SUBROUTINE ReadIAry ! ( UnIn, Fil, IntAry, AryLen, AryName, AryDescr )
!=======================================================================
   SUBROUTINE ReadIVar ( UnIn, Fil, IntVar, VarName, VarDescr )


      ! This routine reads a single integer variable from the next line of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(OUT)         :: IntVar                                          ! Integer variable being read.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(33)                :: Frmt = "( 2X, I11, 2X, A, T27, ' - ', A )"      ! Output format for integer parameters.




   READ (UnIn,*,IOSTAT=IOS)  IntVar

   CALL CheckIOS ( IOS, Fil, VarName, NumType )

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  IntVar, VarName, VarDescr
   ENDIF


   RETURN
   END SUBROUTINE ReadIVar ! ( UnIn, Fil, IntVar, VarName, VarDescr )
!=======================================================================
   SUBROUTINE ReadLVar ( UnIn, Fil, LogVar, VarName, VarDescr )


      ! This routine reads a single logical variable from the next line of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   LOGICAL, INTENT(OUT)         :: LogVar                                          ! Logical variable being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(33)                :: Frmt = "( 2X, L11, 2X, A, T27, ' - ', A )"      ! Output format for logical parameters.




   READ (UnIn,*,IOSTAT=IOS)  LogVar

   CALL CheckIOS ( IOS, Fil, VarName, FlgType )

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  LogVar, VarName, VarDescr
   ENDIF


   RETURN
   END SUBROUTINE ReadLVar ! ( UnIn, Fil, LogVar, VarName, VarDescr )
!=======================================================================
   SUBROUTINE ReadRAry ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr )


      ! This routine reads a AryLen values into a real array from the next AryLen lines of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output
   USE                             Precision

   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   REAL(ReKi), INTENT(OUT)      :: RealAry(AryLen)                                 ! Real array being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the real array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T27, ' - ', A )" ! Output format for real array parameters



   READ (UnIn,*,IOSTAT=IOS)  ( RealAry(Ind), Ind=1,AryLen )

   CALL CheckIOS ( IOS, Fil, TRIM( AryName ), NumType )

   IF ( Echo )  THEN
      DO Ind=1,AryLen
         WRITE (UnEc,Frmt)  RealAry(Ind), TRIM( AryName ), AryDescr
      ENDDO ! Ind
   ENDIF


   RETURN
   END SUBROUTINE ReadRAry ! ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr )
!=======================================================================
  SUBROUTINE ReadRVar ( UnIn, Fil, RealVar, VarName, VarDescr )


      ! This routine reads a single real variable from the next line of the input file.


   USE                             NWTC_Gen
   USE                             NWTC_Output
   USE                             Precision

   IMPLICIT                        NONE


      ! Argument declarations:

   REAL(ReKi), INTENT(OUT)      :: RealVar                                         ! Real variable being read.

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER( *), INTENT(IN)    :: Fil                                             ! Name of the input file.
   CHARACTER( *), INTENT(IN)    :: VarDescr                                        ! Text string describing the variable.
   CHARACTER( *), INTENT(IN)    :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T27, ' - ', A )" ! Output format for real parameters



   READ (UnIn,*,IOSTAT=IOS)  RealVar

   CALL CheckIOS ( IOS, Fil, VarName, NumType )

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  RealVar, VarName, VarDescr
   ENDIF


   RETURN
   END SUBROUTINE ReadRVar ! ( UnIn, Fil, RealVar, VarName, VarDescr )
!=======================================================================
   SUBROUTINE Wr_Pr ( Str )


   !  This routine writes out a prompt to the screen without
   !  following it with a new line, though a new line precedes it.


   USE                             SysSubs

   IMPLICIT                        NONE


      ! Argument declarations:

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The prompt string to print.



   CALL WrScr ( ' ' )
   CALL WrNR  ( TRIM( Str )//' > ' )


   RETURN
   END SUBROUTINE Wr_Pr ! ( Str )
!=======================================================================
   SUBROUTINE WrScr1 ( Str )


      ! This routine writes out a string to the screen after a blank line.


   USE                             SysSubs

   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*)                 :: Str                                         ! The string to print.



   CALL WrScr ( ' ' )
   CALL WrScr ( TRIM( Str ) )


   RETURN
   END SUBROUTINE WrScr1 ! ( Str )
!=======================================================================

END MODULE NWTC_Subs
