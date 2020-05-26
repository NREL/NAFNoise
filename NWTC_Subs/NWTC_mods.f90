!=======================================================================
MODULE NWTC_Aero


   ! This module stores various constants.


USE                                Precision

TYPE                            :: AeroTable                                    ! Declare new type that is an allocatable table of data.
   REAL(ReKi)                   :: AlfaStal                                     ! The stall AoA for this table.
   REAL(ReKi)                   :: Re                                           ! The Re for this table.
   INTEGER                      :: NumAlf                                       ! Number of angles of attack in the table.
   INTEGER                      :: Ind      = 0                                 ! Last-used index into table.  Zero at beginning.
   REAL(ReKi), ALLOCATABLE      :: Alpha    (:)                                 ! The angle of attack vector.
   REAL(ReKi), ALLOCATABLE      :: Cl       (:)                                 ! The Cl vector.
   REAL(ReKi), ALLOCATABLE      :: Cd       (:)                                 ! The Cd vector.
   REAL(ReKi), ALLOCATABLE      :: Cm       (:)                                 ! The Cm vector.
ENDTYPE AeroTable

TYPE                            :: AlfIndx                                      ! Declare new type that is an allocatable table of alpha indices.
   INTEGER                      :: NumBld                                       ! Number of blades in the table.
   INTEGER                      :: NumElm                                       ! Number of segments in the table.
   INTEGER, ALLOCATABLE         :: Ind      (:,:)                               ! The tables in this supertable.
ENDTYPE AlfIndx

TYPE                            :: ElmTable                                     ! Declare new type that is an allocatable table of data.
   INTEGER                      :: NumTabs                                      ! Number of tables in the supertable for an element.
   TYPE(AeroTable), ALLOCATABLE :: Tab      (:)                                 ! The tables in this supertable.
ENDTYPE ElmTable

REAL(ReKi)                      :: KinVisc                                      ! The kinematic viscosity at hub height.
REAL(ReKi)                      :: Rho                                          ! The air density at hub height.

LOGICAL                         :: UseCm                                        ! Flag to tell if there are Cm data in the airfoil files.


END MODULE NWTC_Aero
!=======================================================================
MODULE NWTC_Const


   ! This module stores various constants.


USE                                Precision

REAL(ReKi), PARAMETER           :: D2R      =  0.017453293                      ! Factor to convert degrees to radians.
REAL(ReKi), PARAMETER           :: Pi       =  3.1415927                        ! Ratio of a circle's circumference to its diameter.
REAL(ReKi), PARAMETER           :: R2D      = 57.295780                         ! Factor to convert radians to degrees.
REAL(ReKi), PARAMETER           :: RPM2RPS  =  0.10471976                       ! Factor to convert revolutions per minute to radians per second.
REAL(ReKi), PARAMETER           :: RPS2RPM  =  9.5492966                        ! Factor to convert radians per second to revolutions per minute.
REAL(ReKi), PARAMETER           :: TwoPi    =  6.2831853                        ! 2*Pi.


END MODULE NWTC_Const
!=======================================================================
MODULE NWTC_Gen


   ! This module stores input variables for general program control.


INTEGER, PARAMETER              :: FlgType  = 1                                 ! Switch for telling if a variable is a flag.
INTEGER, ALLOCATABLE            :: IntIndx  (:,:)                               ! The array of indices holding that last index used for interpolation in IntBlade().
INTEGER, PARAMETER              :: NumType  = 2                                 ! Switch for telling if a variable is a number.
INTEGER, PARAMETER              :: StrType  = 3                                 ! Switch for telling if a variable is a string.


END MODULE NWTC_Gen
!=======================================================================
MODULE NWTC_Output


   ! This module stores input variables for general program control.


INTEGER                         :: UnEc     = 19                                ! I/O unit number for the echo file.

LOGICAL                         :: Echo                                         ! Flag that specifies whether or not to produce an echo file.

CHARACTER(1), PARAMETER         :: Tab      = CHAR( 9 )                         ! The tab character.


END MODULE NWTC_Output
!=======================================================================
MODULE ProgMod


   ! This module stores the name and version number of the programs that use this library..


!Start of proposed change.  v1.00a-mlb  02-Jul-2004
CHARACTER(32)                   :: NWTCVer  = 'v1.00a-mlb (02-Jul-2004)'        ! The version (including date) of the NWTC routines.
!End of proposed change.  v1.00a-mlb  02-Jul-2004
CHARACTER(20)                   :: ProgName = 'NAFNoise'                                   ! The name of the calling program.
!Start of proposed change.  v1.00a-mlb  02-Jul-2004
CHARACTER(58)                   :: ProgVer  = '1.00'                                   ! The version (including date) of the calling program.
!End of proposed change.  v1.00a-mlb  02-Jul-2004


END MODULE ProgMod
!=======================================================================
