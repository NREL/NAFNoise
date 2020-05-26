MODULE TNOConstants

USE Precision

REAL (ReKi),PARAMETER :: Cnuk = 5.5
REAL (ReKi),PARAMETER :: kappa = 0.41
REAL (ReKi),PARAMETER :: Cmu = 0.09
REAL (ReKi),PARAMETER :: pi = 3.1415

INTEGER (4),PARAMETER :: limit = 5000

INTEGER (4)             :: i_omega
REAL (ReKi),ALLOCATABLE :: omega(:)

END MODULE TNOConstants

!===========================================================

MODULE Atmosphere

USE Precision

!atmosphere constants
REAL (ReKi) nu
REAL (ReKi) co
REAL (ReKi) rho

END MODULE Atmosphere


!===========================================================

MODULE Wavenumber

USE Precision

REAL (ReKi)           :: k
REAL (ReKi)           :: k1
REAL (ReKi)           :: k3

END MODULE Wavenumber

!===========================================================

MODULE BLParams

USE Precision
REAL (ReKi)           :: d99(2)
REAL (ReKi)           :: Cf(2)
REAL (ReKi)           :: d_star(2)

END MODULE BLParams
!===========================================================

MODULE AirfoilParams

USE Precision

CHARACTER*128         :: airfoil
REAL (ReKi)           :: aofa,a_chord,Mach,Re
REAL (ReKi)           :: xtrup, xtrlo
LOGICAL               :: ISTRIPPED   
LOGICAL               :: ISNACA   
LOGICAL               :: ISSUCTION

END MODULE AirfoilParams

!===========================================================

MODULE Third_Octave_Bands

USE Precision

INTEGER (4),PARAMETER :: NumBands = 34
REAL (ReKi),PARAMETER :: Third_Octave(NumBands) = (/10.,12.5,16.,20.,25.,31.5,40.,50.,63.,80.,	&
                                                    100.,125.,160.,200.,250.,315.,400.,500.,630.,800.,	& 
                                                    1000.,1250.,1600.,2000.,2500.,3150.,4000.,5000.,6300.,8000.,	&
                                                    10000.,12500.,16000.,20000./)
                                       
END MODULE Third_Octave_Bands
