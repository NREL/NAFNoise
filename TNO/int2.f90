FUNCTION int2(k1)

USE Atmosphere
USE TNOConstants

implicit none

REAL (ReKi) :: int2
REAL (ReKi) :: k1
REAL (ReKi), EXTERNAL :: Pressure

int2 = omega(i_omega)/co/k1*Pressure(k1)

RETURN 

END FUNCTION int2