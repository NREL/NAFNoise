FUNCTION Pressure(k1_in)

    USE Atmosphere
    USE AirfoilParams
    USE BLParams
    USE Precision
    USE TNOConstants
    USE Wavenumber

    implicit none

    ! Variables
    REAL (ReKi):: a,b,answer
    REAL (ReKi):: abserr,resabs,resasc
    
    REAL (ReKi):: k1_in,Pressure

    REAL (ReKi), EXTERNAL :: int1

    a = 0. !1e-4*d99(1)
    IF (ISSUCTION)THEN
        b = d99(1)
    ELSE
        b = d99(2)
    ENDIF
    

    k1 = k1_in
    k3 = 0.
    k= sqrt(k1**2+k3**2)

    CALL qk61(int1,a,b,answer,abserr,resabs,resasc)
               
    Pressure = 4.*rho**2*k1**2./(k1**2.+k3**2.)*answer
               
RETURN
               
END FUNCTION Pressure