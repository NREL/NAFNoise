FUNCTION int1(x2)

USE Atmosphere
USE BLParams
USE Precision
USE TNOConstants
USE Wavenumber
USE AirfoilParams

implicit none

REAL (ReKi):: alpha
REAL (ReKi):: alpha_gauss
REAL (ReKi):: Cfin
REAL (ReKi):: delta 
REAL (ReKi):: dudx
REAL (ReKi):: int1
REAL (ReKi):: ke
REAL (ReKi):: k1_hat
REAL (ReKi):: k3_hat
REAL (ReKi):: kT
REAL (ReKi):: L
REAL (ReKi):: Nut
REAL (ReKi):: phi22
REAL (ReKi):: phim
REAL (ReKi):: ums
REAL (ReKi):: u_star
REAL (ReKi):: U
REAL (ReKi):: Uc
REAL (ReKi):: Uo
REAL (ReKi):: W
REAL (ReKi):: x2

REAL (ReKi), EXTERNAL :: gamma,dgamma

Uo=Mach*co

!constants from xfoil
if (ISSUCTION) then
    alpha = 0.45 ! = 0.3 pressure, = 0.45 suction
    Cfin = Cf(1)
    delta = d99(1)
else
    alpha = 0.30
    Cfin = Cf(2)
    delta = d99(2)
endif
if (Cfin .le. 0.) then
    write(*,*) 'Cf is less than zero, Cf = ',Cfin
    stop
endif
u_star = Uo*sqrt(Cfin/2.)

L = 0.085*delta/kappa*tanh(kappa*x2/(0.085*delta))

if (x2 .gt. delta)then
     U = Uo
     dudx = 0.
     int1 = 0.
     RETURN
else
     W = 1.-cos(pi*x2/delta);
     U = u_star*(1./kappa*log(u_star*x2/nu) +Cnuk+ (Uo/u_star-1./kappa*log(u_star*delta/nu)-Cnuk)*0.5*W)
     dudx = u_star*(1./(kappa*x2)+(Uo/u_star-1./kappa*log(u_star*delta/nu)-Cnuk)* &
            0.5*(pi/delta)*sin(pi*x2/delta))
endif
       
ke=sqrt(pi)/L*0.4213560764 !gamma(5./6.)/gamma(1./3.)
k1_hat = k1/ke
k3_hat = k3/ke

Nut = (L*kappa)**2.*abs(dudx)
kT = sqrt((Nut*dudx)**2./Cmu)
ums = alpha*kT

Uc = 0.7*U
alpha_gauss = 0.05*Uc/L

phim = 1./(alpha_gauss*sqrt(pi))*exp(-((omega(i_omega)-Uc*k1)/alpha_gauss)**2.)

phi22 = 4./9./pi*1/ke**2.*(k1_hat**2.+k3_hat**2.)/(1.+k1_hat**2.+k3_hat**2.)**(7./3.)

int1 = L*ums*(dudx)**2*phi22*phim*exp(-2*abs(k)*x2)

RETURN

END FUNCTION int1
