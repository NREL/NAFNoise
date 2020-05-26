!-----main program-------------------------------------------g.guidati--
! 
     subroutine TI_Noise
! 
!-----------------------------------------------------------------------
!               
!     franco guidati    IAG
!                       Uni Stuttgart
!
! 
!     scope             main program
! 
!.......................................................................
!               declarations
!.......................................................................
     USE TINoiseGeneric
     USE TINoiseGeo
     USE TINoiseInput

!.......................................................................
!     execute program 
!.......................................................................

      ! initialize important constants
      call INICON
      ! read in run parameters
      call READIN
     
      ! loop over all airfoil to be analysed
      do i=1,nairfoil
!        open(10,file=trim(cairfoil(i)))
!        open(20,file=trim(cdescript(i))//'-geo.dat')
!        open(21,file=trim(cdescript(i))//'-cp.dat')
!        open(22,file=trim(cdescript(i))//'-path.dat')
!        open(24,file=trim(cdescript(i))//'-bema.dat')
!        open(128,file=trim(cdescript(i))//'-dirfp.dat',recl=200)
!        open(129,file=trim(cdescript(i))//'-diraf.dat',recl=200)
!        open(28,file=trim(cdescript(i))//'-splfp.out')
!        open(79,file=trim(cdescript(i))//'-splaf.out')
	     alfa = alpha_in(i) * pi2 / 360.0d0
        call DEFGEO
        call DRM_AER
        call DRM_ACU
!        close(24)        
!        close(28)
!        close(79)
!        close(128)
!        close(129)
      end do
          
      
      ! close(25)



!.......................................................................
!               end of program
!.......................................................................
    
!***********************************************************************
      end subroutine TI_Noise

