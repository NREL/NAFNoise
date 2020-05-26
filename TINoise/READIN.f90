!-----subroutine---------------------------------------------g.guidati--
! 
     subroutine READIN
! 
!-----------------------------------------------------------------------
!               
!     franco guidati    IAG
!                       Uni Stuttgart
!
! 
!     scope             initialise constants
! 
!.......................................................................
!               declarations
!.......................................................................
     USE TINoiseGeneric
     USE TINoiseInput
     USE Atmosphere
     USE AirfoilParams
     USE Third_Octave_Bands
     
     IMPLICIT                        NONE  
        ! Local variables.            
     INTEGER(4)                   :: i
     
!.......................................................................
!     initialise the input variables for TI Noise
!.......................................................................
      nairfoil = 1
      csound = co
      chord = a_chord
      do i=1,nairfoil
         cairfoil (i) = airfoil
         cdescript (i) = airfoil
         alpha_in (i) = aofa
         mach_ti = Mach
      enddo

      nfreq = NumBands
      freq_in(1:NumBands) = Third_Octave
      
!      do i=1,nairfoil
!        write(*,*) cairfoil(i),cdescript(i)
!      end do
      

      return     
!***********************************************************************
      end 

