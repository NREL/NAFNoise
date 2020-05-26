!=======================================================================
MODULE Precision


   ! This module stores constants to specify the KIND of variables.


IMPLICIT                        NONE

INTEGER   , PARAMETER        :: DbKi     =  8                                   ! Default kind for double-precision numbers.
INTEGER   , PARAMETER        :: ReKi     =  4                                   ! Default kind for real numbers.
 ! NOTE: Use compile option "/real_size:64" (or "/4R8") when using ReKi = 8


END MODULE Precision
