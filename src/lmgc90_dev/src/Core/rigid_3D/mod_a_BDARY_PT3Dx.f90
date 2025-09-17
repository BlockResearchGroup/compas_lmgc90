!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program 
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
!
! This software is governed by the CeCILL license under French law and
! abiding by the rules of distribution of free software.  You can  use, 
! modify and/ or redistribute the software under the terms of the CeCILL
! license as circulated by CEA, CNRS and INRIA at the following URL
! "http://www.cecill.info". 
!
! As a counterpart to the access to the source code and  rights to copy,
! modify and redistribute granted by the license, users are provided only
! with a limited warranty  and the software's author,  the holder of the
! economic rights,  and the successive licensors  have only  limited
! liability. 
!
! In this respect, the user's attention is drawn to the risks associated
! with loading,  using,  modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean  that it is complicated to manipulate,  and  that  also
! therefore means  that it is reserved for developers  and  experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards their
! requirements in conditions enabling the security of their systems and/or 
! data to be ensured and,  more generally, to use and operate it in the 
! same conditions as regards security. 
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
! To report bugs, suggest enhancements, etc. to the Authors, contact
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================
MODULE a_BDARY_PT3Dx    

  !!****h* LMGC90.CORE/a_BDARY_PT3Dx
  !! NAME
  !!  module a_BDARY_PT3Dx
  !! CHILDREN
  !!  LMGC90.CORE/UTILITIES
  !!****
  
  USE utilities
  use overall, only : PI_g

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_PT3Dx(data,rad4vol,I1,I2,I3,localframe,shift)

    IMPLICIT NONE
    REAL(kind=8)                      :: radius,rad4vol,I1,I2,I3
    REAL(kind=8),DIMENSION(:),POINTER :: data
    REAL(kind=8),DIMENSION(3,3)       :: localframe
    REAL(kind=8),DIMENSION(3)         :: shift

    ALLOCATE(data(3))
    READ(G_clin(28:90),'(3(7X,D14.7))') data(1),data(2),data(3)

    radius = rad4vol
    rad4vol = 4.d0*radius*radius*radius*PI_g/3.d0
    if (I1 == 0.d0) I1 = 0.4d0*radius*radius*rad4vol
    if (I2 == 0.d0) I2 = 0.4d0*radius*radius*rad4vol
    if (I3 == 0.d0) I3 = 0.4d0*radius*radius*rad4vol
!    PRINT*,'PT3Dx ',I1
    localframe = 0.0
    localframe(1,1) = 1.D0
    localframe(2,2) = 1.D0
    localframe(3,3) = 1.D0

    shift = data

  END SUBROUTINE read_BDARY_PT3Dx
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_PT3Dx(nfich,itacty,tacID,color,shift)

    IMPLICIT NONE
    INTEGER                           ::  nfich,itacty
    CHARACTER(len=5)                  ::  tacID,color
    REAL(kind=8),DIMENSION(3)         ::  shift
    
    WRITE(nfich,104) tacID,itacty,'color',color, &
         'coo1=',shift(1),'coo2=',shift(2),'coo3=',shift(3)
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,3(2X,A5,D14.7))
    
  END SUBROUTINE write_BDARY_PT3Dx
!!!------------------------------------------------------------------------

END MODULE a_BDARY_PT3Dx

