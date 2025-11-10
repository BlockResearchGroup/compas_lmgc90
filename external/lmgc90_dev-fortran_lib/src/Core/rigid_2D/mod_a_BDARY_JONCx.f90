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
MODULE a_BDARY_JONCx                                       

  !!****h* LMGC90.CORE/a_BDARY_JONCx
  !! NAME
  !!  module a_BDARY_JONCx
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****
  
  USE utilities
  use overall, only : PI_g

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_JONCx(axes,area,avrd,gyrd)

    IMPLICIT NONE
    
    REAL(kind=8),DIMENSION(:),POINTER :: axes
    REAL(kind=8)                      :: area
    REAL(kind=8)                      :: avrd,gyrd
    
    ALLOCATE(axes(2))

    READ(G_clin(28:69),'(2(7X,D14.7))') axes(1),axes(2)
    
    area = (4.d0*axes(1)*axes(2)) + (PI_g * axes(2) * axes(2))
    
    avrd = SQRT(area/PI_g)

    gyrd = sqrt(((PI_g*axes(2)*axes(2)*( (axes(1)*axes(1)) + (0.5*axes(2)*axes(2)) )) &
         + (2.d0*axes(1)*axes(2)*( (axes(1)*axes(1)) + (axes(2)*axes(2)))) ) / area)


  END SUBROUTINE read_BDARY_JONCx
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_JONCx(nfich,itacty,tacID,color,ax1,ax2)

    IMPLICIT NONE

    INTEGER,INTENT(in)   ::  nfich
    INTEGER              ::  itacty
    CHARACTER(len=5)     ::  tacID,color
    REAL(kind=8)         ::  ax1,ax2
    
    WRITE(nfich,104) tacID,itacty,'color',color,'ax1 =',ax1,'ax2 =',ax2   
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
    
  END SUBROUTINE write_BDARY_JONCx
!!!------------------------------------------------------------------------

END MODULE a_BDARY_JONCx

