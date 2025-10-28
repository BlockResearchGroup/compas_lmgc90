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
MODULE a_BDARY_xKSID    

  !!****h* LMGC90.CORE/a_BDARY_xKSID
  !! NAME
  !!  module a_BDARY_xKSID
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****
  
  USE utilities
  use overall, only : PI_g
  
  IMPLICIT NONE
  
CONTAINS
  
  !------------------------------------------------------------------------
  SUBROUTINE read_BDARY_xKSID(bdry_radius,area)
    
    IMPLICIT NONE
    
    REAL(kind=8),DIMENSION(:),POINTER ::  bdry_radius
    REAL(kind=8)                      :: area
    
    ALLOCATE(bdry_radius(1))
    
    READ(G_clin(28:48),'(1(7X,D14.7))') bdry_radius(1)
    
    area =-PI_g * bdry_radius(1) * bdry_radius(1)
    
  END SUBROUTINE read_BDARY_xKSID
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  SUBROUTINE write_BDARY_xKSID(nfich,itacty,tacID,color,bdry_radius)
    
    IMPLICIT NONE
    INTEGER,INTENT(in)   ::  nfich
    INTEGER              ::  itacty
    CHARACTER(len=5)     ::  tacID,color
    REAL(kind=8)         ::  bdry_radius   
    
    WRITE(nfich,104) tacID,itacty,'color',color,'byrd=',bdry_radius
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,1(2X,A5,D14.7))
    
  END SUBROUTINE write_BDARY_xKSID
  !------------------------------------------------------------------------
  
END MODULE a_BDARY_xKSID
