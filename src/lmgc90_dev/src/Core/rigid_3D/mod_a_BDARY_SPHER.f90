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
MODULE a_BDARY_SPHER    

  !!****h* LMGC90.CORE/a_BDARY_SPHER
  !! NAME
  !!  module a_BDARY_SPHER
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****

  USE utilities
  use overall, only : PI_g

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_SPHER(bndry_radius,volume,I1,I2,I3,localframe)

    IMPLICIT NONE
    REAL(kind=8)                      :: volume,I1,I2,I3
    INTEGER,     DIMENSION(:),POINTER :: idata
    REAL(kind=8),DIMENSION(:),POINTER :: bndry_radius
    REAL(kind=8),DIMENSION(3,3)       :: localframe

    ALLOCATE(bndry_radius(1))
    READ(G_clin(28:48),'(1(7X,D14.7))') bndry_radius(1)

    volume = 4.d0*PI_g*bndry_radius(1)*bndry_radius(1)*bndry_radius(1)/3.d0
    I1     = 0.4d0*bndry_radius(1)*bndry_radius(1)*volume
    I2     = I1
    I3     = I1

    localframe = 0.0d0
    localframe(1,1) = 1.D0
    localframe(2,2) = 1.D0
    localframe(3,3) = 1.D0

  END SUBROUTINE read_BDARY_SPHER
!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_SPHEb(bndry_radius,volume,I1,I2,I3,localframe,shift)

    IMPLICIT NONE
    REAL(kind=8)                      :: volume,I1,I2,I3
    INTEGER,     DIMENSION(:),POINTER :: idata
    REAL(kind=8),DIMENSION(:),POINTER :: bndry_radius
    REAL(kind=8),DIMENSION(3,3)       :: localframe
    REAL(kind=8),DIMENSION(3)         :: shift

    ALLOCATE(bndry_radius(1))
    READ(G_clin(28:48),'(1(7X,D14.7))') bndry_radius(1)

    volume = 4.d0*PI_g*bndry_radius(1)*bndry_radius(1)*bndry_radius(1)/3.d0
    I1     = 0.4d0*bndry_radius(1)*bndry_radius(1)*volume
    I2     = I1
    I3     = I1

    localframe = 0.0d0
    localframe(1,1) = 1.D0
    localframe(2,2) = 1.D0
    localframe(3,3) = 1.D0

    IF ( .NOT. read_G_clin()) THEN
       call faterr('a_BDARY_SPHER::read_BDARY_SPHEb','error reading')  
    END IF
    READ(G_clin(35:48),'(D14.7)') shift(1)
    READ(G_clin(56:69),'(D14.7)') shift(2)
    READ(G_clin(77:90),'(D14.7)') shift(3)

  END SUBROUTINE read_BDARY_SPHEb
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_SPHER(nfich,itacty,tacID,color,DATA)
    
    IMPLICIT NONE
    INTEGER                             :: nfich,itacty
    CHARACTER(len=5)                    :: tacID,color
    REAL(kind=8),DIMENSION(:),POINTER   :: DATA
    REAL(kind=8)                        :: bdry_radius
    
    bdry_radius = DATA(1)

    WRITE(nfich,104) tacID,itacty,'color',color,'byrd=',bdry_radius

104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,1(2X,A5,D14.7))

 END SUBROUTINE write_BDARY_SPHER
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_SPHEb(nfich,itacty,tacID,color,DATA,shift)
    
    IMPLICIT NONE
    INTEGER                             :: nfich,itacty
    CHARACTER(len=5)                    :: tacID,color
    REAL(kind=8),DIMENSION(:),POINTER   :: DATA
    REAL(kind=8)                        :: bdry_radius
    REAL(kind=8),DIMENSION(3)           :: shift

    bdry_radius = DATA(1)

    WRITE(nfich,104) tacID,itacty,'color',color,'byrd=',bdry_radius
    WRITE(nfich,131) 'coo1=',shift(1),'coo2=',shift(2),'coo3=',shift(3)

104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,1(2X,A5,D14.7))
131 FORMAT(27X,3(2X,A5,D14.7))

  END SUBROUTINE write_BDARY_SPHEb
!!!------------------------------------------------------------------------

END MODULE a_BDARY_SPHER
