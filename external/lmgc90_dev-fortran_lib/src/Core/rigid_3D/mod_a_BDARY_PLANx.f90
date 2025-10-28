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
MODULE a_BDARY_PLANx   
                                    
  !!****h* LMGC90.CORE/a_BDARY_PLANx
  !! NAME
  !!  module a_BDARY_PLANx
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****
  USE utilities

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_PLANx(axes,volume,I1,I2,I3,localframe,shift)
    
    ! axes(1) corresponds to the half length dir 1 (L1)
    ! axes(2) corresponds to the half length dir 2 (L2)
    ! axes(3) corresponds to the half thickness    (e)
    !
    ! inertie geometrique du plan 
    ! vol = L1 * L2 * e
    ! I1,I2 plan        I1=vol*(L2 ** 2 + e ** 2) /12   I1= vol *( L1**2 + e**2) / 12  
    ! I3 axe hors plan  I3 = vol * (L1**2 + L2**2) /12    

    IMPLICIT NONE
    INTEGER                           :: i
    REAL(kind=8)                      :: volume,I1,I2,I3
    REAL(kind=8),DIMENSION(:),POINTER :: axes
    REAL(kind=8),DIMENSION(3,3)       :: localframe
    REAL(kind=8),DIMENSION(3)         :: shift
    character(len=17) :: IAM
          !12345678901234567
    IAM = 'BDARY_PLANx::read'

    ALLOCATE(axes(3))

    READ(G_clin(35:48),'(D14.7)') axes(1)
    READ(G_clin(56:69),'(D14.7)') axes(2)
    READ(G_clin(77:90),'(D14.7)') axes(3)
    
    volume = 8.D0*axes(1)*axes(2)*axes(3)
    
    I1 = volume * (axes(2)*axes(2)+axes(3)*axes(3))/ 3.D0
    I2 = volume * (axes(1)*axes(1)+axes(3)*axes(3))/ 3.D0
    I3 = volume * (axes(2)*axes(2)+axes(1)*axes(1))/ 3.D0
    
    localframe = 0.0
    localframe(1,1) = 1.D0
    localframe(2,2) = 1.D0
    localframe(3,3) = 1.D0
    
    IF( .NOT. read_G_clin()) RETURN

    ! if present reads the embeded frame 
    IF( G_clin(30:39) .EQ. 'localframe') THEN

       IF ( .NOT. read_G_clin()) THEN
          call faterr(IAM,'error reading localframe::alpha')
       END IF

       READ(G_clin(35:48),'(D14.7)') localframe(1,1)
       READ(G_clin(56:69),'(D14.7)') localframe(2,1)
       READ(G_clin(77:90),'(D14.7)') localframe(3,1)

       IF ( .NOT. read_G_clin()) THEN
          call faterr(IAM,'error reading localframe::beta')
       END IF

       READ(G_clin(35:48),'(D14.7)') localframe(1,2)
       READ(G_clin(56:69),'(D14.7)') localframe(2,2)
       READ(G_clin(77:90),'(D14.7)') localframe(3,2)

       IF ( .NOT. read_G_clin()) THEN
          call faterr(IAM,'error reading localframe::gamma')
       END IF

       READ(G_clin(35:48),'(D14.7)') localframe(1,3)
       READ(G_clin(56:69),'(D14.7)') localframe(2,3)
       READ(G_clin(77:90),'(D14.7)') localframe(3,3)

       IF ( .NOT. read_G_clin()) THEN
          call faterr(IAM,'error reading')  
       END IF

       READ(G_clin(35:48),'(D14.7)') shift(1)
       READ(G_clin(56:69),'(D14.7)') shift(2)
       READ(G_clin(77:90),'(D14.7)') shift(3)
    ELSE
       BACKSPACE(G_nfich)
    END IF

  END SUBROUTINE read_BDARY_PLANx
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_PLANx(nfich,itacty,tacID,color,DATA,localframe,shift)
    
    IMPLICIT NONE
    INTEGER                           :: nfich,itacty
    CHARACTER(len=5)                  :: tacID,color
    REAL(kind=8),DIMENSION(:),POINTER :: DATA
    REAL(kind=8),DIMENSION(3)         :: shift
    REAL(kind=8),DIMENSION(3,3)       :: localframe

    WRITE(nfich,104) tacID,itacty,'color',color,'axe1=',DATA(1),'axe2=',DATA(2),'axe3=',DATA(3)
    WRITE(nfich,'(29X,10A)') 'localframe'
    WRITE(nfich,131) 'alp1=',localframe(1,1),'alp2=',localframe(2,1),'alp3=',localframe(3,1)
    WRITE(nfich,131) 'bet1=',localframe(1,2),'bet2=',localframe(2,2),'bet3=',localframe(3,2)
    WRITE(nfich,131) 'gam1=',localframe(1,3),'gam2=',localframe(2,3),'gam3=',localframe(3,3)
    WRITE(nfich,131) 'coo1=',shift(1),'coo2=',shift(2),'coo3=',shift(3)

104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,3(2X,A5,D14.7))
131 FORMAT(27X,3(2X,A5,D14.7))

  END SUBROUTINE write_BDARY_PLANx
!!!------------------------------------------------------------------------


 END MODULE a_BDARY_PLANx
