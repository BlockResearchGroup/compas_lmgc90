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
MODULE a_BDARY_CYLND   
                                    
  !!****h* LMGC90.CORE/a_BDARY_CYLND
  !! NAME
  !!  module a_BDARY_CYLND
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****

  USE utilities
  use overall, only : PI_g

  IMPLICIT NONE

  PUBLIC read_BDARY_CYLND,write_BDARY_CYLND

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_CYLND(axes,volume,I1,I2,I3,localframe,shift)
    
    ! axes(1) corresponds to the half height
    ! axes(2) corresponds to the base radius
    !
    ! inertie geometrique du cylindre rayon r hauteur h
    ! vol = pi * r**2 * h
    ! I1,I2 plan        I1=I2= vol *( r**2 / 4 + h**2 / 12)  
    ! I3 axe verticale  I3 = vol * r**2 * 0.5    



    IMPLICIT NONE
    REAL(kind=8)                      :: volume,I1,I2,I3,r2
    REAL(kind=8),DIMENSION(:),POINTER :: axes
    REAL(kind=8),DIMENSION(3,3)       :: localframe
    REAL(kind=8),DIMENSION(3)         :: shift
    character(len=17) :: IAM
          !12345678901234567
    IAM = 'BDARY_CYLND::read'

 
    ALLOCATE(axes(2))
    READ(G_clin(28:69),'(2(7X,D14.7))') axes(1),axes(2)
    
    r2 = axes(2)*axes(2)
    
    volume = PI_g*r2*(2.D0*axes(1))
    I1     = ((0.25d0*r2) + (axes(1)*axes(1)/3.D0))*volume
    I2     = I1
    I3     = 0.5d0*r2*volume
    
    localframe = 0.d0
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
    
  END SUBROUTINE read_BDARY_CYLND
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_CYLND(nfich,itacty,tacID,color,DATA,localframe,shift)

    IMPLICIT NONE
    INTEGER                           :: nfich,itacty
    CHARACTER(len=5)                  :: tacID,color
    REAL(kind=8),DIMENSION(:),POINTER :: DATA
    REAL(kind=8),DIMENSION(3)         :: shift
    REAL(kind=8),DIMENSION(3,3)       :: localframe

    WRITE(nfich,104) tacID,itacty,'color',color,'High=',DATA(1),'byrd=',DATA(2)
    WRITE(nfich,'(29X,10A)') 'localframe'
    WRITE(nfich,131) 'alp1=',localframe(1,1),'alp2=',localframe(2,1),'alp3=',localframe(3,1)
    WRITE(nfich,131) 'bet1=',localframe(1,2),'bet2=',localframe(2,2),'bet3=',localframe(3,2)
    WRITE(nfich,131) 'gam1=',localframe(1,3),'gam2=',localframe(2,3),'gam3=',localframe(3,3)
    WRITE(nfich,131) 'coo1=',shift(1),'coo2=',shift(2),'coo3=',shift(3)
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
131 FORMAT(27X,3(2X,A5,D14.7))

  END SUBROUTINE write_BDARY_CYLND
!!!------------------------------------------------------------------------

END MODULE a_BDARY_CYLND
