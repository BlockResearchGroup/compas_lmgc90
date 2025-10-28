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

!> RigidKinematic contains a set of function and subroutine
!> usefull to manage rigid body motion 
!>@author f dubois
MODULE RigidKinematic
 
  use utilities
  use algebra
  
  implicit none
  private

  public update_inertia_frame33, &
         update_inertia_frame22

  contains

!!!-----------------------------------------------------------------------------------------!
  !> knowing frame,velocity,time step compute the new frame
  SUBROUTINE update_inertia_frame33(method,H,V,oldframe,newframe,prediction)

    IMPLICIT NONE
    real(kind=8)                :: H        !< time step
    INTEGER                     :: method   !< kind of method: 1=linearized, 2=HughesWinget

    REAL(kind=8),DIMENSION(3)   :: V        !< spin
    REAL(kind=8),DIMENSION(3,3) :: oldframe !< known frame
    REAL(kind=8),DIMENSION(3,3) :: newframe !< new frame

    !> an optional parameter to compute an interpolated solution 
    real(kind=8),optional       :: prediction

    ! ***
    REAL(kind=8),DIMENSION(3)   :: alphai,betai,gammai
    REAL(kind=8),DIMENSION(3)   :: alphaf,betaf,gammaf
    REAL(kind=8),DIMENSION(3)   :: Wf

    REAL(kind=8),DIMENSION(3,3) :: Omega,temp,Q
    INTEGER                     :: err

                                       !12345678901234567890123456789012345678901234
    CHARACTER(len=46)           :: IAM='mod_RigidKinematic::TT_compute_inertia_frame33'

    alphai(1:3) = oldframe(1:3,1)
    betai(1:3)  = oldframe(1:3,2)
    gammai(1:3) = oldframe(1:3,3)

!
! Vbegin est la vitesse de rotation exprimee dans le repere principal d'inertie
! on la calcule dans le repere absolu
!

    Wf(1:3)= V(1)*alphai(1:3) + &     ! vitesse rotation
             V(2)*betai(1:3)  + &
             V(3)*gammai(1:3) 

    SELECT CASE (method)
    case(1)

       Wf(1:3) = H*Wf

       ! Alphaf=Alphai+h*Wf^Alphai
       
       alphaf = alphai + cross_product(Wf,alphai)
       
       ! Betaf=Betai+h*Wf^Betai
       
       betaf = betai + cross_product(Wf,betai)
       
       ! gammaf=gammai+h*Wf^gammai
       
       gammaf = gammai + cross_product(Wf,gammai)                   
       
       CALL GRAMM_SCHMIDT(alphaf,betaf,gammaf)
       
    case(2)

       ! matrice de rotation dans le repere absolu

       Omega(1,1) =  0.d0 ; Omega(1,2) = -Wf(3); Omega(1,3) =  Wf(2) 
       Omega(2,1) =  Wf(3); Omega(2,2) =  0.d0 ; Omega(2,3) = -Wf(1)
       Omega(3,1) = -Wf(2); Omega(3,2) =  Wf(1); Omega(3,3) =  0.d0 

       temp = Id33 - ((0.5 * H) * Omega)

       call inverse33(temp,err)

       if (err == 1) then
         print*,temp(1,:)
         print*,temp(2,:)
         print*,temp(3,:)
         call faterr(IAM,'Non invertible matrix')
       endif

       Q = temp

       temp = Id33 + ((0.5 * H) * Omega)

       ! delta R
       Q = MATMUL(Q,temp)

       ! pour calculer coorTT
       if (present(prediction)) Q = 0.5*(Q+Id33)

       alphaf = MATMUL(Q,alphai)
       betaf  = MATMUL(Q,betai)
       gammaf = MATMUL(Q,gammai)

    case default
      call faterr(IAM,'Unknown method')
    END select
    
    newframe(1:3,1) = alphaf(1:3)
    newframe(1:3,2) = betaf(1:3)
    newframe(1:3,3) = gammaf(1:3)

  END SUBROUTINE update_inertia_frame33
!!!------------------------------------------------------------------------
  !> knowing frame,velocity,time step compute the new frame
  SUBROUTINE update_inertia_frame22(method,H,V,oldframe,newframe)

    IMPLICIT NONE
    real(kind=8)                :: H        !< time step
    INTEGER                     :: method   !< kind of method: not used here

    REAL(kind=8)                :: V        !< spin
    REAL(kind=8),DIMENSION(2,2) :: oldframe !< known frame
    REAL(kind=8),DIMENSION(2,2) :: newframe !< new frame
  
    ! ***
    REAL(kind=8),DIMENSION(2,2) :: temp
    real(kind=8) :: dtt

                                       !12345678901234567890123456789012345678901234
    CHARACTER(len=46)           :: IAM='mod_RigidKinematic::TT_compute_inertia_frame22'

    if (method /= 0) call logmes(IAM//'::method parameter not used ; 0 is expected')

    ! one computes the rotation matrix due to the increment of angle dtt
    dtt = H*V

    temp(1,1) = cos(dtt); temp(1,2) =-sin(dtt) 
    temp(2,1) = sin(dtt); temp(2,2) = cos(dtt)

    ! one computes the new frame 
    newframe = matmul(temp,oldframe)

  END SUBROUTINE update_inertia_frame22
!!!------------------------------------------------------------------------



END MODULE
