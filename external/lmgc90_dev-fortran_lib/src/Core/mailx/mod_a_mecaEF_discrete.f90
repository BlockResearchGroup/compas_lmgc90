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

!> zarbi element necessary to add springs, nodal masses, etc
MODULE a_mecaEF_discrete

USE overall
USE utilities
use algebra
USE bulk_behaviour
USE models

private

TYPE T_mecaEF_discrete
   private
   integer :: name
   integer :: N_NODE
   integer :: N_DOF_by_NODE
END TYPE T_mecaEF_discrete

TYPE(T_mecaEF_discrete),DIMENSION(1) :: mecaEF

integer, parameter, private :: i_sprng = 1

public :: get_nb_ele_discrete, &
          init_mecaEF_discrete, &
          get_NAME_mecaEF_discrete, &
          get_N_NODE_mecaEF_discrete, &
          get_N_DOF_by_NODE_mecaEF_discrete, &
          get_bw_mecaEF_discrete, &
          compute_elementary_bulk_discrete, &
          compute_elementary_mass_discrete


CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_discrete()

  IMPLICIT NONE

  get_nb_ele_discrete=SIZE(mecaEF)

END FUNCTION get_nb_ele_discrete

SUBROUTINE init_mecaEF_discrete
  IMPLICIT NONE
                           !1234567890123456789012345678901234
  CHARACTER(len=34) :: IAM='a_mecaEF_discrete::init_mecaEF_discrete'

  mecaEF(1)%name          = i_sprng
  mecaEF(1)%N_NODE        = 2
  mecaEF(1)%N_DOF_by_NODE = nbdime

END SUBROUTINE init_mecaEF_discrete

character(len=5) function get_NAME_mecaEF_discrete(nb)
  implicit none
  integer, intent(in) :: nb

  select case(mecaEF(nb)%name)
  case( i_sprng )
    get_NAME_mecaEF_discrete = 'SPRNG'
  case default
    get_NAME_mecaEF_discrete = 'xxxxx'
  end select
end function get_NAME_mecaEF_discrete

INTEGER FUNCTION get_N_NODE_mecaEF_discrete(nb)
  IMPLICIT NONE
  INTEGER :: nb

  get_N_NODE_mecaEF_discrete=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_NODE_mecaEF_discrete

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF_discrete(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_mecaEF_discrete=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_mecaEF_discrete


INTEGER FUNCTION get_bw_mecaEF_discrete(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,in,jn,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_mecaEF_discrete=0
  DO in=1,mecaEF(nb)%N_NODE-1
    DO jn=in+1,mecaEF(nb)%N_NODE
      tempo = ABS(nodes(in) - nodes(jn))
      IF (tempo .GT. get_bw_mecaEF_discrete) get_bw_mecaEF_discrete=tempo
    ENDDO
  ENDDO

  get_bw_mecaEF_discrete = (get_bw_mecaEF_discrete+1)*mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_mecaEF_discrete

!=========================================================================

SUBROUTINE compute_elementary_bulk_discrete(nb,ppsnb,X,U,ibdyty,iblmty,F,K)

  INTEGER             ,INTENT(IN) :: nb         ! le numero de l'element
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER             ,INTENT(IN) :: ibdyty,iblmty
  REAL(KIND=8)                    :: X(:,:)  ! coordonnees des sommets (nbdime,2)
  REAL(KIND=8)                    :: U(:,:)  ! deplacement des sommets (nbdime,2)
  REAL(KIND=8)                    :: K(:,:)  ! matrice de rigidite elementaire (2*nbdime,2*nbdime)
  REAL(KIND=8)                    :: F(:)    ! forces interieures elementaires (2*nbdime)

  !****
  real(kind=8) :: tmp(nbdime) ,signe(nbdime),norm
  integer :: mdlnb,lawnb
  integer :: i

  !print*,ibdty,iblmty
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  call get_discrete_stiffness(lawnb,tmp)
  !print*,tmp

  K = 0.d0
  F = 0.d0

  !signe(:) = X(:,1) - X(:,2)
  !select case(nbdime)
  !case(2) 
  ! norm = length2(signe)
  !case(3)
  ! norm = length3(signe)
  !end select   

  !print*,norm

  !if (norm == 0.d0) then
  !  signe=1.d0
  !else
  !  signe = signe / norm
  !endif

  do i=1,nbdime
    K(       i,       i) = tmp(i)
    K(nbdime+i,       i) =-tmp(i)
    K(i       ,nbdime+i) =-tmp(i)
    K(nbdime+i,nbdime+i) = tmp(i)
  enddo

  !print*,U(:,1),U(:,2)

  ! attention convention signe bizarres
  do i=1,nbdime
    F(i       ) =  tmp(i)*(U(i,1) - U(i,2))
    F(nbdime+i) = -tmp(i)*(U(i,1) - U(i,2))
  enddo

END SUBROUTINE 

SUBROUTINE compute_elementary_mass_discrete(nb,ppsnb,X,M)

  INTEGER     , INTENT(IN) :: nb         ! le numero de l'element
  integer,dimension(:),intent(in) :: ppsnb
  REAL(KIND=8)             :: X(:,:)     ! position des sommets
  REAL(KIND=8)             :: M(:,:)     ! matrice de masse elementaire

  !***
  real(kind=8) :: tmp(nbdime)
  integer :: mdlnb,lawnb

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  call get_discrete_mass(lawnb,tmp)

  M = 0.
  do i=1,nbdime
    M(       i,       i) = tmp(i)
    M(nbdime+i,nbdime+i) = tmp(i)
  enddo

END SUBROUTINE 

END MODULE a_mecaEF_discrete

