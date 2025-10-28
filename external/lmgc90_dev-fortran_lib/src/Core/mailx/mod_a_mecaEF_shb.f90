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
MODULE a_mecaEF_shb
  !!****h* LMGC90.CORE/a_mecaEF_SHB
  !! NAME
  !!  module a_mecaEF_bar
  !! PURPOSE
  !! Basic computations on shell Finite Elements
  !! ---> Unstable yet !!
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/a_EF
  !!****

USE parameters

USE utilities
USE algebra
USE a_MATRIX

USE a_EF

USE bulk_behaviour

USE models

USE ExternalModelsHandler

USE MAILx

USE user

IMPLICIT NONE

TYPE T_mecaEF_SHB
   integer  :: name
   integer  :: N_NODE
   integer  :: N_DOF_by_NODE
   integer  :: T_FONC_FORME
   integer  :: N_GP_RIG_THICKNESS
   integer  :: N_GP_RIG_SURFACE
   integer  :: SCH_GAUSS_RIG
   type(T_PT_GAUSS), dimension(:), pointer :: PG  => null()
   type(T_PT_GAUSS), dimension(:), pointer :: hPG => null()
   real(KIND=8)                            :: Coef_C
   real(KIND=8)                            :: Coef
   real(KIND=8)                            :: Coef_INT
   integer                                 :: N_GP_MAS
   integer                                 :: SCH_GAUSS_MAS
   type(T_PT_GAUSS), dimension(:), pointer :: mPG => null()
   logical                                 :: with_stab = .FALSE.
   
   INTEGER                 :: N_CONT_GP_RIG
   INTEGER                 :: N_TETA_PG
   
   !fd 24/03/11 mapping matrices gp -> node (noeuds sommets)
   real(kind=8), dimension(:,:), pointer :: gp2node   => null()
   !fd 24/03/11 mapping matrices node -> edge (noeuds cote)
   real(kind=8), dimension(:,:), pointer :: node2edge => null()

   !fd 06/02/13 ajouts vecteurs et matrices de travail
   real(kind=8), dimension(:)  , pointer  :: coor_ele     => null()
   real(kind=8), dimension(:)  , pointer  :: primal_ele   => null()
   real(kind=8), dimension(:)  , pointer  :: dual_ele     => null()
   real(kind=8), dimension(:,:), pointer  :: operator_ele => null()
   
   ! DA : Ajout de l'operateur gradient sous integre Hallquist
   real(kind=8), dimension(:,:), pointer :: RLoc => null()
   real(kind=8), dimension(:,:), pointer ::  H   => null()

END TYPE T_mecaEF_SHB 

TYPE(T_mecaEF_SHB),DIMENSION(1),PRIVATE :: mecaEF

integer, parameter, private :: i_shb6x = 1
integer, parameter, private :: i_shb8x = 2
integer, parameter, private :: i_shb15 = 3
integer, parameter, private :: i_shb20 = 4

PUBLIC get_N_GP_RIG_mecaEF_SHB , get_SCH_GAUSS_RIG_mecaEF_SHB, & 
       get_N_GP_MAS_mecaEF_SHB , get_SCH_GAUSS_MAS_mecaEF_SHB

PUBLIC get_T_FONC_FORME_mecaEF_SHB , &
       get_gp_ptr_mecaEF_SHB, &
       get_ele_ptr_mecaEF_SHB

CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_SHB(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_SHB=SIZE(mecaEF)

END FUNCTION get_nb_ele_SHB

SUBROUTINE init_mecaEF_SHB
  IMPLICIT NONE
  logical :: is_initialize = .false.
  INTEGER :: itempo,i,j,errare
  REAL(kind=long),POINTER :: CG(:,:),POIDS_ELE(:)
  REAL(KIND=LONG)         :: SIG_EPS_Coef
!                           123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF_shb::init_mecaEF_SHB'
!
  if( is_initialize ) return
  
  NULLIFY(CG,POIDS_ELE)
!
! EF 3D SURF  
!
!
! SHB6x
!
  mecaEF(1)%NAME          = i_shb6x
  mecaEF(1)%with_stab     = .FALSE.
  mecaEF(1)%N_NODE        = 6
  mecaEF(1)%N_DOF_by_NODE = 3
  mecaEF(1)%T_FONC_FORME  = i_PRP1
  mecaEF(1)%N_GP_RIG_THICKNESS = 5 
  mecaEF(1)%N_GP_RIG_SURFACE   = 1
  mecaEF(1)%Coef_C        = 0.45D0
  mecaEF(1)%Coef          = 2.0D0
  mecaEF(1)%Coef_INT      = 0.5D0
  mecaEF(1)%N_CONT_GP_RIG = 0 
  mecaEF(1)%SCH_GAUSS_RIG = i_PR15
  mecaEF(1)%N_TETA_PG     = 0      
  mecaEF(1)%N_GP_MAS      = 6
  mecaEF(1)%SCH_GAUSS_MAS = i_PR06
  allocate(mecaEF(1)%H(2,6))

  ! DA : H1, H2
  mecaEF(1)%H(1,1) = 0.0D0; mecaEF(1)%H(1,2) =-1.0D0; mecaEF(1)%H(1,3) = 0.0D0; 
  mecaEF(1)%H(1,4) = 0.0D0; mecaEF(1)%H(1,5) = 1.0D0; mecaEF(1)%H(1,6) = 0.0D0; 
  
  mecaEF(1)%H(2,1) = 0.0D0; mecaEF(1)%H(2,2) = 0.0D0; mecaEF(1)%H(2,3) =-1.0D0; 
  mecaEF(1)%H(2,4) = 0.0D0; mecaEF(1)%H(2,5) = 0.0D0; mecaEF(1)%H(2,6) = 1.0D0;

!!! !
!!! ! SHB15
!!! !
!!!   mecaEF(2)%NAME          ='SHB15'
!!!   mecaEF(2)%with_stab     = .FALSE.
!!!   mecaEF(2)%N_NODE        = 15
!!!   mecaEF(2)%N_DOF_by_NODE = 3
!!!   mecaEF(2)%T_FONC_FORME  = i_PRP2
!!!   mecaEF(2)%N_GP_RIG_THICKNESS = 5 
!!!   mecaEF(2)%N_GP_RIG_SURFACE   = 3
!!!   mecaEF(2)%Coef_C        = 1.0D0
!!!   mecaEF(2)%Coef          = 1.0D0
!!!   mecaEF(2)%Coef_INT      = 1.0D0
!!!   mecaEF(2)%N_CONT_GP_RIG = 0 
!!!   mecaEF(2)%SCH_GAUSS_RIG = i_PR35
!!!   mecaEF(2)%N_TETA_PG     = 0      
!!!   mecaEF(2)%N_GP_MAS      = 6
!!!   mecaEF(2)%SCH_GAUSS_MAS = i_PR06
!!!   allocate(mecaEF(2)%H(11,15))
!!!   
!!!   ! DA : H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11
!!!   mecaEF(2)%H(1,1) = 0.0D0; mecaEF(2)%H(1,2) =-0.5D0; mecaEF(2)%H(1,3) =-1.0D0; mecaEF(2)%H(1,4) =-0.5D0; mecaEF(2)%H(1,5) = 0.0D0;
!!!   mecaEF(2)%H(1,6) = 0.0D0; mecaEF(2)%H(1,7) = 0.0D0; mecaEF(2)%H(1,8) = 0.0D0; mecaEF(2)%H(1,9) = 0.0D0; mecaEF(2)%H(1,10)= 0.0D0;
!!!   mecaEF(2)%H(1,11)= 0.5D0; mecaEF(2)%H(1,12)= 1.0D0; mecaEF(2)%H(1,13)= 0.5D0; mecaEF(2)%H(1,14)= 0.0D0; mecaEF(2)%H(1,15)= 0.0D0
!!! 
!!!   mecaEF(2)%H(2,1) = 0.0D0; mecaEF(2)%H(2,2) = 0.0D0; mecaEF(2)%H(2,3) = 0.0D0; mecaEF(2)%H(2,4) =-0.5D0; mecaEF(2)%H(2,5) =-1.0D0;
!!!   mecaEF(2)%H(2,6) =-0.5D0; mecaEF(2)%H(2,7) = 0.0D0; mecaEF(2)%H(2,8) = 0.0D0; mecaEF(2)%H(2,9) = 0.0D0; mecaEF(2)%H(2,10)= 0.0D0;
!!!   mecaEF(2)%H(2,11)= 0.0D0; mecaEF(2)%H(2,12)= 0.0D0; mecaEF(2)%H(2,13)= 0.5D0; mecaEF(2)%H(2,14)= 1.0D0; mecaEF(2)%H(2,15)= 0.5D0
!!! 
!!!   mecaEF(2)%H(3,1) = 0.0D0; mecaEF(2)%H(3,2) = 0.0D0; mecaEF(2)%H(3,3) = 0.0D0; mecaEF(2)%H(3,4) =0.25D0; mecaEF(2)%H(3,5) = 0.0D0;
!!!   mecaEF(2)%H(3,6) = 0.0D0; mecaEF(2)%H(3,7) = 0.0D0; mecaEF(2)%H(3,8) = 0.0D0; mecaEF(2)%H(3,9) = 0.0D0; mecaEF(2)%H(3,10)= 0.0D0;
!!!   mecaEF(2)%H(3,11)= 0.0D0; mecaEF(2)%H(3,12)= 0.0D0; mecaEF(2)%H(3,13)=0.25D0; mecaEF(2)%H(3,14)= 0.0D0; mecaEF(2)%H(3,15)= 0.0D0
!!! 
!!!   mecaEF(2)%H(4,1) = 0.0D0; mecaEF(2)%H(4,2) = 0.0D0; mecaEF(2)%H(4,3) = 0.0D0; mecaEF(2)%H(4,4)=-0.25D0; mecaEF(2)%H(4,5) = 0.0D0;
!!!   mecaEF(2)%H(4,6) = 0.0D0; mecaEF(2)%H(4,7) = 0.0D0; mecaEF(2)%H(4,8) = 0.0D0; mecaEF(2)%H(4,9) = 0.0D0; mecaEF(2)%H(4,10)= 0.0D0;
!!!   mecaEF(2)%H(4,11)= 0.0D0; mecaEF(2)%H(4,12)= 0.0D0; mecaEF(2)%H(4,13)=0.25D0; mecaEF(2)%H(4,14)= 0.0D0; mecaEF(2)%H(4,15)= 0.0D0
!!! 
!!!   mecaEF(2)%H(5,1) = 0.0D0; mecaEF(2)%H(5,2) =0.25D0; mecaEF(2)%H(5,3) = 1.0D0; mecaEF(2)%H(5,4) =0.25D0; mecaEF(2)%H(5,5) = 0.0D0;
!!!   mecaEF(2)%H(5,6) = 0.0D0; mecaEF(2)%H(5,7) = 0.0D0; mecaEF(2)%H(5,8) = 1.0D0; mecaEF(2)%H(5,9) = 0.0D0; mecaEF(2)%H(5,10)= 0.0D0;
!!!   mecaEF(2)%H(5,11)=0.25D0; mecaEF(2)%H(5,12)= 1.0D0; mecaEF(2)%H(5,13)=0.25D0; mecaEF(2)%H(5,14)= 0.0D0; mecaEF(2)%H(5,15)= 0.0D0
!!! 
!!!   mecaEF(2)%H(6,1) = 0.0D0; mecaEF(2)%H(6,2) = 0.0D0; mecaEF(2)%H(6,3) = 0.0D0; mecaEF(2)%H(6,4) =0.25D0; mecaEF(2)%H(6,5) = 1.0D0;
!!!   mecaEF(2)%H(6,6) =0.25D0; mecaEF(2)%H(6,7) = 0.0D0; mecaEF(2)%H(6,8) = 0.0D0; mecaEF(2)%H(6,9) = 1.0D0; mecaEF(2)%H(6,10)= 0.0D0;
!!!   mecaEF(2)%H(6,11)= 0.0D0; mecaEF(2)%H(6,12)= 0.0D0; mecaEF(2)%H(6,13)=0.25D0; mecaEF(2)%H(6,14)= 1.0D0; mecaEF(2)%H(6,15)=0.25D0
!!! 
!!!   mecaEF(2)%H(7,1) = 1.0D0; mecaEF(2)%H(7,2) = 1.0D0; mecaEF(2)%H(7,3) = 1.0D0; mecaEF(2)%H(7,4) = 1.0D0; mecaEF(2)%H(7,5) = 1.0D0;
!!!   mecaEF(2)%H(7,6) = 1.0D0; mecaEF(2)%H(7,7) = 0.0D0; mecaEF(2)%H(7,8) = 0.0D0; mecaEF(2)%H(7,9) = 0.0D0; mecaEF(2)%H(7,10)= 1.0D0;
!!!   mecaEF(2)%H(7,11)= 1.0D0; mecaEF(2)%H(7,12)= 1.0D0; mecaEF(2)%H(7,13)= 1.0D0; mecaEF(2)%H(7,14)= 1.0D0; mecaEF(2)%H(7,15)= 1.0D0
!!! 
!!!   mecaEF(2)%H(8,1) = 0.0D0; mecaEF(2)%H(8,2)=-0.25D0; mecaEF(2)%H(8,3) =-1.0D0; mecaEF(2)%H(8,4)=-0.25D0; mecaEF(2)%H(8,5) = 0.0D0;
!!!   mecaEF(2)%H(8,6) = 0.0D0; mecaEF(2)%H(8,7) = 0.0D0; mecaEF(2)%H(8,8) = 0.0D0; mecaEF(2)%H(8,9) = 0.0D0; mecaEF(2)%H(8,10)= 0.0D0;
!!!   mecaEF(2)%H(8,11)=0.25D0; mecaEF(2)%H(8,12)= 1.0D0; mecaEF(2)%H(8,13)=0.25D0; mecaEF(2)%H(8,14)= 0.0D0; mecaEF(2)%H(8,15)= 0.0D0
!!! 
!!!   mecaEF(2)%H(9,1) = 0.0D0; mecaEF(2)%H(9,2) = 0.0D0; mecaEF(2)%H(9,3) = 0.0D0; mecaEF(2)%H(9,4)=-0.25D0; mecaEF(2)%H(9,5) =-1.0D0;
!!!   mecaEF(2)%H(9,6)=-0.25D0; mecaEF(2)%H(9,7) = 0.0D0; mecaEF(2)%H(9,8) = 0.0D0; mecaEF(2)%H(9,9) = 0.0D0; mecaEF(2)%H(9,10)= 0.0D0;
!!!   mecaEF(2)%H(9,11)= 0.0D0; mecaEF(2)%H(9,12)= 0.0D0; mecaEF(2)%H(9,13)=0.25D0; mecaEF(2)%H(9,14)= 1.0D0; mecaEF(2)%H(9,15)=0.25D0
!!! 
!!!   mecaEF(2)%H(10,1) = 0.0D0; mecaEF(2)%H(10,2) = 0.5D0; mecaEF(2)%H(10,3) = 1.0D0; mecaEF(2)%H(10,4) = 0.5D0; mecaEF(2)%H(10,5) = 0.0D0;
!!!   mecaEF(2)%H(10,6) = 0.0D0; mecaEF(2)%H(10,7) = 0.0D0; mecaEF(2)%H(10,8) = 0.0D0; mecaEF(2)%H(10,9) = 0.0D0; mecaEF(2)%H(10,10)= 0.0D0;
!!!   mecaEF(2)%H(10,11)= 0.5D0; mecaEF(2)%H(10,12)= 1.0D0; mecaEF(2)%H(10,13)= 0.5D0; mecaEF(2)%H(10,14)= 0.0D0; mecaEF(2)%H(10,15)= 0.0D0
!!! 
!!!   mecaEF(2)%H(11,1) = 0.0D0; mecaEF(2)%H(11,2) = 0.0D0; mecaEF(2)%H(11,3) = 0.0D0; mecaEF(2)%H(11,4) = 0.5D0; mecaEF(2)%H(11,5) = 1.0D0;
!!!   mecaEF(2)%H(11,6) = 0.5D0; mecaEF(2)%H(11,7) = 0.0D0; mecaEF(2)%H(11,8) = 0.0D0; mecaEF(2)%H(11,9) = 0.0D0; mecaEF(2)%H(11,10)= 0.0D0;
!!!   mecaEF(2)%H(11,11)= 0.0D0; mecaEF(2)%H(11,12)= 0.0D0; mecaEF(2)%H(11,13)= 0.5D0; mecaEF(2)%H(11,14)= 1.0D0; mecaEF(2)%H(11,15)= 0.5D0
!!! 
!!! 
!!! !
!!! ! SHB8
!!! !
!!!   mecaEF(3)%NAME          ='SHB8x'
!!!   mecaEF(3)%with_stab     = .TRUE.
!!!   mecaEF(3)%N_NODE        = 8
!!!   mecaEF(3)%N_DOF_by_NODE = 3
!!!   mecaEF(3)%T_FONC_FORME  = i_H_P1
!!!   mecaEF(3)%N_GP_RIG_THICKNESS = 5 
!!!   mecaEF(3)%N_GP_RIG_SURFACE   = 1 
!!!   mecaEF(3)%Coef_C        = SQRT(0.1D0)
!!!   mecaEF(3)%Coef          = 8.0D0
!!!   mecaEF(3)%Coef_INT      = 4.0D0
!!!   mecaEF(3)%N_CONT_GP_RIG = 0 
!!!   mecaEF(3)%SCH_GAUSS_RIG = i_H115
!!!   mecaEF(3)%N_TETA_PG     = 0      
!!!   mecaEF(3)%N_GP_MAS      = 8
!!!   mecaEF(3)%SCH_GAUSS_MAS = i_H222
!!!   allocate(mecaEF(3)%H(4,8))
!!!   ! DA : H1, H2, H3, H4
!!!   mecaEF(3)%H(1,1) = 1.0D0; mecaEF(3)%H(1,2) = 1.0D0; mecaEF(3)%H(1,3) =-1.0D0; mecaEF(3)%H(1,4) =-1.0D0
!!!   mecaEF(3)%H(1,5) =-1.0D0; mecaEF(3)%H(1,6) =-1.0D0; mecaEF(3)%H(1,7) = 1.0D0; mecaEF(3)%H(1,8) = 1.0D0
!!!   
!!!   mecaEF(3)%H(2,1) = 1.0D0; mecaEF(3)%H(2,2) =-1.0D0; mecaEF(3)%H(2,3) =-1.0D0; mecaEF(3)%H(2,4) = 1.0D0
!!!   mecaEF(3)%H(2,5) =-1.0D0; mecaEF(3)%H(2,6) = 1.0D0; mecaEF(3)%H(2,7) = 1.0D0; mecaEF(3)%H(2,8) =-1.0D0
!!!   
!!!   mecaEF(3)%H(3,1) = 1.0D0; mecaEF(3)%H(3,2) =-1.0D0; mecaEF(3)%H(3,3) = 1.0D0; mecaEF(3)%H(3,4) =-1.0D0
!!!   mecaEF(3)%H(3,5) = 1.0D0; mecaEF(3)%H(3,6) =-1.0D0; mecaEF(3)%H(3,7) = 1.0D0; mecaEF(3)%H(3,8) =-1.0D0
!!!   
!!!   mecaEF(3)%H(4,1) =-1.0D0; mecaEF(3)%H(4,2) = 1.0D0; mecaEF(3)%H(4,3) =-1.0D0; mecaEF(3)%H(4,4) = 1.0D0
!!!   mecaEF(3)%H(4,5) = 1.0D0; mecaEF(3)%H(4,6) =-1.0D0; mecaEF(3)%H(4,7) = 1.0D0; mecaEF(3)%H(4,8) =-1.0D0
!!!  
!!! !
!!! ! SHB20
!!! !
!!!   mecaEF(4)%NAME          ='SHB20'
!!!   mecaEF(4)%with_stab     = .FALSE.
!!!   mecaEF(4)%N_NODE        = 20
!!!   mecaEF(4)%N_DOF_by_NODE = 3
!!!   mecaEF(4)%T_FONC_FORME  = i_H_P2
!!!   mecaEF(4)%N_GP_RIG_THICKNESS = 5 
!!!   mecaEF(4)%N_GP_RIG_SURFACE   = 4
!!!   mecaEF(4)%Coef_C        = 1.0D0
!!!   mecaEF(4)%Coef          = 1.0D0
!!!   mecaEF(4)%Coef_INT      = 1.0D0
!!!   mecaEF(4)%N_CONT_GP_RIG = 0 
!!!   mecaEF(4)%SCH_GAUSS_RIG = i_H225
!!!   mecaEF(4)%N_TETA_PG     = 0      
!!!   mecaEF(4)%N_GP_MAS      = 27
!!!   mecaEF(4)%SCH_GAUSS_MAS = i_H333
!!!   allocate(mecaEF(4)%H(16,20))
!!!   mecaEF(4)%H(16,20) = 0.D0
!!!   ! DA : H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16
!!!   mecaEF(4)%H(1,1) = 1.0D0; mecaEF(4)%H(1,2) =-1.0D0; mecaEF(4)%H(1,3) =-1.0D0; mecaEF(4)%H(1,4) = 1.0D0;
!!!   mecaEF(4)%H(1,5) =-1.0D0; mecaEF(4)%H(1,6) = 1.0D0; mecaEF(4)%H(1,7) = 1.0D0; mecaEF(4)%H(1,8) =-1.0D0;
!!!   mecaEF(4)%H(1,9) = 0.0D0; mecaEF(4)%H(1,10)=-1.0D0; mecaEF(4)%H(1,11)= 0.0D0; mecaEF(4)%H(1,12)= 1.0D0;
!!!   mecaEF(4)%H(1,13)= 0.0D0; mecaEF(4)%H(1,14)= 0.0D0; mecaEF(4)%H(1,15)= 0.0D0; mecaEF(4)%H(1,16)= 0.0D0;
!!!   mecaEF(4)%H(1,17)= 0.0D0; mecaEF(4)%H(1,18)= 1.0D0; mecaEF(4)%H(1,19)= 0.0D0; mecaEF(4)%H(1,20)=-1.0D0;
!!! 
!!!   mecaEF(4)%H(2,1) = 1.0D0; mecaEF(4)%H(2,2) = 1.0D0; mecaEF(4)%H(2,3) =-1.0D0; mecaEF(4)%H(2,4) =-1.0D0;
!!!   mecaEF(4)%H(2,5) =-1.0D0; mecaEF(4)%H(2,6) =-1.0D0; mecaEF(4)%H(2,7) = 1.0D0; mecaEF(4)%H(2,8) = 1.0D0;
!!!   mecaEF(4)%H(2,9) = 1.0D0; mecaEF(4)%H(2,10)= 0.0D0; mecaEF(4)%H(2,11)=-1.0D0; mecaEF(4)%H(2,12)= 0.0D0;
!!!   mecaEF(4)%H(2,13)= 0.0D0; mecaEF(4)%H(2,14)= 0.0D0; mecaEF(4)%H(2,15)= 0.0D0; mecaEF(4)%H(2,16)= 0.0D0;
!!!   mecaEF(4)%H(2,17)=-1.0D0; mecaEF(4)%H(2,18)= 0.0D0; mecaEF(4)%H(2,19)= 1.0D0; mecaEF(4)%H(2,20)= 0.0D0;


  DO i=1,SIZE(mecaEF)
   
   ! concerning behaviour
    ALLOCATE(mecaEF(i)%PG(get_N_GP_RIG_mecaEF_SHB(i) + 1),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating meca_ef%PG')
    END IF
    
    ALLOCATE(mecaEF(i)%hPG(get_N_GP_RIG_mecaEF_SHB(i) + 1),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating meca_ef%hPG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_RIG_mecaEF_SHB(i),CG,POIDS_ELE)
    
    DO j=1,get_N_GP_RIG_mecaEF_SHB(i) + 1
      mecaEF(i)%PG(j)%POIDS=POIDS_ELE(j)
      mecaEF(i)%PG(j)%ZETA =CG(3,j)
      
      NULLIFY(mecaEF(i)%PG(j)%N)
      CALL fonct_forme(get_T_FONC_FORME_mecaEF_SHB(i),CG(:,j),mecaEF(i)%PG(j)%N)

      NULLIFY(mecaEF(i)%PG(j)%DN)
      CALL derive_forme(get_T_FONC_FORME_mecaEF_SHB(i),CG(:,j),mecaEF(i)%PG(j)%DN)
      
      NULLIFY(mecaEF(i)%hPG(j)%N)
      CALL fonct_forme_shb(get_T_FONC_FORME_mecaEF_SHB(i),CG(:,j),mecaEF(i)%hPG(j)%N)
      
      NULLIFY(mecaEF(i)%hPG(j)%DN)
      CALL derive_forme_shb(get_T_FONC_FORME_mecaEF_SHB(i),CG(:,j),mecaEF(i)%hPG(j)%DN)
      
    ENDDO

    ALLOCATE(mecaEF(i)%mPG(get_N_GP_MAS_mecaEF_SHB(i)),stat=errare)

    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating meca_ef%mPG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_MAS_mecaEF_SHB(i),CG,POIDS_ELE)

    DO j=1,get_N_GP_MAS_mecaEF_SHB(i)
      mecaEF(i)%mPG(j)%POIDS=POIDS_ELE(j)   

      NULLIFY(mecaEF(i)%mPG(j)%N)
      CALL fonct_forme(get_T_FONC_FORME_mecaEF_SHB(i),CG(:,j),mecaEF(i)%mPG(j)%N)

      NULLIFY(mecaEF(i)%mPG(j)%DN)
      CALL derive_forme(get_T_FONC_FORME_mecaEF_SHB(i),CG(:,j),mecaEF(i)%mPG(j)%DN)

    ENDDO
    ! concerning mapping gp -> node

    SELECT CASE(mecaEF(i)%NAME)
     CASE( i_shb6x, i_shb8x )     !les lineaires
       allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,get_N_GP_RIG_mecaEF_SHB(i)))
       if (mecaEF(i)%N_NODE == get_N_GP_RIG_mecaEF_SHB(i)) then
         mecaEF(i)%gp2node = 0.d0
         call compute_gp2node(mecaEF(i)%T_FONC_FORME,mecaEF(i)%N_NODE, &
                              mecaEF(i)%SCH_GAUSS_RIG,get_N_GP_RIG_mecaEF_SHB(i), &
                              mecaEF(i)%gp2node)
       else
         if (get_N_GP_RIG_mecaEF_SHB(i) == 5) then
            mecaEF(i)%gp2node = 0.D0
            ! DA : Les deformations et contraintes sont constantes sur la peau superieur et inferieure
            SIG_EPS_Coef = 1.0D0 / mecaEF(i)%PG(3)%ZETA
            if (mecaEF(i)%NAME == i_shb6x) then
               mecaEF(i)%gp2node(1,3) = SIG_EPS_Coef; mecaEF(i)%gp2node(2,3) = SIG_EPS_Coef; mecaEF(i)%gp2node(3,3) = SIG_EPS_Coef 
               mecaEF(i)%gp2node(4,5) = SIG_EPS_Coef; mecaEF(i)%gp2node(5,5) = SIG_EPS_Coef; mecaEF(i)%gp2node(6,5) = SIG_EPS_Coef
            endif
            if (mecaEF(i)%NAME == i_shb8x) then 
               mecaEF(i)%gp2node(1,3) = SIG_EPS_Coef; mecaEF(i)%gp2node(2,3) = SIG_EPS_Coef; mecaEF(i)%gp2node(3,3) = SIG_EPS_Coef;
               mecaEF(i)%gp2node(4,3) = SIG_EPS_Coef 
               mecaEF(i)%gp2node(5,5) = SIG_EPS_Coef; mecaEF(i)%gp2node(6,5) = SIG_EPS_Coef; mecaEF(i)%gp2node(7,5) = SIG_EPS_Coef;
               mecaEF(i)%gp2node(8,5) = SIG_EPS_Coef
            endif
         else
            call faterr(IAM,'Impossible')
         endif
       endif
       nullify(mecaEF(i)%node2edge)
       
    CASE( i_shb20 )
       allocate(mecaEF(i)%gp2node(8,get_N_GP_RIG_mecaEF_SHB(i)))
       call compute_gp2node(i_H_P1, 8, &
                            mecaEF(i)%SCH_GAUSS_RIG,get_N_GP_RIG_mecaEF_SHB(i), &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-8,8))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,4)=0.5d0
       mecaEF(i)%node2edge(4,4)=0.5d0;mecaEF(i)%node2edge(4,1)=0.5d0
       mecaEF(i)%node2edge(5,5)=0.5d0;mecaEF(i)%node2edge(5,6)=0.5d0
       mecaEF(i)%node2edge(6,6)=0.5d0;mecaEF(i)%node2edge(6,7)=0.5d0
       mecaEF(i)%node2edge(7,7)=0.5d0;mecaEF(i)%node2edge(7,8)=0.5d0
       mecaEF(i)%node2edge(8,8)=0.5d0;mecaEF(i)%node2edge(8,5)=0.5d0
       mecaEF(i)%node2edge(9,1)=0.5d0;mecaEF(i)%node2edge(9,5)=0.5d0
       mecaEF(i)%node2edge(10,2)=0.5d0;mecaEF(i)%node2edge(10,6)=0.5d0
       mecaEF(i)%node2edge(11,3)=0.5d0;mecaEF(i)%node2edge(11,7)=0.5d0
       mecaEF(i)%node2edge(12,4)=0.5d0;mecaEF(i)%node2edge(12,8)=0.5d0
       
    CASE( i_shb15 )
       allocate(mecaEF(i)%gp2node(6,get_N_GP_RIG_mecaEF_SHB(i)))
       call compute_gp2node(i_PRP1, 6, &
                            mecaEF(i)%SCH_GAUSS_RIG,get_N_GP_RIG_mecaEF_SHB(i), &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-6,6))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,1)=0.5d0
       mecaEF(i)%node2edge(4,4)=0.5d0;mecaEF(i)%node2edge(4,1)=0.5d0
       mecaEF(i)%node2edge(5,5)=0.5d0;mecaEF(i)%node2edge(5,2)=0.5d0
       mecaEF(i)%node2edge(6,6)=0.5d0;mecaEF(i)%node2edge(6,3)=0.5d0
       mecaEF(i)%node2edge(7,4)=0.5d0;mecaEF(i)%node2edge(7,5)=0.5d0
       mecaEF(i)%node2edge(8,6)=0.5d0;mecaEF(i)%node2edge(8,5)=0.5d0
       mecaEF(i)%node2edge(9,6)=0.5d0;mecaEF(i)%node2edge(9,4)=0.5d0
       
    CASE DEFAULT
      print*,i,mecaEF(i)%NAME
      call FATERR(IAM,'gp2node can t be computed for this element')
    END SELECT

    allocate(mecaEF(i)%coor_ele(nbdime*mecaEF(i)%n_node), &
             mecaEF(i)%primal_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node), &
             mecaEF(i)%dual_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node), &
             mecaEF(i)%operator_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node,mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node))
    
  ENDDO 

  is_initialize = .true.

  IF(ASSOCIATED(CG)) DEALLOCATE(CG)
  IF(ASSOCIATED(POIDS_ELE)) DEALLOCATE(POIDS_ELE)

END SUBROUTINE init_mecaEF_SHB

integer(kind=4) FUNCTION get_T_FONC_FORME_mecaEF_SHB(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_T_FONC_FORME_mecaEF_SHB=mecaEF(nb)%T_FONC_FORME

END FUNCTION get_T_FONC_FORME_mecaEF_SHB

INTEGER FUNCTION get_N_GP_MAS_mecaEF_SHB(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_MAS_mecaEF_SHB=mecaEF(nb)%N_GP_MAS

END FUNCTION get_N_GP_MAS_mecaEF_SHB

integer(kind=4) FUNCTION get_SCH_GAUSS_MAS_mecaEF_SHB(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_MAS_mecaEF_SHB=mecaEF(nb)%SCH_GAUSS_MAS

END FUNCTION get_SCH_GAUSS_MAS_mecaEF_SHB

character(len=5) function get_NAME_mecaEF_SHB(nb)
  implicit none
  integer, intent(in) :: nb
  select case(mecaEF(nb)%name)
  case( i_shb6x )
    get_NAME_mecaEF_shb = 'SHB6x'
  case default
    get_NAME_mecaEF_shb = 'xxxxx'
  end select
end function get_NAME_mecaEF_SHB

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF_SHB(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_mecaEF_SHB=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_mecaEF_SHB

INTEGER FUNCTION get_N_NODE_mecaEF_SHB(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_mecaEF_SHB=mecaEF(nb)%N_NODE

END FUNCTION get_N_NODE_mecaEF_SHB


INTEGER FUNCTION get_bw_mecaEF_SHB(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,i,j,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_mecaEF_SHB=0
  DO i=1,mecaEF(nb)%N_NODE-1
    DO j=i+1,mecaEF(nb)%N_NODE
      tempo = ABS(nodes(i) - nodes(j))
      IF (tempo .GT. get_bw_mecaEF_SHB) get_bw_mecaEF_SHB=tempo
    ENDDO
  ENDDO

  get_bw_mecaEF_SHB = (get_bw_mecaEF_SHB+1)*mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_mecaEF_SHB

!==========================================================

!---------------------------------------------------------------------------
! Pour les elements SHELL3D
! CALCUL DES COORD LOCALES XL ET DE LA MATRICES DE ROTATION LOCAL,GLOBAL
! Pour passer les informations nodales    
!---------------------------------------------------------------------------
      
SUBROUTINE COORD_LOC_SHB3D(N_NE,ZETA,X,X_LOC)

 IMPLICIT NONE

 INTEGER     , INTENT(IN)     :: N_NE
 REAL(KIND=8), INTENT(IN)     :: ZETA
 REAL(KIND=8), INTENT(IN)     :: X(:,:)

 REAL(KIND=8), DIMENSION(:,:) :: X_LOC  
! Variables locales
 REAL(KIND=LONG),DIMENSION(3)                 :: Xcenter
 REAL(KIND=LONG),DIMENSION(:,:)  ,ALLOCATABLE :: Xcoq
 INTEGER                      :: Inode

!                           123456789012345678901234567890
 CHARACTER(len=30) :: IAM='a_mecaEF_SHB::COORD_LOC_SHB3D'

SELECT CASE(N_NE)

   CASE(6) ! DA : il s'agit du SHB6
      ALLOCATE(Xcoq(3,3))
      Xcoq = 0.0D0
      Xcenter = ZERO
      
      DO Inode=1,3
         Xcoq(1,Inode) = 0.5D0*(1.D0 - ZETA) * X(1,Inode) + 0.5D0*(1.D0 + ZETA) * X(1,Inode + 3)
         Xcoq(2,Inode) = 0.5D0*(1.D0 - ZETA) * X(2,Inode) + 0.5D0*(1.D0 + ZETA) * X(2,Inode + 3)
         Xcoq(3,Inode) = 0.5D0*(1.D0 - ZETA) * X(3,Inode) + 0.5D0*(1.D0 + ZETA) * X(3,Inode + 3)
      ENDDO
      
      Xcenter(1) = (Xcoq(1,1) + Xcoq(1,2) + Xcoq(1,3))/3.D0
      Xcenter(2) = (Xcoq(2,1) + Xcoq(2,2) + Xcoq(2,3))/3.D0
      Xcenter(3) = (Xcoq(3,1) + Xcoq(3,2) + Xcoq(3,3))/3.D0
      
   CASE(8) ! DA : il s'agit du SHB8
      ALLOCATE(Xcoq(3,4))
      Xcoq = 0.0D0
      Xcenter = ZERO
      
      DO Inode=1,4
         Xcoq(1,Inode) = 0.5D0*(1.D0 - ZETA) * X(1,Inode) + 0.5D0*(1.D0 + ZETA) * X(1,Inode + 4)
         Xcoq(2,Inode) = 0.5D0*(1.D0 - ZETA) * X(2,Inode) + 0.5D0*(1.D0 + ZETA) * X(2,Inode + 4)
         Xcoq(3,Inode) = 0.5D0*(1.D0 - ZETA) * X(3,Inode) + 0.5D0*(1.D0 + ZETA) * X(3,Inode + 4)
      ENDDO
      
      Xcenter(1) = (Xcoq(1,1) + Xcoq(1,2) + Xcoq(1,3) + Xcoq(1,4))/4.D0
      Xcenter(2) = (Xcoq(2,1) + Xcoq(2,2) + Xcoq(2,3) + Xcoq(1,4))/4.D0
      Xcenter(3) = (Xcoq(3,1) + Xcoq(3,2) + Xcoq(3,3) + Xcoq(1,4))/4.D0
   
   CASE(15) ! DA : il s'agit du SHB15
      ALLOCATE(Xcoq(3,6))
      Xcoq = 0.0D0
      Xcenter = ZERO
      
      CALL faterr(IAM,' Coord local de degree not implemented')
   
   CASE(20) ! DA : il s'agit du SHB20
      ALLOCATE(Xcoq(3,8))
      Xcoq = 0.0D0
      Xcenter = ZERO
      
      CALL faterr(IAM,' Coord local de degree not implemented')
      
   CASE DEFAULT
END SELECT

if( allocated(Xcoq) ) deallocate(Xcoq)

END SUBROUTINE COORD_LOC_SHB3D

!==========================================================

!---------------------------------------------------------------------------
! Pour les elements SHELL3D
! CALCUL DES COORD LOCALES XL ET DE LA MATRICES DE ROTATION LOCAL,GLOBAL
! Pour passer les informations nodales    
!---------------------------------------------------------------------------
      
SUBROUTINE MAT_PASSAGE_SHB3D(ibdyty,iblmty,ig,N_NE,DN,X,P_GL,P_LG)

 IMPLICIT NONE

 INTEGER     , INTENT(IN)     :: N_NE
 REAL(KIND=8), INTENT(IN)     :: DN(:,:)
 REAL(KIND=8), INTENT(IN)     :: X(:,:)

 REAL(KIND=8), dimension(:,:), pointer :: P_GL, P_LG !  P_LG : Passage Local > Global, P_GL : Passage Global > Local 
 REAL(KIND=8)                 :: vec_x_x, vec_x_y,vec_x_z
 REAL(KIND=8)                 :: vec_y_x, vec_y_y,vec_y_z
 REAL(KIND=8)                 :: vec_z_x, vec_z_y,vec_z_z
 REAL(KIND=8), DIMENSION(3,3) :: R_GL  
! Variables locales
 INTEGER                      :: ig,ibdyty,iblmty,inull,I,J
 INTEGER                      :: rank
 REAL(KIND=8)                 :: Norm
!                           123456789012345678901234567890
 CHARACTER(len=30) :: IAM='a_mecaEF_SHB::mat_passage_SHB3d'
 LOGICAL                      :: is_vec_field 


! Initialisation a vide des nouveaux pointeurs

IF(ASSOCIATED(P_GL)) THEN ; DEALLOCATE(P_GL) ; NULLIFY(P_GL) ; ENDIF
IF(ASSOCIATED(P_LG)) THEN ; DEALLOCATE(P_LG) ; NULLIFY(P_LG) ; ENDIF

ALLOCATE(P_GL(3*N_NE,3*N_NE))
P_GL=ZERO
ALLOCATE(P_LG(3*N_NE,3*N_NE))
P_LG=ZERO

! DA : Test pour utliser une orientation donnee par l'utilisateur
is_vec_field = .FALSE.

vec_x_x = 0.d0; vec_x_y = 0.d0; vec_x_z = 0.d0
vec_y_x = 0.d0; vec_y_y = 0.d0; vec_y_z = 0.d0
vec_z_x = 0.d0; vec_z_y = 0.d0; vec_z_z = 0.d0

rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
if ( rank > 0 ) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,vec_z_x)

rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
if ( rank > 0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,vec_z_y)

rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
if ( rank > 0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,vec_z_z)

if (sqrt(vec_z_x*vec_z_x + vec_z_y*vec_z_y + vec_z_z*vec_z_z) <= 0.1) then
   is_vec_field = .FALSE.
else
   is_vec_field = .FALSE.
endif

if (is_vec_field) then 
  
   rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
   if ( rank > 0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,vec_x_x)   
   rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
   if ( rank > 0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,vec_x_y)   
   rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
   if ( rank > 0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,vec_x_z)   
   
   vec_y_x = vec_z_y*vec_x_z - vec_z_z*vec_x_y
   vec_y_y = vec_z_z*vec_x_x - vec_z_x*vec_x_z
   vec_y_z = vec_z_x*vec_x_y - vec_z_y*vec_x_x
   
   R_GL(1,:) = (/ vec_x_x , vec_x_y , vec_x_z /)
   R_GL(2,:) = (/ vec_y_x , vec_y_y , vec_y_z /)
   R_GL(3,:) = (/ vec_z_x , vec_z_y , vec_z_z /)

else
   ! Calcul de la matrice Jacobienne, de son determinant, et de son inverse
   R_GL(1,:)=(/DOT_PRODUCT(DN(1,1:3),X(1,1:3)),DOT_PRODUCT(DN(1,:),X(2,:)),DOT_PRODUCT(DN(1,:),X(3,:)) /)
   R_GL(2,:)=(/DOT_PRODUCT(DN(2,:),X(1,:)),DOT_PRODUCT(DN(2,:),X(2,:)),DOT_PRODUCT(DN(2,:),X(3,:)) /)
   R_GL(3,:)=(/DOT_PRODUCT(DN(3,:),X(1,:)),DOT_PRODUCT(DN(3,:),X(2,:)),DOT_PRODUCT(DN(3,:),X(3,:)) /)
   
   ! DA : On utilise la matrice jacobienne pour definir le repere local
   !      v1 = R_GL(1,:)
   !      v2 = R_GL(2,:)
   !      v3 = R_GL(3,:)
   
   ! DA : Calcul du vecteur Z avec le produit vectoriel X vectoriel Y
   
   R_GL(3,1) = R_GL(1,2) * R_GL(2,3) - R_GL(1,3) * R_GL(2,2)
   R_GL(3,2) = R_GL(1,3) * R_GL(2,1) - R_GL(1,1) * R_GL(2,3)
   R_GL(3,3) = R_GL(1,1) * R_GL(2,2) - R_GL(1,2) * R_GL(2,1)
   
   ! DA : Normalisation du vecteur Z
   Norm = dsqrt(dot_product(R_GL(3,:),R_GL(3,:)))
   R_GL(3,:) = R_GL(3,:)/Norm
   
   ! DA : Calcul du vecteur X avec le produit vectoriel Y vectoriel Z
   
   R_GL(1,1) = R_GL(2,2) * R_GL(3,3) - R_GL(2,3) * R_GL(3,2)
   R_GL(1,2) = R_GL(2,3) * R_GL(3,1) - R_GL(2,1) * R_GL(3,3)
   R_GL(1,3) = R_GL(2,1) * R_GL(3,2) - R_GL(2,2) * R_GL(3,1)
   
   ! DA : Normalisation du vecteur X
   Norm = dsqrt(dot_product(R_GL(1,:),R_GL(1,:)))
   R_GL(1,:) = R_GL(1,:)/Norm
   
   ! DA : Calcul du vecteur Y avec le produit vectoriel Z vectoriel X
   
   R_GL(2,1) = R_GL(3,2) * R_GL(1,3) - R_GL(3,3) * R_GL(1,2)
   R_GL(2,2) = R_GL(3,3) * R_GL(1,1) - R_GL(3,1) * R_GL(1,3)
   R_GL(2,3) = R_GL(3,1) * R_GL(1,2) - R_GL(3,2) * R_GL(1,1)
   
   ! DA : Normalisation du vecteur Y
   Norm = dsqrt(dot_product(R_GL(2,:),R_GL(2,:)))
   R_GL(2,:) = R_GL(2,:)/Norm
  
endif

! Creation de la matrice de passage des infos nodales globale au info nodales locale
DO I=1,N_NE
   P_GL((I-1)*3+1:I*3,(I-1)*3+1:I*3) = TRANSPOSE(R_GL)
ENDDO

P_LG = TRANSPOSE(P_GL)

END SUBROUTINE MAT_PASSAGE_SHB3D

!
!============= low level private routines ==================
!

INTEGER FUNCTION get_N_GP_RIG_mecaEF_SHB(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_RIG_mecaEF_SHB=mecaEF(nb)%N_GP_RIG_THICKNESS*mecaEF(nb)%N_GP_RIG_SURFACE

END FUNCTION get_N_GP_RIG_mecaEF_SHB

integer(kind=4) FUNCTION get_SCH_GAUSS_RIG_mecaEF_SHB(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_RIG_mecaEF_SHB=mecaEF(nb)%SCH_GAUSS_RIG

END FUNCTION get_SCH_GAUSS_RIG_mecaEF_SHB

INTEGER FUNCTION get_N_GP_mecaEF_SHB(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_mecaEF_SHB=mecaEF(nb)%N_GP_RIG_THICKNESS * mecaEF(nb)%N_GP_RIG_SURFACE

END FUNCTION get_N_GP_mecaEF_SHB
 
!> get a pointer on working element array
subroutine get_ele_ptr_mecaEF_SHB(id,coor_ele,primal_ele,dual_ele,operator_ele)
  implicit none 
  integer :: id
  real(kind=8), pointer :: coor_ele(:),primal_ele(:),dual_ele(:),operator_ele(:,:)

  coor_ele     => mecaEF(id)%coor_ele
  primal_ele   => mecaEF(id)%primal_ele
  dual_ele     => mecaEF(id)%dual_ele
  operator_ele => mecaEF(id)%operator_ele

end subroutine

!------------------------------------------------------------------------------!
!> get a pointer on a gauss_pt object stored at gp ig of ele type id
function get_gp_ptr_mecaEF_SHB(id,ig)
  implicit none 
  integer :: id,ig
  type (T_pt_gauss), pointer :: get_gp_ptr_mecaEF_SHB

  get_gp_ptr_mecaEF_SHB => mecaEF(id)%PG(ig)

end function

subroutine compute_gp2node(type_interpolation,nbn,type_integration,nbgp,mat)
  implicit none
  integer(kind=4) :: type_interpolation,type_integration
  integer      :: nbn,nbgp
  real(kind=8) :: mat(nbn,nbgp)
  ! ***
  integer      :: i,j,ig
  real(kind=8) :: tmp
  type(T_mat_sym) :: M,M_fact

  ! *** propre a la resolution
  character(len=1) :: isprecon
  real(kind=8) :: scale(nbn)

  ! position points de gauss (dim,nbgp)
  REAL(kind=long),POINTER :: CG(:,:)
  ! poids points de gauss (nbgp) <- pas utilise ici
  REAL(kind=long),POINTER :: poids(:) 
  ! valeurs fonctions de formes aux points de gauss (
  REAL(kind=long),POINTER :: N(:)
  
  nullify(CG,poids)
  CALL pos_gauss(type_integration,CG,POIDS)

  if (size(cg,dim=2)-1 /= nbgp) then
    call FATERR('a_mecaEF_shb::compute_gp2node','nbgp inconsistancy')
  endif

  call new_matrix(M,nbn) 
  call zero_matrix(M) 

  call new_matrix(M_fact,nbn) 
  call zero_matrix(M_fact) 

  do i=1,nbn
    do j=1,nbn
      do ig=1,nbgp 
        NULLIFY(N)
        CALL fonct_forme(type_interpolation,CG(:,ig),N)
        tmp = N(i)*N(j)
        call add_to_matrix(M,tmp,i,j)
        deallocate(N)
      enddo
    end do
  end do

  do ig=1,nbgp 
    NULLIFY(N)
    CALL fonct_forme(type_interpolation,CG(:,ig),N)

    if (ig==1) then
      call x_solve_linear_system(M,N,M_fact,scale,isprecon,'E')
    else
      call x_solve_linear_system(M,N,M_fact,scale,isprecon,'F')
    endif

    mat(:,ig)=N(:)

    deallocate(N)
  end do

  call free_matrix(M)
  call free_matrix(M_fact)

  deallocate(cg,poids)

end subroutine

!------------------------------------------------------------------------------!
!    Calcul de la masse elementaire  [Me]=Sum rho [N]t [N] µi                  !
!------------------------------------------------------------------------------!

SUBROUTINE compute_elementary_mass_SHB(i,ppsnb,X,M)

  IMPLICIT NONE

  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN) :: i 
  integer,dimension(:)        :: ppsnb
  ! coordonnees des sommets
  REAL(KIND=8)                :: X(:,:)     
  ! matrice de rig. elem.
  REAL(KIND=8)                :: M(:,:)     

  REAL(KIND=8)                :: J,vol,xm,rho

  INTEGER                     :: IG,in,mdlnb,lawnb

  ! derivee de N par rapport a X   
  REAL(KIND=8), POINTER       :: DNX(:,:)
  
  REAL(KIND=8), POINTER       :: P(:,:), InvJ(:,:)
  ! les fonctions de formes rangees comme il faut
  REAL(KIND=8),ALLOCATABLE    ::  xn(:,:)  

  INTEGER :: ni,nj,nk,ie,je,ke

  REAL(KIND=8) :: diamas

  ! somme des elements diagonaux de la matrice de masse coherente
  REAL (kind=8) :: sumDiagMass 
  ! masse totale de l'element divisee par la somme des elements diagonaux de la matrice de masse coherente
  REAL (kind=8) :: alpha 

  ! nbDIME est defini dans overall

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  M = 0.d0

  rho= get_RHO(lawnb)

  if (rho == 0.d0) return

  NULLIFY(DNX)
  NULLIFY(P,InvJ)
  ALLOCATE(P(3,3))
  P=ZERO
  P(1,1) = 1.D0
  P(2,2) = 1.D0
  P(3,3) = 1.D0
  
  ALLOCATE(xn(nbDIME,nbDIME*mecaEF(i)%N_NODE))

  xm=0.d0; vol = 0.d0

  DO IG=1,mecaEF(i)%N_GP_MAS     ! Pour tous les points de Gauss

    xn=0.d0
   
    CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%mPG(IG)%DN, &
                        X,DNX)
                         
    CALL JACOBIEN_ISO3D(mecaEF(i)%mPG(ig)%DN, X, J,INVJ)
    vol = vol + J * mecaEF(i)%mPG(ig)%POIDS
    xm  = xm  + J * rho * mecaEF(i)%mPG(ig)%POIDS

    SELECT CASE(nbDIME)
    CASE(2)
      ni=0;nj=0
      DO in=1,mecaEF(i)%N_NODE
        ni=nj+1;nj=ni+1
        xn(1,ni)=mecaEF(i)%mPG(ig)%N(in)
        xn(2,nj)=mecaEF(i)%mPG(ig)%N(in)        
      ENDDO
    CASE(3)
      ni=0;nj=0;nk=0
      DO in=1,mecaEF(i)%N_NODE
        ni=nk+1;nj=ni+1;nk=nj+1
        xn(1,ni)=mecaEF(i)%mPG(ig)%N(in)
        xn(2,nj)=mecaEF(i)%mPG(ig)%N(in)        
        xn(3,nk)=mecaEF(i)%mPG(ig)%N(in)        
      ENDDO
    CASE DEFAULT
    END SELECT

    M=M+(MATMUL(TRANSPOSE(xn),xn)*J*Rho*mecaEF(i)%mPG(ig)%POIDS)            !  ke= Blt.D.Bl.coef

  ENDDO

  ! lumping
  !fd attention avec un Q8 ca donne une masse negative !

  IF (get_eleop_value(mdlnb,'mstrg') == 'lump_' ) THEN

    if (mecaEF(i)%NAME /= i_shb20) then   

      ! raw sum
      DO ie=1,nbDIME*mecaEF(i)%N_NODE
        diamas=0.d0
        DO je=1,nbDIME*mecaEF(i)%N_NODE
          diamas=diamas+M(ie,je)
          IF (ie /= je) M(ie,je)=0.d0
        END DO
        M(ie,ie)=diamas 
      END DO

    else
      ! Autre méthode de lumping: "Special Lumping Technique" - voir bouquin de Hugues
      ! elle donne TOUJOURS des masses lumpees strictement positives!   
 
      ! on calcule la somme des elements diagonaux de la matrice de masse coherente
      sumDiagMass = 0.d0

      DO ie=1,nbDIME*mecaEF(i)%N_NODE
        sumDiagMass = sumDiagMass + M(ie, ie)
      END DO

      ! on calcule alpha: masse totale de l'element divisee par la somme des elements diagonaux de la matrice de masse coherente
      ! i.e. alpha = xm/sumDiagMass
      alpha = xm/sumDiagMass

      ! on en deduit les coefficients de la matrice de masse lumpee
      ! en fonctions de ceux de la mtrice de masse coherente
      ! pour la ligne i: 
      !  * M_lump(i, i) = alpha*M(i, i)
      !  * M_lump(i, j) = 0, pour j different de i
      DO ie=1,nbDIME*mecaEF(i)%N_NODE 
        DO je=1,nbDIME*mecaEF(i)%N_NODE
          IF (ie /= je) M(ie, je)=0.d0
        END DO 
        M(ie, ie)=alpha*M(ie, ie) 
      END DO
    endif  
  ENDIF

  deallocate(DNX,xn); nullify(DNX)
  deallocate(P,InvJ); nullify(P,InvJ)

END SUBROUTINE

! -------------------------------------------------------------------------------
! DA : Methode de calcul des gradients
!      d/dx = Bx + Sum[dH_alpha / dx Gamma_alpha]
!      d/dy = By + Sum[dH_alpha / dy Gamma_alpha]
!      d/dz = Bz + Sum[dH_alpha / dz Gamma_alpha]
! -------------------------------------------------------------------------------
SUBROUTINE GRADIENT_SHB3D(N_NE,INVJ,DH,BX0,GAMMA,DNX)

 IMPLICIT NONE

 INTEGER     , INTENT(IN)  :: N_NE
 REAL(KIND=8), INTENT(IN)  :: DH(:,:),BX0(:,:),GAMMA(:,:),INVJ(:,:)

 REAL(KIND=8), POINTER     :: DNX(:,:)

! Variables locales
 INTEGER                   :: I,alpha,inode,Ia,j
 REAL(KIND=8), ALLOCATABLE :: DHX(:,:)

!                           1234567890123456789012345678
  CHARACTER(len=28) :: IAM='a_mecaEF_SHB::gradient_SHB3d'

! Initialisation a vide des nouveaux pointeurs


IF(ASSOCIATED(DNX)) THEN ; DEALLOCATE(DNX) ; NULLIFY(DNX) ; ENDIF

ALLOCATE(DNX(3,N_NE))
ALLOCATE(DHX(3,size(DH,2)))
DHX = ZERO

! Calcul de la matrice des gradients au points (x,y)

DO I=1,size(DH,2)
   DHX(:,I)=MATMUL(INVJ,DH(:,I))
ENDDO

! On initialize la matrice gradient avec le gradient en 0
DNX = BX0

! On ajoute les contributions des fonctions H.GAMMA
do Ia = 1, size(DH,2)
  do j = 1, 3
      DNX(j,:) = DNX(j,:) + GAMMA(Ia,:)*DHX(j,Ia)
  end do
end do

DEALLOCATE(DHX)

END SUBROUTINE GRADIENT_SHB3D

! ----------------------------------------------------------------------------------------------
! DA : Construction de la matrice Gradient SHB de stabilisation                                ]
! ----------------------------------------------------------------------------------------------

SUBROUTINE GRADIENT_STAB_SHB3D(N_NE,INVJ,V,GAMMA,DNX_1, DNX_2)

 IMPLICIT NONE

 INTEGER     , INTENT(IN)  :: N_NE
 REAL(KIND=8), INTENT(IN)  :: GAMMA(:,:),INVJ(:,:)
 REAL(KIND=8)              :: V
 REAL(KIND=8), POINTER     :: DNX_1(:,:), DNX_2(:,:)

! Variables locales
 INTEGER                   :: I,alpha,inode,Ia,j

!                           1234567890123456789012345678
  CHARACTER(len=28) :: IAM='a_mecaEF_SHB::gradient_SHB3d'

! Initialisation a vide des nouveaux pointeurs


IF(ASSOCIATED(DNX_1)) THEN ; DEALLOCATE(DNX_1) ; NULLIFY(DNX_1) ; ENDIF
IF(ASSOCIATED(DNX_2)) THEN ; DEALLOCATE(DNX_2) ; NULLIFY(DNX_2) ; ENDIF

ALLOCATE(DNX_1(3,N_NE))
ALLOCATE(DNX_2(3,N_NE))

! On initialize la matrice gradient a zero
DNX_1 = ZERO
DNX_2 = ZERO

! On ajoute les contributions des fonctions H.GAMMA atention marche pour le SHB8 seulement

! Mode de torsion H1 et H2
DNX_1(3,:) = DNX_1(3,:) + (GAMMA(1,:) + GAMMA(2,:))*SQRT(V/3.0D0)
! Mode en sablier H3 et H4
DNX_1(1,:) = DNX_1(1,:) + INVJ(1,1)*SQRT(V)*(1.0D0/3.0D0)*GAMMA(4,:)
DNX_1(2,:) = DNX_1(2,:) + INVJ(2,2)*SQRT(V)*(1.0D0/3.0D0)*GAMMA(4,:)
! Mode avec assumed strain
DNX_2(1,:) = DNX_2(1,:) + INVJ(1,1)*SQRT(V/3.0D0)*GAMMA(3,:)
DNX_2(2,:) = DNX_2(2,:) + INVJ(2,2)*SQRT(V/3.0D0)*GAMMA(3,:)


END SUBROUTINE GRADIENT_STAB_SHB3D

! ----------------------------------------------------------------------------------------------
! DA : Construction de la matrice GAMMA_alpha = (1/coef)*[ H_alpha - Sum (H_alpha^T . X_j) B_j ]
! ----------------------------------------------------------------------------------------------
SUBROUTINE GAMMA_SHB(N_NE,H,BHX,COEF,X,GAMMA)

 IMPLICIT NONE

 INTEGER     , INTENT(IN)  :: N_NE
 REAL(KIND=8), INTENT(IN)  :: H(:,:),BHX(:,:)
 REAL(KIND=8), INTENT(IN)  :: X(:,:)
 REAL(KIND=8), INTENT(IN)  :: COEF

 REAL(KIND=8), POINTER     :: GAMMA(:,:)

! Variables locales
 INTEGER                   :: iALPHA,J,ia,k
 REAL(KIND=8), ALLOCATABLE :: HXB(:)

!                           12345678901234567890123
  CHARACTER(len=23) :: IAM='a_mecaEF_SHB::GAMMA_SHB'

! Initialisation a vide des nouveaux pointeurs

IF(ASSOCIATED(GAMMA)) THEN ; DEALLOCATE(GAMMA) ; NULLIFY(GAMMA) ; ENDIF

ALLOCATE(GAMMA(size(H,1),N_NE))
ALLOCATE(HXB(N_NE))
GAMMA = ZERO
HXB = ZERO

! Calcul de la matrice Gamma
DO iALPHA = 1,size(H,1)
   HXB = ZERO
   DO J=1,3
      HXB = HXB + dot_product(H(iALPHA,:),X(J,:))*BHX(J,:)
   ENDDO
   GAMMA(iALPHA,:) = (1.0D0/coef)*(H(iALPHA,:) - HXB)
ENDDO

DEALLOCATE(HXB)

END SUBROUTINE GAMMA_SHB

! -------------------------------------------------------------------------------
! DA : Utiliser pour calculer les gradient a l origine du repere
! -------------------------------------------------------------------------------
SUBROUTINE GRADIENT_ISO3D(N_NE,DN,X,DNX)

 IMPLICIT NONE

 INTEGER     , INTENT(IN)  :: N_NE
 REAL(KIND=8), INTENT(IN)  :: DN(:,:)
 REAL(KIND=8), INTENT(IN)  :: X(:,:)

 REAL(KIND=8), POINTER     :: DNX(:,:)

! Variables locales
 REAL(KIND=8), ALLOCATABLE :: J(:,:), INVJ(:,:)
 REAL(KIND=8)              :: DETJ
 INTEGER                   :: I
!                           1234567890123456789012345678
  CHARACTER(len=28) :: IAM='a_mecaEF_SHB::gradient_iso3d'

! Initialisation a vide des nouveaux pointeurs

IF(ASSOCIATED(DNX)) THEN ; DEALLOCATE(DNX) ; NULLIFY(DNX) ; ENDIF

ALLOCATE(J(3,3),INVJ(3,3))
ALLOCATE(DNX(3,N_NE))
DNX = ZERO
INVJ = ZERO

! Calcul de la matrice Jacobienne, de son determinant, et de son inverse

J(1,:)=(/DOT_PRODUCT(DN(1,:),X(1,:)),DOT_PRODUCT(DN(1,:),X(2,:)),DOT_PRODUCT(DN(1,:),X(3,:)) /)
J(2,:)=(/DOT_PRODUCT(DN(2,:),X(1,:)),DOT_PRODUCT(DN(2,:),X(2,:)),DOT_PRODUCT(DN(2,:),X(3,:)) /)
J(3,:)=(/DOT_PRODUCT(DN(3,:),X(1,:)),DOT_PRODUCT(DN(3,:),X(2,:)),DOT_PRODUCT(DN(3,:),X(3,:)) /)

DETJ=  J(1,1)*(J(2,2)*J(3,3)-J(2,3)*J(3,2)) &
      -J(1,2)*(J(2,1)*J(3,3)-J(3,1)*J(2,3)) &
      +J(1,3)*(J(2,1)*J(3,2)-J(3,1)*J(2,2))     

IF (ABS(DETJ).LT.1.E-16) call faterr(IAM,' Matrice Jacobienne non inversible')

INVJ(1,1)= (J(2,2)*J(3,3)-J(3,2)*J(2,3))/DETJ
INVJ(2,1)=-(J(2,1)*J(3,3)-J(3,1)*J(2,3))/DETJ
INVJ(3,1)= (J(2,1)*J(3,2)-J(3,1)*J(2,2))/DETJ
INVJ(1,2)=-(J(1,2)*J(3,3)-J(3,2)*J(1,3))/DETJ
INVJ(2,2)= (J(1,1)*J(3,3)-J(3,1)*J(1,3))/DETJ
INVJ(3,2)=-(J(1,1)*J(3,2)-J(3,1)*J(1,2))/DETJ
INVJ(1,3)= (J(1,2)*J(2,3)-J(2,2)*J(1,3))/DETJ
INVJ(2,3)=-(J(1,1)*J(2,3)-J(2,1)*J(1,3))/DETJ
INVJ(3,3)= (J(1,1)*J(2,2)-J(2,1)*J(1,2))/DETJ

! Calcul de la matrice des gradients au points (x,y)
DO I=1,N_NE
   DNX(:,I)=MATMUL(INVJ,DN(:,I))
ENDDO

DEALLOCATE(J,INVJ)

END SUBROUTINE GRADIENT_ISO3D

! -------------------------------------------------------------------------------
! DA : Utiliser pour calculer le jacobien en chaque point de Gauss
! -------------------------------------------------------------------------------
SUBROUTINE JACOBIEN_ISO3D(DN,X,DETJ,INVJ)

 IMPLICIT NONE

 REAL(KIND=8), INTENT(IN)  :: DN(:,:)
 REAL(KIND=8), INTENT(OUT) :: DETJ
 REAL(KIND=8), INTENT(IN)  :: X(:,:)
 real(kind=8), dimension(:,:), pointer :: INVJ

! Variables locales
 INTEGER                   :: I,alpha,inode
 REAL(KIND=8), ALLOCATABLE :: J(:,:)
!                           1234567890123456789012345678
  CHARACTER(len=28) :: IAM='a_mecaEF_SHB::jacobien_iso3d'

ALLOCATE(J(3,3))
if(associated(invj)) then
  deallocate(invj)
  nullify(invj)
endif
ALLOCATE(INVJ(3,3))
INVJ = ZERO
J = ZERO
! Calcul de la matrice Jacobienne, de son determinant, et de son inverse

J(1,:)=(/DOT_PRODUCT(DN(1,:),X(1,:)),DOT_PRODUCT(DN(1,:),X(2,:)),DOT_PRODUCT(DN(1,:),X(3,:)) /)
J(2,:)=(/DOT_PRODUCT(DN(2,:),X(1,:)),DOT_PRODUCT(DN(2,:),X(2,:)),DOT_PRODUCT(DN(2,:),X(3,:)) /)
J(3,:)=(/DOT_PRODUCT(DN(3,:),X(1,:)),DOT_PRODUCT(DN(3,:),X(2,:)),DOT_PRODUCT(DN(3,:),X(3,:)) /)               
DETJ=  ABS(J(1,1)*(J(2,2)*J(3,3)-J(2,3)*J(3,2)) &
      -J(1,2)*(J(2,1)*J(3,3)-J(3,1)*J(2,3)) &
      +J(1,3)*(J(2,1)*J(3,2)-J(3,1)*J(2,2)))

IF (DETJ.LT.1.E-16) call faterr(IAM,' Matrice Jacobienne non inversible')

INVJ(1,1)= (J(2,2)*J(3,3)-J(3,2)*J(2,3))/DETJ
INVJ(2,1)=-(J(2,1)*J(3,3)-J(3,1)*J(2,3))/DETJ
INVJ(3,1)= (J(2,1)*J(3,2)-J(3,1)*J(2,2))/DETJ
INVJ(1,2)=-(J(1,2)*J(3,3)-J(3,2)*J(1,3))/DETJ
INVJ(2,2)= (J(1,1)*J(3,3)-J(3,1)*J(1,3))/DETJ
INVJ(3,2)=-(J(1,1)*J(3,2)-J(3,1)*J(1,2))/DETJ
INVJ(1,3)= (J(1,2)*J(2,3)-J(2,2)*J(1,3))/DETJ
INVJ(2,3)=-(J(1,1)*J(2,3)-J(2,1)*J(1,3))/DETJ
INVJ(3,3)= (J(1,1)*J(2,2)-J(2,1)*J(1,2))/DETJ

DEALLOCATE(J)

END SUBROUTINE JACOBIEN_ISO3D

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_bulk_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty, &
                                       Fint,compute_fint,            &
                                       K,compute_stiffness,          &
                                       push_fields )

   implicit none
   INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   real(kind=8)                    :: dt
   REAL(KIND=LONG)                 :: X(:,:),U(:,:) ! coordonnees des sommets
   REAL(KIND=LONG)                 :: K(:,:)        ! matrice de rig. elem.
   REAL(KIND=LONG),DIMENSION(:)    :: Fint
   logical :: compute_fint,compute_stiffness,push_fields
                           !123456789012345678901
   character(len=21):: IAM='a_meca_EF_SHB::bulk'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)

   select case(get_eleop_value(mdlnb,'kine_'))
   case('small')
     call bulk_hpp_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,compute_fint,K,compute_stiffness,push_fields)
   case('large')
     call bulk_gd_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,compute_fint,K,compute_stiffness,push_fields)
   case default
     call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
     call FATERR(IAM,'kinematic type unknown (small | large)')
   end select

END SUBROUTINE

SUBROUTINE BULK_HPP_SHB_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,K)

  IMPLICIT NONE
  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                     
  integer,dimension(:)           ::ppsnb
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U     ! coordonnees de départ,deplacement total
  REAL(KIND=LONG),DIMENSION(3)   :: Xcenter ! coordonnees du centre de l'element

  real(kind=8)                   :: dt

  REAL(KIND=LONG),DIMENSION(:,:) :: K      ! matrice de rig. elem.
  REAL(KIND=LONG),DIMENSION(:)   :: Fint 

 ! variables locales

  REAL(KIND=LONG), POINTER       :: Bl(:,:)
  REAL(KIND=LONG), POINTER       :: DNX(:,:),Ps(:,:),InvJ(:,:),Ps_T(:,:)
  REAL(KIND=LONG)                :: J

  INTEGER                        :: IG,IG_S, IG_T, Inode

  INTEGER                        :: anisotropie

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb,bhvnb

  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0 
  REAL(KIND=LONG),DIMENSION(:,:),ALLOCATABLE :: KLOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: FLOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: XLOC
  
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1 
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D
  

  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4                      :: calcD

  INTEGER                        :: istrg
  !
  ! internal and stress values at Gauss points
  ! for computation with a model internal to lmgc90
  real(kind=8), dimension(6,4) :: vpg
  real(kind=8), dimension(4,4) :: Sloc
  
  real(kind=8) :: Tref,UMTT,field,field_begin


                           !1234567890123456789012345
  CHARACTER(len=27) :: IAM='a_mecaEF_SHB::bulk_gd_iso'

  ! Initialisation a vide des pointeurs
  nullify(DNX,Bl)
  nullify(Ps,InvJ)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))
  
  ALLOCATE(KLOC(3*mecaEF(i)%N_NODE, 3*mecaEF(i)%N_NODE))
  KLOC = 0.0D0
  ALLOCATE(FLOC(3*mecaEF(i)%N_NODE))
  FLOC = 0.0D0
  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  
  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  Fint=ZERO
  K=ZERO
  
  ! DA : Calcul du centre de l'element pour le repere local
  Xcenter = ZERO
  DO Inode=1,mecaEF(i)%N_NODE
      Xcenter(1) = Xcenter(1) + X(1,Inode)
      Xcenter(2) = Xcenter(2) + X(2,Inode)
      Xcenter(3) = Xcenter(3) + X(3,Inode)
  ENDDO
  
  Xcenter = Xcenter / mecaEF(i)%N_NODE

  DO Inode=1,mecaEF(i)%N_NODE
      X(1,Inode) = X(1,Inode) - Xcenter(1)
      X(2,Inode) = X(2,Inode) - Xcenter(2)
      X(3,Inode) = X(3,Inode) - Xcenter(3)
  ENDDO
  
  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
     
     ! Initialisation a vide des valeurs nodales
     KLOC = ZERO
     FLOC = ZERO
     ULOC = ZERO
     XLOC = ZERO
     
     !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface moyenne
     CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
     
     ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
     XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
     
     ! ATTENTION : X contient les coordonnees locales de l element
     X = RESHAPE(source = XLOC,shape=(/3, mecaEF(i)%N_NODE/))

     ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
     DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS

       ! DA : Recuperation de la matrice gradient au point de gauss de l'epaisseur
       !      Recuperation du gradient en zero : 
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN,X,DNX)

       ! on rapatrie les infos du debut de pas
       !print *,'Num point de Gauss : ',IG
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       IF (extP_nb /= 0) THEN 
   
         do if=1,extP_nb
           name=get_external_field_name(mdlnb,if)
           extP_lbl(if)=name
           extP_len(if)=len_trim(name)
           rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
           CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if))
   
         enddo
   
       ELSE
   
         extP_lbl(1)=' '
         extP_val(1)=0.
   
       ENDIF
        
        ! CALCUL DU GRADIENT DES DEFORMATIONS
        !*** Calcul de [1+d(u_n+1)/d(x_0)]
        ! DA : On utilise les fonctions de forme SHB pour le gradient
        !      
        CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X,J,INVJ)
        
        IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
   
         if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'elas_') then 
           call logmes('mater ='//get_eleop_value_bypps(ppsnb(ig),'mater'))
           call FATERR(IAM,'Using isext == no___ is only possible with mater == elas ')
         endif
   
         CALL D_SOLID_SHB(ppsnb(ig),D)
         
         istrg = 1
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl)    ! formation de Bl  
         
         GRAD1 = MATMUL(Bl, ULOC)
   
         CALL comp_stress_shb(ppsnb(ig),GRAD1,FLUX1)
   
       ELSE
   
         ! on va utiliser la notion de field attache au model
   
         extP_nb = get_external_field_nb(mdlnb)
   
         IF (extP_nb /= 0) THEN 
   
           do if=1,extP_nb
             name=get_external_field_name(mdlnb,if)
             extP_lbl(if)=name
             extP_len(if)=len_trim(name)
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
   
             CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field)
   
             extP_val(if) = field
   
             !fd a voir si ca est mieux ...
             !CALL get_meca_field_begin_MAILx(ibdyty,iblmty,ig,rank,field_begin)
             !extP_val(if) = (UMTT*field_begin + theta*field)
   
             if (trim(name) == 'TEMPERATURE') then
               Tref = get_Tref_meca(bhvnb)
               if (dabs(extP_val(if) - Tref) < 1e-10) extP_val(if) = Tref
             endif
   
   
             !!$ print *,'<<--------------------'
             !!$ print *,ibdyty,iblmty,ig,if,extP_val(if)
             !!$ print *,extP_lbl(if),extP_len(if)
             !!$ print *,'-------------------->>'
   
             !if (iblmty==1) print*,extP_val
           enddo
   
         ELSE
   
           extP_lbl(1)=' '
           extP_val(1)=0.
   
         ENDIF
   
         istrg = 2
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl)
   
         GRAD1 = MATMUL(Bl, ULOC)
   
         calcD=1
   
         CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                 GRAD0,FLUX0,INTERNAL0, &
                                 GRAD1,FLUX1,INTERNAL1, &
                                 D,dt,calcD)
   
         !if (iblmty==1 ) then
         !  print*,'0',GRAD0,FLUX0
         !  print*,'1',GRAD1,FLUX1
         !  !print*,extP_val
         !endif
   
       ENDIF 
       !
       !  ke= Bt.D.B.coef
       !
       ! Calcul de la matrice elementaire
       KLOC = KLOC + (MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*J*mecaEF(i)%PG(ig)%POIDS)

       FLOC = FLOC + (MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*J*mecaEF(i)%PG(ig)%POIDS)

       ! Pour tous les point de Gauss
       IG = IG + 1 
     ENDDO
     
     ! Passage des efforts en coordonnes globale
     Fint = Fint + MATMUL(TRANSPOSE(Ps),FLOC)
     
     ! Passage de la matrice en coordonnes globale
     K = K + MATMUL(TRANSPOSE(Ps),MATMUL(KLOC,Ps))
     
     ! Passage a une autre epaisseur
  ENDDO

  deallocate(extP_lbl,extP_len,extP_val)

  deallocate(DNX,Bl,invJ);  nullify(DNX,Bl,invJ)
  deallocate(Ps,Ps_T)    ;  nullify(Ps,Ps_T)
  deallocate(KLOC,FLOC,ULOC,XLOC)
  
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE BULK_HPP_SHB_ISO

SUBROUTINE BULK_GD_SHB_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,K)

  IMPLICIT NONE
  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                     
  integer,dimension(:)           ::ppsnb
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U     ! coordonnees de départ,deplacement total
  REAL(KIND=LONG),DIMENSION(3)   :: Xcenter ! coordonnees du centre de l'element

  real(kind=8)                   :: dt

  REAL(KIND=LONG),DIMENSION(:,:) :: K      ! matrice de rig. elem.
  REAL(KIND=LONG),DIMENSION(:)   :: Fint 

 ! variables locales

  REAL(KIND=LONG), POINTER       :: B(:,:)
  REAL(KIND=LONG), POINTER       :: BX0(:,:),Ps(:,:),InvJ(:,:),Ps_T(:,:)
  REAL(KIND=LONG)                :: J

  INTEGER                        :: IG,IG_S, IG_T, Inode

  INTEGER                        :: anisotropie

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb

  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0 
  REAL(KIND=LONG),DIMENSION(:,:),ALLOCATABLE :: KLOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: FLOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: XLOC
  
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1 
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D
  

  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4                      :: calcD

  INTEGER                        :: istrg
  !
  ! internal and stress values at Gauss points
  ! for computation with a model internal to lmgc90
  real(kind=8), dimension(6,4) :: vpg
  real(kind=8), dimension(4,4) :: Sloc


                           !1234567890123456789012345
  CHARACTER(len=27) :: IAM='a_mecaEF_SHB::bulk_gd_iso'

  ! Initialisation a vide des pointeurs
  nullify(BX0,B)
  nullify(Ps,InvJ)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))
  
  ALLOCATE(KLOC(3*mecaEF(i)%N_NODE, 3*mecaEF(i)%N_NODE))
  KLOC = 0.0D0
  ALLOCATE(FLOC(3*mecaEF(i)%N_NODE))
  FLOC = 0.0D0
  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  
  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  Fint=ZERO
  K=ZERO
  
  ! DA : Calcul du centre de l'element pour le repere local
  Xcenter = ZERO
  DO Inode=1,mecaEF(i)%N_NODE
      Xcenter(1) = Xcenter(1) + X(1,Inode)
      Xcenter(2) = Xcenter(2) + X(2,Inode)
      Xcenter(3) = Xcenter(3) + X(3,Inode)
  ENDDO
  
  Xcenter = Xcenter / mecaEF(i)%N_NODE

  DO Inode=1,mecaEF(i)%N_NODE
      X(1,Inode) = X(1,Inode) - Xcenter(1)
      X(2,Inode) = X(2,Inode) - Xcenter(2)
      X(3,Inode) = X(3,Inode) - Xcenter(3)
  ENDDO
  
  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
     
     ! Initialisation a vide des valeurs nodales
     KLOC = ZERO
     FLOC = ZERO
     ULOC = ZERO
     XLOC = ZERO
     
     !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface moyenne
     CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
     
     ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
     XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
     
     ! ATTENTION : X contient les coordonnees locales de l element
     X = RESHAPE(source = XLOC,shape=(/3, mecaEF(i)%N_NODE/))

     ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
     DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS

       ! DA : Recuperation de la matrice gradient au point de gauss de l'epaisseur
       !      Recuperation du gradient en zero : 
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN,X,BX0)

       ! on rapatrie les infos du debut de pas
       !print *,'Num point de Gauss : ',IG
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       IF (extP_nb /= 0) THEN 
   
         do if=1,extP_nb
           name=get_external_field_name(mdlnb,if)
           extP_lbl(if)=name
           extP_len(if)=len_trim(name)
           rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
           CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if))
   
         enddo
   
       ELSE
   
         extP_lbl(1)=' '
         extP_val(1)=0.
   
       ENDIF
        
        ! CALCUL DU GRADIENT DES DEFORMATIONS
        !*** Calcul de [1+d(u_n+1)/d(x_0)]
        ! DA : On utilise les fonctions de forme SHB pour le gradient
        !      
        CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X,J,INVJ)
        
        CALL B_SHB3D(mecaEF(i)%N_NODE,BX0,mecaEF(i)%Coef_C,B)

        GRAD1 = Id3 + MATMUL(B, ULOC)
        !
        FLUX1 = ZERO
        IF (nb_internal /= 0 ) INTERNAL1 = ZERO
        !
        calcD=1
        !print *,'Body : ',ibdyty, ' calcul LdC'
        
        CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                 GRAD0,FLUX0,INTERNAL0, &
                                 GRAD1,FLUX1,INTERNAL1, &
                                 D,dt,calcD)
       !
       !  ke= Bt.D.B.coef
       !
       ! Calcul de la matrice elementaire
       
       KLOC = KLOC + (MATMUL(TRANSPOSE(B),MATMUL(D,B))*J*mecaEF(i)%PG(ig)%POIDS)

       FLOC = FLOC + (MATMUL(TRANSPOSE(B),FLUX1(1:SIZE(B,dim=1)))*J*mecaEF(i)%PG(ig)%POIDS)

       ! Pour tous les point de Gauss
       IG = IG + 1 
     ENDDO
     
     ! Passage des efforts en coordonnes globale
     Fint = Fint + MATMUL(TRANSPOSE(Ps),FLOC)
     
     ! Passage de la matrice en coordonnes globale
     K = K + MATMUL(TRANSPOSE(Ps),MATMUL(KLOC,Ps))
     
     ! Passage a une autre epaisseur
  ENDDO

  deallocate(extP_lbl,extP_len,extP_val)

  deallocate(BX0,B)       ;  nullify(BX0,B)
  deallocate(Ps,Ps_T,InvJ);  nullify(Ps,Ps_T,InvJ)
  deallocate(KLOC,FLOC,ULOC,XLOC)
  
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE BULK_GD_SHB_ISO

SUBROUTINE BULK_GD_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty, &
                        Fint,compute_fint,           &
                        K,compute_stiffness,         &
                        push_fields)

  IMPLICIT NONE
  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                     
  integer,dimension(:)           ::ppsnb
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U     ! coordonnees de départ,deplacement total
  REAL(KIND=LONG),DIMENSION(3)   :: Xcenter ! coordonnees du centre de l'element

  real(kind=8)                   :: dt

  REAL(KIND=LONG),DIMENSION(:,:) :: K      ! matrice de rig. elem.
  REAL(KIND=LONG),DIMENSION(:)   :: Fint 

 ! variables locales

  REAL(KIND=LONG), POINTER       :: DNX(:,:),B(:,:)
  REAL(KIND=LONG), POINTER       :: GAMMA(:,:),BX0(:,:),Ps(:,:),InvJ(:,:),Ps_T(:,:)
  REAL(KIND=LONG)                :: J

  INTEGER                        :: IG,IG_S, IG_T, Inode

  INTEGER                        :: anisotropie

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb
  logical :: compute_fint,compute_stiffness,push_fields
  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0 
  REAL(KIND=LONG),DIMENSION(:,:),ALLOCATABLE :: KLOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: FLOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:)  ,ALLOCATABLE :: XLOC
  REAL(KIND=8),DIMENSION(:,:)   ,ALLOCATABLE :: X_LOC
  
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1 
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D
  

  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4                      :: calcD

  INTEGER                        :: istrg
  !
  ! internal and stress values at Gauss points
  ! for computation with a model internal to lmgc90
  real(kind=8), dimension(6,4) :: vpg
  real(kind=8), dimension(4,4) :: Sloc


                           !1234567890123456789012345
  CHARACTER(len=27) :: IAM='a_mecaEF_SHB::bulk_gd_shb'

  ! Initialisation a vide des pointeurs
  nullify(BX0,GAMMA)
  nullify(Ps,Ps_T)
  nullify(INVJ,DNX,B)


  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))
  
  ALLOCATE(KLOC(3*mecaEF(i)%N_NODE, 3*mecaEF(i)%N_NODE))
  KLOC = 0.0D0
  ALLOCATE(FLOC(3*mecaEF(i)%N_NODE))
  FLOC = 0.0D0
  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  ALLOCATE(X_LOC(3,mecaEF(i)%N_NODE))
  X_LOC = 0.0D0
  
  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  Fint=ZERO
  K=ZERO
  
  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
     
     ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
     DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
     
       KLOC = ZERO
       FLOC = ZERO
       ULOC = ZERO
       XLOC = ZERO
       X_LOC = ZERO

       !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface coque
       CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)

       ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
       XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
       X_LOC = RESHAPE(source=XLOC,shape=(/ 3, mecaEF(i)%N_NODE/))

       ! DA : Recuperation de la matrice gradient en 0,0,0
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(get_N_GP_mecaEF_SHB(i) + 1)%DN, X_LOC,BX0)

       ! DA : Recuperation des fonctions de forme pour le hors plan
       CALL GAMMA_SHB(mecaEF(i)%N_NODE,mecaEF(i)%H,BX0,mecaEF(i)%Coef,X_LOC,GAMMA)

       ! DA : Calcul de la matrice Jacobienne dans le repere local
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X_LOC,J,INVJ)
       
       ! DA : Calcul de la matrice gradient dans le repere local mode de deformation Coques
       CALL GRADIENT_SHB3D(mecaEF(i)%N_NODE,INVJ,mecaEF(i)%hPG(ig)%DN,BX0,GAMMA,DNX)
       
       ! on rapatrie les infos du debut de pas
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       IF (extP_nb /= 0) THEN 
   
         do if=1,extP_nb
           name=get_external_field_name(mdlnb,if)
           extP_lbl(if)=name
           extP_len(if)=len_trim(name)
           rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
           CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if))
   
         enddo
   
       ELSE
   
         extP_lbl(1)=' '
         extP_val(1)=0.
   
       ENDIF
        
        ! CALCUL DU GRADIENT DES DEFORMATIONS
        !*** Calcul de [1+d(u_n+1)/d(x_0)]
        ! DA : On utilise les fonctions de forme SHB pour le gradient
        !      
        
        CALL B_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,B)

        GRAD1 = Id3 + MATMUL(B, ULOC)
        !
        FLUX1 = ZERO
        IF (nb_internal /= 0 ) INTERNAL1 = ZERO
        !
        calcD=1
        !print *,'Body : ',ibdyty, ' calcul LdC'
        
        CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                 GRAD0,FLUX0,INTERNAL0, &
                                 GRAD1,FLUX1,INTERNAL1, &
                                 D,dt,calcD)
       
       if (compute_stiffness) then
            ! Calcul de la matrice elementaire
            KLOC = KLOC + mecaEF(i)%Coef_INT*(MATMUL(TRANSPOSE(B),MATMUL(D,B))*J*mecaEF(i)%PG(ig)%POIDS)
            ! Passage de la matrice integre dans l'epaisseur en coordonnes globale
            K = K + MATMUL(Ps_T,MATMUL(KLOC,Ps))
       endif
              
       if (compute_fint) then
             ! Calcul des efforts internes
             FLOC = FLOC + mecaEF(i)%Coef_INT*(MATMUL(TRANSPOSE(B),FLUX1(1:SIZE(B,dim=1)))*J*mecaEF(i)%PG(ig)%POIDS)
             ! Passage des efforts integre dans l'epaisseur en coordonnes globale
             Fint = Fint + MATMUL(Ps_T,FLOC)
       endif
   
       if (push_fields) then
             ! DA : Stockage des informations aux Points de Gauss
             CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1) ! DA : On laisse les informations dans le repere local de la coque
             CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1) ! DA : On laisse les informations dans le repere local de la coque
             
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,1))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,2))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,3))
             
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_x')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,1))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_y')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,2))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_z')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,3))
             
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,1))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,2))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,3))
             
             IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
       endif
       
       ! Pour tous les point de Gauss
       IG = IG + 1

     ENDDO

    ! Passage a une autre point de gauss
     
  ENDDO

  ! Fin du calcul de la matrice elementaire
  DEALLOCATE(extP_lbl,extP_len,extP_val)
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

  deallocate(DNX,B,InvJ)  ; nullify(DNX,B,InvJ)
  deallocate(BX0,GAMMA)   ; nullify(BX0,GAMMA)
  deallocate(Ps,Ps_T)     ; nullify(Ps,Ps_T)
  deallocate(KLOC,FLOC,ULOC,XLOC,X_LOC)
     
END SUBROUTINE BULK_GD_SHB

! ---------------
SUBROUTINE Stress2Fint_SHB_ISO(i,ppsnb,X,ibdyty,iblmty,FINT)

  IMPLICIT NONE

  ! le numero de l'element
  INTEGER         , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty 
  ! coordonnees des sommets
  REAL(KIND=LONG)              :: X(:,:)                
  REAL(KIND=LONG),DIMENSION(3)   :: Xcenter ! coordonnees du centre de l'element
  ! Vecteur des forces internes ele 
  REAL(KIND=LONG)              :: FINT(:)               

  ! variables locales
  REAL(KIND=LONG), POINTER       :: B(:,:),InvJ(:,:)
  REAL(KIND=LONG), POINTER       :: BX0(:,:),Ps(:,:),Ps_T(:,:)

  ! coordonnee locale
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: FLOC
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: XLOC

  REAL(KIND=LONG)              :: J
  INTEGER                      :: IG,IG_T,IG_S,inode

  REAL(KIND=LONG) ,ALLOCATABLE :: FLUX1(:) ! vecteur de travail local

  INTEGER                      :: mdlnb,inull,nb_external,istrg
  real(kind=8), dimension(3,3)             :: tenseur3

  ! Initialisation a vide des pointeurs
  nullify(BX0,B)
  nullify(Ps,InvJ)
     

  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external=get_nb_external_variables(mdlnb)

  ALLOCATE(FLUX1(nb_external))

  ALLOCATE(FLOC(3*mecaEF(i)%N_NODE))
  FLOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0

  FINT = ZERO

  ! DA : Calcul du centre de l'element pour le repere local
  Xcenter = ZERO
  DO Inode=1,mecaEF(i)%N_NODE
      Xcenter(1) = Xcenter(1) + X(1,Inode)
      Xcenter(2) = Xcenter(2) + X(2,Inode)
      Xcenter(3) = Xcenter(3) + X(3,Inode)
  ENDDO
  
  Xcenter = Xcenter / mecaEF(i)%N_NODE

  DO Inode=1,mecaEF(i)%N_NODE
      X(1,Inode) = X(1,Inode) - Xcenter(1)
      X(2,Inode) = X(2,Inode) - Xcenter(2)
      X(3,Inode) = X(3,Inode) - Xcenter(3)
  ENDDO

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
     
     FLOC = ZERO
     XLOC = ZERO
     
     !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface moyenne
     CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
     
     XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
     
     ! ATTENTION : X contient les coordonnees locales de l element
     X = RESHAPE(source = XLOC,shape=(/3, mecaEF(i)%N_NODE/))
     
     ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
     DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
       
       ! DA : Recuperation de la matrice gradient sur le plan moyen de la coque
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, &
                           X,BX0)
       
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X,J,INVJ)
       
       IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
         istrg = 1
         ! formation de Bl 
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,BX0,mecaEF(i)%Coef_C,istrg,B)
   
       ELSE
         istrg = 2
         select case(get_eleop_value_bypps(ppsnb(ig),'kine_'))
         case('small')
   
           CALL Bl_SHB3D(mecaEF(i)%N_NODE,BX0,mecaEF(i)%Coef_C,istrg,B)   
   
         case('large')
   
           CALL B_SHB3D(mecaEF(i)%N_NODE,BX0,mecaEF(i)%Coef_C,B)
   
         end select
    
       ENDIF
       
       ! Recuperation des contraintes internes dans le repere local
       CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX1)
       
       FLOC = FLOC + (MATMUL(TRANSPOSE(B),FLUX1)*J*mecaEF(i)%PG(ig)%POIDS)
       
       IG = IG + 1 ! Pour tous les point de Gauss
     ENDDO
     
     ! Passage des efforts en coordonnes globale
     Fint = Fint + MATMUL(TRANSPOSE(Ps),FLOC)
     
  ENDDO

  deallocate(BX0,B); nullify(BX0,B)
  
  deallocate(Ps,Ps_T,InvJ); nullify(Ps,Ps_T,InvJ)

  deallocate(FLOC,XLOC,FLUX1)

END SUBROUTINE Stress2Fint_SHB_ISO

! ---------------
SUBROUTINE Stress2Fint_SHB(i,ppsnb,X,ibdyty,iblmty,FINT)

  IMPLICIT NONE

  ! le numero de l'element
  INTEGER         , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty 
  ! coordonnees des sommets
  REAL(KIND=LONG)              :: X(:,:)                
  ! Vecteur des forces internes ele 
  REAL(KIND=LONG)              :: FINT(:)               

  ! variables locales
  REAL(KIND=LONG), POINTER       :: DNX(:,:),B(:,:),InvJ(:,:)
  REAL(KIND=LONG), POINTER       :: GAMMA(:,:),BX0(:,:),Ps(:,:),Ps_T(:,:)

  ! coordonnee locale
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: FLOC
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: XLOC
  REAL(KIND=8),DIMENSION(:,:)   ,ALLOCATABLE :: X_LOC

  REAL(KIND=LONG)              :: J
  INTEGER                      :: IG,IG_T,IG_S,inode

  REAL(KIND=LONG) ,ALLOCATABLE :: FLUX1(:) ! vecteur de travail local

  INTEGER                      :: mdlnb,inull,nb_external,istrg
  real(kind=8), dimension(3,3)             :: tenseur3

  ! Initialisation a vide des pointeurs
  nullify(BX0,GAMMA)
  nullify(Ps,Ps_T)
  nullify(INVJ,DNX,B)

  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external=get_nb_external_variables(mdlnb)

  ALLOCATE(FLUX1(nb_external))

  ALLOCATE(FLOC(3*mecaEF(i)%N_NODE))
  FLOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  ALLOCATE(X_LOC(3,mecaEF(i)%N_NODE))
  X_LOC = 0.0D0

  FINT = ZERO

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
     
     ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
     DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS

       FLOC = ZERO
       XLOC = ZERO
       X_LOC = ZERO

       !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface coque
       CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)

       XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
       X_LOC = RESHAPE(source=XLOC,shape=(/ 3, mecaEF(i)%N_NODE/))

       ! DA : Recuperation de la matrice gradient en 0,0,0
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(get_N_GP_mecaEF_SHB(i) + 1)%DN, X_LOC,BX0)

       ! DA : Recuperation des fonctions de forme pour le hors plan
       CALL GAMMA_SHB(mecaEF(i)%N_NODE,mecaEF(i)%H,BX0,mecaEF(i)%Coef,X_LOC,GAMMA)
       
       ! DA : Calcul de la matrice Jacobienne dans le repere local
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X_LOC,J,INVJ)
       
       ! DA : Calcul de la matrice gradient dans le repere local mode de deformation Coques
       CALL GRADIENT_SHB3D(mecaEF(i)%N_NODE,INVJ,mecaEF(i)%hPG(ig)%DN,BX0,GAMMA,DNX)
       
       
       IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
         istrg = 1
         ! formation de Bl 
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,B)
   
       ELSE
         istrg = 2
         select case(get_eleop_value_bypps(ppsnb(ig),'kine_'))
         case('small')
   
           CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,B)   
   
         case('large')
   
           CALL B_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,B)
   
         end select
    
       ENDIF
       
       ! Recuperation des contraintes internes dans le repere local
       CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX1)
       
       FLOC = FLOC + mecaEF(i)%Coef_INT*(MATMUL(TRANSPOSE(B),FLUX1)*J*mecaEF(i)%PG(ig)%POIDS)
       
       IG = IG + 1 ! Pour tous les point de Gauss
     ENDDO
     
     ! Passage des efforts en coordonnes globale
     Fint = Fint + MATMUL(Ps_T,FLOC)
     
  ENDDO

  deallocate(DNX,BX0,B,GAMMA); nullify(DNX,BX0,B,GAMMA)
  deallocate(Ps,Ps_T,InvJ)   ; nullify(Ps,Ps_T,InvJ)
  
  deallocate(FLOC,XLOC,X_LOC)
  deallocate(FLUX1)

END SUBROUTINE Stress2Fint_SHB

! ------------------------------------------------------------------------------

SUBROUTINE B_SHB3D(N_NE,DNX,C,B)

! computes the B matrix (gradient of deformation)  at a gauss point

!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                  dNI / dY       0          0                                 !
!                  dNI / dZ       0          0                                 ! 
!                      0      dNI / dX       0                                 !  
!                      0      dNI / dY       0                                 !
!                      0      dNI / dZ       0                                 !
!                      0          0      dNI / dX                              !  
!                      0          0      dNI / dY                              !
!                      0          0      dNI / dZ ]                            !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=8), POINTER       :: DNX(:,:)
REAL(KIND=8)                :: C
REAL(KIND=8), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I
!                           1234567890123456789
CHARACTER(len=19) :: IAM='a_mecaEF_SHB::B_SHB3D'
! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF

ALLOCATE(B(9,3*N_NE))
DO I=1,N_NE
   B(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
   B(2,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,   ZERO      ,   ZERO      /)
   B(3,3*(I-1)+1:3*I) = (/C*DNX(3,I)  ,   ZERO      ,   ZERO      /)
   B(4,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(1,I)   ,   ZERO      /)      
   B(5,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
   B(6,3*(I-1)+1:3*I) = (/   ZERO     ,C*DNX(3,I)   ,   ZERO      /)
   B(7,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,C*DNX(1,I)   /)      
   B(8,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,C*DNX(2,I)   /)
   B(9,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
ENDDO
 
END SUBROUTINE B_SHB3D

! ------------------------------------------------------------------------------

!------------------------------------------------------------------------------!
!    Calcul de la rigidite elementaire  [Ke]=Sum [Bl]t[D][Bl]µi                !
!------------------------------------------------------------------------------!

SUBROUTINE BULK_HPP_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty, &
                        Fint,compute_fint,            &
                        K,compute_stiffness,          &
                        push_fields)
                        
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty
  ! coordonnees des sommets
  REAL(KIND=LONG)                 :: X(:,:),U(:,:)
  real(kind=8)                    :: dt
  ! matrice de rig. elem.
  REAL(KIND=LONG)                 :: K(:,:)        
  REAL(KIND=LONG),DIMENSION(:)    :: Fint 

  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:), & !   
                                     Bl(:,:),  & ! matrice Bl
                                     D(:,:), &   ! matrice de comportement
                                     InvJ(:,:), &
                                     DNX_STAB_1(:,:), &
                                     DNX_STAB_2(:,:), &
                                     Bl_STAB(:,:)

  REAL(KIND=LONG), POINTER        :: GAMMA(:,:),BX0(:,:),Ps(:,:),Ps_T(:,:)
  REAL(KIND=LONG)                 :: J, VOL

  INTEGER                         :: IG,IG_S,IG_T,Inode

  INTEGER                         :: anisotropie

  INTEGER                         :: nb_external, nb_internal
  INTEGER                         :: mdlnb,bhvnb
  logical :: compute_fint,compute_stiffness,push_fields

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)     ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(KIND=8),DIMENSION(:,:)   ,ALLOCATABLE :: KLOC
  REAL(KIND=8),DIMENSION(:)     ,ALLOCATABLE :: FLOC
  REAL(KIND=8),DIMENSION(:)     ,ALLOCATABLE :: ULOC
  REAL(KIND=8),DIMENSION(:)     ,ALLOCATABLE :: XLOC
  REAL(KIND=8),DIMENSION(:,:)   ,ALLOCATABLE :: X_LOC
  REAL(kind=8),DIMENSION(:)     ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

  !fd 21/04/08 modif pour gestion des field

  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  ! switch forme de B 
  INTEGER :: istrg

  ! demande calcul matrice tangente
  INTEGER(kind=4) :: calcD

  character(len=5) :: kinematic,formulation

                          !12345678901234567890123
  character(len=25):: IAM='a_meca_EF_SHB::bulk_hpp'

  real(kind=8) :: Tref,UMTT,field,field_begin

  !fd 13/09
  !mdl is the same for all gp of the element also some time its faster 
  !to take information from the first one ...

  ! Initialisation a vide des pointeurs
  nullify(BX0,GAMMA)
  nullify(Ps,Ps_T)
  nullify(INVJ,DNX,DNX_STAB_1,DNX_STAB_2,Bl_STAB)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,bhvnb)

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external), &
           FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))

  ALLOCATE(KLOC(3*mecaEF(i)%N_NODE, 3*mecaEF(i)%N_NODE))
  KLOC = 0.0D0
  ALLOCATE(FLOC(3*mecaEF(i)%N_NODE))
  FLOC = 0.0D0
  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  ALLOCATE(X_LOC(3,mecaEF(i)%N_NODE))
  X_LOC = 0.0D0

  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

    extP_lbl(1)=' '
    extP_val(1)=0.

  ENDIF
  ! 
  UMTT = (1.d0 - theta)

  !
  K=ZERO
  Fint=ZERO

  ! DA : Appel a la stabilisation de l'element 
  IF (mecaEF(i)%with_stab) THEN
  
      !DA :  on calcule la matrice de passage global local sur le point de Gauss central
      CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,1,mecaEF(i)%N_NODE,mecaEF(i)%PG(1)%DN, X, Ps, Ps_T)
      XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
      X_LOC = RESHAPE(source=XLOC,shape=(/ 3, mecaEF(i)%N_NODE/))
      ! DA : Calcul du gradient moyen dans l'element
      CALL MEAN_GRADIENT_SHB(i,X,VOL,BX0)
      !CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(get_N_GP_mecaEF_SHB(i) + 1)%DN, X_LOC,BX0)
      ! DA : Recuperation des fonctions de forme pour le hors plan
      CALL GAMMA_SHB(mecaEF(i)%N_NODE,mecaEF(i)%H,BX0,mecaEF(i)%Coef,X_LOC,GAMMA)
      ! DA : Calcul de la matrice Jacobienne dans le repere local
      CALL JACOBIEN_ISO3D(mecaEF(i)%PG(1)%DN,X_LOC,J,INVJ)
      ! DA : Calcul de la matrice gradient dans le repere local mode de deformation Coques
      CALL GRADIENT_STAB_SHB3D(mecaEF(i)%N_NODE,INVJ,VOL,GAMMA,DNX_STAB_1,DNX_STAB_2)
      ! DA : Assemblage de la matrice gradient de la stabilisation
      CALL Bl_STAB_SHB3D(mecaEF(i)%N_NODE,DNX_STAB_1, DNX_STAB_2,mecaEF(i)%Coef_C,1,Bl_STAB)
      ! DA : Desallocations des pointeurs
      DEALLOCATE(DNX_STAB_1,DNX_STAB_2); NULLIFY(DNX_STAB_1,DNX_STAB_2)
  ENDIF

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
    
    ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
    DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
    
       KLOC = ZERO
       FLOC = ZERO
       ULOC = ZERO
       XLOC = ZERO
       X_LOC = ZERO
       
       !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface coque
       CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
            
       ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
       XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
       X_LOC = RESHAPE(source=XLOC,shape=(/ 3, mecaEF(i)%N_NODE/))

       ! DA : Recuperation de la matrice gradient en 0,0,0
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(get_N_GP_mecaEF_SHB(i) + 1)%DN, X_LOC,BX0)
       ! DA : Recuperation des fonctions de forme pour le hors plan
       CALL GAMMA_SHB(mecaEF(i)%N_NODE,mecaEF(i)%H,BX0,mecaEF(i)%Coef,X_LOC,GAMMA)
       ! DA : Calcul de la matrice Jacobienne dans le repere local
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X_LOC,J,INVJ)
       ! DA : Calcul de la matrice gradient dans le repere local mode de deformation Coques
       CALL GRADIENT_SHB3D(mecaEF(i)%N_NODE,INVJ,mecaEF(i)%hPG(ig)%DN,BX0,GAMMA,DNX)

       ! on rapatrie les infos du debut de pas
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       FLUX1 = 0.D0
       GRAD1 = 0.D0
       IF (nb_internal /= 0 ) INTERNAL1 = 0.D0

       IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
   
         if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'elas_') then 
           call logmes('mater ='//get_eleop_value_bypps(ppsnb(ig),'mater'))
           call FATERR(IAM,'Using isext == no___ is only possible with mater == elas ')
         endif
   
         CALL D_SOLID_SHB(ppsnb(ig),D)
         
         istrg = 1
         ! DA : Formation de la matrice des deformations
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl)
         GRAD1 = MATMUL(Bl, ULOC)

         CALL comp_stress_shb(ppsnb(ig),GRAD1,FLUX1)
   
       ELSE
   
         ! on va utiliser la notion de field attache au model
   
         extP_nb = get_external_field_nb(mdlnb)
   
         IF (extP_nb /= 0) THEN 
   
           do if=1,extP_nb
             name=get_external_field_name(mdlnb,if)
             extP_lbl(if)=name
             extP_len(if)=len_trim(name)
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
   
             CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field)
   
             extP_val(if) = field
   
             if (trim(name) == 'TEMPERATURE') then
               Tref = get_Tref_meca(bhvnb)
               if (dabs(extP_val(if) - Tref) < 1e-10) extP_val(if) = Tref
             endif

           enddo
   
         ELSE
   
           extP_lbl(1)=' '
           extP_val(1)=0.
   
         ENDIF
   
         istrg = 2
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl)
         GRAD1 = MATMUL(Bl, ULOC)
   
         calcD=1
   
         CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                 GRAD0,FLUX0,INTERNAL0, &
                                 GRAD1,FLUX1,INTERNAL1, &
                                 D,dt,calcD)
   
       ENDIF 
       
       if (compute_stiffness) then
            ! Calcul de la matrice elementaire
            KLOC = mecaEF(i)%Coef_INT*(MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*J*mecaEF(i)%PG(ig)%POIDS)
            ! Ajout de la matrice de stabilisation
            IF ((mecaEF(i)%with_stab) .and. (IG == 1)) KLOC = KLOC + MATMUL(TRANSPOSE(Bl_STAB),MATMUL(D,Bl_STAB))
            ! Passage de la matrice integre dans l'epaisseur en coordonnes globale
            K = K + MATMUL(Ps_T,MATMUL(KLOC,Ps))
       endif
              
       if (compute_fint) then
            ! Calcul des efforts internes
            FLOC = mecaEF(i)%Coef_INT*(MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*J*mecaEF(i)%PG(ig)%POIDS)
            ! Ajout de la matrice de stabilisation
            IF ((mecaEF(i)%with_stab) .and. (IG == 1)) FLOC = FLOC + MATMUL(MATMUL(TRANSPOSE(Bl_STAB),MATMUL(D,Bl_STAB)),ULOC)
            ! Passage des efforts integre dans l'epaisseur en coordonnes globale
            Fint = Fint + MATMUL(Ps_T,FLOC)
       endif
   
       if (push_fields) then
             ! DA : Stockage des informations aux Points de Gauss
             CALL put_strain_MAILx(ibdyty,iblmty,ig,mecaEF(i)%Coef_INT*GRAD1) ! DA : On laisse les informations dans le repere local de la coque
             CALL put_stress_MAILx(ibdyty,iblmty,ig,mecaEF(i)%Coef_INT*FLUX1) ! DA : On laisse les informations dans le repere local de la coque
             
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(1,1))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(1,2))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(1,3))
             
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_x')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(2,1))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_y')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(2,2))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_z')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(2,3))
             
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(3,1))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(3,2))
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
             if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps_T(3,3))
             
             IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
       endif
       
       ! Pour tous les point de Gauss
       IG = IG + 1

     ENDDO
     
     ! Passage a une autre point de gauss
     
  ENDDO
  
  ! DA : Appel a la stabilisation de l'element 
  if (associated(Bl_STAB)) then
    deallocate(Bl_STAB)
    nullify(Bl_STAB)
  end if
  
  deallocate(extP_lbl,extP_len,extP_val)
  deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

  deallocate(DNX,Bl,InvJ); nullify(DNX,Bl,InvJ)
  deallocate(BX0,GAMMA)  ; nullify(BX0,GAMMA)
  deallocate(Ps,Ps_T)    ; nullify(Ps,Ps_T)

  deallocate(KLOC,FLOC,ULOC,XLOC,X_LOC)

END SUBROUTINE BULK_HPP_SHB

SUBROUTINE  Bl_SHB3D(N_NE,DNX,C,istrg,Bl)

! computes the Bl matrix (symmetric deformation) at a gauss point

!istrg == 1 => voigt (a verifier pour le 3D !!)
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   Bl est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                      0      dNI / dY       0                                 ! 
!                      0          0      dNI / dZ                              !
!                  dNI / dY   dNI / dX       0                                 !
!                      0      dNI / dZ   dNI / dY                              !
!                  dNI / dZ       0      dNI / dX  ]                           !
!------------------------------------------------------------------------------!


!istrg==2 => line by line and lower part (C-storage)!!
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   BI est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                  dNI / dY   dNI / dX       0                                 !
!                      0      dNI / dY       0                                 ! 
!                  dNI / dZ       0      dNI / dX                              !  
!                      0      dNI / dZ   dNI / dY                              !
!                      0          0      dNI / dZ ]                            !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE        ! number of nodes
REAL(KIND=8), POINTER       :: DNX(:,:)    ! array containing the derivatives of interpolation function at gauss point
REAL(KIND=8)                :: C           ! Coef_C assumed strain 
INTEGER                        :: istrg       ! the way you want to store the deformation voigt=1, stainier=2
REAL(KIND=8), POINTER       :: Bl(:,:)     ! 

! Variable locale
INTEGER                        :: I
                                      !1234567890123456789012 
CHARACTER(len=22)              :: IAM='a_mecaEF_SHB::Bl_SHB3d'

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(Bl)) THEN ; DEALLOCATE(Bl) ; NULLIFY(Bl) ; ENDIF

SELECT CASE (istrg)
CASE(1)
  ALLOCATE(Bl(6,3*N_NE))
  DO I=1,N_NE
     Bl(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
     Bl(2,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
     Bl(3,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
     Bl(4,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,  DNX(1,I)   ,   ZERO      /)
     Bl(5,3*(I-1)+1:3*I) = (/   ZERO     ,C*DNX(3,I)   ,C*DNX(2,I)   /)
     Bl(6,3*(I-1)+1:3*I) = (/C*DNX(3,I)  ,   ZERO      ,C*DNX(1,I)   /)
  ENDDO
CASE(2)
ALLOCATE(Bl(6,3*N_NE))
  DO I=1,N_NE
     Bl(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
     Bl(2,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,  DNX(1,I)   ,   ZERO      /)
     Bl(3,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
     Bl(4,3*(I-1)+1:3*I) = (/C*DNX(3,I)  ,   ZERO      ,C*DNX(1,I)   /)      
     Bl(5,3*(I-1)+1:3*I) = (/   ZERO     ,C*DNX(3,I)   ,C*DNX(2,I)   /)
     Bl(6,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
  ENDDO
CASE default
  CALL FATERR(IAM,'error allocating Bl')
END SELECT
 
END SUBROUTINE Bl_SHB3D

SUBROUTINE  Bl_STAB_SHB3D(N_NE,DNX_1,DNX_2,C,istrg,Bl)

! computes the Bl matrix (symmetric deformation) at a gauss point

!istrg == 1 => voigt (a verifier pour le 3D !!)
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   Bl est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                      0      dNI / dY       0                                 ! 
!                      0          0      dNI / dZ                              !
!                  dNI / dY   dNI / dX       0                                 !
!                      0      dNI / dZ   dNI / dY                              !
!                  dNI / dZ       0      dNI / dX  ]                           !
!------------------------------------------------------------------------------!


!istrg==2 => line by line and lower part (C-storage)!!
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   BI est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                  dNI / dY   dNI / dX       0                                 !
!                      0      dNI / dY       0                                 ! 
!                  dNI / dZ       0      dNI / dX                              !  
!                      0      dNI / dZ   dNI / dY                              !
!                      0          0      dNI / dZ ]                            !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE        ! number of nodes
REAL(KIND=8), POINTER       :: DNX_1(:,:)    ! array containing the derivatives of interpolation function at gauss point
REAL(KIND=8), POINTER       :: DNX_2(:,:)    ! array containing the derivatives of interpolation function at gauss point
REAL(KIND=8)                :: C           ! Coef_C assumed strain 
INTEGER                        :: istrg       ! the way you want to store the deformation voigt=1, stainier=2
REAL(KIND=8), POINTER       :: Bl(:,:)     ! 

! Variable locale
INTEGER                        :: I
                                      !123456789012345678901234567
CHARACTER(len=27)              :: IAM='a_mecaEF_SHB::Bl_STAB_SHB3d'

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(Bl)) THEN ; DEALLOCATE(Bl) ; NULLIFY(Bl) ; ENDIF

SELECT CASE (istrg)
CASE(1)
  ALLOCATE(Bl(6,3*N_NE))
  DO I=1,N_NE
     Bl(1,3*(I-1)+1:3*I) = (/  DNX_1(1,I) + DNX_2(1,I)  ,   ZERO                    ,   ZERO                    /)
     Bl(2,3*(I-1)+1:3*I) = (/   ZERO                    ,  DNX_1(2,I) + DNX_2(2,I)  ,   ZERO                    /)
     Bl(3,3*(I-1)+1:3*I) = (/   ZERO                    ,   ZERO                    ,  DNX_1(3,I) + DNX_2(3,I)  /)
     Bl(4,3*(I-1)+1:3*I) = (/  DNX_1(2,I) + DNX_2(2,I)  ,  DNX_1(1,I) + DNX_2(1,I)  ,   ZERO                    /)
     Bl(5,3*(I-1)+1:3*I) = (/   ZERO                    ,  DNX_1(3,I)+C*DNX_2(3,I)  ,  DNX_1(2,I)+C*DNX_2(2,I)  /)
     Bl(6,3*(I-1)+1:3*I) = (/  DNX_1(3,I)+C*DNX_2(3,I)  ,   ZERO                    ,  DNX_1(1,I)+C*DNX_2(1,I)  /)
  ENDDO
CASE(2)
ALLOCATE(Bl(6,3*N_NE))
  DO I=1,N_NE
     Bl(1,3*(I-1)+1:3*I) = (/  DNX_1(1,I) + DNX_2(1,I)  ,   ZERO                    ,   ZERO                    /)
     Bl(2,3*(I-1)+1:3*I) = (/  DNX_1(2,I) + DNX_2(2,I)  ,  DNX_1(1,I) + DNX_2(1,I)  ,   ZERO                    /)
     Bl(3,3*(I-1)+1:3*I) = (/   ZERO                    ,  DNX_1(2,I) + DNX_2(2,I)  ,   ZERO                    /)
     Bl(4,3*(I-1)+1:3*I) = (/  DNX_1(3,I)+C*DNX_2(3,I)  ,   ZERO                    ,  DNX_1(1,I)+C*DNX_2(1,I)  /)
     Bl(5,3*(I-1)+1:3*I) = (/   ZERO                    ,  DNX_1(3,I)+C*DNX_2(3,I)  ,  DNX_1(2,I)+C*DNX_2(2,I)  /)
     Bl(6,3*(I-1)+1:3*I) = (/   ZERO                    ,   ZERO                    ,  DNX_1(3,I) + DNX_2(3,I)  /)
  ENDDO
CASE default
  CALL FATERR(IAM,'error allocating Bl')
END SELECT
 
END SUBROUTINE Bl_STAB_SHB3D

!------------------------------------------------------------------------
SUBROUTINE gpv2node_3D_SHB(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain,        2==cauchy stress, 
!                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress
!                                        5==Internal variable,     6==Local Direction
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field,ii
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele
  REAL(kind=8),DIMENSION(:),allocatable :: GaussPointValues_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: NbNodes_stored,i_node

!                            1234567890123456789012345678901
  CHARACTER(len=31)  :: IAM='a_mecaEF_shb::gpv2node_3D_SHB'

  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),LOCALDIR(:,:),Field(:,:)     ! vecteur de travail local
  integer :: ig,if,inull,nb_external,nb_internal

  real(kind=8) :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

  ! TODO mettre ca dans l'element fini
  real(kind=8) :: f1,f2,f3,f4
  integer :: nbs,nbm,rank

  NbGp_ele =  get_N_GP_RIG_mecaEF_SHB(i)  
  NbNodes_ele = mecaEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele 

  if (fieldsize /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
  NodalValues = 0.d0

  if (NSTEP < 1) return

  allocate(GaussPointValues_ele(NbGp_ele))
  GaussPointValues_ele=0.d0

  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  allocate(Field(Fieldsize,NbGp_ele))
  Field = 0.d0

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))
  ALLOCATE(LOCALDIR(9,NbGp_ele))

  
  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx(ibdyty,iblmty,ig,GRAD(:,ig))
    
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))

    !fd initialisation a identite
    LOCALDIR(:,ig)=0.d0
    LOCALDIR(1,ig)=1.d0
    LOCALDIR(5,ig)=1.d0
    LOCALDIR(9,ig)=1.d0

 
    !fd au cas ou ca existe on recupere   
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(1,ig))
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(2,ig))    
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(3,ig))
    
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_x')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(4,ig))
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_y')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(5,ig))
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_z')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(6,ig))

    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(7,ig))    
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(8,ig))
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
    if (rank >0) CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,LOCALDIR(9,ig))

  enddo

  Select case(required_field)

    case(1) ! Euler Almansi Strain

       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         do ig=1,NbGp_ele
           Field(1:6,ig) = GRAD(1:6,ig) 
           FIELD(7,ig)= GRAD(1,ig) + GRAD(2,ig) + GRAD(3,ig)
         enddo
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           FF(:)=GRAD(:,ig)

           !fd calcul de F^-1 on stocke dans A33

           A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
           tmp=determinant33(A33)
           A33T=transpose(A33)             ! F

           call inverse33(A33T, inull)     !F^-1

           if (inull == 1) then
             print*,'Body ',ibdyty,' element ',iblmty,' gp ',ig
             print*,A33T(1,:)
             print*,A33T(2,:)
             print*,A33T(3,:)
             call faterr(IAM,'Non inversible F')
           endif


           A33=transpose(A33T)             !F^-T

           !fd almansi e = 0.5 (I - F^-T F^-1) = 0.5 (I - A33 A33T)
           !fd field e11 e12 e22 e13 e23 e33
           FIELD(1,ig)=0.5*(1.d0 - (A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)))
           FIELD(2,ig)=0.5*(     - (A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
           FIELD(3,ig)=0.5*(1.d0 - (A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)))
           FIELD(4,ig)=0.5*(     - (A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
           FIELD(5,ig)=0.5*(     - (A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
           FIELD(6,ig)=0.5*(1.D0 - (A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)))
           FIELD(7,ig)=tmp
         enddo
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

    case(3) ! Green Lagrange Strain
      
       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         do ig=1,NbGp_ele
           Field(1:6,ig) = GRAD(1:6,ig) 
           FIELD(7,ig)= GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
         enddo
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           FF(:)=GRAD(:,ig)

           !fd calcul de F^-1 on stocke dans A33

           A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
           tmp=determinant33(A33)
           A33T=transpose(A33)             ! F

           !da lagrange e = 0.5 (F^TF - I) = 0.5 (A33 A33T - I)
           !fd field e11 e12 e22 e13 e23 e33
           FIELD(1,ig)=0.5*((A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)) - 1.0)
           FIELD(2,ig)=0.5*((A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
           FIELD(3,ig)=0.5*((A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)) - 1.0)
           FIELD(4,ig)=0.5*((A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
           FIELD(5,ig)=0.5*((A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
           FIELD(6,ig)=0.5*((A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)) - 1.0)
           FIELD(7,ig)=tmp

         enddo
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF
    
    case(2) ! Cauchy Stress

       !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          do ig=1,NbGp_ele
            FIELD(1:6,ig) = FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           !fd cauchy s = J^-1 P F^T

           PP(:)=FLUX(:,ig); FF(:)=GRAD(:,ig)
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           A33T=reshape(FF,(/3,3/))         ! A33T=F^T car stockage C de FF


           tmp=determinant33(A33T)
           if (tmp /= 0.d0) tmp=1.d0/tmp 
 
           !fd cauchy s = J^-1 P F^T = tmp A33 A33T
           !fd field s11 s12 s22 s13 s23 s33

           FIELD(1,ig)=tmp*(A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1))
           FIELD(2,ig)=tmp*(A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2))
           FIELD(3,ig)=tmp*(A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2))
           FIELD(4,ig)=tmp*(A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3))
           FIELD(5,ig)=tmp*(A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3))
           FIELD(6,ig)=tmp*(A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3))

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       do ig=1,NbGp_ele
         tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
         PP(1) = FIELD(1,ig) - tmp
         PP(2) = FIELD(2,ig)
         PP(3) = FIELD(3,ig) - tmp
         PP(4) = FIELD(4,ig)
         PP(5) = FIELD(5,ig)
         PP(6) = FIELD(6,ig) - tmp

         FIELD(7,ig) = dsqrt(1.5*(PP(1)**2 + &
                                  PP(3)**2 + &
                                  PP(6)**2 + &
                                  (2.*(PP(2)**2 + &
                                       PP(4)**2 + &
                                       pp(5)**2 )))) 
       enddo
    
    case(4) ! Piola Kirchoff Stress
    
       !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          do ig=1,NbGp_ele
            FIELD(1:6,ig) = FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           PP(:)=FLUX(:,ig);
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           
           !da field e11 e12 e22 e13 e23 e33  
           FIELD(1,ig)=A33(1,1)
           FIELD(2,ig)=A33(1,2)
           FIELD(3,ig)=A33(2,2)
           FIELD(4,ig)=A33(1,3)
           FIELD(5,ig)=A33(2,3)
           FIELD(6,ig)=A33(3,3)

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       do ig=1,NbGp_ele
         tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
         PP(1) = FIELD(1,ig) - tmp
         PP(2) = FIELD(2,ig)
         PP(3) = FIELD(3,ig) - tmp
         PP(4) = FIELD(4,ig)
         PP(5) = FIELD(5,ig)
         PP(6) = FIELD(6,ig) - tmp

         FIELD(7,ig) = dsqrt(1.5*(PP(1)**2 + &
                                  PP(3)**2 + &
                                  PP(6)**2 + &
                                  (2.*(PP(2)**2 + &
                                       PP(4)**2 + &
                                       pp(5)**2 )))) 
       enddo
      
   case(5) ! DA : Internal variables 
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
        CALL LOGMES(IAM//' when smoothing internal variable only external models are supported ')
      ELSE
         do ig=1,NbGp_ele
            do ii=1,nb_internal
              Field(ii,ig) = INTERNAL(ii,ig) 
             enddo
         enddo
      ENDIF

    case(6) ! Local direction
        
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN

        FIELD = LOCALDIR

      ELSE
       !DA : ......
       do ig=1,NbGp_ele
         
         FIELD(1:3,ig) = LOCALDIR(1:3,ig)
         FIELD(4:6,ig) = LOCALDIR(4:6,ig)
         FIELD(7:9,ig) = LOCALDIR(7:9,ig)
         
       enddo
      ENDIF

    case default
      CALL FATERR(IAM,'Unsupported required_field : 1:: Almansi strain   , 2:: Cauchy stress, &
                                                    3:: Green Lag. Strain, 4:: PK1, &
                                                    5:: Internal variable, 6:: Local direction')
  endselect

  nbs=size(mecaEF(i)%gp2node,dim=1)
  nbm=size(mecaEF(i)%node2edge,dim=1)
  do if=1,fieldsize
     v_nodes(1:nbs) = matmul(mecaEF(i)%gp2node,Field(if,:))
     if (associated(mecaEF(i)%node2edge)) then
       v_nodes(nbs+1:nbs+nbm) = matmul(mecaEF(i)%node2edge,v_nodes(1:nbs)) 
     endif
     NodalValues(if,:) = NodalValues(if,:) + v_nodes(:)
  enddo

  deallocate(v_nodes,GRAD,FLUX,INTERNAL,LOCALDIR,GaussPointValues_ele,field)

END SUBROUTINE gpv2node_3D_SHB
! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_fields_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty)

   implicit none
   INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   REAL(KIND=LONG)                 :: X(:,:),U(:,:) ! coordonnees et deplacement des sommets
   real(kind=8)                    :: dt

                           !123456789012345678901234567890123456789012345678
   character(len=48):: IAM='a_meca_EF_SHB::compute_elementary_fiedls_SHB'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)

   select case(get_eleop_value(mdlnb,'kine_'))
   case('small')
     !all fields_hpp_SHB_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty)
     call fields_hpp_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty)
   case('large')
     call fields_gd_SHB_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty)
   case default
     call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
     call FATERR(IAM,'kinematic type unknown (small | large)')
   end select

END SUBROUTINE
!----------------------------------------------------------------------------!

SUBROUTINE fields_HPP_SHB_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty)

  IMPLICIT NONE

  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                       
  integer,dimension(:)           :: ppsnb

  ! coordonnees de départ,deplacement total
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U
  REAL(KIND=LONG),DIMENSION(3)   :: Xcenter ! coordonnees du centre de l'element
  !
  real(kind=8)                    :: dt

  ! variables locales
  REAL(KIND=LONG), POINTER       :: DNX(:,:),Bl(:,:)
  REAL(KIND=LONG), POINTER       :: Ps(:,:),InvJ(:,:),Ps_T(:,:)
  
  REAL(KIND=LONG)                :: J
  REAL(KIND=LONG),DIMENSION(3,3) :: tenseur3

  INTEGER                        :: IG,IG_S,IG_T
 
  INTEGER                        :: anisotropie,inode

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb
  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD0,FLUX0,INTERNAL0
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD1,FLUX1,INTERNAL1
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D
  ! coordonnee locale
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: XLOC
  
  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4        :: calcD
  
  INTEGER          :: istrg

                           !12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_mecaEF_SHB::fields_hpp_iso'

  ! Initialisation a vide des pointeurs
  nullify(DNX,Bl)
  nullify(Ps,InvJ)
     

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  
  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF


  ! DA : Calcul du centre de l'element pour le repere local
  Xcenter = ZERO
  DO Inode=1,mecaEF(i)%N_NODE
      Xcenter(1) = Xcenter(1) + X(1,Inode)
      Xcenter(2) = Xcenter(2) + X(2,Inode)
      Xcenter(3) = Xcenter(3) + X(3,Inode)
  ENDDO
  
  Xcenter = Xcenter / mecaEF(i)%N_NODE

  DO Inode=1,mecaEF(i)%N_NODE
      X(1,Inode) = X(1,Inode) - Xcenter(1)
      X(2,Inode) = X(2,Inode) - Xcenter(2)
      X(3,Inode) = X(3,Inode) - Xcenter(3)
  ENDDO

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
    
     ! Initialisation a vide des valeurs nodales
     ULOC = ZERO
     XLOC = ZERO
    
    !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface moyenne
    CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
    
    ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
    XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
    
    ! ATTENTION : X contient les coordonnees locales de l element
    X = RESHAPE(source = XLOC,shape=(/3, mecaEF(i)%N_NODE/))
    
    ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
    DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
       ! on rapatrie les infos du debut de pas
       
       ! DA : Recuperation de la matrice gradient sur le plan moyen de la coque
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, &
                           X,DNX)
        
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       IF (extP_nb /= 0) THEN 
   
         do if=1,extP_nb
           name=get_external_field_name(mdlnb,if)
           extP_lbl(if)=name
           extP_len(if)=len_trim(name)
           rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
           CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if))
   
         enddo
   
       ELSE
   
         extP_lbl(1)=' '
         extP_val(1)=0.
   
       ENDIF
       
       ! DA : On utilise les fonctions de forme SHB pour le gradient de surface
       !      
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X,J,INVJ)

    IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN

      istrg = 1
      CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl) ! formation de Bl 
                                
      GRAD1 = MATMUL(Bl,ULOC)

      CALL comp_stress(ppsnb(ig),GRAD1,FLUX1)

    ELSE
      !DA : Modification salvatrice a verifier ?
      !extP_nb = get_meca_field_size_MAILx(ibdyty,iblmty,ig)
      IF (extP_nb /= 0) THEN 

        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          extP_lbl(if)=name
          extP_len(if)=len_trim(name)

          rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if) )

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      istrg = 2
      CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl)    ! formation de Bl        

      GRAD1 = MATMUL(Bl,ULOC)
 
      calcD=0

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                               GRAD0,FLUX0,INTERNAL0, &
                               GRAD1,FLUX1,INTERNAL1, &
                               D,dt,calcD)


    ENDIF
   
       ! DA : On stocke les infos en coordonnees locales
       CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
       CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,3))
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,3))
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,3))
       
       IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
       
       ! Passage au prochain point de Gauss
       IG = IG + 1
   ENDDO
  ENDDO

  deallocate(extP_lbl,extP_len,extP_val)

  deallocate(DNX,Bl)      ; nullify(DNX,Bl)
  deallocate(Ps,Ps_T,InvJ); nullify(Ps,Ps_T,InvJ)

  deallocate(ULOC,XLOC)

  deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_HPP_SHB_ISO

SUBROUTINE fields_GD_SHB_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty)

  IMPLICIT NONE

  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                       
  integer,dimension(:)           :: ppsnb

  ! coordonnees de départ,deplacement total
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U
  REAL(KIND=LONG),DIMENSION(3)   :: Xcenter ! coordonnees du centre de l'element
  !
  real(kind=8)                    :: dt

  ! variables locales
  REAL(KIND=LONG), POINTER       :: B(:,:)
  REAL(KIND=LONG), POINTER       :: BX0(:,:),Ps(:,:),InvJ(:,:),Ps_T(:,:)
  
  REAL(KIND=LONG)                :: J
  REAL(KIND=LONG),DIMENSION(3,3) :: tenseur3

  INTEGER                        :: IG,IG_S,IG_T
 
  INTEGER                        :: anisotropie,inode

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb
  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD0,FLUX0,INTERNAL0
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD1,FLUX1,INTERNAL1
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D
  ! coordonnee locale
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: XLOC
  
  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4        :: calcD
  
  INTEGER          :: istrg

                           !12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_mecaEF_SHB::fields_gd_iso'

  ! Initialisation a vide des pointeurs
  nullify(BX0,B)
  nullify(Ps,InvJ)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  
  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF


  ! DA : Calcul du centre de l'element pour le repere local
  Xcenter = ZERO
  DO Inode=1,mecaEF(i)%N_NODE
      Xcenter(1) = Xcenter(1) + X(1,Inode)
      Xcenter(2) = Xcenter(2) + X(2,Inode)
      Xcenter(3) = Xcenter(3) + X(3,Inode)
  ENDDO
  
  Xcenter = Xcenter / mecaEF(i)%N_NODE

  DO Inode=1,mecaEF(i)%N_NODE
      X(1,Inode) = X(1,Inode) - Xcenter(1)
      X(2,Inode) = X(2,Inode) - Xcenter(2)
      X(3,Inode) = X(3,Inode) - Xcenter(3)
  ENDDO

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
    
     ! Initialisation a vide des valeurs nodales
     ULOC = ZERO
     XLOC = ZERO
    
    !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface moyenne
    CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
    
    ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
    XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
    
    ! ATTENTION : X contient les coordonnees locales de l element
    X = RESHAPE(source = XLOC,shape=(/3, mecaEF(i)%N_NODE/))
    
    ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
    DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
       ! on rapatrie les infos du debut de pas
       
       ! DA : Recuperation de la matrice gradient sur le plan moyen de la coque
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, &
                           X,BX0)
        
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       IF (extP_nb /= 0) THEN 
   
         do if=1,extP_nb
           name=get_external_field_name(mdlnb,if)
           extP_lbl(if)=name
           extP_len(if)=len_trim(name)
           rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
           CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if))
   
         enddo
   
       ELSE
   
         extP_lbl(1)=' '
         extP_val(1)=0.
   
       ENDIF
       
       ! DA : On utilise les fonctions de forme SHB pour le gradient de surface
       !      
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X,J,INVJ)

       CALL B_SHB3D(mecaEF(i)%N_NODE,BX0,mecaEF(i)%Coef_C,B)
   
   !
       IF (NSTEP == 1) THEN
         !fd a modifier avec la procedure d'initialisation de laurent
         FLUX0     = ZERO
         GRAD0     = Id3
         INTERNAL0 = 0.d0
         IF (nb_internal /= 0 .AND. SIZE(INTERNAL0) .GE. SIZE(Id3) ) THEN
           INTERNAL0(1:SIZE(Id3))=Id3
         ENDIF 
       ENDIF
   
       ! CALCUL DU GRADIENT DES DEFORMATIONS
       !*** Calcul de [1+d(u_n+1)/d(x_0)]
       
       
       GRAD1 = MATMUL(B,ULOC)
       !print *,'Body : ',ibdyty, ' calcul gradient SHB'
       !print *,'Gradient SHB : ',GRAD1(1:3)
       !print *,'             : ',GRAD1(4:6)
       !print *,'             : ',GRAD1(7:9)
       
       GRAD1 = Id3 + GRAD1
       
       FLUX1 = ZERO
       IF (nb_internal /= 0 ) INTERNAL1 = ZERO
       !
   
       calcD=0
       
       CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,dt,calcD)
   
       ! DA : On stocke les infos en coordonnees locales
       CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
       CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,3))
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,3))
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,3))
       
       IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
       
       ! Passage au prochain point de Gauss
       IG = IG + 1
   ENDDO
  ENDDO
  deallocate(extP_lbl,extP_len,extP_val)

  deallocate(BX0,B)       ; nullify(BX0,B)
  deallocate(Ps,Ps_T,InvJ); nullify(Ps,Ps_T,InvJ)

  deallocate(ULOC,XLOC)

  deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_GD_SHB_ISO

!----------------------------------------------------------------------------!
SUBROUTINE fields_GD_SHB(i,ppsnb,dt,X,U,ibdyty,iblmty)

  IMPLICIT NONE

  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                       
  integer,dimension(:)           :: ppsnb

  ! coordonnees de départ,deplacement total
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U
  !
  real(kind=8)                    :: dt

  ! variables locales
  REAL(KIND=LONG), POINTER       :: DNX(:,:),B(:,:)
  REAL(KIND=LONG), POINTER       :: GAMMA(:,:),BX0(:,:),Ps(:,:),InvJ(:,:),Ps_T(:,:)
  
  REAL(KIND=LONG)                :: J
  REAL(KIND=LONG),DIMENSION(3,3) :: tenseur3

  INTEGER                        :: IG,IG_S,IG_T
 
  INTEGER                        :: anisotropie,inode

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb
  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD0,FLUX0,INTERNAL0
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD1,FLUX1,INTERNAL1
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D
  ! coordonnee locale
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: XLOC
  REAL(KIND=LONG),DIMENSION(:,:),ALLOCATABLE :: X_LOC
  
  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4        :: calcD
  
  INTEGER          :: istrg

                           !12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_mecaEF_SHB::fields_gd_iso'

  ! Initialisation a vide des pointeurs
  nullify(BX0,GAMMA)
  nullify(Ps,Ps_T)
  nullify(INVJ,DNX,B)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  ALLOCATE(X_LOC(3,mecaEF(i)%N_NODE))
  X_LOC = 0.0D0
  
  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
  
     ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
     DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
    
       ULOC = ZERO
       XLOC = ZERO
       X_LOC = ZERO
    
       !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface coque
       CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
    
       ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
       XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
       X_LOC = RESHAPE(source=XLOC,shape=(/ 3, mecaEF(i)%N_NODE/))

       ! DA : Recuperation de la matrice gradient en 0,0,0
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(get_N_GP_mecaEF_SHB(i) + 1)%DN, X_LOC,BX0)
      
       ! DA : Recuperation des fonctions de forme pour le hors plan
       CALL GAMMA_SHB(mecaEF(i)%N_NODE,mecaEF(i)%H,BX0,mecaEF(i)%Coef,X_LOC,GAMMA)
       
       ! DA : Calcul de la matrice Jacobienne dans le repere local
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X_LOC,J,INVJ)
       
       ! DA : Calcul de la matrice gradient dans le repere local mode de deformation Coques
       CALL GRADIENT_SHB3D(mecaEF(i)%N_NODE,INVJ,mecaEF(i)%hPG(ig)%DN,BX0,GAMMA,DNX)
       
       ! on rapatrie les infos du debut de pas
       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       IF (extP_nb /= 0) THEN 
   
         do if=1,extP_nb
           name=get_external_field_name(mdlnb,if)
           extP_lbl(if)=name
           extP_len(if)=len_trim(name)
           rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
           CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if))
   
         enddo
   
       ELSE
   
         extP_lbl(1)=' '
         extP_val(1)=0.
   
       ENDIF
       
       ! DA : On utilise les fonctions de forme SHB pour le gradient de surface
       CALL B_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,B)
   
   !
       IF (NSTEP == 1) THEN
         !fd a modifier avec la procedure d'initialisation de laurent
         FLUX0     = ZERO
         GRAD0     = Id3
         INTERNAL0 = 0.d0
         IF (nb_internal /= 0 .AND. SIZE(INTERNAL0) .GE. SIZE(Id3) ) THEN
           INTERNAL0(1:SIZE(Id3))=Id3
         ENDIF 
       ENDIF
   
       ! CALCUL DU GRADIENT DES DEFORMATIONS
       !*** Calcul de [1+d(u_n+1)/d(x_0)]
       GRAD1 = MATMUL(B,ULOC)
       GRAD1 = Id3 + GRAD1
       
       FLUX1 = ZERO
       IF (nb_internal /= 0 ) INTERNAL1 = ZERO
       !
   
       calcD=0
       
       CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,dt,calcD)
   
       ! DA : On stocke les infos en coordonnees locales a la coque
       CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
       CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_X_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(1,3))
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Y_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(2,3))
       
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_x')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,1))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_y')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,2))
       rank = get_meca_field_rank_MAILx(ibdyty,iblmty,'VEC_Z_z')
       if (rank > 0) CALL set_meca_field_MAILx(ibdyty,iblmty,ig,rank,Ps(3,3))
       
       IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
       
       ! Passage au prochain point de Gauss
       IG = IG + 1
   ENDDO
  ENDDO

  deallocate(extP_lbl,extP_len,extP_val)

  deallocate(DNX,B,BX0)   ; nullify(DNX,B,BX0)
  deallocate(Ps,Ps_T,InvJ); nullify(Ps,Ps_T,InvJ)

  deallocate(ULOC,XLOC,X_LOC)

  deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_GD_SHB

!----------------------------------------------------------------------------!

!----------------------------------------------------------------------------!

SUBROUTINE fields_HPP_SHB (i,ppsnb,dt,X,U,ibdyty,iblmty)

  IMPLICIT NONE

  INTEGER         , INTENT(IN) :: I,ibdyty,iblmty  ! le numero de l'element 
  integer, dimension(:)        :: ppsnb
  REAL(KIND=LONG)              :: X(:,:)     ! coordonnees des sommets
  REAL(KIND=LONG)              :: U(:,:)     ! DDL elementaires
  real(kind=8)                 :: dt

  ! variables locales
  REAL(KIND=LONG) , POINTER    :: DNX(:,:), & !   
                                  Bl(:,:), &    ! matrice Bl  (epsilon=Bl q
                                  D(:,:)      ! matrice de comportement
                                  
  REAL(KIND=LONG), POINTER     :: GAMMA(:,:),BX0(:,:),Ps(:,:),InvJ(:,:),Ps_T(:,:)
                                  
  REAL(KIND=LONG)              :: J
  INTEGER                      :: IG,IG_T,IG_S,Inode

  INTEGER                      :: anisotropie
 
  INTEGER                      :: nb_external, nb_internal
  INTEGER                      :: mdlnb

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)    ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(kind=8),DIMENSION(:)    ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1
  REAL(KIND=8),DIMENSION(:,:)  ,ALLOCATABLE :: X_LOC

  INTEGER(kind=4)                      :: calcD

  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val
  ! coordonnee locale
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: ULOC
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: XLOC
  
  ! switch forme de B 
  INTEGER :: istrg,inull

  real(kind=8) :: UMTT,field,field_begin

  ! Initialisation a vide des pointeurs
  nullify(BX0,GAMMA)
  nullify(Ps,Ps_T)
  nullify(INVJ,DNX,Bl)
        
  umtt = (1.d0 - theta)

  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  IF (extP_nb /= 0) THEN 

    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(0,0))

  ALLOCATE(ULOC(3*mecaEF(i)%N_NODE))
  ULOC = 0.0D0
  ALLOCATE(XLOC(3*mecaEF(i)%N_NODE))
  XLOC = 0.0D0
  ALLOCATE(X_LOC(3,mecaEF(i)%N_NODE))
  X_LOC = 0.0D0

  IG = 1
  
  ! DA : Pour tous les points de Gauss qui porte la direction de la surface moyenne
  ! voir regle de quadrature compatible dans mod_a_EF.f90
  DO IG_S=1,mecaEF(i)%N_GP_RIG_SURFACE
    
    ! DA : Pour tous les points de Gauss qui porte l'epaisseur de la coque
    DO IG_T=1,mecaEF(i)%N_GP_RIG_THICKNESS
    
        ! Initialisation a vide des valeurs nodales
        ULOC = ZERO
        XLOC = ZERO
        X_LOC = ZERO
       
       !DA :  on calcule la matrice de passage global local sur le point de Gauss de la surface coque
       CALL MAT_PASSAGE_SHB3D(ibdyty,iblmty,IG_S,mecaEF(i)%N_NODE,mecaEF(i)%PG(IG)%DN, X, Ps, Ps_T)
    
       ULOC = MATMUL(Ps,RESHAPE(source=U,shape=(/ SIZE(U) /)))
       XLOC = MATMUL(Ps,RESHAPE(source=X,shape=(/ SIZE(X) /)))
       X_LOC = RESHAPE(source=XLOC,shape=(/ 3, mecaEF(i)%N_NODE/))

       ! DA : Recuperation de la matrice gradient en 0,0,0
       CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%PG(get_N_GP_mecaEF_SHB(i) + 1)%DN, X_LOC,BX0)
      
       ! DA : Recuperation des fonctions de forme pour le hors plan
       CALL GAMMA_SHB(mecaEF(i)%N_NODE,mecaEF(i)%H,BX0,mecaEF(i)%Coef,X_LOC,GAMMA)
       
       ! DA : Calcul de la matrice Jacobienne dans le repere local
       CALL JACOBIEN_ISO3D(mecaEF(i)%PG(IG)%DN,X_LOC,J,INVJ)
       
       ! DA : Calcul de la matrice gradient dans le repere local mode de deformation Coques
       CALL GRADIENT_SHB3D(mecaEF(i)%N_NODE,INVJ,mecaEF(i)%hPG(ig)%DN,BX0,GAMMA,DNX )
   
       ! on rapatrie les infos du debut de pas

       CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
       CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
       IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
   
       FLUX1 = 0.D0
       GRAD1 = 0.D0
       IF ( nb_internal /= 0 ) INTERNAL1 = 0.D0
       
       IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
   
         istrg = 1
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl) ! formation de Bl 
                                   
         GRAD1 = MATMUL(Bl,ULOC)
   
         CALL comp_stress_shb(ppsnb(ig),GRAD1,FLUX1)
   
       ELSE
         !DA : Modification salvatrice a verifier ?
         !extP_nb = get_meca_field_size_MAILx(ibdyty,iblmty,ig)
         IF (extP_nb /= 0) THEN 
   
           do if=1,extP_nb
             name=get_external_field_name(mdlnb,if)
             extP_lbl(if)=name
             extP_len(if)=len_trim(name)
   
             rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
             CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if) )
   
           enddo
   
         ELSE
   
           extP_lbl(1)=' '
           extP_val(1)=0.
   
         ENDIF
   
         istrg = 2
         CALL Bl_SHB3D(mecaEF(i)%N_NODE,DNX,mecaEF(i)%Coef_C,istrg,Bl)    ! formation de Bl        
   
         GRAD1 = MATMUL(Bl,ULOC)
    
         calcD=0
   
         CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                                  GRAD0,FLUX0,INTERNAL0, &
                                  GRAD1,FLUX1,INTERNAL1, &
                                  D,dt,calcD)

       ENDIF
   

       ! Passage au prochain point de Gauss
       IG = IG + 1
    
   ENDDO
  ENDDO

  deallocate(extP_lbl, extP_len, extP_val  )

  deallocate(Bl,BX0,DNX,GAMMA); nullify(Bl,BX0,DNX,GAMMA)
  deallocate(Ps,InvJ)         ; nullify(Ps,InvJ)

  deallocate(ULOC,XLOC,X_LOC)
  deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_HPP_SHB

!*************************************************************************
SUBROUTINE get_coor_pg_SHB(i,X,coor_pg)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i             ! le numero de l'element
REAL(KIND=8)              :: X(:,:)        ! coordonnees des sommets
REAL(KIND=8)              :: coor_pg(:,:)  ! coordonnees des points de Gauss
INTEGER                      :: ig,idime


 DO ig=1,get_N_GP_RIG_mecaEF_SHB(i)                 ! Pour tous les points de Gauss
   do idime=1,nbDIME
     coor_pg(idime,ig)=dot_product(mecaEF(i)%PG(ig)%N(:),X(idime,:))
   enddo
 ENDDO

END SUBROUTINE get_coor_pg_SHB

!------------------------------------------------------------------------
SUBROUTINE gpv_3D_SHB(i,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)

!fd routine qui recupere les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress
!                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress
!                                        5==Local coordinate z - local thickness direction and x - local direction
!                                        6==Internal variable
!  field           : the computed values
!  fieldsize       : number of components of the field 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: field
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: NbGp_ele

!                            1234567890123456789012345678
  CHARACTER(len=28)  :: IAM='a_mecaEF_SHB::gpv_3D_SHB'

  REAL(KIND=LONG) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:)     ! vecteur de travail local
  integer :: ig,inull,nb_external,nb_internal,ii

  real(kind=8) :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

  NbGp_ele =  get_N_GP_RIG_mecaEF_SHB(i)  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  if (fieldsize /= size(Field,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbGp_ele /= size(Field,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif

  Field = 0.d0

  if (NSTEP < 1) return

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  do ig=1,NbGp_ele
    CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

    case(1) ! strain

       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         do ig=1,NbGp_ele
           Field(:,ig) = GRAD(:,ig) 
         enddo
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           FF(:)=GRAD(:,ig)

           !fd calcul de F^-1 on stocke dans A33

           A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
           A33T=transpose(A33)             ! F

           call inverse33(A33T, inull)     !F^-1

           if (inull == 1) then
             print*,'Body ',ibdyty,' element ',iblmty,' gp ',ig
             print*,A33T(1,:)
             print*,A33T(2,:)
             print*,A33T(3,:)
             call faterr(IAM,'Non inversible F')
           endif


           A33=transpose(A33T)             !F^-T

           !fd almansi e = 0.5 (I - F^-T F^-1) = 0.5 (I - A33 A33T)
           !fd field e11 e12 e22 e13 e23 e33
           FIELD(1,ig)=0.5*(1.d0 - (A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)))
           FIELD(2,ig)=0.5*(     - (A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
           FIELD(3,ig)=0.5*(1.d0 - (A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)))
           FIELD(4,ig)=0.5*(     - (A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
           FIELD(5,ig)=0.5*(     - (A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
           FIELD(6,ig)=0.5*(1.D0 - (A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)))
         enddo
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

    case(3) ! Green Lagrange Strain
      
       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         do ig=1,NbGp_ele
           Field(1:6,ig) = GRAD(1:6,ig) 
           FIELD(7,ig)= GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
         enddo
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           FF(:)=GRAD(:,ig)

           !fd calcul de F^-1 on stocke dans A33

           A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
           tmp=determinant33(A33)
           A33T=transpose(A33)             ! F

           !da lagrange e = 0.5 (F^TF - I) = 0.5 (A33 A33T - I)
           !fd field e11 e12 e22 e13 e23 e33
           FIELD(1,ig)=0.5*((A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)) - 1.0)
           FIELD(2,ig)=0.5*((A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
           FIELD(3,ig)=0.5*((A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)) - 1.0)
           FIELD(4,ig)=0.5*((A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
           FIELD(5,ig)=0.5*((A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
           FIELD(6,ig)=0.5*((A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)) - 1.0)
           FIELD(7,ig)=tmp

         enddo
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

    case(2) ! stress

       !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          do ig=1,NbGp_ele
            FIELD(1:6,ig) = FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           !fd cauchy s = J^-1 P F^T

           PP(:)=FLUX(:,ig);FF(:)=GRAD(:,ig)
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           A33T=reshape(FF,(/3,3/))         ! A33T=F^T car stockage C de FF


           tmp=determinant33(A33T)
           if (tmp /= 0.d0) tmp=1.d0/tmp 
 
           !fd cauchy s = J^-1 P F^T = tmp A33 A33T
           !fd field s11 s12 s22 s13 s23 s33

           FIELD(1,ig)=tmp*(A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1))
           FIELD(2,ig)=tmp*(A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2))
           FIELD(3,ig)=tmp*(A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2))
           FIELD(4,ig)=tmp*(A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3))
           FIELD(5,ig)=tmp*(A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3))
           FIELD(6,ig)=tmp*(A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3))

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       do ig=1,NbGp_ele
         tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
         PP(1) = FIELD(1,ig) - tmp
         PP(2) = FIELD(2,ig)
         PP(3) = FIELD(3,ig) - tmp
         PP(4) = FIELD(4,ig)
         PP(5) = FIELD(5,ig)
         PP(6) = FIELD(6,ig) - tmp

         FIELD(7,ig) = dsqrt(1.5*(PP(1)**2 + &
                                  PP(3)**2 + &
                                  PP(6)**2 + &
                                  (2.*(PP(2)**2 + &
                                       PP(4)**2 + &
                                       pp(5)**2 )))) 
       enddo
      
    case(4) ! Piola Kirchoff Stress
    
       !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          do ig=1,NbGp_ele
            FIELD(1:6,ig) = FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           PP(:)=FLUX(:,ig);
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           
           !da field e11 e12 e22 e13 e23 e33  
           FIELD(1,ig)=A33(1,1)
           FIELD(2,ig)=A33(1,2)
           FIELD(3,ig)=A33(2,2)
           FIELD(4,ig)=A33(1,3)
           FIELD(5,ig)=A33(2,3)
           FIELD(6,ig)=A33(3,3)

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       do ig=1,NbGp_ele
         tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
         PP(1) = FIELD(1,ig) - tmp
         PP(2) = FIELD(2,ig)
         PP(3) = FIELD(3,ig) - tmp
         PP(4) = FIELD(4,ig)
         PP(5) = FIELD(5,ig)
         PP(6) = FIELD(6,ig) - tmp

         FIELD(7,ig) = dsqrt(1.5*(PP(1)**2 + &
                                  PP(3)**2 + &
                                  PP(6)**2 + &
                                  (2.*(PP(2)**2 + &
                                       PP(4)**2 + &
                                       pp(5)**2 )))) 
       enddo
    
    case(6) ! DA : Internal variables 
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
        !CALL LOGMES(IAM//' when smoothing internal variable only external models are supported ')
      ELSE
         do ig=1,NbGp_ele
            do ii=1,nb_internal
              Field(ii,ig) = INTERNAL(ii,ig) 
             enddo
         enddo
      ENDIF
    
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1::strain, 2::stress')
  endselect


  deallocate(GRAD,FLUX,INTERNAL)

END SUBROUTINE gpv_3D_SHB

!------------------------------------------------------------------------
SUBROUTINE gp_external_field_3D_SHB(i,ppsnb,ibdyty,iblmty,Field)

!fd routine qui recupere les valeurs aux points de gauss
!  i               : shell element id
!  mdlnb           : model
!  field           : the computed values

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: field
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: NbGp_ele,nbf,if,ig,rank,inull,nbi,iif
  !REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: INTERNAL
  character(len=30) :: name

!                            123456789012345678901234567890123456789012
  CHARACTER(len=42)  :: IAM='a_mecaEF_SHB::gp_external_field_3D_SHB'


  NbGp_ele =  get_N_GP_RIG_mecaEF_SHB(i)  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nbf = get_external_field_nb(mdlnb)
!~   nbi = get_nb_internal_variables(mdlnb)
  
  if (nbf /= size(Field,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbGp_ele /= size(Field,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
  
  
  !print *,'Nb fields : ',nbf
  do if = 1 , nbf 
    
    name=get_external_field_name(mdlnb,if)
    !print *,'Field name : ',name
    rank=get_meca_field_rank_MAILx(ibdyty,iblmty,name)
    do ig=1,NbGp_ele
      CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field(if,ig))
    enddo
  enddo
  
!~   ALLOCATE(INTERNAL(nbi))
!~   
!~   CALL get_internal_MAILx(ibdyty,iblmty,ig,INTERNAL)
!~   
!~   do if = 1 , nbi
!~     iif = if + nbf
!~     do ig=1,NbGp_ele
!~       field(if + nbf,ig) = INTERNAL(if)
!~     enddo
!~   enddo
  
end subroutine

!*************************************************************************
!*************************************************************************
SUBROUTINE interpolate_node2pg_SHB(i,valnoe,valpg)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i           ! le numero de l'element
REAL(KIND=8)              :: valnoe(:)   ! valeurs aux sommets
REAL(KIND=8)              :: valpg(:)    ! valeurs aux points de Gauss
INTEGER                      :: ig

 DO ig=1,get_N_GP_RIG_mecaEF_SHB(i)                 ! Pour tous les points de Gauss
   valpg(ig)=dot_product(mecaEF(i)%PG(ig)%N(:),valnoe(:))
 ENDDO

END SUBROUTINE interpolate_node2pg_SHB

!> \brief Get the number of dof of a node of an element
function get_N_DOF_of_NODE_mecaEF_shb(id, i_node)
  implicit none
  integer(kind=4), intent(in) :: id               !< [in] id of the mechanical iso element
  integer(kind=4), intent(in) :: i_node           !< [in] index of the node
  integer(kind=4) :: get_N_DOF_of_NODE_mecaEF_shb !< [return] number of dof in the element

  get_N_DOF_of_NODE_mecaEF_shb = mecaEF(id)%N_DOF_by_NODE
end function

!> \brief Get the number of dof in an element
function get_N_DOF_mecaEF_shb(id)
  implicit none
  integer(kind=4), intent(in) :: id       !< [in] id of the mechanical iso element
  integer(kind=4) :: get_N_DOF_mecaEF_shb !< [return] number of dof in the element

  get_N_DOF_mecaEF_shb =  mecaEF(id)%N_NODE * mecaEF(id)%N_DOF_by_NODE
end function

SUBROUTINE compute_elementary_volume_SHB(i,X,volume)

      IMPLICIT NONE
      
      ! le numero de l'element dans la liste locale
      INTEGER        , INTENT(IN) :: i 
      ! coordonnees des sommets
      REAL(KIND=LONG)             :: X(:,:)     
      ! volume
      REAL(KIND=LONG)             :: volume, J
      
      ! ***
      REAL(KIND=LONG)             :: COEFINT,R
      INTEGER                     :: IG
      REAL(KIND=LONG), POINTER    :: INVJ(:,:) ! derivée de N par rapport a X   
      
      
      NULLIFY(INVJ)
      volume = 0.d0
      DO IG=1,mecaEF(i)%N_GP_MAS     ! Pour tous les points de Gauss
      
         CALL JACOBIEN_ISO3D(mecaEF(i)%mPG(IG)%DN,X,J,INVJ)
         volume = volume + J*mecaEF(i)%Coef_INT*mecaEF(i)%PG(ig)%POIDS
      
      ENDDO
      
      DEALLOCATE(INVJ);NULLIFY(INVJ)

END SUBROUTINE

SUBROUTINE MEAN_GRADIENT_SHB(i,X,volume,B_MEAN)

      IMPLICIT NONE
      
      ! le numero de l'element dans la liste locale
      INTEGER        , INTENT(IN) :: i 
      ! coordonnees des sommets
      REAL(KIND=LONG)             :: X(:,:)     
      ! volume
      REAL(KIND=LONG)             :: volume, J
      
      ! ***
      REAL(KIND=LONG)             :: COEFINT,R
      INTEGER                     :: IG
      REAL(KIND=LONG), POINTER    :: INVJ(:,:), DNX(:,:), B_MEAN(:,:) ! derivée de N par rapport a X   
      
      IF(ASSOCIATED(B_MEAN)) THEN ; DEALLOCATE(B_MEAN) ; NULLIFY(B_MEAN) ; ENDIF
      ALLOCATE(B_MEAN(3,mecaEF(i)%N_NODE))
      
      NULLIFY(INVJ, DNX)
      volume = 0.d0
      B_MEAN = ZERO
      
      DO IG=1,mecaEF(i)%N_GP_MAS     ! Pour tous les points de Gauss
         
         ! DA : Calcul du volume
         CALL JACOBIEN_ISO3D(mecaEF(i)%mPG(IG)%DN,X,J,INVJ)
         
         volume = volume + J*mecaEF(i)%Coef_INT*mecaEF(i)%mPG(ig)%POIDS
         
         ! DA : Calcul du gradient
         CALL GRADIENT_ISO3D(mecaEF(i)%N_NODE,mecaEF(i)%mPG(IG)%DN, X, DNX)
         
         ! DA : Calcul du gradient moyen dans l element
         B_MEAN = B_MEAN + DNX
      
      ENDDO
      
      ! DA : Calcul du gradient moyen dans l element
      B_MEAN = B_MEAN/volume
      
      DEALLOCATE(INVJ, DNX);NULLIFY(INVJ, DNX)

    END SUBROUTINE
    
subroutine check_elementary_ppset_shb(i,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty

  !**
  integer                         :: ig  
  
  do IG=1,get_N_GP_RIG_mecaEF_SHB(i)

    IF (get_eleop_value_bypps(ppsnb(ig),'isext') /= 'no___') THEN
      call check_external_ppset(ppsnb(ig)) 
    endif
   
  enddo   
  
end subroutine check_elementary_ppset_shb

END MODULE a_mecaEF_SHB
