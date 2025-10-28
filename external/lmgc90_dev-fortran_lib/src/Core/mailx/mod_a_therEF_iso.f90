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
MODULE a_therEF_iso

use parameters

USE utilities
USE algebra
USE a_EF
USE a_MATRIX

USE bulk_behaviour

USE ExternalModelsHandler

USE models

USE MAILx

TYPE T_therEF_iso
   CHARACTER(len=5)        :: NAME
   INTEGER                 :: N_NODE
   INTEGER                 :: N_DOF_by_NODE
   integer(kind=4)         :: T_FONC_FORME
   INTEGER                 :: N_PG
   integer(kind=4)         :: SCH_GAUSS
   TYPE(T_PT_GAUSS),POINTER:: PG(:)
   
   ! da 19/06/12 mapping matrices gp -> node (noeuds sommets)
   real(kind=8),pointer    :: gp2node(:,:)
   ! da 19/06/12 mapping matrices node -> edge (noeuds cote)
   real(kind=8),pointer    :: node2edge(:,:) 
   
END TYPE T_therEF_iso 

TYPE(T_therEF_iso),DIMENSION(12),PRIVATE :: therEF

PRIVATE get_N_PG_therEF 
PUBLIC get_SCH_GAUSS_therEF , &
       get_gp_ptr_therEF_iso

! am: on rend publique la fonction renvoyant le type de fonction de 
!     forme associe a une element
!PRIVATE get_T_FONC_FORME_therEF 

CONTAINS

!============ some low level function ==================
!== you to create to construct the a_therEF data base ==
!

INTEGER FUNCTION get_nb_ele_iso(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_iso=SIZE(therEF)

END FUNCTION get_nb_ele_iso

SUBROUTINE init_therEF_iso
  IMPLICIT NONE
  logical :: is_initialize = .false.
  INTEGER :: itempo,i,j,errare
  REAL(kind=long),POINTER :: CG(:,:),POIDS_ELE(:)
!                           1234567890123456789012345678901
  CHARACTER(len=31) :: IAM='a_therEF_iso::init_therEF_iso'

  if( is_initialize ) return
!
  NULLIFY(CG,POIDS_ELE)
!
! EF 2D
!
! T3
  therEF(1)%NAME          ='T3xxx'
  therEF(1)%N_NODE        = 3
  therEF(1)%N_DOF_by_NODE = 1
  therEF(1)%T_FONC_FORME  = i_T_P1
  therEF(1)%N_PG          = 1
  therEF(1)%SCH_GAUSS     = i_TR01
! T6
  therEF(2)%NAME          ='T6xxx'
  therEF(2)%N_NODE        = 6
  therEF(2)%N_DOF_by_NODE = 1
  therEF(2)%T_FONC_FORME  = i_T_P1
  therEF(2)%N_PG          = 3
  therEF(2)%SCH_GAUSS     = i_TR03
! Q4
  therEF(3)%NAME          ='Q4xxx'
  therEF(3)%N_NODE        = 4
  therEF(3)%N_DOF_by_NODE = 1
  therEF(3)%T_FONC_FORME  = i_Q_P1
  therEF(3)%N_PG          = 4 
  therEF(3)%SCH_GAUSS     = i_Q2x2
! Q8
  therEF(4)%NAME          ='Q8xxx'
  therEF(4)%N_NODE        = 8
  therEF(4)%N_DOF_by_NODE = 1
  therEF(4)%T_FONC_FORME  = i_Q_P2
  therEF(4)%N_PG          = 9 
  therEF(4)%SCH_GAUSS     = i_Q3x3
! Q8R
  therEF(5)%NAME          ='Q8Rxx'
  therEF(5)%N_NODE        = 8
  therEF(5)%N_DOF_by_NODE = 1
  therEF(5)%T_FONC_FORME  = i_Q_P2
  therEF(5)%N_PG          = 4
  therEF(5)%SCH_GAUSS     = i_Q2x2
! 
! EF 3D VOL
!
! H8
  therEF(6)%NAME          ='H8xxx'
  therEF(6)%N_NODE        = 8
  therEF(6)%N_DOF_by_NODE = 1
  therEF(6)%T_FONC_FORME  = i_H_P1
  therEF(6)%N_PG          = 8
  therEF(6)%SCH_GAUSS     = i_H222
! H20
  therEF(7)%NAME          ='H20xx'
  therEF(7)%N_NODE        = 20
  therEF(7)%N_DOF_by_NODE = 1
  therEF(7)%T_FONC_FORME  = i_H_P2
  therEF(7)%N_PG          = 27 
  therEF(7)%SCH_GAUSS     = i_H333
! H20R
  therEF(8)%NAME          ='H20Rx'
  therEF(8)%N_NODE        = 20
  therEF(8)%N_DOF_by_NODE = 3
  therEF(8)%T_FONC_FORME  = i_H_P2
  therEF(8)%N_PG          = 8 
  therEF(8)%SCH_GAUSS     = i_H222
! TE4
  therEF(9)%NAME          ='TE4xx'
  therEF(9)%N_NODE        = 4
  therEF(9)%N_DOF_by_NODE = 1
  therEF(9)%T_FONC_FORME  = i_TEP1
  therEF(9)%N_PG          = 1
  therEF(9)%SCH_GAUSS     = i_TE01
! TE10
  therEF(10)%NAME          ='TE10x'
  therEF(10)%N_NODE        = 10
  therEF(10)%N_DOF_by_NODE = 1
  therEF(10)%T_FONC_FORME  = i_TEP2
  therEF(10)%N_PG          = 4
  therEF(10)%SCH_GAUSS     = i_TE04
! PRI6
  therEF(11)%NAME          ='PRI6x'
  therEF(11)%N_NODE        = 6
  therEF(11)%N_DOF_by_NODE = 1
  therEF(11)%T_FONC_FORME  = i_PRP1
  therEF(11)%N_PG          = 6
  therEF(11)%SCH_GAUSS     = i_PR06
! PRI15
  therEF(12)%NAME          ='PRI15'
  therEF(12)%N_NODE        = 15
  therEF(12)%N_DOF_by_NODE = 3
  therEF(12)%T_FONC_FORME  = i_PRP2
  therEF(12)%N_PG          = 6
  therEF(12)%SCH_GAUSS     = i_PR06

  DO i=1,SIZE(therEF)
    ALLOCATE(therEF(i)%PG(get_N_PG_therEF(i)),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating therm_ef%PG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_therEF(i),CG,POIDS_ELE)

    DO j=1,get_N_PG_therEF(i)

      therEF(i)%PG(j)%POIDS=POIDS_ELE(j)   

      NULLIFY(therEF(i)%PG(j)%N)
      CALL fonct_forme(get_T_FONC_FORME_therEF_iso(i),CG(:,j),therEF(i)%PG(j)%N)

      NULLIFY(therEF(i)%PG(j)%DN)
      CALL derive_forme(get_T_FONC_FORME_therEF_iso(i),CG(:,j),therEF(i)%PG(j)%DN)
    ENDDO
    
        ! concerning mapping gp -> node

    SELECT CASE(therEF(i)%NAME)
    CASE('T3xxx')
       allocate(therEF(i)%gp2node(therEF(i)%N_NODE,therEF(i)%N_PG))
       therEF(i)%gp2node = 1.d0
       nullify(therEF(i)%node2edge)
    CASE('Q4xxx','Q4P0x','H8xxx','TE4xx')        !les lineaires 
       allocate(therEF(i)%gp2node(therEF(i)%N_NODE,therEF(i)%N_PG))
       if (therEF(i)%N_NODE == therEF(i)%N_PG) then
         therEF(i)%gp2node = 0.d0
         call compute_gp2node(therEF(i)%T_FONC_FORME,therEF(i)%N_NODE, &
                              therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                              therEF(i)%gp2node)
       else
         if (therEF(i)%N_PG == 1) then
           therEF(i)%gp2node = 1.d0
         else
            call faterr(IAM,'Impossible')
         endif
       endif
       nullify(therEF(i)%node2edge)
    CASE('PRI6x')        !les lineaires 
       allocate(therEF(i)%gp2node(therEF(i)%N_NODE,therEF(i)%N_PG))
       therEF(i)%gp2node = 0.d0
       call compute_gp2node(therEF(i)%T_FONC_FORME,therEF(i)%N_NODE, &
                            therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                            therEF(i)%gp2node)
       nullify(therEF(i)%node2edge)
    CASE('T6xxx')
       allocate(therEF(i)%gp2node(3,therEF(i)%N_PG))
       call compute_gp2node(i_T_P1, 3, &
                            therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                            therEF(i)%gp2node)
       allocate(therEF(i)%node2edge(therEF(i)%N_NODE-3,3))
       therEF(i)%node2edge=0.d0
       therEF(i)%node2edge(1,1)=0.5d0;therEF(i)%node2edge(1,2)=0.5d0
       therEF(i)%node2edge(2,2)=0.5d0;therEF(i)%node2edge(2,3)=0.5d0
       therEF(i)%node2edge(3,3)=0.5d0;therEF(i)%node2edge(3,1)=0.5d0
    CASE('Q8xxx','Q8Rxx')
       allocate(therEF(i)%gp2node(4,therEF(i)%N_PG))
       call compute_gp2node(i_Q_P1, 4, &
                            therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                            therEF(i)%gp2node)
       allocate(therEF(i)%node2edge(therEF(i)%N_NODE-4,4))
       therEF(i)%node2edge=0.d0
       therEF(i)%node2edge(1,1)=0.5d0;therEF(i)%node2edge(1,2)=0.5d0
       therEF(i)%node2edge(2,2)=0.5d0;therEF(i)%node2edge(2,3)=0.5d0
       therEF(i)%node2edge(3,3)=0.5d0;therEF(i)%node2edge(3,4)=0.5d0
       therEF(i)%node2edge(4,4)=0.5d0;therEF(i)%node2edge(4,1)=0.5d0
    CASE('H20xx','H20Rx')
       allocate(therEF(i)%gp2node(8,therEF(i)%N_PG))
       call compute_gp2node(i_H_P1, 8, &
                            therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                            therEF(i)%gp2node)
       allocate(therEF(i)%node2edge(therEF(i)%N_NODE-8,8))
       therEF(i)%node2edge=0.d0
       therEF(i)%node2edge(1,1)=0.5d0;therEF(i)%node2edge(1,2)=0.5d0
       therEF(i)%node2edge(2,2)=0.5d0;therEF(i)%node2edge(2,3)=0.5d0
       therEF(i)%node2edge(3,3)=0.5d0;therEF(i)%node2edge(3,4)=0.5d0
       therEF(i)%node2edge(4,4)=0.5d0;therEF(i)%node2edge(4,1)=0.5d0
       therEF(i)%node2edge(5,5)=0.5d0;therEF(i)%node2edge(5,6)=0.5d0
       therEF(i)%node2edge(6,6)=0.5d0;therEF(i)%node2edge(6,7)=0.5d0
       therEF(i)%node2edge(7,7)=0.5d0;therEF(i)%node2edge(7,8)=0.5d0
       therEF(i)%node2edge(8,8)=0.5d0;therEF(i)%node2edge(8,5)=0.5d0
       therEF(i)%node2edge(9,1)=0.5d0;therEF(i)%node2edge(9,5)=0.5d0
       therEF(i)%node2edge(10,2)=0.5d0;therEF(i)%node2edge(10,6)=0.5d0
       therEF(i)%node2edge(11,3)=0.5d0;therEF(i)%node2edge(11,7)=0.5d0
       therEF(i)%node2edge(12,4)=0.5d0;therEF(i)%node2edge(12,8)=0.5d0
    CASE('TE10x')
       allocate(therEF(i)%gp2node(4,therEF(i)%N_PG))
       call compute_gp2node(i_TEP1, 4, &
                            therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                            therEF(i)%gp2node)
       allocate(therEF(i)%node2edge(therEF(i)%N_NODE-4,4))
       therEF(i)%node2edge=0.d0
       therEF(i)%node2edge(1,1)=0.5d0;therEF(i)%node2edge(1,2)=0.5d0
       therEF(i)%node2edge(2,2)=0.5d0;therEF(i)%node2edge(2,3)=0.5d0
       therEF(i)%node2edge(3,3)=0.5d0;therEF(i)%node2edge(3,1)=0.5d0
       therEF(i)%node2edge(4,1)=0.5d0;therEF(i)%node2edge(4,4)=0.5d0
       therEF(i)%node2edge(5,2)=0.5d0;therEF(i)%node2edge(5,4)=0.5d0
       therEF(i)%node2edge(6,3)=0.5d0;therEF(i)%node2edge(6,4)=0.5d0
    CASE('PRI15')
       allocate(therEF(i)%gp2node(6,therEF(i)%N_PG))
       call compute_gp2node(i_PRp1, 6, &
                            therEF(i)%SCH_GAUSS,therEF(i)%N_PG, &
                            therEF(i)%gp2node)
       allocate(therEF(i)%node2edge(therEF(i)%N_NODE-6,6))
       therEF(i)%node2edge=0.d0
       therEF(i)%node2edge(1,1)=0.5d0;therEF(i)%node2edge(1,2)=0.5d0
       therEF(i)%node2edge(2,2)=0.5d0;therEF(i)%node2edge(2,3)=0.5d0
       therEF(i)%node2edge(3,3)=0.5d0;therEF(i)%node2edge(3,1)=0.5d0
       therEF(i)%node2edge(4,4)=0.5d0;therEF(i)%node2edge(4,5)=0.5d0
       therEF(i)%node2edge(5,5)=0.5d0;therEF(i)%node2edge(5,6)=0.5d0
       therEF(i)%node2edge(6,6)=0.5d0;therEF(i)%node2edge(6,4)=0.5d0
       therEF(i)%node2edge(7,1)=0.5d0;therEF(i)%node2edge(7,4)=0.5d0
       therEF(i)%node2edge(8,2)=0.5d0;therEF(i)%node2edge(8,5)=0.5d0
       therEF(i)%node2edge(9,3)=0.5d0;therEF(i)%node2edge(9,6)=0.5d0
    CASE DEFAULT
      print*,i,therEF(i)%NAME
      call FATERR(IAM,'gp2node can t be computed for this element')
    END SELECT
    
  ENDDO 

  !do i=1,SIZE(therEF)
  !  print*,therEF(i)%name
  !  print*,'gp2node'
  !  print*,therEF(i)%gp2node
  !  if (associated(therEF(i)%node2edge)) then
  !     print*,'node2edge'
  !     print*,therEF(i)%node2edge
  !  endif
  !enddo

  if( associated(CG) ) deallocate(CG)
  if( associated(POIDS_ELE) ) deallocate(POIDS_ELE)

  is_initialize = .true.

END SUBROUTINE init_therEF_iso

CHARACTER(len=5) FUNCTION get_NAME_therEF_iso(nb)
  IMPLICIT NONE
  INTEGER :: nb
  get_NAME_therEF_iso=therEF(nb)%NAME
END FUNCTION get_NAME_therEF_iso

INTEGER FUNCTION get_N_DOF_by_NODE_therEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_therEF_iso=therEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_therEF_iso

INTEGER FUNCTION get_N_NODE_therEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_therEF_iso=therEF(nb)%N_NODE

END FUNCTION get_N_NODE_therEF_iso


INTEGER FUNCTION get_bw_therEF_iso(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,i,j,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_therEF_iso=0
  DO i=1,therEF(nb)%N_NODE-1
    DO j=i+1,therEF(nb)%N_NODE
      tempo = ABS(nodes(i) - nodes(j))
      IF (tempo .GT. get_bw_therEF_iso) get_bw_therEF_iso=tempo
    ENDDO
  ENDDO

  get_bw_therEF_iso = (get_bw_therEF_iso+1)*therEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_therEF_iso

INTEGER FUNCTION get_N_GP_therEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_therEF_iso=therEF(nb)%N_PG

END FUNCTION get_N_GP_therEF_iso

INTEGER FUNCTION get_N_PG_therEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_therEF=therEF(nb)%N_PG

END FUNCTION get_N_PG_therEF

integer(kind=4) FUNCTION get_SCH_GAUSS_therEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_therEF=therEF(nb)%SCH_GAUSS

END FUNCTION get_SCH_GAUSS_therEF

integer(kind=4) FUNCTION get_T_FONC_FORME_therEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_T_FONC_FORME_therEF_iso=therEF(nb)%T_FONC_FORME

END FUNCTION get_T_FONC_FORME_therEF_iso

!=========================================================
!============ PARTIE ISSUE DE EVE 3.1 ====================
!=========================================================

SUBROUTINE GRADIENT_ISO(N_NE,N,DN,POIDS,X,DNX,COEFINT,R)

!fd gradient 


 IMPLICIT NONE

 INTEGER        , INTENT(IN)  :: N_NE
 REAL(KIND=LONG), INTENT(IN)  :: N(:),DN(:,:)
 REAL(KIND=LONG), INTENT(IN)  :: POIDS

 REAL(KIND=LONG), INTENT(IN)  :: X(:,:)

 REAL(KIND=LONG), POINTER     :: DNX(:,:)
 REAL(KIND=LONG), INTENT(OUT) :: COEFINT
 REAL(KIND=LONG), INTENT(OUT) :: R

! Variables locales
 INTEGER                      :: I
 REAL(KIND=LONG)              :: DETJ
 REAL(KIND=LONG), ALLOCATABLE :: J(:,:),INVJ(:,:)

 character(len=28) :: IAM
       !12345678912345678912345678
 IAM = 'a_therEF_iso::GRADIENT_ISO'

! Initialisation a vide des nouveaux pointeurs

IF(ASSOCIATED(DNX)) THEN ; DEALLOCATE(DNX) ; NULLIFY(DNX) ; ENDIF

SELECT CASE(DIME_mod)
 CASE(i_2D_strain,i_2D_stress,i_2D_axisym )

   ALLOCATE(J(2,2),INVJ(2,2))
   ALLOCATE(DNX(2,N_NE))
   DNX=ZERO
 
   J=ZERO

   ! Calcul de la matrice Jacobienne, de son determinant, et de son inverse   
   J(1,:)=(/ DOT_PRODUCT(DN(1,:),X(1,:)) , DOT_PRODUCT(DN(1,:),X(2,:)) /)
   J(2,:)=(/ DOT_PRODUCT(DN(2,:),X(1,:)) , DOT_PRODUCT(DN(2,:),X(2,:)) /)                
   DETJ=J(1,1)*J(2,2)-J(1,2)*J(2,1)
   IF(ABS(DETJ) < 1.E-10) call faterr(IAM,'Matrice Jacobienne non inv. ')
   INVJ(1,:)=(/  J(2,2)/DETJ ,-J(1,2)/DETJ /)
   INVJ(2,:)=(/ -J(2,1)/DETJ , J(1,1)/DETJ /)
   COEFINT=DETJ*POIDS
   
 CASE(i_3D)

   ALLOCATE(J(3,3),INVJ(3,3))
   ALLOCATE(DNX(3,N_NE)) 
   DNX=ZERO

   ! Calcul de la matrice Jacobienne, de son determinant, et de son inverse
   J(1,:)=(/DOT_PRODUCT(DN(1,:),X(1,:)),DOT_PRODUCT(DN(1,:),X(2,:)),DOT_PRODUCT(DN(1,:),X(3,:)) /)
   J(2,:)=(/DOT_PRODUCT(DN(2,:),X(1,:)),DOT_PRODUCT(DN(2,:),X(2,:)),DOT_PRODUCT(DN(2,:),X(3,:)) /)
   J(3,:)=(/DOT_PRODUCT(DN(3,:),X(1,:)),DOT_PRODUCT(DN(3,:),X(2,:)),DOT_PRODUCT(DN(3,:),X(3,:)) /)               
   DETJ=  J(1,1)*(J(2,2)*J(3,3)-J(2,3)*J(3,2)) &
         -J(1,2)*(J(2,1)*J(3,3)-J(3,1)*J(2,3)) &
         +J(1,3)*(J(2,1)*J(3,2)-J(3,1)*J(2,2))     
   IF(ABS(DETJ).LT.1.E-10) call faterr(IAM,' Mat. Jacob. non inversible ')
   
   !da 28/06/12 attention il s'agit en fait de sa transposee
   !INVJ(1,1)= (J(2,2)*J(3,3)-J(3,2)*J(2,3))/DETJ
   !INVJ(1,2)=-(J(2,1)*J(3,3)-J(3,1)*J(2,3))/DETJ
   !INVJ(1,3)= (J(2,1)*J(3,2)-J(3,1)*J(2,2))/DETJ
   !INVJ(2,1)=-(J(1,2)*J(3,3)-J(3,2)*J(1,3))/DETJ
   !INVJ(2,2)= (J(1,1)*J(3,3)-J(3,1)*J(1,3))/DETJ
   !INVJ(2,3)=-(J(1,1)*J(3,2)-J(3,1)*J(1,2))/DETJ
   !INVJ(3,1)= (J(1,2)*J(2,3)-J(2,2)*J(1,3))/DETJ
   !INVJ(3,2)=-(J(1,1)*J(2,3)-J(2,1)*J(1,3))/DETJ
   !INVJ(3,3)= (J(1,1)*J(2,2)-J(2,1)*J(1,2))/DETJ
   
   INVJ(1,1)= (J(2,2)*J(3,3)-J(3,2)*J(2,3))/DETJ
   INVJ(2,1)=-(J(2,1)*J(3,3)-J(3,1)*J(2,3))/DETJ
   INVJ(3,1)= (J(2,1)*J(3,2)-J(3,1)*J(2,2))/DETJ
   INVJ(1,2)=-(J(1,2)*J(3,3)-J(3,2)*J(1,3))/DETJ
   INVJ(2,2)= (J(1,1)*J(3,3)-J(3,1)*J(1,3))/DETJ
   INVJ(3,2)=-(J(1,1)*J(3,2)-J(3,1)*J(1,2))/DETJ
   INVJ(1,3)= (J(1,2)*J(2,3)-J(2,2)*J(1,3))/DETJ
   INVJ(2,3)=-(J(1,1)*J(2,3)-J(2,1)*J(1,3))/DETJ
   INVJ(3,3)= (J(1,1)*J(2,2)-J(2,1)*J(1,2))/DETJ
   
   COEFINT=DETJ*POIDS
     
END SELECT

! Calcul de la matrice des gradients au points (x,y)
DO I=1,N_NE
   DNX(:,I)=MATMUL(INVJ,DN(:,I))
ENDDO

! Calcul du rayon dans le cas axisymetrique
IF (DIME_mod == i_2D_axisym ) THEN

   ! Calcul du rayon
   R=DOT_PRODUCT(N,X(1,:))

   COEFINT=COEFINT*(6.2831853_LONG)*R

ENDIF

DEALLOCATE(J,INVJ)

END SUBROUTINE GRADIENT_ISO

!============                    ====================

!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   BI est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BlI = [ dNI / dX                                                    !
!                  dNI / dY                                                    !
!                      0     ]                                                 !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX                                                    !
!                  dNI / dY                                                    !
!                  dNI / dZ ]                                                  !
!------------------------------------------------------------------------------!

!fd Bl est une matrice de taille 3 sur dim=1, 
!   ce qui est consistant avec les tailles de grad et flux tjs a 3 

SUBROUTINE  Bl_ISO(N_NE,N,DNX,Bl)
IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: DNX(:,:)
REAL(KIND=LONG), POINTER       :: Bl(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(Bl)) THEN ; DEALLOCATE(Bl) ; NULLIFY(Bl) ; ENDIF

SELECT CASE(nbDIME)

 CASE(2)
   ALLOCATE(Bl(3,N_NE))
   DO I=1,N_NE
      Bl(1,I) = DNX(1,I)
      Bl(2,I) = DNX(2,I)
      Bl(3,I) = ZERO
   ENDDO

 CASE(3)
   ALLOCATE(Bl(3,N_NE))
   DO I=1,N_NE
      Bl(1,I) = DNX(1,I)
      Bl(2,I) = DNX(2,I)
      Bl(3,I) = DNX(3,I)
   ENDDO
 CASE DEFAULT
   call faterr('a_therEF_iso::Bl','not supported dimension (must be 2 or 3)')
END SELECT
 
END SUBROUTINE Bl_ISO

!============                    ====================
!------------------------------------------------------------------------------!
!    Calcul de la conductivite elementaire  [Ke]=Sum [Bl]t[D][Bl]µi                !
!------------------------------------------------------------------------------!

SUBROUTINE CONDUCTIVITY_ISO(i,ppsnb,dt,X,T,ibdyty,iblmty, &
                            Fint,need_Fint,K,need_K,push_f)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i ! le numero de l'element dans la liste locale
INTEGER                     :: ibdyty,iblmty
REAL(KIND=LONG)             :: X(:,:),T(:) ! coordonnees des sommets, valeur de la variable scalaire
real(kind=long)             :: dt            ! not used yet, but will be with matlib
REAL(KIND=LONG)             :: K(:,:)        ! matrice de rig. elem.
REAL(KIND=LONG),DIMENSION(:):: Fint
logical :: need_Fint, need_K, push_f

! variables locales
REAL(KIND=LONG), POINTER    :: DNX(:,:), & !   
                               Bl(:,:),  & ! matrice Bl
                               D(:,:)      ! matrice de comportement

REAL(KIND=LONG)             :: COEFINT,R,coco,Tg

INTEGER                     :: IG,idim,mdlnb,lawnb
INTEGER                     :: nb_external, nb_internal

logical :: is_coco_field

integer,dimension(:) :: ppsnb

! zone de stockage: gradient,flux,internal,operateur tangent

REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

! parametres externes
! gestion de multiples field|extP pour couplage
integer           :: if,rank
character(len=30) :: name
INTEGER*4                                    :: extP_nb
CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
INTEGER*4        , DIMENSION(:), allocatable :: extP_len
REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val
                        !1234567890123456789012345678901
character(len=31):: IAM='a_ther_EF_iso::CONDUCTIVITY_ISO'
real(kind=8) :: field,field_begin
! demande calcul matrice tangente
INTEGER(kind=4) :: calcD

! Pour faire tourner le poreux
logical :: a_recoder

! Initialisation a vide des pointeurs
NULLIFY(Bl,D,DNX)
is_coco_field=.false.
a_recoder=.true.

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

if (get_coco_type(lawnb) .ne. 0) is_coco_field=.true.

if (a_recoder) then
  nb_external = 3
  nb_internal = 0
else
  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)
endif

ALLOCATE(GRAD0(nb_external),GRAD1(nb_external), &
         FLUX0(nb_external),FLUX1(nb_external))
ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))

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

if( need_K    ) K    = ZERO
if( need_Fint ) Fint = ZERO

DO IG=1,therEF(i)%N_PG     ! Pour tous les points de Gauss

    ! on rapatrie les infos du debut de pas
    CALL get_flux_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_grad_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF (nb_internal /= 0 ) INTERNAL1 = 0.D0
    
    CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

    CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl) ! formation de Bl 

    GRAD1 = MATMUL(Bl,T)

    !IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
    if( a_recoder ) then
     
      if (is_coco_field) then

          rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'COCO')
          CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,coco)

          CALL COCO_ISO(ppsnb(ig),D,coco)
          !print *,'Utilisation conduction non lineaire : ',coco
          CALL comp_flux(ppsnb(ig),GRAD1,FLUX1,coco)
      else
          CALL COCO_ISO(ppsnb(ig),D)
          CALL comp_flux(ppsnb(ig),GRAD1,FLUX1)
      endif

      !print *,' Matrice de conduction LMGC90 : ',D(1,1),D(1,2),D(1,3)
      !print *,' ---------------------------- : ',D(2,1),D(2,2),D(2,3)
      !print *,' ---------------------------- : ',D(3,1),D(3,2),D(3,3)
     
   
    ELSE
      ! on va utiliser la notion de field attache au model
      extP_nb = get_external_field_nb(mdlnb)

      IF (extP_nb /= 0) THEN 

        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          extP_lbl(if)=name
          extP_len(if)=len_trim(name)
          rank = get_ther_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,field)
          extP_val(if) = field

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      calcD = 0
      if (need_K) calcD = 1

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                              GRAD0,FLUX0,INTERNAL0, &
                              GRAD1,FLUX1,INTERNAL1, &
                              D,H,calcD)
                              
      !print *,' Matrice de conduction Matlib : ',D(1,1),D(1,2),D(1,3)
      !print *,' ---------------------------- : ',D(2,1),D(2,2),D(2,3)
      !print *,' ---------------------------- : ',D(3,1),D(3,2),D(3,3)
      
      !print *,'Flux de conduction matlib : ',FLUX1
      
    endif

    !  ke= Blt.D.Bl.coef
    if( need_K   ) K    = K + MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*COEFINT
    if( need_Fint) Fint = Fint + (MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*COEFINT)

    if (push_f) then
      call put_grad_MAILx(ibdyty,iblmty,ig,GRAD1)
      call put_flux_MAILx(ibdyty,iblmty,ig,FLUX1)
      if (nb_internal /= 0 ) call put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
    end if

ENDDO

deallocate(extP_lbl,extP_len,extP_val)

deallocate(Bl,D,DNX) ;  nullify(Bl,D,DNX)
deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)

END SUBROUTINE CONDUCTIVITY_ISO

!> \brief Compute elementary conductivity matrix for an element
!> API compatible with simulation module
subroutine conductivity_iso2(i,ppsnb,dt,X,T,fields,map,names,F,K)
  implicit none
  !> [in] number of the element in the local list
  integer(kind=4),                   intent(in)  :: i
  !> [in] property set
  integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb
  !> [in] time differential (matlib use...)
  real(kind=8),                      intent(in)  :: dt
  !> [in] coordinates of the vertices of the element
  real(kind=8),      dimension(:,:), intent(in)  :: X
  !> [in] temperature on the vertices of the element
  real(kind=8),      dimension(:),   intent(in)  :: T
  !> [in] map to acces quadrature point fields
  integer(kind=4),   dimension(:,:), intent(in)  :: map
  !> [in] map to get quadrature fields name
  character(len=30), dimension(:)  , intent(in)  :: names
  !> [in] fields values at quadrature points
  real(kind=8),      dimension(:)  , intent(in)  :: fields
  !> [out] internal flux elementary matrix
  real(kind=8),      dimension(:),   intent(out) :: F
  !> [out] conductivity elementary matrix
  real(kind=8),      dimension(:,:), intent(out) :: K
  !
  real(kind=8), pointer :: DNX(:,:), & !   
                           Bl(:,:),  & ! Bl matrix
                           D(:,:)      ! behaviour matrix
  real(kind=8)    :: coefint, R, coco
  integer(kind=4) :: ig, idim, i_coco
  logical         :: is_coco_field

  integer(kind=4) :: mdlnb, lawnb

  ! for external field management
  integer(kind=4) :: nb_external, nb_internal
  real(kind=8), dimension(:), allocatable :: GRAD0,FLUX0,INTERNAL0
  real(kind=8), dimension(:), allocatable :: GRAD1,FLUX1,INTERNAL1
  integer(kind=4)   :: if,rank
  character(len=30) :: name
  integer(kind=4)                              :: extP_nb
  character(len=30), dimension(:), allocatable :: extP_lbl
  integer(kind=4)  , dimension(:), allocatable :: extP_len
  real(kind=8)     , dimension(:), allocatable :: extP_val
  !
  integer(kind=4)   :: calcD
  character(len=80) :: cout
  character(len=31) :: IAM
  !1234567890123456789012345678901
  IAM = 'a_ther_EF_iso::conductivity_iso2'

  call get_ppset_value(ppsnb(1),mdlnb,lawnb)

  nullify(Bl,D,DNX)
  
  allocate(D(3,3))
  D    = 0.D0
  coco = 0.d0
  
  if (get_coco_type(lawnb) == 0) then
    coco = get_coco(lawnb)
    do idim = 1, 3
      D(idim,idim) = coco
    end do
    is_coco_field = .false.
  else
    is_coco_field = .true.
  endif

  K(:,:) = ZERO
  
  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)
  
  allocate(GRAD0(nb_external),GRAD1(nb_external), &
           FLUX0(nb_external),FLUX1(nb_external))
  allocate(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  
  ! on va utiliser la notion de field attache au model
  extP_nb = get_external_field_nb(mdlnb)
  if( extP_nb /= 0 ) then 
    allocate(extP_lbl(extP_nb), &
             extP_len(extP_nb), &       
             extP_val(extP_nb)  )
  else
    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))
  
    extP_lbl(1)=' '
    extP_val(1)=0.
  end if

  do ig = 1, therEF(i)%N_PG

    ! on rapatrie les infos du debut de pas
    FLUX0 = fields(map(2,ig)+1:map(3,ig))
    FLUX1 = 0.D0

    ! index of grad in gauss_map : 1
    GRAD0 = fields(map(1,ig)+1:map(2,ig))
    GRAD1 = 0.D0

    if( nb_internal /= 0 ) then
      INTERNAL0 = fields(map(3,ig)+1:map(4,ig))
      INTERNAL1 = 0.d0
    end if
    
    call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,coefint,R)

    call Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl) ! formation de Bl 

    GRAD1 = MATMUL(Bl,T)

    if( get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___' ) then

      !rm : what should be tested here ?
      !if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'elas_') then 
      !  write(cout,'(A)') 'Using isext == no___ is only possible with mater == elas'
      !  write(cout,'(A,1x,A)') 'and mater =', get_eleop_value_bypps(ppsnb(ig),'mater')
      !  call faterr(IAM,cout)
      !endif

      if (is_coco_field) then 
          i_coco = get_index_from_string('COCO',names,size(names))
          coco = fields( map(i_coco,ig)+1 )
          CALL COCO_ISO(ppsnb(ig),D,coco)
          !print *,'Utilisation conduction non lineaire : ',coco
          CALL comp_flux(ppsnb(ig),GRAD1,FLUX1,coco)
      else
          CALL COCO_ISO(ppsnb(ig),D)
          CALL comp_flux(ppsnb(ig),GRAD1,FLUX1)
      endif

    else

      if( extP_nb /= 0 ) then 
        do if = 1, extP_nb
          name = get_external_field_name(mdlnb,if)
          extP_lbl(if)=name
          extP_len(if)=len_trim(name)
          extP_val(if) = fields(map(3+if,ig)+1)
        end do
      end if

      calcD=1

      call compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                               GRAD0,FLUX0,INTERNAL0, &
                               GRAD1,FLUX1,INTERNAL1, &
                               D,dt,calcD)

    end if

    K = K + MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl)) * coefint      !  ke= Blt.D.Bl.coef
    F = F +(MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*coefint)

  end do

  deallocate(Bl,D,DNX) ;  nullify(Bl,D,DNX)
  deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)
  deallocate(extP_lbl, extP_len, extP_val)

end subroutine conductivity_iso2

!============                    ====================
!------------------------------------------------------------------------------!
!    Calcul de la capacite elementaire  [Me]=Sum sphv [N]t [N] µi                  !
!------------------------------------------------------------------------------!

SUBROUTINE CAPACITY_ISO(i,ppsnb,dt,X,DT_ele,ibdyty,iblmty,Fint,M)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i! le numero de l'element dans la liste locale
INTEGER                     :: ibdyty,iblmty
real(kind=long)             :: dt         ! not used yet but will be with matlib
REAL(KIND=LONG)             :: X(:,:)     ! coordonnees des sommets
REAL(KIND=LONG)             :: M(:,:)     ! matrice de rig. elem.
REAL(KIND=LONG),DIMENSION(:):: Fint
REAL(KIND=LONG),DIMENSION(:):: DT_ele

REAL(KIND=LONG)             :: COEFINT,R,sphv,rho,xm,vol

INTEGER                     :: IG,in,mdlnb,lawnb 

REAL(KIND=LONG), POINTER    :: DNX(:,:)
! les fonctions de formes rangees comme il faut
REAL(KIND=8),ALLOCATABLE    ::  xn(:,:)  

REAL(kind=long) :: SPHVig

INTEGER :: ie,je,ni,nj,nk

REAL(kind=8) :: diamas
! somme des elements diagonaux de la matrice de masse coherente
REAL (kind=8) :: sumDiagMass 
! masse totale de l'element divisee par la somme des elements diagonaux de la matrice de masse coherente
REAL (kind=8) :: alpha

logical :: is_sphv_field
integer :: rank

integer,dimension(:) :: ppsnb

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Initialisation a vide des pointeurs
NULLIFY(DNX)
is_sphv_field=.false. 
!fd print*,'dans capacity iso, type d element :',i
!fd print*,'X en entree'
!fd write(*,'(2(1x,E20.14))') X

M=ZERO

if (get_sphv_type(lawnb) == 0) then
  SPHV = get_sphv(lawnb)
else
  is_sphv_field=.true.
endif

rho = get_rho(lawnb)

ALLOCATE(xn(1,therEF(i)%N_NODE))

xm=0.d0; vol = 0.d0

DO IG=1,therEF(i)%N_PG     ! Pour tous les points de Gauss

  if (is_sphv_field) then 
    rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPHV')
    CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,SPHV)
  endif
  
  xn=0.d0
  SPHVig = rho*SPHV

  CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                    therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)
                    
  vol = vol + COEFINT
  xm  = xm  + COEFINT*SPHVig

   ni=0
   DO in=1,therEF(i)%N_NODE
     ni=ni+1
     xn(1,ni)=therEF(i)%PG(ig)%N(in)
   ENDDO

 M=M+(MATMUL(TRANSPOSE(xn),xn)*COEFINT*SPHVig)            !  ke= Blt.D.Bl.coef

ENDDO

  ! lumping
  !fd attention avec un Q8 ca donne une masse negative !

  IF (get_eleop_value(mdlnb,'cstrg') == 'lump_' ) THEN

    if (therEF(i)%NAME /= 'Q8xxx' .and. &
        therEF(i)%NAME /= 'Q8Rxx' .and. &
        therEF(i)%NAME /= 'H20xx' .and. &
        therEF(i)%NAME /= 'H20Rx' ) then   

      ! raw sum
      DO ie=1,therEF(i)%N_NODE
        diamas=0.d0
        DO je=1,therEF(i)%N_NODE
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
      
      DO ie=1,therEF(i)%N_NODE
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
      DO ie=1,therEF(i)%N_NODE 
        DO je=1,therEF(i)%N_NODE
          IF (ie /= je) M(ie, je)=0.d0
        END DO 
        M(ie, ie)=alpha*M(ie, ie) 
      END DO
    endif      
  ENDIF

Fint = MATMUL(M,DT_ele)*(1.d0/dt)

DEALLOCATE(dnx,xn)

END SUBROUTINE CAPACITY_ISO

!> \brief Compute elementary capacity matrix for an element
!> API compatible with simulation module
subroutine capacity_iso2(i,ppsnb,dt,X,DT_ele,fields,map,names,F,M)
  implicit none
  !> [in] number of the element in the local list
  integer(kind=4),                   intent(in)  :: i
  !> [in] property set
  integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb
  !> [in] time differential
  real(kind=8),                      intent(in)  :: dt
  !> [in] coordinates of the vertices of the element
  real(kind=8),      dimension(:,:), intent(in)  :: X
  !> [in] temperature variation on the vertices of the element
  real(kind=8),      dimension(:)  , intent(in)  :: DT_ele
  !> [in] map to acces quadrature point fields
  integer(kind=4),   dimension(:,:), intent(in)  :: map
  !> [in] map to get quadrature fields name
  character(len=30), dimension(:)  , intent(in)  :: names
  !> [in] fields values at quadrature points
  real(kind=8),      dimension(:)  , intent(in)  :: fields
  !> [out] internal flux
  real(kind=8),      dimension(:)  , intent(out) :: F
  !> [out] capacity elementary matrix
  real(kind=8),      dimension(:,:), intent(out) :: M
  !
  logical         :: is_sphv_field
  real(kind=long) :: sphv,rho
  real(kind=long) :: coefint, R, sphvig, diamas
  integer(kind=4) :: ie, je, ig, i_sphv
  real(kind=long), pointer :: dnx(:,:)

  integer(kind=4) :: mdlnb, lawnb

  nullify(dnx)
  
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  
  M(:,:) = ZERO
  
  rho = get_rho(lawnb)

  if (get_sphv_type(lawnb) == 0) then
    is_sphv_field=.false.
    sphv = get_sphv(lawnb)
    sphvig = rho*sphv
  else
    is_sphv_field=.true.
  endif

  do ig = 1, therEF(i)%N_PG
  
    if (is_sphv_field) then 
      i_sphv = get_index_from_string('SPHV',names,size(names))
      sphv = fields( map(i_sphv,ig)+1 )
      sphvig = rho*sphv
    endif

    call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,coefint,R)
  
    do ie=1,therEF(i)%N_NODE
      do je=1,therEF(i)%N_NODE
        M(ie,je)=M(ie,je)+ (therEF(i)%PG(ig)%N(ie)*therEF(i)%PG(ig)%N(je)*coefint*sphvig)
      end do
    end do
  
  end do

  if( get_eleop_value(mdlnb,'cstrg') == 'lump_' ) then
    do ie = 1, therEF(i)%N_NODE
      diamas=0.d0
      do je = 1, therEF(i)%N_NODE
        diamas = diamas +M(ie,je)
        if( ie /= je ) M(ie,je)=0.d0
      end do
      M(ie,ie) = diamas 
    end do
  end if

  F = MATMUL(M,DT_ele)*(1.0D0/dt)

  deallocate(dnx)

end subroutine capacity_iso2


SUBROUTINE FINT_ISO(i,ppsnb,dt,X,ibdyty,iblmty,FINT)

!fd routine qui calcule un terme source du a la variation d'entropie meca
! BROKEN  
! il faudrait passer l'entropie par un external field et non via l'ancien mecanisme de partage des pg 

IMPLICIT NONE

INTEGER         , INTENT(IN) :: i ! le numero de l'element
INTEGER                      :: ibdyty,iblmty
real(kind=long)              :: dt
REAL(KIND=LONG)              :: X(:,:)        ! coordonnees des sommets
REAL(KIND=LONG)              :: FINT(:)       ! Vecteur des forces internes ele 

! variables locales
REAL(KIND=LONG) , POINTER    :: DNX(:,:)

REAL(KIND=LONG)  :: S0,S1     ! mechanical entropy
REAL(KIND=LONG)  :: gpT       ! gauss point temperature

INTEGER                      :: IG,mdlnb,lawnb
REAL(KIND=LONG)              :: COEFINT,R

integer,dimension(:) :: ppsnb

FINT=0.d0

return

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Initialisation a vide des pointeurs
NULLIFY(DNX)

 DO IG=1,therEF(i)%N_PG                 ! Pour tous les points de Gauss

   CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                     therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

   CALL get_Mentropy_0_MAILx(ibdyty,iblmty,ig,S0)
   CALL get_Mentropy_1_MAILx(ibdyty,iblmty,ig,S1)
   CALL get_T_1_MAILx(ibdyty,iblmty,ig,gpT)

!fd   print*,S0,S1,gpT,H

   coefint = coefint*gpT*(S1-S0)/H

   FINT=FINT+ (therEF(i)%PG(ig)%N*COEFINT)

 ENDDO

 DEALLOCATE(DNX) ; NULLIFY(DNX)

END SUBROUTINE FINT_ISO

subroutine fint_iso2(i,ppsnb,dt,X,old_fields,new_fields,map,names,FINT)

!fd routine qui calcule un terme source du a la variation d'entropie meca
! BROKEN !

  implicit none
  !> [in] number of the element in the local list
  integer(kind=4),                   intent(in)  :: i
  !> [in] property set
  integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb
  !> [in] time differential
  real(kind=8),                      intent(in)  :: dt
  !> [in] coordinates of the vertices of the element
  real(kind=8),      dimension(:,:), intent(in)  :: X
  !> [in] map to acces quadrature point fields
  integer(kind=4),   dimension(:,:), intent(in)  :: map
  !> [in] map to get quadrature fields name
  character(len=30), dimension(:)  , intent(in)  :: names
  !> [in] fields values at quadrature points of previous time step
  real(kind=8),      dimension(:)  , intent(in)  :: old_fields
  !> [in] current fields values at quadrature points
  real(kind=8),      dimension(:)  , intent(in)  :: new_fields
  !> [out] internal flux
  real(kind=8),      dimension(:)  , intent(out) :: FINT
  
  ! variables locales
  real(kind=long), pointer :: DNX(:,:)
  real(kind=long)          :: S0,S1    ! mechanical entropy
  real(kind=long)          :: gpT      ! gauss point temperature
  integer(kind=4)          :: i_mentropy, i_gpT

  integer(kind=4) :: IG,mdlnb,lawnb
  real(kind=long) :: COEFINT,R
 
  fint = 0.d0
  return 

  call get_ppset_value(ppsnb(1),mdlnb,lawnb)
  
  ! Initialisation a vide des pointeurs
  nullify(DNX)
  
  do ig = 1, therEF(i)%N_PG ! Pour tous les points de Gauss
  
    call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)
  
    i_mentropy = get_index_from_string('Mentropy',names,size(names))
    S0 = old_fields( map(i_mentropy,ig) )
    S1 = new_fields( map(i_mentropy,ig) )

    i_gpT = get_index_from_string('T',names,size(names))
    gpT   = old_fields( map(i_gpT,ig) )
  
    !fd   print*,S0,S1,gpT,H
    coefint = coefint*gpT*(S1-S0)/dt
  
    FINT = FINT + (therEF(i)%PG(ig)%N*COEFINT)
  
  end do
  
  deallocate(DNX); nullify(DNX)

end subroutine fint_iso2

!*************************************************************************
SUBROUTINE get_coor_pg_ISO(i,X,coor_pg)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i             ! le numero de l'element
REAL(KIND=LONG)              :: X(:,:)        ! coordonnees des sommets
REAL(KIND=LONG)              :: coor_pg(:,:)  ! coordonnees des points de Gauss
INTEGER                      :: ig


 DO ig=1,therEF(i)%N_PG                 ! Pour tous les points de Gauss
   coor_pg(1,ig)=dot_product(therEF(i)%PG(ig)%N(:),X(1,:))
   coor_pg(2,ig)=dot_product(therEF(i)%PG(ig)%N(:),X(2,:))
   coor_pg(3,ig)=dot_product(therEF(i)%PG(ig)%N(:),X(3,:))
 ENDDO

END SUBROUTINE get_coor_pg_ISO
!*************************************************************************
!*************************************************************************
SUBROUTINE interpolate_node2pg_ISO(i,valnoe,valpg)

! routine calculant la valeur aux points de Gauss, d'un champ donne nodal

implicit none

INTEGER         , INTENT(IN) :: i           ! le numero de l'element
REAL(KIND=LONG)              :: valnoe(:)   ! valeurs aux sommets
REAL(KIND=LONG)              :: valpg(:)    ! valeurs aux points de Gauss
INTEGER                      :: ig

 DO ig=1,therEF(i)%N_PG                 ! Pour tous les points de Gauss
   valpg(ig)=dot_product(therEF(i)%PG(ig)%N(:),valnoe(:))
 ENDDO

END SUBROUTINE interpolate_node2pg_ISO
!*************************************************************************

!am : debut des fonctions supplementaires:

! fonction qui calule le volume d'un element
function element_volume_ISO(i, X)

   implicit none

   ! variables d'entree
   integer, intent(in)      :: i ! numero de l'element dans la liste locale
   real(kind=8), intent(in) :: X(:,:) ! coordonnees des sommets de l'element

   ! valeur de retour
   real(kind=8)             :: element_volume_ISO ! volume de l'element
   
   ! variables locales   
   real(kind=8)             :: V ! pour calculer le volume de l'element
   integer                  :: iG ! indice de boucle

   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT ! pour recuperer le produit du poids associe
                                       ! au point de Gauss courant (pour l'integration)
                                       ! et de la valeur du jacobien au point de Gauss 
                                       ! courant (changement de variable pour integrer
                                       ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         !point de Gauss courant
   
   ! Initialisation a vide des pointeurs
   NULLIFY(DNX)
 
   ! on initialise a 0 le volume de la maille
   V=0.d0

   ! Pour tous les points de Gauss
   do iG=1,therEF(i)%N_PG     

      ! on recupere la valeur du produit du poids associe au point de Gauss courant
      ! et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
      call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                        therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

      ! on ajoute la contribution du point de Gauss courant au calcul du volume
      ! de l'element
      ! N.B.: le volume de l'element est la somme pour tous les points de Gauss
      !       de l'element iG de w(iG)*det(J)(x_iG)
      V = V + COEFINT

   end do

   ! on libere l'espace memoire occupe par
   deallocate(dnx)

   ! on renvoie le volume de l'element
   element_volume_ISO = V

end function element_volume_ISO

! fonction qui calcule, pour un element, une part du volume a affecter
! a chaque noeud, en calculant l'integrale des fonctions de formes
subroutine element_volume_by_node_ISO(i, X, V_nod)

   implicit none

   ! variables d'entree
   integer, intent(in)      :: i ! numero de l'element dans la liste locale
   real(kind=8), intent(in) :: X(:,:) ! coordonnees des sommets de l'element

   ! variable de sortie
   real(kind=8)             :: element_volume_ISO ! volume de l'element
   real(kind=8), dimension(:), intent(out) :: V_nod ! volume a affecter a chaque noeud
                                                    ! V_nod(j) contient le
                                                    ! volume a affecter
                                                    ! au noeud i de l'element
                                                    ! on a : somme sur j noeud
                                                    ! de l'elelement
                                                    ! de V_nod(j) = volume de
                                                    ! l'element
 
   ! variables locales   
   integer                  :: iNod ! indice de boucle sur les noeuds 
   integer                  :: iG ! indice de boucle sur les points de Gauss

   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT ! pour recuperer le produit du poids associe
                                       ! au point de Gauss courant (pour l'integration)
                                       ! et de la valeur du jacobien au point de Gauss 
                                       ! courant (changement de variable pour integrer
                                       ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         !point de Gauss courant
   
   ! Initialisation a vide des pointeurs
   NULLIFY(DNX)
 
   ! on initialise a 0 le volume de la maille, associe chaque noeud
   V_nod = 0.d0

   ! Pour tous les points de Gauss
   do iG=1,therEF(i)%N_PG     
 
      ! on recupere la valeur du produit du poids associe au point de Gauss courant
      ! et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
      call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                        therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

      ! on ajoute la contribution du point de Gauss courant au calcul du volume
      ! de l'element, associe au noeud courant
      ! N.B.: le volume de l'element associe au noeud courant est l'integrale sur l'element de la
      !       focntion de forme associee aus neoud courant (iNod), i.e. la somme pour tous les points de Gauss
      !       de l'element iG de N_iNod(x_iG)*w(iG)*det(J)(x_iG)
      V_nod = V_nod + therEF(i)%PG(ig)%N*COEFINT

   end do

   ! on libere l'espace memoire occupe par la matrice des gradients
   deallocate(dnx)

end subroutine element_volume_by_node_ISO

! fonction qui calcule le vecteur elementaire corespondant a la contribution
! du terme de Biot pour un element isoparametrique
subroutine compute_elementary_F_Biot_ISO(i, X, u, P, F_Biot)

   implicit none

   ! variables d'entree
   integer, intent(in)      :: i ! numero de l'element dans la liste locale
   real(kind=8), intent(in) :: X(:, :) ! coordonnees des sommets de l'element
   real(kind=8), intent(in) :: u(:, :) ! vitesses barycentriques aux sommets 
                                       ! de l'element
   real(kind=8), intent(in) :: P(:) ! pression aux noeuds de l'element, pour le calcul du terme de Biot :
      !   * la pression moyenne (P0) dans le cas linearise
      !   * la pression au debut du pas de temps dans le cas non-lineaire 
 
   ! variables de sortie
   real(kind=8), intent(out) :: F_Biot(:) ! vecteur elementaire contenant la
                                          ! contribution du terme de Biot pour
                                          ! l'element consdiere
 
   ! variables locales
   integer                  :: iNod ! indice de boucle sur les noeuds de l'element
   integer                  :: iG ! indice de boucle sur les points de Gauss
   real(kind=8)             :: div_u ! pour calculer la divergence de u, au point de Gauss
                                     ! courant
   real(kind=8)             :: P_G ! pression au point de Gauss courant  
 
   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT ! pour recuperer le produit du poids associe
                                       ! au point de Gauss courant (pour l'integration)
                                       ! et de la valeur du jacobien au point de Gauss 
                                       ! courant (changement de variable pour integrer
                                       ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         !point de Gauss courant
 
   ! on initialise a 0 le vecteur elementaire
   F_Biot = 0.d0
  
   ! Pour tous les points de Gauss
   do iG=1,therEF(i)%N_PG     

      ! on calcule la pression au point de Gauss courant (par 
      ! interpolation  des pressions nodales a l'aide des fonctions de
      ! forme)
      P_G = dot_product(therEF(i)%PG(ig)%N(:), P)

      ! on annulle le pointeur DNX pour recuperer le matrice des gradients
      nullify(DNX)

      ! on recupere :
      !   * la matrice des gradients: DNX
      !   * la valeur du produit du poids associe au point de Gauss courant
      !     et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
      call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                        therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

      ! on calcule la divergence de u au point de Gauss courant
      ! div(u)(x_iG) = somme, pour j allant de 1 a nbDIME de : du_j/dx_j(x_iG)
      ! ou du_j/dx_j(x_iG) = somme, pour iNod noeud de l'element de : u^iNod_j*dN_iNod/dx_j(x_iG)

      ! on l'initialise a 0
      div_u = 0.d0

      ! pour chaque noeud de l'element
      do iNod=1, therEF(i)%N_NODE
         
         ! on ajoute la contribution du noeud courant au calcul de la divergence
         div_u = div_u + dot_product(u(iNod, :), DNX(:, iNod))
  
      end do
      
      ! test: affichage de la divergence
      !print*, 'iG=', iG, ' div(u)=', div_u
 
      ! pour chaque noeud de l'element
      do iNod=1, therEF(i)%N_NODE
       
          ! on ajoute la contribution du point de Gauss courant
          ! a la composante associee au noeud courant du vecteur
          ! elementaire:
          !   -P(x_iG)*div(u)(x_iG)*N_iNod(x_iG)*w(iG)*det(J)(x_iG)
          F_Biot(iNod) = F_Biot(iNod) - P_G*div_u*therEF(i)%PG(ig)%N(iNod)*COEFINT
   
      end do

      ! on libere l'espce memoire occupe par DNX
      deallocate(DNX)

   end do
   
end subroutine compute_elementary_F_Biot_ISO

! fonction qui calcule le vecteur elementaire corespondant a la contribution
! du terme d'advection pour un element isoparametrique
subroutine compute_elementary_F_advection_ISO(i, X, v, T, F_advection)

   implicit none

   ! variables d'entree
   integer, intent(in)      :: i ! numero de l'element dans la liste locale
   real(kind=8), intent(in) :: X(:, :) ! coordonnees des sommets de l'element
   real(kind=8), intent(in) :: v(:, :) ! vitesses d'advection aux sommets 
                                       ! de l'element
   real(kind=8), intent(in) :: T(:) ! temperatures aux noeuds de l'element, 
      ! au debut du pas de temps  
 
   ! variables de sortie
   real(kind=8), intent(out) :: F_advection(:) ! vecteur elementaire contenant la
                                               ! contribution du terme d'advection pour
                                          ! l'element consdiere
 
   ! variables locales
   integer                  :: iNod ! indice de boucle sur les noeuds de l'element
   integer                  :: iG ! indice de boucle sur les points de Gauss
   real(kind=8), dimension(nbDIME) :: v_PG ! pour calculer la vitesse d'advection au point de Gauss courant
 
   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT ! pour recuperer le produit du poids associe
                                       ! au point de Gauss courant (pour l'integration)
                                       ! et de la valeur du jacobien au point de Gauss 
                                       ! courant (changement de variable pour integrer
                                       ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         ! point de Gauss courant

   ! on initialise a 0 le vecteur elementaire
   F_advection = 0.d0
  
   ! Pour tous les points de Gauss
   do iG=1,therEF(i)%N_PG     

      ! on calcule la vitesse d'advection au point de Gauss courant (par 
      ! interpolation des vitesses nodales a l'aide des fonctions de
      ! forme)
      v_PG = matmul(therEF(i)%PG(ig)%N(:), v)

      ! on annulle le pointeur DNX pour recuperer le matrice des gradients
      nullify(DNX)

      ! on recupere :
      !   * la matrice des gradients: DNX
      !   * la valeur du produit du poids associe au point de Gauss courant
      !     et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
      call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                        therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

      ! pour chaque noeud de l'element
      do iNod=1, therEF(i)%N_NODE
       
          ! on ajoute la contribution du point de Gauss courant
          ! a la composante associee au noeud courant du vecteur
          ! elementaire:
          !   -v(x_iG).grad(T)(x_iG)*div(u)(x_iG)*N_iNod(x_iG)*w(iG)*det(J)(x_iG)
          ! ou grad(T)(x) = [B_e(x)]{T_e}, soit :
          ! grad(T)(x) = matmul(DNX, T_ele)
          F_advection(iNod) = F_advection(iNod) &
                              - dot_product(v_PG, matmul(DNX, T))* &
                              therEF(i)%PG(ig)%N(iNod)*COEFINT
   
      end do
          
      ! on libere l'espace memoire occupe par DNX
      deallocate(DNX)

   end do
   
end subroutine compute_elementary_F_advection_ISO

! fonction qui calcule le gradient de temperature aux noeuds sommets d'un element
subroutine compute_grad_T_ele_ISO(i, X, T, grad_T)

   implicit none

   ! variables d'entree
   ! numero de l'element dans la liste locale
   integer, intent(in)      :: i 
   ! coordonnees des sommets de l'element
   real(kind=8), intent(in) :: X(:,:) 
   ! temperatures aux sommets de l'element
   real(kind=8), intent(in) :: T(:) 
   
   ! variables de sortie
   ! gradient de temperatures aux sommets de l'element
   real(kind=8), intent(out) :: grad_T(:, :) 
   
   ! variables locales :
   integer(kind=4) :: T_FONC_FORME ! type de fonction de forme de lelement
   real(kind=8), pointer :: X_Ref(:, :) ! coordonnees des noeuds de l'element de
                                        ! reference
   real(kind=8), pointer :: N_Ref(:) ! valeurs des focntions de formes pour le noeud 
                                     ! sommet de l'element de reference courant
   real(kind=8), pointer :: DN_Ref(:, :) ! derivees des fonctions de formes pour le noeud 
                                         ! sommet de l'element de reference courant
   real(kind=8), pointer :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                      ! noeud sommet courant
   integer :: iNod ! indice de boucle sur les neouds de l'element
   integer :: j ! indice de boucle sur la dimension spatiale
   
   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: POIDS ! poids associe a un point de Gauss
   real(kind=8)             :: R ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT ! pour recuperer le produit du poids associe
                                       ! au point de Gauss courant (pour l'integration)
                                       ! et de la valeur du jacobien au point de Gauss 
                                       ! courant (changement de variable pour integrer
                                       ! sur l'element de reference)
   
   ! Initialisation a vide des pointeurs
   nullify(X_Ref)
   nullify(N_Ref)
   nullify(DN_Ref)
   nullify(DNX)
 
   ! les poids n'ont pas de sens ici
   POIDS = 0.d0
 
   ! on recupere le type de fonction de forme de l'element
   T_FONC_FORME = therEF(i)%T_FONC_FORME
 
   ! on recupere les coordonnees des noeuds sommets de l'element de reference
   call get_Nodes_Coor_Ref(T_FONC_FORME, X_Ref)
   
   ! pour chaque neoud sommet de l'element
   do iNod=1, therEF(i)%N_NODE
   
      ! on calcule les valeurs de fonctions de forme
      call FONCT_FORME(T_FONC_FORME, X_Ref(:, iNod), N_Ref)
   
      ! on calcule les derivees des fonctions de forme
      call DERIVE_FORME(T_FONC_FORME, X_Ref(:, iNod), DN_Ref)
   
      ! on calcule la matrice des gradients dans l'element, pour le noeud
      ! courant
      ! pour un noeud k, on calcule : (dN_i/dx_j(x_k))ij , pour i dans {1, ..., nombre de noeuds de l'element}
      !                                                      et j dans {1, ..., nBDIME}
      call GRADIENT_ISO(therEF(i)%N_NODE, N_Ref, DN_ref, &
                        POIDS, X, DNX, COEFINT, R)

      ! on peut maintenant calculer le gradient de temperature au noeud
      ! courant :
      ! grad(T)(x_k) = [B_e(x_k)]{T_e}
      grad_T(iNod, :) = matmul(DNX, T)

      ! on desalloue les pointeurs utilises pour le calcul des fonctions de formes et
      ! des derivees des fonctions de formes, du noeud sommet courant
      deallocate(N_ref)
      nullify(N_Ref)
      deallocate(DN_Ref)
      nullify(DN_Ref)

   end do

   ! on libere l'espace memoire occupe par les coodonnees de references
   deallocate(X_ref)
   nullify(X_Ref)
   ! on libere l'espace memoire occupe par les gradients dans l'element
   deallocate(dnx)
   
end subroutine compute_grad_T_ele_ISO

!> fonction qui calcule le vecteur elementaire correspondant a la divergence d un field
!> pour un element isoparametrique
subroutine compute_elementary_field_divergence_ISO(i, X, vfield, Flux)

   implicit none

   ! variables d'entree
   !> element id 
   integer, intent(in)      :: i         
   !> nodal coordinates 
   real(kind=8), intent(in) :: X(:, :), vfield(:,:)  
 
   ! variables de sortie
   !> elementary external flux
   real(kind=8), intent(out) :: Flux(:)   
 
   ! variables locales
   integer                  :: iNod,iG     
   ! divergence
   real(kind=8)             :: div
 
   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R         ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT   ! pour recuperer le produit du poids associe
                                         ! au point de Gauss courant (pour l'integration)
                                         ! et de la valeur du jacobien au point de Gauss 
                                         ! courant (changement de variable pour integrer
                                         ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         !point de Gauss courant
 
   ! on initialise a 0 le vecteur elementaire
   Flux = 0.d0
  
   ! Pour tous les points de Gauss
   do ig=1,therEF(i)%N_PG     


     ! on annulle le pointeur DNX pour recuperer le matrice des gradients
     nullify(DNX)

     ! on recupere :
     !   * la matrice des gradients: DNX
     !   * la valeur du produit du poids associe au point de Gauss courant
     !     et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
     call GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                       therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

     ! on calcule la divergence de field au point de Gauss courant
     ! div(field)(x_iG) = somme, pour j allant de 1 a nbDIME de : dfield_j/dx_j(x_iG)
     ! ou dfield_j/dx_j(x_iG) = somme, pour iNod noeud de l'element de : field^iNod_j*dN_iNod/dx_j(x_iG)

      ! on l'initialise a 0
      div = 0.d0

      ! pour chaque noeud de l'element
      do iNod=1, therEF(i)%N_NODE
         
         ! on ajoute la contribution du noeud courant au calcul de la divergence
         div = div + dot_product(vField(:,iNod), DNX(:, iNod))
         !print*,'v',vField(:,iNod)
         !print*,'dn',DNX(:, iNod)  
      end do
      
      ! test: affichage de la divergence
      ! print*, 'iG=', iG, ' div=', div
 
      ! pour chaque noeud de l'element
      do iNod=1, therEF(i)%N_NODE
       
          ! on ajoute la contribution du point de Gauss courant
          ! a la composante associee au noeud courant du vecteur
          ! elementaire:
          !   div(field)(x_iG)*N_iNod(x_iG)*w(iG)*det(J)(x_iG)

          !fd le 06/04/2014 ... on a besoin de 

          Flux(iNod) = Flux(iNod) + div*therEF(i)%PG(ig)%N(iNod)*COEFINT
   
      end do

      ! on libere l'espce memoire occupe par DNX
      deallocate(DNX)

   end do

end subroutine compute_elementary_field_divergence_ISO


subroutine get_id_theref_iso(name,id)
  implicit none
  CHARACTER(len=5) :: name
  integer      :: id
                           !1234567890123456789012345678901
  character(len=31) :: IAM='a_therEF_iso::get_id_theref_iso'

  do id=1,size(therEF)
    if (name == therEF(id)%name) exit
  enddo
  
  if (id > size(therEF)) then
    call LOGMES('Error '//IAM//': Unknown element:'//name)
  endif

end subroutine

! rm's shit for new arch
function get_N_DOF_therEF_iso(id)
  implicit none
  integer(kind=4), intent(in) :: id       !< [in] id of the thermal iso element
  integer(kind=4) :: get_N_DOF_therEF_iso !< [return] number of dof in the element

  get_N_DOF_therEF_iso =  therEF(id)%N_NODE * therEF(id)%N_DOF_by_NODE
end function

!> \brief Get the number of dof of a node of an element
function get_N_DOF_of_NODE_therEF_iso(id, i_node)
  implicit none
  integer(kind=4), intent(in) :: id               !< [in] id of the mechanical iso element
  integer(kind=4), intent(in) :: i_node           !< [in] index of the node
  integer(kind=4) :: get_N_DOF_of_NODE_therEF_iso !< [return] number of dof in the element

  get_N_DOF_of_NODE_therEF_iso = therEF(id)%N_DOF_by_NODE
end function
  
SUBROUTINE ADD_SOURCE_ISO(i,rank,X,ibdyty,iblmty,SINT)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i,ibdyty,iblmty ! le numero de l'element dans la liste locale
REAL(KIND=LONG)             :: X(:,:)        ! coordonnees des sommets
REAL(KIND=LONG)             :: SINT(:)       ! matrice de rig. elem.

! variables locales
REAL(KIND=LONG), POINTER    :: DNX(:,:)
REAL(KIND=LONG)             :: COEFINT,R,SOURCE
INTEGER                     :: IG,rank

! Initialisation a vide des pointeurs
NULLIFY(DNX)

SINT = ZERO

  DO IG=1,therEF(i)%N_PG                 ! Pour tous les points de Gauss

    CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,SOURCE)
    !print *,'SOURCE : ',SOURCE
    CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)
   
    COEFINT  =  COEFINT*SOURCE
    SINT = SINT + (therEF(i)%PG(ig)%N*COEFINT)
    !print *,'S int : ',SINT
  ENDDO
  DEALLOCATE(DNX) ; NULLIFY(DNX)

END SUBROUTINE ADD_SOURCE_ISO

!------------------------------------------------------------------------------!
!> get a pointer on a gauss_pt object stored at gp ig of ele type id
function get_gp_ptr_therEF_iso(id,ig)
  implicit none 
  integer :: id,ig
  type (T_pt_gauss), pointer :: get_gp_ptr_therEF_iso

  get_gp_ptr_therEF_iso => therEF(id)%PG(ig)

end function
!------------------------------------------------------------------------------!


SUBROUTINE CONVECTIVE_ISO(i,ppsnb,dt,X,T_ele,DT_ele,ibdyty,iblmty,Fint,Kv,Kc)

 IMPLICIT NONE

 ! le numero de l'element dans la liste locale
 INTEGER        , INTENT(IN) :: i 
 INTEGER                     :: ibdyty,iblmty
 ! coordonnees des sommets, valeur de la variable scalaire
 REAL(KIND=LONG)             :: X(:,:)      
 ! matrice de convection. elem. + capacite supg
 REAL(KIND=LONG)             :: Kv(:,:),Kc(:,:) 
 ! vitesse de convection aux noeuds passer par le field SPEED_X,SPEED_Y,SPEED_Z     
 REAL(KIND=LONG),DIMENSION(:):: Fint
 REAL(KIND=LONG)             :: T_ele(:), DT_ele(:)

 ! variables locales
 REAL(KIND=LONG), POINTER    :: DNX(:,:), & !   
                                Bl(:,:),  & ! matrice Bl
                                D(:,:)      ! matrice de comportement


REAL(KIND=LONG)             :: COEFINT,R,Ux,Uy,Uz, Norm_U, T1_SUPG,dt
REAL(KIND=LONG)             :: coco,Pe,SPHV,rho,vol,diamas,lenght,sumdiagmass,alpha,xm


 INTEGER                     :: IG,idim,mdlnb,lawnb,j,je,ie
 INTEGER                     :: nb_external, nb_internal

 integer,dimension(:) :: ppsnb
 logical :: is_coco_field
 logical :: is_sphv_field
 logical :: is_supg
 logical :: is_char
 logical :: is_lump
 ! zone de stockage: gradient,flux,internal,operateur tangent

 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: N_SUPG,N

 ! parametres externes
 ! gestion de multiples field|extP pour couplage
 integer           :: if,rank
 character(len=30) :: name

                        !12345678901234567890123456789
character(len=29):: IAM='a_ther_EF_iso::CONVECTIVE_ISO'
real(kind=8) :: field,field_begin

real(kind=8) :: U(1,3)

is_coco_field=.false.
is_sphv_field=.false.
is_supg = .false.
is_char = .false.
is_lump = .false.

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

IF (get_eleop_value(mdlnb,'adve_') == 'supg_' ) is_supg = .true.
IF (get_eleop_value(mdlnb,'adve_') == 'char_' ) is_char = .true.
IF (get_eleop_value(mdlnb,'cstrg') == 'lump_' ) is_lump = .true.

if (get_sphv_type(lawnb) == 0) then
  SPHV = get_sphv(lawnb)
else
  is_sphv_field=.true.
endif

rho = get_rho(lawnb)

! Calcul de la convection SUPG
IF ((is_supg) .OR. (is_char)) THEN

    ! Recherche du coefficient de diffusion pour evaluation du Peclet local
    
    if (get_coco_type(lawnb) .ne. 0) is_coco_field=.true.

    vol = element_volume_ISO(i, X)
    
    ! Recherche du volume de l'element
    SELECT CASE(DIME_mod)
      CASE(i_2D_strain,i_2D_stress,i_2D_axisym )
           lenght = sqrt(vol/PI_g)
      CASE(i_3D)
           lenght = SIGN(ABS((3.0d0*vol)/(4.0d0*PI_g))**(1.0/3.0),(3.0d0*vol)/(4.0d0*PI_g))
    END SELECT

    ALLOCATE(N_SUPG(therEF(i)%N_NODE,1))
        
ENDIF

ALLOCATE(N(therEF(i)%N_NODE,1))

U = 0.D0
! Initialisation a vide des pointeurs
NULLIFY(Bl,D,DNX)

Kv=ZERO
Kc=ZERO

DO IG=1,therEF(i)%N_PG     ! Pour tous les points de Gauss
    
    ! Recherche de la vitesse pour le Point de Gauss
    SELECT CASE(DIME_mod)
    
      CASE(i_2D_strain,i_2D_stress,i_2D_axisym )

           rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPEED_X')
           CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,Ux)
           rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPEED_Y')
           CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,Uy)
           U(1,1) = Ux
           U(1,2) = Uy
           U(1,3) = ZERO
           Norm_U = sqrt(Ux*Ux + Uy*Uy)
           
      CASE(i_3D)

           rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPEED_X')
           CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,Ux)
           rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPEED_Y')
           CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,Uy)
           rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPEED_Z')
           CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,Uz)
           U(1,1) = Ux
           U(1,2) = Uy
           U(1,3) = Uz
           Norm_U = sqrt(Ux*Ux + Uy*Uy + Uz*Uz)
  
    END SELECT
    
    if (is_sphv_field) then 
        rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'SPHV')
       CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,SPHV)
    endif
    
    CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

    CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl) ! formation de Bl
    
    ! Creation de la fonction de ponderation

    DO j=1,therEF(i)%N_NODE  ! Boucle sur les noeuds
       N(j,1) =  therEF(i)%PG(ig)%N(j) 
    ENDDO
    
    Kv = Kv + MATMUL(N,MATMUL(U,Bl))*COEFINT*rho*SPHV
    
    ! On passe a la contribution SUPG
    ! Recherche du coefficient de diffusion pour Peclet local
    if ((is_supg) .OR. (is_char)) then 
        if (is_coco_field) then 
             rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'COCO')
             CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,coco)
             CALL COCO_ISO(ppsnb(ig),D,coco)
        else
             CALL COCO_ISO(ppsnb(ig),D)
             coco = D(1,1)
        endif
        ! Calcul du Peclet local
        Pe = lenght * Norm_u * rho * SPHV / coco
        if (is_supg) THEN
         T1_SUPG = ((1.0 - 1.0/((Pe/2.0) + 1.0)) * lenght)/(2.0*Norm_U)
        ENDIF
       if (is_char) THEN
         T1_SUPG = H
       ENDIF
        ! Creation de la fonction de ponderation methode SUPG
        N_SUPG = T1_SUPG*transpose(matmul(U,Bl))
        Kv = Kv + MATMUL(N_SUPG,MATMUL(U,Bl))*COEFINT*rho*SPHV
        Kc = Kc + MATMUL(N_SUPG,TRANSPOSE(N_SUPG))*rho*SPHV*COEFINT    
       
    endif
         
ENDDO

! lumping si supg
!   print*,'type de capacite:',get_capacity_THERx(mdlnb)
!!get_capacity_storage_THERx(mdlnb) 
IF ((is_supg) .OR. (is_char)) THEN
   DEALLOCATE(N_SUPG)
   IF (is_lump) THEN

    if (therEF(i)%NAME /= 'Q8xxx' .and. &
        therEF(i)%NAME /= 'Q8Rxx' .and. &
        therEF(i)%NAME /= 'H20xx' .and. &
        therEF(i)%NAME /= 'H20Rx' ) then   

      ! raw sum
      DO ie=1,therEF(i)%N_NODE
        diamas=0.d0
        DO je=1,therEF(i)%N_NODE
          diamas=diamas+Kc(ie,je)
          IF (ie /= je) Kc(ie,je)=0.d0
        END DO
        Kc(ie,ie)=diamas 
      END DO

    else
      ! Autre méthode de lumping: "Special Lumping Technique" - voir bouquin de Hugues
      ! elle donne TOUJOURS des masses lumpees strictement positives!   
 
      ! on calcule la somme des elements diagonaux de la matrice de masse coherente
      sumDiagMass = 0.d0
      
      DO ie=1,therEF(i)%N_NODE
        sumDiagMass = sumDiagMass + Kc(ie, ie)
      END DO
      
      ! on calcule alpha: masse totale de l'element divisee par la somme des elements diagonaux de la matrice de masse coherente
      ! i.e. alpha = xm/sumDiagMass
      alpha = xm/sumDiagMass
      
      ! on en deduit les coefficients de la matrice de masse lumpee
      ! en fonctions de ceux de la mtrice de masse coherente
      ! pour la ligne i: 
      !  * M_lump(i, i) = alpha*M(i, i)
      !  * M_lump(i, j) = 0, pour j different de i
      DO ie=1,therEF(i)%N_NODE 
        DO je=1,therEF(i)%N_NODE
          IF (ie /= je) Kc(ie, je)=0.d0
        END DO 
        Kc(ie, ie)=alpha*Kc(ie, ie) 
      END DO
    endif      
  ENDIF
ENDIF

DEALLOCATE(Bl,DNX,N)

Fint = MATMUL(Kv,T_ele) + MATMUL(Kc,DT_ele)*(1.d0/dt)

END SUBROUTINE CONVECTIVE_ISO

!============                    ====================
!--------------------------------------------------------------------------------------------------------!
!    Calcul de la conductivite elementaire Lagrangien total    K lagrangien = J . F^(-1) . k . F  ^(-1)  !
!--------------------------------------------------------------------------------------------------------!

SUBROUTINE CONDUCTIVITY_GD_ISO(i,ppsnb,dt,X,U,T,ibdyty,iblmty, &
                               Fint,need_Fint,K,need_K,push_f)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i ! le numero de l'element dans la liste locale
INTEGER                     :: ibdyty,iblmty
REAL(KIND=LONG)             :: X(:,:),T(:), U(:,:) ! coordonnees des sommets, valeur de la variable scalaire
real(kind=long)             :: dt            ! not used yet, but will be with matlib
REAL(KIND=LONG)             :: K(:,:)        ! matrice de rig. elem.
REAL(KIND=LONG),DIMENSION(:):: Fint
logical :: need_Fint, need_K, push_f

! variables locales
REAL(KIND=LONG), POINTER    :: DNX(:,:), & !   
                               Bl(:,:),  & ! matrice Bl
                               D(:,:)      ! matrice de comportement
                               
REAL(KIND=LONG),POINTER     :: F_T(:,:),F_1(:,:)

REAL(KIND=LONG)             :: COEFINT,R,coco,J

INTEGER                     :: IG,idim,mdlnb,lawnb
INTEGER                     :: nb_external, nb_internal

logical :: is_coco_field

integer,dimension(:) :: ppsnb

! zone de stockage: gradient,flux,internal,operateur tangent

REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

! parametres externes
! gestion de multiples field|extP pour couplage
integer           :: if,rank
character(len=30) :: name
INTEGER*4                                    :: extP_nb
CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
INTEGER*4        , DIMENSION(:), allocatable :: extP_len
REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val
                        !1234567890123456789012345678901234
character(len=34):: IAM='a_ther_EF_iso::CONDUCTIVITY_GD_ISO'
real(kind=8) :: field,field_begin
! demande calcul matrice tangente
INTEGER(kind=4) :: calcD

! Pour faire tourner le poreux
logical :: a_recoder

is_coco_field = .false.
a_recoder = .true.

! Initialisation a vide des pointeurs
NULLIFY(Bl,D,DNX)
NULLIFY(F_T,F_1)

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

if (get_coco_type(lawnb) .ne. 0) is_coco_field=.true.

!fd a recoder
nb_external=3
nb_internal=0

ALLOCATE(GRAD0(nb_external),GRAD1(nb_external), &
         FLUX0(nb_external),FLUX1(nb_external))
ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))

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

K=ZERO
Fint=ZERO

DO IG=1,therEF(i)%N_PG     ! Pour tous les points de Gauss

    ! on rapatrie les infos du debut de pas
    CALL get_flux_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_grad_0_MAILx(ibdyty,iblmty,ig,GRAD0)

    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF (nb_internal /= 0 ) INTERNAL1 = 0.D0
    
    CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

    CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl) ! formation de Bl 

    GRAD1 = MATMUL(Bl,T)

    !IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
    if( a_recoder ) then

      if (is_coco_field) then

          rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'COCO')
          CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,coco)
          CALL COCO_ISO(ppsnb(ig),D,coco)
          CALL comp_flux(ppsnb(ig),GRAD1,FLUX1,coco)
      else
          CALL COCO_ISO(ppsnb(ig),D)
          CALL comp_flux(ppsnb(ig),GRAD1,FLUX1)
      endif

      !print *,' Matrice de conduction LMGC90 : ',D(1,1),D(1,2),D(1,3)
      !print *,' ---------------------------- : ',D(2,1),D(2,2),D(2,3)
      !print *,' ---------------------------- : ',D(3,1),D(3,2),D(3,3)
      ! Partie de la transformation Lagrangienne

    ELSE
      ! on va utiliser la notion de field attache au model
      extP_nb = get_external_field_nb(mdlnb)

      IF (extP_nb /= 0) THEN

        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          extP_lbl(if)=name
          extP_len(if)=len_trim(name)
          rank = get_ther_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,field)
          extP_val(if) = field

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      calcD = 0
      if (need_K) calcD = 1

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                              GRAD0,FLUX0,INTERNAL0, &
                              GRAD1,FLUX1,INTERNAL1, &
                              D,H,calcD)

      !print *,' Matrice de conduction Matlib : ',D(1,1),D(1,2),D(1,3)
      !print *,' ---------------------------- : ',D(2,1),D(2,2),D(2,3)
      !print *,' ---------------------------- : ',D(3,1),D(3,2),D(3,3)
    endif

    CALL GRAD_TRANSFORMATION(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,R,U,F_1,J,F_T)
    D = J * matmul(F_1,matmul(D,F_T))

    !  ke= Blt.D.Bl.coef
    if( need_K   ) K    = K + MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*COEFINT
    if( need_Fint) Fint = Fint + (MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*COEFINT)

    if (push_f) then
      call put_grad_MAILx(ibdyty,iblmty,ig,GRAD1)
      call put_flux_MAILx(ibdyty,iblmty,ig,FLUX1)
      if (nb_internal /= 0 ) call put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
    end if

ENDDO

deallocate(extP_lbl,extP_len,extP_val)
deallocate(DNX,Bl,D,F_1,F_T) ;  nullify(DNX,Bl,D,F_1,F_T)
deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)

END SUBROUTINE CONDUCTIVITY_GD_ISO



SUBROUTINE GRAD_TRANSFORMATION(N_NE,N,DNX,R,U,F_1,J,F_T)

! computes the inv F matrix (inverse de la Jacobienne de la transformation)

!------------------------------------------------------------------------------!
!                                                                              !
!  EN 3D : inv F = [ dX / dx   dX / dy    dX / dz                              !
!                    dY / dx   dY / dy    dY / dz                              !
!                    dZ / dx   dZ / dy    dZ / dz   ]                          !
!                                                                              !
!------------------------------------------------------------------------------!


IMPLICIT NONE

INTEGER                      :: N_NE
REAL(KIND=LONG), POINTER     :: N(:)
REAL(KIND=LONG), POINTER     :: DNX(:,:)
REAL(KIND=LONG)              :: R
REAL(KIND=LONG)              :: U(:,:)
REAL(KIND=LONG),DIMENSION(:,:),POINTER :: F_1
REAL(KIND=LONG)              :: J
REAL(KIND=LONG),DIMENSION(:,:),POINTER :: F_T

REAL(KIND=LONG), POINTER     :: B(:,:)
REAL(KIND=LONG)              :: Ur

REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE:: Grad_Ux
REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE:: Grad_Uy
REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE:: Grad_Uz
  
INTEGER :: inull
                        !1234567890123456789012345678901234
CHARACTER(len=34):: IAM='a_ther_EF_iso::grad_transformation'

IF(ASSOCIATED(F_T)) THEN ; DEALLOCATE(F_T) ; NULLIFY(F_T) ; ENDIF
IF(ASSOCIATED(F_1)) THEN ; DEALLOCATE(F_1) ; NULLIFY(F_1) ; ENDIF
   
NULLIFY( B )
   
SELECT CASE(DIME_mod)

    CASE(i_3D)
        ALLOCATE(F_1(3,3))
        F_1(:,:) = 0.d0
        ALLOCATE(F_T(3,3))
        F_T(:,:) = 0.d0
        ALLOCATE(Grad_Ux(3))
        Grad_Ux(:) = 0.d0
        ALLOCATE(Grad_Uy(3))
        Grad_Uy(:) = 0.d0
        ALLOCATE(Grad_Uz(3))
        Grad_Uz(:) = 0.d0
        
        CALL Bl_ISO(N_NE,N,DNX,B)

        Grad_Ux = MATMUL(B,U(1,:))
        Grad_Uy = MATMUL(B,U(2,:))
        Grad_Uz = MATMUL(B,U(3,:))
        
        F_1(1,:)=(/  1.0 + Grad_Ux(1) ,       Grad_Ux(2) ,       Grad_Ux(3) /)
        F_1(2,:)=(/        Grad_Uy(1) , 1.0 + Grad_Uy(2) ,       Grad_Uy(3) /)
        F_1(3,:)=(/        Grad_Uz(1) ,       Grad_Uz(2) , 1.0 + Grad_Uz(3) /)
        
        J   = determinant33(F_1)
        call inverse33(F_1, inull)

        if (inull == 1) then
           call faterr(IAM,'non invertible matrix')
        endif
        
        F_T = transpose(F_1)
        
        DEALLOCATE(Grad_Uz)
        
    CASE(i_2D_strain)

        ALLOCATE(F_1(3,3))
        F_1(:,:) = 0.d0
        ALLOCATE(F_T(3,3))
        F_T(:,:) = 0.d0
        ALLOCATE(Grad_Ux(3))
        Grad_Ux(:) = 0.d0
        ALLOCATE(Grad_Uy(3))
        Grad_Uy(:) = 0.d0
        
        CALL Bl_ISO(N_NE,N,DNX,B)
        
        Grad_Ux = MATMUL(B,U(1,:))
        Grad_Uy = MATMUL(B,U(2,:))
        
        F_1(1,:)=(/  1.0 + Grad_Ux(1) ,       Grad_Ux(2) ,0.d0/)
        F_1(2,:)=(/        Grad_Uy(1) , 1.0 + Grad_Uy(2) ,0.d0/)
        F_1(3,:)=(/        0.d0       , 0.d0             , 1.d0/)
        
        J   = determinant33(F_1)
        call inverse33(F_1, inull)
        if (inull == 1) then
           call faterr(IAM,'non invertible matrix')
        endif
                
        
        F_T = transpose(F_1)
        
    CASE(i_2D_axisym)

        ALLOCATE(F_1(3,3))
        F_1(:,:) = 0.d0
        ALLOCATE(F_T(3,3))
        F_T(:,:) = 0.d0
        ALLOCATE(Grad_Ux(3))
        Grad_Ux(:) = 0.d0
        ALLOCATE(Grad_Uy(3))
        Grad_Uy(:) = 0.d0
        
        CALL Bl_ISO(N_NE,N,DNX,B)

        Grad_Ux = MATMUL(B,U(1,:))
        Grad_Uy = MATMUL(B,U(2,:))

        Ur = DOT_PRODUCT(N,U(1,:))
        
        F_1(1,:)=(/  1.0 + Grad_Ux(1) ,       Grad_Ux(2) ,0.d0/)
        F_1(2,:)=(/        Grad_Uy(1) , 1.0 + Grad_Uy(2) ,0.d0/)
        F_1(3,:)=(/        0.d0       , 0.d0             ,1.0 + Ur/R/)
        
        J   = determinant33(F_1)
        call inverse33(F_1, inull)
        if (inull == 1) then
           call faterr(IAM,'non invertible matrix')
        endif
        
        F_T = transpose(F_1)
    
    CASE DEFAULT
        CALL FATERR(IAM,'Unsupported dimension: '//get_dime_mode_name_from_id(dime_mod))
END SELECT

DEALLOCATE(Grad_Ux);DEALLOCATE(Grad_Uy);
DEALLOCATE(B); NULLIFY(B)

END SUBROUTINE GRAD_TRANSFORMATION

! ------------------------------------------------------------------------------
! DA : Ajout pour la projection des point de gauss au noeuds
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

  if (size(cg,dim=2) /= nbgp) then
    call FATERR('a_therEF_iso::compute_gp2node','nbgp inconsistancy')
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

!------------------------------------------------------------------------
SUBROUTINE gpv2node_iso(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==grad, 2==flux
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues
  REAL(kind=8),DIMENSION(:,:),allocatable   :: Field

  INTEGER :: NbNodes_ele,NbGp_ele, errare
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: NbNodes_stored, nbm, nbs, mdlnb

!                            12345678901234567890123456
  CHARACTER(len=26)  :: IAM='a_therEF_iso::gpv2node_iso'

  integer :: if

  NbGp_ele =  therEF(i)%N_PG
  NbNodes_ele = therEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele 

  if (fieldsize /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
  NodalValues = 0.d0

  ALLOCATE(field(3,NbGp_ele),stat=errare)
  IF (errare /= 0) THEN
     CALL FATERR(IAM,'allocating field')
  ENDIF

  call gpv_iso(i,mdlnb,ibdyty,iblmty,required_Field,Field,3)

  !print*,field

  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  nbs=size(therEF(i)%gp2node,dim=1)
  nbm=size(therEF(i)%node2edge,dim=1)
  do if=1,fieldsize
     v_nodes(1:nbs) = matmul(therEF(i)%gp2node,Field(if,:))
     if (associated(therEF(i)%node2edge)) then
       v_nodes(nbs+1:nbm) = matmul(therEF(i)%node2edge,v_nodes(1:nbs)) 
     endif
     NodalValues(if,:) = NodalValues(if,:) + v_nodes(:)

  enddo

  deallocate(v_nodes)


END SUBROUTINE gpv2node_iso

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_fields_ISO(i,ppsnb,dt,X,T,ibdyty,iblmty,U)

   implicit none
   INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   REAL(KIND=LONG)                 :: X(:,:),T(:) ! coordonnees et deplacement des sommets
   REAL(KIND=LONG),OPTIONAL       :: U(:,:) ! Mouvement du maillage
   real(kind=8)                    :: dt

                           !12345678901234567890123456789012345678901234
   character(len=44):: IAM='a_ther_EF_iso::compute_elementary_fiedls_iso'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)

   !select case(get_eleop_value(mdlnb,'kine_'))
   !case('small')
    !    print *,'Appel fields_hpp_iso',ibdyty,iblmty
        call fields_hpp_iso(i,ppsnb,dt,X,T,ibdyty,iblmty)
   !case('large')
    !    call fields_gd_iso(i,ppsnb,dt,X,U,T,ibdyty,iblmty)
   !case default
   !     call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
   !     call FATERR(IAM,'kinematic type unknown (small | large)')
   !end select

END SUBROUTINE


!------------------------------------------------------------------------
SUBROUTINE gpv_iso(i,mdlnb,ibdyty,iblmty,required_Field,Field,FieldSize)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==gradient, 2==flux 
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: Field
  INTEGER :: NbGp_ele
  
  INTEGER :: NbNodes_stored

!                            123456789012345678901
  CHARACTER(len=21)  :: IAM='a_therEF_iso::gpv_iso'

  REAL(KIND=LONG) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:)     ! vecteur de travail local
  integer :: ig,inull,nb_external,nb_internal

  NbGp_ele =  therEF(i)%N_PG

  if (fieldsize /= size(Field,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbGp_ele /= size(Field,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
  
  Field = 0.d0

  !fd a recoder
  !nb_external=get_nb_external_variables(mdlnb)
  !nb_internal=get_nb_internal_variables(mdlnb)

  nb_external=3
  nb_internal=0
  
  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  do ig=1,NbGp_ele
    CALL get_flux_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_grad_1_MAILx(ibdyty,iblmty,ig,GRAD(:,ig))
    
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

    case(1) ! Gradient 
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN

        !IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN


         do ig=1,NbGp_ele


            !print*,grad(:,ig)
            
            Field(1,ig) = GRAD(1,ig) 
            Field(2,ig) = GRAD(2,ig)
            Field(3,ig) = GRAD(3,ig)
            
         enddo
         
        !ELSEIF (get_eleop_value(mdlnb,'kine_') == 'large') THEN

         !do ig=1,NbGp_ele
            
         !   Field(1,ig) = GRAD(1,ig) 
         !   Field(2,ig) = GRAD(2,ig)
         !   Field(3,ig) = GRAD(3,ig)
            
         !enddo
         
        !ENDIF

      ELSE
       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         
         !CALL LOGMES(IAM//' CAUTION : Only internal models are supported ')
         do ig=1,NbGp_ele
            
            Field(1,ig) = GRAD(1,ig) 
            Field(2,ig) = GRAD(2,ig)
            Field(3,ig) = GRAD(3,ig)
            
         enddo
        
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         
         !CALL LOGMES(IAM//' CAUTION : Only internal models are supported ')

         do ig=1,NbGp_ele
            
            Field(1,ig) = GRAD(1,ig) 
            Field(2,ig) = GRAD(2,ig)
            Field(3,ig) = GRAD(3,ig)
            
         enddo

       ENDIF

     ENDIF

    case(2) ! Flux
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
      
        !IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         do ig=1,NbGp_ele
            
            Field(1,ig) =  -FLUX(1,ig) 
            Field(2,ig) =  -FLUX(2,ig)
            Field(3,ig) =  -FLUX(3,ig)
                    
         enddo
        !ENDIF
        !IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
        ! do ig=1,NbGp_ele
        !    
        !    Field(1,ig) = FLUX(1,ig) 
        !    Field(2,ig) = FLUX(2,ig)
        !    Field(3,ig) = FLUX(3,ig)
        !            
        !enddo
       !ENDIF
 
      ELSE
       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         
         !CALL LOGMES(IAM//'CAUTION : Only internal models are supported ')
         do ig=1,NbGp_ele
            
            Field(1,ig) = -FLUX(1,ig) 
            Field(2,ig) = -FLUX(2,ig)
            Field(3,ig) = -FLUX(3,ig)
            
         enddo
        
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         
         !CALL LOGMES(IAM//'CAUTION : Only internal models are supported ')

         do ig=1,NbGp_ele
            
            Field(1,ig) = -FLUX(1,ig) 
            Field(2,ig) = -FLUX(2,ig)
            Field(3,ig) = -FLUX(3,ig)
            
         enddo

       ENDIF

     ENDIF

    case default
      CALL FATERR(IAM,'Unsupported required_field : 1::grad, 2::flux')
  endselect

  deallocate(GRAD,FLUX,INTERNAL)

END SUBROUTINE gpv_iso

!----------------------------------------------------------------------------!

SUBROUTINE fields_HPP_ISO (i,ppsnb,dt,X,T,ibdyty,iblmty)

  IMPLICIT NONE

  INTEGER         , INTENT(IN) :: I,ibdyty,iblmty  ! le numero de l'element 
  integer, dimension(:)        :: ppsnb
  REAL(KIND=LONG)              :: X(:,:)     ! coordonnees des sommets
  REAL(KIND=LONG)              :: T(:)     ! DDL elementaires
  real(kind=8)                 :: dt

  ! variables locales
  REAL(KIND=LONG) , POINTER    :: DNX(:,:), & !   
                                  Bl(:,:), &    ! matrice Bl  (epsilon=Bl q
                                  D(:,:)      ! matrice de comportement
  REAL(KIND=LONG)              :: COEFINT,R,coco
  INTEGER                      :: IG

  INTEGER                      :: anisotropie
 
  INTEGER                      :: nb_external, nb_internal
  INTEGER                      :: mdlnb

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

  INTEGER(kind=4)                      :: calcD

  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank,lawnb
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  ! switch forme de B 
  INTEGER :: istrg,inull
  
  logical :: is_coco_field
  
  ! Pour faire tourner le poreux
  logical :: a_recoder

  real(kind=8) :: field,field_begin

  !print*,T

  ! Initialisation a vide des pointeurs
  NULLIFY(Bl,DNX)
  is_coco_field=.false.
  a_recoder=.true.
  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  
  if (get_coco_type(lawnb) .ne. 0) is_coco_field=.true.
  
  if (a_recoder) then
    nb_external=3
    nb_internal=0
  else
    nb_external=get_nb_external_variables(mdlnb)
    nb_internal=get_nb_internal_variables(mdlnb)
  endif 
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

    !extP_lbl(1)=' '
    !extP_val(1)=0.

  ENDIF

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(0,0))

  DO IG=1,therEF(i)%N_PG                 ! Pour tous les points de Gauss

    ! on rapatrie les infos du debut de pas
    CALL get_flux_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_grad_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
    
    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF (nb_internal /= 0 )INTERNAL1 = 0.D0

    CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)
    
    
    !IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
    IF (a_recoder) THEN

      istrg = 1
      CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl) ! formation de Bl 
                                
      GRAD1 = MATMUL(Bl,T)

      if (is_coco_field) then 
         rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'COCO')
         CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,coco)
         CALL COCO_ISO(ppsnb(ig),D,coco)
         CALL comp_flux(ppsnb(ig),GRAD1,FLUX1,coco)
      else
         CALL COCO_ISO(ppsnb(ig),D)
         CALL comp_flux(ppsnb(ig),GRAD1,FLUX1)
      endif

    ELSE

      !DA : Modification salvatrice a verifier ?
      !extP_nb = get_meca_field_size_MAILx(ibdyty,iblmty,ig)
      IF (extP_nb /= 0) THEN 

        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          extP_lbl(if)=name
          extP_len(if)=len_trim(name)

          rank = get_ther_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if) )

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      istrg = 2
      CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl)      

      GRAD1 = MATMUL(Bl,T)
 
      calcD=0

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                               GRAD0,FLUX0,INTERNAL0, &
                               GRAD1,FLUX1,INTERNAL1, &
                               D,dt,calcD)

    ENDIF
    
 
    !print*,ibdyty,iblmty,ig,nb_external,nb_internal,mdlnb,lawnb

    !print*,ibdyty,iblmty,ig,grad1

    CALL put_grad_MAILx(ibdyty,iblmty,ig,GRAD1(1:3))
    CALL put_flux_MAILx(ibdyty,iblmty,ig,FLUX1)
    IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)

    !print*,'ca passe !'

  ENDDO

  deallocate(extP_lbl, extP_len, extP_val  )

  DEALLOCATE(Bl,DNX) ; NULLIFY(Bl,DNX)
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_HPP_ISO

!----------------------------------------------------------------------------!

SUBROUTINE fields_GD_ISO (i,ppsnb,dt,X,U,T,ibdyty,iblmty)

  IMPLICIT NONE

  INTEGER         , INTENT(IN) :: I,ibdyty,iblmty  ! le numero de l'element 
  integer, dimension(:)        :: ppsnb
  REAL(KIND=LONG)              :: X(:,:),U(:,:)     ! coordonnees des sommets
  REAL(KIND=LONG)              :: T(:)     ! DDL elementaires
  real(kind=8)                 :: dt, J

  ! variables locales
  REAL(KIND=LONG) , POINTER    :: DNX(:,:), & !   
                                    Bl(:,:), &    ! matrice Bl  (epsilon=Bl q
                                    D(:,:)      ! matrice de comportement
                                  
  REAL(KIND=LONG),POINTER     :: F_T(:,:),F_1(:,:)
                                  
  REAL(KIND=LONG)              :: COEFINT,R,coco
  INTEGER                      :: IG

  INTEGER                      :: anisotropie
 
  INTEGER                      :: nb_external, nb_internal
  INTEGER                      :: mdlnb

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

  INTEGER(kind=4)                      :: calcD

  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank,lawnb
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  ! switch forme de B 
  INTEGER :: istrg,inull
  
  logical :: is_coco_field
  
  ! Pour faire tourner le poreux
  logical :: a_recoder

  real(kind=8) :: field,field_begin

  ! Initialisation a vide des pointeurs
  NULLIFY(Bl,DNX)
  NULLIFY(F_T,F_1)
  is_coco_field=.false.
  a_recoder=.true.
  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  
  if (get_coco_type(lawnb) .ne. 0) is_coco_field=.true.
  
  if (a_recoder) then
    nb_external=3
    nb_internal=0
  else
    nb_external=get_nb_external_variables(mdlnb)
    nb_internal=get_nb_internal_variables(mdlnb)
  endif 

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

    !extP_lbl(1)=' '
    !extP_val(1)=0.

  ENDIF


  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(0,0))

  DO IG=1,therEF(i)%N_PG                 ! Pour tous les points de Gauss

    ! on rapatrie les infos du debut de pas
    CALL get_flux_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_grad_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)
    
    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF (nb_internal /= 0 )INTERNAL1 = 0.D0

    CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                      therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)
    

    !IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN
    IF (a_recoder) THEN

      istrg = 1
      CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl) ! formation de Bl 
                                
      GRAD1 = MATMUL(Bl,T)

      if (is_coco_field) then 
         rank = get_ther_field_rank_MAILx(ibdyty,iblmty,'COCO')
         CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,coco)
         CALL COCO_ISO(ppsnb(ig),D,coco)
         CALL comp_flux(ppsnb(ig),GRAD1,FLUX1,coco)
      else
         CALL COCO_ISO(ppsnb(ig),D)
         CALL comp_flux(ppsnb(ig),GRAD1,FLUX1)
      endif

    ELSE
      !DA : Modification salvatrice a verifier ?
      !extP_nb = get_meca_field_size_MAILx(ibdyty,iblmty,ig)
      IF (extP_nb /= 0) THEN 

        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          extP_lbl(if)=name
          extP_len(if)=len_trim(name)

          rank = get_ther_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_ther_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(if) )

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      istrg = 2
      CALL Bl_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,Bl)      

      GRAD1 = MATMUL(Bl,T)
 
      calcD=0

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
                               GRAD0,FLUX0,INTERNAL0, &
                               GRAD1,FLUX1,INTERNAL1, &
                               D,dt,calcD)

    ENDIF
    
    
    CALL put_grad_MAILx(ibdyty,iblmty,ig,GRAD1)
    CALL GRAD_TRANSFORMATION(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,DNX,R,U,F_1,J,F_T)
    D = J * matmul(F_1,matmul(D,F_T))
    FLUX1 = MATMUL(D,GRAD1)

    CALL put_flux_MAILx(ibdyty,iblmty,ig,FLUX1)
    IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)

  ENDDO

  deallocate(extP_lbl, extP_len, extP_val  )

  DEALLOCATE(Bl,DNX,F_1,F_T) ; NULLIFY(Bl,DNX,F_1,F_T)
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_GD_ISO
!============                    ====================
!------------------------------------------------------------------------------!
!    Calcul de la capacite elementaire  [Me]=Sum [N]t [N] µi                  !
!------------------------------------------------------------------------------!

SUBROUTINE MEAN_PRESSURE_ISO(i,X,ibdyty,iblmty,M)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i! le numero de l'element dans la liste locale
INTEGER                     :: ibdyty,iblmty
REAL(KIND=LONG)             :: X(:,:)     ! coordonnees des sommets
REAL(KIND=LONG)             :: M(:,:)     ! matrice de rig. elem.

REAL(KIND=LONG)             :: COEFINT,R

INTEGER                     :: IG

REAL(KIND=LONG), POINTER    :: DNX(:,:)

INTEGER :: ie,je

REAL(kind=8) :: diamas

! Initialisation a vide des pointeurs
NULLIFY(DNX)


M=ZERO

DO IG=1,therEF(i)%N_PG     ! Pour tous les points de Gauss

  CALL GRADIENT_ISO(therEF(i)%N_NODE,therEF(i)%PG(ig)%N,therEF(i)%PG(ig)%DN, &
                    therEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

   DO ie=1,therEF(i)%N_NODE
     DO je=1,therEF(i)%N_NODE
         M(ie,je)=M(ie,je)+ (therEF(i)%PG(ig)%N(ie)*therEF(i)%PG(ig)%N(je)*coefint)
     END DO
   END DO

ENDDO

! lumping
   DO ie=1,therEF(i)%N_NODE
       diamas=0.d0
       DO je=1,therEF(i)%N_NODE
           diamas=diamas+M(ie,je)
           IF (ie /= je) M(ie,je)=0.d0
       END DO
       M(ie,ie)=diamas 
   END DO

DEALLOCATE(dnx)

END SUBROUTINE MEAN_PRESSURE_ISO

subroutine check_elementary_ppset_iso(i,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty

  !**
  integer                         :: ig  
  
  do IG=1,therEF(i)%N_PG

    IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'yes__') THEN
      call check_external_ppset(ppsnb(ig)) 
    endif
   
  enddo   
  
end subroutine check_elementary_ppset_iso



END MODULE a_therEF_iso
