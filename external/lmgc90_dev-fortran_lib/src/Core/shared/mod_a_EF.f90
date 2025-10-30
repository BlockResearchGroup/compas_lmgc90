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

!> basic finite element routines

MODULE a_EF

use parameters, only : get_index_from_string

use utilities
use algebra

IMPLICIT NONE

private

public POS_GAUSS                              , &
       FONCT_FORME                            , &
       FONCT_FORME_SHB                        , &
       DERIVE_FORME                           , &
       DERIVE_FORME_SHB                       , &
       second_forme                           , &
       get_Nodes_Coor_Ref                     , &
       get_gp_coor                            , &
       get_nearest_gp                         , &
       interpolate_field                      , &
       integrate_field                        , &
       compute_element_size                   , &
       get_ptr_edge2vertices                  , &
       get_first_order_interpolation_id       , &
       get_quadrature_from_id                 , &
       get_interpolation_from_id              , &
       get_interpolation_id_from_nodes_and_dim, &
       get_nb_points_from_quadrature          , &
       compute_center

!> interpolation ID
integer(kind=4), public, parameter :: i_l_p1 = 1
integer(kind=4), public, parameter :: i_l_p2 = i_l_p1 + 1
integer(kind=4), public, parameter :: i_l_p3 = i_l_p2 + 1
integer(kind=4), public, parameter :: i_t_p1 = i_l_p3 + 1
integer(kind=4), public, parameter :: i_t_p2 = i_t_p1 + 1
integer(kind=4), public, parameter :: i_q_p1 = i_t_p2 + 1
integer(kind=4), public, parameter :: i_q_p2 = i_q_p1 + 1
integer(kind=4), public, parameter :: i_qcp2 = i_q_p2 + 1
integer(kind=4), public, parameter :: i_h_p1 = i_qcp2 + 1
integer(kind=4), public, parameter :: i_h_p2 = i_h_p1 + 1
integer(kind=4), public, parameter :: i_tep1 = i_h_p2 + 1
integer(kind=4), public, parameter :: i_tep2 = i_tep1 + 1
integer(kind=4), public, parameter :: i_prp1 = i_tep2 + 1
integer(kind=4), public, parameter :: i_prp2 = i_prp1 + 1

!> quadrature ID
integer(kind=4), public, parameter :: i_lig1 = 1
integer(kind=4), public, parameter :: i_lig2 = i_lig1 + 1
integer(kind=4), public, parameter :: i_lig3 = i_lig2 + 1
integer(kind=4), public, parameter :: i_tr01 = i_lig3 + 1
integer(kind=4), public, parameter :: i_tr03 = i_tr01 + 1
integer(kind=4), public, parameter :: i_tr04 = i_tr03 + 1
integer(kind=4), public, parameter :: i_tr06 = i_tr04 + 1
integer(kind=4), public, parameter :: i_q1x1 = i_tr06 + 1
integer(kind=4), public, parameter :: i_q2x2 = i_q1x1 + 1
integer(kind=4), public, parameter :: i_q3x3 = i_q2x2 + 1
integer(kind=4), public, parameter :: i_h222 = i_q3x3 + 1
integer(kind=4), public, parameter :: i_h333 = i_h222 + 1
integer(kind=4), public, parameter :: i_te01 = i_h333 + 1
integer(kind=4), public, parameter :: i_te04 = i_te01 + 1
integer(kind=4), public, parameter :: i_pr06 = i_te04 + 1
integer(kind=4), public, parameter :: i_pr08 = i_pr06 + 1
integer(kind=4), public, parameter :: i_pr15 = i_pr08 + 1
integer(kind=4), public, parameter :: i_pr35 = i_pr15 + 1
integer(kind=4), public, parameter :: i_h225 = i_pr35 + 1
integer(kind=4), public, parameter :: i_h115 = i_h225 + 1
integer(kind=4), public, parameter :: i_pr12 = i_h115 + 1
integer(kind=4), public, parameter :: i_pr01 = i_pr12 + 1

!> finite element type ID
integer, public, parameter :: i_iso  = 1
integer, public, parameter :: i_bar  = i_iso + 1
integer, public, parameter :: i_she  = i_bar + 1
integer, public, parameter :: i_dis  = i_she + 1
integer, public, parameter :: i_shb  = i_dis + 1
integer, public, parameter :: i_joint= i_shb + 1
character(len=5), dimension(6), private :: fe_type = (/'iso  ', 'bar  ', &
                                                       'shell', 'discr', &
                                                       'shb  ', 'joint' /)

!!! defining some usefull symbolic constants

! longueur d un reel ( kind = 8 ) foireux   
INTEGER , PARAMETER,public  :: LONG=8    
  
REAL(KIND=LONG), PARAMETER, public :: ZERO=0._LONG   , UN=1._LONG    , DEUX=2._LONG  , &
                                      TROIS =  3._LONG , QUATRE = 4._LONG, SIX=6._LONG, &
                                      DOUZE = 12._LONG ,                                &
                                      US2   = 0.5_LONG , US4=0.25_LONG , US8=0.125_LONG

!> a type to store weight, shape function, derivative of shape function at a quadrature point 
TYPE,public :: T_PT_GAUSS
   real(kind=long)         :: zeta
   REAL(KIND=LONG)         :: POIDS
   REAL(KIND=LONG),POINTER :: N(:)    => null()
   REAL(KIND=LONG),POINTER :: DN(:,:) => null()
END TYPE T_PT_GAUSS 

!> a linked list type to store an array of quadrature points with interpolation and quadrature id
type, public :: T_link_pt_gauss
  integer(kind=4)                         :: interpolation_id, quadrature_id
  type(T_pt_gauss), dimension(:), pointer :: GP   => null()
  type(T_link_pt_gauss)         , pointer :: next => null()
end type

!> arrays to get edge to vertices map for quadratic elements
!> depends on vertices local numbering which is first the linear vertices, then the quadratic vertices
!> \todo: change these arrays to use one G_i_list instead (indexed by interpolation parameter index)
integer(kind=4), dimension(2,2),  target, public :: l_p1_edge2vertices = reshape( (/0,0,0,0 /), shape=(/2,2/) )
integer(kind=4), dimension(2,3),  target, public :: l_p2_edge2vertices = reshape( (/0,0,0,0,1,2 /), shape=(/2,3/) )
integer(kind=4), dimension(2,4),  target, public :: l_p3_edge2vertices = reshape( (/0,0,0,0,1,2,1,2 /), shape=(/2,4/) )
integer(kind=4), dimension(2,3),  target, public :: t_p1_edge2vertices = reshape( (/0,0,0,0,0,0 /), shape=(/2,3/) )
integer(kind=4), dimension(2,6),  target, public :: t_p2_edge2vertices = reshape( (/0,0,0,0,0,0,1,2,2,3,3,1/), shape=(/2,6/) )
integer(kind=4), dimension(2,4),  target, public :: q_p1_edge2vertices = reshape( (/0,0,0,0,0,0,0,0 /),shape=(/2,4/) )
integer(kind=4), dimension(2,8),  target, public :: q_p2_edge2vertices = reshape( (/0,0,0,0,0,0,0,0,  &
                                                                                    1,2,2,3,3,4,4,1 /), shape=(/2,8/) )
integer(kind=4), dimension(2,8),  target, public :: h_p1_edge2vertices = reshape( (/0,0,0,0,0,0,0,0, &
                                                                                    0,0,0,0,0,0,0,0 /), shape=(/2,8/) )
integer(kind=4), dimension(2,20), target, public :: h_p2_edge2vertices = reshape( (/0,0,0,0,0,0,0,0, &
                                                                                    0,0,0,0,0,0,0,0, &
                                                                                    1,2,2,3,3,4,4,1, &
                                                                                    5,6,6,7,7,8,8,5, &
                                                                                    1,5,2,6,3,7,4,8 /), shape=(/2,20/) )
integer(kind=4), dimension(2,4) , target, public :: tep1_edge2vertices = reshape( (/0,0,0,0,0,0,0,0/), shape=(/2,4/) )
integer(kind=4), dimension(2,10), target, public :: tep2_edge2vertices = reshape( (/0,0,0,0,0,0,0,0, &
                                                                                    1,2,2,3,3,1    , &
                                                                                    1,4,2,4,3,4    /), shape=(/2,10/) )
integer(kind=4), dimension(2,6) , target, public :: prp1_edge2vertices = reshape( (/0,0,0,0,0,0,0,0,0,0,0,0/), shape=(/2,6/) )

!> information on vertices numbering of the elements :
!> triangle:
!> y
!> ^
!> |
!>
!> 3
!> | \
!> 6   5
!> |     \
!> 1---4--2 ->x
!
!> quadrangle:
!> y
!> ^
!> |
!>
!> 4---7---3
!> |       |
!> 8       6
!> |       |
!> 1---5---2 ->x
!
!> hexahedron:
!> 8----15----7
!> |\         |\
!> | 16       | 14
!> 20 \       19 \
!> |   5----13+---6
!> |   |      |   |
!> 4---+11----3   |
!>  \ 17       \  18
!>  12 |       10 |
!>    \|         \|
!>     1----9-----2
!
!> tetrahedron:
!>
!>                4
!>              ,/|`\
!>            ,/  |  `\
!>          ,8    '.   `10
!>        ,/       9     `\
!>      ,/         |       `\
!>     1--------7--'.--------3
!>      `\.         |      ,/
!>         `\.      |    ,6
!>            `5.   '. ,/
!>               `\. |/
!>                  `2
!
!> prism :
!>     eta
!>      ^  3
!>      | /| \
!>      |/ 9   8
!>      15 |     \
!>     /   1---7--2
!>    6   /      /
!>    | \13    14  ->ksi
!>    12/ 11   /
!>    |/    \ /
!>    4--10--5
!>
!>  /
!>|/_
!>zeta

CONTAINS

!-----------------------------------------------------
! Partie liee aux points de Gauss   
!-----------------------------------------------------

!> computes positions & weights of gauss points for a given quadrature rule
!> given pointers are automatically reallocated 
SUBROUTINE POS_GAUSS(I_TYPE_GAUSS,COORD_GAUSS,POIDS)
  !remplit les tableaux des coordonnees et des poids 
  !des points de Gauss

  IMPLICIT NONE

  !> gauss quadrature rule: LIG1|TR01|TR03|TR06|Q2*2|Q3*3|H222|H333|H225|TE01|TE04|PR06|PR15
  integer(kind=4), intent(in)   ::  i_type_gauss
  !> gauss point coordinates
  REAL(KIND=LONG) , POINTER     :: COORD_GAUSS(:,:) 
  !> weights
  REAL(KIND=LONG) , POINTER     :: POIDS(:)         
 
  ! local variables 
  REAL(KIND=LONG), PARAMETER    :: A=0.445948490915965_LONG,  &
                                   B=0.091576213509771_LONG       

  REAL(KIND=LONG), DIMENSION(2),PARAMETER ::  &
     X2=(/-0.577350269189626_LONG ,0.577350269189626_LONG /)  

  REAL(KIND=LONG), DIMENSION(3),PARAMETER ::  &
     X3=(/-0.774596669241483_LONG ,           ZERO        ,0.774596669241483_LONG /), &
     WL=(/ 0.555555555555556_LONG ,0.888888888888889_LONG ,0.555555555555556_LONG /)
  
  REAL(KIND=LONG), DIMENSION(5),PARAMETER ::  &
     Z5=(/           ZERO         ,   0.538469310105683_LONG, 0.906179845938664_LONG , &
          - 0.538469310105683_LONG, - 0.906179845938664_LONG /), &
     W5=(/  0.568888888888889_LONG,   0.478628670499366_LONG, 0.236926885056189_LONG ,&
            0.478628670499366_LONG,   0.236926885056189_LONG /)
   
  INTEGER           :: I,J,K,IG
  REAL(KIND=LONG)   :: aa,bb

  character(len=80) :: cout

  IF (ASSOCIATED(COORD_GAUSS)) DEALLOCATE(COORD_GAUSS)
  NULLIFY(COORD_GAUSS)
  IF (ASSOCIATED(POIDS)) DEALLOCATE(POIDS) 
  NULLIFY (POIDS)

  SELECT CASE(I_TYPE_GAUSS)
  CASE(i_LIG1)
    ! au centre
    ALLOCATE(COORD_GAUSS(1,1),POIDS(1))  
    COORD_GAUSS(1,1)=ZERO
    POIDS(1)=DEUX
    
  CASE(i_LIG2)
    ALLOCATE(COORD_GAUSS(1,2),POIDS(2))  
    COORD_GAUSS(1,1)= X2(2)
    COORD_GAUSS(1,2)=-X2(2)
    POIDS(1)=UN 
    POIDS(2)=UN 

  CASE(i_LIG3)
    ALLOCATE(COORD_GAUSS(1,3),POIDS(3))  
    COORD_GAUSS(1,1)=-X3(3)
    COORD_GAUSS(1,2)= ZERO
    COORD_GAUSS(1,3)= X3(3)
    POIDS(1)= WL(1)
    POIDS(2)= WL(2)
    POIDS(3)= WL(1)

  CASE(i_TR01)
    ALLOCATE(COORD_GAUSS(2,1),POIDS(1))  
    COORD_GAUSS(1,1)=UN/TROIS   ; COORD_GAUSS(2,1)=UN/TROIS
    POIDS(1)=US2
             
  CASE(i_TR03) 
    ALLOCATE(COORD_GAUSS(2,3),POIDS(3))  
    COORD_GAUSS(1,1)=UN/SIX       ;  COORD_GAUSS(2,1)=UN/SIX
    COORD_GAUSS(1,2)=DEUX/TROIS   ;  COORD_GAUSS(2,2)=UN/SIX
    COORD_GAUSS(1,3)=UN/SIX       ;  COORD_GAUSS(2,3)=DEUX/TROIS
    POIDS=UN/SIX 

  CASE(i_TR04)
    ALLOCATE(COORD_GAUSS(2, 4), POIDS(4))
    COORD_GAUSS(1, 1)=1.d0/3.d0    ;  COORD_GAUSS(2, 1)=1.d0/3.d0 ! 1/3 ; 1/3
    COORD_GAUSS(1, 2)=0.2d0        ;  COORD_GAUSS(2, 2)=0.2d0     ! 1/5 ; 1/5
    COORD_GAUSS(1, 3)=0.6d0        ;  COORD_GAUSS(2, 3)=0.2d0     ! 3/5 ; 1/5
    COORD_GAUSS(1, 4)=0.2d0        ;  COORD_GAUSS(2, 4)=0.6d0     ! 1/5 ; 3/5
    POIDS(1)  =-27.d0/96.d0
    POIDS(2:4)= 25.d0/96.d0

  CASE(i_TR06)
    ALLOCATE(COORD_GAUSS(2,6),POIDS(6)) 
    COORD_GAUSS(1,1)=A            ;  COORD_GAUSS(2,1)=A
    COORD_GAUSS(1,2)=UN-DEUX*A    ;  COORD_GAUSS(2,2)=A
    COORD_GAUSS(1,3)=A            ;  COORD_GAUSS(2,3)=UN-DEUX*A
    COORD_GAUSS(1,4)=B            ;  COORD_GAUSS(2,4)=B
    COORD_GAUSS(1,5)=UN-DEUX*B    ;  COORD_GAUSS(2,5)=B
    COORD_GAUSS(1,6)=B            ;  COORD_GAUSS(2,6)=UN-DEUX*B  
    POIDS(1:3)=0.111690794839005_LONG
    POIDS(4:6)=0.054975871827661_LONG        
  
  CASE(i_Q1x1)

    ! 4 ----------- 3
    !   |          |
    !   |    1     |
    !   |          |
    ! 1 ------------ 2

    ALLOCATE(COORD_GAUSS(2,1),POIDS(1))
    COORD_GAUSS(1:2,1) = ZERO
    POIDS(1) = DEUX*DEUX

  CASE(i_Q2x2) 

  !fd   4 ----------- 3
  !fd     | 3     4  |
  !fd     |          |
  !fd     | 1     2  |
  !fd   1 ------------ 2

    ALLOCATE(COORD_GAUSS(2,4),POIDS(4))
    DO I=1,2
      DO J=1,2
        IG=I+(J-1)*2
        COORD_GAUSS(1,IG)=X2(I)
        COORD_GAUSS(2,IG)=X2(J)
      ENDDO
    ENDDO
    POIDS(1:4)=UN 
                     
  CASE(i_Q3x3)
    ALLOCATE(COORD_GAUSS(2,9),POIDS(9))   
    DO I=1,3
      DO J=1,3
        IG=I+(J-1)*3
        COORD_GAUSS(1,IG)=X3(I)
        COORD_GAUSS(2,IG)=X3(J)
        POIDS(IG)=WL(I)*WL(J)
      ENDDO
    ENDDO 
 
  CASE(i_H222)
    ALLOCATE(COORD_GAUSS(3,8),POIDS(8))    
    DO I=1,2
      DO J=1,2
        DO K=1,2
          IG=I+(J-1)*2+(K-1)*4
          COORD_GAUSS(1,IG)=X2(I)
          COORD_GAUSS(2,IG)=X2(J)
          COORD_GAUSS(3,IG)=X2(K)
        ENDDO
      ENDDO
    ENDDO
    POIDS=UN
                      
  CASE(i_H333)
    ALLOCATE(COORD_GAUSS(3,27),POIDS(27))  
    DO I=1,3
      DO J=1,3
        DO K=1,3
          IG=I+(J-1)*3+(K-1)*9
          COORD_GAUSS(1,IG)=X3(I)
          COORD_GAUSS(2,IG)=X3(J)
          COORD_GAUSS(3,IG)=X3(K)
          POIDS(IG)=WL(I)*WL(J)*WL(K)
        ENDDO
      ENDDO
    ENDDO

  CASE(i_H225)
    ALLOCATE(COORD_GAUSS(3,20 + 1),POIDS(20 + 1))  
    DO I=1,2
      DO J=1,2
        DO K=1,5
          IG=K+(J-1)*5+(I-1)*10
          COORD_GAUSS(1,IG)=X2(I)
          COORD_GAUSS(2,IG)=X2(J)
          COORD_GAUSS(3,IG)=Z5(K)
          POIDS(IG)= W5(K)
        ENDDO
      ENDDO
    ENDDO
    COORD_GAUSS(:,IG + 1)=ZERO
    POIDS(IG + 1)=ZERO

  CASE(i_H115)
    ALLOCATE(COORD_GAUSS(3,5 + 1),POIDS(5 + 1))  
    DO K=1,5
       IG=K
       COORD_GAUSS(1,K)=0.D0
       COORD_GAUSS(2,K)=0.D0
       COORD_GAUSS(3,K)=Z5(K)
       POIDS(K)= W5(K)
    ENDDO
    COORD_GAUSS(:,IG + 1)=ZERO
    POIDS(IG + 1)=ZERO

  CASE(i_TE01)
    ALLOCATE(COORD_GAUSS(3,1),POIDS(1))  
    COORD_GAUSS(1,1)=UN/QUATRE   ; COORD_GAUSS(2,1)=UN/QUATRE ; COORD_GAUSS(3,1)=UN/QUATRE
    POIDS(1)=UN/SIX 
             
  CASE(i_TE04) 
    aa = (5.d0 - dsqrt(5.d0))/20.d0
    bb = (5 + 3.d0*dsqrt(5.d0))/20.d0

    ALLOCATE(COORD_GAUSS(3,4),POIDS(4))  
    COORD_GAUSS(1,1) = aa ;  COORD_GAUSS(2,1) = aa ; COORD_GAUSS(3,1) = aa  
    COORD_GAUSS(1,2) = aa ;  COORD_GAUSS(2,2) = aa ; COORD_GAUSS(3,2) = bb  
    COORD_GAUSS(1,3) = aa ;  COORD_GAUSS(2,3) = bb ; COORD_GAUSS(3,3) = aa
    COORD_GAUSS(1,4) = bb ;  COORD_GAUSS(2,4) = aa ; COORD_GAUSS(3,4) = aa
    POIDS=1.d0/24.d0 

  CASE(i_PR01)

    ALLOCATE(COORD_GAUSS(3,1),POIDS(1))  

    COORD_GAUSS(1,1) = 0.3333333333333333d0      ;  COORD_GAUSS(2,1) = 0.3333333333333333d0      ; COORD_GAUSS(3,1) = 0.0D0

    POIDS = 1.0d0

  CASE(i_PR06)

    ALLOCATE(COORD_GAUSS(3,6),POIDS(6))  

    ! schema de aster
    COORD_GAUSS(1,1) = 0.5d0      ;  COORD_GAUSS(2,1) = 0.5d0      ; COORD_GAUSS(3,1) = -sqrt(3.D0)/3.D0
    COORD_GAUSS(1,2) = 0.0d0      ;  COORD_GAUSS(2,2) = 0.5d0      ; COORD_GAUSS(3,2) = -sqrt(3.D0)/3.D0
    COORD_GAUSS(1,3) = 0.5d0      ;  COORD_GAUSS(2,3) = 0.0d0      ; COORD_GAUSS(3,3) = -sqrt(3.D0)/3.D0
    COORD_GAUSS(1,4) = 0.5d0      ;  COORD_GAUSS(2,4) = 0.5d0      ; COORD_GAUSS(3,4) =  sqrt(3.D0)/3.D0
    COORD_GAUSS(1,5) = 0.0d0      ;  COORD_GAUSS(2,5) = 0.5d0      ; COORD_GAUSS(3,5) =  sqrt(3.D0)/3.D0
    COORD_GAUSS(1,6) = 0.5d0      ;  COORD_GAUSS(2,6) = 0.0d0      ; COORD_GAUSS(3,6) =  sqrt(3.D0)/3.D0

    POIDS = UN/SIX

  CASE(i_PR08)

    ALLOCATE(COORD_GAUSS(3,8),POIDS(8))  

    ! schema de aster

    COORD_GAUSS(1,1) = UN/TROIS   ;  COORD_GAUSS(2,1) = UN/TROIS   ; COORD_GAUSS(3,1) = -0.577350269189626_LONG
    COORD_GAUSS(1,2) = 0.6d0      ;  COORD_GAUSS(2,2) = 0.2d0      ; COORD_GAUSS(3,2) = -0.577350269189626_LONG  
    COORD_GAUSS(1,3) = 0.2d0      ;  COORD_GAUSS(2,3) = 0.6d0      ; COORD_GAUSS(3,3) = -0.577350269189626_LONG
    COORD_GAUSS(1,4) = 0.2d0      ;  COORD_GAUSS(2,4) = 0.2d0      ; COORD_GAUSS(3,4) = -0.577350269189626_LONG
    COORD_GAUSS(1,5) = UN/TROIS   ;  COORD_GAUSS(2,5) = UN/TROIS   ; COORD_GAUSS(3,5) =  0.577350269189626_LONG
    COORD_GAUSS(1,6) = 0.6d0      ;  COORD_GAUSS(2,6) = 0.2d0      ; COORD_GAUSS(3,6) =  0.577350269189626_LONG
    COORD_GAUSS(1,7) = 0.2d0      ;  COORD_GAUSS(2,7) = 0.6d0      ; COORD_GAUSS(3,7) =  0.577350269189626_LONG
    COORD_GAUSS(1,8) = 0.2d0      ;  COORD_GAUSS(2,8) = 0.2d0      ; COORD_GAUSS(3,8) =  0.577350269189626_LONG
    
    
    POIDS(1) = -27.d0/96.d0       ;  POIDS(2) = 25.d0/96.d0          ;  POIDS(3) = 25.d0/96.d0         ;
    POIDS(4) = 25.d0/96.d0        ;  POIDS(5) = -27.d0/96.d0         ;  POIDS(6) = 25.d0/96.d0         ;  
    POIDS(7) = 25.d0/96.d0        ;  POIDS(8) = 25.d0/96.d0          ;  

  CASE(i_PR35)

    ALLOCATE(COORD_GAUSS(3,15 + 1),POIDS(15 + 1))  
    IG = 0
    DO K=1,5
          IG = IG + 1
          COORD_GAUSS(1,IG)=0.5d0
          COORD_GAUSS(2,IG)=0.5d0
          COORD_GAUSS(3,IG)=Z5(K)
          POIDS(IG)= W5(K)
    ENDDO
    DO K=1,5
          IG = IG + 1
          COORD_GAUSS(1,IG)=0.0d0
          COORD_GAUSS(2,IG)=0.5d0
          COORD_GAUSS(3,IG)=Z5(K)
          POIDS(IG)= W5(K)
    ENDDO
    DO K=1,5
          IG = IG + 1
          COORD_GAUSS(1,IG)=0.5d0
          COORD_GAUSS(2,IG)=0.0d0
          COORD_GAUSS(3,IG)=Z5(K)
          POIDS(IG)= W5(K)
    ENDDO
    COORD_GAUSS(:,IG + 1)=ZERO
    POIDS(IG + 1)=ZERO

  CASE(i_PR15)

    ALLOCATE(COORD_GAUSS(3,5 + 1),POIDS(5 + 1))  
    IG = 0
    DO K=1,5
          IG = IG + 1
          COORD_GAUSS(1,IG)=1.d0/3.d0
          COORD_GAUSS(2,IG)=1.d0/3.d0
          COORD_GAUSS(3,IG)=Z5(K)
          POIDS(IG)= W5(K)
    ENDDO
    COORD_GAUSS(:,IG + 1)=ZERO
    POIDS(IG + 1)=ZERO

  CASE(i_PR12)

    ALLOCATE(COORD_GAUSS(3,3 + 1),POIDS(3 + 1))  
    IG = 0
    IG = IG + 1
    COORD_GAUSS(1,IG)=1.d0/3.d0
    COORD_GAUSS(2,IG)=1.d0/3.d0
    COORD_GAUSS(3,IG)=ZERO
    POIDS(IG)= ZERO
    DO K=1,2
          IG = IG + 1
          COORD_GAUSS(1,IG)=1.d0/3.d0
          COORD_GAUSS(2,IG)=1.d0/3.d0
          COORD_GAUSS(3,IG)=X2(K)
          POIDS(IG)= 1.D0
    ENDDO
    COORD_GAUSS(:,IG + 1)=ZERO
    POIDS(IG + 1)=ZERO

  CASE DEFAULT
    write(cout,'(A,1x,A)') 'Unknown quadrature ID :', get_quadrature_from_id(i_type_gauss)
    call faterr('a_EF::POS_GAUSS',cout)
    
  END SELECT
END SUBROUTINE POS_GAUSS

!-----------------------------------------------------
! Partie liee aux fonctions de formes
!-----------------------------------------------------

!> evaluates element shape functions at a given node (in the reference frame)
!> pointers are automatically reallocated
SUBROUTINE FONCT_FORME(I_TYPE_FORME,KEZ,N)
  !------------------------------------------------------------------------------!
  !  remplit le tableau des fonctions de forme   N=[ N1 , N2 ,  ....   ]         !
  !------------------------------------------------------------------------------!  

  IMPLICIT NONE
 
  !> interpolation function type :T_P1|T_P2|Q_P1|Q_P2|H_P1|H_P2|TEP1|TEP2|PRP1|PRP2
  integer(kind=4), intent(in) :: I_TYPE_FORME
  !> node coordinates
  REAL(KIND=LONG)         :: KEZ(:) 
  !> array of value on each element node
  REAL(KIND=LONG),POINTER :: N(:)
      
  ! local variables
  REAL(KIND=LONG)         :: KSI,ETA,ZET,UMKME,UPK,UMK,UPE,UME,UMK2, &
                             UME2,UMZ,UPZ,UMZ2,UMKEZ

  REAL(KIND=LONG)         :: a,b,l

  character(len=80)       :: cout

  ! Initialisation des nouveaux pointeurs
  IF(ASSOCIATED(N)) DEALLOCATE(N)
  NULLIFY(N)

  SELECT CASE(SIZE(KEZ))
  CASE(1)
    KSI=KEZ(1)
  CASE(2)
    KSI=KEZ(1) ; ETA=KEZ(2)
  CASE(3)
    KSI=KEZ(1) ; ETA=KEZ(2)  ; ZET=KEZ(3)
  END SELECT
 
  SELECT CASE(I_TYPE_FORME)
  
  CASE(i_L_P1) ! segment lineaire
      ALLOCATE(N(2))
      N=RESHAPE((/ 0.5d0*(UN-KSI) , &
                   0.5d0*(UN+KSI) /),(/SIZE(N)/))
    
  CASE(i_L_P2) ! segment quadratique
      ALLOCATE(N(3))
      N=RESHAPE((/ -0.5d0*(UN-KSI)*KSI , &
                    0.5d0*(UN+KSI)*KSI , &
                    (UN-KSI)*(UN+KSI)   /),(/SIZE(N)/))
                    
  CASE(i_L_P3) ! segment cubique
      ALLOCATE(N(4))
      N=RESHAPE((/ (16.d0/9.d0) *(UN-KSI)*(KSI+(1.d0/3.d0))*( KSI-(1.d0/3.d0)) , &
                  -(16.d0/9.d0) *(UN+KSI)*(KSI+(1.d0/3.d0))*(-KSI+(1.d0/3.d0)) , &
                   (16.d0/27.d0)*(KSI-UN)*(KSI+UN)*(KSI-(1.d0/3.d0)) , &
                  -(16.d0/27.d0)*(KSI-UN)*(KSI+UN)*(KSI+(1.d0/3.d0)) /),(/SIZE(N)/))
    
  CASE(i_T_P1) ! triangle lineaire
      ALLOCATE(N(3))
      N=RESHAPE((/ UN-KSI-ETA   , &
           KSI          , &
           ETA           /),(/SIZE(N)/))
      
  CASE(i_T_P2) ! triangle quadratique      
      UMKME=UN-KSI-ETA  
      ALLOCATE(N(6))        
      N=RESHAPE((/ UMKME*(DEUX*UMKME-UN)   , &
           KSI  *(DEUX*KSI  -UN)   , &  
           ETA  *(DEUX*ETA  -UN)   , &
           QUATRE *UMKME *KSI      , &
           QUATRE *KSI   *ETA      , &
           QUATRE *ETA   *UMKME     /),(/SIZE(N)/))
      
  CASE(i_Q_P1) ! quadrilatere lineaire
      ALLOCATE(N(4))
      N=RESHAPE((/ US4*(UN-KSI)*(UN-ETA) , &
           US4*(UN+KSI)*(UN-ETA) , &      
           US4*(UN+KSI)*(UN+ETA) , &     
           US4*(UN-KSI)*(UN+ETA)  /),(/SIZE(N)/)) 
                
  CASE(i_Q_P2) ! quadrilatere quadratique     
      UPK =UN+KSI      ;  UPE =UN+ETA
      UMK =UN-KSI      ;  UME =UN-ETA
      UMK2=UN-KSI*KSI  ;  UME2=UN-ETA*ETA
      ALLOCATE(N(8))
      N=RESHAPE((/ -US4*UMK*UME*(UN+KSI+ETA)  , &
                   -US4*UPK*UME*(UN-KSI+ETA)  , &
                   -US4*UPK*UPE*(UN-KSI-ETA)  , &
                   -US4*UMK*UPE*(UN+KSI-ETA)  , &     
                    US2*UMK2*UME              , &
                    US2*UPK*UME2              , &
                    US2*UMK2*UPE              , &
                    US2*UMK*UME2               /),(/SIZE(N)/)) 

  CASE(i_QcP2) ! quadrilatere quadratique complet    
      UPK =UN+KSI      ;  UPE =UN+ETA
      UMK =UN-KSI      ;  UME =UN-ETA
      UMK2=UN-KSI*KSI  ;  UME2=UN-ETA*ETA
      ALLOCATE(N(9))
      N=RESHAPE((/  US4*KSI*ETA*UMK*UME      , &
                   -US4*KSI*ETA*UPK*UME      , &
                    US4*KSI*ETA*UPK*UPE      , &
                   -US4*KSI*ETA*UMK*UPE      , &     
                   -UMK2*ETA*UME*(1.d0/2.d0) , &
                    KSI*UPK*UME2*(1.d0/2.d0) , &
                    UMK2*ETA*UPE*(1.d0/2.d0) , &
                   -KSI*UMK*UME2*(1.d0/2.d0) , &
                    UME2*UMK2                 /),(/SIZE(N)/)) 

  CASE(i_H_P1) ! hexaedre lineaire           
      UPK=UN+KSI  ;  UPE=UN+ETA  ;  UPZ=UN+ZET
      UMK=UN-KSI  ;  UME=UN-ETA  ;  UMZ=UN-ZET
      ALLOCATE(N(8))
      N=RESHAPE((/  US8*UMK *UME *UMZ       , &
            US8*UPK *UME *UMZ       , &
            US8*UPK *UPE *UMZ       , &
            US8*UMK *UPE *UMZ       , &
            US8*UMK *UME *UPZ       , &
            US8*UPK *UME *UPZ       , &
            US8*UPK *UPE *UPZ       , &
            US8*UMK *UPE *UPZ       /),(/SIZE(N)/))
                 
  CASE(i_H_P2) !Hexaedre quadratique            
      UMK =UN-KSI     ; UME =UN-ETA      ;  UMZ =UN-ZET
      UPK =UN+KSI     ; UPE =UN+ETA      ;  UPZ =UN+ZET
      UMK2=UN-KSI*KSI ; UME2=UN-ETA*ETA  ;  UMZ2=UN-ZET*ZET
      ALLOCATE(N(20))
      N=RESHAPE((/  US8*UMK *UME *UMZ *(-DEUX-KSI-ETA-ZET)   , & !1
                    US8*UPK *UME *UMZ *(-DEUX+KSI-ETA-ZET)   , & !2
                    US8*UPK *UPE *UMZ *(-DEUX+KSI+ETA-ZET)   , & !3
                    US8*UMK *UPE *UMZ *(-DEUX-KSI+ETA-ZET)   , & !4
                    US8*UMK *UME *UPZ *(-DEUX-KSI-ETA+ZET)   , & !5
                    US8*UPK *UME *UPZ *(-DEUX+KSI-ETA+ZET)   , & !6
                    US8*UPK *UPE *UPZ *(-DEUX+KSI+ETA+ZET)   , & !7
                    US8*UMK *UPE *UPZ *(-DEUX-KSI+ETA+ZET)   , & !8
                    US4*UMK2*UME *UMZ                        , & !9  arretes face basse
                    US4*UPK *UME2*UMZ                        , & !10
                    US4*UMK2*UPE *UMZ                        , & !11
                    US4*UMK *UME2*UMZ                        , & !12
                    US4*UMK2*UME *UPZ                        , & !13 arretes face haute
                    US4*UPK *UME2*UPZ                        , & !14
                    US4*UMK2*UPE *UPZ                        , & !15
                    US4*UMK *UME2*UPZ                        , & !16 
                    US4*UMK *UME *UMZ2                       , & !17 arretes milieu
                    US4*UPK *UME *UMZ2                       , & !18
                    US4*UPK *UPE *UMZ2                       , & !19
                    US4*UMK *UPE *UMZ2                         & !20
                 /),(/SIZE(N)/))

  CASE(i_TEP1) ! tetraedre lineaire           
      UMKEZ=UN-KSI-ETA-ZET  
      ALLOCATE(N(4))
      N=RESHAPE((/  UMKEZ , &
                    KSI   , &
                    ETA   , &
                    ZET      /),(/SIZE(N)/))
                 
  CASE(i_TEP2) ! tetraedre quadratique            
      UMKEZ=UN-KSI-ETA-ZET
      ALLOCATE(N(10))
      N=RESHAPE((/  -UMKEZ*(1.d0 - 2.d0*UMKEZ)   , & !1
                    -KSI*(1.d0 - 2.d0*KSI)       , & !2
                    -ETA*(1.d0 - 2.d0*ETA)       , & !3
                    -ZET*(1.d0 - 2.D0*ZET)       , & !4
                    4.D0*KSI*UMKEZ               , & !5 arretes base
                    4.D0*KSI*ETA                 , & !6
                    4.d0*ETA*UMKEZ               , & !7
                    4.d0*ZET*UMKEZ               , & !8 arretes base vers sommet
                    4.D0*KSI*ZET                 , & !9
                    4.D0*ETA*ZET                 /),(/SIZE(N)/))

  CASE(i_PRP1) ! prisme lineaire
      l=1.d0 - ksi -eta
      a=0.5d0*(1.d0 - zet)
      b=0.5d0*(1.d0 + zet)
      ALLOCATE(N(6))
      N=RESHAPE((/  l*a , &
                    KSI*a   , &
                    ETA*a   , &
                    l*b , &
                    KSI*b   , &
                    ETA*b /),(/SIZE(N)/))

!~   CASE(i_PRP2) ! DA : prisme quadratique (code aster)
!~ 
!~       ALLOCATE(N(15))
!~       N=RESHAPE((/  eta*(1.d0 - ksi)*(2.d0*eta - 2.d0 - ksi)/2.d0 , &
!~                     zet*(1.d0 - ksi)*(2.d0*zet - 2.d0 - ksi)/2.d0 , &
!~                     (ksi - 1.d0)*(1.d0 - eta - zet)*(ksi + 2.d0*eta + 2.d0*zet)/2.d0 , &
!~                     eta*(1.d0 + ksi)*(2.d0*eta - 2.d0 + ksi)/2.d0 , &
!~                     zet*(1.d0 + ksi)*(2.d0*zet - 2.d0 + ksi)/2.d0 , &
!~                   (-ksi - 1.d0)*(1.d0 - eta - zet)*(- ksi + 2.d0*eta + 2.d0*zet)/2.d0 , &
!~                     2.d0*eta*zet*(1.d0 - ksi) , &
!~                     2.d0*zet*(1.d0 - eta - zet)*(1.d0 - ksi) , &
!~                     
!~                     2.d0*eta*(1.d0 - eta - zet)*(1.d0 - ksi) , &
!~                     eta*(1.d0 - ksi*ksi) , &
!~                     zet*(1.d0 - ksi*ksi) , &
!~                     (1.d0 - eta - zet)*(1.d0 - ksi*ksi) , &
!~                     2.d0*eta*zet*(1.d0 + ksi) , &
!~                     2.d0*zet*(1.d0 - eta - zet)*(1.d0 + ksi) , &
!~                     2.d0*eta*(1.d0 - eta - zet)*(1.d0 + ksi)/),(/SIZE(N)/))

  CASE(i_PRP2) ! quadratic wedge
      l= 1.d0 - ksi -eta
      a=-0.5d0*(1.d0 - zet)*zet
      b= 0.5d0*(1.d0 + zet)*zet
      umz2 = 1.d0 - zet*zet
      ALLOCATE(N(15))
      N=RESHAPE((/  l*(2.d0*  l-1.d0)*a, & !inf triangle vertices
                  ksi*(2.d0*ksi-1.d0)*a, &  
                  eta*(2.d0*eta-1.d0)*a, &
                    l*(2.d0*  l-1.d0)*b, & !sup triangle vertices
                  ksi*(2.d0*ksi-1.d0)*b, &  
                  eta*(2.d0*eta-1.d0)*b, &
                 4.d0*  l*ksi*a        , & !inf edges
                 4.d0*ksi*eta*a        , &
                 4.d0*eta*  l*a        , &
                 4.d0*  l*ksi*b        , & !sup edges
                 4.d0*ksi*eta*b        , &
                 4.d0*eta*  l*b        , &
                    l*umz2             , & !medium edges
                  ksi*umz2             , &
                  eta*umz2 /),(/size(N)/))
             
  CASE DEFAULT
    write(cout,'(A,1x,A)') 'Unknown interpolation ID :', get_interpolation_from_id(i_type_forme)
    call faterr('a_EF::FONCT_FORM',cout)

  END SELECT

END SUBROUTINE FONCT_FORME
            
!> evaluates element derivative of shape functions at a given node (in the reference frame)             
SUBROUTINE DERIVE_FORME(I_TYPE_FORME,KEZ,DN)
  !------------------------------------------------------------------------------!
  !  Remplit le tableau des derivees des fonctions de forme                      !   
  !  DN=[ dN1/dKSI , dN2/dKSI ,  ....     ]                                      !
  !     [ dN1/dETA , dN2/dETA ,  ...      ]                                      !
  !     [ dN1/dZET , dN2/dZET ,  ...      ]                                      !
  !------------------------------------------------------------------------------   
  
  IMPLICIT NONE
  !> available shape functions :T_P1|T_P2|Q_P1|Q_P2|H_P1|H_P2|TEP1|TEP2|PRP1
  integer(kind=4), intent(in) :: I_TYPE_FORME 
  !> node coordinates
  REAL(KIND=LONG)         :: KEZ(:) 
  !> array of values at each element node
  REAL(KIND=LONG),POINTER :: DN(:,:)
     
  ! local variables
  REAL(KIND=LONG)         :: KSI,ETA,ZET,UMKME,UPK,UMK,UPE,UME,UPZ,UMZ
  REAL(KIND=LONG)         :: UMK2,UME2,UMZ2,DKPE,DKME,KP2E,KM2E,UMKEZ,DKPU,DEPU,DKMU,DEMU
  REAL(KIND=LONG)         :: a,b,l

  character(len=80)       :: cout

  IF(ASSOCIATED(DN)) DEALLOCATE(DN)
  NULLIFY(DN)

  SELECT CASE(SIZE(KEZ))
  CASE(1)
    KSI=KEZ(1)
  CASE(2)
    KSI=KEZ(1) ; ETA=KEZ(2)
  CASE(3)
    KSI=KEZ(1) ; ETA=KEZ(2)  ; ZET=KEZ(3)
  END SELECT
 
  SELECT CASE(I_TYPE_FORME)

  CASE(i_L_P1) ! segment lineaire
    ALLOCATE(DN(1,2))
    DN=RESHAPE((/ -0.5d0   , 0.5d0   /),(/1,2/))

  CASE(i_L_P2) ! segment quadratique
    ALLOCATE(DN(1,3))
    DN=RESHAPE((/ 0.5d0*(UN-2.d0*KSI)   , 0.5d0*(UN+2.d0*KSI) , (UN-2.d0*KSI) /),(/1,3/))

  CASE(i_L_P3) ! segment cubique
    ALLOCATE(DN(1,4))
    DN=RESHAPE((/ - (16.d0/81.d0)*(27.d0*KSI*KSI-18.d0*KSI-UN)  , &
                  + (16.d0/81.d0)*(27.d0*KSI*KSI+18.d0*KSI-UN)  , & 
                  + (16.d0/81.d0)*(9.d0*KSI*KSI-2.d0*KSI-TROIS) , & 
                  - (16.d0/81.d0)*(9.d0*KSI*KSI+2.d0*KSI-TROIS) /),(/1,4/))

  CASE(i_T_P1) ! triangle lineaire
    ALLOCATE(DN(2,3))
    DN=RESHAPE((/ -UN   , -UN     , &
                   UN   ,  ZERO   , &
                   ZERO ,  UN      /),(/2,3/))
      
  CASE(i_T_P2) ! triangle quadratique      
    UMKME=UN-KSI-ETA
    ALLOCATE(DN(2,6))          
    DN=RESHAPE((/ UN-QUATRE*UMKME    , UN-QUATRE*UMKME    , & 
                  QUATRE*KSI-UN      , ZERO               , &
                  ZERO               , QUATRE*ETA-UN      , &
                  QUATRE*(UMKME-KSI) ,-QUATRE*KSI         , &
                  QUATRE*ETA         , QUATRE*KSI         , &
                 -QUATRE*ETA         , QUATRE*(UMKME-ETA)  /),(/2,6/))
           
  CASE(i_Q_P1) ! quadrilatre  lineaire
    ALLOCATE(DN(2,4))  
    DN=RESHAPE((/ -US4*(UN-ETA) , -US4*(UN-KSI) , & 
                   US4*(UN-ETA) , -US4*(UN+KSI) , &
                   US4*(UN+ETA) ,  US4*(UN+KSI) , &
                  -US4*(UN+ETA) ,  US4*(UN-KSI)  /),(/2,4/))

                
  CASE(i_Q_P2) ! quadrilatere quadratique     
    UPK =UN+KSI        ;  UPE =UN+ETA
    UMK =UN-KSI        ;  UME =UN-ETA
    UMK2=UN-KSI*KSI    ;  UME2=UN-ETA*ETA
    DKPE=DEUX*KSI+ETA  ;  DKME=DEUX*KSI-ETA
    KP2E=KSI+DEUX*ETA  ;  KM2E=KSI-DEUX*ETA
    ALLOCATE(DN(2,8))                 
    DN=RESHAPE((/ US4 *UME *DKPE , US4 *UMK *KP2E , &
                  US4 *UME *DKME ,-US4 *UPK *KM2E , &
                  US4 *UPE *DKPE , US4 *UPK *KP2E , &
                  US4 *UPE *DKME ,-US4 *UMK *KM2E , &      
                  -UME*KSI       ,-US2*UMK2       , &
                   US2*UME2      ,-UPK*ETA        , &     
                  -UPE*KSI       , US2*UMK2       , &
                  -US2*UME2      ,-UMK*ETA         /),(/2,8/))

  CASE(i_QcP2) ! quadrilatere quadratique complet
    UPK =UN+KSI        ;  UPE =UN+ETA
    UMK =UN-KSI        ;  UME =UN-ETA
    UMK2=UN-KSI*KSI    ;  UME2=UN-ETA*ETA
    DKPU=DEUX*KSI+UN   ;  DKMU=DEUX*KSI-UN
    DEPU=DEUX*ETA+UN   ;  DEMU=DEUX*ETA-UN
    ALLOCATE(DN(2,9))                 
    DN=RESHAPE((/-US4 *DKMU*ETA*UME      ,-US4 *KSI*UMK*DEMU     , &
                 -US4 *DKPU*ETA*UME      ,-US4 *KSI*UPK*DEMU     , &
                  US4 *DKPU*ETA*UPE      , US4 *KSI*UPK*DEPU     , &
                  US4 *DKMU*ETA*UPE      ,-US4 *KSI*UMK*DEPU     , &      
                  KSI*ETA*UME            , DEMU*UMK2*(1.d0/2.d0) , &
                  DKPU*UME2*(1.d0/2.d0)  ,-ETA*KSI*UPK           , &     
                 -KSI*ETA*UPE            , DEPU*UMK2*(1.d0/2.d0) , &
                  DKMU*UME2*(1.d0/2.d0)  , ETA*KSI*UMK           , &
                 -2.d0*KSI*UME2          , -2.d0*ETA*UMK2        /),(/2,9/))

  CASE(i_H_P1) ! hexaedre lineaire           
    UPK=UN+KSI  ;  UPE=UN+ETA  ;  UPZ=UN+ZET
    UMK=UN-KSI  ;  UME=UN-ETA  ;  UMZ=UN-ZET
    ALLOCATE(DN(3,8))
    DN=RESHAPE((/-US8*UME*UMZ ,-US8*UMK*UMZ ,-US8*UMK*UME , &
                  US8*UME*UMZ ,-US8*UPK*UMZ ,-US8*UPK*UME , &
                  US8*UPE*UMZ , US8*UPK*UMZ ,-US8*UPK*UPE , &
                 -US8*UPE*UMZ , US8*UMK*UMZ ,-US8*UMK*UPE , &
                 -US8*UME*UPZ ,-US8*UMK*UPZ , US8*UMK*UME , &
                  US8*UME*UPZ ,-US8*UPK*UPZ , US8*UPK*UME , &
                  US8*UPE*UPZ , US8*UPK*UPZ , US8*UPK*UPE , &
                 -US8*UPE*UPZ , US8*UMK*UPZ , US8*UMK*UPE  /),(/3,8/))
                 
  CASE(i_H_P2) !Hexaedre quadratique                   
    UMK =UN-KSI     ; UME =UN-ETA      ;  UMZ =UN-ZET
    UPK =UN+KSI     ; UPE =UN+ETA      ;  UPZ =UN+ZET
    UMK2=UN-KSI*KSI ; UME2=UN-ETA*ETA  ;  UMZ2=UN-ZET*ZET
    ALLOCATE(DN(3,20))
    DN(1,:)=(/ -US8*UME*UMZ*(-UN-DEUX*KSI-ETA-ZET) , &
                US8*UME*UMZ*(-UN+DEUX*KSI-ETA-ZET) , &     
                US8*UPE*UMZ*(-UN+DEUX*KSI+ETA-ZET) , &    
               -US8*UPE*UMZ*(-UN-DEUX*KSI+ETA-ZET) , &
               -US8*UME*UPZ*(-UN-DEUX*KSI-ETA+ZET) , &    
                US8*UME*UPZ*(-UN+DEUX*KSI-ETA+ZET) , &    
                US8*UPE*UPZ*(-UN+DEUX*KSI+ETA+ZET) , &
               -US8*UPE*UPZ*(-UN-DEUX*KSI+ETA+ZET) , &                            
               -US2*KSI *UME *UMZ                  , &
                US4*UME2*UMZ                       , &
               -US2*KSI *UPE *UMZ                  , &
               -US4*UME2*UMZ                       , &
               -US2*KSI *UME *UPZ                  , &
                US4*UME2*UPZ                       , &
               -US2*KSI *UPE *UPZ                  , &
               -US4*UME2*UPZ                       , &
               -US4*UME *UMZ2                      , &
                US4*UME *UMZ2                      , &
                US4*UPE *UMZ2                      , &
               -US4*UPE *UMZ2                        &
             /)

    DN(2,:)=(/ -US8*UMK*UMZ*(-UN-KSI-DEUX*ETA-ZET) , &
               -US8*UPK*UMZ*(-UN+KSI-DEUX*ETA-ZET) , &     
                US8*UPK*UMZ*(-UN+KSI+DEUX*ETA-ZET) , &
                US8*UMK*UMZ*(-UN-KSI+DEUX*ETA-ZET) , &
               -US8*UMK*UPZ*(-UN-KSI-DEUX*ETA+ZET) , &
               -US8*UPK*UPZ*(-UN+KSI-DEUX*ETA+ZET) , &
                US8*UPK*UPZ*(-UN+KSI+DEUX*ETA+ZET) , &
                US8*UMK*UPZ*(-UN-KSI+DEUX*ETA+ZET) , &               
               -US4*UMK2*UMZ                       , &
               -US2*ETA *UPK *UMZ                  , &
                US4*UMK2*UMZ                       , &
               -US2*ETA *UMK *UMZ                  , &
               -US4*UMK2*UPZ                       , &
               -US2*ETA *UPK *UPZ                  , &
                US4*UMK2*UPZ                       , &
               -US2*ETA *UMK *UPZ                  , &
               -US4*UMK *UMZ2                      , &
               -US4*UPK *UMZ2                      , &
                US4*UPK *UMZ2                      , &
                US4*UMK *UMZ2                        &
             /)

    DN(3,:)=(/ -US8*UMK*UME*(-UN-KSI-ETA-DEUX*ZET) , &
               -US8*UPK*UME*(-UN+KSI-ETA-DEUX*ZET) , &   
               -US8*UPK*UPE*(-UN+KSI+ETA-DEUX*ZET) , &  
               -US8*UMK*UPE*(-UN-KSI+ETA-DEUX*ZET) , &         
                US8*UMK*UME*(-UN-KSI-ETA+DEUX*ZET) , &
                US8*UPK*UME*(-UN+KSI-ETA+DEUX*ZET) , &
                US8*UPK*UPE*(-UN+KSI+ETA+DEUX*ZET) , &
                US8*UMK*UPE*(-UN-KSI+ETA+DEUX*ZET) , &               
               -US4*UMK2*UME                       , &
               -US4*UPK *UME2                      , &
               -US4*UMK2*UPE                       , &
               -US4*UMK *UME2                      , &
                US4*UMK2*UME                       , &
                US4*UPK *UME2                      , &
                US4*UMK2*UPE                       , &
                US4*UMK *UME2                      , &
               -US2*ZET *UMK *UME                  , &
               -US2*ZET *UPK *UME                  , &
               -US2*ZET *UPK *UPE                  , &
               -US2*ZET *UMK *UPE                    &
             /)

  CASE(i_TEP1) ! tetraedre lineaire
    ALLOCATE(DN(3,4))
    DN = RESHAPE((/ -UN   , -UN   , -UN   , &
                     UN   ,  ZERO ,  ZERO , &
                     ZERO ,  UN   ,  ZERO , &
                     ZERO ,  ZERO ,  UN      /),(/3,4/))
 
  CASE(i_TEP2) ! tetraedre quadratique
    UMKEZ=UN-KSI-ETA-ZET
    ALLOCATE(DN(3,10))
    DN = RESHAPE((/  1.d0 - 4.d0*UMKEZ   , 1.d0 - 4.d0*UMKEZ   , 1.d0 - 4.d0*UMKEZ , &
                     -1.d0 + 4.d0*KSI    , 0.D0                , 0.D0              , &
                     0.D0                , -1.D0 + 4.d0*ETA    , 0.D0              , &
                     0.d0                , 0.d0                , -1.d0 + 4.d0*ZET  , &
                     4.D0*(UMKEZ-KSI)    , -4.D0*KSI           , -4.D0*KSI         , &
                     4.d0*ETA            , 4.D0*KSI            , 0.D0              , &
                     -4.d0*ETA           , 4.d0*(UMKEZ - ETA)  , -4.d0*ETA         , &
                     -4.d0*ZET           , -4.d0*ZET           , 4.d0*(UMKEZ - ZET), &
                     4.d0*ZET            , 0.d0                , 4.d0*KSI          , &
                     0.d0                , 4.d0*ZET            , 4.d0*ETA          /),(/3,10/))
 
  CASE(i_PRP1) ! prisme lineaire
    ALLOCATE(DN(3,6))
    l=1.d0 - ksi - eta
    a=0.5*(1.d0 - zet)
    b=0.5*(1.d0 + zet)
    DN = RESHAPE((/ -a    , -a    , -0.5*l   , &
                     a    ,  ZERO , -0.5*ksi , &
                     ZERO ,   a   , -0.5*eta , &
                     -b   ,  -b   ,  0.5*l   , &
                      b   ,  ZERO ,  0.5*ksi , &
                     ZERO ,   b   ,  0.5*eta /),(/3,6/))

!~   CASE(i_PRP2) ! prisme quadratique
!~     ALLOCATE(DN(3,15))
!~ 
!~     DN(1,:) = (/ 0.5d0*(1.d0 - zet)*(4.d0*ksi + 4.d0*eta + zet - 2.d0) , &
!~                  2.d0 *(1.d0 - zet)*(1.d0 - 2.d0*ksi - eta)            , &
!~                  0.5d0*(1.d0 - zet)*(-2.d0 + 4.d0*ksi - zet)           , &
!~                  2.d0*(1.d0 - zet)*eta                                 , &
!~                  ZERO                                                  , &
!~                  -2.d0*(1.d0 - zet)*eta                                , &
!~                  -1.d0 + zet*zet                                       , &
!~                  1.d0 - zet*zet                                        , &
!~                  ZERO                                                  , &
!~                  0.5d0*(1.d0 + zet)*(4.d0*ksi + 4.d0*eta - zet - 2.d0) , &
!~                  2.d0 *(1.d0 + zet)*(1.d0 - 2.d0*ksi - eta)            , &
!~                  0.5d0*(1.d0 + zet)*(-2.d0 + 4.d0*ksi + zet)           , &
!~                  2.d0*(1.d0 + zet)*eta                                 , &
!~                  ZERO                                                  , &
!~                  -2.d0*(1.d0 + zet)*eta /)
!~                  
!~     DN(2,:) = (/ 0.5d0*(1.d0 - zet)*(4.d0*ksi + 4.d0*eta + zet - 2.d0) , &
!~                  -2.d0*(1.d0 - zet)*eta                                , &
!~                  ZERO                                                  , &
!~                  2.d0*(1.d0 - zet)*eta                                 , &
!~                  0.5d0*(1.d0 - zet)*(-2.d0 + 4.d0*eta - zet)           , &
!~                  2.d0 *(1.d0 - zet)*(1.d0 - ksi - 2.d0*eta)            , &
!~                  -1.d0 + zet*zet                                       , &
!~                  ZERO                                                  , &
!~                  1.d0 - zet*zet                                        , &
!~                  0.5d0*(1.d0 + zet)*(4.d0*ksi + 4.d0*eta - zet - 2.d0) , &
!~                  -2.d0*(1.d0 + zet)*ksi                                , &
!~                  ZERO                                                  , &
!~                  2.d0*(1.d0 + zet)*ksi                                 , &
!~                  0.5d0*(1.d0 + zet)*(- 2.d0 + 4.d0*eta + zet )         , &
!~                  2.d0 *(1.d0 + zet)*(1.d0 - 2.d0*eta - ksi) /)
!~ 
!~     DN(3,:) = (/ 0.5d0*(1.d0 - ksi - eta)*(2.d0*ksi + 2.d0*eta + 2.d0*zet - 1.d0) , &
!~                  -2.d0*ksi*(1.d0 - ksi - eta)                          , &
!~                  0.5d0*ksi*(1.d0 - 2.d0*ksi + 2.d0*zet)                , &
!~                  -2.d0*ksi*eta                                         , &
!~                  0.5d0*eta*(1.d0 - 2.d0*eta + 2.d0*zet)                , &
!~                  -2.d0*eta*(1.d0 - ksi - eta)                          , &
!~                  -2.d0*zet*(1.d0 - ksi - eta)                          , &
!~                  -2.d0*zet*ksi                                         , &
!~                  -2.d0*zet*eta                                         , &
!~                  0.5d0*(1.d0 - ksi - eta)*(-2.d0*ksi - 2.d0*eta + 2.d0*zet + 1.d0) , &
!~                  2.d0*ksi*(1.d0 - ksi - eta)                           , &
!~                  0.5d0*ksi*(-1.d0 + 2.d0*ksi + 2.d0*zet)               , &
!~                  2.d0*ksi*eta                                          , &
!~                  0.5d0*eta*(-1.d0 + 2.d0*eta + 2.d0*zet)               , &
!~                  2.d0*eta*(1.d0 - ksi - eta) /)

  CASE(i_PRP2) ! quadratic wedge
    ALLOCATE(DN(3,15))
    l = 1.d0 - ksi -eta
    a=-0.5d0*(1.d0 - zet)*zet
    b= 0.5d0*(1.d0 + zet)*zet
    umz2 = 1.d0 - zet*zet
    DN=reshape((/ (1.d0-4.d0*l  )*a, (1.d0-4.d0*l)*a  ,   l*(2.d0*  l-1.d0)*(zet-0.5d0), & 
                  (4.d0*ksi-1.d0)*a, 0.d0             , ksi*(2.d0*ksi-1.d0)*(zet-0.5d0), &
                  0.d0             , (4.d0*eta-1.d0)*a, eta*(2.d0*eta-1.d0)*(zet-0.5d0), &
                  (1.d0-4.d0*l  )*b, (1.d0-4.d0*l)*b  ,   l*(2.d0*  l-1.d0)*(zet+0.5d0), & 
                  (4.d0*ksi-1.d0)*b, 0.d0             , ksi*(2.d0*ksi-1.d0)*(zet+0.5d0), &
                  0.d0             , (4.d0*eta-1.d0)*b, eta*(2.d0*eta-1.d0)*(zet+0.5d0), &
                  4.d0*(l-ksi)*a   ,-4.d0*ksi*a       , 4.d0*  l*ksi*(zet-0.5d0)       , &
                  4.d0*eta*a       , 4.d0*ksi*a       , 4.d0*ksi*eta*(zet-0.5d0)       , &
                 -4.d0*eta*a       , 4.d0*(l-eta)*a   , 4.d0*eta*  l*(zet-0.5d0)       , &
                  4.d0*(l-ksi)*b   ,-4.d0*ksi*b       , 4.d0*  l*ksi*(zet+0.5d0)       , &
                  4.d0*eta*b       , 4.d0*ksi*b       , 4.d0*ksi*eta*(zet+0.5d0)       , &
                 -4.d0*eta*b       , 4.d0*(l-eta)*b   , 4.d0*eta*  l*(zet+0.5d0)       , &
                 -umz2             ,-umz2             ,-2.d0*l*zet                     , &
                  umz2             , 0.d0             ,-2.d0*ksi*zet                   , &
                  0.d0             , umz2             ,-2.d0*eta*zet          /),(/3,15/))
  CASE DEFAULT
    write(cout,'(A,1x,A)') 'Unknown interpolation ID :', get_interpolation_from_id(i_type_forme)
    call faterr('a_EF::DERIVE_FORME',cout)

  END SELECT

END SUBROUTINE DERIVE_FORME

!> evaluates element second derivative of shape functions at a given node (in the reference frame)             
subroutine second_forme(I_TYPE_FORME,KEZ,DDN)
  !------------------------------------------------------------------------------!
  !  Remplit le tableau des derivees des fonctions de forme                      !   
  !  transpose(DDN) =[ d2N1/dKSI2   , d2N2/dKSI2   ,  .... ]                     !
  !                  [ d2N1/dETA2   , d2N2/dETA2   ,  ...  ]                     !
  !                  [ d2N1/dZET2   , d2N2/dZET2   ,  ...  ]                     !
  !                  [ d2N1/dKSIdETA, d2N2/dKSIdETA,  ...  ]                     !
  !                  [ d2N1/dKSIdZET, d2N2/dKSIdZET,  ...  ]                     !
  !                  [ d2N1/dETAdZET, d2N2/dETAdZET,  ...  ]                     !
  !------------------------------------------------------------------------------!  
  implicit none
  !> available shape functions :T_P1|T_P2|Q_P1|Q_P2|H_P1|H_P2|TEP1|TEP2|PRP1
  integer(kind=4), intent(in) :: i_type_forme 
  !> node coordinates
  real(kind=long), intent(in) :: kez(:) 
  !> array of values at each element node
  real(kind=long), pointer :: ddn(:,:)
     
  ! local variables
  real(kind=long) :: KSI,ETA,ZET,UPK,UMK,UPE,UME,UPZ,UMZ
  real(kind=long) :: DKMU, DKPU, UP2E, UM2E, DKMDE, DKPDE, UMK2, UME2, UMZ2
  real(kind=long) :: a,b,l,da,db

  character(len=80)       :: cout

  select case(size(KEZ))
  case(1)
    KSI = KEZ(1)
  case(2)
    KSI = KEZ(1); ETA = KEZ(2)
  case(3)
    KSI = KEZ(1); ETA = KEZ(2); ZET = KEZ(3)
  end select
 
  select case(i_type_forme)

  case(i_L_P1) ! segment lineaire
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/2,1/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(2,1))
    ddn = zero

  case(i_L_P2) ! segment quadratique
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/3,1/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(3,1))
    ddn = reshape((/ -1.d0, 1.d0, -2.d0 /),(/3,1/))

  case(i_L_P3) ! segment cubique
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/4,1/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(4,1))
    ddn = reshape((/- (16.d0/81.d0)*(27.d0*KSI-18.d0), &
                    + (16.d0/81.d0)*(27.d0*KSI+18.d0), &
                    + (16.d0/81.d0)*( 9.d0*KSI- 2.d0), &
                    - (16.d0/81.d0)*( 9.d0*KSI+ 2.d0) /),(/4,1/))

  case(i_T_P1) ! triangle lineaire
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/3,3/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(3,3))
    ddn = zero

  case(i_T_P2) ! triangle quadratique
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/6,3/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(6,3))
    ddn(:,1) = (/ QUATRE, QUATRE, ZERO  , &
                 -DEUX  , ZERO  , ZERO  /)
    ddn(:,2) = (/ QUATRE, ZERO  , QUATRE, &
                  ZERO  , ZERO  ,-DEUX  /)
    ddn(:,3) = (/ QUATRE, ZERO  , ZERO, &
                 -QUATRE, QUATRE,-QUATRE/)

  case(i_Q_P1) ! quadrilatre  lineaire
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/4,3/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(4,3))
    ddn(:,1) = ZERO
    ddn(:,2) = ZERO
    ddn(:,1) = (/ US4,-US4, US4,-US4 /)

  case(i_Q_P2) ! quadrilatere quadratique
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/8,3/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(8,3))
    UPK   = UN+KSI ;  UPE =UN+ETA
    UMK   = UN-KSI ;  UME =UN-ETA
    DKPDE = DEUX*KSI+DEUX*ETA;
    DKMDE = DEUX*KSI-DEUX*ETA;
    ddn(:,1) = (/ US2*UME       , US2*UME       , US2*UPE       , US2*UPE       , &
                 -UME           , ZERO          ,-UPE           , ZERO           /)
    ddn(:,2) = (/ US2*UMK       , US2*UPK       , US2*UPK       , US2*UMK       , &
                  ZERO          ,-UPK           , ZERO          ,-UMK            /)
    ddn(:,3) = (/ US4*(UN-DKPDE),-US4*(UN+DKMDE), US4*(UN+DKPDE),-US4*(UN-DKMDE), &
                  KSI           ,-ETA           ,-KSI           , ETA            /)

  case(i_QcP2) ! quadrilatere quadratique complet
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/9,3/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(9,3))
    UPK =UN+KSI        ;  UPE =UN+ETA
    UMK =UN-KSI        ;  UME =UN-ETA
    UMK2=UN-KSI*KSI    ;  UME2=UN-ETA*ETA
    DKPU=DEUX*KSI+UN   ;  DKMU=DEUX*KSI-UN
    UM2E=UN-DEUX*ETA   ;  UP2E=UN+DEUX*ETA
    ddn(:,1) = (/-US2*ETA*UME,-US2*ETA*UME, US2*ETA*UPE, US2*ETA*UPE, &
                      ETA*UME, UME2       ,-    ETA*UPE, UME2       ,-DEUX*UME2 /)
    ddn(:,2) = (/-US2*KSI*UMK,-US2*KSI*UPK, US2*KSI*UPK,-US2*KSI*UMK, &
                  UMK2       ,-    KSI*UPK, UMK2       ,     KSI*UMK,-DEUX*UMK2 /)
    ddn(:,3) = (/-US4*DKMU*UM2E,-US4*DKPU*UM2E, US4*DKPU*UP2E, US4*DKMU*UP2E, &
                  KSI*UM2E     ,-DKPU*ETA     ,-KSI*UP2E     ,-DKMU*ETA, QUATRE*KSI*ETA /)

  case(i_H_P1) ! hexaedre lineaire
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/8,6/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(8,6))
    UPK=UN+KSI  ;  UPE=UN+ETA  ;  UPZ=UN+ZET
    UMK=UN-KSI  ;  UME=UN-ETA  ;  UMZ=UN-ZET
    ddn(:,1:3) = ZERO
    ddn(:,4) =(/ US8*UMZ,-US8*UMZ, US8*UMZ,-US8*UMZ, &
                 US8*UPZ,-US8*UPZ, US8*UPZ,-US8*UPZ /)
    ddn(:,5) =(/ US8*UME,-US8*UME,-US8*UPE, US8*UPE, &
                -US8*UME, US8*UME, US8*UPE,-US8*UPE /)
    ddn(:,6) =(/ US8*UMK, US8*UPK,-US8*UPK,-US8*UMK, &
                -US8*UMK,-US8*UPK, US8*UPK, US8*UMK /)

  case(i_H_P2) !Hexaedre quadratique
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/20,6/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(20,6))
    UMK =UN-KSI     ; UME =UN-ETA      ;  UMZ =UN-ZET
    UPK =UN+KSI     ; UPE =UN+ETA      ;  UPZ =UN+ZET
    UMK2=UN-KSI*KSI ; UME2=UN-ETA*ETA  ;  UMZ2=UN-ZET*ZET
    ddn(:,1) = (/ US4*UME*UMZ , US4*UME*UMZ , US4*UPE*UMZ , US4*UPE*UMZ , &
                  US4*UME*UPZ , US4*UME*UPZ , US4*UPE*UPZ , US4*UPE*UPZ , &
                 -US2*UME*UMZ , ZERO        ,-US2*UPE*UMZ , ZERO        , &
                 -US2*UME*UPZ , ZERO        ,-US2*UPE*UPZ , ZERO        , &
                  ZERO        , ZERO        , ZERO        , ZERO        /)

    ddn(:,2) = (/ US4*UMK*UMZ, US4*UPK*UMZ, US4*UPK*UMZ, US4*UMK*UMZ, &
                  US4*UMK*UPZ, US4*UPK*UPZ, US4*UPK*UPZ, US4*UMK*UPZ, &
                  ZERO       ,-US2*UPK*UMZ, ZERO       ,-US2*UMK*UMZ, &
                  ZERO       ,-US2*UPK*UPZ, ZERO       ,-US2*UMK*UPZ, &
                  ZERO       , ZERO       , ZERO       , ZERO         /)

    ddn(:,3) = (/ US4*UMK*UME, US4*UPK*UME, US4*UPK*UPE, US4*UMK*UPE, &
                  US4*UMK*UME, US4*UPK*UME, US4*UPK*UPE, US4*UMK*UPE, &
                  ZERO       , ZERO       , ZERO       , ZERO       , &
                  ZERO       , ZERO       , ZERO       , ZERO       , &
                 -US2*UMK*UME,-US2*UPK*UME,-US2*UPK*UPE,-US2*UMK*UPE /)

    ddn(:,4) = (/ US8*UMZ*(-DEUX*KSI-DEUX*ETA-ZET) ,-US8*UMZ*(+DEUX*KSI-DEUX*ETA-ZET) , &
                  US8*UMZ*(+DEUX*KSI+DEUX*ETA-ZET) ,-US8*UMZ*(-DEUX*KSI+DEUX*ETA-ZET) , &
                  US8*UPZ*(-DEUX*KSI-DEUX*ETA+ZET) ,-US8*UPZ*(+DEUX*KSI-DEUX*ETA+ZET) , &
                  US8*UPZ*(+DEUX*KSI+DEUX*ETA+ZET) ,-US8*UPZ*(-DEUX*KSI+DEUX*ETA+ZET) , &
                  US2*KSI*UMZ ,-US2*ETA*UMZ ,-US2*KSI*UMZ , US2*ETA*UMZ , &
                  US2*KSI*UPZ ,-US2*ETA*UPZ ,-US2*KSI*UPZ , US2*ETA*UPZ , &
                  US4*UMZ2    ,-US4*UMZ2    , US4*UMZ2    , -US4*UMZ2   /)

    ddn(:,5) = (/-US8*UME*(     DEUX*KSI+ETA+DEUX*ZET) , US8*UME*(    -DEUX*KSI+ETA+DEUX*ZET) , &
                  US8*UPE*(    -DEUX*KSI-ETA+DEUX*ZET) ,-US8*UPE*(     DEUX*KSI-ETA+DEUX*ZET) , &
                 -US8*UME*(DEUX+DEUX*KSI+ETA-DEUX*ZET) , US8*UME*(DEUX-DEUX*KSI+ETA-DEUX*ZET) , &
                  US8*UPE*(DEUX-DEUX*KSI-ETA-DEUX*ZET) ,-US8*UPE*(DEUX+DEUX*KSI-ETA-DEUX*ZET) , &
                  US2*KSI *UME ,-US4*UME2     , US2*KSI *UPE , US4*UME2     , &
                 -US2*KSI *UME , US4*UME2     ,-US2*KSI *UPE ,-US4*UME2     , &
                  US2*UME*ZET  ,-US2*UME*ZET  ,-US2*UPE*ZET  , US2*UPE*ZET  /)

    ddn(:,6) = (/-US8*UMK*( KSI+DEUX*ETA+DEUX*ZET) ,-US8*UPK*(-KSI+DEUX*ETA+DEUX*ZET) , &
                  US8*UPK*(-KSI-DEUX*ETA+DEUX*ZET) , US8*UMK*( KSI-DEUX*ETA+DEUX*ZET) , &
                 -US8*UMK*(-KSI-DEUX*ETA+DEUX*ZET) ,-US8*UPK*( KSI-DEUX*ETA+DEUX*ZET) , &
                  US8*UPK*( KSI+DEUX*ETA+DEUX*ZET) , US8*UMK*(-KSI+DEUX*ETA+DEUX*ZET) , &
                  US4*UMK2    , US2*ETA*UPK ,-US4*UMK2    , US2*ETA*UMK , &
                 -US4*UMK2    ,-US2*ETA*UPK , US4*UMK2    ,-US2*ETA*UMK , &
                  US2*UMK*ZET , US2*UPK*ZET ,-US2*UPK*ZET ,-US2*UMK*ZET  /)

  case(i_TEP1) ! tetraedre lineaire
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/4,6/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(4,6))
    ddn = ZERO

  case(i_TEP2) ! tetraedre quadratique
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/10,6/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(10,6))
    ddn(:,1) = (/ 4.d0, 4.d0, 0.d0, 0.d0,-8.d0 , &
                  0.d0, 0.d0, 0.d0, 0.d0, 0.d0 /)
 
    ddn(:,2) = (/ 4.d0, 0.d0, 4.d0, 0.d0, 0.d0 , &
                  0.d0,-8.d0, 0.d0, 0.d0, 0.d0  /)

    ddn(:,3) = (/ 4.d0, 0.d0, 0.d0, 4.d0, 0.d0 , &
                  0.d0, 0.d0,-8.d0, 0.d0, 0.d0 /)

    ddn(:,4) = (/ 4.d0, 0.d0, 0.d0, 0.d0,-4.d0 , &
                  4.d0,-4.d0, 0.d0, 0.d0, 0.d0 /)
 
    ddn(:,5) = (/ 4.d0, 0.d0, 0.d0, 0.d0,-4.d0 , &
                  0.d0, 0.d0,-4.d0, 4.d0, 0.d0 /)
 
    ddn(:,6) = (/ 4.d0, 0.d0, 4.d0, 0.d0 , 0.d0 , &
                  0.d0,-4.d0,-4.d0, 0.d0 , 4.d0  /)

  case(i_PRP1) ! prisme lineaire
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/6,6/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(6,6))

    ddn(:,1:4) = ZERO

    ddn(:,5) = (/ 0.5d0 ,-0.5d0, ZERO ,-0.5d0, 0.5d0, ZERO /)
    ddn(:,6) = (/ 0.5d0 , ZERO ,-0.5d0,-0.5d0, ZERO , 0.5d0/)


  case(i_PRP2) ! quadratic wedge
    if(associated(DDN)) then
      if( any(shape(DDN)/=(/15,6/)) ) then
        deallocate(DDN)
        nullify(DDN)
      end if
    end if
    if(.not. associated(DDN)) allocate(ddn(15,6))
    l  = 1.d0 - ksi -eta
    a  =-0.5d0*(1.d0 - zet)*zet
    b  = 0.5d0*(1.d0 + zet)*zet
    da =-0.5d0*(1.d0-2.d0*zet)
    db = 0.5d0*(1.d0+2.d0*zet)
    umz2 = 1.d0 - zet*zet

    ddn(:,1) = (/ 4.d0*a , 4.d0*a, 0.d0 , &
                  4.d0*b , 4.d0*b, 0.d0 , &
                 -8.d0*a , 0.d0  , 0.d0 , &
                 -8.d0*b , 0.d0  , 0.d0 , &
                  0.d0   , 0.d0  , 0.d0 /)

    ddn(:,2) = (/ 4.d0*a , 0.d0  , 4.d0*a , &
                  4.d0*b , 0.d0  , 4.d0*b , &
                  0.d0   , 0.d0  ,-8.d0*a , &
                  0.d0   , 0.d0  ,-8.d0*b , &
                  0.d0   , 0.d0  , 0.d0   /)

    ddn(:,3) = (/ l*(2.d0*l-1.d0), ksi*(2.d0*ksi-1.d0), eta*(2.d0*eta-1.d0), &
                  l*(2.d0*l-1.d0), ksi*(2.d0*ksi-1.d0), eta*(2.d0*eta-1.d0), &
                  4.d0*l*ksi     , 4.d0*ksi*eta       , 4.d0*eta*l         , &
                  4.d0*l*ksi     , 4.d0*ksi*eta       , 4.d0*eta*l         , &
                 -2.d0*l         ,-2.d0*ksi*zet       ,-2.d0*eta*zet       /)

    ddn(:,4) = (/ 4.d0*a , 0.d0   , 0.d0   , &
                  4.d0*b , 0.d0   , 0.d0   , &
                 -4.d0*a , 4.d0*a ,-4.d0*a , &
                 -4.d0*b , 4.d0*b ,-4.d0*b , &
                  0.d0   , 0.d0   , 0.d0   /)

    ddn(:,5) = (/ (1.d0-4.d0*l  )*da, (4.d0*ksi-1.d0)*da, 0.d0       , &
                  (1.d0-4.d0*l  )*db, (4.d0*ksi-1.d0)*db, 0.d0       , &
                  4.d0*(l-ksi)*da   , 4.d0*eta*da       ,-4.d0*eta*da, &
                  4.d0*(l-ksi)*db   , 4.d0*eta*db       ,-4.d0*eta*db, &
                  2.d0*zet          ,-2.d0*zet          , 0.d0       /)

    ddn(:,6) = (/ (1.d0-4.d0*l)*da  , 0.d0        , (4.d0*eta-1.d0)*da, &
                  (1.d0-4.d0*l)*db  , 0.d0        , (4.d0*eta-1.d0)*db, &
                 -4.d0*ksi*da       , 4.d0*ksi*da , 4.d0*(l-eta)*da  , &
                 -4.d0*ksi*db       , 4.d0*ksi*db , 4.d0*(l-eta)*db  , &
                  2.d0*zet          , 0.d0        ,-2.d0*zet          /)

  case default
    write(cout,'(A,1x,A)') 'Unknown interpolation ID :', get_interpolation_from_id(i_type_forme)
    call faterr('a_EF::dderive_forme',cout)

  end select

end subroutine second_forme

!> evaluates positions of an element nodes in the reference frame
!> \todo needs to be achieved for all types of element 
subroutine get_Nodes_Coor_Ref(i_Fonc_Forme, Coor_Ref_N)
  ! fonction qui renvoie les coordonnees, dans l'element de reference, 
  ! des noeuds d'un element
  ! on les donne facon LMGC: 
  !   Coor_Ref_N(j, i) : la composante j de la position du noeud i
  ! N.B.: faire un "merge" avec la fonction "d-LibUtilf/NodesRefCoor.f" des routines de DD  

  implicit none

  ! variables d'entree:
  !> interpolation function type :T_P1|T_P2|Q_P1|Q_P2|H_P1|H_P2|TEP1|TEP2|PRP1|PRP2
  integer(kind=4), intent(in) :: i_Fonc_Forme ! type de la fonction de forme de l'element
 
  ! variables de sortie:
  !> array of coordinates
  real(kind=8), dimension(:, :), pointer :: Coor_Ref_N  ! les coordonnees de reference des noeuds du maillage
   
  ! variables locales :     123456789012345678901234
  character(len=24) :: IAM='a_EF::get_Nodes_Coor_Ref'
  character(len=80) :: cout
   
  ! si le pointeur servant a stocker les coordonnees de references des
  ! noeuds est deja allouee
  if (associated(Coor_Ref_N)) then
  
    ! on le desalloue
    deallocate(Coor_Ref_N)
    ! on indique qu'il n'est plus alloue
    nullify(Coor_Ref_N)
   
  end if
   
  ! en fonction du type de fonction de forme considere
  select case(i_Fonc_Forme)

  case(i_T_P1) ! cas du triangle a trois noeuds
      
    ! on alloue le pointeur
    allocate(Coor_Ref_N(2, 3))
    ! on donne les coordonnées
    Coor_Ref_N(1:2, 1) =  (/ 0.d0 , 0.d0 /) 
    Coor_Ref_N(1:2, 2) =  (/ 1.d0 , 0.d0 /)
    Coor_Ref_N(1:2, 3) =  (/ 0.d0 , 1.d0 /)

  case(i_Q_P1) ! cas du quadrangle a quatre neouds

    ! on alloue le pointeur
    allocate(Coor_Ref_N(2, 4))
    ! on donne les coordonnees
    Coor_Ref_N(1:2, 1) = (/ -1.d0, -1.d0 /)
    Coor_Ref_N(1:2, 2) = (/  1.d0, -1.d0 /)
    Coor_Ref_N(1:2, 3) = (/  1.d0,  1.d0 /)
    Coor_Ref_N(1:2, 4) = (/ -1.d0,  1.d0 /)

  case default

    ! si on ne reconnait pas le type d'élément
    ! on affiche un mesage d'erreur
    write(cout,'(A,1x,A)') 'unimplemented element type', get_interpolation_from_id(i_Fonc_Forme)
    call FATERR(IAM, cout)


  end select

end subroutine get_Nodes_Coor_Ref

!> evaluates the coordinates of gauss points of a given element for a given quadrature
SUBROUTINE get_GP_Coor(NNOE,I_TYPE_FORME,I_TYPE_GAUSS,ndime,node_coor,gp_coor)

  IMPLICIT NONE

  !> number of nodes of the element
  INTEGER,intent(in)           :: NNOE
  !> element interpolation
  integer(kind=4), intent(in)  :: i_TYPE_FORME
  !> element quadrature rule
  integer(kind=4), intent(in)  :: i_TYPE_GAUSS 
  !> space dimension
  integer,intent(in) :: ndime

  !> node coordinates (ndime,nnoe)
  REAL(KIND=LONG), INTENT(IN)  ::node_coor(:,:)
 
  !> gp coordinates (ndime,nnoe)
  REAL(KIND=LONG),pointer ::gp_coor(:,:)

  ! ****
  ! local variables

  ! gauss point coordinates in reference frame (ndime,ngp)
  REAL(kind=long), POINTER      :: CG(:,:)

  ! weights (ngp)
  REAL(KIND=LONG), POINTER      :: POIDS(:)

  ! element shape function value at gp (nnoe)
  REAL(KIND=LONG), POINTER      :: N(:)

  integer :: ngp,ig,in,id

  !                         1234567890123
  CHARACTER(len=13) :: IAM='a_EF::GP_coor'

  NULLIFY(N)

  ! calcul des coordonnees et des poids
  NULLIFY(CG,POIDS)
  CALL pos_gauss(i_TYPE_GAUSS,CG,POIDS)

  ngp = size(poids)

  if (associated(gp_coor)) deallocate(gp_coor)

  allocate(gp_coor(ndime,ngp))

  DO ig=1,ngp
 
    CALL fonct_forme(i_TYPE_FORME,CG(:,ig),N)

    ! calcul des coordonnees du point de Gauss dans l'espace reel
    do id=1,ndime
      gp_coor(id,ig)=DOT_PRODUCT(N(1:nnoe),node_coor(id,1:nnoe))
    enddo

    deallocate(N)
    NULLIFY(N)

  enddo 
  
  deallocate(CG,POIDS)
  nullify(CG,POIDS)

END SUBROUTINE get_GP_Coor

!> gives the nearest gauss points to a given coordinate of a given element for a given quadrature
subroutine get_nearest_GP(NNOE,I_TYPE_FORME,I_TYPE_GAUSS,ndime,node_coor,coor,idgp)
  implicit none
  !> number of nodes of the element
  integer, intent(in) :: NNOE
  !> element interpolation
  integer, intent(in) :: i_TYPE_FORME
  !> element quadrature rule
  integer, intent(in) :: i_TYPE_GAUSS 
  !> space dimension
  integer, intent(in) :: ndime
  !> node coordinates (ndime,nnoe)
  real(kind=LONG), intent(in)  :: node_coor(:,:)
  !> gp coordinates (ndime,nnoe)
  real(kind=LONG), intent(out) ::coor(:)
  !> id of the gp
  integer, intent(out) :: idgp  
  ! ****
  ! local variables
  ! gp coordinates (ndime,nnoe)
  real(kind=LONG), dimension(:,:), pointer :: gp_coor
  ! un vec bidon de dim 3   
  real(kind=8),dimension(3) :: vec
  real(kind=8) :: d2,dmin
  integer :: ngp,ig,in,id
  !                         12345678901234567890
  character(len=20) :: IAM='a_EF::get_nearest_GP'

  nullify(gp_coor)
  call get_gp_coor(nnoe, I_TYPE_FORME,I_TYPE_GAUSS,ndime,node_coor,gp_coor)

  ngp  = size(gp_coor,dim=2)
  dmin = 1.d+20

  do ig = 1,ngp

    vec=0.d0
    vec(1:ndime)=gp_coor(1:ndime,ig)-coor(1:ndime)
    d2 = dot_product(vec,vec)
    if (d2 < dmin) then
      dmin = d2
      idgp=ig 
    end if  

  end do

  deallocate(gp_coor)
  nullify(gp_coor)

end subroutine

!> Compute the value of a field at a given point of an element using its interpolation functions
subroutine interpolate_field(field,nb_nodes,i_type_form,space_dim,X,res)
  implicit none
  !> number of nodes of the element
  integer(kind=4), intent(in) :: nb_nodes
  !> values at nodes of the field to interpolate
  real(kind=8), dimension(nb_nodes), intent(in) :: field
  !> space dimension
  integer(kind=4), intent(in) :: space_dim
  !> coordinates of the point in the element at which to interpolate
  real(kind=8), dimension(space_dim), intent(in) :: X
  !> interpolation type id
  integer(kind=4), intent(in) :: i_type_form
  !> interpolated value
  real(kind=8) :: res
  !
  real(kind=8), dimension(:), pointer :: N

  N => null()

  res = 0.d0

  call fonct_forme(i_type_form,X,N)

  res = dot_product( N,field )

  if( associated(N) ) then
    deallocate(N)
    nullify(N)
  end if

end subroutine

!> evaluates the integral of a scalar field over a given element with a given quadrature rule
SUBROUTINE INTEGRATE_field(field,NNOE,I_TYPE_FORME,I_TYPE_GAUSS,ndime,is_axi,X,res)

  IMPLICIT NONE

  !> value of the field at gauss points (ngp)
  REAL(KIND=LONG), INTENT(IN)  :: field(:)

  !> number of nodes of the element
  INTEGER                      :: NNOE
  !> element interpolation
  integer(kind=4), intent(in)  :: I_TYPE_FORME
  !> element quadrature rule
  integer(kind=4), intent(in)  :: I_TYPE_GAUSS 
  !> space dimension  
  integer                      :: ndime
  !> is axisymetric computation
  logical                      :: is_axi
  !> node coordinates (ndime,nnoe)
  REAL(KIND=LONG), INTENT(IN)  :: X(:,:)
  !> integral of the function over the element
  REAL(KIND=LONG), INTENT(OUT)  :: res

  ! ****
  ! local variables

  ! gauss point coordinates in reference frame (ndime,ngp)
  REAL(kind=long), POINTER      :: CG(:,:)

  ! weights (ngp)
  REAL(KIND=LONG), POINTER      :: POIDS(:)

  ! element shape function value at gp (nnoe)
  REAL(KIND=LONG), POINTER      :: N(:)

  ! element shape function value derivative at gp (ndime,nnoe)
  REAL(KIND=LONG), POINTER      :: DN(:,:)

  REAL(KIND=LONG)               :: DETJ
  ! quadrature weight 
  REAL(KIND=LONG)               :: COEFINT

  ! jacobienne (ndime,ndime)
  REAL(KIND=LONG), ALLOCATABLE :: J(:,:)

  ! coordinate of the gauss points (ndime)
  REAL(KIND=LONG),allocatable :: gp_coor(:)

  integer :: ngp,ig,in,id

  ! variables utilisees dans le cas des elements de
  ! dimension inferieure a la dimension de l'espace reel
  ! (e.g. un triangle en 3D)
  real(kind=8), dimension(3) :: v1, v2 
  real(kind=8) :: tmp

  !                         123456789012345678901
  CHARACTER(len=21) :: IAM='a_EF::Integrate_Field'
  CHARACTER(len=80) :: cout

  if (is_axi .and. ndime /= 2) CALL FATERR(IAM,'axisymetry is only available in 2D')

  NULLIFY(N,DN)

  ! calcul des coordonnees et des poids des points de gauss
  NULLIFY(CG,POIDS)
  CALL pos_gauss(I_TYPE_GAUSS,CG,POIDS)

  ngp = size(poids)

  allocate(gp_coor(ndime))

  res=0.d0

  ! selon la dimension
  SELECT CASE(ndime)
  ! cas 2D
  CASE(2) 
    ! selon le type d'element, i.e. de fonction de forme
    select case(i_type_forme)
    ! cas des elements surfaciques cas standard : element surfacique en 2D
    case(i_T_P1, i_T_P2, i_Q_P1, i_Q_P2) 

      IF (.NOT. ALLOCATED(J)) ALLOCATE(J(ndime,ndime))
      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(I_TYPE_FORME,CG(:,ig),DN)

        ! Calcul de la matrice Jacobienne et de son determinant
         
        J=ZERO

        J(1,:)=(/ DOT_PRODUCT(DN(1,:),X(1,:)) , DOT_PRODUCT(DN(1,:),X(2,:)) /)
        J(2,:)=(/ DOT_PRODUCT(DN(2,:),X(1,:)) , DOT_PRODUCT(DN(2,:),X(2,:)) /)                

        DETJ=J(1,1)*J(2,2)-J(1,2)*J(2,1)

        COEFINT=DETJ*POIDS(ig)

        IF (is_axi) THEN
          ! calcul des fonctions de formes au point de Gauss
          CALL fonct_forme(I_TYPE_FORME,CG(:,ig),N)
          ! calcul des coordonnees du point de Gauss dans l'espace reel
          do id=1,ndime
            gp_coor(id)=DOT_PRODUCT(N(1:nnoe),X(id,1:nnoe))
          enddo

          ! on multiplie par le rayon
          COEFINT=COEFINT*(6.2831853_LONG)*gp_coor(1)
          deallocate(N)
          NULLIFY(N)

        ENDIF

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig)

        deallocate(DN)
        NULLIFY(DN) 
      enddo 
      deallocate(J)

    ! cas des elements lineiques, immerges dans un espace 2D 
    ! idee generale : on voit l'element lineique de reference, comme l arete d'un element surfacique de reference
    ! e.g. un segment quadratique a 3 noeuds, comme la face eta=-1 d'un quadrangle quadratique a 8 noeuds
    ! Soit s la parametrisation de l'element surfacique (ksi de l'element surfacique de reference correspondant).
    ! Sur l'element surfacique, un point est repere par :
    !    x = Ni(s) xi
    ! Le jacobien de la transformation utilisee pour realiser le changement de variable (ksi, eta) -> s
    ! s'ecrit : 
    !    J_S = | dx/d(s)|

    case(i_L_P2) 
      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(i_Q_P2, (/ CG(1, ig), -1.d0 /), DN)

        ! on calcule :
        !   * v1 = dx/d(s), i.e. v1 = dNi/d(ksi) xi
        v1=0.d0
        !calcul des composantes de v1          
        ! mapping entre noeuds i_L_P2 (1,2,3) <-> i_Q_P2 (1,2,5) 
        do id=1,ndime
          v1(id) = DN(1, 1)*X(id, 1) +  DN(1, 2)*X(id, 2) +  DN(1, 5)*X(id, 3)
        end do

        ! on en deduit det(J) = J_S = | v1 |
        detJ = length2(v1) 

        ! on peut alors calculer : w_PG*det(J)
        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig) 

        deallocate(DN)
        NULLIFY(DN)
      enddo


    !>TODO: les autres elements lineiques, immerge dans un espace 2D (cf. Touzot p 214)
    case default ! cas general
      write(cout,'(A,1x,A,1x,A)') 'integration using', get_interpolation_from_id(I_TYPE_FORME), 'in 2D is not defined!'
      call faterr(IAM, cout)
    end select

  CASE(3)
    ! selon le type d'element, i.e. de fonction de forme
    select case(i_type_forme)
    ! cas des elements volumiques, 
    case(i_H_P1, i_H_P2, i_TEP1, i_TEP2, i_PRP1) 
     
      IF (.NOT. ALLOCATED(J)) ALLOCATE(J(ndime,ndime))

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(I_TYPE_FORME,CG(:,ig),DN)

        ! Calcul de la matrice Jacobienne et de son determinant

        J=ZERO

        J(1,:)=(/DOT_PRODUCT(DN(1,:),X(1,:)),DOT_PRODUCT(DN(1,:),X(2,:)),DOT_PRODUCT(DN(1,:),X(3,:)) /)
        J(2,:)=(/DOT_PRODUCT(DN(2,:),X(1,:)),DOT_PRODUCT(DN(2,:),X(2,:)),DOT_PRODUCT(DN(2,:),X(3,:)) /)
        J(3,:)=(/DOT_PRODUCT(DN(3,:),X(1,:)),DOT_PRODUCT(DN(3,:),X(2,:)),DOT_PRODUCT(DN(3,:),X(3,:)) /)               

        DETJ=  J(1,1)*(J(2,2)*J(3,3)-J(2,3)*J(3,2)) &
              -J(1,2)*(J(2,1)*J(3,3)-J(3,1)*J(2,3)) &
              +J(1,3)*(J(2,1)*J(3,2)-J(3,1)*J(2,2))     

        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig)

        deallocate(DN)
        NULLIFY(DN)
      enddo

      deallocate(J)

    ! cas des elements surfaciques, immerges dans un espace 3D (cf. Touzot p 215)
    ! idee generale : on voit l'element surfacique de reference, comme la face S d'un element volumique de reference
    ! e.g. un triangle lineaire a 3 noeuds, comme la face zeta=0 d'un tetraedre lineaire a 4 noeuds
    ! Soit (s1, s2) la parametrisation de l'element surfacique (typiquement (ksi, eta), (ksi, zeta) ou (eta, ksi) de
    ! l'element volumique de reference correspondant).
    ! Sur l'element surfacique, un point est repere par :
    !    x = Ni(s1, s2) xi
    ! Le jacobien de la transformation utilisee pour realiser le changement de variable (ksi, eta, zeta) -> (s1, s2)
    ! s'ecrit : 
    !    J_S = | dx/d(s1) x dx/d(s2) |

    case(i_T_P1)

      ! cas du triangle lineaire immerge dans un espace 3D
      ! dans ce cas, l'element de reference correspond a la face zeta=0 d'un tetraedre lineaire
      ! on a alors s1=ksi, s2=eta et, en notant M les fonctions de formes du triangle et N celles du tetraedre lineaire:
      !   M1(s1, s2)=N1(ksi, eta, zeta=0),
      !   M2(s1, s2)=N2(ksi, eta, zeta=0),
      !   M3(s1, s2)=N3(ksi, eta, zeta=0) 
      !   et
      !   N4(ksi, eta, zeta=0)=0

      ! on calcule les derivees des fonctions de formes, pour un tetraedre lineaire
      ! N.B.: les coordonneees des points de Gauss (ksiG, etaG), dans le triangle, deviennent (ksiG, etaG, 0) dans le tetraedre

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(i_TEP1, (/ CG(1, ig), CG(2, ig), 0.d0 /), DN)

        ! on calcule :
        !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
        !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
        v1=0.d0
        v2=0.d0
        !calcul des composantes de v1 & v2          
        do id=1,ndime
          v1(id) = dot_product(DN(1, 1:3), X(id, 1:3))
          v2(id) = dot_product(DN(2, 1:3), X(id, 1:3))
        end do

        ! on en deduit det(J) = J_S = | v1 x v2 |
        detJ = length3(cross_product(v1, v2)) 

        ! on peut alors calculer : w_PG*det(J)
        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig) 

        deallocate(DN)
        NULLIFY(DN)
      enddo

    case(i_Q_P1) 

      ! cas du quadrangle lineaire immerge dans un espace 3D
      ! dans ce cas, l'element de reference correspond a la face zeta=-1 d'un hexaedre lineaire
      ! on a alors s1=ksi, s2=eta et, en notant M les fonctions de formes du quadrangle et N celles de l hexaedre lineaire :
      !   M1(s1, s2)=N1(ksi, eta, zeta=-1),
      !   M2(s1, s2)=N2(ksi, eta, zeta=-1),
      !   M3(s1, s2)=N3(ksi, eta, zeta=-1),
      !   M4(s1, s2)=N4(ksi, eta, zeta=-1),
      !   et
      !   N5...8(ksi, eta, zeta=-1)=0


      ! on calcule les derivees des fonctions de formes, pour un hexaedre lineaire
      ! N.B.: les coordonneees des points de Gauss (ksiG, etaG), dans le quadrangle deviennent (ksiG, etaG, 0) dans l hexaedre

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(i_H_P1, (/ CG(1, ig), CG(2, ig),-1.d0 /), DN)

        ! on calcule :
        !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
        !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
        v1=0.d0
        v2=0.d0
        !calcul des composantes de v1 & v2
        do id=1,ndime
          v1(id) = dot_product(DN(1, 1:4), X(id, 1:4))
          v2(id) = dot_product(DN(2, 1:4), X(id, 1:4))
        end do

        ! on en deduit det(J) = J_S = | v1 x v2 |
        detJ = length3(cross_product(v1, v2)) 

        ! on peut alors calculer : w_PG*det(J)
        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig) 

        deallocate(DN)
        NULLIFY(DN)
      enddo
    case(i_T_P2)

      ! cas du triangle lineaire immerge dans un espace 3D
      ! dans ce cas, l'element de reference correspond a la face zeta=0 d'un tetraedre lineaire
      ! on a alors s1=ksi, s2=eta et, en notant M les fonctions de formes du triangle et N celles du tetraedre lineaire:
      !   M1(s1, s2)=N1(ksi, eta, zeta=0),
      !   M2(s1, s2)=N2(ksi, eta, zeta=0),
      !   M3(s1, s2)=N3(ksi, eta, zeta=0) 
      !   et
      !   N4(ksi, eta, zeta=0)=0

      ! on calcule les derivees des fonctions de formes, pour un tetraedre lineaire
      ! N.B.: les coordonneees des points de Gauss (ksiG, etaG), dans le triangle, deviennent (ksiG, etaG, 0) dans le tetraedre

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(i_TEP2, (/ CG(1, ig), CG(2, ig), 0.d0 /), DN)

        ! on calcule :
        !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
        !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
        v1=0.d0
        v2=0.d0
        !calcul des composantes de v1 & v2          
        do id=1,ndime
          v1(id) = dot_product(DN(1, 1:6), X(id, 1:6))
          v2(id) = dot_product(DN(2, 1:6), X(id, 1:6))
        end do

        ! on en deduit det(J) = J_S = | v1 x v2 |
        detJ = length3(cross_product(v1, v2)) 

        ! on peut alors calculer : w_PG*det(J)
        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig) 

        deallocate(DN)
        NULLIFY(DN)
      enddo

    case(i_Q_P2) 

      ! cas du quadrangle lineaire immerge dans un espace 3D
      ! dans ce cas, l'element de reference correspond a la face zeta=-1 d'un hexaedre lineaire
      ! on a alors s1=ksi, s2=eta et, en notant M les fonctions de formes du quadrangle et N celles de l hexaedre lineaire :
      !   M1(s1, s2)=N1(ksi, eta, zeta=-1),
      !   M2(s1, s2)=N2(ksi, eta, zeta=-1),
      !   M3(s1, s2)=N3(ksi, eta, zeta=-1),
      !   M4(s1, s2)=N4(ksi, eta, zeta=-1),
      !   et
      !   N5...8(ksi, eta, zeta=-1)=0


      ! on calcule les derivees des fonctions de formes, pour un hexaedre lineaire
      ! N.B.: les coordonneees des points de Gauss (ksiG, etaG), dans le quadrangle deviennent (ksiG, etaG, 0) dans l hexaedre

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(i_H_P2, (/ CG(1, ig), CG(2, ig),-1.d0 /), DN)

        ! on calcule :
        !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
        !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
        v1=0.d0
        v2=0.d0
        !calcul des composantes de v1 & v2
        do id=1,ndime
          v1(id) = dot_product(DN(1, 1:8), X(id, 1:8))
          v2(id) = dot_product(DN(2, 1:8), X(id, 1:8))
        end do

        ! on en deduit det(J) = J_S = | v1 x v2 |
        detJ = length3(cross_product(v1, v2)) 

        ! on peut alors calculer : w_PG*det(J)
        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint*field(ig) 

        deallocate(DN)
        NULLIFY(DN)
      enddo

    case default ! cas general

      !TODO: derouler la methode pour les autres elements
      ! N.B.: pour les elements quadratiques, un renumerotation pourra etre necessaire 
      ! (e.g. le Q8 de reference n'est pas une face du H20, avec les numerotations de reference!

      !TODO: cas d'un element lineique, immerge dans un espace 3D (cf. Touzot p 214)

      write(cout, '(A,1x,A,1x,A)') 'integration using ', get_interpolation_from_id(I_TYPE_FORME), ' in 3D is not defined!'
      call faterr(IAM, cout)
    end select

  CASE DEFAULT
    CALL FATERR(IAM,'Unknown ndime value')
  END SELECT

  deallocate(cg, poids, gp_coor)

END SUBROUTINE

!> computes length/surface/volume of an element
SUBROUTINE compute_element_size(NNOE,I_TYPE_FORME,I_TYPE_GAUSS,ndime,is_axi,node_coor,res)

  IMPLICIT NONE

  !> number of nodes of the element
  INTEGER                      :: NNOE
  !> element interpolation
  integer(kind=4), intent(in)  :: I_TYPE_FORME
  !> element quadrature rule
  integer(kind=4), intent(in)  :: I_TYPE_GAUSS 
  !> space dimension
  integer                      :: ndime
  !> to say if 2D case is axisymmetry
  logical                      :: is_axi
  !> node coordinates (ndime,nnoe)
  REAL(KIND=LONG), INTENT(IN)  :: node_coor(:,:)
  !> size of the element
  REAL(KIND=LONG), INTENT(OUT)  :: res

  ! ****
  ! local variables

  ! gauss point coordinates in reference frame (ndime,ngp)
  REAL(kind=long), POINTER      :: CG(:,:)

  ! weights (ngp)
  REAL(KIND=LONG), POINTER      :: POIDS(:)

  ! element shape function value at gp (nnoe)
  REAL(KIND=LONG), POINTER      :: N(:)

  ! element shape function value derivative at gp (ndime,nnoe)
  REAL(KIND=LONG), POINTER      :: DN(:,:)

  REAL(KIND=LONG)               :: DETJ
  ! quadrature weight 
  REAL(KIND=LONG)               :: COEFINT

  ! jacobienne (ndime,ndime)
  REAL(KIND=LONG), ALLOCATABLE :: J(:,:)

  ! coordinate of the gauss points (ndime)
  REAL(KIND=LONG),allocatable :: gp_coor(:)

  integer :: ngp,ig,in,id

  ! variables utilisees dans le cas des elements de
  ! dimension inferieure a la dimension de l'espace reel
  ! (e.g. un trinagle en 3D)

  ! vecteurs utilises pour des calculs intermediaire
  real(kind=8), dimension(3) :: v1, v2 
  real(kind=8) :: tmp

  !                         1234567890123456789012
  CHARACTER(len=22) :: IAM='a_EF::Compute_ele_size'
  CHARACTER(len=80) :: cout

  if (is_axi .and. ndime /= 2) CALL FATERR(IAM,'axisymetry is only available in 2D')

  NULLIFY(N,DN)

  ! calcul des coordonnees et des poids des points de gauss
  NULLIFY(CG,POIDS)
  CALL pos_gauss(I_TYPE_GAUSS,CG,POIDS)

  ngp = size(poids)

  allocate(gp_coor(ndime))

  res=0.d0

  ! selon la dimension
  SELECT CASE(ndime)
  ! cas 2D
  CASE(2) 
    ! selon le type d'element, i.e. de fonction de forme
    select case(i_type_forme)
    !cas des elements surfaciques
    case(i_T_P1, i_T_P2, i_Q_P1, i_Q_P2) ! 

      IF (.NOT. ALLOCATED(J)) ALLOCATE(J(ndime,ndime))
      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(I_TYPE_FORME,CG(:,ig),DN)

        ! Calcul de la matrice Jacobienne et de son determinant
         
        J=ZERO

        J(1,:)=(/ DOT_PRODUCT(DN(1,:),node_coor(1,:)) , DOT_PRODUCT(DN(1,:),node_coor(2,:)) /)
        J(2,:)=(/ DOT_PRODUCT(DN(2,:),node_coor(1,:)) , DOT_PRODUCT(DN(2,:),node_coor(2,:)) /)                

        DETJ=J(1,1)*J(2,2)-J(1,2)*J(2,1)

        COEFINT=DETJ*POIDS(ig)
 
        IF (is_axi) THEN
          ! calcul des fonctions de formes au point de Gauss
          CALL fonct_forme(I_TYPE_FORME,CG(:,ig),N)
          ! calcul des coordonnees du point de Gauss dans l'espace reel
          do id=1,ndime
            gp_coor(id)=DOT_PRODUCT(N(1:nnoe),node_coor(id,1:nnoe))
          enddo

          ! on multiplie par le rayon
          COEFINT=COEFINT*(6.2831853_LONG)*gp_coor(1)
          deallocate(N)
          NULLIFY(N)

        ENDIF

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint

        deallocate(DN)
        NULLIFY(DN) 
      enddo 
      deallocate(J)

     !TODO: cas d'un element lineique, immerge dans un espace 2D (cf. Touzot p 214)

    case default ! cas general
      write(cout, '(A,1x,A,1x,A)') 'integration using', get_interpolation_from_id(I_TYPE_FORME), 'in 2D is not defined!'
      call faterr(IAM, cout)
    end select

  CASE(3)
    ! selon le type d'element, i.e. de fonction de forme
    select case(i_type_forme)

    ! cas des elements volumiques
    case(i_H_P1, i_H_P2, i_TEP1, i_TEP2, i_PRP1) 
      ! cas standard : element surfacique en 2D
      IF (.NOT. ALLOCATED(J)) ALLOCATE(J(ndime,ndime))

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(I_TYPE_FORME,CG(:,ig),DN)

        ! Calcul de la matrice Jacobienne et de son determinant

        J=ZERO

        J(1,:)=(/DOT_PRODUCT(DN(1,:),node_coor(1,:)), &
                 DOT_PRODUCT(DN(1,:),node_coor(2,:)), &
                 DOT_PRODUCT(DN(1,:),node_coor(3,:)) /)
        J(2,:)=(/DOT_PRODUCT(DN(2,:),node_coor(1,:)), &
                 DOT_PRODUCT(DN(2,:),node_coor(2,:)), &
                 DOT_PRODUCT(DN(2,:),node_coor(3,:)) /)
        J(3,:)=(/DOT_PRODUCT(DN(3,:),node_coor(1,:)), &
                 DOT_PRODUCT(DN(3,:),node_coor(2,:)), &
                 DOT_PRODUCT(DN(3,:),node_coor(3,:)) /)               

        DETJ=  J(1,1)*(J(2,2)*J(3,3)-J(2,3)*J(3,2)) &
              -J(1,2)*(J(2,1)*J(3,3)-J(3,1)*J(2,3)) &
              +J(1,3)*(J(2,1)*J(3,2)-J(3,1)*J(2,2))     

        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint

        deallocate(DN)
        NULLIFY(DN)
      enddo
      deallocate(J)

    ! cas des elements surfaciques, immerges dans un espace 3D (cf. Touzot p 215)
    ! idee generale : on voit l'element surfacique de reference, comme la face S d'un element volumique de reference
    ! e.g. un triangle lineaire a 3 noeuds, comme la face zeta=0 d'un tetraedre lineaire a 4 neouds
    ! Soit (s1, s2) la parametrisation de l'element surfacique (typiquement (ksi, eta), (ksi, zeta) ou (eta, ksi) de
    ! l'element volumique de reference correspondant.
    ! Sur l'element surfacique, un point est repere par :
    !    x = Ni(s1, s2) xi
    ! Le jacobien de la transformation utilisee pour realiser le changement de variable (ksi, eta, zeta) -> (s1, s2)
    ! s'ecrit : 
    !    J_S = | dx/d(s1) x dx/d(s2) |

    ! cas du triangle lineaire a trois noeud, immerge dans un espace 3D
    ! dans ce cas, l'element de reference correspond a la face zeta=0 d'un tetraedre lineaire a 4 neouds
    ! on a alors s1=ksi, s2=eta et, en notant M les fonctions de formes du trangles et N celles du tetraedre lineaires :
    !   M1(s1, s2)=N1(ksi, eta, zeta=0),
    !   M2(s1, s2)=N2(ksi, eta, zeta=0),
    !   M3(s1, s2)=N3(ksi, eta, zeta=0) et
    !   N4(ksi, eta, zeta=0)=0

    case(i_T_P1) 

      ! on calcule les derivees des fonctions de formes, pour un tetraedre lineaire
      ! N.B.: les coordonneees des points de Gauss (ksiG, etaG), dans le triangle, deviennent (ksiG, etaG, 0), 
      ! dans le tetraedre

      res = 0.d0
      DO ig=1,ngp

        CALL derive_forme(i_TEP1, (/ CG(1, ig), CG(2, ig), 0.d0 /), DN)

        ! on calcule :
        !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
        !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
        v1=0.d0
        v2=0.d0
        do id=1, 3
          v1(id) = dot_product(DN(1, 1:3), node_coor(id, 1:3))
          v2(id) = dot_product(DN(2, 1:3), node_coor(id, 1:3))
        end do

        ! on en deduit det(J) = J_S = | v1 x v2 |
        detJ = length3(cross_product(v1, v2))

        ! on peut alors calculer : w_PG*det(J)
        COEFINT=DETJ*POIDS(ig)

        ! ajout de la contribution du point de Gauss courant a l'integrale
        res = res + coefint

        deallocate(DN)
        NULLIFY(DN)
      enddo

    !TODO: derouler la methode pour les autres elements
    ! N.B.: pour les elements quadratiques, un renumerotation pourra etre necessaire 
    ! (e.g. le Q8 de reference n'est pas une face du H20, avec les numerotaions de reference!

    !TODO: cas d'un element lineique, immerge dans un espace 3D (cf. Touzot p 214)

    case default ! cas general
     write(cout, '(A,1x,A,1x,A)') 'integration using', get_interpolation_from_id(I_TYPE_FORME), 'in 3D is not defined!'
     call faterr(IAM, cout)
    end select

  CASE DEFAULT
    CALL FATERR(IAM,'Unknown ndime value')
  END SELECT

  deallocate(cg, poids, gp_coor)

END SUBROUTINE

!> \brief get number of Gauss points needed by a given quadrature
function get_nb_points_from_quadrature(id)
  implicit none
  !> number of Gauss points needed
  integer(kind=4) :: get_nb_points_from_quadrature
  !> quadrature id
  integer(kind=4), intent(in) :: id

  select case(id)
  case(i_lig1, i_tr01, i_q1x1, i_te01)
    get_nb_points_from_quadrature = 1
  case(i_tr03)
    get_nb_points_from_quadrature = 3
  case(i_tr04, i_q2x2, i_te04)
    get_nb_points_from_quadrature = 4
  case(i_tr06, i_pr06)
    get_nb_points_from_quadrature = 6
  case(i_h222)
    get_nb_points_from_quadrature = 8
  case(i_q3x3)
    get_nb_points_from_quadrature = 9
  case(i_h333)
    get_nb_points_from_quadrature = 27
  case default
    get_nb_points_from_quadrature = 0
  end select

end function

!> Compute geometric center of an element
subroutine compute_center(i, X, center)
  implicit none
  !> element id
  integer(kind=4), intent(in)  :: i 
  !> nodes coordinates
  real(kind=long), intent(in)  :: X(:,:)     
  !> center coordinates
  real(kind=long), intent(out) :: center(:)
  ! ***
  real(kind=8), parameter :: un_tier  = 1.d0/3.d0
  real(kind=8), parameter :: un_quart = 1.d0/4.d0
  integer(kind=4) :: idime, nbdime
  real(kind=8), dimension(:), pointer :: N
  real(kind=long) :: KEZ(3)

  nullify(N)

  kez=0.

  select case(i)
    case(i_L_P1, i_L_P2,i_L_P3) ! line
      nbdime = 1
      kez(1) = 1.d0/2.d0

    case(i_T_P1, i_T_P2) ! triangle
      nbdime = 2
      kez(1:2) = un_tier
      
    case(i_Q_P1, i_Q_P2, i_QCP2) ! quadrangle
        nbdime = 2
               
    case(i_H_P1, i_H_P2) ! hexaedron
        nbdime =3 
                 
    case(i_TEP1, i_TEP2) ! tetraedron
       nbdime =3 
       kez(1:3) = un_quart
                 
    case(i_PRP1, i_PRP2) ! wedge
       nbdime =3 
       kez(1:2) = un_tier

  end select

  call fonct_forme(i,KEZ,N)

  center = 0.d0
  do idime = 1, nbDIME
    center(idime) = dot_product(N(:),X(idime,:))
  end do

  deallocate(N)

end subroutine compute_center

!!! ------------------------------------------------------------------
!!! utilities

!> \brief get edge2vertices map from its interpolation id
function get_ptr_edge2vertices(id)
  implicit none
  !> id of an interpolation
  integer(kind=4), intent(in) :: id
  !> id of the corresponding first order interpolation
  integer(kind=4), dimension(:,:), pointer :: get_ptr_edge2vertices

  select case(id)
  case(i_l_p1)
    get_ptr_edge2vertices => l_p1_edge2vertices
  case(i_l_p2)
    get_ptr_edge2vertices => l_p2_edge2vertices
  case(i_t_p1)
    get_ptr_edge2vertices => t_p1_edge2vertices
  case(i_t_p2)
    get_ptr_edge2vertices => t_p2_edge2vertices
  case(i_q_p1)
    get_ptr_edge2vertices => q_p1_edge2vertices
  case(i_q_p2)
    get_ptr_edge2vertices => q_p2_edge2vertices
  case(i_h_p1)
    get_ptr_edge2vertices => h_p1_edge2vertices
  case(i_h_p2)
    get_ptr_edge2vertices => h_p2_edge2vertices
  case(i_tep1)
    get_ptr_edge2vertices => tep1_edge2vertices
  case(i_tep2)
    get_ptr_edge2vertices => tep2_edge2vertices
  case(i_prp1)
    get_ptr_edge2vertices => prp1_edge2vertices
  case default
    call faterr('a_EF::get_ptr_edge2vertices','unknown interpolation ID')
  end select

end function

!> \brief get first order interpolation id of a n-th order interpolation
function get_first_order_interpolation_id(id, nb_nodes)
  implicit none
  !> id of an interpolation
  integer(kind=4), intent(in) :: id
  !> number of node of the returned interpolation
  integer(kind=4), intent(out) :: nb_nodes
  !> id of the corresponding first order interpolation
  integer(kind=4) :: get_first_order_interpolation_id

  select case( id )
  case( i_l_p1, i_l_p2, i_l_p3 )
    get_first_order_interpolation_id = i_l_p1
    nb_nodes = 2
  case( i_t_p1, i_t_p2 )
    get_first_order_interpolation_id = i_t_p1
    nb_nodes = 3
  case( i_q_p1, i_q_p2 )
    get_first_order_interpolation_id = i_q_p1
    nb_nodes = 4
  case( i_h_p1, i_h_p2 )
    get_first_order_interpolation_id = i_h_p1
    nb_nodes = 8
  case( i_tep1, i_tep2 )
    get_first_order_interpolation_id = i_tep1
    nb_nodes = 4
  case( i_prp1 )
    get_first_order_interpolation_id = i_prp1
    nb_nodes = 6
  case default
    call faterr('mod_a_EF::get_first_odrer_interpolation_id', 'unknown interpolation id')
  end select

end function

!> \brief get name of interpolation from id
function get_interpolation_from_id(id)
  implicit none
  !> id of an interpolation
  integer(kind=4), intent(in) :: id
  !> name of an interpolation
  character(len=4) :: get_interpolation_from_id

  select case( id )
  case( i_l_p1 )
    get_interpolation_from_id = 'L_P1'
  case( i_l_p2 )
    get_interpolation_from_id = 'L_P2'
  case( i_l_p3 )
    get_interpolation_from_id = 'L_P3'
  case( i_t_p1 )
    get_interpolation_from_id = 'T_P1'
  case( i_t_p2 )
    get_interpolation_from_id = 'T_P2'
  case( i_q_p1 )
    get_interpolation_from_id = 'Q_P1'
  case( i_q_p2 )
    get_interpolation_from_id = 'Q_P2'
  case( i_qcp2 )
    get_interpolation_from_id = 'QcP2'
  case( i_h_p1 )
    get_interpolation_from_id = 'H_P1'
  case( i_h_p2 )
    get_interpolation_from_id = 'H_P2'
  case( i_tep1 )
    get_interpolation_from_id = 'TEP1'
  case( i_tep2 )
    get_interpolation_from_id = 'TEP2'
  case( i_prp1 )
    get_interpolation_from_id = 'PRP1'
  case default
    get_interpolation_from_id = '----'
  end select

end function

!> \brief get interpolation id from number of nodes and space dimension
!> \todo : gerer cas lignes quadratique et cubique 
integer(kind=4) function get_interpolation_id_from_nodes_and_dim(nb_nodes,space_dim)
  implicit none
  !> number of nodes of the element
  integer(kind=4), intent(in) :: nb_nodes
  !> space dimension
  integer(kind=4), intent(in) :: space_dim 

  get_interpolation_id_from_nodes_and_dim = -99

  select case(space_dim) 
  case(2)
    select case(nb_nodes) 
    case(2)
      get_interpolation_id_from_nodes_and_dim = i_l_p1 
    case(3)
      get_interpolation_id_from_nodes_and_dim = i_t_p1 
    case(4)
      get_interpolation_id_from_nodes_and_dim = i_q_p1 
    case(6)
      get_interpolation_id_from_nodes_and_dim = i_t_p2 
    case(8)
      get_interpolation_id_from_nodes_and_dim = i_q_p2 
    case(9)
      get_interpolation_id_from_nodes_and_dim = i_qcp2 
    end select
  case(3)
    select case(nb_nodes) 
    case(8)
      get_interpolation_id_from_nodes_and_dim = i_h_p1 
    case(20)
      get_interpolation_id_from_nodes_and_dim = i_h_p2 
    case(4)
      get_interpolation_id_from_nodes_and_dim = i_tep1 
    case(10)
      get_interpolation_id_from_nodes_and_dim = i_tep2 
    case(6)
      get_interpolation_id_from_nodes_and_dim = i_prp1 
    case(15)
      get_interpolation_id_from_nodes_and_dim = i_prp2 
    end select
  end select

end function

!> \brief get name of quadrature from id
function get_quadrature_from_id(id)
  implicit none
  !> id of a quadrature
  integer(kind=4), intent(in) :: id
  !> name of a quadrature
  character(len=4) :: get_quadrature_from_id

  select case( id )
  case( i_lig1 )
    get_quadrature_from_id = 'LIG1'
  case( i_lig2 )
    get_quadrature_from_id = 'LIG2'
  case( i_lig3 )
    get_quadrature_from_id = 'LIG3'
  case( i_tr01 )
    get_quadrature_from_id = 'TR01'
  case( i_tr03 )
    get_quadrature_from_id = 'TR03'
  case( i_tr04 )
    get_quadrature_from_id = 'TR04'
  case( i_tr06 )
    get_quadrature_from_id = 'TR06'
  case( i_q1x1 )
    get_quadrature_from_id = 'Q1*1'
  case( i_q2x2 )
    get_quadrature_from_id = 'Q2*2'
  case( i_q3x3 )
    get_quadrature_from_id = 'Q3*3'
  case( i_h222 )
    get_quadrature_from_id = 'H222'
  case( i_h333 )
    get_quadrature_from_id = 'H333'
  case( i_h225 )
    get_quadrature_from_id = 'H225'
  case( i_te01 )
    get_quadrature_from_id = 'TE01'
  case( i_te04 )
    get_quadrature_from_id = 'TE04'
  case( i_pr06 )
    get_quadrature_from_id = 'PR06'
  case( i_pr08 )
    get_quadrature_from_id = 'PR08'
  case default
    get_quadrature_from_id = '----'
  end select

end function

!--------------------------------------------------------
! Partie liee aux fonctions de formes des elements SHB
!--------------------------------------------------------

!> evaluates element shape functions at a given node (in the reference frame)
SUBROUTINE FONCT_FORME_SHB(I_TYPE_FORME,KEZ,H)
  !------------------------------------------------------------------------------!
  !  remplit le tableau des fonctions de forme   H=[ H1 , H2 ,  ....   ]         !
  !------------------------------------------------------------------------------!  

  IMPLICIT NONE
 
  !> interpolation function type : H_P1|H_P2|PRP1|PRP2
  integer(kind=4), intent(in) :: I_TYPE_FORME
  !> node coordinates
  REAL(KIND=LONG)         :: KEZ(:) 
  !> array of value on each element node
  REAL(KIND=LONG),POINTER :: H(:)
      
  ! Variables locales
  REAL(KIND=LONG)         :: KSI,ETA,ZET,UMKME,UPK,UMK,UPE,UME,UMK2, &
                             UME2,UMZ,UPZ,UMZ2,UMKEZ

  REAL(KIND=LONG)         :: a,b,l

  character(len=80)       :: cout

  ! Initialisation des nouveaux pointeurs 
  IF(ASSOCIATED(H)) DEALLOCATE(H)
  NULLIFY(H)

  SELECT CASE(SIZE(KEZ))
  CASE(1)
    KSI=KEZ(1)
  CASE(2)
    KSI=KEZ(1) ; ETA=KEZ(2)
  CASE(3)
    KSI=KEZ(1) ; ETA=KEZ(2)  ; ZET=KEZ(3)
  END SELECT
 
  SELECT CASE(I_TYPE_FORME)
  
  CASE(i_H_P1) ! hexaedre lineaire           
      ALLOCATE(H(4))
      H=RESHAPE((/  ZET*ETA       , &
                    KSI*ZET       , &
                    ETA*ETA       , &
                    KSI*ETA*ZET   /),(/SIZE(H)/))
                 
  CASE(i_H_P2) !Hexaedre quadratique            
      ALLOCATE(H(16))
      H=RESHAPE((/  KSI*ZET        , & !1
                    KSI*ETA        , & !2
                    ETA*ZET        , & !3
                    KSI*KSI        , & !4
                    ETA*ETA        , & !5
                    ZET*ZET        , & !6
                    KSI*ETA*ZET    , & !7
                    ZET*ZET*ETA    , & !8
                    ZET*ZET*KSI    , & !9
                    ETA*ETA*KSI    , & !10
                    ETA*ETA*ZET    , & !11
                    KSI*KSI*ZET    , & !12
                    KSI*KSI*ETA    , & !13
                    ETA*KSI*ZET*ZET, & !14
                    ETA*KSI*ZET*ETA, & !15
                    ETA*KSI*ZET*KSI/),(/SIZE(H)/))
  CASE(i_PRP1) ! prisme lineaire
      ALLOCATE(H(2))
      H=RESHAPE((/  KSI*ZET       , &
                    ETA*ZET       /),(/SIZE(H)/))
                    
  CASE(i_PRP2) ! prisme quadratique
      ALLOCATE(H(11))
      H=RESHAPE((/  KSI*ZET        , & !1
                    KSI*ETA        , & !2
                    ETA*ZET        , & !3
                    KSI*ETA*ZET    , & !4
                    KSI*KSI        , & !5
                    ETA*ETA        , & !6
                    ZET*ZET        , & !7
                    KSI*KSI*ZET    , & !8
                    ETA*ETA*ZET    , & !9
                    ZET*ZET*KSI    , & !10
                    ETA*ZET*ZET    /),(/SIZE(H)/))
      
  CASE DEFAULT
    write(cout,'(A,1x,A)') 'Unknown interpolation SHB ID :', get_interpolation_from_id(i_type_forme)
    call faterr('a_EF::FONCT_FORM_SHB',cout)

  END SELECT

END SUBROUTINE FONCT_FORME_SHB


!--------------------------------------------------------
! Partie liee aux derivees des fonctions de formes des elements SHB
!--------------------------------------------------------

!> evaluates element shape functions at a given node (in the reference frame)
SUBROUTINE DERIVE_FORME_SHB(I_TYPE_FORME,KEZ,DH)
  !------------------------------------------------------------------------------!
  !  Remplit le tableau des derivees des fonctions de forme  SHB                 !   
  !  DH=[ dH1/dKSI , dH2/dKSI ,  ....     ]                                      !
  !     [ dH1/dETA , dH2/dETA ,  ...      ]                                      !
  !     [ dH1/dZET , dH2/dZET ,  ...      ]                                      !
  !------------------------------------------------------------------------------   

  IMPLICIT NONE
 
  !> interpolation function type : H_P1|H_P2|PRP1|PRP2
  integer(kind=4), intent(in) :: I_TYPE_FORME
  !> node coordinates
  REAL(KIND=LONG)         :: KEZ(:) 
  !> array of value on each element node
  REAL(KIND=LONG),POINTER :: DH(:,:)
      
  ! Variables locales
  REAL(KIND=LONG)         :: KSI,ETA,ZET,UMKME,UPK,UMK,UPE,UME,UMK2, &
                             UME2,UMZ,UPZ,UMZ2,UMKEZ

  REAL(KIND=LONG)         :: a,b,l

  character(len=80)       :: cout

  ! Initialisation des nouveaux pointeurs 
  IF(ASSOCIATED(DH)) DEALLOCATE(DH)
  NULLIFY(DH)

  SELECT CASE(SIZE(KEZ))
  CASE(1)
    KSI=KEZ(1)
  CASE(2)
    KSI=KEZ(1) ; ETA=KEZ(2)
  CASE(3)
    KSI=KEZ(1) ; ETA=KEZ(2)  ; ZET=KEZ(3)
  END SELECT
 
  SELECT CASE(I_TYPE_FORME)
  
  CASE(i_H_P1) ! hexaedre lineaire           
      ALLOCATE(DH(3,4))
      
      DH=RESHAPE((/  ZERO        , ZET         , ETA          , &
                     ZET         , ZERO        , KSI          , &
                     ETA         , KSI         , ZERO         , &
                     ETA*ZET     , KSI*ZET     , KSI*ETA        /),(/3,4/))
                     
  CASE(i_H_P2) !Hexaedre quadratique            
  
      print *,'DA : H_P2 - Fonction SHB non realise'
      ALLOCATE(DH(3,16))
      DH = ZERO
      
!~       DH=RESHAPE((/ KSI*ZET        , & !1
!~                     KSI*ETA        , & !2
!~                     ETA*ZET        , & !3
!~                     KSI*KSI        , & !4
!~                     ETA*ETA        , & !5
!~                     ZET*ZET        , & !6
!~                     KSI*ETA*ZET    , & !7
!~                     ZET*ZET*ETA    , & !8
!~                     ZET*ZET*KSI    , & !9
!~                     ETA*ETA*KSI    , & !10
!~                     ETA*ETA*ZET    , & !11
!~                     KSI*KSI*ZET    , & !12
!~                     KSI*KSI*ETA    , & !13
!~                     ETA*KSI*ZET*ZET, & !14
!~                     ETA*KSI*ZET*ETA, & !15
!~                     ETA*KSI*ZET*KSI/),(/3,16/))

  CASE(i_PRP1) ! prisme lineaire
      ALLOCATE(DH(3,2))

      DH=RESHAPE((/  ZET         , ZERO        , KSI           , &
                     ZERO        , ZET         , ETA           /),(/3,2/))
                    
  CASE(i_PRP2) ! prisme quadratique
  
      print *,'DA : PRP2 - Fonction SHB non realise'
      ALLOCATE(DH(3,11))
      DH = ZERO
!~       DH=RESHAPE((/  KSI*ZET        , & !1
!~                     KSI*ETA        , & !2
!~                     ETA*ZET        , & !3
!~                     KSI*ETA*ZET    , & !4
!~                     KSI*KSI        , & !5
!~                     ETA*ETA        , & !6
!~                     ZET*ZET        , & !7
!~                     KSI*KSI*ZET    , & !8
!~                     ETA*ETA*ZET    , & !9
!~                     ZET*ZET*KSI    , & !10
!~                     ETA*ZET*ZET    /),(/3,11/))
      
  CASE DEFAULT
    write(cout,'(A,1x,A)') 'Unknown interpolation SHB ID :', get_interpolation_from_id(i_type_forme)
    call faterr('a_EF::DERIVE_FORM_SHB',cout)

  END SELECT

END SUBROUTINE DERIVE_FORME_SHB

! Get integer identifier from finite element type name
function get_fe_type_id_from_name(fe_type_name)
  implicit none
  !> [in] fe_type_name
  character(len=5), intent(in) :: fe_type_name
  !> [return] integer id
  integer(kind=4)              :: get_fe_type_id_from_name

  get_fe_type_id_from_name = get_index_from_string(fe_type_name,fe_type,size(fe_type))

end function

! Get finite element type name from integer identifier
function get_fe_type_name_from_id(id)
  implicit none
  !> [in] integer id
  integer(kind=4), intent(in) :: id
  !> [return] finite element type name
  character(len=5)            :: get_fe_type_name_from_id

  if( id > 0 .and. id <= size(fe_type) ) then
    get_fe_type_name_from_id = fe_type(id)
  else
    get_fe_type_name_from_id = 'xxxxx'
  end if

end function

END MODULE a_EF
