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
MODULE a_mecaEF_joint

  ! joint finite Element for mecanical problems
  ! these elements
  !  dont have any large/small def option (defo = disp jump)
  !  mass

use overall
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

private

TYPE T_mecaEF_joint
   integer                   :: name
   INTEGER                   :: N_NODE
   INTEGER                   :: N_DOF_by_NODE
   integer(kind=4)           :: T_FONC_FORME
   INTEGER                   :: N_PG_RIG
   integer(kind=4)           :: SCH_GAUSS_RIG
   TYPE(T_PT_GAUSS), POINTER :: PG(:)

   ! mapping matrices gp -> node (noeuds sommets)
   real(kind=8),pointer      :: gp2node(:,:) 
   ! mapping matrices node -> edge (noeuds cote)
   real(kind=8),pointer      :: node2edge(:,:) 

   !ajouts vecteurs et matrices de travail
   real(kind=8),pointer      :: coor_ele(:),primal_ele(:),dual_ele(:),operator_ele(:,:)

END TYPE T_mecaEF_joint 

TYPE(T_mecaEF_joint),DIMENSION(6),PRIVATE :: mecaEF

integer, parameter, private :: i_j2xx2 = 1 ! 2D 2+2 nodes
integer, parameter, private :: i_j3xx2 = 2 ! 2D 3+3 nodes (quadratic)
integer, parameter, private :: i_j3xx3 = 3 ! 3D 3+3 nodes
integer, parameter, private :: i_j6xx3 = 4 ! 3D 6+6 nodes (quadratic)
integer, parameter, private :: i_j4xx3 = 5 ! 3D 4+4 nodes 
integer, parameter, private :: i_j8xx3 = 6 ! 3D 8+8 nodes (quadratic)

PUBLIC  get_nb_ele_joint, &
        init_mecaEF_joint, &
        get_NAME_mecaEF_joint, &
        get_N_NODE_mecaEF_joint, &
        get_N_DOF_by_NODE_mecaEF_joint, &
        get_N_DOF_mecaEF_joint, &
        get_N_DOF_of_NODE_mecaEF_joint, &
        get_bw_mecaEF_joint, &
        get_N_GP_mecaEF_joint, &
        get_coor_pg_JOINT, &
        compute_elementary_bulk_JOINT, &
        ! compute_elementary_fields_JOINT, &
        compute_elementary_center_joint, &
        ENERGY_JOINT, &
        POWER_JOINT, &
        Stress2Fint_JOINT, &
        interpolate_node2pg_JOINT, &
        gpv2node_2D_joint, &
        gpv2node_3D_joint, &
        gpv2element_3D_joint, &         
        get_gp_ptr_mecaEF_joint, &
        compute_elementary_field_joint, &
        get_elementary_field_joint, &
        gp_external_field_3d_joint, &
        gpv_field_joint, &
        gpv_vec_joint, &        
        gpv_frame_joint, &        
        gpv_internal_joint, &
        get_ele_ptr_mecaEF_joint, &
        get_nearest_gp_mecaEF_joint, &
        check_elementary_ppset_joint, &
        gpv_endo_JOINT

CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_joint(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_joint=SIZE(mecaEF)

END FUNCTION get_nb_ele_joint

SUBROUTINE init_mecaEF_joint
  IMPLICIT NONE
  logical :: is_initialized = .false.
  INTEGER :: itempo,i,j,errare,nn
  REAL(kind=long),POINTER :: CG(:,:),POIDS_ELE(:)
!                           123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF_joint::init_mecaEF_joint'

  if( is_initialized ) return
!
! EF 2D
!
! J2 - liaison ele lineaires a base S2
  mecaEF(i_j2xx2)%name          = i_j2xx2
  mecaEF(i_j2xx2)%N_NODE        = 4
  mecaEF(i_j2xx2)%N_DOF_by_NODE = 2
  mecaEF(i_j2xx2)%T_FONC_FORME  = i_L_P1
  mecaEF(i_j2xx2)%N_PG_RIG      = 2 
  mecaEF(i_j2xx2)%SCH_GAUSS_RIG = i_LIG2
! J3 - liaison ele quadratiques a base S3
  mecaEF(i_j3xx2)%name          = i_j3xx2
  mecaEF(i_j3xx2)%N_NODE        = 6
  mecaEF(i_j3xx2)%N_DOF_by_NODE = 2
  mecaEF(i_j3xx2)%T_FONC_FORME  = i_L_P2
  mecaEF(i_j3xx2)%N_PG_RIG      = 3 
  mecaEF(i_j3xx2)%SCH_GAUSS_RIG = i_LIG3
! 
! EF 3D
!
! J3 - liaison ele lineaires a base T3   
  mecaEF(i_j3xx3)%name          = i_j3xx3
  mecaEF(i_j3xx3)%N_NODE        = 6
  mecaEF(i_j3xx3)%N_DOF_by_NODE = 3
  mecaEF(i_j3xx3)%T_FONC_FORME  = i_T_P1
  mecaEF(i_j3xx3)%N_PG_RIG      = 3 
  mecaEF(i_j3xx3)%SCH_GAUSS_RIG = i_TR03
! J6 liaison ele quadratiques a base T6   
  mecaEF(i_j6xx3)%name          = i_j6xx3
  mecaEF(i_j6xx3)%N_NODE        = 12
  mecaEF(i_j6xx3)%N_DOF_by_NODE = 3
  mecaEF(i_j6xx3)%T_FONC_FORME  = i_T_P2
  mecaEF(i_j6xx3)%N_PG_RIG      = 3
  mecaEF(i_j6xx3)%SCH_GAUSS_RIG = i_TR03
! Q4 liaison ele lineaires a base Q4   
  mecaEF(i_j4xx3)%name          = i_j4xx3
  mecaEF(i_j4xx3)%N_NODE        = 8
  mecaEF(i_j4xx3)%N_DOF_by_NODE = 3
  mecaEF(i_j4xx3)%T_FONC_FORME  = i_Q_P1
  mecaEF(i_j4xx3)%N_PG_RIG      = 4 
  mecaEF(i_j4xx3)%SCH_GAUSS_RIG = i_Q2x2
! Q8 liaison ele quadratique a base Q8   
  mecaEF(i_j8xx3)%name          = i_j8xx3
  mecaEF(i_j8xx3)%N_NODE        = 16
  mecaEF(i_j8xx3)%N_DOF_by_NODE = 3
  mecaEF(i_j8xx3)%T_FONC_FORME  = i_Q_P2
  mecaEF(i_j8xx3)%N_PG_RIG      = 9 
  mecaEF(i_j8xx3)%SCH_GAUSS_RIG = i_Q3x3

  NULLIFY(CG,POIDS_ELE)

  DO i=1,SIZE(mecaEF)

    ! concerning behaviour

    ALLOCATE(mecaEF(i)%PG(get_N_PG_RIG_mecaEF(i)),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating meca_ef%PG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_RIG_mecaEF(i),CG,POIDS_ELE)

    DO j=1,get_N_PG_RIG_mecaEF(i)

      mecaEF(i)%PG(j)%POIDS=POIDS_ELE(j)   

      NULLIFY(mecaEF(i)%PG(j)%N)
      CALL fonct_forme(get_T_FONC_FORME_mecaEF(i),CG(:,j),mecaEF(i)%PG(j)%N)

      NULLIFY(mecaEF(i)%PG(j)%DN)
      CALL derive_forme(get_T_FONC_FORME_mecaEF(i),CG(:,j),mecaEF(i)%PG(j)%DN)
    ENDDO

    ! concerning mapping gp -> node

    SELECT CASE(mecaEF(i)%name)
    CASE(i_j2xx2,i_j3xx3,i_j4xx3) !les lineaires

       allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG))
       nn=mecaEF(i)%N_NODE/2
       if (nn == mecaEF(i)%N_PG_RIG) then
         mecaEF(i)%gp2node = 0.d0
         call compute_gp2node(mecaEF(i)%T_FONC_FORME,nn, &
                              mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                              mecaEF(i)%gp2node(1:nn,:))
         mecaEF(i)%gp2node(nn+1:mecaEF(i)%N_NODE,:)=mecaEF(i)%gp2node(1:nn,:)

         ! print*,'joint' 
         ! print*,i,mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG
         ! print*,mecaEF(i)%gp2node
         ! print*,'---------------'
         
       else
         if (mecaEF(i)%N_PG_RIG == 1) then
           allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG))
           mecaEF(i)%gp2node = 1.d0
         else
            call faterr(IAM,'Impossible')
         endif
      endif
      
       nullify(mecaEF(i)%node2edge)

    !fd a voir plus tard
       
    CASE(i_j3xx2)
       allocate(mecaEF(i)%gp2node(3,mecaEF(i)%N_PG_RIG))
       ! call compute_gp2node(i_T_P1, 3, &
       !                      mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
       !                      mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-3,3))
       mecaEF(i)%node2edge=0.d0
       ! mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       ! mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       ! mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,1)=0.5d0
    CASE(i_j6xx3)       
       allocate(mecaEF(i)%gp2node(3,mecaEF(i)%N_PG_RIG))
       ! call compute_gp2node(i_T_P1, 3, &
       !                      mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
       !                      mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-3,3))
       mecaEF(i)%node2edge=0.d0
       ! mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       ! mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       ! mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,1)=0.5d0
    CASE(i_j8xx3)
       allocate(mecaEF(i)%gp2node(4,mecaEF(i)%N_PG_RIG))
       ! call compute_gp2node(i_Q_P1, 4, &
       !                      mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
       !                      mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-4,4))
       mecaEF(i)%node2edge=0.d0
       ! mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       ! mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       ! mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,4)=0.5d0
       ! mecaEF(i)%node2edge(4,4)=0.5d0;mecaEF(i)%node2edge(4,1)=0.5d0
    CASE DEFAULT
      call FATERR(IAM,'gp2node can t be computed for this element')
    END SELECT

    allocate(mecaEF(i)%coor_ele(nbdime*mecaEF(i)%n_node), &
             mecaEF(i)%primal_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node), &
             mecaEF(i)%dual_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node), &
             mecaEF(i)%operator_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node,mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node))
  ENDDO 


  IF(ASSOCIATED(CG)) DEALLOCATE(CG)
  IF(ASSOCIATED(POIDS_ELE)) DEALLOCATE(POIDS_ELE)

  is_initialized = .true.

END SUBROUTINE init_mecaEF_joint

function get_name_mecaEF_joint(nb)
  implicit none
  integer, intent(in) :: nb
  character(len=5) :: get_NAME_mecaEF_joint

  select case(nb)
  case( i_j2xx2 )
    get_NAME_mecaEF_joint = 'J2xx2'
  case( i_j3xx2 )
    get_NAME_mecaEF_joint = 'J3xx2'
  case( i_j3xx3 )
    get_NAME_mecaEF_joint = 'J3xx3'
  case( i_j6xx3 )
    get_NAME_mecaEF_joint = 'J6xx3'
  case( i_J4xx3 )
    get_NAME_mecaEF_joint = 'J4xx3'
  case( i_J8xx3 )
    get_NAME_mecaEF_joint = 'J8xx3'
  case default
    get_NAME_mecaEF_joint = 'xxxxx'
  end select

end function get_NAME_mecaEF_joint

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF_joint(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_mecaEF_joint=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_mecaEF_joint

INTEGER FUNCTION get_N_NODE_mecaEF_joint(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_mecaEF_joint=mecaEF(nb)%N_NODE

END FUNCTION get_N_NODE_mecaEF_joint


INTEGER FUNCTION get_bw_mecaEF_joint(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,i,j,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_mecaEF_joint=0
  DO i=1,mecaEF(nb)%N_NODE-1
    DO j=i+1,mecaEF(nb)%N_NODE
      tempo = ABS(nodes(i) - nodes(j))
      IF (tempo .GT. get_bw_mecaEF_joint) get_bw_mecaEF_joint=tempo
    ENDDO
  ENDDO

  get_bw_mecaEF_joint = (get_bw_mecaEF_joint+1)*mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_mecaEF_joint

INTEGER FUNCTION get_N_GP_mecaEF_joint(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_mecaEF_joint=mecaEF(nb)%N_PG_RIG

END FUNCTION get_N_GP_mecaEF_joint

! ------------------------------------------------------------------------------

SUBROUTINE  Bl_JOINT(N_NE,N,DN,POIDS,X,Bl,COEFINT,Q)

! computes the Bl matrix (elongation) at a gauss point
! strain/stress are written in the local frame !!
! Bl first rotates the displacement field from global to local and then differentiate
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl (avant rotation) pour les elements joint                             !
!                                                                              !
!  EN 2D : Bl  = [ N1    0  ...  -N1   0 ...                                   !
!                   0   N1  ...    0 -N1 ... ]                                 ! 
!                                                                              !
!  EN 3D : Bl  =  [ N1    0   0 ...  -N1   0   0 ...                           !
!                    0   N1   0 ...    0 -N1   0 ...                           ! 
!                    0    0  N1        0   0 -N1 ]                             !
!
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER     , INTENT(IN)    :: N_NE        ! number of nodes
REAL(KIND=8), INTENT(IN)    :: N(:)        ! array containing interpolation polynome value at gauss point
REAL(KIND=8), INTENT(IN)    :: DN(:,:)     ! array containing the derivatives of interpolation function at gauss point
REAL(KIND=8), POINTER       :: Bl(:,:)     ! 
REAL(KIND=8), INTENT(IN)    :: X(:,:)
REAL(KIND=8), INTENT(IN)    :: POIDS
REAL(KIND=8), INTENT(OUT)   :: COEFINT
real(kind=8)                :: Q(:,:)
! Variable locale
INTEGER                     :: I,J,nbn
real(kind=8)                :: V(3,2),T(3),S(3),NN(3),Q22T(2,2),Q33T(3,3),TMP22(2,2),TMP33(3,3),dS,R

                                      !123456789012345678901234 
CHARACTER(len=24)              :: IAM='a_mecaEF_joint::Bl_joint'

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(Bl)) THEN ; DEALLOCATE(Bl) ; NULLIFY(Bl) ; ENDIF

V=ZERO
Q33T=ZERO
Q22T=ZERO
     
SELECT CASE(DIME_mod)
CASE(i_2D_stress,i_2D_strain,i_2D_axisym)

     nbn = size(X,2)/2

     ! par convention le premier vecteur c'est z
     
     V(1,1)=0.d0
     V(2,1)=0.d0
     V(3,1)=1.d0
     
     V(1,2)=DOT_PRODUCT(DN(1,:),X(1,1:nbn))
     V(2,2)=DOT_PRODUCT(DN(1,:),X(2,1:nbn))    
     V(3,2)=0.d0

     NN = cross_product(V(:,1),V(:,2))
     dS=length3(NN)
     NN = NN/dS

     S = cross_product(NN,V(:,1))

     Q22T(1,1:2) = S(1:2)
     Q22T(2,1:2) = NN(1:2)      

     Q(1:2,1) = S(1:2)
     Q(1:2,2) = NN(1:2)
     
     ALLOCATE(Bl(2,2*N_NE))
     DO I=1,N_NE/2
        TMP22(1,:)=(/  N(I)  ,   ZERO  /)
        TMP22(2,:)=(/  ZERO  ,   N(I)  /)

        TMP22 = matmul(TMP22,Q22T)         
        
        Bl(1,2*(I-1)+1:2*I) = -TMP22(1,1:2)
        Bl(2,2*(I-1)+1:2*I) = -TMP22(2,1:2)

        J=I+(N_NE/2)
        Bl(1,2*(J-1)+1:2*J) = TMP22(1,1:2) 
        Bl(2,2*(J-1)+1:2*J) = TMP22(2,1:2)
     ENDDO

CASE(i_3D)

     nbn = size(X,2)/2
     
     V(1,1)=DOT_PRODUCT(DN(1,:),X(1,1:nbn))
     V(2,1)=DOT_PRODUCT(DN(1,:),X(2,1:nbn))    
     V(3,1)=DOT_PRODUCT(DN(1,:),X(3,1:nbn))    

     V(1,2)=DOT_PRODUCT(DN(2,:),X(1,1:nbn))
     V(2,2)=DOT_PRODUCT(DN(2,:),X(2,1:nbn))     
     V(3,2)=DOT_PRODUCT(DN(2,:),X(3,1:nbn))

     NN(:) = cross_product(V(:,1),V(:,2))
     dS=length3(NN)
     NN = NN/dS

     T = V(:,1)/length3(V(:,1))

     S = cross_product(NN,T)     

     Q33T(1,:) = T(:)
     Q33T(2,:) = S(:)     
     Q33T(3,:) = NN(:)      

     Q(:,1) = T(:)
     Q(:,2) = S(:)
     Q(:,3) = NN(:)

     
     ALLOCATE(Bl(3,3*N_NE))
     DO I=1,N_NE/2

        TMP33(1,:)= (/  N(I)  ,   ZERO  ,   ZERO   /)
        TMP33(2,:)= (/  ZERO  ,   N(I)  ,   ZERO   /)
        TMP33(3,:)= (/  ZERO  ,   ZERO  ,   N(I)   /)

        TMP33 = matmul(TMP33,Q33T)         
        
        Bl(1,3*(I-1)+1:3*I) = -TMP33(1,1:3)
        Bl(2,3*(I-1)+1:3*I) = -TMP33(2,1:3) 
        Bl(3,3*(I-1)+1:3*I) = -TMP33(3,1:3) 

        J=I+(N_NE/2)
        Bl(1,3*(J-1)+1:3*J) = TMP33(1,1:3)
        Bl(2,3*(J-1)+1:3*J) = TMP33(2,1:3)
        Bl(3,3*(J-1)+1:3*J) = TMP33(3,1:3)
     ENDDO

CASE DEFAULT
   call faterr(IAM,'DIME not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT

COEFINT=dS*POIDS

IF (DIME_mod == i_2D_axisym ) THEN

   ! Calcul du rayon
   R=DOT_PRODUCT(N,X(1,:))

   COEFINT=COEFINT*(6.2831853_LONG)*R

ENDIF

END SUBROUTINE Bl_JOINT

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
! tout est exprime dans le repere local
! grad  contient la defo s,t,n
! flux  contient la contrainte s,t,n
! internal (variable)  
! internal (stockage)             
!
!------------------------------------------------------------------------------!
!    Calcul de la rigidite elementaire  [Ke]=Sum [Bl]t[D][Bl]i                !
!------------------------------------------------------------------------------!
!
SUBROUTINE compute_elementary_bulk_JOINT(i,ppsnb,ibdyty,iblmty,dt,X,U, &
                                       Fint,compute_fint,            &
                                       K,compute_stiffness,          &
                                       push_fields )

  implicit none

  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty
  ! coordonnees des sommets
  REAL(KIND=LONG)                 :: X(:,:)
  ! deplacemement des sommets
  REAL(KIND=LONG)                 :: U(:,:) 
  real(kind=8)                    :: dt
  ! matrice de rig. elem.
  REAL(KIND=LONG)                 :: K(:,:)        
  REAL(KIND=LONG),DIMENSION(:)    :: Fint
  !
  logical :: compute_fint,compute_stiffness,push_fields

  ! variables locales
  REAL(KIND=LONG), POINTER        :: Bl(:,:),  & ! matrice Bl
                                     D(:,:)      ! matrice de comportement

  real(kind=8),allocatable        :: QQ(:,:)     ! repere local

  REAL(KIND=LONG)                 :: COEFINT,R

  INTEGER                         :: IG

  INTEGER                         :: anisotropie

  INTEGER                         :: nb_external, nb_internal
  INTEGER                         :: mdlnb,lawnb,inull

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

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

  real(kind=8) :: Tref,field,field_begin

  !fd 09/2020 gestion des fields pour les materiaux
  !           ne fonctionne qu'en isotrope 
  logical :: ec_is_vfield
  real(kind=8) ::my_ec_elas(3),my_ec_mc(9),my_ec_fczm(15)
  !
  integer :: enb,ienb

  ! gestion de l'endommagement initial
  logical      :: with_preendo
  integer      :: rank_preendo
  real(kind=8) :: preendo

                           !123456789012345678901
  character(len=21):: IAM='a_meca_EF_joint::bulk'

  with_preendo=.FALSE.
  
  ! Initialisation a vide des pointeurs

  NULLIFY(Bl,D)

  allocate(QQ(nbdime,nbdime))
  QQ=0.d0
  
  !fd 13/09
  ! mdl is the same for all gp of the element also some time its faster 
  ! to take information from the first one ...

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  ! Allocation internal purpose arrays
  
  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external), &
           FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  ! on va utiliser les fields attaches au model pour recupere les variables externes
  ! seuls certains fields sont valables TEMPERATURE

  extP_nb = get_external_field_nb(mdlnb)

  enb=0
  do if=1,extP_nb
    name=get_external_field_name(mdlnb,if)
    if (trim(name) == 'TEMPERATURE') enb=enb+1
    if (trim(name) == 'ENDO') then
      with_preendo = .TRUE.
      rank_preendo = get_meca_field_rank_MAILx(ibdyty,iblmty,'ENDO')
    endif
  enddo

  IF (enb /= 0) THEN 

    allocate(extP_lbl(enb), &
             extP_len(enb), &       
             extP_val(enb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

    extP_lbl(1)=' '
    extP_val(1)=0.

  ENDIF

  !
  K=ZERO
  Fint=ZERO

  ec_is_vfield=.FALSE.
  if (get_joint_param_type(lawnb) /= 0) ec_is_vfield=.TRUE.
  
  ! Pour tous les points de Gauss
  DO IG=1,mecaEF(i)%N_PG_RIG

    ! on rapatrie les infos du debut de pas

    CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_strain_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF (nb_internal /= 0 ) INTERNAL1 = 0.D0

    ! IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN

    if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'JELAS' .and. &
        get_eleop_value_bypps(ppsnb(ig),'mater') /= 'J__MC' .and. &
        get_eleop_value_bypps(ppsnb(ig),'mater') /= 'JFCZM') then 
        call logmes('mater ='//get_eleop_value_bypps(ppsnb(ig),'mater'))
        call FATERR(IAM,'Using isext == no___ is only possible with mater == JELAS|J__MC|JFCZM')
      endif
      
      ! formation de Bl avec stockage lmgc90

      CALL Bl_JOINT(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN,mecaEF(i)%PG(ig)%POIDS,X,Bl,COEFINT,QQ)    

      GRAD1(1:SIZE(Bl,dim=1)) = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /)))

      ! on fait du quasi Newton, la matrice D est tjs celle de l'elasticite
      
      if ( .not. ec_is_vfield) then      
         CALL D_JOINT(ppsnb(ig),D)
         select case(get_eleop_value_bypps(ppsnb(ig),'mater'))
         case('JELAS') 
           CALL comp_stress_joint_elas(ppsnb(ig),GRAD1,FLUX1)
         case('J__MC')
            CALL comp_stress_joint_MC(ppsnb(ig),GRAD0,FLUX0,INTERNAL0,GRAD1,FLUX1,INTERNAL1,.False.)
         case('JFCZM')
            if (with_preendo) then
              CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank_preendo,preendo)
              INTERNAL0(6)=max(INTERNAL0(6),preendo)
            endif  
            CALL comp_stress_joint_FCZM(ppsnb(ig),GRAD0,FLUX0,INTERNAL0,GRAD1,FLUX1,INTERNAL1,.False.)
         end select   
      else
        select case(get_eleop_value_bypps(ppsnb(ig),'mater'))
        case('JELAS')
           call get_meca_vfield_MAILx(ibdyty,iblmty,ig,get_meca_vfield_rank_MAILx(ibdyty,iblmty,'joint_param'),my_ec_elas,3)
           CALL D_JOINT(ppsnb(ig),D,my_ec_elas)
           CALL comp_stress_joint_elas(ppsnb(ig),GRAD1,FLUX1,my_ec_elas)
        case('J__MC')
           call get_meca_vfield_MAILx(ibdyty,iblmty,ig,get_meca_vfield_rank_MAILx(ibdyty,iblmty,'joint_param'),my_ec_mc,9)
           CALL D_JOINT(ppsnb(ig),D,my_ec_mc(1:3))
           CALL comp_stress_joint_MC(ppsnb(ig),GRAD0,FLUX0,INTERNAL0,GRAD1,FLUX1,INTERNAL1,.False.,my_ec_mc)
        case('JFCZM')
           call get_meca_vfield_MAILx(ibdyty,iblmty,ig,get_meca_vfield_rank_MAILx(ibdyty,iblmty,'joint_param'),my_ec_fczm,15)
           CALL D_JOINT(ppsnb(ig),D,my_ec_fczm(1:3))
           CALL comp_stress_joint_FCZM(ppsnb(ig),GRAD0,FLUX0,INTERNAL0,GRAD1,FLUX1,INTERNAL1,.False.,my_ec_fczm)
        end select   
           
      endif
 
    internal1(nb_internal-(nbdime*nbdime))=coefint
    internal1(nb_internal+1-(nbdime*nbdime):nb_internal)=reshape(QQ,(/ size(QQ) /))

    !  ke= Blt.D.Bl.coef
    if (compute_stiffness) K = K +  MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*COEFINT            
              
    if (compute_fint) Fint=Fint+(MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*COEFINT)

    if (push_fields) then
      CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
      CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
      IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
    endif
    
  ENDDO


  ! if (compute_stiffness) then
  !   print*,'Fint'
  !   write(*,'(24(1x,D14.5))') K
  ! endif   
  
  ! if (compute_fint) then
  !   print*,'Fint'
  !   write(*,'(3(1x,D14.5))') Fint
  ! endif

  deallocate(extP_lbl,extP_len,extP_val)

  DEALLOCATE(Bl) ;  NULLIFY(Bl)
  
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)
  deallocate(D) ; nullify(D)

  DEALLOCATE(QQ)
  
END SUBROUTINE

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

SUBROUTINE gpv_endo_JOINT(i,ppsnb,ibdyty,iblmty,endo)

  implicit none

  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty

  
  INTEGER                         :: IG

  INTEGER                         :: anisotropie

  INTEGER                         :: nb_external, nb_internal
  INTEGER                         :: mdlnb,lawnb,inull
  
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: INTERNAL
  
  real(kind=8),dimension(:)       :: endo

                           !1234567890123456789012345
   character(len=25):: IAM='a_meca_EF_joint::gpv_endo'


  
  !fd 13/09
  ! mdl is the same for all gp of the element also some time its faster 
  ! to take information from the first one ...

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  ! Allocation internal purpose arrays
  
  nb_internal=get_nb_internal_variables(mdlnb)
  
  IF (nb_internal /= 0 ) then
    ALLOCATE(INTERNAL(nb_internal))  
  else
    endo=0.d0
    return 
  endif   

  ! Pour tous les points de Gauss
  DO IG=1,mecaEF(i)%N_PG_RIG
     
      CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL)
     
      if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'JELAS' .and. &
          get_eleop_value_bypps(ppsnb(ig),'mater') /= 'J__MC'  .and. &
          get_eleop_value_bypps(ppsnb(ig),'mater') /= 'JFCZM') then 
        call logmes('mater ='//get_eleop_value_bypps(ppsnb(ig),'mater'))
        call FATERR(IAM,'Using isext == no___ is only possible with mater == JELAS|J__MC|JFCZM')
      endif
      
      select case(get_eleop_value_bypps(ppsnb(ig),'mater'))
         
      case('JELAS')
         endo(ig)=0.d0         
      case('J__MC')
         if (internal(6) > 1.d0) then 
            endo(ig)=1.d0
         else
            endo(ig)=0.d0
         endif   
      case('JFCZM')
         endo(ig) = internal(6)
      end select   
    
  ENDDO
      
  DEALLOCATE(INTERNAL)
  
END SUBROUTINE

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

SUBROUTINE compute_elementary_center_joint(i,X,center)

IMPLICIT NONE

! le numero de l'element dans la liste locale
INTEGER        , INTENT(IN) :: i 
! coordonnees des sommets
REAL(KIND=LONG)             :: X(:,:)     
! volume
REAL(KIND=LONG)             :: center(:)

CALL Compute_center(mecaEF(i)%T_FONC_FORME,X,center)

END SUBROUTINE

!============= low level private routines ==================

INTEGER FUNCTION get_N_PG_RIG_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_RIG_mecaEF=mecaEF(nb)%N_PG_RIG

END FUNCTION get_N_PG_RIG_mecaEF

integer(kind=4) FUNCTION get_SCH_GAUSS_RIG_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_RIG_mecaEF=mecaEF(nb)%SCH_GAUSS_RIG

END FUNCTION get_SCH_GAUSS_RIG_mecaEF


integer(kind=4) FUNCTION get_T_FONC_FORME_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_T_FONC_FORME_mecaEF=mecaEF(nb)%T_FONC_FORME

END FUNCTION get_T_FONC_FORME_mecaEF



!*************************************************************************

!> computes deformation energy 
SUBROUTINE ENERGY_JOINT(i,ppsnb,ibdyty,iblmty,X,E_def)

 IMPLICIT NONE

 ! le numero de l'element
 INTEGER      , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty
 INTEGER                   :: mdlnb,lawnb 
 ! coordonnees/dep/vit des sommets
 REAL(KIND=8)              :: X(:,:)
 REAL(KIND=LONG), POINTER  :: Bl(:,:)
 real(kind=8),allocatable  :: Q(:,:)
      
 !*** variables locales

 ! vecteur de travail local
 REAL(KIND=8) ,ALLOCATABLE :: Sloc(:),Eloc(:)     

 INTEGER                   :: IG
 REAL(KIND=8)              :: COEFINT,rho,E_def

 INTEGER                   :: n_stress_gp

 CALL get_ppset_value(ppsnb(1),mdlnb,lawnb) 
 
 n_stress_gp=get_nb_external_variables(mdlnb)

 ! Initialisation a vide des pointeurs

 allocate(Q(nbdime,nbdime))
 
 ALLOCATE(Sloc(n_stress_gp),Eloc(n_stress_gp))

 E_def = 0.d0

 DO IG=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss

   CALL Bl_JOINT(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN,mecaEF(i)%PG(ig)%POIDS,X,Bl,COEFINT,Q)    
    
   CALL get_stress_1_MAILx(ibdyty,iblmty,ig,Sloc)
   CALL get_strain_1_MAILx(ibdyty,iblmty,ig,Eloc)

   E_def = E_def + (0.5*DOT_PRODUCT(Eloc,Sloc)*COEFINT)
 
 ENDDO

 DEALLOCATE(sloc,Eloc)
 deallocate(Q)
 
END SUBROUTINE ENERGY_JOINT

!*************************************************************************

SUBROUTINE POWER_JOINT(i,mdlnb,lawnb,ibdyty,iblmty,X,V,P_def)

 IMPLICIT NONE

 ! le numero de l'element
 INTEGER      , INTENT(IN) :: i,mdlnb,lawnb 
 INTEGER                   :: ibdyty,iblmty
 ! coordonnees/vit
 REAL(KIND=8)              :: X(:,:),V(:)
 REAL(KIND=8)              :: P_def

 ! variables locales
 REAL(KIND=8) , POINTER    :: Bl(:,:)
 real(kind=8) ,allocatable :: Q(:,:)

 ! vecteur de travail local
 REAL(KIND=8) ,ALLOCATABLE :: Sloc(:),Eloc(:)     

 INTEGER                   :: IG
 REAL(KIND=8)              :: COEFINT,R,rho

 INTEGER                   :: n_stress_gp,istrg

                          !123456789012345678901234567
 CHARACTER(len=27) :: IAM='a_mecaEF_joint::compute_power'


 n_stress_gp=get_nb_external_variables(mdlnb)

 ! Initialisation a vide des pointeurs
 allocate(Q(nbdime,nbdime))
 ALLOCATE(Sloc(n_stress_gp),Eloc(n_stress_gp))

 P_def = 0.d0

 ! Pour tous les points de Gauss
 DO IG=1,mecaEF(i)%N_PG_RIG                 

   CALL get_stress_1_MAILx(ibdyty,iblmty,ig,Sloc)

   eloc=0.d0

   CALL Bl_JOINT(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN,mecaEF(i)%PG(ig)%POIDS,X,Bl,COEFINT,Q)
                                
   Eloc(1:SIZE(Bl,dim=1)) = MATMUL(Bl,V)

   P_def = P_def + (DOT_PRODUCT(Eloc,Sloc)*COEFINT)  ! s : desp/dt

 ENDDO

 if( associated(Bl) ) deallocate(Bl)
 DEALLOCATE(sloc,Eloc) ; NULLIFY(Bl)
 deallocate(Q)
 
END SUBROUTINE POWER_JOINT

!*************************************************************************

SUBROUTINE get_coor_pg_JOINT(i,X,coor_pg)

 ! routine calculant les coordonnees des points de Gauss
 implicit none

 INTEGER         , INTENT(IN) :: i             ! le numero de l'element
 REAL(KIND=8)                 :: X(:,:)        ! coordonnees des sommets
 REAL(KIND=8)                 :: coor_pg(:,:)  ! coordonnees des points de Gauss
 INTEGER                      :: ig,idime

 ! Pour tous les points de Gauss
 DO ig=1,mecaEF(i)%N_PG_RIG                 
   do idime=1,nbDIME
     coor_pg(idime,ig)=dot_product(mecaEF(i)%PG(ig)%N(1:size(mecaEF(i)%PG(1)%N)),X(idime,1:size(mecaEF(i)%PG(1)%N)))
   enddo
 ENDDO

END SUBROUTINE get_coor_pg_JOINT

!*************************************************************************

SUBROUTINE interpolate_node2pg_JOINT(i,valnoe,valpg)

 ! routine calculant les coordonnees des points de Gauss

 implicit none

 INTEGER         , INTENT(IN) :: i           ! le numero de l'element
 REAL(KIND=8)                 :: valnoe(:)   ! valeurs aux sommets
 REAL(KIND=8)                 :: valpg(:)    ! valeurs aux points de Gauss
 INTEGER                      :: ig

 !fd attention N a la taille d'un des 2 supports  

 ! Pour tous les points de Gauss
 DO ig=1,mecaEF(i)%N_PG_RIG                 
   valpg(ig)=dot_product(mecaEF(i)%PG(ig)%N(1:size(mecaEF(i)%PG(1)%N)),valnoe(1:size(mecaEF(i)%PG(1)%N)))
 ENDDO

END SUBROUTINE interpolate_node2pg_JOINT

!*************************************************************************

SUBROUTINE gpv2node_2D_joint(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,NbFields,NbNodes_stored)

 !fd routine qui ramene aux noeuds les valeurs aux points de gauss
 !  i               : iso element id
 !  mdlnb           : model of the element  
 !  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
 !  nodalvalues     : the computed values
 !  nbfields        : 
 !  nbnodes_stored  :


  ! on remonte a la MatLib
  !           2D xx,yy,xy=0,zz=0,trace ou vm

  
  IMPLICIT NONE
  INTEGER :: i,ibdyty,iblmty,nbfields,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele,ii
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: NbNodes_stored,i_node

  !                          123456789012345678901234567
  CHARACTER(len=27)  :: IAM='a_mecaEF_joint::gpv2node_2D'
  
  ! vecteurs de travail locaux
  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),Field(:,:),VEC(:),Q(:,:) 
  integer :: ig,if,mdlnb,inull,nb_external,nb_internal

  real(kind=8) :: tmp,tens(2,2)
  
  integer :: nbs,nbm

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele

  if (nbfields /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming size first dim')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming size second dim')
  endif
  NodalValues = 0.d0

  if (NSTEP < 1) return
  
  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  allocate(Field(Nbfields,NbGp_ele))
  Field = 0.d0

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))
  ALLOCATE(VEC(nb_external))
  ALLOCATE(Q(nbdime,nbdime))

  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx(ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

  case(1) ! Euler Almansi Strain

      IF (get_eleop_value(mdlnb,'isext') == 'no___') then
         
          do ig=1,NbGp_ele

            ! on remet tout dans le repere global
            Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))

            tens=0.d0
            tens(1,2)=grad(1,ig)
            tens(2,2)=grad(2,ig)            
            tens(2,1)=tens(1,2)

            tens = matmul(tens,transpose(Q)) 

            tens = matmul(Q,tens) 
            
            FIELD(1,ig) = tens(1,1)
            FIELD(2,ig) = tens(2,2)
            FIELD(3,ig) = tens(2,1)
            FIELD(4,IG) = 0.d0
            FIELD(5,ig) = tens(1,1) + tens(2,2)
            
          enddo
      ELSE

        CALL FATERR(IAM,'isext must be no___')

      ENDIF
     
    case(3) ! Green Lagrange Strain

      CALL FATERR(IAM,'large strain not possible') 

    case(2) ! Cauchy stress

      IF (get_eleop_value(mdlnb,'isext') == 'no___') then
          ! cauchy     
          ! xx yy xy zz

          do ig=1,NbGp_ele

            ! on remet tout dans le repere global
            Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))

            tens=0.d0
            tens(1,2)=FLUX(1,ig)
            tens(2,2)=FLUX(2,ig)            
            tens(2,1)=tens(1,2)      

            tens = matmul(tens,transpose(Q)) 

            tens = matmul(Q,tens) 

            FIELD(1,ig) = tens(1,1)
            FIELD(2,ig) = tens(2,2)
            FIELD(3,ig) = tens(2,1)
            FIELD(4,ig) = 0.d0

            tmp= (tens(1,1)+tens(2,2))/2.d0
  
            tens(1,1) = tens(1,1)-tmp
            tens(2,2) = tens(2,2)-tmp

            FIELD(5,ig) = dsqrt(1.5d0*(tens(1,1)**2 +        &
                                       tens(2,2)**2 +        &
                                       tmp**2       +        &
                                       (2.d0*(tens(1,2)**2)  &
                                       )                     & 
                                      )                      &
                               ) 
          enddo
         
      ELSE

        CALL FATERR(IAM,'isext must be no___')

      ENDIF   

    case(4) ! Piola Kirchoff Stress

      CALL FATERR(IAM,'large stress not possible')
            
    case(5)
      
      if (nb_internal > 0) then

        call logmes('boarf') 
         
      endif
      
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1|3::strain, 2|4::stress, 5::internal variables')      
  endselect

  nbs=size(mecaEF(i)%gp2node,dim=1)
  nbm=size(mecaEF(i)%node2edge,dim=1)

  !print*,nbs,nbm

  do if=1,nbfields
     !print*,'-',if
     v_nodes(1:nbs) = matmul(mecaEF(i)%gp2node,Field(if,:))

     !print*,v_nodes(1:nbs)

     !print*, associated(mecaEF(i)%node2edge)

     if ( associated(mecaEF(i)%node2edge) ) then
       v_nodes(nbs+1:nbs+nbm) = matmul(mecaEF(i)%node2edge,v_nodes(1:nbs)) 
       !print*,v_nodes(nbs+1:nbs+nbm)
     endif
    
     NodalValues(if,:) = NodalValues(if,:) + v_nodes(:)

  enddo

  !print*,nbfields
  !do if=1,nbfields
  !  print*,'-',if
  !  print*,Field(if,:)
  !  print*,associated(mecaEF(i)%node2edge)
  !  if (associated(mecaEF(i)%node2edge)) print*,mecaEF(i)%node2edge
  !  print*,NodalValues(if,:)
  !enddo 

  deallocate(v_nodes,GRAD,FLUX,INTERNAL,field,Q,VEC)

END SUBROUTINE gpv2node_2D_joint

!------------------------------------------------------------------------

SUBROUTINE gpv2node_3D_joint(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

  !fd routine qui ramene aux noeuds les valeurs aux points de gauss
  !  i               : iso element id
  !  mdlnb           : model
  !  required_field  : the expected field. 1==almansy strain,        2==cauchy stress, 
  !                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress
  !                                        5==internal variables  
  !  nodalvalues     : the computed values
  !  fieldsize       : number of components of the field 
  !  nbnodes_stored  : in some cases the field is computed 
  !                    on less nodes than the element one (ie for quadratic elements) 


  ! on remonte a la MatLib
  !           3D xx,yx,yy,zx,zy,zz,trace ou vm  
  
  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: NbNodes_stored,i_node

  !                          123456789012345678901234567890123
  CHARACTER(len=33)  :: IAM='a_mecaEF_joint::gpv2node_3D_joint'

  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),Field(:,:),VEC(:),Q(:,:) 
  integer :: ig,if,inull,nb_external,nb_internal,ii

  real(kind=8) :: tmp,tens(3,3)

  integer :: nbs,nbm

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele 

  if (fieldsize /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming size first dim')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming size second dim')
  endif
  NodalValues = 0.d0

  if (NSTEP < 1) return

  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  allocate(Field(Fieldsize,NbGp_ele))
  Field = 0.d0

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))
  ALLOCATE(VEC(nb_external))
  ALLOCATE(Q(nbdime,nbdime))
  
  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx  (ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo


  ! print*,required_field
  
  Select case(required_field)

    case(1) ! Euler Almansi Strain
      IF (get_eleop_value(mdlnb,'isext') == 'no___') then
         
          do ig=1,NbGp_ele

            ! on remet tout dans le repere global
            Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))

            tens=0.d0
            tens(1,3)=grad(1,ig)
            tens(2,3)=grad(2,ig)            
            tens(3,3)=grad(3,ig)
            tens(3,1)=tens(1,3)
            tens(3,2)=tens(2,3)      

            tens = matmul(tens,transpose(Q)) 

            tens = matmul(Q,tens) 
            
            FIELD(1,ig) = tens(1,1)
            FIELD(2,ig) = tens(2,1)
            FIELD(3,ig) = tens(2,2)
            FIELD(4,ig) = tens(3,1)
            FIELD(5,ig) = tens(3,2)
            FIELD(6,ig) = tens(3,3)
            FIELD(7,ig) = tens(1,1) + tens(2,2) + tens(3,3)
            
          enddo
      ELSE

        CALL FATERR(IAM,'isext must be no___')

      ENDIF   

    case(3) ! Green Lagrange Strain

      CALL FATERR(IAM,'large strain not possible') 
       
    case(2) ! Cauchy Stress
       
      IF (get_eleop_value(mdlnb,'isext') == 'no___') then
          ! cauchy     
          ! xx yx yy zx zy zz

          do ig=1,NbGp_ele

            ! on remet tout dans le repere global
            Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))

            tens=0.d0
            tens(1,3)=FLUX(1,ig)
            tens(2,3)=FLUX(2,ig)            
            tens(3,3)=FLUX(3,ig)
            tens(3,1)=tens(1,3)
            tens(3,2)=tens(2,3)      

            tens = matmul(tens,transpose(Q)) 

            tens = matmul(Q,tens) 

            FIELD(1,ig) = tens(1,1)
            FIELD(2,ig) = tens(2,1)
            FIELD(3,ig) = tens(2,2)
            FIELD(4,ig) = tens(3,1)
            FIELD(5,ig) = tens(3,2)
            FIELD(6,ig) = tens(3,3)

            tmp= (tens(1,1)+tens(2,2)+tens(3,3))/3.d0
  
            tens(1,1) = tens(1,1)-tmp
            tens(2,2) = tens(2,2)-tmp
            tens(3,3) = tens(3,3)-tmp         

            FIELD(7,ig) = dsqrt(1.5d0*(tens(1,1)**2 +        &
                                       tens(2,2)**2 +        &
                                       tens(3,3)**2 +        &
                                       (2.d0*(tens(1,2)**2 + &
                                              tens(1,3)**2 + &
                                              tens(2,3)**2 ) &
                                       )                     & 
                                      )                      &
                               ) 


          enddo
         
      ELSE

        CALL FATERR(IAM,'isext must be no___')

      ENDIF   
    
    case(4) ! Piola Kirchoff Stress

      CALL FATERR(IAM,'large stress not possible')
            
    case(5)
      
      if (nb_internal > 0) then

        call logmes('boarf') 
         
      endif
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1|3::strain, 2|4::stress, 5::internal variables')
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

  ! print*, '  nv  : '
  ! do i=1,size(NodalValues,dim=2)
  !   print*,NodalValues(:,i)
  ! enddo
    
  deallocate(v_nodes,GRAD,FLUX,INTERNAL,field,Q,VEC)

END SUBROUTINE gpv2node_3D_joint


SUBROUTINE gpv2element_3D_joint(i,mdlnb,ibdyty,iblmty,required_Field,Field,FieldSize)

  !fd routine qui calcule une moyenne sur l'element des valeurs aux points de gauss
  !  i               : element id
  !  mdlnb           : model
  !  required_field  : the expected field. 1==almansy strain,        2==cauchy stress, 
  !                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress
  !                                        5==internal variables  
  !  Field     : the computed values
  !  fieldsize       : number of components of the field 

  ! on remonte
  !           2D xx,yy,xy=0,zz=0,trace ou vm
  !           3D xx,yx,yy,zx,zy,zz,trace ou vm  
  
  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:),INTENT(inout) :: Field

  INTEGER :: NbNodes_ele,NbGp_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: i_node

  !                          123456789012345678901234567890123456
  CHARACTER(len=36)  :: IAM='a_mecaEF_joint::gpv2element_3D_joint'

  ! vecteurs de travail local  
  REAL(KIND=8), ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),VEC(:),Q(:,:) 
  integer :: ig,if,inull,nb_external,nb_internal,ii

  real(kind=8) :: tmp,tens(3,3)

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE

  if (fieldsize /= size(Field)) then
    CALL FATERR(IAM,'Non conforming sizes')
  endif

  Field = 0.d0

  if (NSTEP < 1) return

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))
  ALLOCATE(VEC(nb_external))
  ALLOCATE(Q(nbdime,nbdime))
  
  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx  (ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo


  ! print*,required_field
  
  Select case(required_field)

    case(1) ! Euler Almansi Strain
      IF (get_eleop_value(mdlnb,'isext') == 'no___') then
         
          do ig=1,NbGp_ele

            ! on remet tout dans le repere global
            Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))

            tens=0.d0
            tens(1,3)=grad(1,ig)
            tens(2,3)=grad(2,ig)            
            tens(3,3)=grad(3,ig)
            tens(3,1)=tens(1,3)
            tens(3,2)=tens(2,3)      

            tens = matmul(tens,transpose(Q)) 

            tens = matmul(Q,tens) 
            
            FIELD(1) = FIELD(1) + tens(1,1)
            FIELD(2) = FIELD(2) + tens(2,1)
            FIELD(3) = FIELD(3) + tens(2,2)
            FIELD(4) = FIELD(4) + tens(3,1)
            FIELD(5) = FIELD(5) + tens(3,2)
            FIELD(6) = FIELD(6) + tens(3,3)
            FIELD(7) = FIELD(7) + (tens(1,1) + tens(2,2) + tens(3,3))
            
          enddo
      ELSE

        CALL FATERR(IAM,'isext must be no___')

      ENDIF   

      Field = Field / NbGp_ele
      
    case(3) ! Green Lagrange Strain

      CALL FATERR(IAM,'large strain not possible') 
       
    case(2) ! Cauchy Stress
       
      IF (get_eleop_value(mdlnb,'isext') == 'no___') then
          ! cauchy     
          ! xx yx yy zx zy zz

          do ig=1,NbGp_ele

            ! on remet tout dans le repere global
            Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))

            tens=0.d0
            tens(1,3)=FLUX(1,ig)
            tens(2,3)=FLUX(2,ig)            
            tens(3,3)=FLUX(3,ig)
            tens(3,1)=tens(1,3)
            tens(3,2)=tens(2,3)      

            tens = matmul(tens,transpose(Q)) 

            tens = matmul(Q,tens) 

            FIELD(1) = FIELD(1) + tens(1,1)
            FIELD(2) = FIELD(2) + tens(2,1)
            FIELD(3) = FIELD(3) + tens(2,2)
            FIELD(4) = FIELD(4) + tens(3,1)
            FIELD(5) = FIELD(5) + tens(3,2)
            FIELD(6) = FIELD(6) + tens(3,3)

            tmp= (tens(1,1)+tens(2,2)+tens(3,3))/3.d0
  
            tens(1,1) = tens(1,1)-tmp
            tens(2,2) = tens(2,2)-tmp
            tens(3,3) = tens(3,3)-tmp         

            FIELD(7) = FIELD(7) + dsqrt(1.5d0*(tens(1,1)**2 +        &
                                               tens(2,2)**2 +        &
                                               tens(3,3)**2 +        &
                                               (2.d0*(tens(1,2)**2 + &
                                                      tens(1,3)**2 + &
                                                      tens(2,3)**2 ) &
                                               )                     & 
                                              )                      &
                                       ) 


          enddo
         
      ELSE

        CALL FATERR(IAM,'isext must be no___')

      ENDIF   

      Field = Field / NbGp_ele
      
    case(4) ! Piola Kirchoff Stress

      CALL FATERR(IAM,'large stress not possible')
            
    case(5)
      
      if (nb_internal > 0) then

        call logmes('boarf') 
         
      endif
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1|3::strain, 2|4::stress, 5::internal variables')
  endselect

  deallocate(GRAD,FLUX,INTERNAL,Q,VEC)

END SUBROUTINE gpv2element_3D_joint

!------------------------------------------------------------------------


!------------------------------------------------------------------------

SUBROUTINE gpv_field_joint(i,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)

!fd routine qui recupere les tenseurs des contraintes ou deformation aux points de gauss
!  i               : element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  field           : the computed values
!  fieldsize       : number of components of the field ; ici c'est comme du 2D|3D ! 

! le repere local contient t, s, n  

  IMPLICIT NONE
  integer,intent(in)       :: ppsnb(:)
  INTEGER                  :: i,mdlnb,ibdyty,iblmty,fieldsize,required_field
  REAL(kind=8)             :: field(:,:)
  REAL(KIND=8),ALLOCATABLE :: internal(:)
  real(kind=8)             :: vec(3),tens(3,3),Q(3,3),tmp
  INTEGER                  :: NbGp_ele

  !                          1234567890123456789012345678901
  CHARACTER(len=31)  :: IAM='a_mecaEF_joint::gpv_field_joint'

  ! vecteurs de travail locaux
  integer :: ig,inull,nb_external,nb_internal

  NbGp_ele =  mecaEF(i)%N_PG_RIG
  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)  

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)  

  ALLOCATE(INTERNAL(nb_internal))
  
  if (size(field,dim=2) /= NbGp_ele) then
     call faterr(IAM,'non conforming size dim 2')
  endif
  
  Field = 0.d0

  if (NSTEP < 1) return
  
  Select case(required_field)

  case(1) ! strain

      do ig=1,NbGp_ele
         CALL get_strain_1_MAILx(ibdyty,iblmty,ig,vec)
         CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL)  

         Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal),(/nbdime,nbdime/))

         tens=0.d0
         tens(1,3)=vec(1)
         tens(2,3)=vec(2)            
         tens(3,3)=vec(3)
         tens(3,1)=tens(1,3)
         tens(3,2)=tens(2,3)      

         tens = matmul(tens,transpose(Q)) 

         tens = matmul(Q,tens) 

         FIELD(1,ig) = tens(1,1)
         FIELD(2,ig) = tens(2,1)
         FIELD(3,ig) = tens(2,2)
         FIELD(4,ig) = tens(3,1)
         FIELD(5,ig) = tens(3,2)
         FIELD(6,ig) = tens(3,3)
         FIELD(7,ig) = tens(1,1) + tens(2,2) + tens(3,3)

      enddo

  case(2) ! stress

      do ig=1,NbGp_ele
         CALL get_stress_1_MAILx(ibdyty,iblmty,ig,vec)
         CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL)  

         Q = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal),(/nbdime,nbdime/))

         tens=0.d0
         tens(1,3)=vec(1)
         tens(2,3)=vec(2)            
         tens(3,3)=vec(3)
         tens(3,1)=tens(1,3)
         tens(3,2)=tens(2,3)      

         tens = matmul(tens,transpose(Q)) 

         tens = matmul(Q,tens) 

         FIELD(1,ig) = tens(1,1)
         FIELD(2,ig) = tens(2,1)
         FIELD(3,ig) = tens(2,2)
         FIELD(4,ig) = tens(3,1)
         FIELD(5,ig) = tens(3,2)
         FIELD(6,ig) = tens(3,3)

         
         tmp= (tens(1,1)+tens(2,2)+tens(3,3))/3.d0

         tens(1,1) = tens(1,1)-tmp
         tens(2,2) = tens(2,2)-tmp
         tens(3,3) = tens(3,3)-tmp         

         FIELD(7,ig) = dsqrt(1.5d0*(tens(1,1)**2 +        &
                                    tens(2,2)**2 +        &
                                    tens(3,3)**2 +        &
                                    (2.d0*(tens(1,2)**2 + &
                                           tens(1,3)**2 + &
                                           tens(2,3)**2 ) &
                                    )                     & 
                                   )                      &
                            ) 
      enddo

  case default
      CALL FATERR(IAM,'Unsupported required_field : 1::strain, 2::stress')
  endselect
   
  deallocate(internal)

  
END SUBROUTINE gpv_field_joint

!------------------------------------------------------------------------
SUBROUTINE gpv_vec_joint(i,ppsnb,ibdyty,iblmty,required_Field,Field)

!fd routine qui recupere les vecteurs contraintes ou deformation dans le repere local aux points de gauss d'un joint
!  i               : element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  field           : the computed values
!  fieldsize       : 3

! stockage local t, s, n  

  IMPLICIT NONE
  integer,intent(in)       :: ppsnb(:)
  INTEGER                  :: i,mdlnb,ibdyty,iblmty,fieldsize,required_field
  REAL(kind=8)             :: field(:,:)
  REAL(KIND=8),ALLOCATABLE :: internal(:,:)
  real(kind=8)             :: vec(3),Q(3,3),tmp
  INTEGER                  :: NbGp_ele

  !                          12345678901234567890123456789
  CHARACTER(len=29)  :: IAM='a_mecaEF_joint::gpv_vec_joint'

  ! vecteurs de travail locaux
  integer :: ig,inull,nb_external,nb_internal

  NbGp_ele =  mecaEF(i)%N_PG_RIG
  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)  

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)  

  ALLOCATE(INTERNAL(nb_internal,nbgp_ele))
    
  if (size(field,dim=1) /= nbdime) then
      call faterr(IAM,'unexpected size dim 1')
  endif
  
  if (size(field,dim=2) /= NbGp_ele) then
     call faterr(IAM,'non conforming size dim 2')
  endif
  
  Field = 0.d0

  if (NSTEP < 1) return
  
  Select case(required_field)

  case(1) ! strain

      do ig=1,NbGp_ele
         CALL get_strain_1_MAILx(ibdyty,iblmty,ig,vec(1:nbdime))
         
         ! CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))  
         ! Q(1:nbdime,1:nbdime) = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))  
         ! vec(1:nbdime)=matmul(Q(1:nbdime,1:nbdime),vec(1:nbdime))
         
         field(1:nbdime,ig) = vec(1:nbdime)

      enddo

  case(2) ! stress

      do ig=1,NbGp_ele
         CALL get_stress_1_MAILx(ibdyty,iblmty,ig,vec(1:nbdime))
         
         ! CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))  
         ! Q(1:nbdime,1:nbdime) = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))  
         ! vec(1:nbdime)=matmul(Q(1:nbdime,1:nbdime),vec(1:nbdime))
         
         field(1:nbdime,ig) = vec(1:nbdime)

      enddo
      
  case(3) ! effort

      do ig=1,NbGp_ele
         CALL get_stress_1_MAILx(ibdyty,iblmty,ig,vec(1:nbdime))
         
         CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))  

         ! Q(1:nbdime,1:nbdime) = reshape(internal(nb_internal+1-(nbdime*nbdime):nb_internal,ig),(/nbdime,nbdime/))  
         ! vec(1:nbdime)=matmul(Q(1:nbdime,1:nbdime),vec(1:nbdime))
         
         field(1:nbdime,ig) = vec(1:nbdime)*internal(nb_internal-(nbdime*nbdime),ig)

      enddo

  case default
      CALL FATERR(IAM,'Unsupported required_field : 1::strain, 2::stress')
  endselect
   
  deallocate(internal)

  
END SUBROUTINE gpv_vec_joint

!------------------------------------------------------------------------

SUBROUTINE gpv_frame_joint(i,ppsnb,ibdyty,iblmty,Field,FieldSize)

!fd routine qui recupere le repere local aux points de gauss
!  i               : element id
!  mdlnb           : model
!  field           : the computed values
!  fieldsize       : number of components of the field 

! stockage s, t, n  

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize
  REAL(kind=8),DIMENSION(:,:) :: field
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: NbGp_ele
  real(kind=8),dimension(:),allocatable :: internal1

  !                          1234567890123456789012345678901
  CHARACTER(len=31)  :: IAM='a_mecaEF_joint::gpv_frame_joint'

  ! vecteurs de travail locaux
  integer :: ig,inull,nb_internal

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)  

  if (size(field,dim=2) /= NbGp_ele) then
     call faterr(IAM,'non conforming size dim 1')
  endif

  if (size(field,dim=1) /= nbdime*nbdime) then
     call faterr(IAM,'non conforming size dim 2')
  endif
  
  Field = 0.d0

  if (NSTEP < 1) return

  nb_internal=get_nb_internal_variables(mdlnb)  

  ALLOCATE(INTERNAL1(nb_internal))
  
  do ig=1,NbGp_ele
     CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL1)     
     field(1:nbdime*nbdime,ig) = internal1(nb_internal+1-(nbdime*nbdime):nb_internal)
  enddo

  deallocate(internal1)
  
END SUBROUTINE gpv_frame_joint

!------------------------------------------------------------------------

SUBROUTINE gpv_internal_joint(i,ppsnb,ibdyty,iblmty,Field,FieldSize)

!fd routine qui recupere les valeurs des internals aux points de gauss
!  i               : element id
!  ppsnb           : property set
!  field           : the computed values
!  fieldsize       : number of components of the field 

  IMPLICIT NONE
  INTEGER :: i,ibdyty,iblmty,fieldsize
  REAL(kind=8),DIMENSION(:,:),pointer :: field
  integer,dimension(:),intent(in) :: ppsnb

  !*** variables locales
  !                          123456789012345678901234567890
  CHARACTER(len=30)  :: IAM='a_mecaEF_joint::gpv_internal_joint'
  INTEGER :: NbGp_ele
  INTEGER :: mdlnb,inull,ig

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  if (associated(field)) then
    deallocate(field)
    field => null()
  endif

  if (NSTEP < 1) return

  fieldsize=get_nb_internal_variables(mdlnb)  

  if (fieldsize == 0) return

  ALLOCATE(field(fieldsize,NbGp_ele))

  do ig=1,NbGp_ele

       CALL get_internal_1_MAILx(ibdyty,iblmty,ig,field(:,ig))

  enddo

END SUBROUTINE gpv_internal_joint

!------------------------------------------------------------------------
!------------------------------------------------------------------------

SUBROUTINE gp_external_field_3D_joint(i,ppsnb,ibdyty,iblmty,Field)

  ! todo gestion de l affichage des internals

  !fd routine qui recupere les valeurs aux points de gauss
  !  i               : element id
  !  mdlnb           : model
  !  field           : the computed values

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: field
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: NbGp_ele,nbf,if,ig,rank,inull,nbi,iif
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: INTERNAL
  character(len=30) :: name

  !                          12345678901234567890123456789012345678
  CHARACTER(len=39)  :: IAM='a_mecaEF_joint::gp_external_field_3D_joint'


  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nbf = get_external_field_nb(mdlnb)

!!$  nbi = get_nb_internal_variables(mdlnb)
!!$
!!$  if (nbf+nbi /= size(Field,dim=1)) then

  if (nbf /= size(Field,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbGp_ele /= size(Field,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
         
  do if = 1 , nbf 
    name=get_external_field_name(mdlnb,if)
    rank=get_meca_field_rank_MAILx(ibdyty,iblmty,name)
    do ig=1,NbGp_ele
      CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field(if,ig))
    enddo
  enddo

!!$  if ( nbi == 0) return  
!!$
!!$  ALLOCATE(INTERNAL(nbi))
!!$  
!!$  CALL get_internal_MAILx(ibdyty,iblmty,ig,INTERNAL)
!!$  
!!$  do if = 1 , nbi
!!$    iif = if + nbf
!!$    do ig=1,NbGp_ele
!!$      field(if + nbf,ig) = INTERNAL(if)
!!$    enddo
!!$  enddo
!!$  
!!$  deallocate(internal)

end subroutine

!------------------------------------------------------------------------

SUBROUTINE Stress2Fint_JOINT(i,ppsnb,ibdyty,iblmty,X,FINT)

  IMPLICIT NONE

  ! le numero de l'element
  INTEGER         , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty 
  ! coordonnees des sommets
  REAL(KIND=LONG)              :: X(:,:)                
  ! Vecteur des forces internes ele 
  REAL(KIND=LONG)              :: FINT(:)               

  ! variables locales
  REAL(KIND=LONG) , POINTER    :: Bl(:,:)
  real(kind=8), allocatable    :: Q(:,:)     
  REAL(KIND=LONG)              :: COEFINT,R
  INTEGER                      :: IG

  REAL(KIND=LONG) ,ALLOCATABLE :: FLUX(:) ! vecteur de travail local

  INTEGER                      :: mdlnb,inull,nb_external

  !                            123456789012345678901234567
  CHARACTER(len=27)  :: IAM='a_mecaEF_joint::stress2fint_joint'

  ! Initialisation a vide des pointeurs
  NULLIFY(Bl)

  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external=get_nb_external_variables(mdlnb)

  ALLOCATE(FLUX(nb_external))
  allocate(Q(nbdime,nbdime))
  
  FINT=ZERO

  DO IG=1,mecaEF(i)%N_PG_RIG                 

    CALL Bl_JOINT(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN,mecaEF(i)%PG(ig)%POIDS,X,Bl,COEFINT,Q) 
   
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,Flux)

    FINT=FINT+(MATMUL(TRANSPOSE(Bl),Flux(1:SIZE(Bl,dim=1)))*COEFINT)

 ENDDO

 DEALLOCATE(Bl) ; NULLIFY(Bl)
 DEALLOCATE(FLUX)
 deallocate(Q)
 
END SUBROUTINE Stress2Fint_JOINT

!*************************************************************************
!> routines calculant les fields aux 
!> pg en appelant une routine user

SUBROUTINE compute_elementary_field_joint(i,ppsnb,time,dt)
  implicit none

  INTEGER        ,INTENT(IN) :: i             ! le numero de l'element
  integer        ,intent(in) :: ppsnb(:)      ! le pps du gp
  real(kind=8)               :: time,dt

  !*** variables locales a la routine
  !                                123456789012345678901234567890123456789012
  CHARACTER(len=42)        :: IAM='a_mecaEF_joint::compute_elementary_field_joint'

  call gp_field('incre',time,dt,nbdime,mecaEF(i)%N_PG_RIG)

END SUBROUTINE 

SUBROUTINE get_elementary_field_joint(i,ppsnb,name,time,dt,coor_node,field_pg)
  implicit none

  INTEGER        ,INTENT(IN) :: i              ! le numero de l'element
  integer        ,intent(in) :: ppsnb(:)       ! le pps du gp
  CHARACTER(len=30)          :: name           ! nom du field
  REAL(KIND=8),intent(in) :: coor_node(:,:) ! coordonnees des points de Gauss
  REAL(KIND=8),intent(in) :: field_pg(:)    ! field aux points de Gauss
  real(kind=8)               :: time,dt

  !*** variables locales a la routine
  !                                12345678901234567890123456789012345678
  CHARACTER(len=38)        :: IAM='a_mecaEF_joint::get_elementary_field_joint'

  integer :: idime,ig

  REAL(KIND=8),allocatable :: coor_pg(:,:) ! coordonnees des points de Gauss

  allocate(coor_pg(nbdime,mecaEF(i)%N_PG_RIG))

  DO ig=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss
    do idime=1,nbDIME
      coor_pg(idime,ig)=dot_product(mecaEF(i)%PG(ig)%N(:),coor_node(idime,:))
    enddo
  ENDDO

  call gp_field('setgp',time,dt,nbdime,mecaEF(i)%N_PG_RIG, &
                field_name=name,gp_coor=coor_pg,field_value=field_pg)

  deallocate(coor_pg)

END SUBROUTINE 

!*************************************************************************

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
    call FATERR('a_mecaEF_joint::compute_gp2node','nbgp inconsistancy')
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

subroutine get_id_mecaef_joint(name,id)
  implicit none
  CHARACTER(len=5) :: name
  integer      :: id
                           !1234567890123456789012345678901
  character(len=31) :: IAM='a_mecaEF_joint::get_id_mecaef_joint'

  do id=1,size(mecaEF)
    if (name == get_NAME_mecaEF_joint(mecaEF(id)%name) ) exit
  enddo
  
  if (id > size(mecaEF)) then
    call FATERR(IAM,'Unknown element:'//name)
  endif

end subroutine

!------------------------------------------------------------------------------!

!> \brief Get the number of dof in an element
function get_N_DOF_mecaEF_joint(id)
  implicit none
  integer(kind=4), intent(in) :: id       !< [in] id of the mechanical iso element
  integer(kind=4) :: get_N_DOF_mecaEF_joint !< [return] number of dof in the element

  get_N_DOF_mecaEF_joint =  mecaEF(id)%N_NODE * mecaEF(id)%N_DOF_by_NODE
end function

!------------------------------------------------------------------------------!

!> \brief Get the number of dof of a node of an element
function get_N_DOF_of_NODE_mecaEF_joint(id, i_node)
  implicit none
  integer(kind=4), intent(in) :: id               !< [in] id of the mechanical iso element
  integer(kind=4), intent(in) :: i_node           !< [in] index of the node
  integer(kind=4) :: get_N_DOF_of_NODE_mecaEF_joint !< [return] number of dof in the element

  get_N_DOF_of_NODE_mecaEF_joint = mecaEF(id)%N_DOF_by_NODE
end function

!------------------------------------------------------------------------------!

!> get a pointer on a gauss_pt object stored at gp ig of ele type id
function get_gp_ptr_mecaEF_joint(id,ig)
  implicit none 
  integer :: id,ig
  type (T_pt_gauss), pointer :: get_gp_ptr_mecaEF_joint

  get_gp_ptr_mecaEF_joint => mecaEF(id)%PG(ig)

end function


!> get a pointer on working element array
subroutine get_ele_ptr_mecaEF_joint(id,coor_ele,primal_ele,dual_ele,operator_ele)
  implicit none 
  integer :: id
  real(kind=8), pointer :: coor_ele(:),primal_ele(:),dual_ele(:),operator_ele(:,:)

  coor_ele => mecaEF(id)%coor_ele
  primal_ele => mecaEF(id)%primal_ele
  dual_ele => mecaEF(id)%dual_ele
  operator_ele => mecaEF(id)%operator_ele

end subroutine


subroutine get_nearest_gp_mecaEF_joint(eleid,X,coor,gpid)
  implicit none
  ! le numero de l'element dans la liste locale
  integer, intent(in) :: eleid       
  ! coordonnees des sommets
  real(kind=LONG), dimension(:,:), intent(in)  :: X
  ! coordonnees du point
  real(kind=LONG), dimension(:)  , intent(out) :: coor
  ! gp id
  integer, intent(out) :: gpid

  call get_nearest_gp(mecaEF(eleID)%N_NODE       , &
                      mecaEF(eleID)%T_FONC_FORME , &
                      mecaEF(eleID)%SCH_GAUSS_RIG, &
                      nbdime, X, coor, gpid        ) 
  
end subroutine
  
subroutine check_elementary_ppset_joint(i,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty

  !**
  integer                         :: ig,mdlnb,lawnb
  integer                         :: drank,ecrank
  real(kind=8)                    :: density,elas_coeff(2)
  ! le materiau est defini par element

  
  IF (get_eleop_value_bypps(ppsnb(1),'isext') /= 'no___') THEN

    call get_ppset_value(ppsnb(1),mdlnb,lawnb)   
     
    drank=0
    if (get_rho_type(lawnb) .ne. 0) then
      drank = get_meca_field_rank_MAILx(ibdyty,iblmty,'density')
    endif
     
    ecrank=0 
    if (get_elas_coeff_type(lawnb) .ne. 0) then
      ecrank = get_meca_vfield_rank_MAILx(ibdyty,iblmty,'elas_coeff')
    endif
     
    do IG=1,mecaEF(i)%N_PG_RIG

       if (drank==0 .and. ecrank==0) then
         call check_external_ppset(ppsnb(ig))  
       else if (drank/=0 .and. ecrank==0) then           
         call get_meca_field_MAILx(ibdyty,iblmty,ig,drank,density)
         call check_external_ppset(ppsnb(ig),sf_density=density)  
       else if (drank/=1 .and. ecrank/=1) then           
         call get_meca_field_MAILx(ibdyty,iblmty,ig,drank,density)
         call get_meca_vfield_MAILx(ibdyty,iblmty,ig,ecrank,elas_coeff,2)          
         call check_external_ppset(ppsnb(ig),sf_density=density,vf_elas_coeff=elas_coeff)  
       else if (drank==0 .and. ecrank/=1) then           
         call get_meca_vfield_MAILx(ibdyty,iblmty,ig,ecrank,elas_coeff,2)          
         call check_external_ppset(ppsnb(ig),vf_elas_coeff=elas_coeff)  
       endif
      
    enddo   
  endif  
end subroutine check_elementary_ppset_joint

END MODULE a_mecaEF_joint
