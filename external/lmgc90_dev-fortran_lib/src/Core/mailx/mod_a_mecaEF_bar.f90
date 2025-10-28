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
MODULE a_mecaEF_bar

  !! Basic computations on bar Finite Elements
  !! ---> Unstable yet !!
  !!****

use overall

USE utilities
USE a_EF

USE bulk_behaviour

USE models

USE ExternalModelsHandler

USE MAILx

implicit none

private

TYPE T_mecaEF_bar
   integer         :: name
   integer         :: N_NODE
   integer         :: N_DOF_by_NODE
   integer         :: T_FONC_FORME
   integer         :: N_PG_RIG
   integer         :: SCH_GAUSS_RIG
   integer         :: N_PG_MAS
   integer(kind=4) :: SCH_GAUSS_MAS
   type(T_PT_GAUSS), pointer :: PG(:)
   !mapping gp -> node (noeuds sommets)
   real(kind=8),pointer      :: gp2node(:,:) 
   !mapping node -> edge (noeuds cote)
   real(kind=8),pointer      :: node2edge(:,:) 
END TYPE T_mecaEF_bar 

TYPE(T_mecaEF_bar),DIMENSION(1),PRIVATE :: mecaEF

integer, parameter, private :: i_barxx = 1

public get_nb_ele_bar, &
       init_mecaEF_bar, &
       get_NAME_mecaEF_bar, &
       get_N_NODE_mecaEF_bar, &
       get_N_DOF_by_NODE_mecaEF_bar, &
       get_bw_mecaEF_bar, &
       get_N_GP_mecaEF_bar, &       
       compute_elementary_bulk_bar, &
       compute_elementary_mass_bar, &
       compute_elementary_fields_bar, &
       gpv2node_2D_bar, gpv2node_3D_bar, &
       get_coor_pg_BAR, gpv_bar, gp_external_field_bar

PRIVATE get_N_PG_RIG_mecaEF, get_SCH_GAUSS_RIG_mecaEF
        !get_T_FONC_FORME_mecaEF

CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_bar(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_bar=SIZE(mecaEF)

END FUNCTION get_nb_ele_bar

SUBROUTINE init_mecaEF_bar
  IMPLICIT NONE
!                           12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_mecaEF_bar::init_mecaEF_bar'

! bar FE 2D/3D
  mecaEF(1)%name          = i_barxx
  mecaEF(1)%N_NODE        = 2
  mecaEF(1)%N_DOF_by_NODE = nbdime
  mecaEF(1)%T_FONC_FORME  = i_l_p1
  mecaEF(1)%N_PG_RIG      = 1 
  mecaEF(1)%SCH_GAUSS_RIG = i_LIG1
  mecaEF(1)%N_PG_MAS      = 1
  mecaEF(1)%SCH_GAUSS_MAS = i_lig1
!
  allocate(mecaEF(1)%gp2node(mecaEF(1)%N_NODE,mecaEF(1)%N_PG_RIG))
  mecaEF(1)%gp2node = 1.d0
  nullify(mecaEF(1)%node2edge)

END SUBROUTINE init_mecaEF_bar

character(len=5) function get_NAME_mecaEF_bar(i)
  implicit none
  integer, intent(in) :: i

  select case(mecaEF(i)%name)
  case( i_barxx )
    get_NAME_mecaEF_bar='BARxx'
  case default
    get_NAME_mecaEF_bar='xxxxx'
  end select

end function get_NAME_mecaEF_bar

INTEGER FUNCTION get_N_NODE_mecaEF_bar(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_mecaEF_bar=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_NODE_mecaEF_bar

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF_bar(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_mecaEF_bar=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_mecaEF_bar

INTEGER FUNCTION get_bw_mecaEF_bar(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,i,j,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_mecaEF_bar=0
  DO i=1,mecaEF(nb)%N_NODE-1
    DO j=i+1,mecaEF(nb)%N_NODE
      tempo = ABS(nodes(i) - nodes(j))
      IF (tempo .GT. get_bw_mecaEF_bar) get_bw_mecaEF_bar=tempo
    ENDDO
  ENDDO

  get_bw_mecaEF_bar = (get_bw_mecaEF_bar+1)*mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_mecaEF_bar

INTEGER FUNCTION get_N_GP_mecaEF_bar(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_mecaEF_bar=mecaEF(nb)%N_PG_RIG

END FUNCTION get_N_GP_mecaEF_bar


!=========================================================================
!
! Rigidity of the bar element
!
SUBROUTINE compute_elementary_bulk_bar(i,ppsnb,dt,X,U,ibdyty,iblmty, &
                                       Fint,compute_fint,            &
                                       K,compute_stiffness,          &
                                       push_fields )

  INTEGER        , INTENT(IN)     :: I             ! rank of the element
  integer        , intent(in)     :: ppsnb(:)      ! property-set of gp
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty ! body and element rank
  real(kind=8)                    :: dt            ! time step
  REAL(KIND=LONG)                 :: X(:,:),U(:,:) ! coordinates and displacement
  REAL(KIND=LONG)                 :: K(:,:)        ! matrice de rig. elem.
  REAL(KIND=LONG)                 :: Fint(:) 
  logical                         :: compute_fint, &
                                     compute_stiffness, &
                                     push_fields
                                         !12345678901234567890123456789012345678
  character(len=38)               :: IAM='a_meca_EF_bar::compute_elementary_bulk'

  ! ***
  ! variables locales
  REAL(KIND=LONG), POINTER       :: Bl(:,:) => NULL()
  REAL(KIND=LONG)                :: COEFINT

  ! Young Modulus, section, axial pre-stress
  REAL(KIND=LONG)                :: YOUNG,A0,S0
  ! axial strain and stress
  REAL(KIND=LONG)                :: axial_strain, axial_stress
  
  integer                        :: mdlnb,lawnb,extP_nb,if,rank
  INTEGER                        :: nb_external, nb_internal
  integer                        :: anisotropie !(0 iso, 1 ortho, 2 an)
  REAL(kind=8),DIMENSION(21)     :: elas_coeff
  character(len=30) :: name
  
  !  strain and stress
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD0,FLUX0,INTERNAL0
  ! 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD1,FLUX1,INTERNAL1

  real(kind=8) :: H2(16) =  (/ 1.d0,  0.d0, -1.d0,  0.d0, &
                               0.d0,  1.d0,  0.d0, -1.d0, &
                              -1.d0,  0.d0,  1.d0,  0.d0, &
                               0.d0, -1.d0,  0.d0,  1.d0 /)
  
  real(kind=8) :: H3(36) =  (/ 1.d0,  0.d0,  0.d0, -1.d0,  0.d0,  0.d0, &
                               0.d0,  1.d0,  0.d0,  0.d0, -1.d0,  0.d0, &
                               0.d0,  0.d0,  1.d0,  0.d0,  0.d0, -1.d0, &
                              -1.d0,  0.d0,  0.d0,  1.d0,  0.d0,  0.d0, &
                               0.d0, -1.d0,  0.d0,  0.d0,  1.d0,  0.d0, &
                               0.d0,  0.d0, -1.d0,  0.d0,  0.d0,  1.d0 /)
  
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)  

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),FLUX0(nb_external),INTERNAL0(nb_internal))
  ALLOCATE(GRAD1(nb_external),FLUX1(nb_external),INTERNAL1(nb_internal))

  GRAD1     = 0.d0
  FLUX1     = 0.d0
  INTERNAL1 = 0.d0

  ! recovering Young Modulus
  CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)

  if (anisotropie == 0) then
    Young = elas_coeff(1)
  else
    call faterr(IAM,'Unsupported isotropy')
  endif
   
  ! default values of section and prestress
  A0 = 1.d0 ! section 
  S0 = 0.d0 ! axial pre-stress

  extP_nb = get_external_field_nb(mdlnb)
  IF (extP_nb == 0) THEN
    do if=1,extP_nb
      name=get_external_field_name(mdlnb,if)
      if (trim(name) == 'SECTION') then        
        rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
        CALL get_meca_field_MAILx(ibdyty,iblmty,1,rank,A0)
      endif        
      if (trim(name) == 'PRE-STRESS') then        
        rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
        CALL get_meca_field_MAILx(ibdyty,iblmty,1,rank,S0)
      endif        
    enddo       
  endif

  ! recovering previous strain/stress
  CALL get_stress_0_MAILx(ibdyty,iblmty,1,FLUX0)
  CALL get_strain_0_MAILx  (ibdyty,iblmty,1,GRAD0)
  IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,1,INTERNAL0)

  ! computing strain/stress

  call GL_strain_bar(X,U,axial_strain)
  axial_stress = S0 + Young*axial_strain

  ! computing gradient
  CALL GRADIENT_BAR(X,U,Bl,COEFINT)

  !print*,'<',IAM
  !print*,coefint
  !print*,BL
  
  Fint=ZERO
  K=ZERO

  !
  ! K = A0*L0*E*Bt.B + A0*L0*S*H
  !
  
  if (compute_stiffness) then
     K = K + (MATMUL(TRANSPOSE(Bl),Bl)*YOUNG*A0*COEFINT)
     if (nbdime == 2) then
       K = K + A0*COEFINT*axial_stress*reshape(H2,(/4,4/))
     else if (nbdime == 3) then
       K = K + A0*COEFINT*axial_stress*reshape(H3,(/6,6/))
     endif  
  endif 

  !print*,K
  
  !
  !  Fint= A0*L0*S*Bt
  !

  if (compute_fint) Fint = Fint + A0*COEFINT*axial_stress*Bl(1,:)

  if (push_fields) then
    GRAD1(1) = axial_strain
    FLUX1(1) = axial_stress
    !
    CALL put_stress_MAILx(ibdyty,iblmty,1,FLUX1)
    CALL put_strain_MAILx(ibdyty,iblmty,1,GRAD1)
    IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,1,INTERNAL1)
  endif

  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)

  !print*,IAM,'>'

END SUBROUTINE compute_elementary_bulk_bar

!> computes strain and stress fields  
SUBROUTINE compute_elementary_fields_bar(i,ppsnb,dt,X,U,ibdyty,iblmty)

  implicit none
  INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty
  REAL(KIND=LONG)                 :: X(:,:),U(:,:) ! coordonnees et deplacement des sommets
  real(kind=8)                    :: dt

                          !12345678901234567890123456789012345678901234
  character(len=44):: IAM='a_meca_EF_iso::compute_elementary_fields_bar'

  ! Young Modulus, section, axial pre-stress
  REAL(KIND=LONG)                :: YOUNG,A0,S0
  ! axial strain and stress
  REAL(KIND=LONG)                :: axial_strain, axial_stress
  
  integer                        :: mdlnb,lawnb,extP_nb,if,rank
  INTEGER                        :: nb_external, nb_internal
  integer                        :: anisotropie !(0 iso, 1 ortho, 2 an)
  REAL(kind=8),DIMENSION(21)     :: elas_coeff
  character(len=30) :: name

  ! gradient de la transformation et contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD0,FLUX0,INTERNAL0
  ! 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD1,FLUX1,INTERNAL1
  
   
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ! recovering Young Modulus
  CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)

  if (anisotropie == 0) then
    Young = elas_coeff(1)
  else
    call faterr(IAM,'Unsupported isotropy')
  endif
   
  ! default values of section and prestress
  A0 = 1.d0 ! section 
  S0 = 0.d0 ! axial pre-stress

  extP_nb = get_external_field_nb(mdlnb)
  IF (extP_nb == 0) THEN
    do if=1,extP_nb
      name=get_external_field_name(mdlnb,if)
      if (trim(name) == 'SECTION') then        
        rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
        CALL get_meca_field_MAILx(ibdyty,iblmty,1,rank,A0)
      endif        
      if (trim(name) == 'PRE-STRESS') then        
        rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
        CALL get_meca_field_MAILx(ibdyty,iblmty,1,rank,S0)
      endif        
    enddo       
  endif

  ! 1 gp
  CALL get_stress_0_MAILx(ibdyty,iblmty,1,FLUX0)
  CALL get_strain_0_MAILx  (ibdyty,iblmty,1,GRAD0)
  IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,1,INTERNAL0)

  ! computing strain/stress

  call GL_strain_bar(X,U,axial_strain)
  axial_stress = S0 + Young*axial_strain

  GRAD1(1) = axial_strain
  FLUX1(1) = axial_stress

  CALL put_stress_MAILx(ibdyty,iblmty,1,FLUX1)
  CALL put_strain_MAILx(ibdyty,iblmty,1,GRAD1)
  IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,1,INTERNAL1)
 
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)
   
END SUBROUTINE

!> computes mass matrix
SUBROUTINE compute_elementary_mass_bar(i,ppsnb,X,M,ibdyty,iblmty)

  IMPLICIT NONE

  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty 
  integer,dimension(:)        :: ppsnb
  ! coordonnees des sommets
  REAL(KIND=8)                :: X(:,:)     
  ! matrice de rig. elem.
  REAL(KIND=8)                :: M(:,:)     

  REAL(KIND=8)                :: A0,L0,nodal_mass,rho

  INTEGER                     :: mdlnb,lawnb,extP_nb,if,rank,j
  character(len=30) :: name

  ! nbDIME est defini dans overall

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  M = 0.d0

  rho= get_RHO(lawnb)

  if (rho == 0.d0) return

  A0 = 1.d0
  extP_nb = get_external_field_nb(mdlnb)
  IF (extP_nb == 0) THEN
    do if=1,extP_nb
      name=get_external_field_name(mdlnb,if)
      if (trim(name) == 'SECTION') then        
        rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
        CALL get_meca_field_MAILx(ibdyty,iblmty,1,rank,A0)
      endif        
    enddo       
  endif

  if (nbdime ==2) then
    L0 = sqrt((X(1,1)-X(1,2))**2 + (X(2,1)-X(2,2))**2) 
  else if (nbdime==3) then
    L0 = sqrt((X(1,1)-X(1,2))**2 + (X(2,1)-X(2,2))**2 + (X(3,1)-X(3,2))**2)     
  endif   
     
  nodal_mass = 0.5d0*rho*A0*L0

  !print*,'nmass',nodal_mass
  


  ! taille matrice nbdime*nbdime  

  do j=1,2*nbdime
    M(j,j) = nodal_mass
  enddo
 
END SUBROUTINE compute_elementary_mass_bar


!------------------------------------------------------------------------
SUBROUTINE gpv2node_2D_bar(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,NbFields,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model of the element  
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  nodalvalues     : the computed values
!  nbfields        : 
!  nbnodes_stored  :

  IMPLICIT NONE
  INTEGER :: i,ibdyty,iblmty,nbfields,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele,ii
  REAL(kind=8),DIMENSION(:),allocatable :: GaussPointValues_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  REAL(kind=8) :: f1,f2,f3
  INTEGER :: NbNodes_stored,i_node

!                            1234567890123456789012345
  CHARACTER(len=25)  :: IAM='a_mecaEF_iso::gpv2node_2D'
  
  ! vecteurs de travail locaux
  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),Field(:,:)     
  integer :: ig,if,mdlnb,inull,nb_external,nb_internal

  real(kind=8) :: tmp,PP(5),FF(5)
  integer :: nbs,nbm

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele

  if (nbfields /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes')
  endif

  allocate(GaussPointValues_ele(NbGp_ele))
  GaussPointValues_ele=0.d0

  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  allocate(Field(Nbfields,NbGp_ele))
  Field = 0.d0

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  CALL get_stress_1_MAILx(ibdyty,iblmty,1,FLUX(:,ig))
  CALL get_strain_1_MAILx  (ibdyty,iblmty,1,GRAD(:,ig))
  IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,1,INTERNAL(:,ig))

  Select case(required_field)
  case(1) ! strain
    Field(1,1) = GRAD(1,1) 
  case(2) ! stress
    FIELD(1,1) = FLUX(1,1)
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

  deallocate(v_nodes,GRAD,FLUX,INTERNAL,GaussPointValues_ele,field)

END SUBROUTINE gpv2node_2D_bar

!------------------------------------------------------------------------
SUBROUTINE gpv2node_3D_bar(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain,        2==cauchy stress, 
!                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress 
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele
  REAL(kind=8),DIMENSION(:),allocatable :: GaussPointValues_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: NbNodes_stored,i_node

!                            12345678901234567890123456789
  CHARACTER(len=29)  :: IAM='a_mecaEF_iso::gpv2node_3D_iso'

  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),Field(:,:)     ! vecteur de travail local
  integer :: ig,if,inull,nb_external,nb_internal

  real(kind=8) :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

  ! TODO mettre ca dans l'element fini
  real(kind=8),dimension(:,:),allocatable :: smat,mmat ! calcul valeur aux noeuds sommet, noeuds milieu 
  real(kind=8) :: f1,f2,f3,f4
  integer :: nbs,nbm

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
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

  CALL get_stress_1_MAILx(ibdyty,iblmty,1,FLUX(:,1))
  CALL get_strain_1_MAILx  (ibdyty,iblmty,1,GRAD(:,1))
  IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,1,INTERNAL(:,1))

  Select case(required_field)
  case(1) ! Euler Almansi Strain
    FIELD(1,1) = GRAD(1,1)
  case(2) ! Cauchy Stress
    FIELD(1,1) = FLUX(1,1)
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
  !print *, '  nv  : ', NodalValues(:,1:4)

  deallocate(v_nodes,GRAD,FLUX,INTERNAL,GaussPointValues_ele,field)
  !deallocate(smat)
  !if (allocated(mmat)) deallocate(mmat)

END SUBROUTINE gpv2node_3D_bar

!============= low level private routines ==================
!------------------------------------------------------------------------------!
! computes GL: e = L^2 - L0^2 / 2*L0^2 = B0 u + 0.5 u^T H u
!------------------------------------------------------------------------------!
SUBROUTINE GL_Strain_BAR(X,U,e)
  implicit none 
  REAL(KIND=LONG), INTENT(IN)  :: X(:,:),U(:,:)
  REAL(KIND=LONG)              :: e

  ! Variables locales
  REAL(KIND=LONG)     :: X12,Y12,Z12,UX12,UY12,UZ12,L02,iL02

  if (nbdime == 2) then

    X12=x(1,1)-x(1,2)  ; Y12=x(2,1)-x(2,2)
    UX12=U(1,1)-U(1,2)  ; UY12=U(2,1)-U(2,2) 

    L02= X12**2 + Y12**2

    iL02 = 1.d0 / L02

    e = iL02 * (X12*UX12 + Y12*UY12 + 0.5d0*(UX12*UX12 + UY12*UY12))
      
  else if (nbdime == 3) then

    X12=x(1,1)-x(1,2)  ; Y12=x(2,1)-x(2,2)  ; Z12=x(3,1)-x(3,2)
    UX12=u(1,1)-u(1,2)  ; UY12=u(2,1)-u(2,2)  ; UZ12=u(3,1)-u(3,2)   

    L02= X12**2 + Y12**2 + Z12**2

    iL02 = 1.d0 / L02

    e = iL02 * (X12*UX12 + Y12*UY12 + Z12*UZ12 + &
               0.5d0*(UX12*UX12 + UY12*UY12 + UZ12*UZ12))

  endif   

end subroutine GL_Strain_BAR
  
!------------------------------------------------------------------------------!
! compute Bl = B0 + u^T H
!------------------------------------------------------------------------------!
SUBROUTINE GRADIENT_BAR(X,U,Bl,COEFINT)
  implicit none
  REAL(KIND=LONG), INTENT(IN)  :: X(:,:),U(:,:)
  REAL(KIND=LONG), POINTER     :: Bl(:,:)
  REAL(KIND=LONG), INTENT(OUT) :: COEFINT

  ! Variables locales
  REAL(KIND=LONG)     :: X12,Y12,Z12,L02
  REAL(KIND=LONG)     :: xx(3,2)

  IF (ASSOCIATED(Bl)) THEN ; DEALLOCATE(Bl) ; NULLIFY(Bl) ; ENDIF

  xx(1:nbdime, 1:2)  = X(1:nbdime, 1:2) + U(1:nbdime,1:2)
     
  !print*,'<GRADIENT_BAR'
  !print*,nbdime
  !print*,X
  !print*,U 
  
  ALLOCATE(Bl(1,2*nbdime))

  if (nbdime == 2) then

    X12=xx(1,1)-xx(1,2)  ; Y12=xx(2,1)-xx(2,2)

    L02= (x(1,1)-x(1,2))**2 + (x(2,1)-x(2,2))**2
 
    COEFINT=SQRT(L02)

    Bl(1,1)=  X12/L02  ; Bl(1,2) =  Y12/L02
    Bl(1,3)= -X12/L02  ; Bl(1,4) = -Y12/L02

      
  else if (nbdime == 3) then

    X12=xx(1,1)-xx(1,2)  ; Y12=xx(2,1)-xx(2,2)  ; Z12=xx(3,1)-xx(3,2)   

    L02= (x(1,1)-x(1,2))**2 + (x(2,1)-x(2,2))**2 + (x(3,1)-x(3,2))**2

    COEFINT=SQRT(L02)

    Bl(1,1)=  X12/L02  ; Bl(1,2) =  Y12/L02  ; Bl(1,3) =  Z12/L02
    Bl(1,4)= -X12/L02  ; Bl(1,5) = -Y12/L02  ; Bl(1,6) = -Z12/L02

  endif

  !print*,L02,X12,Y12,Z12
  !print*,Bl
  !print*,'GRADIENT_BAR>'

END SUBROUTINE GRADIENT_BAR


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

!character(len=4) function get_T_FONC_FORME_mecaEF(nb)
!  implicit none
!  integer          :: nb
!
!  get_T_FONC_FORME_mecaEF=mecaEF(nb)%T_FONC_FORME
!
!end function get_T_FONC_FORME_mecaEF

!> Compute the cooridnates of the Gauss points
subroutine get_coor_pg_BAR(i,X,coor_pg)
  implicit none
  !> index of the element
  integer(kind=4), intent(in) :: i
  !> coordinates of the element
  real(kind=8), dimension(:,:), intent(in) :: X
  !> coordinates of the Gauss points
  real(kind=8), dimension(:,:), intent(out) :: coor_pg
  !
  integer(kind=4) :: ig,idime

  ! only linear bar with 1 GP :
  coor_pg(:,1) = ( X(:,1)+X(:,2) ) / 2.d0

end subroutine

!> Get Gauss points values of field
subroutine gpv_bar(i,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)
  implicit none
  !> bar element index
  integer(kind=4), intent(in) :: i
  !> property set number
  integer(kind=4), dimension(:), intent(in) :: ppsnb
  !> MAILx body id
  integer(kind=4), intent(in) :: ibdyty
  !> MAILx element id
  integer(kind=4), intent(in) :: iblmty
  !> number of component of the field
  integer(kind=4), intent(in) :: fieldsize
  !> type of field required (1: almansy strain, 2: cauchy stress )
  integer(kind=4), intent(in) :: required_field
  !> the required field values
  real(kind=8), dimension(:,:), intent(inout) :: field
  !
  integer(kind=4)   :: NbGp_ele
  character(len=21) :: IAM

  ! vecteurs de travail locaux
  real(kind=LONG), dimension(:,:), allocatable :: GRAD, FLUX, INTERNAL
  integer(kind=4) :: ig, mdlnb, inull, nb_external, nb_internal

        !123456789012345678901
  IAM = 'a_mecaEF_bar::gpv_bar'

  NbGp_ele = mecaEF(i)%N_PG_RIG  
  call get_ppset_value(ppsnb(1),mdlnb,inull)

  if (fieldsize /= size(field,dim=1)) then
    call faterr(IAM,'Non conforming sizes first dim')
  end if

  if (NbGp_ele /= size(field,dim=2)) then
    call faterr(IAM,'Non conforming sizes second dim')
  end if

  field = 0.d0

  if (nstep < 1) return

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  allocate(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  allocate(INTERNAL(nb_internal,NbGp_ele))

  do ig = 1, NbGp_ele
    call get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    call get_strain_1_MAILx  (ibdyty,iblmty,ig,GRAD(:,ig))
    if (nb_internal /= 0 ) call get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  end do

  select case(required_field)

  case(1) ! strain

    do ig = 1, NbGp_ele
      field(1,ig) = grad(1,ig)
    end do

  case(2) ! stress

    do ig = 1, NbGp_ele
      field(1,ig) = flux(1,ig)
    end do

  endselect

  deallocate(GRAD,FLUX,INTERNAL)

end subroutine gpv_bar

!> Get external field values at Gauss points
!> todo: gestion de l affichage des internals
subroutine gp_external_field_bar(i,ppsnb,ibdyty,iblmty,Field)
  !> bar element index
  integer(kind=4), intent(in) :: i
  !> property set number
  integer(kind=4), dimension(:), intent(in) :: ppsnb
  !> MAILx body id
  integer(kind=4), intent(in) :: ibdyty
  !> MAILx element id
  integer(kind=4), intent(in) :: iblmty
  !> the field value
  real(kind=8), dimension(:,:), intent(inout) :: field
  !
  integer(kind=4) :: NbGp_ele, mdlnb, nbf, if, ig, rank, inull
  character(len=30) :: name
  character(len=35) :: IAM

!        12345678901234567890123456789012345
  IAM = 'a_mecaEF_bar::gp_external_field_bar'

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  call get_ppset_value(ppsnb(1),mdlnb,inull)

  nbf = get_external_field_nb(mdlnb)

  if (nbf /= size(Field,dim=1)) then
    call faterr(IAM,'Non conforming sizes first dim')
  end if

  if (NbGp_ele /= size(Field,dim=2)) then
    call faterr(IAM,'Non conforming sizes second dim')
  end if
         
  do if = 1, nbf 
    name = get_external_field_name(mdlnb,if)
    rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
    do ig = 1, NbGp_ele
      call get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field(if,ig))
    end do
  end do

end subroutine

END MODULE a_mecaEF_bar

