!todo
!
! clarifier la notion de nbdime et nbdof !!!
! a priori coor devrait etre dimensionne avec nbdime.
!
! avec les decisions sur l'architecture de lmgc90
! notamment 1 avatar == 1 modele
! ce module doit devenir un module generique pour les modeles
! inversion sens de visibilite. 
!
! reprendre les fields pour avoir la possibilite d'affecter
! au point de Gauss: scalaire/vecteur/matrice/tenseur
! utilisation par exemple :
! scalaire: module d'Young 
! vecteur: une position relative
! matrice: un repere local
! une defo ou une contrainte
!
! sur les elements tout gerer par des fields
! non des field mutualise sur un element
! optimiser 


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
!> \defgroup Mailx  Finite Element

!> geometrical description and internal database for deformable bodies discretized by Finite Element
MODULE MAILx                                       

  USE overall
  USE utilities
  USE a_DOF

  ! 2D
  USE a_BDARY_CLxxx
  USE a_BDARY_ALpxx
  USE a_BDARY_PT2DL
  USE a_BDARY_DISKL

  ! 3D
  USE a_BDARY_xSpxx
  USE a_BDARY_POLYD

  IMPLICIT NONE

  PRIVATE

  !
  !>TODO la notion de public/private
  !

  INTEGER   :: nb_MAILx                  ! number of deformable bodies, public
  INTEGER   :: ifrom,ito,IGOZZZ

  !------------------------------------------------------------------------
  ! Description of the bulk geometric properties,
  ! the bulk physical types to construct body models are described in 
  ! (a_mecaEF, a_thermEF, ...)
  !
  ! we add the elementary external and internal variables fields for each
  ! gauss point
  !
  !------------------------------------------------------------------------
  !> \addtogroup Mailx 
  !>\{
  TYPE,PUBLIC :: T_blmty 
     ! type of geometric bulk element: T3xxx,Q4xxx,....
     character(len=5) :: blmID
     ! table de connectivite
     integer(kind=4) , dimension(:), pointer :: NODES => null()
     ! nickname of bulk model (meca, therm ...) & FE elementary options
     character(len=5), dimension(:), pointer :: model => null()
     ! nickname of bulk material corresponding to the bulk model
     character(len=5), dimension(:), pointer :: behav => null()

     logical :: is_meca_present = .false.
     ! gauss point values (begining/end of time step)
     type(T_elem_meca), dimension(:,:), pointer :: meca_gpv => null()

     logical :: is_ther_present = .false.
     ! gauss point values ( 2 * nb de point de gauss) 2 == profondeur en temps
     type(T_elem_ther), dimension(:,:), pointer :: ther_gpv => null()

     logical :: is_thmc_present = .false.
     ! gauss point values ( 2 * nb de point de gauss)
     type(T_elem_thmc), dimension(:,:), pointer :: thmc_gpv => null()
     
     logical :: is_poro_present = .false.
     ! gauss point values sur la meca ( 2 * nb de point de gauss)
     type(T_elem_poro), dimension(:,:), pointer :: poro_gpv_meca => null()
     ! gauss point values sur la pression ( 2 * nb de point de gauss)
     type(T_elem_poro), dimension(:,:), pointer :: poro_gpv_ther => null()

     ! rm: try for wsc
     logical :: is_multi_present = .false.
     ! map to access grad/flux for a physic... there should be a ccinternal !
     integer(kind=4), dimension(:), allocatable :: ccgrad
     ! gauss point values (only time depth = 2)
     type(T_elem_multi), dimension(:), pointer  :: multi_gpv => null()

  END TYPE T_blmty

  !------------------------------------------------------------------------
  ! Description of the contact elements located on a deformable body
  !
  ! 2D: 
  !     CLxxx candidate node located on a segment
  !     idata(1:2)=(/a,b/) , rdata(1) = (/ apab/)
  !     ALpxx antagoniste segment
  !    +ALpxx an adjacent segment to the previous one
  !     idata(0:nb_ALxxx+1) = (/ nb_alxxx, n1 , n2, ... /) , rdata = null
  !     PT2DL candidate node located on a segment for PT2DL 2 PT2DL relation
  !     idata(1:2) = (/a,b/) , rdata(1) = (/apab/)
  !     DISKL candidate disk located on a segment for DISKL 2 DISKx relation
  !     idata(1:2) = (/a,b/) , rdata(1) = (/apab,rd,brpm/)
  ! 3D: 
  !     CSpxx candidate surface
  !    +CSpxx candidate surface adjacent to the previous one
  !     idata(0:sz) = (/sz, nbnf1, n1, ..., nbn, nbnf2, ... /) , rdata = null
  !     ASpxx antagoniste surface
  !    +ASpxx antagoniste surface adjacent to the previous one
  !     idata(0:sz) = (/sz, nbnf1, n1, ..., nbn, nbnf2, ... /) , rdata = null
  !     POLYD closed polyedral surface
  !     idata(1:nb_faces+2) = (/ nb_vertex, nb_faces , (i1,i2,i3) , (i1,i2,i3),  ... /) , rdata = null
  ! 
  ! obsolet:
  !     CSxx3 candidate node located on a triangular surface
  !     idata(1:3)=(/a,b,c/) , rdata(1:3) = (/w1,w2,w3/)
  !     CSxx4 candidate node located on a quadrilateral surface
  !     idata(1:4)=(/a,b,c,d/) , rdata(1:4) = (/w1,w2,w3,w4/)
  !------------------------------------------------------------------------
  TYPE,PUBLIC :: T_BDARY  ! candidate or antagonist contactor boundary

     !all datas necessary to define the boundary

     INTEGER,DIMENSION(:),POINTER         :: idata => null() ! integer
     REAL(kind=8),DIMENSION(:),POINTER    :: rdata => null() ! reals

  END TYPE T_BDARY
  !------------------------------------------------------------------------

  TYPE,PUBLIC :: T_tacty 

     CHARACTER(len=5) :: tacID
     CHARACTER(len=5) :: color
     TYPE(T_BDARY)    :: BDARY       ! standard contactor (the boundary) 

     
     logical                           :: is_periodic       ! is periodic ?
     real(kind=8),dimension(:),pointer :: per_vec => null() ! periodic vector


  END TYPE T_tacty

  !------------------------------------------------------------------------
  ! Description du champ par element (valeurs aux points de Gauss)
  !
  ! 
  ! mechanical part:
  !
  !   external : stress, strain, strain_rate  (taille depend de la dimension du pb)
  !
  !   internal : depend du modele constitutif (dynamique)
  !
  ! thermal part:
  !
  !   external : temperature, ref_temperature
  !
  ! thermo mechanical part :
  !
  !   external : grad (strain:grad Temp), flux (stress:thermal flux)
  !   internal
  !
  !
  ! fd 18/09/06 
  !  on peut se demander si il ne faudrait pas unifier tout ca
  !  en n'ayant un modele generique qu'on instancie plusieurs fois      
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! Coupled field 
  ! If the behaviour of a model depends on an external field
  !  field_name  : for example TEMPERATURE 
  !  field_value : gauss point vector
  !
  ! may be revised because the name of the field is stored on each gp
  !
  ! chaque element peut porter des fields. 
  ! petit cafouillage si on a des fields differents d'un element a l'autre !!
  ! les histoires de rank ne peuvent pas etre geres par les model ou bulk
  ! info a mettre plutot dans les pps
  !------------------------------------------------------------------------  


  TYPE,PUBLIC :: T_elem_meca

     real(kind=8), dimension(:), pointer :: stress   => null()
     real(kind=8), dimension(:), pointer :: strain   => null()
     real(kind=8), dimension(:), pointer :: internal => null()

     ! Scalar fields name (may be stored at the element|model level)
     character(len=30), dimension(:)  , pointer :: field_name   => null()
     ! Scalar fields values
     real(kind=8)     , dimension(:)  , pointer :: field_value  => null()
     ! Vector fields name (may be stored at the element|model level)
     character(len=30), dimension(:)  , pointer :: vfield_name  => null()
     ! Vector fields values
     real(kind=8)     , dimension(:,:), pointer :: vfield_value => null()

  END TYPE T_elem_meca

  TYPE,PUBLIC :: T_elem_ther

     real(kind=8), dimension(:), pointer :: grad     => null()
     real(kind=8), dimension(:), pointer :: flux     => null()
     real(kind=8), dimension(:), pointer :: internal => null()

     real(kind=8) :: T,Tref

     ! Scalar fields name (may be stored at the element|model level)
     character(len=30), dimension(:)  , pointer :: field_name   => null()
     ! Scalar fields values
     real(kind=8)     , dimension(:)  , pointer :: field_value  => null()
     ! Vector fields name (may be stored at the element|model level)
     character(len=30), dimension(:)  , pointer :: vfield_name  => null()
     ! Vector fields values
     real(kind=8)     , dimension(:,:), pointer :: vfield_value => null()

  END TYPE T_elem_ther

  TYPE,PUBLIC :: T_elem_poro

     real(kind=8), dimension(:), pointer :: stress   => null()
     real(kind=8), dimension(:), pointer :: strain   => null()
     real(kind=8), dimension(:), pointer :: grad     => null()
     real(kind=8), dimension(:), pointer :: flux     => null()
     real(kind=8), dimension(:), pointer :: internal => null()

     real(kind=8) :: T,Tref

     ! Scalar fields name (may be stored at the element|model level)
     character(len=30), dimension(:)  , pointer :: field_name   => null()
     ! Scalar fields values
     real(kind=8)     , dimension(:)  , pointer :: field_value  => null()
     ! Vector fields name (may be stored at the element|model level)
     character(len=30), dimension(:)  , pointer :: vfield_name  => null()
     ! Vector fields values
     real(kind=8)     , dimension(:,:), pointer :: vfield_value => null()

  END TYPE T_elem_poro

  TYPE,PUBLIC :: T_elem_thmc

     real(kind=8), dimension(:), pointer :: grad     => null()
     real(kind=8), dimension(:), pointer :: flux     => null()
     real(kind=8), dimension(:), pointer :: internal => null()

  END TYPE T_elem_thmc

  type, public :: T_nodal_field
      character(len=30) :: name
      integer(kind=4)   :: dim1
      real(kind=8), dimension(:), pointer :: values => null()
  end type T_nodal_field


  !rm : for wsc
  !> type to store Gauss points fields for multi-phasic model
  type, public :: T_elem_multi

     !> grad
     real(kind=8), dimension(:,:), pointer :: grad => null()
     !> flux
     real(kind=8), dimension(:,:), pointer :: flux => null()
     !> internal
     real(kind=8), dimension(:,:), pointer :: internal => null()


     !> external scalar fields name
     character(len=30), dimension(:)    , pointer :: field_name   => null()
     !> external fields scalar value
     real(kind=8)     , dimension(:,:)  , pointer :: field_value  => null()
     !> external vector fields name
     character(len=30), dimension(:)    , pointer :: vfield_name  => null()
     !> external vector fields value
     real(kind=8)     , dimension(:,:,:), pointer :: vfield_value => null()

  end type T_elem_multi


  ! generic body type ----------------------------------------------------- 

  TYPE,PUBLIC :: T_MAILx

     ! liste des elements/comportement/connectivite
     type(T_blmty), dimension(:), pointer ::  blmty => null()

     ! nodes are arrays where dof are stored. Several kinds of nodes 
     ! are used. The specy of each node inodty for each body ibdyty, 
     ! is hiden in bdyty(ibdyty)%nodty.
     ! In bodies MAILx, specy NO2xx or NO3xx are essentially used. In this specy
     ! 2 or 3 degrees of freedom are stored for each node,
     ! the mechanical meaning of it beeing described below.
     ! In order to avoid calling recursively too many pointers, nodes
     ! data are concatenated in a single list with index iccdof.
     ! It avoids also defining collection of node types since the data are
     ! merely dof datas. 
     ! The dof idofty of node inodty of body ibodty has index
     ! iccdof = bdyty(ibdyty)%ccdof(inodty)+idof.
     ! In a reverse way, in the body ibdyty, the index iccdof corresponds
     ! to node inodty = bdyty(ibdyty)%nodnb(iccdof) and 
     ! to dof    idof = bdyty(ibdyty)%dofnb(iccdof) .
     ! The comments below stand for idof running from 1 to 3.
     type(T_nodty), dimension(:), pointer   ::  nodty => null()

     ! the comments below stand for idof running from 1 to 3.
     ! In a reverse way, in the body ibdyty, the index iccdof corresponds
     ! to node:
     ! iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
     integer(kind=4), dimension(:), pointer ::  ccdof => null()
     ! inodty = bdyty(ibdyty)%nodnb(iccdof)
     integer(kind=4), dimension(:), pointer ::  nodnb => null()
     ! and to dof:
     ! idof = bdyty(ibdyty)%dofnb(iccdof)
     integer(kind=4), dimension(:), pointer ::  dofnb => null()
     ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
     ! T_force_driven, with similar meanings.

     ! nodes coordinates in reference configuration
     real(kind=8), dimension(:), pointer    :: cooref => null()
     ! nodes coordinates in actual configuration
     real(kind=8), dimension(:), pointer    :: coor   => null()


     type(T_tacty), dimension(:), pointer   ::  tacty => null()


     !!!!!!! des tableaux d'administration des EF
     ! neighbor element to a node
     type(G_i_list), dimension(:), pointer  :: nod2blmty => null()
     ! neighbor element to an element
     type(G_i_list), dimension(:), pointer  :: ele2blmty => null()

     !> maximum number of elements a node belongs to (used to size a working array for sparse system)
     integer(kind=4) :: max_nod2el

     logical :: is_meca  = .false.
     logical :: is_ther  = .false.
     logical :: is_poro  = .false.
     logical :: is_multi = .false.

     type(T_nodal_field), dimension(:), pointer :: nodal_fields      => null()

     integer(kind=4),     dimension(:), pointer :: boundary_elements => null()

  END TYPE T_MAILx

!>\}
  TYPE(T_MAILx),DIMENSION(:),ALLOCATABLE,PUBLIC ::  M_bdyty

  !
  ! a type allowing to construct some reverse mapping 
  !
  TYPE,PUBLIC ::  T_MAILx_2_localMAILx

     integer(kind=4) :: bdyty
     integer(kind=4), dimension(:), pointer :: nodty => null()
     integer(kind=4), dimension(:), pointer :: blmty => null()

  END TYPE T_MAILx_2_localMAILx


!!!------------------------------------------------------------------------

  PUBLIC add_dof2bodies_MAILx, &
       read_in_bodies_MAILx, &
       write_out_bodies_MAILx, &
       write_xxx_gpv_MAILx, &
       get_write_GPV_actif_MAILx

  PUBLIC itest_bdyty_MAILx,itest_nodty_MAILx, &
       get_nb_MAILx,get_cooref_nodty_MAILx, &
       get_coor_nodty_MAILx,get_nb_tacty_MAILx, &
       get_tacID_MAILx,get_idata_MAILx,get_rdata_MAILx, &
       get_idata_sz_MAILx,get_color_MAILx,get_nb_node_MAILx, &
       get_nb_cell_MAILx,get_nb_node_by_cell_MAILx, &
       get_connect_cell_MAILx,&
       init_mecagpv_MAILx,update_mecagpv_MAILx, &
       init_thergpv_MAILx,update_thergpv_MAILx, &
       init_porogpv_MAILx,update_porogpv_MAILx, &
       init_multigpv_MAILx,update_multigpv_MAILx, &
       put_stress_MAILx,get_stress_0_MAILx,get_stress_1_MAILx, &
       put_strain_MAILx,get_strain_0_MAILx,get_strain_1_MAILx, &
       get_strain_rate_MAILx, &
       put_internal_MAILx,get_internal_0_MAILx,get_internal_1_MAILx, &
       put_flux_MAILx,get_flux_0_MAILx,get_flux_1_MAILx, &
       put_grad_MAILx,get_grad_0_MAILx, get_grad_1_MAILx, &
       put_T_MAILx,get_T_0_MAILx,get_T_1_MAILx, &
       put_Tref_MAILx,get_Tref_0_MAILx,get_Tref_1_MAILx, &
       get_Mentropy_0_MAILx,get_Mentropy_1_MAILx, &
!!$       put_ther_internal_MAILx,get_ther_internal_MAILx, &
       get_nb_outline_MAILx , &
       get_meca_field_size_MAILx, get_meca_field_name_MAILx, &
       set_meca_field_MAILx,get_meca_field_begin_MAILx,get_meca_field_MAILx, &
       get_meca_field_rank_MAILx, &
       get_meca_vfield_rank_MAILx, get_meca_vfield_max_size_MAILx, &
       set_meca_vfield_MAILx,get_meca_vfield_MAILx, &
       get_ther_field_size_MAILx, get_ther_field_name_MAILx, &
       set_ther_field_MAILx,get_ther_field_begin_MAILx,get_ther_field_MAILx, &
       get_ther_field_rank_MAILx, &
       get_ther_vfield_rank_MAILx, get_ther_vfield_max_size_MAILx, &
       set_ther_vfield_MAILx,get_ther_vfield_MAILx, &
       get_poro_field_size_MAILx, get_poro_field_name_MAILx, &
       get_poro_field_begin_MAILx,get_poro_field_MAILx, &
       get_poro_field_rank_MAILx, &
       set_poro_field_MAILx, set_poro_vfield_MAILx, &
       !get_multi_field_size_MAILx, get_multi_field_name_MAILx, &
       !get_multi_field_begin_MAILx,&
       get_multi_field_rank_MAILx, get_multi_field_MAILx, set_multi_field_MAILx, &
       get_multi_grad_MAILx, get_multi_flux_MAILx, &
       set_multi_grad_MAILx, set_multi_flux_MAILx, &
       get_multi_vfield_rank_MAILx, get_multi_vfield_max_size_MAILx, &
       set_multi_vfield_MAILx,get_multi_vfield_MAILx, &
       !set_multi_field_MAILx ,&
       comp_cell_area,get_nb_model_MAILx,get_model_MAILx,get_behav_MAILx, &
       ! fd 11062012
       init_nodal_fields_MAILx, init_nodal_field_MAILx,  get_nodal_field_rank_MAILx, &
       set_nodal_field_MAILx, get_nodal_field_nodty_MAILx, &
       get_model_type_MAILx, &
       get_edge_MAILx      , &
       get_blmty_from_nodes_MAILx

!fd new 16/06/09
PUBLIC set_nb_MAILx, &
       set_nb_bulks_MAILx, &
       set_nodes_of_bulk_MAILx, &
       set_model_and_behav_of_bulk_MAILx, &
       set_nb_nodes_MAILx, &
       set_cooref_nodes_MAILx, &
       build_working_arrays_MAILx, &   
       set_nb_tacts_MAILx, &
       set_tact_MAILx, &
       get_periodic_MAILx, &
       set_nb_bulks_of_MAILx, & !-> rm: melimelo
       set_nb_nodes_of_MAILx, &
       set_bulk_of_MAILx, &
       set_node_of_MAILx, &
       get_max_nod2el

!rm new 10/07/12
public clean_memory_MAILx

!!rm for check_TE4xx
!private identity

CONTAINS
!> \addtogroup Mailx 
!>\{
!!!---------------------------------------------
  subroutine read_in_bodies_MAILx(v_maj,v_min)
    implicit none
    integer(kind=4), intent(in) :: v_maj, v_min

    G_nfich=get_io_unit()
    open(unit=G_nfich,file=trim(location(in_bodies(:))))
    call read_bodies(v_maj,v_min)
    close(G_nfich)

  end subroutine read_in_bodies_MAILx
!!!---------------------------------------------
  subroutine write_out_bodies_MAILx(v_maj,v_min)
    implicit none
    integer(kind=4), intent(in) :: v_maj, v_min
    !
    integer(kind=4) :: nfich

    nfich = get_io_unit()
    open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_bodies(:))))    
    call write_bodies(nfich,v_maj,v_min)
    close(nfich)

  end subroutine write_out_bodies_MAILx
!!!---------------------------------------------
  SUBROUTINE write_xxx_gpv_MAILx(which)

    IMPLICIT NONE

    INTEGER :: which,nfich,lc

    nfich = get_io_unit()

    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_gpv)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_gpv(1:lc)))) 
       CALL write_gpv(nfich)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_gpv)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_gpv(1:lc)))) 
       CALL write_gpv(nfich)
       CLOSE(nfich)
    CASE(6)
       CALL write_gpv(6)
    END SELECT

  END SUBROUTINE write_xxx_gpv_MAILx
!!!------------------------------------------------------------------------
  subroutine read_bodies(v_maj,v_min)
    implicit none
    integer(kind=4), intent(in) :: v_maj, v_min
    !
    INTEGER            :: ibdyty,iblmty,inodty,itacty,iccdof,idof,nbdof,inodes
    INTEGER            :: imodel,errare,itest
    CHARACTER(len=103) :: cout
    CHARACTER(len=22)  :: IAM='mod_MAILx::read_bodies'

    ! une liste temporaire pour compter par objet MAILx le nombre 
    ! d'elements auquels appartient un noeud.

    TYPE(G_i_list),DIMENSION(:),POINTER :: t_i_list
    INTEGER                             :: t_i,G_i_length

    integer :: il,nb_lines

    integer :: in,ie,i1,i2

    logical :: forget_it

    ! first reading: sizing array of bodies bdyty  

    ibdyty=0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE            ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest .NE. ifound) CYCLE
       ibdyty = ibdyty + 1
    END DO
    REWIND(G_nfich)

    WRITE(cout,'(I0,1x,A)') ibdyty,'MAILx found'
    CALL LOGMES(cout)
    CALL LOGMES('--')

    nb_MAILx=ibdyty

    ALLOCATE(M_bdyty(nb_MAILx),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating bdyty')
    END IF

    ALLOCATE(t_i_list(nb_MAILx),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating t_i_list')
    END IF

    ! second reading: 
    ! sizing list of bulk elements
    ! sizing list of nodes 
    ! sizing list of contactors

    ibdyty=0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE            ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest .NE. ifound) CYCLE
       ibdyty = ibdyty+1

       iblmty = 0
       inodty = 0
       itacty = 0

       M_bdyty(ibdyty)%is_meca=.FALSE.
       M_bdyty(ibdyty)%is_ther=.FALSE.
       M_bdyty(ibdyty)%is_poro=.FALSE.
       M_bdyty(ibdyty)%is_multi=.FALSE.

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE          ! fishing for the keyword 'blmty'
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_blmty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) iblmty=iblmty+1
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

       IF (iblmty /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M_bdyty%blmty')
          END IF

       ELSE
          NULLIFY(M_bdyty(ibdyty)%blmty)
          WRITE(cout,'(1X,A27,1X,I5)') 'WARNING: MAILx without bulk',ibdyty
          CALL LOGMES(cout)
       END IF

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE          ! fishing for the keyword 'nodty'
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_nodty_MAILx(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) inodty = inodty+1
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)
       errare = 0
       IF (inodty /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M_bdyty%nodty')
          END IF

          ALLOCATE(M_bdyty(ibdyty)%nod2blmty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M_bdyty%nod2blmty')
          END IF

          ALLOCATE(t_i_list(ibdyty)%G_i(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating t_i_list%G_i')
          END IF

          t_i_list(ibdyty)%G_i = 0

       ELSE
          NULLIFY(M_bdyty(ibdyty)%nodty)
          NULLIFY(M_bdyty(ibdyty)%nod2blmty)
          WRITE(cout,'(1X,A28,1X,I5)') 'WARNING: MAILx without nodes',ibdyty
          CALL LOGMES(cout)
       END IF

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'tacty') CYCLE          ! fishing for the keyword 'tacty' 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_tacty(G_clin,ibdyty,v_maj,v_min)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) itacty=itacty+1
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

       IF (itacty /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%tacty(itacty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M_bdyty%tacty')
          END IF
       ELSE 
          NULLIFY(M_bdyty(ibdyty)%tacty)
          WRITE(cout,'(1X,A32,1X,I5)') 'WARNING: MAILx without contactor',ibdyty
          CALL LOGMES(cout)
       END IF
    END DO

    REWIND(G_nfich)   

    ! third reading: filling types

    ibdyty=0

    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE            ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest .NE. ifound) CYCLE
       ibdyty=ibdyty+1

       iblmty = 0
       inodty = 0
       itacty = 0

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE          ! fishing for the keyword 'blmty' 
          DO
             imodel = 0
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_blmty(G_clin,ibdyty,nb_lines)                      
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN
                iblmty=iblmty+1  
                READ(G_clin(2:6),'(A5)') M_bdyty(ibdyty)%blmty(iblmty)%blmID
                DO
                   DO il = 1, nb_lines    ! skipping extra nodes lines
                      IF( .NOT. read_G_clin()) THEN
                         CALL FATERR(IAM,'xxx') 
                      ENDIF
                   ENDDO

                   nb_lines=0

                   IF( .NOT. read_G_clin()) EXIT        ! fishing for the keyword 'model' 
                   IF (G_clin(2:6) /= '     ') EXIT     ! oups! ... one line to                    
                   IF (G_clin(16:20) == 'model') THEN
                      imodel=imodel+1
                   ELSE
                      CALL FATERR(IAM,'error reading models 1')
                   END IF
                END DO
                BACKSPACE(G_nfich)

                IF (imodel /= 0) THEN
                   ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%model(imodel),stat=errare)
                   ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%behav(imodel),stat=errare)
                   IF (errare /= 0) THEN
                      CALL FATERR(IAM,'error allocating M_bdyty%blmty%model')
                   END IF
                ELSE 
                   NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%model)
                   NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%behav)
                   !                                          1234567890123456789012345678
                   WRITE(cout,'(1X,A36,1X,I5)') 'WARNING: MAILx without model & behav',ibdyty
                   CALL LOGMES(cout)
                END IF
             END IF
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE          ! fishing for the keyword 'nodty' 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_nodty_MAILx(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT                      
             IF (itest == ifound) THEN
                inodty=inodty+1
                call new_nodty(M_bdyty(ibdyty)%nodty(inodty),G_clin(2:6)) 
             END IF
             CYCLE
          END DO
          EXIT       
       END DO
       BACKSPACE(G_nfich)   

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'tacty') CYCLE          ! fishing for the keyword 'tacty'
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_tacty(G_clin,ibdyty,v_maj,v_min)      ! permet de passer les "multi" frontières
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT                                            
             IF (itest == ifound) THEN
                itacty=itacty+1
                READ(G_clin(2:6),'(A5)') M_bdyty(ibdyty)%tacty(itacty)%tacID

             END IF
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)   

    END DO

    REWIND(G_nfich)   

    ! concatenate dof to operate bodies node data
    ! first: sizing 

    DO ibdyty=1,SIZE(M_bdyty)

       iccdof=0

       DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)
          iccdof = iccdof + nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))
       END DO

       IF (iccdof /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%nodnb(iccdof),stat=errare)
          ALLOCATE(M_bdyty(ibdyty)%dofnb(iccdof),stat=errare)
          ALLOCATE(M_bdyty(ibdyty)%cooref(iccdof),stat=errare)
          ALLOCATE(M_bdyty(ibdyty)%coor(iccdof),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating X,V,...')
          END IF
       ELSE 
          NULLIFY(M_bdyty(ibdyty)%nodnb)
          NULLIFY(M_bdyty(ibdyty)%dofnb)
          NULLIFY(M_bdyty(ibdyty)%cooref)
          NULLIFY(M_bdyty(ibdyty)%coor)

          !                                    12345678901234567890123456
          WRITE(cout,'(1X,A26,1X,I5)') 'WARNING: MAILx without DOF',ibdyty
          CALL LOGMES(cout)
       END IF

       !    array node -> first global ddl

       IF (SIZE(M_bdyty(ibdyty)%nodty) /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%ccdof(SIZE(M_bdyty(ibdyty)%nodty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating ccdof')
          END IF
       ELSE 
          NULLIFY(M_bdyty(ibdyty)%ccdof)
          !                                    123456789012345678901234567
          WRITE(cout,'(1X,A27,1X,I5)') 'WARNING: MAILx without node',ibdyty
          CALL LOGMES(cout)
       END IF
    END DO

    !  second: filling ordering arrays

    DO ibdyty=1,SIZE(M_bdyty)

       iccdof=0

       DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)
          M_bdyty(ibdyty)%ccdof(inodty)=iccdof
          DO idof=1,nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))
             iccdof=iccdof+1
             M_bdyty(ibdyty)%nodnb(iccdof)=inodty       ! reverse mapping
             M_bdyty(ibdyty)%dofnb(iccdof)=idof         ! reverse mapping
          END DO
       END DO
    END DO

    ! fourth reading: filling in data

    ibdyty=0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE            ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest .NE. ifound) CYCLE
       ibdyty = ibdyty+1

       iblmty = 0
       inodty = 0
       itacty = 0

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE          ! fishing for the keyword 'blmty' 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_blmty(G_clin,ibdyty)                      
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN
                iblmty = iblmty+1  
                READ(G_clin(2:6),'(A5)') M_bdyty(ibdyty)%blmty(iblmty)%blmID
                SELECT CASE(G_clin(2:6))
                CASE('S2xxx')   
                   CALL read_S2xxx(ibdyty,iblmty)
                CASE('S3xxx')   
                   CALL read_S3xxx(ibdyty,iblmty)
                CASE('T3xxx')   
                   CALL read_T3xxx(ibdyty,iblmty)
                CASE('T6xxx')   
                   CALL read_T6xxx(ibdyty,iblmty)
                CASE('Q4xxx')    
                   CALL read_Q4xxx(ibdyty,iblmty)
                CASE('Q8xxx')    
                   CALL read_Q8xxx(ibdyty,iblmty)
                CASE('H8xxx')
                   CALL read_H8xxx(ibdyty,iblmty)
                CASE('H20xx')    
                   CALL read_H20xx(ibdyty,iblmty)
                CASE('TE4xx')    
                   CALL read_TE4xx(ibdyty,iblmty)
                CASE('TE10x')    
                   CALL read_TE10x(ibdyty,iblmty)
                CASE('PRI6x')    
                   CALL read_PRI6x(ibdyty,iblmty)
                CASE('PRI15')    
                   CALL read_PRI15(ibdyty,iblmty)
                CASE default  
                   CALL FATERR(IAM,'bulk element unknown')
                END SELECT

                DO inodes=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%NODES)
                   t_i_list(ibdyty)%G_i(M_bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)) =    &
                        t_i_list(ibdyty)%G_i(M_bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)) + 1
                END DO

                imodel = 0

                DO
                   IF( .NOT. read_G_clin()) EXIT        ! fishing for the keyword 'model' 
                   IF (G_clin(2:6) /= '     ') EXIT     ! oups! ... one line to 

                   IF (G_clin(16:20) == 'model') THEN
                      imodel=imodel+1
                      READ(G_clin(23:27),'(A5)')  M_bdyty(ibdyty)%blmty(iblmty)%model(imodel)
                      READ(G_clin(37:41),'(A5)')  M_bdyty(ibdyty)%blmty(iblmty)%behav(imodel)
                   ELSE
                      CALL FATERR(IAM,'error reading models 2')
                   END IF
                END DO
                BACKSPACE(G_nfich)

                M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present=.FALSE. 
                NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv)

                M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present=.FALSE. 
                NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv)


                M_bdyty(ibdyty)%blmty(iblmty)%is_thmc_present=.FALSE. 
                NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv)
                
                M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present=.FALSE. 
                NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca)
                NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther)

                M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present=.FALSE. 
                NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv)

             END IF
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE          ! fishing for the keyword 'nodty' 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest = itest_nodty_MAILx(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT                      
             IF (itest == ifound) THEN

                inodty = inodty+1

                nbdof  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))
                iccdof = M_bdyty(ibdyty)%ccdof(inodty)

                CALL read_a_nodty(G_clin,M_bdyty(ibdyty)%cooref(iccdof+1:iccdof+nbdof))

                M_bdyty(ibdyty)%coor(iccdof+1:iccdof+nbdof) = M_bdyty(ibdyty)%cooref(iccdof+1:iccdof+nbdof)

             END IF
             CYCLE
          END DO
          EXIT       
       END DO
       BACKSPACE(G_nfich)   

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'tacty') CYCLE          ! fishing for the keyword 'tacty'
          DO
             IF( .NOT. read_G_clin()) EXIT

             itest = itest_tacty(G_clin,ibdyty,v_maj,v_min)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT                                            
             IF (itest == ifound) THEN
                itacty = itacty+1
                if( v_maj < 3 .and. v_min < 1 ) then
                  if( G_clin(2:6)=='CSpx3' .or. G_clin(2:6)=='CSpx4' .or. &
                      G_clin(2:6)=='CSpx6' .or. G_clin(2:6)=='CSpx8' ) then
                    M_bdyty(ibdyty)%tacty(itacty)%tacID = 'CSpxx'
                  else if( G_clin(2:6)=='ASpx3' .or. G_clin(2:6)=='ASpx4' ) then
                    M_bdyty(ibdyty)%tacty(itacty)%tacID = 'ASpxx'
                  else
                    read(G_clin( 2: 6),'(A5)')M_bdyty(ibdyty)%tacty(itacty)%tacID
                  end if
                else if( v_maj < 3 .and. v_min < 2 ) then
                  read(G_clin( 2: 6),'(A5)')M_bdyty(ibdyty)%tacty(itacty)%tacID
                else
                  call faterr(IAM,'Reading with future version format')
                end if
                READ(G_clin(23:27),'(A5)')M_bdyty(ibdyty)%tacty(itacty)%color

                M_bdyty(ibdyty)%tacty(itacty)%is_periodic = .false.
                nullify(M_bdyty(ibdyty)%tacty(itacty)%per_vec)

                if( v_maj < 3 .and. v_min < 1 ) then
                  select case(G_clin(2:6))
                  case('CLxxx')
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))
                     call read_BDARY_CLxxx(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                           M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata,G_clin)

                  case('ALpxx')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
                     call read_BDARY_ALpxx(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)

                  case('PT2DL','PT2TL')
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))
                     call read_BDARY_PT2DL(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                           M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata,G_clin)
                     
                  case('DISKL')
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(3))
                     call read_BDARY_DISKL(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                           M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata,G_clin)

                  case('CSpx3','CSpx4','CSpx6','CSpx8')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                             M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
                     call read_BDARY_xSpxx('C',M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata,v_maj,v_min)

                  case('ASpx3','ASpx4','ASpx6','ASpx8')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                             M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
                     call read_BDARY_xSpxx('A',M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata,v_maj,v_min)

                  case('POLYD')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                             M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
                     call read_BDARY_POLYD(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)  
                  end select
                  
                else if( v_maj < 3 .and. v_min < 2 ) then
                  select case(G_clin(2:6))
                  case('CLxxx')
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))
                     call read_BDARY_CLxxx(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                           M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata,G_clin)

                  case('ALpxx')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
                     call read_BDARY_ALpxx(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)

                  case('PT2DL','PT2TL')
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))
                     call read_BDARY_PT2DL(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                           M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata,G_clin)

                  case('DISKL')
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
                     allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(3))
                     call read_BDARY_DISKL(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                           M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata,G_clin)

                  case('CSpxx','CSpx0','CSpx1','CSpx2')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                             M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
                     call read_BDARY_xSpxx('C',M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata,v_maj,v_min)

                  case('ASpxx')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                             M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
                     call read_BDARY_xSpxx('A',M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata,v_maj,v_min)

                  case('POLYD')
                     nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                             M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
                     call read_BDARY_POLYD(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)  
                  end select
                else
                  call faterr(IAM,'Reading with future version format')
                end if

             END IF
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

    END DO

    do ibdyty = 1, nb_MAILx
      nullify(M_bdyty(ibdyty)%nodal_fields)
    end do

    !fd on test les elements

    DO ibdyty=1,nb_MAILx
       
       DO iblmty=1,size(M_bdyty(ibdyty)%blmty)
         SELECT CASE(M_bdyty(ibdyty)%blmty(iblmty)%blmID)
         CASE('TE4xx')    
            CALL check_TE4xx(ibdyty,iblmty)
         CASE DEFAULT
         END SELECT
       ENDDO
    ENDDO    

    ! calcul de la liste des elements contenant un noeud

    DO ibdyty=1,nb_MAILx
       M_bdyty(ibdyty)%max_nod2el = 0
       DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)

          G_i_length = t_i_list(ibdyty)%G_i(inodty)
          M_bdyty(ibdyty)%max_nod2el = max(M_bdyty(ibdyty)%max_nod2el,G_i_length)

          IF (G_i_length /= 0) THEN
             ALLOCATE(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i(G_i_length),stat=errare)
             IF (errare /= 0) THEN
                CALL FATERR(IAM,'error allocating M_bdyty%nod2blmty%G_i')
             END IF
          ELSE 
             NULLIFY(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
             WRITE(cout,'(1X,A29,1X,I0,1x,A,1x,I0)') 'WARNING: Node without element', inodty,' body ',ibdyty
             CALL LOGMES(cout)
          END IF
       END DO

       ! on a besoin de la liste pour indexer

       DO t_i=1,SIZE(M_bdyty(ibdyty)%nodty)
          t_i_list(ibdyty)%G_i(t_i)=0         
       END DO

       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
          DO inodes=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%NODES)
             t_i= M_bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)

             t_i_list(ibdyty)%G_i(t_i)=t_i_list(ibdyty)%G_i(t_i)+1 
             M_bdyty(ibdyty)%nod2blmty(t_i)%G_i(t_i_list(ibdyty)%G_i(t_i))=iblmty      
          END DO
       END DO
    END DO

    if( associated(t_i_list) ) then
      do ibdyty = 1, nb_MAILx
        if( associated(t_i_list(ibdyty)%G_i) ) deallocate(t_i_list(ibdyty)%G_i)
      end do
      deallocate(t_i_list)
    end if

    ! calcul de la liste des elements adjacents a un element
  
    !TODO basculer cette merde dans DiscreteGeometry


    allocate(t_i_list(nb_MAILx))
    
    DO ibdyty=1,nb_MAILx

       forget_it=.TRUE.

       allocate(t_i_list(ibdyty)%G_i(SIZE(M_bdyty(ibdyty)%blmty)))
       t_i_list(ibdyty)%G_i = 0

       ! on compte
       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
         SELECT CASE(M_bdyty(ibdyty)%blmty(iblmty)%blmID)
         CASE('S2xxx')
            ! pas de bord
            ! t_i_list(ibdyty)%G_i(iblmty) = t_i_list(ibdyty)%G_i(iblmty) + 0
         CASE('T3xxx','T6xxx')
            forget_it = .FALSE.
            do in=1,3 
              i1 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(in)
              i2 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(modulo(in,3)+1)
              do ie=1,size(M_bdyty(ibdyty)%nod2blmty(i1)%G_i)
                if (M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie) == iblmty) cycle
                if (count(M_bdyty(ibdyty)%nod2blmty(i2)%G_i == M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie)) == 0) cycle
                t_i_list(ibdyty)%G_i(iblmty) = t_i_list(ibdyty)%G_i(iblmty) + 1
                exit
              enddo
            end do
         CASE('Q4xxx','Q8xxx')
            forget_it = .FALSE.
            do in=1,4 
              i1 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(in)
              i2 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(modulo(in,4)+1)
              do ie=1,size(M_bdyty(ibdyty)%nod2blmty(i1)%G_i)
                if (M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie) == iblmty) cycle
                if (count(M_bdyty(ibdyty)%nod2blmty(i2)%G_i == M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie)) == 0) cycle
                t_i_list(ibdyty)%G_i(iblmty) = t_i_list(ibdyty)%G_i(iblmty) + 1
                exit
              enddo
            end do
         CASE('H8xxx','H20xx')

         CASE('TE4xx','TE10x')

         CASE('PRI6x')

         end select

       END DO

       if (forget_it) cycle

       ALLOCATE(M_bdyty(ibdyty)%ele2blmty(SIZE(M_bdyty(ibdyty)%blmty)))

       ! on dimensionne
       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)

          G_i_length = t_i_list(ibdyty)%G_i(iblmty)

          IF (G_i_length /= 0) THEN
             ALLOCATE(M_bdyty(ibdyty)%ele2blmty(iblmty)%G_i(G_i_length),stat=errare)
             IF (errare /= 0) THEN
                CALL FATERR(IAM,'error allocating M_bdyty%ele2blmty%G_i')
             END IF
          ELSE 
             NULLIFY(M_bdyty(ibdyty)%ele2blmty(iblmty)%G_i)
             if (SIZE(M_bdyty(ibdyty)%blmty) > 1) then
               WRITE(cout,'(1X,A40,1X,I7,1x,A,1x,I0)') 'WARNING: blmty without neighbor elements',iblmty,' body ',ibdyty
               CALL LOGMES(cout)
             endif  
          END IF

          t_i_list(ibdyty)%G_i(iblmty) = 0

       END DO

       ! on rempli les listes d ele voisins a un ele et a un noeud
       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
         SELECT CASE(M_bdyty(ibdyty)%blmty(iblmty)%blmID)
         CASE('S2xxx')
            ! pas de bord
            ! t_i_list(ibdyty)%G_i(iblmty) = t_i_list(ibdyty)%G_i(iblmty) + 0
         CASE('T3xxx','T6xxx')
            do in=1,3 
              i1 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(in)
              i2 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(modulo(in,3)+1)
              do ie=1,size(M_bdyty(ibdyty)%nod2blmty(i1)%G_i)
                if (M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie) == iblmty) cycle
                if (count(M_bdyty(ibdyty)%nod2blmty(i2)%G_i == M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie)) == 0) cycle
                t_i_list(ibdyty)%G_i(iblmty) = t_i_list(ibdyty)%G_i(iblmty) + 1
                M_bdyty(ibdyty)%ele2blmty(iblmty)%G_i(t_i_list(ibdyty)%G_i(iblmty))=M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie)
                exit
              enddo
            end do
         CASE('Q4xxx','Q8xxx')
            do in=1,3 
              i1 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(in)
              i2 = M_bdyty(ibdyty)%blmty(iblmty)%nodes(modulo(in,4)+1)
              do ie=1,size(M_bdyty(ibdyty)%nod2blmty(i1)%G_i)
                if (M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie) == iblmty) cycle
                if (count(M_bdyty(ibdyty)%nod2blmty(i2)%G_i == M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie)) == 0) cycle
                t_i_list(ibdyty)%G_i(iblmty) = t_i_list(ibdyty)%G_i(iblmty) + 1
                M_bdyty(ibdyty)%ele2blmty(iblmty)%G_i(t_i_list(ibdyty)%G_i(iblmty))=M_bdyty(ibdyty)%nod2blmty(i1)%G_i(ie)
                exit
              enddo
            end do
         CASE('H8xxx','H20xx')

         CASE('TE4xx','TE10x')

         CASE('PRI6x')

         end select

       END DO


       allocate(M_bdyty(ibdyty)%boundary_elements(SIZE(M_bdyty(ibdyty)%blmty)))

       ! on compte combien l element a de bord libre  
       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)

          M_bdyty(ibdyty)%boundary_elements(iblmty) = -1

         SELECT CASE(M_bdyty(ibdyty)%blmty(iblmty)%blmID)
         CASE('S2xxx')
           M_bdyty(ibdyty)%boundary_elements(iblmty) = 0
         CASE('T3xxx','T6xxx')
           M_bdyty(ibdyty)%boundary_elements(iblmty) = 3 - size(M_bdyty(ibdyty)%ele2blmty(iblmty)%G_i)
         CASE('Q4xxx','Q8xxx')
           M_bdyty(ibdyty)%boundary_elements(iblmty) = 4 - size(M_bdyty(ibdyty)%ele2blmty(iblmty)%G_i)
         CASE('H8xxx','H20xx')

         CASE('TE4xx','TE10x')

         CASE('PRI6x')

         end select

       END DO


    END DO

    if( associated(t_i_list) ) then
      do ibdyty = 1, nb_MAILx
        if( associated(t_i_list(ibdyty)%G_i) ) deallocate(t_i_list(ibdyty)%G_i)
      end do
      deallocate(t_i_list)
    end if

  end subroutine read_bodies
!!!------------------------------------------------------------------------ 
  subroutine write_bodies(nfich,v_maj,v_min)
    implicit none
    integer(kind=4), intent(in) :: nfich, v_maj, v_min
    !
    INTEGER             :: ibdyty,iblmty,imodel,inodty,itacty,nbdof,iccdof
    CHARACTER(len=103)  :: cout
    CHARACTER(len=19)   :: IAM='MAILx::write_bodies'

    DO ibdyty=1,SIZE(M_bdyty)

       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101) 'MAILx',ibdyty
       WRITE(nfich,'(A6)') '$blmty'

       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
          SELECT CASE(M_bdyty(ibdyty)%blmty(iblmty)%blmID)
          CASE('S2xxx')
             CALL write_S2xxx(ibdyty,iblmty,nfich)
          CASE('S3xxx')
             CALL write_S3xxx(ibdyty,iblmty,nfich)   
          CASE('T3xxx')
             CALL write_T3xxx(ibdyty,iblmty,nfich)                   
          CASE('T6xxx')
             CALL write_T6xxx(ibdyty,iblmty,nfich)                   
          CASE('Q4xxx')
             CALL write_Q4xxx(ibdyty,iblmty,nfich)                   
          CASE('Q8xxx')
             CALL write_Q8xxx(ibdyty,iblmty,nfich)                   
          CASE('H8xxx')
             CALL write_H8xxx(ibdyty,iblmty,nfich)                   
          CASE('H20xx')
             CALL write_H20xx(ibdyty,iblmty,nfich)                   
          CASE('TE4xx')
             CALL write_TE4xx(ibdyty,iblmty,nfich)                   
          CASE('TE10x')
             CALL write_TE10x(ibdyty,iblmty,nfich)                   
          CASE('PRI6x')
             CALL write_PRI6x(ibdyty,iblmty,nfich)                   
          CASE('PRI15')
             CALL write_PRI15(ibdyty,iblmty,nfich)                   
          CASE default  
             WRITE(cout,'(A7,I5,A7,A5,A8)') ' bdyty ',ibdyty,' blmty ',M_bdyty(ibdyty)%blmty(iblmty)%blmID,' unknown'
             CALL FATERR(IAM,cout)
          END SELECT
          DO imodel=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%model)
             WRITE(nfich,102) M_bdyty(ibdyty)%blmty(iblmty)%model(imodel),M_bdyty(ibdyty)%blmty(iblmty)%behav(imodel)
102          FORMAT(15x,'model',2x,A5,2x,'behav',2x,A5)
          END DO
       END DO

       WRITE(nfich,'(A6)') '$nodty'

       DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)

          nbdof  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))
          iccdof = M_bdyty(ibdyty)%ccdof(inodty)

          CALL write_a_nodty(get_nodNAME(M_bdyty(ibdyty)%nodty(inodty)),inodty,    &
               M_bdyty(ibdyty)%cooref(iccdof+1:iccdof+nbdof), &
               'coo',nfich)
       END DO

       WRITE(nfich,'(A6)') '$tacty'
       IF (ASSOCIATED(M_bdyty(ibdyty)%tacty)) THEN

          DO itacty=1,SIZE(M_bdyty(ibdyty)%tacty) 
             SELECT CASE(M_bdyty(ibdyty)%tacty(itacty)%tacID)
             CASE('CLxxx')
                CALL write_BDARY_CLxxx(nfich,itacty,&
                     M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     M_bdyty(ibdyty)%tacty(itacty)%color, &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))

             CASE('ALpxx')
                CALL write_BDARY_ALpxx(nfich,itacty,&
                     M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     M_bdyty(ibdyty)%tacty(itacty)%color, &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)

             CASE('PT2DL','PT2TL')
                CALL write_BDARY_PT2DL(nfich,itacty,&
                     M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     M_bdyty(ibdyty)%tacty(itacty)%color, &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))

             CASE('DISKL')
                CALL write_BDARY_DISKL(nfich,itacty,&
                     M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     M_bdyty(ibdyty)%tacty(itacty)%color, &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(2), &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(3))

             CASE('CSpxx','CSpx0','CSpx1','CSpx2','ASpxx')
                CALL write_BDARY_xSpxx(nfich,itacty,&
                     M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     M_bdyty(ibdyty)%tacty(itacty)%color, &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata,v_maj,v_min)
             CASE('POLYD')
                CALL write_BDARY_POLYD(nfich,itacty,&
                     M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     M_bdyty(ibdyty)%tacty(itacty)%color, &
                     M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
             CASE default
                WRITE(*,'(A6,A5,A36)') &
                     'tacty ',M_bdyty(ibdyty)%tacty(itacty)%tacID, &
                     ' unknown in write_bodies, mod_MAILx'
             END SELECT
          END DO
       END IF
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
    END DO
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 

101 FORMAT(1X,A5,I7)            

  END SUBROUTINE write_bodies
!!!------------------------------------------------------------------------ 
  SUBROUTINE write_gpv(nfich)

    IMPLICIT NONE

    INTEGER            :: nfich,ibdyty,iblmty,ig,nb_gp,i_multi
    CHARACTER(len=103) :: cout
    CHARACTER(len=19)  :: IAM='MAILx::write_gpv'

    DO ibdyty=1,SIZE(M_bdyty)

       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101) 'MAILx',ibdyty
       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
          WRITE(nfich,'(A6)') '$blmty'
          WRITE(nfich,101) M_bdyty(ibdyty)%blmty(iblmty)%blmID,iblmty
          IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv)) THEN
             WRITE(nfich,'(A6)') '$model'
             WRITE(nfich,'(A6)') ' MECAx'
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv,dim=1)

                WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress       

                WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain 

                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal)) &
                     WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal
             END DO
          END IF

          IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv)) THEN
             WRITE(nfich,'(A6)') '$model'
             WRITE(nfich,'(A6)') ' THERM'
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv,dim=1)
                WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%T
                if (associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad)) &
                     WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad       
                if (associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad)) & 
                     WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux 
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal)) &
                     WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal
             END DO
          END IF

          IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv)) THEN
             !123456789012345678901234567890123456789012345678901234567890123456789012
             WRITE(nfich,'(A6)') '$model                                                                  '  
             WRITE(nfich,'(A6)') ' THMCx                                                                  '  
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv,dim=1)
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad)) THEN
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad       
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux 
                ENDIF
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal)) THEN
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal
                ENDIF
             ENDDO
          ENDIF
          
           IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca)) THEN
             !123456789012345678901234567890123456789012345678901234567890123456789012
             WRITE(nfich,'(A6)') '$model                                                                  '  
             WRITE(nfich,'(A6)') ' POROx                                                                  '  
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca,dim=1)
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress)) THEN
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress     
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain
                ENDIF
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal)) &
                     WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal
             ENDDO
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther,dim=1)
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad)) THEN
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad     
                   WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux
                ENDIF
                IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal)) &
                     WRITE(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal
             ENDDO
          ENDIF
          
          !if (associated(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpvs)) then
          !  !123456789012345678901234567890123456789012345678901234567890123456789012
          !  write(nfich,'(A6,66x)') '$model'  
          !  write(nfich,'(A6,66x)') ' MULTI'  
          !  do ig = 1, size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv,dim=1)
          !    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(ig,1)%grad)) then
          !       write(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(ig,1)%grad
          !       write(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(ig,1)%flux
          !    end if
          !    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(ig,1)%internal)) &
          !         write(nfich,'(9D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(ig,1)%internal
          !  end do
          !end if

       END DO
       WRITE(nfich,'(A6)')'$$$$$$'
    END DO
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 

101 FORMAT(1X,A5,I7)            

  END SUBROUTINE write_gpv
!!!------------------------------------------------------------------------   
  FUNCTION get_MAILx(ibdyty)

    IMPLICIT NONE 

    INTEGER  :: ibdyty
    TYPE(T_MAILx) :: get_MAILx

    get_MAILx = M_bdyty(ibdyty)

  END FUNCTION get_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_MAILx(fantome)

    IMPLICIT NONE

    INTEGER,OPTIONAL :: fantome

    get_nb_MAILx = 0
    IF (ALLOCATED(M_bdyty)) get_nb_MAILx = SIZE(M_bdyty)

  END FUNCTION get_nb_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_bdyty_MAILx(clin)

    IMPLICIT NONE

    CHARACTER(len=103) :: clin

    SELECT CASE(clin(2:6))
    CASE('MAILx')
       itest_bdyty_MAILx = ifound
    CASE('$$$$$')
       itest_bdyty_MAILx = inomor
    CASE default
       itest_bdyty_MAILx = isskip
    END SELECT

  END FUNCTION itest_bdyty_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_blmty(clin,ibdyty,nb_lines)

    IMPLICIT NONE

    INTEGER            :: ibdyty    
    CHARACTER(len=103) :: clin

    INTEGER, OPTIONAL  :: nb_lines
    INTEGER :: nbl 
    character(len=80)  :: cout
    nbl=0
    SELECT CASE(clin(2:6))
    CASE('S2xxx','S3xxx','T3xxx','T6xxx','Q4xxx','Q8xxx','H8xxx','H20xx','TE4xx','TE10x','PRI6x','PRI15')
       itest_blmty = ifound
       if (clin(2:6) == 'H20xx') then
         nbl=2
       else if (clin(2:6) == 'TE10x') then
         nbl=1
       else if (clin(2:6) == 'PRI15') then
         nbl=1
       end if
    CASE('     ')
       itest_blmty = isskip
       ! derriere un blmty maille on a forcement un nodty !
    CASE('nodty')
       itest_blmty = inomor
    CASE default
       write(cout,'(A7,A5,A18,I5)')' blmty ',clin(2:6),' unknown in MAILx ', ibdyty
       call faterr('MAILx::itest_blmty',cout)
    END SELECT

    IF (PRESENT(nb_lines)) then
       nb_lines=nbl
       !        print*,'H20xx'
    ENDIF

  END FUNCTION itest_blmty
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_nodty_MAILx(clin,ibdyty)

    IMPLICIT NONE

    INTEGER            :: ibdyty    
    CHARACTER(len=103) :: clin
    character(len=80)  :: cout

    IF (is_a_nodty(clin(2:6))) THEN
       itest_nodty_MAILx = ifound
       RETURN
    END IF

    SELECT CASE(clin(2:6))
    CASE('     ')
       itest_nodty_MAILx = isskip
    CASE('tacty','$$$$$')
       itest_nodty_MAILx = inomor
    CASE default
       write(cout,'(A7,A5,A18,I5,A28)')' nodty ',clin(2:6),' unknown in MAILx ', ibdyty
       call faterr('MAILx::itest_nodty',cout)
    END SELECT

  END FUNCTION itest_nodty_MAILx
!!!------------------------------------------------------------------------
  integer function itest_tacty(clin,ibdyty,v_maj,v_min)
    implicit none
    integer(kind=4)   , intent(in) :: v_maj, v_min
    integer(kind=4)   , intent(in) :: ibdyty
    character(len=103), intent(in) :: clin
    !
    character(len=80)  :: cout

    !fd pour les tacty sur plusieurs lignes

    IF (clin(2:6) == '     ') THEN
       itest_tacty = isskip
       RETURN
    END IF

    if( v_maj < 3 .and. v_min < 1 ) then

      if ( is_explode_patch_ASpxx() ) then
        if (clin(1:6) == '+ALpxx'                            .or. &
            clin(1:6) == '+CSpx3' .or. clin(1:6) == '+CSpx4' .or. &
            clin(1:6) == '+CSpx6' .or. clin(1:6) == '+CSpx8' ) then
           itest_tacty = isskip
           return
        end if
      else
        if (clin(1:6) == '+ALpxx'                            .or. &
            clin(1:6) == '+CSpx3' .or. clin(1:6) == '+CSpx4' .or. &
            clin(1:6) == '+CSpx6' .or. clin(1:6) == '+CSpx8' .or. &
            clin(1:6) == '+ASpx3' .or. clin(1:6) == '+ASpx4' .or. &
            clin(1:6) == '+ASpx6' .or. clin(1:6) == '+ASpx8') then
           itest_tacty = isskip
           return
        end if
      end if

      select case(clin(2:6))
      case('CL1xx','AL2xx') 
         itest_tacty = isskip
      case('CLxxx','ALpxx','PT2DL','PT2TL','DISKL','CSpx3','CSpx4','CSpx6','CSpx8','ASpx3','ASpx4','ASpx6','ASpx8','POLYD') 
         itest_tacty = ifound
      case('JONCx','DISKx','POLYG') !manque d'autres non ?
         itest_tacty = isskip
      case('$$$$$')
         itest_tacty = inomor
      case default
         write(cout,'(A7,A5,A18,I5,A28)')' tacty ',clin(2:6),' unknown in MAILx ', ibdyty
         call faterr('MAILx::itest_tacty',cout)
      end select

    else if( v_maj < 3 .and. v_min < 2 ) then

      if ( is_explode_patch_ASpxx() ) then
        if (clin(1:6) == '+ALpxx'                            .or. &
            clin(1:6) == '+CSpxx' .or. clin(1:6) == '+CSpx0' .or. &
            clin(1:6) == '+CSpx1' .or. clin(1:6) == '+CSpx2' ) then
           itest_tacty = isskip
           return
        end if
      else
        if (clin(1:6) == '+ALpxx'                            .or. &
            clin(1:6) == '+CSpxx' .or. clin(1:6) == '+CSpx0' .or. &
            clin(1:6) == '+CSpx1' .or. clin(1:6) == '+CSpx2' .or. &
            clin(1:6) == '+ASpxx'                           ) then
           itest_tacty = isskip
           return
        end if
      end if

      select case(clin(2:6))
      case('CL1xx','AL2xx') 
         itest_tacty = isskip
      case('CLxxx','ALpxx','PT2DL','PT2TL','DISKL','CSpxx','CSpx0','CSpx1','CSpx2','ASpxx','POLYD') 
         itest_tacty = ifound
      case('JONCx','DISKx','POLYG') !manque d'autres non ?
         itest_tacty = isskip
      case('$$$$$')
         itest_tacty = inomor
      case default
         write(cout,'(A7,A5,A18,I5,A28)')' tacty ',clin(2:6),' unknown in MAILx ', ibdyty
         call faterr('MAILx::itest_tacty',cout)
      end select

    else
      call faterr('MAILx::itest_tacty','Reading with future version format')
    end if

  end function itest_tacty
!!!------------------------------------------------------------------------
  SUBROUTINE read_S3xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_S3xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(3),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF
    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)

    RETURN

  END SUBROUTINE read_S3xxx
!!!------------------------------------------------------------------------
  SUBROUTINE read_S2xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_S2xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(2),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF
    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)

    RETURN

  END SUBROUTINE read_S2xxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_S2xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(2) :: list

    list=M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' S2xxx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_S2xxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_S3xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(3) :: list

    list=M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' S3xxx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_S3xxx
!!!------------------------------------------------------------------------
  SUBROUTINE read_T3xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_T3xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(3),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF
    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)

    RETURN

  END SUBROUTINE read_T3xxx
  
!!!------------------------------------------------------------------------
  SUBROUTINE read_T6xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_T6xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(6),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF
    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
    READ(G_clin(42:48),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(4)
    READ(G_clin(49:55),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(5)
    READ(G_clin(56:62),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(6)

    RETURN

  END SUBROUTINE read_T6xxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_T3xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(3) :: list

    list=M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' T3xxx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_T3xxx
  
!!!------------------------------------------------------------------------
  SUBROUTINE write_T6xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(6) :: list

    list=M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' T6xxx',I7,2x,'nodes',6(I7))

    RETURN

  END SUBROUTINE write_T6xxx
!!!------------------------------------------------------------------------
  SUBROUTINE read_Q4xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER              :: errare,ibdyty,iblmty,k
    CHARACTER(len=21)    :: IAM='mod_MAILx::read_Q4xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(4),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
    READ(G_clin(42:48),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(4)

    RETURN

  END SUBROUTINE read_Q4xxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_Q4xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(4) :: list

    list=M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list 
10  FORMAT(' Q4xxx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_Q4xxx
!!!------------------------------------------------------------------------
  SUBROUTINE read_Q8xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_Q8xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(8),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
    READ(G_clin(42:48),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(4)
    READ(G_clin(49:55),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(5)
    READ(G_clin(56:62),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(6)
    READ(G_clin(63:69),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(7)
    READ(G_clin(70:76),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(8)

    RETURN

  END SUBROUTINE read_Q8xxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_Q8xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(8) :: list

    list=M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' Q8xxx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_Q8xxx
!!!------------------------------------------------------------------------
  SUBROUTINE read_H8xxx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER               :: errare,ibdyty,iblmty,k
    CHARACTER(len=21)    :: IAM='mod_MAILx::read_H8xxx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(8),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
    READ(G_clin(42:48),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(4)
    READ(G_clin(49:55),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(5)
    READ(G_clin(56:62),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(6)
    READ(G_clin(63:69),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(7)
    READ(G_clin(70:76),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(8)

    RETURN

  END SUBROUTINE read_H8xxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_H8xxx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(8) :: list

    list = M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' H8xxx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_H8xxx
!!!------------------------------------------------------------------------
  SUBROUTINE read_H20xx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_H20xx'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(20),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:103),'(8I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=1,8) 
    IF( .NOT. read_G_clin()) THEN 
       CALL FATERR(IAM,'end of file')
    END IF
    READ(G_clin(21:103),'(8I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=9,16) 
    IF( .NOT. read_G_clin()) THEN 
       CALL FATERR(IAM,'end of file')
    END IF
    READ(G_clin(21:103),'(4I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=17,20) 

    RETURN

  END SUBROUTINE read_H20xx
!!!------------------------------------------------------------------------
  SUBROUTINE write_H20xx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k

    WRITE(nfich,10) iblmty,(M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=1,8) 
10  FORMAT(' H20xx',I7,2x,'nodes',8(I7))
    WRITE(nfich,11) (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=9,16)
    WRITE(nfich,11) (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=17,20)
11  FORMAT('      ',7x,2x,5x     ,8(I7))

    RETURN

  END SUBROUTINE write_H20xx
!!!------------------------------------------------------------------------
  SUBROUTINE read_TE4xx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER               :: ibdyty,iblmty

    ! ***
    CHARACTER(len=21)    :: IAM='mod_MAILx::read_TE4xx'
    real(kind=8),dimension(3,4) :: e_coor

    integer :: errare

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(4),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
    READ(G_clin(42:48),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(4)

  END SUBROUTINE read_TE4xx
!!!------------------------------------------------------------------------
  SUBROUTINE check_TE4xx(ibdyty,iblmty)

    use a_EF

    IMPLICIT NONE

    INTEGER               :: ibdyty,iblmty

    ! ***
    CHARACTER(len=21)    :: IAM='mod_MAILx::check_TE4xx'
    real(kind=8),dimension(3,4) :: e_coor
    integer :: k,inodty,iccdof
    real(kind=8) :: vol,tmp(3)
    character(len=80) :: cout

    do k=1,4    
      inodty=M_bdyty(ibdyty)%blmty(iblmty)%NODES(k)
      iccdof = M_bdyty(ibdyty)%ccdof(inodty)
      e_coor(1:3,k) = M_bdyty(ibdyty)%cooref(iccdof+1:iccdof+3)
    enddo

    !print*,ibdyty,iblmty
    !print*,M_bdyty(ibdyty)%blmty(iblmty)%NODES
    !write(*,'(3(1x,D12.5))') e_coor

    call compute_element_size(4,i_TEP1,i_TE04,nbdime,.FALSE.,e_coor,vol)

    if (vol < 0.d0) then
      print*,'volume=',vol

      tmp(1:3) = e_coor(1:3,2)
      e_coor(1:3,2) = e_coor(1:3,3)
      e_coor(1:3,3) = tmp(1:3)

      call compute_element_size(4,i_TEP1,i_TE04,nbdime,.FALSE.,e_coor,vol) 

      if (vol > 0.d0) then  

        print*,'flip'   
        inodty=M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
        M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)=M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
        M_bdyty(ibdyty)%blmty(iblmty)%NODES(3) = inodty
      else

        write(cout,'(A,I0,20(1x,I0))') 'element ',iblmty,M_bdyty(ibdyty)%blmty(iblmty)%NODES
        call logmes(cout)  
        call FATERR(IAM,'wrong orientation of TE4')

      endif

    endif

    RETURN

  END SUBROUTINE check_TE4xx

!!!------------------------------------------------------------------------
  SUBROUTINE write_TE4xx(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(4) :: list

    list = M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' TE4xx',I7,2x,'nodes',8(I7))

    RETURN

  END SUBROUTINE write_TE4xx
!!!------------------------------------------------------------------------
  SUBROUTINE read_TE10x(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER           :: errare,ibdyty,iblmty,k
    CHARACTER(len=21) :: IAM='mod_MAILx::read_TE10x'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(10),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:103),'(8I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=1,8) 
    IF( .NOT. read_G_clin()) THEN 
       CALL FATERR(IAM,'end of file')
    END IF
    READ(G_clin(21:103),'(2I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=9,10) 
    !IF( .NOT. read_G_clin()) THEN 
    !   CALL FATERR(IAM,'end of file')
    !END IF

    RETURN

  END SUBROUTINE read_TE10x
!!!------------------------------------------------------------------------
  SUBROUTINE write_TE10x(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k

    WRITE(nfich,10) iblmty,(M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=1,8) 
10  FORMAT(' TE10x',I7,2x,'nodes',8(I7))
    WRITE(nfich,11) (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=9,10)

11  FORMAT('      ',7x,2x,5x     ,8(I7))

    RETURN

  END SUBROUTINE write_TE10x
!!!------------------------------------------------------------------------
  SUBROUTINE read_PRI6x(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER               :: errare,ibdyty,iblmty,k
    CHARACTER(len=21)    :: IAM='mod_MAILx::read_PRI6x'

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%NODES(6),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating NODES')
    END IF

    READ(G_clin(21:27),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(1)
    READ(G_clin(28:34),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(2)
    READ(G_clin(35:41),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(3)
    READ(G_clin(42:48),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(4)
    READ(G_clin(49:55),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(5)
    READ(G_clin(56:62),'(I7)') M_bdyty(ibdyty)%blmty(iblmty)%NODES(6)

    RETURN

  END SUBROUTINE read_PRI6x
!!!------------------------------------------------------------------------
  SUBROUTINE write_PRI6x(ibdyty,iblmty,nfich)

    IMPLICIT NONE

    INTEGER              :: nfich,ibdyty,iblmty,k
    INTEGER,DIMENSION(6) :: list

    list = M_bdyty(ibdyty)%blmty(iblmty)%NODES

    WRITE(nfich,10) iblmty,list
10  FORMAT(' PRI6x',I7,2x,'nodes',6(I7))

    RETURN

  END SUBROUTINE write_PRI6x
!!!------------------------------------------------------------------------
  subroutine read_PRI15(ibdyty,iblmty)

    implicit none

    integer(kind=4)       :: errare,ibdyty,iblmty,k
    character(len=21)    :: IAM='mod_MAILx::read_PRI15'

    allocate(M_bdyty(ibdyty)%blmty(iblmty)%NODES(15),stat=errare)
    if (errare /= 0) then
      call faterr(iam,'error allocating NODES')
    end if

    read(G_clin(21:103),'(8I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=1,8) 
    if( .not. read_G_clin()) then 
      call faterr(IAM,'end of file')
    end if
    read(G_clin(21:95),'(7I7)') (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=9,15)

  end subroutine read_PRI15
!!!------------------------------------------------------------------------
  subroutine write_PRI15(ibdyty,iblmty,nfich)
    implicit none

    integer(kind=4)                :: nfich,ibdyty,iblmty,k
    integer(kind=4), dimension(15) :: list

    list = M_bdyty(ibdyty)%blmty(iblmty)%NODES

    write(nfich,10) iblmty,(M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=1,8)
10  format(' PRI15',I7,2x,'nodes',8(I7))
    write(nfich,11) (M_bdyty(ibdyty)%blmty(iblmty)%NODES(k),k=9,15)
11  format('      ',7x,2x,5x     ,7(I7))

    return

  end subroutine write_PRI15
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  FUNCTION get_coor_nodty_MAILx(ibdyty,inodty)
    IMPLICIT NONE

    INTEGER :: ibdyty,inodty,iccdof,nbdof
    REAL(kind=8),DIMENSION(nbDIME) :: get_coor_nodty_MAILx

    integer :: toto

    iccdof=M_bdyty(ibdyty)%ccdof(inodty)

    nbdof=nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))

    if (nbdime /= nbdof) then
      print*,nbdime,nbdof
      call faterr('MAILx::get_coor_ndoty','nbdof different from nbdime')
    endif

    get_coor_nodty_MAILx=M_bdyty(ibdyty)%coor(iccdof+1:iccdof+nbdime)

  END FUNCTION get_coor_nodty_MAILx
!!!------------------------------------------------------------------------
  FUNCTION get_cooref_nodty_MAILx(ibdyty,inodty)

    IMPLICIT NONE

    INTEGER :: ibdyty,inodty,iccdof,nbdof
    REAL(kind=8),DIMENSION(nbDIME) :: get_cooref_nodty_MAILx

    iccdof = M_bdyty(ibdyty)%ccdof(inodty)
    nbdof  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))

    get_cooref_nodty_MAILx=M_bdyty(ibdyty)%cooref(iccdof+1:iccdof+nbdof)

  END FUNCTION get_cooref_nodty_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE get_idata_MAILx(ibdyty,itacty,idata)

    IMPLICIT NONE

    INTEGER,INTENT(in)    :: ibdyty,itacty
    INTEGER,INTENT(out),DIMENSION(:) :: idata

    SELECT CASE(M_bdyty(ibdyty)%tacty(itacty)%tacID)

    CASE('CLxxx','ALpxx','PT2DL','PT2TL','DISKL', &
         !'CSpx3','CSpx4','CSpx6','CSpx8','ASpx3','ASpx4','ASpx6','ASpx8', &
         'ASpxx','CSpxx','CSpx0','CSpx1','CSpx2', &                          ! v3.2
         'POLYD')

       idata(1:SIZE(idata))=M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1:SIZE(idata))
    CASE default  
       call faterr('MAILx::get_idata','unknown boundary type: '//M_bdyty(ibdyty)%tacty(itacty)%tacID)
    END SELECT

  END SUBROUTINE get_idata_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE get_idata_sz_MAILx(ibdyty,itacty,idatasz)

    IMPLICIT NONE

    INTEGER,INTENT(in)            :: ibdyty,itacty
    INTEGER,INTENT(out)           :: idatasz

    SELECT CASE(M_bdyty(ibdyty)%tacty(itacty)%tacID)

    CASE('ALpxx', &
         !'CSpx3','CSpx4','CSpx6','CSpx8','ASpx3','ASpx4','ASpx6','ASpx8', & ! v3.1
         'ASpxx','CSpxx','CSpx0','CSpx1','CSpx2')                           ! v3.2
       idatasz=M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(0)
    CASE('POLYD') 
       idatasz=size(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
    CASE default  
       call faterr('MAILx::get_idata_sz','unknown boundary type: '//M_bdyty(ibdyty)%tacty(itacty)%tacID)
    END SELECT

  END SUBROUTINE get_idata_sz_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE get_rdata_MAILx(ibdyty,itacty,rdata)

    IMPLICIT NONE

    INTEGER,INTENT(in)    :: ibdyty,itacty
    REAL(kind=8),INTENT(out),DIMENSION(SIZE(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)) :: rdata

    SELECT CASE(M_bdyty(ibdyty)%tacty(itacty)%tacID)
    CASE('CLxxx','PT2DL','PT2TL')
       rdata(1)=M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1)
    CASE('DISKL')
       rdata(1)=M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1)
       rdata(2)=M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(2)
       rdata(3)=M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(3)
    CASE default  
       call faterr('MAILx::get_rdata','unknown boundary type: '//M_bdyty(ibdyty)%tacty(itacty)%tacID)
    END SELECT

  END SUBROUTINE get_rdata_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_tacty_MAILx(iM_bdyty)

    IMPLICIT NONE
    INTEGER :: iM_bdyty

    IF (ASSOCIATED(M_bdyty(iM_bdyty)%tacty)) THEN
       get_nb_tacty_MAILx=SIZE(M_bdyty(iM_bdyty)%tacty)
    ELSE
       get_nb_tacty_MAILx=0
    ENDIF

  END FUNCTION get_nb_tacty_MAILx
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_tacID_MAILx(iM_bdyty,iM_tacty)

    IMPLICIT NONE

    INTEGER :: iM_bdyty,iM_tacty

    get_tacID_MAILx=M_bdyty(iM_bdyty)%tacty(iM_tacty)%tacID

  END FUNCTION get_tacID_MAILx
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_color_MAILx(iM_bdyty,iM_tacty)

    IMPLICIT NONE

    INTEGER :: iM_bdyty,iM_tacty

    get_color_MAILx=M_bdyty(iM_bdyty)%tacty(iM_tacty)%color

  END FUNCTION get_color_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_node_MAILx(ibdyty)

    IMPLICIT NONE

    INTEGER :: ibdyty

    get_nb_node_MAILx = SIZE(M_bdyty(ibdyty)%nodty)

  END FUNCTION get_nb_node_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_cell_MAILx(ibdyty)

    IMPLICIT NONE

    INTEGER :: ibdyty

    get_nb_cell_MAILx = SIZE(M_bdyty(ibdyty)%blmty)

  END FUNCTION get_nb_cell_MAILx
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_node_by_cell_MAILx(ibdyty,iblmty)

    IMPLICIT NONE

    INTEGER :: ibdyty,iblmty

    get_nb_node_by_cell_MAILx = SIZE(M_bdyty(ibdyty)%blmty(iblmty)%NODES)

  END FUNCTION get_nb_node_by_cell_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE get_connect_cell_MAILx(ibdyty,iblmty,NODES)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty
    INTEGER, DIMENSION(SIZE(M_bdyty(ibdyty)%blmty(iblmty)%NODES)) :: NODES 

    NODES = M_bdyty(ibdyty)%blmty(iblmty)%NODES

  END SUBROUTINE get_connect_cell_MAILx
!!!------------------------------------------------------------------------
!!!
!!! gestion de la bd des champs par elements
!!!
!!!------------------------------------------------------------------------
  SUBROUTINE init_mecagpv_MAILx(ibdyty,iblmty,nb_gp,nb_external,nb_internal,nb_fields,field_name,nb_vfields,vfield_name,vsize)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, nb_gp, ig
    INTEGER :: nb_external, nb_internal

    integer                       ,optional :: nb_fields,nb_vfields,vsize
    character(len=30),dimension(:),optional :: field_name,vfield_name


    M_bdyty(ibdyty)%is_meca=.TRUE.

    if (nb_gp == 0) then
      !fd pour elements ext
      M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present=.FALSE.
      return
    else
      M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present=.TRUE. 
    endif

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(nb_gp,2))

    DO ig=1,nb_gp

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress=0.d0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain=0.d0
       !
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal)
       !
       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress=0.d0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain=0.d0
       !
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal)
       !
       IF (nb_internal /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal = 0.d0 

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal = 0.d0 
       ENDIF


       ! coupled fields

       if ( .not. present(nb_fields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%field_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name(nb_fields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_value(nb_fields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%field_value(nb_fields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%field_name)

          M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name = field_name

       endif

       if (.not. present(nb_vfields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%vfield_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_name(nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_value(vsize,nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%vfield_value(vsize,nb_vfields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%vfield_name)

          M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_name = vfield_name

       end if

    END DO

  END SUBROUTINE init_mecagpv_MAILx
!!!-------------------------------------------------------------------------
  subroutine update_mecagpv_MAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    !
    integer(kind=4) :: iblmty,ig

    DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)

       if (.not. M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) cycle

       DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(:,1),dim=1)

          M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress 
          M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain 

          IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal)) & 
               M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal= &
               M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal 

          IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_value)) & 
               M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%field_value= &
               M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_value 

          ! on n'update pas field_name car ne semble pas avoir de sens

          if (associated(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_value)) & 
               M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%vfield_value= &
               M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_value 

       END DO
    END DO

  end subroutine update_mecagpv_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_stress_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: s
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress = s
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress)
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress = s(1:sz)
    ENDIF

  END SUBROUTINE put_stress_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_stress_0_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: s

    s=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       s=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress)
       s(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress
    ENDIF

  END SUBROUTINE get_stress_0_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_stress_1_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: s

    s=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       s=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress)
       s(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress
    ENDIF

  END SUBROUTINE get_stress_1_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_strain_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: e
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain = e
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain)
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain = e(1:sz)
    ENDIF
    
  END SUBROUTINE put_strain_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_strain_0_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: e
    
    e=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       e=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz=size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain)
       e(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain
    ENDIF

  END SUBROUTINE get_strain_0_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_strain_1_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: e
    
    e=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       e=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain)
       e(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain
    ENDIF

  END SUBROUTINE get_strain_1_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_strain_rate_MAILx(ibdyty,iblmty,ig,e_dot)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: e_dot
    
    e_dot=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       e_dot=(  M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain  &
         - M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain)/H
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       e_dot=(  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain  &
         - M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain)/H
    ENDIF

  END SUBROUTINE get_strain_rate_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_internal_MAILx(ibdyty,iblmty,ig,i)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: i

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal = i
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal = i
    ENDIF

  END SUBROUTINE put_internal_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_internal_0_MAILx(ibdyty,iblmty,ig,i)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: i
    
    i=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       i=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       i=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal
    ENDIF

  END SUBROUTINE get_internal_0_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_internal_1_MAILx(ibdyty,iblmty,ig,i)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: i
    
    i=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
       i=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       i=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal
    ENDIF

  END SUBROUTINE get_internal_1_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE get_Mentropy_0_MAILx(ibdyty,iblmty,ig,S)

    ! a virer 

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: S

    S=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal(2)    

  END SUBROUTINE get_Mentropy_0_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE get_Mentropy_1_MAILx(ibdyty,iblmty,ig,S)

    ! a virer 
    
    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: S

    S=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal(2)

  END SUBROUTINE get_Mentropy_1_MAILx
!!!---------------------------------------------------------------------------
  integer function get_meca_field_size_MAILx(ibdyty,iblmty,ig)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig

    
    if ((M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present)) then
        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name)) then
            get_meca_field_size_MAILx=size(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name)
        else
            get_meca_field_size_MAILx= 0
        endif
    endif 
    if ((M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present)) then
        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)) then
            get_meca_field_size_MAILx=size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)
        else
            get_meca_field_size_MAILx= 0
        endif
    endif

  END function 
!!!---------------------------------------------------------------------------
!!!---------------------------------------------------------------------------
  SUBROUTINE get_meca_field_name_MAILx(ibdyty,iblmty,ig,if,field_name)

    IMPLICIT NONE

    INTEGER           :: ibdyty, iblmty, ig, if
    character(len=30) :: field_name

    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name)) then
      field_name = M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_name(if)
    endif

    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)) then 
      field_name = M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name(if)
    endif

  END SUBROUTINE 
!!!---------------------------------------------------------------------------
!!!---------------------------------------------------------------------------
  !> set the value of the field
  !> 
  SUBROUTINE set_meca_field_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER          :: ibdyty, iblmty, ig, if
    REAL(kind=8)     :: s

    M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_value(if)=s

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  !> get the rank of a field 
  !> assumes that the fields are the same on all gp
  !
  integer function get_meca_field_rank_MAILx(ibdyty,iblmty,name)

    IMPLICIT NONE

    INTEGER          :: ibdyty, iblmty
    character(len=*) :: name

    integer          :: if

    get_meca_field_rank_MAILx= 0

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN
      if (associated(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(1,1)%field_name)) then
        do if=1,size(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(1,1)%field_name)
          if (M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(1,1)%field_name(if)==name) then
            get_meca_field_rank_MAILx=if
            exit
          endif
        enddo
      endif   
    ENDIF
     
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
      if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%field_name)) then  
        do if=1,size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%field_name)
          if (M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%field_name(if)==name) then
            get_meca_field_rank_MAILx=if
            exit
          endif
        enddo
      endif 
    ENDIF

  END function
!!!---------------------------------------------------------------------------
  SUBROUTINE get_meca_field_begin_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER      :: ibdyty, iblmty, ig, if
    REAL(kind=8) :: s

    s=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN    
      s=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%field_value(if)
    ENDIF

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
      s=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_value(if)
    ENDIF

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  SUBROUTINE get_meca_field_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER      :: ibdyty, iblmty, ig, if
    REAL(kind=8) :: s
    
    s=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) THEN    
      s=M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%field_value(if)
    ENDIF

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
      s=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value(if)
    ENDIF

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  !> get the rank of a vector field 
  !> assumes that the fields are the same on all gp
  integer function get_meca_vfield_rank_MAILx(ibdyty,iblmty,name)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> name of vector field
    character(len=*), intent(in) :: name
    !
    integer(kind=4) :: if

    get_meca_vfield_rank_MAILx = 0

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) then
      do if = 1, size(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(1,1)%vfield_name)
        if (M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(1,1)%vfield_name(if)==name) then
          get_meca_vfield_rank_MAILx = if
          exit
        end if
      end do
    end if

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) then
      do if = 1, size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%vfield_name)
        if (M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%vfield_name(if)==name) then
          get_meca_vfield_rank_MAILx = if
          exit
        end if
      end do
    end if

  end function
!!!---------------------------------------------------------------------------
  !> get the maximum size  of a vector field 
  integer(kind=4) function get_meca_vfield_max_size_MAILx(ibdyty,iblmty)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty

    get_meca_vfield_max_size_MAILx = 0

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) then

      get_meca_vfield_max_size_MAILx = size(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(1,1)%vfield_value,1)

    end if

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) then

      get_meca_vfield_max_size_MAILx = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%vfield_value,1)

    end if

  end function
!!!---------------------------------------------------------------------------
  !> set the value of a vector field
  subroutine set_meca_vfield_MAILx(ibdyty,iblmty,ig,if,s,sd)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> gauss point index
    integer(kind=4), intent(in) :: ig
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> size of vector field
    integer(kind=4), intent(in) :: sd
    !> new vector field values
    real(kind=8), dimension(sd), intent(in) :: s

    M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_value(1:sd,if) = s(1:sd)

  end subroutine
!!!---------------------------------------------------------------------------
  !> get the value of a a vector field
  subroutine get_meca_vfield_MAILx(ibdyty,iblmty,ig,if,s,sd)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> gauss point index
    integer(kind=4), intent(in) :: ig
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> size of vector field
    integer(kind=4), intent(in) :: sd
    !> vector field values
    real(kind=8), dimension(sd), intent(out) :: s
    
    s(:) = 0.d0
    if (M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present) then
      s(1:sd) = M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%vfield_value(1:sd,if)
    end if

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) then
      s(1:sd) = M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_value(1:sd,if)
    end if

  end subroutine
!!!---------------------------------------------------------------------------
!!!---------------------------------------------------------------------------
  SUBROUTINE init_thergpv_MAILx(ibdyty,iblmty,nb_gp,nb_external,nb_internal,Tini,nb_fields,field_name,nb_vfields,vfield_name,vsize)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, nb_gp, ig
    INTEGER :: nb_external,nb_internal
    REAL(kind=8) :: Tini

    integer                       ,optional :: nb_fields
    character(len=30),dimension(:),optional :: field_name

    integer                       ,optional :: nb_vfields,vsize
    character(len=30),dimension(:),optional :: vfield_name

    M_bdyty(ibdyty)%is_ther = .TRUE.
    M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present=.TRUE.

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(nb_gp,2))

    DO ig=1,nb_gp

       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux, &
               M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad, &
               M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux, &
               M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad)

       IF (nb_external /=0 ) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux(nb_external)=0.d0

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad(nb_external)=0.d0

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux(nb_external)=0.d0

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad(nb_external)=0.d0
       ENDIF

       !
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal)
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal)
       !
       IF (nb_internal /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal = 0.d0 

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal = 0.d0 
       END IF
       !
       M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%T=Tini
       M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%T=Tini

       ! coupled fields

       if ( .not. present(nb_fields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name(nb_fields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value(nb_fields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_value(nb_fields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_name)

          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name = field_name

       endif

       if (.not. present(nb_vfields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%vfield_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_name(nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_value(vsize,nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%vfield_value(vsize,nb_vfields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%vfield_name)

          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_name = vfield_name

       end if

       !
    END DO
  END SUBROUTINE init_thergpv_MAILx
!!!----------------------------------------------------------------------------
  SUBROUTINE update_thergpv_MAILx(ibdyty)

    IMPLICIT NONE

    INTEGER :: ibdyty
    ! ***
    integer :: iblmty, ig

    !print*,ibdyty
    DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
      !print*,iblmty

      if (.not. M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) cycle

      DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(:,1),dim=1)
        !print*,ig
        M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%T    = &
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%T 

        IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux)) THEN
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux = &
            M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux 

          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad = &
            M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad 
        ENDIF

        IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal)) & 
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal= &
            M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal 

        IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value)) & 
          M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_value=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value 

        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_value)) & 
             M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%vfield_value= &
             M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_value 

      END DO
    END DO

  END SUBROUTINE update_thergpv_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_flux_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: s
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux=s
    ENDIF

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux)
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux=s(1:sz)
    ENDIF
    
  END SUBROUTINE put_flux_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_flux_0_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: s

    s = 0.D0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       s=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux)
       s(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux
    ENDIF

  END SUBROUTINE get_flux_0_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_flux_1_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: s

    s = 0.D0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       s=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux)
       s(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux
    ENDIF

  END SUBROUTINE get_flux_1_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_grad_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: e

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad=e
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad)
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad=e(1:sz)
    ENDIF

  END SUBROUTINE put_grad_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_grad_0_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: e

    e=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       e=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad)
       e(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad
    ENDIF

  END SUBROUTINE get_grad_0_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_grad_1_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig, sz
    REAL(kind=8),DIMENSION(:) :: e

    e=0.d0
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       e=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       sz = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad)
       e(1:sz)=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad
    ENDIF

  END SUBROUTINE get_grad_1_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_T_MAILx(ibdyty,iblmty,ig,T)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: T
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%T = T
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%T = T
    ENDIF

  END SUBROUTINE put_T_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_T_0_MAILx(ibdyty,iblmty,ig,T)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: T
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       T=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%T
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       T=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%T
    ENDIF

  END SUBROUTINE get_T_0_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_T_1_MAILx(ibdyty,iblmty,ig,T)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: T

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       T=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%T
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       T=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%T
    ENDIF

  END SUBROUTINE get_T_1_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE put_Tref_MAILx(ibdyty,iblmty,ig,Tref)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: Tref
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%Tref=Tref
    ENDIF
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%Tref=Tref
    ENDIF

  END SUBROUTINE put_Tref_MAILx
!!!---------------------------------------------------------------------------
  SUBROUTINE get_Tref_0_MAILx(ibdyty,iblmty,ig,Tref)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: Tref

    Tref=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%Tref

  END SUBROUTINE get_Tref_0_MAILx
!!!---------------------------------------------------------------------------  
  SUBROUTINE get_Tref_1_MAILx(ibdyty,iblmty,ig,Tref)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8) :: Tref

    Tref=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%Tref

  END SUBROUTINE get_Tref_1_MAILx
!!!---------------------------------------------------------------------------
  integer function get_ther_field_size_MAILx(ibdyty,iblmty,ig)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig

    !if (associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name)) then
    !   get_ther_field_size_MAILx=size(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name)
    !else
    !   get_ther_field_size_MAILx= 0
    !end if

    if ((M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present)) then
        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name)) then
            get_ther_field_size_MAILx=size(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name)
        else
            get_ther_field_size_MAILx= 0
        endif
    endif 
    if ((M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present)) then
        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_name)) then
            get_ther_field_size_MAILx=size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_name)
        else
            get_ther_field_size_MAILx= 0
        endif
    endif

  END function 
!!!---------------------------------------------------------------------------
!!!---------------------------------------------------------------------------
  SUBROUTINE get_ther_field_name_MAILx(ibdyty,iblmty,ig,if,field_name)

    IMPLICIT NONE

    INTEGER           :: ibdyty, iblmty, ig, if
    character(len=30) :: field_name

    !if ( .not. associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name)) stop
    !field_name = M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name(if)

    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name)) then
        field_name = M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_name(if)
    endif

    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)) then 
        field_name = M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name(if)
    endif


  END SUBROUTINE 
!!!---------------------------------------------------------------------------
!!!---------------------------------------------------------------------------
  !> set the value of the field
  !> 
  SUBROUTINE set_ther_field_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER          :: ibdyty, iblmty, ig, if
    REAL(kind=8)     :: s

    M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value(if)=s

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  !> get the rank of a field 
  !> assumes that the fields are the same on all gp
  !
  integer function get_ther_field_rank_MAILx(ibdyty,iblmty,name)

    IMPLICIT NONE

    INTEGER          :: ibdyty, iblmty
    character(len=*) :: name

    integer          :: if

    get_ther_field_rank_MAILx= 0

   IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN
    do if=1,size(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(1,1)%field_name)
      if (M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(1,1)%field_name(if)==name) then
        get_ther_field_rank_MAILx=if
        exit
      endif
    enddo
   ENDIF
   
   IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN    
        do if=1,size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%field_name)
          if (M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%field_name(if)==name) then
            get_ther_field_rank_MAILx=if
            exit
          endif
        enddo
    ENDIF
    
  END function
!!!---------------------------------------------------------------------------
  SUBROUTINE get_ther_field_begin_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER      :: ibdyty, iblmty, ig, if
    REAL(kind=8) :: s

    !s=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_value(if)

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN    
      s=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%field_value(if)
    ENDIF

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
      s=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%field_value(if)
    ENDIF

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  SUBROUTINE get_ther_field_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER      :: ibdyty, iblmty, ig, if
    REAL(kind=8) :: s

    !s=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value(if)
    
    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) THEN    
      s=M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%field_value(if)
    ENDIF

    IF (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) THEN
      s=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_value(if)
    ENDIF

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  !> get the rank of a vector field 
  !> assumes that the fields are the same on all gp
  integer function get_ther_vfield_rank_MAILx(ibdyty,iblmty,name)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> name of vector field
    character(len=*), intent(in) :: name
    !
    integer(kind=4) :: if

    get_ther_vfield_rank_MAILx = 0

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) then
      do if = 1, size(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(1,1)%vfield_name)
        if (M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(1,1)%vfield_name(if)==name) then
          get_ther_vfield_rank_MAILx = if
          exit
        end if
      end do
    end if

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) then
      do if = 1, size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%vfield_name)
        if (M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%vfield_name(if)==name) then
          get_ther_vfield_rank_MAILx = if
          exit
        end if
      end do
    end if

  end function
!!!---------------------------------------------------------------------------
  !> get the maximum size  of a vector field 
  integer(kind=4) function get_ther_vfield_max_size_MAILx(ibdyty,iblmty)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty

    get_ther_vfield_max_size_MAILx = 0

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) then

      get_ther_vfield_max_size_MAILx = size(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(1,1)%vfield_value,1)

    end if

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) then

      get_ther_vfield_max_size_MAILx = size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%vfield_value,1)

    end if

  end function
!!!---------------------------------------------------------------------------
  !> set the value of a vector field
  subroutine set_ther_vfield_MAILx(ibdyty,iblmty,ig,if,s,sd)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> gauss point index
    integer(kind=4), intent(in) :: ig
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> size of vector field
    integer(kind=4), intent(in) :: sd
    !> new vector field values
    real(kind=8), dimension(sd), intent(in) :: s

    M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_value(1:sd,if) = s(1:sd)

  end subroutine
!!!---------------------------------------------------------------------------
  !> get the value of a a vector field
  subroutine get_ther_vfield_MAILx(ibdyty,iblmty,ig,if,s,sd)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> gauss point index
    integer(kind=4), intent(in) :: ig
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> size of vector field
    integer(kind=4), intent(in) :: sd
    !> vector field values
    real(kind=8), dimension(sd), intent(out) :: s
    
    s(:) = 0.d0
    if (M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present) then
      s(1:sd) = M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%vfield_value(1:sd,if)
    end if

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) then
      s(1:sd) = M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_value(1:sd,if)
    end if

  end subroutine
!!!---------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE init_thmc_gpv_MAILx(ibdyty,iblmty,nb_gp,nb_external,nb_internal)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, nb_gp, ig
    INTEGER :: nb_external,nb_internal
    REAL(kind=8) :: Tini

    M_bdyty(ibdyty)%blmty(iblmty)%is_thmc_present=.TRUE.

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(nb_gp,2))

    DO ig=1,nb_gp

       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux, &
            M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad, &
            M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%flux, &
            M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%grad)

       IF (nb_external /=0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux(nb_external)=0.d0

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad(nb_external)=0.d0

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%flux(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%flux(nb_external)=0.d0

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%grad(nb_external))
          M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%grad(nb_external)=0.d0

       ENDIF
       !
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal, &
            M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%internal)
       !
       IF (nb_internal /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal = 0.d0 

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%internal = 0.d0 
       ENDIF
       !
    ENDDO
  END SUBROUTINE init_thmc_gpv_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE update_thmc_gpv_MAILx

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig

    !fd on doit rajouter un paranoiac test pour savoir si cet element est bien thmc, URGENT

    DO ibdyty=1,SIZE(M_bdyty)
       DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)
          DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(:,1),dim=1)

             IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%flux)) THEN
                M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%flux =M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux 
                M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%grad =M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad 
             ENDIF

             IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal)) THEN
                M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%internal=M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal 
             ENDIF
          ENDDO
       ENDDO
    ENDDO

  END SUBROUTINE update_thmc_gpv_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE put_thmc_internal_MAILx(ibdyty,iblmty,ig,i)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: i

    M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%internal=i

  END SUBROUTINE put_thmc_internal_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE get_thmc_internal_MAILx(ibdyty,iblmty,ig,i)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: i

    i = M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%internal

  END SUBROUTINE get_thmc_internal_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE put_thmc_flux_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: s

    M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux=s

  END SUBROUTINE put_thmc_flux_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE get_thmc_flux_0_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: s

    s=M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%flux

  END SUBROUTINE get_thmc_flux_0_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE get_thmc_flux_1_MAILx(ibdyty,iblmty,ig,s)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: s

    s=M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%flux

  END SUBROUTINE get_thmc_flux_1_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE put_thmc_grad_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: e

    M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,1)%grad=e


  END SUBROUTINE put_thmc_grad_MAILx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE get_thmc_grad_MAILx(ibdyty,iblmty,ig,e)

    IMPLICIT NONE
    INTEGER :: ibdyty, iblmty, ig
    REAL(kind=8),DIMENSION(:) :: e

    e=M_bdyty(ibdyty)%blmty(iblmty)%thmc_gpv(ig,2)%grad


  END SUBROUTINE get_thmc_grad_MAILx

  ! >-------------------------THMCx--------------------------------------->
!!!------------------------------------------------------------------------
  SUBROUTINE get_nb_outline_MAILx(iM_ibdyty,nb_outline)

    IMPLICIT NONE

    INTEGER,INTENT(in)       :: iM_ibdyty
    INTEGER,INTENT(out)      :: nb_outline
    INTEGER                  :: itacty,nb_DISKL

    nb_outline = 0

    DO itacty=1,SIZE(M_bdyty(iM_ibdyty)%tacty)
       IF (M_bdyty(iM_ibdyty)%tacty(itacty)%tacID == 'DISKL') THEN
          nb_outline =nb_outline + 1 
       END IF
    END DO

  END SUBROUTINE get_nb_outline_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE add_dof2bodies_MAILx

    IMPLICIT NONE

    INTEGER :: iM_bdyty

    DO iM_bdyty=1,nb_MAILx       
       M_bdyty(iM_bdyty)%cooref(:)=M_bdyty(iM_bdyty)%coor(:)
    END DO

  END SUBROUTINE add_dof2bodies_MAILx
!!!------------------------------------------------------------------------
  SUBROUTINE comp_cell_area(ibdyty,iblmty,area)

    IMPLICIT NONE
    INTEGER :: ibdyty,iblmty
    INTEGER :: iccdof1,nbdof1,iccdof2,nbdof2
    INTEGER, DIMENSION(SIZE(M_bdyty(ibdyty)%blmty(iblmty)%NODES)) :: NODES 

    REAL(kind=8)              :: AREA
    REAL(kind=8),DIMENSION(2) :: X1,X2

    NODES = M_bdyty(ibdyty)%blmty(iblmty)%NODES
    AREA  = 0.D0

    SELECT CASE(M_bdyty(ibdyty)%blmty(iblmty)%blmID)
    CASE('S2xxx')

       iccdof1 = M_bdyty(ibdyty)%ccdof(NODES(1))
       nbdof1  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(1)))

       iccdof2 = M_bdyty(ibdyty)%ccdof(NODES(2))
       nbdof2  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(2)))

       X1   = M_bdyty(ibdyty)%coor(iccdof1+1:iccdof1+nbdof1) - M_bdyty(ibdyty)%coor(iccdof2+1:iccdof2+nbdof2)
       AREA = SQRT(X1(1)*X1(1)+X1(2)*X1(2))

    CASE('T3xxx')   

       iccdof1 = M_bdyty(ibdyty)%ccdof(NODES(1))
       nbdof1  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(1)))

       iccdof2 = M_bdyty(ibdyty)%ccdof(NODES(2))
       nbdof2  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(2)))

       X1   = M_bdyty(ibdyty)%coor(iccdof1+1:iccdof1+nbdof1) - M_bdyty(ibdyty)%coor(iccdof2+1:iccdof2+nbdof2)

       iccdof2 = M_bdyty(ibdyty)%ccdof(NODES(3))
       nbdof2  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(3)))

       X2   = M_bdyty(ibdyty)%coor(iccdof1+1:iccdof1+nbdof1) - M_bdyty(ibdyty)%coor(iccdof2+1:iccdof2+nbdof2)

       AREA = 0.5*ABS(X1(1)*X2(2)-X1(2)*X2(1))

    CASE('Q4xxx')    

       iccdof1 = M_bdyty(ibdyty)%ccdof(NODES(1))
       nbdof1  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(1)))

       iccdof2 = M_bdyty(ibdyty)%ccdof(NODES(2))
       nbdof2  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(2)))

       X1   = M_bdyty(ibdyty)%coor(iccdof1+1:iccdof1+nbdof1) - M_bdyty(ibdyty)%coor(iccdof2+1:iccdof2+nbdof2)

       iccdof2 = M_bdyty(ibdyty)%ccdof(NODES(3))
       nbdof2  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(3)))

       X2   = M_bdyty(ibdyty)%coor(iccdof1+1:iccdof1+nbdof1) - M_bdyty(ibdyty)%coor(iccdof2+1:iccdof2+nbdof2)

       AREA = 0.5*ABS(X1(1)*X2(2)-X1(2)*X2(1))

       X2 = - X2
       
       iccdof1 = M_bdyty(ibdyty)%ccdof(NODES(4))
       nbdof1  = nbdof_a_nodty(M_bdyty(ibdyty)%nodty(NODES(4)))

       X1   = M_bdyty(ibdyty)%coor(iccdof2+1:iccdof2+nbdof2) - M_bdyty(ibdyty)%coor(iccdof1+1:iccdof1+nbdof1)

       AREA = AREA + 0.5*ABS(X1(1)*X2(2)-X1(2)*X2(1))

    CASE default  
       call faterr('MAILx::comp_cell_area','bulk element unknown')
    END SELECT

  END SUBROUTINE comp_cell_area
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_model_MAILx(ibdyty,iblmty)

    IMPLICIT NONE
    INTEGER :: ibdyty,iblmty

    get_nb_model_MAILx = SIZE(M_bdyty(ibdyty)%blmty(iblmty)%model)

  END FUNCTION get_nb_model_MAILx
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_model_MAILx(ibdyty,iblmty,imodel)

    IMPLICIT NONE
    INTEGER :: ibdyty,iblmty,imodel

    get_model_MAILx = M_bdyty(ibdyty)%blmty(iblmty)%model(imodel)

  END FUNCTION get_model_MAILx
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_behav_MAILx(ibdyty,iblmty,imodel)

    IMPLICIT NONE
    INTEGER :: ibdyty,iblmty,imodel

    get_behav_MAILx = M_bdyty(ibdyty)%blmty(iblmty)%behav(imodel)

  END FUNCTION get_behav_MAILx
!!!------------------------------------------------------------------------
  !fd a quoi sert ce truc ?
  !fd 20/04/08
  LOGICAL FUNCTION get_write_GPV_actif_MAILx(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_GPV_actif_MAILx = write_GPV_actif

  END FUNCTION get_write_GPV_actif_MAILx
!!!------------------------------------------------------------------------
!!! ajout de l'API pour charger les modeles depuis l'exterieur
!!!------------------------------------------------------------------------
  subroutine set_nb_MAILx(i4)
    implicit none
                              !12345678901234567890123
    character(len=23)  :: IAM='mod_MAILx::set_nb_MAILx'
    character(len=103) :: cout

    integer :: i4,ibdyty
    integer :: errare

    nb_MAILx = i4

    allocate(M_bdyty(nb_MAILx),stat=errare)
    if (errare /= 0) then
      call FATERR(IAM,'error allocating bdyty')
    end if

    do ibdyty=1,i4
      nullify(M_bdyty(ibdyty)%blmty)
      nullify(M_bdyty(ibdyty)%nodty)
      nullify(M_bdyty(ibdyty)%tacty)
    enddo

    write(cout,'(1X,I5,1X,A5,1X,A5)') nb_MAILx,'MAILx','found'
    call logmes(cout)

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_nb_bulks_MAILx(ibdyty,i4)
    implicit none
                              !1234567890123456789012345678
    character(len=29)  :: IAM='mod_MAILx::set_nb_bulks_MAILx'
    integer :: ibdyty,iblmty,i4
    integer :: errare

    allocate(M_bdyty(ibdyty)%blmty(i4),stat=errare)
    if (errare /= 0) then
      call FATERR(IAM,'error allocating M_bdyty%blmty')
    end if

    do iblmty=1,i4
      nullify(M_bdyty(ibdyty)%blmty(iblmty)%model)
      nullify(M_bdyty(ibdyty)%blmty(iblmty)%behav)

       M_bdyty(ibdyty)%blmty(iblmty)%is_meca_present=.false. 
       nullify(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv)

       M_bdyty(ibdyty)%blmty(iblmty)%is_ther_present=.false. 
       nullify(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv)
    enddo

  end subroutine
!!!------------------------------------------------------------------------

! fd a finir 

  subroutine set_nodes_of_bulk_MAILx(ibdyty,iblmty,vec_i4)
    implicit none
                              !1234567890123456789012345678
    character(len=28)  :: IAM='mod_MAILx::set_nb_nodes_of_bulk_MAILx'
    character(len=103) :: cout
    integer :: ibdyty,iblmty,vec_i4(:),sz
    integer :: errare

    sz = size(vec_i4)

    if (sz /= 0) then
      allocate(M_bdyty(ibdyty)%blmty(iblmty)%NODES(sz),stat=errare)
      if (errare /= 0) then
    call FATERR(IAM,'error allocating M_bdyty%blmty%NODES')
      end if

      M_bdyty(ibdyty)%blmty(iblmty)%NODES = vec_i4

      if (nbdime == 2) then

        if (sz == 3) then
          M_bdyty(ibdyty)%blmty(iblmty)%blmID='T3xxx'
        else if ( sz == 4) then
          M_bdyty(ibdyty)%blmty(iblmty)%blmID='Q4xxx' 
        else
          !rm : 03/02/17 : for charms method
          M_bdyty(ibdyty)%blmty(iblmty)%blmID='extFE' 
        endif

      else if (nbdime==3) then
          
        if (sz == 4) then
          M_bdyty(ibdyty)%blmty(iblmty)%blmID='TE4xx'
        else if ( sz == 8) then
          M_bdyty(ibdyty)%blmty(iblmty)%blmID='H8xxx' 
        else
          !rm : 03/02/17 : for charms method
          M_bdyty(ibdyty)%blmty(iblmty)%blmID='extFE' 
        endif

      endif
      !

    else
      nullify(M_bdyty(ibdyty)%blmty(iblmty)%NODES)
      write(cout,'(1X,A28,1X,I5)') 'WARNING: MAILx without nodes',ibdyty
      call LOGMES(cout)
    end if

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_model_and_behav_of_bulk_MAILx(ibdyty,iblmty,mat_c5)
    implicit none
    integer          :: ibdyty,iblmty,nb,i  
    character(len=5) :: mat_c5(:,:)

    nb = size(mat_c5,dim=1)    

    allocate(M_bdyty(ibdyty)%blmty(iblmty)%model(nb), &
             M_bdyty(ibdyty)%blmty(iblmty)%behav(nb) )

    do i=1,nb
      M_bdyty(ibdyty)%blmty(iblmty)%model(i)=mat_c5(i,1)
      M_bdyty(ibdyty)%blmty(iblmty)%behav(i)=mat_c5(i,2)
    enddo

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_nb_nodes_MAILx(ibdyty,i4)
    implicit none
                              !1234567890123456789012345678
    character(len=28)  :: IAM='mod_MAILx::set_nb_nodes_MAILx'
    character(len=103) :: cout
    integer :: ibdyty,i4,inodty
    integer :: errare

    if (i4 /= 0) then
      allocate(M_bdyty(ibdyty)%nodty(i4),stat=errare)
      if (errare /= 0) then
        call FATERR(IAM,'error allocating M_bdyty%nodty')
      end if

      if (nbdime == 2 ) then

        do inodty=1,i4
          call new_nodty(M_bdyty(ibdyty)%nodty(inodty),'NO2xx')
        enddo

        allocate(M_bdyty(ibdyty)%cooref(2*i4),stat=errare)
        allocate(M_bdyty(ibdyty)%coor  (2*i4),stat=errare)
        if (errare /= 0) then
          call FATERR(IAM,'error allocating M_bdyty%cooref')
        end if


      else if (nbdime == 3 ) then
        do inodty=1,i4
          call new_nodty(M_bdyty(ibdyty)%nodty(inodty),'NO3xx')
        enddo

        allocate(M_bdyty(ibdyty)%cooref(3*i4),stat=errare)
        allocate(M_bdyty(ibdyty)%coor  (3*i4),stat=errare)
        if (errare /= 0) then
          call FATERR(IAM,'error allocating M_bdyty%cooref')
        end if


      else
        call FATERR(IAM,'dime unsupported')
      endif

      allocate(M_bdyty(ibdyty)%nod2blmty(i4),stat=errare)
      if (errare /= 0) then
        call FATERR(IAM,'error allocating M_bdyty%nod2blmty')
      end if
       

    else
      nullify(M_bdyty(ibdyty)%nodty)
      nullify(M_bdyty(ibdyty)%nod2blmty)
!                                    1234567890123456789012345678
      write(cout,'(1X,A28,1X,I5)') 'WARNING: MAILx without nodes',ibdyty
      call LOGMES(cout)
    end if      

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_cooref_nodes_MAILx(ibdyty,vec_r8)
    implicit none
                              !123456789012345678901234567890123
    character(len=33)  :: IAM='mod_MAILx::set_cooref_nodes_MAILx'
    character(len=103) :: cout
    integer :: ibdyty
    integer :: errare
    real(kind=8) :: vec_r8(:)

    M_bdyty(ibdyty)%cooref = vec_r8
    M_bdyty(ibdyty)%coor   = vec_r8

  end subroutine
!!!------------------------------------------------------------------------
  subroutine build_working_arrays_MAILx
    implicit none

    ! a working array 

    TYPE(G_i_list),DIMENSION(:),POINTER :: work_i_list
    integer :: ibdyty,iblmty,nbn,inodty,t_i,errare
    integer :: iccdof,idof
    integer :: G_i_length

    character(len=103) :: cout
    character(len=33)  :: IAM='mod_MAILx::build_working_arrays_MAILx'

    ! necessaire a la construction de la liste du nb d'element auquel appartien un noeud

    allocate(work_i_list(nb_MAILx),stat=errare)
    if (errare /= 0) then
      call FATERR(IAM,'error allocating work_i_list')
    end if

    do ibdyty=1,nb_MAILx
 
      !  dimensionnement des tableaux 

      iccdof=0

      !fd a revoir car le type du noeud mal defini (geo au lieu de model)

      do inodty=1,size(M_bdyty(ibdyty)%nodty)
        iccdof=iccdof + nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))
      end do

      if (iccdof /= 0) then
        allocate(M_bdyty(ibdyty)%nodnb(iccdof),stat=errare)
        allocate(M_bdyty(ibdyty)%dofnb(iccdof),stat=errare)
        if (errare /= 0) then
          call FATERR(IAM,'error allocating X,V,...')
        end if
      else 
        nullify(M_bdyty(ibdyty)%nodnb)
        nullify(M_bdyty(ibdyty)%dofnb)

!                                     12345678901234567890123456
        write(cout,'(1X,A26,1X,I5)') 'WARNING: MAILx without DOF',ibdyty
        call LOGMES(cout)
      end if

!    array node -> first global ddl

      if (size(M_bdyty(ibdyty)%nodty) /= 0) then
        allocate(M_bdyty(ibdyty)%ccdof(size(M_bdyty(ibdyty)%nodty)),stat=errare)
        if (errare /= 0) then
          call FATERR(IAM,'error allocating ccdof')
        end if
      else 
        nullify(M_bdyty(ibdyty)%ccdof)
!                                     123456789012345678901234567
        write(cout,'(1X,A27,1X,I5)') 'WARNING: MAILx without node',ibdyty
        call LOGMES(cout)
      end if

      iccdof=0

      do inodty=1,size(M_bdyty(ibdyty)%nodty)
        M_bdyty(ibdyty)%ccdof(inodty)=iccdof
        do idof=1,nbdof_a_nodty(M_bdyty(ibdyty)%nodty(inodty))
          iccdof=iccdof+1
          M_bdyty(ibdyty)%nodnb(iccdof)=inodty       ! reverse mapping
          M_bdyty(ibdyty)%dofnb(iccdof)=idof         ! reverse mapping
        end do
      end do

      nbn = size(M_bdyty(ibdyty)%nodty)
      allocate(work_i_list(ibdyty)%G_i(nbn),stat=errare)
      if (errare /= 0) then
        call FATERR(IAM,'error allocating work_i_list%G_i')
      end if
      work_i_list(ibdyty)%G_i = 0 

      do iblmty=1,size(M_bdyty(ibdyty)%blmty)
        do inodty=1,size(M_bdyty(ibdyty)%blmty(iblmty)%NODES)
       work_i_list(ibdyty)%G_i(M_bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)) =    &
       work_i_list(ibdyty)%G_i(M_bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)) + 1
        enddo
      enddo

      do inodty=1,size(M_bdyty(ibdyty)%nodty)
        G_i_length = work_i_list(ibdyty)%G_i(inodty)

        if (G_i_length /= 0) then
          allocate(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i(G_i_length),stat=errare)
          if (errare /= 0) then
            call FATERR(IAM,'error allocating M_bdyty%nod2blmty%G_i')
          end if
        else 
          nullify(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
!                                       123456789012345678901234567
          write(cout,'(1X,A27,1X,I5)') 'WARNING: MAILx without node',ibdyty
          call LOGMES(cout)
        end if
      enddo

      work_i_list(ibdyty)%G_i = 0         

      do iblmty=1,size(M_bdyty(ibdyty)%blmty)
        do inodty=1,size(M_bdyty(ibdyty)%blmty(iblmty)%NODES)
          t_i= M_bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)

          work_i_list(ibdyty)%G_i(t_i)=work_i_list(ibdyty)%G_i(t_i)+1
          M_bdyty(ibdyty)%nod2blmty(t_i)%G_i(work_i_list(ibdyty)%G_i(t_i))=iblmty

        enddo
      end do

    enddo

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_nb_tacts_MAILx(ibdyty,i4) 
    implicit none
    integer :: ibdyty,i4,errare

    character(len=103) :: cout
                              !123456789012345678901234567890
    character(len=29)  :: IAM='mod_MAILx::set_nb_tacts_MAILx'

    allocate(M_bdyty(ibdyty)%tacty(i4),stat=errare)

    if (errare /= 0) then
      call FATERR(IAM,'error allocating M_bdyty%tacty')
    end if
  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_tact_MAILx(ibdyty,itacty,vec_c5,vec_i4,vec_r8,is_perio,vec_perio) 
    implicit none
    integer :: ibdyty,itacty,errare

    character(len=5)          :: vec_c5(:)
    integer         ,optional :: vec_i4(:)
    real(kind=8)    ,optional :: vec_r8(:)
    integer         ,optional :: is_perio
    real(kind=8)    ,optional :: vec_perio(3)
    
    character(len=103) :: cout
                              !123456789012345678901234567890
    character(len=29)  :: IAM='mod_MAILx::set_nb_tacts_MAILx'

    integer :: sz

    if (present(is_perio) .and. is_perio == 1) then 
       M_bdyty(ibdyty)%tacty(itacty)%is_periodic = .true.
       allocate(M_bdyty(ibdyty)%tacty(itacty)%per_vec(3))
       M_bdyty(ibdyty)%tacty(itacty)%per_vec = vec_perio
    else 
       M_bdyty(ibdyty)%tacty(itacty)%is_periodic = .false.
       nullify(M_bdyty(ibdyty)%tacty(itacty)%per_vec)
    endif

    M_bdyty(ibdyty)%tacty(itacty)%tacID=vec_c5(1)
    M_bdyty(ibdyty)%tacty(itacty)%color=vec_c5(2)

!    print *,vec_c5(2)

    select case (vec_c5(1))
    case ('CLxxx')  
       allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(2))
       allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata(1))

       M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata= vec_i4
       M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata= vec_r8

    case('ALpxx')

       !fp idata(0) = nb elements surfaciques + 1 (== nb noeuds)
       !fp idata(1) = numero 1er noeud
       !fp idata(2:x) = numero noeuds suivants

       sz = size(vec_i4) - 1

       allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(0:sz))
       nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)     

       M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(0)=vec_i4(1)
       M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1:sz)=vec_i4(2:sz+1)

    case( 'ASpxx', 'CSpxx','CSpx0','CSpx1','CSpx2' )

       sz = size(vec_i4) - 1

       allocate(M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(0:sz))
       nullify(M_bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)     

       !fp idata(0) = taille reelle du tableau 
       !fp idata(1:x) = nb noeuds ele 1 + liste noeuds de ele 1 + ... + nb noeuds ele n + liste noeuds ele n

       M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(0)=vec_i4(1)
       M_bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1:sz)=vec_i4(2:sz+1)

    case default
       call faterr(IAM,'unimplemented tacty '//vec_c5(1))
    end select
  end subroutine
!!!------------------------------------------------------------------------
 subroutine get_periodic_MAILx(ibdyty,itacty,ivec_per)

   implicit none
   integer,intent(inout)    :: ibdyty,itacty
   real(kind=8),intent(inout),dimension(3) :: ivec_per

   ivec_per = 0.d0
   select case(M_bdyty(ibdyty)%tacty(itacty)%tacID)

     case('CLxxx','ALpxx','CSpxx','CSpx0','CSpx1','CSpx2','ASpxx')

       if( M_bdyty(ibdyty)%tacty(itacty)%is_periodic) ivec_per=M_bdyty(ibdyty)%tacty(itacty)%per_vec
     case default  
       call faterr('MAILx::get_periodic','unknown boundary type: '//M_bdyty(ibdyty)%tacty(itacty)%tacID)
   end select

 end subroutine

!!! melimelo styled functions
!!! beaucoup pompees sur les fonction au dessus...

  subroutine set_nb_bulks_of_MAILx(bdy,nb_bulks)
    implicit none
    type(T_MAILx), pointer :: bdy
    integer :: nb_bulks
    !                         !12345678901234567890123456789012
    character(len=32)  :: IAM='mod_MAILx::set_nb_bulks_of_MAILx'
    integer :: iblmty, errare

    print*,'try to create ',nb_bulks,' blmty for a MAILx'

    allocate(bdy%blmty(nb_bulks),stat=errare)
    if (errare /= 0) then
      call FATERR(IAM,'error allocating bdy%blmty')
    end if

    do iblmty=1,nb_bulks
      nullify(bdy%blmty(iblmty)%model)
      nullify(bdy%blmty(iblmty)%behav)

       bdy%blmty(iblmty)%is_meca_present=.false. 
       nullify(bdy%blmty(iblmty)%meca_gpv)

       bdy%blmty(iblmty)%is_ther_present=.false. 
       nullify(bdy%blmty(iblmty)%ther_gpv)
    enddo

    print*,'succeed ! '

  end subroutine

  subroutine set_nb_nodes_of_MAILx(bdy,nb_nodes)
    implicit none
    type(T_MAILx), pointer :: bdy
    integer :: nb_nodes
    !                         !12345678901234567890123456789
    character(len=29)  :: IAM='mod_MAILx::set_nb_nodes_MAILx'
    character(len=103) :: cout
    integer :: inodty, errare


    print*,'try to set ',nb_nodes, 'nodes for a MAILx'

    if (nb_nodes /= 0) then
      allocate(bdy%nodty(nb_nodes),stat=errare)
      if (errare /= 0) then
        call FATERR(IAM,'error allocating bdy%nodty')
      end if


      allocate(bdy%nod2blmty(nb_nodes),stat=errare)
      if (errare /= 0) then
        call FATERR(IAM,'error allocating bdyty%nod2blmty')
      end if
       
    else
      nullify(bdy%nodty)
      nullify(bdy%nod2blmty)
!                             1234567890123456789012345678
      write(cout,'(1X,A28)') 'WARNING: MAILx without nodes'
      call LOGMES(cout)
    end if      

    print *,'succeed !'

  end subroutine

  subroutine set_bulk_of_MAILx(bdy,iblmty,type, model, behav, connec)
    implicit none
    type(T_MAILx), pointer :: bdy
    integer :: iblmty, connec(:)
    character(len=5) :: type, model, behav
    !                         !1234567890123456789012345678
    character(len=28)  :: IAM='mod_MAILx::set_bulk_of_MAILx'
    character(len=103) :: cout
    integer :: sz, errare

    print*,'try to set blmty',iblmty,' of a MAILx'

    allocate(bdy%blmty(iblmty)%model(1), &
             bdy%blmty(iblmty)%behav(1) )
    bdy%blmty(iblmty)%model(1)=model
    bdy%blmty(iblmty)%behav(1)=behav

    sz = size(connec)

    if (sz /= 0) then
      allocate(bdy%blmty(iblmty)%NODES(sz),stat=errare)
      if (errare /= 0) then
        call FATERR(IAM,'error allocating bdy%blmty%NODES')
      end if

      bdy%blmty(iblmty)%NODES = connec

      bdy%blmty(iblmty)%blmID=type

    else
      nullify(bdy%blmty(iblmty)%NODES)
      write(cout,'(1X,A28)') 'WARNING: MAILx without nodes'
      call LOGMES(cout)
    end if

    print*,'succeed ! '

  end subroutine
!!!------------------------------------------------------------------------

  subroutine set_node_of_MAILx(bdy, inodty, type, coor, visible)
    implicit none
    type(T_MAILx), pointer :: bdy
    integer                :: inodty
    character(len=5)       :: type
    real(kind=8)           :: coor(:)
    logical                :: visible
    !                         !1234567890123456789012345678
    character(len=28)  :: IAM='mod_MAILx::set_node_of_MAILx'
    character(len=103) :: cout
    TYPE(G_i_list),DIMENSION(:),POINTER :: work_i_list
    integer :: iblmty,nbn,t_i,errare
    integer :: iccdof,idof
    integer :: G_i_length

    print*,'try to set node of a MAILx'

    ! this allocation cannot be done in set_nb_nodes...
    if( .not. associated(bdy%cooref) ) then
      if (type == 'NO2xx') then
        allocate(bdy%cooref(2*size(bdy%nodty)),stat=errare)
        allocate(bdy%coor  (2*size(bdy%nodty)),stat=errare)
        if (errare /= 0) then
          call FATERR(IAM,'error allocating bdy%cooref')
        end if
      else if (type == 'NO3xx') then
        allocate(bdy%cooref(3*size(bdy%nodty)),stat=errare)
        allocate(bdy%coor  (3*size(bdy%nodty)),stat=errare)
        if (errare /= 0) then
          call FATERR(IAM,'error allocating bdy%cooref')
        end if
      else
        call FATERR(IAM,'dime unsupported')
      endif
    endif

    call new_nodty(bdy%nodty(inodty),type)

    if( type == 'NO2xx' ) then
      bdy%cooref(2*inodty-1:2*inodty) = coor(:)
      bdy%coor(2*inodty-1:2*inodty)   = coor(:)
    else if( type == 'NO3xx' ) then
      bdy%cooref(3*inodty-2:3*inodty) = coor(:)
      bdy%coor(3*inodty-2:3*inodty)   = coor(:)
    end if

  end subroutine

!!!------------------------------------------------------------------------
!!!
!!! DA : gestion de la bd des champs par elements des milieu poreu
!!!
!!!------------------------------------------------------------------------

  SUBROUTINE init_porogpv_MAILx(ibdyty,iblmty,nb_gp_meca,nb_gp_ther,nb_external,nb_internal,&
                                nb_fields,field_name,nb_vfields,vfield_name,vsize)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, nb_gp_meca, nb_gp_ther, ig
    INTEGER :: nb_external, nb_internal

    integer                       ,optional :: nb_fields
    character(len=30),dimension(:),optional :: field_name

    integer                       ,optional :: nb_vfields,vsize
    character(len=30),dimension(:),optional :: vfield_name

    M_bdyty(ibdyty)%is_poro=.TRUE.
    M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present=.TRUE.

    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(nb_gp_meca,2))
    ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(nb_gp_ther,2))

    DO ig=1,nb_gp_meca

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress=0.d0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain=0.d0
       !
       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress=0.d0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain=0.d0
       
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal)
       !
       IF (nb_internal /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal = 0.d0 

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal = 0.d0
       ELSE
          
          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal)
          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal)
          
       ENDIF
       
       !
       ! coupled fields
       
       if ( .not. present(nb_fields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name(nb_fields)  )
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value(nb_fields) )
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_value(nb_fields) )

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_name)

          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name = field_name

       endif

       if (.not. present(nb_vfields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%vfield_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_name(nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_value(vsize,nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%vfield_value(vsize,nb_vfields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%vfield_name)

          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_name = vfield_name

       end if

    END DO
    

    ! le nb_external pour la meca ne peut pas etre celui du ther ...
    ! ... donc on cache ici qq chose de tres laid; par exemple grad et flux sont tjs de taille 3 !!

    DO ig=1,nb_gp_ther

       nb_external = 3 !<- fd c est debile mais actuellement en thermique tout a la dim 3 (voir models)!!
       nb_internal = 0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad=0.d0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux=0.d0
       !
       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad=0.d0

       ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux(nb_external))
       M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux=0.d0
       
       NULLIFY(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal)
       !
       IF (nb_internal /= 0) THEN
          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal = 0.d0 

          ALLOCATE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal(nb_internal))
          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal = 0.d0
       ELSE
           nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal)
           nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal)
       ENDIF
       
       !
       ! coupled fields

       if ( .not. present(nb_fields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%field_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%field_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_name(nb_fields) )
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_value(nb_fields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%field_value(nb_fields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%field_name)

          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_name = field_name

       endif

       if (.not. present(nb_vfields)) then 

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_value)

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%vfield_name, &
                  M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%vfield_value)
       else

          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_name(nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_value(vsize,nb_vfields))
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%vfield_value(vsize,nb_vfields))

          nullify(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%vfield_name)

          M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_name = vfield_name

       end if

    END DO

  END SUBROUTINE init_porogpv_MAILx
!!!-------------------------------------------------------------------------
  SUBROUTINE update_porogpv_MAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty

    INTEGER ::  iblmty,ig

    !fd on doit rajouter un paranoiac test pour savoir si les elements sont bien meca, URGENT

   DO iblmty=1,SIZE(M_bdyty(ibdyty)%blmty)

      if (.not. M_bdyty(ibdyty)%blmty(iblmty)%is_poro_present) cycle

      DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(:,1),dim=1)

         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress 
         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain 

         IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal)) & 
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal= &
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal

         IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value)) & 
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_value= &
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value

         ! on n'update pas field_name car ne semble pas avoir de sens
        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_value)) & 
             M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%vfield_value= &
             M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_value 

      END DO
      
      DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(:,1),dim=1)

         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux
         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad 

         IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal)) & 
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal= &
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal 

         IF (ASSOCIATED(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_value)) &
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%field_value= &
              M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_value 
      
        if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_value)) & 
             M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%vfield_value= &
             M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_value 
      
      END DO
   END DO

  END SUBROUTINE update_porogpv_MAILx

!!!---------------------------------------------------------------------------

!!!---------------------------------------------------------------------------

  SUBROUTINE get_poro_field_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER      :: ibdyty, iblmty, ig, if
    REAL(kind=8) :: s

    s=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value(if)

  END SUBROUTINE


!!!---------------------------------------------------------------------------
  SUBROUTINE get_poro_field_begin_MAILx(ibdyty,iblmty,ig,if,s)

    IMPLICIT NONE

    INTEGER      :: ibdyty, iblmty, ig, if
    REAL(kind=8) :: s

    s=M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%field_value(if)

  END SUBROUTINE
  
!!!---------------------------------------------------------------------------
  SUBROUTINE get_poro_field_name_MAILx(ibdyty,iblmty,ig,if,field_name)

    IMPLICIT NONE

    INTEGER           :: ibdyty, iblmty, ig, if
    character(len=30) :: field_name

    if ( .not. associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)) then
       call faterr('MAILx::get_poro_field_name','no poro field')
    end if

    field_name = M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name(if)

  END SUBROUTINE 
  
  !!!---------------------------------------------------------------------------
  integer function get_poro_field_size_MAILx(ibdyty,iblmty,ig)

    IMPLICIT NONE

    INTEGER :: ibdyty, iblmty, ig

    if (associated(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)) then

       get_poro_field_size_MAILx=size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_name)
    else

       get_poro_field_size_MAILx= 0
    end if

  END function 
  
!!!---------------------------------------------------------------------------
  !> set the value of the field
  !> 
  SUBROUTINE set_poro_field_MAILx(ibdyty,iblmty,ig,if,s,phys)

    IMPLICIT NONE

    INTEGER          :: ibdyty, iblmty, ig, if
    REAL(kind=8)     :: s
    character(len=4) :: phys

    SELECT CASE(phys)
    CASE('MECA')
         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%field_value(if)=s
    CASE('THER')
         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%field_value(if)=s
    CASE DEFAULT
         PRINT*,'Mauvaise physique demandee'
         call faterr('MAILx::set_poro_field','wrong physic asked: '//phys)
    END SELECT

  END SUBROUTINE
!!!---------------------------------------------------------------------------
  !> get the rank of a field 
  !> assumes that the fields are the same on all gp
  !
  integer function get_poro_field_rank_MAILx(ibdyty,iblmty,name)

    IMPLICIT NONE

    INTEGER          :: ibdyty, iblmty
    character(len=*) :: name

    integer          :: if

    get_poro_field_rank_MAILx= 0

    do if=1,size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%field_name)
      if (M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(1,1)%field_name(if)==name) then
        get_poro_field_rank_MAILx=if
        exit
      endif
    enddo
    !do if=1,size(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%field_name)
     ! if (M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(1,1)%field_name(if)==name) then
      !  get_poro_field_rank_MAILx=if
       ! exit
      !endif
    !enddo

  END function
!!!---------------------------------------------------------------------------
  !> set the value of a vector field
  subroutine set_poro_vfield_MAILx(ibdyty,iblmty,ig,if,s,sd,phys)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> gauss point index
    integer(kind=4), intent(in) :: ig
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> size of vector field
    integer(kind=4), intent(in) :: sd
    !> new vector field values
    real(kind=8), dimension(sd), intent(in) :: s
    !> physic type to set
    character(len=4), intent(in) :: phys

    select CASE(phys)
    case('MECA')
      M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%vfield_value(1:sd,if) = s(1:sd)
    case('THER')
      M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%vfield_value(1:sd,if) = s(1:sd)
    case default
      call logmes('Mauvaise physique demandee')
      call faterr('MAILx::set_poro_field','wrong physic asked: '//phys)
    end select

  end subroutine

!!!------------------------------------------------------------------------
!!! rm : database management for multi-phasic
!!!------------------------------------------------------------------------
  !> Initializing multi_gpv structure
  subroutine init_multigpv_MAILx(ibdyty,iblmty,nb_gp,grad_sizes,internal_size,&
                                 nb_ext_fields,ext_field_names,nb_vfields,vfield_name,vsize)
    implicit none
    !> MAILx id
    integer(kind=4), intent(in) :: ibdyty
    !> bulk element id
    integer(kind=4), intent(in) :: iblmty
    !> number of Gauss points to store
    integer(kind=4), intent(in) :: nb_gp
    !> size of grad/flux fields
    integer(kind=4), dimension(:), intent(in) :: grad_sizes
    !> size of internal field
    integer(kind=4), intent(in) :: internal_size
    !> number of external scalar fields
    integer(kind=4), intent(in), optional :: nb_ext_fields
    !> name of the external scalar fields  (same name for every physics)
    character(len=30),dimension(:),optional :: ext_field_names
    !> number of external vector fields
    integer(kind=4), optional :: nb_vfields
    !> name of the external vector fields  (same name for every physics)
    character(len=30),dimension(:),optional :: vfield_name
    !> maximum size of the vector fields
    integer(kind=4), optional :: vsize
    !**
    integer(kind=4) :: i_td, nb_physics, i_phys, grad_size

    M_bdyty(ibdyty)%is_multi = .true.
    M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present = .true.

    ! filling ccgrad map
    nb_physics = size(grad_sizes)
    allocate(M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(nb_physics+1))
    M_bdyty(ibdyty)%blmty(iblmty)%ccgrad = 0
    do i_phys = 1,  nb_physics
      M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(i_phys+1) = M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(i_phys) + grad_sizes(i_phys)
    end do
    grad_size = sum(grad_sizes)

    allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(2))

    do i_td = 1, 2

      allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%grad(grad_size,nb_gp))
      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%grad = 0.d0

      allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%flux(grad_size,nb_gp))
      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%flux = 0.d0

      nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%internal)
      !
      if (internal_size /= 0) then
        allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%internal(internal_size,nb_gp))
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%internal = 0.d0
      end if
     
      !
      ! external (coupled?) fields
      if ( .not. present(nb_ext_fields)) then 
        nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_name)
        nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value)
      else
        if (i_td==1) then
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_name(nb_ext_fields))
          M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_name = ext_field_names
        else
          nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_name)
        end if

        allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(nb_gp,nb_ext_fields))
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value = 0.d0
      endif

      if (.not. present(nb_vfields)) then 
        nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_name)
        nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value)
      else
        if (i_td==1) then
          allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_name(nb_vfields))
          M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_name = vfield_name
        else
          nullify(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_name)
        end if

         allocate(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value(vsize,nb_gp,nb_vfields))
         M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value = 0.d0
      end if

    end do
    
  end subroutine init_multigpv_MAILx

  !> \brief Update the multigpv state
  subroutine update_multigpv_MAILx(ibdyty)
    implicit none
    !> MAILx index
    integer(kind=4), intent(in) :: ibdyty
    !
    integer(kind=4) :: iblmty

    do iblmty = 1, size(M_bdyty(ibdyty)%blmty)

      if (.not. M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) cycle

      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(2)%grad = M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%grad
      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(2)%flux = M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%flux

      if (associated(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%internal)) & 
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(2)%internal = &
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%internal 

      if (associated(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%field_value)) & 
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(2)%field_value = &
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%field_value 

      if (associated(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%vfield_value)) & 
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(2)%vfield_value= &
        M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%vfield_value 

    end do

  end subroutine update_multigpv_MAILx

  !> \brief get the rank of a field 
  !> assumes that the fields are the same on all gp
  integer function get_multi_field_rank_MAILx(i_bdyty,i_blmty,name)
    implicit none
    !> MAILx index
    integer(kind=4) , intent(in) :: i_bdyty
    !> bulk element index
    integer(kind=4) , intent(in) :: i_blmty
    !> name of the desired field
    character(len=*), intent(in) :: name
    !
    integer(kind=4):: if

    get_multi_field_rank_MAILx = 0

    do if = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(1)%field_name)
      if (M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(1)%field_name(if)==name) then
        get_multi_field_rank_MAILx = if
        return
      endif
    enddo

  end function

  !> \brief get the value of grad field of a multi element
  subroutine get_multi_grad_MAILx(ibdyty,iblmty,i_phys,i_td,s)
    implicit none
    !> MAILx body number
    integer(kind=4), intent(in) :: ibdyty
    !> MAILx element number
    integer(kind=4), intent(in) :: iblmty
    !> physic id
    integer(kind=4), intent(in) :: i_phys
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> grad values
    real(kind=8), dimension(:,:), intent(out) :: s
    !
    integer(kind=4) :: nb_gp, g_dim, shift

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then
      g_dim = size(s,dim=1)
      nb_gp = size(s,dim=2)

      shift = M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(i_phys)

      s = M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%grad(shift+1:shift+g_dim,1:nb_gp)

    end if

  end subroutine

  !> \brief get the value of flux field of a multi element
  subroutine get_multi_flux_MAILx(ibdyty,iblmty,i_phys,i_td,s)
    implicit none
    !> MAILx body number
    integer(kind=4), intent(in) :: ibdyty
    !> MAILx element number
    integer(kind=4), intent(in) :: iblmty
    !> physic id
    integer(kind=4), intent(in) :: i_phys
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> flux values
    real(kind=8), dimension(:,:), intent(out) :: s
    !
    integer(kind=4) :: nb_gp, g_dim, shift

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then

      g_dim = size(s,dim=1)
      nb_gp = size(s,dim=2)

      ! same map for grad and flux...
      shift = M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(i_phys)

      s = M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%flux(shift+1:shift+g_dim,1:nb_gp)

    end if

  end subroutine

  !> \brief set the value of grad field of a multi element
  subroutine set_multi_grad_MAILx(ibdyty,iblmty,i_phys,i_td,s)
    implicit none
    !> MAILx body number
    integer(kind=4), intent(in) :: ibdyty
    !> MAILx element number
    integer(kind=4), intent(in) :: iblmty
    !> physic id
    integer(kind=4), intent(in) :: i_phys
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> new grad values
    real(kind=8), dimension(:,:), intent(in) :: s
    !
    integer(kind=4) :: nb_gp, g_dim, shift

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then

      g_dim = size(s,dim=1)
      nb_gp = size(s,dim=2)

      shift = M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(i_phys)

      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%grad(shift+1:shift+g_dim,1:nb_gp) = s

    end if

  end subroutine

  !> \brief set the value of flux field of a multi element
  subroutine set_multi_flux_MAILx(ibdyty,iblmty,i_phys,i_td,s)
    implicit none
    !> MAILx body number
    integer(kind=4), intent(in) :: ibdyty
    !> MAILx element number
    integer(kind=4), intent(in) :: iblmty
    !> physic id
    integer(kind=4), intent(in) :: i_phys
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> new flux values
    real(kind=8), dimension(:,:), intent(in) :: s
    !
    integer(kind=4) :: nb_gp, g_dim, shift

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then

      g_dim = size(s,dim=1)
      nb_gp = size(s,dim=2)

      ! same map for grad and flux...
      shift = M_bdyty(ibdyty)%blmty(iblmty)%ccgrad(i_phys)

      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%flux(shift+1:shift+g_dim,1:nb_gp) = s

    end if

  end subroutine

  !> \brief get the value of a field of a multi element
  subroutine get_multi_field_MAILx(ibdyty,iblmty,i_td,if,s)
    implicit none
    !> MAILx body number
    integer(kind=4), intent(in) :: ibdyty
    !> MAILx element number
    integer(kind=4), intent(in) :: iblmty
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> desired field index
    integer(kind=4), intent(in) :: if
    !> desired field value
    real(kind=8), dimension(:), intent(out) :: s
    character(len=22) :: IAM='MAILx::get_multi_field'

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then
      if (size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if))<size(s)) then
        print *, size(s), size( M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if))
        call faterr(IAM,'size mismatch')
      end if
      !M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if)=0.d0
      s = 0.d0
      s = M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(1:size(s),if)
    end if

  end subroutine

  !> \brief set the value of a field of a multi element
  subroutine set_multi_field_MAILx(ibdyty,iblmty,i_td,if,s)
    implicit none
    !> MAILx body number
    integer(kind=4), intent(in) :: ibdyty
    !> MAILx element number
    integer(kind=4), intent(in) :: iblmty
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> desired field index
    integer(kind=4), intent(in) :: if
    !> new desired field value
    real(kind=8), dimension(:), intent(in) :: s
    character(len=22) :: IAM='MAILx::set_multi_field'
    
    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then
      if (size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if))<size(s)) then
        print *, size(s), size( M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if))
        call faterr(IAM,'size mismatch')
      endif  
      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if)=0.d0
      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(1:size(s),if) = s
    end if

  end subroutine
!!!---------------------------------------------------------------------------
  !> get the rank of a vector field 
  !> assumes that the fields are the same on all gp
  integer function get_multi_vfield_rank_MAILx(ibdyty,iblmty,name)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> name of vector field
    character(len=*), intent(in) :: name
    !
    integer(kind=4) :: if

    get_multi_vfield_rank_MAILx = 0

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then
      do if = 1, size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%vfield_name)
        if (M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%vfield_name(if)==name) then
          get_multi_vfield_rank_MAILx = if
          exit
        end if
      end do
    end if

  end function
!!!---------------------------------------------------------------------------
  !> get the maximum size  of a vector field 
  integer(kind=4) function get_multi_vfield_max_size_MAILx(ibdyty,iblmty)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty

    get_multi_vfield_max_size_MAILx = 0

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then

      get_multi_vfield_max_size_MAILx = size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(1)%vfield_value,1)

    end if

  end function
!!!---------------------------------------------------------------------------
  !> set the value of a vector field
  subroutine set_multi_vfield_MAILx(ibdyty,iblmty,i_td,if,ic,s)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> vector field componenent to set
    integer(kind=4), intent(in) :: ic
    !> new vector field values
    real(kind=8), dimension(:), intent(in) :: s

    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then
      if (size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if))<size(s)) then
        print *, size(s), size( M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value(:,:,if))
        call faterr('MAILx::set_multi_vfield','size mismatch')
      endif  
      M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value(ic,1:size(s),if) = s
    end if

  end subroutine
!!!---------------------------------------------------------------------------
  !> get the value of a a vector field
  subroutine get_multi_vfield_MAILx(ibdyty,iblmty,i_td,if,s)
    implicit none
    !> mailx index
    integer(kind=4), intent(in) :: ibdyty
    !> element index
    integer(kind=4), intent(in) :: iblmty
    !> time depth
    integer(kind=4), intent(in) :: i_td
    !> vector field index
    integer(kind=4), intent(in) :: if
    !> vector field values
    real(kind=8), dimension(:,:), intent(out) :: s
    
    if (M_bdyty(ibdyty)%blmty(iblmty)%is_multi_present) then
      if (size(M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%field_value(:,if))<size(s)) then
        print *, size(s), size( M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value(:,:,if))
        call faterr('MAILx::get_multi_vfield','size mismatch')
      endif  
      s(:,:) = M_bdyty(ibdyty)%blmty(iblmty)%multi_gpv(i_td)%vfield_value(1:size(s,1),1:size(s,2),if)
    end if

  end subroutine

!!!---------------------------------------------------------------------------

  !> initialize nodal fields for one MAILx
  subroutine init_nodal_fields_MAILx(ibdyty,nb_nodal_fields) 
     implicit none
     integer, intent(in) :: ibdyty,nb_nodal_fields
     integer             :: i

     allocate(M_bdyty(ibdyty)%nodal_fields(nb_nodal_fields)) 
     do i=1,nb_nodal_fields   
       M_bdyty(ibdyty)%nodal_fields(i)%name = ''
       nullify(M_bdyty(ibdyty)%nodal_fields(i)%values)
     enddo
  end subroutine

!--------------------------------------------------------------
  
  subroutine init_nodal_field_MAILx(ibdyty,name,rank,sz) 
     implicit none
     character(len=30) :: name
     integer, intent(in) :: ibdyty,rank,sz

     allocate(M_bdyty(ibdyty)%nodal_fields(rank)%values(sz*size(M_bdyty(ibdyty)%nodty))) 
     M_bdyty(ibdyty)%nodal_fields(rank)%name = name
     M_bdyty(ibdyty)%nodal_fields(rank)%dim1 = sz
     M_bdyty(ibdyty)%nodal_fields(rank)%values = 0.d0

  end subroutine

!--------------------------------------------------------------
  
  function get_nodal_field_rank_MAILx(ibdyty,name)
     implicit none
     character(len=30) :: name
     integer, intent(in) :: ibdyty
     integer             :: get_nodal_field_rank_MAILx,i
    
     get_nodal_field_rank_MAILx=0
     do i=1,size(M_bdyty(ibdyty)%nodal_fields)  
       if (M_bdyty(ibdyty)%nodal_fields(i)%name == name) get_nodal_field_rank_MAILx=i
     enddo
  end function

!--------------------------------------------------------------
  
  subroutine set_nodal_field_MAILx(ibdyty,rank,field) 
     implicit none
     character(len=30) :: name
     integer, intent(in) :: ibdyty,rank
     real(kind=8),dimension(:) :: field 

     !integer :: j,dim1

     M_bdyty(ibdyty)%nodal_fields(rank)%values = field

     !dim1 = M_bdyty(ibdyty)%nodal_fields(rank)%dim1

  end subroutine

!--------------------------------------------------------------
  
  function get_nodal_field_nodty_MAILx(ibdyty,inodty,sz,rank)
     implicit none

     integer, intent(in) :: ibdyty,inodty,sz,rank
     real(kind=8),dimension(sz) :: get_nodal_field_nodty_MAILx
     integer :: idof

     idof = M_bdyty(ibdyty)%nodal_fields(rank)%dim1*(inodty - 1)
     get_nodal_field_nodty_MAILx = M_bdyty(ibdyty)%nodal_fields(rank)%values(idof+1 : idof+sz)

     !print*,rank,inodty
     !print*,sz,inodty,get_nodal_field_nodty_MAILx


  end function

!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_model_type_MAILx(iM_bdyty)

    IMPLICIT NONE
    INTEGER :: iM_bdyty

    IF (M_bdyty(iM_bdyty)%is_meca) THEN
        get_model_type_MAILx = 'MECAx'
    ENDIF
    IF (M_bdyty(iM_bdyty)%is_ther) THEN
        get_model_type_MAILx = 'THERx'
    ENDIF
    IF (M_bdyty(iM_bdyty)%is_poro) THEN
        get_model_type_MAILx = 'POROx'
    ENDIF

  END FUNCTION get_model_type_MAILx

!--------------------------------------------------------------
  
  !> Get max_nod2el attribut of a MAILx body
  function get_max_nod2el(ibdyty)
    implicit none
    !> index of MAILx
    integer(kind=4), intent(in) :: ibdyty
    !> value of max_nod2el attribut
    integer(kind=4) :: get_max_nod2el

    get_max_nod2el = M_bdyty(ibdyty)%max_nod2el
  end function

!--------------------------------------------------------------
  
  FUNCTION get_tacty2blmty_MAILx(ibdyty,nodeA,nodeB)
  
  ! Fonction pour obtenir l'element accroche a un CLxxx ou ALpxx
  
    IMPLICIT NONE
    INTEGER :: ibdyty,nodeA,nodeB
    INTEGER :: get_tacty2blmty_MAILx
    
    INTEGER :: iblmty_A,iblmty_B,blmty_A,blmty_B
    LOGICAL :: found

                             !1234567890123456789012    
    character(len=22) :: IAM='MAILx::get_tacty2blmty' 

    if ( nbdime /= 2 ) call faterr(IAM,'2D function') 
    
    found = .FALSE.
    get_tacty2blmty_MAILx = 0
    
    DO iblmty_A=1,size(M_bdyty(ibdyty)%nod2blmty(nodeA)%G_i)
        blmty_A = M_bdyty(ibdyty)%nod2blmty(nodeA)%G_i(iblmty_A)
        DO iblmty_B=1,size(M_bdyty(ibdyty)%nod2blmty(nodeB)%G_i)
            blmty_B = M_bdyty(ibdyty)%nod2blmty(nodeB)%G_i(iblmty_B)
            IF (blmty_A==blmty_B) THEN
                get_tacty2blmty_MAILx = blmty_A
                found = .TRUE.
                EXIT
            ENDIF
        ENDDO
        IF (found) EXIT
    ENDDO
    
  END FUNCTION get_tacty2blmty_MAILx

!--------------------------------------------------------------  

  ! Fonction pour obtenir l'element accroche a un CLxxx ou ALpxx
  function get_blmty2edge_MAILx(ibdyty, iblmty, nodeA, nodeB)
    implicit none
    integer, intent(in) :: ibdyty,iblmty,nodeA,nodeB
    integer             :: get_blmty2edge_MAILx
    ! local variables
    integer :: in, jn, nbn
    logical :: found
                             !123456789012345678901    
    character(len=21) :: IAM='MAILx::get_blmty2edge' 

    if ( nbdime /= 2 ) call faterr(IAM,'2D function') 
    
    found = .FALSE.
    get_blmty2edge_MAILx = 0

    nbn = size(M_bdyty(ibdyty)%blmty(iblmty)%nodes)
    in=maxloc(M_bdyty(ibdyty)%blmty(iblmty)%nodes,dim=1,mask=M_bdyty(ibdyty)%blmty(iblmty)%nodes == nodeA)
    jn=maxloc(M_bdyty(ibdyty)%blmty(iblmty)%nodes,dim=1,mask=M_bdyty(ibdyty)%blmty(iblmty)%nodes == nodeB)

    if (in == 0 .or. jn == 0) return

    !fd on est dans le bon sens
    if (M_bdyty(ibdyty)%blmty(iblmty)%nodes(jn) == M_bdyty(ibdyty)%blmty(iblmty)%nodes(modulo(in,nbn)+1)) get_blmty2edge_MAILx = in

    !fd on est en sens inverse 
    if (M_bdyty(ibdyty)%blmty(iblmty)%nodes(in) == M_bdyty(ibdyty)%blmty(iblmty)%nodes(modulo(jn,nbn)+1)) get_blmty2edge_MAILx = -jn

    
  end function get_blmty2edge_MAILx

!--------------------------------------------------------------

  subroutine get_edge_MAILx(ibdyty, nodeA, nodeB, iblmty, iedge)
    implicit none
    integer, intent(in)  :: ibdyty, nodeA, nodeB
    integer, intent(out) :: iblmty, iedge
    character(len=128)   :: cout

                             !123456789012345    
    character(len=15) :: IAM='MAILx::get_edge' 
    
    iblmty = get_tacty2blmty_MAILx(ibdyty,nodeA,nodeB)
    
    if (iblmty == 0) then
       write(cout,'("body ",I0," beginning node ",I0," end node ",I0)') ibdyty,nodeA,nodeB
       call logmes(cout)
       ! call faterr(IAM,'unable to find element')       
       call logmes(IAM//'unable to find element')
       iedge = 0
       return
    endif
    
    iedge = get_blmty2edge_MAILx(ibdyty,iblmty,nodeA,nodeB)

    if (iedge == 0) then
       write(cout,'(A,I0,A,I0)') 'body ',ibdyty,' element ',iblmty
       call logmes(cout)       
       write(cout,'(A,I0,I0)')   'looking for ',nodeA,nodeB
       call logmes(cout)
       write(cout,'(A,I0)')      'in ',M_bdyty(ibdyty)%blmty(iblmty)%nodes
       call logmes(cout)       
       ! call faterr(IAM,'unable to find edge of element')
       call logmes(IAM//' unable to find edge of element')       
    endif
  end subroutine


  subroutine get_blmty_from_nodes_MAILx(ibdyty, inodes, iblmty)
    implicit none
    integer,               intent(in)  :: ibdyty
    integer, dimension(:), intent(in)  :: inodes
    integer,               intent(out) :: iblmty
    !
    logical :: found
    integer :: i_node, i_b, i_n, i_n_test
    integer, dimension(:), pointer :: n2b, n2b_test

    i_node =  inodes(1)
    n2b    => M_bdyty(ibdyty)%nod2blmty(i_node)%G_i
    do i_b = 1, size(n2b)
        iblmty = n2b(i_b)
        found = .true.
        do i_n = 2, size(inodes)
            i_n_test = inodes(i_n)
            n2b_test => M_bdyty(ibdyty)%nod2blmty(i_n_test)%G_i
            if( .not. any( n2b_test == iblmty ) ) then
                found = .false.
                exit
            end if
        end do
        if( found ) then
          return
        end if
    end do

    iblmty = 0

  end subroutine

!--------------------------------------------------------------

  subroutine clean_memory_MAILx()
    implicit none
    integer(kind=4)   :: i_bdyty, i_blmty, i_nodty, i_tacty, ip, i, j
    character(len=80) :: cout

    if( .not. allocated(M_bdyty) ) return

    nb_MAILx = 0

    do i_bdyty = 1, size(M_bdyty)

     if( associated(M_bdyty(i_bdyty)%blmty) ) then

       do i_blmty = 1, size(M_bdyty(i_bdyty)%blmty)

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%NODES) ) then  
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%NODES)
           M_bdyty( i_bdyty )%blmty( i_blmty )%NODES => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%model) ) then
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%model)
           M_bdyty( i_bdyty )%blmty( i_blmty )%model => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%behav) ) then
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%behav)
           M_bdyty( i_bdyty )%blmty( i_blmty )%behav => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv) ) then

           do j = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv,2)
             do i = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv,1)

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%stress) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%stress)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%stress => null( )
               end if

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%strain) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%strain)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%strain => null( )
               end if

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%internal) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%internal)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%internal => null( )
               end if

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%field_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%field_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%field_name => null( )
               end if

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%field_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%field_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%field_value => null( )
               end if

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%vfield_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%vfield_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%vfield_name => null( )
               end if

               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%vfield_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv(i,j)%vfield_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv( i, j )%vfield_value => null( )
               end if

             end do
           end do

           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%meca_gpv)
           M_bdyty( i_bdyty )%blmty( i_blmty )%meca_gpv => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv) ) then
           do j = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv,2)
             do i = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv,1)
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%grad) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%grad)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%grad => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%flux) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%flux)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%flux => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%internal) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%internal)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%internal => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%field_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%field_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%field_name => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%field_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%field_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%field_value => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%vfield_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%vfield_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%vfield_name => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%vfield_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv(i,j)%vfield_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv( i, j )%vfield_value => null( )
               end if
             end do
           end do
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ther_gpv)
           M_bdyty( i_bdyty )%blmty( i_blmty )%ther_gpv => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv) ) then
           do j = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv,2)
             do i = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv,1)
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv(i,j)%grad) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv(i,j)%grad)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%thmc_gpv( i, j )%grad => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv(i,j)%flux) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv(i,j)%flux)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%thmc_gpv( i, j )%flux => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv(i,j)%internal) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv(i,j)%internal)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%thmc_gpv( i, j )%internal => null( )
               end if
             end do
           end do
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%thmc_gpv)
           M_bdyty( i_bdyty )%blmty( i_blmty )%thmc_gpv => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca) ) then
           do j = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca,2)
             do i = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca,1)
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%stress) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%stress)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%stress => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%strain) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%strain)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%strain => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%grad) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%grad)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j)%grad => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%flux) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%flux)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%flux => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%internal) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%internal)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%internal => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%field_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%field_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%field_name => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%field_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%field_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%field_value => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%vfield_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%vfield_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%vfield_name => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%vfield_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca(i,j)%vfield_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca( i, j )%vfield_value => null( )
               end if
             end do
           end do
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_meca)
           M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_meca => null( )
         end if

         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther) ) then
           do j = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther,2)
             do i = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther,1)
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%stress) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%stress)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%stress => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%strain) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%strain)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%strain => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%grad) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%grad)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%grad => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%flux) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%flux)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%flux => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%internal) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%internal)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%internal => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%field_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%field_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%field_name => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%field_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%field_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%field_value => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%vfield_name) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%vfield_name)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%vfield_name => null( )
               end if
               if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%vfield_value) ) then
                 deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther(i,j)%vfield_value)
                 M_bdyty( i_bdyty )%blmty( i_blmty )%poro_gpv_ther( i, j )%vfield_value => null( )
               end if
             end do
           end do
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther)
           M_bdyty(i_bdyty)%blmty(i_blmty)%poro_gpv_ther => null( )
         end if

         if( allocated(M_bdyty(i_bdyty)%blmty(i_blmty)%ccgrad) ) deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%ccgrad)
         if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv) ) then
           do i = 1, size(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv)
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%grad) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%grad) 
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%grad => null( )
             end if
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%flux) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%flux) 
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%flux => null( )
             end if
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%internal) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%internal) 
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%internal => null( )
             end if
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%field_name) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%field_name)
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%field_name => null( )
             end if
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%field_value) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%field_value)
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%field_value => null( )
             end if
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%vfield_name) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%vfield_name)
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%vfield_name => null( )
             end if
             if( associated(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%vfield_value) ) then
               deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv(i)%vfield_value)
               M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv( i )%vfield_value => null( )
             end if
           end do
           deallocate(M_bdyty(i_bdyty)%blmty(i_blmty)%multi_gpv)
           M_bdyty( i_bdyty )%blmty( i_blmty )%multi_gpv => null( )
         end if

       end do
       deallocate(M_bdyty(i_bdyty)%blmty)
       M_bdyty( i_bdyty )%blmty => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%nodty) ) then 
        deallocate(M_bdyty(i_bdyty)%nodty)
        M_bdyty( i_bdyty )%nodty => null( )
     end if
     if( associated(M_bdyty(i_bdyty)%ccdof) ) then 
        deallocate(M_bdyty(i_bdyty)%ccdof)
        M_bdyty( i_bdyty )%ccdof => null( )
     end if
     if( associated(M_bdyty(i_bdyty)%nodnb) ) then 
        deallocate(M_bdyty(i_bdyty)%nodnb)
        M_bdyty( i_bdyty )%nodnb => null( )
     end if
     if( associated(M_bdyty(i_bdyty)%dofnb) ) then 
        deallocate(M_bdyty(i_bdyty)%dofnb)
        M_bdyty( i_bdyty )%dofnb => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%cooref)) then 
        deallocate(M_bdyty(i_bdyty)%cooref)
        M_bdyty( i_bdyty )%cooref => null( )
     end if
     if( associated(M_bdyty(i_bdyty)%coor)  ) then 
        deallocate(M_bdyty(i_bdyty)%coor)
        M_bdyty( i_bdyty )%coor => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%tacty) ) then
       do i_tacty = 1, size(M_bdyty(i_bdyty)%tacty)
         if( associated(M_bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata) ) then
           deallocate(M_bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata)
           M_bdyty( i_bdyty )%tacty( i_tacty )%BDARY%idata => null( )
         end if
         if( associated(M_bdyty(i_bdyty)%tacty(i_tacty)%BDARY%rdata) ) then
           deallocate(M_bdyty(i_bdyty)%tacty(i_tacty)%BDARY%rdata)
           M_bdyty( i_bdyty )%tacty( i_tacty )%BDARY%rdata => null( )
         end if
         if( associated(M_bdyty(i_bdyty)%tacty(i_tacty)%per_vec) ) then
           deallocate(M_bdyty(i_bdyty)%tacty(i_tacty)%per_vec)
           M_bdyty( i_bdyty )%tacty( i_tacty )%per_vec => null( )
         end if
       end do
       deallocate(M_bdyty(i_bdyty)%tacty)
       M_bdyty( i_bdyty )%tacty => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%nod2blmty) ) then
       do i_nodty = 1, size(M_bdyty(i_bdyty)%nod2blmty)
         if( associated(M_bdyty(i_bdyty)%nod2blmty(i_nodty)%G_i) ) then
           deallocate(M_bdyty(i_bdyty)%nod2blmty(i_nodty)%G_i)
           M_bdyty( i_bdyty )%nod2blmty( i_nodty )%G_i => null( )
         end if
       end do
       deallocate(M_bdyty(i_bdyty)%nod2blmty)
        M_bdyty( i_bdyty )%nod2blmty => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%ele2blmty) ) then
       do i_blmty = 1, size(M_bdyty(i_bdyty)%ele2blmty)
         if( associated(M_bdyty(i_bdyty)%ele2blmty(i_blmty)%G_i) ) then
           deallocate(M_bdyty(i_bdyty)%ele2blmty(i_blmty)%G_i)
           M_bdyty( i_bdyty )%ele2blmty( i_blmty )%G_i => null( )
         end if
       end do
       deallocate(M_bdyty(i_bdyty)%ele2blmty)
       M_bdyty( i_bdyty )%ele2blmty => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%nodal_fields) ) then
       do i_nodty = 1, size(M_bdyty(i_bdyty)%nodal_fields)
         if( associated(M_bdyty(i_bdyty)%nodal_fields(i_nodty)%values) ) then
           deallocate(M_bdyty(i_bdyty)%nodal_fields(i_nodty)%values)
           M_bdyty( i_bdyty )%nodal_fields( i_nodty )%values => null( )
         end if
       end do
       deallocate(M_bdyty(i_bdyty)%nodal_fields)
       M_bdyty( i_bdyty )%nodal_fields => null( )
     end if

     if( associated(M_bdyty(i_bdyty)%boundary_elements) ) then
        deallocate(M_bdyty(i_bdyty)%boundary_elements)
        M_bdyty( i_bdyty )%boundary_elements => null( )
     end if

    end do

    deallocate(M_bdyty)

  end subroutine clean_memory_MAILx


END MODULE MAILx
