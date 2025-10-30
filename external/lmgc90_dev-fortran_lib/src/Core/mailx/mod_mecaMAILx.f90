! todo 
! ecrire un document tex !!
!
! mettre au propre
!
! adapter a l'utilisation des fields (reel,vecteur,matrice,tenseur) 
!
! 3 types de routine pour mettre les fields 
!
! bynode -> l'utilisateur via l'api transfert au ele qui interpole au gp
! bygp -> l'utilisateur via l'api transfert au ele qui interpole au gp
! bysub -> l'ele delegue a une routine user qui rend les valeurs au pg

! les frame peuvent etre gere comme des fields 

!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
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

!> manages FE mechanical models: assembling, time integration, etc
MODULE mecaMAILx

  use LMGC90_MPI

  USE overall
  USE utilities
  USE parameters
  !USE timer
  USE algebra
  USE DiscreteGeometry
  USE RigidKinematic

  USE bulk_behaviour
  USE models
  
  USE a_DOF
  
  USE a_EF
  USE a_mecaEF
  
  USE MAILx
  USE externalFEM

  use a_system, only : T_link_connec             , &
                       g_system                  , &
                       initialize_system         , &
                       get_nb_non_zero           , &
                       erase_elementary_matrix   , &
                       add_to_elementary_matrix  , &
                       set_vector                , &
                       assemble_elementary_vector, &
                       multiply_system           , &
                       erase_drvdofs             , &
                       set_drvdofs               , &
                       set_drvvalues             , &
                       solve_system              , &
                       build_system              , &
                       erase_system              , &
                       get_i_indices             , &
                       get_j_indices             , &
                       get_val                   , &
                       get_vector                , &
                       sparse_storage_available

  use MAILx_type, only : T_mecaMAILx

  IMPLICIT NONE
  
  PRIVATE
  
  type( T_mecaMAILx ), dimension(:), allocatable, private, target :: bdyty
  
  ! mapping between local and global bdyty rank
  
  INTEGER,DIMENSION(:),ALLOCATABLE,PRIVATE           :: bdyty2M_bdyty
  
  ! reverse mapping between global and local bdyty,blmty,nodty numbering
  
  TYPE(T_MAILx_2_localMAILx),DIMENSION(:),ALLOCATABLE,PUBLIC :: M2meca 

  !fd 
  INTEGER :: nb_mecaMAILx=0
  ! when abonning new kind of body to entity list
  ! you need to know how many entities already exist  
  INTEGER :: nb_existing_entities     

  ! parametres permettant de stopper un calcul si la vitesse est stabilisee.
  REAL(kind=8)      :: eqs_tol
  INTEGER           :: eqs_ichecktype
  INTEGER,PARAMETER :: iQvlcy = 1 , iMvlcy = 2

  !type de stockage des matrices
  ! i_diagonal, i_sparse, i_band, i_skyline, i_full
  INTEGER :: Matrix_storage = -99
  !type de profil des matrices
  ! i_sym , i_std
  INTEGER :: Matrix_shape = i_sym
  ! pour les matrices denses
  LOGICAL :: with_renum = .TRUE.


  !fd 5/11/09
  ! define the strategy new/re-use of ppset
  ! try to reduce computation time introduced when using a new ppset by gp with matlib  
  LOGICAL :: use_existing_ppset = .TRUE.

  !fd un theta pour la partie rigide de la modelisation
  real(kind=8) :: Rtheta=0.5

  real(kind=8) :: tol_coro=1e-12

  ! PTA: 22/03/2013 euristique pour gerer le repertoire de sauvegarde des matrices preconW_IDmailx
  LOGICAL :: is_first_precon_W_to_be_saved = .TRUE.  

  !fd introducing mass scaling
  real(kind=8) :: mass_scaling=1.d0

  PUBLIC get_nb_mecaMAILx, &
         compute_bulk_mecaMAILx,&
         externalFEM_compute_bulk_mecaMAILx,&
         update_bulk_mecaMAILx, &
         externalFEM_update_bulk_mecaMAILx, &
         compute_free_vlocy_mecaMAILx, &
         externalFEM_compute_free_vlocy_mecaMAILx, &
         assemb_KT_mecaMAILx, &
         apply_drvdof_KT_mecaMAILx, &
         assemb_RHS_mecaMAILx, &
         compute_residue_norm_mecaMAILx, &
         update_dof_mecaMAILx, &
         compute_dof_mecaMAILx, &
         externalFEM_compute_dof_mecaMAILx, &
         increment_mecaMAILx, &
         compute_Fext_mecaMAILx, &
         compute_mass_mecaMAILx, &
         fatal_damping_mecaMAILx,&
         check_equilibrium_state_mecaMAILx, &
         set_data_equilibrium_mecaMAILx, &
         read_in_driven_dof_mecaMAILx, &
         write_out_driven_dof_mecaMAILx, &
         read_in_gpv_mecaMAILx, &
         read_in_dof_mecaMAILx, &
         write_xxx_dof_mecaMAILx, &
         write_xxx_Rnod_mecaMAILx, &
         load_behaviours_mecaMAILx, &
         write_xxx_nodforces, &
         update_existing_entities_mecaMAILx, &
         load_models_mecaMAILx, &
         push_ppset_mecaMAILx, &
         use_new_ppset_mecaMAILx,&
         check_mecaMAILx, &
         get_write_DOF_mecaMAILx, &
         get_write_Rnod_mecaMAILx, &
         set_precon_body_mecaMAILx, &
         compute_precon_W_mecaMAILx, &
         put_precon_W_mecaMAILx, &
         init_precon_W_mecaMAILx, &
         get_nodes_precon_mecaMAILx, &
         put_vector_mecaMAILx, &
         get_vector_mecaMAILx, &
         get_ptr_body_vector_mecaMAILx, &
         get_nodal_vector_mecaMAILx, &
         get_field_rank, &
         set_field_bynode, set_field_byuser, set_field_byelem, & !set_field_bygp, &
         get_vfield_rank, &
         set_vfield_bynode, set_vfield_byelem, &
         set_ortho_frame_byuser, &  !set_ortho_frame_bynode, & set_ortho_frame_bygp, &
         get_nb_nodes_mecaMAILx, &
         get_nb_elements_mecaMAILx, &
         terminate_mecaMAILx, &
         display_bulk_element_mecamailx, &
         get_color_mecaMAILx, &
         !< rigid/coro
         set_coro_body_mecaMAILx, &
         set_rigid_body_mecaMAILx, &
         build_rigid_bodies_mecaMAILx, &
         add_Rreac_mecaMAILx, &
         get_RcoorTT_mecaMAILx, &
         get_Rcooref_mecaMAILx, &
         get_Rinertia_frame_mecaMAILx  , &
         get_Rinertia_frameTT_mecaMAILx, &
         get_Rvlocy_mecaMAILx,&
         Set_RV_driven_dofs_mecaMAILx, &
         Set_RV_driven_dof_value_mecaMAILx, &
         put_Rvector_mecaMAILx, &
         get_Rvector_mecaMAILx, &
         set_tol_coro_mecaMAILx,&  !pta 21/01/2012          
         ! rigid/coro >
         skip_defo_computation_mecaMAILx, &
         compute_rayleigh_damping_mecaMAILx, &
         compute_rayleigh_damping_discrete_mecaMAILx, &
         is_rigid_mecaMAILx, &
         set_vlocy_drvdof_mecaMAILx, &
         compute_configurationTT_mecaMAILx, &
         get_cooref_mecaMAILx, &
         get_connectivity_mecaMAILx, &
         get_materials_mecaMAILx   , &
         get_All_mecaMAILx, &
         get_reac_nodty_mecaMAILx, &
         add_field_divergence_mecaMAILx, &
         get_Deformation_Energy_mecaMAILx, &  
         get_Kinetic_Energy_mecaMAILx, &  
         Get_ptr_neighborEle2node_mecaMAILx, &
         Get_ptr_neighborEle2ele_mecaMAILx, &
         Get_ptr_boundaryElements_mecaMAILx, &
         get_2DNodalInternal_mecaMAILx, &
         get_nb_internal_mecaMAILx, &
         get_dofstatus_mecaMAILx, &
         get_gp_strain_mecaMAILx, &
         get_gp_stress_mecaMAILx, &
         get_gp_strain_triaxiality_mecaMAILx, &
         get_gp_stress_triaxiality_mecaMAILx,&
         get_gp_all_joint_mecaMAILx         
         
!!!=============== methodes ===================================!

  PUBLIC add_reac_nodty_mecaMAILx, nullify_reac_mecaMAILx,&
         nullify_vlocy_mecaMAILx,comp_vlocy_mecaMAILx, &
         comp_vlocy_bynode_mecaMAILx, &
         get_V_nodty_mecaMAILx,get_Vbegin_nodty_mecaMAILx,&
         get_X_nodty_mecaMAILx,&
         get_Vfree_nodty_mecaMAILx,get_Vaux_nodty_mecaMAILx, &
         get_Vddm_nodty_mecaMAILx, & 
         get_coor_nodty_mecaMAILx, &
         get_coorTT_nodty_mecaMAILx, &
         get_cooref_nodty_mecaMAILx, &
         get_coorbegin_nodty_mecaMAILx, &
         get_Vwear_nodty_mecaMAILx,put_Vwear_nodty_mecaMAILx, &
         get_entity_mecaMAILx,get_nodal_forces_mecaMAILx,get_nodal_displacements_mecaMAILx,&
         get_2DNodalStress_mecaMAILx,get_2DNodalStrain_mecaMAILx, get_2DNodalInternalVariables_mecaMAILx,&
         get_3DNodalStress_mecaMAILx,get_3DNodalStrain_mecaMAILx, get_3DNodalInternalVariables_mecaMAILx, &
         get_3DElementStress_mecaMAILx,&
         set_precon_node_mecaMAILx, &
         compute_energy_mecaMAILx, &
         compute_work_mecaMAILx, &
         set_Matrix_Storage_mecaMAILx, &
         set_Matrix_Shape_mecaMAILx, &
         set_without_renum_mecaMAILx, &
         get_visible_mecaMAILx, &  !PTA
         set_visible_mecaMAILx, &  !PTA
         !!add_spring_to_node_mecaMAILx, &
         get_matrix_mecaMAILx   , &
         get_drv_vlocy_mecaMAILx, &
         comp_drv_vlocy_mecaMAILx, &
         get_gp_coor_mecaMAILx, &
         get_gp_Internals, &
         get_gp_principalfield, &                  
         get_Elements_Internal, &
         get_Elements_Internal_integral, &         
         Get_Elements_Volume, &
         Get_Elements_Center, &
         get_ptr_elements_energy, &
         comp_elements_energy, &
         Get_Elements_Jacobian, &
         GetPtr_Elements_Visibility, &
         Get_Elements_Neighbor, &
         load_precon_W_body_mecaMAILx, & !PTA 22/03/2013
         GetPtr_preconW, & !PTA 22/03/2013
         compute_kinetic_energy_mecaMAILx, & !PTA
         get_nearest_gp_mecaMAILx, &
         add_body_force_to_fext_mecaMAILx,&
         check_properties_mecaMAILx, &
         get_nb_gp_by_elem_mecaMAILx, &
         mass_scaling_mecamAILx, &
         switch_vlocy_driven_dof, &
         update_vlocy_driven_dof_structures, &
         is_node_dof_driven_mecaMAILx, &
         is_elem_dof_driven_mecaMAILx
  
  ! rm * : merdier pour global solver SiconosNumerics
  public get_nb_dofs_mecaMAILx   , &
         get_i_indices_mecaMAILx , &
         get_j_indices_mecaMAILx , &
         get_val_g_sys_mecaMAILx , &
         get_rhs_vector_mecaMAILx, &
         get_nb_nz_g_sys_mecaMAILx, &
         get_dof_node_mecaMAILx, &
         compute_forces_mecaMAILx, &
         prep_global_mecaMAILx, &
         post_global_mecaMAILx


  !rm: accessor for yales coupling
  public get_ptr_drvdofs_mecaMAILx, &
         get_ptr_nodnb_mecaMAILx

  !rm: accessor for hdf5
  public get_nb_gp_mecaMAILx, &
         get_field_mecaMAILx, &
         set_field_mecaMAILx

  public clean_memory_mecaMAILx, &
       Compute_Info_PrincipalStressField_mecaMAILx, &
       compute_PDF_pressure_mecaMAILx


  PRIVATE put_coor_begin, &
          put_coor      , &
          compute_Fres  , &  ! <- rm added
          hydrostatic_sym_tensor_2d_, &
          strain_norm_sym_tensor_2d_, &
          stress_norm_sym_tensor_2d_
          

  public get_bdyty_mecaMAILx

CONTAINS 

!------------------------------------------------------------------------

  subroutine get_bdyty_mecaMAILx( arg_bdyty )

    implicit none

    type( T_mecaMAILx ), dimension( : ), pointer :: arg_bdyty

    arg_bdyty => bdyty

  end subroutine get_bdyty_mecaMAILx

!!!---------------------------------------------------------
!!!
!!!---------------------------------------------------------
  SUBROUTINE read_in_driven_dof_mecaMAILx

    IMPLICIT NONE

    G_nfich = get_io_unit()
    OPEN(unit=G_nfich,file=TRIM(location(in_driven_dof(:))))
    CALL read_driven_dof
    CLOSE(G_nfich)

  END SUBROUTINE read_in_driven_dof_mecaMAILx
!!!---------------------------------------------------------
  SUBROUTINE write_out_driven_dof_mecaMAILx

    IMPLICIT NONE

    INTEGER :: nfich

    nfich = get_io_unit()
    OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_driven_dof(:))))
    CALL write_driven_dof(nfich)
    CLOSE(nfich)

  END SUBROUTINE write_out_driven_dof_mecaMAILx
!!!---------------------------------------------------------
  !> \brief Read a GPV file to initialize database
  subroutine read_in_gpv_mecaMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_gpv(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_gpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_gpv(:))))
    end if

    call read_in_gpv
    close(G_nfich)

  end subroutine read_in_gpv_mecaMAILx
!!!---------------------------------------------------------
  !> \brief Read a DOF file to initialize database
  subroutine read_in_dof_mecaMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_dof(:))))
    else
      open(unit=G_nfich,file=trim(location(last_dof(:))))
    end if

    call read_in_dof
    close(G_nfich)
    
  end subroutine read_in_dof_mecaMAILx
!!!---------------------------------------------------------
  SUBROUTINE write_xxx_dof_mecaMAILx(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_dof)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_dof(1:lc)))) 
       CALL write_out_dof(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_dof)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_dof(1:lc)))) 
       CALL write_out_dof(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_dof(6,ifrom,ito)
    END SELECT

  END SUBROUTINE write_xxx_dof_mecaMAILx
!!!---------------------------------------------------------
  subroutine write_xxx_Rnod_mecaMAILx(which,list,length)
    implicit none
    integer(kind=4), intent(in) :: which,length
    integer(kind=4), dimension(:), pointer :: list
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc)))) 
       call write_out_Rnod(nfich,list,length)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc)))) 
       call write_out_Rnod(nfich,list,length)
       CLOSE(nfich)
    CASE(6)
       call write_out_Rnod(6,list,length)
    END SELECT
    
  END SUBROUTINE write_xxx_Rnod_mecaMAILx
!!!---------------------------------------------------------
  subroutine write_xxx_nodforces(which,list,length)
    implicit none
    integer(kind=4), intent(in) :: which,length
    integer(kind=4), dimension(:), pointer :: list
    !
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc)))) 
       call write_out_nodforces(nfich,list,length)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc)))) 
       call write_out_nodforces(nfich,list,length)
       CLOSE(nfich)
    CASE(6)
       call write_out_nodforces(6,list,length)
    END SELECT
    
  end subroutine write_xxx_nodforces
!!!---------------------------------------------------------
!!!
!!!---------------------------------------------------------
  SUBROUTINE load_models_mecaMAILx

    IMPLICIT NONE

    INTEGER :: nb_MAILx
    INTEGER :: iM_bdyty,iM_blmty,iM_model,inodty
    INTEGER :: ibdyty,iblmty,imodel,iM_nodty
    INTEGER :: errare,itest,imdlnb,iccdof,idof,iG_i,itempo
!    INTEGER :: itempo,bw

    INTEGER :: nb_external,nb_internal
    
    !fd 22/04/08 external fields
    INTEGER :: IF,nbf,nb_ef,nb_bf
    CHARACTER(len=30),DIMENSION(:),ALLOCATABLE :: field_name
    !rm 25/03/15 external vector fields
    integer(kind=4) :: nb_evf,vsize
    character(len=30), dimension(:), allocatable :: vfield_name

    CHARACTER(len=5) :: ctempo
    CHARACTER(len=103) :: cout
                              !1234567890123456789012
    CHARACTER(len=22)  :: IAM='mecaMAILx::load_models'

    INTEGER,DIMENSION(:),ALLOCATABLE :: edof2gdof 

    INTEGER :: i,p_inodty,bw

    INTEGER,DIMENSION(:),ALLOCATABLE :: perm,inv_perm,i4_vector

    type(T_link_connec), pointer ::connectivities, tmp

    integer(kind=4) ::  max_nod2el, max_dofs_adj, max_conn

    ! 0 initialisations
    
    ! initialisation de la liste des mecaEF_xxx disponibles
    CALL init_mecaEF

    ! initialisation de la map MAILx -> mecaMAILx
    nb_MAILx=get_nb_MAILx()

    ALLOCATE(M2meca(nb_MAILx),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating M2meca')
    END IF
  
    DO iM_bdyty=1,nb_MAILx
       M2meca(iM_bdyty)%bdyty=0
       NULLIFY(M2meca(iM_bdyty)%nodty)
       NULLIFY(M2meca(iM_bdyty)%blmty)
    END DO

    ! choose a default value for matrix_storage if not set
    if( Matrix_storage < 0 ) then
      if( nbDIME == 3 ) then
        if( sparse_storage_available ) then
          Matrix_storage = i_sparse
        else
          Matrix_storage = i_band
        end if
      else
        Matrix_storage = i_band
      end if
    end if
    
    ! first reading: sizing array of models  

    itest = 0
    if( .not. allocated(modelz) .or. size(modelz) < 1 ) then
        call faterr(IAM,'please call ReadModels before trying to LoadModels')
    end if

    DO imodel=1,SIZE(modelz)
       IF (modelz(imodel)%mdlty == 'MECAx') itest = itest + 1 
    END DO
    
    WRITE(cout,'(I0,1x,A)') itest,'MECAx models declared'
    CALL LOGMES(cout)

    ! then constructing the meca EF database 
    
    ! first scaning of the MAILx array looking for MECAx models 
    ! database determining the size of bdyty
    
    ibdyty=0
    
    if( .not. allocated(M_bdyty) ) then
        call faterr(IAM,'please call ReadBodies before trying to LoadModels')
    end if

    DO iM_bdyty=1,SIZE(M_bdyty)
       itest=0
       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'MECAx') itest=1
                END IF
             END DO
             IF (itest == 1) EXIT
          END DO
          IF (itest == 1) EXIT
       END DO
       IF (itest == 1) ibdyty =ibdyty + 1 
    END DO
    
    nb_mecaMAILx = ibdyty

    IF (ibdyty == 0) THEN
       CALL LOGMES('no MECAx BODIES found')
       CALL LOGMES('if any check BODIES.DAT or MODELS.DAT')
    ELSE
      WRITE(cout,'(I0,1x,A)') nb_mecaMAILx,'MECAx BODIES found'
      CALL LOGMES(cout)
    END IF
    
    CALL LOGMES('--')

    IF (ibdyty == 0) RETURN

    if (M_INTEGRATOR_ID == 0) then
      call faterr('load_models','mechanical integrator not defined')    
    endif
   
    ALLOCATE(bdyty(ibdyty),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating bdyty')
    END IF

    ! la map mecaMAILx -> MAILx
    ALLOCATE(bdyty2M_bdyty(ibdyty),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating bdyty2M_bdyty')
    END IF

    ! second scaning of the MAILx looking for MECAx models
    ! filling the maps bdyty2M_bdyty and M2meca(iM_bdyty)%bdyty
    ! sizing bdyty(ibdyty)%blmty, bdyty(ibdyty)%blmty2M_blmty  

    ibdyty=0

    DO iM_bdyty=1,SIZE(M_bdyty)
       itest  = 0
       iblmty = 0

       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'MECAx') THEN 
                      IF (itest == 0) THEN
                         ibdyty =ibdyty + 1
                         bdyty2M_bdyty(ibdyty)=iM_bdyty              
                         M2meca(iM_bdyty)%bdyty=ibdyty
                         itest=1
                      END IF
                      iblmty =iblmty + 1
                   END IF
                END IF
             END DO
          END DO
       END DO
       IF (itest /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty')
          END IF

          ALLOCATE(bdyty(ibdyty)%blmty2M_blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty2M_blmty')
          END IF

          ALLOCATE(M2meca(iM_bdyty)%blmty(SIZE(M_bdyty(iM_bdyty)%blmty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M2meca%blmty')
          END IF
          M2meca(iM_bdyty)%blmty=0

          ALLOCATE(M2meca(iM_bdyty)%nodty(SIZE(M_bdyty(iM_bdyty)%nodty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M2meca%nodty')
          END IF
          M2meca(iM_bdyty)%nodty=0

          !fd element erosion 
          ALLOCATE(bdyty(ibdyty)%eviz(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%eviz')
          END IF
          bdyty(ibdyty)%eviz = 1
       END IF
    END DO
    
    ! third scaning of the MAILx database: 
    ! filling the components of bdyty

    if( get_nb_bulk_behav() < 1 ) then
        call faterr(IAM,'Please call ReadBehaviours before LoadModels')
    end if

    DO ibdyty=1,SIZE(bdyty)

       iM_bdyty=bdyty2M_bdyty(ibdyty)

       iblmty=0

       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'MECAx') THEN 
                      iblmty =iblmty + 1

                      bdyty(ibdyty)%blmty2M_blmty(iblmty)=iM_blmty
                      M2meca(iM_bdyty)%blmty(iM_blmty)=iblmty 

                      ! allocation de la table de connectivite
                      inodty=SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%NODES(inodty),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%NODES')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%NODES(:)=0

                      if (modelz(imodel)%ID(1:3) == 'EXT') then
                        if (.not. is_externalFEM) call FATERR(IAM,'EXTxx element are only available when using externalFEM')
                        bdyty(ibdyty)%blmty(iblmty)%blmnb=0
                        bdyty(ibdyty)%blmty(iblmty)%mdlnb=imodel
                        bdyty(ibdyty)%blmty(iblmty)%lawnb = 0
                        read(modelz(imodel)%ID(4:5),'(I2)') bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

                        !fp
                        M_bdyty(iM_bdyty)%is_meca = .TRUE.
                      else
                        if (is_externalFEM) call FATERR(IAM,'externalFEM works only with EXTxx elements')

                        bdyty(ibdyty)%blmty(iblmty)%blmnb=get_nb_in_mecaEF(modelz(imodel)%ID)
                        bdyty(ibdyty)%blmty(iblmty)%mdlnb=imodel

                        !xxx fd test 30/01/09 
                        !xxx on court circuite le load behaviours
                        bdyty(ibdyty)%blmty(iblmty)%lawnb = &
                        get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
                        !xxx
                        bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node=get_N_DOF_by_NODE_mecaEF(modelz(imodel)%ID)

                        ! ca n'est valide que si les noeuds ont tous le meme nombre de ddl 
                        idof = inodty*bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%edof2gdof(idof),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%edof2gdof')
                        bdyty(ibdyty)%blmty(iblmty)%edof2gdof(:)=0
                      
                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%stiffness(idof,idof),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%stiffness')
                        bdyty(ibdyty)%blmty(iblmty)%stiffness(:,:)=0.d0
                      
                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%mass(idof,idof),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%mass')
                        bdyty(ibdyty)%blmty(iblmty)%mass(:,:)=0.d0
                      
                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%damping(idof,idof),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%damping')
                        bdyty(ibdyty)%blmty(iblmty)%damping(:,:)=0.d0
                        !
                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%Fext(idof,2),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%Fext')
                        bdyty(ibdyty)%blmty(iblmty)%Fext(:,:)=0.d0
                        !
                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%Fint(idof,2),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%Fint')
                        bdyty(ibdyty)%blmty(iblmty)%Fint(:,:)=0.d0
                        !
                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ttFint(idof),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%ttFint')
                        bdyty(ibdyty)%blmty(iblmty)%ttFint(:)=0.d0

                        ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%RHSloc(idof),stat=errare)
                        IF (errare /= 0) CALL FATERR(IAM,'error allocating bdyty%blmty%RHSloc')
                        bdyty(ibdyty)%blmty(iblmty)%RHSloc(:)=0.d0

                        nb_external=modelz(imodel)%nb_external_variables
                        nb_internal=modelz(imodel)%nb_internal_variables

                        !PRINT*,'--->',iM_bdyty,iM_blmty,imodel,modelz(imodel)%ID,nb_external,nb_internal

                        !fd 22/04/08, 28/01/09 
                        ! gestion des fields: external (model) & bulk
                        ! on rapatrie tous les fields declares et on s'en sert pour initialiser les champs aux pg 
                        ! on donne au field le rang dans la pile de field stocke au pg
  
                        nb_ef = get_external_field_nb(imodel) 
                        nb_bf = get_bulk_field_nb(bdyty(ibdyty)%blmty(iblmty)%lawnb)
                        nbf = nb_ef + nb_bf

                        nb_evf = get_external_nb_vfield(imodel) 

                        IF (nbf /= 0) THEN
                          ALLOCATE(field_name(nbf))
                          DO IF=1,nb_ef
                            field_name(IF) = get_external_field_name(imodel,IF)
                          ENDDO                      
                          DO IF=1,nb_bf
                             field_name(nb_ef+IF) = get_bulk_field_name(imodel,IF)
                          ENDDO                      
                        END IF

                        if (nb_evf /= 0 ) then
                          allocate(vfield_name(nb_evf))
                          do IF = 1,nb_evf
                            vfield_name(IF) = get_external_vfield_name(imodel,IF)
                          end do
                          vsize = get_external_vfield_max_size(imodel)
                        end if

                        if( nbf/=0 .and. nb_evf/=0 ) then
                          call init_mecagpv_MAILx(iM_bdyty,iM_blmty, &
                                                  get_N_GP_mecaEF(modelz(imodel)%ID), &
                                                  nb_external,nb_internal, &
                                                  nbf,field_name,nb_evf,vfield_name,vsize)

                          deallocate(field_name,vfield_name)

                        else if( nbf/=0 ) then
                          call init_mecagpv_MAILx(iM_bdyty,iM_blmty, & 
                                                  get_N_GP_mecaEF(modelz(imodel)%ID), &
                                                  nb_external,nb_internal, &
                                                  nbf,field_name)

                          deallocate(field_name)

                        else if( nb_evf/=0 ) then
                          call init_mecagpv_MAILx(iM_bdyty,iM_blmty, & 
                                                  get_N_GP_mecaEF(modelz(imodel)%ID), &
                                                  nb_external,nb_internal, &
                                                  nb_vfields=nb_evf,vfield_name=vfield_name,vsize=vsize)

                          deallocate(vfield_name)

                        ELSE
                          CALL init_mecagpv_MAILx(iM_bdyty,iM_blmty, & 
                                                  get_N_GP_mecaEF(modelz(imodel)%ID), &
                                                  nb_external,nb_internal)
                        ENDIF
                      endif
                   END IF
                END IF
             END DO
          END DO
       END DO

       IF (iblmty == 0) THEN
          CALL FATERR(IAM,'no blmty')
       END IF
    END DO


    ! perform some temporary computations
    ! we look for the nodes owning to mecaMAILx
    ! 
    ! first we scan the MAILx database and we count the 
    ! nodes owning to a MECAx element 
    !
    ! second we fill the bdyty ... database and we determine
    ! the nodty of a node which is the highest one
    !
    
    DO ibdyty=1,SIZE(bdyty)

       iM_bdyty=bdyty2M_bdyty(ibdyty)
       inodty=0

       DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
          DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
             iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)
             IF (M2meca(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
                inodty = inodty+1
                EXIT
             END IF
          END DO
       END DO
       IF (inodty /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%nodty')
          END IF

          ALLOCATE(bdyty(ibdyty)%nodty2M_nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%nodty2M_nodty')
          END IF

          bdyty(ibdyty)%nb_nodes = inodty

          DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
             call new_nodty(bdyty(ibdyty)%nodty(inodty),'     ')
             bdyty(ibdyty)%nodty2M_nodty(inodty)=0
          END DO
       ELSE
          CALL FATERR(IAM,'error computing size of bdyty%nodty')
       END IF

       inodty=0
       DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
          itest=0
          DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
             iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)      
             IF (M2meca(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
                IF (itest == 0) THEN
                   inodty = inodty+1
                   bdyty(ibdyty)%nodty2M_nodty(inodty)=iM_nodty
                   M2meca(iM_bdyty)%nodty(iM_nodty)=inodty
                   itest=1
                END IF
                ! a la peche au type de l'element ...
                iblmty=M2meca(iM_bdyty)%blmty(iM_blmty)
                imodel=bdyty(ibdyty)%blmty(iblmty)%mdlnb
                if (modelz(imodel)%ID(1:3) == 'EXT') then
                   read(modelz(imodel)%ID(4:5),'(I2)') itempo
                else
                  ctempo=modelz(imodel)%ID
                  ! a la peche au type de noeuds de l'element
                  itempo=get_n_dof_by_node_mecaEF(ctempo) 
                endif
                ! on garde le type de celui qui a le + de ddls
                IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) <= itempo) &
                   call new_nodty(bdyty(ibdyty)%nodty(inodty),get_node_name_from_id(itempo))
             END IF
          END DO
       END DO
    END DO

    DO iM_bdyty=1,SIZE(M2meca)
       IF (M2meca(iM_bdyty)%bdyty /=0) THEN
          DO iM_blmty=1,SIZE(M2meca(iM_bdyty)%blmty)
             IF (M2meca(iM_bdyty)%blmty(iM_blmty) /=0) THEN
                ibdyty=M2meca(iM_bdyty)%bdyty
                iblmty=M2meca(iM_bdyty)%blmty(iM_blmty)
                DO inodty=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
                   iM_nodty=M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)
                   bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)=M2meca(iM_bdyty)%nodty(iM_nodty)
                END DO
             END IF
          END DO
       END IF
    END DO

    DO ibdyty=1,SIZE(bdyty)

       iccdof = 0

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iccdof=iccdof + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       END DO

       bdyty(ibdyty)%nbdof=iccdof
 
       IF (iccdof /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%nodnb(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%dofnb(iccdof),stat=errare)

          if ( M_INTEGRATOR_ID == INTEGRATOR_BETA2 ) then
            ALLOCATE(bdyty(ibdyty)%Xprev(iccdof),stat=errare)
          else
            nullify(bdyty(ibdyty)%Xprev)
          end if

          ALLOCATE(bdyty(ibdyty)%Vbegin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Xbegin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%V(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%X(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vlast(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vfree(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vaux(iccdof),stat=errare)
          if( DDM_SCHWARTZ ) then
             ALLOCATE(bdyty(ibdyty)%Vddm(iccdof),stat=errare)
          else
             NULLIFY(bdyty(ibdyty)%Vddm)
          end if
          ALLOCATE(bdyty(ibdyty)%residu(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Fext(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Fint(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Finert(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%momentum(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Ireac(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Iaux(iccdof),stat=errare)
          
          ALLOCATE(bdyty(ibdyty)%Vwear(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Xwear(iccdof),stat=errare)

          ALLOCATE(bdyty(ibdyty)%periodicnode(iccdof),stat=errare)

          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating X,V')
          END IF
          
          bdyty(ibdyty)%nodnb=0
          bdyty(ibdyty)%dofnb=0
    
          if( M_INTEGRATOR_ID == INTEGRATOR_BETA2 ) bdyty(ibdyty)%Xprev =0.d0
          
          bdyty(ibdyty)%Vbegin=0.d0
          bdyty(ibdyty)%V     =0.d0
          bdyty(ibdyty)%Vlast =0.d0
          bdyty(ibdyty)%Xbegin=0.d0
          bdyty(ibdyty)%X     =0.d0
          bdyty(ibdyty)%Vfree =0.D0
          bdyty(ibdyty)%Vaux  =0.d0
          if( DDM_SCHWARTZ ) then
             bdyty(ibdyty)%Vddm  =0.d0
          end if
          bdyty(ibdyty)%residu=0.d0
          bdyty(ibdyty)%Fext  =0.d0 
          bdyty(ibdyty)%Fint  =0.d0
          bdyty(ibdyty)%Finert=0.d0
          bdyty(ibdyty)%momentum=0.d0
          bdyty(ibdyty)%is_reac_modified=.FALSE.
          bdyty(ibdyty)%Ireac =0.d0
          bdyty(ibdyty)%Iaux  =0.d0
          
          bdyty(ibdyty)%Vwear  =0.d0
          bdyty(ibdyty)%Xwear  =0.d0

          bdyty(ibdyty)%is_precon=.FALSE.
          bdyty(ibdyty)%saved_precon_W=.FALSE. !PTA 22/03/2013
          nullify(bdyty(ibdyty)%nodes_precon)
          nullify(bdyty(ibdyty)%w_precon)
          nullify(bdyty(ibdyty)%Vaux_precon)
          nullify(bdyty(ibdyty)%p2g,bdyty(ibdyty)%g2p)

          bdyty(ibdyty)%is_coro=.FALSE.
          bdyty(ibdyty)%is_rigid=.FALSE.
          bdyty(ibdyty)%skip_defo_comp=.FALSE.
          
          bdyty(ibdyty)%visible     = .TRUE.
          bdyty(ibdyty)%is_periodic = .FALSE.

          bdyty(ibdyty)%periodicnode = 0

          ALLOCATE(bdyty(ibdyty)%coorTT(nbdime,SIZE(bdyty(ibdyty)%nodty)),stat=errare)
          bdyty(ibdyty)%coorTT=0.d0
          ALLOCATE(bdyty(ibdyty)%RcoorTT(nbdime),stat=errare)
          bdyty(ibdyty)%RcoorTT=0.d0

       ELSE 

          NULLIFY(bdyty(ibdyty)%nodnb)
          NULLIFY(bdyty(ibdyty)%dofnb)
          NULLIFY(bdyty(ibdyty)%Xprev)
          NULLIFY(bdyty(ibdyty)%Vbegin)
          NULLIFY(bdyty(ibdyty)%Xbegin)
          NULLIFY(bdyty(ibdyty)%V)
          NULLIFY(bdyty(ibdyty)%X)
          NULLIFY(bdyty(ibdyty)%Vlast)
          NULLIFY(bdyty(ibdyty)%Vfree)
          NULLIFY(bdyty(ibdyty)%Vaux)
          if( DDM_SCHWARTZ ) then
             NULLIFY(bdyty(ibdyty)%Vddm)
          end if
          NULLIFY(bdyty(ibdyty)%residu)
          NULLIFY(bdyty(ibdyty)%Fext)
          NULLIFY(bdyty(ibdyty)%Fint)
          NULLIFY(bdyty(ibdyty)%Finert)
          NULLIFY(bdyty(ibdyty)%momentum)
          NULLIFY(bdyty(ibdyty)%Ireac)
          NULLIFY(bdyty(ibdyty)%Iaux)
          
          NULLIFY(bdyty(ibdyty)%Vwear)
          NULLIFY(bdyty(ibdyty)%Xwear)

          NULLIFY(bdyty(ibdyty)%periodicnode)

          bdyty(ibdyty)%is_precon=.FALSE.          
          bdyty(ibdyty)%saved_precon_W=.FALSE. !PTA 22/03/2013
          bdyty(ibdyty)%is_coro=.FALSE.          
          bdyty(ibdyty)%is_rigid=.FALSE.
          bdyty(ibdyty)%skip_defo_comp=.FALSE.

          bdyty(ibdyty)%visible     = .TRUE.
          bdyty(ibdyty)%is_periodic = .FALSE.

          NULLIFY(bdyty(ibdyty)%coorTT)
          NULLIFY(bdyty(ibdyty)%RcoorTT)

          WRITE(cout,'(A,I0)') 'Warning mecaMAILx without DOF ',ibdyty 
          CALL LOGMES(cout)
       END IF

       ! array node -> first global ddl

       !fd attention j'augmente la taille de ccdof pour l'utilisation du g_system

       IF (SIZE(bdyty(ibdyty)%nodty) /= 0) THEN

          ALLOCATE(bdyty(ibdyty)%ccdof(SIZE(bdyty(ibdyty)%nodty)+1),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating ccdof in read_models')
          END IF
       ELSE 
          NULLIFY(bdyty(ibdyty)%ccdof)
          PRINT*,'Warning MAILx without node',ibdyty 
       END IF
    END DO

!  second: filling ordering arrays, and element local 2 global dof correspondance
    
    DO ibdyty=1,SIZE(bdyty)

       iccdof=0

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          bdyty(ibdyty)%ccdof(inodty)=iccdof
          DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
             iccdof=iccdof+1
             bdyty(ibdyty)%nodnb(iccdof)=inodty       ! reverse mapping
             bdyty(ibdyty)%dofnb(iccdof)=idof         ! reverse mapping
          END DO
       END DO

       bdyty(ibdyty)%ccdof(size(bdyty(ibdyty)%nodty)+1) = iccdof  
       
       if (is_externalFEM) cycle

       max_conn = 0
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          iccdof=0
          max_conn = max(max_conn, size(bdyty(ibdyty)%blmty(iblmty)%NODES))
          DO itempo=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
             DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                iccdof=iccdof+1
                bdyty(ibdyty)%blmty(iblmty)%edof2gdof(iccdof)=bdyty(ibdyty)%ccdof(inodty)+idof
             END DO
          END DO
       END DO

      !! connectivite      
      connectivities => get_ll_connectivity_mecaMAILx(ibdyty)

      !print*,'initialisation ', Matrix_storage, Matrix_shape
      max_nod2el = get_max_nod2el(bdyty2M_bdyty(ibdyty))
      max_dofs_adj = max_nod2el * max_conn * nbDIME

      call initialize_system(bdyty(ibdyty)%g_sys,Matrix_storage,Matrix_shape,bdyty(ibdyty)%ccdof,connectivities,max_dofs_adj)

      do while( associated(connectivities) )
        tmp => connectivities%n
        deallocate(connectivities%connec)
        deallocate(connectivities)
        connectivities => tmp
      end do

    END DO

    if (is_externalFEM) return

!
    IF (itchache) THEN

       PRINT*,'Nombre de corps mecaMAILx:',SIZE(bdyty)
       DO ibdyty=1,SIZE(bdyty)
          PRINT*,'==========================================='
          PRINT*,'Corps: ',ibdyty
          PRINT*,'Correspondance dans la base Mailx: ',bdyty2M_bdyty(ibdyty)
          PRINT*,'PARANOIAC TEST local->global->local',M2meca(bdyty2M_bdyty(ibdyty))%bdyty
          PRINT*,'**nodty************'
          DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
             PRINT*,'Noeud: ',inodty
             PRINT*,'Type de noeud: ',get_nodNAME(bdyty(ibdyty)%nodty(inodty))
             PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%nodty2M_nodty(inodty)
             PRINT*,'ddl s commencant a: ',bdyty(ibdyty)%ccdof(inodty),'nombre: ',nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
             PRINT*,'PARANOIAC TEST local->global->local',M2meca(bdyty2M_bdyty(ibdyty))%nodty(bdyty(ibdyty)%nodty2M_nodty(inodty))
          ENDDO
          PRINT*,'**blmty************'
          DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
             PRINT*,'Element: ',iblmty
             PRINT*,'Connectivite:',bdyty(ibdyty)%blmty(iblmty)%NODES(:)
             imdlnb=bdyty(ibdyty)%blmty(iblmty)%mdlnb
             PRINT*,'Num de l element dans la liste mecaEF: ',bdyty(ibdyty)%blmty(iblmty)%blmnb
             PRINT*,'ID du modele: ',modelz(imdlnb)%ID,' Num de modele: ',imdlnb 
             PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%blmty2M_blmty(iblmty)
          ENDDO
       ENDDO
       

       PRINT*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

!
    END IF


    DO ibdyty=1,SIZE(bdyty)

      ! le second membre
      ALLOCATE(bdyty(ibdyty)%RHS(bdyty(ibdyty)%nbdof),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating bdyty%RHS')
      END IF
      bdyty(ibdyty)%RHS = 0.d0

      ! 
      bdyty(ibdyty)%W_pot=0.d0
      bdyty(ibdyty)%W_cin=0.d0
      bdyty(ibdyty)%W_def=0.d0
      bdyty(ibdyty)%W_ddl=0.d0
      bdyty(ibdyty)%W_con=0.d0
      !
      bdyty(ibdyty)%P_pot=0.d0
      bdyty(ibdyty)%P_cin=0.d0
      bdyty(ibdyty)%P_def=0.d0
      bdyty(ibdyty)%P_ddl=0.d0
      bdyty(ibdyty)%P_con=0.d0
    END DO


  END SUBROUTINE load_models_mecaMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE update_existing_entities_mecaMAILx

    IMPLICIT NONE

    nb_existing_entities = get_nb_ENTITY()

    CALL add_nb_ENTITY(nb_mecaMAILx)

  END SUBROUTINE update_existing_entities_mecaMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE load_behaviours_mecaMAILx

    IMPLICIT NONE

    INTEGER :: ibdyty,iblmty,ibehav,imodel
    INTEGER :: iM_bdyty,iM_blmty,iM_behav,iM_model
    
    INTEGER :: itest 
    
    CHARACTER(len=103) :: cout
                              !12345678901234567890123456
    CHARACTER(len=26)  :: IAM='mecaMAILx::load_behaviours'

    IF (nb_mecaMAILx == 0) RETURN
    IF (is_externalFEM) RETURN

    DO ibdyty=1,SIZE(bdyty)
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
          
          imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb

          !fd 12/12/07 on va a la peche au materiau definit dans le fichier BODIES.DAT
          !fd pour ca il faut trouver le rang du couple modele/materiau lu a partir du numero de modele stocke
          !fd du lourd !!

          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
            IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) EXIT
          ENDDO

          IF (iM_model >  SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)) THEN
             !                    123456789012345678901234567890123456789
             WRITE(cout,'(A39)') 'unable to recover the global model rank'
             CALL FATERR(IAM,cout)
          ENDIF


          !print*,imodel,M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(imodel)
          !print*,get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(imodel))

          bdyty(ibdyty)%blmty(iblmty)%lawnb = &
            get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
          
          IF (bdyty(ibdyty)%blmty(iblmty)%lawnb == 0) THEN
             !                                 1234567890          123456789          123456789012345678901 
             WRITE(cout,'(A10,I0,A9,I0,A21)') 'mecaMAILx ',ibdyty,' element ',iblmty,' without behaviour !?'
             
             CALL LOGMES('check BODIES.DAT in DATBOX')
             CALL LOGMES('check BEHAVIOURS.DAT in DATBOX')
             CALL FATERR(IAM,cout)
          END IF
       END DO
    END DO
    
  END SUBROUTINE load_behaviours_mecaMAILx
!------------------------------------------------------------------------ 
SUBROUTINE push_ppset_mecaMAILx

   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,ibehav,imodel
   INTEGER :: iM_bdyty,iM_blmty,iM_behav

   INTEGER :: itest 

   CHARACTER(len=103) :: cout
                             !123456789012345678901234567890 
   CHARACTER(len=21)  :: IAM='mecaMAILx::push_ppset'

   INTEGER :: igp,nb_gp

   IF (nb_mecaMAILx == 0) RETURN
   IF (is_externalFEM) RETURN

   DO ibdyty=1,SIZE(bdyty)
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
       ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb

       !fd new 13/08/09
       ! one needs a ppset by gp 
       !fd 16/06/2011 or element (discrete)

       nb_gp=MAX(1,get_N_GP_mecaEF(modelz(imodel)%ID))
       ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ppsnb(nb_gp))

       !fd new 05/11/09
       ! one can re-use a ppset if already defined
       ! the strategy new/re-use is defined through the flag use_existing_ppset

       DO igp=1,nb_gp
         bdyty(ibdyty)%blmty(iblmty)%ppsnb(igp) = get_ppset_nb(use_existing_ppset,imodel,ibehav)
       ENDDO
     END DO
   END DO  
   
 END SUBROUTINE push_ppset_mecaMAILx
!------------------------------------------------------------------------
!!!------------------------------------------------------------------------   
  SUBROUTINE read_driven_dof

    IMPLICIT NONE
    
    INTEGER :: errare,itest  
    INTEGER :: ivd,ifd
    INTEGER :: ibdyty,inodty,dofnb
    INTEGER :: iM_bdyty,iM_nodty
    integer :: chnod
    CHARACTER(len=103) :: cout
                              !123456789012345678901234567890
    CHARACTER(len=26)  :: IAM='mecaMAILx::read_driven_dof'
  
    IF (nb_mecaMAILx == 0) RETURN

    !
    ! initialisation
    !

    DO ibdyty=1,SIZE(bdyty)
       
       NULLIFY(bdyty(ibdyty)%vlocy_driven_dof, &
               bdyty(ibdyty)%Vdriv, &
               bdyty(ibdyty)%Xdriv, &
               bdyty(ibdyty)%VdrivBeg , &
               bdyty(ibdyty)%drvdofs  , &
               bdyty(ibdyty)%drvvalues, &
               bdyty(ibdyty)%drvstatus)

       bdyty(ibdyty)%nb_vlocy_driven_dof=0
       
       allocate(bdyty(ibdyty)%drvstatus(bdyty(ibdyty)%nb_nodes))
       bdyty(ibdyty)%drvstatus(:)=0

       NULLIFY(bdyty(ibdyty)%force_driven_dof, &
               bdyty(ibdyty)%Fdriv, &
               bdyty(ibdyty)%FdrivBeg)
       
       bdyty(ibdyty)%nb_force_driven_dof=0

       ! rigid or coro driven dof
       bdyty(ibdyty)%nb_RV_driven=0
       bdyty(ibdyty)%RV_driven_dof= 0
       bdyty(ibdyty)%RV_driven= 0.d0 

       ! status management
       allocate(bdyty(ibdyty)%drvstatus(bdyty(ibdyty)%nb_nodes))
       bdyty(ibdyty)%drvstatus(:)=0

    END DO

    ! first reading: sizing array vlocy_driven_dof  

    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty'

       ivd=0
       ifd=0

       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'Problem reading bdyty')
       END IF
       itest=itest_bdyty_MAILx(G_clin)  
       IF (itest .NE. ifound) CYCLE

       !    we keep the body number   
       READ(G_clin(7:13),'(I7)') iM_bdyty    

       IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
       
          WRITE(cout,'(A12,I0,A30)')'body number ',iM_bdyty,' does not belong to collection'
          CALL FATERR(IAM,cout)
       END IF

       ibdyty = M2meca(iM_bdyty)%bdyty
       !if it's a body without MECAx behaviour

       IF (ibdyty == 0) CYCLE     

       DO
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'MECAx') CYCLE                ! fishing the MECAx part 

          DO
             IF ( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) == '     ') CYCLE
             IF (G_clin(2:6) == 'model' .OR. G_clin(2:6) == '$$$$$') EXIT 
             
             IF (G_clin(2:6) /= 'nodty') THEN                ! fishing for the keyword 'nodty' 
                CALL FATERR(IAM,'keyword nodty expected')
             END IF

             DO
                IF( .NOT. read_G_clin()) EXIT
                IF(G_clin(2:6) == '     ') CYCLE
                IF (.NOT. is_a_nodty(G_clin(2:6))) THEN
                   CALL FATERR(IAM,'Problem reading nodty')
                END IF

                chnod= get_node_id_from_name( G_clin(2:6) )
          
                READ(G_clin(7:13),'(I7)') iM_nodty    

                IF (iM_nodty <= 0 .OR. iM_nodty > SIZE(M_bdyty(iM_bdyty)%nodty)) THEN 
                   WRITE(cout,'(A12,I0,A25,I0)') 'node number ',iM_nodty,' does not belong to body ',iM_bdyty
                   CALL FATERR(IAM,cout)
                ENDIF
                
                inodty=M2meca(iM_bdyty)%nodty(iM_nodty)

                IF ( chnod > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) ) THEN

                   WRITE(cout,'(A6,A5,A49)') 'nodty ',chnod,' incompatible with the one belonging to the body '
                   CALL FATERR(IAM,cout)
                END IF

                DO
                   IF( .NOT. read_G_clin()) EXIT
                   IF (G_clin(2:6) == '     ') CYCLE
                   IF (G_clin(2:6) /= 'dofty') THEN          ! fishing for the keyword 'dofty'
                      CALL FATERR(IAM,'keyword dofty expected')
                   END IF
                   !
                   DO
                      IF( .NOT. read_G_clin()) EXIT
                      SELECT CASE(G_clin(2:6))
                      CASE('vlocy') 

                         ivd=ivd+1

                         READ(G_clin( 9: 13),'(I5)') dofnb

                         IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                            WRITE(cout,'(A11,I5,A26,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                            CALL FATERR(IAM,cout)
                         END IF
                         
                      CASE('force') 
                         ifd=ifd+1
                         READ(G_clin( 9: 13),'(I5)') dofnb

                         IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                            WRITE(cout,'(A11,I5,A26,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                            CALL FATERR(IAM,cout)
                         END IF
                         
                      CASE('     ')
                         
                      CASE default
                         EXIT
                      END SELECT
                   END DO
                   EXIT
                END DO
                EXIT
             END DO
             BACKSPACE(G_nfich)
          END DO
          BACKSPACE(G_nfich)
       END DO
       BACKSPACE(G_nfich)  
       
       bdyty(ibdyty)%nb_vlocy_driven_dof=ivd

       ALLOCATE(bdyty(ibdyty)%vlocy_driven_dof(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating vlocy_driven_dof')
       END IF
       
       ALLOCATE(bdyty(ibdyty)%Vdriv(ivd),bdyty(ibdyty)%VdrivBeg(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Vdriv')
       END IF

       ALLOCATE(bdyty(ibdyty)%Xdriv(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Xdriv')
       END IF
       
       ALLOCATE(bdyty(ibdyty)%drvdofs(ivd),bdyty(ibdyty)%drvvalues(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating drvdofs/drvvalues')
       END IF

       IF (ivd == 0) THEN
          ! WRITE (cout,'(A,I0,A)') 'Warning: mecaMAILx ',ibdyty,' without vlocy_driven_dof'
          ! CALL LOGMES(cout)   
       ELSE
         bdyty(ibdyty)%Vdriv(:)    =0.d0
         bdyty(ibdyty)%VdrivBeg(:) =0.d0
         bdyty(ibdyty)%Xdriv(:)    =0.d0
         bdyty(ibdyty)%drvdofs(:)  =0
         bdyty(ibdyty)%drvvalues(:)=0.d0
       ENDIF
       
       bdyty(ibdyty)%nb_force_driven_dof=ifd
       
       ALLOCATE(bdyty(ibdyty)%force_driven_dof(ifd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating force_driven_dof')
       END IF

       ALLOCATE(bdyty(ibdyty)%Fdriv(ifd),bdyty(ibdyty)%FdrivBeg(ifd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Fdriv')
       END IF

       IF (ifd == 0) THEN
          ! WRITE (cout,'(A,I0,A)') 'Warning: mecaMAILx ',ibdyty,' without force_driven_dof'
          ! CALL LOGMES(cout)
       ELSE
         bdyty(ibdyty)%FdrivBeg(:)=0.d0
         bdyty(ibdyty)%Fdriv(:)   =0.d0
       END IF
    END DO

   ! second reading: filling in data
    
    REWIND(G_nfich)

    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty' 

       ivd=0
       ifd=0

       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'Problem reading bdyty')
       ENDIF
       itest=itest_bdyty_MAILx(G_clin)                      
       IF (itest /= ifound) CYCLE

       !    we keep the body number   
       READ(G_clin(7:13),'(I7)') iM_bdyty    
       ibdyty=M2meca(iM_bdyty)%bdyty
       
       DO    
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'MECAx') CYCLE                ! fishing the MECAx part 

          DO
             IF ( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) == '     ') CYCLE
             IF (G_clin(2:6) == 'model' .OR. G_clin(2:6) == '$$$$$') EXIT 

             IF (G_clin(2:6) /= 'nodty') THEN                ! fishing for the keyword 'nodty' 
                CALL FATERR(IAM,'keyword nodty expected')
             END IF

             DO
                IF( .NOT. read_G_clin()) EXIT
                IF(G_clin(2:6) == '     ') CYCLE
                IF (.NOT. is_a_nodty(G_clin(2:6))) THEN
                   CALL FATERR(IAM,'Problem reading nodty')
                END IF

                chnod= get_node_id_from_name( G_clin(2:6) )
          
                READ(G_clin(7:13),'(I7)') iM_nodty    

                inodty=M2meca(iM_bdyty)%nodty(iM_nodty)

                DO
                   IF( .NOT. read_G_clin()) EXIT
                   IF (G_clin(2:6) == '     ') CYCLE
                   IF (G_clin(2:6) /= 'dofty') THEN          ! fishing for the keyword 'dofty'
                      CALL FATERR(IAM,'keyword dofty expected')
                   ENDIF
                   !
                   DO
                      IF( .NOT. read_G_clin()) EXIT
                      SELECT CASE(G_clin(2:6))
                      CASE('vlocy') 
                         ivd=ivd+1
                         
                         NULLIFY(bdyty(ibdyty)%vlocy_driven_dof(ivd)%time_evolution%x, &
                                 bdyty(ibdyty)%vlocy_driven_dof(ivd)%time_evolution%fx)
                         
                         CALL read_a_driven_dof(chnod,inodty,G_clin,bdyty(ibdyty)%vlocy_driven_dof(ivd))
                         
                      CASE('force') 
                         ifd=ifd+1
                         
                         NULLIFY(bdyty(ibdyty)%force_driven_dof(ifd)%time_evolution%x, &
                              bdyty(ibdyty)%force_driven_dof(ifd)%time_evolution%fx)
                         
                         CALL read_a_driven_dof(chnod,inodty,G_clin,bdyty(ibdyty)%force_driven_dof(ifd))
                         
                      CASE('     ')
                         
                      CASE default
                         EXIT
                      END SELECT
                   END DO
                   
                   EXIT
                END DO
                EXIT
             END DO
             BACKSPACE(G_nfich)
          END DO
          BACKSPACE(G_nfich)
       END DO
       BACKSPACE(G_nfich)  
    END DO

    ! set drvstatus
    do ibdyty = 1, nb_mecaMAILx
      call update_drvstatus(ibdyty)
    end do

  END SUBROUTINE read_driven_dof

  subroutine update_drvstatus(ibdyty)
    implicit none
    integer, intent(in) :: ibdyty
    !
    integer :: ivd, ifd, inod, idof

    if( .not. associated(bdyty(ibdyty)%drvstatus) ) return

    bdyty(ibdyty)%drvstatus(:) = 0

    do ivd = 1, bdyty(ibdyty)%nb_vlocy_driven_dof

      if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

      call owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

      ! en x 1, y 10, z 100
      bdyty(ibdyty)%drvstatus(inod) = bdyty(ibdyty)%drvstatus(inod)+(10.**(idof-1))  

    end do

    do ifd = 1, bdyty(ibdyty)%nb_force_driven_dof

      call owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inod,idof)

      ! en x 2, y 20, z 200
      bdyty(ibdyty)%drvstatus(inod) = bdyty(ibdyty)%drvstatus(inod)+(2*(10.**(idof-1)))  

    end do     

  end subroutine update_drvstatus

!!!------------------------------------------------------------------------   
  SUBROUTINE write_driven_dof(nfich)

    IMPLICIT NONE

    INTEGER :: ivd,ifd,iivd,iifd,nfich
    INTEGER :: ibdyty,inodty,idof
    INTEGER :: iM_bdyty,iM_nodty

    IF (nb_mecaMAILx== 0) RETURN

    DO ibdyty=1,SIZE(bdyty)

       IF (       bdyty(ibdyty)%nb_vlocy_driven_dof == 0 &
            .AND. bdyty(ibdyty)%nb_force_driven_dof == 0 ) CYCLE

       ! the ibdyty body has some driven dof
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)    'MAILx',bdyty2M_bdyty(ibdyty)
       WRITE(nfich,'(A6)') '$model'
       WRITE(nfich,'(A6)') ' MECAx'

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iivd=0
          iifd=0
          DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
             IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%vlocy_driven_dof(ivd))) THEN
                iivd=ivd
                EXIT
             END IF
          END DO
          DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%force_driven_dof(ifd))) THEN
                iifd=ifd
                EXIT
             END IF
          END DO

          IF (iivd > 0 .OR. iifd > 0) THEN
             ! the ibdyty body with inodty node has driven dof
             WRITE(nfich,'(A6)') '$nodty'
             iM_bdyty=bdyty2M_bdyty(ibdyty)
             iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
             !
             WRITE(nfich,101) get_nodNAME(M_bdyty(iM_bdyty)%nodty(iM_nodty)),iM_nodty
             ! write some 
             CALL write_a_driven_dof(nfich)

             DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
                IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%vlocy_driven_dof(ivd))) THEN
                   CALL write_a_driven_dof(nfich,'vlocy',bdyty(ibdyty)%vlocy_driven_dof(ivd))
                END IF
             END DO
             DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
                IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%force_driven_dof(ifd))) THEN
                   CALL write_a_driven_dof(nfich,'force',bdyty(ibdyty)%force_driven_dof(ifd))
                END IF
             END DO
          END IF
       END DO
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
    END DO
    
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
    CLOSE(nfich) 
      
101 FORMAT(1X,A5,I7)    
 
  END SUBROUTINE write_driven_dof
!!!------------------------------------------------------------------------   
  SUBROUTINE read_in_dof

    IMPLICIT NONE

    INTEGER          :: ibdyty,inodty,nbdof,iccdof
    INTEGER          :: iM_bdyty,iM_nodty,iM_ccdof
    CHARACTER(len=5) :: test,chnod
    INTEGER          :: itest,errare
                             !123456789012345678901234567890    
    CHARACTER(len=22)  :: IAM='mecaMAILx::read_in_dof'
    CHARACTER(len=103) :: cout
    REAL(kind=8) :: x(6)

    IF (nb_mecaMAILx == 0) RETURN
    
    DO ibdyty = 1,SIZE(bdyty)
       DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

          if ( M_INTEGRATOR_ID == INTEGRATOR_BETA2 )  &
            bdyty(ibdyty)%Xprev(iccdof+1:iccdof+nbdof)=0.d0

          bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) =0.D0
          bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) =0.D0

          bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)      =0.D0
          bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)      =0.D0
       ENDDO
       !am & pta : initialisation a 0 des vitesses et deplacements du centre
       !d'inertie du rigide equivalent
       IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
         bdyty(ibdyty)%RXbegin =0.D0
         bdyty(ibdyty)%RVbegin =0.D0

         bdyty(ibdyty)%RX      =0.D0
         bdyty(ibdyty)%RV      =0.D0
       ENDIF
    END DO
   
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty_MAILx(G_clin)                      
       IF (itest == ifound) THEN
          READ(G_clin(7:13),'(I7)') ibdyty
          IF (ibdyty <= 0 .OR. ibdyty > SIZE(bdyty)) THEN
             WRITE(cout,'(A12,I0,A60)') 'body number ',ibdyty,' does not belong to collection'
             CALL LOGMES('Error '//IAM//': '//cout)
             !STOP
          END IF
       ELSE
          CYCLE
       END IF

       ! fd recherche du model ...

       DO    
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'MECAx') CYCLE                ! fishing the MECAx part 

          DO    
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty' 
             DO
                IF( .NOT. read_G_clin()) EXIT
                itest = itest_nodty_MAILx(G_clin,ibdyty)
                IF (itest == isskip) CYCLE
                IF (itest == inomor) EXIT                      
                IF (itest == ifound) THEN
                  READ(G_clin(7:13),'(I7)') inodty
                  IF (inodty < 0 .OR. inodty > SIZE(bdyty(ibdyty)%nodty)) THEN 
                    WRITE(cout,'(A12,I0,A25,I0,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                    CALL FATERR(IAM,cout)
                  END IF

                  ! on lit les deplacements et les vitesses du centre
                  ! d'inertie du rigide equivalent + l'orientation du repere
                  ! principal d'inertie du rigide equivalent
                  
                  IF (inodty == 0 .AND. (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro)) THEN

                    select case (nbdime) 
                    case(2)
                      chnod='NO3xx'
                      CALL G_read_a_nodty(bdyty(ibdyty)%RXbegin,chnod)

                      !fd
                      !fd on lit les vitesses
                      !fd    
                      IF( .NOT. read_G_clin()) THEN
                        call faterr(IAM,'error reading Vbegin')
                      END IF
                
                      CALL G_read_a_nodty(bdyty(ibdyty)%RVbegin,chnod)
                      
                      bdyty(ibdyty)%RX = bdyty(ibdyty)%RXbegin     
                      bdyty(ibdyty)%RV = bdyty(ibdyty)%RVbegin

                      bdyty(ibdyty)%LocalFrameIni(1,1) = cos(bdyty(ibdyty)%RXbegin(3)) ; bdyty(ibdyty)%LocalFrameIni(1,2) =-sin(bdyty(ibdyty)%RXbegin(3))
                      bdyty(ibdyty)%LocalFrameIni(2,1) = sin(bdyty(ibdyty)%RXbegin(3)) ; bdyty(ibdyty)%LocalFrameIni(2,2) = cos(bdyty(ibdyty)%RXbegin(3))

                      bdyty(ibdyty)%LocalFrame(1:2,1:2)=bdyty(ibdyty)%LocalFrameIni(1:2,1:2)
                      bdyty(ibdyty)%LocalFrameTT(1:2,1:2)=bdyty(ibdyty)%LocalFrameIni(1:2,1:2)

                      
                      PRINT*,ibdyty,' a une contribution rigide '
                      PRINT*,'rx ',bdyty(ibdyty)%RX
                      PRINT*,'rv ',bdyty(ibdyty)%RV
                      PRINT*,'a ',bdyty(ibdyty)%LocalFrame(:,1)
                      PRINT*,'b ',bdyty(ibdyty)%LocalFrame(:,2)
                       
                    case(3)   
                      chnod='NO6xx'
                      !fd
                      !fd on lit les deplacements
                      !fd    
                      CALL G_read_a_nodty(x,chnod)
                      bdyty(ibdyty)%RXbegin(1:nbdime) = x(1:nbdime)
                      !fd
                      !fd on lit les vitesses
                      !fd    
                      IF( .NOT. read_G_clin()) THEN
                        call faterr(IAM,'error reading Vbegin')
                      END IF
                
                      CALL G_read_a_nodty(bdyty(ibdyty)%RVbegin,chnod)
                      
                      bdyty(ibdyty)%RX = bdyty(ibdyty)%RXbegin     
                      bdyty(ibdyty)%RV = bdyty(ibdyty)%RVbegin
          
                      !fd
                      !fd on lit alpha
                      !fd
                      chnod='NO3xx'
                      IF( .NOT. read_G_clin()) THEN
                        call faterr(IAM,'error reading alpha')
                      END IF
                      CALL G_read_a_nodty(bdyty(ibdyty)%LocalFrameIni(1:3,1),chnod)
                      !fd
                      !fd on lit beta
                      !fd
                      IF( .NOT. read_G_clin()) THEN
                        call faterr(IAM,'error reading beta')
                      END IF
                      CALL G_read_a_nodty(bdyty(ibdyty)%LocalFrameIni(1:3,2),chnod)
                      !fd  
                      !fd on lit gamma
                      !fd
                      IF( .NOT. read_G_clin()) THEN
                        call faterr(IAM,'error reading gamma')
                      END IF
                      CALL G_read_a_nodty(bdyty(ibdyty)%LocalFrameIni(1:3,3),chnod)
                    
                      bdyty(ibdyty)%LocalFrame(1:3,1:3)=bdyty(ibdyty)%LocalFrameIni(1:3,1:3)
                      bdyty(ibdyty)%LocalFrameTT(1:3,1:3)=bdyty(ibdyty)%LocalFrameIni(1:3,1:3)

                      PRINT*,ibdyty,' a une contribution rigide '
                      PRINT*,'rx ',bdyty(ibdyty)%RX
                      PRINT*,'rv ',bdyty(ibdyty)%RV
                      PRINT*,'a ',bdyty(ibdyty)%LocalFrame(:,1)
                      PRINT*,'b ',bdyty(ibdyty)%LocalFrame(:,2)
                      PRINT*,'c ',bdyty(ibdyty)%LocalFrame(:,3)

                      CYCLE

                   case default
                      call faterr(IAM,'unsupported dimension')
                   end select

                  !am & pta : on s'arrete si on trouve la description du centre
                  !d'inertie pour un corps deformable classique
                  ELSE IF (inodty == 0 .AND. .NOT. (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro)) THEN
                    WRITE(cout,'(A12,I0,A25,I0,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                    CALL FATERR(IAM,cout)
                  END IF


                  !fd merde du centre d inertie
                  IF (inodty == 0 ) cycle
                  
                  IF (get_node_id_from_name(G_clin(2:6)) > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN 
                     WRITE(cout,'(A5,I0,A22,I0)') G_clin(2:6),inodty,' is not nodty of body ', ibdyty
                     CALL FATERR(IAM,cout)
                  END IF

                  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                  iccdof=bdyty(ibdyty)%ccdof(inodty)

                  chnod=G_clin(2:6)

                  ! lecture X(tps-H)
                  CALL G_read_a_nodty(bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof),chnod)

                  IF( .NOT. read_G_clin()) EXIT 

                  ! lecture V(tps-H)
                  CALL G_read_a_nodty(bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof),chnod)

                  select case( M_INTEGRATOR_ID )
                  case( INTEGRATOR_BETA2 )

                    ! on est au chargement du pas de temps en cours 

                    ! X(tps-H)
                    bdyty(ibdyty)%Xprev(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)

                    ! X(tps) = X(tps-H)+H*V(tps-H) <- approximation
                    bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) + &
                                                                 (H * bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof))

                    ! X(tps+H) = 0.
                    bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) = 0.d0

                    ! V(tps) = 0.
                    bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) = 0.d0 

                  case( INTEGRATOR_MOREAU ) 
                    ! normalement ca ne devrait pas etre util 
                    bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
                    bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
                  case( INTEGRATOR_QS ) 
                    ! normalement ca ne devrait pas etre util 
                    bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) = 0.d0 !bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
                    bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) = 0.d0 !bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
                  end select
                  CYCLE
                ENDIF
             END DO !les valeurs aux nodty
             EXIT       
          END DO ! $nodty 
          EXIT
       END DO ! $model
    END DO ! $bdyty


    ! actualisation des cordonnees du maillage coor(tps-H)

    select case( M_INTEGRATOR_ID )
    case( INTEGRATOR_BETA2 )

      DO ibdyty=1,SIZE(bdyty)
        iM_bdyty=bdyty2M_bdyty(ibdyty)
        DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
          M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+2)= &
               M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+2) + &
               bdyty(ibdyty)%Xprev(iccdof+1:iccdof+2)                
        END DO
      END DO

    case( INTEGRATOR_MOREAU )    

      DO ibdyty=1,SIZE(bdyty)
        iM_bdyty=bdyty2M_bdyty(ibdyty)
        DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
          M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+2)= &
            M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+2) + &
            bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+2)                
        END DO
      END DO
     
    case( INTEGRATOR_QS )    

      DO ibdyty=1,SIZE(bdyty)
        iM_bdyty=bdyty2M_bdyty(ibdyty)
        DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
          M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+2)= &
            M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+2) + &
            bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+2)                
        END DO
      END DO

    end select

  END SUBROUTINE read_in_dof
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_dof(nfich,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: ifrom,ito
    INTEGER :: ibdyty,inodty,iccdof,nfich,nbdof
    INTEGER :: lc 

    REAL(kind=8) :: x(6)
   
    IF (nb_mecaMAILx == 0) RETURN

  
    ! TODO pour is_rigid: reconstruire le mvt global = mvt local en rep global + mvt corps rigide 

    select case( M_INTEGRATOR_ID )
    case( INTEGRATOR_BETA2 )
      DO ibdyty = ifrom,ito
       
         WRITE(nfich,'(A6)') '$bdyty'
         WRITE(nfich,101)     'MAILx',ibdyty
         WRITE(nfich,'(A6)') '$model'
         WRITE(nfich,'(A6)') ' MECAx'
         WRITE(nfich,'(A6)') '$nodty'

         DO inodty = 1,SIZE(bdyty(ibdyty)%nodty) 
   
            nbdof  = nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
            iccdof = bdyty(ibdyty)%ccdof(inodty)

            !fd 0_o attention on sauve au temps TPS apres le update dof

            CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                               bdyty(ibdyty)%Xprev(iccdof+1:iccdof+nbdof), &
                               'X  ',nfich)

            !fd 0_o
            CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                               bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof), &
                               'V  ',nfich)
         END DO

         WRITE(nfich,'(A6)')'$$$$$$'
         WRITE(nfich,'(A6)')'      '
      END DO
    case(INTEGRATOR_MOREAU)
      DO ibdyty = ifrom,ito
       
         WRITE(nfich,'(A6)') '$bdyty'
         WRITE(nfich,101)     'MAILx',ibdyty
         WRITE(nfich,'(A6)') '$model'
         WRITE(nfich,'(A6)') ' MECAx'
         WRITE(nfich,'(A6)') '$nodty'

         DO inodty = 1,SIZE(bdyty(ibdyty)%nodty) 
   
            nbdof  = nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
            iccdof = bdyty(ibdyty)%ccdof(inodty)

            CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                               bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof), &
                               'X  ',nfich)

            CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                               bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof), &
                               'V  ',nfich)
         END DO
         !am & pta : si corps rigide est attache au maille, on ecrit :
         ! les depalcements du centre d'inertie, les vitesses de corps rigides du
         ! rigide equivalent + l'orientation principal d'inertie du rigide
         ! equivalent 
         IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
           WRITE(nfich,'("!rigid part")') 
           select case(nbdime)
           case(2)
             CALL write_a_nodty('NO3xx',0, bdyty(ibdyty)%RX,'X  ',nfich)
             CALL write_a_nodty('NO3xx',0, bdyty(ibdyty)%RV,'V  ',nfich)
           case(3)   
             x=0.d0
             x(1:nbdime) = bdyty(ibdyty)%RX(1:nbdime)
             CALL write_a_nodty('NO6xx',0, x               ,'X  ',nfich)
             CALL write_a_nodty('NO6xx',0, bdyty(ibdyty)%RV,'V  ',nfich)
             CALL write_a_nodty('alpha',0, &
                                bdyty(ibdyty)%LocalFrame(1:3,1),'   ',nfich)
             CALL write_a_nodty('beta ',0, &
                                bdyty(ibdyty)%LocalFrame(1:3,2),'   ',nfich)
             CALL write_a_nodty('gamma',0, &
                                bdyty(ibdyty)%LocalFrame(1:3,3),'   ',nfich)
           end select  
         ENDIF

         WRITE(nfich,'(A6)')'$$$$$$'
         WRITE(nfich,'(A6)')'      '
      END DO
      
    case(INTEGRATOR_QS)
         
      DO ibdyty = ifrom,ito
       
         WRITE(nfich,'(A6)') '$bdyty'
         WRITE(nfich,101)     'MAILx',ibdyty
         WRITE(nfich,'(A6)') '$model'
         WRITE(nfich,'(A6)') ' MECAx'
         WRITE(nfich,'(A6)') '$nodty'

         DO inodty = 1,SIZE(bdyty(ibdyty)%nodty) 
   
            nbdof  = nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
            iccdof = bdyty(ibdyty)%ccdof(inodty)

            CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                               bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof), &
                               'X  ',nfich)

            CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                               bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof), &
                               'V  ',nfich)
         END DO

         WRITE(nfich,'(A6)')'$$$$$$'
         WRITE(nfich,'(A6)')'      '

      END DO

    end select

    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
    
101 FORMAT(1X,A5,I7)            
           
  END SUBROUTINE write_out_dof
!!!------------------------------------------------------------------------
  SUBROUTINE read_in_gpv

    IMPLICIT NONE

    INTEGER          :: ibdyty,iblmty,itest
    INTEGER          :: iM_bdyty,iM_blmty
    INTEGER          :: errare,mdlnb,nb_external,nb_internal,ig,i
                              !123456789012345678901234567890    
    CHARACTER(len=22)  :: IAM='mecaMAILx::read_in_gpv'
    CHARACTER(len=103) :: cout
    CHARACTER(len=5)   :: ctest

    IF (nb_mecaMAILx == 0) RETURN
    
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest == ifound) THEN
          READ(G_clin(7:13),'(I7)') iM_bdyty
          IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
             WRITE(cout,'(A12,I0,A60)') 'body number ',iM_bdyty,' does not belong to collection'
             CALL FATERR(IAM,cout)
          END IF
       ELSE
          CYCLE
       END IF

       DO
          IF( .NOT. read_G_clin()) EXIT           
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE                 ! fishing for the keyword 'blmty'

          IF( .NOT. read_G_clin()) EXIT

          !fd a voir l'utilisation de test_blmty de MAILx 
          READ(G_clin(7:13),'(I7)') iM_blmty     
          SELECT CASE(G_clin(2:6))
          CASE('T3xxx','T6xxx','Q4xxx','Q8xxx','H8xxx','H20xx','TE4xx','TE10x','PRI6x','PRI15')
             ctest = 'found'
          CASE('     ')
             ctest = 'sskip'
             ! derriere un blmty maille on a forcement un nodty !
          CASE('nodty')
             ctest = 'nomor'
          CASE default
             WRITE(cout,'(A7,A5,A34)')' blmty ',G_clin(2:6),' unknown in read_in_gpv '
             call faterr(IAM,cout)
          END SELECT

          ! fd recherche du model ...

          DO    
             IF ( .NOT. read_G_clin()) EXIT 
             IF (G_clin(2:6) == '$$$$$') THEN
                BACKSPACE(G_nfich)
                EXIT
             ENDIF
             IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
             IF ( .NOT. read_G_clin()) THEN
                CALL FATERR(IAM,'Problem reading model')
             ENDIF
             
             IF (G_clin(2:6) /= 'MECAx') CYCLE                ! fishing the MECAx part 
             
             
             ibdyty = M2meca(iM_bdyty)%bdyty
             iblmty = M2meca(iM_bdyty)%blmty(iM_blmty)
             
             mdlnb = bdyty(ibdyty)%blmty(iblmty)%mdlnb
             
             nb_external=modelz(mdlnb)%nb_external_variables
             nb_internal=modelz(mdlnb)%nb_internal_variables
             
             IF (nb_external == 0) THEN
                write(cout,'(A)') 'Material without external variable !'
                write(cout,'(A)') 'Check DATBOX/BODIES.DAT and/or DATBOX/MODELS.DAT'
                call faterr(IAM,cout)
             ENDIF
             
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv,dim=1)
                
                IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading meca_gpv values')
                
                ! gerer le bordel sur plusieurs lignes
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress(i)       
                ENDDO
                
                M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%stress = &
                     M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%stress       
                
                IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading meca_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain(i)       
                ENDDO
                
                M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%strain = &
                     M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%strain       
                
                IF (nb_internal /= 0 ) THEN
                   IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading meca_gpv values')
                   
                   DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal)
                      READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal(i)       
                   ENDDO
                   
                   M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,1)%internal = &
                        M_bdyty(ibdyty)%blmty(iblmty)%meca_gpv(ig,2)%internal       
                ENDIF
                
             ENDDO
             EXIT
          ENDDO ! $model
          CYCLE
       ENDDO !blmty
       CYCLE
    END DO ! $bdyty
   
  END SUBROUTINE read_in_gpv
!------------------------------------------------------------------------
!------------------------------------------------------------------------

  !> initializes nodal unknown fields X,V,.. 
  !> computes Vdriv, Xdriv

  SUBROUTINE increment_mecaMAILx 

   IMPLICIT NONE 

   INTEGER      :: ibdyty,iblmty,ivd,inod,idof,iccdof
   REAL(kind=8) :: Vdrivenbegin,Vdriven,Xdrivenbegin,Xdriven
   REAL(kind=8) :: spin(3) 
   REAL(kind=8) :: TTH,UMTTH,RTTH,UMRTTH

                              !12345678901234567890
   character(len=20) :: IAM = 'mecaMAILx::increment'


   IF (nb_mecaMAILx == 0) RETURN

   !fd mesure d hygiene
   do ibdyty=1,nb_mecaMAILx
     call nullify_reac_mecaMAILx(ibdyty,iIreac)
     call nullify_reac_mecaMAILx(ibdyty,iIaux_)      
   enddo 

   IF (is_externalFEM) THEN

     !fd a voir si c'est vraiment sa place
     DO ibdyty=1,SIZE(bdyty)    
       bdyty(ibdyty)%Vfree=0.d0
     ENDDO

     CALL externalFEM_increment(TPS,H)

     RETURN
   ENDIF

   DO ibdyty=1,SIZE(bdyty)

      IF (.NOT. bdyty(ibdyty)%visible) CYCLE

      select case (M_INTEGRATOR_ID)
      case( INTEGRATOR_BETA2 )

        !fd 0_o on est sur l'intervalle de temps [tpsbegin, tps]
   
        !fd 0_o initialisation de V(tps+0.5) 

        bdyty(ibdyty)%V = 0.d0

        !fd 0_o initialisation de X(tps+H)  

        bdyty(ibdyty)%X = 0.d0

        !fd 0_o on impose V a tps+0.5 ; c'est faux si h^+ /= h^- et si beta2 /= 0.5

        DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
          iccdof=bdyty(ibdyty)%ccdof(inod)+idof

          CALL comp_a_driven_dof_at_t(bdyty(ibdyty)%vlocy_driven_dof(ivd),TPS+0.5D0*H,Vdriven)

          !fd 0_o X(tps+H) = X(tps) + H*V(tps+0.5)
          bdyty(ibdyty)%Xdriv(ivd) = bdyty(ibdyty)%Xbegin(iccdof) + H*Vdriven
          bdyty(ibdyty)%Vdriv(ivd) = Vdriven
          bdyty(ibdyty)%VdrivBeg(ivd) = 0.d0

        END DO

      case( INTEGRATOR_MOREAU )

        TTH = theta*H
        UMTTH = (1.d0 - theta)*H

        RTTH = Rtheta*H
        UMRTTH = (1.d0 - Rtheta)*H

        !on place le corps rigide equivalent dans la configuration
        !intermediaire : integration des vitesses de translation + calcul de la
        !nouvelle orientation du repere principal d'inertie (integration des rotations)

        IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

           !fd prediction des valeurs dans la config courante
           bdyty(ibdyty)%RV = bdyty(ibdyty)%RVbegin                


           SELECT CASE (nbdime) 
           CASE(2)

             bdyty(ibdyty)%RX = bdyty(ibdyty)%RXbegin + &
                                UMRTTH*bdyty(ibdyty)%RVbegin + &
                                RTTH*bdyty(ibdyty)%RV
              
             CALL update_inertia_frame22(0,H,bdyty(ibdyty)%RVbegin(3),bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrame)
             
           CASE(3)

             !fd on ne prend que la partie translation 
             bdyty(ibdyty)%RX(1:nbdime) = bdyty(ibdyty)%RXbegin(1:nbdime) + &
                                          UMRTTH*bdyty(ibdyty)%RVbegin(1:nbdime) + &
                                          RTTH*bdyty(ibdyty)%RV(1:nbdime)
              
             spin(1:3) = bdyty(ibdyty)%RV(4:6)
             CALL update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrame)
             
           END SELECT

        ENDIF

        !fd %V est il une prediction ? 
        ! car si on adapte le pas de temps faudrait peut-etre modifier %V 

        bdyty(ibdyty)%V= bdyty(ibdyty)%Vbegin                

        !fd ne sert a rien !! 
        bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin+ &
                         (UMTTH* bdyty(ibdyty)%Vbegin)+ &
                         (TTH* bdyty(ibdyty)%V)

        DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
          iccdof=bdyty(ibdyty)%ccdof(inod)+idof

          CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)

          Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
          Xdriven    = Xdrivenbegin + (UMTTH*Vdrivenbegin)+(TTH*Vdriven)
          bdyty(ibdyty)%Vdriv(ivd) = Vdriven
          bdyty(ibdyty)%VdrivBeg(ivd) = Vdrivenbegin
          bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

        END DO

        !fd pour %V pas mis a %Vdriven ?

      case( INTEGRATOR_QS )

        !fd %V est il une prediction de l increment de deplacement 
        ! on adapte si le pas de temps a changé 

        bdyty(ibdyty)%V = (H/oldH)*bdyty(ibdyty)%Vbegin                

        !fd %X est une prediction du deplacement
        bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + bdyty(ibdyty)%V

        DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
          iccdof=bdyty(ibdyty)%ccdof(inod)+idof

          CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)

          !Vdriven est une vitesse mmais %Vdriv un increment de déplacement
          
          Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
          Xdriven    = Xdrivenbegin + H*Vdriven
          
          bdyty(ibdyty)%Vdriv(ivd)    = H*Vdriven
          bdyty(ibdyty)%VdrivBeg(ivd) = oldH*Vdrivenbegin
          bdyty(ibdyty)%Xdriv(ivd)    = Xdriven 

        END DO

      case default     

        call faterr(IAM,'Unkown integrator')

      end select  

      !fd mesure salutaire
      do iblmty = 1, size(bdyty(ibdyty)%blmty)
        bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) = 0.d0
        bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) = 0.d0
      end do

   END DO

 END SUBROUTINE increment_mecaMAILx 
!------------------------------------------------------------------------
!------------------------------------------------------------------------

 !> computes free velocity Vfree

 subroutine compute_free_vlocy_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   integer(kind=4) :: ivd,inod,idof,iccdof,jdofR
   INTEGER :: i,info, ivd_

   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: RForce  

!                           123456789012345678901234567890
   CHARACTER(len=30) :: IAM='mecaMAILx::compute_free_vlocy'
   CHARACTER(len=80) :: cout

   REAL(kind=8),DIMENSION(6)      :: v_loc
   REAL(kind=8),DIMENSION(:),allocatable :: vR_glob
   real(kind=8) :: norm,xx,yy

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   select case (M_INTEGRATOR_ID)
   case( INTEGRATOR_BETA2 )

     ! vitesse libre purement defo

     bdyty(ibdyty)%Vfree = 0.d0

     ! le second membre de g_sys est RHS

     call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%RHS)

     ivd_=0
     if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
       ! on collecte les valeurs des ddl imposes
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

         if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

         ivd_=ivd_+1
         CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

         iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

         bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd)

       ENDDO
     
       ! on les passe au g_system

       call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

     endif

     ! on resout

     CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vfree,info)

     IF (info > 0) THEN
       PRINT*,ibdyty
       CALL FATERR(IAM,'No solution')
     ENDIF

     !print*,'Vfree' 
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree


     ! si rien n'a ete demande explicitement pour la config de contact 
     ! on calcule avec des valeurs par defaut

     if (.not. is_contactdetectionconfiguration_defined) then

        ! on detecte au debut du pas ...

        call compute_configurationTT_mecaMAILx(ibdyty)

        !call logmes('mecaMAILx::contact configuration automatically computed')

     endif

   case( INTEGRATOR_MOREAU )

   !   la formulation utilisee est incrementale qu'on soit en hpp ou en gd
   !   %V contient la prediction de la vitesse qui peut etre differente de %Vbegin qui 
   !   est la vitesse a la fin du pas precedent


     !IF (itchache) THEN

       ! PRINT*,'objet ',ibdyty

       ! PRINT*,'prediction de la vitesse:'
       ! PRINT*,bdyty(ibdyty)%V
  
       ! PRINT*,' '
       ! PRINT*,'RHS'
       ! if (nbdime == 2) then
       !   write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
       ! else if (nbdime == 3) then
       !   write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
       ! endif       


       ! PRINT*,'==========================='

     !ENDIF

     !if (ibdyty == 45) then
     !  print*,'RHS'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     !endif

     ! vitesse libre rigide

     IF (bdyty(ibdyty)%is_rigid) then

       bdyty(ibdyty)%RVfree= bdyty(ibdyty)%RVbegin + H*bdyty(ibdyty)%inv_mR*(bdyty(ibdyty)%RFext+bdyty(ibdyty)%RFint)

       IF (bdyty(ibdyty)%nb_RV_driven /= 0) THEN
         DO ivd=1,bdyty(ibdyty)%nbdofR
           IF (bdyty(ibdyty)%RV_driven_dof(ivd) /= 0)  bdyty(ibdyty)%RVfree(ivd) =  bdyty(ibdyty)%RV_driven(ivd) 
         ENDDO
       ENDIF


     ! vitesse libre coro

     else if (bdyty(ibdyty)%is_coro) THEN

       bdyty(ibdyty)%RIaux= H*(bdyty(ibdyty)%RFext+bdyty(ibdyty)%RFint)

       bdyty(ibdyty)%RVfree= bdyty(ibdyty)%RVbegin + bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIaux

       IF (bdyty(ibdyty)%nb_RV_driven /= 0) THEN
         DO ivd=1,bdyty(ibdyty)%nbdofR
           IF (bdyty(ibdyty)%RV_driven_dof(ivd) /= 0)  bdyty(ibdyty)%RVfree(ivd) =  bdyty(ibdyty)%RV_driven(ivd) 
         ENDDO
       ENDIF


       ! contribution de l'acceleration mR*(RVfree - RVbegin) = RIaux
       ! sur le second membre defo 
       ! le 1/H disparait car on integre cette contribution

       IF (bdyty(ibdyty)%nb_RV_driven /= 0) THEN
         do i=1,bdyty(ibdyty)%nbdofR
           IF (bdyty(ibdyty)%RV_driven_dof(i) /= 0) then
             ! pour les noeuds bloques on rajoute le bout pas dans Vfree
              bdyty(ibdyty)%RIaux(i) = bdyty(ibdyty)%RIaux(i) + &
                                       bdyty(ibdyty)%mR(i)*(bdyty(ibdyty)%RVfree(i)-bdyty(ibdyty)%RVbegin(i))
           endif
         enddo
       endif 

       if (.false.) then
       select case(nbdime)
       case(2)
         write(*,'(A,3(1x,D12.5))') 'RVbeg', bdyty(ibdyty)%RVbegin(:)
         write(*,'(A,3(1x,D12.5))') 'RFext', bdyty(ibdyty)%RFext(:)
         write(*,'(A,3(1x,D12.5))') 'RFint', bdyty(ibdyty)%RFint(:)
         write(*,'(A,3(1x,D12.5))') 'RVfre', bdyty(ibdyty)%RVfree(:)
         write(*,'(A,3(1x,D12.5))') 'RIaux', bdyty(ibdyty)%RIaux
       case(3)   
         write(*,'(A,6(1x,D12.5))') 'RVbeg', bdyty(ibdyty)%RVbegin(:)
         write(*,'(A,6(1x,D12.5))') 'RFext', bdyty(ibdyty)%RFext(:)
         write(*,'(A,6(1x,D12.5))') 'RFint', bdyty(ibdyty)%RFint(:)
         write(*,'(A,6(1x,D12.5))') 'RVfre', bdyty(ibdyty)%RVfree(:)
         write(*,'(A,6(1x,D12.5))') 'RIaux', bdyty(ibdyty)%RIaux
       end select 
       endif
       
       ! on calcule : Iaux = (D2R)^T*(mR(RVfree-RVbegin))

       bdyty(ibdyty)%Iaux=0.d0
       do idof=1,bdyty(ibdyty)%nbdof  
         do jdofR=1,bdyty(ibdyty)%nbdofR
           bdyty(ibdyty)%Iaux(idof) = bdyty(ibdyty)%Iaux(idof) + &
                                      (bdyty(ibdyty)%D2R(jdofR, idof) * bdyty(ibdyty)%RIaux(jdofR))
         enddo
       enddo

       if (.false.) then
       select case(nbdime)
       case(2)
         print*,'RHS' 
         write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS(1:8)
         print*,'(D2R)^T*(mR*(RVfree-RVbegin))'  
         write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Iaux(1:8)
         xx=0.d0
         yy=0.d0
         do idof=1,bdyty(ibdyty)%nbdof
           if (mod(idof,2) == 0) then
             xx = xx + bdyty(ibdyty)%Iaux(idof)
             yy = yy + bdyty(ibdyty)%RHS(idof)              
           endif 
         enddo
         print*,xx/H, bdyty(ibdyty)%Rfext(2),yy
         
       case(3)
         print*,'RHS' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS(1:9)
         print*,'(D2R)^T*(mR*(RVfree-RVbegin))'  
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Iaux(1:9)
       end select
       endif 
       ! on modifie RHS du defo

       bdyty(ibdyty)%RHS = bdyty(ibdyty)%RHS - bdyty(ibdyty)%Iaux


       !!$     !fd pour les ddl bloques on injecte une force pour annuler le mouvement du maillage
       !!$
       !!$     IF (bdyty(ibdyty)%nb_RV_driven /= 0) THEN
       !!$       !print*,'correction'  
       !!$       bdyty(ibdyty)%RIaux=0.d0
       !!$       DO ivd=1,bdyty(ibdyty)%nbdofR
       !!$          IF (bdyty(ibdyty)%RV_driven_dof(ivd) /= 0) then
       !!$            bdyty(ibdyty)%RIaux(ivd) = bdyty(ibdyty)%mR(ivd)*(bdyty(ibdyty)%RVfree(ivd) - bdyty(ibdyty)%RVbegin(ivd)) &
       !!$                                       - H*(bdyty(ibdyty)%RFext(ivd)+bdyty(ibdyty)%RFint(ivd))
       !!$
       !!$            !write(*,'(I0,6(1x,D12.5))'),ivd, bdyty(ibdyty)%RIaux(ivd)
       !!$          ENDIF
       !!$       ENDDO
       !!$
       !!$       bdyty(ibdyty)%Iaux=0.d0
       !!$       do idof=1,bdyty(ibdyty)%nbdof  
       !!$         !write(*,'(6(1x,D12.5))') bdyty(ibdyty)%D2R(:,idof)
       !!$         do jdofR=1,bdyty(ibdyty)%nbdofR
       !!$           IF (bdyty(ibdyty)%RV_driven_dof(jdofR) == 0) cycle         
       !!$           bdyty(ibdyty)%Iaux(idof) = bdyty(ibdyty)%Iaux(idof) + &
       !!$                                      (bdyty(ibdyty)%D2R(jdofR, idof) * bdyty(ibdyty)%RIaux(jdofR))
       !!$
       !!$           !write(*,'(I0,1x,I0,3(1x,D12.5))')idof,jdofR,bdyty(ibdyty)%RIaux(jdofR),bdyty(ibdyty)%D2R(jdofR,idof),bdyty(ibdyty)%Iaux(idof)
       !!$         enddo
       !!$       enddo
       !!$       !print*,'(D2R)^T*(correction)'
       !!$       !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Iaux(1:9)
       !!$
       !!$       bdyty(ibdyty)%RHS = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Iaux
       !!$
       !!$     ENDIF
       !!$
     ENDIF

     ! vitesse libre purement defo

     bdyty(ibdyty)%Vfree = 0.d0

     IF (.not. bdyty(ibdyty)%is_rigid .or. .NOT. bdyty(ibdyty)%skip_defo_comp) THEN

       !
       !print*,'RHS'
       !if (nbdime==2) then
       !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
       !else if (nbdime==3) then    
       !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
       !endif
       !print*,'---'
        
       ! le second membre de g_sys est RHS
       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%RHS)


       if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
         ! on collecte les valeurs des ddl imposes
         ivd_=0 
         DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

           if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle    

           ivd_=ivd_+1 
           
           CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

           iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

           bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd) - bdyty(ibdyty)%V(iccdof)

           !print*,ivd,iccdof,bdyty(ibdyty)%vdriv(ivd),bdyty(ibdyty)%drvvalues(ivd)
           
         ENDDO
     
         ! on les passe au g_system

         call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

       endif

       ! on resoud
       CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vfree,info)

       ! cas sym band ; rendre matrice publique
       ! PRINT*,' '
       ! PRINT*,'la matrice'
       ! PRINT*,bdyty(ibdyty)%g_sys%matrix%id
       ! PRINT*,bdyty(ibdyty)%g_sys%matrix%sym_band%bw
       ! PRINT*,bdyty(ibdyty)%g_sys%matrix%sym_band%n       
       ! PRINT*,bdyty(ibdyty)%g_sys%matrix%sym_band%V

       
       IF (info > 0) THEN
         PRINT*,ibdyty
         CALL FATERR(IAM,'No solution')
       ENDIF

       ! print*,'Vfree' 
       ! write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree

       bdyty(ibdyty)%Vfree=  bdyty(ibdyty)%V + bdyty(ibdyty)%Vfree

       ! print*,'V + Vfree' 
       ! write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree


       if (bdyty(ibdyty)%is_coro) then
         !fd paranoiac check


         v_loc(1:bdyty(ibdyty)%nbdofR) = matmul(bdyty(ibdyty)%D2R,bdyty(ibdyty)%Vfree) 

         norm = sqrt(dot_product(v_loc,v_loc))
         if( norm > tol_coro ) then
           write(cout,'(A,I0,1x,D12.5)') '@WARNING || L VDfree ||',ibdyty, norm
           call logmes(cout,.true.)
         !else
         !  write(cout,'(A,I0,1x,D12.5)') '|| L VDfree ||',ibdyty, norm
         !  call logmes(cout)
         end if

         !allocate(vR_glob(bdyty(ibdyty)%nbdof))
         !vR_glob= matmul(bdyty(ibdyty)%R2D,v_loc(1:bdyty(ibdyty)%nbdofR))

         !write(*,'(A,I0,1x,D12.5)') 'GLVfree',ibdyty,max(maxval(vR_glob),dabs(minval(vR_glob)))

         !if (maxval(vR_glob) > tol_coro .or. dabs(minval(vR_glob)) > tol_coro) then  !!! pta
         !  print*,'vR_glob: ',ibdyty
         !  if (nbDIME==2) then
         !    write(*,'(3(1x,D12.5))') vR_glob
         !  else if (nbDIME==3) then
         !    write(*,'(3(1x,D12.5))') vR_glob
         !  endif
         !  call FATERR(IAM,'rigid body free velocity too high for coro scheme')
         !endif

         !deallocate(vR_glob)

       endif

     ELSE

       !fd c'est mieux que ca      bdyty(ibdyty)%Vfree= bdyty(ibdyty)%V
       bdyty(ibdyty)%Vfree= 0.d0
 
     ENDIF

     !print*,'objet ',ibdyty
     !print*,'Vfree'
     !if (nbdime == 2) then
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Vfree
     !else if (nbdime == 3) then
     !
     !print*,'Vfree' 
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree
     !endif       

     !max(maxval(bdyty(ibdyty)%Vfree),abs(minval(bdyty(ibdyty)%Vfree)))

     !if (ibdyty == 45)  then
     !  print*,'Vfree' 
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree
     !endif


     !fd si rien n'a ete demande explicitement pour la config de contact 
     !   on calcule avec des valeurs par defaut

     if (.not. is_contactdetectionconfiguration_defined) then

        call compute_configurationTT_mecaMAILx(ibdyty)

        !call logmes('mecaMAILx::contact configuration automatically computed')

     endif

   case( INTEGRATOR_QS )

     !   la formulation utilisee est incrementale qu'on soit en hpp ou en gd
     !   %V contient la prediction de l'increment de deplacement qui peut etre differente de %Vbegin qui 
     !   est l'increment de deplacement a la fin du pas precedent


     ! vitesse libre purement defo

     bdyty(ibdyty)%Vfree = 0.d0

     ! le second membre de g_sys est RHS

     ! print*,'RHS' 
     ! write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS

     
     call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%RHS)

     if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
       ! on collecte les valeurs des ddl imposes

       ivd_=0 
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

         if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle 

         ivd_=ivd_+1
         
         CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
          
         iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

         bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd) - bdyty(ibdyty)%V(iccdof)

       ENDDO
     
       ! on les passe au g_system

       call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

     endif

     ! on resout
     CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vfree,info)

     IF (info > 0) THEN
       PRINT*,ibdyty
       CALL FATERR(IAM,'No solution')
     ENDIF

     ! print*,'correction de Vfree' 
     ! write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree

     bdyty(ibdyty)%Vfree=  bdyty(ibdyty)%V + bdyty(ibdyty)%Vfree

     CALL apply_vlocy_driven_dof(ibdyty,iVfree)
     
     !print*,'V + Vfree' 
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree


     !print*,'objet ',ibdyty
     !print*,'Vfree'
     !if (nbdime == 2) then
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Vfree
     !else if (nbdime == 3) then
     !
     !print*,'Vfree' 
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree
     !endif       

     !max(maxval(bdyty(ibdyty)%Vfree),abs(minval(bdyty(ibdyty)%Vfree)))

     !if (ibdyty == 45)  then
     !  print*,'Vfree' 
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree
     !endif


     !fd si rien n'a ete demande explicitement pour la config de contact 
     !   on calcule avec des valeurs par defaut

     if (.not. is_contactdetectionconfiguration_defined) then

        call compute_configurationTT_mecaMAILx(ibdyty)

        !call logmes('mecaMAILx::contact configuration automatically computed')

     endif

   case default

     call faterr(IAM,'Unknown integrator')

   end select

end subroutine compute_free_vlocy_mecaMAILx

!------------------------------------------------------------------------    
!------------------------------------------------------------------------

subroutine externalFEM_compute_free_vlocy_mecaMAILx
  implicit none
  integer(kind=4) :: ibdyty

  if( nb_mecaMAILx == 0 ) return

  call externalFEM_compute_free_vlocy

  do ibdyty = 1, nb_mecaMAILx
    call externalFEM_get_free_vlocy(ibdyty,bdyty(ibdyty)%Vfree)
  end do

end subroutine
 
!------------------------------------------------------------------------    
!------------------------------------------------------------------------

 !> computes velocities and displacement (V and X)

 subroutine compute_dof_mecaMAILx(ibdyty)
   implicit none 
   integer(kind=4), intent(in) :: ibdyty
   !
   integer(kind=4) :: ivd,ibdy,inod,idof,iccdof,info,i,ivd_
   CHARACTER(len=40) :: cout
   CHARACTER(len=20) ::fmt

   REAL(kind=8) :: UMTTH,TTH,RTTH,UMRTTH,norm
   REAL(kind=8),DIMENSION(3)      :: spin
   REAL(kind=8),DIMENSION(6)      :: v_loc
   REAL(kind=8),DIMENSION(:),allocatable :: vaux_glob,vR_glob

   logical :: oldies =.TRUE.

   !                         12345678901234567890123
   CHARACTER(len=23) :: IAM='mecaMAILx::compute_dof '


   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   select case( M_INTEGRATOR_ID )
   case( INTEGRATOR_BETA2 )

     if ( bdyty(ibdyty)%is_reac_modified) then

       !fd 0_o Reac qui est l'impulsion a ete rescale ... 

       bdyty(ibdyty)%Iaux = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Ireac

       ! le second membre de g_sys est Iaux
       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)
  
       if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
         ! on collecte les valeurs des ddl imposes


         ivd_=0 
         DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

           if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

           ivd_=ivd_+1
           
           CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
 
            iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

            bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd)
 
         ENDDO
     
         ! on les passe au g_system

         call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

       endif

       ! on resout
       CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%V,info)

       IF (info > 0) THEN
         PRINT*,ibdyty
         CALL FATERR(IAM,'No solution')
       ENDIF
     else
       bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree
     endif 

     !fd 0_o calcul de X(TPS+H)

     bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + H*bdyty(ibdyty)%V 

     ! on corrige V pour que ce soit V(tps)

     bdyty(ibdyty)%V =  0.5d0 * c1_beta2 * ( c4_beta2 * bdyty(ibdyty)%X  &
                                           + (c2_beta2 - c4_beta2) * bdyty(ibdyty)%Xbegin &
                                           - c2_beta2 * bdyty(ibdyty)%Xprev &
                                           + c3_beta2 * bdyty(ibdyty)%Vbegin )

     !write(*,'(2(1x,D12.5))') bdyty(ibdyty)%V
     !print*,'---'

   case( INTEGRATOR_MOREAU ) 

     UMTTH = (1.d0 - theta)*H
     TTH   = theta*H

     RTTH = Rtheta*H
     UMRTTH = (1.d0 - Rtheta)*H
   
     ! Rigid part computation
     ! for corotationel formulation as well

     IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

        bdyty(ibdyty)%RV = bdyty(ibdyty)%RVfree + bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIreac


        ! actualisation du repere courant

        SELECT CASE (nbdime) 
        CASE(2)
          ! actualisation de la translation X qui contenait le mi-pas
          ! rq: on ecrit comme ca pour pouvoir faire du point fixe
          bdyty(ibdyty)%RX = bdyty(ibdyty)%RXbegin + &
                             UMRTTH*bdyty(ibdyty)%RVbegin + & 
                             RTTH*bdyty(ibdyty)%RV
           
          CALL update_inertia_frame22(0,H,bdyty(ibdyty)%RV(3),bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrame)
        CASE(3)
          ! actualisation de la translation X qui contenait le mi-pas
          ! rq: on ecrit comme ca pour pouvoir faire du point fixe
          bdyty(ibdyty)%RX(1:nbdime) = bdyty(ibdyty)%RXbegin(1:nbdime) + &
                                       UMRTTH*bdyty(ibdyty)%RVbegin(1:nbdime) + & 
                                       RTTH*bdyty(ibdyty)%RV(1:nbdime)
          
          spin(1:3) = bdyty(ibdyty)%RV(4:6)
          CALL update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrame)
        END SELECT

        !write(*,'(A,6(1x,D12.5))') 'RReac', bdyty(ibdyty)%RReac(:)/H
        !write(*,'(A,6(1x,D12.5))') 'RV   ', bdyty(ibdyty)%RV(:)
        !write(*,'(A,6(1x,D12.5))') 'RX   ', bdyty(ibdyty)%RX(1:nbdime)

     END IF

     bdyty(ibdyty)%Vlast = bdyty(ibdyty)%V  

     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Reac(:)/H      

     IF (.not. bdyty(ibdyty)%is_rigid .or. .NOT. bdyty(ibdyty)%skip_defo_comp) THEN

        if ( oldies ) then

          bdyty(ibdyty)%Vaux =0.d0

          if ( bdyty(ibdyty)%is_reac_modified) then

            call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Ireac)
   
            if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
               bdyty(ibdyty)%drvvalues = 0.d0
  
               ! on les passe au g_system
               call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           
  
            endif

            ! on resout
            CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

            IF (info > 0) THEN
               PRINT*,ibdyty
               CALL FATERR(IAM,'No solution')
            ENDIF

          endif

          bdyty(ibdyty)%V=bdyty(ibdyty)%Vfree + bdyty(ibdyty)%Vaux
          
          CALL apply_vlocy_driven_dof(ibdyty,iV____)

        else 

          bdyty(ibdyty)%Vaux =0.d0

          if ( bdyty(ibdyty)%is_reac_modified) then

            bdyty(ibdyty)%Iaux = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Ireac

            ! le second membre de g_sys est Iaux
            call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)
            
            if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
               ! on collecte les valeurs des ddl imposes

              ivd_=0 
              DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

                if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle  

                ivd_=ivd_+1
                
                CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
   
                iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

                bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd) - bdyty(ibdyty)%V(iccdof)
 
              ENDDO
     
              ! on les passe au g_system

              call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

            endif

            ! on resout
            CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)
  
            IF (info > 0) THEN
              PRINT*,ibdyty
              CALL FATERR(IAM,'No solution')
            ENDIF

          endif

          bdyty(ibdyty)%V=  bdyty(ibdyty)%V + bdyty(ibdyty)%Vaux

        endif 

     ELSE
     
        bdyty(ibdyty)%V=0.d0

     ENDIF


     if (.false.) then
       if ( nbdime == 2 ) then
          print*,'REAC' 
          write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Ireac
          print*,'RHS' 
          write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
          print*,'V' 
          write(*,'(2(1x,D12.5))') bdyty(ibdyty)%V
       else if (nbdime ==3) then
         print*,'REAC'   
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Ireac
         print*,'RHS' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
!        print*,'Iaux' 
!        write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Iaux
         print*,'Vaux' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux
         print*,'Vfree' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree
         print*,'V' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%V
       endif
     endif
    
     !fd le deplacement resulte de l'integration de la vitesse sans mouvement de corps rigide

     if (bdyty(ibdyty)%is_coro) then

       !fd paranoiac check
       v_loc(1:bdyty(ibdyty)%nbdofR) = matmul(bdyty(ibdyty)%D2R,bdyty(ibdyty)%V) 
       norm = sqrt(dot_product(v_loc,v_loc))
       if ( norm > tol_coro ) then
         write(cout,'(A,I0,1x,D12.5)') '@WARNING || L VD ||',ibdyty, norm
         call logmes(cout,.true.)
       !else 
       !  write(cout,'(A,I0,1x,D12.5)') '|| L VD ||',ibdyty, norm
       !  call logmes(cout)
       endif
       !allocate(vaux_glob(bdyty(ibdyty)%nbdof),vR_glob(bdyty(ibdyty)%nbdof))
       !vaux_glob=UMTTH*bdyty(ibdyty)%Vbegin + TTH*bdyty(ibdyty)%V
       !v_loc(1:bdyty(ibdyty)%nbdofR) = matmul(bdyty(ibdyty)%D2R,vaux_glob) 
       !vR_glob= matmul(bdyty(ibdyty)%R2D,v_loc(1:bdyty(ibdyty)%nbdofR))
       !write(*,'(A,I0,1x,D12.5)') 'GLV____',ibdyty,max(maxval(vR_glob),dabs(minval(vR_glob)))

       !if (maxval(vR_glob) > tol_coro .or. dabs(minval(vR_glob)) > tol_coro) then
       !  print*,'vR_glob: ',ibdyty
       !  if (nbDIME==2) then
       !    write(*,'(3(1x,D12.5))') vR_glob
       !  else if (nbDIME==3) then
       !    write(*,'(3(1x,D12.5))') vR_glob
       !  endif
       !  call FATERR(IAM,'rigid body velocity too high for coro scheme')
       !endif

       !fd on ne corrige pas volontairement 

       !bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + vaux_glob 
    
       !deallocate(vaux_glob,vR_glob)

       bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + &
                         UMTTH*bdyty(ibdyty)%Vbegin + &
                         TTH*bdyty(ibdyty)%V 

     else
  
       bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + &
                         UMTTH*bdyty(ibdyty)%Vbegin + &
                         TTH*bdyty(ibdyty)%V 
     endif

     WHERE(dabs(bdyty(ibdyty)%X)<1.d-24) bdyty(ibdyty)%X=0.d0

     !print*,'X'
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%X


   case( INTEGRATOR_QS ) 

     bdyty(ibdyty)%Vlast = bdyty(ibdyty)%V  

     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Reac(:)/H      

        if ( oldies ) then

          bdyty(ibdyty)%Vaux =0.d0

          if ( bdyty(ibdyty)%is_reac_modified) then

            call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Ireac/H)
   
            if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
               bdyty(ibdyty)%drvvalues = 0.d0
  
               ! on les passe au g_system
               call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           
  
            endif

            ! on resout
            CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

            IF (info > 0) THEN
               PRINT*,ibdyty
               CALL FATERR(IAM,'No solution')
            ENDIF


          endif

          bdyty(ibdyty)%V=bdyty(ibdyty)%Vfree + bdyty(ibdyty)%Vaux
          
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
        else 

          bdyty(ibdyty)%Vaux =0.d0

          if ( bdyty(ibdyty)%is_reac_modified) then

            bdyty(ibdyty)%Iaux = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Ireac/H

            ! le second membre de g_sys est Iaux
            call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)

  
            if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
               ! on collecte les valeurs des ddl imposes

              ivd_=0 
              DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

                if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle  

                ivd_=ivd_+1
                
                CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
   
                iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

                bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd) - bdyty(ibdyty)%V(iccdof)
 
              ENDDO
     
              ! on les passe au g_system

              call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

            endif

            ! on resout
            CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)
  
            IF (info > 0) THEN
              PRINT*,ibdyty
              CALL FATERR(IAM,'No solution')
            ENDIF

          endif

          bdyty(ibdyty)%V=  bdyty(ibdyty)%V + bdyty(ibdyty)%Vaux
          
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
        endif 

     if (.false.) then
       if ( nbdime == 2 ) then
          print*,'REAC' 
          write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Ireac
          print*,'RHS' 
          write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
          print*,'V' 
          write(*,'(2(1x,D12.5))') bdyty(ibdyty)%V
       else if (nbdime ==3) then
         print*,'REAC'   
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Ireac
         print*,'RHS' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
!        print*,'Iaux' 
!        write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Iaux
         print*,'Vaux' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux
         print*,'Vfree' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vfree
         print*,'V' 
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%V
       endif
     endif
    
  
     bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + bdyty(ibdyty)%V 

     WHERE(dabs(bdyty(ibdyty)%X)<1.d-24) bdyty(ibdyty)%X=0.d0

     !print*,'X'
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%X
     
   case default

     call faterr(IAM,'Unknown integrator')

   end select

 end subroutine compute_dof_mecaMAILx

!------------------------------------------------------------------------  
!------------------------------------------------------------------------  

 subroutine externalFEM_compute_dof_mecaMAILx
   implicit none 
   integer(kind=4) :: ibdyty
   real(kind=8) :: UMTTH, TTH
   character(len=34) :: IAM
   !      1234567890123456789012345678901234
   IAM = 'mecaMAILx::externalFEM_compute_dof'

   if( nb_mecaMAILx == 0 ) return
   
   CALL externalFEM_compute_V()

   select case( M_INTEGRATOR_ID )
   case( INTEGRATOR_MOREAU )

     UMTTH = (1.d0 - theta)*H
     TTH   = theta*H

     !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ibdyty)
     !$OMP DO SCHEDULE(RUNTIME)
     do ibdyty = 1, nb_mecaMAILx

       if( .not. bdyty(ibdyty)%visible ) cycle

       bdyty(ibdyty)%Vlast = bdyty(ibdyty)%V  
       CALL externalFEM_get_V(ibdyty,bdyty(ibdyty)%V)
       bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin + & 
                        UMTTH*bdyty(ibdyty)%Vbegin + &
                        TTH*bdyty(ibdyty)%V
     end do
     !$OMP END DO
     !$OMP END PARALLEL

   case( INTEGRATOR_BETA2 )
     !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ibdyty)
     !$OMP DO SCHEDULE(RUNTIME)
     do ibdyty = 1, nb_mecaMAILx

       if( .not. bdyty(ibdyty)%visible ) cycle

       bdyty(ibdyty)%Vlast = bdyty(ibdyty)%V
       call externalFEM_get_V(ibdyty,bdyty(ibdyty)%V)
       bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + H*bdyty(ibdyty)%V

     end do

     !$OMP END DO
     !$OMP END PARALLEL

   case default

     call faterr(IAM,'Unknown integrator')

   end select
   call MPI_BARRIER( lmgc_mpi_world_comm, code_MPI)

 end subroutine

!!!------------------------------------------------------------------------    
!!!------------------------------------------------------------------------    

  !> updates coordinates and switches nodal fields for the next step X->Xbeg ...

  subroutine update_dof_mecaMAILx(ibdyty)
    implicit none 
    integer(kind=4), intent(in) :: ibdyty
    !
    integer(kind=4) :: inodty,iccdof
    integer(kind=4) :: iM_bdyty,iM_nodty,iM_ccdof

    !fd tableaux aux pour rigid 
    REAL(kind=8),DIMENSION(nbdime):: coor_aux
    character(len=80) :: cout
    character(len=21) :: IAM

    !      123456789012345678901
    IAM = 'mecaMAILx::update_dof'

    if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(ibdyty)%visible ) return

    select case( M_INTEGRATOR_ID )
    case( INTEGRATOR_BETA2 )

      ! calcul partie defo 

      ! actualisation des cordonnees du maillage (en repere globale)
 
      iM_bdyty=bdyty2M_bdyty(ibdyty)

      DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
         iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
 
         !calcul des nouvelles coordonnees pour un corps maille classique
         M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = &
           M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) + bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbDIME)                

      END DO

      ! maj pour le pas suivant

      bdyty(ibdyty)%Vbegin = bdyty(ibdyty)%V

      bdyty(ibdyty)%Xprev=bdyty(ibdyty)%Xbegin 

      bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X


    case( INTEGRATOR_MOREAU ) 

      ! gestion partie rigide et coro 

      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN    
         bdyty(ibdyty)%RVbegin        = bdyty(ibdyty)%RV
         bdyty(ibdyty)%RXbegin        = bdyty(ibdyty)%RX

         bdyty(ibdyty)%LocalFrameIni  = bdyty(ibdyty)%LocalFrame
      ENDIF

      ! calcul partie defo 
      bdyty(ibdyty)%Vbegin=bdyty(ibdyty)%V                
      bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X 
      
      ! actualisation des cordonnees du maillage (en repere globale)
 
      iM_bdyty=bdyty2M_bdyty(ibdyty) 

      DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
        iccdof=bdyty(ibdyty)%ccdof(inodty)
        iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
        iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
 
        ! on calcule dans le repere principal d'inertie
        ! on tourne dans le global 

        !fd a confirmer la partie XR ?

        !calcul des nouvelles coordonnees dans le repere global
        IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
          ! actualisation des coordonnees du maillage dans le localframe
          coor_aux(1:nbdime) = bdyty(ibdyty)%cooref_local(1:nbdime,inodty) + &
                               bdyty(ibdyty)%X(iccdof+1:iccdof+nbdime)

           ! actualisation des cooordonnees dans le repere global
           M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = bdyty(ibdyty)%cooref_G(1:nbdime) + &
                                                                bdyty(ibdyty)%RX(1:nbdime) + &
                                                                MATMUL(bdyty(ibdyty)%localframe,coor_aux)

           !on a fini de traiter ce corps et on passe directement au suivant
           CYCLE
           
        ENDIF

        !calcul des nouvelles coordonnees pour un corps maille classique
        M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = &
          M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) + bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME)                

      END DO
     
    case( INTEGRATOR_QS ) 

      ! calcul partie defo 
      bdyty(ibdyty)%Vbegin=bdyty(ibdyty)%V                
      bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X 
      
      ! actualisation des cordonnees du maillage (en repere globale)
 
      iM_bdyty=bdyty2M_bdyty(ibdyty) 

      DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
        iccdof=bdyty(ibdyty)%ccdof(inodty)
        iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
        iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)

        !calcul des nouvelles coordonnees pour un corps maille classique
        M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = &
          M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) + bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME)                

      END DO
     
    case default

      call faterr(IAM,'Unknown integrator')

    end select

  end subroutine update_dof_mecaMAILx

!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 

  subroutine compute_mass_mecaMAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    !
    INTEGER :: errare
    INTEGER :: iblmty
    INTEGER :: idof
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele

                             !12345678901234567890123
    CHARACTER(len=23) :: IAM='mecaMAILx::compute_mass'
    character(len=80) :: cout

    !!!
    !!! on calcule ici la masse sur la configuration de reference 
    !!! elle peut etre lumpee
    !!!

    if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(ibdyty)%visible ) return

    select case( M_INTEGRATOR_ID )
    case( INTEGRATOR_BETA2 )
      !
      !write(*,'(A,I0)') 'Body: ',ibdyty
      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        !
        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          !print*,'skip ',iblmty,' from ',IAM
          bdyty(ibdyty)%blmty(iblmty)%mass = 0.d0
          do idof=1,size(bdyty(ibdyty)%blmty(iblmty)%mass,dim=1)
            bdyty(ibdyty)%blmty(iblmty)%mass(idof,idof) = 1.d-14
          enddo
          cycle
        endif
        !
        ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

        IF (errare /= 0) THEN
          CALL LOGMES('Error '//IAM//': allocating coor_ele')
        END IF

        !! beurk il y a des trucs pas secure entre nodty et DIME !!

        coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

        !
        ! on calcule les matrices elementaires ici
        ! a revoir plus tard ... 
        ! 

        CALL compute_elementary_mass(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     ibdyty,iblmty,                     &
                                     coor_ele,bdyty(ibdyty)%blmty(iblmty)%mass)       


        DEALLOCATE(coor_ele)
      END DO

    case( INTEGRATOR_MOREAU ) 

      !
      !write(*,'(A,I0)') 'Body: ',ibdyty
      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        !
        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          !print*,'skip ',iblmty,' from ',IAM
          bdyty(ibdyty)%blmty(iblmty)%mass = 0.d0
          do idof=1,size(bdyty(ibdyty)%blmty(iblmty)%mass,dim=1)
            bdyty(ibdyty)%blmty(iblmty)%mass(idof,idof) = 1.d-14
          enddo
          cycle
        endif
        !
        ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

        IF (errare /= 0) THEN
          CALL LOGMES('Error '//IAM//': allocating coor_ele')
        END IF

        !! beurk il y a des trucs pas secure entre nodty et DIME !!

        ! on utilise les coordonnees des noeuds dans le repere
        ! principal d'inertie pour calculer les matrices de masses elementaires
        IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
          coor_ele=get_cooref_local_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
        ELSE 
          coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
        ENDIF
        !
        ! on calcule les matrices elementaires ici
        ! a revoir plus tard ... 
        ! 
        !PRINT*,ibdyty,iblmty
        !PRINT*,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
        !PRINT*,bdyty(ibdyty)%blmty(iblmty)%NODES
        !PRINT*,coor_ele(1,:)
        !PRINT*,coor_ele(2,:)

        CALL compute_elementary_mass(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     ibdyty,iblmty                    ,&
                                     coor_ele,bdyty(ibdyty)%blmty(iblmty)%mass)


        !write(*,'(A,I0)') 'Element: ',iblmty
        !write(*,'(A)') 'masse:'
        !write(*,'(24(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%mass

        !fd print*,'masse:'
        !fd print '(12(1x,D14.7))',bdyty(ibdyty)%blmty(iblmty)%mass

        
       
        DEALLOCATE(coor_ele)
      END DO

    case( INTEGRATOR_QS ) 

      !
      !write(*,'(A,I0)') 'Body: ',ibdyty
      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        !
        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          !print*,'skip ',iblmty,' from ',IAM
          bdyty(ibdyty)%blmty(iblmty)%mass = 0.d0
          ! do idof=1,size(bdyty(ibdyty)%blmty(iblmty)%mass,dim=1)
          !   bdyty(ibdyty)%blmty(iblmty)%mass(idof,idof) = 1.d-14
          ! enddo
          cycle
        endif
        !
        ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

        IF (errare /= 0) THEN
          CALL LOGMES('Error '//IAM//': allocating coor_ele')
        END IF

        coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

        !
        ! on calcule les matrices elementaires ici
        ! a revoir plus tard ... 
        ! 

        CALL compute_elementary_mass(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     ibdyty,iblmty                    ,&
                                     coor_ele,bdyty(ibdyty)%blmty(iblmty)%mass)


        !write(*,'(A,I0)') 'Element: ',iblmty
        !write(*,'(A)') 'masse:'
        !write(*,'(24(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%mass

        !fd print*,'masse:'
        !fd print '(12(1x,D14.7))',bdyty(ibdyty)%blmty(iblmty)%mass

        
       
        DEALLOCATE(coor_ele)
      END DO

    case default

      call faterr(IAM,'Unknown integrator')

    end select


  end subroutine compute_mass_mecaMAILx

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 

  subroutine compute_Fext_mecaMAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    !
    integer(kind=4) :: iblmty,inodty
    integer(kind=4) :: idof,ifd,inod,iccdof
    real(kind=8)    :: Febegin,Fe

    integer(kind=4) :: i,nbdof,errare
    REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele

!                           12345678901234567890123
    CHARACTER(len=23) :: IAM='mecaMAILx::compute_Fext'
    character(len=80) :: cout

    !
    ! on calcule ici les forces exterieures nodales:
    !   la contribution de la gravite M*grav 
    !   la contribution des forces imposees dans DRV_DOF
    !

    if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(ibdyty)%visible ) return

    !write(*,'(A,I0)') 'Body: ',ibdyty

    bdyty(ibdyty)%Fext=0.d0

    select case (M_INTEGRATOR_ID)
    case( INTEGRATOR_BETA2 )

      IF ((nbDIME == 2 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0)) .OR. &
          (nbDIME == 3 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0 .OR. grav3 /= 0.d0))) THEN 

        DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

          if (bdyty(ibdyty)%eviz(iblmty) == 0) cycle

          !write(*,'(A,I0)') 'Element: ',iblmty
          nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

          ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

          DV_ele=0.d0

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
            IF (nbDIME == 2) THEN
              DV_ele((i-1)*nbdof+1) = grav1
              DV_ele((i-1)*nbdof+2) = grav2
            ELSE IF (nbDIME == 3) THEN
              DV_ele((i-1)*nbdof+1) = grav1
              DV_ele((i-1)*nbdof+2) = grav2
              DV_ele((i-1)*nbdof+3) = grav3
            ELSE
              call faterr(IAM,'unknown value of nbDIME')
            ENDIF
          END DO

          DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

            inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            ! 

            bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) + &
                                                        (DV_ele((i-1)*nbdof+1:i*nbdof))
            !write(*,*) '     fe : ', bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof)
          END DO

          DEALLOCATE(DV_ele)

        ENDDO
      ENDIF

      DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof

        CALL owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inod,idof)
        iccdof=bdyty(ibdyty)%ccdof(inod)+idof   

        CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)

        bdyty(ibdyty)%FdrivBeg(ifd)= Febegin
        bdyty(ibdyty)%Fdriv(ifd)   = Fe     
 
        bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof) + Fe

      END DO

      !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fext

    case( INTEGRATOR_MOREAU )
      
      ! initialisation a 0 des forces exterieures pour le rigide equivalent
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) then
        bdyty(ibdyty)%RFext=0.d0 
        bdyty(ibdyty)%RFint=0.d0 
      endif
      !
      ! gravity are computed locally ...

      IF ((nbDIME == 2 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0)) .OR. &
          (nbDIME == 3 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0 .OR. grav3 /= 0.d0))) THEN 

        ! on appplique la gravite uniquement au rigide equivalent, le cas echeant
        IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

          bdyty(ibdyty)%RFext(1) = bdyty(ibdyty)%mR(1)*grav1
          bdyty(ibdyty)%RFext(2) = bdyty(ibdyty)%mR(2)*grav2
          IF (nbdime == 3)  bdyty(ibdyty)%RFext(3) = bdyty(ibdyty)%mR(3)*grav3

        ENDIF

        if( bdyty(ibdyty)%is_rigid ) return

        DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

          if (bdyty(ibdyty)%eviz(iblmty) == 0) cycle

          !write(*,'(A,I0)') 'Element: ',iblmty
          nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

          ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

          DV_ele=0.d0

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
            IF (nbDIME == 2) THEN
              DV_ele((i-1)*nbdof+1) = grav1
              DV_ele((i-1)*nbdof+2) = grav2
            ELSE IF (nbDIME == 3) THEN
              DV_ele((i-1)*nbdof+1) = grav1
              DV_ele((i-1)*nbdof+2) = grav2
              DV_ele((i-1)*nbdof+3) = grav3
            ELSE
              call faterr(IAM,'unknown value of nbDIME')
            ENDIF
          END DO

          DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

            inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            ! 

            bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) + &
                                                        (DV_ele((i-1)*nbdof+1:i*nbdof))
            !write(*,*) '     fe : ', bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof)
          END DO

          DEALLOCATE(DV_ele)

        ENDDO
      ENDIF

      !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fext

      ! driven forces

      DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof

        CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)

        bdyty(ibdyty)%FdrivBeg(ifd)= Febegin
        bdyty(ibdyty)%Fdriv(ifd)   = Fe     
 
        CALL owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inod,idof)

        iccdof=bdyty(ibdyty)%ccdof(inod)+idof   
        bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+((1.D0-THETA)*Febegin)+(THETA*Fe)

      END DO


      !--- fd fext 
      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        CALL assemble_elementary_vector(bdyty(ibdyty)%Fext,(1.d0-THETA)*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) + &
                                                                 THETA *bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) , &
                                                          bdyty(ibdyty)%blmty(iblmty)%edof2gdof)    
      ENDDO    
      
      !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fext

    case( INTEGRATOR_QS )
      
      !
      ! gravity are computed locally ...

      IF ((nbDIME == 2 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0)) .OR. &
          (nbDIME == 3 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0 .OR. grav3 /= 0.d0))) THEN 

        DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

          if (bdyty(ibdyty)%eviz(iblmty) == 0) cycle

          !write(*,'(A,I0)') 'Element: ',iblmty
          nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

          ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

          DV_ele=0.d0

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
            IF (nbDIME == 2) THEN
              DV_ele((i-1)*nbdof+1) = grav1
              DV_ele((i-1)*nbdof+2) = grav2
            ELSE IF (nbDIME == 3) THEN
              DV_ele((i-1)*nbdof+1) = grav1
              DV_ele((i-1)*nbdof+2) = grav2
              DV_ele((i-1)*nbdof+3) = grav3
            ELSE
              call faterr(IAM,'unknown value of nbDIME')
            ENDIF
          END DO

          DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

            inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            ! 

            bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) + &
                                                        (DV_ele((i-1)*nbdof+1:i*nbdof))
            !write(*,*) '     fe : ', bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof)
          END DO

          DEALLOCATE(DV_ele)

        ENDDO
      ENDIF

      !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fext

      ! driven forces

      DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof

        CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)

        bdyty(ibdyty)%FdrivBeg(ifd)= Febegin
        bdyty(ibdyty)%Fdriv(ifd)   = Fe     
 
        CALL owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inod,idof)

        iccdof=bdyty(ibdyty)%ccdof(inod)+idof   
        bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+ Fe

      END DO


      !--- fd fext 
      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        CALL assemble_elementary_vector(bdyty(ibdyty)%Fext,bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) , &
                                                          bdyty(ibdyty)%blmty(iblmty)%edof2gdof)    
      ENDDO    
      
      !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fext

      
    case default  
   
      call faterr(IAM,'Unknown integrator')

    end select    

  end subroutine compute_Fext_mecaMAILx

!!!------------------------------------------------------------------------  
!!!------------------------------------------------------------------------  

subroutine compute_bulk_mecaMAILx(ibdyty,istate)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  ! =0 calcul du second membre stiffness & ttFint, 
  !    les contraintes et defo pas gardees si on calcule au point milieu
  ! =1 calcul contraintes et deformations + stockage
  ! =2 calcul second membre 
  integer(kind=4), intent(in) :: istate 
  ! ***
  INTEGER :: errare
  INTEGER :: iblmty
  INTEGER :: iM_bdyty,iM_blmty
  INTEGER :: idof,nbdof
  INTEGER :: i,inodty,iccdof,imodel
  
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: dep_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint

  logical :: compute_fint,compute_stiffness,push_fields

  
  !                         12345678901234567890123
  CHARACTER(len=23) :: IAM='mecaMAILx::compute_bulk'
  CHARACTER(len=80) :: cout
  
  if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
    write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
    call faterr(IAM,cout)
  end if

  if ( .not. bdyty(ibdyty)%visible ) return
  
  select case (M_INTEGRATOR_ID)
  case( INTEGRATOR_BETA2 )

    !fd 0_o avec beta2 on connait le deplacement
    !fd donc on calcule et garde les defo et contraintes au premier passage
    if (istate == 0) return

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
     
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'Error allocating coor_ele')
      ENDIF
     
      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
     
      nbdof = bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
     
      ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)), &
               stat=errare)
     
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'Error allocating dep_ele')
      ENDIF
     
      !
      ! on calcule les matrices elementaires ici
      ! a revoir plus tard ... 
      ! 
     
      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
     
      IF (istate==2) THEN
        
        !if (bdyty(ibdyty)%eviz(iblmty) == 0) then
        !  !print*,'skip ',iblmty,' from ',IAM
        !  bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0
        !  bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0
        !  deallocate(coor_ele,dep_ele)
        ! cycle
        !endif

        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          dep_ele=0.
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
           
            ! passage au numero global
            inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof = bdyty(ibdyty)%ccdof(inodty) 

            !fd 0_o attention ici Xbegin = X(tps)
            dep_ele(1:nbdof,i) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
          END DO
        endif

        ! calcul de K et ttFint 
        
        compute_fint=.True.
        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_stiffness=.False.
        bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0

        !fd 0_o dans la mesure ou X(tps) est connu on fait tout d'un coup ; push_fields=.False.
        push_fields=.True.

        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,H,                                      &  
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)       

      ELSE IF (istate == 1) THEN

        ! il faudrait ne rien faire car ici la contrainte calculee avant est la bonne        
        ! a voir ....

        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          !print*,'skip ',iblmty,' from ',IAM
          dep_ele = 0.d0 
        else

           DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             
               ! passage au numero global
               inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
               ! position un dans le vecteur global pour le numero inodty      
               iccdof=bdyty(ibdyty)%ccdof(inodty) 

               !fd 0_o attention ici Xbegin = X(tps)
               dep_ele(1:nbdof,i)= bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
           
          END DO
        endif
        

        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.False.
        compute_stiffness=.False.
        push_fields=.True.


        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,H               ,                       &  
             coor_ele,dep_ele                  ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)       

      ELSE
        write(cout,'(A,1x,I0)') 'unsupported istate:', istate
        call faterr(IAM,cout)
      ENDIF
     
      DEALLOCATE(coor_ele,dep_ele)

    ENDDO

  case( INTEGRATOR_MOREAU )

    ! si on ne cherche pas a calculer la deformation, on zappe
    if( bdyty(ibdyty)%is_rigid .and. bdyty(ibdyty)%skip_defo_comp ) return
  
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
     
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'Error allocating coor_ele')
      ENDIF
     
      ! on utilise les coordonnees des noeuds dans le repere principal
      ! d'inertie pour calculer les matrices de rigidite elementaires
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
        coor_ele = get_cooref_local_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      ELSE 
        coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      ENDIF
     
      nbdof = bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
     
      ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)), &
               stat=errare)
     
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'Error allocating dep_ele')
      ENDIF
     
      !
      ! on calcule les matrices elementaires ici
      ! a revoir plus tard ... 
      ! 
     
      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
     
      IF (istate==0) THEN
        
        !if (bdyty(ibdyty)%eviz(iblmty) == 0) then
        !  !print*,'skip ',iblmty,' from ',IAM
        !  bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0
        !  bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0
        !  deallocate(coor_ele,dep_ele)
        ! cycle
        !endif

        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          dep_ele=0.d0
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
           
            ! passage au numero global
            inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof = bdyty(ibdyty)%ccdof(inodty) 
            dep_ele(1:nbdof,i)= theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) + &
                                (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
          END DO
        endif

        ! calcul de K et ttFint 
        ! ne conserve pas les deformations et les contraintes
        
        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.True.
        compute_stiffness=.True.
        push_fields=.False.

        !print*,'dep ele'
        !if (nbdime == 2) then
        !  write(*,'(2(1x,D12.5))') dep_ele
        !else if (nbdime == 3) then
        !  write(*,'(3(1x,D12.5))') dep_ele
        !endif
       
        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,theta*H         ,                       &  
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)       

        
        if (bdyty(ibdyty)%eviz(iblmty) == 0) bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0


        !write(*,'(2(1x,D12.5))') Fint

        
      ELSE IF (istate == 1) THEN
        
        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          !print*,'skip ',iblmty,' from ',IAM
          dep_ele = 0.d0 
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             
             ! passage au numero global
             inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
             ! position un dans le vecteur global pour le numero inodty      
             iccdof=bdyty(ibdyty)%ccdof(inodty) 
             dep_ele(1:nbdof,i)= bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
           
          END DO
        endif

        !print*,'dep ele'
        !if (nbdime == 2) then
        !  write(*,'(2(1x,D12.5))') dep_ele
        !else if (nbdime == 3) then
        !  write(*,'(3(1x,D12.5))') dep_ele
        !endif
        
        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.False.
        compute_stiffness=.False.
        push_fields=.True.

        
        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,H               ,                       &  
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)       

      ELSE IF (istate==2) THEN

        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          dep_ele=0.d0
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

            ! passage au numero global
            inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i)
            ! position un dans le vecteur global pour le numero inodty
            iccdof = bdyty(ibdyty)%ccdof(inodty)
            dep_ele(1:nbdof,i)= theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) + &
                                (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
          END DO
        endif

        ! calcul de ttFint
        ! ne conserve pas les deformations et les contraintes

        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.True.
        compute_stiffness=.False.
        push_fields=.False.

        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,theta*H         ,                       &
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)

      ELSE
        write(cout,'(A,1x,I0)') 'unsupported istate:', istate
        call faterr(IAM,cout)
      ENDIF
     
      DEALLOCATE(coor_ele,dep_ele)

    ENDDO

  case( INTEGRATOR_QS )

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
     
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'Error allocating coor_ele')
      ENDIF
     
      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
     
      nbdof = bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
     
      ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)), &
               stat=errare)
     
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'Error allocating dep_ele')
      ENDIF
     
      !
      ! on calcule les matrices elementaires ici
      ! a revoir plus tard ... 
      ! 
     
      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
     
      IF (istate==0) THEN
        
        !if (bdyty(ibdyty)%eviz(iblmty) == 0) then
        !  !print*,'skip ',iblmty,' from ',IAM
        !  bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0
        !  bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0
        !  deallocate(coor_ele,dep_ele)
        ! cycle
        !endif

        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          dep_ele=0.d0
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
           
            ! passage au numero global
            inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof = bdyty(ibdyty)%ccdof(inodty) 
            dep_ele(1:nbdof,i)= bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
                                
          END DO
        endif

        ! calcul de K et ttFint 
        ! ne conserve pas les deformations et les contraintes
        
        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.True.
        compute_stiffness=.True.
        push_fields=.False.

        !print*,'dep ele'
        !if (nbdime == 2) then
        !  write(*,'(2(1x,D12.5))') dep_ele
        !else if (nbdime == 3) then
        !  write(*,'(3(1x,D12.5))') dep_ele
        !endif
       
        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,H               ,                       &  
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness,compute_stiffness,  &
             push_fields)       
        
        if (bdyty(ibdyty)%eviz(iblmty) == 0) bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0


        !write(*,'(2(1x,D12.5))') Fint

        
      ELSE IF (istate == 1) THEN
        
        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          !print*,'skip ',iblmty,' from ',IAM
          dep_ele = 0.d0 
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             
             ! passage au numero global
             inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
             ! position un dans le vecteur global pour le numero inodty      
             iccdof=bdyty(ibdyty)%ccdof(inodty) 
             dep_ele(1:nbdof,i)= bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
           
          END DO
        endif

        !print*,'dep ele'
        !if (nbdime == 2) then
        !  write(*,'(2(1x,D12.5))') dep_ele
        !else if (nbdime == 3) then
        !  write(*,'(3(1x,D12.5))') dep_ele
        !endif
        
        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.False.
        compute_stiffness=.False.
        push_fields=.True.

        
        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,H               ,                       &  
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)       

      ELSE IF (istate==2) THEN

        if (bdyty(ibdyty)%eviz(iblmty) == 0) then
          dep_ele=0.d0
        else

          DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

            ! passage au numero global
            inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i)
            ! position un dans le vecteur global pour le numero inodty
            iccdof = bdyty(ibdyty)%ccdof(inodty)
            dep_ele(1:nbdof,i)= bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
          END DO
        endif

        ! calcul de ttFint
        ! ne conserve pas les deformations et les contraintes

        bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0

        compute_fint=.True.
        compute_stiffness=.False.
        push_fields=.False.

        CALL compute_elementary_bulk(                                  &
             bdyty(ibdyty)%blmty(iblmty)%blmnb ,                       &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb ,                       &
             iM_bdyty,iM_blmty,H               ,                       &
             coor_ele,dep_ele,                                         &
             bdyty(ibdyty)%blmty(iblmty)%ttFint,compute_fint,          &
             bdyty(ibdyty)%blmty(iblmty)%stiffness, compute_stiffness, &
             push_fields)
        
      ELSE
        write(cout,'(A,1x,I0)') 'unsupported istate:', istate
        call faterr(IAM,cout)
      ENDIF
     
      DEALLOCATE(coor_ele,dep_ele)

    ENDDO

  case default 

    call faterr(IAM,'Unknown integrator')

  end select

end subroutine compute_bulk_mecaMAILx

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------

subroutine externalFEM_compute_bulk_mecaMAILx()
  implicit none

  if( nb_mecaMAILx == 0 ) return

  call externalFEM_compute_bulk

end subroutine
  
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------

 subroutine assemb_KT_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   integer(kind=4)   :: iblmty,ivd,iccdof,idof,inod
   real(kind=8)      :: HT,HT2
   character(len=80) :: cout

                            !12345678901234567890
   character(len=20) :: IAM='mecaMAILx::assemb_KT'

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr('mecaMAILx::assemb_KT_mecaMAILx',cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

!   print*,'assemb_KT_mecaMAILx'

   !write(*,'(A,I0)') 'Body: ',ibdyty

   ! si on cherche pas a calculer le deformation, on zappe
   if( bdyty(ibdyty)%is_rigid .and. bdyty(ibdyty)%skip_defo_comp ) return

   select case (M_INTEGRATOR_ID)
   case( INTEGRATOR_BETA2 )

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
       call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%mass,c1_beta2)
     ENDDO

   case( INTEGRATOR_MOREAU ) 

     HT=THETA*H
     HT2=HT*HT

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
       ! print*,'-- ele mat'
       ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata
       call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%mass,mass_scaling)
       ! print*,'-- ele mat'
       ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata
       call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%damping,HT)
       ! print*,'-- ele mat'
       !  write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata
       call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%stiffness,HT2)
       ! print*,'-- ele mat'
       ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata

!       if (iblmty == 1) then
        ! print*,HT,HT2
        ! print*,mass_scaling
        ! print*,'-- mass'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%blmty(iblmty)%mass
        ! print*,'-- ele mat'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata
        ! print*,'-- damping'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%blmty(iblmty)%damping
        ! print*,'-- stiffness'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%blmty(iblmty)%stiffness
!       endif

     enddo
     
   case( INTEGRATOR_QS ) 

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
       ! print*,'-- ele mat'
       !  write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata
       call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%stiffness,1.d0)
       ! print*,'-- ele mat'
       ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata

!       if (iblmty == 1) then
        ! print*,HT,HT2
        ! print*,mass_scaling
        ! print*,'-- mass'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%blmty(iblmty)%mass
        ! print*,'-- ele mat'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%g_sys%matrices_loc(iblmty)%rdata
        ! print*,'-- damping'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%blmty(iblmty)%damping
        ! print*,'-- stiffness'
        ! write(*,'(12(1x,D8.1))') bdyty(ibdyty)%blmty(iblmty)%stiffness
!       endif

     enddo
     
   case default

     call FATERR(IAM,'Unknown integrator') 

   end select  

 end subroutine assemb_KT_mecaMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 subroutine apply_drvdof_KT_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   integer(kind=4)   :: ivd,iccdof,idof,inod,ivd_
   character(len=80) :: cout

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr('mecaMAILx::apply_drvdof_KT_mecaMAILx',cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return
   !write(*,'(A,I0)') 'Body: ',ibdyty

   ! si on ne cherche pas a calculer le deformation, on zappe
   if( bdyty(ibdyty)%is_rigid .and. bdyty(ibdyty)%skip_defo_comp ) return

   if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then

     ivd_=0 
     DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

       if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle
        
       ivd_=ivd_+1
       
       CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

       bdyty(ibdyty)%drvdofs(ivd_)=bdyty(ibdyty)%ccdof(inod)+idof 

     enddo

     call set_drvdofs(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvdofs)

   else
  
     call erase_drvdofs(bdyty(ibdyty)%g_sys)

   endif

 end subroutine apply_drvdof_KT_mecaMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 subroutine assemb_RHS_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   !                         123456789012345678901
   character(len=21) :: IAM='mecaMAILx::assemb_RHS'
   character(len=80) :: cout
   integer(kind=4)   :: iblmty,i,inodty,iccdof,nbdof,errare

   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele, visco, M_factor
   REAL(kind=8) :: TT,UMTT,HTT,HUMTT,norm
   integer :: idofR,jdof

!   print*,'assemb_RHS_mecaMAILx'

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   !fd
   !fd d'abord les contributions elementaires
   !fd

   bdyty(ibdyty)%RHS=0.D0
   bdyty(ibdyty)%Fint=0.D0
   bdyty(ibdyty)%Finert=0.D0
   bdyty(ibdyty)%momentum=0.D0

   select case( M_INTEGRATOR_ID )
   case( INTEGRATOR_BETA2 )

      !print*,'X'
      !write(*,'(2(1x,E12.5))') bdyty(ibdyty)%X
      !print*,'Xbegin'
      !write(*,'(2(1x,E12.5))') bdyty(ibdyty)%Xbegin

      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

         nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

         ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

         IF (errare /= 0) THEN
           CALL FATERR(IAM, 'Error allocating dv_ele')
         ENDIF

         ALLOCATE(visco(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
         IF (errare /= 0) THEN
           CALL FATERR(IAM, 'Error allocating visco')
         ENDIF

         allocate(M_factor(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)))

         DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
            ! passage au numero global
            inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
            ! position un dans le vecteur global pour le numero inodty     
            iccdof=bdyty(ibdyty)%ccdof(inodty)
           
            !fd 0_o calcul de H^-*V(tps-0.5H)
            DV_ele((i-1)*nbdof+1:i*nbdof)= bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) - &
                                           bdyty(ibdyty)%Xprev(iccdof+1:iccdof+nbdof)

            M_factor((i-1)*nbdof+1:i*nbdof) = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)

         END DO

        !write(*,'(2(1x,E12.5))') DV_ele

        !fd 0_o ca doit etre C * V(tps-0.5H)

        visco = MATMUL(bdyty(ibdyty)%blmty(iblmty)%damping,DV_ele)/H

        !fd 0_o c1*M ( c2 ( qi-qi-1 ) + c3 Vi-1) <=> h-*c2 Vi-1/2 + c3 Vi-1

        DV_ele   = c2_beta2 * DV_ele + c3_beta2 * M_factor
        M_factor = c1_beta2 * MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

        !write(*,'(2(1x,E12.5))') visco


        bdyty(ibdyty)%blmty(iblmty)%RHSloc = M_factor + &
                                             ((  bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) &
                                               - visco                                 &
                                               - bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) &
                                               - bdyty(ibdyty)%blmty(iblmty)%ttFint(:))* H)

        CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

        CALL assemble_elementary_vector(bdyty(ibdyty)%Fint, bdyty(ibdyty)%blmty(iblmty)%ttFint(:), &
                                                            bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  
      
        
        DEALLOCATE(DV_ele,visco,M_factor)
      END DO

      bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(H*bdyty(ibdyty)%Fext)
      


   case( INTEGRATOR_MOREAU )

     ! si on ne cherche pas a calculer la deformation, on zappe
     if( bdyty(ibdyty)%is_rigid .and. bdyty(ibdyty)%skip_defo_comp ) return

     TT = THETA
     UMTT = (1.d0-TT)   
     HTT = H*TT
     HUMTT=H*(1.d0-TT)  

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node  

       ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM, 'Error allocating dv_ele')
       ENDIF

       ALLOCATE(visco(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM, 'Error allocating visco')
       ENDIF

       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         !fd c'est completement idiot car dans increment on fait V=Vbegin 
         !
         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) - &
                                       bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
         visco((i-1)*nbdof+1:i*nbdof)= TT*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) + &
                                       UMTT*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)

       END DO


       DV_ele = mass_scaling*MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)
       visco = MATMUL(bdyty(ibdyty)%blmty(iblmty)%damping,visco)

       bdyty(ibdyty)%blmty(iblmty)%RHSloc=  DV_ele  &
                                         + ( &
!fd fext
!                                            ( HUMTT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) + &
!                                                 HTT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) &
!                                             ) &
                                           -( HUMTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
                                                 HTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) &
                                            ) &
                                           -( H*bdyty(ibdyty)%blmty(iblmty)%ttFint(:) ) &
                                           -( H*visco ) &
                                           )
    !


       !  if (iblmty > 1) then
       !    write(*,'(A,I0)') 'Element: ',iblmty
       !    write(*,'(3(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%RHSloc
       !     print*,bdyty(ibdyty)%blmty(iblmty)%edof2gdof
       !  endif 

       CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

       CALL assemble_elementary_vector(bdyty(ibdyty)%Fint,-(  TT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) + &
                                                            UMTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
                                                                 bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + &
                                                                 visco                                    ) , &
                                       bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       CALL assemble_elementary_vector(bdyty(ibdyty)%Finert,(-DV_ele/(H*mass_scaling)),bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         !fd c'est completement idiot car dans increment on fait V=Vbegin 
         !
         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
       END DO

       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

       CALL assemble_elementary_vector(bdyty(ibdyty)%momentum,DV_ele,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       DEALLOCATE(DV_ele)
       DEALLOCATE(visco)

     ENDDO

     !
     !if (nbdime==2)then
     !  print*,'RHS'
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
     !  print*,'Fint'
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Fint
     !  print*,'Fext'
     !  write(*,'(2(1x,D12.5))') H*bdyty(ibdyty)%Fext
     !  print*,'Finert'
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Finert
     !else if (nbdime==3)then
     !  print*,'RHS'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     !  print*,'Fint'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fint
     !  print*,'Fext'
     !  write(*,'(3(1x,D12.5))') H*bdyty(ibdyty)%Fext
     !  print*,'Finert'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Finert
     !endif




     !fd
     !fd ensuite les contributions nodales 
     !fd

     bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(H*bdyty(ibdyty)%Fext)

     !print*,'RHS+Fext'
     !if (nbdime==2)then
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
     !else if (nbdime==3)then
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     !endif
     !print*,'========='

     IF (bdyty(ibdyty)%is_coro) then
       ! on calcule : RFint = (R2D)^T*Fint
       bdyty(ibdyty)%Rfint=0.d0
       do idofR=1,bdyty(ibdyty)%nbdofR
         do jdof=1,bdyty(ibdyty)%nbdof  
           bdyty(ibdyty)%Rfint(idofR) = bdyty(ibdyty)%Rfint(idofR) + &
                                        (bdyty(ibdyty)%R2D(jdof, idofR) * bdyty(ibdyty)%Fint(jdof))
         enddo
       enddo

       !norm = sqrt(dot_product(bdyty(ibdyty)%Rfint,bdyty(ibdyty)%Rfint))
       !write(cout,'(A,I0,1x,D12.5)') '|| G^T Fint ||',ibdyty, norm
       !call logmes(cout)

       !write(*,'(6(1x,D12.5))') bdyty(ibdyty)%RFint
       !print*,'---'
     endif

   case( INTEGRATOR_QS )

     bdyty(ibdyty)%Finert = 0.d0
     bdyty(ibdyty)%momentum = 0.d0
      
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node  

       bdyty(ibdyty)%blmty(iblmty)%RHSloc= -bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) - bdyty(ibdyty)%blmty(iblmty)%ttFint(:)

        ! if (iblmty > 1) then
        !   write(*,'(A,I0)') 'Element: ',iblmty
        !   write(*,'(3(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%RHSloc
        !    print*,bdyty(ibdyty)%blmty(iblmty)%edof2gdof
        ! endif 

       CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

       CALL assemble_elementary_vector(bdyty(ibdyty)%Fint,-(bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) + &
                                                            bdyty(ibdyty)%blmty(iblmty)%ttFint(:)) , &
                                       bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  


       ! ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       ! IF (errare /= 0) THEN
       !   CALL FATERR(IAM, 'Error allocating dv_ele')
       ! ENDIF

       
       ! DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
       !   ! passage au numero global
       !   inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
       !   ! position un dans le vecteur global pour le numero inodty     
       !   iccdof=bdyty(ibdyty)%ccdof(inodty)
       !   !fd c'est completement idiot car dans increment on fait V=Vbegin 
       !   !
       !   DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
       ! END DO

       ! DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

       ! CALL assemble_elementary_vector(bdyty(ibdyty)%momentum,DV_ele,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       ! DEALLOCATE(DV_ele)
       
     ENDDO

     !
     !if (nbdime==2)then
     !  print*,'RHS'
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
     !  print*,'Fint'
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Fint
     !  print*,'Fext'
     !  write(*,'(2(1x,D12.5))') H*bdyty(ibdyty)%Fext
     !  print*,'Finert'
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Finert
     !else if (nbdime==3)then
     !  print*,'RHS'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     !  print*,'Fint'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fint
     !  print*,'Fext'
     !  write(*,'(3(1x,D12.5))') H*bdyty(ibdyty)%Fext
     !  print*,'Finert'
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Finert
     !endif




     !fd
     !fd ensuite les contributions nodales 
     !fd

     bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(bdyty(ibdyty)%Fext)

     ! print*,'RHS+Fext'
     ! if (nbdime==2)then
     !   write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
     ! else if (nbdime==3)then
     !   write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     ! endif
     ! print*,'========='


   case default

     call FATERR(IAM,'Unknown integrator') 

   end select

 end subroutine assemb_RHS_mecaMAILx
!------------------------------------------------------------------------
 subroutine compute_forces_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   !                         1234567890123456789012345
   character(len=25) :: IAM='mecaMAILx::compute_forces'
   character(len=80) :: cout
   integer(kind=4)   :: iblmty,i,inodty,iccdof,nbdof,errare,iM_bdyty,iM_blmty,ifd,idof,isz,k

   REAL(kind=8),DIMENSION(:),ALLOCATABLE ::  inertia, damping, internal, fext 
   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele

   !fd play
   integer(kind=4),DIMENSION(:),ALLOCATABLE :: map
   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: faux

   REAL(kind=8) :: UsH, gravity(3),Fe,Febegin

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   !rm : why this ? Not use here and makes compute_residue_norm wrong
   !bdyty(ibdyty)%RHS=0.d0

   bdyty(ibdyty)%Fint=0.D0
   bdyty(ibdyty)%Finert=0.D0
   !fd fext bdyty(ibdyty)%Fext=0.D0

   if( .not. bdyty(ibdyty)%visible ) return

   UsH = 1.d0 / H

   ! IF (nbDIME == 2) THEN
   !   gravity = (/ grav1, grav2, 0.d0 /)
   ! ELSE IF (nbDIME == 3) THEN
   !   gravity = (/ grav1, grav2, grav3 /)
   ! ELSE
   !   call faterr(IAM,'unsupported dimension')
   ! ENDIF

   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

     ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
     
     IF (errare /= 0) CALL FATERR(IAM,'Error allocating coor_ele')
     
     ! on utilise les coordonnees des noeuds dans le repere principal
     ! d'inertie pour calculer les matrices de rigidite elementaires
     IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
       coor_ele = get_cooref_local_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
     ELSE 
       coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
     ENDIF

     nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

     isz = nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

!!$     ALLOCATE(inertia(isz),stat=errare)
!!$     IF (errare /= 0) CALL FATERR(IAM, 'Error allocating inertia')

!!$     ALLOCATE(damping(isz),stat=errare)
!!$     IF (errare /= 0) CALL FATERR(IAM, 'Error allocating damping')

     !fd fext
     ! ALLOCATE(fext(isz),stat=errare)
     ! IF (errare /= 0) CALL FATERR(IAM, 'Error allocating fext')

     ALLOCATE(internal(isz),stat=errare)
     IF (errare /= 0) CALL FATERR(IAM, 'Error allocating internal')

     ALLOCATE(faux(isz),stat=errare)
     IF (errare /= 0) CALL FATERR(IAM, 'Error allocating faux')

     ALLOCATE(map(isz),stat=errare)
     IF (errare /= 0) CALL FATERR(IAM, 'Error allocating map')

     DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
       ! passage au numero global
       inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
       ! position un dans le vecteur global pour le numero inodty     
       iccdof=bdyty(ibdyty)%ccdof(inodty)
           
!!$       inertia((i-1)*nbdof+1:i*nbdof)=usH*(bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) - &
!!$                                          bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof))

!!$       damping((i-1)*nbdof+1:i*nbdof) = bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)

       !fd fext fext((i-1)*nbdof+1:i*nbdof) = gravity(1:nbDIME)

       do k=1,nbdof
         map((i-1)*nbdof+k) = iccdof+k
       enddo
     END DO

     ! force d inertie

!!$     inertia = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,inertia)
!!$     CALL assemble_elementary_vector(bdyty(ibdyty)%Finert,inertia,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

     faux = usH*(bdyty(ibdyty)%V(map(1:isz)) - bdyty(ibdyty)%Vbegin(map(1:isz)))
     faux = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,faux)
     CALL assemble_elementary_vector(bdyty(ibdyty)%Finert,faux,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

     ! force externe
     !fd fext
     ! fext = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,fext)
     ! CALL assemble_elementary_vector(bdyty(ibdyty)%Fext,fext+bdyty(ibdyty)%blmty(iblmty)%Fext(:,1), &
     !                                 bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  


     ! forces internes: amortissement + rigidite
!!$     damping = MATMUL(bdyty(ibdyty)%blmty(iblmty)%damping,damping)

     faux(1:isz) = bdyty(ibdyty)%V(map(1:isz))
     faux = MATMUL(bdyty(ibdyty)%blmty(iblmty)%damping,faux)


     iM_bdyty = bdyty2M_bdyty(ibdyty)
     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

     CALL compute_elementary_internal_force(     &
             bdyty(ibdyty)%blmty(iblmty)%blmnb,  &
             bdyty(ibdyty)%blmty(iblmty)%ppsnb,  &
             iM_bdyty,iM_blmty,coor_ele,         &
             internal                            )       

     internal = internal + faux

     CALL assemble_elementary_vector(bdyty(ibdyty)%Fint, internal , bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

!!$     DEALLOCATE(inertia,damping,internal,coor_ele,fext,faux)
     !fd fext DEALLOCATE(internal,coor_ele,fext,faux,map)
     DEALLOCATE(internal,coor_ele,faux,map)     

   ENDDO

   !fd fext
   ! DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof

   !   CALL owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inodty,idof)
   !   iccdof=bdyty(ibdyty)%ccdof(inodty)+idof   

   !   CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)

   !   bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof) + Fe

   ! END DO

   bdyty(ibdyty)%residu =  bdyty(ibdyty)%Fint + bdyty(ibdyty)%Finert -(UsH*bdyty(ibdyty)%Ireac) - bdyty(ibdyty)%Fext 
   !bdyty(ibdyty)%residu = (UsH*bdyty(ibdyty)%Ireac) + bdyty(ibdyty)%Fext - bdyty(ibdyty)%Fint - bdyty(ibdyty)%Finert
   ! bdyty(ibdyty)%Finert - bdyty(ibdyty)%Fext + bdyty(ibdyty)%Fint - bdyty(ibdyty)%Ireac 


   !if (nbdime==2)then 
   !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
   !  print*,'Fint'
   !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%Fint
   !else if (nbdime == 3) then
   !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
   !  print*,'Fint'
   !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fint
   !endif

 end subroutine compute_forces_mecaMAILx
!------------------------------------------------------------------------ 
 subroutine update_bulk_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   integer(kind=4) :: errare,istop
   integer(kind=4) :: iblmty
 !                           1234567890123456789012
   character(len=22) :: IAM='mecaMAILx::update_bulk'
   character(len=80) :: cout
 
   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   IF (is_externalFEM) THEN
     CALL externalFEM_update_bulk(istop)
     IF (istop == 0) TPSbegin=1.d+20
     RETURN
   ENDIF
 
 !
 ! calcul de la contrainte et mise a jour des grandeurs internes
 !
   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     !
     bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
     bdyty(ibdyty)%blmty(iblmty)%Fint(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fint(:,1)
 
   ENDDO
 
   call update_mecagpv_MAILx(bdyty2M_bdyty(ibdyty))

 end subroutine update_bulk_mecaMAILx

 subroutine externalFEM_update_bulk_mecaMAILx()
   implicit none
   !
   integer(kind=4) :: istop
 
   if( nb_mecaMAILx == 0 ) return

   if( is_externalFEM ) then
     call externalFEM_update_bulk(istop)
     if( istop == 0 ) TPSbegin=1.d+20
     return
   end if
 end subroutine
 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
!> routine de calcul de la norme du residu

 subroutine compute_residue_norm_mecaMAILx(norm_res, norm_V, ibdyty)
   implicit none
   integer(kind=4), intent(in)  :: ibdyty
   real(kind=8)   , intent(out) :: norm_res, norm_V
   !
   real(kind=8)      :: max_dV,max_V
   real(kind=8)      :: max_res,max_reac,max_f,max_fint,max_finert,max_fext
   character(len=40) :: cout
   character(len=30) :: IAM
   !      123456789012345678901234567890
   IAM = 'mecaMAILx:compute_residue_norm'

   norm_res=0.d0

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   norm_res=1.d+20


   !IF (is_externalFEM) THEN
   !  CALL externalFEM_check_convergence(iconv)
   !  RETURN
   !ENDIF

   norm_Res = 0.d0
   norm_V   = 0.d0

   !!! Etude de la convergence !!!

   !print*,'Corps : ',ibdyty

   ! if (itchache) then
   !   print*,' '
   !   print*,'residu libre'
   !   print*,bdyty(ibdyty)%RHS
   !   print*,' '
   !   print*,'efforts de contact'
   !   print*,bdyty(ibdyty)%Ireac
   ! endif

   bdyty(ibdyty)%Vaux = bdyty(ibdyty)%V  - bdyty(ibdyty)%Vlast  

!fd debile ?
!fd
   max_dV=MAX(MAXVAL(bdyty(ibdyty)%Vaux),ABS(MINVAL(bdyty(ibdyty)%Vaux)))
!fd
   max_V=MAX(MAXVAL(bdyty(ibdyty)%V),ABS(MINVAL(bdyty(ibdyty)%V)))


   IF (max_V <= 1d-10 ) max_V=1.D0

   norm_V=max_dV/max_V

!   if (itchache) then
!     print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!     print*,'Norme en vitesse :           '
!     print*,'max delta V =',max_dV
!     !     print*,'norme en V  =',tmp
!     print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!
!   endif

   !print*,bdyty(ibdyty)%Fint

   if (M_INTEGRATOR_ID == INTEGRATOR_QS ) then
     bdyty(ibdyty)%Vaux = bdyty(ibdyty)%RHS + (bdyty(ibdyty)%Ireac/H)
     CALL nullify_vlocy_driven_dof(ibdyty,iVaux_) 

     bdyty(ibdyty)%residu = bdyty(ibdyty)%Vaux

     max_res    = MAX(MAXVAL(bdyty(ibdyty)%residu),ABS(MINVAL(bdyty(ibdyty)%residu)))

     max_fint   = MAX(MAXVAL(bdyty(ibdyty)%Fint),ABS(MINVAL(bdyty(ibdyty)%Fint)))

     max_finert = MAX(MAXVAL(bdyty(ibdyty)%Finert),ABS(MINVAL(bdyty(ibdyty)%Finert)))

     max_fext   = MAX(MAXVAL(bdyty(ibdyty)%Fext),ABS(MINVAL(bdyty(ibdyty)%Fext)))

     max_reac   = MAX(MAXVAL(bdyty(ibdyty)%Ireac/H),ABS(MINVAL(bdyty(ibdyty)%Ireac/H)))
      
   else
      bdyty(ibdyty)%Vaux = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Ireac
      CALL nullify_vlocy_driven_dof(ibdyty,iVaux_) 

      bdyty(ibdyty)%residu = bdyty(ibdyty)%Vaux

     max_res=MAX(MAXVAL(bdyty(ibdyty)%residu),ABS(MINVAL(bdyty(ibdyty)%residu)))

     max_fint =H*MAX(MAXVAL(bdyty(ibdyty)%Fint),ABS(MINVAL(bdyty(ibdyty)%Fint)))

     max_finert = H*MAX(MAXVAL(bdyty(ibdyty)%Finert),ABS(MINVAL(bdyty(ibdyty)%Finert)))

     max_fext =  H*MAX(MAXVAL(bdyty(ibdyty)%Fext),ABS(MINVAL(bdyty(ibdyty)%Fext)))

     max_f = MAX(max_fint,MAX(max_finert,max_fext))

     max_reac = MAX(MAXVAL(bdyty(ibdyty)%Ireac),ABS(MINVAL(bdyty(ibdyty)%Ireac)))
      
   endif
  
   max_f      = MAX(max_fint,MAX(max_finert,max_fext))

! bof bof
! bof bof

   IF (max_f <= 1d-01 ) max_f=1.D0

   norm_res=max_res/max_f

!   if (itchache) then
!   print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!   print*,'Norme en residu :           '
!   print*,'max residu    =',max_res
!   print*,'max force int =',max_fint
!   print*,'max force inertie =',max_finert
!   print*,'max reac =',max_reac
!   print*,'norme en res  =',tmp
!   print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!   endif

 end subroutine compute_residue_norm_mecaMAILx
!------------------------------------------------------------------------
! internal routines
!------------------------------------------------------------------------   
SUBROUTINE apply_vlocy_driven_dof(ibdyty,storage)

IMPLICIT NONE 
INTEGER :: ivd,ibdyty,inod,idof,iccdof
INTEGER :: storage

   IF (nb_mecaMAILx == 0) RETURN

   SELECT CASE(storage)
   CASE(iV____)
   
     ! applying driven dof
      DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
         
         if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle
         
         CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
         iccdof=bdyty(ibdyty)%ccdof(inod)+idof   

         bdyty(ibdyty)%V(iccdof)=bdyty(ibdyty)%Vdriv(ivd)    
         bdyty(ibdyty)%X(iccdof)=bdyty(ibdyty)%Xdriv(ivd)
      END DO
   CASE(iVfree)      
      ! applying driven dof
      DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
         
         if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle
         
         CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
         iccdof=bdyty(ibdyty)%ccdof(inod)+idof   

         bdyty(ibdyty)%Vfree(iccdof)=bdyty(ibdyty)%Vdriv(ivd)
      END DO  
             
   CASE default
      call faterr('mecaMAILx::apply_vlocy_driven_dof','unknown storage')
   END SELECT      



END SUBROUTINE apply_vlocy_driven_dof
!------------------------------------------------------------------------   
!------------------------------------------------------------------------   
SUBROUTINE nullify_vlocy_driven_dof(ibdyty,storage)

  IMPLICIT NONE 
  INTEGER :: ivd,ibdyty,inod,idof,iccdof
  INTEGER :: storage

  IF (nb_mecaMAILx == 0) RETURN

  ! initializing driven dof
  DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

     if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active) cycle
     
     CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

     iccdof=bdyty(ibdyty)%ccdof(inod)+idof   

     SELECT CASE(storage)
       CASE(iV____)
         bdyty(ibdyty)%V(iccdof)=0.d0
         bdyty(ibdyty)%X(iccdof)=0.d0
       CASE(iVfree)
         bdyty(ibdyty)%Vfree(iccdof)=0.d0
       CASE(iVaux_)
         bdyty(ibdyty)%Vaux(iccdof)=0.d0
       CASE default
         call faterr('mecaMAILx::nullify_vlocy_driven_dof','unknown storage')
     END SELECT      

   END DO  

END SUBROUTINE nullify_vlocy_driven_dof
!------------------------------------------------------------------------   !
!------------------------------------------------------------------------  
SUBROUTINE nullify_reac_mecaMAILx(ibdyty,storage)

  !
  ! called by vitrad
  !
  
  IMPLICIT NONE 
  INTEGER :: ibdyty
  INTEGER :: storage
  
  IF (nb_mecaMAILx == 0) RETURN

  SELECT CASE(storage)
    CASE (iIreac)
 
      !print*,'nullify reac'

      bdyty(ibdyty)%is_reac_modified=.false.
      bdyty(ibdyty)%Ireac = 0.d0

      ! on annulle aussi la partie rigide de la reaction, le cas echeant
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) bdyty(ibdyty)%RIreac = 0.d0

    CASE (iIaux_)
      bdyty(ibdyty)%Iaux = 0.d0

      ! on annulle aussi la partie rigide de la reaction, le cas echeant
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) bdyty(ibdyty)%RIaux = 0.d0

    CASE default
      call faterr('mecaMAILx::nullify_reac','unknown storage')
  END SELECT 
  
 END SUBROUTINE nullify_reac_mecaMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 SUBROUTINE comp_vlocy_mecaMAILx(ibdyty,storage)

   !
   ! called by vitrad
   !
  
   IMPLICIT NONE 
   INTEGER :: ibdyty,info
   INTEGER :: storage

!                           1234567890123456789012345
   CHARACTER(len=25) :: IAM='mecaMAILx::compute_vlocy'
   CHARACTER(len=80) :: mes

   !INTEGER :: timer1=0,timer2=0

   IF (nb_mecaMAILx == 0) RETURN
                                              !01234567890123456789
   !IF (timer1 == 0) timer1 = get_new_itimer_ID('COMP Vaux=M^-1 Iaux ')
   !IF (timer2 == 0) timer2 = get_new_itimer_ID('COMP Vaux=M^-1 Ireac')

   !IF (storage == iVaux_e_invM_t_Ireac .AND. timer1 /= 0) CALL start_itimer(timer1)
   !IF (storage == iVaux_e_invM_t_Iaux_ .AND. timer2 /= 0) CALL start_itimer(timer2)

   IF (is_externalFEM) THEN
     SELECT CASE(storage)  
     CASE (iVaux_e_invM_t_Ireac)
       CALL externalFEM_comp_vlocy_mecaMAILx(ibdyty,bdyty(ibdyty)%Ireac,bdyty(ibdyty)%Vaux)
     CASE (iVaux_e_invM_t_Iaux_)
       CALL externalFEM_comp_vlocy_mecaMAILx(ibdyty,bdyty(ibdyty)%Iaux,bdyty(ibdyty)%Vaux)
     CASE default
       call faterr(IAM,'unknown storage')
     END SELECT 

     RETURN
   ENDIF

!   print*,'< vitrad'
!   print*,ibdyty
!   print*,bdyty(ibdyty)%Reac

   bdyty(ibdyty)%Vaux=0.d0


   ! if (bdyty(ibdyty)%is_rigid) then
    ! print*,'comp vlocy rigid'
   ! else
    ! print*,'comp vlocy defo'
   ! endif


   SELECT CASE(storage) 
!     case (iV____e_invM_t_Reac_)
!
!       bdyty(ibdyty)%V=bdyty(ibdyty)%Reac
!
!       call solve_linear_system(KTaux,bdyty(ibdyty)%V)
!
!       print*,bdyty(ibdyty)%V
!
     CASE (iVaux_e_invM_t_Ireac)

       !am & pta : on calcule la partie rigide de la correction de la vitesse,
       ! et on annulle le Vaux, car on a pas d'effort deformable
       IF (bdyty(ibdyty)%is_rigid) THEN

          !print*,'Vaux_e_invM_t_Reac'
          !print*,bdyty(ibdyty)%RReac
          !print*,bdyty(ibdyty)%inv_mR


          bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIreac

          !fd car on est rigide
          bdyty(ibdyty)%Vaux  = 0.d0            

          RETURN

       ENDIF

       !am & pta : on calcule la partie rigide de la correction de la vitesse,
       ! le Vaux est calcule par la suite a partir des efforts purment deformables
       IF (bdyty(ibdyty)%is_coro) THEN

          !print*,'Vaux_e_invM_t_Ireac'
          !print*,bdyty(ibdyty)%RIReac
          !print*,bdyty(ibdyty)%inv_mR

          bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIreac

       ENDIF


       ! print*,'====================='
       ! print*,'Ireac                '       
       ! print*,bdyty(ibdyty)%Ireac


       IF (bdyty(ibdyty)%is_precon) THEN

         CALL compute_precon_vaux_mecaMAILx(ibdyty,storage)

       ELSE
 
         if (bdyty(ibdyty)%is_reac_modified) then

           !fd 0_o pour l'explicit il va manquer un terme devant Reac

           if (M_INTEGRATOR_ID == INTEGRATOR_QS) then  
             call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Ireac/H)
           else
             call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Ireac)             
           endif
           
           if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then

             bdyty(ibdyty)%drvvalues = 0.d0

             ! on les passe au g_system
             call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

           endif
           ! on resoud
           CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

           IF (info > 0) THEN
             PRINT*,ibdyty,info
             CALL FATERR(IAM,'No solution')
           ENDIF
         endif

       ENDIF

       ! print*,'Vaux                 '       
       ! print*,bdyty(ibdyty)%Vaux
       ! print*,'====================='

     CASE (iVaux_e_invM_t_Iaux_)

       !print*,'Vaux_e_invM_t_Iaux'

       !am & pta : on calcule la partie rigide de la correction de la vitesse,
       ! et on annulle le Vaux, car on a pas d'effort deformable
       IF (bdyty(ibdyty)%is_rigid) THEN

          ! print*,'Vaux_e_invM_t_Ireac'
          ! print*,bdyty(ibdyty)%RIaux
          ! print*,bdyty(ibdyty)%inv_mR

          bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIaux
          bdyty(ibdyty)%Vaux  = 0.d0
          RETURN

       ENDIF

       !am & pta : on calcule la partie rigide de la correction de la vitesse,
       ! le Vaux est calcule par la suite a partir des efforts purment deformables
       IF (bdyty(ibdyty)%is_coro) THEN

          ! print*,'Vaux_e_invM_t_Reac'
          ! print*,bdyty(ibdyty)%RIaux
          ! print*,bdyty(ibdyty)%inv_mR

          bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIaux

       ENDIF

        !print*,'====================='
        !print*,'Iaux                 '       
        !print*,bdyty(ibdyty)%Iaux

       IF (bdyty(ibdyty)%is_precon) THEN

         CALL compute_precon_vaux_mecaMAILx(ibdyty,storage)

       ELSE
         if (M_INTEGRATOR_ID == INTEGRATOR_QS) then  
           call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux/H)
         else
           call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)
         endif
         if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then

           bdyty(ibdyty)%drvvalues = 0.d0

           ! on les passe au g_system
           call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

         endif

         ! on resoud
         CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

         IF (info > 0 ) THEN
           WRITE(mes,'(A,1x,I0)') 'mecaMAILx:',ibdyty
           CALL logmes(mes)
           CALL FATERR(IAM,'No solution')
         ENDIF

       ENDIF

       !print*,'Vaux                 '       
       !write(*,'(3(1x,D12.5))'),bdyty(ibdyty)%Vaux
       !print*,'====================='

     CASE default
         WRITE(mes,'(A,1x,I0)') 'storage:',ibdyty
         CALL LOGMES(mes)
         CALL FATERR(IAM,'storage type unknown')
   END SELECT 

   !IF (storage == iVaux_e_invM_t_Ireac .AND. timer1 /= 0) CALL stop_itimer(timer1)
   !IF (storage == iVaux_e_invM_t_Iaux_ .AND. timer2 /= 0) CALL stop_itimer(timer2)

        
!fd inutile    call free_matrix(KTaux)

!   print*,'vitrad >'

 END SUBROUTINE comp_vlocy_mecaMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 SUBROUTINE comp_vlocy_bynode_mecaMAILx(ibdyty,list,storage)

   !
   ! called by vitrad
   !
  
   IMPLICIT NONE 
   INTEGER :: ibdyty,info
   INTEGER :: storage
   INTEGER,DIMENSION(:) :: list
!                            1234567890123456789012345678
   CHARACTER(len=28) :: IAM='mecaMAILx::comp_vlocy_bynode'

   IF (nb_mecaMAILx == 0) RETURN

   IF (is_externalFEM) call FATERR(IAM,'impossible to use externalFEM and precon')
   IF (.not. bdyty(ibdyty)%is_precon) call FATERR(IAM,'needs precon')

   !print*,'< vitrad'
   !print*,ibdyty
   !print*,bdyty(ibdyty)%Ireac

   SELECT CASE(storage) 

   CASE (iVaux_e_invM_t_Ireac)

     ! on calcule la partie rigide de la correction de la vitesse,
     ! et on annulle le Vaux, car on a pas d'effort deformable
     IF (bdyty(ibdyty)%is_rigid) THEN

       !print*,'Vaux_e_invM_t_Ireac'
       !print*,bdyty(ibdyty)%RIreac
       !print*,bdyty(ibdyty)%inv_mR


       bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIreac

       !fd car on est rigide
       bdyty(ibdyty)%Vaux  = 0.d0            

       RETURN

     ENDIF

     !am & pta : on calcule la partie rigide de la correction de la vitesse,
     ! le Vaux est calcule par la suite a partir des efforts purment deformables
     IF (bdyty(ibdyty)%is_coro) THEN

       !print*,'Vaux_e_invM_t_Ireac'
       !print*,bdyty(ibdyty)%RIreac
       !print*,bdyty(ibdyty)%inv_mR

       bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIreac

     ENDIF


     CALL compute_precon_vaux_bynode_mecaMAILx(ibdyty,list,storage)

   CASE (iVaux_e_invM_t_Iaux_)
 
     !print*,'Vaux_e_invM_t_Iaux'

     ! on calcule la partie rigide de la correction de la vitesse,
     ! et on annulle le Vaux, car on a pas d'effort deformable
     IF (bdyty(ibdyty)%is_rigid) THEN

       !print*,'Vaux_e_invM_t_Ireac'
       !print*,bdyty(ibdyty)%RIaux
       !print*,bdyty(ibdyty)%inv_mR

       bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIaux
       bdyty(ibdyty)%Vaux  = 0.d0            
       RETURN

     ENDIF

     !am & pta : on calcule la partie rigide de la correction de la vitesse,
     ! le Vaux est calcule par la suite a partir des efforts purment deformables
     IF (bdyty(ibdyty)%is_coro) THEN

       !print*,'Vaux_e_invM_t_Iaux_'
       !print*,bdyty(ibdyty)%RIaux
       !print*,bdyty(ibdyty)%inv_mR

       bdyty(ibdyty)%RVaux = bdyty(ibdyty)%inv_mR*bdyty(ibdyty)%RIaux

     ENDIF

     CALL compute_precon_vaux_bynode_mecaMAILx(ibdyty,list,storage)

   CASE default

     call FATERR(IAM,'kind of "storage" not supported')

   END SELECT 
        
   !print*,'vitrad >'

 END SUBROUTINE comp_vlocy_bynode_mecaMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
SUBROUTINE nullify_vlocy_mecaMAILx(ibdyty,storage)

  !
  ! called by SDL solver
  !
  
  IMPLICIT NONE 
  INTEGER :: ibdyty
  INTEGER :: storage
  
   IF (nb_mecaMAILx == 0) RETURN

  SELECT CASE(storage)
    CASE (iVaux_)
      !am & pta : on initialise a 0 la partie rigide de Vaux
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) bdyty(ibdyty)%RVaux = 0.D0
      bdyty(ibdyty)%Vaux = 0.D0
    CASE default
      call faterr('mecaMAILx::nullify_vlocy','unknown storage')
  END SELECT 
  
END SUBROUTINE nullify_vlocy_mecaMAILx
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------    
  subroutine fatal_damping_mecaMAILx(ibdyty)
    implicit none 
    integer(kind=4), intent(in) :: ibdyty
    !
    integer(kind=4) :: inodty,iccdof
    integer(kind=4) :: iM_bdyty,iM_nodty,iM_ccdof
    character(len=40) :: cout
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'mecaMAILx:fatal_damping'

    if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(ibdyty)%visible ) return
    
    !am & pta : on initialise a 0 la partie rigide de V
    IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) bdyty(ibdyty)%RV = 0.D0
    bdyty(ibdyty)%V=0.D0                

  end subroutine fatal_damping_mecaMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE set_data_equilibrium_mecaMAILx(checktype,tol)
    IMPLICIT NONE
    
    CHARACTER(len=5) :: checktype
    REAL(kind=8)     :: tol

    eqs_tol = tol

    SELECT CASE(checktype)
    CASE('Qvlcy')
       eqs_ichecktype = iQvlcy    
    CASE('Mvlcy')
       eqs_ichecktype = iMvlcy
    CASE default
       call faterr('mecaMAILx::set_data_equilibrium','unknown checktype: '//checktype)
    END SELECT

  END SUBROUTINE set_data_equilibrium_mecaMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE check_equilibrium_state_mecaMAILx(info)
    IMPLICIT NONE 

    INTEGER           :: ibdyty,inodty,iccdof,nbdof,i,nn
    REAL(kind=8)      :: norm,Qnorm,Mnorm
    LOGICAL           :: info
    CHARACTER(len=80) :: cout

    Qnorm = 0.D0
    Mnorm =-1.D20
    nn    = 0

    !am & pta : prevoir le cas rigide...

    DO ibdyty=1,SIZE(bdyty)

       if( .not. bdyty(ibdyty)%visible ) cycle

       DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)
          nn = nn +1
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          norm = 0.d0
          DO i=1,nbdof
             norm= norm + (bdyty(ibdyty)%Vbegin(iccdof+i)**2)
          ENDDO
          norm = dsqrt(norm)

          Qnorm = Qnorm + norm
          Mnorm= MAX (Mnorm,norm)                                
       END DO
    END DO

    Qnorm = Qnorm / REAL(nn,8)

    WRITE(cout,'(1X,A3,2(3X,A12,D10.3,1X))') &
         ' @ ','Qnorm / tol=',Qnorm/eqs_tol,'Mnorm / tol=',Mnorm/eqs_tol 
    CALL LOGMES(cout)

    info = .FALSE.

    SELECT CASE(eqs_ichecktype)
    CASE(iQvlcy)
       IF (Qnorm <= eqs_tol) info = .TRUE.
    CASE(iMvlcy)
       IF (Mnorm <= eqs_tol) info = .TRUE.
    END SELECT

    
  END SUBROUTINE check_equilibrium_state_mecaMAILx 
!!!------------------------------------------------------------------------    
 SUBROUTINE add_reac_nodty_mecaMAILx(ibdyty,inodty,reac,storage)

   !
   ! called by injj
   !
  
   IMPLICIT NONE 

                            !1234567890123456789012345
   character(len=25) :: IAM='mecaMAILx::add_reac_nodty'

   INTEGER :: ibdyty,inodty,iccdof,idof,jdof,idofR
   INTEGER :: storage

   REAL(kind=8),DIMENSION(nbDIME)  :: reac
   REAL(kind=8),DIMENSION(nbDIME)  :: iaux
   REAL(kind=8),DIMENSION(:),allocatable :: iaux_R
   REAL(kind=8),DIMENSION(6)       :: reac_loc
   REAL(kind=8),DIMENSION(nbDIME)  :: dloc ! bras de levier

   logical :: compute = .FALSE.

   iccdof=bdyty(ibdyty)%ccdof(inodty)

   reac_loc=0.d0

   !print*,'< injj'
   !print*,ibdyty,inodty
   !write(*,'(6(1x,D12.5))') reac

   !print *, "add_reac :"
   !print*, ibdyty, inodty

   IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

     ! calcul de la contribution rigide

     ! resultante en global

     reac_loc(1:nbdime)=reac(1:nbdime)
     !
     ! moment en local
     ! ... on passe la reaction en repere inertie
     DO idof=1,nbdime
       iaux(idof) = DOT_PRODUCT(bdyty(ibdyty)%localframeTT(:,idof),reac(:))
     ENDDO

     ! ... on calcule le moment avec la coordonnee du noeud du maillage dans le repere local

     dloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
            bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime) + &
            H*(vw_b*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime) + vw_e*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdime))

     IF (nbdime == 2) THEN

       reac_loc(3) = dloc(1)*iaux(2) - dloc(2)*iaux(1)

     ELSE IF (nbdime == 3) THEN

       reac_loc(4:6) = cross_product(dloc,iaux)

     ENDIF

     SELECT CASE(storage)
     CASE (iIreac)

       ! resultante en global

       bdyty(ibdyty)%RIreac = bdyty(ibdyty)%RIreac + reac_loc(1:bdyty(ibdyty)%nbdofR)


     CASE (iIaux_)

       bdyty(ibdyty)%RIaux = bdyty(ibdyty)%RIaux + reac_loc(1:bdyty(ibdyty)%nbdofR)

       !print*,'RIaux' 
       !write(*,'(6(1x,D12.5))') bdyty(ibdyty)%RIaux

     CASE (iFext_)

       bdyty(ibdyty)%RFext = bdyty(ibdyty)%RFext + reac_loc(1:bdyty(ibdyty)%nbdofR)

     END SELECT 

   ENDIF

   ! calcul de la reaction totale deformable

   !   * cas rigide
   IF (bdyty(ibdyty)%is_rigid) THEN
     !fd comme on est rigide on met la contribution defo a 0 
     reac = 0.d0
   END IF

   !   * cas coro
   IF (bdyty(ibdyty)%is_coro) THEN

     allocate(iaux_R(bdyty(ibdyty)%nbdof))

     !fd si on fait du coro il faut juste virer la contribution rigide de reac
     ! idee generale :
     ! on veut calculer : reac^D = iaux - (D2R)^T*(R2D)^T*iaux
     ! on calcule : reac_loc = (R2D)^T*iaux
     ! on calcule : reac^R = (D2R)^T*reac_loc, de sorte que : reac^D = iaux - reac^R 

     ! 1) on sait qu'on travaille sur un noeud donne, on doit extraire une
     ! tranches (lignes) de l'operateur (D2R)^T, assemble pour tout le maillage.
     ! On va donc calculer reac^R par composante, avec : reac^R(i) = (D2R)^T(j, :)*Rreac
     ! ou j designe la ligne de D2R^T corespondant a la composante i de la
     ! reaction au noeud considere (i.e. j=iccdof + i) 
     ! 2) on a stocke D2R, et on doit donc considere in tranche de D2R
     ! (colonnes)

     ! calcul de la contribution rigide avec la reaction 
     ! exprimee dans le repere principal d'inertie

     reac_loc=0.d0
     do idofR=1,bdyty(ibdyty)%nbdofR
       do jdof=1,nbDIME
         reac_loc(idofR) = reac_loc(idofR) + &
                           (bdyty(ibdyty)%R2D(iccdof + jdof, idofR) * iaux(jdof))
       enddo
     enddo

     !xxx
     ! pas clair si il faut 
     !DO idofR=1,bdyty(ibdyty)%nbdofR
     !   IF (bdyty(ibdyty)%RV_driven_dof(idofR) /= 0) reac_loc(idofR) = 0.d0
     !ENDDO
     !xxx

     ! calcul de reac^R 
     ! ... pour chaque composante de l'objet 
     DO idof=1, bdyty(ibdyty)%nbdof
        iaux_R(idof)=DOT_PRODUCT(bdyty(ibdyty)%D2R(1:bdyty(ibdyty)%nbdofR, idof), reac_loc(1:bdyty(ibdyty)%nbdofR))
     END DO


     !Rq ici reac_loc est bien dans le rep principale d'inertie      


     ! affichages en fonction du type de stockage
     SELECT CASE(storage)
       ! cas Reac
       CASE (iIreac)


       ! cas Iaux
       CASE (iIaux_)

          !write(*,'(A,3(1x,D12.5))') 'reac',reac
          !write(*,'(A,6(1x,D12.5))') 'RIaux',bdyty(ibdyty)%RIaux
          !write(*,'(A,3(1x,D12.5))') 'reac inertie',iaux
          !write(*,'(A,6(1x,D12.5))') '(R2D)^T*reac inertie',reac_loc
          !write(*,'(A,3(1x,D12.5))') '(D2R)^T*(R2D)^T*reac inertie',iaux_R(iccdof+1:iccdof+nbdime)         
          !do idofR=1,bdyty(ibdyty)%nbdofR
          !   print*,dot_product(bdyty(ibdyty)%R2D(:, idofR),iaux(:))
          !enddo
          !write(*,'(A,3(1x,D12.5))') 'reac^D', iaux - iaux_R(iccdof+1:iccdof+nbdime)         
          !print*,'--'

       CASE (iFext_)

     END SELECT 

     ! attention iaux_R distribue sur tout le monde !!

     SELECT CASE(storage)
     CASE (iIreac)

       bdyty(ibdyty)%is_reac_modified=.true.

       DO idof=1,nbDIME
         bdyty(ibdyty)%Ireac(iccdof+idof)=  &
         bdyty(ibdyty)%Ireac(iccdof+idof)+iaux(idof)
       END DO

       bdyty(ibdyty)%Ireac = bdyty(ibdyty)%Ireac - iaux_R

     CASE (iIaux_)
       DO idof=1,nbDIME
         bdyty(ibdyty)%Iaux(iccdof+idof)=   &
         bdyty(ibdyty)%Iaux(iccdof+idof)+iaux(idof)
       END DO
 
       bdyty(ibdyty)%Iaux = bdyty(ibdyty)%Iaux - iaux_R

       !write(*,'(A,3(1x,D12.5))') 'Iaux', bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbDIME)
       !print*,'--'

     CASE (iFext_)
       DO idof=1,nbDIME
         bdyty(ibdyty)%Fext(iccdof+idof)=  &
         bdyty(ibdyty)%Fext(iccdof+idof)+iaux(idof)
       END DO

       bdyty(ibdyty)%Fext = bdyty(ibdyty)%Fext - iaux_R       

     CASE default
         call faterr(IAM,'error reac type not known')
     END SELECT 

     deallocate(iaux_R)

   else

     select case (M_INTEGRATOR_ID)
     case( INTEGRATOR_BETA2 )

       SELECT CASE(storage)
       CASE (iIreac)

         bdyty(ibdyty)%is_reac_modified=.true.

         DO idof=1,nbDIME
           bdyty(ibdyty)%Ireac(iccdof+idof)=  &
           bdyty(ibdyty)%Ireac(iccdof+idof) + reac(idof)
         END DO

       CASE (iIaux_)

         DO idof=1,nbDIME
           bdyty(ibdyty)%Iaux(iccdof+idof)=   &
           bdyty(ibdyty)%Iaux(iccdof+idof) + reac(idof)
         END DO

         !write(*,'(A,3(1x,D12.5))') 'Iaux', bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbDIME)
         !print*,'--'

       CASE (iFext_)

         DO idof=1,nbDIME
           bdyty(ibdyty)%Fext(iccdof+idof)=   &
           bdyty(ibdyty)%Fext(iccdof+idof) + reac(idof)
         END DO

       CASE default
           call faterr(IAM,'error reac type not known')
       END SELECT 

     case( INTEGRATOR_MOREAU )
     !contribution deformable

       SELECT CASE(storage)
       CASE (iIreac)

         bdyty(ibdyty)%is_reac_modified=.true.

         DO idof=1,nbDIME
           bdyty(ibdyty)%Ireac(iccdof+idof)=  &
           bdyty(ibdyty)%Ireac(iccdof+idof)+reac(idof)
         END DO

       CASE (iIaux_)

         DO idof=1,nbDIME
           bdyty(ibdyty)%Iaux(iccdof+idof)=   &
           bdyty(ibdyty)%Iaux(iccdof+idof)+reac(idof)
         END DO

         !write(*,'(A,3(1x,D12.5))') 'Iaux', bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbDIME)
         !print*,'--'

       CASE (iFext_)

         DO idof=1,nbDIME
           bdyty(ibdyty)%Fext(iccdof+idof)=   &
           bdyty(ibdyty)%Fext(iccdof+idof)+reac(idof)
         END DO


       CASE default
           call faterr(IAM,'error reac type not known')
       END SELECT 

     case( INTEGRATOR_QS )
     !contribution deformable

       SELECT CASE(storage)
       CASE (iIreac)

         bdyty(ibdyty)%is_reac_modified=.true.

         DO idof=1,nbDIME
           bdyty(ibdyty)%Ireac(iccdof+idof)=  &
           bdyty(ibdyty)%Ireac(iccdof+idof)+reac(idof)
         END DO

       CASE (iIaux_)

         DO idof=1,nbDIME
           bdyty(ibdyty)%Iaux(iccdof+idof)=   &
           bdyty(ibdyty)%Iaux(iccdof+idof)+reac(idof)
         END DO

         !write(*,'(A,3(1x,D12.5))') 'Iaux', bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbDIME)
         !print*,'--'

       CASE (iFext_)

         DO idof=1,nbDIME
           bdyty(ibdyty)%Fext(iccdof+idof)=   &
           bdyty(ibdyty)%Fext(iccdof+idof)+reac(idof)
         END DO


       CASE default
           call faterr(IAM,'error reac type not known')
       END SELECT 

     case default     

       call faterr(IAM,'Unkown integrator')

     end select  


   END IF


   !print*,'injj >'

END SUBROUTINE add_reac_nodty_mecaMAILx


!!!------------------------------------------------------------------------    
 function get_reac_nodty_mecaMAILx(ibdyty,inodty,storage)

   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty
   REAL(kind=8),DIMENSION(nbDIME)  :: get_reac_nodty_mecaMAILx
   INTEGER :: storage
   ! ****
   integer :: iccdof
                            !12345678901234567890123456
   character(len=26) :: IAM='mecaMAILx::get_reac_nodty'

   iccdof=bdyty(ibdyty)%ccdof(inodty)

   IF (bdyty(ibdyty)%is_coro .or. bdyty(ibdyty)%is_rigid) THEN
     call faterr(IAM,'unavailable for coro or rigid')
   ELSE
     SELECT CASE(storage)
     CASE (iIreac)
       get_reac_nodty_mecaMAILx(1:nbdime) = bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdime)
     CASE (iIaux_)
       get_reac_nodty_mecaMAILx(1:nbdime) = bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbdime)
     CASE (iFext_)
       get_reac_nodty_mecaMAILx(1:nbdime) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdime)
     CASE default
        call faterr(IAM,'storage id not known')
     END SELECT 
   ENDIF
end function
!------------------------------------------------------------------------
!> computes contact detection coordinates
subroutine compute_configurationTT_mecaMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  !
  integer(kind=4)                :: inodty
  REAL(kind=8),DIMENSION(nbDIME) :: coortt,spin
  ! ***
  INTEGER                        :: iM_bdyty,iM_nodty,iccdof
  REAL(kind=8),DIMENSION(nbDIME) :: cooref, Xbegin, Vbegin, coorloc, V, X
  real(kind=8) :: angle
  character(len=40) :: cout
  character(len=33) :: IAM
  !      123456789012345678901234567890123
  IAM = 'mecaMAILx:compute_configurationTT'

  if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
    write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
    call faterr(IAM,cout)
  end if

  if( .not. bdyty(ibdyty)%visible ) return

  select case( M_INTEGRATOR_ID )
  case( INTEGRATOR_BETA2 )

    iM_bdyty=bdyty2M_bdyty(ibdyty)

    do inodty=1,bdyty(ibdyty)%nb_nodes

      iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
      cooref=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

      !fd 0_o on recupere X(tps)
      X = get_Xbegin_nodty_mecaMAILx(ibdyty,inodty)

      !fd 0_o on detecte en coor(tps) ; la condition de contact est verifiee a coor(tps+H)
      bdyty(ibdyty)%coorTT(:,inodty) = cooref + X

    enddo

  case( INTEGRATOR_MOREAU ) 

    if (.not. is_contactdetectionconfiguration_defined) then
      vw_b = 1.d0 - theta
      vw_e = 0.d0
      !call logmes('mecaMAILx::default contact configuration is used')
    endif

    IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

      !print*,'Rigid Body'
      ! partie translation du mvt de corps rigide
      bdyty(ibdyty)%RcoorTT(1:nbdime) = bdyty(ibdyty)%cooref_G(1:nbdime) + &
                                        bdyty(ibdyty)%RXbegin(1:nbdime) + &
                                        (H*(vw_b*bdyty(ibdyty)%RVbegin(1:nbdime) + vw_e*bdyty(ibdyty)%RV(1:nbdime)))


      SELECT CASE (nbdime) 
      CASE(2)
        angle = vw_b*bdyty(ibdyty)%RVbegin(3) + vw_e*bdyty(ibdyty)%RV(3)
        CALL update_inertia_frame22(0,H,angle,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrameTT)
      CASE(3)
        spin(1:3) = vw_b*bdyty(ibdyty)%RVbegin(4:6) + vw_e*bdyty(ibdyty)%RV(4:6)
        CALL update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrameTT)
      END SELECT

      do inodty=1,bdyty(ibdyty)%nb_nodes

        ! partie deformable dans le repere principal d inertie
        iccdof=bdyty(ibdyty)%ccdof(inodty)
        coorloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
                  bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime) + &
                  (H*(vw_b*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime) + vw_e*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime)))

        ! on recombine
        bdyty(ibdyty)%coorTT(:,inodty) =  bdyty(ibdyty)%RcoorTT + &
                                          MATMUL(bdyty(ibdyty)%LocalFrameTT,coorloc)
  
        !write(*,'(I0,3(1x,D12.5))') inodty,bdyty(ibdyty)%coorTT
     enddo
     
     !print*,'xx>', tps, bdyty(ibdyty)%coorTT(:,2)-0.10001

   ELSE
  
      iM_bdyty=bdyty2M_bdyty(ibdyty)

      do inodty=1,bdyty(ibdyty)%nb_nodes

        iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
        cooref=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

        Xbegin = get_Xbegin_nodty_mecaMAILx(ibdyty,inodty)

        Vbegin = get_Vbegin_nodty_mecaMAILx(ibdyty,inodty)
        V      = get_V_nodty_mecaMAILx(ibdyty,inodty)

        bdyty(ibdyty)%coorTT(:,inodty) = cooref + Xbegin + (H*(vw_b*Vbegin + vw_e*V))
      enddo
      !write(*,'(3(1x,D12.5))')  bdyty(ibdyty)%coorTT
    ENDIF

  case( INTEGRATOR_QS ) 

    if (.not. is_contactdetectionconfiguration_defined) then
      !on detecte dans la config begin    
      !vw_b = 1.d0
      vw_b = 0.d0       
      vw_e = 0.d0
      !call logmes('mecaMAILx::default contact configuration is used')
    endif
  
    iM_bdyty=bdyty2M_bdyty(ibdyty)

    do inodty=1,bdyty(ibdyty)%nb_nodes

      iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
      cooref=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

      Xbegin = get_Xbegin_nodty_mecaMAILx(ibdyty,inodty)
      Vbegin = H*get_Vbegin_nodty_mecaMAILx(ibdyty,inodty)
      V      = H*get_V_nodty_mecaMAILx(ibdyty,inodty)

      bdyty(ibdyty)%coorTT(:,inodty) = cooref + Xbegin + (vw_b*Vbegin + vw_e*V)
      
    enddo
    !write(*,'(3(1x,D12.5))')  bdyty(ibdyty)%coorTT

   case default

     call faterr(IAM,'Unknown integrator')

   end select

end subroutine compute_configurationTT_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns reference coordinates of nodes of an element

FUNCTION get_cooref_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES
  INTEGER                     :: iM_bdyty,iM_nodty

  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_cooref_ele

  iM_bdyty=bdyty2M_bdyty(ibdyty)

!  print*,iM_bdyty,ibdyty

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    get_cooref_ele(:,inodes)=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

!    print*,inodty,iM_nodty,coor_ele(:,inodes)

  ENDDO

END FUNCTION get_cooref_ele

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns current coordinates of nodes of an element

FUNCTION get_coor_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES
  INTEGER                     :: iM_bdyty,iM_nodty
  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_coor_ele

  iM_bdyty=bdyty2M_bdyty(ibdyty)

!  print*,iM_bdyty,ibdyty

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
    get_coor_ele(:,inodes)=get_coor_nodty_MAILx(iM_bdyty,iM_nodty)

!    print*,inodty,iM_nodty,get_coor_ele(:,inodes)

  ENDDO
END FUNCTION get_coor_ele

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns reference coordinates of nodes of an element in the inertial frame
!> useful when using rigid or coro models

FUNCTION get_cooref_local_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE

  INTEGER                     :: ibdyty,iblmty,nbNODES
  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_cooref_local_ele
  ! ***
  INTEGER                     :: inodty,inodes
                                     !1234567890123456789012345678901
  character(len=31)           :: IAM='mecaMAILx::get_cooref_local_ele'

  if (.not. bdyty(ibdyty)%is_rigid .AND. .not. bdyty(ibdyty)%is_coro) &
    call FATERR(IAM,'is only avaible for rigid or coro models')

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    get_cooref_local_ele(:,inodes)=bdyty(ibdyty)%cooref_local(:,inodty)
  ENDDO

END FUNCTION get_cooref_local_ele

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns current coordinates of nodes of an element in the inertial frame
!> useful when using rigid or coro models

FUNCTION get_coor_local_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE

  INTEGER                     :: ibdyty,iblmty,nbNODES
  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_coor_local_ele
  ! ***
  INTEGER                     :: inodty,inodes,iccdof
                                     !12345678901234567890123456789
  character(len=29)           :: IAM='mecaMAILx::get_coor_local_ele'

  if (.not. bdyty(ibdyty)%is_rigid .AND. .not. bdyty(ibdyty)%is_coro) &
    call FATERR(IAM,'is only avaible for rigid or coro models')

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iccdof=bdyty(ibdyty)%ccdof(inodty)
    get_coor_local_ele(:,inodes)=bdyty(ibdyty)%cooref_local(:,inodty) + bdyty(ibdyty)%X(iccdof+1:iccdof+nbdime)
  ENDDO

END FUNCTION get_coor_local_ele

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> computes displacement at the begining of the time step

FUNCTION get_Xbegin_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Xbegin_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof

  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    get_Xbegin_nodty_mecaMAILx = get_coorbegin_nodty_mecaMAILx(ibdyty,inodty) - &
                                 get_cooref_nodty_mecaMAILx(ibdyty,inodty)
  ELSE
    iccdof=bdyty(ibdyty)%ccdof(inodty)
    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

    ! attention au cafouillage entre nbdof et nbDIME
    ! ici on remonte 1:nbdime
    nbdof=MIN(nbdof,nbDIME)

    get_Xbegin_nodty_mecaMAILx = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)

  ENDIF

END FUNCTION get_Xbegin_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

FUNCTION get_X_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_X_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof

  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    get_X_nodty_mecaMAILx = get_coor_nodty_mecaMAILx(ibdyty,inodty) - &
                            get_cooref_nodty_mecaMAILx(ibdyty,inodty)
  ELSE
    iccdof=bdyty(ibdyty)%ccdof(inodty)

    ! attention au cafouillage entre nbdof et nbDIME

    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
    nbdof=MIN(nbdof,nbDIME)

    get_X_nodty_mecaMAILx = bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
  ENDIF
END FUNCTION get_X_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

FUNCTION get_Vbegin_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vbegin_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc


  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)


  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    ! attention la vitesse due a la rotation rigide est dans le repere d'inertie
    !TODO voir cafouillage 2D/3D

    dloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
           bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime)

    ! calcul contribution rotation de corps rigide (dans repere d inertie)
    IF (nbdime == 2) THEN
      Vaux(1) = -bdyty(ibdyty)%RVbegin(3)*dloc(2)
      Vaux(2) =  bdyty(ibdyty)%RVbegin(3)*dloc(1)
    ELSE IF (nbdime == 3) THEN
      Vaux = cross_product(bdyty(ibdyty)%RVbegin(4:6),dloc)
    ENDIF

    ! translation rigide + rotation rigide + vitesse defo
    get_Vbegin_nodty_mecaMAILx = bdyty(ibdyty)%RVbegin(1:nbdime)+ &
                                 MATMUL(bdyty(ibdyty)%LocalFrameIni,Vaux + bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime))

  ELSE
    get_Vbegin_nodty_mecaMAILx = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
    if (M_INTEGRATOR_ID == INTEGRATOR_QS ) get_Vbegin_nodty_mecaMAILx = get_Vbegin_nodty_mecaMAILx/H
  ENDIF

END FUNCTION get_Vbegin_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

FUNCTION get_V_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_V_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    !TODO voir cafouillage 2D/3D

    ! effet de l entrainement par la rotation
    ! ... la position de calcul 
    dloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
           bdyty(ibdyty)%X(iccdof+1:iccdof+nbdime)

    ! ... le calcul de la vitesse 
    IF (nbdime == 2) THEN
      Vaux(1) = -bdyty(ibdyty)%RV(3)*dloc(2)
      Vaux(2) =  bdyty(ibdyty)%RV(3)*dloc(1)
    ELSE IF (nbdime == 3) THEN
      Vaux = cross_product(bdyty(ibdyty)%RV(4:6),dloc)
    ENDIF

    ! vitesse au noeud = translation rigide + rotation rigide + vitesse defo
    get_V_nodty_mecaMAILx = bdyty(ibdyty)%RV(1:nbdime) + &
                            MATMUL(bdyty(ibdyty)%LocalFrame,Vaux + bdyty(ibdyty)%V(iccdof+1:iccdof+nbdime)) 

  ELSE
    get_V_nodty_mecaMAILx = bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
    if (M_INTEGRATOR_ID == INTEGRATOR_QS ) get_V_nodty_mecaMAILx = get_V_nodty_mecaMAILx/H
  ENDIF

END FUNCTION get_V_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

FUNCTION get_Vaux_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vaux_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    ! la rotation dand rep TT
    dloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
           bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime) + &
           H*(vw_b*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime) + vw_e*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdime))

    IF (nbdime == 2) THEN
      Vaux(1) = -bdyty(ibdyty)%RVaux(3)*dloc(2)
      Vaux(2) =  bdyty(ibdyty)%RVaux(3)*dloc(1)
    ELSE IF (nbdime == 3) THEN
      Vaux = cross_product(bdyty(ibdyty)%RVaux(4:6),dloc)
    ENDIF

    ! translation + rotation + defo
    get_Vaux_nodty_mecaMAILx = bdyty(ibdyty)%RVaux(1:nbdime)+ &
                               MATMUL(bdyty(ibdyty)%LocalFrameTT,Vaux + bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdime))

    !write(*,'(A,3(1x,D12.5))')  'Rvaux trans',bdyty(ibdyty)%RVaux(1:nbdime)
    !write(*,'(A,3(1x,D12.5))')  'Rvaux rot  ',MATMUL(bdyty(ibdyty)%LocalFrameTT,Vaux)
    !!write(*,'(A,6(1x,D12.5))')  'D2R DVaux  ',v_loc
    !write(*,'(A,3(1x,D12.5))')  '(Id - R2D D2R DVaux)  ', bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdime) - v_glob
    !print*,'---'
  ELSE
    get_Vaux_nodty_mecaMAILx = bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdof)
    if (M_INTEGRATOR_ID == INTEGRATOR_QS ) get_Vaux_nodty_mecaMAILx = get_Vaux_nodty_mecaMAILx/H     
  ENDIF
END FUNCTION get_Vaux_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

FUNCTION get_Vfree_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vfree_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  !global
  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    ! la rotation dans le rep princ d'inertie TT
    dloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
           bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime) + &
           H*(vw_b*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime) + vw_e*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdime))

    IF (nbdime == 2) THEN
      Vaux(1) = -bdyty(ibdyty)%RVfree(3)*dloc(2)
      Vaux(2) =  bdyty(ibdyty)%RVfree(3)*dloc(1)
    ELSE IF (nbdime == 3) THEN
      Vaux = cross_product(bdyty(ibdyty)%RVfree(4:6),dloc)
    ENDIF

    !write(*,'(A,3(1x,D12.5))') 'RVfree trans', bdyty(ibdyty)%RVfree(1:nbdime)
    !write(*,'(A,3(1x,D12.5))') 'RVfree rot  ',  MATMUL(bdyty(ibdyty)%LocalFrameTT,Vaux)
    !write(*,'(A,3(1x,D12.5))') 'DVfree      ',  MATMUL(bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbdime) -v_glob)

    ! rotation + translation + defo
    get_Vfree_nodty_mecaMAILx = bdyty(ibdyty)%RVfree(1:nbdime)+ &
                                MATMUL(bdyty(ibdyty)%LocalFrameTT,Vaux + bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbdime)) 
  ELSE
    get_Vfree_nodty_mecaMAILx = bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbdof)
    if (M_INTEGRATOR_ID == INTEGRATOR_QS ) get_Vfree_nodty_mecaMAILx = get_Vfree_nodty_mecaMAILx/H     
  ENDIF

  !write(*,'(A,1x,I0,3(1x,D12.5))') 'Vfree',inodty, get_Vfree_nodty_mecaMAILx
  !print*,'---'

END FUNCTION get_Vfree_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------
FUNCTION get_Vddm_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vddm_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    ! la rotation dand rep TT
    dloc = bdyty(ibdyty)%cooref_local(:,inodty) + & 
           bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime) + &
           H*(vw_b*bdyty(ibdyty)%Vddm(iccdof+1:iccdof+nbdime) + vw_e*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdime))

    IF (nbdime == 2) THEN
      Vaux(1) = -bdyty(ibdyty)%RVaux(3)*dloc(2)
      Vaux(2) =  bdyty(ibdyty)%RVaux(3)*dloc(1)
    ELSE IF (nbdime == 3) THEN
      Vaux = cross_product(bdyty(ibdyty)%RVaux(4:6),dloc)
    ENDIF

    ! translation + rotation + defo
    get_Vddm_nodty_mecaMAILx = bdyty(ibdyty)%RVaux(1:nbdime)+ &
                               MATMUL(bdyty(ibdyty)%LocalFrameTT,Vaux + bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdime))

    !write(*,'(A,3(1x,D12.5))')  'Rvaux trans',bdyty(ibdyty)%RVaux(1:nbdime)
    !write(*,'(A,3(1x,D12.5))')  'Rvaux rot  ',MATMUL(bdyty(ibdyty)%LocalFrameTT,Vaux)
    !!write(*,'(A,6(1x,D12.5))')  'D2R DVaux  ',v_loc
    !write(*,'(A,3(1x,D12.5))')  '(Id - R2D D2R DVaux)  ', bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdime) - v_glob
    !print*,'---'
  ELSE
    get_Vddm_nodty_mecaMAILx = bdyty(ibdyty)%Vddm(iccdof+1:iccdof+nbdof)
  ENDIF
END FUNCTION get_Vddm_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns reference coordinates

FUNCTION get_cooref_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_cooref_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iM_bdyty,iM_nodty


  iM_bdyty=bdyty2M_bdyty(ibdyty)
  iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

  get_cooref_nodty_mecaMAILx = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

END FUNCTION get_cooref_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns begin coordinates

FUNCTION get_coorbegin_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty, inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_coorbegin_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iM_bdyty, iM_nodty, iccdof
  REAL(kind=8),DIMENSION(nbDIME) :: coorloc

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

  !am & pta : on passe les coordonnees de la configuration intermediaire du
  !repere principal d'inertie au repere global
  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    !print*,'Rigid Body'

    iccdof=bdyty(ibdyty)%ccdof(inodty)

    ! partie translation du mvt de corps rigide
    get_coorbegin_nodty_mecaMAILx(1:nbdime) = bdyty(ibdyty)%cooref_G(1:nbdime) + bdyty(ibdyty)%RXbegin(1:nbdime)

    ! partie deformable
    coorloc = bdyty(ibdyty)%cooref_local(1:nbdime,inodty) + bdyty(ibdyty)%Xbegin(1:nbdime)

    !write(*,'(I0,3(1x,D12.5))') inodty,coorloc

    get_coorbegin_nodty_mecaMAILx =  get_coorbegin_nodty_mecaMAILx + &
                                     MATMUL(bdyty(ibdyty)%LocalFrameini,coorloc)

    !write(*,'(I0,3(1x,D12.5))') inodty,get_coorbegin_nodty_mecaMAILx

  ELSE

    get_coorbegin_nodty_mecaMAILx = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty) + &
                                   get_Xbegin_nodty_mecaMAILx(ibdyty,inodty)

  ENDIF
END FUNCTION get_coorbegin_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns contact detection coordinates

FUNCTION get_coorTT_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_coorTT_nodty_mecaMAILx
  ! ***

  get_coorTT_nodty_mecaMAILx = bdyty(ibdyty)%coorTT(:,inodty)

END FUNCTION get_coorTT_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> returns current coordinates

FUNCTION get_coor_nodty_mecaMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty, inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_coor_nodty_mecaMAILx
  ! ***
  INTEGER                        :: iM_bdyty, iM_nodty, iccdof
  REAL(kind=8),DIMENSION(nbDIME) :: coorloc

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

  !am & pta : on passe les coordonnees de la configuration intermediaire du
  !repere principal d'inertie au repere global
  IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN

    !print*,'Rigid Body'

    iccdof=bdyty(ibdyty)%ccdof(inodty)

    ! partie translation du mvt de corps rigide
    get_coor_nodty_mecaMAILx(1:nbdime) = bdyty(ibdyty)%cooref_G(1:nbdime) + bdyty(ibdyty)%RX(1:nbdime)

    ! partie deformable

    coorloc = bdyty(ibdyty)%cooref_local(:,inodty) + bdyty(ibdyty)%X(iccdof+1:iccdof+nbdime)

    !write(*,'(I0,3(1x,D12.5))') inodty,coorloc

    get_coor_nodty_mecaMAILx =  get_coor_nodty_mecaMAILx + &
                                MATMUL(bdyty(ibdyty)%LocalFrame,coorloc)

    !write(*,'(I0,3(1x,D12.5))') inodty,get_coor_nodty_mecaMAILx

  ELSE

    get_coor_nodty_mecaMAILx = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty) + &
                               get_X_nodty_mecaMAILx(ibdyty,inodty)

  ENDIF
END FUNCTION get_coor_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

 subroutine write_out_Rnod(nfich,list,length)
   implicit none
   integer(kind=4), intent(in) :: nfich,length
   integer(kind=4), dimension(:), pointer :: list
   !
   INTEGER :: ibdyty,inodty,iccdof,nbdof
   INTEGER :: lc 
   INTEGER :: err 

   err = 0

   IF (nfich == 1) THEN
     lc = LEN_TRIM(out_Rnod)
     OPEN(unit=1,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc))),IOSTAT=err) 
   ELSE IF (nfich == 2) THEN
     lc = LEN_TRIM(last_Rnod)
     OPEN(unit=2,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc))),IOSTAT=err) 
   END IF

   !fd a voir si il faut affiner
   IF ( err /= 0) RETURN 

   if( .not. associated(list) ) then

     do ibdyty = 1, nb_mecaMAILx
                            !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(nfich,'(A72)') '$bdyty                                                                  '
       WRITE(nfich,101) 'MAILx',ibdyty
                            !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(nfich,'(A72)') '$nodty                                                                  ' 

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)

         nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
         iccdof=bdyty(ibdyty)%ccdof(inodty)

         CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                            bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdof)/H, &
                            'R/H',nfich)

       ENDDO
         WRITE(nfich,'(A6)')'$$$$$$'
         WRITE(nfich,'(A6)')'      '
     end do 

   else

     do ibdyty = 1, length
                            !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(nfich,'(A72)') '$bdyty                                                                  '
       WRITE(nfich,101) 'MAILx',list(ibdyty)
                            !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(nfich,'(A72)') '$nodty                                                                  ' 

       DO inodty=1,SIZE(bdyty(list(ibdyty))%nodty)

         nbdof=nbdof_a_nodty(bdyty(list(ibdyty))%nodty(inodty))
         iccdof=bdyty(list(ibdyty))%ccdof(inodty)

         CALL write_a_nodty(get_nodNAME(bdyty(list(ibdyty))%nodty(inodty)),inodty, &
                            bdyty(list(ibdyty))%Ireac(iccdof+1:iccdof+nbdof)/H, &
                            'R/H',nfich)

       ENDDO
         WRITE(nfich,'(A6)')'$$$$$$'
         WRITE(nfich,'(A6)')'      '
     end do 

   end if
!                      123456789012345678901234567890123456789012345678901234567890123456789012
   WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
101   FORMAT(1X,A5,I7)            
           
   IF (nfich == 1) CLOSE(1)
   IF (nfich == 2) CLOSE(2)
   
 end subroutine write_out_Rnod

!!!------------------------------------------------------------------------

 subroutine write_out_nodforces(nfich,list,length)
   implicit none
   integer(kind=4), intent(in) :: nfich,length
   integer(kind=4), dimension(:), pointer :: list
   !
   integer(kind=4) :: ibdyty,inodty,iccdof,nbdof
   real(kind=8)    ::sumx,sumy

   if( .not. associated(list) ) then

     do ibdyty = 1, nb_mecaMAILx

       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101) 'MAILx',ibdyty
       WRITE(nfich,'(A6)') '$nodty'

       sumx=0.d0
       sumy=0.d0
       
       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdof)/H, &
                             'R/H',nfich)
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%Fint(iccdof+1:iccdof+nbdof),&
                             'Fin',nfich)
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%Finert(iccdof+1:iccdof+nbdof), &
                             'Mac',nfich)

          sumx= sumx + bdyty(ibdyty)%Ireac(iccdof+1)/H &
                     + bdyty(ibdyty)%Fint(iccdof+1) &
                     + bdyty(ibdyty)%Finert(iccdof+1)

          sumy= sumy + bdyty(ibdyty)%Ireac(iccdof+2)/H &
                     + bdyty(ibdyty)%Fint(iccdof+2) &
                     + bdyty(ibdyty)%Finert(iccdof+2)

       END DO

       WRITE(nfich,'(2(1x,D20.13))') sumx,sumy
       
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
     end do

   else

     do ibdyty = 1, length

       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101) 'MAILx',list(ibdyty)
       WRITE(nfich,'(A6)') '$nodty'

       sumx=0.d0
       sumy=0.d0
       
       DO inodty=1,SIZE(bdyty(list(ibdyty))%nodty)
          
          nbdof=nbdof_a_nodty(bdyty(list(ibdyty))%nodty(inodty))
          iccdof=bdyty(list(ibdyty))%ccdof(inodty)
          
          CALL write_a_nodty(get_nodNAME(bdyty(list(ibdyty))%nodty(inodty)),inodty, &
                             bdyty(list(ibdyty))%Ireac(iccdof+1:iccdof+nbdof)/H, &
                             'R/H',nfich)
          CALL write_a_nodty(get_nodNAME(bdyty(list(ibdyty))%nodty(inodty)),inodty, &
                             bdyty(list(ibdyty))%Fint(iccdof+1:iccdof+nbdof),&
                             'Fin',nfich)
          CALL write_a_nodty(get_nodNAME(bdyty(list(ibdyty))%nodty(inodty)),inodty, &
                             bdyty(list(ibdyty))%Finert(iccdof+1:iccdof+nbdof), &
                             'Mac',nfich)

          sumx= sumx + bdyty(list(ibdyty))%Ireac(iccdof+1)/H &
                     + bdyty(list(ibdyty))%Fint(iccdof+1) &
                     + bdyty(list(ibdyty))%Finert(iccdof+1)

          sumy= sumy + bdyty(list(ibdyty))%Ireac(iccdof+2)/H &
                     + bdyty(list(ibdyty))%Fint(iccdof+2) &
                     + bdyty(list(ibdyty))%Finert(iccdof+2)

       END DO

       WRITE(nfich,'(2(1x,D20.13))') sumx,sumy
       
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
     end do

   end if
   !                     123456789012345678901234567890123456789012345678901234567890123456789012
   WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
   
101 FORMAT(1X,A5,I7)            
   
 end subroutine write_out_nodforces

!!!------------------------------------------------------------------------

 SUBROUTINE update_wear

   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty,iccdof

   INTEGER :: iM_bdyty,iM_nodty,iM_ccdof

   IF (nb_mecaMAILx == 0) RETURN  

   DO ibdyty=1,SIZE(bdyty)
     bdyty(ibdyty)%V=bdyty(ibdyty)%V+bdyty(ibdyty)%Vwear                
     bdyty(ibdyty)%X=bdyty(ibdyty)%X+H*bdyty(ibdyty)%Vwear
     bdyty(ibdyty)%Xwear=bdyty(ibdyty)%Xwear+H*bdyty(ibdyty)%Vwear
   END DO
 END SUBROUTINE update_wear

!------------------------------------------------------------------------ 

 FUNCTION get_Vwear_nodty_mecaMAILx(ibdyty,inodty)

   IMPLICIT NONE
   INTEGER                        :: ibdyty,inodty,iccdof,nbdof
   REAL(kind=8),DIMENSION(nbDIME) :: get_Vwear_nodty_mecaMAILx

   iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

   nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
   nbdof=MIN(nbdof,nbDIME)

   get_Vwear_nodty_mecaMAILx = bdyty(ibdyty)%Vwear(iccdof+1:iccdof+nbdof)

 END FUNCTION get_Vwear_nodty_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

 SUBROUTINE put_Vwear_nodty_mecaMAILx(ibdyty,inodty,Vwear)

   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty,iccdof,idof

   REAL(kind=8),DIMENSION(2)  :: Vwear

   iccdof=bdyty(ibdyty)%ccdof(inodty)
 
   DO idof=1,2
     IF (Vwear(idof)/=0.D0) THEN
       IF (bdyty(ibdyty)%Vwear(iccdof+idof) == 0.d0) THEN
         bdyty(ibdyty)%Vwear(iccdof+idof)= Vwear(idof)
       ELSE
         bdyty(ibdyty)%Vwear(iccdof+idof)= (bdyty(ibdyty)%Vwear(iccdof+idof)+Vwear(idof))*0.5d0
       ENDIF
     ENDIF
   END DO
 
 END SUBROUTINE put_Vwear_nodty_mecaMAILx
!------------------------------------------------------------------------
 SUBROUTINE write_Xwear

   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty,iccdof

   INTEGER :: iM_bdyty,iM_nodty,iM_ccdof

   IF (nb_mecaMAILx == 0) RETURN  

   DO ibdyty=1,SIZE(bdyty)
     WRITE(*,*)'Xwear',  bdyty(ibdyty)%Xwear
   END DO

 END SUBROUTINE write_Xwear
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------    
 SUBROUTINE nullify_Vwear

   IMPLICIT NONE 
   INTEGER :: ibdyty

   IF (nb_mecaMAILx == 0) RETURN  

   DO ibdyty=1,SIZE(bdyty)
     bdyty(ibdyty)%Vwear=0.d0
   END DO

 END SUBROUTINE nullify_Vwear
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 INTEGER FUNCTION get_entity_mecaMAILx(ibdyty)

    IMPLICIT NONE

    INTEGER,INTENT(in)       :: ibdyty

    get_entity_mecaMAILx = nb_existing_entities + ibdyty

 END FUNCTION get_entity_mecaMAILx

!------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_mecaMAILx()

   IMPLICIT NONE

   get_nb_mecaMAILx = nb_mecaMAILx

 END FUNCTION get_nb_mecaMAILx
!------------------------------------------------------------------------
!> get the forces at node
!>
 SUBROUTINE get_nodal_forces_mecaMAILx(ibdyty,inodty,sz,reac,fint,finert,fext,res,momentum)

    IMPLICIT NONE

    INTEGER,INTENT(in)         :: ibdyty,inodty,sz
    REAL(kind=8),DIMENSION(sz) :: reac,fint,Finert,fext,res,momentum
    ! *****
    INTEGER                    :: nbdof,iccdof
    !                         123456789012345678901234567
    CHARACTER(len=27) :: IAM='mecaMAILx::get_nodal_forces'
    CHARACTER(len=80) :: cout

    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

    IF (sz > nbdof) THEN
      write(cout,'(3(A,1x,I0,1x))') 'sz',sz,'nbdof',nbdof,'inodty',inodty
      call LOGMES(cout)
      CALL FATERR(IAM,'sz wanted greater than nbdof')
    ENDIF

    iccdof=bdyty(ibdyty)%ccdof(inodty)

    reac(1:sz)   = bdyty(ibdyty)%Ireac(iccdof+1:iccdof+sz)/H

    ! - int sigma:eps - C*v
    fint(1:sz)   = bdyty(ibdyty)%Fint(iccdof+1:iccdof+sz)

    ! - m a 
    finert(1:sz) = bdyty(ibdyty)%Finert(iccdof+1:iccdof+sz)

    ! fext + m g
    fext(1:sz)   = bdyty(ibdyty)%Fext(iccdof+1:iccdof+sz)

    ! mv
    momentum     = bdyty(ibdyty)%momentum(iccdof+1:iccdof+sz)

    ! res doit etre fint + finert - reac - fext
    !faux res          = reac + fint + finert + fext
    res          = bdyty(ibdyty)%residu(iccdof+1:iccdof+sz)
    
 END SUBROUTINE get_nodal_forces_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> gets displacement and velocity of node

 SUBROUTINE get_nodal_displacements_mecaMAILx(ibdyty,inodty,sz,disp,vel)

    IMPLICIT NONE

    INTEGER,INTENT(in)         :: ibdyty,inodty,sz
    REAL(kind=8),DIMENSION(sz) :: disp,vel
    ! *****
    INTEGER                    :: nbdof
    !                         1234567890123456789012345678901234
    CHARACTER(len=34) :: IAM='mecaMAILx::get_nodal_displacements'

    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

    IF (sz > nbdof) THEN
      CALL FATERR(IAM,'sz wanted greater than nbdof')
    ENDIF

    disp(1:sz)  =  get_X_nodty_mecaMAILx(ibdyty,inodty)
    vel(1:sz)   =  get_V_nodty_mecaMAILx(ibdyty,inodty)

 END SUBROUTINE get_nodal_displacements_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

 SUBROUTINE get_2DNodalStress_mecaMAILx(ibdyty,S)

  IMPLICIT NONE
  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: S
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty,i,nbe
                           !1234567890123456789012345678
  CHARACTER(len=28) :: IAM='mecaMAILx::get_2DNodalStress'

  !fd cette routine effectue un lissage aux noeuds des contraintes calculees aux points de gauss

  !fd on ne tient pas compte des joints
  
  !fd il n'est pas sur qu'elle devrait se trouver ici ...
  !fd bref ca ne me plait pas.
  
  ! allocation dans routine appelante (5,nb_nodes)
  ! stockage a la MatLib Sxx,Syy,Sxy,Szz,Svm

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  S = 0.d0

  nbfields = 5

  IF ( nbfields /= SIZE(S,dim=1)) THEN
    PRINT *,nbfields,SIZE(S,dim=1)
    CALL FATERR(IAM,'Non conforming size first dim')
  ENDIF

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     
    if (is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,'joint    ')) cycle
     
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                     bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                     iM_bdyty,iM_blmty,2,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO   

    DEALLOCATE(temp)

  ENDDO 
     
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    nbe=0 
    do i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
      if (is_ele_mecaEF(bdyty(ibdyty)%blmty(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i(i))%blmnb,'joint   ')) cycle     
      nbe=nbe+1
    enddo
     
    S(:,inodty)=S(:,inodty)/nbe
  ENDDO   

  ! DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
  !   S(:,inodty)=S(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ! ENDDO    

  
END SUBROUTINE get_2DNodalStress_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

SUBROUTINE get_2DNodalStrain_mecaMAILx(ibdyty,E)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: E
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty,i,nbe

                           !1234567890123456789012345678
  CHARACTER(len=28) :: IAM='mecaMAILx::get_2DNodalStrain'

  ! allocation dans routine appelante (5,nb_nodes)
  ! Epsxx,Epsyy,Epsxy,Epszz,J

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  E = 0.d0

  nbfields=5

  IF ( nbfields /= SIZE(E,dim=1)) THEN
    PRINT *,nbfields,SIZE(E,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     
    if ( is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,'joint   ') ) cycle
     
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)


    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0
    
    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                   bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                   iM_bdyty,iM_blmty,1,temp,NbFields,NbNodes_stored)
 
    DO inodty=1,NbNodes_stored
      E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

     
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    nbe=0 
    do i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
      if (is_ele_mecaEF(bdyty(ibdyty)%blmty(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i(i))%blmnb,'joint   ')) cycle     
      nbe=nbe+1
    enddo
     
    E(:,inodty)=E(:,inodty)/nbe
  ENDDO   
 
  ! DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
  !   E(:,inodty)=E(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ! ENDDO    

END SUBROUTINE get_2DNodalStrain_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

SUBROUTINE get_2DNodalInternalVariables_mecaMAILx(ibdyty,a)

  !fd uniquement les variables scalaires
  
  IMPLICIT NONE

  INTEGER :: ibdyty,iiv,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,i,nbe
  INTEGER :: iM_bdyty,iM_blmty
  !                         12345678901234567890123456789012345689
  CHARACTER(len=39) :: IAM='mecaMAILx::get_2DNodalInternalVariable'


  REAL(kind=8),DIMENSION(:,:) :: a

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  a= 0.d0

  !fd pifometre ; rendre ca dynamique
  nbfields = 10
  
  iM_bdyty = bdyty2M_bdyty(ibdyty)
  
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    if (is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,'joint    ')) cycle     
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                        bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                        iM_bdyty,iM_blmty,5,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      A(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=A(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 
     
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)

    nbe=0 
    do i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
      if (is_ele_mecaEF(bdyty(ibdyty)%blmty(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i(i))%blmnb,'joint   ')) cycle     
      nbe=nbe+1
    enddo
     
    A(:,inodty)=A(:,inodty)/nbe
  ENDDO   
  
  ! DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
  !   A(:,inodty)=A(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ! ENDDO    

  
END SUBROUTINE get_2DNodalInternalVariables_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

 SUBROUTINE get_3DNodalStress_mecaMAILx(ibdyty,S)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,i,nbe
  INTEGER :: iM_bdyty,iM_blmty
  !                         1234567890123456789012345678
  CHARACTER(len=28) :: IAM='mecaMAILx::get_3DNodalStress'

  !fd cette routine effectue un lissage aux noeuds des contraintes calculees aux points de gauss
  !fd il n'est pas sur qu'elle devrait se trouver ici ...
  !fd bref ca ne me plait pas.

  ! allocation dans routine appelante (7,nb_nodes)
  ! stockage a la MatLib Sxx,Sxy,Syy,Sxz,Syz,Szz,Svm

  REAL(kind=8),DIMENSION(:,:) :: S

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  S = 0.d0

  nbfields = 7

  IF ( nbfields /= SIZE(S,dim=1)) THEN
    PRINT *,nbfields,SIZE(S,dim=1)
    CALL FATERR(IAM,'Non conforming size first dim')
  ENDIF

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    if (is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,'joint    ')) cycle
     
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                     bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                     iM_bdyty,iM_blmty,2,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

     
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)

    nbe=0 
    do i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
      if (is_ele_mecaEF(bdyty(ibdyty)%blmty(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i(i))%blmnb,'joint   ')) cycle     
      nbe=nbe+1
    enddo
     
    S(:,inodty)=S(:,inodty)/nbe
  ENDDO   
  
  ! DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
  !   !print*,inodty,S(6,inodty),SIZE(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
  !   S(:,inodty)=S(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ! ENDDO    

END SUBROUTINE get_3DNodalStress_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

SUBROUTINE get_3DNodalStrain_mecaMAILx(ibdyty,E)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,i,nbe
  INTEGER :: iM_bdyty,iM_blmty
  !                         1234567890123456789012345678
  CHARACTER(len=28) :: IAM='mecaMAILx::get_3DNodalStrain'

  ! allocation dans routine appelante (7,nb_nodes)
  ! Exx,Exy,Eyy,Exz,Eyz,Ezz,J

  REAL(kind=8),DIMENSION(:,:) :: E

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  E = 0.d0

  nbfields=7

  IF ( nbfields /= SIZE(E,dim=1)) THEN
    PRINT *,nbfields,SIZE(E,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    if (is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,'joint    ')) cycle
     
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0


    CALL gpv2node_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                    bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                    iM_bdyty,iM_blmty,1,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

     
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    nbe=0 
    do i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
      if (is_ele_mecaEF(bdyty(ibdyty)%blmty(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i(i))%blmnb,'joint   ')) cycle     
      nbe=nbe+1
    enddo
     
    E(:,inodty)=E(:,inodty)/nbe
  ENDDO   
  
  ! DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
  !   E(:,inodty)=E(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ! ENDDO    

  
END SUBROUTINE get_3DNodalStrain_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

SUBROUTINE get_3DNodalInternalVariables_mecaMAILx(ibdyty,a)

  !fd uniquement les variables scalaires
  
  IMPLICIT NONE

  INTEGER :: ibdyty,iiv,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,i,nbe
  INTEGER :: iM_bdyty,iM_blmty
  !                         12345678901234567890123456789012345689
  CHARACTER(len=39) :: IAM='mecaMAILx::get_3DNodalInternalVariable'


  REAL(kind=8),DIMENSION(:,:) :: a

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  a= 0.d0

  !fd le cas le plus defavorable ; rendre ca dynamique
  nbfields = 57
  
  iM_bdyty = bdyty2M_bdyty(ibdyty)
  
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    if (is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,'joint    ')) cycle     
      
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                        bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                        iM_bdyty,iM_blmty,5,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      A(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=A(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

     
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    nbe=0 
    do i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
      if (is_ele_mecaEF(bdyty(ibdyty)%blmty(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i(i))%blmnb,'joint   ')) cycle     
      nbe=nbe+1
    enddo
     
    A(:,inodty)=A(:,inodty)/nbe
  ENDDO   
  
  ! DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
  !   A(:,inodty)=A(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ! ENDDO    

  
END SUBROUTINE get_3DNodalInternalVariables_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

 SUBROUTINE get_3DElementStress_mecaMAILx(ibdyty,S)

  IMPLICIT NONE
  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: S
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty
                           !1234567890123456789012345678901234
  CHARACTER(len=34) :: IAM='mecaMAILx::get_all_3DElementStress'

  nbfields = 7
  
  ! allocate(S(nbfields,size(bdyty(ibdyty)%blmty))
  S = 0.d0
  
  do ibdyty=1,size(bdyty) 
  
     iM_bdyty = bdyty2M_bdyty(ibdyty)

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

       CALL gpv2element_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                        bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                        iM_bdyty,iM_blmty,2,S(:,iblmty),NbFields)
     ENDDO 

  enddo   
     
END SUBROUTINE get_3DElementStress_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------

LOGICAL FUNCTION get_write_DOF_mecaMAILx(fantome)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: fantome

  get_write_DOF_mecaMAILx = write_DOF

END FUNCTION get_write_DOF_mecaMAILx

!------------------------------------------------------------------------

LOGICAL FUNCTION get_write_Rnod_mecaMAILx(fantome)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: fantome

  get_write_Rnod_mecaMAILx = write_Rnod

END FUNCTION get_write_Rnod_mecaMAILx

!------------------------------------------------------------------------

LOGICAL FUNCTION CHECK_mecaMAILx(fantome)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: fantome
  INTEGER :: nb_MAILx
    
  nb_MAILx = get_nb_MAILx()
  CHECK_mecaMAILx = .FALSE.
    
  IF(nb_MAILx.NE.0) CHECK_mecaMAILx = .TRUE.

END FUNCTION CHECK_mecaMAILx

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 

 !> declares that on this body a reduced (on contact dofs) W matrix will be evaluated  

 SUBROUTINE set_precon_body_mecaMAILx (iM_bdyty)
   IMPLICIT NONE
   INTEGER :: iM_bdyty
   INTEGER ::   ibdyty

   PRINT*,'Le corps ',iM_bdyty,' est precon'

   ibdyty=M2meca(iM_bdyty)%bdyty
   bdyty(ibdyty)%is_precon = .TRUE.

   ALLOCATE(bdyty(ibdyty)%nodes_precon(bdyty(ibdyty)%nb_nodes))
   bdyty(ibdyty)%nodes_precon = 0

   ! dans le cas coro on est oblige de calcul M^-1 et pas juste la restriction au bord
   if (bdyty(ibdyty)%is_coro) bdyty(ibdyty)%nodes_precon = 1

 END SUBROUTINE set_precon_body_mecaMAILx

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 

 !> allows contactors to declare support nodes  

 SUBROUTINE set_precon_node_mecaMAILx(ibdyty,inodty)
   IMPLICIT NONE

   INTEGER ::   ibdyty,  inodty

!   PRINT*,ibdyty,bdyty(ibdyty)%is_precon

   IF (.NOT. bdyty(ibdyty)%is_precon) RETURN 


!   PRINT*,"mecaMAILx: ",ibdyty,"Noeud: ",inodty

   bdyty(ibdyty)%nodes_precon(inodty) = 1

 END SUBROUTINE set_precon_node_mecaMAILx

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------

 !> computes the W matrix

 SUBROUTINE compute_precon_W_mecaMAILx
 IMPLICIT NONE

 !fd 
 ! assemble KT
 ! applique les cdl sur KT
 ! factorisation de KT
 ! boucle sur les noeuds precon et calcule la valeur precon  
 !fd


   REAL(kind=8) :: HT,HT2
   INTEGER :: ibdyty,iblmty,inodty,idof,jdof,gdof,ivd,iccdof,inod,info,ivd_
   INTEGER :: nbd,nbn

!                           1234567890123456789012345678
   CHARACTER(len=28) :: IAM='mecaMAILx::compute_precon_W'
   character(len=80) :: cout

   IF (nb_mecaMAILx == 0) RETURN

   HT=THETA*H
   HT2=HT*HT

   !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ibdyty,inodty,iblmty,inod,idof,jdof,gdof,ivd,nbn,nbd,info)
   !$OMP DO SCHEDULE(RUNTIME)
   DO ibdyty=1,SIZE(bdyty)

     IF (.NOT. bdyty(ibdyty)%is_precon) CYCLE
     
     IF (bdyty(ibdyty)%saved_precon_W) CYCLE !pta 22/03/2013

     write(cout,'(A,I0)') 'On construit W_precon pour le corps ',ibdyty
     call logmes(cout)

     !fd 0/ preparation (do the same than init_precon_W)

     nbd = 0; nbn = 0
     DO inodty=1,bdyty(ibdyty)%nb_nodes

       IF (bdyty(ibdyty)%nodes_precon(inodty) /= 0 ) THEN
         nbn  = nbn + 1
         nbd  = nbd + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       ENDIF

     ENDDO

     bdyty(ibdyty)%nbdof_precon=nbd

     write(cout,'(A,I0,A,I0)') 'Corps : ',ibdyty,'          nb dof : ',bdyty(ibdyty)%nbdof
     call logmes(cout)
     write(cout,'(A,I0,A,I0)') 'Corps : ',ibdyty,'  nb node precon : ',nbn
     call logmes(cout)
     write(cout,'(A,I0,A,I0)') 'Corps : ',ibdyty,'   nb dof precon : ',bdyty(ibdyty)%nbdof_precon
     call logmes(cout)

     ALLOCATE(bdyty(ibdyty)%W_precon(nbd,nbd))
     !ALLOCATE(bdyty(ibdyty)%W_precon_T(nbd,nbd))
     ALLOCATE(bdyty(ibdyty)%Vaux_precon(nbd))
     ALLOCATE(bdyty(ibdyty)%p2g(nbd))
     ALLOCATE(bdyty(ibdyty)%g2p(bdyty(ibdyty)%nbdof))

     nbd = 0; nbn = 0
     bdyty(ibdyty)%g2p = 0

     DO inodty=1,bdyty(ibdyty)%nb_nodes

       IF (bdyty(ibdyty)%nodes_precon(inodty) /= 0 ) THEN
         nbn  = nbn + 1
         
         DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
           bdyty(ibdyty)%p2g(nbd+idof) = bdyty(ibdyty)%ccdof(inodty) + idof
           bdyty(ibdyty)%g2p(bdyty(ibdyty)%ccdof(inodty) + idof) = nbd + idof 
         ENDDO
         nbd  = nbd + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       ENDIF

     ENDDO

     write(cout,'(A,I0,A)') 'Corps : ',ibdyty,'  -> maps ok'
     call logmes(cout)

     !paranoiac test
     DO idof=1,bdyty(ibdyty)%nbdof_precon
       IF (bdyty(ibdyty)%g2p(bdyty(ibdyty)%p2g(idof)) /= idof) THEN
         write(cout,'(A,I0,A)') 'Corps : ',ibdyty,'  -> wrong maps'
         call faterr(IAM,cout)
       ENDIF
     ENDDO

    !print*,'p2g:'
    !print*,bdyty(ibdyty)%p2g        
    !print*,'====='

    !fd 1/ assemblage ...

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

        call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%mass)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%damping,HT)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%stiffness,HT2)

     ENDDO 

     write(cout,'(A,I0,A)') 'Corps : ',ibdyty,'  -> assemblage ok'
     call logmes(cout)

     !fd 2/ cdl ...

     if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then

       ivd_=0 
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

         if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

         ivd_=ivd_+1 
         
         CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

         bdyty(ibdyty)%drvdofs(ivd_)=bdyty(ibdyty)%ccdof(inod)+idof 

       enddo

       call set_drvdofs(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvdofs)

     else
  
       call erase_drvdofs(bdyty(ibdyty)%g_sys)

     endif 

     write(cout,'(A,I0,A)') 'Corps : ',ibdyty,'  -> cdl ok'
     call logmes(cout)

     !fd 3/ precon

     !fd la factorisation est faite a la premiere resolution => un test a chaque resolution !!
     !fd il faut separer les 2 etapes.

     if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then

       bdyty(ibdyty)%drvvalues = 0.d0

       ! on les passe au g_system
       call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

     endif

     DO jdof=1,bdyty(ibdyty)%nbdof_precon

       bdyty(ibdyty)%Iaux = 0.d0

       !fd on pose la perturbation

       gdof = bdyty(ibdyty)%p2g(jdof)
       bdyty(ibdyty)%Iaux(gdof) = 1.d0

       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)

       ! on resoud
       CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

       IF (info > 0) THEN
         PRINT*,ibdyty
         write(cout,'(A,I0,A)') 'Corps : ',ibdyty,'  -> No solution'
         CALL FATERR(IAM,cout)
       ENDIF


       !if (gdof==518) then
       !  print*,'Vaux precon' 
       !  print*,bdyty(ibdyty)%Vaux
       !endif

       !fd on recupere la colonne de W_precon

       DO idof=1, bdyty(ibdyty)%nbdof_precon
         gdof = bdyty(ibdyty)%p2g(idof)
         bdyty(ibdyty)%W_precon(idof,jdof) = bdyty(ibdyty)%Vaux(gdof)
       ENDDO

     ENDDO

     write(cout,'(A,I0,A)') 'Corps : ',ibdyty,'  -> construction ok'
     call logmes(cout)

     bdyty(ibdyty)%Vaux = 0.d0

   ENDDO
   !$OMP END DO
   !$OMP END PARALLEL

!!$   do ibdyty=1,nb_mecaMAILx
!!$     print*,"mecaMAILx:",ibdyty
!!$     if (bdyty(ibdyty)%is_precon) then
!!$       print*,bdyty(ibdyty)%nbdof_precon
!!$       do idof = 1, bdyty(ibdyty)%nbdof_precon
!!$         print*,bdyty(ibdyty)%W_precon(idof,idof)
!!$       enddo
!!$     else
!!$       print*,"pas concerne"
!!$     endif
!!$   end do 
!!$

 END SUBROUTINE compute_precon_W_mecaMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 !> instead of computing the W matrix it can be "pushed" from outside lmgc90
 !> this method build an empty matrix
 !> method used for the coupling with Code_Aster
 SUBROUTINE init_precon_W_mecaMAILx
   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,inodty,idof,jdof,gdof,ivd,iccdof,inod
   INTEGER :: nbd,nbn


   IF (nb_mecaMAILx == 0) RETURN

   DO ibdyty=1,SIZE(bdyty)

     IF (.NOT. bdyty(ibdyty)%is_precon) CYCLE

     PRINT*,'On initialise W_precon pour le corps ',ibdyty

     nbd = 0; nbn = 0
     DO inodty=1,bdyty(ibdyty)%nb_nodes

       IF (bdyty(ibdyty)%nodes_precon(inodty) /= 0 ) THEN
         nbn  = nbn + 1
         nbd  = nbd + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       ENDIF

     ENDDO

     bdyty(ibdyty)%nbdof_precon=nbd
     ALLOCATE(bdyty(ibdyty)%W_precon(nbd,nbd))
     ALLOCATE(bdyty(ibdyty)%Vaux_precon(nbd))
     ALLOCATE(bdyty(ibdyty)%p2g(nbd))
     ALLOCATE(bdyty(ibdyty)%g2p(bdyty(ibdyty)%nbdof))

     nbd = 0; nbn = 0
     bdyty(ibdyty)%g2p = 0
     DO inodty=1,bdyty(ibdyty)%nb_nodes
       IF (bdyty(ibdyty)%nodes_precon(inodty) /= 0 ) THEN
         nbn  = nbn + 1
         DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
           bdyty(ibdyty)%p2g(nbd+idof) = bdyty(ibdyty)%ccdof(inodty) + idof
           bdyty(ibdyty)%g2p(bdyty(ibdyty)%ccdof(inodty) + idof) = nbd + idof 
         ENDDO
         nbd  = nbd + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       ENDIF

     ENDDO
   ENDDO

 END SUBROUTINE init_precon_W_mecaMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 !> get the list of nodes that are preconditionned
 !> method used for the coupling with Code_Aster
 SUBROUTINE get_nodes_precon_mecaMAILx(ibdyty, nodes_precon)
 IMPLICIT NONE
   INTEGER(kind=4), INTENT(in) :: ibdyty
   INTEGER(kind=4), DIMENSION(:), POINTER :: nodes_precon
   !
   INTEGER(kind=4) :: nb_precon

   IF( ASSOCIATED(nodes_precon) ) NULLIFY(nodes_precon)
   IF( bdyty(ibdyty)%is_precon ) THEN
     nb_precon = bdyty(ibdyty)%nbdof_precon
     ALLOCATE(nodes_precon(nb_precon))
     nodes_precon(1:nb_precon) = bdyty(ibdyty)%p2g(1:nb_precon)
   END IF
 END SUBROUTINE

!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 !> instead of computing the W matrix it can be "pushed" from outside lmgc90
 !> this method fill the W matrix
 !> method used for the coupling with Code_Aster

 SUBROUTINE put_precon_W_mecaMAILx(ibdyty,inodty,jdof,vaux,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,inodty,jnodty,jdof,nbdof
   INTEGER ::  iccdof, jccdof
   INTEGER :: piccdof,pjccdof
   REAL(kind=8),DIMENSION(nbdof) :: vaux
   CHARACTER(len=27)  :: IAM='mecaMAILx::put_precon_W'

   IF (nb_mecaMAILx == 0) RETURN

   IF (.NOT. bdyty(ibdyty)%is_precon) THEN
    CALL logmes('skip body')
    RETURN
   ENDIF

   IF (bdyty(ibdyty)%nodes_precon(inodty) == 0 ) THEN
     CALL logmes('skip node')
     RETURN
   ENDIF

   IF (nbdof /= bdyty(ibdyty)%nbdof ) THEN
     CALL faterr(IAM,'nb_dof non concordant')
   ENDIF

!  print*,ibdyty,inodty,jdof,nbdof
!  print*,vaux

! on recalcule le num de ddl dans W_precon du noeud inodty 

   pjccdof=0
   DO jnodty=1,inodty-1
     IF (bdyty(ibdyty)%nodes_precon(jnodty) /=0) pjccdof  = pjccdof + nbdof_a_nodty(bdyty(ibdyty)%nodty(jnodty))
   ENDDO
   pjccdof = pjccdof + jdof

!   print*,pjccdof

!fd on recupere la colonne de W_precon

   DO piccdof=1, bdyty(ibdyty)%nbdof_precon
     iccdof = bdyty(ibdyty)%p2g(piccdof)
!     print*,piccdof,iccdof,Vaux(iccdof)
     bdyty(ibdyty)%W_precon(piccdof,pjccdof) = Vaux(iccdof)
   ENDDO

 END SUBROUTINE put_precon_W_mecaMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE compute_precon_vaux_bynode_mecaMAILx(ibdyty,list,storage)
   IMPLICIT NONE
   INTEGER :: ibdyty,  i, inodty, idof
   INTEGER :: storage
   INTEGER,DIMENSION(:) :: list
   !fd new
   INTEGER :: iccdof
                             !1234567890123456789012345678901234567
   character(len=37):: IAM = 'mecaMAILx::compute_precon_vaux_bynode'

   !fd reduction
   SELECT CASE(storage)
   CASE(iVaux_e_invM_t_Ireac) 
!     print*,"Reac"
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Ireac(bdyty(ibdyty)%p2g(idof))
     ENDDO
!     print*,bdyty(ibdyty)%Vaux_precon

   CASE(iVaux_e_invM_t_Iaux_)
     !print*,"Iaux"
     !print*, bdyty(ibdyty)%Iaux
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Iaux(bdyty(ibdyty)%p2g(idof))
     ENDDO
!     print*,bdyty(ibdyty)%Vaux_precon

   CASE default
     call faterr(IAM,'storage type not known')
   END SELECT 

!fd produit matrice vecteur + expansion

   !if (storage == iVaux_e_invM_t_Iaux_)  print*,'Vaux'

   !print*,'new'
   bdyty(ibdyty)%Vaux=0.D0
   DO i=1,SIZE(list) 
     inodty = list(i)
     iccdof = bdyty(ibdyty)%ccdof(inodty)

     !if (storage == iVaux_e_invM_t_Iaux_) print*,i,inodty,iccdof

     DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

       IF (bdyty(ibdyty)%g2p(iccdof+idof) == 0) THEN
         call faterr(IAM,'inconsistency in precon maps')
       ENDIF

       bdyty(ibdyty)%Vaux(iccdof+idof) = DOT_PRODUCT(bdyty(ibdyty)%W_precon(bdyty(ibdyty)%g2p(iccdof+idof),:), &
                                                     bdyty(ibdyty)%Vaux_precon(:))

       !if (storage == iVaux_e_invM_t_Iaux_) print*,'Vaux(',iccdof+idof,') = ',bdyty(ibdyty)%Vaux(iccdof+idof)

     ENDDO

   ENDDO

 END SUBROUTINE compute_precon_vaux_bynode_mecaMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 SUBROUTINE compute_precon_vaux_mecaMAILx(ibdyty,storage)
   IMPLICIT NONE
   INTEGER :: ibdyty,  inodty, idof
   INTEGER :: storage

!     print*,"mecaMAILx:",ibdyty
!     print*,bdyty(ibdyty)%nbdof_precon
!     do idof = 1, bdyty(ibdyty)%nbdof_precon
!         print*,bdyty(ibdyty)%W_precon(idof,idof)
!         print*,"pdof ",idof," =gdof ",bdyty(ibdyty)%p2g(idof)
!     enddo
!!$   


!fd reduction
   SELECT CASE(storage)
   CASE(iVaux_e_invM_t_Ireac) 
!     print*,"Ireac"
!     print*,bdyty(ibdyty)%Ireac
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Ireac(bdyty(ibdyty)%p2g(idof))
     ENDDO
   CASE(iVaux_e_invM_t_Iaux_)
!     print*,"Iaux"
!     print*,bdyty(ibdyty)%Iaux
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Iaux(bdyty(ibdyty)%p2g(idof))
     ENDDO
   CASE default
     call faterr('mecaMAILx::compute_precon_vaux','storage type not known')
   END SELECT 

!fd produit matrice vecteur
   bdyty(ibdyty)%Vaux_precon = MATMUL(bdyty(ibdyty)%W_precon,bdyty(ibdyty)%Vaux_precon)

!fd expansion
   bdyty(ibdyty)%Vaux=0.D0
   DO idof = 1, bdyty(ibdyty)%nbdof_precon
     bdyty(ibdyty)%Vaux(bdyty(ibdyty)%p2g(idof)) = bdyty(ibdyty)%Vaux_precon(idof) 
   ENDDO

   !if (storage == iVaux_e_invM_t_Ireac) then
   !  print*,ibdyty
   !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux
   !  print*,'==='
   !endif
 END SUBROUTINE compute_precon_vaux_mecaMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 SUBROUTINE set_coro_body_mecaMAILx (iM_bdyty)
   IMPLICIT NONE
   INTEGER :: iM_bdyty
   INTEGER ::   ibdyty
   !fd on vient declarer que ce corps est a rendre coro
   character(len=80) :: cout

   write(cout,*) 'Body ',iM_bdyty,' is coro'
   call logmes(cout)

   ibdyty=M2meca(iM_bdyty)%bdyty

   bdyty(ibdyty)%is_coro = .TRUE.
 
   !am & pta : coro :
   ! * calcul de la matrice de masse pour le rigide equivalent, de son inverse, 
   ! du repere principal d'inertie et de l'operateur R2D
   CALL settle_body_as_rigid(ibdyty)
   ! * calcul de l'operateur D2R a faire par la suite
   ! (appel a : build_body_as_rigid)

 END SUBROUTINE set_coro_body_mecaMAILx
!------------------------------------------------------------------------ 
!-PTA-------------------------------------------------------------------- 

  ! cette fonction n'est qu'un "getter"

  FUNCTION compute_kinetic_energy_mecaMAILx()
  IMPLICIT NONE

  REAL(kind=8) :: compute_kinetic_energy_mecaMAILx
  REAL(kind=8) :: Eg_pot,Eg_def,Eg_cin

!                           123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='mecaMAILX::compute_kinetic_energy'

  call compute_energy_mecaMAILx(Eg_cin,Eg_def,Eg_pot)

  compute_kinetic_energy_mecaMAILx = Eg_cin

 END FUNCTION compute_kinetic_energy_mecaMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 

! todo logmes -> faterr

SUBROUTINE compute_energy_mecaMAILx(Eg_cin,Eg_def,Eg_pot)
  IMPLICIT NONE

  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE :: dep_ele,V_ele,A_ele
  INTEGER :: ibdyty,iblmty,iM_bdyty,iM_blmty
  INTEGER :: idof,nbdof,inodty,iccdof,i,ivd,ifd,id

  REAL(kind=8) :: Eg_pot,Eg_def,Eg_cin
  REAL(kind=8) :: E_pot,E_def,E_cin

  !                         12345678901234567890123456
  CHARACTER(len=25) :: IAM='mecaMAILx::compute_energy'

  !
  ! calcul des energie corps par corps
  !

  Eg_pot=0.d0;Eg_def=0.d0;Eg_cin=0.d0

  IF (nb_mecaMAILx == 0) RETURN

  DO ibdyty=1,SIZE(bdyty)

    ! energie
    bdyty(ibdyty)%E_pot = 0.d0
    bdyty(ibdyty)%E_cin = 0.d0
    bdyty(ibdyty)%E_def = 0.d0

    if( .not. bdyty(ibdyty)%visible ) cycle

    iM_bdyty = bdyty2M_bdyty(ibdyty)

    !
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
      !
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      !! beurk il y a des trucs pas secure entre nodty et DIME !!

      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   

      ALLOCATE(dep_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating dep_ele')
      ENDIF

      ALLOCATE(V_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating V_ele')
      ENDIF

      ALLOCATE(A_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating A_ele')
      ENDIF

      id = 0
      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty) 
         dep_ele(id+1:id+nbdof)=bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
         V_ele(id+1:id+nbdof)  =bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
         if (nbdime ==2 ) then
           A_ele(id+1:id+nbdof)  = (/ grav1 , grav2 /) 
         else if (nbdime == 3) then
           A_ele(id+1:id+nbdof)  = (/ grav1 , grav2, grav3 /) 
         else
           A_ele = 0.d0 
         endif     

         id = id + nbdof
      END DO

      bdyty(ibdyty)%E_cin = bdyty(ibdyty)%E_cin + &
                            0.5d0*dot_product(V_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%mass,V_ele))

      bdyty(ibdyty)%E_pot = bdyty(ibdyty)%E_pot + &
                            dot_product(A_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%mass,dep_ele))

      CALL compute_elementary_energy(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     iM_bdyty,iM_blmty, &
                                     coor_ele,E_def)  

      bdyty(ibdyty)%E_def = bdyty(ibdyty)%E_def + E_def

      DEALLOCATE(coor_ele,dep_ele,v_ele,a_ele)

    ENDDO

    Eg_pot = Eg_pot + bdyty(ibdyty)%E_pot
    Eg_cin = Eg_cin + bdyty(ibdyty)%E_cin
    Eg_def = Eg_def + bdyty(ibdyty)%E_def
  
  ENDDO

 END SUBROUTINE compute_energy_mecaMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 !> computes total works (kinetic, deformation, potential, degree of freedom, contact) 
 subroutine compute_work_mecamailx(Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con)
  implicit none

  ! works
  REAL(kind=8) :: Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con
  ! power
  REAL(kind=8) ::  P_cin, P_def, P_pot, P_ddl, P_con
  ! previous power (necessary to integrate)
  REAL(kind=8) :: pP_cin,pP_def,pP_pot,pP_ddl,pP_con

  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE :: dep_ele,V_ele,A_ele,grav_ele
  INTEGER :: ibdyty,iblmty,iM_bdyty,iM_blmty
  INTEGER :: idof,nbdof,inodty,iccdof,i,ivd,ifd,id

  REAL(kind=8) :: tt,umtt


  !                         12345678901234567890123
  CHARACTER(len=23) :: IAM='mecaMAILx::compute_work'


  Wg_cin=0.d0; Wg_def=0.d0; Wg_pot=0.d0; Wg_ddl=0.d0; Wg_con=0.d0

  IF (nb_mecaMAILx == 0) RETURN

  DO ibdyty=1,SIZE(bdyty)

    ! puissance: on stocke l'ancienne valeur ...
    pP_pot = bdyty(ibdyty)%P_pot 
    pP_cin = bdyty(ibdyty)%P_cin
    pP_def = bdyty(ibdyty)%P_def
    pP_ddl = bdyty(ibdyty)%P_ddl
    pP_con = bdyty(ibdyty)%P_con

    ! ... et on initialise
    bdyty(ibdyty)%P_pot = 0.d0 
    bdyty(ibdyty)%P_cin = 0.d0 
    bdyty(ibdyty)%P_def = 0.d0
    bdyty(ibdyty)%P_ddl = 0.d0 
    bdyty(ibdyty)%P_con = 0.d0 

    if( .not. bdyty(ibdyty)%visible ) cycle

    iM_bdyty = bdyty2M_bdyty(ibdyty)

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      !! beurk il y a des trucs pas secure entre nodty et DIME !!

      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   

      ALLOCATE(dep_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating dep_ele')
      ENDIF

      ALLOCATE(V_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating V_ele')
      ENDIF

      ALLOCATE(A_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating A_ele')
      ENDIF

      ALLOCATE(grav_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating grav_ele')
      ENDIF

      id = 0
      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty) 
         dep_ele(id+1:id+nbdof)=bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
         V_ele(id+1:id+nbdof)  =bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
         A_ele(id+1:id+nbdof)  =(bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) - bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof))/H
         if (nbdime ==2 ) then
           grav_ele(id+1:id+nbdof)  = (/ grav1 , grav2 /) 
         else if (nbdime == 3) then
           grav_ele(id+1:id+nbdof)  = (/ grav1 , grav2, grav3 /) 
         else
           grav_ele = 0.d0 
         endif     

         id = id + nbdof
      END DO


      bdyty(ibdyty)%P_cin = bdyty(ibdyty)%P_cin + &
                            dot_product(V_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%mass,A_ele))

      bdyty(ibdyty)%P_pot = bdyty(ibdyty)%P_pot + &
                            dot_product(V_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%mass,grav_ele))


!!$      CALL compute_elementary_power(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                    bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
!!$                                    bdyty(ibdyty)%blmty(iblmty)%lawnb, &
!!$                                    iM_bdyty,iM_blmty, &
!!$                                    coor_ele,V_ele,P_def)       
!!$      bdyty(ibdyty)%P_def = bdyty(ibdyty)%P_def + P_def


      DEALLOCATE(coor_ele,dep_ele,v_ele,a_ele,grav_ele)

    ENDDO


    bdyty(ibdyty)%P_def = bdyty(ibdyty)%P_def + &
                          dot_product(RESHAPE(source=bdyty(ibdyty)%V,shape=(/ SIZE(bdyty(ibdyty)%V) /)), &
                                      RESHAPE(source=bdyty(ibdyty)%Fint,shape=(/ SIZE(bdyty(ibdyty)%Fint) /)))


    !print*,ibdyty,bdyty(ibdyty)%P_pot,bdyty(ibdyty)%P_cin,bdyty(ibdyty)%P_def

    !! conditions aux limites
    DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

      if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle
       
      CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inodty,idof)

      iccdof=bdyty(ibdyty)%ccdof(inodty)+idof 

      !fd on ne met pas Reac car normalement il n'y a pas de contact sur un ddl impose
      ! c'est un - car Finert = - Ma et Fint = - Int sigm : eps
      bdyty(ibdyty)%P_ddl = bdyty(ibdyty)%P_ddl - &
                            bdyty(ibdyty)%Vdriv(ivd)*(bdyty(ibdyty)%Finert(iccdof)+bdyty(ibdyty)%Fint(iccdof))

    ENDDO

    !print*,ibdyty,bdyty(ibdyty)%P_ddl

    DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof

      CALL owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inodty,idof)

      iccdof=bdyty(ibdyty)%ccdof(inodty)+idof   

      bdyty(ibdyty)%P_ddl = bdyty(ibdyty)%P_ddl + &
                            bdyty(ibdyty)%Fdriv(ifd)*bdyty(ibdyty)%V(iccdof) 
    END DO

    !print*,ibdyty,bdyty(ibdyty)%P_ddl

    !! contact

    bdyty(ibdyty)%P_con = bdyty(ibdyty)%P_con - &
                          DOT_PRODUCT(bdyty(ibdyty)%V,bdyty(ibdyty)%Ireac/H)


    !print*,ibdyty,bdyty(ibdyty)%P_con


    ! integration sur un pas de temps

    tt = 0.5 !theta
    umtt = 1.d0 - tt

    !if (dabs( bdyty(ibdyty)%P_pot ) > 1e-10) then 
        bdyty(ibdyty)%W_pot = bdyty(ibdyty)%W_pot + H*(tt*bdyty(ibdyty)%P_pot + umtt*pP_pot)
    !endif

    !if (dabs( bdyty(ibdyty)%P_cin ) > 1e-10) then 
        bdyty(ibdyty)%W_cin = bdyty(ibdyty)%W_cin + H*(tt*bdyty(ibdyty)%P_cin + umtt*pP_cin)
    !endif

    !if (dabs( bdyty(ibdyty)%P_def ) > 1e-10) then 
        bdyty(ibdyty)%W_def = bdyty(ibdyty)%W_def + H*(tt*bdyty(ibdyty)%P_def + umtt*pP_def)
    !endif

    !if (dabs( bdyty(ibdyty)%P_con ) > 1e-10) then 
        bdyty(ibdyty)%W_con =  bdyty(ibdyty)%W_con + H*(tt*bdyty(ibdyty)%P_con + umtt*pP_con)
    !endif

    !if (dabs( bdyty(ibdyty)%P_ddl ) > 1e-10) then 
        bdyty(ibdyty)%W_ddl = bdyty(ibdyty)%W_ddl + H*(tt*bdyty(ibdyty)%P_ddl + umtt*pP_ddl)
    !endif

    Wg_pot = Wg_pot + bdyty(ibdyty)%W_pot
    Wg_cin = Wg_cin + bdyty(ibdyty)%W_cin
    Wg_def = Wg_def + bdyty(ibdyty)%W_def
    Wg_ddl = Wg_ddl + bdyty(ibdyty)%W_con
    Wg_con = Wg_con + bdyty(ibdyty)%W_ddl

  ENDDO

 end subroutine 

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------

 SUBROUTINE compute_numerical_work_mecaMAILx(WFddl_cumul,WFdef_cumul,WFext_cumul,WFcon_cumul,Wdiss_cumul)
  IMPLICIT NONE

  INTEGER           :: ibdyty, ivd, ifd, inodty, idof, iccdof,iblmty
  ! Travaux cumules des differentes forces
  REAL(kind=8)      :: WFddl_cumul, WFdef_cumul, WFext_cumul, WFcon_cumul, Wdiss_cumul 
 ! Travaux des differentes forces
  REAL(kind=8)      :: WFddl, WFcon, WFdef, WFext, Wdiss               
 
                           !123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='mecaMAILx::compute_numerical_work'

  WFddl_cumul=0.d0 ; WFdef_cumul=0.d0 ; WFext_cumul=0.d0 ; WFcon_cumul=0.d0 ; Wdiss_cumul=0.d0  

  IF (nb_mecaMAILx == 0) RETURN

  !Parcours des corps
  DO ibdyty=1,SIZE(bdyty)
  
   WFddl=0.d0 ; WFdef=0.d0 ; WFext=0.d0 ; WFcon=0.d0 ; Wdiss=0.d0 
   if( .not. bdyty(ibdyty)%visible ) cycle

    !IF (bdyty(ibdyty)%halo_rank > 1) cycle
    
    !On utilise Vaux pour stocker le déplacement necessaire au calcul du travail des forces
    !On modifie le vecteur du G system pour utiliser multiply_system pour le travail des ddl imposés

    select case( M_INTEGRATOR_ID ) 
    case( INTEGRATOR_BETA2 )

       !Ceci est egal à (V+Vbegin)/2
       bdyty(ibdyty)%Vaux = ( bdyty(ibdyty)%X - bdyty(ibdyty)%Xprev ) / ( H + H )
       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%V)

    case default

       bdyty(ibdyty)%Vaux = 0.5d0 * (bdyty(ibdyty)%Vbegin+bdyty(ibdyty)%V)
       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%V - bdyty(ibdyty)%Vlast)

    end select
       
    !Parcours des degres imposes en vitesse
    call multiply_system(bdyty(ibdyty)%g_sys)
    call get_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux )
    DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
      if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle       
      CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inodty,idof)
      iccdof=bdyty(ibdyty)%ccdof(inodty)+idof
      WFddl = WFddl + ( bdyty(ibdyty)%Iaux(iccdof) - bdyty(ibdyty)%RHS(iccdof) - bdyty(ibdyty)%Ireac(iccdof) ) &
              * bdyty(ibdyty)%Vaux(iccdof)
    end do
    
    !Parcours des degres imposes en force
    DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
      CALL owner_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),inodty,idof)
      iccdof=bdyty(ibdyty)%ccdof(inodty)+idof 
      WFddl = WFddl + bdyty(ibdyty)%Fdriv(ifd) * bdyty(ibdyty)%Vaux(iccdof)
    END DO      
   
    !Travail des forces interieures
    WFdef = WFdef + DOT_PRODUCT(bdyty(ibdyty)%Vaux, bdyty(ibdyty)%Fint)*H
    
    !Travail des forces exterieures
    WFext = WFext + DOT_PRODUCT(bdyty(ibdyty)%Vaux, bdyty(ibdyty)%Fext)*H
    
    !Travail des forces de contact
    WFcon = WFcon + DOT_PRODUCT(bdyty(ibdyty)%Vaux, bdyty(ibdyty)%Ireac)
    
    !Travail dissipe numeriquement par le schema
    select case( M_INTEGRATOR_ID ) 
    case( INTEGRATOR_BETA2 )
       !Rien a faire le schema est conservatif
    case default
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,-bdyty(ibdyty)%blmty(iblmty)%mass)
       ENDDO   
       call set_vector(bdyty(ibdyty)%g_sys, &
                          ( bdyty(ibdyty)%V-bdyty(ibdyty)%Vlast ) )
       call multiply_system(bdyty(ibdyty)%g_sys)
       call get_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux )
       Wdiss = Wdiss - DOT_PRODUCT(bdyty(ibdyty)%Vaux, bdyty(ibdyty)%Iaux)
    end select

    !On calcule le cumul
    WFddl_cumul = WFddl_cumul + WFddl
    WFdef_cumul = WFdef_cumul + WFdef
    WFext_cumul = WFext_cumul + WFext
    WFcon_cumul = WFcon_cumul + WFcon
    Wdiss_cumul = Wdiss_cumul + Wdiss

  end do

END SUBROUTINE compute_numerical_work_mecaMAILx

!------------------------------------------------------------------------
!------------------------------------------------------------------------ 

 SUBROUTINE put_vector_mecaMAILx(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect
   !
   integer(kind=4) :: iM_bdyty

                            !123456789012345678901
   CHARACTER(len=21) :: IAM='mecaMAILx::put_vector'

   IF (nb_mecaMAILx == 0) RETURN

   IF (nbdof /= bdyty(ibdyty)%nbdof ) THEN
     print*,id_vect,nbdof,bdyty(ibdyty)%nbdof
     call FATERR(IAM,'nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
    CASE('X____')
     bdyty(ibdyty)%X=vect
    CASE('Xbeg_')
     bdyty(ibdyty)%Xbegin=vect
    CASE('V____')
      if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
        bdyty(ibdyty)%V=H*vect    
      else     
        bdyty(ibdyty)%V=vect
      endif   
    CASE('Vbeg_')
      if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
        bdyty(ibdyty)%Vbegin=H*vect
      else 
        bdyty(ibdyty)%Vbegin=vect
      endif   
    CASE('Vfree')
      if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
         bdyty(ibdyty)%Vfree=H*vect
      else 
         bdyty(ibdyty)%Vfree=vect
      endif   
    CASE('Reac_')
     bdyty(ibdyty)%Ireac=vect*H
    CASE('Raux_')
     bdyty(ibdyty)%Iaux=vect*H
    CASE('Ireac')
     bdyty(ibdyty)%Ireac=vect
    CASE('Iaux_')
     bdyty(ibdyty)%Iaux=vect
    CASE('Fext_')
     bdyty(ibdyty)%Fext=bdyty(ibdyty)%Fext+vect
    CASE('Fint_')
     bdyty(ibdyty)%Fint=vect
    CASE('Coor_')
     CALL put_coor(ibdyty, vect, nbdof)
    CASE('Coorb')
     CALL put_coor_begin(ibdyty, vect, nbdof)
    CASE('Coor0')
     iM_bdyty=bdyty2M_bdyty(ibdyty)
     CALL set_cooref_nodes_MAILx(iM_bdyty, vect)

    CASE DEFAULT
     call faterr(IAM,'unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE put_vector_mecaMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE get_vector_mecaMAILx(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   ! variables d'entree
   INTEGER, INTENT(in) :: ibdyty ! numero de corps
   INTEGER, INTENT(in) :: nbdof ! nombre total de ddl pour le corps
   CHARACTER(len=5) :: id_vect ! identifiant du champ a recuperer

   ! variable de sortie
   REAL(kind=8), DIMENSION(nbdof), INTENT(out) :: vect ! vecteur
      ! concatenant les valeurs aux noeud de chaque champ (vectoriel)  

   ! variables locales
   INTEGER :: inodty 
   INTEGER :: iccdof 
   ! numero du corps MAILx corespondant au modele courant
   INTEGER :: iM_bdyty 
   INTEGER :: iM_nodty 
   INTEGER :: iM_ccdof 

                            !123456789012345678901
   CHARACTER(len=21) :: IAM='mecaMAILx::get_vector'

   

   IF (nb_mecaMAILx == 0) RETURN

   IF (nbdof /= bdyty(ibdyty)%nbdof ) THEN
     print*,id_vect,nbdof,bdyty(ibdyty)%nbdof      
     call FATERR(IAM,'nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
   CASE('Coor0')
       ! on recupere le numero du corps MAILx corespondant au modele courant
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       ! pour chaque noeud
       DO inodty=1, bdyty(ibdyty)%nb_nodes
          ! on recupere l'indice qui debute la tranche concerant le noeud 
          ! courant dans un vecteur asssemble 
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          ! on recupere le numero du noeud courant dans la numerotation du coprs MAILx
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          ! on recupere l'indice qui debute la tranche concerant le noeud 
          ! courant dans un vecteur asssemble, pour le corps MAILx
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)

          ! on peut alors stocker les coordonnees de reference du noeud courant
          ! dans le vecteur resultat
          vect(iccdof+1:iccdof+nbDIME)=M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME)
       END DO
   CASE('Coorb')
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       DO inodty=1, bdyty(ibdyty)%nb_nodes
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)

          vect(iccdof+1:iccdof+nbDIME) = M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) &
                                       + bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbDIME)
       END DO
   CASE('Coor_')
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       DO inodty=1, bdyty(ibdyty)%nb_nodes
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)

          !vect(iccdof+1:iccdof+nbDIME) = M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) &
          !                             + bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME)
          vect(iccdof+1:iccdof+nbDIME) = M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME)
                                       
       END DO
   CASE('X____')
     vect=bdyty(ibdyty)%X
   CASE('Xbeg_')
     vect=bdyty(ibdyty)%Xbegin
   CASE('V____')
     if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
        vect=bdyty(ibdyty)%V/H          
     else
        vect=bdyty(ibdyty)%V
     endif   
   CASE('Vbeg_')
     if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
        vect=bdyty(ibdyty)%Vbegin/H          
     else
        vect=bdyty(ibdyty)%Vbegin
     endif   
  CASE('Vaux_')
      if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
        vect=bdyty(ibdyty)%Vaux/H          
     else
        vect=bdyty(ibdyty)%Vaux
     endif   
   CASE('Vfree')
      if (M_INTEGRATOR_ID == INTEGRATOR_QS) then
        vect=bdyty(ibdyty)%Vfree/H          
     else
        vect=bdyty(ibdyty)%Vfree
     endif   
   CASE('Reac_')
     vect=bdyty(ibdyty)%Ireac/H
   CASE('Raux_')
     vect=bdyty(ibdyty)%Iaux/H
   CASE('Ireac')
     vect=bdyty(ibdyty)%Ireac
   CASE('Iaux_')
     vect=bdyty(ibdyty)%Iaux
   CASE('Fext_')
     vect=bdyty(ibdyty)%Fext
   CASE('Fint_')
     vect=bdyty(ibdyty)%Fint
   CASE DEFAULT
     call faterr(IAM,'Sorry unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE get_vector_mecaMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 SUBROUTINE get_ptr_body_vector_mecaMAILx(id_field,ibdyty,ptr)
 IMPLICIT NONE

   ! variables d'entree
   INTEGER, INTENT(in) :: ibdyty ! numero de corps
   INTEGER, INTENT(in) :: id_field ! identifiant du champ a recuperer

   ! variable de sortie 
   REAL(kind=8), DIMENSION(:), pointer :: ptr ! vecteur
      ! concatenant les valeurs aux noeud de chaque champ (vectoriel)  

                            !123456789012345678901
   CHARACTER(len=18) :: IAM='mecaMAILx::get_ptr' 

   IF (nb_mecaMAILx == 0) RETURN
   
   SELECT CASE(id_field)
   CASE(iX____)
     ptr=>bdyty(ibdyty)%X
   CASE(iXbeg_)
     ptr=>bdyty(ibdyty)%Xbegin
   CASE(iV____)
     ptr=>bdyty(ibdyty)%V
   CASE(iVbeg_)
     ptr=>bdyty(ibdyty)%Vbegin
   CASE(iVaux_)
     ptr=>bdyty(ibdyty)%Vaux
   CASE(iVfree)
     ptr=>bdyty(ibdyty)%Vfree
   CASE(iVddm_)
     ptr=>bdyty(ibdyty)%Vddm  
   CASE(iIreac)
     ptr=>bdyty(ibdyty)%Ireac
   CASE(iIaux_)
     ptr=>bdyty(ibdyty)%Iaux
   CASE(iFext_)
     ptr=>bdyty(ibdyty)%Fext
   CASE(iFint_)
     ptr=>bdyty(ibdyty)%Fint
   CASE DEFAULT
     call faterr(IAM,'Sorry unknown id ')
   END SELECT

 END SUBROUTINE get_ptr_body_vector_mecaMAILx

 function get_ptr_drvdofs_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   integer(kind=4), dimension(:), pointer :: get_ptr_drvdofs_mecaMAILx
   !
   integer(kind=4) :: ivd, inod, idof

   if( ibdyty<1 .or. ibdyty>nb_mecaMAILx ) then
     call faterr('mecaMAILx::get_ptr_drvdofs','Unknown body')
   endif

   if( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0 ) then

     do ivd = 1, bdyty(ibdyty)%nb_vlocy_driven_dof
       if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle    
       call owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
       bdyty(ibdyty)%drvdofs(ivd)=bdyty(ibdyty)%ccdof(inod)+idof
     end do

   endif

   get_ptr_drvdofs_mecaMAILx => bdyty(ibdyty)%drvdofs

 end function get_ptr_drvdofs_mecaMAILx

 function get_ptr_nodnb_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   integer(kind=4), dimension(:), pointer :: get_ptr_nodnb_mecaMAILx

   if( ibdyty<1 .or. ibdyty>nb_mecaMAILx ) then
     call faterr('mecaMAILx::get_ptr_drvdofs','Unknown body')
   endif

   get_ptr_nodnb_mecaMAILx => bdyty(ibdyty)%nodnb

 end function get_ptr_nodnb_mecaMAILx

!------------------------------------------------------------------------
 SUBROUTINE get_dofstatus_mecaMAILx(ibdyty,vect,nb_nodes)
 IMPLICIT NONE

   ! variables d'entree
   INTEGER, INTENT(in) :: ibdyty  ! numero de corps
   INTEGER, INTENT(in) :: nb_nodes! nombre total de noeuds pour le corps
   ! variable de sortie
   ! vecteur contenant le status du noeud
   REAL(kind=8), DIMENSION(nb_nodes), INTENT(out) :: vect 
      
   integer(kind=4)   :: ivd,iccdof,idof,inod,ifd
   
                            !123456789012345678901234
   CHARACTER(len=24) :: IAM='mecaMAILx::get_dofstatus'

   IF (nb_mecaMAILx == 0) RETURN

   IF (nb_nodes /= bdyty(ibdyty)%nb_nodes ) THEN
     call FATERR(IAM,'nb_nodes non concordant')
   ENDIF

   vect(:) = bdyty(ibdyty)%drvstatus(:)

 END SUBROUTINE    
   
 !------------------------------------------------------------------------  
 SUBROUTINE set_Matrix_Storage_mecaMAILx(type)
 IMPLICIT NONE
 character(len=8) :: type
 character(len=29) :: IAM

     !12345678901234567890123456789
 IAM='mecaMAILx::set_matrix_storage'

 Matrix_storage=get_matrix_storage_id_from_name(type)

 if (Matrix_storage == -99) call faterr(IAM,'unknown storage type '//type)

 END SUBROUTINE set_Matrix_Storage_mecaMAILx
!------------------------------------------------------------------------  
 SUBROUTINE set_Matrix_Shape_mecaMAILx(type)
 IMPLICIT NONE
 character(len=8) :: type
 character(len=27) :: IAM

     !123456789012345678901234567
 IAM='mecaMAILx::set_matrix_shape'

 Matrix_shape=get_matrix_shape_id_from_name(type)

 if (Matrix_storage == -99) call faterr(IAM,'unknown shape type '//type)

 END SUBROUTINE set_Matrix_Shape_mecaMAILx

 !------------------------------------------------------------------------  

 SUBROUTINE set_without_renum_mecaMAILx
 IMPLICIT NONE

   with_renum=.FALSE.

 END SUBROUTINE set_without_renum_mecaMAILx

!------------------------------------------------------------------------ 

 SUBROUTINE set_field_bynode(ibdyty,field_rank,fsize,field)
   IMPLICIT NONE

!!   character(len=30),intent(in)             :: name  ! en attendant de savoir retrouver le rang de facon efficace
   INTEGER,INTENT(in)                       :: ibdyty,fsize,field_rank
   REAL(kind=8),INTENT(in),DIMENSION(fsize) :: field

   INTEGER :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare

   REAL(kind=8),ALLOCATABLE,DIMENSION(:) :: efield_gp,efield_node

                            !1234567890123456
   CHARACTER(len=16) :: IAM='set_field_bynode'

   IF (nb_mecaMAILx == 0) RETURN

   IF (fsize /= bdyty(ibdyty)%nb_nodes) THEN
     CALL FATERR(IAM,'non conforming vector fsize')
   ENDIF

   !fd interpolation de field au noeud a field au pg

   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(efield_node(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating efield_node')
      END IF

      DO in=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
        efield_node(in) = field(bdyty(ibdyty)%blmty(iblmty)%NODES(in))
      ENDDO

      imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb
      ALLOCATE(efield_gp(get_N_GP_mecaEF(modelz(imodel)%ID)),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating efield_gp')
      END IF

      !
      ! on passe des noeuds aux pg
      ! 

      CALL interpolate_node2gp(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                               efield_node,efield_gp)       

      !fd on pose dans la bd mailx

      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

      DO ig=1,get_N_GP_mecaEF(modelz(imodel)%ID)
        CALL set_meca_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(ig))
      ENDDO 
      DEALLOCATE(efield_node,efield_gp)
   ENDDO

 END SUBROUTINE set_field_bynode

 ! Set value of an external field on an element (same value for all Gauss points)
 subroutine set_field_byelem(ibdyty,field_rank,fsize,field)
   implicit none
   !> body on which to set field value
   integer(kind=4), intent(in) :: ibdyty
   !> rank of the field ot set
   integer(kind=4), intent(in) :: field_rank
   !> size of input field array (must be nb_elem)
   integer(kind=4), intent(in) :: fsize
   !> field values per element
   real(kind=8), intent(in), dimension(fsize) :: field
   !
   integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel
   character(len=16) :: IAM
        !1234567890123456
   IAM ='set_field_byelem'

   if (nb_mecaMAILx == 0) return

   if (fsize /= size(bdyty(ibdyty)%blmty)) then
     call FATERR(IAM,'non conforming vector fsize')
   end if

   do iblmty = 1, size(bdyty(ibdyty)%blmty)

     iM_bdyty = bdyty2M_bdyty(ibdyty)
     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
     imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

     do ig = 1, get_N_GP_mecaEF(modelz(imodel)%ID)
       call set_meca_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,field(iblmty))
     end do 
   end do

 end subroutine set_field_byelem

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
! cette routine parcourt tous les corps, tous les elements 
! et interroge une routine user que voit l element pour recuperer la valeur du 
! field demande
!
 subroutine set_field_byuser(ibdyty,field_rank)
   implicit none
   integer(kind=4), intent(in) :: ibdyty, field_rank 
   !
   integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare
   REAL(kind=8),ALLOCATABLE :: coor_ele(:,:),field_ele(:)
                            !123456789012345
   CHARACTER(len=15) :: IAM='set_field_byuser'
   CHARACTER(len=30) :: name, cout

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

!todo corriger cette merde on passe un blmnb et un ppsnb juste pour arriver a user
!     cet appel lance de l'interieur une actualisation du field dans l'outil client   

   call compute_elementary_field(bdyty(ibdyty)%blmty(1)%blmnb, &
                                 bdyty(ibdyty)%blmty(1)%ppsnb, &
                                 TPS,H)       

   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

      ALLOCATE(field_ele(get_N_GP_mecaEF(modelz(imodel)%ID)),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating field_ele')
      END IF


!todo immerger la gestion des fields dans l'ele ce qui evitera de devoir
!     traverser la structure de donnees pour poser une bouse dans mailx 

      !fd on pose dans la bd mailx

      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

!todo corriger cette merde dans un element tout le monde a le meme nb de field

      CALL get_meca_field_name_MAILx(iM_bdyty,iM_blmty,1,field_rank,name)


      CALL get_elementary_field(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                name,                              &
                                TPS,H,                             &
                                coor_ele,field_ele)       


      DO ig=1,get_N_GP_mecaEF(modelz(imodel)%ID)
        CALL set_meca_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,field_ele(ig))
      ENDDO 

      DEALLOCATE(coor_ele,field_ele)
   ENDDO

end subroutine set_field_byuser
!------------------------------------------------------------------------ 
INTEGER FUNCTION get_field_rank(ibdyty,iblmty,name)
  IMPLICIT NONE
  INTEGER :: ibdyty,iblmty
  INTEGER :: iM_bdyty,iM_blmty
  CHARACTER(len=*) :: name

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_field_rank = get_meca_field_rank_MAILx(iM_bdyty,iM_blmty,name)

END FUNCTION
!------------------------------------------------------------------------ 
!> \brief Set a vector field by nodes
!> Values at nodes are used to interpolate and store
!> corresponding value at each Gauss point
subroutine set_vfield_bynode(ibdyty,field_rank,vfield,dim1,dim2)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> vector field index
  integer(kind=4), intent(in) :: field_rank
  !> first dimension of input field array (vector field size)
  integer(kind=4), intent(in) :: dim1
  !> second dimension of input field array (number of nodes)
  integer(kind=4), intent(in) :: dim2
  !> input field value
  real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare,i_f
  real(kind=8), dimension(:,:), allocatable :: efield_gp,efield_node

  character(len=17) :: IAM
        !12345678901234567
  IAM = 'set_vfield_bynode'

  if (nb_mecaMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_mecaMAILx ) call faterr(IAM,'wrong mecaMAILx index')

  if (dim2/= bdyty(ibdyty)%nb_nodes) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  !fd interpolation de field au noeud a field au pg
  do iblmty = 1, size(bdyty(ibdyty)%blmty)

     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

     ! here ?
     if (dim1 > get_meca_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
       call faterr(IAM,'input vector field too large')
     end if

     allocate(efield_node(size(bdyty(ibdyty)%blmty(iblmty)%NODES),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_node')
     end if

     do in = 1, size(bdyty(ibdyty)%blmty(iblmty)%NODES)
       efield_node(in,:) = vfield(:,bdyty(ibdyty)%blmty(iblmty)%NODES(in))
     end do

     imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
     allocate(efield_gp(get_N_GP_mecaEF(modelz(imodel)%ID),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_gp')
     end if

     !
     ! on passe des noeuds aux pg
     ! 
     do i_f = 1, dim1
       call interpolate_node2gp(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                efield_node(:,i_f),efield_gp(:,i_f))
     end do

     !fd on pose dans la bd mailx

     do ig = 1,get_N_GP_mecaEF(modelz(imodel)%ID)
       call set_meca_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(ig,:),dim1)
     end do

     deallocate(efield_node,efield_gp)
  end do

end subroutine set_vfield_bynode
!------------------------------------------------------------------------
!> \brief Set a vector field by at constant value on the element
subroutine set_vfield_byelem(ibdyty,field_rank,vfield,dim1,dim2)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> vector field index
  integer(kind=4), intent(in) :: field_rank
  !> first dimension of input field array (vector field size)
  integer(kind=4), intent(in) :: dim1
  !> second dimension of input field array (number of elements)
  integer(kind=4), intent(in) :: dim2
  !> input field value
  real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel
  character(len=17) :: IAM
       !12345678901234567
  IAM ='set_vfield_byelem'

  if (nb_mecaMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_mecaMAILx ) call faterr(IAM,'wrong mecaMAILx index')

  if (dim2/= size(bdyty(ibdyty)%blmty)) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    ! here ?
    if (dim1 > get_meca_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
      call faterr(IAM,'input vector field too large')
    end if

    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_mecaEF(modelz(imodel)%ID)
      call set_meca_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,vfield(:,iblmty),dim1)
    end do 
  end do

end subroutine set_vfield_byelem

!------------------------------------------------------------------------
!> Get rank of an external vector field of an element
integer(kind=4) function get_vfield_rank(ibdyty,iblmty,name)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> element index
  integer(kind=4), intent(in) :: iblmty
  !> name of field to get rank of
  character(len=*), intent(in) :: name
  !
  integer(kind=4) :: iM_bdyty,iM_blmty

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_vfield_rank = get_meca_vfield_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
INTEGER FUNCTION get_nb_nodes_mecaMAILx(ibdyty)
  IMPLICIT NONE
  INTEGER :: ibdyty

  get_nb_nodes_mecaMAILx = bdyty(ibdyty)%nb_nodes

END FUNCTION
!------------------------------------------------------------------------ 
INTEGER FUNCTION get_nb_elements_mecaMAILx(ibdyty)
  IMPLICIT NONE
  INTEGER :: ibdyty

  get_nb_elements_mecaMAILx = size(bdyty(ibdyty)%blmty) 

END FUNCTION

!------------------------------------------------------------------------ 
SUBROUTINE terminate_mecaMAILx
  IMPLICIT NONE

  IF (is_externalFEM) THEN
    CALL ExternalFEM_terminate
    RETURN
  ENDIF
END SUBROUTINE

!------------------------------------------------------------------------ 

SUBROUTINE DISPLAY_bulk_element_mecaMAILx(ibdyty,iblmty)
  IMPLICIT NONE
  INTEGER :: ibdyty,iblmty,idof
  
  PRINT *,'Body ',ibdyty,' bulk ',iblmty
  PRINT *,'stiffness :'
  DO idof=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%stiffness,dim=2)
    PRINT '(24(1x,D8.1))', bdyty(ibdyty)%blmty(iblmty)%stiffness(:,idof)
  ENDDO
  PRINT *,'mass :'
  DO idof=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%mass,dim=2)
     PRINT '(24(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%mass(:,idof)
  ENDDO
  PRINT *,'ttfint :'
  PRINT '(1(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%ttfint(:)
  PRINT *,'fext :'
  PRINT '(1(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%fext(:,1)
  PRINT *,'fint :'
  PRINT '(1(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%fint(:,1)

  PRINT*,'===='
END SUBROUTINE

!!!------------------------------------------------------------------------  
!!!------------------------------------------------------------------------  

  SUBROUTINE use_new_ppset_mecaMAILx

    IMPLICIT NONE
    use_existing_ppset = .FALSE.

  END SUBROUTINE use_new_ppset_mecaMAILx

!!!------------------------------------------------------------------------  
!!!------------------------------------------------------------------------  

  !> routine qui va fixer le repere d'orthotropie aux points 
  !> de Gauss en appelant une routine utilisateur

  subroutine SET_ORTHO_FRAME_byuser(ibdyty)
    IMPLICIT NONE 
    integer(kind=4), intent(in) :: ibdyty
    !
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    INTEGER :: iblmty
    INTEGER :: iM_bdyty,iM_blmty
    INTEGER :: i,inodty,iccdof
    INTEGER :: imodel,errare

    !                         123456789012345678901235678901234
    CHARACTER(len=34) :: IAM='mecaMAILx::set_ortho_frame_byuser'
    character(len=80) :: cout

    if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(ibdyty)%visible ) return

    !
    ! calcul de la contrainte et mise a jour des grandeurs internes
    !
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      ! appel a la routine de User pour calculer le repere
      CALL compute_elementary_ortho_frame(bdyty(ibdyty)%blmty(iblmty)%blmnb, & ! le type d'element
                                          bdyty(ibdyty)%blmty(iblmty)%ppsnb, & ! le jeux de ppset
                                          iM_bdyty, iM_blmty               , &
                                          coor_ele)                             ! les coordonnees

      DEALLOCATE(coor_ele)

    ENDDO

  end subroutine    


  !> routine qui va fixer le repere d'orthotropie aux points 
  !> de Gauss en appelant une routine utilisateur

  subroutine SET_ORTHO_FRAME_bygp(ibdyty)
    implicit none 
    integer(kind=4), intent(in) :: ibdyty
    !
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    INTEGER :: iblmty
    INTEGER :: iM_bdyty,iM_blmty
    INTEGER :: i,inodty,iccdof
    INTEGER :: imodel,errare

    REAL(kind=8) ,DIMENSION(:,:) ,ALLOCATABLE :: coor_pg
    REAL(kind=8) ,DIMENSION(:,:,:) ,ALLOCATABLE :: frame

    !                         12345678901234567890123567
    CHARACTER(len=27) :: IAM='mecaMAILx::set_ortho_frame'
    character(len=80) :: cout

    if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(ibdyty)%visible ) return

    !
    ! calcul de la contrainte et mise a jour des grandeurs internes
    !
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

      ALLOCATE(coor_pg(nbDIME,get_N_GP_mecaEF(modelz(imodel)%ID)),stat=errare)
      coor_pg = 0.d0

      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      CALL get_ele_gp_coor(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                           bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                           iM_bdyty,iM_blmty,coor_ele, &
                           coor_pg)

      !
      ALLOCATE(frame(nbdime,nbdime,SIZE(coor_pg,dim=2)))

      
      ! appel a la routine de User pour calculer le repere
      ! a reprendre call gp_ortho_frame(nbdime,get_N_GP_mecaEF(modelz(imodel)%ID),coor_pg,frame)


      !call set_ele_pg_ortho_frame(frame)

      DEALLOCATE(coor_ele,coor_pg,frame)

    ENDDO

  end subroutine    

  !------------------------------------------------------------------------

  FUNCTION get_dep_R_TT_nodty(ibdyty,inodty)
    IMPLICIT NONE
    INTEGER                        :: ibdyty,inodty,iccdof,nbdof
    REAL(kind=8),DIMENSION(nbDIME) :: get_dep_R_TT_nodty
  
    iccdof=bdyty(ibdyty)%ccdof(inodty)
  
    ! attention au cafouillage entre nbdof et nbDIME
  
    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
    nbdof=MIN(nbdof,nbDIME)
  
    get_dep_R_TT_nodty = bdyty(ibdyty)%dep_R_TT(iccdof+1:iccdof+nbdof)
  
  END FUNCTION get_dep_R_TT_nodty

!------------------------------------------------------------------------ 
! le merdier de fred pour faire du 
!------------------------------------------------------------------------ 
 SUBROUTINE set_rigid_body_mecaMAILx (iM_bdyty)
   IMPLICIT NONE
   INTEGER :: iM_bdyty
   ! ***
   INTEGER ::   ibdyty
                            !1234567890123456789012345
   CHARACTER(len=25) :: IAM='mecaMAILx::set_rigid_body'

   !fd on vient declarer que ce corps est a rendre coro

   IF (nb_mecaMAILx == 0) THEN
      CALL FATERR(IAM,' impossible no mecaMAILx available')
   ENDIF

   PRINT*,'body ',iM_bdyty,' is rigid'

   ibdyty=M2meca(iM_bdyty)%bdyty
   bdyty(ibdyty)%is_rigid = .TRUE.

   CALL settle_body_as_rigid(ibdyty)


 END SUBROUTINE set_rigid_body_mecaMAILx
!------------------------------------------------------------------------ 
 SUBROUTINE skip_defo_computation_mecaMAILx(iM_bdyty)
   IMPLICIT NONE
   INTEGER :: iM_bdyty
   ! ***
   INTEGER ::   ibdyty
                            !12345678901234567890123456789012
   CHARACTER(len=32) :: IAM='mecaMAILx::skip_defo_computation'

   ibdyty=M2meca(iM_bdyty)%bdyty
   bdyty(ibdyty)%skip_defo_comp = .TRUE.

 END SUBROUTINE 
!------------------------------------------------------------------------
 SUBROUTINE settle_body_as_rigid(ibdyty)
   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,inodty,nbno_e,nbel_e,errare,iccdof,iM_bdyty,iM_nodty

   INTEGER :: idof,idof_e,jdof,jdof_e,iR,nbd

   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele, coor_G_e ! coordonnees elementaires et du centre d'inertie de l'element

   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: coor_G,S_e,d         ! coordonnees centre inertie, surf/vol element, bras de levier

   REAL(kind=8) :: a,b,c,tmp,p,rho

   REAL(kind=8) :: M,I_e(3,3),Ip(3)                    ! masse, inertie element, inertie diag 
   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: I,Fp     ! matrice inertie pleine, local frame

   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: mR                    
   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: R2D
   
   REAL(kind=8),DIMENSION(nbdime,nbdime) :: frame_transpose

   REAL(kind=8) :: H2T2,fd_rescale

   INTEGER :: info,nbdofR

   LOGICAL :: itchatche=.FALSE.

   !                         1234567890123456789012345678901
   CHARACTER(len=31) :: IAM='mecaMAILx::settle_body_as_rigid'
   CHARACTER(len=80) :: cout

   CHARACTER(len=20) ::fmt
   
   !fd cuisine pour gestion des H8
      
   integer      :: connec_faces_H8(4,6)
   integer :: if,in

   integer :: err_
   
   ! quelques pre-requis
  
   !IF (bdyty(ibdyty)%is_precon) THEN
   !  WRITE(cout,'(a,i0)') 'Object: ',ibdyty
   !  CALL LOGMES(cout)
   !  CALL FATERR(IAM,'rigid and precon methods can not be used at the time')
   !ENDIF

   !fd cuisine pour gestion des H8
   connec_faces_H8(:,1) = (/ 1,4,3,2 /)
   connec_faces_H8(:,2) = (/ 1,2,6,5 /)
   connec_faces_H8(:,3) = (/ 2,3,7,6 /)
   connec_faces_H8(:,4) = (/ 3,4,8,7 /)
   connec_faces_H8(:,5) = (/ 4,1,5,8 /)
   connec_faces_H8(:,6) = (/ 5,6,7,8 /)     


   !am & pta : le coro utilise les memes initialisations que le cas
   !"is_rigid"

   !pas de cdl en vitesse autorisees !!
   !attention ce test n'a pas forcement de sens si le fichier cdl pas lu
   ! donc je vire. A faire ailleurs.
   !if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
   !  write(cout,'(a,i0)') 'Object: ',ibdyty
   !  call LOGMES(cout)
   !  call FATERR(IAM,'imposed velocity not allowed')
   !endif


   iM_bdyty=bdyty2M_bdyty(ibdyty)

   fd_rescale=1.d0

   H2T2=THETA*H*THETA*H

   !fd 
   ! approche rigid tiree de la tete de fred dub
   ! technique assez proche du precon standard
   !  -calcul masse, inertie, centre inertie, repere inertie d'un objet maille
   !  -calcul de l'operateur de relevement G des vitesses (upscaling rigide -> defo) 
   !  -calcul du projecteur L des vitesses (downscaling defo -> rigide)
   !  -calcul W
   !fd

   IF (nbDIME == 2) THEN
     nbdofR=3   ! vx,vy,wz
     ALLOCATE(I(1,1))
   ELSE IF (nbDIME == 3) THEN
     nbdofR=6   ! vx,vy,vz,w1,w2,w3
     ALLOCATE(I(3,3)) 
   ELSE
     call faterr(IAM,'unsupported dim of the problem')
   ENDIF

   bdyty(ibdyty)%nbdofR = nbdofR

   ALLOCATE(coor_G(nbDime),mR(nbdofR),d(nbDime),Fp(nbdime,nbdime))

   nbel_e = SIZE(bdyty(ibdyty)%blmty)

   ALLOCATE(coor_G_e(nbDime,nbel_e),S_e(nbel_e))

   call logmes('Calcul masse et inertie de l objet (rigide equivalent)')

   !fd on parcourt les elements

   M=0.d0
   I=0.d0
   coor_G=0.d0
     
   IF (nbdime ==2) THEN

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       !fd faire une modif pour utilisation field 
       rho = get_rho(bdyty(ibdyty)%blmty(iblmty)%lawnb)

       nbno_e=SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
       ALLOCATE(coor_ele(nbDIME,nbno_e),stat=errare)
       coor_ele=get_cooref_ele(ibdyty,iblmty,nbno_e) 

       SELECT CASE(nbno_e)
       CASE(3)
         coor_G_e(:,iblmty) = (coor_ele(:,1) + coor_ele(:,2) +coor_ele(:,3))/3.d0
         d = coor_ele(:,2) - coor_ele(:,1)
         a = dsqrt( DOT_PRODUCT(d,d) )
         d = coor_ele(:,3) - coor_ele(:,2)
         b = dsqrt( DOT_PRODUCT(d,d) )
         d = coor_ele(:,1) - coor_ele(:,3)
         c = dsqrt( DOT_PRODUCT(d,d) )
         p = 0.5D0 * (a+b+c)
         S_e(iblmty) = dsqrt( p * (p-a) * (p-b) * (p-c))

         d = coor_ele(:,1) - ((coor_ele(:,2)+coor_ele(:,3))*0.5)
         a = DOT_PRODUCT(d,d)
         d = coor_ele(:,2) - ((coor_ele(:,1)+coor_ele(:,3))*0.5)
         b = DOT_PRODUCT(d,d)
         d = coor_ele(:,3) - ((coor_ele(:,2)+coor_ele(:,1))*0.5)
         c = DOT_PRODUCT(d,d)

         I = I + (rho * S_e(iblmty) * (a+b+c)/27.d0)

       CASE default
         CALL FATERR(IAM,'unsupported 2D element')
       END SELECT

       DEALLOCATE(coor_ele)

       M = M + (rho * S_e(iblmty))
       coor_G = coor_G + (rho * S_e(iblmty)*coor_G_e(:,iblmty))

     ENDDO   

     coor_G = coor_G / M
 
     !fd correction huyghens

     DO iblmty=1,nbel_e
       d = coor_G_e(:,iblmty) - coor_G
       I = I + (rho * S_e(iblmty) * DOT_PRODUCT(d,d))
     END DO

     mR(1) = M
     mR(2) = M
     mR(3) = I(1,1)

     ! on cree un localframe a l'identite 
     Fp = Id22

     !print*,'----'
     !print*,mR
     !print*,'----'
     
   ELSE IF (nbdime==3) THEN

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       !fd faire une modif pour utilisation field         
       rho = get_rho(bdyty(ibdyty)%blmty(iblmty)%lawnb)

       nbno_e=SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

       ALLOCATE(coor_ele(nbDIME,nbno_e),stat=errare)

       coor_ele=get_cooref_ele(ibdyty,iblmty,nbno_e) 

      !print*,ibdyty,iblmty,nbno_e
      !do inodty=1,nbno_e
      !  print*,coor_ele(:,inodty)
      !enddo

       SELECT CASE(nbno_e)
       CASE(4) !tetraedron
         coor_G_e(:,iblmty) = (coor_ele(:,1) + coor_ele(:,2) +coor_ele(:,3) +coor_ele(:,4))*0.25d0
         CALL compute_vol_tetrahedron(coor_ele(:,1),coor_ele(:,2),coor_ele(:,3),coor_ele(:,4),S_e(iblmty))

         if (S_e(iblmty) < 0.d0) then
           write(cout,'("element ",I0,"of body ",I0," has a negative volume ",E12.5)') iblmty,ibdyty,S_e(iblmty)  
           call logmes(cout)
           call faterr(IAM,'element with wrong orientation')
         endif
         
         !print*,S_e(iblmty)

         CALL compute_inertia_tetrahedron(coor_ele(:,1) - coor_G_e(:,iblmty), &
                                          coor_ele(:,2) - coor_G_e(:,iblmty), &
                                          coor_ele(:,3) - coor_G_e(:,iblmty), &
                                          coor_ele(:,4) - coor_G_e(:,iblmty), &
                                          I_e)

         !print*,I_e

       CASE(8) !hexaedron

         call compute_volume_inertia_global_frame_surface_Q4(8,6,connec_faces_H8,coor_ele,.TRUE.,S_e(iblmty),coor_G_e(:,iblmty),I_e, err_)

         if (err_ > 0) then
            write(cout,'("element ",I0,"of body ",I0," has something wrong")') iblmty,ibdyty
            call faterr(IAM,cout)
         endif   
         
         if (S_e(iblmty) < 0.d0) then
           write(cout,'("element ",I0,"of body ",I0," has a negative volume ",E12.5)') iblmty,ibdyty,S_e(iblmty)  
           call logmes(cout)
           call faterr(IAM,'element with wrong orientation')
         endif

         !print*,iblmty
         !do if=1,6   
         !  print*,if
         !  write(*,'(3(1x,D12.5))') (coor_ele(:,connec_faces_H8(in,if)),in=1,4)
         !  print*,'---'
         !enddo
         !print*,S_e(iblmty)
         !write(*,'(3(1x,D12.5))')coor_G_e(:,iblmty)
         !write(*,'(3(1x,D12.5))') I_e

       CASE default
         CALL FATERR(IAM,'unsupported 3D element')
       END SELECT

       DEALLOCATE(coor_ele)

       I = I + (rho * I_e)

       M = M + (rho * S_e(iblmty))

       coor_G = coor_G + (rho * S_e(iblmty)*coor_G_e(:,iblmty))

     ENDDO
   
     coor_G = coor_G / M
 
     !fd correction huyghens

     DO iblmty=1,nbel_e
       d = coor_G_e(:,iblmty) - coor_G

       I(1,1) = I(1,1) + rho * S_e(iblmty)*(d(2)*d(2) + d(3)*d(3)) 
       I(2,2) = I(2,2) + rho * S_e(iblmty)*(d(1)*d(1) + d(3)*d(3)) 
       I(3,3) = I(3,3) + rho * S_e(iblmty)*(d(1)*d(1) + d(2)*d(2)) 
       I(1,2) = I(1,2) - rho * S_e(iblmty)*(d(1)*d(2))
       I(2,1) = I(2,1) - rho * S_e(iblmty)*(d(1)*d(2))
       I(1,3) = I(1,3) - rho * S_e(iblmty)*(d(1)*d(3))
       I(3,1) = I(3,1) - rho * S_e(iblmty)*(d(1)*d(3))
       I(2,3) = I(2,3) - rho * S_e(iblmty)*(d(2)*d(3))
       I(3,2) = I(3,2) - rho * S_e(iblmty)*(d(2)*d(3))

     END DO

     CALL diagonalise33(I,Ip,Fp)

     mR(1:3) = M
     mR(4:6) = Ip(1:3)

   ELSE
     CALL LOGMES('Error '//IAM//': dim not supported')
   ENDIF


   IF (itchatche) THEN
     PRINT*,'centre inertie '
     PRINT*,coor_G
     PRINT*,'Masse et Inertie '
     PRINT*, mR
     PRINT*,'frame'
     PRINT*,Fp(:,1)
     PRINT*,Fp(:,2)
     IF (nbdime == 3) PRINT*,Fp(:,3)
   ENDIF

   ! on conserve

   IF (.NOT. ALLOCATED(bdyty(ibdyty)%cooref_G)) ALLOCATE(bdyty(ibdyty)%cooref_G(nbdime))
   IF (.NOT. ALLOCATED(bdyty(ibdyty)%localframeini)) ALLOCATE(bdyty(ibdyty)%localframeini(nbdime,nbdime))
   IF (.NOT. ALLOCATED(bdyty(ibdyty)%localframe)) ALLOCATE(bdyty(ibdyty)%localframe(nbdime,nbdime))
   IF (.NOT. ALLOCATED(bdyty(ibdyty)%localframett)) ALLOCATE(bdyty(ibdyty)%localframett(nbdime,nbdime))
   IF (.NOT. ALLOCATED(bdyty(ibdyty)%mR)) ALLOCATE(bdyty(ibdyty)%mR(nbdofR))
   IF (.NOT. ALLOCATED(bdyty(ibdyty)%inv_mR)) ALLOCATE(bdyty(ibdyty)%inv_mR(nbdofR))
   IF (.NOT. ALLOCATED(bdyty(ibdyty)%cooref_local)) ALLOCATE(bdyty(ibdyty)%cooref_local(nbdime,bdyty(ibdyty)%nb_nodes)) 

   bdyty(ibdyty)%cooref_G = coor_G
   bdyty(ibdyty)%localframeini = Fp
   bdyty(ibdyty)%localframe = Fp
   bdyty(ibdyty)%localframett = Fp

   bdyty(ibdyty)%mR = mR

   DO iR=1,nbdofR
     bdyty(ibdyty)%inv_mR(iR) = 1.d0 / mR(iR)
   ENDDO

  DEALLOCATE(I,coor_G,mR)
  DEALLOCATE(coor_G_e,S_e)

  ! il faut mettre le maillage dans le repere principale d'inertie
  ! et tout est calcul dans ce repere

  SELECT CASE (nbdime)
  CASE(2)
    DO inodty =1, bdyty(ibdyty)%nb_nodes
      iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
      bdyty(ibdyty)%cooref_local(:,inodty) = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty) - bdyty(ibdyty)%cooref_G
    ENDDO
  CASE(3)

    !PRINT*,'frame ini de: ',ibdyty
    !WRITE(*,'(3(1x,D12.5))') bdyty(ibdyty)%localframeini

    frame_transpose = transpose33(bdyty(ibdyty)%localframeini)

    DO inodty =1, bdyty(ibdyty)%nb_nodes

      iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

      d = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty) - bdyty(ibdyty)%cooref_G

      ! fd bugy
      !bdyty(ibdyty)%cooref_local(:,inodty) = matmul(bdyty(ibdyty)%localframeini,d)

      
      bdyty(ibdyty)%cooref_local(:,inodty) = MATMUL(frame_transpose,d)

    ENDDO
  END SELECT


   IF (itchatche) THEN
     PRINT*,'coordonnees dans le repere principale d inertie '
     DO inodty =1, bdyty(ibdyty)%nb_nodes
       WRITE(*,'(I0,3(1x,D12.5))') inodty,bdyty(ibdyty)%cooref_local(:,inodty)  
     ENDDO
   ENDIF



  ! construction de R2D (G) 

  ALLOCATE(R2D(bdyty(ibdyty)%nbdof,nbdofR))

  DO inodty=1,bdyty(ibdyty)%nb_nodes
    ! on utilise la coordonnee par rapport au centre d'inertie
    d = bdyty(ibdyty)%cooref_local(:,inodty)
    iccdof = bdyty(ibdyty)%ccdof(inodty) 

    ! v(M) = v(G) + w x GM 
    IF (nbdime == 2 ) THEN

      R2D(iccdof+1,1) = 1.d0;  R2D(iccdof+1,2) = 0.D0; R2D(iccdof+1,3) = -d(2)
      R2D(iccdof+2,1) = 0.d0;  R2D(iccdof+2,2) = 1.D0; R2D(iccdof+2,3) =  d(1)

      !print*,iccdof
      !print*,R2D(iccdof+1,1),R2D(iccdof+1,2),R2D(iccdof+1,3)
      !print*,R2D(iccdof+2,1),R2D(iccdof+2,2),R2D(iccdof+2,3)

    ELSE IF (nbdime == 3 ) THEN

      R2D(iccdof+1,1) = 1.d0;  R2D(iccdof+1,2) = 0.D0; R2D(iccdof+1,3) = 0.d0
      R2D(iccdof+1,4) = 0.d0;  R2D(iccdof+1,5) = d(3); R2D(iccdof+1,6) =-d(2)

      R2D(iccdof+2,1) = 0.d0;  R2D(iccdof+2,2) = 1.D0; R2D(iccdof+2,3) = 0.d0
      R2D(iccdof+2,4) =-d(3);  R2D(iccdof+2,5) = 0.d0; R2D(iccdof+2,6) = d(1)

      R2D(iccdof+3,1) = 0.d0;  R2D(iccdof+3,2) = 0.D0; R2D(iccdof+3,3) = 1.d0
      R2D(iccdof+3,4) = d(2);  R2D(iccdof+3,5) =-d(1); R2D(iccdof+3,6) = 0.d0
          
      !print*,iccdof
      !write(*,'(6(1x,D12.5))') R2D(iccdof+1,1:6)
      !write(*,'(6(1x,D12.5))') R2D(iccdof+2,1:6)
      !write(*,'(6(1x,D12.5))') R2D(iccdof+3,1:6)

    ENDIF

  ENDDO

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%R2D)) ALLOCATE(bdyty(ibdyty)%R2D(SIZE(R2D,dim=1),SIZE(R2D,dim=2)))
  bdyty(ibdyty)%R2D = R2D
  
  DEALLOCATE(R2D)
  DEALLOCATE(d,Fp)

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RV)) ALLOCATE(bdyty(ibdyty)%RV(nbdofR))
  bdyty(ibdyty)%RV=0.d0 

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RVbegin)) ALLOCATE(bdyty(ibdyty)%RVbegin(nbdofR))
  bdyty(ibdyty)%RVbegin = 0.d0

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RVfree)) ALLOCATE(bdyty(ibdyty)%RVfree(nbdofR))
  bdyty(ibdyty)%RVfree = 0.d0

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RVaux)) ALLOCATE(bdyty(ibdyty)%RVaux(nbdofR))
  bdyty(ibdyty)%RVaux = 0.d0

  !IF (.NOT. ALLOCATED(bdyty(ibdyty)%RX)) ALLOCATE(bdyty(ibdyty)%RX(nbdime))
  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RX)) ALLOCATE(bdyty(ibdyty)%RX(nbdofR))  
  bdyty(ibdyty)%RX = 0.d0

  !IF (.NOT. ALLOCATED(bdyty(ibdyty)%RXbegin)) ALLOCATE(bdyty(ibdyty)%RXbegin(nbdime))
  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RXbegin)) ALLOCATE(bdyty(ibdyty)%RXbegin(nbdofR))  
  bdyty(ibdyty)%RXbegin = 0.d0

  !IF (.NOT. ALLOCATED(bdyty(ibdyty)%RX_TT)) ALLOCATE(bdyty(ibdyty)%RX_TT(nbdime))
  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RX_TT)) ALLOCATE(bdyty(ibdyty)%RX_TT(nbdofR))  
  bdyty(ibdyty)%RX_TT = 0.d0

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RIreac)) ALLOCATE(bdyty(ibdyty)%RIreac(nbdofR))
  bdyty(ibdyty)%RIreac = 0.d0

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RIaux)) ALLOCATE(bdyty(ibdyty)%RIaux(nbdofR))
  bdyty(ibdyty)%RIaux = 0.d0

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RFext)) ALLOCATE(bdyty(ibdyty)%RFext(nbdofR))
  bdyty(ibdyty)%RFext = 0.d0

  IF (.NOT. ALLOCATED(bdyty(ibdyty)%RFint)) ALLOCATE(bdyty(ibdyty)%RFint(nbdofR))
  bdyty(ibdyty)%RFint = 0.d0

 END SUBROUTINE 

 !------------------------------------------------------------------------

 SUBROUTINE build_rigid_bodies_mecaMAILx()
   IMPLICIT NONE
   ! ***

   INTEGER ::   ibdyty

                            !123456789012345678901234567
   CHARACTER(len=27) :: IAM='mecaMAILx::build_rigid_body'
   character(len=80) :: cout

   !fd on vient finir la construction du corps rigide 

   IF (nb_mecaMAILx == 0) THEN
      CALL FATERR(IAM,' impossible no mecaMAILx available')
   ENDIF

   DO ibdyty=1,SIZE(bdyty)

     ! calcul de l'operateur D2R, a partir du rigide equivalent
     IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN 
       write(cout,*) 'body ',ibdyty,' is rigid or coro'
       call logmes(cout)
       CALL build_body_as_rigid(ibdyty)
     ENDIF

   ENDDO

 END SUBROUTINE build_rigid_bodies_mecaMAILx
!------------------------------------------------------------------------
 SUBROUTINE build_body_as_rigid(ibdyty)
   IMPLICIT NONE

   INTEGER :: ibdyty

   ! ***
   INTEGER :: iblmty,inodty,errare,iccdof
   INTEGER :: idof,idof_e,jdof,jdof_e,iR

   integer :: idofR,jdofR

   LOGICAL :: itchatche=.FALSE.

   REAL(kind=8),ALLOCATABLE :: D2R(:,:),vec_loc(:),vec_glob(:)

   real(kind=8) :: xx
  
   !                         123456789012345678901234567890
   CHARACTER(len=30) :: IAM='mecaMAILx::build_body_as_rigid'

   !fd 
   ! approche rigide tiree de la tete de fred dub
   ! technique assez proche du precon standard
   ! settle as rigid :
   !  -calcul masse, inertie, centre inertie, repere inertie d'un objet maille
   !  -calcul de l'operateur de relevement G des vitesses (upscaling rigide -> defo) 
   ! build as rigid:
   !  -calcul du projecteur L des vitesses (downscaling defo -> rigide)
   ! !! pas calcul de W
   !fd

   ALLOCATE(D2R(bdyty(ibdyty)%nbdofR,bdyty(ibdyty)%nbdof))

   ! construction de D2R (L)

   !fd construction des D2R (L) par assemblage des m^-1 G^T M^e
   D2R = 0.d0
   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
     DO idof_e=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%mass,dim=1)
       idof = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(idof_e)
       DO iR=1,bdyty(ibdyty)%nbdofR
         DO jdof_e=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%mass,dim=2)
           jdof= bdyty(ibdyty)%blmty(iblmty)%edof2gdof(jdof_e)
           D2R(iR,idof) = D2R(iR,idof) +  &
                          bdyty(ibdyty)%inv_mR(iR)*bdyty(ibdyty)%R2D(jdof,iR)*bdyty(ibdyty)%blmty(iblmty)%mass(jdof_e,idof_e)
         ENDDO
       ENDDO
     ENDDO
   ENDDO

   if (.False.) then
      
     !!fd paranoiac test : GL = Id_U_R
   
     allocate(vec_loc(bdyty(ibdyty)%nbdofR),vec_glob(bdyty(ibdyty)%nbdof))
     print*,'vitesses'
     do idofR=1,bdyty(ibdyty)%nbdofR
       print*,idofR
       vec_loc = 0.d0 ; vec_loc(idofR) = 1.d0
       ! --
       print*,'loc'
       select case (nbdime)
       case(2)
         write(*,'(3(1x,D12.5))') vec_loc          
       case(3)   
         write(*,'(6(1x,D12.5))') vec_loc
       end select
       ! --
       vec_glob = matmul(bdyty(ibdyty)%R2D,vec_loc)
       ! --
       print*,'glob = R2D loc'
       select case (nbdime)
       case(2)
         write(*,'(2(1x,D12.5))') vec_glob
       case(3)
         write(*,'(3(1x,D12.5))') vec_glob         
       end select             
       ! --
       print*,'loc = D2R glob'
       select case (nbdime)
       case(2)
         write(*,'(3(1x,D12.5))') matmul(D2R,vec_glob)
       case(3)   
         write(*,'(6(1x,D12.5))') matmul(D2R,vec_glob)
       end select   
     enddo

     print*,'forces'
     do idofR=1,bdyty(ibdyty)%nbdofR
       ! --
       print*,idofR
       vec_loc = 0.d0 ; vec_loc(idofR) = 1.d0
       print*,'loc'
       select case (nbdime)
       case(2)
         write(*,'(3(1x,D12.5))') vec_loc          
       case(3)   
         write(*,'(6(1x,D12.5))') vec_loc
       end select
       ! --
       DO idof=1,bdyty(ibdyty)%nbdof
        vec_glob(idof)=DOT_PRODUCT(D2R(:, idof), vec_loc(:))
       END DO

       print*,'glob = D2R^T loc'
       select case (nbdime)
       case(2)
         write(*,'(2(1x,D12.5))') vec_glob
       case(3)
         write(*,'(3(1x,D12.5))') vec_glob         
       end select

       ! one more paranoiac test
       ! on verifie que la resultante de la distribution vaut bien la solicitation  
       if (idofR == 1 .or. idofR == 2) then
         xx=0.d0 
         do idof=1,bdyty(ibdyty)%nbdof
           if (mod(idof,1) == 0) then
             xx = xx + vec_glob(idof)
           endif
         enddo 
         print*,xx, vec_loc(idofR)
       endif
       !--
       DO jdofR=1,bdyty(ibdyty)%nbdofR
         vec_loc(jdofR)=DOT_PRODUCT(bdyty(ibdyty)%R2D(:, jdofR), vec_glob(:))
       END DO
       !--
       print*,'loc = R2D^T glob'
       select case (nbdime)
       case(2)
         write(*,'(3(1x,D12.5))') vec_loc
       case(3)   
         write(*,'(6(1x,D12.5))') vec_loc 
       end select   
     enddo
     deallocate(vec_loc,vec_glob)
   endif  

   IF (.NOT. ALLOCATED(bdyty(ibdyty)%D2R)) ALLOCATE(bdyty(ibdyty)%D2R(SIZE(D2R,dim=1),SIZE(D2R,dim=2)))
   bdyty(ibdyty)%D2R = D2R


   !print*,ibdyty
   !write(*,'(3(1x,D12.5))') D2R(1,:)
   !print*,'--'
   !write(*,'(3(1x,D12.5))') D2R(2,:)
   !print*,'--'
   !write(*,'(3(1x,D12.5))') D2R(3,:)
   !print*,'--'
   !write(*,'(3(1x,D12.5))') D2R(4,:)
   !print*,'--'
   !write(*,'(3(1x,D12.5))') D2R(5,:)
   !print*,'--'
   !write(*,'(3(1x,D12.5))') D2R(6,:)

   DEALLOCATE(D2R)

 END SUBROUTINE 

!------------------------------------------------------------------------
!!!PTA---------------------------------------------------------------------
  LOGICAL FUNCTION get_visible_mecaMAILx(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty

    get_visible_mecaMAILx = bdyty(ibdyty)%visible
    
  END FUNCTION get_visible_mecaMAILx
!!!PTA---------------------------------------------------------------------
  SUBROUTINE  set_visible_mecaMAILx(ibdyty,FLAG)

    IMPLICIT NONE
    INTEGER :: ibdyty
    LOGICAL :: FLAG

    bdyty(ibdyty)%visible = FLAG
    
  END SUBROUTINE set_visible_mecaMAILx
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_color_mecaMAILx(ibdyty,itacty)

    IMPLICIT NONE

    INTEGER :: ibdyty,itacty
    INTEGER :: iM_bdyty !,iM_tacty

    iM_bdyty=bdyty2M_bdyty(ibdyty)

    get_color_mecaMAILx=get_color_MAILx(iM_bdyty,itacty)

  END FUNCTION get_color_mecaMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE add_Rreac_mecaMAILx(ibdyty,xxccdof,xxreac,storage)

    IMPLICIT NONE
    INTEGER                    :: ibdyty,iccdof,storage
    INTEGER,     DIMENSION(6)  :: xxccdof
    REAL(kind=8),DIMENSION(6)  :: xxreac
    
    SELECT CASE(storage)
    CASE (iIreac)

       !       print*,'add Rreac'
       !       print*,bdyty(ibdyty)%RReac,' += ',xxreac


       bdyty(ibdyty)%RIreac=bdyty(ibdyty)%RIreac+xxreac

    CASE (iIaux_)

       !print*,'Iaux'
       !print*,xxreac

       bdyty(ibdyty)%RIaux=bdyty(ibdyty)%RIaux+xxreac

    END SELECT
    
  END SUBROUTINE add_Rreac_mecaMAILx
!!!------------------------------------------------------------------------
   FUNCTION get_RcoorTT_mecaMAILx(ibdyty,itacty)
    IMPLICIT NONE
    INTEGER :: ibdyty
    INTEGER,OPTIONAL :: itacty !fd je ne sais pas a quoi ca sert ...
    REAL(kind=8) :: get_RcoorTT_mecaMAILx(3)

    get_RcoorTT_mecaMAILx = 0.d0
    get_RcoorTT_mecaMAILx(1:nbdime) = bdyty(ibdyty)%RcoorTT(1:nbdime)

  END FUNCTION
!!!------------------------------------------------------------------------
   FUNCTION get_Rcooref_mecaMAILx(ibdyty,itacty)
    IMPLICIT NONE
    INTEGER :: ibdyty
    INTEGER,OPTIONAL :: itacty !fd je ne sais pas a quoi ca sert ...
    REAL(kind=8) :: get_Rcooref_mecaMAILx(3)

    get_Rcooref_mecaMAILx = 0.d0
    get_Rcooref_mecaMAILx(1:nbdime) = bdyty(ibdyty)%cooref_G(1:nbdime)

  END FUNCTION
!!!------------------------------------------------------------------------
  function get_Rinertia_frameTT_mecaMAILx(ibdyty)
    implicit none
    integer :: ibdyty
    real(kind=8),DIMENSION(3,3) :: get_Rinertia_frameTT_mecaMAILx

    get_Rinertia_frameTT_mecaMAILx = bdyty(ibdyty)%LocalFrameTT

  end function
!!!------------------------------------------------------------------------
  function get_Rinertia_frame_mecaMAILx(id_mat, ibdyty)
    implicit none
    ! Rigid inertia matrix to get
    character(len=5) :: id_mat
    integer :: ibdyty
    real(kind=8),DIMENSION(3,3) :: get_Rinertia_frame_mecaMAILx

    select case(id_mat)
    case('RFbeg')
      get_Rinertia_frame_mecaMAILx = bdyty(ibdyty)%LocalFrameIni
    case('RF___')
      get_Rinertia_frame_mecaMAILx = bdyty(ibdyty)%LocalFrame
    case('RFTT_')
      get_Rinertia_frame_mecaMAILx = bdyty(ibdyty)%LocalFrameTT
    case default
      call faterr('mecaMAILx::get_Rinertia_frame','unknown id: '//id_mat)
    end select
  end function

!!!------------------------------------------------------------------------
  FUNCTION get_Rvlocy_mecaMAILx(ibdyty,storage)
    IMPLICIT NONE
    INTEGER                   :: ibdyty,storage
    REAL(kind=8),DIMENSION(6) :: get_Rvlocy_mecaMAILx

    SELECT CASE(storage)
    CASE(iV____)
       get_Rvlocy_mecaMAILx = bdyty(ibdyty)%RV

       !print*,'V'
       !print*,bdyty(ibdyty)%RV

    CASE(iVbeg_)
       get_Rvlocy_mecaMAILx = bdyty(ibdyty)%RVbegin

       !print*,'Vbegin'
       !print*,bdyty(ibdyty)%RVbegin

    CASE(iVfree)
       get_Rvlocy_mecaMAILx = bdyty(ibdyty)%RVfree

       !print*,'Vfree'
       !print*,bdyty(ibdyty)%RVfree

    CASE(iVaux_)
       get_Rvlocy_mecaMAILx = bdyty(ibdyty)%RVaux

       !print*,'Vaux'
       !print*,bdyty(ibdyty)%RVaux

    CASE default
    END SELECT

  END FUNCTION

 !------------------------------------------------------------------------ 

 !------------------------------------------------------------------------   
 SUBROUTINE get_Rvector_mecaMAILx(id_vect,ibdyty,vect,nbdofR)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdofR
   REAL(kind=8),DIMENSION(nbdofR) :: vect
   CHARACTER(len=5) :: id_vect

                            !1234567890123456789012
   CHARACTER(len=22) :: IAM='mecaMAILx::get_Rvector'

   vect = 0.d0
   
   IF (.not. bdyty(ibdyty)%is_coro .and. .not. bdyty(ibdyty)%is_rigid) THEN
     call FATERR(IAM,'The object is neither coro nor rigid') 
   ENDIF


   
   SELECT CASE(id_vect)
    CASE('Coor0')
     vect=bdyty(ibdyty)%cooref_G
    CASE('Coorb')
     vect= bdyty(ibdyty)%coorbegin_G
    CASE('X____')
     vect=bdyty(ibdyty)%RX
    CASE('Xbeg_')
     vect=bdyty(ibdyty)%RXbegin
    CASE('XTT__')
     vect=bdyty(ibdyty)%RX_TT
    CASE('V____')
     vect=bdyty(ibdyty)%RV
    CASE('Vbeg_')
     vect=bdyty(ibdyty)%RVbegin
    CASE('Vfree')
     vect=bdyty(ibdyty)%RVfree
    CASE('Vaux_')
     vect=bdyty(ibdyty)%RVaux
    CASE('Raux_')
     vect=bdyty(ibdyty)%RIaux/H
    CASE('Reac_')
     vect=bdyty(ibdyty)%RIreac/H
    CASE('Iaux_')
     vect=bdyty(ibdyty)%RIaux
    CASE('Ireac')
     vect=bdyty(ibdyty)%RIreac
    CASE('Fext_')
     vect=bdyty(ibdyty)%RFext
    CASE('Fint_')
     vect=bdyty(ibdyty)%RFint
    CASE DEFAULT
     call faterr('mecaMAILx::get_Rvector','unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE get_Rvector_mecaMAILx
 
 !------------------------------------------------------------------------ 

 !------------------------------------------------------------------------   
 
 SUBROUTINE put_Rvector_mecaMAILx(id_vect,ibdyty,vect,nbdofR)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdofR
   REAL(kind=8),DIMENSION(nbdofR) :: vect
   CHARACTER(len=5) :: id_vect

                            !1234567890123456789012
   CHARACTER(len=22) :: IAM='mecaMAILx::put_Rvector'

   IF (nbdofR /= SIZE(bdyty(ibdyty)%RV) ) THEN
     call FATERR(IAM,'nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
   CASE('X____')
     bdyty(ibdyty)%RX=vect
   CASE('Xbeg_')
     bdyty(ibdyty)%RXbegin=vect
   CASE('V____')
     bdyty(ibdyty)%RV=vect
   CASE('Vbeg_')
     bdyty(ibdyty)%RVbegin=vect
   CASE('Raux_')
     bdyty(ibdyty)%RIaux=vect*H
   CASE('Reac_')
     bdyty(ibdyty)%RIreac=vect*H
   CASE('Iaux_')
     bdyty(ibdyty)%RIaux=vect
   CASE('Ireac')
     bdyty(ibdyty)%RIreac=vect
   CASE('Vfree')
     bdyty(ibdyty)%RVfree=vect
   CASE('Fext_')
     bdyty(ibdyty)%RFext=bdyty(ibdyty)%RFext+vect
   CASE('Coor0')
     bdyty(ibdyty)%cooref_G = vect
   CASE DEFAULT
     call faterr('mecaMAILx::put_Rvector','unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE put_Rvector_mecaMAILx

 !------------------------------------------------------------------------ 

 !------------------------------------------------------------------------   
 SUBROUTINE get_nodal_vector_mecaMAILx(id_vect,ibdyty,inodty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,inodty,nbdof
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect
   !*** 
   INTEGER :: iccdof
   CHARACTER(len=27)::IAM='mecaMAILx::get_nodal_vector'

   IF (nb_mecaMAILx == 0) RETURN

   iccdof = bdyty(ibdyty)%ccdof(inodty)

   IF (nbdof /= nbdime ) THEN
     CALL FATERR(IAM,'nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
    CASE('X____')
     vect=bdyty(ibdyty)%X(iccdof+1:iccdof+nbdime)
    CASE('Xbeg_')
     vect=bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdime)
    CASE('V____')
     vect=bdyty(ibdyty)%V(iccdof+1:iccdof+nbdime)
    CASE('Vbeg_')
     vect=bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdime)
    CASE('Vaux_')
     vect=bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdime)
    CASE('Vfree')
     vect=bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbdime)
    CASE('Ireac')
     vect=bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdime)
    CASE('Fext_')
     vect=bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdime)
    CASE('Fint_')
     vect=bdyty(ibdyty)%Fint(iccdof+1:iccdof+nbdime)
    CASE DEFAULT
     call faterr(IAM,'unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE get_nodal_vector_mecaMAILx

 !------------------------------------------------------------------------ 

 !------------------------------------------------------------------------ 
 subroutine compute_Rayleigh_damping_mecaMAILx(ibdyty,alpha,beta)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   real(kind=8)   , intent(in) :: alpha,beta
   !
   integer(kind=4) :: id,iblmty,imodel
   !CHARACTER(len=5) :: c_alpha,c_beta
   character(len=40) :: cout
 
   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr('mecaMAILx::compute_Rayleigh_damping_mecaMAILx',cout)
   end if
 
   if( .not. bdyty(ibdyty)%visible ) return
   !print*,'compute normal damping'
 
   !write(*,'(A,I0)') 'Body: ',ibdyty
 
   if( bdyty(ibdyty)%is_rigid .and. bdyty(ibdyty)%skip_defo_comp ) return
 
   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
 
      imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
 
      id = get_eleop_id(imodel,'discr') 
      if ( id /= 0) then
        IF (get_eleop_value(imodel,id) == 'yes__') CYCLE
      endif
 
      !c_alpha = get_eleop_value(imodel,'Alpha')
      !c_beta  = get_eleop_value(imodel,'Beta_')
      !read(c_alpha,'(F5.2)') alpha
      !read(c_beta ,'(F5.2)') beta
      !print*,alpha,beta
 
 
      !write(*,'(A,I0)') 'Element: ',iblmty
      !write(*,'(12(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%mass
 
      bdyty(ibdyty)%blmty(iblmty)%damping=  alpha*bdyty(ibdyty)%blmty(iblmty)%mass + &
                                            beta*bdyty(ibdyty)%blmty(iblmty)%stiffness
 
      !if (ibdyty == 1 .and. iblmty == 1) then
      !  write(*,'(24(1x,E9.2))') bdyty(ibdyty)%blmty(iblmty)%mass
      !  print*,'--' 
      !  write(*,'(24(1x,E9.2))') bdyty(ibdyty)%blmty(iblmty)%stiffness
      !  print*,'--' 
      !  write(*,'(24(1x,E9.2))') bdyty(ibdyty)%blmty(iblmty)%damping
      !endif
 
 
   ENDDO    
 
 end subroutine

 subroutine compute_Rayleigh_damping_discrete_mecaMAILx(ibdyty,beta)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   real(kind=8)   , intent(in) :: beta
   !
   integer(kind=4)   :: id,iblmty,imodel
   character(len=40) :: cout

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr('mecaMAILx::compute_Rayleigh_damping_discrete_mecaMAILx',cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   if( bdyty(ibdyty)%is_rigid .and. bdyty(ibdyty)%skip_defo_comp ) return

   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb

      id = get_eleop_id(imodel,'discr') 
      if ( id == 0) then
        CYCLE
      else 
        IF (get_eleop_value(imodel,id) == 'yes__') bdyty(ibdyty)%blmty(iblmty)%damping=beta*bdyty(ibdyty)%blmty(iblmty)%stiffness
      endif
   ENDDO    

 end subroutine

  LOGICAL FUNCTION is_rigid_mecaMAILx(ibdyty)
    IMPLICIT NONE 
    INTEGER :: ibdyty

    is_rigid_mecaMAILx = bdyty(ibdyty)%is_rigid .or. bdyty(ibdyty)%is_coro
 
  END FUNCTION

!------------------------------------------------------------------------ 

SUBROUTINE Set_RV_driven_dofs_mecaMAILx(ibdyty,i4vec,length) 
  IMPLICIT NONE
  INTEGER :: ibdyty,i4vec(length),length,i

                          !123456789012345678901234567
  CHARACTER(len=27):: IAM='Set_RV_driven_dof_mecaMAILx'

  IF ( .NOT. bdyty(ibdyty)%is_coro .AND. &
       .NOT. bdyty(ibdyty)%is_rigid ) CALL FATERR(IAM,'body has no rigid dof') 

  bdyty(ibdyty)%nb_RV_driven = 0 
  bdyty(ibdyty)%RV_driven_dof= 0
  bdyty(ibdyty)%RV_driven= 0.d0 
  DO i=1,length
    IF ( i4vec(i) == 0 ) CALL FATERR(IAM,'why are you setting a RV dof to free ?')
    bdyty(ibdyty)%nb_RV_driven = bdyty(ibdyty)%nb_RV_driven + 1
    bdyty(ibdyty)%inv_mR(i4vec(i)) = 0.d0
    bdyty(ibdyty)%RV_driven_dof(i4vec(i))=1 
  ENDDO
END SUBROUTINE

!------------------------------------------------------------------------ 

SUBROUTINE Set_RV_driven_dof_value_mecaMAILx(ibdyty,idof,rv) 
  IMPLICIT NONE
  INTEGER :: ibdyty,idof
  REAL(kind=8) :: rv
  character(len=80) :: cout

                          !12345678901234567890123456789
  CHARACTER(len=29):: IAM='Set_RV_driven_value_mecaMAILx'

  IF ( .NOT. bdyty(ibdyty)%is_coro .AND. &
       .NOT. bdyty(ibdyty)%is_rigid ) CALL FATERR(IAM,'body has no rigid dof') 

  IF (bdyty(ibdyty)%nb_RV_driven /= 0 .AND. &
     bdyty(ibdyty)%RV_driven_dof(idof) /= 0) THEN

    bdyty(ibdyty)%RV_driven(idof)= rv
  ELSE
    write(cout,'(A,I0,A,I0,A,D12.5)') 'body: ',ibdyty,' dof: ',idof,' value: ',rv
    call LOGMES(cout)
    write(cout,'(A,I0)') 'nb driven dof: ', bdyty(ibdyty)%nb_RV_driven
    call LOGMES(cout)
    write(cout,'(A,6(1x,I0))') 'dof state (0: free | 1: driven): ', bdyty(ibdyty)%RV_driven_dof(:)
    call LOGMES(cout)
    CALL FATERR(IAM,'impossible to set RV value of a free dof')
  ENDIF
END SUBROUTINE

!------------------------------------------------------------------------ 

  !> \brief Set X field of a body from its nodes' coordinates
  !> copied from am function, only used by put_vector_mecaMAILx
  SUBROUTINE put_coor(ibdyty, new_coor, nbdof)
    IMPLICIT NONE 
    INTEGER(kind=4), INTENT(in) :: ibdyty !< [in] body number
    INTEGER(kind=4), INTENT(in) :: nbdof  !< [in] size of coor array
    REAL(kind=8), DIMENSION(nbdof), INTENT(in) :: new_coor !< [in] new node coordinates
    !
    INTEGER(kind=4) :: inodty, iccdof, iM_nodty, iM_ccdof, iM_bdyty

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    DO inodty =1, bdyty(ibdyty)%nb_nodes
      iccdof   = bdyty(ibdyty)%ccdof(inodty)
      iM_nodty = bdyty(ibdyty)%nodty2M_nodty(inodty)
      iM_ccdof = M_bdyty(iM_bdyty)%ccdof(iM_nodty)

      bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME) = new_coor(iccdof+1:iccdof+nbDIME) &
                                              - M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME)
    END DO
       
  END SUBROUTINE put_coor

!------------------------------------------------------------------------ 

  !> \brief Set Xbegin field of a body from its nodes' coordinates
  !> copied from am function, only used by put_vector_mecaMAILx
  SUBROUTINE put_coor_begin(ibdyty, new_coor, nbdof)
    IMPLICIT NONE 
    INTEGER(kind=4), INTENT(in) :: ibdyty !< [in] body number
    INTEGER(kind=4), INTENT(in) :: nbdof  !< [in] size of coor array
    REAL(kind=8), DIMENSION(nbdof), INTENT(in) :: new_coor !< [in] new node coordinates at beginning of time step
    !
    INTEGER(kind=4) :: inodty, iccdof, iM_nodty, iM_ccdof, iM_bdyty

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    DO inodty =1, bdyty(ibdyty)%nb_nodes
      iccdof   = bdyty(ibdyty)%ccdof(inodty)
      iM_nodty = bdyty(ibdyty)%nodty2M_nodty(inodty)
      iM_ccdof = M_bdyty(iM_bdyty)%ccdof(iM_nodty)

      bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbDIME) = new_coor(iccdof+1:iccdof+nbDIME) &
                                                   - M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME)
    END DO
       
  END SUBROUTINE put_coor_begin
  
  subroutine get_matrix_mecaMAILx(what,ibdyty, mat, size1, size2)
    implicit none
    character(len=5), intent(in) :: what
    integer(kind=4) , intent(in) :: ibdyty
    real(kind=8)    , dimension(:,:), pointer :: mat
    integer(kind=4) :: size1, size2
    !
    integer(kind=4) :: iblmty, nb_dof, enbdof, ej, ei, j, i
   
    nb_dof = bdyty(ibdyty)%nbdof
    allocate(mat(nb_dof,nb_dof))

    mat(1:nb_dof,1:nb_dof) = 0.D0
    select case(what)
    case('mass_')
      do  iblmty = 1, size(bdyty(ibdyty)%blmty)
        enbdof = size(bdyty(ibdyty)%blmty(iblmty)%mass,1)
        do ej = 1, enbdof
          j = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(ej)
          do ei = 1, enbdof
            i = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(ei)
            mat(i,j) = mat(i,j) + bdyty(ibdyty)%blmty(iblmty)%mass(ei,ej)
          end do
        end do
      end do
    case('stiff')
      do  iblmty = 1, size(bdyty(ibdyty)%blmty)
        enbdof = size(bdyty(ibdyty)%blmty(iblmty)%stiffness,1)
        do ej = 1, enbdof
          j = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(ej)
          do ei = 1, enbdof
            i = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(ei)
            mat(i,j) = mat(i,j) + bdyty(ibdyty)%blmty(iblmty)%stiffness(ei,ej)
          end do
        end do
      end do
    case('damp_')
      do  iblmty = 1, size(bdyty(ibdyty)%blmty)
        enbdof = size(bdyty(ibdyty)%blmty(iblmty)%damping,1)
        do ej = 1, enbdof
          j = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(ej)
          do ei = 1, enbdof
            i = bdyty(ibdyty)%blmty(iblmty)%edof2gdof(ei)
            mat(i,j) = mat(i,j) + bdyty(ibdyty)%blmty(iblmty)%damping(ei,ej)
          end do
        end do
      end do
    case default
      call faterr('mecaMAILx::get_matrix','unknown id: '//what)
    end select

  end subroutine

  subroutine get_drv_vlocy_mecaMAILx(ibdyty, indices, values)
  !permet d'extraire les vitesses imposees pour chaque corps ibdytyd 
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4), dimension(:), pointer :: indices
    real(kind=8)   , dimension(:), pointer :: values
    !
    integer(kind=4) :: i, nb_drvdof
    real(kind=8)    :: Vbegind

    if( nb_mecaMAILx<1 ) return

    nb_drvdof = bdyty(ibdyty)%nb_vlocy_driven_dof
    if( nb_drvdof==0 ) return

    allocate( indices(nb_drvdof), values(nb_drvdof) )

    do i = 1, nb_drvdof
      indices(i) =  nbDIME * (bdyty(ibdyty)%vlocy_driven_dof(i)%nodnb-1) &
                    + bdyty(ibdyty)%vlocy_driven_dof(i)%dofnb
      call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(i),Vbegind,values(i))
    end do

  end subroutine

  subroutine comp_drv_vlocy_mecaMAILx(ibdyty, values)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    real(kind=8), dimension(:)  :: values
    !
    integer(kind=4)   :: i
    real(kind=8)      :: Vbegind
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'mecaMAILx::comp_drv_vlocy_mecaMAILx'

    if( size(values) /= bdyty(ibdyty)%nb_vlocy_driven_dof ) then
      call faterr(IAM,'wrong size')
    endif

    do i=1, size(values)
      call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(i), Vbegind,values(i))
    end do
  end subroutine

!!! pta 23/01/2012
 subroutine set_tol_coro_mecaMAILx(tol)
   implicit none
   real(kind=8), intent(in) :: tol

   tol_coro = tol
 end subroutine

! Routine pour imposer les conditions aux limites par des champs externes
!------------------------------------------------------------------------
 SUBROUTINE set_vlocy_drvdof_mecaMAILx(ibdyty,ndof,node, Vdriven)
 IMPLICIT NONE

   
   REAL(kind=8),INTENT(in) :: Vdriven
   INTEGER     ,INTENT(in) :: ibdyty, node, ndof
   REAL(kind=8)            :: Vdrivenbegin, Xdrivenbegin, Xdriven
   INTEGER                 :: ivd,idof,inod,idrvdof,trouve
   
   character(len=26) :: IAM
   !      12345678901234567890123456
   IAM = 'set_vlocy_drvdof_mecaMAILx'
   
   
   IF (nb_mecaMAILx == 0) RETURN
   trouve = 1
   
   DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

       if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle       
     
       CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
      
       IF ((inod==node) .AND. (idof==ndof)) THEN
         !print *,'Application DRV DOF noeud : ',inod, ' dof : ',idof, ' value : ',nodal_value
         idrvdof = bdyty(ibdyty)%ccdof(inod)+idof

         Vdrivenbegin = bdyty(ibdyty)%Vbegin(idrvdof)
         Xdrivenbegin = bdyty(ibdyty)%Xbegin(idrvdof)
  
         Xdriven      = Xdrivenbegin + (1.D0-THETA)*H*Vdrivenbegin+THETA*H*Vdriven
         bdyty(ibdyty)%Vdriv(ivd) = Vdriven
         bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

         CALL apply_vlocy_driven_dof(ibdyty,iVfree)
         trouve = 0
          
       ENDIF

   ENDDO
   
   IF (trouve== 1) CALL faterr(IAM,'driven dof index not found')
   
 END SUBROUTINE set_vlocy_drvdof_mecaMAILx

 function get_cooref_mecaMAILx(ibdyty)
   implicit none 
   integer :: ibdyty
   real(kind=8),dimension(:,:),pointer :: get_cooref_mecaMAILx
   ! ***
   integer :: inodty,iM_bdyty,iM_nodty

   get_cooref_mecaMAILx => null()

   iM_bdyty=bdyty2M_bdyty(ibdyty)

   allocate(get_cooref_mecaMAILx(nbdime,bdyty(ibdyty)%nb_nodes)) 

   do inodty=1,bdyty(ibdyty)%nb_nodes

     iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty) 

     ! finalement on fait ca et on utilisera le warp pour voir la deformee
     get_cooref_mecaMAILx(1:nbdime,inodty) = get_cooref_nodty_mecaMAILx(ibdyty,inodty)

   enddo

 end function

 function get_All_mecaMAILx(ibdyty)
   implicit none 
   integer :: ibdyty
   real(kind=8),dimension(:,:),pointer :: get_All_mecaMAILx
   ! ***
   integer :: inodty,sz,nbn,ns,ne,idx
   REAL(kind=8),DIMENSION(:,:),allocatable :: S, E, val
   REAL(kind=8),DIMENSION(:)  ,allocatable :: Fres
   get_All_mecaMAILx => null()

   nbn=bdyty(ibdyty)%nb_nodes
   select case (nbdime) 
   case(2)
     !    X   V   gr  fl  Fext Fint Reac Fdyn Fres
     sz = 2 + 2 + 5 + 5 + 2  + 2  + 2  + 2  + 2
     ne=5
     ns=5
     ! DA : Attention au 
     ! xx, yy, xy, zz, vm|J
     allocate(S(5,nbn),E(5,nbn))

     if( is_externalFEM ) then    
        allocate(val(nbn,21))
        call externalFEM_get_val(ibdyty,nbn,21,val)
        S(1,:) = val(:,1)   !xx
        S(2,:) = val(:,5)   !yy
        S(3,:) = val(:,2)   !xy
        S(4,:) = val(:,9)   !zz
        S(5,:) = val(:,10)  !vm
        E(1,:) = val(:,11)  !xx
        E(2,:) = val(:,15)  !yy
        E(3,:) = val(:,12)  !xy
        E(4,:) = val(:,19)  !zz
        E(5,:) = val(:,21)  !vm
        deallocate(val)
     else
        call get_2DNodalStrain_mecaMAILx(ibdyty,E)
        call get_2DNodalStress_mecaMAILx(ibdyty,S)
     end if
   case(3)
     !    X   V   gr  fl  Fext Fint Reac Fdyn Fres
     sz = 3 + 3 + 7 + 7 + 3  + 3  + 3  + 3  + 3
     ne=7 
     ns=7
     ! xx, xy, yy, xz, yz, zz, vm|J
     allocate(S(7,bdyty(ibdyty)%nb_nodes), &
              E(7,bdyty(ibdyty)%nb_nodes))
     if( is_externalFEM ) then    
        allocate(val(nbn,21))
        call externalFEM_get_val(ibdyty,nbn,21,val)
        S(1,:) = val(:,1)   !xx
        S(2,:) = val(:,2)   !xy
        S(3,:) = val(:,5)   !yy
        S(4,:) = val(:,3)   !xz
        S(5,:) = val(:,6)   !yz
        S(6,:) = val(:,9)   !zz
        S(7,:) = val(:,10)  !vm
        E(1,:) = val(:,11)  !xx
        E(2,:) = val(:,12)  !xy
        E(3,:) = val(:,15)  !yy
        E(4,:) = val(:,13)  !xz
        E(5,:) = val(:,16)  !yz
        E(6,:) = val(:,19)  !zz
        E(7,:) = val(:,21)  !vm
        deallocate(val)
     else
        call get_3DNodalStrain_mecaMAILx(ibdyty,E)
        call get_3DNodalStress_mecaMAILx(ibdyty,S)
     end if
   end select

   !allocate(Fres(nbdime*bdyty(ibdyty)%nb_nodes))
   !call compute_Fres(ibdyty,Fres)

   allocate(get_All_mecaMAILx(sz,bdyty(ibdyty)%nb_nodes)) 

   get_All_mecaMAILX = 0.D0
   do inodty=1,bdyty(ibdyty)%nb_nodes
     idx=0
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = get_X_nodty_mecaMAILx(ibdyty,inodty)
     idx = idx + nbdime
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = get_V_nodty_mecaMAILx(ibdyty,inodty)
     idx = idx + nbdime  
     get_All_mecaMAILx(idx+1:idx+ne,inodty) = E(1:ne,inodty)
     idx = idx + ne  
     get_All_mecaMAILx(idx+1:idx+ns,inodty) = S(1:ns,inodty)
     idx = idx + ns  
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = bdyty(ibdyty)%fext(nbdime*(inodty-1)+1:nbdime*inodty)
     idx = idx + nbdime  
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = -bdyty(ibdyty)%fint(nbdime*(inodty-1)+1:nbdime*inodty)
     idx = idx + nbdime  
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = bdyty(ibdyty)%ireac(nbdime*(inodty-1)+1:nbdime*inodty)/H
     idx = idx + nbdime  
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = -bdyty(ibdyty)%finert(nbdime*(inodty-1)+1:nbdime*inodty)
     idx = idx + nbdime  
     get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = bdyty(ibdyty)%residu(nbdime*(inodty-1)+1:nbdime*inodty)

     !get_All_mecaMAILx(idx+1:idx+nbdime,inodty) = Fres(nbdime*(inodty-1)+1:nbdime*inodty)

   enddo

   !deallocate(E,S,Fres)
   deallocate(E,S)

 end function

 function get_ll_connectivity_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in)  :: ibdyty
   type(T_link_connec), pointer :: get_ll_connectivity_mecaMAILx
   !
   integer(kind=4) :: nb_elem, iblmty
   type(T_link_connec), pointer :: last, new

   allocate( get_ll_connectivity_mecaMAILx )
   allocate( get_ll_connectivity_mecaMAILx%connec(1) )

   nb_elem = size(bdyty(ibdyty)%blmty)

   get_ll_connectivity_mecaMAILx%connec(1) = nb_elem

   last => get_ll_connectivity_mecaMAILx

   !cc_elem_bdyty(ibdyty+1) = cc_elem_bdyty(ibdyty) + nb_elem
   do iblmty = 1, nb_elem
     allocate( new )
     allocate( new%connec(size(bdyty(ibdyty)%blmty(iblmty)%NODES)) )
     new%connec = bdyty(ibdyty)%blmty(iblmty)%NODES
     last%n => new
     last   => new
   end do

 end function

 function get_connectivity_mecaMAILx(ibdyty)
   implicit none
   integer :: ibdyty
   integer(kind=4),dimension(:),pointer :: get_connectivity_mecaMAILx
   ! ***
   integer :: sz,iblmty,inode

   get_connectivity_mecaMAILx => null()

   ! on compte
   sz=1
   do iblmty=1,size(bdyty(ibdyty)%blmty)
     sz = sz + size(bdyty(ibdyty)%blmty(iblmty)%nodes) + 1
   enddo

   ! on alloue
   allocate(get_connectivity_mecaMAILx(sz)) 

   ! on rempli 
   sz=1
   get_connectivity_mecaMAILx(sz) = size(bdyty(ibdyty)%blmty)
   do iblmty=1,size(bdyty(ibdyty)%blmty)
     sz = sz + 1
     get_connectivity_mecaMAILx(sz)=size(bdyty(ibdyty)%blmty(iblmty)%nodes)
     do inode=1,size(bdyty(ibdyty)%blmty(iblmty)%nodes)
       sz = sz + 1
       get_connectivity_mecaMAILx(sz) = bdyty(ibdyty)%blmty(iblmty)%nodes(inode)
     enddo
   enddo

 end function

 !> get the material id of each element
 subroutine get_materials_mecaMAILx(i_bdyty, materials, nb_elem)
    implicit none
    integer, intent(in) :: i_bdyty, nb_elem
    integer, dimension(nb_elem) :: materials
    !
    integer :: i_blmty

    if( nb_elem /= size(bdyty(i_bdyty)%blmty) ) then
      call faterr('[mecaMAILx::get_materials]','wrong size of vector')
    end if

    do i_blmty = 1, nb_elem
      materials( i_blmty ) = bdyty(i_bdyty)%blmty(i_blmty)%lawnb
    end do

 end subroutine

 !> compute the volume of each element
 function Get_Elements_Volume(ibdyty)
    implicit none 
    integer :: ibdyty
    !> volumes, array created outside
    real(kind=8),dimension(:),pointer :: Get_Elements_Volume
    ! ***
    !                         123456789012345678901234567890
    character(len=30) :: IAM='mecaMAILx::Get_Elements_Volume'
    INTEGER :: errare,iblmty
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    real(kind=8) :: volume

    Get_Elements_Volume => null()

    allocate(Get_Elements_Volume(size(bdyty(ibdyty)%blmty))) 

    do iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      END IF
 
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
        coor_ele=get_cooref_local_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      ELSE 
        coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      ENDIF

      call compute_elementary_volume(bdyty(ibdyty)%blmty(iblmty)%blmnb,coor_ele,volume)

      Get_Elements_Volume(iblmty) = volume

      deallocate(coor_ele)
    enddo

 end function Get_Elements_Volume
  
 !> compute the center of each element
 function Get_Elements_Center(ibdyty)
    implicit none 
    integer :: ibdyty
    
    real(kind=8), dimension(nbdime) :: center
    !> centers
    real(kind=8),dimension(:),pointer :: Get_Elements_Center
    ! ***
    !                         123456789012345678901234567890
    character(len=30) :: IAM='mecaMAILx::Get_Elements_Center'
    INTEGER :: errare,iblmty
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele

    Get_Elements_Center => null()

    allocate(Get_Elements_Center(nbdime*size(bdyty(ibdyty)%blmty))) 

    do iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      END IF
 
      IF (bdyty(ibdyty)%is_rigid .OR. bdyty(ibdyty)%is_coro) THEN
        coor_ele=get_cooref_local_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      ELSE 
        coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      ENDIF

      call compute_elementary_center(bdyty(ibdyty)%blmty(iblmty)%blmnb, coor_ele, center)
      
      Get_Elements_Center(1+nbdime*(iblmty-1) : nbdime*iblmty) = center

      deallocate(coor_ele)
    enddo

 end function

 !> compute the volume of each element
 function Get_Elements_Jacobian(ibdyty)
    implicit none 
    integer :: ibdyty
    !> volumes, array created outside
    real(kind=8),dimension(:),pointer :: Get_Elements_Jacobian
    ! ***
    !                         12345678901234567890123456789012
    character(len=32) :: IAM='mecaMAILx::Get_Elements_Jacobian'
    INTEGER :: errare,iblmty,iM_bdyty,iM_blmty
    real(kind=8) :: jacobian

    Get_Elements_Jacobian => null()

    allocate(Get_Elements_Jacobian(size(bdyty(ibdyty)%blmty))) 

    iM_bdyty=bdyty2M_bdyty(ibdyty)

    do iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      if (bdyty(ibdyty)%eviz(iblmty) ==  0) then
         Get_Elements_Jacobian(iblmty) = 0.d0
         cycle
      endif

      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      call compute_elementary_jacobian(bdyty(ibdyty)%blmty(iblmty)%blmnb,bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                       iM_bdyty,iM_blmty,jacobian)

      Get_Elements_Jacobian(iblmty) = jacobian

    enddo

 end function

 function get_ptr_elements_energy(ibdyty)
    implicit none 
    integer, intent(in) :: ibdyty
    real(kind=8), dimension(:), pointer :: get_ptr_elements_energy
    !
    integer :: nb_elem

    get_ptr_elements_energy => null()

    if( ibdyty < 0 .or. ibdyty > nb_mecaMAILx ) return

    if( .not. associated( bdyty(ibdyty)%elem_energy ) ) then
      nb_elem = size(bdyty(ibdyty)%blmty)
      allocate( bdyty(ibdyty)%elem_energy(nb_elem) )
      bdyty(ibdyty)%elem_energy = 0.d0
    else
      nb_elem = size(bdyty(ibdyty)%blmty)
      if( size(bdyty(ibdyty)%elem_energy) /= nb_elem ) then
        deallocate( bdyty(ibdyty)%elem_energy )
        allocate( bdyty(ibdyty)%elem_energy(nb_elem) )
        bdyty(ibdyty)%elem_energy = 0.d0
      end if
    end if

    get_ptr_elements_energy => bdyty(ibdyty)%elem_energy

 end function

 !> compute the energy of each element
 subroutine comp_elements_energy(ibdyty)
    implicit none 
    integer, intent(in) :: ibdyty
    ! ***
    integer      :: nb_elem, nb_no, nbdof
    integer      :: iblmty, iM_blmty, iM_bdyty
    integer      :: i, id, inodty, iccdof
    real(kind=8) :: E_def, E_cin !, E_pot
    real(kind=8), dimension(:)  , allocatable :: V_ele
    real(kind=8), dimension(:,:), allocatable :: coor_ele
    !                                      1234567890123456789012345678901
    character(len=31), parameter :: IAM = 'mecaMAILx::comp_elements_energy'

    !INTEGER :: errare,iblmty,idof,nbdof,inodty,iccdof,i,iM_bdyty,iM_blmty,id
    !REAL(kind=8),DIMENSION(:),ALLOCATABLE :: V_ele
    !REAL(kind=8) :: E_pot,E_def,E_cin

    if( ibdyty < 0 .or. ibdyty > nb_mecaMAILx ) return

    nb_elem = size(bdyty(ibdyty)%blmty)

    ! check realloc too ?
    if( .not. associated( bdyty(ibdyty)%elem_energy ) ) then
      allocate( bdyty(ibdyty)%elem_energy(nb_elem) ) 
    else
      if( size(bdyty(ibdyty)%elem_energy) /= nb_elem ) then
        deallocate( bdyty(ibdyty)%elem_energy )
        allocate( bdyty(ibdyty)%elem_energy(nb_elem) )
      end if
    end if

    iM_bdyty = bdyty2M_bdyty(ibdyty)

    do iblmty = 1, nb_elem

      if (bdyty(ibdyty)%eviz(iblmty) ==  0) then
         bdyty(ibdyty)%elem_energy(iblmty) = 0.d0
         cycle
      endif
 
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

      nb_no = size(bdyty(ibdyty)%blmty(iblmty)%NODES)
      nbdof = bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

      if( .not. allocated( coor_ele ) ) then
        allocate( coor_ele(nbDIME,nb_no) )
      else
        if( size(coor_ele,2) /= nb_no ) then
          deallocate( coor_ele )
          allocate( coor_ele(nbDIME,nb_no) )
        end if
      end if

      if( .not. allocated( V_ele ) ) then
        allocate( V_ele(nbdof*nb_no) )
      else
        if( size(V_ele) /= nbdof*nb_no ) then
          deallocate( V_ele )
          allocate( V_ele(nbdof*nb_no) )
        end if
      end if

      coor_ele = get_cooref_ele(ibdyty, iblmty, nb_no)

      ! dof counting
      id = 0

      do i = 1, nb_no
         ! global numbering
         inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i)
         ! global dof index position of desired node
         iccdof = bdyty(ibdyty)%ccdof(inodty)
         V_ele(id+1:id+nbdof) = bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
         id = id + nbdof
      end do

      call compute_elementary_energy(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     iM_bdyty, iM_blmty, &
                                     coor_ele, E_def )

      bdyty(ibdyty)%E_cin = bdyty(ibdyty)%E_cin + &
                            0.5d0*dot_product(V_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%mass,V_ele))

      bdyty(ibdyty)%elem_energy(iblmty) = E_def + E_cin !+ E_pot



    end do

    deallocate(coor_ele, v_ele)

 end subroutine

 !> Get a pointer on the body array visibility 
 function GetPtr_Elements_Visibility(ibdyty)
    implicit none 
    integer :: ibdyty
    integer,dimension(:),pointer :: GetPtr_Elements_Visibility  

    GetPtr_Elements_Visibility => bdyty(ibdyty)%eviz    

 end function

 function Get_Elements_Neighbor(ibdyty, tol) 
    use ann, ann_clean_memory => clean_memory
    implicit none
    integer     , intent(in) :: ibdyty
    real(kind=8), intent(in) :: tol
    integer     , dimension(:,:), pointer :: Get_Elements_Neighbor
    ! ***
    integer      :: nb_ele, nb_no, iblmty, max_neighbors
    real(kind=8), dimension(nbdime) :: center
    real(kind=8), dimension(:,:), allocatable :: coor_ele
    real(kind=8), dimension(:,:), pointer     :: centers, dists
                                          !12345678901234567890123456789012
    character(len=32), parameter :: IAM = 'mecaMAILx::Get_Elements_Neighbor'

    nb_ele = SIZE(bdyty(ibdyty)%blmty)

    Get_Elements_Neighbor => null()
    centers => null()
    dists   => null()

    ! initializations

    ! first generate the center of all elements

    allocate( centers(nbdime, nb_ele) )

    do iblmty = 1, nb_ele

      nb_no = size(bdyty(ibdyty)%blmty(iblmty)%NODES)

      ! only allocate if needed
      if( .not. allocated(coor_ele) ) then
        allocate( coor_ele(nbDIME, nb_no) )
      else
        if( size(coor_ele) /= nb_no ) then
          deallocate( coor_ele )
          allocate( coor_ele(nbDIME, nb_no) )
        end if
      end if

      if (bdyty(ibdyty)%is_rigid .or. bdyty(ibdyty)%is_coro) then
        coor_ele = get_cooref_local_ele(ibdyty, iblmty, nb_no)
      else 
        coor_ele = get_cooref_ele(ibdyty, iblmty, nb_no)
      end if

      call compute_elementary_center(bdyty(ibdyty)%blmty(iblmty)%blmnb, coor_ele, center)

      centers(:,iblmty) = center(:)

    enddo
    ! before forgetting
    deallocate(coor_ele)


    !write(*,'(3(1x,D12.5))') centers

    ! setting ANN library:
    call set_nb_kd(1)
    call add_kd_tree(1, centers, nb_ele, nbDIME)

    ! recherche des voisins
    call radii_search_kd_tree(1, centers, nb_ele, tol, Get_Elements_Neighbor, dists, max_neighbors)

    ! remove first element which is identity in this case:
    Get_Elements_Neighbor(1:max_neighbors-1,:) = Get_Elements_Neighbor(2:max_neighbors,:)
    Get_Elements_Neighbor(max_neighbors,:) = 0

    deallocate(centers)
    call ann_clean_memory()
    ! beurk beurk beurk
    deallocate(dists)

 end function


  !> add the divergence of a diagonal tensorial field to external force
  SUBROUTINE add_field_divergence_mecaMAILx(ibdyty, ivfield)
  IMPLICIT NONE

  INTEGER                                 :: ibdyty,ivfield
  integer                                 :: iblmty,nbdof,NbNo,iM_bdyty,iM_blmty,inodty,iM_nodty,iccdof
  INTEGER                                 :: errare,i

  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: fext_ele
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele,vfield_ele
                                                 !1234567890123456789012345678901
  CHARACTER(len=41)                       :: IAM='mecaMAILx::add_field_divergence'

  IF (nb_mecaMAILx == 0) RETURN

  !
    !print *,'add_field_divergence_mecaMAILx on body : ',ibdyty
    !print *,'field coming from nodal field : '         ,itfield

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      
      NbNo = SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
      
      IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
      ALLOCATE(coor_ele(nbDIME,NbNo),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating coor_ele')
      ENDIF
    
      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node

      if (nbdof /= nbdime) call FATERR(IAM,'mismatch nbdof nbdime')

      IF (ALLOCATED(fext_ele)) DEALLOCATE(fext_ele)
      ALLOCATE(fext_ele(nbdof*NbNo),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating fext_ele')
      ENDIF
    
      IF (ALLOCATED(vfield_ele)) DEALLOCATE(vfield_ele)
      ALLOCATE(vfield_ele(NbNo,nbDIMe),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating vfield_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,NbNo) 

      vfield_ele = get_vfield_ele(ibdyty,iblmty,NbNo,nbdime,ivfield)

      !
      ! on calcule les sources interieures
      ! 
      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
      
      !print*,iblmty
      !print*,'field '
      !do i=1,size(vfield_ele,dim=1)
      !  write(6,'(3(1x,D12.5))') vfield_ele(i,:) 
      !enddo
      CALL compute_elementary_field_divergence_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,&
                                                      coor_ele, vfield_ele,fext_ele)

      !print*,'fext '
      !write(6,'(3(1x,D12.5))') fext_ele 
      !                             
      ! On ajoute cette contribution à la fin du pas
      !

      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
        ! position un dans le vecteur global pour le numero inodty     
        iccdof=bdyty(ibdyty)%ccdof(inodty)
        ! 

        bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) + &
                                                    (fext_ele((i-1)*nbdof+1:i*nbdof))
        !write(*,*) '     fe : ', bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof)
      END DO

      DEALLOCATE(fext_ele)
                                   
    ENDDO
 END SUBROUTINE add_field_divergence_mecaMAILx

 !------------------------------------------------------------------------
 FUNCTION get_vfield_ele(ibdyty,iblmty,nbNODES,sz,rank)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES,sz,rank
  INTEGER                     :: iM_bdyty,iM_nodty

  REAL(kind=8),DIMENSION(nbNODES,sz) :: get_vfield_ele

  !print*,ibdyty,iblmty,nbNODES,sz,rank

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  DO inodes=1,nbNODES

    !print*,inodes
  
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    !print*,inodty,iM_nodty

    get_vfield_ele(inodes,1:sz)=get_nodal_field_nodty_MAILx(iM_bdyty,iM_nodty,sz,rank)

    !print*,get_vfield_ele(inodes,1:sz)

    !print*,'---'

  ENDDO

END FUNCTION get_vfield_ele

 !------------------------------------------------------------------------ 
 !> routine qui calcule le min|mean|max|<min>|<max> des contraintes principales
 !> min is the min value of all gp 
 !> mean is the mean pressure
 !> max is the max value of all the gp
 !> <min> is the mean value of the min pstress of all gp
 !> <max> is the mean value of the max pstress of all gp     
  function Compute_Info_PrincipalStressField_mecaMAILx(ibdyty)

    IMPLICIT NONE

    real(kind=8),dimension(5) :: Compute_Info_PrincipalStressField_mecaMAILx
    integer :: ibdyty 

    !***         
    ! nom de la fonction        12345678901234567890123456789012345678901234567890123
    CHARACTER(len=53) :: IAM = 'mod_mecaMAILx::compute_info_principalstress_mecaMAILx' 
    ! les donnees extraitent des pg
    INTEGER :: nbgp,ig,iblmty,imodel,nbgpt,iM_bdyty,iM_blmty,errare
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: field ! sigma, s1, s2, s3,
    REAL(kind=8),DIMENSION(3,3)             :: stress 

    REAL(kind=8),DIMENSION(3,3) ::m33
    REAL(kind=8),DIMENSION(3)  ::v3 
    REAL(kind=8) :: smin,smean,smax,msmin,msmax

    nbgpt=0 
    smin=1d+20
    msmin=0.d0
    smean=0.
    smax=-1d+20 
    msmax=0.d0
    
    iM_bdyty=bdyty2M_bdyty(ibdyty)

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
      nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      if( allocated(field) .and. nbgp /= size(field,2) ) deallocate(field)
      if( .not. allocated(field) ) ALLOCATE(field(7,nbgp),stat=errare)
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating field')
      ENDIF

      field = 0.d0

      ! on remonte un tenseur de Cauchy 3D (xx,xy,yy,xz,yz,zz) meme si on est en 2D
      CALL get_ele_gp_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                             bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                             iM_bdyty,iM_blmty, &
                             2,Field,7)


      DO ig=1,nbgp
 
        stress(1,1) = field(1,ig) ; stress (1,2) = field(2,ig) ; stress (1,3) = field(4,ig)     
        stress(2,1) = field(2,ig) ; stress (2,2) = field(3,ig) ; stress (2,3) = field(5,ig)     
        stress(3,1) = field(4,ig) ; stress (3,2) = field(5,ig) ; stress (3,3) = field(6,ig)     

        !print*,'--------------------------------'
        !print*,iblmty,ig
        !print*,'stress'
        !write(*,'(3(1x,E12.5))') stress

        CALL diagonalise33(stress,v3,m33)

        nbgpt = nbgpt + 1
        smean = smean + ((v3(1)+v3(2)+v3(3))/3.d0) 
        smin = min(smin,minval(v3))
        msmin = msmin + minval(v3)
        smax = max(smax,maxval(v3))
        msmax = msmax + maxval(v3)

      enddo
    enddo

    smean = smean / real(nbgpt)
    msmin = msmin / real(nbgpt)
    msmax = msmax / real(nbgpt)

    Compute_Info_PrincipalStressField_mecaMAILx = (/smin, smean, smax, msmin, msmax/)
    deallocate(field)

  end function Compute_Info_PrincipalStressField_mecaMAILx

 !------------------------------------------------------------------------ 
 !> routine qui calcule la pdf des contraintes
  function compute_PDF_pressure_mecaMAILx()

    IMPLICIT NONE

    real(kind=8),dimension(44) :: Compute_PDF_pressure_mecaMAILx

    !***         
    ! nom de la fonction        12345678901234567890123456789012345678901234567890123
    CHARACTER(len=53) :: IAM = 'mod_mecaMAILx::Compute_PDF_pressure_mecaMAILx'
    
    ! les donnees extraitent des pg
    INTEGER :: nbgp,ig,iblmty,ibdyty,imodel,nbgpt,iM_bdyty,iM_blmty,errare,id,i,nbp
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: field ! sigma, s1, s2, s3,
    REAL(kind=8),DIMENSION(3,3)             :: stress 

    REAL(kind=8),DIMENSION(3,3) :: m33
    REAL(kind=8),DIMENSION(3)   :: v3 
    REAL(kind=8)                :: p,pp,dp,pmin,pmax
    REAL(kind=8)                :: np,npp,ndp,npmin,npmax    

    real(kind=8),dimension(:,:),pointer :: xx
    
    pmin=1d+20
    pmax=-1d+20 
    npmin=1d+20
    npmax=-1d+20 

    do ibdyty=1,nb_mecaMAILx
    
      iM_bdyty=bdyty2M_bdyty(ibdyty)

      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

        imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
        nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)
        iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

        if( allocated(field) .and. nbgp /= size(field,2) ) deallocate(field)
        if( .not. allocated(field) ) ALLOCATE(field(7,nbgp),stat=errare)
        IF (errare /= 0) THEN
          CALL FATERR(IAM,'allocating field')
        ENDIF

        field = 0.d0

        ! on remonte un tenseur de Cauchy 3D (xx,xy,yy,xz,yz,zz) meme si on est en 2D
        CALL get_ele_gp_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                               bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                               iM_bdyty,iM_blmty, &
                               2,Field,7)


        DO ig=1,nbgp
   
          stress(1,1) = field(1,ig) ; stress (1,2) = field(2,ig) ; stress (1,3) = field(4,ig)     
          stress(2,1) = field(2,ig) ; stress (2,2) = field(3,ig) ; stress (2,3) = field(5,ig)     
          stress(3,1) = field(4,ig) ; stress (3,2) = field(5,ig) ; stress (3,3) = field(6,ig)     

          !print*,'--------------------------------'
          !print*,iblmty,ig
          !print*,'stress'
          !write(*,'(3(1x,E12.5))') stress
          CALL diagonalise33(stress,v3,m33)
          
          p = (v3(1)+v3(2)+v3(3))/3.d0
          pp = (field(1,ig)+field(3,ig)+field(6,ig))/3.d0

          if (p-pp > 1e6) call faterr(IAM,'pb with pressure')
          
          pmin = min(pmin,p)
          pmax = max(pmax,p)

        enddo
      enddo
      
      xx => get_all_mecaMAILx(ibdyty)
      do i=1,size(xx,dim=2)
         np=(xx(13+1,i)+xx(13+3,i)+xx(13+6,i))/3.d0
         npmin = min(npmin,np)
         npmax = max(npmax,np)
      enddo
      deallocate(xx)
      
    enddo

    dp = (pmax-pmin)/20.d0
    ndp = (npmax-npmin)/20.d0
    
    compute_PDF_pressure_mecaMAILx = 0.d0
    compute_PDF_pressure_mecaMAILx(21) = pmin
    compute_PDF_pressure_mecaMAILx(22) = pmax    
    compute_PDF_pressure_mecaMAILx(43) = npmin
    compute_PDF_pressure_mecaMAILx(44) = npmax    
    
    nbgpt=0
    nbp=0

    do ibdyty=1,nb_mecaMAILx
    
      iM_bdyty=bdyty2M_bdyty(ibdyty)

      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

        imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
        nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)
        iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

        if( allocated(field) .and. nbgp /= size(field,2) ) deallocate(field)
        if( .not. allocated(field) ) ALLOCATE(field(7,nbgp),stat=errare)
        IF (errare /= 0) THEN
          CALL FATERR(IAM,'allocating field')
        ENDIF

        field = 0.d0

        ! on remonte un tenseur de Cauchy 3D (xx,xy,yy,xz,yz,zz) meme si on est en 2D
        CALL get_ele_gp_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                               bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                               iM_bdyty,iM_blmty, &
                               2,Field,7)
        
        DO ig=1,nbgp
   
          nbgpt = nbgpt + 1

          pp = (field(1,ig)+field(3,ig)+field(6,ig))/3.d0

          id = ceiling((pp-pmin)/dp)
          !managing roundig error
          id = max(1,id)
          id=  min(20,id)

          compute_PDF_pressure_mecaMAILx(id) = compute_PDF_pressure_mecaMAILx(id) + 1

        enddo
      enddo

      xx => get_all_mecaMAILx(ibdyty)
      do i=1,size(xx,dim=2)
        np=(xx(13+1,i)+xx(13+3,i)+xx(13+6,i))/3.d0
        id = ceiling((np-npmin)/ndp)
        !managing roundig error
        id = max(1,id)
        id=  min(20,id)

        compute_PDF_pressure_mecaMAILx(22+id) = compute_PDF_pressure_mecaMAILx(22+id) + 1

      enddo

      nbp = nbp + size(xx,dim=2)
      deallocate(xx)
      
    enddo

    compute_PDF_pressure_mecaMAILx(1:20) = compute_PDF_pressure_mecaMAILx(1:20) / nbgpt
    compute_PDF_pressure_mecaMAILx(22+1:22+20) = compute_PDF_pressure_mecaMAILx(22+1:22+20) / nbp

    deallocate(field)

  end function compute_PDF_pressure_mecaMAILx

 !------------------------------------------------------------------------ 

  !> compute the coordinates of all gp of a mecaMAILx
  function get_gp_coor_mecaMAILx(ibdyty)
     implicit none
     integer :: ibdyty
     real(kind=8), dimension(:,:), pointer :: get_gp_coor_mecaMAILx
     ! ***
     !                         1234567890123456789012
     character(len=22) :: IAM='mecaMAILx::get_gp_coor'
     integer :: iblmty, iM_bdyty, iM_blmty
     integer :: nb_gp, i_gp
     real(kind=8), dimension(:,:), allocatable :: coor_ele

     get_gp_coor_mecaMAILx => null()

     nb_gp = 0
     do iblmty = 1, size(bdyty(ibdyty)%blmty)
       nb_gp = nb_gp + get_nb_gp_mecaMAILx(ibdyty, iblmty)
     end do


     allocate( get_gp_coor_mecaMAILx(nbDIME, nb_gp) )

     i_gp = 0
     do iblmty = 1, size(bdyty(ibdyty)%blmty)
       !
       allocate( coor_ele(nbDIME, size(bdyty(ibdyty)%blmty(iblmty)%NODES)) )

       coor_ele = get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES))

       nb_gp  = get_nb_gp_mecaMAILx(ibdyty, iblmty)
       get_gp_coor_mecaMAILx(:,i_gp+1:i_gp+nb_gp) = 0.d0

       iM_bdyty = bdyty2M_bdyty(ibdyty)
       iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

       call get_ele_gp_coor(bdyty(ibdyty)%blmty(iblmty)%blmnb        , &
                            bdyty(ibdyty)%blmty(iblmty)%ppsnb        , &
                            iM_bdyty, iM_blmty, coor_ele             , &
                            get_gp_coor_mecaMAILx(:,i_gp+1:i_gp+nb_gp)   )
        i_gp = i_gp + nb_gp
        deallocate( coor_ele )
      end do

  end function

  function Get_gp_PrincipalField(ibdyty,iblmty,ig,ifield)
    !
    ! routine qui recupere les contraintes principales en un point de gauss
    !
    IMPLICIT NONE

    real(kind=8),dimension(12) :: Get_gp_PrincipalField
    integer :: ibdyty,iblmty,ig,ifield

    !***         
    ! nom de la fonction        123456789012345678901234567890123456
    CHARACTER(len=36) :: IAM = 'mod_mecaMAILx::get_gp_principalfield' 

    ! les donnees extraitent des pg
    
    INTEGER :: iM_bdyty,iM_blmty,imodel,nbgp,errare,i,j

    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: field ! stress or strain 
    REAL(kind=8),DIMENSION(3,3)  :: tensor 

    REAL(kind=8),DIMENSION(3,3)  :: m33
    REAL(kind=8),DIMENSION(3)    :: v3
    
    iM_bdyty=bdyty2M_bdyty(ibdyty)
    iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

    imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
    nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)

    if ( allocated(field) .and. nbgp /= size(field,2) ) deallocate(field)
    if ( .not. allocated(field) ) ALLOCATE(field(7,nbgp),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'allocating field')
    ENDIF
    
    field = 0.d0

    ! on remonte un tenseur de Cauchy 3D (xx,xy,yy,xz,yz,zz) meme si on est en 2D
    CALL get_ele_gp_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                           bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                           iM_bdyty,iM_blmty, &
                           ifield,field,7)
    if (ifield == 1) then
      tensor(1,1) =       field(1,ig) ; tensor (1,2) = 0.5d0*field(2,ig) ; tensor (1,3) = 0.5d0*field(4,ig)     
      tensor(2,1) = 0.5d0*field(2,ig) ; tensor (2,2) =       field(3,ig) ; tensor (2,3) = 0.5d0*field(5,ig)     
      tensor(3,1) = 0.5d0*field(4,ig) ; tensor (3,2) = 0.5d0*field(5,ig) ; tensor (3,3) =       field(6,ig)
    else    
      tensor(1,1) = field(1,ig) ; tensor (1,2) = field(2,ig) ; tensor (1,3) = field(4,ig)     
      tensor(2,1) = field(2,ig) ; tensor (2,2) = field(3,ig) ; tensor (2,3) = field(5,ig)     
      tensor(3,1) = field(4,ig) ; tensor (3,2) = field(5,ig) ; tensor (3,3) = field(6,ig)     
    endif
   
    CALL diagonalise33(tensor,v3,m33)

    get_gp_PrincipalField(1:3) = v3(1:3)
    do j=1,3
      do i=1,3
        get_gp_PrincipalField(3*j+i)=m33(i,j)   
      enddo
    enddo  
    deallocate(field)

  end function Get_gp_PrincipalField

  function Get_gp_Internals(ibdyty,iblmty,ig)
    !
    ! routine qui recupere une valeur au point de gauss
    !
    IMPLICIT NONE

    real(kind=8),dimension(:),pointer :: Get_gp_internals
    integer :: ibdyty,iblmty,ig

    !***         
    ! nom de la fonction        1234567890123456789012345678901
    CHARACTER(len=31) :: IAM = 'mod_mecaMAILx::get_gp_internals' 

    ! les donnees extraitent des pg
    INTEGER :: imodel,nbgp,iM_bdyty,iM_blmty,errare,fieldsize

    REAL(kind=8),DIMENSION(:,:),pointer :: field

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
       
    imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
    nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)

    if ( ig > nbgp ) call faterr(IAM,'wrong gp number')


    field => null()
      
    ! on remonte les internals aux pg
    CALL get_ele_gp_all_internal(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                 bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                 iM_bdyty,iM_blmty, &
                                 field,fieldsize)
    
    Get_gp_Internals => null()

    if (fieldsize > 0) then
    
      allocate(get_gp_internals(fieldsize),stat=errare)
      IF (errare /= 0) THEN
        CALL faterr(IAM,'error allocating get_gp_internals')
      END IF

      get_gp_internals=field(:,ig)
      
    endif  

    if ( associated(field) ) deallocate(field)

  end function Get_gp_Internals

  function Get_Elements_Internal(ibdyty,i,flag)
    !
    ! routine qui calcule une quantite sur chaque element a partir d'une variable interne stockee aux points de Gauss
    ! flag= 1: mean, 2: sum, 3:max, 4: min
    !
    IMPLICIT NONE

    real(kind=8),dimension(:),pointer :: Get_Elements_internal
    integer :: ibdyty,i,flag 

    !***         
    ! nom de la fonction        123456789012345678901234567890123456
    CHARACTER(len=36) :: IAM = 'mod_mecaMAILx::get_elements_internal' 
    ! les donnees extraitent des pg
    INTEGER :: iblmty,imodel,nbgp,iM_bdyty,iM_blmty,errare,fieldsize
    REAL(kind=8),DIMENSION(:,:),pointer :: field

    Get_Elements_Internal => null()

    allocate(get_elements_internal(SIZE(bdyty(ibdyty)%blmty)),stat=errare)
    IF (errare /= 0) THEN
      CALL LOGMES('Error '//IAM//': allocating get_elements_internal')
    END IF

    get_elements_internal = 0.d0

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
       
      imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
      nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)

      if ( associated(field) ) deallocate(field)
      field => null()
      
      ! on remonte les internals aux pg
      CALL get_ele_gp_all_internal(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                             bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                             iM_bdyty,iM_blmty, &
                             field,fieldsize)
      
      ! on verifie quand meme que le champs existe 
      if (fieldsize >= i) then
         select case(flag)
         case(1)   
           get_elements_internal(iblmty)=sum(field(i,:))/nbgp
         case(2)
           get_elements_internal(iblmty)=sum(field(i,:))
         case(3) 
           get_elements_internal(iblmty)=maxval(field(i,:))
         case(4)
           get_elements_internal(iblmty)=minval(field(i,:))
         case default
           call faterr(IAM,' unexpected flag 1: mean, 2: sum, 3:max, 4: min')
         end select            
      endif

    enddo
  end function Get_elements_Internal

  function Get_Elements_Internal_integral(ibdyty,i)
    !
    ! routine qui intègre une variable interne stockee aux points de Gauss
    !
    IMPLICIT NONE

    real(kind=8),dimension(:),pointer :: Get_Elements_internal_integral
    integer :: ibdyty,i

    !***         
    ! nom de la fonction        123456789012345678901234567890123456789012345
    CHARACTER(len=45) :: IAM = 'mod_mecaMAILx::get_elements_internal_integral' 

    ! 
    INTEGER :: iblmty,iM_bdyty,iM_blmty,errare

    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    real(kind=8) :: val
    
    Get_Elements_Internal_integral => null()

    allocate(get_elements_internal_integral(SIZE(bdyty(ibdyty)%blmty)),stat=errare)
    IF (errare /= 0) THEN
      CALL LOGMES('Error '//IAM//': allocating get_elements_internal_integral')
    END IF

    get_elements_internal_integral = 0.d0

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL LOGMES('Error '//IAM//': allocating coor_ele')
      END IF

      !! beurk il y a des trucs pas secure entre nodty et DIME !!

      coor_ele=get_coor_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 


      ! on remonte les internals aux pg
      CALL get_ele_internal_integral(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     iM_bdyty,iM_blmty, &
                                     coor_ele,i,val)

      Get_Elements_Internal_integral(iblmty) = val

      deallocate(coor_ele) 
      
    enddo
  end function Get_elements_Internal_integral

  
  function get_nb_dofs_mecaMAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4) :: get_nb_dofs_mecaMAILx

    get_nb_dofs_mecaMAILx = bdyty(ibdyty)%nbdof

  end function get_nb_dofs_mecaMAILx

  function get_nb_nz_g_sys_mecaMAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4) :: get_nb_nz_g_sys_mecaMAILx

    get_nb_nz_g_sys_mecaMAILx = get_nb_non_zero(bdyty(ibdyty)%g_sys)

  end function get_nb_nz_g_sys_mecaMAILx

  subroutine get_i_indices_mecaMAILx(ibdyty, i_list)
    implicit none
    integer(kind=4), intent(in)   :: ibdyty
    integer(kind=4), dimension(:) :: i_list

    call get_i_indices(bdyty(ibdyty)%g_sys,i_list)

  end subroutine get_i_indices_mecaMAILx

  subroutine get_j_indices_mecaMAILx(ibdyty, j_list)
    implicit none
    integer(kind=4), intent(in)   :: ibdyty
    integer(kind=4), dimension(:) :: j_list

    call get_j_indices(bdyty(ibdyty)%g_sys,j_list)

  end subroutine get_j_indices_mecaMAILx

  subroutine get_val_g_sys_mecaMAILx(ibdyty, val_list)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    real(kind=8), dimension(:)  :: val_list

    call get_val(bdyty(ibdyty)%g_sys,val_list)

  end subroutine get_val_g_sys_mecaMAILx

  subroutine get_rhs_vector_mecaMAILx(ibdyty, rhs_list)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    real(kind=8), dimension(:)  :: rhs_list

    call get_vector(bdyty(ibdyty)%g_sys,rhs_list)
    !fd bug rhs_list = rhs_list + bdyty(ibdyty)%momentum

  end subroutine get_rhs_vector_mecaMAILx

  subroutine get_dof_node_mecaMAILx(ibdyty,inodty,iccdof,nbdof)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,inodty
    integer(kind=4) :: iccdof,nbdof
        
    iccdof=bdyty(ibdyty)%ccdof(inodty)
    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

  end subroutine

  function get_nb_internal_mecaMAILx(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4) :: get_nb_internal_mecaMAILx
    integer(kind=4) :: iblmty,imodel,nb_internal

    get_nb_internal_mecaMAILx = 0
    do iblmty=1,size(bdyty(ibdyty)%blmty)
  
      imodel=bdyty(ibdyty)%blmty(iblmty)%mdlnb
      nb_internal = modelz(imodel)%nb_internal_variables
      IF (nb_internal > get_nb_internal_mecaMAILx) THEN
         get_nb_internal_mecaMAILx = nb_internal
      ENDIF
  
    enddo 

  end function get_nb_internal_mecaMAILx

 !> computes the deformation energy due to a given field
 function Get_Deformation_Energy_mecaMAILx(ibdyty,dep)
    implicit none 
    integer :: ibdyty
    real(kind=8),dimension(:) :: dep
    real(kind=8) :: Get_Deformation_Energy_mecaMAILx
    ! ***
    !                         123456789012345678901234567890123
    character(len=33) :: IAM='mecaMAILx::Get_Deformation_Energy'

    INTEGER :: errare,iblmty,idof,nbdof,inodty,iccdof,i

    REAL(kind=8), DIMENSION(:), pointer :: coor_ele,dep_ele,dual_ele
    REAL(kind=8), DIMENSION(:,:), pointer :: operator_ele


    Get_Deformation_Energy_mecaMAILx = 0.d0

    do iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      if (bdyty(ibdyty)%eviz(iblmty) ==  0) cycle

      coor_ele => null()
      dep_ele => null()
      dual_ele => null()
      operator_ele => null()

      call get_ele_ptr_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,coor_ele,dep_ele,dual_ele,operator_ele)
 
      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   

      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
        ! passage au numero global
        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
        iccdof=bdyty(ibdyty)%ccdof(inodty) 
        idof = (i-1)*nbdof
        dep_ele(idof+1:idof+nbdof)=dep(iccdof+1:iccdof+nbdof)
      END DO

      ! print*,ibdyty,iblmty
      ! print*,dep_ele
      ! print*,bdyty(ibdyty)%blmty(iblmty)%stiffness
      ! print*,'-----'
      Get_Deformation_Energy_mecaMAILx = Get_Deformation_Energy_mecaMAILx + &
         (0.5*dot_product(dep_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%stiffness,dep_ele)))

    enddo

 end function

 !> computes the kinetic energy due to a given velocity field
 function Get_Kinetic_Energy_mecaMAILx(ibdyty,vel)
    implicit none 
    integer :: ibdyty
    real(kind=8),dimension(:) :: vel
    real(kind=8) :: Get_Kinetic_Energy_mecaMAILx
    ! ***
    !                         12345678901234567890123456789
    character(len=29) :: IAM='mecaMAILx::Get_Kinetic_Energy'

    INTEGER :: errare,iblmty,idof,nbdof,inodty,iccdof,i

    REAL(kind=8), DIMENSION(:), pointer :: coor_ele,primal_ele,dual_ele
    REAL(kind=8), DIMENSION(:,:), pointer :: operator_ele


    Get_Kinetic_Energy_mecaMAILx = 0.d0

    do iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      if (bdyty(ibdyty)%eviz(iblmty) ==  0) cycle

      coor_ele => null()
      primal_ele => null()
      dual_ele => null()
      operator_ele => null()

      call get_ele_ptr_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,coor_ele,primal_ele,dual_ele,operator_ele)
 
      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   

      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
        ! passage au numero global
        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
        iccdof=bdyty(ibdyty)%ccdof(inodty) 
        idof = (i-1)*nbdof
        primal_ele(idof+1:idof+nbdof)=vel(iccdof+1:iccdof+nbdof)
      END DO

      Get_Kinetic_Energy_mecaMAILx = Get_Kinetic_Energy_mecaMAILx + &
         (0.5*dot_product(primal_ele,matmul(bdyty(ibdyty)%blmty(iblmty)%mass,primal_ele)))

    enddo

 end function

 !> returns the list of neighbor elements to an element
 function get_ptr_neighborEle2ele_mecaMAILx(ibdyty,iblmty)
    implicit none 
    integer :: ibdyty,iblmty
    integer, dimension(:), pointer :: get_ptr_neighborEle2ele_mecaMAILx
    !***
    integer :: iM_bdyty, iM_blmty

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

    get_ptr_neighborEle2ele_mecaMAILx => M_bdyty(iM_bdyty)%ele2blmty(iM_blmty)%G_i

 end function

 !> returns the list of neighbor elements to a node
 function get_ptr_neighborEle2node_mecaMAILx(ibdyty,inodty)
    implicit none 
    integer :: ibdyty,inodty
    integer, dimension(:), pointer :: get_ptr_neighborEle2node_mecaMAILx
    !***
    integer :: iM_bdyty, iM_nodty

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    get_ptr_neighborEle2node_mecaMAILx => M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i

 end function

 !> returns 
 function get_ptr_boundaryElements_mecaMAILx(ibdyty)
    implicit none 
    integer :: ibdyty
    integer, dimension(:), pointer :: get_ptr_boundaryElements_mecaMAILx
    !***
    integer :: iM_bdyty
    iM_bdyty=bdyty2M_bdyty(ibdyty)

    get_ptr_boundaryElements_mecaMAILx => M_bdyty(iM_bdyty)%boundary_elements

 end function

!-PTA-22/03/2013--------------------------------------------------------- 
 !> Get a pointer on the body preconW
 function GetPtr_preconW(ibdyty)
    implicit none 
    integer :: ibdyty
    real(kind=8), dimension(:,:),pointer :: GetPtr_preconW

    GetPtr_preconW => bdyty(ibdyty)%W_precon

 end function

!-PTA-22/03/2013--------------------------------------------------------- 
 !> save/write the reduced (on contact dofs) W matrix 
 SUBROUTINE load_precon_W_body_mecaMAILx (iM_bdyty)
   IMPLICIT NONE
   INTEGER :: iM_bdyty
   INTEGER ::   ibdyty

   ibdyty=M2meca(iM_bdyty)%bdyty
   bdyty(ibdyty)%saved_precon_W = .TRUE.

 END SUBROUTINE load_precon_W_body_mecaMAILx

!------------------------------------------------------------------------
! DA : Permettre la visualisation des variables internes (Attention au lissage pour le passage aux noeuds)
!------------------------------------------------------------------------
SUBROUTINE get_2DNodalInternal_mecaMAILx(ibdyty,internal)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: internal
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,NbNodes_stored,iM_bdyty,iM_blmty, nb_internal,imodel

                           !123456789012345678901234567890
  CHARACTER(len=30) :: IAM='mecaMAILx::get_2DNodalInternal'
  character(len=80) :: cout

  ! allocation dans routine appelante (nb_internal,nb_nodes)
  ! Ordre donne par matlib

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  internal = 0.d0
  
  nb_internal = get_nb_internal_mecaMAILx(ibdyty)
  
  IF ( nb_internal /= SIZE(internal,dim=1)) THEN
    write(cout,*) nb_internal, size(internal,dim=1)
    call logmes(cout,.true.)
    call FATERR(IAM,'Non conforming size')
  ENDIF

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)

    ALLOCATE(temp(nb_internal,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                     bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                     iM_bdyty,iM_blmty,5,temp,nb_internal,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      internal(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=internal(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

 
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    internal(:,inodty)=internal(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO    

END SUBROUTINE get_2DNodalInternal_mecaMAILx

!!!------------------------------------------------------------------------    
!!!------------------------------------------------------------------------    

 !> \brief Computes LHS * Dofs to get resulting Force/Flux at boundary condition

 subroutine compute_Fres(i_bdyty, Fres)
   implicit none
   !> body number
   integer(kind=4), intent(in) :: i_bdyty
   !> resulting vector (already allocated)
   real(kind=8), dimension(:), intent(out) :: Fres

   Fres = 0.D0
   call set_vector(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%Vbegin)
   call multiply_system(bdyty(i_bdyty)%g_sys)
   call get_vector(bdyty(i_bdyty)%g_sys,Fres)

 end subroutine compute_Fres

 function get_nb_gp_mecaMAILx(i_bdyty, i_blmty)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> number of gauss points in the element
   integer :: get_nb_gp_mecaMAILx
   !
   integer :: iM_bdyty, iM_blmty

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   if( associated( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv ) ) then
       get_nb_gp_mecaMAILx = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1 )
   else
       get_nb_gp_mecaMAILx = 0
   end if

 end function get_nb_gp_mecaMAILx

 subroutine get_field_mecaMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:stress, 2:strain, 3:internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(out) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%stress(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%stress(:)
     end do
   case( 2 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%strain(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%strain(:)
     end do
   case( 3 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%internal(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%internal(:)
     end do
   end select

 end subroutine get_field_mecaMAILx

 subroutine set_field_mecaMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:stress, 2:strain, 3:internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(in) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%stress(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,1)%stress(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%stress(:) = field_array(1:field_size,i_gp)
     end do
   case( 2 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%strain(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,1)%strain(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%strain(:) = field_array(1:field_size,i_gp)
     end do
   case( 3 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%internal(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,1)%internal(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%meca_gpv(i_gp,2)%internal(:) = field_array(1:field_size,i_gp)
     end do
   end select

 end subroutine set_field_mecaMAILx

 !> find nearest gp
 subroutine get_nearest_gp_mecaMAILx(ibdyty,iblmty,coor,gpid)
    implicit none 
    integer      :: ibdyty,iblmty,gpid
    real(kind=8) :: coor(:)
    ! ***
    !                         1234567890123456789012345
    character(len=25) :: IAM='mecaMAILx::Get_nearest_gp'

    INTEGER :: errare

    REAL(kind=8), DIMENSION(:,:), allocatable :: coor_ele

    if (is_externalFEM) then
      gpid = 0
      return
    end if

    ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
    IF (errare /= 0) THEN
       CALL LOGMES('Error '//IAM//': allocating coor_ele')
    END IF

    coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
    
    call get_nearest_gp_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,coor_ele,coor,gpid)
    
    deallocate(coor_ele)
    
 end subroutine get_nearest_gp_mecaMAILx

 function get_gp_strain_mecaMAILx(ibdyty, iblmty, igp)
   implicit none

   !> gauss point strain field
   real(kind=8),dimension(:),pointer :: get_gp_strain_mecaMAILx
   !> body number
   integer, intent(in) :: ibdyty
   !> element number
   integer, intent(in) :: iblmty
   !> gp number
   integer, intent(in) :: igp
   !
   integer :: M_ibdyty, M_iblmty, field_size, errare

   ! nom de la fonction        1234567890123456789012345678
   CHARACTER(len=28) :: IAM = 'mod_mecaMAILx::get_gp_strain' 
   
   M_ibdyty = bdyty2M_bdyty(ibdyty)
   M_iblmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

   field_size = size(M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%strain(:))
   if (field_size > 0) then
     allocate(get_gp_strain_mecaMAILx(field_size),stat=errare)
     IF (errare /= 0) THEN
       CALL faterr(IAM,'error allocating get_gp_internals')
     END IF
     get_gp_strain_mecaMAILx(1:field_size) = M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%strain(:)
   else
     nullify(get_gp_strain_mecaMAILx)
   endif

 end function get_gp_strain_mecaMAILx

 function get_gp_stress_mecaMAILx(ibdyty, iblmty, igp)
   implicit none
   !> gauss point stress field
   real(kind=8), dimension(:), pointer :: get_gp_stress_mecaMAILx
   !> body number
   integer, intent(in) :: ibdyty
   !> element number
   integer, intent(in) :: iblmty
   !> gp number
   integer, intent(in) :: igp
   !
   integer :: M_ibdyty, M_iblmty, field_size, errare
   
   ! nom de la fonction        1234567890123456789012345678
   CHARACTER(len=28) :: IAM = 'mod_mecaMAILx::get_gp_stress' 

   M_ibdyty = bdyty2M_bdyty(ibdyty)
   M_iblmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

   field_size = size(M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%stress(:))
   if (field_size > 0) then
     allocate(get_gp_stress_mecaMAILx(field_size),stat=errare)
     IF (errare /= 0) THEN
       CALL faterr(IAM,'error allocating get_gp_internals')
     END IF
     get_gp_stress_mecaMAILx(1:field_size) = M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%stress(:)
   else
     nullify(get_gp_stress_mecaMAILx)
   endif

 end function get_gp_stress_mecaMAILx

 subroutine get_gp_strain_triaxiality_mecaMAILx(ibdyty, iblmty, igp, triaxiality)
   implicit none
   !> body number
   integer, intent(in) :: ibdyty
   !> element number
   integer, intent(in) :: iblmty
   !> gp number
   integer, intent(in) :: igp
   !> gauss point field value
   real(kind=8), dimension(9) :: field_array
   !> triaxiality calculate with field value
   real(kind=8), intent(out) :: triaxiality
   !
   integer :: M_ibdyty, M_iblmty, field_size

   M_ibdyty = bdyty2M_bdyty(ibdyty)
   M_iblmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

   field_array = 0.d0
   field_size = size(M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%strain(:) )
   field_array(1:field_size) = M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%strain(:)

   ! +1.d-16 pour eviter toute division par 0.d0
   triaxiality = hydrostatic_sym_tensor_2d_(field_array) / (strain_norm_sym_tensor_2d_(field_array)+1.d-16)

 end subroutine get_gp_strain_triaxiality_mecaMAILx
 
 subroutine get_gp_stress_triaxiality_mecaMAILx(ibdyty, iblmty, igp, triaxiality)
   implicit none
   !> body number
   integer, intent(in) :: ibdyty
   !> element number
   integer, intent(in) :: iblmty
   !> gp number
   integer, intent(in) :: igp
   !> gauss point field value
   real(kind=8), dimension(9) :: field_array
   !> triaxiality calculate with field value
   real(kind=8), intent(out) :: triaxiality
   !
   integer :: M_ibdyty, M_iblmty, field_size

   M_ibdyty = bdyty2M_bdyty(ibdyty)
   M_iblmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

   field_array = 0.d0
   field_size = size(M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%strain(:) )
   field_array(1:field_size) = M_bdyty(M_ibdyty)%blmty(M_iblmty)%meca_gpv(igp,2)%stress(:)

   ! +1.d-16 pour eviter toute division par 0.d0
   triaxiality = hydrostatic_sym_tensor_2d_(field_array) / (stress_norm_sym_tensor_2d_(field_array)+1.d-16)

 end subroutine get_gp_stress_triaxiality_mecaMAILx

!!!------------------------------------------------------------------------
! Some function used by get_gp_strain_triaxiality_mecaMAILx subroutine

! evaluate the hydrostatic value of a 2D symmetric tensor given by Matlib [a1,a2,a3,a4]
 function hydrostatic_sym_tensor_2D_(vec) result(res)
   implicit none

   real(kind=8), dimension(9), intent(in) :: vec
   real(kind=8) :: res

   res = (1.d0/3.d0)*(vec(1)+vec(3)+vec(4))

 end function hydrostatic_sym_tensor_2D_

! evaluate the Von Mises strain norm of a 2D symmetric tensor given by Matlib [a1,a2,a3,a4]
 function strain_norm_sym_tensor_2D_(vec) result(res)
   implicit none

   real(kind=8), dimension(9), intent(in) :: vec
   real(kind=8) :: res

   res = 2.d0/3.d0*sqrt(vec(1)**2+3.d0*vec(2)**2+vec(3)**2-vec(3)*vec(4)+vec(4)**2-vec(1)*(vec(3)+vec(4)))

 end function strain_norm_sym_tensor_2D_

! evaluate the Von Mises stress norm of a 2D symmetric tensor given by Matlib [a1,a2,a3,a4]
 function stress_norm_sym_tensor_2D_(vec) result(res)
   implicit none

   real(kind=8), dimension(9), intent(in) :: vec
   real(kind=8) :: res

   res = sqrt(vec(1)**2+3.d0*vec(2)**2+vec(3)**2-vec(3)*vec(4)+vec(4)**2-vec(1)*(vec(3)+vec(4)))

 end function stress_norm_sym_tensor_2D_

!------------------------------------------------------------------------

 !> computes free velocity Vfree

 subroutine prep_global_mecaMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   !
   integer(kind=4) :: iblmty,nbdof,errare,ivd,inodty,iccdof,inod,idof,ivd_
   INTEGER         :: i,info
   real(kind=8)    :: TT,UMTT,HTT,HUMTT
   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele, visco   

   !                         1234567890123456789012
   CHARACTER(len=22) :: IAM='mecaMAILx::prep_global'
   CHARACTER(len=80) :: cout

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   !fd
   !fd d'abord les contributions elementaires
   !fd

   bdyty(ibdyty)%RHS=0.D0
   bdyty(ibdyty)%Fint=0.D0
   bdyty(ibdyty)%Finert=0.D0
   bdyty(ibdyty)%momentum=0.D0

   select case( M_INTEGRATOR_ID )
   case( INTEGRATOR_BETA2 )
     stop
   case( INTEGRATOR_QS )
     stop
  case( INTEGRATOR_MOREAU )

     ! si on ne cherche pas a calculer la deformation, on zappe
     if( bdyty(ibdyty)%is_rigid .or. bdyty(ibdyty)%is_coro ) call faterr(IAM,'Not possible')

     TT = THETA
     UMTT = (1.d0-TT)   
     HTT = H*TT
     HUMTT=H*(1.d0-TT)  

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node  

       ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM, 'Error allocating dv_ele')
       ENDIF

       ALLOCATE(visco(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM, 'Error allocating visco')
       ENDIF

       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         !fd c'est completement idiot car dans increment on fait V=Vbegin 
         !
         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) - &
                                       bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
         visco((i-1)*nbdof+1:i*nbdof)= TT*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) + &
                                       UMTT*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)

       END DO


       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)
       visco = MATMUL(bdyty(ibdyty)%blmty(iblmty)%damping,visco)

       bdyty(ibdyty)%blmty(iblmty)%RHSloc=  DV_ele +  &
                                            (( HUMTT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) + &
                                                 HTT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) &
                                             ) &
                                            -( HUMTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
                                                 HTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) &
                                             ) &
                                            -( H*bdyty(ibdyty)%blmty(iblmty)%ttFint(:) ) &
                                            -( H*visco ) &
                                            )

       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         !fd c'est completement idiot car dans increment on fait V=Vbegin 
         !
         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
       END DO

       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass+(HTT*HTT*bdyty(ibdyty)%blmty(iblmty)%stiffness),DV_ele)       

       bdyty(ibdyty)%blmty(iblmty)%RHSloc= bdyty(ibdyty)%blmty(iblmty)%RHSloc + DV_ele
       
       CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

       CALL assemble_elementary_vector(bdyty(ibdyty)%Fint,-(  TT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) + &
                                                            UMTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
                                                                 bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + &
                                                                 visco                                    ) , &
                                       bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       CALL assemble_elementary_vector(bdyty(ibdyty)%Finert,(-DV_ele/H),bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         !fd c'est completement idiot car dans increment on fait V=Vbegin 
         !
         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
       END DO

       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)

       CALL assemble_elementary_vector(bdyty(ibdyty)%momentum,DV_ele,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       DEALLOCATE(DV_ele)
       DEALLOCATE(visco)

     ENDDO

     !fd
     !fd ensuite les contributions nodales 
     !fd

     bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(H*bdyty(ibdyty)%Fext)

     bdyty(ibdyty)%Vfree = 0.d0

     ! le second membre de g_sys est RHS
     call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%RHS)


     if (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) then
       ! on collecte les valeurs des ddl imposes
       ivd_=0 
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

         if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

         ivd_=ivd_+1
         
         CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)

         iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

         bdyty(ibdyty)%drvvalues(ivd_) = bdyty(ibdyty)%vdriv(ivd)

       ENDDO
     
       ! on les passe au g_system

       call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

     endif


     CALL build_system(bdyty(ibdyty)%g_sys)
     
     if (.not. is_contactdetectionconfiguration_defined) then

        call compute_configurationTT_mecaMAILx(ibdyty)

     endif

     

   case default

     call faterr(IAM,'Unknown integrator')

   end select

end subroutine prep_global_mecaMAILx

!------------------------------------------------------------------------
!> computes velocities and displacement (V and X)
 subroutine post_global_mecaMAILx(ibdyty)
   implicit none 
   integer(kind=4), intent(in) :: ibdyty
   !
   CHARACTER(len=40) :: cout
   real(kind=8)    :: UMTTH, TTH   
   !                         1234567890123456789012
   CHARACTER(len=22) :: IAM='mecaMAILx::post_global'

   if( ibdyty < 1 .or. ibdyty > nb_mecaMAILx ) then
     write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
     call faterr(IAM,cout)
   end if

   if( .not. bdyty(ibdyty)%visible ) return

   select case( M_INTEGRATOR_ID )
   case( INTEGRATOR_BETA2 )
     stop
   case( INTEGRATOR_QS )
     stop
   case( INTEGRATOR_MOREAU ) 

     UMTTH = (1.d0 - theta)*H
     TTH   = theta*H

     bdyty(ibdyty)%Vlast = bdyty(ibdyty)%V  

     bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + &
                       UMTTH*bdyty(ibdyty)%Vbegin + &
                       TTH*bdyty(ibdyty)%V 

     WHERE(dabs(bdyty(ibdyty)%X)<1.d-24) bdyty(ibdyty)%X=0.d0

   case default

     call faterr(IAM,'Unknown integrator')

   end select

 end subroutine post_global_mecaMAILx


 subroutine add_body_force_to_fext_mecaMAILx(ibdyty,vec,vsize)
   implicit none

   integer      :: ibdyty,vsize
   real(kind=8) :: vec(vsize)

   !***

   integer                  :: iblmty,nbdof,isz,ldof,errare,i,iccdof,inodty,k
   real(kind=8)             :: usH
   real(kind=8),allocatable :: faux(:)
                                   !1234567890123456789012
   character(len=22)        :: IAM='add_body_force_to_fext'
   
   usH = 1.d0 / H
   
   do iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      
     nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node
     isz = nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
     
     ALLOCATE(faux(isz),stat=errare)
     IF (errare /= 0) CALL FATERR(IAM, 'Error allocating faux')
     faux=0.d0

     ldof=1
     DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
       ! passage au numero global
       inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
       ! position un dans le vecteur global pour le numero inodty     
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       do k=1,nbdof
         faux(ldof) = vec(iccdof+k)*usH
         ldof=ldof+1 
       enddo
     enddo     
     faux = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,faux)

     CALL assemble_elementary_vector(bdyty(ibdyty)%Fext, faux , bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
     
     deallocate(faux)     
   enddo   
   
 end subroutine add_body_force_to_fext_mecaMAILx

 subroutine check_properties_mecaMAILx()
   implicit none

   !                         123456789012345678901234567
   CHARACTER(len=27) :: IAM='mecaMAILx::check_properties'
   CHARACTER(len=80) :: cout

   integer :: ibdyty,iblmty,iM_bdyty,iM_blmty
   
   if (nb_mecaMAILx == 0 .or. is_externalFEM ) return
   
   DO ibdyty=1,SIZE(bdyty)
   
     iM_bdyty = bdyty2M_bdyty(ibdyty)
   
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
           
       iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
 
       call check_elementary_ppset(bdyty(ibdyty)%blmty(iblmty)%blmnb,bdyty(ibdyty)%blmty(iblmty)%ppsnb,&
                                   iM_bdyty,iM_blmty)
      
     enddo
   enddo
 end subroutine check_properties_mecaMAILx

 subroutine get_nb_gp_by_elem_mecaMAILx(elems, nb_gp, nb_elems)

    implicit none
    character(len=5), dimension(:), pointer :: elems
    integer         , dimension(:), pointer :: nb_gp
    integer :: nb_elems
    !
    integer :: i_elem

    CALL init_mecaEF

    nb_elems = nb_mecaEF
    allocate(elems(nb_elems))
    allocate(nb_gp(nb_elems))

    do i_elem = 1, nb_elems
        elems(i_elem) = mecaEF(i_elem)%NAME
        nb_gp(i_elem) = get_N_GP_mecaEF( mecaEF(i_elem)%NAME )
    end do

 end subroutine get_nb_gp_by_elem_mecaMAILx

 subroutine mass_scaling_mecaMAILx(scale)
   implicit none

   !                         12345678901234567890123
   CHARACTER(len=23) :: IAM='mecaMAILx::mass_scaling'
   real(kind=8)      :: scale

   mass_scaling=scale

 end subroutine mass_scaling_mecaMAILx

 subroutine get_gp_all_joint_mecaMAILx(all) 

   implicit none


   real(kind=8),pointer,dimension(:,:) :: all
   
   integer :: nbgpt

   integer :: ibdyty,iblmty,imodel,iM_bdyty,iM_blmty,nbgp,errare,ig,idx,nbdime_

   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele,e,s,r,frame

   real(kind=8) :: Q22(2,2),Q33(3,3)

   character(len=27) :: IAM='mecaMAILx::get_gp_all_joint'
   character(len=8) :: name   

   ! calcul du nb de gp dimensionnement des tableaux
   nbgpt = 0
   name = 'joint   '
   
   DO ibdyty=1,SIZE(bdyty)
     !
     if( .not. bdyty(ibdyty)%visible ) cycle
        
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       if (.not. is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,name)) cycle
        
       imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
       nbgpt = nbgpt + get_N_GP_mecaEF(modelz(imodel)%ID)

     ENDDO
   ENDDO

   ! print*,nbgpt


   ! dans all on remonte la meme chose en 2D et 3D (l local g global)   
   ! coor(3),n(3),el(3),sl(3),eg(3),sg(3),endo,rn,rt,mat
   allocate(all(22,nbgpt))
   all=0.d0

   nbgpt = 0
   DO ibdyty=1,SIZE(bdyty)
     !
     if( .not. bdyty(ibdyty)%visible ) cycle

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        
       if (.not. is_ele_mecaEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,name)) cycle
        
       imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
       nbgp = get_N_GP_mecaEF(modelz(imodel)%ID)

       ! recuperation des position des gp

       ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating coor_ele')
       ENDIF

       coor_ele=get_coor_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

       iM_bdyty=bdyty2M_bdyty(ibdyty)
       iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

       CALL get_ele_gp_coor(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                            bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                            iM_bdyty,iM_blmty,coor_ele, &
                            all(1:nbdime,nbgpt+1:nbgpt+nbgp))

       DEALLOCATE(coor_ele)

       ALLOCATE(e(nbdime,nbgp),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating e')
       ENDIF
      
       ALLOCATE(s(nbdime,nbgp),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating s')
       ENDIF
      
       ALLOCATE(r(nbdime,nbgp),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating r')
       ENDIF
      
       ALLOCATE(frame(nbDIME*nbDIME,nbgp),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating frame')
       ENDIF

       ! ces infos sont dans le rep global 
       CALL get_joint_gp_vec(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                              bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                              iM_bdyty,iM_blmty, &
                              1,e)
       
       CALL get_joint_gp_vec(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                              bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                              iM_bdyty,iM_blmty, &
                              2,s)
       
       CALL get_joint_gp_vec(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                              bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                              iM_bdyty,iM_blmty, &
                              3,r)

       CALL get_joint_gp_frame(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                             bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                             iM_bdyty,iM_blmty, &
                             frame,nbdime*nbdime)
       
       CALL get_joint_gp_endo(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                             bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                             iM_bdyty,iM_blmty, &
                             all(19,nbgpt+1:nbgpt+nbgp))

       nbdime_=3
       do ig=1,nbgp
          
         ! la normale  
         idx=nbdime_ 
         all(idx+1:idx+nbdime,nbgpt+ig) = frame(nbdime*(nbdime-1)+1:nbdime*nbdime,ig)
         
         ! les vecteurs locaux
         idx=idx+nbdime_
         if (nbdime==2) then
           ! la premiere composante locale est nulle 
           all(idx+2:idx+nbdime_,nbgpt+ig) = e(1:nbdime,ig)
           idx=idx+nbdime_ 
           all(idx+2:idx+nbdime_,nbgpt+ig) = s(1:nbdime,ig)
         else  
           all(idx+1:idx+nbdime,nbgpt+ig) = e(1:nbdime,ig)
           idx=idx+nbdime_ 
           all(idx+1:idx+nbdime,nbgpt+ig) = s(1:nbdime,ig)
         endif
        
         ! les vecteurs globaux
         idx=idx+nbdime_
         if (nbdime==2) then
           Q22  = reshape(frame(:,ig),(/nbdime,nbdime/))
           ! la derniere composante (z) est nulle 
           all(idx+1:idx+nbdime,nbgpt+ig) = matmul(Q22,e(1:nbdime,ig))
           idx=idx+nbdime_ 
           all(idx+1:idx+nbdime,nbgpt+ig) = matmul(Q22,s(1:nbdime,ig))
         else
           Q33  = reshape(frame(:,ig),(/nbdime,nbdime/))           
           all(idx+1:idx+nbdime,nbgpt+ig) = matmul(Q33,e(1:nbdime,ig))
           idx=idx+nbdime_ 
           all(idx+1:idx+nbdime,nbgpt+ig) = matmul(Q33,s(1:nbdime,ig))
         endif

         ! les reactions
         if (nbdime==2) then         
           all(20,nbgpt+ig) = abs(r(1,ig))
           all(21,nbgpt+ig) = r(2,ig)
         else  
           all(20,nbgpt+ig) = sqrt(r(1,ig)**2+r(2,ig)**2)
           all(21,nbgpt+ig) = r(3,ig)         
         endif 

         ! le materiau
         all(22,nbgpt+ig) = bdyty(ibdyty)%blmty(iblmty)%lawnb

         
       enddo
       
       deallocate(e,s,r,frame)

       nbgpt = nbgpt + nbgp

     END DO
   END DO

 end subroutine get_gp_all_joint_mecaMAILx
 

  !> \brief Disable or enable a velocity constraint for a given body in a given dof
  subroutine switch_vlocy_driven_dof( ibdyty, inod, idof, ival )

    implicit none

    integer, intent( in ) :: ibdyty
    integer, intent( in ) :: inod
    integer, intent( in ) :: idof    
    integer, intent( in ) :: ival

    ! ****
    integer :: ivd,inod_,idof_
    logical :: found_dof_vlocy
                                  !12345678901234567890123456789012345
    character( len =35 ) :: IAM = 'meca_MAILx::switch_vlocy_driven_dof'
    character( len =80 ) :: cout

    found_dof_vlocy = .FALSE.

    ! nullifying inv_mass where degrees of freedom are driven
    do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof

      CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod_,idof_)
       
      if ( idof_ == idof .and. inod_ == inod) then

        if ( ival == 0 ) then
          bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active = .FALSE.

        else
          bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active = .TRUE.

        endif

        found_dof_vlocy = .TRUE.
        exit

      endif
    end do

    if ( .not. found_dof_vlocy ) then
      write( cout, '("body ",I0," node ",I0," dof ",I0)' ) ibdyty, inod, idof
      call logmes( cout )
      call faterr( IAM, 'Error while trying to switch a velocy driven dof status' )
    end if

  end subroutine switch_vlocy_driven_dof

  subroutine update_vlocy_driven_dof_structures(ibdyty)
    implicit none

    integer, intent( in ) :: ibdyty

    integer               :: ivd,nbdrv,errare
                                  !123456789012345678901234567890123456789012
    character( len =42 ) :: IAM = 'meca_MAILx::update_vlocy_driven_structures'

    nbdrv=0
    do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof
      if (bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active) then
        nbdrv=nbdrv+1
      endif   
    enddo

    if (nbdrv /= size(bdyty(ibdyty)%drvdofs)) then

      deallocate(bdyty(ibdyty)%drvdofs)
      deallocate(bdyty(ibdyty)%drvvalues)       
    
      ALLOCATE(bdyty(ibdyty)%drvdofs(nbdrv),bdyty(ibdyty)%drvvalues(nbdrv),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating drvdofs/drvvalues')
       END IF
    endif

    bdyty(ibdyty)%drvdofs=0
    bdyty(ibdyty)%drvvalues=0.d0

    call update_drvstatus(ibdyty)

  end subroutine
 !------------------------------------------------------------------------
  logical function is_node_dof_driven_mecaMAILx(ibdyty, inodty)
    integer, intent(in) :: ibdyty, inodty
    !
    integer :: test

    if (nbDIME == 2) then
      test = 11
    else 
      test = 111
    end if

    is_node_dof_driven_mecaMAILx = .false.

    if( .not. associated(bdyty(ibdyty)%drvstatus) ) return

    ! check if node has all its dof velocity driven
    is_node_dof_driven_mecaMAILx = bdyty(ibdyty)%drvstatus(inodty) == test

  end function is_node_dof_driven_mecaMAILx

 !------------------------------------------------------------------------
  logical function is_elem_dof_driven_mecaMAILx(ibdyty, iblmty)
    integer, intent(in) :: ibdyty, iblmty
    !
    integer :: i_node

    is_elem_dof_driven_mecaMAILx = .false.

    if( .not. associated(bdyty(ibdyty)%drvstatus) ) return

    do i_node = 1, size( bdyty(ibdyty)%blmty(iblmty)%NODES )
      is_elem_dof_driven_mecaMAILx = bdyty(ibdyty)%drvstatus(i_node) == 111
      if( .not. is_elem_dof_driven_mecaMAILx ) exit
    end do

  end function is_elem_dof_driven_mecaMAILx

 !------------------------------------------------------------------------
  subroutine clean_memory_mecaMAILx()
    implicit none
    integer(kind=4)   :: i_bdyty, i_blmty, i
    character(len=80) :: cout

    if( allocated(M2meca) ) then
      do i = 1, size(M2meca)
        if( associated(M2meca(i)%nodty) ) then 
           deallocate(M2meca(i)%nodty)
           M2meca(i)%nodty => null( )
        end if
        if( associated(M2meca(i)%blmty) ) then
           deallocate(M2meca(i)%blmty)
           M2meca(i)%blmty => null( )
        end if
      end do
      deallocate(M2meca)
    end if
   
    if( .not. allocated(bdyty) ) return

    nb_mecaMAILx=0

    do i_bdyty = 1, size(bdyty)

      if( associated(bdyty(i_bdyty)%blmty) ) then
        do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%NODES) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
            bdyty( i_bdyty )%blmty( i_blmty )%NODES => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb)
            bdyty( i_bdyty )%blmty( i_blmty )%ppsnb => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)
            bdyty( i_bdyty )%blmty( i_blmty )%edof2gdof => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%stiffness) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%stiffness)
            bdyty( i_bdyty )%blmty( i_blmty )%stiffness => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%mass) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%mass)
            bdyty( i_bdyty )%blmty( i_blmty )%mass => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%damping) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%damping)
            bdyty( i_bdyty )%blmty( i_blmty )%damping => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fext) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fext)
            bdyty( i_bdyty )%blmty( i_blmty )%Fext => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fint) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fint)
            bdyty( i_bdyty )%blmty( i_blmty )%Fint => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ttFint) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ttFint)
            bdyty( i_bdyty )%blmty( i_blmty )%ttFint => null( )
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc)
            bdyty( i_bdyty )%blmty( i_blmty )%RHSloc => null( )
          end if
        end do
        deallocate(bdyty(i_bdyty)%blmty)
        bdyty( i_bdyty )%blmty => null( )
      end if  

      if( associated(bdyty(i_bdyty)%blmty2M_blmty) ) then 
         deallocate(bdyty(i_bdyty)%blmty2M_blmty)
         bdyty( i_bdyty )%blmty2M_blmty => null( )
      end if
      if( associated(bdyty(i_bdyty)%nodty)         ) then 
         deallocate(bdyty(i_bdyty)%nodty)
         bdyty( i_bdyty )%nodty => null( )
      end if
      if( associated(bdyty(i_bdyty)%nodty2M_nodty) ) then 
         deallocate(bdyty(i_bdyty)%nodty2M_nodty)
         bdyty( i_bdyty )%nodty2M_nodty => null( )
      end if
      if( associated(bdyty(i_bdyty)%Vlast)         ) then 
         deallocate(bdyty(i_bdyty)%Vlast)
         bdyty( i_bdyty )%Vlast => null( )
      end if
      if( associated(bdyty(i_bdyty)%Vbegin)        ) then 
         deallocate(bdyty(i_bdyty)%Vbegin)
         bdyty( i_bdyty )%Vbegin => null( )
      end if
      if( associated(bdyty(i_bdyty)%V)             ) then 
         deallocate(bdyty(i_bdyty)%V)
         bdyty( i_bdyty )%V => null( )
      end if
      if( associated(bdyty(i_bdyty)%Xprev)         ) then 
         deallocate(bdyty(i_bdyty)%Xprev)
         bdyty( i_bdyty )%Xprev => null( )
      end if
      if( associated(bdyty(i_bdyty)%Xbegin)        ) then 
         deallocate(bdyty(i_bdyty)%Xbegin)
         bdyty( i_bdyty )%Xbegin => null( )
      end if
      if( associated(bdyty(i_bdyty)%X)             ) then 
         deallocate(bdyty(i_bdyty)%X)
         bdyty( i_bdyty )%X => null( )
      end if
      if( associated(bdyty(i_bdyty)%Fext)          ) then 
         deallocate(bdyty(i_bdyty)%Fext)
         bdyty( i_bdyty )%Fext => null( )
      end if
      if( associated(bdyty(i_bdyty)%Fint)          ) then 
         deallocate(bdyty(i_bdyty)%Fint)
         bdyty( i_bdyty )%Fint => null( )
      end if
      if( associated(bdyty(i_bdyty)%Ireac)          ) then 
         deallocate(bdyty(i_bdyty)%Ireac)
         bdyty( i_bdyty )%Ireac => null( )
      end if
      if( associated(bdyty(i_bdyty)%Iaux)          ) then 
         deallocate(bdyty(i_bdyty)%Iaux)
         bdyty( i_bdyty )%Iaux => null( )
      end if
      if( associated(bdyty(i_bdyty)%Vfree)         ) then 
         deallocate(bdyty(i_bdyty)%Vfree)
         bdyty( i_bdyty )%Vfree => null( )
      end if
      if( associated(bdyty(i_bdyty)%Vaux)          ) then 
         deallocate(bdyty(i_bdyty)%Vaux)
         bdyty( i_bdyty )%Vaux => null( )
      end if
      if( associated(bdyty(i_bdyty)%residu)        ) then 
         deallocate(bdyty(i_bdyty)%residu)
         bdyty( i_bdyty )%residu => null( )
      end if
      if( associated(bdyty(i_bdyty)%Finert)        ) then 
         deallocate(bdyty(i_bdyty)%Finert)
         bdyty( i_bdyty )%Finert => null( )
      end if
      if( associated(bdyty(i_bdyty)%momentum)      ) then 
         deallocate(bdyty(i_bdyty)%momentum)
         bdyty( i_bdyty )%momentum => null( )
      end if
      if( associated(bdyty(i_bdyty)%ccdof)         ) then 
         deallocate(bdyty(i_bdyty)%ccdof)
         bdyty( i_bdyty )%ccdof => null( )
      end if
      if( associated(bdyty(i_bdyty)%nodnb)         ) then 
         deallocate(bdyty(i_bdyty)%nodnb)
         bdyty( i_bdyty )%nodnb => null( )
      end if
      if( associated(bdyty(i_bdyty)%dofnb)         ) then 
         deallocate(bdyty(i_bdyty)%dofnb)
         bdyty( i_bdyty )%dofnb => null( )
      end if
      if( associated(bdyty(i_bdyty)%coorTT)        ) then 
         deallocate(bdyty(i_bdyty)%coorTT)
         bdyty( i_bdyty )%coorTT => null( )
      end if
      if( associated(bdyty(i_bdyty)%RHS)           ) then 
         deallocate(bdyty(i_bdyty)%RHS)
         bdyty( i_bdyty )%RHS => null( )
      end if

      if( associated(bdyty(i_bdyty)%vlocy_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%vlocy_driven_dof)
          if( associated(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x)
            bdyty( i_bdyty )%vlocy_driven_dof( i )%time_evolution%x => null( )
          end if

          if( associated(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx)
            bdyty( i_bdyty )%vlocy_driven_dof( i )%time_evolution%fx => null( )
          end if
        end do

        deallocate(bdyty(i_bdyty)%vlocy_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%Vdriv)    ) then 
         deallocate(bdyty(i_bdyty)%Vdriv)
         bdyty( i_bdyty )%Vdriv => null( )
      end if
      if( associated(bdyty(i_bdyty)%VdrivBeg) ) then 
         deallocate(bdyty(i_bdyty)%VdrivBeg)
         bdyty( i_bdyty )%VdrivBeg => null( )
      end if
      if( associated(bdyty(i_bdyty)%Xdriv)    ) then 
         deallocate(bdyty(i_bdyty)%Xdriv)
         bdyty( i_bdyty )%Xdriv => null( )
      end if
      if( associated(bdyty(i_bdyty)%drvdofs)  ) then 
         deallocate(bdyty(i_bdyty)%drvdofs)
         bdyty( i_bdyty )%drvdofs => null( )
      end if
      if( associated(bdyty(i_bdyty)%drvvalues)) then 
         deallocate(bdyty(i_bdyty)%drvvalues)
         bdyty( i_bdyty )%drvvalues => null( )
      end if
      if( associated(bdyty(i_bdyty)%drvstatus)) then 
         deallocate(bdyty(i_bdyty)%drvstatus)
         bdyty( i_bdyty )%drvstatus => null( )
      end if

      if( associated(bdyty(i_bdyty)%force_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%force_driven_dof)
          if( associated(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x)
            bdyty( i_bdyty )%force_driven_dof( i )%time_evolution%x => null( )
          end if
          if( associated(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx)
            bdyty( i_bdyty )%force_driven_dof( i )%time_evolution%fx => null( )
          end if
        end do
        deallocate(bdyty(i_bdyty)%force_driven_dof)
        bdyty( i_bdyty )%force_driven_dof => null( )
      end if

      if( associated(bdyty(i_bdyty)%Fdriv)    ) then 
         deallocate(bdyty(i_bdyty)%Fdriv)
         bdyty( i_bdyty )%Fdriv => null( )
      end if
      if( associated(bdyty(i_bdyty)%FdrivBeg) ) then 
         deallocate(bdyty(i_bdyty)%FdrivBeg)
         bdyty( i_bdyty )%FdrivBeg => null( )
      end if

      if( associated(bdyty(i_bdyty)%Xwear) ) then 
         deallocate(bdyty(i_bdyty)%Xwear)
         bdyty( i_bdyty )%Xwear => null( )
      end if
      if( associated(bdyty(i_bdyty)%Vwear) ) then 
         deallocate(bdyty(i_bdyty)%Vwear)
         bdyty( i_bdyty )%Vwear => null( )
      end if

      if( associated(bdyty(i_bdyty)%nodes_precon)) then 
         deallocate(bdyty(i_bdyty)%nodes_precon)
         bdyty( i_bdyty )%nodes_precon => null( )
      end if
      if( associated(bdyty(i_bdyty)%W_precon)    ) then 
         deallocate(bdyty(i_bdyty)%W_precon)
         bdyty( i_bdyty )%W_precon => null( )
      end if
      if( associated(bdyty(i_bdyty)%Vaux_precon) ) then 
         deallocate(bdyty(i_bdyty)%Vaux_precon)
         bdyty( i_bdyty )%Vaux_precon => null( )
      end if
      if( associated(bdyty(i_bdyty)%p2g)         ) then 
         deallocate(bdyty(i_bdyty)%p2g)
         bdyty( i_bdyty )%p2g => null( )
      end if
      if( associated(bdyty(i_bdyty)%g2p)         ) then 
         deallocate(bdyty(i_bdyty)%g2p)
         bdyty( i_bdyty )%g2p => null( )
      end if

      if( allocated(bdyty(i_bdyty)%cooref_local) ) deallocate(bdyty(i_bdyty)%cooref_local)
      if( allocated(bdyty(i_bdyty)%R2D)          ) deallocate(bdyty(i_bdyty)%R2D)
      if( allocated(bdyty(i_bdyty)%D2R)          ) deallocate(bdyty(i_bdyty)%D2R)
      if( allocated(bdyty(i_bdyty)%cooref_G)     ) deallocate(bdyty(i_bdyty)%cooref_G)
      if( allocated(bdyty(i_bdyty)%coorbegin_G)  ) deallocate(bdyty(i_bdyty)%coorbegin_G)
      if( allocated(bdyty(i_bdyty)%LocalFrameIni)) deallocate(bdyty(i_bdyty)%LocalFrameIni)
      if( allocated(bdyty(i_bdyty)%LocalFrame)   ) deallocate(bdyty(i_bdyty)%LocalFrame)
      if( allocated(bdyty(i_bdyty)%LocalFrameTT) ) deallocate(bdyty(i_bdyty)%LocalFrameTT)
      if( allocated(bdyty(i_bdyty)%RV)           ) deallocate(bdyty(i_bdyty)%RV)
      if( allocated(bdyty(i_bdyty)%RVbegin)      ) deallocate(bdyty(i_bdyty)%RVbegin)
      if( allocated(bdyty(i_bdyty)%RVaux)        ) deallocate(bdyty(i_bdyty)%RVaux)
      if( allocated(bdyty(i_bdyty)%RVfree)       ) deallocate(bdyty(i_bdyty)%RVfree)
      if( allocated(bdyty(i_bdyty)%RX)           ) deallocate(bdyty(i_bdyty)%RX)
      if( allocated(bdyty(i_bdyty)%RXbegin)      ) deallocate(bdyty(i_bdyty)%RXbegin)
      if( allocated(bdyty(i_bdyty)%RX_TT)        ) deallocate(bdyty(i_bdyty)%RX_TT)
      if( allocated(bdyty(i_bdyty)%RIreac)       ) deallocate(bdyty(i_bdyty)%RIreac)
      if( allocated(bdyty(i_bdyty)%RIaux)        ) deallocate(bdyty(i_bdyty)%RIaux)
      if( allocated(bdyty(i_bdyty)%RFext)        ) deallocate(bdyty(i_bdyty)%RFext)
      if( allocated(bdyty(i_bdyty)%RFint)        ) deallocate(bdyty(i_bdyty)%RFint)
      if( allocated(bdyty(i_bdyty)%dep_R_TT)     ) deallocate(bdyty(i_bdyty)%dep_R_TT)
      if( allocated(bdyty(i_bdyty)%mR)           ) deallocate(bdyty(i_bdyty)%mR)
      if( allocated(bdyty(i_bdyty)%inv_mR)       ) deallocate(bdyty(i_bdyty)%inv_mR)
      if( associated(bdyty(i_bdyty)%RcoorTT)     ) then
         deallocate(bdyty(i_bdyty)%RcoorTT)
         bdyty( i_bdyty )%RcoorTT => null( )
      end if

      if( associated(bdyty(i_bdyty)%periodicnode) ) then
         deallocate(bdyty(i_bdyty)%periodicnode)
         bdyty( i_bdyty )%periodicnode => null( )
      end if

      call erase_system(bdyty(i_bdyty)%g_sys)

      if( associated(bdyty(i_bdyty)%eviz) ) then 
         deallocate(bdyty(i_bdyty)%eviz)
         bdyty( i_bdyty )%eviz => null( )
      end if

      if( associated(bdyty(i_bdyty)%elem_energy) ) then 
         deallocate(bdyty(i_bdyty)%elem_energy)
         bdyty( i_bdyty )%elem_energy => null( )
      end if

    end do

    if( allocated(bdyty        ) ) deallocate(bdyty)

    if( allocated(bdyty2M_bdyty) ) deallocate(bdyty2M_bdyty)

  end subroutine clean_memory_mecaMAILx

 
END MODULE mecaMAILx

