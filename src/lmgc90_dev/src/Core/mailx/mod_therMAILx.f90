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
!> \defgroup TherMailx Thermal Finite Element Modelling
!> \ingroup  Mailx 
!> \brief This module has methods to deal with thermal finite element model.
MODULE therMAILx

  USE overall
  USE utilities
  USE parameters
  
  USE bulk_behaviour
  USE models
  
  USE a_DOF
  
  !USE a_matrix
  
  USE a_EF
  USE a_therEF
  
  USE MAILx
  
  use a_system, only : g_system                  , &
                       T_link_connec             , &
                       initialize_system         , &
                       get_nb_non_zero           , &
                       erase_elementary_matrix   , &
                       add_to_elementary_matrix  , &
                       set_vector                , &
                       assemble_elementary_vector, &
                       erase_drvdofs             , &
                       set_drvdofs               , &
                       set_drvvalues             , &
                       solve_system              , &
                       erase_system              , &
                       sparse_storage_available

  use MAILX_type, only : T_therMAILx
  
  IMPLICIT NONE
  
  PRIVATE
  
  type( T_therMAILx ), dimension( : ), allocatable, private, target :: bdyty


  TYPE :: T_therMESHLESS
     
     !TODO: create map nodty2XXX -> contactors, rbdy?

     TYPE(T_nodty),DIMENSION(:),POINTER :: nodty    ! ID of the nodty storing the nodal deg of freedom ...

     REAL(kind=8),DIMENSION(:),POINTER :: Tbegin,T,Tlast  ! Tbegin   temperature at the beginning of time step;
                                                          ! T        temperature during the current time step;

     REAL(kind=8),DIMENSION(:),POINTER :: Taux

     REAL(kind=8),DIMENSION(:),POINTER :: Fext,Fint ! Fext     external force, external momentum ...
                                                    !
                                                    ! Fint     internal force, internal momentum ...

     REAL(kind=8),DIMENSION(:),POINTER :: residu

     ! part concerning the driven dof's of the bdyty

     
     INTEGER                                 :: nb_temp_driven_dof
     TYPE(T_driven_dof),DIMENSION(:),POINTER :: temp_driven_dof 
     REAL(kind=8),DIMENSION(:),POINTER       :: Tdriv
     !fd new 
     logical,dimension(:),pointer            :: is_Tdriv_active
     
     ! pour simplifier la vie avec le g_system
     integer(kind=4),DIMENSION(:),POINTER    :: drvdofs
     REAL(kind=8),DIMENSION(:),POINTER       :: drvvalues
     
     INTEGER                                 :: nb_flux_driven_dof
     TYPE(T_driven_dof),DIMENSION(:),POINTER :: flux_driven_dof 
     REAL(kind=8),DIMENSION(:),POINTER       :: Fdriv
     
     integer                             :: nb_nodes
     INTEGER                           :: nbdof     ! total number of dof of a bdyty
     
     INTEGER ,DIMENSION(:),POINTER     :: ccdof     ! iccdof = bdyty(ibdyty)%ccdof(inodty)+1 for thermal analysis
                                                    ! the comments below stand for idof running from 1 to N_DOF_by_NODE
     INTEGER ,DIMENSION(:),POINTER     :: nodnb     ! inodty = bdyty(ibdyty)%nodnb(iccdof)
                                                    ! and to dof:
     INTEGER ,DIMENSION(:),POINTER     :: dofnb     ! idof = bdyty(ibdyty)%dofnb(iccdof)
                                                    ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
                                                    ! T_force_driven, with similar meanings.
     
  END type T_therMESHLESS

  TYPE(T_therMESHLESS),DIMENSION(:),ALLOCATABLE,PRIVATE :: extra_bdyty !

  ! mapping between local  to global bdyty number
  
  INTEGER,DIMENSION(:),ALLOCATABLE,PRIVATE           :: bdyty2M_bdyty
  
  ! reverse mapping between global 2 local bdyty,blmty,nodty numbering
  
  TYPE(T_MAILx_2_localMAILx),DIMENSION(:),ALLOCATABLE,PUBLIC :: M2therm
  
  !fd 
  INTEGER :: nb_therMAILx=0
  
  LOGICAL :: use_existing_ppset = .TRUE.
  
  !type de stockage des matrices
  ! i_diagonal, i_sparse, i_band, i_skyline, i_exploded, i_full
  INTEGER :: Matrix_storage = -99
  !type de profil des matrices
  ! i_sym, i_std
  INTEGER :: Matrix_shape = i_sym
  ! pour les matrices denses
  LOGICAL :: with_renum = .TRUE.
  
  !=============== methodes ===================================!
  
  PUBLIC compute_conductivity_therMAILx, &
         compute_capacity_therMAILx, &
         compute_ttFint_therMAILx, &
         compute_Fext_therMAILx, &
         assemb_KT_therMAILx, &
         assemb_RHS_therMAILx, &
         trial_assemb_KT_therMAILx, &
         trial_assemb_RHS_therMAILx, &
         increment_therMAILx, &
         comp_dof_therMAILx, &
         update_dof_therMAILx, &
         update_bulk_therMAILx, &
         compute_residue_norm_therMAILx, &
         read_in_driven_dof_therMAILx, &
         read_models_therMAILx, &
         read_behaviours_therMAILx, &
         write_xxx_dof_therMAILx, &
         read_in_dof_therMAILx, &
         write_out_driven_dof_therMAILx, &
         read_in_gpv_therMAILx, &
         CHECK_therMAILx, &
         get_write_DOF_therMAILx, &
         get_write_Rnod_therMAILx, & 
         set_field_bynode, set_field_byelem, &
         get_field_rank, &
         set_vfield_bynode, set_vfield_byelem, &
         get_vfield_rank, &
         get_gp_coor_therMAILx  , &
         get_gp_field_therMAILx, &
         remove_driven_temp, &
         put_vector_therMAILx, &
         get_vector_therMAILx, &
         set_temp_drvdof_therMAILx, &
         initialize_elementary_flux_therMAILx, &
         set_Matrix_Storage_therMAILx, &
         set_without_renum_therMAILx, &
         check_properties_therMAILx, &
         get_nb_gp_by_elem_therMAILx



  PUBLIC get_T_nodty_therMAILx,get_nb_therMAILx, &
         add_convection2KT_therMAILx,add_convection2RHS_therMAILx, &
         get_Tbegin_nodty_therMAILx,get_nb_nodes_therMAILx 
 
  !rm: accessor for global solver
  public get_ptr_drvdofs_therMAILx, &
         get_ptr_Tbeg_therMAILx   , &
         get_ptr_T_therMAILx      , &
         get_ptr_Tdriv_therMAILx  , &
         get_ptr_Fext_therMAILx   , &
         get_lhs_loc_therMAILx    , &
         get_rhs_loc_therMAILx    , &
         get_ptr_ccdof_therMAILx  , &
         get_ptr_connec_therMAILx , &
         get_nb_max_dofs_adj_therMAILx
 
  !am: fonctions supplementaires:
  PUBLIC get_nb_elements_therMAILx, &
         only_one_elem_type_therMAILx, &
         get_N_NODE_therMAILx, &
         get_T_FONC_FORME_therMAILx, &
         get_conn_therMAILx, &
         get_cooref_node_therMAILx, &
         compute_element_volume_therMAILx, &
         element_volume_by_node_therMAILx, &
         compute_elementary_F_Biot_therMAILx, &
         compute_elementary_F_advection_therMAILx, &
         compute_grad_T_ele_therMAILx, &
         add_external_flux_ele_therMAILx, &
         add_theta_external_flux_ele_therMAILx, &
         compute_Fext_Poisson_therMAILx, & ! <-debut thermique stationnaire
         assemb_KT_Poisson_therMAILx, & 
         assemb_RHS_Poisson_therMAILx, &        
         !compute_dof_Poisson_therMAILx, &
         compute_dof_Poisson_Neumann_therMAILx, & ! <- fin thermique stationnaire
         add_source_therMAILx, &
         push_ppset_therMAILx, &
         compute_convection_therMAILx, &
         set_Matrix_shape_therMAILx, &
         add_field_divergence_therMAILx, &
         get_NodalGrad_therMAILx ,&
         get_NodalFlux_therMAILx ,&
         apply_drvdof_KT_therMAILx, &
         get_coor_therMAILx, &
         get_connectivity_therMAILx, &
         get_All_therMAILx

  public clean_memory_therMAILx

  !rm: accessor for hdf5
  public get_nb_gp_therMAILx, &
         get_field_therMAILx, &
         set_field_therMAILx

  public get_bdyty_therMAILx

CONTAINS 

!------------------------------------------------------------------------

  subroutine get_bdyty_therMAILx( arg_bdyty )

    implicit none

    type( T_therMAILx ), dimension( : ), pointer :: arg_bdyty

    arg_bdyty => bdyty

  end subroutine get_bdyty_therMAILx

!> \defgroup RWTherMailx Read/Write
!> \ingroup TherMailx
!> \brief This part details the interface to read/write quantities
!> \addtogroup RWTherMailx
!>\{

  SUBROUTINE read_in_driven_dof_therMAILx

    IMPLICIT NONE

    G_nfich = get_io_unit()
    OPEN(unit=G_nfich,file=TRIM(location(in_driven_dof(:))))
    CALL read_driven_dof
    CLOSE(G_nfich)

  END SUBROUTINE read_in_driven_dof_therMAILx
!!!------------------------------------------------------------------------
  !> \brief Read a DOF file to initialize database
  subroutine read_in_dof_therMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
    else if(step > 1) then
      open(unit=G_nfich,file=trim(location(out_dof(:))))
    else
      open(unit=G_nfich,file=trim(location(last_dof(:))))
    end if

    call read_in_dof
    close(G_nfich)
    
  end subroutine read_in_dof_therMAILx
!!!------------------------------------------------------------------------
  !> \brief Read a GPV file to initialize database
  subroutine read_in_gpv_therMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_gpv(:))))
    else if(step > 1) then
      open(unit=G_nfich,file=trim(location(out_gpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_gpv(:))))
    end if

    call read_in_gpv
    close(G_nfich)

  end subroutine read_in_gpv_therMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_driven_dof_therMAILx

    IMPLICIT NONE

    INTEGER :: nfich

    nfich = get_io_unit()
    OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_driven_dof(:))))
    CALL write_driven_dof(nfich)
    CLOSE(nfich)

  END SUBROUTINE write_out_driven_dof_therMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_dof_therMAILx(which,ifrom,ito)

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

  END SUBROUTINE write_xxx_dof_therMAILx

!!!----------------------------------------------------------
  SUBROUTINE read_models_therMAILx

    IMPLICIT NONE
    
    INTEGER :: nb_MAILx
    INTEGER :: iM_bdyty,iM_blmty,iM_model,iM_nodty
    INTEGER :: ibdyty,iblmty,imodel,inodty
    INTEGER :: errare,itest,imdlnb,iccdof,idof,iG_i
    INTEGER :: itempo,bw,i

    INTEGER :: nb_external,nb_internal
    
    CHARACTER(len=5)   :: ctempo
    CHARACTER(len=103) :: cout
    CHARACTER(len=28)  :: IAM='therMAILx::read_model'

    REAL(kind=8) :: Tref=293.d0

    !fd 30/01/09 external fields
    INTEGER :: if,nbf,nb_ef,nb_bf
    Character(len=30),dimension(:),allocatable :: field_name
    !
    integer(kind=4) :: nb_evf,vsize
    character(len=30), dimension(:), allocatable :: vfield_name
    
    type(T_link_connec), pointer ::connectivities, tmp

    integer(kind=4) ::  max_nod2el, max_dofs_adj, max_conn

    ! 0 initialisations

    CALL init_therEF

    nb_MAILx=get_nb_MAILx()

   ALLOCATE(M2therm(nb_MAILx),stat=errare)
   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating M2therm')
   END IF
  

   DO iM_bdyty=1,nb_MAILx
     M2therm(iM_bdyty)%bdyty=0
     NULLIFY(M2therm(iM_bdyty)%nodty)
     NULLIFY(M2therm(iM_bdyty)%blmty)
   ENDDO

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
    

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! first reading: sizing array of models  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if( .not. allocated(modelz) .or. size(modelz) < 1 ) then
       call faterr(IAM,'please call ReadModels before trying to LoadModels')
   end if

   itest=0
   DO imodel=1,SIZE(modelz)
     IF (modelz(imodel)%mdlty == 'THERM') itest=itest+1 
   END DO   

   WRITE(cout,'(I0,1x,A)') itest,'THERM models declared'
   CALL LOGMES(cout)

   
  ! then constructing the therm EF database 

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! first scaning of the M_MAILx and models 
  ! database determining the size of bdyty
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

   if( .not. allocated(M_bdyty) ) then
       call faterr(IAM,'please call ReadBodies before trying to LoadModels')
   end if

   ibdyty=0

   DO iM_bdyty=1,SIZE(M_bdyty)
     itest=0
     DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
       DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)

         DO imodel=1,SIZE(modelz)
           IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
             IF (modelz(imodel)%mdlty == 'THERM') itest=1
           ENDIF
         ENDDO   
         IF (itest == 1) EXIT
       ENDDO
       IF (itest == 1) EXIT
     ENDDO
     IF (itest == 1) ibdyty =ibdyty + 1 
   ENDDO

   ALLOCATE(bdyty(ibdyty),stat=errare)
   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating bdyty')
   END IF

   nb_therMAILx = ibdyty

   IF (ibdyty == 0) THEN
     CALL LOGMES('no therMAILx', .true.)
     CALL LOGMES('if any check BODIES.DAT or MODELS.DAT', .true.)
   ELSE
     ALLOCATE(bdyty2M_bdyty(ibdyty),stat=errare)
     IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating bdyty2M_bdyty')
     END IF

   END IF  
  
  IF (ibdyty == 0) RETURN   

  if (T_INTEGRATOR_ID == 0) then
      call faterr('load_models','thermal integrator not defined')    
  endif   
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! second scaning of the M_MAILx and models database 
  ! filling the correspondance table bdyty2M_bdyty and
  !  M2therm(iM_bdyty)%bdyty
  ! sizing bdyty(ibdyty)%blmty, bdyty(ibdyty)%blmty2M_blmty  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

   ibdyty=0

   DO iM_bdyty=1,SIZE(M_bdyty)
     itest=0
     iblmty=0
     DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
       DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)

         DO imodel=1,SIZE(modelz)
           IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
             IF (modelz(imodel)%mdlty == 'THERM') THEN 
               IF (itest == 0) THEN
                 ibdyty =ibdyty + 1
                 bdyty2M_bdyty(ibdyty)=iM_bdyty              
                 M2therm(iM_bdyty)%bdyty=ibdyty
                 itest=1
               ENDIF
               iblmty =iblmty + 1
             ENDIF
           ENDIF
         ENDDO   
       END DO
     ENDDO
     IF (itest /= 0) THEN
       ALLOCATE(bdyty(ibdyty)%blmty(iblmty),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'error allocating bdyty%blmty')
       END IF

       ALLOCATE(bdyty(ibdyty)%blmty2M_blmty(iblmty),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'error allocating bdyty%blmty2M_blmty')
       END IF

       ALLOCATE(M2therm(iM_bdyty)%blmty(SIZE(M_bdyty(iM_bdyty)%blmty)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'error allocating M2therm%blmty')
       END IF
       M2therm(iM_bdyty)%blmty=0

       ALLOCATE(M2therm(iM_bdyty)%nodty(SIZE(M_bdyty(iM_bdyty)%nodty)),stat=errare)
       IF (errare /= 0) THEN
         CALL FATERR(IAM,'error allocating M2therm%nodty')
       END IF
       M2therm(iM_bdyty)%nodty=0

     END IF  
   ENDDO

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! third scaning of the MAILx database: 
  ! filling the components of bdyty
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

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
             IF (modelz(imodel)%mdlty == 'THERM') THEN 
               iblmty =iblmty + 1

!                 print*,ibdyty,iblmty,modelz(imodel)%ID,get_nb_in_therEF(modelz(imodel)%ID)

               bdyty(ibdyty)%blmty(iblmty)%blmnb=get_nb_in_therEF(modelz(imodel)%ID)
               bdyty(ibdyty)%blmty(iblmty)%mdlnb=imodel

!xxx fd test 30/01/09

               bdyty(ibdyty)%blmty(iblmty)%lawnb = &
                  get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
!xxx
               bdyty(ibdyty)%blmty2M_blmty(iblmty)=iM_blmty
               M2therm(iM_bdyty)%blmty(iM_blmty)=iblmty 
!              construction de la table de connectivite
!
               inodty=SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%NODES(inodty),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%NODES')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%NODES(:)=0

!              construction de la table de correspondance entre ddl locaux et ddl globaux
!              et des matrices et vecteurs elementaires
!

!
! est ce valide !!!!
! il faudrait verifier que le nombre de noeuds lus et dans le type sont les memes ! 
!

               idof=inodty*get_N_DOF_by_NODE_therEF(modelz(imodel)%ID)
               bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node=get_N_DOF_by_NODE_therEF(modelz(imodel)%ID)
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%edof2gdof(idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%edof2gdof')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%edof2gdof(:)=0

               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%conductivity(idof,idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%conductivity')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%conductivity(:,:)=0.d0

               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%convection(idof,idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%convective')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%convection(:,:)=0.d0

               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%capacity(idof,idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%capacity')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%capacity(:,:)=0.d0
               
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%capacity_supg(idof,idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%capacity_supg')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%capacity_supg(:,:)=0.d0
               
               !ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%KTloc(idof,idof),stat=errare)
               !IF (errare /= 0) THEN
               !  CALL FATERR(IAM,'error allocating bdyty%blmty%KTloc')
               !END IF
               !bdyty(ibdyty)%blmty(iblmty)%KTloc(:,:)=0.d0
!
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%Fext(idof,2),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%Fext')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%Fext(:,:)=0.d0
!
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%Fint(idof,2),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%Fint')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%Fint(:,:)=0.d0
               !am : allocation et initialisation des vecteurs de flux externes, dans la
               !     configuration milieu
               allocate(bdyty(ibdyty)%blmty(iblmty)%ttFext(idof),stat=errare)
               if (errare /= 0) then
                 call FATERR(IAM,'error allocating bdyty%blmty%ttFext')
               end if
               bdyty(ibdyty)%blmty(iblmty)%ttFext(:)=0.d0
!
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ttFint(idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%ttFint')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%ttFint(:)=0.d0
!
               ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%RHSloc(idof),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating bdyty%blmty%RHSloc')
               END IF
               bdyty(ibdyty)%blmty(iblmty)%RHSloc(:)=0.d0
!
!              construction du vecteur contenant les valeurs aux points de Gauss
!

               nb_external=modelz(imodel)%nb_external_variables
               nb_internal=modelz(imodel)%nb_internal_variables


               !fd 30/01/09 
               ! gestion des fields: external (model) & bulk
               ! on rapatrie tous les fields declares et on s'en sert pour initialiser les champs aux pg 
               ! on donne au field le rang dans la pile de field stocke au pg

               nb_ef = get_external_field_nb(imodel) 

               nb_bf = get_bulk_field_nb(bdyty(ibdyty)%blmty(iblmty)%lawnb)
               nbf = nb_ef + nb_bf

               nb_evf = get_external_nb_vfield(imodel) 

               if (nbf /= 0) then
                 allocate(field_name(nbf))
                 do if=1,nb_ef
                    field_name(if) = get_external_field_name(imodel,if)
                 enddo                      
                 do if=1,nb_bf
                    field_name(nb_ef+if) = get_bulk_field_name(bdyty(ibdyty)%blmty(iblmty)%lawnb,if)
                 enddo                      
               end if

               if (nb_evf /= 0 ) then
                 allocate(vfield_name(nb_evf))
                 do IF = 1,nb_evf
                   vfield_name(IF) = get_external_vfield_name(imodel,IF)
                 end do
                 vsize = get_external_vfield_max_size(imodel)
               end if

               if( nbf/=0 .and. nb_evf/=0 ) then
                 CALL init_thergpv_MAILx(iM_bdyty,iM_blmty, & 
                                         get_N_GP_therEF(modelz(imodel)%ID), &
                                         nb_external,nb_internal,Tref, &
                                         nbf,field_name,nb_evf,vfield_name,vsize)

                 deallocate(field_name,vfield_name)

               else if( nbf/=0 ) then
                 call init_thergpv_MAILx(iM_bdyty,iM_blmty, & 
                                         get_N_GP_therEF(modelz(imodel)%ID), &
                                         nb_external,nb_internal,Tref, &
                                         nbf,field_name)

                 deallocate(field_name)

               else if( nb_evf/=0 ) then
                 call init_thergpv_MAILx(iM_bdyty,iM_blmty, & 
                                         get_N_GP_therEF(modelz(imodel)%ID), &
                                         nb_external,nb_internal,Tref, &
                                         nb_vfields=nb_evf,vfield_name=vfield_name,vsize=vsize)

                 deallocate(vfield_name)

               else
                 call init_thergpv_MAILx(iM_bdyty,iM_blmty, & 
                                         get_N_GP_therEF(modelz(imodel)%ID), &
                                         nb_external,nb_internal,Tref)
               end if
            ENDIF
           END IF
         ENDDO
       END DO
     ENDDO

     IF (iblmty == 0) THEN
       CALL FATERR(IAM,'no blmty')
     ENDIF
   ENDDO

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !perform some temporary computations
   ! we look for the nodes owing to therMAILx
   ! 
   ! first we scan the MAILx database and we count the 
   ! nodes owing to a THERM element 
   !
   ! second we fill the bdyty ... database and we determine
   ! the nodty of a node which is the highest one
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   DO ibdyty=1,SIZE(bdyty)
     iM_bdyty=bdyty2M_bdyty(ibdyty)
     inodty=0

!     print*,iM_bdyty,size(M_bdyty(iM_bdyty)%nodty),size(M_bdyty(iM_bdyty)%nod2blmty)
              
     DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
       DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
         iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)
         IF (M2therm(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
           inodty = inodty+1
           EXIT
         ENDIF
       ENDDO
     ENDDO

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
       ENDDO
     ELSE
      CALL FATERR(IAM,'error computing size of bdyty%nodty')
     ENDIF       

     inodty=0
     DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
       itest=0
       DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
         iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)      
         IF (M2therm(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
           IF (itest == 0) THEN
             inodty = inodty+1
             bdyty(ibdyty)%nodty2M_nodty(inodty)=iM_nodty
             M2therm(iM_bdyty)%nodty(iM_nodty)=inodty
             itest=1
           ENDIF
! a la peche au type de l'element ...
           iblmty=M2therm(iM_bdyty)%blmty(iM_blmty)
           imodel=bdyty(ibdyty)%blmty(iblmty)%mdlnb
           ctempo=modelz(imodel)%ID
! a la peche au type de noeuds de l'element
           itempo=get_n_dof_by_node_therEF(ctempo) 

! on garde le type de celui qui a le + de ddls
           IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) <= itempo) &
             call new_nodty(bdyty(ibdyty)%nodty(inodty),get_node_name_from_id(itempo))
         ENDIF
       ENDDO
     ENDDO
   ENDDO

   DO iM_bdyty=1,SIZE(M2therm)
     IF (M2therm(iM_bdyty)%bdyty /=0) THEN
       DO iM_blmty=1,SIZE(M2therm(iM_bdyty)%blmty)
         IF (M2therm(iM_bdyty)%blmty(iM_blmty) /=0) THEN
           ibdyty=M2therm(iM_bdyty)%bdyty
           iblmty=M2therm(iM_bdyty)%blmty(iM_blmty)
           DO inodty=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             iM_nodty=M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)
             bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)=M2therm(iM_bdyty)%nodty(iM_nodty)
           ENDDO
         ENDIF
       ENDDO
     ENDIF
   ENDDO

   DO ibdyty=1,SIZE(bdyty)

     iccdof=SIZE(bdyty(ibdyty)%nodty)

     bdyty(ibdyty)%nbdof=iccdof

     IF (iccdof /= 0) THEN
       ALLOCATE(bdyty(ibdyty)%nodnb(iccdof),stat=errare)
       ALLOCATE(bdyty(ibdyty)%dofnb(iccdof),stat=errare)
       ALLOCATE(bdyty(ibdyty)%Tbegin(iccdof),stat=errare)
       ALLOCATE(bdyty(ibdyty)%T(iccdof)     ,stat=errare)
       ALLOCATE(bdyty(ibdyty)%Tlast(iccdof)     ,stat=errare)
       ALLOCATE(bdyty(ibdyty)%Taux(iccdof)  ,stat=errare)
       ALLOCATE(bdyty(ibdyty)%residu(iccdof),stat=errare)
       ALLOCATE(bdyty(ibdyty)%Fext(iccdof)  ,stat=errare)
       ALLOCATE(bdyty(ibdyty)%Fint(iccdof)  ,stat=errare)

       IF (errare /= 0) THEN
         CALL FATERR(IAM,'error allocating X,V')
       END IF

       bdyty(ibdyty)%nodnb=0
       bdyty(ibdyty)%dofnb=0

       bdyty(ibdyty)%Tbegin=0.d0
       bdyty(ibdyty)%T     =0.d0
       bdyty(ibdyty)%Tlast =0.d0
       bdyty(ibdyty)%Taux  =0.d0
       bdyty(ibdyty)%residu=0.d0
       bdyty(ibdyty)%Fext  =0.d0 
       bdyty(ibdyty)%Fint  =0.d0
     ELSE 
     
       NULLIFY(bdyty(ibdyty)%nodnb)
       NULLIFY(bdyty(ibdyty)%dofnb)
     
       NULLIFY(bdyty(ibdyty)%Tbegin)
       NULLIFY(bdyty(ibdyty)%T)
       NULLIFY(bdyty(ibdyty)%Tlast)
       NULLIFY(bdyty(ibdyty)%Taux)
       NULLIFY(bdyty(ibdyty)%residu)
       NULLIFY(bdyty(ibdyty)%Fext)
       NULLIFY(bdyty(ibdyty)%Fint)

!                              1234567890123456789012345
       WRITE(cout,'(A25,I0)') 'Warning MAILx without DOF',ibdyty 
       CALL LOGMES(cout)
     END IF
     
     ! array node -> first global ddl

     ! fd attention j'augmente la taille de ccdof pour l'utilisation du g_system
     
     IF (SIZE(bdyty(ibdyty)%nodty) /= 0) THEN

          ALLOCATE(bdyty(ibdyty)%ccdof(SIZE(bdyty(ibdyty)%nodty)+1),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating ccdof in load_models')
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

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       iccdof=0
       DO itempo=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
         bdyty(ibdyty)%blmty(iblmty)%edof2gdof(itempo)=inodty
       ENDDO
     ENDDO
     
     !!ALLOCATE(bdyty(ibdyty)%perm(bdyty(ibdyty)%nbdof))
     !!ALLOCATE(bdyty(ibdyty)%inv_perm(bdyty(ibdyty)%nbdof))

        
     !!bdyty(ibdyty)%perm = (/ (i,i=1,bdyty(ibdyty)%nbdof) /)
     !!bdyty(ibdyty)%inv_perm = bdyty(ibdyty)%perm
     
     !! connectivite      
     connectivities => get_ll_connectivity_therMAILx(ibdyty)

     max_dofs_adj = get_nb_max_dofs_adj_therMAILx(ibdyty)

     call initialize_system(bdyty(ibdyty)%g_sys,Matrix_storage,Matrix_shape,bdyty(ibdyty)%ccdof,connectivities,max_dofs_adj)
     
     do while( associated(connectivities) )
       tmp => connectivities%n
        deallocate(connectivities%connec)
       deallocate(connectivities)
       connectivities => tmp
     end do

   END DO
 
 IF (itchache) THEN

   PRINT*,'Nombre de corps therMAILx:',SIZE(bdyty)
   DO ibdyty=1,SIZE(bdyty)
     PRINT*,'==========================================='
     PRINT*,'Corps: ',ibdyty
     PRINT*,'Correspondance dans la base Mailx: ',bdyty2M_bdyty(ibdyty)
     PRINT*,'PARANOIAC TEST local->global->local',M2therm(bdyty2M_bdyty(ibdyty))%bdyty
     PRINT*,'**nodty************'
     DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
       PRINT*,'Noeud: ',inodty
       PRINT*,'Type de noeud: ',get_nodNAME(bdyty(ibdyty)%nodty(inodty))
       PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%nodty2M_nodty(inodty)
       PRINT*,'ddl s commencant a: ',bdyty(ibdyty)%ccdof(inodty),'nombre: ',nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
     PRINT*,'PARANOIAC TEST local->global->local',M2therm(bdyty2M_bdyty(ibdyty))%nodty(bdyty(ibdyty)%nodty2M_nodty(inodty))
     ENDDO
     PRINT*,'**blmty************'
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       PRINT*,'Element: ',iblmty
       PRINT*,'Connectivite:',bdyty(ibdyty)%blmty(iblmty)%NODES(:)
       imdlnb=bdyty(ibdyty)%blmty(iblmty)%mdlnb
       PRINT*,'Num de l element dans la liste therEF: ',bdyty(ibdyty)%blmty(iblmty)%blmnb
       PRINT*,'ID du modele: ',modelz(imdlnb)%ID,' Num de modele: ',imdlnb 
       PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%blmty2M_blmty(iblmty)
     ENDDO
   ENDDO


  PRINT*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

ENDIF

! DA : Utilisation des G_matrix pour la thermique

    DO ibdyty=1,SIZE(bdyty)

      ! le second membre
      ALLOCATE(bdyty(ibdyty)%RHS(bdyty(ibdyty)%nbdof),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating bdyty%RHS')
      END IF
      bdyty(ibdyty)%RHS = 0.d0

    END DO


END SUBROUTINE read_models_therMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE read_behaviours_therMAILx

   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,ibehav,imodel
   INTEGER :: iM_bdyty,iM_blmty,iM_behav,iM_model

   INTEGER :: itest 

   CHARACTER(len=103) :: cout
                             !123456789012345678901234567
   CHARACTER(len=27)  :: IAM='therMAILx::read_behaviours'


!xxx fd 30/01/09  BUGGY voir mecaMAILx !!


   IF (nb_therMAILx == 0) RETURN

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
         bdyty(ibdyty)%blmty(iblmty)%lawnb = &
         get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))

       IF (bdyty(ibdyty)%blmty(iblmty)%lawnb == 0) THEN
!                                    12345678901          12345678901234567 
         WRITE(cout,'(A11,I0,A17)') 'therm MAILX ',ibdyty,'without behaviour'
       
         CALL LOGMES('check BODIES.DAT in DATBOX')
         CALL LOGMES('check BEHAVIOURS.DAT in DATBOX')
         CALL FATERR(IAM,cout)
       END IF
     END DO
   END DO  
   
END SUBROUTINE read_behaviours_therMAILx
!------------------------------------------------------------------------
!>\}
!!!---------------------------------------------------------
SUBROUTINE write_xxx_Rnod_therMAILx(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc)))) 
       !CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc)))) 
       !CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       !CALL write_out_Rnod(6,ifrom,ito)
    END SELECT
    
END SUBROUTINE write_xxx_Rnod_therMAILx
!------------------------------------------------------------------------   
SUBROUTINE read_driven_dof

   IMPLICIT NONE

   INTEGER :: errare ,itest 
   INTEGER :: ivd,ifd
   INTEGER :: ibdyty,inodty,dofnb
   INTEGER :: iM_bdyty,iM_nodty
   CHARACTER(len=103) :: cout
   CHARACTER(len=5)   :: chnod
!                               123456789012345678901234567
   CHARACTER(len=27)    :: IAM='therMAILx::read_driven_dof'
  
   IF (nb_therMAILx == 0) RETURN

   ! am: on initialise a 0 le nombre de noeuds avec CL imposees
   !     pour le cas ou on veut avoir dT/dn nul au bord, sur tout le bord
   !     i.e. sans donner de noeud avec CL imposee

   ! pour chaque modele de thermique
   do ibdyty=1, size(bdyty)

      NULLIFY(bdyty(ibdyty)%temp_driven_dof, &
              bdyty(ibdyty)%Tdriv, bdyty(ibdyty)%is_Tdriv_active, &
              bdyty(ibdyty)%drvdofs,bdyty(ibdyty)%drvvalues)
      ! on initialise le nombre de noeuds avec temperature imposee a 0
      bdyty(ibdyty)%nb_temp_driven_dof = 0
 
      
      NULLIFY(bdyty(ibdyty)%flux_driven_dof, &
              bdyty(ibdyty)%Fdriv)
      ! on initialise le nombre de noeuds avec flux impose a 0
      bdyty(ibdyty)%nb_flux_driven_dof = 0
      
   end do

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
   ! first reading: sizing array vlocy_driven_dof  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   

   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty'

     ivd=0
     ifd=0


     IF( .NOT. read_G_clin()) THEN
       CALL FATERR(IAM,'Problem reading bdyty')
     ENDIF
     itest = itest_bdyty_MAILx(G_clin)  
     IF (itest .NE. ifound) CYCLE

!    we keep the body number   
     READ(G_clin(7:13),'(I7)') iM_bdyty    



     IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
       
       WRITE(cout,'(A12,I0,A30)')'body number ',iM_bdyty,' does not belong to collection'
                                 !123456789012            12345678901234567890123456789
       CALL FATERR(IAM,cout)
     END IF

     ibdyty=M2therm(iM_bdyty)%bdyty

!    if it's a body without THERMx behaviour

     IF (ibdyty == 0) CYCLE     
!
     DO
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) == '$$$$$') EXIT
       IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
       IF ( .NOT. read_G_clin()) THEN
         CALL FATERR(IAM,'Problem reading model')
       ENDIF   

       IF (G_clin(2:6) /= 'THERM') CYCLE                ! fishing the THERMx part 

       DO
         IF ( .NOT. read_G_clin()) EXIT
         IF (G_clin(2:6) == '     ') CYCLE
         IF (G_clin(2:6) == 'model' .OR. G_clin(2:6) == '$$$$$') EXIT 

         IF (G_clin(2:6) /= 'nodty') THEN                ! fishing for the keyword 'nodty' 
           CALL FATERR(IAM,'keyword nodty expected')
         ENDIF

         DO
           IF( .NOT. read_G_clin()) EXIT
           IF(G_clin(2:6) == '     ') CYCLE
           IF (.NOT. is_a_nodty(G_clin(2:6))) THEN
             CALL FATERR(IAM,'Problem reading nodty')
           ENDIF           

           chnod=G_clin(2:6)
          
           READ(G_clin(7:13),'(I7)') iM_nodty    

           IF (iM_nodty <= 0 .OR. iM_nodty > SIZE(M_bdyty(iM_bdyty)%nodty)) THEN 
             WRITE(cout,'(A12,I0,A25,I0)') 'node number ',iM_nodty,' does not belong to body ',iM_bdyty
                                           !123456789012            1234567890123456789012345
             CALL FATERR(IAM,cout)
           ENDIF

           inodty=M2therm(iM_bdyty)%nodty(iM_nodty)

           IF ( get_node_id_from_name(chnod) > &
                nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN

               WRITE(cout,'(A6,A5,A49)') 'nodty ',chnod,' incompatible with the one belonging to the body '
                                         !123456         1234567890123456789012345678901234567890123456789
               CALL FATERR(IAM,cout)
           ENDIF

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
                 CASE('temp ') 
                   ivd=ivd+1

                   READ(G_clin( 9: 13),'(I5)') dofnb

                   IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                     WRITE(cout,'(A11,I5,A26,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                                                   !12345678901         12345678901234567890123456
                     CALL FATERR(IAM,cout)
                   ENDIF


                 CASE('flux ') 
                   ifd=ifd+1
                   READ(G_clin( 9: 13),'(I5)') dofnb

                   IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                     WRITE(cout,'(A11,I5,A26,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                                                   !12345678901         12345678901234567890123456
                     CALL FATERR(IAM,cout)
                   ENDIF

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
     ENDDO
     BACKSPACE(G_nfich)  


     bdyty(ibdyty)%nb_temp_driven_dof=ivd

     ALLOCATE(bdyty(ibdyty)%temp_driven_dof(ivd),stat=errare)
     IF (errare/=0) THEN
       CALL FATERR(IAM,'error allocating temp_driven_dof')
     END IF 

     ALLOCATE(bdyty(ibdyty)%Tdriv(ivd),stat=errare)
     IF (errare/=0) THEN
       CALL FATERR(IAM,'error allocating Tdriv')
     END IF 

     ALLOCATE(bdyty(ibdyty)%is_Tdriv_active(ivd),stat=errare)
     IF (errare/=0) THEN
       CALL FATERR(IAM,'error allocating is_Tdriv_active')
     END IF 
     bdyty(ibdyty)%is_Tdriv_active = .true.

     ALLOCATE(bdyty(ibdyty)%drvdofs(ivd),bdyty(ibdyty)%drvvalues(ivd),stat=errare)
     IF (errare/=0) THEN
        CALL FATERR(IAM,'error allocating drvdofs/drvvalues')
     END IF

     IF (ivd == 0) THEN
       WRITE (cout,'(A,I0,A)') 'Warning: therMAILx ',ibdyty,' without temp_driven_dof'
       CALL LOGMES(cout)
     ELSE
         bdyty(ibdyty)%Tdriv(:)    =0.d0
         bdyty(ibdyty)%drvdofs(:)  =0
         bdyty(ibdyty)%drvvalues(:)=0.d0
     ENDIF

     bdyty(ibdyty)%nb_flux_driven_dof=ifd

     ALLOCATE(bdyty(ibdyty)%flux_driven_dof(ifd),stat=errare)
     IF (errare/=0) THEN
       CALL FATERR(IAM,'error allocating flux_driven_dof')
     END IF

     ALLOCATE(bdyty(ibdyty)%Fdriv(ifd),stat=errare)
     IF (errare/=0) THEN
       CALL FATERR(IAM,'error allocating Fdriv')
     END IF

     IF (ifd == 0) THEN
       WRITE (cout,'(A,I0,A)') 'Warning: therMAILx ',ibdyty,' without flux_driven_dof'
       CALL LOGMES(cout)
     ELSE
         bdyty(ibdyty)%Fdriv(:)   =0.d0
     ENDIF


   END DO

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
   ! second reading: filling in data
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     

   REWIND(G_nfich)

   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty' 

     ivd=0
     ifd=0

     IF( .NOT. read_G_clin()) THEN
       CALL FATERR(IAM,'Problem reading bdyty')
     ENDIF
     itest = itest_bdyty_MAILx(G_clin)                      
     IF (itest .NE. ifound) CYCLE

!    we keep the body number   
     READ(G_clin(7:13),'(I7)') iM_bdyty    
     ibdyty=M2therm(iM_bdyty)%bdyty

     DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) == '$$$$$') EXIT
       IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
       IF ( .NOT. read_G_clin()) THEN
         CALL FATERR(IAM,'Problem reading model')
       ENDIF   

       IF (G_clin(2:6) /= 'THERM') CYCLE                ! fishing the THERM part 

       DO
         IF ( .NOT. read_G_clin()) EXIT
         IF (G_clin(2:6) == '     ') CYCLE
         IF (G_clin(2:6) == 'model' .OR. G_clin(2:6) == '$$$$$') EXIT 

         IF (G_clin(2:6) /= 'nodty') THEN                ! fishing for the keyword 'nodty' 
           CALL FATERR(IAM,'keyword nodty expected')
         ENDIF

         DO
           IF( .NOT. read_G_clin()) EXIT
           IF(G_clin(2:6) == '     ') CYCLE
           IF (.NOT. is_a_nodty(G_clin(2:6))) THEN
             CALL FATERR(IAM,'Problem reading nodty')
           ENDIF           

           chnod=G_clin(2:6)
          
           READ(G_clin(7:13),'(I7)') iM_nodty    

           inodty=M2therm(iM_bdyty)%nodty(iM_nodty)

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
                 CASE('temp ') 
                   ivd=ivd+1
                 
                   CALL read_a_driven_dof(chnod,inodty,G_clin,bdyty(ibdyty)%temp_driven_dof(ivd))

                 CASE('flux ') 
                   ifd=ifd+1

                   CALL read_a_driven_dof(chnod,inodty,G_clin,bdyty(ibdyty)%flux_driven_dof(ifd))

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
     ENDDO
     BACKSPACE(G_nfich)  

   ENDDO
  
END SUBROUTINE read_driven_dof
!!!------------------------------------------------------------------------ 
 SUBROUTINE write_driven_dof(nfich)

    IMPLICIT NONE

    INTEGER :: ivd,ifd,iivd,iifd,nfich
    INTEGER :: ibdyty,inodty,idof
    INTEGER :: iM_bdyty,iM_nodty
    
    IF (nb_therMAILx == 0) RETURN
    
    DO ibdyty = 1,SIZE(bdyty)
       ! the ibdyty body has some driven dof
       
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)      'MAILx',bdyty2M_bdyty(ibdyty)
       WRITE(nfich,'(A6)') '$model'
       WRITE(nfich,'(A6)') ' THERM'
       
       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iivd=0
          iifd=0
          DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof
             IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%temp_driven_dof(ivd))) THEN
                iivd=ivd
                EXIT
             END IF
          END DO
          DO ifd=1,bdyty(ibdyty)%nb_flux_driven_dof
             IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%flux_driven_dof(ifd))) THEN
                iifd=ifd
                EXIT
             END IF
          END DO
          
          IF (iivd > 0 .OR. iifd > 0) THEN
             ! the ibdyty body with inodty node has driven dof
             WRITE(nfich,'(A6)') '$nodty'
             iM_bdyty=bdyty2M_bdyty(ibdyty)
             iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
             WRITE(nfich,101) get_nodNAME(M_bdyty(iM_bdyty)%nodty(iM_nodty)),inodty
             ! write some 
             CALL write_a_driven_dof(nfich)
             DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof
                IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%temp_driven_dof(ivd))) THEN
                   CALL write_a_driven_dof(nfich,'temp ',bdyty(ibdyty)%temp_driven_dof(ivd))
                END IF
             END DO
             DO ifd=1,bdyty(ibdyty)%nb_flux_driven_dof
                IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%flux_driven_dof(ifd))) THEN
                   CALL write_a_driven_dof(nfich,'flux ',bdyty(ibdyty)%flux_driven_dof(ifd))
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
    INTEGER          :: itest,iM_bdyty,iM_nodty,iM_ccdof
    CHARACTER(len=5) :: chnod
    INTEGER          :: errare
    
    CHARACTER(len=23)  :: IAM='therMAILx::read_in_dof'
    CHARACTER(len=103) :: cout
    
    IF (nb_therMAILx == 0) RETURN
    
    DO ibdyty = 1,SIZE(bdyty)
       DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)
          bdyty(ibdyty)%Tbegin(inodty) =273.D0
          bdyty(ibdyty)%T(inodty)      =273.D0
       END DO
    END DO
    
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest .NE. ifound) CYCLE
       READ(G_clin(7:13),'(I7)') ibdyty
       IF (ibdyty <= 0 .OR. ibdyty > SIZE(bdyty)) THEN
          WRITE(cout,'(A12,I0,A60)') 'body number ',ibdyty,' does not belong to collection'
          CALL LOGMES('Error '//IAM//': '//cout)
       END IF

       ! fd recherche du model ...

       DO    
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'THERM') CYCLE                ! fishing the THERM part 

          DO    
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty' 
             DO
                IF( .NOT. read_G_clin()) EXIT
                itest=itest_nodty_MAILx(G_clin,ibdyty)
                IF (itest == isskip) CYCLE
                IF (itest == inomor) EXIT                      
                IF (itest == ifound) THEN
                   READ(G_clin(7:13),'(I7)')inodty
                   IF (inodty <= 0 .OR. inodty > SIZE(bdyty(ibdyty)%nodty)) THEN 
                      WRITE(cout,'(A12,I0,A25,I0,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                      CALL FATERR(IAM,cout)
                   END IF

                   IF (get_node_id_from_name(G_clin(2:6)) > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN 
                      WRITE(cout,'(A5,I0,A22,I0)') G_clin(2:6),inodty,' is not nodty of body ', ibdyty
                      CALL FATERR(IAM,cout)
                   END IF

                   chnod = G_clin(2:6)

                   CALL G_read_a_nodty(bdyty(ibdyty)%Tbegin(inodty:inodty),chnod)

                   bdyty(ibdyty)%T(inodty:inodty) = bdyty(ibdyty)%Tbegin(inodty:inodty) 
                END IF
                CYCLE
             END DO ! valeurs aux nodty 
             EXIT       
          END DO ! $nodty
          EXIT
       END DO ! $models
       CYCLE
    END DO ! $bdyty
    
END SUBROUTINE read_in_dof
!!!------------------------------------------------------------------------
SUBROUTINE write_out_dof(nfich,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: ifrom,ito
    INTEGER :: ibdyty,inodty,nfich
    INTEGER :: lc 
   
    !!   write(66,'(2(1x,D20.13)') TPS,bdyty(1)%T(1)-293.

    DO ibdyty = ifrom,ito
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)'MAILx',ibdyty
       WRITE(nfich,'(A6)') '$model'
       WRITE(nfich,'(A6)') ' THERM'
       WRITE(nfich,'(A6)') '$nodty'

       DO inodty = 1,SIZE(bdyty(ibdyty)%nodty) 
          
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%T(inodty:inodty), &
                             'T  ',nfich)
       END DO
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
    END DO
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
101 FORMAT(1X,A5,I7)            
    
END SUBROUTINE write_out_dof
!!!------------------------------------------------------------------------
SUBROUTINE read_in_gpv

    IMPLICIT NONE

    INTEGER          :: ibdyty,iblmty
    INTEGER          :: iM_bdyty,iM_blmty
    INTEGER          :: errare,itest
    INTEGER          :: mdlnb,nb_external,nb_internal,ig,i
 
    CHARACTER(len=22)  :: IAM='therMAILx::read_in_gpv'
    CHARACTER(len=103) :: cout
    CHARACTER(len=5)   :: ctest

    IF (nb_therMAILx == 0) RETURN

    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest .NE. ifound) CYCLE
       READ(G_clin(7:13),'(I7)') iM_bdyty
       IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
          WRITE(cout,'(A12,I0,A60)') 'body number ',iM_bdyty,' does not belong to collection'
          CALL FATERR(IAM,cout)
       END IF
    
       DO
          IF( .NOT. read_G_clin()) EXIT           
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE                 ! fishing for the keyword 'blmty'
          IF( .NOT. read_G_clin()) EXIT
          
          !fd a voir l'utilisation de test_blmty de MAILx
!!!mr?
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
             write(cout,'(A7,A5,A34)')' blmty ',G_clin(2:6),' unknown in read_in_gpv '
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
             END IF
             
             IF (G_clin(2:6) /= 'THERM') CYCLE                ! fishing the THERM part 
             
             ibdyty = M2therm(iM_bdyty)%bdyty
             iblmty = M2therm(iM_bdyty)%blmty(iM_blmty)
             
             mdlnb = bdyty(ibdyty)%blmty(iblmty)%mdlnb
             
             nb_external=modelz(mdlnb)%nb_external_variables
             nb_internal=modelz(mdlnb)%nb_internal_variables
             
             IF (nb_external == 0) THEN
                write(cout, '(A)') 'Material without external variable !'
                write(cout, '(A)') 'Check DATBOX/BODIES.DAT and/or DATBOX/MODELS.DAT'
                call faterr(IAM,cout)
             END IF
             
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv,dim=1)
                
                IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading ther_gpv values')
                
                READ(G_clin(1:28),'(2D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%T
                M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%T = &
                     M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%T
                
                IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading ther_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad(i)       
                END DO
                
                M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%grad= &
                     M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%grad    
                
                IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading ther_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux(i)       
                END DO
                
                M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%flux = &
                     M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%flux      
                
                IF (nb_internal /= 0) THEN
                   
                   IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading ther_gpv values')
                   
                   DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal)
                      READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal(i)       
                   END DO
                   M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,1)%internal = &
                        M_bdyty(ibdyty)%blmty(iblmty)%ther_gpv(ig,2)%internal
                END IF
             END DO
             EXIT
          END DO ! $model
          CYCLE
       END DO !blmty
       CYCLE
    END DO ! $bdyty
    
END SUBROUTINE read_in_gpv

!> \defgroup SystemTherMailx System 
!> \ingroup TherMailx
!> \brief This part describe the routines for solving the thermal problem
!> \addtogroup SystemTherMailx
!> \{
SUBROUTINE increment_therMAILx

    IMPLICIT NONE 

    INTEGER :: ibdyty,iblmty,ivd,inod,idof,iccdof
    REAL(kind=8) :: Tdrivenbegin,Tdriven
  
    IF (nb_therMAILx == 0) RETURN

    ! initializing T

    DO ibdyty=1,SIZE(bdyty)
       bdyty(ibdyty)%T= bdyty(ibdyty)%Tbegin                
       do iblmty = 1, SIZE(bdyty(ibdyty)%blmty)
         bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.d0
       end do
    END DO
    
    ! computing driven dof  
    DO ibdyty=1,SIZE(bdyty)
      DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof

         if (.not. bdyty(ibdyty)%is_Tdriv_active(ivd)) cycle
         
         CALL comp_a_driven_dof(bdyty(ibdyty)%Temp_driven_dof(ivd),Tdrivenbegin,Tdriven)
 
          bdyty(ibdyty)%Tdriv(ivd) = Tdriven
       END DO
    END DO
    
    !am: on initialise a 0 les flux externes par element a la fin du pas
    do ibdyty=1, size(bdyty)
      do iblmty=1, size(bdyty(ibdyty)%blmty)
         ! les flux externes a la fin du pas de temps
         bdyty(ibdyty)%blmty(iblmty)%Fext(:, 1) = 0.d0
         ! les flux eternes dans la configuration milieu
         bdyty(ibdyty)%blmty(iblmty)%ttFext = 0.d0
      end do
    end do

END SUBROUTINE increment_therMAILx
!!!------------------------------------------------------------------------    

SUBROUTINE initialize_elementary_flux_therMAILx

    IMPLICIT NONE 

    INTEGER :: ibdyty,iblmty

    IF (nb_therMAILx == 0) RETURN

    ! on initialise a 0 les flux externes par element au temps courant
    do ibdyty=1, size(bdyty)
      do iblmty=1, size(bdyty(ibdyty)%blmty)
         ! les flux externes a la fin du pas de temps
         bdyty(ibdyty)%blmty(iblmty)%Fext(:, 1) = 0.d0
         ! les flux eternes dans la configuration milieu
         bdyty(ibdyty)%blmty(iblmty)%ttFext = 0.d0
      end do
    end do

END SUBROUTINE

SUBROUTINE update_dof_therMAILx(ibdyty)

  IMPLICIT NONE 

  INTEGER :: ibdyty
                           !1234567890123456789012
  CHARACTER(len=22) :: IAM='thermMAILX::update_dof'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif
   
  bdyty(ibdyty)%Tbegin=bdyty(ibdyty)%T                

END SUBROUTINE update_dof_therMAILx
!!!------------------------------------------------------------------------  
SUBROUTINE compute_conductivity_therMAILx( ibdyty , istate)
  
    IMPLICIT NONE
    integer(kind=4) :: ibdyty
    INTEGER,INTENT(in) :: istate ! =0 comp lhs & rhs, =1 comp fields

    INTEGER :: errare,i,inodty
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: T_ele
    REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint

    INTEGER :: iblmty
    INTEGER :: iM_bdyty,iM_blmty

                             !12345678901234567890123456789012
    CHARACTER(len=32) :: IAM='thermMAILX::compute_conductivity'

    IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
      call faterr(IAM,'Unknown body') 
    endif
    
    if (istate /= 0 .and. istate /= 1) then
      CALL FATERR(IAM,'unsupported istate')
    endif

    !
    !CALL zero_matrix(bdyty(ibdyty)%KT)
    !

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       !
       IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
       ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       
       IF (errare /= 0) THEN
          CALL FATERR(IAM,'allocating coor_ele')
       ENDIF
       
       coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

       IF (ALLOCATED(T_ele)) DEALLOCATE(T_ele)
       ALLOCATE(T_ele(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       
       IF (errare /= 0) THEN
          CALL FATERR(IAM,'allocating T_ele')
       ENDIF

              
       IF (istate==0) THEN
           
           !print *,'compute conductivity istate = 0'
           
           IF (ALLOCATED(Fint)) DEALLOCATE(Fint)
           ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
       
           IF (errare /= 0) THEN
             CALL FATERR(IAM,'allocating Fint')
           ENDIF
       
           Fint = 0.d0

           DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

                  ! passage au numero global
                  inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
                  T_ele(i) = THETA_t * bdyty(ibdyty)%T(inodty) + (1.D0-THETA_t) * bdyty(ibdyty)%Tbegin(inodty)
                  !T_ele(i) =  bdyty(ibdyty)%T(inodty)
           END DO

           !
           ! on calcule les matrices elementaires ici
           ! a revoir plus tard ... 
           ! 

           iM_bdyty=bdyty2M_bdyty(ibdyty)
           iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

           CALL compute_elementary_conductivity(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                                bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                                H,coor_ele,T_ele,iM_bdyty,iM_blmty, &
                                                Fint, .true., &
                                                bdyty(ibdyty)%blmty(iblmty)%conductivity, .true., &
                                                .false.)

           bdyty(ibdyty)%blmty(iblmty)%ttFint(:) = bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + Fint

           DEALLOCATE(Fint)

       ELSE IF (istate == 1) THEN

           !print *,'compute conductivity istate = 1'
           DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

               ! passage au numero global
               inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i)
               T_ele(i)= bdyty(ibdyty)%T(inodty)

           END DO
     
           iM_bdyty=bdyty2M_bdyty(ibdyty)
           iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)     
           
           CALL compute_elementary_conductivity(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                                bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                                H,coor_ele,T_ele,iM_bdyty,iM_blmty, &
                                                Fint, .false., &
                                                bdyty(ibdyty)%blmty(iblmty)%conductivity, .false., &
                                                .true.)

       ENDIF
             
       DEALLOCATE(coor_ele,T_ele)

    END DO

END SUBROUTINE compute_conductivity_therMAILx
!!!------------------------------------------------------------------------ 
SUBROUTINE compute_capacity_therMAILx(ibdyty)

  IMPLICIT NONE

  INTEGER :: ibdyty
  ! ***
  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: DT_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint
  INTEGER :: iblmty,i,inodty
  INTEGER :: iM_bdyty,iM_blmty
  !                         1234567890123456789012345678
  CHARACTER(len=28) :: IAM='thermMAILX::compute_capacity'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif
  
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    !
    IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
    ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

    IF (errare /= 0) THEN
      CALL FATERR(IAM,'allocating coor_ele')
    ENDIF

    coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

    IF (ALLOCATED(DT_ele)) DEALLOCATE(DT_ele)
    ALLOCATE(DT_ele(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

    IF (errare /= 0) THEN
      CALL FATERR(IAM,'allocating DT_ele')
    ENDIF

    IF (ALLOCATED(Fint)) DEALLOCATE(Fint)
    ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

    IF (errare /= 0) THEN
      CALL FATERR(IAM,'allocating Fint')
    ENDIF

    Fint = 0.d0

    DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
  
      ! passage au numero global
      inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
      DT_ele(i) = bdyty(ibdyty)%T(inodty) - bdyty(ibdyty)%Tbegin(inodty)
  
    END DO

    !
    ! on calcule les matrices elementaires ici
    ! a revoir plus tard ... 
    ! 
    iM_bdyty  =  bdyty2M_bdyty(ibdyty)
    iM_blmty  =  bdyty(ibdyty)%blmty2M_blmty(iblmty)

    CALL compute_elementary_capacity(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     H,coor_ele,DT_ele,iM_bdyty,iM_blmty, &
                                     Fint , &
                                     bdyty(ibdyty)%blmty(iblmty)%capacity)
                                    
    bdyty(ibdyty)%blmty(iblmty)%ttFint(:) = bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + Fint
         
    DEALLOCATE(coor_ele,dt_ele,Fint)
          
  ENDDO

END SUBROUTINE compute_capacity_therMAILx
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------ 
SUBROUTINE compute_ttfint_therMAILx(ibdyty)
  IMPLICIT NONE

  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint
  INTEGER :: ibdyty,iblmty
  INTEGER :: iM_bdyty,iM_blmty
  INTEGER :: idof
  INTEGER :: nbdof,i,inodty,iccdof
  !                         1234567890123456789012345
  CHARACTER(len=25) :: IAM='therMAILX::compute_ttfint'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  !
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    !
    IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
    ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

    IF (errare /= 0) THEN
      CALL LOGMES('Error '//IAM//': allocating coor_ele')
    ENDIF

    coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      
    IF (ALLOCATED(Fint)) DEALLOCATE(Fint)
    ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

    IF (errare /= 0) THEN
      CALL FATERR(IAM,'allocating Fint')
    ENDIF

    Fint = 0.d0
    !
    ! on calcule les forces interieures
    ! 

    iM_bdyty=bdyty2M_bdyty(ibdyty)
    iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

    CALL compute_elementary_ttfint(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                   bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                   H,coor_ele,iM_bdyty,iM_blmty, &
                                   Fint)
                                     
    bdyty(ibdyty)%blmty(iblmty)%ttFint(:) = bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + Fint
      
    DEALLOCATE(coor_ele,Fint)
      !
  ENDDO

END SUBROUTINE compute_ttfint_therMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE compute_Fext_therMAILx(ibdyty)
  IMPLICIT NONE

  INTEGER :: ibdyty
  INTEGER :: idof,ifd,inod
  REAL(kind=8) :: Febegin,Fe

  INTEGER :: i

  !                         12345678901234567890123
  CHARACTER(len=23) :: IAM='therMAILx::compute_Fext'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,' Unknown body') 
  endif

  !fd
  !fd calcul des flux a t[n+theta*h]
  !fd

  bdyty(ibdyty)%Fext=0.d0

  ! driven forces

  DO ifd=1,bdyty(ibdyty)%nb_flux_driven_dof

    CALL comp_a_driven_dof(bdyty(ibdyty)%flux_driven_dof(ifd),Febegin,Fe)

    bdyty(ibdyty)%Fdriv(ifd)=((1.D0-THETA_t)*Febegin)+(THETA_t*Fe)

    CALL owner_of_a_driven_dof(bdyty(ibdyty)%flux_driven_dof(ifd),inod,idof)

    bdyty(ibdyty)%Fext(inod)=bdyty(ibdyty)%Fext(inod) + bdyty(ibdyty)%Fdriv(ifd)  

  END DO

END SUBROUTINE compute_Fext_therMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE assemb_KT_therMAILx(ibdyty)
  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,i
  INTEGER :: idof

  !                         12345678901234567890
  CHARACTER(len=20) :: IAM='therMAILX::assemb_KT'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  !CALL zero_matrix(bdyty(ibdyty)%KT)
  !CALL G_zero(bdyty(ibdyty)%KT)

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    !print*,'capacity'
    !write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%capacity
    !print*,'conductivity'
    !write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%conductivity
    !print*,'convection'
    !write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%convection

    call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
    call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%capacity)
    !!call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%capacity_supg)
    call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%conductivity,H*THETA_t)
    call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%convection,H*THETA_t)

  ENDDO 

END SUBROUTINE assemb_KT_therMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
SUBROUTINE apply_drvdof_KT_therMAILx(ibdyty)
  IMPLICIT NONE

  INTEGER :: ibdyty,ivd,iccdof,idof,inod

  !                         12345678901234567890123456
  CHARACTER(len=26) :: IAM='therMAILX::apply_drvdof_KT'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  !write(*,'(A,I0)') 'Body: ',ibdyty

  if (bdyty(ibdyty)%nb_temp_driven_dof /= 0) then

    DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof

      CALL owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)
         
      bdyty(ibdyty)%drvdofs(ivd)=bdyty(ibdyty)%ccdof(inod)+idof 

    enddo
    call set_drvdofs(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvdofs)

  else
  
    call erase_drvdofs(bdyty(ibdyty)%g_sys)

  endif

END SUBROUTINE apply_drvdof_KT_therMAILx  
!------------------------------------------------------------------------ 
SUBROUTINE assemb_RHS_therMAILx(ibdyty)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,i,inodty,iccdof,nbdof,errare
  REAL(kind=8),DIMENSION(:),ALLOCATABLE :: T_ele,DT_ele
  !                         123456789012345678901
  CHARACTER(len=21) :: IAM='therMAILX::assemb_RHS'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  bdyty(ibdyty)%RHS=0.D0
  bdyty(ibdyty)%Fint=0.D0

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    !print*,'element'
    !print*,ibdyty,iblmty

    !print*,'T_ele'
    !write(*,'(4(1x,E20.13)') T_ele

    bdyty(ibdyty)%blmty(iblmty)%RHSloc= H *( THETA_t * bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)  + &
                                             (1.d0 -THETA_t) * bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) - & 
                                             bdyty(ibdyty)%blmty(iblmty)%ttFint(:)  +                  &
    !am : ajout de la contribution des vecteurs des flux externes, dans
    !     la configuration milieu 

                                             bdyty(ibdyty)%blmty(iblmty)%ttFext(:) &
                                           )

    CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

    bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.D0

  ENDDO
    
  bdyty(ibdyty)%RHS = bdyty(ibdyty)%RHS + (H*bdyty(ibdyty)%Fext)



END SUBROUTINE assemb_RHS_therMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE update_bulk_therMAILx(ibdyty)
  IMPLICIT NONE

  integer :: ibdyty
  ! ***
  INTEGER :: errare
  INTEGER :: iM_bdyty,iblmty
  !                         1234567890123456789012345678
  CHARACTER(len=28) :: IAM='therMAILX::update_therm_bulk'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  !
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    !
    bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
    bdyty(ibdyty)%blmty(iblmty)%Fint(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fint(:,1)
  
  ENDDO

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  CALL update_thergpv_MAILx(iM_bdyty)

END SUBROUTINE update_bulk_therMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE compute_fint_therMAILx
  IMPLICIT NONE

  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: T_ele
  INTEGER :: ibdyty,iblmty
  INTEGER :: iM_bdyty,iM_blmty
  INTEGER :: idof
  INTEGER :: nbdof,i,inodty,iccdof
!                           1234567890123456789012345678901234567
  CHARACTER(len=37) :: IAM='thermMAILX::compute_fint_therMAILx'


   IF (nb_therMAILx == 0) RETURN

  DO ibdyty=1,SIZE(bdyty)
    !
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      !
    ENDDO
  ENDDO

 END SUBROUTINE compute_fint_therMAILx
!------------------------------------------------------------------------ 
!> \}
!------------------------------------------------------------------------   
SUBROUTINE apply_temp_driven_dof(ibdyty,storage)

  IMPLICIT NONE 
  INTEGER :: ivd,ibdyty,inod,idof
  INTEGER :: storage

  !                         12345678901234567890123456789012
  character(len=32) :: IAM='therMAILx::apply_temp_driven_dof'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  SELECT CASE (storage)

  CASE(iTaux_)

    DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof

       if (.not. bdyty(ibdyty)%is_Tdriv_active(ivd)) cycle

       CALL owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)

       bdyty(ibdyty)%Taux(inod)=bdyty(ibdyty)%Tdriv(ivd)    
    END DO  
  CASE default
    call faterr(IAM,'Unknown storage')
  END SELECT
END SUBROUTINE apply_temp_driven_dof
!------------------------------------------------------------------------   
!------------------------------------------------------------------------   
SUBROUTINE nullify_temp_driven_dof(ibdyty,storage)

  IMPLICIT NONE 
  INTEGER :: ivd,ibdyty,inod,idof,iccdof
  INTEGER :: storage

  !                         1234567890123456789012345678901234
  character(len=34) :: IAM='therMAILx::nullify_temp_driven_dof'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  SELECT CASE (storage) 
  CASE (iTaux_)
    DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof

      if (.not. bdyty(ibdyty)%is_Tdriv_active(ivd)) cycle

      CALL owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)

      bdyty(ibdyty)%Taux(inod)=0.d0

    ENDDO
  CASE default
    call faterr(IAM,'Unknown storage')
  END SELECT

END SUBROUTINE nullify_temp_driven_dof
!------------------------------------------------------------------------   
!------------------------------------------------------------------------
SUBROUTINE comp_dof_therMAILx(ibdyty)

  IMPLICIT NONE 
  INTEGER :: ibdyty,ivd,ibdy,inod,idof,iccdof,info

  !                         1234567890123456789
  character(len=19) :: IAM='therMAILx::comp_dof'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  bdyty(ibdyty)%Tlast = bdyty(ibdyty)%T
  bdyty(ibdyty)%Taux = 0.d0

  CALL set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%RHS)
    
  IF (bdyty(ibdyty)%nb_temp_driven_dof /= 0) THEN
    ! on collecte les valeurs des ddl imposes
    DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof

      CALL owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)

      iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

      bdyty(ibdyty)%drvvalues(ivd) = bdyty(ibdyty)%Tdriv(ivd) - bdyty(ibdyty)%T(iccdof)

    ENDDO
       
    ! on les passe au g_system

    call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)  
  ENDIF
    
  CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Taux,info)

  bdyty(ibdyty)%T = bdyty(ibdyty)%Tlast + bdyty(ibdyty)%Taux

 END SUBROUTINE comp_dof_therMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
FUNCTION get_cooref_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES
  INTEGER                     :: iM_bdyty,iM_nodty

  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_cooref_ele

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    get_cooref_ele(:,inodes)=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

  ENDDO

END FUNCTION get_cooref_ele
!------------------------------------------------------------------------
!------------------------------------------------------------------------
FUNCTION get_coor_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES
  INTEGER                     :: iM_bdyty,iM_nodty
  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_coor_ele

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    get_coor_ele(:,inodes)=get_coor_nodty_MAILx(iM_bdyty,iM_nodty)

  ENDDO
END FUNCTION get_coor_ele
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!> \defgroup DofsTherMailx Dofs
!> \ingroup TherMailx
!> \addtogroup DofsTherMailx
!>\{
FUNCTION get_Tbegin_nodty_therMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER      :: ibdyty,inodty
  REAL(kind=8) :: get_Tbegin_nodty_therMAILx

  get_Tbegin_nodty_therMAILx = bdyty(ibdyty)%Tbegin(inodty)

END FUNCTION get_Tbegin_nodty_therMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
FUNCTION get_T_nodty_therMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER      :: ibdyty,inodty
  REAL(kind=8) :: get_T_nodty_therMAILx

  get_T_nodty_therMAILx = bdyty(ibdyty)%T(inodty)

END FUNCTION get_T_nodty_therMAILx
!> \}
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
!> \defgroup MeshTherMailx Mesh/Model
!> \ingroup TherMailx
!> \addtogroup MeshTherMailx
!>\{
 INTEGER FUNCTION get_nb_therMAILx()

   IMPLICIT NONE

   get_nb_therMAILx = nb_therMAILx

 END FUNCTION get_nb_therMAILx

!------------------------------------------------------------------------ 
SUBROUTINE set_field_bynode(ibdyty,field_rank,fsize,field)
  IMPLICIT NONE

  INTEGER,INTENT(in)                       :: ibdyty,fsize,field_rank
  REAL(kind=8),INTENT(in),DIMENSION(fsize) :: field

  INTEGER :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare

  REAL(kind=8),ALLOCATABLE,DIMENSION(:) :: efield_gp,efield_node

                           !123456789012345
  CHARACTER(len=15) :: IAM='set_field_bynode'

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

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
    ALLOCATE(efield_gp(get_N_GP_therEF(modelz(imodel)%ID)),stat=errare)

    IF (errare /= 0) THEN
      CALL FATERR(IAM,'allocating efield_gp')
    END IF

    !
    ! on passe des noeuds aux pg
    ! 

    CALL interpolate_node2pg(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                             efield_node,efield_gp)       


    !fd on pose dans la bd mailx

    iM_bdyty = bdyty2M_bdyty(ibdyty)
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    DO ig=1,get_N_GP_therEF(modelz(imodel)%ID)
      CALL set_ther_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(ig))
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

  if (nb_therMAILx == 0) return

  if (fsize /= size(bdyty(ibdyty)%blmty)) then
    call FATERR(IAM,'non conforming vector fsize')
  end if

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_bdyty = bdyty2M_bdyty(ibdyty)
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_therEF(modelz(imodel)%ID)
      call set_ther_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,field(iblmty))
    end do 
  end do

end subroutine set_field_byelem

!------------------------------------------------------------------------ 
integer function get_field_rank(ibdyty,iblmty,name)
  IMPLICIT NONE
  INTEGER :: ibdyty,iblmty
  INTEGER :: iM_bdyty,iM_blmty
  character(len=*) :: name

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_field_rank = get_ther_field_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
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

  if (nb_therMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_therMAILx ) call faterr(IAM,'wrong therMAILx index')

  if (dim2/= bdyty(ibdyty)%nb_nodes) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  !fd interpolation de field au noeud a field au pg
  do iblmty = 1, size(bdyty(ibdyty)%blmty)

     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

     ! here ?
     if (dim1 > get_ther_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
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
     allocate(efield_gp(get_N_GP_therEF(modelz(imodel)%ID),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_gp')
     end if

     !
     ! on passe des noeuds aux pg
     ! 
     do i_f = 1, dim1
       call interpolate_node2pg(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                efield_node(:,i_f),efield_gp(:,i_f))
     end do

     !fd on pose dans la bd mailx

     do ig = 1,get_N_GP_therEF(modelz(imodel)%ID)
       call set_ther_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(ig,:),dim1)
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

  if (nb_therMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_therMAILx ) call faterr(IAM,'wrong therMAILx index')

  if (dim2/= size(bdyty(ibdyty)%blmty)) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    if (dim1 > get_ther_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
      call faterr(IAM,'input vector field too large')
    end if

    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_therEF(modelz(imodel)%ID)
      call set_ther_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,vfield(:,iblmty),dim1)
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

  get_vfield_rank = get_ther_vfield_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------ 
integer function get_nb_nodes_therMAILx(ibdyty)
  IMPLICIT NONE
  INTEGER :: ibdyty

  get_nb_nodes_therMAILx = bdyty(ibdyty)%nb_nodes

end function


function get_ptr_Tbeg_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  real(kind=8), dimension(:), pointer :: get_ptr_Tbeg_therMAILx

  get_ptr_Tbeg_therMAILx => bdyty(ibdyty)%Tbegin

end function get_ptr_Tbeg_therMAILx

function get_ptr_T_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  real(kind=8), dimension(:), pointer :: get_ptr_T_therMAILx

  get_ptr_T_therMAILx => bdyty(ibdyty)%T

end function get_ptr_T_therMAILx

function get_ptr_Tdriv_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  real(kind=8), dimension(:), pointer :: get_ptr_Tdriv_therMAILx

  get_ptr_Tdriv_therMAILx => bdyty(ibdyty)%Tdriv

end function get_ptr_Tdriv_therMAILx

function get_ptr_Fext_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  real(kind=8), dimension(:), pointer :: get_ptr_Fext_therMAILx

  get_ptr_Fext_therMAILx => bdyty(ibdyty)%Fext

end function get_ptr_Fext_therMAILx

function get_ptr_drvdofs_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  integer(kind=4), dimension(:), pointer :: get_ptr_drvdofs_therMAILx
  !
  integer(kind=4) :: ivd, inod, idof

  if( ibdyty<1 .or. ibdyty>nb_therMAILx ) then
    call faterr('therMAILx::get_ptr_drvdofs','Unknown body')
  endif

  if( bdyty(ibdyty)%nb_temp_driven_dof /= 0 ) then

    do ivd = 1, bdyty(ibdyty)%nb_temp_driven_dof
      call owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)
      bdyty(ibdyty)%drvdofs(ivd)=bdyty(ibdyty)%ccdof(inod)+idof
    end do

  endif

  get_ptr_drvdofs_therMAILx => bdyty(ibdyty)%drvdofs

end function get_ptr_drvdofs_therMAILx

function get_ptr_ccdof_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  integer(kind=4), dimension(:), pointer :: get_ptr_ccdof_therMAILx

  get_ptr_ccdof_therMAILx => bdyty(ibdyty)%ccdof

end function get_ptr_ccdof_therMAILx

function get_ptr_connec_therMAILx(ibdyty,iblmty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty, iblmty
  integer(kind=4), dimension(:), pointer :: get_ptr_connec_therMAILx

  get_ptr_connec_therMAILx => bdyty(ibdyty)%blmty(iblmty)%NODES

end function get_ptr_connec_therMAILx

function get_nb_max_dofs_adj_therMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in)     :: ibdyty
  integer(kind=4) :: get_nb_max_dofs_adj_therMAILx
  !
  integer(kind=4) :: max_conn, iblmty

  max_conn = 0
  do iblmty = 1, size(bdyty(ibdyty)%blmty)
    max_conn = max(max_conn, size(bdyty(ibdyty)%blmty(iblmty)%NODES))
  end do

  get_nb_max_dofs_adj_therMAILx = max_conn * get_max_nod2el(bdyty2M_bdyty(ibdyty))

end function get_nb_max_dofs_adj_therMAILx

subroutine get_lhs_loc_therMAILx(ibdyty,iblmty,lhs_loc,nde)
  implicit none
  !> body number
  integer(kind=4), intent(in) :: ibdyty
  !> element number
  integer(kind=4), intent(in) :: iblmty
  !> elementary assemble lhs term
  real(kind=8), intent (out) :: lhs_loc(20,20)
  !> real size of elementary matrix
  integer(kind=4), intent(out) :: nde
  !
  !integer(kind=4) :: idof
  character(len=18) :: IAM = 'therMAILx::get_lhs_loc'
  
  if (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  end if

  ! Number of Dof by Element
  nde = size(bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
  lhs_loc(1:nde,1:nde) =           bdyty(ibdyty)%blmty(iblmty)%capacity+ &
                         H*THETA_t*bdyty(ibdyty)%blmty(iblmty)%conductivity+&
                         H*THETA_t*bdyty(ibdyty)%blmty(iblmty)%convection

end subroutine get_lhs_loc_therMAILx 

subroutine get_rhs_loc_therMAILx(ibdyty,iblmty,rhs_loc,nde)
  implicit none
  !>body number
  integer(kind=4), intent(in) :: ibdyty
  !>element number
  integer(kind=4), intent(in) :: iblmty
  !> elementary assemble rhs term
  real(kind=8), intent (out) :: rhs_loc(20)
  !> real size of elementary matrix
  integer(kind=4), intent(out) :: nde
  !
  !integer(kind=4) :: idof
  character(len=18) :: IAM = 'therMAILx::get_rhs_loc'
  
  if (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  end if

   !>\TODO WARNING missing Fext
   nde = size(bdyty(ibdyty)%blmty(iblmty)%RHSloc)
   rhs_loc(1:nde) = H *( THETA_t        * bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) + &
                        (1.d0 -THETA_t) * bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) - &
                                          bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + &
                                          bdyty(ibdyty)%blmty(iblmty)%ttFext(:)   &
                        )

end subroutine get_rhs_loc_therMAILx 



!----------------------------------------------------------------------
!am : debut des fonction supplementaires

! fonction qui renvoie le nombre d'elements portant un modele de thermique
! donne
function get_nb_elements_therMAILx(ibdyty)

   implicit none
   
   ! variables d'entree:
   integer, intent(in) :: ibdyty ! indice d'un modele dans therMAILx
   
   ! valeur de retour:
   integer :: get_nb_elements_therMAILx ! nombre d'elements portant un modele
      ! de thermique

   get_nb_elements_therMAILx = size(bdyty(ibdyty)%blmty)

end function get_nb_elements_therMAILx

! fonction qui renvoie vrai ssi tous les elements du maillage sur lequel
! repose le modele de thermique sont identiques
function only_one_elem_type_therMAILx(ibdyty)

   ! variable d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique

   ! valeur de retour :
   logical :: only_one_elem_type_therMAILx ! vaut vrai ssi tous les 
      ! elements du maillage sont du meme type

   ! variables locales :
   logical :: res ! pour coinstuire le resultat
   integer :: blmnb ! indice, dans ther_EF, de l'element utilise pour 
                    ! tout le maillage (si il existe)
   integer :: iblmty ! indice de boucle sur les elements
   
   ! on suppose initialement que tous les elements sont du meme type
   res = .true.
   
   ! on recupere l'indice dans teher_EF du premier element
   blmnb = bdyty(ibdyty)%blmty(1)%blmnb
  
   ! pour chaque element restant
   do iblmty=2, size(bdyty(ibdyty)%blmty)
   
      ! res contient vrai ssi l'element courant est du meme type que 
      ! les autres
      res = (blmnb == bdyty(ibdyty)%blmty(iblmty)%blmnb)
      
      ! si ce n'est pas le cas, on sort de la boucle
      if (.not. res) exit
   
   end do

   ! on renvoie le resultat obtenu
   only_one_elem_type_therMAILx = res

end function only_one_elem_type_therMAILx

! fonction qui recupere le nombre de noeud d'un element sur lequel est
! defini un modele de thermique
function get_N_NODE_therMAILx(ibdyty, iblmty)

   implicit none

   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique
   integer, intent(in) :: iblmty ! indice d'un element portant le modele de thermique
   
   ! valeur de retour :
   integer :: get_N_NODE_therMAILx ! nombre de noeud de l'element
   
   ! variables locales :
   integer :: iM_bdyty ! indice, dans MAILx, du corps portant le modele 
                       ! de thermique ibdyty
   integer :: iM_blmty ! indice dans la numerotation du corps iM_bdyty, de
                       ! l'element d'indice iblmty, dans la numerotation de
                       ! therMAILx
   character(len=5) :: blmID ! nom de l'element, dans la nomenclature LMGC

   ! on le numero du corps dans MAILx
   iM_bdyty=bdyty2M_bdyty(ibdyty)
   
   ! on recupere le numero de l'element dans la numerotation des 
   ! elements du corps d'indice iM_bdyty dans MAILx
   iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

   ! on recupere le nom de l'element
   blmID = M_bdyty(iM_bdyty)%blmty(iM_blmty)%blmID

   ! on renvoie le nombre de noeud pour cet element
   get_N_NODE_therMAILx = get_N_NODE_therEF(blmID)

end function get_N_NODE_therMAILx

! fonction qui recupere le type de la fonction de forme d'un element sur lequel est
! defini un modele de thermique
function get_T_FONC_FORME_therMAILx(ibdyty, iblmty)

   implicit none

   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique
   integer, intent(in) :: iblmty ! indice d'un element portant le modele de thermique
   
   ! valeur de retour :
   integer(kind=4) :: get_T_FONC_FORME_therMAILx ! type de la fonction
                                                 ! de forme de l'element
   
   ! on renvoie le type de la fonction de forme de cet element
   get_T_FONC_FORME_therMAILx = get_T_FONC_FORME_therEF( &
                                   bdyty(ibdyty)%blmty(iblmty)%blmnb)

end function get_T_FONC_FORME_therMAILx

! fonction qui renvoie un noeud d'indice donne dans la table de 
! connectivite, exprimee dans la numerotation locale dea noeuds, d'un
! element donne
subroutine get_conn_therMAILx(ibdyty, iblmty, conn, nb_nodes_loc)

   implicit none
   
   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique
   integer, intent(in) :: iblmty ! indice d'un element portant le modele de thermique
   integer, intent(in) :: nb_nodes_loc ! nombre de noeuds de la maille, presume
   
   ! variable de sortie :
   integer, dimension(nb_nodes_loc), intent(out) :: conn ! pour recuperer la table 
                                 ! de connectivite de l'element considere

   ! variables locales  :      1234567890123456789
   character(len=19) :: IAM = 'therMAILx::get_conn'
   
   ! on verifie que le nombre de noeud de la maille donne a bien un 
   ! sens pour cet element
   if (nb_nodes_loc .ne. size(bdyty(ibdyty)%blmty(iblmty)%NODES)) then
      ! si le nombre de noeud de la maille donne n'est pas celkui de
      ! l'element, on affiche un message d'erreur
      call faterr(IAM, &
              'recuperation de la table de connectivite impossible!')
   end if

   ! on recupere la table de connectivite de l'element
   conn = bdyty(ibdyty)%blmty(iblmty)%NODES

end subroutine get_conn_therMAILx

! fonction qui recupere les coordonnees de reference d'un noeud du 
! maillage sur lequel repose le modele de thermique 
function get_cooref_node_therMAILx(ibdyty, inodty)

   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice du modele de thermique dans therMAILx
   integer, intent(in) :: inodty ! indice du noeud dans la numerotation de noeuds portes
                                 ! par les elements portant un modele de thermique
                     
   ! valeur de retour :
   real(kind=8), dimension(nbDIME) :: get_cooref_node_therMAILx
   
   ! variables locales :
   integer :: iM_bdyty ! indice, dans MAILx, du corps portant le modele 
                       ! de thermique ibdyty
   integer :: iM_nodty ! indice dans la numerotation du corps iM_bdyty, du
                       ! du noeud d'indice inodty, dans la numerotation de
                       ! therMAILx

   ! on recupere le numero du corps dans MAILx
   iM_bdyty=bdyty2M_bdyty(ibdyty)
   
   ! on recupere le numero du noeud dans la numerotation des noeuds du
   ! corps d'indice iM_bdyty dans MAILx
   iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

   ! on recupere les coorodonnees de reference du noeud
   get_cooref_node_therMAILx = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

end function get_cooref_node_therMAILx

SUBROUTINE put_vector_therMAILx(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect

   IF (nb_therMAILx == 0) RETURN

   !fd pour la thermique nbdof == nb_nodes
   IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
     call faterr('therMAILx::put_vector','nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
    CASE('T____')
     bdyty(ibdyty)%T=vect
    CASE('Tbeg_')
     bdyty(ibdyty)%Tbegin=vect
    CASE('Fext_')
     bdyty(ibdyty)%Fext=bdyty(ibdyty)%Fext+vect
    CASE('Fint_')
     bdyty(ibdyty)%Fint=vect
    CASE('Taux_')
     bdyty(ibdyty)%Taux=vect
    CASE DEFAULT
     call faterr('therMAILx::put_vector','unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE put_vector_therMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE get_vector_therMAILx(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect

   ! variables locales
   INTEGER :: inodty 
   INTEGER :: iccdof 
   INTEGER :: iM_bdyty 
   INTEGER :: iM_nodty 
   INTEGER :: iM_ccdof 

                            !123456789012345678901
   CHARACTER(len=21) :: IAM='therMAILx::get_vector'

   IF (nb_therMAILx == 0) RETURN

   !fd pour la thermique nbdof == nb_nodes
   IF (id_vect /= 'Coor0' .and. nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
     call faterr(IAM,'nbdof non concordant')

   else if (id_vect == 'Coor0' .and. nbdof /= bdyty(ibdyty)%nb_nodes*nbdime ) THEN
     call faterr(IAM,'nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
   CASE('Coor0')
     ! on recupere le numero du corps MAILx corespondant au modele courant
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       ! pour chaque noeud
       DO inodty=1, bdyty(ibdyty)%nb_nodes
          ! on recupere l'indice qui debute la tranche concerant le noeud 
          ! courant dans un vecteur asssemble 
          iccdof=nbdime*(inodty-1)
          ! on recupere le numero du noeud courant dans la numerotation du coprs MAILx
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          ! on recupere l'indice qui debute la tranche concerant le noeud 
          ! courant dans un vecteur asssemble, pour le corps MAILx
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)

          ! on peut alors stocker les coordonnees de reference du noeud courant
          ! dans le vecteur resultat
          vect(iccdof+1:iccdof+nbDIME)=M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME)
       END DO
   CASE('T____')
     vect=bdyty(ibdyty)%T
   CASE('Tbeg_')
     vect=bdyty(ibdyty)%Tbegin
   CASE('Fint_')
     vect=bdyty(ibdyty)%Fint
   CASE('Fext_')
     vect=bdyty(ibdyty)%Fext
   CASE('Taux_')
     vect=bdyty(ibdyty)%Taux
   CASE DEFAULT
     call faterr(IAM,'Sorry unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE get_vector_therMAILx

 !> compute the coordinates of all gp of a therMAILx
 function get_gp_coor_therMAILx(ibdyty)
   implicit none
   integer :: ibdyty
   real(kind=8), dimension(:,:), pointer :: get_gp_coor_therMAILx
   ! ***
   !                         1234567890123456789012
   character(len=22) :: IAM='therMAILx::get_gp_coor'
   integer :: iblmty, iM_bdyty, iM_blmty
   integer :: nb_gp, i_gp
   real(kind=8), dimension(:,:), allocatable :: coor_ele

   get_gp_coor_therMAILx => null()

   nb_gp = 0
   do iblmty = 1, size(bdyty(ibdyty)%blmty)
     nb_gp = nb_gp + get_nb_gp_therMAILx(ibdyty, iblmty)
   end do


   allocate( get_gp_coor_therMAILx(nbDIME, nb_gp) )

   i_gp = 0
   do iblmty = 1, size(bdyty(ibdyty)%blmty)
     !
     allocate( coor_ele(nbDIME, size(bdyty(ibdyty)%blmty(iblmty)%NODES)) )

     coor_ele = get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES))

     nb_gp  = get_nb_gp_therMAILx(ibdyty, iblmty)
     get_gp_coor_therMAILx(:,i_gp+1:i_gp+nb_gp) = 0.d0

     iM_bdyty = bdyty2M_bdyty(ibdyty)
     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

     call get_ele_pg_coor(bdyty(ibdyty)%blmty(iblmty)%blmnb, coor_ele, &
                          get_gp_coor_therMAILx(:,i_gp+1:i_gp+nb_gp)   )
      i_gp = i_gp + nb_gp
      deallocate( coor_ele )
    end do

 end function

 function get_gp_field_therMAILx(ibdyty,iblmty,ig,ifield)
   !
   ! routine qui recupere les contraintes principales en un point de gauss
   !
   IMPLICIT NONE

   real(kind=8),dimension(3) :: get_gp_field_therMAILx
   integer :: ibdyty,iblmty,ig,ifield
   ! 
   integer :: iM_bdyty, iM_blmty, imodel, nbgp
   real(kind=8), dimension(:,:), allocatable :: field
   !***         
   ! nom de la fonction        123456789012345678901234567
   CHARACTER(len=27) :: IAM = 'mod_therMAILx::get_gp_field' 

   iM_bdyty=bdyty2M_bdyty(ibdyty)
   iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

   imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
   nbgp = get_N_GP_therEF(modelz(imodel)%ID)

   allocate(field(3,nbgp))

   get_gp_field_therMAILx = 0.d0

   call get_ele_pg_fields(bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                          bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                          iM_bdyty,iM_blmty, &
                          ifield,field,3)

   get_gp_field_therMAILx(:) = field(:,ig)

   deallocate(field)
 end function get_gp_field_therMAILx

!
!------------------------------------------------------------------------ 
SUBROUTINE compute_residue_norm_therMAILx(norm_res, norm_T, ibdyty)

  IMPLICIT NONE

  INTEGER      :: ibdyty,ibdy_T,ibdy_Res
  REAL(kind=8) :: max_dT,max_T,norm_T
  REAL(kind=8) :: max_res,max_fint,norm_Res

                            !1234567890123456789012345678901
  CHARACTER(len=31) :: IAM='thermMAILX::compute_residue_norm'
  character(len=80) :: cout

  IF (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
    call faterr(IAM,'Unknown body') 
  endif

  norm_Res=0.d0
  ibdy_Res = 1
  norm_T=0.d0
  ibdy_T = 1

  !!! Etude de la convergence !!!

  !print*,'Corps : ',ibdyty
  !if (itchache) then
  !  print*,' ' 
  !  print*,'residu libre'
  !  print*,bdyty(ibdyty)%RHS
  !  print*,' '
  !  print*,'efforts de contact'
  !  print*,bdyty(ibdyty)%Reac
  !endif

  bdyty(ibdyty)%Taux = bdyty(ibdyty)%T - bdyty(ibdyty)%Tlast  

  max_dT=MAX(MAXVAL(bdyty(ibdyty)%Taux),ABS(MINVAL(bdyty(ibdyty)%Taux)))

  max_T=MAX(MAXVAL(bdyty(ibdyty)%T),ABS(MINVAL(bdyty(ibdyty)%T)))

  IF (max_T <= 1d-10 ) max_T=1.D0

  norm_T=max_dT/max_T

  !if (itchache) then
  !  print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  !  print*,'Norme en vitesse :           '
  !  print*,'max delta V =',max_dV
  !  print*,'max V       =',max_V
  !  print*,'norme en V  =',norm_T
  !  print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  !endif

  bdyty(ibdyty)%Taux = bdyty(ibdyty)%RHS 

  CALL nullify_temp_driven_dof(ibdyty,iTaux_) 

  bdyty(ibdyty)%residu = bdyty(ibdyty)%Taux
     
  max_res=MAX(MAXVAL(bdyty(ibdyty)%residu),ABS(MINVAL(bdyty(ibdyty)%residu)))
  max_fint =H*MAX(MAXVAL(bdyty(ibdyty)%Fint),ABS(MINVAL(bdyty(ibdyty)%Fint)))
  
  ! bof bof

  IF (max_fint <= 1.d-01 ) max_fint=1.D0
  norm_res=max_res/max_fint
 
  !       if (itchache) then
  !       print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  !       print*,'Norme en residu :           '
  !       print*,'max residu    =',max_res
  !       print*,'max force int =',max_fint
  !       print*,'norme en res  =',norm_res
  !       print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  !       endif

  write(cout,'(1X)')
  call logmes(cout)
  write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxRes/MaxFint = ',norm_Res,'body : ',ibdy_Res  
  call logmes(cout)
  write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxDV /MaxV    = ',norm_T  ,'body : ',ibdy_T 
  call logmes(cout)
  write(cout,'(1X)')
  call logmes(cout)


 END SUBROUTINE compute_residue_norm_therMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE add_convection2KT_therMAILx(ibdyty,l_ther_dof,convection_matrix)
 IMPLICIT NONE

   INTEGER                     :: ibdyty,iblmty
   INTEGER, DIMENSION(2)       :: l_ther_dof
   REAL(kind=8),DIMENSION(2,2) :: convection_matrix       

   ! DA : Attention probleme pour add elementary matrix qu'est ce donc : l_ther_dof
   !CALL add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,convection_matrix,H*THETA_t)
   !CALL G_assemb(bdyty(ibdyty)%KT,H*THETA_t*convection_matrix,l_ther_dof)
   !CALL assemb_matrix(bdyty(ibdyty)%g_sys,convection_matrix,l_ther_dof)
 END SUBROUTINE add_convection2KT_therMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE add_convection2RHS_therMAILx(ibdyty,l_ther_dof,convection_matrix,h_vector)
 IMPLICIT NONE

   INTEGER :: ibdyty

   INTEGER, DIMENSION(2)       :: l_ther_dof
   REAL(kind=8)                :: tempconvection
   REAL(kind=8),DIMENSION(2)   :: l_ther_T_loc,convection_rhs,l_ther_T_loc_begin,h_vector,TTheta
   !REAL(kind=8),DIMENSION(2) :: vecteur_base=(/1.,1./)
   REAL(kind=8),DIMENSION(2,2) :: convection_matrix       

   l_ther_T_loc(:)       = bdyty(ibdyty)%T(l_ther_dof(:))
   l_ther_T_loc_begin(:) = bdyty(ibdyty)%Tbegin(l_ther_dof(:))
   TTheta                = THETA_t*l_ther_T_loc(:)+(1.-THETA_t)*l_ther_T_loc_begin(:)
   l_ther_T_loc          = MATMUL(convection_matrix,TTheta)
   !l_ther_T_loc_begin    = MATMUL(convection_matrix,l_ther_T_loc_begin)
   CALL assemble_elementary_vector(bdyty(ibdyty)%RHS, &
                                   -H*(l_ther_T_loc-h_vector), &
                                   l_ther_dof)

END SUBROUTINE add_convection2RHS_therMAILx
!------------------------------------------------------------------------
  LOGICAL FUNCTION get_write_DOF_therMAILx(fantome)    

    IMPLICIT NONE
    INTEGER,optional :: fantome
    get_write_DOF_therMAILx = write_DOF

  END FUNCTION get_write_DOF_therMAILx
!------------------------------------------------------------------------
  LOGICAL FUNCTION get_write_Rnod_therMAILx(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Rnod_therMAILx = write_Rnod

  END FUNCTION get_write_Rnod_therMAILx
!------------------------------------------------------------------------
  LOGICAL FUNCTION CHECK_therMAILx(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome
    INTEGER :: nb_MAILx
    
    nb_MAILx = get_nb_MAILx()
    CHECK_therMAILx = .FALSE.
    
    IF(nb_MAILx.NE.0) CHECK_therMAILx = .TRUE.

  END FUNCTION CHECK_therMAILx

! fonction qui calcule le volume d'un element donne pour un modele de thermique
! donne
function compute_element_volume_therMAILx(ibdyty, iblmty)

   implicit none

   ! variables d'entree:
   integer, intent(in) :: ibdyty ! numero du modele de thermique considere
   integer, intent(in) :: iblmty ! numero de l'element considere
   
   ! valeur de retour:
   real(kind=8) :: compute_element_volume_therMAILx ! volume de l'element 
      ! considere 

   ! variables locales
   integer :: errare ! pour la gestion ds erreurs lors de l'allocation memoire
   real(kind=8), dimension(:,:), allocatable :: coor_ele ! pour stocker les 
      ! coordonnees des sommets des elements
   integer :: iM_bdyty ! pour recuperer l'indice dans MAILx, du corps sur lequel
      ! repose le modele de thermique considere 
   integer :: iM_blmty ! pour recuperer l'indice dans M_bdyty(iM_bdyty)%blmty de
      ! l'element courant (ibdyty)
   !                         1234567890123456789012345678901234
   character(len=34) :: IAM='thermMAILX::compute_element_volume'

   ! s'il n'y a aucun modele de thermique, on ne fait rien...
   if (nb_therMAILx == 0) return

   ! on teste que le modele de thermique considere est bien defini
   if (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
      
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'unknown therMAILx model!')
   
   end if
   
   ! on alloue l'espace memoire pour stocker les coordonnees
   ! des sommets de l'element courant
   if (allocated(coor_ele)) deallocate(coor_ele)
   allocate(coor_ele(nbDIME, size(bdyty(ibdyty)%blmty(iblmty)%NODES)), stat=errare)

   if (errare /= 0) then
     call LOGMES('Error '//IAM// ': allocating coor_ele')
   endif

   ! on les recupere
   coor_ele=get_cooref_ele(ibdyty,iblmty, size(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

   ! on peut alors calculer le volume de l'element 
   compute_element_volume_therMAILx = &
      compute_element_volume(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                             coor_ele)

end function compute_element_volume_therMAILx

! fonction qui calcule, pour un element, une part du volume a affecter
! a chaque noeud
subroutine element_volume_by_node_therMAILx(ibdyty, iblmty, V_nod)

   implicit none

   ! variables d'entree:
   integer, intent(in) :: ibdyty ! numero du modele de thermique considere
   integer, intent(in) :: iblmty ! numero de l'element considere
   
   ! variable de sortie :
   real(kind=8), dimension(:), intent(out) :: V_nod ! volume a affecter a chaque noeud
                                                    ! V_nod(j) contient le volume a affecter
                                                    ! au noeud i de l'element
                                                    ! on a : somme sur j noeud de l'elelement
                                                    ! de V_nod(j) = volume de l'element
   
   ! variables locales :
   integer :: errare ! pour la gestion ds erreurs lors de l'allocation memoire
   real(kind=8), dimension(:,:), allocatable :: coor_ele ! pour stocker les 
      ! coordonnees des sommets des elements
   integer :: iM_bdyty ! pour recuperer l'indice dans MAILx, du corps sur lequel
      ! repose le modele de thermique considere 
   integer :: iM_blmty ! pour recuperer l'indice dans M_bdyty(iM_bdyty)%blmty de
      ! l'element courant (ibdyty)
   !                         1234567890123456789012345678901234
   character(len=34) :: IAM='thermMAILX::compute_element_volume_by_node'

   ! s'il n'y a aucun modele de thermique, on ne fait rien...
   if (nb_therMAILx == 0) return

   ! on teste que le modele de thermique considere est bien defini
   if (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
      
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'unknown therMAILx model!')
   
   end if
   
   ! on alloue l'espace memoire pour stocker les coordonnees
   ! des sommets de l'element courant
   if (allocated(coor_ele)) deallocate(coor_ele)
   allocate(coor_ele(nbDIME, size(bdyty(ibdyty)%blmty(iblmty)%NODES)), stat=errare)

   if (errare /= 0) then
     call LOGMES('Error '//IAM// ': allocating coor_ele')
   endif

   ! on les recupere
   coor_ele=get_cooref_ele(ibdyty,iblmty, size(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

   ! on calcule la part du volume de l'element courant a affecter a chaque noeud
   call element_volume_by_node_therEF(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                      coor_ele, V_nod)

end subroutine element_volume_by_node_therMAILx

! fonction qui calcule le vecteur elementaire contenant le terme source
! correspondant au terme de Biot, dans le cas de la surcharge du modele
! de thermique pour resoudre l'equation d'evolution de la pression dans
! le cadre d'un couplage gaz-grains
subroutine compute_elementary_F_Biot_therMAILx(ibdyty, iblmty, NbNo, u_ele, F_Biot_loc, P0)

   implicit none

   ! variables d'entree:
   integer, intent(in) :: NbNo ! nombre de noeuds de l'element courant
   integer, intent(in) :: ibdyty ! numero du modele de thermique considere
   integer, intent(in) :: iblmty ! indice d'un element portant le modele 
                                 ! de thermique considere
   real(kind=8), dimension(NbNo, nbDIME), intent(in) :: u_ele ! contient les 
                                 ! vitesses barycentriques aux neouds de l'element
                                 ! u(i, :) = vitesse barycentrique du noeud i, 
                                 ! i dans {1, ..., NbNo}
   real(kind=8), intent(in), optional :: P0 ! pression moyenne du gaz (utilisee
      ! seulement dans le cas linearise)  
   
   ! variable de sortie:
   real(kind=8), dimension(NbNo), intent(out) :: F_Biot_loc
      ! vecteur elementaire contenant la contribution du terme de Biot pour
      ! l'element consdiere

   ! variables locales       1234567890123456789012345678901234567890123456
   character(len=46) :: IAM='therMAILx::compute_elementary_F_Biot'
   integer :: inodty ! indice d'un noeud, donne dans la numerotaion locale de l'element, dans
                     ! la numerotation globale
   real(kind=8), dimension(nbDIME, NbNo) :: coor_ele ! pour recuperer les coorodnnees de l'element
   real(kind=8), dimension(NbNo) :: P_ele ! pression aux noeuds, a utililiser
      ! pour le calcul du terme de Biot :
      !   * la pression moyenne (P0) dans le cas linearise
      !   * la pression au debut du pas de temps dans le cas non-lineaire 
   integer :: i ! indice de boucle sur les noeuds de l'element
 
   ! on teste que le modele de thermique considere est bien defini
   if (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
      
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'unknown therMAILx model!')
   
   end if
   
   ! on verifie que le nombre de noeuds de l'element attendu est bon
   if (NbNo .ne. size(bdyty(ibdyty)%blmty(iblmty)%NODES)) then
   
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'inconsistant number of nodes in element!')
   
   end if

   ! on recupere les coordonees de l'element
   coor_ele=get_cooref_ele(ibdyty, iblmty, NbNo) 

   ! on recupere la pression aux noeuds a utliser :

   ! si on est dans le cas linearise
   if (present(P0)) then
      ! la pression a utiliser est la pression moyenne
      P_ele = P0
   ! sinon, 
   else
      ! la pression a utiliser est la pression au debut du pas de temps
   
      ! on recupere la temperature (i.e. la pression), au debut du pas de temps, aux noeuds
      ! sommets de l'element
      do i=1, NbNo
         ! on recupere l'indice du noeud courant dans la numerotation globale
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! on peut alors recuperer la temperature, au debut du pas de temps, au noeud courant
         P_ele(i)=bdyty(ibdyty)%Tbegin(inodty)
      end do
   end if

   ! on calcule le vecteur elementaire pour le noeud courant
   call compute_elementary_F_Biot_therEF(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                         coor_ele, u_ele, P_ele, F_Biot_loc)
   
end subroutine compute_elementary_F_Biot_therMAILx

! fonction qui calcule le vecteur elementaire contenant le terme source
! correspondant a un terme d'advection explicite.
! N.B.: necessaire dans le cas de la surcharge du modele
!       de thermique pour resoudre l'equation d'evolution de la pression dans
!       le cadre d'un couplage gaz-grains (cas general)
subroutine compute_elementary_F_advection_therMAILx(ibdyty, iblmty, NbNo, v_ele, F_advection_loc)

   implicit none

   ! variables d'entree:
   integer, intent(in) :: NbNo ! nombre de noeuds de l'element courant
   integer, intent(in) :: ibdyty ! numero du modele de thermique considere
   integer, intent(in) :: iblmty ! indice d'un element portant le modele 
                                 ! de thermique considere
   real(kind=8), dimension(NbNo, nbDIME), intent(in) :: v_ele ! contient les 
                                 ! vitesses vitesses aux neouds de l'element
                                 ! v(i, :) = vitesse d'advection du noeud i, 
                                 ! i dans {1, ..., NbNo}
   
   ! variable de sortie:
   real(kind=8), dimension(NbNo), intent(out) :: F_advection_loc
      ! vecteur elementaire contenant la contribution du terme d'advection pour
      ! l'element consdiere

   ! variables locales       1234567890123456789012345678901234567890123456
   character(len=46) :: IAM='therMAILx::compute_elementary_F_advection'
   integer :: inodty ! indice d'un noeud, donne dans la numerotaion locale de l'element, dans
                     ! la numerotation globale
   real(kind=8), dimension(nbDIME, NbNo) :: coor_ele ! pour recuperer les coorodnnees de l'element
   real(kind=8), dimension(NbNo) :: T_ele ! temperatures aux noeuds
      ! au debut du pas de temps 
   integer :: i ! indice de boucle sur les noeuds de l'element
 
   ! on teste que le modele de thermique considere est bien defini
   if (ibdyty < 1 .or. ibdyty > nb_therMAILx) then
      
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'unknown therMAILx model!')
   
   end if
   
   ! on verifie que le nombre de noeuds de l'element attendu est bon
   if (NbNo .ne. size(bdyty(ibdyty)%blmty(iblmty)%NODES)) then
   
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'inconsistant number of nodes in element!')
   
   end if

   ! on recupere les coordonees de l'element
   coor_ele=get_cooref_ele(ibdyty, iblmty, NbNo) 

   ! on recupere la temperature, au debut du pas de temps, aux noeuds
   ! sommets de l'element
   do i=1, NbNo
      ! on recupere l'indice du noeud courant dans la numerotation globale
      inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
      ! on peut alors recuperer la temperature, au debut du pas de temps, au noeud courant
      T_ele(i)=bdyty(ibdyty)%Tbegin(inodty)
   end do
 
   ! on calcule le vecteur elementaire pour le noeud courant
   call compute_elementary_F_advection_therEF(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                         coor_ele, v_ele, T_ele, F_advection_loc)
   
end subroutine compute_elementary_F_advection_therMAILx

! fonction qui recupere les gradients de temperature, a la fin du pas de temps,
! pour les noeuds d'un element
subroutine compute_grad_T_ele_therMAILx(ibdyty, iblmty, NbNo, grad_T_ele)

   implicit none
   
   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice du modele de thermique dans therMAILx
   integer, intent(in) :: iblmty ! indice de l'element dans la numerotation des
                                 ! elements portant le modele de thermique ibdyty
   integer, intent(in) :: NbNo ! nombre de noeuds de l'element d'indice iblmty
                     
   ! variable de sortie :
   real(kind=8), dimension(NbNo, nbDIME), intent(out) :: grad_T_ele ! valeur du gradient de temperature
                                                                    ! en chaque noeud de l'element:
                                                                    ! grad_T_ele(i, :) contient le
                                                                    ! gradient de temperature au noeud i
   
   ! variables locales :     12345678901234567890123456789
   character(len=29) :: IAM='therMAILx::compute_grad_T_ele'
   integer :: inodty ! indice d'un noeud, donne dans la numerotaion locale de l'element, dans
                     ! la numerotation globale
   real(kind=8), dimension(nbDIME, NbNo) :: coor_ele ! pour recuperer les coorodnnees de l'element
   real(kind=8), dimension(NbNo) :: T_ele ! pour recuperer la temperature, a la fin du pas de temps,
                                          ! de l'element
   integer :: i ! indice de boucle sur les noeuds de l'element
   
   ! on verifie que le nombre de noeuds de l'element attendu est bon
   if (NbNo .ne. size(bdyty(ibdyty)%blmty(iblmty)%NODES)) then
   
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'inconsistant number of nodes in element!')
   
   end if

   ! on recupere les coordonees de l'element
   coor_ele=get_cooref_ele(ibdyty, iblmty, NbNo) 

   ! on recupere la temperature, a la fin du pas de temps, aux noeuds
   ! sommets de l'element
   do i=1, NbNo

      ! on recupere l'indice du noeud courant dans la numerotation globale
      inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
      
      ! on peut allors recuperer la remperature au noeud courant
      T_ele(i)=bdyty(ibdyty)%T(inodty)
      
   end do

   !print*,'T'
   !print*,T_ele

   ! on calcule le gradient de temperature aux noeuds de l'element courant
   call compute_grad_T_ele_therEF(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                  coor_ele, T_ele, grad_T_ele)

   !print*,'gradT'
   !print*,grad_T_ele
   
end subroutine compute_grad_T_ele_therMAILx

! fonction qui cherche le rang d'un field, dans la liste des fields associes a
! un element, portant un modele de thermique
function get_field_rank_therMAILx(ibdyty, field_name)

   implicit none

   ! variables d'entree :
   integer :: ibdyty ! index du modele de thermique considere
   character(len=*) :: field_name ! nom du fields
   
   ! valeur de retour :
   integer :: get_field_rank_therMAILx ! rang du field de nom field_name

   ! variables locales       1234567890123456789012345
   character(len=25) :: IAM='therMAILx::get_field_rank'
   integer :: field_rank ! pour recuperer le rang du field
   integer :: iM_bdyty ! indice du corps maille sur lequel repose le 
                       ! modele de thermique dans MAILx
   integer :: iM_blmty ! indice de l'element courant dans la numerotation
                       ! des elements du corps iM_bdyty dans MAILx
   integer :: iblmty ! indice de boucle sur les elements

   ! on recupere l'indice du corps maille sur lequel repose le modele de thermique
   ! dans MAILx
   iM_bdyty=bdyty2M_bdyty(ibdyty)
  
   ! on recupere le numero du premier element,
   ! dans la numerotation des elements du corps iM_bdyty dans MAILx
   iM_blmty=bdyty(ibdyty)%blmty2M_blmty(1)             

   ! on recupere le rang du field pour cet element
   field_rank = get_ther_field_rank_MAILx(iM_bdyty, iM_blmty, field_name)

   ! pour chaque autre element
   do iblmty=2, size(bdyty(ibdyty)%blmty)
          
      ! on recupere le numero de l'element courant dans la numeroation des
      ! elements du corps iM_bdyty, dans MAILx
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)          
      
      ! si le rang du field dans l'element courant est different du rang du field
      ! trouve pour les precedents elements
      if (field_rank .ne. &
          get_ther_field_rank_MAILx(iM_bdyty, iM_blmty, field_name)) &
      then
      
         ! on affiche un message d'erreur
         call faterr(IAM, 'field rank depends of the element index!')
      
      end if
   
   end do

   ! si le rang ne depend pas de l'element, on le renvoie
   get_field_rank_therMAILx = field_rank

end function get_field_rank_therMAILx

! fonction qui ajoute une contribution exterieure au flux externe, par 
! element, a la fin du pas de temps, a un element donne
subroutine add_external_flux_ele_therMAILx(ibdyty, iblmty, NbNo, F_ext)

   implicit none
   
   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique
   integer, intent(in) :: iblmty ! indice d'un element portant le modele de thermique
                                 ! considere
   integer, intent(in) :: NbNo ! nombre de noeuds de l'element attendu
   real(kind=8), dimension(NbNo), intent(in) :: F_ext ! vecteur elementaire 
                                                      ! corespondant a un flux
                                                      ! externe, a ajouter au
                                                      ! flux externe de l'element
                                                      ! considere
   
   ! variables locales :     12345678901234567890123456789012
   character(len=32) :: IAM='therMAILx::add_external_flux_ele'

   ! on teste si le vecteur elementaire est defini sur le meme nombre de
   ! noeuds que l'element  
   if (NbNo .ne. size(bdyty(ibdyty)%blmty(iblmty)%NODES)) then
   
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'uncompatible numbers of nodes')
   
   end if

   ! on ajoute la contribution de F_ext aux flux externes de l'element
   bdyty(ibdyty)%blmty(iblmty)%Fext(:, 1) = &
      bdyty(ibdyty)%blmty(iblmty)%Fext(:, 1) + F_ext

end subroutine add_external_flux_ele_therMAILx

! fonction qui ajoute une contribution exterieure au flux externe, par 
! element, a l'instant milieu, a un element donne
subroutine add_theta_external_flux_ele_therMAILx(ibdyty, iblmty, NbNo, ttF_ext)

   implicit none
   
   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique
   integer, intent(in) :: iblmty ! indice d'un element portant le modele de thermique
                                 ! considere
   integer, intent(in) :: NbNo ! nombre de noeuds de l'element attendu
   real(kind=8), dimension(NbNo), intent(in) :: ttF_ext ! vecteur elementaire 
      ! corespondant a un flux externe, a l'instant milieu, a ajouter au flux 
      ! externe de l'element considere
   
   ! variables locales :     12345678901234567890123456789012
   character(len=32) :: IAM='therMAILx::add_external_flux_ele'

   ! on teste si le vecteur elementaire est defini sur le meme nombre de
   ! noeuds que l'element  
   if (NbNo .ne. size(bdyty(ibdyty)%blmty(iblmty)%NODES)) then
   
      ! si ce n'est pas le cas, on affiche un message d'erreur
      call faterr(IAM, 'uncompatible numbers of nodes')
   
   end if

   ! on ajoute la contribution de F_ext aux flux externes, a l'instant milieu,
   ! de l'element
   bdyty(ibdyty)%blmty(iblmty)%ttFext = &
      bdyty(ibdyty)%blmty(iblmty)%ttFext + ttF_ext

end subroutine add_theta_external_flux_ele_therMAILx

!fd remove imposed temp condition 
subroutine remove_driven_temp(ibdyty)
   implicit none
   integer :: ibdyty

   bdyty(ibdyty)%is_Tdriv_active = .false.

end subroutine 

! fonctions permettant de resoudre une equation de thermique stationnaire (type Poisson)
!     [K]{T} = {f}

! * caclul des flux externes imposes (CL de type Neumann)
!> \defgroup PoissonTherMailx Poisson Problem
!> \ingroup SystemTherMailx 
!> \addtogroup PoissonTherMailx
!> \{
subroutine compute_Fext_Poisson_therMAILx

   implicit none
   integer :: ibdyty,iblmty,inodty
   integer :: idof,ifd,inod,iccdof
   real(kind=8) :: Febegin,Fe

   integer :: i,nbdof,errare
   real(kind=8), dimension(:), allocatable :: DV_ele

   !am: recuperation des flux a t[n+1]

   ! s'il n'y a pas de modele de thermique, on ne fait rien...
   if (nb_therMAILx == 0) return

   ! pour chaque modele de thermique
   do ibdyty=1, size(bdyty)
      ! on annule le vecteur des flux externes imposes
      bdyty(ibdyty)%Fext=0.d0

      ! pour chaque noeud avec flux externe impopse
      do ifd=1,bdyty(ibdyty)%nb_flux_driven_dof

         ! on recupere le flux externe impose à la fin du pas de temps
         call comp_a_driven_dof(bdyty(ibdyty)%flux_driven_dof(ifd),Febegin,Fe)

         ! on le stocke dans le flux externe impose pour ce noeud
         bdyty(ibdyty)%Fdriv(ifd)= Fe

         ! on recupere l'index, dans la numerotation globale, du noeud considere
         call owner_of_a_driven_dof(bdyty(ibdyty)%flux_driven_dof(ifd),inod,idof)

         ! on ajoute la contribution de ce neoud au vecteur des flux externes imposes
         bdyty(ibdyty)%Fext(inod)=bdyty(ibdyty)%Fext(inod) + bdyty(ibdyty)%Fdriv(ifd)  

      end do

   end do

end subroutine compute_Fext_Poisson_therMAILx

! * assemblage de la matrice :
subroutine assemb_KT_Poisson_therMAILx

   implicit none

   integer :: ibdyty,iblmty,i
   integer :: idof

   ! s'il n'y a pas de modele de thermique, on ne fait rien...
   if (nb_therMAILx == 0) return

   ! pour chaque modele de thermique
   do ibdyty=1, size(bdyty)

      ! on annulle la matrice du systeme assemble
      !call G_zero(bdyty(ibdyty)%KT)

      ! pour chaque element
      do iblmty=1, size(bdyty(ibdyty)%blmty)

		call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%conductivity)

!fd      print*,'element'
!fd      print*,ibdyty,iblmty
!fd      print*,'capacity'
!fd      write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%capacity
!fd      print*,'conductivity'
!fd      write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%conductivity

         ! on calcule la matrice elementaire [KT_e] :
         ! il ne s'agit que de la matrice de conductivite elementaire
         !bdyty(ibdyty)%blmty(iblmty)%KTloc=bdyty(ibdyty)%blmty(iblmty)%conductivity

!fd      print*,'KTloc'
!fd      write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%KTloc

         ! on ajoute la contibution de la matrice elementaire courante [KT_e] a la matrice globale [KT]
         !call G_assemb(bdyty(ibdyty)%KT,bdyty(ibdyty)%blmty(iblmty)%KTloc, &
         !                 bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
      end do 

   end do
  
end subroutine assemb_KT_Poisson_therMAILx

! * assemblage du second membre :
subroutine assemb_RHS_Poisson_therMAILx

   implicit none

   integer :: ibdyty,iblmty,i,inodty,iccdof,nbdof,errare
   
   ! s'il n'y a pas de modele de thermique, on ne fait rien...
   if (nb_therMAILx == 0) return

   ! pour chaque modele de thermique
   do ibdyty=1, size(bdyty)

      ! on annulle le second membre du systeme assemble 
      bdyty(ibdyty)%RHS=0.D0

      ! pour chaque element
      do iblmty=1, size(bdyty(ibdyty)%blmty)

         ! on calcule le second memebre elementaire {f_e} :
         ! on CHOISIT d'utiliser le vecteur elementaire des flux externes, moyennes sur le pas
         ! de temps (ce qui n'a pas de sens ici), ttFext
         bdyty(ibdyty)%blmty(iblmty)%RHSloc= bdyty(ibdyty)%blmty(iblmty)%ttFext(:) 

!fd         print*,'RHSloc'
!fd         print*,ibdyty,iblmty
!fd
!fd         write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%ttFext
         !
         ! on ajoute la contribution du second memebre elementaire {f_e} au second memebre global {f}
         call assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

      end do

      ! on ajoute la contibution des flux externes imposes (CL de type Neumann)
      bdyty(ibdyty)%RHS = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Fext

   end do

end subroutine assemb_RHS_Poisson_therMAILx

! * calcul du champ de temperature (elimination des CL de type Dirichlet + inversion du systeme) :
!~ subroutine compute_dof_Poisson_therMAILx
!~ 
!~    implicit none  
!~    integer :: ibdyty,ivd,ibdy,inod,idof,iccdof,info
!~ 
!~    ! s'il n'y a pas de modele de thermique, on ne fait rien...
!~    if (nb_therMAILx == 0) return
!~ 
!~    ! pour chaque modele de thermique
!~    do ibdyty=1, size(bdyty)
!~ 
!~       ! on annulle le second membre du systeme, tenant compte des CL
!~       bdyty(ibdyty)%Taux = 0.d0
!~ 
!~       ! elimination des CL de type Dirichlet par substitution :
!~       !   * calcul du second membre du systeme assemble, tenant compte des CL
!~ 
!~       call apply_temp_driven_dof(ibdyty,iTaux_)
!~ 
!~       bdyty(ibdyty)%Taux = -bdyty(ibdyty)%Taux
!~ 
!~       call G_product_vector(bdyty(ibdyty)%KT,bdyty(ibdyty)%Taux)
!~  
!~       bdyty(ibdyty)%Taux = bdyty(ibdyty)%Taux + bdyty(ibdyty)%RHS
!~ 
!~       call apply_temp_driven_dof(ibdyty,iTaux_) 
!~ 
!~       ! ici, Taux contient le second membre du systeme assemble, tenant compte des CL
!~       ! (de type Dirichlet)
!~ 
!~       !   * "elimination des equations associees aux noeuds avec temperature imposee"
!~       !     dans la matrice du systeme assemble 
!~ 
!~       do ivd=1,bdyty(ibdyty)%nb_temp_driven_dof
!~ 
!~          if (.not. bdyty(ibdyty)%is_Tdriv_active(ivd)) cycle
!~ 
!~          call owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)
!~      
!~          call G_apply_drvdof(bdyty(ibdyty)%KT,inod)
!~ 
!~       end do
!~ 
!~       ! resolution du systeme lineaire assemble, et tenant compte des CL de type Dirichlet
!~       call G_solve_linear_system(bdyty(ibdyty)%KT,bdyty(ibdyty)%Taux,info)
!~ 
!~       ! le champ de temperature stationnaire est obtenu 
!~       ! directement par l'inversion du systeme lineaire
!~       bdyty(ibdyty)%T = bdyty(ibdyty)%Taux
!~ 
!~    end do
!~ 
!~ end subroutine compute_dof_Poisson_therMAILx

! * calcul du champ de temperature, dans le cas ou on impose des CL de Neumann
! , flux de pression impose, sur tout le bord : construction d'un probleme
! de moindres carres en ajoutant la contrainte <p> = 0 + resolution 
subroutine compute_dof_Poisson_Neumann_therMAILx

   implicit none

   ! variables locales :
   real(kind=8), dimension(:, :), pointer :: KT_least_square
   real(kind=8), dimension(:), pointer :: RHS_least_square
   integer(kind=4) :: ibdyty

   KT_least_square  => null()
   RHS_least_square => null()

   ! s'il n'y a pas de modele de thermique, on ne fait rien...
   if (nb_therMAILx == 0) return

   ! pour chaque modele de thermique
   do ibdyty=1, size(bdyty)   

      ! on alloue la matrice et le second membre du systeme des moindres carres
      allocate(KT_least_square(size(bdyty(ibdyty)%nodty) + 1, &
                               size(bdyty(ibdyty)%nodty)))
      allocate(RHS_least_square(size(bdyty(ibdyty)%nodty) + 1))

      ! on copie la matrice du systeme assemblee, dans la matrice du systeme des
      ! moindres carres
      !call copy_mat_sym_band_in_mat_full(bdyty(ibdyty)%KT, KT_least_square)
 
      ! on copie le second membre assemble dans le second membre du du systeme
      ! des moindres carres
      RHS_least_square(1:size(bdyty(ibdyty)%nodty)) = bdyty(ibdyty)%RHS

      ! on rajoute la ligne corespondant a la contrainte supplementaire :
      ! <p> = 0
      KT_least_square(size(bdyty(ibdyty)%nodty) + 1, :) = 1.d0
      RHS_least_square(size(bdyty(ibdyty)%nodty) + 1) = 0.d0

      ! on resoud le probleme de moindres carres
      call solve_least_square_problem(KT_least_square, RHS_least_square)

      ! le champ de temperature stationnaire est la solution du systeme
      ! des moindres carres
      bdyty(ibdyty)%T = RHS_least_square(1:size(bdyty(ibdyty)%nodty))

      ! on affiche le residu des moindres carres :
      print*, 'residu : ', abs(RHS_least_square(size(bdyty(ibdyty)%nodty) + 1))

      ! on desalloue la matrice et le second membre du systeme des moindres
      ! carres
      deallocate(KT_least_square)
      nullify(KT_least_square)
      deallocate(RHS_least_square)
      nullify(RHS_least_square)

   end do

end subroutine compute_dof_Poisson_Neumann_therMAILx
!> \}
!------------------------------------------------------------------------
!------------------------------------------------------------------------
integer function get_ndofs_therMAILx(ibdyty,imodel)
  IMPLICIT NONE
  INTEGER :: ibdyty,imodel
  
  get_ndofs_therMAILx = get_N_DOF_by_NODE_therEF(modelz(imodel)%ID)*get_nb_nodes_therMAILx(ibdyty)
end function get_ndofs_therMAILx 

 !------------------------------------------------------------------------
 !DA : Routine d'ajout au second membre d'un terme representant une
 !     source volumique
 !
 
  SUBROUTINE add_source_therMAILx(ibdyty, ifield)
    IMPLICIT NONE

    INTEGER :: errare
    INTEGER :: ibdyty,iblmty,ifield,NbNo,iM_bdyty,iM_blmty
    REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: SINT
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    !                         1234567890123456789012345678901
    CHARACTER(len=31) :: IAM='therMAILx::add_source_therMAILx'

    IF (nb_therMAILx == 0) RETURN

    !
    !print *,'add_source_therMAILx on body : ',ibdyty
    !print *,'source coming from field : '    ,ifield

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

      NbNo = SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
      
      IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
      ALLOCATE(coor_ele(nbDIME,NbNo),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,NbNo)     

      IF (ALLOCATED(SINT)) DEALLOCATE(SINT)
      ALLOCATE(SINT(NbNo),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating SINT')
      ENDIF

      !
      ! on calcule les sources interieures
      ! 
      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
      
      CALL add_elementary_source(bdyty(ibdyty)%blmty(iblmty)%blmnb,ifield,&
                                 coor_ele,iM_bdyty,iM_blmty,SINT)
      !                             
      ! On ajoute cette contribution à la fin du pas
      !
 
      !print*,iblmty,sint

     
      CALL add_external_flux_ele_therMAILx(ibdyty, iblmty, NbNo, SINT)
   
      deallocate(coor_ele,sint)   
                                
    ENDDO

  END SUBROUTINE add_source_therMAILx

  !> add the divergence of a vectorial field to external flux
  SUBROUTINE add_field_divergence_therMAILx(ibdyty, ivfield)
  IMPLICIT NONE

  INTEGER                                 :: errare
  INTEGER                                 :: ibdyty,iblmty,ifield,NbNo,iM_bdyty,iM_blmty,inodty,iM_nodty,ivfield
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: flux_ele
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele,vfield_ele
                                                 !1234567890123456789012345678901
  CHARACTER(len=31)                       :: IAM='therMAILx::add_field_divergence'

  IF (nb_therMAILx == 0) RETURN

  !
    !print *,'add_field_divergence_therMAILx on body : ',ibdyty
    !print *,'field coming from nodal field : '         ,ivfield

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      
      NbNo = SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
      
      IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
      ALLOCATE(coor_ele(nbDIME,NbNo),stat=errare)

      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,NbNo)      
   
      IF (ALLOCATED(flux_ele)) DEALLOCATE(flux_ele)
      ALLOCATE(flux_ele(NbNo),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating flux_ele')
      ENDIF
    
      IF (ALLOCATED(vfield_ele)) DEALLOCATE(vfield_ele)
      ALLOCATE(vfield_ele(nbDIMe,NbNo),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating vfield_ele')
      ENDIF

      vfield_ele = get_vfield_ele(ibdyty,iblmty,NbNo,nbdime,ivfield)

      !
      ! on calcule les sources interieures
      ! 
      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
      
      !print*,iblmty
      !print*,'field '
      !write(6,'(2(1x,D12.5))') vfield_ele

      CALL compute_elementary_field_divergence_therEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,&
                                                         coor_ele, vfield_ele,flux_ele)
      !                             
      ! On ajoute cette contribution à la fin du pas
      !

      !print*,'flux '
      !write(6,'((1x,D12.5))') flux_ele
      
      CALL add_external_flux_ele_therMAILx(ibdyty, iblmty, NbNo, flux_ele)

      deallocate(coor_ele,flux_ele,vfield_ele)
                                   
    ENDDO
  END SUBROUTINE add_field_divergence_therMAILx

!------------------------------------------------------------------------ 
! DA : Utilisation de ppset en thermique
SUBROUTINE push_ppset_therMAILx

   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,ibehav,imodel
   INTEGER :: iM_bdyty,iM_blmty,iM_behav

   INTEGER :: itest 

   CHARACTER(len=103) :: cout
                             !123456789012345678901234567890 
   CHARACTER(len=21)  :: IAM='therMAILx::push_ppset'

   INTEGER :: igp,nb_gp

   IF (nb_therMAILx == 0) RETURN

   DO ibdyty=1,SIZE(bdyty)
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
       ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb

       !fd new 13/08/09
       ! one needs a ppset by gp 
       !fd 16/06/2011 or element (discrete)

       nb_gp=MAX(1,get_N_GP_therEF(modelz(imodel)%ID))
       ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ppsnb(nb_gp))

       !fd new 05/11/09
       ! one can re-use a ppset if already defined
       ! the strategy new/re-use is defined through the flag use_existing_ppset

       DO igp=1,nb_gp
         bdyty(ibdyty)%blmty(iblmty)%ppsnb(igp) = get_ppset_nb(use_existing_ppset,imodel,ibehav)
       ENDDO
     END DO
   END DO  
   
 END SUBROUTINE push_ppset_therMAILx

!!!------------------------------------------------------------------------  
  SUBROUTINE use_new_ppset_therMAILx

    IMPLICIT NONE
    use_existing_ppset = .FALSE.

  END SUBROUTINE use_new_ppset_therMAILx
!!!------------------------------------------------------------------------ 
  SUBROUTINE compute_convection_therMAILx

  IMPLICIT NONE

  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: T_ele, DT_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint
  INTEGER :: ibdyty,iblmty,i,inodty
  INTEGER :: iM_bdyty,iM_blmty
!                           1234567890123456789012345678901234578901
  CHARACTER(len=41) :: IAM='thermMAILX::compute_convection_therMAILx'

  IF (nb_therMAILx == 0) RETURN
  
  DO ibdyty=1,SIZE(bdyty)
    !
    !print *,'Bodies : number : ', ibdyty
    
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating coor_ele')
      ENDIF

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      IF (ALLOCATED(T_ele)) DEALLOCATE(T_ele)
      ALLOCATE(T_ele(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
	  
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating T_ele')
      ENDIF
	  
      IF (ALLOCATED(DT_ele)) DEALLOCATE(DT_ele)
      ALLOCATE(DT_ele(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
	  
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating DT_ele')
      ENDIF
	  
      IF (ALLOCATED(Fint)) DEALLOCATE(Fint)
      ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
	  
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating Fint')
      ENDIF
	  
      Fint = 0.d0

      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
	  ! passage au numero global
	  inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
	  T_ele(i) = THETA_t * bdyty(ibdyty)%T(inodty) + (1.D0-THETA_t) * bdyty(ibdyty)%Tbegin(inodty)
          DT_ele(i) = bdyty(ibdyty)%T(inodty) - bdyty(ibdyty)%Tbegin(inodty)
			  
      END DO
!fd      print*,'coor_ele'
!fd      print*,ibdyty,iblmty
!fd      print*,coor_ele
      !
      ! on calcule les matrices elementaires ici
      ! a revoir plus tard ... 
      ! 
      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      CALL compute_elementary_convection(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                         bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                         H,coor_ele,T_ele,DT_ele,iM_bdyty,iM_blmty, &
                                         Fint, &
                                         bdyty(ibdyty)%blmty(iblmty)%convection,&
                                         bdyty(ibdyty)%blmty(iblmty)%capacity_supg)       
      
      bdyty(ibdyty)%blmty(iblmty)%ttFint(:) = bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + Fint
           
      DEALLOCATE(Fint)
    
    ENDDO

  ENDDO

 END SUBROUTINE compute_convection_therMAILx
!------------------------------------------------------------------------  
 SUBROUTINE set_without_renum_therMAILx
 IMPLICIT NONE

   with_renum=.FALSE.

 END SUBROUTINE set_without_renum_therMAILx
 !------------------------------------------------------------------------  
 SUBROUTINE set_Matrix_Storage_therMAILx(type)
 IMPLICIT NONE
 character(len=8) :: type
 character(len=29) :: IAM

     !12345678901234567890123456789
 IAM='therMAILx::set_matrix_storage'

 Matrix_storage=get_matrix_storage_id_from_name(type)

 if (Matrix_storage == -99) call faterr(IAM,'unknown storage type '//type)

 END SUBROUTINE set_Matrix_Storage_therMAILx
!------------------------------------------------------------------------  
 SUBROUTINE set_Matrix_shape_therMAILx(type)
 IMPLICIT NONE
 character(len=8) :: type
 character(len=27) :: IAM

     !123456789012345678901234567
 IAM='therMAILx::set_matrix_shape'

 Matrix_shape=get_matrix_shape_id_from_name(type)

 if (Matrix_storage == -99) call faterr(IAM,'unknown shape type '//type)

 END SUBROUTINE set_Matrix_shape_therMAILx
!------------------------------------------------------------------------ 

! Routine pour imposer les conditions aux limites par des champs externes
!------------------------------------------------------------------------
 SUBROUTINE set_temp_drvdof_therMAILx(ibdyty,ndof,node, Vdriven)
 
   IMPLICIT NONE
   
   REAL(kind=8),INTENT(in) :: Vdriven
   INTEGER     ,INTENT(in) :: ibdyty, node, ndof
   REAL(kind=8)            :: Tdriven
   INTEGER                 :: ivd,idof,inod,idrvdof,trouve
   
   character(len=26) :: IAM
   !      12345678901234567890123456
   IAM = 'set_vlocy_drvdof_therMAILx'
   
   
   IF (nb_therMAILx == 0) RETURN
	
   trouve = 1
   
   DO ivd=1,bdyty(ibdyty)%nb_temp_driven_dof
	   
       CALL owner_of_a_driven_dof(bdyty(ibdyty)%temp_driven_dof(ivd),inod,idof)
      
       IF ((inod==node) .AND. (idof==ndof)) THEN
		  
          bdyty(ibdyty)%Tdriv(inod) = Tdriven

          CALL apply_temp_driven_dof(ibdyty,iTaux_)
		  
		  trouve = 0
          
       ENDIF

   ENDDO
   
   IF (trouve== 1) CALL faterr(IAM,'driven dof index not found')
   
 END SUBROUTINE set_temp_drvdof_therMAILx

 !------------------------------------------------------------------------
 FUNCTION get_vfield_ele(ibdyty,iblmty,nbNODES,sz,rank)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES,sz,rank
  INTEGER                     :: iM_bdyty,iM_nodty

  REAL(kind=8),DIMENSION(sz,nbNODES) :: get_vfield_ele

  !print*,ibdyty,iblmty,nbNODES,sz,rank

  iM_bdyty=bdyty2M_bdyty(ibdyty)

  DO inodes=1,nbNODES

    !print*,inodes
  
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    !print*,inodty,iM_nodty

    get_vfield_ele(1:sz,inodes)=get_nodal_field_nodty_MAILx(iM_bdyty,iM_nodty,sz,rank)

    !print*,get_vfield_ele(inodes,1:sz)

    !print*,'---'

  ENDDO

END FUNCTION get_vfield_ele
!------------------------------------------------------------------------
! DA : Ajout d'une fonction pour obtenir le gradient de temperature au noeuds du maillage
!------------------------------------------------------------------------
SUBROUTINE get_NodalGrad_therMAILx(ibdyty,GradT)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: GradT
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty

                            !123456789012345678901234
   CHARACTER(len=24) :: IAM='therMAILx::get_NodalGrad'

  ! allocation dans routine appelante (3,nb_nodes)
  ! gard_x,grad_y,grad_z

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  GradT = 0.d0

  nbfields=3

  IF ( nbfields /= SIZE(GradT,dim=1)) THEN
    PRINT *,nbfields,SIZE(GradT,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  DO iblmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)


    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0
    
    CALL gpv2node(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                  bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                  iM_bdyty,iM_blmty,1,temp,NbFields,NbNodes_stored)
      
    DO inodty=1,NbNodes_stored
      GradT(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=GradT(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) &
                                                               + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    GradT(:,inodty)=GradT(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO    

END SUBROUTINE get_NodalGrad_therMAILx
!------------------------------------------------------------------------
! DA : Ajout d'une fonction pour obtenir le flux de chaleur au noeuds du maillage
!------------------------------------------------------------------------
SUBROUTINE get_NodalFlux_therMAILx(ibdyty,GradT)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: GradT
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty

                            !123456789012345678901234
   CHARACTER(len=24) :: IAM='therMAILx::get_NodalFlux'

  ! allocation dans routine appelante (3,nb_nodes)
  ! gard_x,grad_y,grad_z

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  GradT = 0.d0

  nbfields=3

  IF ( nbfields /= SIZE(GradT,dim=1)) THEN
    PRINT *,nbfields,SIZE(GradT,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)


    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0
    
    CALL gpv2node(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                  bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                  iM_bdyty,iM_blmty,2,temp,NbFields,NbNodes_stored)
      
    DO inodty=1,NbNodes_stored

      GradT(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=GradT(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) &
                                                               + temp(:,inodty)

    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

 
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    GradT(:,inodty)=GradT(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO

END SUBROUTINE get_NodalFlux_therMAILx

 function get_coor_therMAILx(ibdyty)
   implicit none 
   integer :: ibdyty
   real(kind=8),dimension(:,:),pointer :: get_coor_therMAILx
   ! ***
   integer :: inode,iM_bdyty,iM_nodty

   get_coor_therMAILx => null()

   iM_bdyty=bdyty2M_bdyty(ibdyty)

   allocate(get_coor_therMAILx(nbdime,bdyty(ibdyty)%nb_nodes)) 

   do inode=1,bdyty(ibdyty)%nb_nodes

     iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inode) 

     get_coor_therMAILx(1:nbdime,inode) = get_coor_nodty_MAILx(iM_bdyty,iM_nodty)

   enddo

 end function

 function get_ll_connectivity_therMAILx(ibdyty)
   implicit none
   integer(kind=4), intent(in)  :: ibdyty
   type(T_link_connec), pointer :: get_ll_connectivity_therMAILx
   !
   integer(kind=4) :: nb_elem, iblmty
   type(T_link_connec), pointer :: last, new

   allocate( get_ll_connectivity_therMAILx )
   allocate( get_ll_connectivity_therMAILx%connec(1) )

   nb_elem = size(bdyty(ibdyty)%blmty)

   get_ll_connectivity_therMAILx%connec(1) = nb_elem

   last => get_ll_connectivity_therMAILx

   !cc_elem_bdyty(ibdyty+1) = cc_elem_bdyty(ibdyty) + nb_elem
   do iblmty = 1, nb_elem
     allocate( new )
     allocate( new%connec(size(bdyty(ibdyty)%blmty(iblmty)%NODES)) )
     new%connec = bdyty(ibdyty)%blmty(iblmty)%NODES
     last%n => new
     last   => new
   end do

 end function

 function get_connectivity_therMAILx(ibdyty)
   implicit none
   integer :: ibdyty
   integer(kind=4),dimension(:),pointer :: get_connectivity_therMAILx
   ! ***
   integer :: sz,iblmty,inode

   get_connectivity_therMAILx => null()

   ! on compte
   sz=1
   do iblmty=1,size(bdyty(ibdyty)%blmty)
     sz = sz + size(bdyty(ibdyty)%blmty(iblmty)%nodes) + 1
   enddo

   ! on alloue
   allocate(get_connectivity_therMAILx(sz)) 

   ! on rempli 
   sz=1
   get_connectivity_therMAILx(sz) = size(bdyty(ibdyty)%blmty)
   do iblmty=1,size(bdyty(ibdyty)%blmty)
     sz = sz + 1
     get_connectivity_therMAILx(sz)=size(bdyty(ibdyty)%blmty(iblmty)%nodes)
     do inode=1,size(bdyty(ibdyty)%blmty(iblmty)%nodes)
       sz = sz + 1
       get_connectivity_therMAILx(sz) = bdyty(ibdyty)%blmty(iblmty)%nodes(inode)
     enddo
   enddo

 end function

 function get_All_therMAILx(ibdyty)
   implicit none 
   integer :: ibdyty
   real(kind=8),dimension(:,:),pointer :: get_All_therMAILx
   REAL(kind=8),DIMENSION(:,:),allocatable :: Flux
   REAL(kind=8),DIMENSION(:,:),allocatable :: Grad
   ! ***
   integer :: inodty,sz,nbn,ns,ne,idx

   get_All_therMAILx => null()

   nbn=bdyty(ibdyty)%nb_nodes
   select case (nbdime) 
   case(2)
     sz = 1 + 3 + 3
     
     allocate(Flux(3,bdyty(ibdyty)%nb_nodes), &
              Grad(3,bdyty(ibdyty)%nb_nodes))
              
     call get_NodalFlux_therMAILx(ibdyty,Flux)
     call get_NodalGrad_therMAILx(ibdyty,Grad)
         
   case(3)
     sz = 1 + 3 + 3
     allocate(Flux(3,bdyty(ibdyty)%nb_nodes), &
              Grad(3,bdyty(ibdyty)%nb_nodes))
              
     call get_NodalFlux_therMAILx(ibdyty,Flux)
     call get_NodalGrad_therMAILx(ibdyty,Grad)
              
   end select

   allocate(get_All_therMAILx(sz,bdyty(ibdyty)%nb_nodes)) 

   do inodty=1,bdyty(ibdyty)%nb_nodes
     idx=0
     get_All_therMAILx(idx+1:idx+1,inodty) = get_T_nodty_therMAILx(ibdyty,inodty)
     idx = idx + 1
     get_All_therMAILx(idx+1:idx+3,inodty) = Grad(1:3,inodty)
     idx = idx + 3 
     get_All_therMAILx(idx+1:idx+3,inodty) = Flux(1:3,inodty)
   enddo
   
   deallocate(Grad,Flux) 

 end function

! ****
!------------------------------------------------------------------------
SUBROUTINE trial_assemb_KT_therMAILx
IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,i
   INTEGER :: idof


   IF (nb_therMAILx == 0) RETURN

   DO ibdyty=1,SIZE(bdyty)


     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

        call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%conductivity)

     ENDDO 

   ENDDO
  
  END SUBROUTINE trial_assemb_KT_therMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
  SUBROUTINE trial_assemb_RHS_therMAILx

   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,i,inodty,iccdof,nbdof,errare

   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: T_ele

   IF (nb_therMAILx == 0) RETURN

   DO ibdyty=1,SIZE(bdyty)

    bdyty(ibdyty)%RHS=0.D0
    bdyty(ibdyty)%Fint=0.D0

    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!
      bdyty(ibdyty)%blmty(iblmty)%RHSloc= - bdyty(ibdyty)%blmty(iblmty)%Fint(:,1)

!                                         H*( (       THETA_t *bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) + &
!                                              (1.d0 -THETA_t)*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)) )


      CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
!
!fd
!fd   PRINT*,'RHSloc'
!fd
!fd   PRINT*,ibdyty,iblmty
!fd

!fd
      WRITE(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%RHSloc
!fd      write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
!fd      write(*,'(4(1x,E20.13))') bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)

    ENDDO

    bdyty(ibdyty)%RHS = bdyty(ibdyty)%RHS + (H*bdyty(ibdyty)%Fext/273.d0)


   ENDDO


END SUBROUTINE trial_assemb_RHS_therMAILx

!------------------------------------------------------------------------ 
  subroutine clean_memory_therMAILx()
    implicit none
    integer(kind=4)   :: i_bdyty, i_blmty, i
    character(len=80) :: cout

    if( allocated(M2therm) ) then
      do i = 1, size(M2therm)
        if( associated(M2therm(i)%nodty) ) deallocate(M2therm(i)%nodty)
        if( associated(M2therm(i)%blmty) ) deallocate(M2therm(i)%blmty)
      end do
      deallocate(M2therm)
    end if

    if( .not. allocated(bdyty) ) return

    nb_therMAILx = 0

    do i_bdyty = 1, size(bdyty)
      if( associated(bdyty(i_bdyty)%blmty) ) then
        do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%conductivity) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%conductivity)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%convection) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%convection)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%capacity) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%capacity)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%capacity_supg) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%capacity_supg)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fext) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fext)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fint) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fint)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ttFint) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ttFint)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc)
          end if
        end do
        deallocate(bdyty(i_bdyty)%blmty)
      end if

      if( associated(bdyty(i_bdyty)%blmty2M_blmty) ) then
        deallocate(bdyty(i_bdyty)%blmty2M_blmty)
      end if

      if( associated(bdyty(i_bdyty)%nodty) ) then
        deallocate(bdyty(i_bdyty)%nodty)
      end if

      if( associated(bdyty(i_bdyty)%nodty2M_nodty) ) then
        deallocate(bdyty(i_bdyty)%nodty2M_nodty)
      end if

      if( associated(bdyty(i_bdyty)%Tlast) ) then
        deallocate(bdyty(i_bdyty)%Tlast)
      end if

      if( associated(bdyty(i_bdyty)%Tbegin) ) then
        deallocate(bdyty(i_bdyty)%Tbegin)
      end if

      if( associated(bdyty(i_bdyty)%T) ) then
        deallocate(bdyty(i_bdyty)%T)
      end if

      if( associated(bdyty(i_bdyty)%Taux) ) then
        deallocate(bdyty(i_bdyty)%Taux)
      end if

      if( associated(bdyty(i_bdyty)%Fext) ) then
        deallocate(bdyty(i_bdyty)%Fext)
      end if

      if( associated(bdyty(i_bdyty)%Fint) ) then
        deallocate(bdyty(i_bdyty)%Fint)
      end if

      if( associated(bdyty(i_bdyty)%residu) ) then
        deallocate(bdyty(i_bdyty)%residu)
      end if

      if( associated(bdyty(i_bdyty)%temp_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%temp_driven_dof)
          if( associated(bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%x)
          end if

          if( associated(bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%fx)
          end if
        end do

        deallocate(bdyty(i_bdyty)%temp_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%Tdriv) ) then
        deallocate(bdyty(i_bdyty)%Tdriv)
      end if
      if( associated(bdyty(i_bdyty)%is_Tdriv_active) ) then
        deallocate(bdyty(i_bdyty)%is_Tdriv_active)
      end if

      if( associated(bdyty(i_bdyty)%drvdofs) ) then
        deallocate(bdyty(i_bdyty)%drvdofs)
      end if

      if( associated(bdyty(i_bdyty)%drvvalues) ) then
        deallocate(bdyty(i_bdyty)%drvvalues)
      end if

      if( associated(bdyty(i_bdyty)%flux_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%flux_driven_dof)
          if( associated(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x)
          end if
          if( associated(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx)
          end if
        end do
        deallocate(bdyty(i_bdyty)%flux_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%Fdriv) ) then
        deallocate(bdyty(i_bdyty)%Fdriv)
      end if

      if( associated(bdyty(i_bdyty)%RHS) ) then
        deallocate(bdyty(i_bdyty)%RHS)
      end if

      if( associated(bdyty(i_bdyty)%ccdof) ) then
        deallocate(bdyty(i_bdyty)%ccdof)
      end if

      if( associated(bdyty(i_bdyty)%nodnb) ) then
        deallocate(bdyty(i_bdyty)%nodnb)
      end if

      if( associated(bdyty(i_bdyty)%dofnb) ) then
        deallocate(bdyty(i_bdyty)%dofnb)
      end if

      call erase_system(bdyty(i_bdyty)%g_sys)

    end do

    deallocate(bdyty)
    
  
    if( allocated(extra_bdyty) ) then
      do i_bdyty = 1, size(extra_bdyty)
        if( associated(extra_bdyty(i_bdyty)%nodty) ) then
          deallocate(extra_bdyty(i_bdyty)%nodty)
        end if

        if( associated(extra_bdyty(i_bdyty)%Tlast) ) then
          deallocate(extra_bdyty(i_bdyty)%Tlast)
        end if

        if( associated(extra_bdyty(i_bdyty)%Tbegin) ) then
          deallocate(extra_bdyty(i_bdyty)%Tbegin)
        end if

        if( associated(extra_bdyty(i_bdyty)%T) ) then
          deallocate(extra_bdyty(i_bdyty)%T)
        end if

        if( associated(extra_bdyty(i_bdyty)%Taux) ) then
          deallocate(extra_bdyty(i_bdyty)%Taux)
        end if

        if( associated(extra_bdyty(i_bdyty)%Fext) ) then
          deallocate(extra_bdyty(i_bdyty)%Fext)
        end if

        if( associated(extra_bdyty(i_bdyty)%Fint) ) then
          deallocate(extra_bdyty(i_bdyty)%Fint)
        end if

        if( associated(extra_bdyty(i_bdyty)%residu) ) then
          deallocate(extra_bdyty(i_bdyty)%residu)
        end if

        if( associated(extra_bdyty(i_bdyty)%temp_driven_dof) ) then
          do i = 1, size(extra_bdyty(i_bdyty)%temp_driven_dof)
            if( associated(extra_bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%x) ) then
              deallocate(extra_bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%x)
            end if

            if( associated(extra_bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%fx) ) then
              deallocate(extra_bdyty(i_bdyty)%temp_driven_dof(i)%time_evolution%fx)
            end if
          end do

          deallocate(extra_bdyty(i_bdyty)%temp_driven_dof)
        end if

        if( associated(extra_bdyty(i_bdyty)%Tdriv) ) then
          deallocate(extra_bdyty(i_bdyty)%Tdriv)
        end if
        if( associated(extra_bdyty(i_bdyty)%is_Tdriv_active) ) then
          deallocate(extra_bdyty(i_bdyty)%is_Tdriv_active)
        end if

        if( associated(extra_bdyty(i_bdyty)%drvdofs) ) then
          deallocate(extra_bdyty(i_bdyty)%drvdofs)
        end if

        if( associated(extra_bdyty(i_bdyty)%drvvalues) ) then
          deallocate(extra_bdyty(i_bdyty)%drvvalues)
        end if

        if( associated(extra_bdyty(i_bdyty)%flux_driven_dof) ) then
          do i = 1, size(extra_bdyty(i_bdyty)%flux_driven_dof)
            if( associated(extra_bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x) ) then
              deallocate(extra_bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x)
            end if
            if( associated(extra_bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx) ) then
              deallocate(extra_bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx)
            end if
          end do
          deallocate(extra_bdyty(i_bdyty)%flux_driven_dof)
        end if

        if( associated(extra_bdyty(i_bdyty)%Fdriv) ) then
          deallocate(extra_bdyty(i_bdyty)%Fdriv)
        end if

        if( associated(extra_bdyty(i_bdyty)%ccdof) ) then
          deallocate(extra_bdyty(i_bdyty)%ccdof)
        end if

        if( associated(extra_bdyty(i_bdyty)%nodnb) ) then
          deallocate(extra_bdyty(i_bdyty)%nodnb)
        end if

        if( associated(extra_bdyty(i_bdyty)%dofnb) ) then
          deallocate(extra_bdyty(i_bdyty)%dofnb)
        end if
      end do
      deallocate(extra_bdyty)
    end if

    if( allocated(bdyty2M_bdyty) ) deallocate(bdyty2M_bdyty)

  end subroutine

 function get_nb_gp_therMAILx(i_bdyty, i_blmty)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> number of gauss points in the element
   integer :: get_nb_gp_therMAILx
   !
   integer :: iM_bdyty, iM_blmty

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   if( associated( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv ) ) then
       get_nb_gp_therMAILx = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1 )
   else
       get_nb_gp_therMAILx = 0
   end if

 end function get_nb_gp_therMAILx

 subroutine get_field_therMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:grad, 2:flux, 3:internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(out) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%grad(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%grad(:)
     end do
   case( 2 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%flux(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%flux(:)
     end do
   case( 3 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%internal(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%internal(:)
     end do
   end select

 end subroutine get_field_therMAILx

 subroutine set_field_therMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:grad, 2:flux, 3:internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(in) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%grad(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,1)%grad(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%grad(:) = field_array(1:field_size,i_gp)
     end do
   case( 2 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%flux(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,1)%flux(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%flux(:) = field_array(1:field_size,i_gp)
     end do
   case( 3 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%internal(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,1)%internal(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%ther_gpv(i_gp,2)%internal(:) = field_array(1:field_size,i_gp)
     end do
   end select

 end subroutine set_field_therMAILx


 subroutine check_properties_therMAILx()
   implicit none

   !                         123456789012345678901234567
   CHARACTER(len=25) :: IAM='therMAILx::check_properties'
   CHARACTER(len=80) :: cout

   integer :: ibdyty,iblmty,iM_bdyty,iM_blmty
   
   if (nb_therMAILx == 0) return
   
   DO ibdyty=1,SIZE(bdyty)
   
     iM_bdyty = bdyty2M_bdyty(ibdyty)
   
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
           
       iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
 
       call check_elementary_ppset(bdyty(ibdyty)%blmty(iblmty)%blmnb,bdyty(ibdyty)%blmty(iblmty)%ppsnb,&
                                   iM_bdyty,iM_blmty)
      
     enddo
   enddo
 end subroutine check_properties_therMAILx

 subroutine get_nb_gp_by_elem_therMAILx(elems, nb_gp, nb_elems)

    implicit none
    character(len=5), dimension(:), pointer :: elems
    integer         , dimension(:), pointer :: nb_gp
    integer :: nb_elems
    !
    integer :: i_elem

    CALL init_therEF

    nb_elems = nb_therEF
    allocate(elems(nb_elems))
    allocate(nb_gp(nb_elems))

    do i_elem = 1, nb_elems
        elems(i_elem) = therEF(i_elem)%NAME
        nb_gp(i_elem) = get_N_GP_therEF( therEF(i_elem)%NAME )
    end do

 end subroutine get_nb_gp_by_elem_therMAILx
 
END MODULE therMAILx

