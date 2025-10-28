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
MODULE wrap_mecaMAILx                                       

  USE ISO_C_BINDING
  USE LMGC90_MPI

  use parameters, only : iIaux_, iIreac

  use overall, only: nstep , &
                     nbDIME, &
                     is_externalFEM
  
  use timer

  USE parameters, ONLY: get_body_vector_id_from_name

  USE mecaMAILx,ONLY:&
       get_nb_mecaMAILx, &
       compute_bulk_mecaMAILx, &
       externalFEM_compute_bulk_mecaMAILx, &
       update_bulk_mecaMAILx, &
       externalFEM_update_bulk_mecaMAILx, &
       compute_free_vlocy_mecaMAILx, &
       externalFEM_compute_free_vlocy_mecaMAILx, &
       assemb_KT_mecaMAILx, &
       assemb_RHS_mecaMAILx, &
!!$       compute_Fint_mecaMAILx, &
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
       get_write_DOF_mecaMAILx, &
       get_write_Rnod_mecaMAILx, &
       set_precon_body_mecaMAILx, &
       compute_precon_W_mecaMAILx, &
       get_nodes_precon_mecaMAILx, &
       set_coro_body_mecaMAILx, &
       set_rigid_body_mecaMAILx, &
       build_rigid_bodies_mecaMAILx, &
       put_precon_W_mecaMAILx, &
       init_precon_W_mecaMAILx, &
       put_vector_mecaMAILx, &
       get_vector_mecaMAILx, &
       get_field_rank, &
       set_field_bynode, &
       set_field_byelem, &
       set_field_byuser, &
       get_vfield_rank, &
       set_vfield_bynode, &
       set_vfield_byelem, &
       set_Matrix_Storage_mecaMAILx, &
       set_Matrix_Shape_mecaMAILx, &
       use_new_ppset_mecaMAILx,&
       set_ortho_frame_byuser, &
       terminate_mecaMAILx, &
       set_without_renum_mecaMAILx, &
       get_2DNodalStress_mecaMAILx, &
       get_2DNodalStrain_mecaMAILx, &
       get_3DNodalStress_mecaMAILx, &
       get_3DNodalStrain_mecaMAILx, &
       get_2DNodalInternalVariables_mecaMAILx, &
       get_3DNodalInternalVariables_mecaMAILx, &
       get_3DElementStress_mecaMAILx, &       
       set_visible_mecaMAILx, &  !PTA
       get_visible_mecaMAILx, &  !PTA
       get_nb_nodes_mecaMAILx, &
       get_nb_elements_mecaMAILx, &
       skip_defo_computation_mecaMAILx, &
       compute_rayleigh_damping_mecaMAILx, &
       compute_rayleigh_damping_discrete_mecaMAILx, &
       !<
       is_rigid_mecaMAILx, &
       get_RcoorTT_mecaMAILx, &
       get_Rcooref_mecaMAILx, &
       get_Rinertia_frame_mecaMAILx, &
       get_coorTT_nodty_mecaMAILx, &
       get_cooref_nodty_mecaMAILx, &
       set_RV_driven_dofs_mecaMAILx, &
       set_RV_driven_dof_value_mecaMAILx, &
       put_Rvector_mecaMAILx, &
       get_Rvector_mecaMAILx, &
       !>
       DISPLAY_bulk_element_mecaMAILx, &
       !!add_spring_to_node_mecaMAILx, &
       apply_drvdof_KT_mecaMAILx, &
       get_matrix_mecaMAILx   , & !rm
       get_drv_vlocy_mecaMAILx, &
       comp_drv_vlocy_mecaMAILx, &
       set_tol_coro_mecaMAILx ,&
       set_vlocy_drvdof_mecaMAILx, &
       compute_configurationTT_mecaMAILx, &
       nullify_reac_mecaMAILx, &
       get_cooref_mecaMAILx, &
       get_connectivity_mecaMAILx, &
       get_materials_mecaMAILx   , &
       get_All_mecaMAILx, &
       get_gp_coor_mecaMAILx, &
       Get_gp_Internals, &
       Get_gp_principalfield, &       
       Get_Elements_Internal, &
       Get_Elements_Internal_Integral, &              
       Get_Elements_Volume, &
       Get_Elements_Center, &       
       Get_Elements_Jacobian, &
       get_ptr_elements_energy, &
       comp_elements_energy, &
       GetPtr_Elements_Visibility, &
       Get_Elements_Neighbor, &
       add_field_divergence_mecaMAILx, &
       clean_memory_mecaMAILx, &
       Compute_Info_PrincipalStressField_mecaMAILx, &
       Compute_PDF_Pressure_mecaMAILx, &       
       get_Deformation_Energy_mecaMAILx, &  
       get_Kinetic_Energy_mecaMAILx, &  
       get_ptr_neighborEle2node_mecaMAILx, &
       get_ptr_neighborEle2ele_mecaMAILx, &
       get_ptr_boundaryElements_mecaMAILx, &
       GetPtr_preconW, & !pta 25/03/2013
       load_precon_W_body_mecaMAILx, & !pta 22/03/2013
       compute_forces_mecaMAILx, &
       get_2DNodalInternal_mecaMAILx, &
       get_nb_internal_mecaMAILx, &
       get_ptr_body_vector_mecaMAILx, &
       get_dofstatus_mecaMAILx, &
       prep_global_mecaMAILx,&
       post_global_mecaMAILx, &
       add_body_force_to_fext_mecaMAILx, &
       check_properties_mecaMAILx, &
       get_nb_gp_mecaMAILx, &
       get_nb_gp_by_elem_mecaMAILx, &
       get_gp_stress_mecaMAILx, &
       get_gp_strain_mecaMAILx, &
       mass_scaling_mecaMAILx, &
       get_gp_all_joint_mecaMAILx, &
       switch_vlocy_driven_dof, &
       update_vlocy_driven_dof_structures                

 use utilities, only: faterr,logmes


 !fd une securite pour verifier que computefield a ete appelee avec updatebulk
 integer :: laststep_computefield=0

 
CONTAINS

!!!---------------------------------------------------

    SUBROUTINE WithoutRenumbering() bind(c, name='mecaMAILx_WithoutRenumbering')
      IMPLICIT NONE

       CALL set_without_renum_mecaMAILx()

    END SUBROUTINE

    SUBROUTINE BandStorage() bind(c, name='mecaMAILx_BandStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_mecaMAILx('band____')

    END SUBROUTINE

    SUBROUTINE SparseStorage() bind(c, name='mecaMAILx_SparseStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_mecaMAILx('sparse__')

    END SUBROUTINE

    SUBROUTINE ExplodedStorage() bind(c, name='mecaMAILx_ExplodedStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_mecaMAILx('exploded')

    END SUBROUTINE


    SUBROUTINE DiagonalStorage() bind(c, name='mecaMAILx_DiagonalStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_mecaMAILx('diagonal')

    END SUBROUTINE

    SUBROUTINE SkylineStorage() bind(c, name='mecaMAILx_SkylineStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_mecaMAILx('skyline_')

    END SUBROUTINE

    SUBROUTINE FullStorage() bind(c, name='mecaMAILx_FullStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_mecaMAILx('full____')

    END SUBROUTINE

    SUBROUTINE SymmetricShape() bind(c, name='mecaMAILx_SymmetricShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_mecaMAILx('sym_____')

    END SUBROUTINE

    SUBROUTINE UnspecifiedShape() bind(c, name='mecaMAILx_UnspecifiedShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_mecaMAILx('std_____')

    END SUBROUTINE

    SUBROUTINE UseNewPPSet() bind(c, name='mecaMAILx_UseNewPPSet')
      IMPLICIT NONE

       CALL use_new_ppset_mecaMAILx()

    END SUBROUTINE

    !am: fonction qui renvoie le nombre de mecaMAILx
    function GetNbMecaMAILx() bind(c, name='mecaMAILx_GetNbMecaMAILx')

       implicit none

       ! valeur de retour
       integer(c_int) :: GetNbMecaMAILx ! nombre mailles pour la mecanique 

       GetNbMecaMAILx = get_nb_mecaMAILx()

    end function GetNbMecaMAILx

    !fd: fonction qui renvoie le nombre de noeuds d'un mecaMAILx
    function GetNbNodes(ibdyty) bind(c, name='mecaMAILx_GetNbNodes')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbNodes ! nombre de noeuds d un mecaMAILx 

       GetNbNodes = get_nb_nodes_mecaMAILx(ibdyty)

    end function GetNbNodes

    !fd: fonction qui renvoie le nombre d'elements d'un mecaMAILx
    function GetNbElements(ibdyty) bind(c, name='mecaMAILx_GetNbElements')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbElements ! nombre de noeuds d un mecaMAILx 

       GetNbElements = get_nb_elements_mecaMAILx(ibdyty)

    end function GetNbElements

    ! number of Gauss Point of an element of a mecaMAILx
    function GetNbGp(ibdyty, iblmty) bind(c, name='mecaMAILx_GetNbGp')
       implicit none
       integer(c_int),value :: ibdyty, iblmty
       ! valeur de retour
       integer(c_int) :: GetNbGp

       GetNbGp = get_nb_gp_mecaMAILx(ibdyty, iblmty)

    end function GetNbGp
    
    SUBROUTINE SetPreconBody(ivalue) bind(c, name='mecaMAILx_SetPreconBody')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: ivalue

       CALL set_precon_body_mecaMAILx(ivalue)

    END SUBROUTINE

    !am: fonction qui applique la precondensation a tous les corps mailles pour la meca
    SUBROUTINE SetPreconAllBodies() bind(c, name='mecaMAILx_SetPreconAllBodies')
       IMPLICIT NONE

       ! variables locales
       integer :: ibdyty ! indice de boucle sur les corps mailles pour la meca
 
       ! on applique la precondensation a chaque corps
       ! fd todo ce numero n'est pas le bon ca doit etre l'indice dans mailx et pas mecamailx
       do ibdyty=1, get_nb_mecaMAILx()
          CALL set_precon_body_mecaMAILx(ibdyty)
       end do

    END SUBROUTINE SetPreconAllBodies

    SUBROUTINE ComputePreconW() bind(c, name='mecaMAILx_ComputePreconW')
      IMPLICIT NONE

       CALL compute_precon_W_mecaMAILx

    END SUBROUTINE

    SUBROUTINE InitPreconW() bind(c, name='mecaMAILx_InitPreconW')
      IMPLICIT NONE

       CALL init_precon_W_mecaMAILx

    END SUBROUTINE

    SUBROUTINE PutPreconW(ivalue1,ivalue2,ivalue3,rvect,ivalue4) bind(c, name='mecaMAILx_PutPreconW')
      IMPLICIT NONE
      integer(c_int),intent(in), value :: ivalue1,ivalue2,ivalue3,ivalue4
      real(c_double),intent(in)        :: rvect(ivalue4)

       CALL put_precon_W_mecaMAILx(ivalue1,ivalue2,ivalue3,rvect,ivalue4)

    END SUBROUTINE

    !> \todo : sort out who make the allocation, python, here, in Core or we do not reallocate and point onto lmgc90 memory
    subroutine GetNodesPrecon(ibdyty, node_list, size_list) bind(c, name='mecaMAILx_GetNodesPrecon')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      type(c_ptr) :: node_list
      integer(c_int), intent(out) :: size_list
      !
      integer(kind=4), dimension(:), pointer :: nodes_precon

      call get_nodes_precon_mecaMAILx(ibdyty, nodes_precon)
      if( associated(nodes_precon) ) then
        size_list = size(nodes_precon)
      else
        size_list = 0
      end if
      node_list = c_loc(nodes_precon(1))
    end subroutine

    SUBROUTINE SetCoroAllBodies() bind(c, name='mecaMAILx_SetCoroAllBodies')
       IMPLICIT NONE

       ! variables locales
       integer :: ibdyty ! indice de boucle sur les corps mailles pour la meca
 
       ! on applique la precondensation a chaque corps
       !fd cet index est bidon ...
       do ibdyty=1, get_nb_mecaMAILx()
          CALL set_coro_body_mecaMAILx(ibdyty)
       end do

    END SUBROUTINE SetCoroAllBodies

    SUBROUTINE SetCoroBody(ibdyty) bind(c, name='mecaMAILx_SetCoroBody')

       implicit none
       integer(c_int),value :: ibdyty
 
       CALL set_coro_body_mecaMAILx(ibdyty)

    END SUBROUTINE SetCoroBody

    SUBROUTINE SetRigidAllBodies() bind(c, name='mecaMAILx_SetRigidAllBodies')
       IMPLICIT NONE

       ! variables locales
       integer :: ibdyty ! indice de boucle sur les corps mailles pour la meca
 
       ! on applique la precondensation a chaque corps
       !fd cet index est bidon ...
       do ibdyty=1, get_nb_mecaMAILx()
          CALL set_rigid_body_mecaMAILx(ibdyty)
       end do

    END SUBROUTINE SetRigidAllBodies

    SUBROUTINE SetRigidBody(ibdyty) bind(c, name='mecaMAILx_SetRigidBody')

       implicit none
       integer(c_int),value :: ibdyty
 
       CALL set_rigid_body_mecaMAILx(ibdyty)

    END SUBROUTINE SetRigidBody

    SUBROUTINE SkipDeformableComputationAllBodies() bind(c, name='mecaMAILx_SkipDeformableComputationAllBodies')
       IMPLICIT NONE

       ! variables locales
       integer :: ibdyty ! indice de boucle sur les corps mailles pour la meca
 
       ! on applique la precondensation a chaque corps
       !fd cet index est bidon ...
       do ibdyty=1, get_nb_mecaMAILx()
          CALL skip_defo_computation_mecaMAILx(ibdyty)
       end do

    END SUBROUTINE  SkipDeformableComputationAllBodies

    SUBROUTINE SkipDeformableComputationBody(ibdyty) bind(c, name='mecaMAILx_SkipDeformableComputationBody')

       implicit none
       integer(c_int),value :: ibdyty
       CALL skip_defo_computation_mecaMAILx(ibdyty)

    END SUBROUTINE  SkipDeformableComputationBody


    SUBROUTINE BuildRigidBodies() bind(c, name='mecaMAILx_BuildRigidBodies')
      IMPLICIT NONE

       CALL build_rigid_bodies_mecaMAILx

    END SUBROUTINE

    subroutine ComputeContactDetectionConfiguration(list_ids, length) bind(c, name='mecaMAILx_ComputeContactDetectionConfiguration')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] compute TT  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_configurationTT_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_configurationTT_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine ComputeContactDetectionConfiguration



    subroutine PutBodyVector(cvalue1_c, ivalue1, mat_in, dim1, dim2) bind(c, name='mecaMAILx_PutBodyVector')
      implicit none
      character(c_char), intent(in), dimension(5) :: cvalue1_c
      integer(c_int)   , intent(in), value        :: ivalue1, dim1, dim2
      type(c_ptr)                  , value        :: mat_in
      !
      real(kind=8), dimension(:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      call c_f_pointer(cptr=mat_in, fptr=vector, shape=(/dim1*dim2/))
      call put_vector_mecaMAILx(cvalue1, ivalue1, vector, dim1*dim2)

    end subroutine

    SUBROUTINE GetBodyVector(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='mecaMAILx_GetBodyVector')
      IMPLICIT NONE
      character(c_char),dimension(5),intent(in) :: cvalue1_c
      integer(c_int), intent(in), value         :: ivalue1
      integer(c_int)                            :: ivalue2, ivalue3
      type(c_ptr)                               :: rvect
      !
      real(kind=8), dimension(:,:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      ivalue2 = nbDIME
      ivalue3 = get_nb_nodes_mecaMAILx(ivalue1)
      allocate(vector(ivalue2,ivalue3))
      CALL get_vector_mecaMAILx(cvalue1,ivalue1,vector,ivalue2*ivalue3)

      rvect = c_loc(vector(1,1))

    END SUBROUTINE

    subroutine GetMaterials(ivalue1,ivect,ivalue2) bind(c, name='mecaMAILx_GetMaterials')
      implicit none
      integer(c_int), intent(in), value         :: ivalue1
      integer(c_int)                            :: ivalue2
      type(c_ptr)                               :: ivect
      !
      integer, dimension(:), pointer :: vector
      integer :: i

      ivalue2 =  get_nb_elements_mecaMAILx(ivalue1)
      allocate( vector( ivalue2 ) )
      call get_materials_mecaMAILx(ivalue1, vector, ivalue2)

      ivect = c_loc(vector(1))

    end subroutine

    SUBROUTINE GetStress(ivalue1,stress,ivalue2,ivalue3) bind(c, name='mecaMAILx_GetStress')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2, ivalue3
      type(c_ptr)                       :: stress
      !
      real(c_double), dimension(:,:), pointer :: S
      integer :: inode,i

      ivalue3 = get_nb_nodes_mecaMAILx(ivalue1)

      if( nbDIME == 2 ) then
        ivalue2 = 5
        allocate(S(ivalue2,ivalue3))
        CALL get_2DNodalStress_mecaMAILx(ivalue1,S)
      else
        ivalue2 = 7
        allocate(S(ivalue2,ivalue3))
        CALL get_3DNodalStress_mecaMAILx(ivalue1,S)
      end if

      stress = c_loc(S(1,1))

    END SUBROUTINE
    
    SUBROUTINE GetStrain(ivalue1,strain,ivalue2,ivalue3) bind(c, name='mecaMAILx_GetStrain')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2,ivalue3
      type(c_ptr)                       :: strain
      !
      real(c_double), dimension(:,:), pointer :: E
      integer :: inode,i

      ivalue3 = get_nb_nodes_mecaMAILx(ivalue1)

      if( nbDIME == 2 ) then
        ivalue2 = 5
        allocate(E(ivalue2,ivalue3))
        CALL get_2DNodalStrain_mecaMAILx(ivalue1,E)
      else
        ivalue2 = 7
        allocate(E(ivalue2,ivalue3))
        CALL get_3DNodalStrain_mecaMAILx(ivalue1,E)
      end if

      strain = c_loc(E(1,1))

    END SUBROUTINE

    SUBROUTINE GetInternalVariables(ivalue1,iv,ivalue2,ivalue3) bind(c, name='mecaMAILx_GetInternalVariables')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2,ivalue3
      type(c_ptr)                       :: iv
      !
      real(c_double), dimension(:,:), pointer :: internal
      integer :: inode,i

      ivalue3 = get_nb_nodes_mecaMAILx(ivalue1)

      if( nbDIME == 2 ) then
        ivalue2 = 10
        allocate(internal(ivalue2,ivalue3))
        CALL get_2DNodalInternalVariables_mecaMAILx(ivalue1,internal)
      else
        ivalue2 = 57
        allocate(internal(ivalue2,ivalue3))
        CALL get_3DNodalInternalVariables_mecaMAILx(ivalue1,internal)
      end if

      iv = c_loc(internal(1,1))

    END SUBROUTINE

    SUBROUTINE GetElementStress(ivalue1,stress,ivalue2,ivalue3) bind(c, name='mecaMAILx_GetElementStress')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2, ivalue3
      type(c_ptr)                       :: stress
      !
      real(c_double), dimension(:,:), pointer :: S

      ivalue3 = get_nb_elements_mecaMAILx(ivalue1)

      if( nbDIME == 2 ) then
        ivalue2 = 5
        allocate(S(ivalue2,ivalue3))
        S=0.
        ! CALL get_2DElementStress_mecaMAILx(ivalue1,S)
      else
        ivalue2 = 7
        allocate(S(ivalue2,ivalue3))
        CALL get_3DElementStress_mecaMAILx(ivalue1,S)
      end if

      stress = c_loc(S(1,1))

    END SUBROUTINE
    
    SUBROUTINE PushProperties() bind(c, name='mecaMAILx_PushProperties')
      IMPLICIT NONE

       CALL push_ppset_mecaMAILx

    END SUBROUTINE

!!! LINEAR PROBLEMS ----------------------------------

    subroutine ComputeFreeVelocity(list_ids, length) bind(c, name='mecaMAILx_ComputeFreeVelocity')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp Free V ')
      call start_itimer(timer_id)

      if( is_externalFEM ) then
         call externalFEM_compute_free_vlocy_mecaMAILx
         call stop_itimer(timer_id)
         return
      endif

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_free_vlocy_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_free_vlocy_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine AssembKT(list_ids, length) bind(c, name='mecaMAILx_AssembKT')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] assemb KT   ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call assemb_KT_mecaMAILx(i)
          call apply_drvdof_KT_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call assemb_KT_mecaMAILx(list(i))
          call apply_drvdof_KT_mecaMAILx(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine OnlyAssembKT(list_ids, length) bind(c, name='mecaMAILx_OnlyAssembKT')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] only ass KT ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call assemb_KT_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call assemb_KT_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ApplyDrvDofKT(list_ids, length) bind(c, name='mecaMAILx_ApplyDrvDofKT')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] apply drv K ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_mecaMAILx()
          call apply_drvdof_KT_mecaMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call apply_drvdof_KT_mecaMAILX(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine AssembRHS(list_ids, length) bind(c, name='mecaMAILx_AssembRHS')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] assemb RHS  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call assemb_RHS_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call assemb_RHS_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    function ComputeResidueNorm(list_ids, length) bind(c, name='mecaMAILx_ComputeResidueNorm')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      real(c_double) :: ComputeResidueNorm
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list
      !
      integer(c_int)  :: ibdy_V, ibdy_Res
      real(c_double)  :: norm_res,norm_V,tmp
      !
      character(len=70) :: cout

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] compResNorm ')
      call start_itimer(timer_id)

      ComputeResidueNorm = 0.d0
      tmp                = 0.d0
      ibdy_V             = 0
      ibdy_Res           = 0

      if( length == 0 ) then  

        do i = 1, get_nb_mecaMAILx()
          call compute_residue_norm_mecaMAILx(norm_res,norm_V,i)
          if( norm_V > tmp ) then
            ibdy_V = i
            tmp    = norm_V
          end if
          if( norm_res > ComputeResidueNorm ) then
            ibdy_Res = i
            ComputeResidueNorm = norm_res
          end if
        end do

      else

        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_residue_norm_mecaMAILx(norm_res,tmp,list(i))
          if( norm_V > tmp ) then
            tmp    = norm_V
            ibdy_V = list(i)
          end if
          if( norm_res > ComputeResidueNorm ) then
            ibdy_Res=list(i)
            ComputeResidueNorm = norm_res
          end if
        end do
      end if

      call logmes(' ')   
      write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxRes/MaxFint = ',ComputeResidueNorm,'body : ',ibdy_Res  
      call logmes(cout)
      write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxDV /MaxV    = ',tmp ,'body : ',ibdy_V 
      call logmes(cout)

      call stop_itimer(timer_id)

    end function

    subroutine ComputeBulk(list_ids, length) bind(c, name='mecaMAILx_ComputeBulk')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp bulk   ')
      call start_itimer(timer_id)

      if( is_externalFEM ) then
        call externalFEM_compute_bulk_mecaMAILx
        call stop_itimer(timer_id)
        return
      end if

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_bulk_mecaMAILx(i,0)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_bulk_mecaMAILX(list(i),0)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeField(list_ids, length) bind(c, name='mecaMAILx_ComputeField')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      laststep_computefield=nstep

      if ( get_nb_mecaMAILx() < 1 .or. is_externalFEM ) return
      
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp field  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_bulk_mecaMAILx(i,1)
          call compute_forces_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_bulk_mecaMAILX(list(i),1)
          call compute_forces_mecaMAILx(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeFint(list_ids, length) bind(c, name='mecaMAILx_ComputeFint')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp fint   ')
      call start_itimer(timer_id)

      if( is_externalFEM ) then
        call externalFEM_compute_bulk_mecaMAILx
        call stop_itimer(timer_id)
        return
      end if

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_bulk_mecaMAILx(i,2)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
        
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_bulk_mecaMAILX(list(i),2)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine
    
    subroutine UpdateBulk(list_ids, length) bind(c, name='mecaMAILx_UpdateBulk')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list
      !
      character(len=26) :: IAM 
    
      if( get_nb_mecaMAILx() < 1 ) return

          !12345678901234567890123456  
      IAM='wrap_mecaMAILx::UpdateBulk'
      
      if (laststep_computefield /= nstep) then
        call faterr(IAM,'You need to call ComputeField before updating')
      endif   
      
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] update bulk ')
      call start_itimer(timer_id)

      if( is_externalFEM ) then
        call externalFEM_update_bulk_mecaMAILx
        call stop_itimer(timer_id)
        return
      end if

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          !fd fait par ComputeField call compute_bulk_mecaMAILx(i,1)
          call update_bulk_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          !fd fait par ComputeField call compute_bulk_mecaMAILx(list(i),1)
          call update_bulk_mecaMAILx(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine UpdateDof(list_ids, length) bind(c, name='mecaMAILx_UpdateDof')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] update dof  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call update_dof_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call update_dof_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if
      call MPI_BARRIER(lmgc_mpi_world_comm, code_MPI)
      call stop_itimer(timer_id)

    end subroutine
    !
    subroutine ComputeDof(list_ids, length) bind(c, name='mecaMAILx_ComputeDof')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp dof    ')
      call start_itimer(timer_id)

      if( is_externalFEM ) then
        call externalFEM_compute_dof_mecaMAILx
        call stop_itimer(timer_id)
        return
      end if

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_dof_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_dof_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine
    !
    SUBROUTINE IncrementStep() bind(c, name='mecaMAILx_IncrementStep')
      IMPLICIT NONE
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method

       CALL increment_mecaMAILx 

    END SUBROUTINE
    !
    subroutine ComputeFext(list_ids, length) bind(c, name='mecaMAILx_ComputeFext')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp Fext   ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_Fext_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_Fext_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine
    !
    subroutine ComputeMass(list_ids, length) bind(c, name='mecaMAILx_ComputeMass')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp mass   ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call compute_mass_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call compute_mass_mecaMAILX(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      call stop_itimer(timer_id)

    end subroutine
    !
    subroutine FatalDamping(list_ids, length) bind(c, name='mecaMAILx_FatalDamping')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

      if( length == 0 ) then  
        do i = 1, get_nb_mecaMAILx()
          call fatal_damping_mecaMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call fatal_damping_mecaMAILX(list(i))
        end do
      end if

    end subroutine
    
    function CheckEquilibriumState() bind(c, name='mecaMAILx_CheckEquilibriumState')
      IMPLICIT NONE
      logical(c_bool) :: CheckEquilibriumState
      LOGICAL         :: check_convergence
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0

       check_convergence = .FALSE.
       CALL check_equilibrium_state_mecaMAILx(check_convergence)
       CheckEquilibriumState = check_convergence

    END function

    SUBROUTINE SetEquilibriumNorm(checktype_c,tol) bind(c, name='mecaMAILx_SetEquilibriumNorm')
      implicit none
      real(c_double), intent(in), value :: tol
      character(c_char), dimension(5)   :: checktype_c
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0
       !!  You must precise the type of check test
       !!   - Qvlcy : quadratic norm of velocy
       !!   - Maxm  : maximum   norm of velocy

       character(len=5) :: checktype
       integer :: i

       checktype = ''
       do i=1,5
         checktype = checktype(1:i-1) // checktype_c(i)
       end do

       IF ( checktype .NE. 'Qvlcy' .AND. checktype .NE. 'Mvlcy') THEN
          print*,' @ WARNING, NEW from 03.12.16.'
          print*,' @ You must precise the type of check test:'
          print*,' @  - Qvlcy : quadratic norm of velocy,'
          print*,' @  - Maxm  : maximum   norm of velocy.'
          STOP
       END IF
       CALL set_data_equilibrium_mecaMAILx(checktype,tol)

    END SUBROUTINE

!!! READING DATA ----------------------------------------------------

    SUBROUTINE ReadDrivenDof() bind(c, name='mecaMAILx_ReadDrivenDof')
      IMPLICIT NONE
       !! PURPOSE
       !!  read DRV_DOF.DAT file

       CALL read_in_driven_dof_mecaMAILx

    END SUBROUTINE
    
    subroutine ReadIniGPV(step) bind(c, name='mecaMAILx_ReadIniGPV')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_gpv_mecaMAILx(step)

    end subroutine

    subroutine ReadIniDof(step) bind(c, name='mecaMAILx_ReadIniDof')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_dof_mecaMAILx(step)

    end subroutine

    SUBROUTINE LoadBehaviours() bind(c, name='mecaMAILx_LoadBehaviours')
      IMPLICIT NONE

       CALL load_behaviours_mecaMAILx 

    END SUBROUTINE

    SUBROUTINE LoadModels() bind(c, name='mecaMAILx_LoadModels')
      IMPLICIT NONE

       CALL load_models_mecaMAILx
       CALL update_existing_entities_mecaMAILx
       
    END SUBROUTINE

!!! WRITTING DATA --------------------------------------------------

    SUBROUTINE WriteDrivenDof() bind(c, name='mecaMAILx_WriteDrivenDof')
      IMPLICIT NONE
       !! PURPOSE
       !!  write DRV_DOF.OUT file

       CALL write_out_driven_dof_mecaMAILx

    END SUBROUTINE

    SUBROUTINE WriteLastDof() bind(c, name='mecaMAILx_WriteLastDof')
       IMPLICIT NONE
       INTEGER :: ifrom,ito
       !! PURPOSE
       !!  write ascii DOF.LAST file

          ifrom = 1  
          ito   = get_nb_mecaMAILx()
          CALL write_xxx_dof_mecaMAILx(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutDof() bind(c, name='mecaMAILx_WriteOutDof')
       IMPLICIT NONE
       LOGICAL :: write_DOF
       INTEGER :: ifrom,ito
       !! PURPOSE
       !!  write ascii DOF.OUT file. Can be activate only each N step

       write_DOF = get_write_DOF_mecaMAILx()

       IF (write_DOF) THEN
         ifrom = 1  
         ito   = get_nb_mecaMAILx()
         CALL write_xxx_dof_mecaMAILx(1,ifrom,ito)
       ENDIF

    END SUBROUTINE

    SUBROUTINE DisplayOutDof() bind(c, name='mecaMAILx_DisplayOutDof')
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !! PURPOSE
       !!  display body degrees of freedom

          ifrom = 1  
          ito   = get_nb_mecaMAILx()
          CALL write_xxx_dof_mecaMAILx(6,ifrom,ito)

    END SUBROUTINE

    subroutine WriteLastRnod(list_ids, length) bind(c, name='mecaMAILx_WriteLastRnod')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( length == 0 ) then  
        list => null()
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      end if

      call write_xxx_Rnod_mecaMAILx(2,list,length)

    end subroutine

    subroutine WriteOutRnod(list_ids, length) bind(c, name='mecaMAILx_WriteOutRnod')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      logical         :: write_Rnod
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] wr out rnod ')
      call start_itimer(timer_id)

      write_Rnod = get_write_Rnod_mecaMAILx()

      if( write_Rnod ) then

        if( length == 0 ) then  
          list => null()
        else
          call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        end if

        call write_xxx_Rnod_mecaMAILx(1,list,length)

      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine DisplayOutRnod(list_ids, length) bind(c, name='mecaMAILx_DisplayOutRnod')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( length == 0 ) then  
        list => null()
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      end if

      call write_xxx_Rnod_mecaMAILx(6,list,length)

    end subroutine

    
    SUBROUTINE DisplayBulkElement(IdBody,IdElem) bind(c, name='mecaMAILx_DisplayBulkElement')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,IdElem
       !! PURPOSE
       !!  display bulk element

       CALL DISPLAY_bulk_element_mecaMAILx(IdBody,IdElem)

    END SUBROUTINE

    subroutine WriteLastNodalForces(list_ids, length) bind(c, name='mecaMAILx_WriteLastNodalForces')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( length == 0 ) then  
        list => null()
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      end if

      call write_xxx_nodforces(2,list,length)

    end subroutine

    subroutine WriteOutNodalForces(list_ids, length) bind(c, name='mecaMAILx_WriteOutNodalForces')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      logical         :: write_Rnod
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] wr Nodal F  ')
      call start_itimer(timer_id)

      write_Rnod = get_write_Rnod_mecaMAILx()

      if( write_Rnod ) then

        if( length == 0 ) then  
          list => null()
        else
          call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        end if

        call write_xxx_nodforces(1,list,length)

      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine DisplayOutNodalForces(list_ids, length) bind(c, name='mecaMAILx_DisplayOutNodalForces')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( length == 0 ) then  
        list => null()
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      end if

      call write_xxx_nodforces(6,list,length)

    end subroutine

    function getScalarFieldRank(ibdyty, iblmty, name) bind(c, name='mecaMAILx_GetScalarFieldRank')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: iblmty
      character(c_char), dimension(*)   :: name
      integer(c_int) :: getScalarFieldRank
      !
      integer(kind=4)   :: i
      character(len=30) :: f_name

      f_name = ''
      i = 1
      do while( name(i) /= c_null_char .and. i <= 30 )
        f_name = f_name(1:i-1) // name(i)
        i = i+1
      end do

      getScalarFieldRank = get_field_rank(ibdyty, iblmty, f_name)

    end function

    subroutine SetScalarFieldByNode(ibdyty,f_rank,f,f_size) bind(c, name='mecaMAILx_SetScalarFieldByNode')
      implicit none
      integer(c_int), intent(in), value :: ibdyty, f_rank, f_size
      real(c_double), intent(in), dimension(f_size) :: f

      call set_field_bynode(ibdyty,f_rank,f_size,f)

    end subroutine

    subroutine SetScalarFieldByElem(ibdyty,f_rank,f,f_size) bind(c, name='mecaMAILx_SetScalarFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty, f_rank, f_size
      real(c_double), intent(in), dimension(f_size) :: f

      call set_field_byelem(ibdyty,f_rank,f_size,f)

    end subroutine

    function getVectorFieldRank(ibdyty, iblmty, name) bind(c, name='mecaMAILx_GetVectorFieldRank')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: iblmty
      character(c_char), dimension(*)   :: name
      integer(c_int) :: getVectorFieldRank
      !
      integer(kind=4)   :: i
      character(len=30) :: f_name

      f_name = ''
      i = 1
      do while( name(i) /= c_null_char .and. i <= 30 )
        f_name = f_name(1:i-1) // name(i)
        i = i+1
      end do

      getVectorFieldRank = get_vfield_rank(ibdyty, iblmty, f_name)

    end function

    subroutine SetVectorFieldByNode(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='mecaMAILx_SetVectorFieldByNode')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_vfield_bynode(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    subroutine SetVectorFieldByElem(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='mecaMAILx_SetVectorFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_vfield_byelem(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    SUBROUTINE Terminate() bind(c, name='mecaMAILx_Terminate')
      IMPLICIT NONE
       !! PURPOSE
       !!  Stop job properly
     
       CALL terminate_mecaMAILx()

    END SUBROUTINE

    subroutine ComputeOrthoFrame(list_ids, length) bind(c, name='mecaMAILx_ComputeOrthoFrame')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp orthoF ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_mecaMAILx()
          call set_ortho_frame_byuser(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call set_ortho_frame_byuser(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeUserField(ifield, list_ids, length) bind(c, name='mecaMAILx_ComputeUserField')
      implicit none
      integer(c_int), intent(in), value :: ifield
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( length == 0 ) then  
        do i = 1, get_nb_mecaMAILx()
          call set_field_byuser(i,ifield)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call set_field_byuser(list(i),ifield)
        end do
      end if

    end subroutine

!!!PTA
    Subroutine SetVisible(ibdyty) bind(c, name='mecaMAILx_SetVisible')
       implicit none
       integer(c_int), intent(in), value :: ibdyty

       CALL set_visible_mecaMAILx(ibdyty,.TRUE.)

    END Subroutine
!!!PTA
    Subroutine SetInvisible(ibdyty) bind(c, name='mecaMAILx_SetInvisible')
       implicit none
       integer(c_int), intent(in), value :: ibdyty

       CALL set_visible_mecaMAILx(ibdyty,.FALSE.)

    END Subroutine
!!!PTA
    function IsVisible(ibdyty) bind(c, name='mecaMAILx_IsVisible')
       implicit none
       integer(c_int), intent(in), value :: ibdyty
       integer(c_int) :: IsVisible

       if(get_visible_mecaMAILx(ibdyty)) then
         IsVisible = 1
       else
         IsVisible = 0
       end if

    end function

    subroutine ComputeRayleighDamping(alpha, beta, list_ids, length) bind(c, name='mecaMAILx_ComputeRayleighDamping')
      implicit none
      real(c_double), intent(in), value :: alpha,beta
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp Rayl D ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_mecaMAILx()
          call compute_rayleigh_damping_mecaMAILx(i,alpha,beta)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_rayleigh_damping_mecaMAILx(list(i),alpha,beta)
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeRayleighDampingDiscreteElement(damp, list_ids, length) &
               bind(c, name='mecaMAILx_ComputeRayleighDampingDiscreteElement')
      implicit none
      real(c_double), intent(in), value :: damp
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp RayDDE ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_mecaMAILx()
          call compute_rayleigh_damping_discrete_mecaMAILx(i,damp)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_rayleigh_damping_discrete_mecaMAILx(list(i),damp)
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine


    !< rigid, coro
    
    function IsRigid(ibdyty) bind(c, name='mecaMAILx_IsRigid')
       implicit none
       integer(c_int), intent(in), value :: ibdyty
       integer(c_int) :: IsRigid

       if(is_rigid_mecaMAILx(ibdyty)) then
         IsRigid = 1
       else
         IsRigid = 0
       end if

    end function IsRigid
     
    subroutine SetTolCoro(tol) bind(c, name='mecaMAILx_SetTolCoro')
    implicit none
    real(c_double), intent(in), value :: tol

      call set_tol_coro_mecaMAILx(tol)
    end subroutine
    
    subroutine GetRigidFrame(cvalue_c, ibdyty,rvect,ivalue1,ivalue2) bind(c, name='mecaMAILx_GetRigidFrame')
      implicit none
      character(c_char), dimension(5), intent(in) :: cvalue_c
      integer(c_int)   , intent(in)  , value      :: ibdyty
      integer(c_int)                              :: ivalue1, ivalue2
      type(c_ptr)                                 :: rvect
      !
      character(len=5) :: cvalue
      integer(kind=4)  :: i
      real(kind=8), dimension(:,:), pointer :: frame

      cvalue = ''
      do i=1,5
        cvalue = cvalue(1:i-1) // cvalue_c(i)
      end do

      ivalue1 = nbDIME
      ivalue2 = nbDIME
      allocate(frame(nbDIME,nbDIME))

      frame = get_Rinertia_frame_mecaMAILx(cvalue, ibdyty)

      rvect = c_loc(frame(1,1))

    end subroutine

    SUBROUTINE GetRigidCoorTT(ibdyty,rvect,ivalue2) bind(c, name='mecaMAILx_GetRigidCoorTT')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int)                    :: ivalue2
      type(c_ptr)                       :: rvect
      !
      real(kind=8), dimension(:), pointer :: coorTT

      ivalue2 = nbDIME
      allocate(coorTT(nbDIME))
      coorTT = get_RcoorTT_mecaMAILx(ibdyty)

      ivalue2 = nbDIME
      rvect = c_loc(coorTT(1))

    END SUBROUTINE

    SUBROUTINE GetNodeCoorTT(ibdyty,inodty,rvect,ivalue2) bind(c, name='mecaMAILx_GetNodeCoorTT')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ibdyty,inodty
      integer(c_int)                    :: ivalue2
      type(c_ptr)                       :: rvect
      !
      real(c_double), dimension(:), pointer :: vect

      ivalue2 = nbDIME
      allocate(vect(nbDIME))
      vect =  get_coorTT_nodty_mecaMAILx(ibdyty,inodty)
      rvect = c_loc(vect(1))

    END SUBROUTINE

    SUBROUTINE GetRigidCooref(ibdyty,rvect,ivalue2) bind(c, name='mecaMAILx_GetRigidCooref')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int)                    :: ivalue2
      type(c_ptr)                       :: rvect
      !
      real(kind=8), dimension(:), pointer :: Rcooref


      ivalue2 = nbDIME
      allocate(Rcooref(nbDIME))
      Rcooref = get_Rcooref_mecaMAILx(ibdyty)

      rvect = c_loc(Rcooref(1))

    END SUBROUTINE

    SUBROUTINE GetNodeCooref(ibdyty,inodty,rvect,ivalue2) bind(c, name='mecaMAILx_GetNodeCooref')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ibdyty,inodty
      integer(c_int)                    :: ivalue2
      type(c_ptr)                       :: rvect
      !
      real(c_double), dimension(:), pointer :: vect 

      ivalue2 = nbDIME
      allocate(vect(nbDIME))
      vect =  get_cooref_nodty_mecaMAILx(ibdyty,inodty)
      rvect = c_loc(vect(1))

    END SUBROUTINE

    SUBROUTINE SetRVDrivenDofs(ibdyty,ivec,dimvec) bind(c, name='mecaMAILx_SetRVDrivenDofs')
      IMPLICIT NONE
      integer(c_int),intent(in), value :: ibdyty,dimvec
      integer(c_int),intent(in)        :: ivec(dimvec)

       CALL Set_RV_driven_dofs_mecaMAILx(ibdyty,ivec,dimvec)

    END SUBROUTINE

    SUBROUTINE SetRVDrivenDofValue(ibdyty,idof,rv) bind(c, name='mecaMAILx_SetRVDrivenDofValue')
      IMPLICIT NONE
      integer(c_int),intent(in), value :: ibdyty,idof
      real(c_double),intent(in),value :: rv


      CALL Set_RV_driven_dof_value_mecaMAILx(ibdyty,idof,rv)

    END SUBROUTINE SetRVDrivenDofValue

    SUBROUTINE PutBodyRVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='mecaMAILx_PutBodyRVector')
      IMPLICIT NONE
      CHARACTER(c_char), DIMENSION(5),INTENT(in) :: cvalue1_c
      INTEGER(c_int),INTENT(in), value           :: ivalue1,ivalue2
      REAL(c_double),INTENT(in) :: rvect(ivalue2)
      !
      CHARACTER(len=5) :: cvalue1
      INTEGER :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      CALL put_Rvector_mecaMAILx(cvalue1,ivalue1,rvect,ivalue2)

    END SUBROUTINE

    SUBROUTINE GetBodyRVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='mecaMAILx_GetBodyRVector')
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int),intent(in), value :: ivalue1
      integer(c_int)                   :: ivalue2
      type(c_ptr)                      :: rvect
      !
      real(c_double), dimension(:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      if( cvalue1(:1) == 'C' ) then
        ivalue2 = nbDIME
      else if (nbdime == 2) then
        ivalue2 = 3
      else
        ivalue2 = 6
      end if

      allocate(vector(ivalue2))

      CALL get_Rvector_mecaMAILx(cvalue1,ivalue1,vector,ivalue2)

      rvect = c_loc(vector(1))

    END SUBROUTINE
    

    ! rigid, coro > 

    subroutine GetBodyMatrix(cvalue_c, ibdyty, rmat, size1, size2) bind(c, name='mecaMAILx_GetBodyMatrix')
      implicit none
      character(c_char), dimension(5), intent(in) :: cvalue_c
      integer(c_int)   , intent(in)  , value      :: ibdyty
      integer(c_int) :: size1,size2 
      type(c_ptr)    :: rmat
      !
      character(len=5) :: cvalue
      integer(kind=4)  :: i
      real(kind=8), dimension(:,:), pointer :: rmat_target

      cvalue = ''
      do i=1,5
        cvalue = cvalue(1:i-1) // cvalue_c(i)
      end do

      rmat_target => null()

      call get_matrix_mecaMAILx(cvalue,ibdyty,rmat_target,size1,size2)

      if( associated(rmat_target) ) then
        size1 = size(rmat_target,1)
        size2 = size(rmat_target,2)
        rmat = c_loc(rmat_target(1,1))
      else
        size1 = 0
        size2 = 0
        rmat = c_null_ptr
      end if

    end subroutine

    subroutine getDrvVlocy(ibdyty, i4_vector, i4_size, r8_vector, r8_size) bind(c, name='mecaMAILx_getDrvVlocy')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(out) :: i4_size, r8_size
      type(c_ptr) :: i4_vector, r8_vector
      !
      integer(kind=4), dimension(:), pointer :: i4_target
      real(kind=8)   , dimension(:), pointer :: r8_target

      i4_target => null()
      r8_target => null()

      call get_drv_vlocy_mecaMAILx(ibdyty, i4_target, r8_target)

      if( associated(i4_target) ) then
        i4_size = size(i4_target)
        r8_size = size(r8_target)

        i4_vector = c_loc(i4_target(1))
        r8_vector = c_loc(r8_target(1))
      else
        i4_size = 0
        r8_size = 0

        i4_vector = c_null_ptr
        r8_vector = c_null_ptr
      end if
      
    end subroutine

    subroutine compDrvVlocy(ibdyty, vector_in, length) bind(c, name='mecaMAILx_computeDrvVlocy')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: length
      type(c_ptr), value :: vector_in
      !
      real(kind=8), dimension(:), pointer :: values

      call c_f_pointer(cptr=vector_in, fptr=values, shape=(/length/))
      call comp_drv_vlocy_mecaMAILx(ibdyty, values)
    end subroutine

    SUBROUTINE SetVlocyDrivenDof(IdBody,f_dof,f_node,f_value) bind(c, name='mecaMAILx_SetVlocyDrivenDof')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,f_dof
      INTEGER(C_INT), INTENT(IN), VALUE :: f_node
      REAL(C_DOUBLE), INTENT(IN), VALUE :: f_value

       !! PURPOSE
       !!  Apply a drvdof on a given body
       
       CALL set_vlocy_drvdof_mecaMAILx(IdBody,f_dof,f_node,f_value)

    END SUBROUTINE

    subroutine NullifyReac(cvalue1_c,idbody) bind(c, name='mecaMAILx_NullifyReac')
      IMPLICIT NONE
      character(c_char),intent(in), dimension(5) :: cvalue1_c
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody

      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      select case(cvalue1)
      case('Ireac')
        call nullify_reac_mecaMAILx(idbody,iIreac)
      case('Iaux_')
        call nullify_reac_mecaMAILx(idbody,iIaux_)
      case default
        call logmes('moi pas comprendre')
      end select
    end subroutine

    subroutine GetCoorefMecaMAILx(idBody, ptr, dim1, dim2) BIND(c, name='mecaMAILx_GetCooref')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_cooref_mecaMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetCoorefMecaMAILx

    subroutine GetAllMecaMAILx(idBody,ptr, dim1, dim2) BIND(c, name='mecaMAILx_GetAll')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_All_mecaMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetAllMecaMAILx

    subroutine GetConnectivityMecaMAILx(idBody, ptr, dim1) BIND(c, name='mecaMAILx_GetConnectivity')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1

      integer(kind=4), dimension(:), pointer :: all

      all => get_connectivity_mecaMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1))
        dim1 = size(all)
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine GetConnectivityMecaMAILx

    subroutine GetElementsVolume(idBody, ptr, length) BIND(c, name='mecaMAILx_GetElementsVolume')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: Volumes       

      Volumes => Get_Elements_Volume(idBody)

      if( associated(Volumes) ) then
        ptr  = c_loc(Volumes(1))
        length = size(Volumes)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine GetElementsVolume
    
    subroutine GetElementsCenter(idBody, ptr, length) BIND(c, name='mecaMAILx_GetElementsCenter')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: Centers       

      Centers => Get_Elements_Center(idBody)

      if( associated(Centers) ) then
        ptr  = c_loc(Centers(1))
        length = size(Centers)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine

    subroutine GetGpCoor(idBody, ptr, dim1, dim2) BIND(c, name='mecaMAILx_GetGpCoor')
      implicit none
      integer(c_int), intent(IN), value :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer  :: gp_coor

      gp_coor => get_gp_coor_mecaMAILx(idBody)

      if( associated(gp_coor) ) then
        ptr  = c_loc(gp_coor(1,1))
        dim1 = size(gp_coor,1)
        dim2 = size(gp_coor,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine

    subroutine GetGpStress(idBody, idEle, idGp, ptr, length) BIND(c, name='mecaMAILx_GetGpStress')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,idEle,idGp
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: field

      field => get_gp_stress_mecaMAILx(idBody, idEle, idGp)

      if( associated(field) ) then
        ptr  = c_loc(field(1))
        length = size(field)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine GetGpStress

    subroutine GetGpStrain(idBody, idEle, idGp, ptr, length) BIND(c, name='mecaMAILx_GetGpStrain')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,idEle,idGp
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: field

      field => get_gp_strain_mecaMAILx(idBody, idEle, idGp)

      if( associated(field) ) then
        ptr  = c_loc(field(1))
        length = size(field)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine GetGpStrain
    
    subroutine GetGpInternals(idBody, idEle, idGp, ptr, length) BIND(c, name='mecaMAILx_GetGpInternals')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,idEle,idGp
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: Internals       

      Internals => Get_Gp_Internals(idBody,idEle,idGp)

      if( associated(Internals) ) then
        ptr  = c_loc(Internals(1))
        length = size(Internals)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine GetGpInternals

    subroutine GetGpPrincipalField(idBody, idEle, idGp, idfield, ptr, dim1, dim2) BIND(c, name='mecaMAILx_GetGpPrincipalField')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE   :: IdBody,idEle,idGp,idfield
      real(kind=8), dimension(:), pointer :: Field
      
      type(c_ptr)                         :: ptr
      integer(c_int)                      :: dim1, dim2

      allocate(Field(12))
      Field = Get_Gp_PrincipalField(idBody,idEle,idGp,idField)
      ptr   = c_loc(Field(1))
      dim1  = 3
      dim2  = 4
      
    end subroutine GetGpPrincipalField
    
    subroutine GetElementsInternal(idBody, id, f, ptr, length) BIND(c, name='mecaMAILx_GetElementsInternal')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,id,f
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: Internals       

      Internals => Get_Elements_Internal(idBody,id,f)

      if( associated(Internals) ) then
        ptr  = c_loc(Internals(1))
        length = size(Internals)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine GetElementsInternal
    
    subroutine GetElementsInternalIntegral(idBody, id, ptr, length) BIND(c, name='mecaMAILx_GetElementsInternalIntegral')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,id
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: Internals       

      Internals => Get_Elements_Internal_integral(idBody,id)

      if( associated(Internals) ) then
        ptr  = c_loc(Internals(1))
        length = size(Internals)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine

    subroutine GetElementsJacobian(idBody, ptr, length) BIND(c, name='mecaMAILx_GetElementsJacobian')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      REAL(kind=8),dimension(:),pointer  :: Jacobians       

      Jacobians => Get_Elements_Jacobian(idBody)

      if( associated(Jacobians) ) then
        ptr  = c_loc(Jacobians(1))
        length = size(Jacobians)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine

    subroutine ComputeElementsEnergy() bind(c, name='mecaMAILx_ComputeElementsEnergy')
      implicit none
      integer :: ibdyty

      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ibdyty)
      !$OMP DO SCHEDULE(RUNTIME)
      do ibdyty = 1, get_nb_mecaMAILx()
        call comp_elements_energy(ibdyty)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

    end subroutine

    subroutine GetPtrElementsEnergy(idBody, ptr, length) bind(c, name='mecaMAILx_GetPtrElementsEnergy')
      implicit none
      integer(c_int), intent(in), value :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      real(c_double), dimension(:), pointer :: Energies

      Energies => get_ptr_elements_energy(idBody)

      if( associated(Energies) ) then
        ptr  = c_loc(Energies(1))
        length = size(Energies)
      else
        ptr  = c_null_ptr
        length = 0
      end if

    end subroutine

    subroutine GetPtrElementsVisibility(idBody, ptr, dim1) BIND(c, name='mecaMAILx_GetPtrElementsVisibility')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1

      integer(kind=4), dimension(:), pointer :: all

      all => GetPtr_Elements_Visibility(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1))
        dim1 = size(all)
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine 

    subroutine GetElementsNeighbor(idBody, tol, ptr, dim1, dim2) BIND(c, name='mecaMAILx_GetElementsNeighbor')
      implicit none
      integer(c_int), intent(in), value :: IdBody
      real(c_double), intent(in), value :: tol
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2
      !
      integer(c_int), dimension(:,:), pointer  :: Neighbors       

      Neighbors => Get_Elements_Neighbor(idBody, tol) 

      if( associated(Neighbors) ) then
        ptr  = c_loc(Neighbors(1,1))
        dim1 = size(Neighbors,1)
        dim2 = size(Neighbors,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine

    SUBROUTINE AddFieldDivergence(ivalue1, ivalue2) bind(c, name='mecaMAILx_AddNodalFieldDivergence')
      IMPLICIT NONE
      integer(c_int),intent(in), value           :: ivalue1, ivalue2

      CALL add_field_divergence_mecaMAILx(ivalue1, ivalue2)


    END SUBROUTINE

    subroutine CleanMemory() bind(c, name='mecaMAILx_CleanMemory')
      implicit none

      call clean_memory_mecaMAILx

    end subroutine

    subroutine ComputeInfoPrincipalStressField(ibdyty, s_ptr, size1) bind(c, name='mecaMAILx_ComputeInfoPrincipalStressField')
      IMPLICIT NONE
      integer(c_int),intent(in), value  :: ibdyty
      integer(c_int) :: size1 
      type(c_ptr)    :: s_ptr

      real(kind=8),dimension(:),pointer :: s
      !

      size1=5
      allocate(s(size1))
      s = Compute_Info_PrincipalStressField_mecaMAILx(ibdyty)


      s_ptr = c_loc(s(1))


    END subroutine ComputeInfoPrincipalStressField
    
    subroutine ComputePDFPressure(s_ptr, size1) bind(c, name='mecaMAILx_ComputePDFPressure')
      IMPLICIT NONE
      integer(c_int) :: size1 
      type(c_ptr)    :: s_ptr

      real(kind=8),dimension(:),pointer :: s
      !

      size1=44
      allocate(s(size1))
      s = Compute_PDF_Pressure_mecaMAILx()
      
      s_ptr = c_loc(s(1))

    END subroutine

    function GetDeformationEnergy(ivalue1,rvect,ivalue2) bind(c, name='mecaMAILx_GetDeformationEnergy')
      IMPLICIT NONE

      integer(c_int),intent(in), value           :: ivalue1,ivalue2
      real(c_double),intent(in) :: rvect(ivalue2)

      real(c_double) :: GetDeformationEnergy

      GetDeformationEnergy = get_deformation_energy_mecaMAILx(ivalue1,rvect)


    END function

    function GetKineticEnergy(ivalue1,rvect,ivalue2) bind(c, name='mecaMAILx_GetKineticEnergy')
      IMPLICIT NONE

      integer(c_int),intent(in), value           :: ivalue1,ivalue2
      real(c_double),intent(in) :: rvect(ivalue2)

      real(c_double) :: GetKineticEnergy

      GetKineticEnergy = get_Kinetic_energy_mecaMAILx(ivalue1,rvect)


    END function

    subroutine GetNeighborElementsToElement(idBody, idEle, ptr, dim1) bind(c, name='mecaMAILx_GetNeighborElementsToElement')
      implicit none
      integer(c_int), intent(in), value :: IdBody, IdEle
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1
      !
      integer(c_int), dimension(:), pointer :: Neighbors, ptr_neigh

      ! for 'security' reasons, a copy is made an not a reference
      ! because this array is ask for only once and should not change
      ! thorough the computation, thus the possibility to change its
      ! is prohibited

      ptr_neigh => get_ptr_neighborEle2ele_mecaMAILx(idBody,idEle)

      if( associated(ptr_neigh) ) then
        dim1 = size(ptr_neigh)
        allocate( Neighbors( dim1 ) )
        Neighbors(:) = ptr_neigh(:)
        ptr  = c_loc(Neighbors(1))
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine

    subroutine GetNeighborElementsToNode(idBody, idNode, ptr, dim1) bind(c, name='mecaMAILx_GetNeighborElementsToNode')
      implicit none
      integer(c_int), intent(in), value :: IdBody, IdNode
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1
      !
      integer(c_int), dimension(:), pointer :: Neighbors, ptr_neigh

      ! for 'security' reasons, a copy is made an not a reference
      ! because this array is ask for only once and should not change
      ! thorough the computation, thus the possibility to change its
      ! is prohibited

      ptr_neigh => get_ptr_neighborEle2node_mecaMAILx(idBody,idNode)

      if( associated(ptr_neigh) ) then
        dim1 = size(ptr_neigh)
        allocate( Neighbors( dim1 ) )
        Neighbors(:) = ptr_neigh(:)
        ptr  = c_loc(Neighbors(1))
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine

    subroutine GetBoundaryElements(idBody, ptr, dim1) bind(c, name='mecaMAILx_GetBoundaryElements')
      implicit none
      integer(c_int), intent(in), value :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1
      !
      integer(c_int), dimension(:), pointer :: Elements, ptr_elem

      ! for 'security' reasons, a copy is made an not a reference
      ! because this array is ask for only once and should not change
      ! thorough the computation, thus the possibility to change its
      ! is prohibited

      ptr_elem => get_ptr_boundaryElements_mecaMAILx(idBody)

      if( associated(ptr_elem) ) then
        dim1 = size(ptr_elem)
        allocate( Elements(dim1) )
        Elements(:) = ptr_elem(:)
        ptr  = c_loc(Elements(1))
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine

    !pta 22/03/2013
    SUBROUTINE LoadWPreconBody(ivalue) bind(c, name='mecaMAILx_LoadWPreconBody')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: ivalue

      CALL load_precon_W_body_mecaMAILx(ivalue)

    END SUBROUTINE

    subroutine GetPtrPreconW(idBody, ptr, dim1, dim2) BIND(c, name='mecaMAILx_GetPtrPreconW')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: idBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2
      real(kind=8), dimension(:,:),pointer :: preconW

      preconW => GetPtr_preconW(idBody)

      if( associated(preconW) ) then
        ptr  = c_loc(preconW(1,1))
        dim1 = size(preconW,1)
        dim2 = size(preconW,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine 

    SUBROUTINE GetInternalVariable(ivalue1,stress,ivalue2,ivalue3) bind(c, name='mecaMAILx_GetInternalVariable')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue3,ivalue2
      type(c_ptr)                       :: stress
      !
      real(c_double), dimension(:,:), pointer :: S
      integer :: inode,i
      
      ivalue2 = get_nb_internal_mecaMAILx(ivalue1)
      ivalue3 = get_nb_nodes_mecaMAILx(ivalue1)
      allocate(S(ivalue2,ivalue3))
      CALL get_2DNodalInternal_mecaMAILx(ivalue1,S)

      stress = c_loc(S(1,1))

    END SUBROUTINE

    !DA: fonction qui renvoie le nombre de variable interne d'un mecaMAILx
    function GetNbInternal(ibdyty) bind(c, name='mecaMAILx_GetNbInternal')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbInternal ! nombre de variable interne d un mecaMAILx 

       GetNbInternal = get_nb_internal_mecaMAILx(ibdyty)

    end function GetNbInternal
    
    subroutine GetPtrBodyVector( cvalue1_c, i_body, out_ptr, length) bind(c, name='mecaMAILx_GetPtrBodyVector')
       implicit none
       
       character(c_char),intent(in), dimension(5) :: cvalue1_c
       integer(c_int), INTENT(IN), value :: i_body
       type(c_ptr)                       :: out_ptr
       integer(c_int)                    :: length

       real(kind=8), dimension(:), pointer :: fortran_ptr
       
       character(len=5) :: cvalue1
       integer :: i, body_vector_id

       cvalue1 = ''
       do i=1,5
          cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
       end do
       body_vector_id = get_body_vector_id_from_name( cvalue1 )

       call get_ptr_body_vector_mecaMAILx(body_vector_id, i_body, fortran_ptr)

       if( associated(fortran_ptr) ) then
          out_ptr  = c_loc(fortran_ptr(1))
          length = size(fortran_ptr)
       else
          out_ptr  = c_null_ptr
          length = 0
       end if

    end subroutine GetPtrBodyVector

    SUBROUTINE GetDofStatus(ivalue1,rvect,ivalue2) bind(c, name='mecaMAILx_GetDofStatus')
      IMPLICIT NONE
      integer(c_int),value  :: ivalue1
      integer(c_int)        :: ivalue2      
      type(c_ptr)           :: rvect
      !
      real(kind=8), dimension(:), pointer :: vector

      ivalue2 = get_nb_nodes_mecaMAILx(ivalue1)
      allocate(vector(ivalue2))
      CALL get_dofstatus_mecaMAILx(ivalue1,vector,ivalue2)

      rvect = c_loc(vector(1))

    END SUBROUTINE

    subroutine PrepGlobalSolver(list_ids, length) bind(c, name='mecaMAILx_PrepGlobalSolver')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

      !                                                 !12345678901234567890
      !if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp Free V ')
      !call start_itimer(timer_id)

      if ( is_externalFEM ) then
        stop 
      !   call externalFEM_compute_free_vlocy_mecaMAILx
      !   call stop_itimer(timer_id)
      !   return
      endif

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call prep_global_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call prep_global_mecaMAILx(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      !call stop_itimer(timer_id)

    end subroutine

    subroutine PostGlobalSolver(list_ids, length) bind(c, name='mecaMAILx_PostGlobalSolver')
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4) :: i
      integer(kind=4), save :: timer_id = 0
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_mecaMAILx() < 1 ) return

      !                                                 !12345678901234567890
      !if( timer_id == 0 ) timer_id = get_new_itimer_ID('[mecaM] comp dof    ')
      !call start_itimer(timer_id)

      if ( is_externalFEM ) then
        stop  
      !  call externalFEM_compute_dof_mecaMAILx
      !  call stop_itimer(timer_id)
      !  return
      end if

      if( length == 0 ) then  
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, get_nb_mecaMAILx()
          call post_global_mecaMAILx(i)
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
        !$OMP DO SCHEDULE(RUNTIME)
        do i = 1, length
          call post_global_mecaMAILx(list(i))
        end do
        !$OMP END DO
        !$OMP END PARALLEL
      end if

      !call stop_itimer(timer_id)

    end subroutine

    subroutine AddBodyForceToFext(ivalue1, mat_in, dim1, dim2) bind(c, name='mecaMAILx_AddBodyForceToFext')
      implicit none

      integer(c_int)   , intent(in), value        :: ivalue1, dim1, dim2
      type(c_ptr)                  , value        :: mat_in
      !
      real(kind=8), dimension(:), pointer :: vector

      call c_f_pointer(cptr=mat_in, fptr=vector, shape=(/dim1*dim2/))
      call add_body_force_to_fext_mecaMAILx(ivalue1, vector, dim1*dim2)

    end subroutine

    subroutine CheckProperties() bind(c, name='mecaMAILx_CheckProperties')
      implicit none
      !

      if( get_nb_mecaMAILx() < 1 ) return

      call check_properties_mecaMAILx()
       
    end subroutine CheckProperties
 
    subroutine GetNbGpByElem(elems, esize, ssize, nbgps, nsize) bind(c, name='mecaMAILx_GetNbGpByElem')
      implicit none

      type(c_ptr) :: elems, nbgps
      integer(c_int) :: esize, ssize, nsize
      !
      character(len=5), dimension(:), pointer :: names
      integer(c_int)  , dimension(:), pointer :: n_GPs

      ssize = 5

      call get_nb_gp_by_elem_mecaMAILx(names, n_Gps, esize)

      nsize = esize
      elems = c_loc( names(1) )
      nbgps = c_loc( n_GPs(1) )
    end subroutine GetNbGpByElem
    
    subroutine MassScaling(scale) bind(c, name='mecaMAILx_MassScaling')
      implicit none
      !
      real(c_double),intent(in),value :: scale

      call mass_scaling_mecaMAILx(scale)
       
    end subroutine MassScaling
    
    subroutine GetGpAllJoint(iv,iv1,iv2) bind(c, name='mecaMAILx_GetGpAllJoint')
      implicit none
      type(c_ptr)    :: iv 
      integer(c_int) :: iv1,iv2
      !
      real(kind=8), dimension(:,:), pointer :: all 

      all=>null()
      
      call get_gp_all_joint_mecaMAILx(all)
      
      if ( associated(all) ) then
         iv1 = size(all,dim=1)
         iv2 = size(all,dim=2)
      else
         iv1 = 0
         iv2 = 0
      end if
      iv = c_loc(all(1,1))
    end subroutine
        
    subroutine SetVisibleVlocyDrivenDof( ibdyty, inod, idof ) &
        bind( c, name='mecaMAILx_SetVisibleVlocyDrivenDof' )

      implicit none
      integer( c_int ), intent( in ), value :: ibdyty
      integer( c_int ), intent( in ), value :: inod
      integer( c_int ), intent( in ), value :: idof

      call switch_vlocy_driven_dof( ibdyty, inod, idof, 1 )

    end subroutine SetVisibleVlocyDrivenDof

    subroutine SetInvisibleVlocyDrivenDof( ibdyty, inod, idof ) &
        bind( c, name='mecaMAILx_SetInvisibleVlocyDrivenDof' )

      implicit none
      integer( c_int ), intent( in ), value :: ibdyty
      integer( c_int ), intent( in ), value :: inod
      integer( c_int ), intent( in ), value :: idof      

      call switch_vlocy_driven_dof( ibdyty, inod, idof, 0 )

    end subroutine SetInvisibleVlocyDrivenDof
    
    subroutine UpdateVlocyDrivenDofStructures( ibdyty) &
        bind( c, name='mecaMAILx_UpdateVlocyDrivenDofStructures' )

      implicit none
      integer( c_int ), intent( in ), value :: ibdyty

      call update_vlocy_driven_dof_structures( ibdyty )

    end subroutine UpdateVlocyDrivenDofStructures


    
END MODULE wrap_mecaMAILx
