!===========================================================================
!
! Copyright 2000-2022 CNRS-UM.
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

! module managing the time integration
module integrator

  use paranoid_checks

  use integrator_parameters
  use parameters

  use utilities
  use algebra
  use a_DOF
  use RigidKinematic

  use state
  use modelization, only : set_cc_meca_nodal_field, &
                           set_cc_ther_nodal_field, &
                           set_cc_x_nodal_field   , &
                           compute_inertia_forces

  implicit none

  private

  !> a generic integrator type
  type, public :: T_integrator
    !
    !> integrator type (moreau, verlet...)
    integer(kind=4) :: i_type  
    !> integrator formulation (meca rigid, meca defor, ther defor...)
    integer(kind=4) :: i_form
    !> number matrices to store to compute left hand side matrix
    integer(kind=4) :: nb_mats
    !> number of nodal fields to store
    integer(kind=4) :: nb_nf
    !> number of elementary rhs contributions to store
    integer(kind=4) :: nb_erc
    !> number of elementary fields to store
    integer(kind=4) :: nb_ef
    !
    !> time depth needed for each nodal field
    integer(kind=4), dimension(:), allocatable :: nf_depths
    !> time depth needed for each elementary rhs contributions
    integer(kind=4), dimension(:), allocatable :: erc_depths
    !> time depth needed for each elementary fields
    integer(kind=4), dimension(:), allocatable :: ef_depths
    !
    real(kind=8), dimension(:), allocatable :: params    !< real parameters of the integrator (dt, theta...)
    !
    real(kind=8), dimension(:), allocatable :: lhs_coeff !< coefficients to affect to matrices to compute left hand side matrix

    !> pointer on a subroutine  to compute the right hand side
    procedure(proto_compute_rhs), pointer, nopass :: compute_rhs => null()
    !> pointer on a subroutine to get a displacement to compute bulks
    procedure(proto_get_config_for_bulk_comp),   pointer, nopass :: get_config_for_bulk_comp => null()
    !> pointer on a subroutine to get nodal_fields' map
    procedure(proto_set_cc_nodal_fields), pointer, nopass :: set_cc_nodal_fields => null()
    !> pointer on a subroutine to compute dual driven dof values
    procedure(proto_compute_dual_drvdofs), pointer, nopass :: compute_dual_drvdofs => null()
    !> pointer on a subroutine to compute the degrees of freedom which were not computed by the solver
    procedure(proto_compute_dofs), pointer, nopass :: compute_dofs => null()
    !> pointer on a subroutine to compute the detection configuration
    procedure(proto_compute_detec_config), pointer, nopass :: compute_detec_config => null()
    !
    integer(kind=4) :: i_solve_nodal_field
  end type

  ! interface of subroutine to compute right and side of a system of equation
  abstract interface
    subroutine proto_compute_rhs(id_modelization, i_type, state, i_element, edof2gdof, i_dof, rhs, params)
      import T_state
      integer(kind=4), intent(in)    :: id_modelization
      integer(kind=4), intent(in)    :: i_type
      type(T_state),   intent(inout) :: state
      integer(kind=4), intent(in)    :: i_element
      integer(kind=4), intent(in)    :: i_dof
      integer(kind=4), dimension(:), intent(in)  :: edof2gdof
      real(kind=8),    dimension(:), intent(out) :: rhs
      real(kind=8),    dimension(:), intent(in)  :: params
    end subroutine
  end interface

  ! interface of subroutine to compute a position/displacement to compute the bulks
  abstract interface
    subroutine proto_get_config_for_bulk_comp(state, U, params)
      import T_state
      type(T_state),   intent(in)                :: state
      real(kind=8),    dimension(:), intent(out) :: U
      real(kind=8),    dimension(:), intent(in)  :: params
    end subroutine
  end interface

  ! interface of subroutine to compute the nodal fields' map
  abstract interface
    subroutine proto_set_cc_nodal_fields(id, i_type, nb_nodes, cc_nodal_fields, nb_dofs_to_solve)
      integer(kind=4), intent(in) :: id
      integer(kind=4), intent(in) :: i_type
      integer(kind=4), intent(in) :: nb_nodes
      integer(kind=4), dimension(:,:), intent(inout) :: cc_nodal_fields
      integer(kind=4),                 intent(out)   :: nb_dofs_to_solve
    end subroutine
  end interface

  ! interface of subroutine to compute the dual driven dofs
  abstract interface
    subroutine proto_compute_dual_drvdofs(time, drvdofs, state, driv, nb_drvdofs, params)
      import T_driven_dof
      import T_state
      real(kind=8),                     intent(in)  :: time
      type(T_driven_dof), dimension(:), intent(in)  :: drvdofs
      type(T_state),                    intent(in)  :: state
      real(kind=8),       dimension(:), intent(out) :: driv
      integer(kind=4),                  intent(in)  :: nb_drvdofs
      real(kind=8),       dimension(:), intent(in)  :: params
    end subroutine
  end interface

  ! interface of subroutine to compute the dofs of system which were not computed by the solver
  abstract interface
    subroutine proto_compute_dofs(state, params)
      import T_state
      type(T_state),               intent(inout) :: state
      real(kind=8),  dimension(:), intent(in)    :: params
    end subroutine
  end interface

  ! interface of subroutine to compute the detection configuration
  abstract interface
    subroutine proto_compute_detec_config(state, params, node_list, rigid_detec_config, defor_detec_config)
      import T_state
      type(T_state),                   intent(in)  :: state
      real(kind=8),    dimension(:),   intent(in)  :: params
      integer(kind=4), dimension(:),   intent(in)  :: node_list
      real(kind=8),    dimension(:,:), allocatable :: rigid_detec_config
      real(kind=8),    dimension(:,:), allocatable :: defor_detec_config
    end subroutine
  end interface


  public new_mechanical_integrator        , &
         new_thermal_integrator           , &
         initialize_meca_rigid_integrator , &
         initialize_meca_defor_integrator , &
         initialize_ther_defor_integrator , &
         erase_integrator                 , &
         display_integrator

  private meca_rigid_moreau_compute_rhs    , &
          meca_defor_moreau_compute_rhs    , &
          ther_defor_moreau_compute_rhs    , &
          meca_set_cc_nodal_fields         , &
          ther_set_cc_nodal_fields         , &
          meca_moreau_displacement_for_bulk, &
          ther_moreau_temperature_for_bulk , &
          meca_rigid_compute_dofs          , &
          meca_defor_compute_dofs          , &
          theta_compute_dual_drvdofs       , &
          theta_integration                , &
          meca_rigid_2D_leap_frog_detec    , &
          meca_rigid_3D_leap_frog_detec    , &
          meca_defor_leap_frog_detec

  contains

  !> \brief Create a new mechanical integrator
  !> Possible values for integrator_type should be: 
  !> 'moreau', 'gear', 'verlet' or 'newmark'
  !> Possible values for formulation are: 
  !> 'rigid', 'defor'
  !> params is a real vector defined accordingly to
  !> integrator_type and formulation values
  function new_mechanical_integrator(integrator_type, formulation, params)
    implicit none
    !> [in] type of integrator to use with mechanical model
    character(len=7),               intent(in) :: integrator_type
    !> [in] type of formulation associated to the new integrator
    character(len=5),               intent(in) :: formulation
    !> [in] list or reals parameters needed by the integrator
    real(kind=8),     dimension(:), intent(in) :: params
    !> [return] new mechanical integrator
    type(T_integrator), pointer :: new_mechanical_integrator
    !
    character(len=80) :: cout
    character(len=37) :: IAM
    !      1234567890123456789012345678901234567
    IAM = 'integrator::new_mechanical_integrator'

    ! rm: all integers variables with p_ prefix come from integrator_parameter module

    allocate(new_mechanical_integrator)

    if( formulation == 'rigid' ) then
      new_mechanical_integrator%i_form = get_formulation_id_from_type('meca_rigid')
    else if( formulation == 'defor' ) then
      new_mechanical_integrator%i_form = get_formulation_id_from_type('meca_defor')
    else
      deallocate(new_mechanical_integrator)
      write (cout,'(A,1x,A,1x,A)') 'unknown formulation type', formulation, 'to create new mechanical integrator'
      call faterr(IAM,cout)
    end if

    new_mechanical_integrator%i_type = get_integrator_id_from_type(integrator_type)
    ! solve velocity
    new_mechanical_integrator%i_solve_nodal_field = p_meca_td_V

    select case( new_mechanical_integrator%i_type )
    case( p_moreau )

      ! moreau integrator has two real data : time_step and theta
      if( size(params) /= 2 ) then
        write (cout,'(A,1x,I0)') 'params input vector should be of size 2 for moreau integrator, is :', size(params)
        call faterr(IAM,cout)
      end if

      allocate(new_mechanical_integrator%params(2))
      new_mechanical_integrator%params(1:2) = params(1:2)

      ! number of fields to store
      new_mechanical_integrator%nb_nf  = p_meca_nb_nodal_fields    !X, V
      allocate(new_mechanical_integrator%nf_depths(p_meca_nb_nodal_fields))
      ! setting time depths for each stored field
      new_mechanical_integrator%nf_depths(p_meca_td_X)      = 2
      new_mechanical_integrator%nf_depths(p_meca_td_V)      = 2

      new_mechanical_integrator%nb_erc = p_meca_nb_el_rhs_contribs !Fint, Fext
      allocate(new_mechanical_integrator%erc_depths(p_meca_nb_el_rhs_contribs))
      new_mechanical_integrator%erc_depths(p_meca_td_Fint)  = 1
      new_mechanical_integrator%erc_depths(p_meca_td_Fext)  = 2


      if( new_mechanical_integrator%i_form == p_meca_rigid ) then
        new_mechanical_integrator%nb_ef = 0 !field
        !allocate(meca_rigid_integrator%ef_depths(0))
      else if( new_mechanical_integrator%i_form == p_meca_defor ) then
        new_mechanical_integrator%nb_ef = p_meca_nb_elementary_fields !field
        allocate(new_mechanical_integrator%ef_depths(p_meca_nb_elementary_fields))
        new_mechanical_integrator%ef_depths(p_meca_td_field) = 2
      !else !impossible...
      !  write (cout,'(A,1x,A,1x,A)') 'unknown formulation type', formulation, 'to create new mechanical integrator'
      !  call faterr(IAM,cout)
      end if

    case default

      write (cout,'(A,1x,A)') 'unknown mechanical integrator type:', get_integrator_type_from_id(new_mechanical_integrator%i_type)
      call faterr(IAM,cout)

    end select

  end function

  !> \brief Create a new thermal integrator
  !> Possible values for integrator_type should be: 
  !> 'moreau', 'gear', 'verlet' or 'newmark'
  !> Possible values for formulation are: 
  !> 'defor'
  !> params is a real vector defined accordingly to
  !> integrator_type and formulation values
  function new_thermal_integrator(integrator_type, formulation, params)
    implicit none
    !> [in] type of integrator to use with thermal model
    character(len=7),               intent(in) :: integrator_type
    !> [in] type of formulation associated to the new integrator
    character(len=5),               intent(in) :: formulation
    !> [in] list or reals parameters needed by the integrator
    real(kind=8),     dimension(:), intent(in) :: params
    !> [return] new thermal integrator
    type(T_integrator), pointer :: new_thermal_integrator
    !
    character(len=80) :: cout
    character(len=34) :: IAM
    !      1234567890123456789012345678901234
    IAM = 'integrator::add_thermal_integrator'

    allocate(new_thermal_integrator)

    if( formulation == 'defor' ) then
      new_thermal_integrator%i_form = get_formulation_id_from_type('ther_defor')
    else
      deallocate(new_thermal_integrator)
      write (cout,'(A,1x,A,1x,A)') 'unknown formulation type', formulation, 'to create new thermal integrator'
      call faterr(IAM,cout)
    end if

    new_thermal_integrator%i_type = get_integrator_id_from_type(integrator_type)

    ! solve temperature
    new_thermal_integrator%i_solve_nodal_field = p_ther_td_T

    select case( new_thermal_integrator%i_type )
    case( p_moreau )

      ! moreau integrator has two real data : time_step and theta
      if( size(params) /= 2 ) then
        write (cout,'(A,1x,I0)') 'params input vector should be of size 2 for moreau integrator, is :', size(params)
        call faterr(IAM,cout)
      end if

      allocate(new_thermal_integrator%params(2))
      new_thermal_integrator%params(1:2) = params(1:2)

      ! number of fields to store
      new_thermal_integrator%nb_nf  = p_ther_nb_nodal_fields      !T
      new_thermal_integrator%nb_erc = p_ther_nb_el_rhs_contribs   !Fint, Fext
      new_thermal_integrator%nb_ef  = p_ther_nb_elementary_fields !field

      allocate(new_thermal_integrator%nf_depths(p_ther_nb_nodal_fields))
      allocate(new_thermal_integrator%erc_depths(p_ther_nb_el_rhs_contribs))
      allocate(new_thermal_integrator%ef_depths(p_ther_nb_elementary_fields))

      ! setting time depths for each stored field
      new_thermal_integrator%nf_depths(p_ther_td_T)     = 2
      new_thermal_integrator%erc_depths(p_ther_td_Fint) = 1
      new_thermal_integrator%erc_depths(p_ther_td_Fext) = 2
      new_thermal_integrator%ef_depths(p_ther_td_field) = 2

    case default

      write (cout,'(A,1x,A)') 'unknown deformable thermal integrator type :', &
                              get_integrator_type_from_id(new_thermal_integrator%i_type)
      call faterr(IAM,cout)

    end select

  end function

  !> \brief Initialize the mechanical rigid integrator
  !> To call only once the parameters are set
  subroutine initialize_meca_rigid_integrator(integr)
    implicit none
    !> [in] integrator object to initialize
    type(T_integrator), pointer :: integr
    !
    real(kind=8)      :: dt, theta
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=44) :: IAM
    !      12345678901234567890123456789012345678901234
    IAM = 'integrator::initialize_meca_rigid_integrator'

    if( .not. associated(integr) ) then
      write(cout,'(A,1x,A)') IAM, 'meca_rigid_integrator not allocated'
      call logmes(cout)
      return
    end if

    ! WARNING : use variables defined in integrator_parameter

    select case( integr%i_type )
    case( p_moreau )

      ! check/display content of meca_integrator ?
      call paranoid_check_r8_size(IAM, integr%params, 2)
      dt    = integr%params(p_time_step)
      theta = integr%params(p_theta)

      ! setting lhs build coefficient for rigid bodies :
      ! M_tilde = 1*Mass
      integr%nb_mats = p_meca_rigid_nb_mats
      allocate( integr%lhs_coeff(p_meca_rigid_nb_mats), stat=errare )
      integr%lhs_coeff(p_mass_mat_index) = 1.d0

      ! setting rhs_integration : RHS = h*Fext_i+1
      integr%compute_rhs => meca_rigid_moreau_compute_rhs

      integr%set_cc_nodal_fields => meca_set_cc_nodal_fields

      integr%compute_dual_drvdofs => theta_compute_dual_drvdofs

      integr%compute_dofs => meca_rigid_compute_dofs

      if( nbDIME == 2 ) then
        integr%compute_detec_config => meca_rigid_2D_leap_frog_detec
      else
        integr%compute_detec_config => meca_rigid_3D_leap_frog_detec
      end if

    case default
      write (cout,'(A,1x,A)') 'Unknown mechanical rigid integrator type:', &
                              get_integrator_type_from_id(integr%i_type)
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Initialize the mechanical deformalbe integrator
  !> To call only once the parameters are set
  subroutine initialize_meca_defor_integrator(integr)
    implicit none
    !> [in] integrator object to initialize
    type(T_integrator), pointer :: integr
    !
    real(kind=8)      :: dt, theta
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=44) :: IAM
    !      12345678901234567890123456789012345678901234
    IAM = 'integrator::initialize_meca_defor_integrator'

    if( .not. associated(integr) ) then
      write(cout,'(A,1x,A)') IAM, 'meca_defor_integrator not allocated'
      call logmes(cout)
      return
    end if

    ! WARNING : use variables defined in integrator_parameter

    select case( integr%i_type )
    case( p_moreau )

      ! check/display content of meca_integrator ?
      call paranoid_check_r8_size(IAM, integr%params, 2)
      dt    = integr%params(p_time_step)
      theta = integr%params(p_theta)

      ! setting lhs build coefficient for deformable bodies :
      ! M_tilde = 1*Mass + h*theta Viscosity + h*h*theta*theta Stiffness
      integr%nb_mats = p_meca_defor_nb_mats
      allocate( integr%lhs_coeff(p_meca_defor_nb_mats) )
      integr%lhs_coeff(p_mass_mat_index) = 1.d0
      integr%lhs_coeff(p_visc_mat_index) = dt*theta
      integr%lhs_coeff(p_stif_mat_index) = dt*dt*theta*theta

      ! setting rhs_integration : RHS = -h*Fint_i+1/2 + h*Fext_i+1 + Finert
      integr%compute_rhs => meca_defor_moreau_compute_rhs

      integr%get_config_for_bulk_comp => meca_moreau_displacement_for_bulk

      integr%set_cc_nodal_fields => meca_set_cc_nodal_fields

      integr%compute_dual_drvdofs => theta_compute_dual_drvdofs

      integr%compute_dofs => meca_defor_compute_dofs

      integr%compute_detec_config => meca_defor_leap_frog_detec

    case default
      write (cout,'(A,1x,A)') 'Unknown mechanical deformable integrator type:', &
                              get_integrator_type_from_id(integr%i_type)
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Initialize the thermal deformable integrator
  !> To call only once the parameters are set
  subroutine initialize_ther_defor_integrator(integr)
    implicit none
    !> [in] integrator object to initialize
    type(T_integrator), pointer :: integr
    !
    real(kind=8)      :: dt, theta
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=44) :: IAM
    !      12345678901234567890123456789012345678901234
    IAM = 'integrator::initialize_ther_defor_integrator'

    if( .not. associated(integr) ) then
      write(cout,'(A,1x,A)') IAM, 'ther_defor_integrator not allocated'
      call logmes(cout)
      return
    end if

    ! WARNING : use variables defined in integrator_parameter

    select case( integr%i_type )
    case( p_moreau )

      ! check/display content of ther_integrator ?
      call paranoid_check_r8_size(IAM, integr%params, 2)
      dt    = integr%params(p_time_step)
      theta = integr%params(p_theta)

      ! setting lhs build coefficient for deformable bodies :
      ! M_tilde = 1*Capacity + h*theta Conductivity
      integr%nb_mats = p_ther_defor_nb_mats
      allocate( integr%lhs_coeff(p_ther_defor_nb_mats) )
      integr%lhs_coeff(p_capa_mat_index) = 1.d0
      integr%lhs_coeff(p_cond_mat_index) = dt*theta

      ! setting rhs_integration : RHS = -h*Fint_i+1/2 + h*theta*Fext_i+1 + h*(1-theta)*Fext_i + Finert
      integr%compute_rhs => ther_defor_moreau_compute_rhs

      integr%get_config_for_bulk_comp => ther_moreau_temperature_for_bulk

      integr%set_cc_nodal_fields => ther_set_cc_nodal_fields

      integr%compute_dual_drvdofs => theta_compute_dual_drvdofs

    case default
      write (cout,'(A,1x,A)') 'Unknown thermal deformable integrator type:', &
                              get_integrator_type_from_id(integr%i_type)
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Compute the configuration in which  the bulk will be computed
  subroutine  meca_moreau_displacement_for_bulk(state, du, params)
    implicit none
    !> [in,out] state on which to compute the rhs
    type(T_state),                 intent(in)  :: state
    !> [out] configuration in which to compute bulks
    real(kind=8),    dimension(:), intent(out) :: du
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:), intent(in)  :: params
    !
    integer(kind=4)   :: i, i1, i2, i3, i4, i5, i6
    real(kind=8)      :: dt, theta
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = "integrator::meca_moreau_displacement"

    dt    = params(p_time_step)
    theta = params(p_theta)

    ! compute medium configuration
    call get_nodal_field_indices_boundary(i1, i2, state, p_meca_td_X, 2)
    call get_nodal_field_indices_boundary(i3, i4, state, p_meca_td_V, 2)
    call get_nodal_field_indices_boundary(i5, i6, state, p_meca_td_V, 1)

    call paranoid_check_r8_size(IAM, du, i2-i1+1)
    call paranoid_check_r8_size(IAM, du, i4-i3+1)

    ! Xtt = Xbegin + THETA*H*( (1-THETA)*Vbegin + THETA*V )
    du(1:i2-i1+1) = state%nodal_fields(i1:i2) + dt * theta * (1.d0-theta) * state%nodal_fields(i3:i4) &
                                              + dt * theta * theta        * state%nodal_fields(i5:i6)
  end subroutine

  !> \brief Compute the temperature in which the bulk will be computed
  subroutine  ther_moreau_temperature_for_bulk(state, temp, params)
    implicit none
    !> [in,out] state on which to compute the rhs
    type(T_state),                 intent(in)  :: state
    !> [out] configuration in which to compute bulks
    real(kind=8),    dimension(:), intent(out) :: temp
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:), intent(in)  :: params
    !
    integer(kind=4)   :: i, i1, i2, i3, i4, i5, i6
    real(kind=8)      :: dt, theta
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = "integrator::ther_moreau_temperature"

    dt    = params(p_time_step)
    theta = params(p_theta)

    ! compute medium configuration
    call get_nodal_field_indices_boundary(i1, i2, state, p_ther_td_T, 2)
    call get_nodal_field_indices_boundary(i3, i4, state, p_ther_td_T, 1)

    call paranoid_check_r8_size(IAM, temp, i2-i1+1)

    ! Ttt = THETA*T + (1-THETA)*Tbegin
    temp(1:i2-i1+1) = theta * state%nodal_fields(i1:i2) + (1.d0-theta) * state%nodal_fields(i3:i4)

  end subroutine

  !> \brief Computation of the elementary RHS_Free of a rigid model_handle for a meca moreau integrator
  !> Prototype must match abstract interface of proto_compute_rhs
  subroutine meca_rigid_moreau_compute_rhs(id_modelization, i_type, state, i_element, edof2gdof, i_dof, rhs, params)
    implicit none
    !> [in] id of the concerned modelization
    integer(kind=4), intent(in)    :: id_modelization
    !> [in] type of the modelization
    integer(kind=4), intent(in)    :: i_type
    !> [in,out] state on which to compute the rhs
    type(T_state),   intent(inout) :: state
    !> [in] index of element 
    integer(kind=4), intent(in)    :: i_element
    !> [in] number of dofs in the element
    integer(kind=4), intent(in)    :: i_dof
    !> [in] map from local dof to global dof
    integer(kind=4), dimension(:), intent(in)  :: edof2gdof
    !> [out] computed elementary rhs_free
    real(kind=8),    dimension(:), intent(out) :: rhs
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:), intent(in)  :: params
    !
    integer(kind=4) :: i, j, i1, i2, i3, i4
    real(kind=8)    :: dt, theta

    dt    = params(p_time_step)
    theta = params(p_theta)

    ! problem with value of Fext at previous time step during first time step

    ! rhs_integration : RHS = h*Fext_i+1
    rhs(1:i_dof) = 0.D0

    call get_element_el_rhs_contrib_indices_boundary(i1, i2, state, p_meca_td_Fext, i_element, 1)
    !call get_el_rhs_contrib_indices_boundary(i3, i4, state, p_meca_td_Fext, i_element, 2)

    !call theta_integration(rhs(1:i_dof), state%el_rhs_contribs(i1:i2), state%el_rhs_contribs(i3:i4), dt, theta, i_dof)
    rhs(1:i_dof) = dt * state%el_rhs_contribs(i1:i2)

  end subroutine

  !> \brief Computation of the elementary RHS_Free of a deformable model_handle for a meca moreau integrator
  subroutine meca_defor_moreau_compute_rhs(id_modelization, i_type, state, i_element, edof2gdof, i_dof, rhs, params)
    implicit none
    !> [in] id of the concerned modelization
    integer(kind=4), intent(in)    :: id_modelization
    !> [in] type of the modelization
    integer(kind=4), intent(in)    :: i_type
    !> [in,out] state on which to compute the rhs
    type(T_state),   intent(inout) :: state
    !> [in] index of element 
    integer(kind=4), intent(in)    :: i_element
    !> [in] number of dofs in the element
    integer(kind=4), intent(in)    :: i_dof
    !> [in] map from local dof to global dof
    integer(kind=4), dimension(:), intent(in)  :: edof2gdof
    !> [out] computed elementary rhs_free
    real(kind=8),    dimension(:), intent(out) :: rhs
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:), intent(in)  :: params
    !
    integer(kind=4) :: i, i1, i2, i3, i4
    real(kind=8)    :: dt, theta, dv(i_dof), inert(i_dof)

    ! when is increment_step like function done ?

    dt    = params(p_time_step)
    theta = params(p_theta)

    ! \todo : solve problem with value of Fext at previous time step during first time step


    ! rhs_integration : RHS = -h*Fint_i+1/2 + h*theta*Fext_i+1 + h*(1-theta)*Fext_i + M*dv
    rhs(1:i_dof) = 0.d0

    ! Fint_i+1/2
    call get_element_el_rhs_contrib_indices_boundary(i1, i2, state, p_meca_td_Fint, i_element, 1)
    rhs(1:i_dof) = rhs(1:i_dof) - dt*state%el_rhs_contribs(i1:i2)

    ! Fext
    call get_element_el_rhs_contrib_indices_boundary(i1, i2, state, p_meca_td_Fext, i_element, 1)
    call get_element_el_rhs_contrib_indices_boundary(i3, i4, state, p_meca_td_Fext, i_element, 2)

    !call theta_integration(rhs(1:i_dof), state%el_rhs_contribs(i1:i2), state%el_rhs_contribs(i3:i4), dt, theta, i_dof)
    rhs(1:i_dof) = rhs(1:i_dof) + dt*state%el_rhs_contribs(i1:i2)

    ! M*dv
    call get_nodal_field_index(i1, state, p_meca_td_V, 1)
    call get_nodal_field_index(i2, state, p_meca_td_V, 2)
    do i = 1, i_dof
      dv(i) = state%nodal_fields(i2+edof2gdof(i)) - state%nodal_fields(i1+edof2gdof(i))
    end do
    call compute_inertia_forces(id_modelization, i_type, i_element, p_mass_mat_index, inert(1:i_dof), dv)
    rhs(1:i_dof) = rhs(1:i_dof) + inert(1:i_dof)
    
  end subroutine

  !> \brief Computation of the elementary RHS_Free of a model_handle for a ther moreau integrator
  subroutine ther_defor_moreau_compute_rhs(id_modelization, i_type, state, i_element, edof2gdof, i_dof, rhs, params)
    implicit none
    !> [in] id of the concerned modelization
    integer(kind=4), intent(in)    :: id_modelization
    !> [in] type of the modelization
    integer(kind=4), intent(in)    :: i_type
    !> [in,out] state on which to compute the rhs
    type(T_state),   intent(inout) :: state
    !> [in] index of element 
    integer(kind=4), intent(in)    :: i_element
    !> [in] number of dofs in the element
    integer(kind=4), intent(in)    :: i_dof
    !> [in] map from local dof to global dof
    integer(kind=4), dimension(:), intent(in)  :: edof2gdof
    !> [out] computed elementary rhs_free
    real(kind=8),    dimension(:), intent(out) :: rhs
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:), intent(in)  :: params
    !
    integer(kind=4) :: i, i1, i2, i3, i4
    real(kind=8)    :: dt, theta, dv(i_dof)

    dt    = params(p_time_step)
    theta = params(p_theta)

    ! todo : compute fint in current configuration
    ! todo : compute fext in current configuration

    ! rhs_integration : RHS = -h*Fint_i+1/2 + h*theta*Fext_i+1 + h*(1-theta)*Fext_i - conductivity*T_i
    rhs(1:i_dof) = 0.d0

    ! Fint_i+1/2
    call get_element_el_rhs_contrib_indices_boundary(i1, i2, state, p_ther_td_Fint, i_element, 1)
    rhs(1:i_dof) = rhs(1:i_dof) - state%el_rhs_contribs(i1:i2)

    ! Fext
    call get_element_el_rhs_contrib_indices_boundary(i1, i2, state, p_ther_td_Fext, i_element, 1)
    call get_element_el_rhs_contrib_indices_boundary(i3, i4, state, p_ther_td_Fext, i_element, 2)
    call theta_integration(rhs(1:i_dof), state%el_rhs_contribs(i3:i4), state%el_rhs_contribs(i1:i2), dt, theta, i_dof)

    ! conductivity * T_i
    call get_nodal_field_index(i1, state, p_ther_td_T, 2)
    do i = 1, i_dof
      dv(i) = - dt * state%nodal_fields(i1+edof2gdof(i)) !rm: not sure about this...
    end do
    call compute_inertia_forces(id_modelization, i_type, i_element, p_cond_mat_index, rhs(1:i_dof), dv)

  end subroutine

  !> \brief Compute the integrated value of dual driven dofs for a theta scheme
  subroutine theta_compute_dual_drvdofs(time, drvdofs, state, driv, nb_drvdofs, params)
    implicit none
    !> [in] beginning time of computation step
    real(kind=8),                     intent(in)  :: time
    !> [in] the list of dual driven dofs
    type(T_driven_dof), dimension(:), intent(in)  :: drvdofs
    !> [in] state to get nf_map
    type(T_state),                    intent(in)  :: state
    !> [out] the computed value of the driven dofs
    real(kind=8),       dimension(:), intent(out) :: driv
    !> [in] the number of driven dofs
    integer(kind=4),                  intent(in)  :: nb_drvdofs
    !> [in] list or reals parameters needed by the integrator
    real(kind=8),       dimension(:), intent(in)  :: params
    !
    integer(kind=4) :: i_drvdof
    real(kind=8)    :: dt, theta, val

    dt    = params(p_time_step)
    theta = params(p_theta)

    do i_drvdof = 1, nb_drvdofs
      call comp_a_driven_dof_at_t(drvdofs(i_drvdof), time+dt, val)
      driv(i_drvdof) = dt * theta * val
      call comp_a_driven_dof_at_t(drvdofs(i_drvdof), time, val)
      driv(i_drvdof) = driv(i_drvdof) + dt * (1.d0-theta) * val
    end do

  end subroutine

  !> \brief Compute displacements with a theta scheme
  subroutine meca_defor_compute_dofs(state, params)
    implicit none
    !> [in,out] state on which to compute
    type(T_state),               intent(inout) :: state
    !> [in] list or reals parameters needed by the integrator
    real(kind=8),  dimension(:), intent(in)    :: params
    !
    integer(kind=4) :: i1, i2, i3, i4, i5, i6
    real(kind=8)    :: dt, theta

    dt    = params(p_time_step)
    theta = params(p_theta)

    call get_nodal_field_indices_boundary(i1, i2, state, p_meca_td_X, 1)
    call get_nodal_field_indices_boundary(i3, i4, state, p_meca_td_X, 2)

    ! X = Xbeg
    state%nodal_fields(i1:i2) = state%nodal_fields(i3:i4)

    call get_nodal_field_indices_boundary(i3, i4, state, p_meca_td_V, 1)
    call get_nodal_field_indices_boundary(i5, i6, state, p_meca_td_V, 2)

    ! X = X + h*theta*V + h*(1-theta)*Vbegin
    call theta_integration(state%nodal_fields(i1:i2), state%nodal_fields(i5:i6), state%nodal_fields(i3:i4), dt, theta, i2-i1+1)

    !print *,' vitesse : '
    !print *, state%nodal_fields(i3:i4)
    !print *,' position: '
    !print *, state%nodal_fields(i1:i2)
  end subroutine

  !> \brief Compute displacements with a theta scheme with integration of rotation in case of 3D
  subroutine meca_rigid_compute_dofs(state, params)
    implicit none
    !> [in,out] state on which to compute
    type(T_state),               intent(inout) :: state
    !> [in] list or reals parameters needed by the integrator
    real(kind=8),  dimension(:), intent(in)    :: params
    !
    integer(kind=4) :: i1, i2, i3, i4, i5, i6
    real(kind=8)    :: dt, theta, omega

    dt    = params(p_time_step)
    theta = params(p_theta)

    call get_nodal_field_index(i1, state, p_meca_td_X, 1)
    call get_nodal_field_index(i2, state, p_meca_td_X, 2)

    i1 = i1 + 1
    i2 = i2 + 1

    ! X = Xbeg
    state%nodal_fields(i1:i1+nbDIME-1) = state%nodal_fields(i2:i2+nbDIME-1)
    state%nodal_fields(i1+nbDIME)      = 0.d0

    call get_nodal_field_index(i3, state, p_meca_td_V, 1)
    call get_nodal_field_index(i4, state, p_meca_td_V, 2)

    i3 = i3 + 1
    i4 = i4 + 1

    ! X = X + h*theta*V + h*(1-theta)*Vbegin for position only
    ! In fact this is a huge fraud : above formula is used for translation AND next first component.
    ! Thus in case of a 2D rigid the angle is already computed and in case of 3D rigid the component
    ! will be rewrite during the call to the function computing the rotation.
    call theta_integration(state%nodal_fields(i1:i1+nbDIME), &
                           state%nodal_fields(i4:i4+nbDIME), state%nodal_fields(i3:i3+nbDIME), dt, theta, nbDIME+1)

    if( nbDIME == 2 ) then
      omega = state%nodal_fields(i1+nbDIME)
      state%nodal_fields(i1+2) = state%nodal_fields(i2+2)*cos(omega)-state%nodal_fields(i2+3)*sin(omega)
      state%nodal_fields(i1+3) = state%nodal_fields(i2+2)*sin(omega)+state%nodal_fields(i2+3)*cos(omega)
      state%nodal_fields(i1+4) = state%nodal_fields(i2+4)*cos(omega)-state%nodal_fields(i2+5)*sin(omega)
      state%nodal_fields(i1+5) = state%nodal_fields(i2+4)*sin(omega)+state%nodal_fields(i2+5)*cos(omega)
      
    else if( nbDIME == 3 ) then
      ! rotation integration using Hughes Winget
      call update_inertia_frame33(2, dt, &
                                  state%nodal_fields(i4+3:i4+5) , &
                                  state%nodal_fields(i2+3:i2+11), &
                                  state%nodal_fields(i1+3:i1+11) )
                                             
    end if

  end subroutine

  !> \brief Add the evaluation of an integral with theta integration to a vector
  ! to inline
  subroutine theta_integration(integral, current, plus_one, delta, theta, array_size)
    implicit none
    !> [in] size of vector integral, current and plus_one
    integer(kind=4), intent(in) :: array_size
    !> [in] delta (time, space...)
    real(kind=8)   , intent(in) :: delta
    !> [in] theta
    real(kind=8)   , intent(in) :: theta
    !> [in] vector at i+1 step
    real(kind=8), dimension(array_size), intent(in)    :: plus_one
    !> [in] vector at i step
    real(kind=8), dimension(array_size), intent(in)    :: current
    !> [inout] vector in which to add the evaluated integral
    real(kind=8), dimension(array_size), intent(inout) :: integral

    integral(1:array_size) = integral(1:array_size) + delta * (1.d0-theta) * current + delta * theta * plus_one

  end subroutine

  !> \brief Compute the cc_nodal_fields map for every nodal_field needed by meca models
  subroutine meca_set_cc_nodal_fields(id, i_type, nb_nodes, cc_nodal_fields, nb_dofs_to_solve)
    implicit none
    !> [in] index of the modelization
    integer(kind=4),    intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4),    intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4),    intent(in) :: nb_nodes
    !> [in,out] map of indices between nodes and nodal field
    integer(kind=4), dimension(:,:), intent(inout) :: cc_nodal_fields
    !> [out] size of the system passed to SOE
    integer(kind=4),                 intent(out)   :: nb_dofs_to_solve
    !
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'integrator::meca_set_cc_nodal_fields'

    call set_cc_x_nodal_field(id, i_type, nb_nodes, cc_nodal_fields(1:nb_nodes+1,p_meca_td_X))
    call set_cc_meca_nodal_field(id, i_type, nb_nodes, cc_nodal_fields(1:nb_nodes+1,p_meca_td_V))
    nb_dofs_to_solve = cc_nodal_fields(nb_nodes+1,p_meca_td_V)

  end subroutine

  !> \brief Compute the cc_nodal_fields map for every nodal_field needed by ther models
  subroutine ther_set_cc_nodal_fields(id, i_type, nb_nodes, cc_nodal_fields, nb_dofs_to_solve)
    implicit none
    !> [in] index of the modelization
    integer(kind=4),    intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4),    intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4),    intent(in) :: nb_nodes
    !> [in,out] map of indices between nodes and nodal field
    integer(kind=4), dimension(:,:), intent(inout) :: cc_nodal_fields
    !> [out] size of the system passed to SOE
    integer(kind=4),                 intent(out)   :: nb_dofs_to_solve
    !
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'integrator::ther_set_cc_nodal_fields'

    call set_cc_ther_nodal_field(id, i_type, nb_nodes, cc_nodal_fields(1:nb_nodes+1,p_ther_td_T))
    nb_dofs_to_solve = cc_nodal_fields(nb_nodes+1,p_ther_td_T)

  end subroutine

  !> \brief Compute the detection configuration of a 2D rigid using leap frog
  subroutine meca_rigid_2D_leap_frog_detec(state, params, node_list, rigid_detec_config, defor_detec_config)
    !> [in,out] state on which to compute the rhs
    type(T_state),                   intent(in) :: state
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:),   intent(in) :: params
    !> [in] list of nodes on which to compute the detection configuration
    integer(kind=4), dimension(:),   intent(in) :: node_list
    !> [out] the detection configuration of the desired nodes
    real(kind=8),    dimension(:,:), allocatable :: rigid_detec_config
    !> [out] the detection configuration of the desired nodes (should not be allocated)
    real(kind=8),    dimension(:,:), allocatable :: defor_detec_config
    !
    real(kind=8), dimension(3) :: tmp
    integer(kind=4)   :: i, i1, i2, i3, i4, i5, i6
    real(kind=8)      :: dt, theta, vw_b, vw_e
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = "integrator::meca_rigid_2D_leap_frog_detec"

    dt    = params(p_time_step)
    theta = params(p_theta)

    !> todo: find where to set/store vw_b, vw_e
    vw_b = 1.d0 - theta
    vw_e = 0.

    ! compute medium configuration
    call get_nodal_field_indices_boundary(i1, i2, state, p_meca_td_X, 2)
    call get_nodal_field_indices_boundary(i3, i4, state, p_meca_td_V, 2)
    call get_nodal_field_indices_boundary(i5, i6, state, p_meca_td_V, 1)

    ! todo: paranoid check node_list = (/ 1 /)
    ! todo: paranoid check X and V field size is 3 and coor is 2

    ! detect = cooref + Xbegin + H*( (1-THETA)*Vbegin + 0*V )
    tmp(1:2) = state%nodes(1:2,1,1) + state%nodal_fields(i1:i1+1)
    tmp(3)   = 0
    tmp(1:3) = tmp(1:3) + dt*vw_b* state%nodal_fields(i3:i4) + dt*vw_e* state%nodal_fields(i5:i6) 

    ! allocation of returned array
    ! \todo check size if already associated ?
    if( .not. allocated(rigid_detec_config) ) allocate(rigid_detec_config(2,3))

    rigid_detec_config(1:2,1) = tmp(1:2)
    rigid_detec_config(1,2)   = state%nodal_fields(i1+2)*cos(tmp(3))-state%nodal_fields(i1+3)*sin(tmp(3))
    rigid_detec_config(2,2)   = state%nodal_fields(i1+2)*sin(tmp(3))+state%nodal_fields(i1+3)*cos(tmp(3))
    rigid_detec_config(1,3)   = state%nodal_fields(i1+4)*cos(tmp(3))-state%nodal_fields(i1+5)*sin(tmp(3))
    rigid_detec_config(2,3)   = state%nodal_fields(i1+4)*sin(tmp(3))+state%nodal_fields(i1+5)*cos(tmp(3))

  end subroutine

  !> \brief Compute the detection configuration of a 3D rigid using leap frog
  subroutine meca_rigid_3D_leap_frog_detec(state, params, node_list, rigid_detec_config, defor_detec_config)
    !> [in,out] state on which to compute the rhs
    type(T_state),                   intent(in) :: state
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:),   intent(in) :: params
    !> [in] list of nodes on which to compute the detection configuration
    integer(kind=4), dimension(:),   intent(in) :: node_list
    !> [out] the detection configuration of the desired nodes
    real(kind=8),    dimension(:,:), allocatable :: rigid_detec_config
    !> [out] the detection configuration of the desired nodes (should not be allocated)
    real(kind=8),    dimension(:,:), allocatable :: defor_detec_config
    !
    real(kind=8), dimension(6) :: tmp
    integer(kind=4)   :: i, i1, i2, i3, i4, i5, i6
    real(kind=8)      :: dt, theta, vw_b, vw_e
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = "integrator::meca_rigid_3D_leap_frog_detec"

    dt    = params(p_time_step)
    theta = params(p_theta)

    !> todo: find where to set/store vw_b, vw_e
    vw_b = 1.d0 - theta
    vw_e = 0.

    ! compute medium configuration
    call get_nodal_field_indices_boundary(i1, i2, state, p_meca_td_X, 2)
    call get_nodal_field_indices_boundary(i3, i4, state, p_meca_td_V, 2)
    call get_nodal_field_indices_boundary(i5, i6, state, p_meca_td_V, 1)

    ! todo: paranoid check node_list = (/ 1 /)
    ! todo: paranoid check X and V field size is 6 and coor is 3
    ! todo: check grouille dt dans update_inertia_frame33...

    ! detect = cooref + Xbegin + H*( (1-THETA)*Vbegin + 0*V )
    tmp(1:6) = state%nodal_fields(i1:i2) + dt*vw_b* state%nodal_fields(i3:i4) + dt*vw_e* state%nodal_fields(i5:i6) 

    ! allocation of returned array
    ! \todo check size if already associated ?
    if( .not. allocated(rigid_detec_config) ) allocate(rigid_detec_config(3,4))

    rigid_detec_config(1:3,1) = state%nodes(1:nbDIME,1,1) + tmp(1:3)

    ! ?
    !spin(1:3) = vw_b * state%nodal_fields(i3+nbDIME:i4) + vw_e * state%nodal_fields(i5+nbDIME:i6)

    ! rotation integration using Hughes Winget
    !call update_inertia_frame33(2, dt, spin, state%nodal_fields(i2+3:i2+11), detec_config(4:12) )
    call update_inertia_frame33(2, 1.d0, tmp(4:6), state%nodal_fields(i2+3:i2+11), rigid_detec_config(1:3,2:4) )
    
  end subroutine

  !> \brief Compute the detection configuration of a deformable using leap frog
  subroutine meca_defor_leap_frog_detec(state, params, node_list, rigid_detec_config, defor_detec_config)
    !> [in,out] state on which to compute the rhs
    type(T_state),                   intent(in) :: state
    !> [in] parameters of the integrator
    real(kind=8),    dimension(:),   intent(in) :: params
    !> [in] list of nodes on which to compute the detection configuration
    integer(kind=4), dimension(:),   intent(in) :: node_list
    !> [out] the detection configuration of the desired nodes (should not be allocated)
    real(kind=8),    dimension(:,:), allocatable :: rigid_detec_config
    !> [out] the detection configuration of the desired nodes
    real(kind=8),    dimension(:,:), allocatable :: defor_detec_config
    !
    real(kind=8), dimension(:,:), allocatable :: tmp
    integer(kind=4)   :: i, i1, i2, i3, i4, i5, i6, nb_nodes, true_shape(2)
    real(kind=8)      :: dt, theta, vw_b, vw_e
    character(len=38) :: IAM
    !      12345678901234567890123456789012345678
    IAM = "integrator::meca_defor_leap_frog_detec"

    dt    = params(p_time_step)
    theta = params(p_theta)

    !> todo: find where to set/store vw_b, vw_e
    vw_b = 1.d0 - theta
    vw_e = 0.

    ! compute medium configuration
    call get_nodal_field_indices_boundary(i1, i2, state, p_meca_td_X, 2)
    call get_nodal_field_indices_boundary(i3, i4, state, p_meca_td_V, 2)
    call get_nodal_field_indices_boundary(i5, i6, state, p_meca_td_V, 1)

    ! todo: paranoid check X, V

    nb_nodes = state%nb_nodes

    ! \todo check size if already associated ?
    if( .not. allocated(defor_detec_config) ) allocate(defor_detec_config(nbDIME,nb_nodes))

    allocate( tmp(nbDIME, nb_nodes) )

    ! \todo : paranoid check : (i2-i1+1) / nb_nodes == nbDIME

    true_shape(1) = nbDIME
    true_shape(2) = nb_nodes

    defor_detec_config = 0.d0
    defor_detec_config(1:nbDIME,node_list) = state%nodes(1:nbDIME,node_list,1)

    tmp = reshape(state%nodal_fields(i1:i2),shape=true_shape)
    defor_detec_config(1:nbDIME,node_list) = defor_detec_config(1:nbDIME,node_list) +         tmp(1:nbDIME,node_list)
    tmp = reshape(state%nodal_fields(i3:i4),shape=true_shape)
    defor_detec_config(1:nbDIME,node_list) = defor_detec_config(1:nbDIME,node_list) + dt*vw_b*tmp(1:nbDIME,node_list)
    tmp = reshape(state%nodal_fields(i5:i6),shape=true_shape)
    defor_detec_config(1:nbDIME,node_list) = defor_detec_config(1:nbDIME,node_list) + dt*vw_e*tmp(1:nbDIME,node_list)

    deallocate(tmp)

  end subroutine
                                             
  !> \brief Delete an integrator
  subroutine erase_integrator(integr)
    implicit none
    !> [in,out] the integrator to delete
    type(T_integrator), pointer :: integr

    if( associated(integr) ) then

      if( allocated(integr%nf_depths)  ) deallocate(integr%nf_depths)
      if( allocated(integr%erc_depths) ) deallocate(integr%erc_depths)
      if( allocated(integr%ef_depths)  ) deallocate(integr%ef_depths)

      if( allocated(integr%params)     ) deallocate(integr%params)
      if( allocated(integr%lhs_coeff)  ) deallocate(integr%lhs_coeff)

      nullify(integr%compute_rhs)
      nullify(integr%get_config_for_bulk_comp)

    end if
  end subroutine

  !> \brief Display an integrator
  subroutine display_integrator(integr, ifich)
    implicit none
    !> [in,out] the integrator to delete
    type(T_integrator), intent(in) :: integr
    !> [in] (optional) unit number in which to write
    integer(kind=4)   , optional   :: ifich
    !
    integer(kind=4) :: i, i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,'(A,1x,A)') 'integrator of type   :', get_integrator_type_from_id(integr%i_type)
    write(i_unit,'(A,1x,A)') 'integrator of form   :', get_formulation_type_from_id(integr%i_form)
    if( allocated(integr%params) ) then
      write(i_unit,'(A,1x)') 'integrator of params :'
      write(i_unit,'(D12.5)') integr%params
    end if
    write(i_unit,'(A,1x,I0)') 'number of nodal field:', integr%nb_nf
    if( allocated(integr%nf_depths) ) then
      write(i_unit,'(A,1x)') 'nodal fields time depths:'
      write(i_unit,'(I0)') integr%nf_depths
    end if
    write(i_unit,'(A,1x,I0)') 'number of elementary fields:', integr%nb_ef
    if( allocated(integr%ef_depths) ) then
      write(i_unit,'(A,1x)') 'elementary fields time depths:'
      write(i_unit,'(I0)') integr%ef_depths
    end if
    write(i_unit,'(A,1x,I0)') 'number of elementary rhs contributions:', integr%nb_erc
    if( allocated(integr%erc_depths) ) then
      write(i_unit,'(A,1x)') 'elementary rhs contribution time depths:'
      write(i_unit,'(I0)') integr%erc_depths
    end if
    write(i_unit,'(A,1x,I0)') 'number of matrices to store to compute left hand side matrix:', integr%nb_mats
    if( allocated(integr%lhs_coeff) ) then
      write(i_unit,'(A,1x)') 'coefficients to compute left hand side matrix:'
      write(i_unit,'(D12.5)') integr%lhs_coeff
    end if

  end subroutine

end module integrator
