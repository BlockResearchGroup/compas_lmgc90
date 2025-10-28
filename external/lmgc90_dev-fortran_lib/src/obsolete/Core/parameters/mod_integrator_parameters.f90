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

!>  This module contains mapping between names and integer (parameter) identifier for integratros
module integrator_parameters

  use utilities

  public

  ! map between integrator type and integer identifier
  integer(kind=4), parameter :: p_moreau  = 1
  !integer(kind=4), parameter :: i_gear    = i_moreau + 1
  !integer(kind=4), parameter :: i_verlet  = i_gear   + 1
  !integer(kind=4), parameter :: i_newmark = i_verlet + 1

  ! map between formulation and integer identifier
  integer(kind=4), parameter :: p_meca_rigid = 1
  integer(kind=4), parameter :: p_meca_defor = p_meca_rigid + 1
  integer(kind=4), parameter :: p_ther_defor = p_meca_defor + 1

  ! Number of fields for each model and field type
  integer(kind=4), parameter :: p_meca_nb_nodal_fields      = 2 ! X, V
  integer(kind=4), parameter :: p_meca_nb_el_rhs_contribs   = 2 ! Fint, Fext
  integer(kind=4), parameter :: p_meca_nb_elementary_fields = 1 ! fields

  integer(kind=4), parameter :: p_ther_nb_nodal_fields      = 1 ! T
  integer(kind=4), parameter :: p_ther_nb_el_rhs_contribs   = 2 ! Fint, Fext
  integer(kind=4), parameter :: p_ther_nb_elementary_fields = 1 ! fields

  ! Indices of field for each type in time depht arrays
  integer(kind=4), parameter :: p_meca_td_X     = 1
  integer(kind=4), parameter :: p_meca_td_V     = 2
  integer(kind=4), parameter :: p_meca_td_Fint  = 1
  integer(kind=4), parameter :: p_meca_td_Fext  = 2
  integer(kind=4), parameter :: p_meca_td_field = 1

  integer(kind=4), parameter :: p_ther_td_T     = 1
  integer(kind=4), parameter :: p_ther_td_Fint  = 1
  integer(kind=4), parameter :: p_ther_td_Fext  = 2
  integer(kind=4), parameter :: p_ther_td_field = 1

  ! Number of matrices for a model
  integer(kind=4), parameter :: p_meca_rigid_nb_mats = 1
  integer(kind=4), parameter :: p_meca_defor_nb_mats = 3

  integer(kind=4), parameter :: p_ther_rigid_nb_mats = 0
  integer(kind=4), parameter :: p_ther_defor_nb_mats = 2

  integer(kind=4), parameter :: p_mass_mat_index = 1
  integer(kind=4), parameter :: p_visc_mat_index = 2
  integer(kind=4), parameter :: p_stif_mat_index = 3

  integer(kind=4), parameter :: p_capa_mat_index = 1
  integer(kind=4), parameter :: p_cond_mat_index = 2

  ! Index of real data in an integrator
  integer(kind=4), parameter :: p_time_step = 1
  integer(kind=4), parameter :: p_theta     = 2

  public get_integrator_id_from_type , &
         get_integrator_type_from_id , &
         get_formulation_id_from_type, &
         get_formulation_type_from_id, &
         get_parameter_id_from_type  , &
         get_parameter_type_from_id

  contains

  !> \brief Get the integrator type integer identifier
  !> Possible values for integrator type are 'moreau',
  !> 'gear', 'verlet' and 'newmark'
  function get_integrator_id_from_type(integrator_type)
    implicit none
    character(len=7), intent(in) :: integrator_type             !< [in] type of integrator
    integer(kind=4)              :: get_integrator_id_from_type !< [return] integer identifier
    !
    character(len=80) :: cout
    character(len=49) :: IAM
    !      1234567890123456789012345678901234567890123456789
    IAM = 'integrator_parameter::get_integrator_id_from_type'

    select case(integrator_type)
    case( 'moreau' )
      get_integrator_id_from_type = p_moreau
    !case( 'gear' )
    !  get_integrator_id_from_type = i_gear
    !case( 'verlet' )
    !  get_integrator_id_from_type = i_verlet
    !case( 'newmark' )
    !  get_integrator_id_from_type = i_newmark
    case default
      write(cout, '(A,1x,A)') 'unknown type:', integrator_type
      call faterr(IAM,cout)
    end select

  end function

  !> \brief Get the integrator type from its integer identifier
  function get_integrator_type_from_id(integrator_id)
    implicit none
    integer(kind=4), intent(in) :: integrator_id               !< [in] integer identifier
    character(len=7)            :: get_integrator_type_from_id !< [return] type of integrator
    !
    character(len=80) :: cout
    character(len=49) :: IAM
    !      1234567890123456789012345678901234567890123456789
    IAM = 'integrator_parameter::get_integrator_type_from_id'

    select case(integrator_id)
    case( p_moreau )
      get_integrator_type_from_id = 'moreau'
    !case( i_gear )
    !  get_integrator_type_from_id = 'gear'
    !case( i_verlet )
    !  get_integrator_type_from_id = 'verlet'
    !case( i_newmark )
    !  get_integrator_type_from_id = 'newmark'
    case default
      write(cout, '(A,1x,A)') 'unknown id:', integrator_id
      call faterr(IAM,cout)
    end select

  end function

  !> \brief Get the formulation type integer identifier
  !> Possible values for formulation are 'meca_rigid',
  !> 'meca_defor' and 'ther_defor'
  function get_formulation_id_from_type(formulation_type)
    implicit none
    character(len=10), intent(in) :: formulation_type             !< [in] type of formulation
    integer(kind=4)               :: get_formulation_id_from_type !< [return] integer identifier
    !
    character(len=80) :: cout
    character(len=50) :: IAM
    !      12345678901234567890123456789012345678901234567890
    IAM = 'integrator_parameter::get_formulation_id_from_type'

    select case(formulation_type)
    case( 'meca_rigid' )
      get_formulation_id_from_type = p_meca_rigid
    case( 'meca_defor' )
      get_formulation_id_from_type = p_meca_defor
    case( 'ther_defor' )
      get_formulation_id_from_type = p_ther_defor
    case default
      write(cout, '(A,1x,A)') 'unknown type:', formulation_type
      call faterr(IAM,cout)
    end select

  end function

  !> \brief Get the formulation type from its integer identifier
  function get_formulation_type_from_id(formulation_id)
    implicit none
    integer(kind=4), intent(in) :: formulation_id               !< [in] integer identifier
    character(len=10)           :: get_formulation_type_from_id !< [return] type of formulation
    !
    character(len=80) :: cout
    character(len=50) :: IAM
    !      12345678901234567890123456789012345678901234567890
    IAM = 'integrator_parameter::get_formulation_type_from_id'

    select case(formulation_id)
    case( p_meca_rigid )
      get_formulation_type_from_id = 'meca_rigid'
    case( p_meca_defor )
      get_formulation_type_from_id = 'meca_defor'
    case( p_ther_defor )
      get_formulation_type_from_id = 'ther_defor'
    case default
      write(cout, '(A,1x,A)') 'unknown id:', formulation_id
      call faterr(IAM,cout)
    end select

  end function

  !> \brief Get the integrator parameter integer identifier
  function get_parameter_id_from_type(parameter_type)
    implicit none
    character(len=9), intent(in) :: parameter_type             !< [in] parameter name
    integer(kind=4)              :: get_parameter_id_from_type !< [return] integer identifier
    !
    character(len=80) :: cout
    character(len=50) :: IAM
    !      12345678901234567890123456789012345678901234567890
    IAM = 'integrator_parameter::get_parameter_id_from_type'

    select case(trim(parameter_type))
    case( 'time step' )
      get_parameter_id_from_type = p_time_step
    case( 'theta' )
      get_parameter_id_from_type = p_theta
    case default
      write(cout, '(A,1x,A)') 'unknown parameter:', parameter_type
      call faterr(IAM,cout)
    end select

  end function

  !> \brief Get the parameter type from its integer identifier
  function get_parameter_type_from_id(parameter_id)
    implicit none
    integer(kind=4), intent(in) :: parameter_id               !< [in] integer identifier
    character(len=9)            :: get_parameter_type_from_id !< [return] type of parameter
    !
    character(len=80) :: cout
    character(len=48) :: IAM
    !      123456789012345678901234567890123456789012345678
    IAM = 'integrator_parameter::get_parameter_type_from_id'

    select case(parameter_id)
    case( p_time_step)
      get_parameter_type_from_id = 'time_step'
    case( p_theta )
      get_parameter_type_from_id = 'theta'
    case default
      write(cout, '(A,1x,A)') 'unknown id:', parameter_id
      call faterr(IAM,cout)
    end select

  end function

end module integrator_parameters

