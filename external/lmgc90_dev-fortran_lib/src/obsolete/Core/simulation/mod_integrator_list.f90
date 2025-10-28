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
module integrator_list

  use integrator_parameters
  use integrator, only : list_data    => T_integrator    , &
                         new_mechanical_integrator       , &
                         new_thermal_integrator          , &
                         initialize_meca_rigid_integrator, &
                         initialize_meca_defor_integrator, &
                         initialize_ther_defor_integrator, &
                         erase_data   => erase_integrator, &
                         display_data => display_integrator!, &

  implicit none

  private

  include 'linkedlist_type.f90'

  type(LINKED_LIST), pointer :: integrators => null()

  public add_integrator           , &
         initialize_integrators   , &
         get_integrator           , &
         set_parameter            , &
         get_parameter            , &
         clean_module

  contains

  include 'linkedlist_methods.f90'

  !> \brief Add a new integrator
  !> Possible values for integrator_type should be: 
  !> 'moreau', 'gear', 'verlet' or 'newmark'
  !> Possible values for model_type are: 
  !> 'meca', 'ther'
  !> Possible values for formulation_type are: 
  !> 'rigid', 'defor'
  !> params is a real vector defined accordingly to
  !> integrator_type and formulation values
  subroutine add_integrator(integrator_type, model_type, formulation_type, params)
    implicit none
    !> [in] type of integrator ('moreau', 'gear', 'verlet' or 'newmark')
    character(len=7),           intent(in) :: integrator_type
    !> [in] type of model ('meca' or 'ther')
    character(len=4),           intent(in) :: model_type
    !> [in] type of formulation ('rigid' or 'defor')
    character(len=5),           intent(in) :: formulation_type
    !> [in] list or reals parameters needed by the integrator
    real(kind=8), dimension(:), intent(in) :: params
    !
    type(list_data), pointer :: new_integrator
    character(len=80) :: cout
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'integrator_list::add_new_integrator'

    if( model_type == 'meca' ) then
      new_integrator => new_mechanical_integrator(integrator_type, formulation_type, params)
    else if( model_type == 'ther' ) then
      new_integrator => new_thermal_integrator(integrator_type, formulation_type, params)
    else
      write(cout, '(A,1x,A4)') 'unknown model type:', model_type
      call faterr(IAM,cout)
    end if

    if( associated(integrators) ) then
      call list_insert( integrators, new_integrator )
    else
      call list_create( integrators, new_integrator )
    end if
    
  end subroutine


  !> \brief Initialize all integrator in the list
  subroutine initialize_integrators()
    implicit none
    type(linked_list), pointer :: current
    type(list_data)  , pointer :: integr

    current => null()
    integr  => null()

    if( associated(integrators) ) then
    
      current => integrators
      do while( associated(current) )
        integr  => list_get_data(current)
        if( associated(integr) ) then
          select case( integr%i_form )
          case(p_meca_rigid)
            call initialize_meca_rigid_integrator(integr)
          case(p_meca_defor)
            call initialize_meca_defor_integrator(integr)
          case(p_ther_defor)
            call initialize_ther_defor_integrator(integr)
          end select
        end if
        current => list_next(current)
      end do

    end if

  end subroutine

  !> \brief Get the integrator of desired formulation
  function get_integrator(i_form)
    implicit none
    !> [in] formulation id of the desired integrator
    integer(kind=4), intent(in) :: i_form
    !> [return] integrator object 
    type(list_data), pointer    :: get_integrator
    !
    type(list_data)  , pointer :: integr
    type(linked_list), pointer :: current

    get_integrator => null()

    current => null()
    integr  => null()

    if( associated(integrators) ) then
    
      current => integrators
      do while( associated(current) )
        integr  => list_get_data(current)
        if( associated(integr) ) then
          if( integr%i_form == i_form ) then
            get_integrator => integr
            return
          end if
        end if
        current => list_next(current)
      end do

    end if

  end function

  !> \brief Set a parameter of the integrator of desired formulation
  subroutine set_parameter(i_form, i_param, val)
    implicit none
    !> [in] id of the formulation of the desired integrator
    integer(kind=4), intent(in) :: i_form
    !> [in] id of the parameter to set
    integer(kind=4), intent(in) :: i_param
    !> [in] new value of the parameter
    real(kind=8),    intent(in) :: val
    !
    type(list_data), pointer :: integr

    integr => get_integrator(i_form)

    ! check integr association ?
    integr%params(i_param) = val

    !!write(cout,'(A,A,A)')   '['//IAM//'] setting parameter "', param_name, '"'
    !!write(cout,'(A,A,A)')   'of integrator of formulation "' formulation, '"'
    !!write(cout,'(A,1x,E0)') 'with value:', new_val
    !!
    !!if( i_param < 1 .or. i_param > size(integrator%params) then
    !!  write(cout,'(A,1x,I0,1x,A,I0,1x,I0,A)') 'parameter index:', i_param, &
    !!                                          'out of range (',1,size(integrator%params),')'
    !!  call faterr(IAM,cout)
    !!end if

  end subroutine

  !> \brief Get a parameter of the integrator of desired formulation
  function get_parameter(i_form, i_param)
    implicit none
    !> [in] id of the formulation of the desired integrator
    integer(kind=4), intent(in) :: i_form
    !> [in] id of the parameter to get
    integer(kind=4), intent(in) :: i_param
    !> [return] value of the parameter
    real(kind=8)                :: get_parameter
    !
    type(list_data), pointer :: integr

    integr => get_integrator(i_form)

    ! check integr association ?
    get_parameter = integr%params(i_param)

  end function

  !> \brief Free memory allocated within the module
  subroutine clean_module()
    implicit none

    call list_destroy(integrators)

  end subroutine

end module integrator_list
