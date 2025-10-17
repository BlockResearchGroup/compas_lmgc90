!> LMGC90 python wrap of model_handler
!> \author F. Dubois
!> \date   January 2010
!>

module wrap_integrator

  use ISO_C_BINDING

  use integrator_parameters, only : get_formulation_type_from_id, &
                                    get_formulation_id_from_type, &
                                    get_parameter_type_from_id  , &
                                    get_parameter_id_from_type

  use integrator_list, only : add_integrator, &
                              set_parameter , &
                              get_parameter , &
                              clean_module

  implicit none

  public addIntegrator, &
         setParameter , &
         getParameter , &
         cleanMemory

  contains

  subroutine addIntegrator(cvalue_c, cvalue2_c, cvalue3_c, params, p_size) bind(C, name='integrator_addIntegrator')
    implicit none
    character(c_char), dimension(7), intent(in) :: cvalue_c
    character(c_char), dimension(4), intent(in) :: cvalue2_c
    character(c_char), dimension(5), intent(in) :: cvalue3_c
    integer(c_int), intent(in), value :: p_size
    real(c_double), intent(in)        :: params(p_size)
    !
    character(len=7) :: cvalue
    character(len=4) :: cvalue2
    character(len=5) :: cvalue3
    integer(kind=4)  :: i

    cvalue = ''
    do i=1,7
      if (cvalue_c(i) == C_NULL_CHAR) exit
      cvalue = cvalue(1:i-1) // cvalue_c(i)
    end do

    cvalue2 = ''
    do i=1,4
      if (cvalue2_c(i) == C_NULL_CHAR) exit
      cvalue2 = cvalue2(1:i-1) // cvalue2_c(i)
    end do

    cvalue3 = ''
    do i=1,5
      if (cvalue3_c(i) == C_NULL_CHAR) exit
      cvalue3 = cvalue3(1:i-1) // cvalue3_c(i)
    end do

    call add_integrator(cvalue, cvalue2, cvalue3, params)

  end subroutine

  subroutine setParameter(c_form, c_name, val) bind(c, name='integrator_setParameter')
    implicit none
    character(c_char), dimension(10), intent(in) :: c_form
    character(c_char), dimension(9) , intent(in) :: c_name
    real(c_double)   , intent(in)   , value      :: val
    !
    integer(kind=4)   :: i, i_form, i_param
    character(len=10) :: formulation
    character(len=9)  :: param_name

    formulation = ''
    do i=1,10
      if (c_form(i) == C_NULL_CHAR) exit
      formulation = formulation(1:i-1) // c_form(i)
    end do

    param_name = ''
    do i=1,9
      if (c_name(i) == C_NULL_CHAR) exit
      param_name = param_name(1:i-1) // c_name(i)
    end do

    i_form  = get_formulation_id_from_type(formulation)
    ! \todo: check param_name and formulation consistency
    i_param = get_parameter_id_from_type(param_name)

    call set_parameter(i_form, i_param, val)

  end subroutine

  function getParameter(c_form, c_name) bind(c, name='integrator_getParameter')
    implicit none
    character(c_char), dimension(10), intent(in) :: c_form
    character(c_char), dimension(9) , intent(in) :: c_name
    real(c_double)                               :: getParameter
    !
    integer(kind=4)   :: i, i_form, i_param
    character(len=10) :: formulation
    character(len=9)  :: param_name

    formulation = ''
    do i=1,10
      if (c_form(i) == C_NULL_CHAR) exit
      formulation = formulation(1:i-1) // c_form(i)
    end do

    param_name = ''
    do i=1,9
      if (c_name(i) == C_NULL_CHAR) exit
      param_name = param_name(1:i-1) // c_name(i)
    end do

    i_form  = get_formulation_id_from_type(formulation)
    ! \todo: check param_name and formulation consistency
    i_param = get_parameter_id_from_type(param_name)

    getParameter = get_parameter(i_form, i_param)

  end function

  subroutine cleanMemory() bind(c, name='integrator_cleanMemory')
    implicit none

    call clean_module
  end subroutine

end module
