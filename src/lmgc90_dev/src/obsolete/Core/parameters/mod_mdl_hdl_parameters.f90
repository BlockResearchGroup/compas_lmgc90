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

!>  This module contains mapping between names and integer (parameter) identifier for model_handler
module mdl_hdl_parameters

  use utilities

  use integrator_parameters

  public

  ! map between modelization type and an integer identifier
  integer(kind=4), parameter :: p_rigid2D   = 1
  integer(kind=4), parameter :: p_rigid3D   = p_rigid2D + 1
  integer(kind=4), parameter :: p_MAILx     = p_rigid3D + 1
  integer(kind=4), parameter :: p_mecaMAILx = p_MAILx   + 1
  integer(kind=4), parameter :: p_therMAILx = p_mecaMAILx + 1

  ! map between type of coordinates and integer identifier
  integer(kind=4), parameter :: p_coor_ref    = 1
  integer(kind=4), parameter :: p_coor_detec  = p_coor_ref   + 1
  integer(kind=4), parameter :: p_coor_begin  = p_coor_detec + 1
  integer(kind=4), parameter :: p_coor_       = p_coor_begin + 1

  public get_model_id_from_type, &
         get_model_type_from_id, &
         is_model_meca         , &
         is_model_ther         , &
         get_coor_id_from_type , &
         get_coor_type_from_id , &
         get_nf_id_from_type   , &
         get_nf_type_from_id   , &
         get_erc_id_from_type  , &
         get_erc_type_from_id

  contains

  ! Note about calls to LOGMES or FATERR :
  !
  ! The functions doing the map between a string and an integer never call FATERR
  ! Only log message are printed and 0 is returned, if a call to FATERR is needed
  ! is decided outside the function
  !
  ! It allows to call the getter/setter in python interpreter and allows mistakes
  ! or tests instead of systematically killing the program

  !> \brief Get the id of a type of model
  function get_model_id_from_type(type)
    implicit none
    character(len=*), intent(in) :: type                   !< [in] model type (RBDY2, RBDY3, MAILx, mecaMAILx or therMAILx)
    integer(kind=4)              :: get_model_id_from_type !< [return] integer id
    !
    character(len=80) :: cout

    get_model_id_from_type = 0

    select case (type)
    case('RBDY2')
      get_model_id_from_type = p_rigid2D
    case('RBDY3')
      get_model_id_from_type = p_rigid3D
    case('MAILx')
      get_model_id_from_type = p_MAILx
    case('mecaMAILx')
      get_model_id_from_type = p_mecaMAILx
    case('therMAILx')
      get_model_id_from_type = p_therMAILx
    case default
      write(cout, '(A,1x,A)') '[mdl_hdl_parameters::get_model_id_from_type] Unkown model type:', type
      call logmes(cout)
      call logmes("Available model types are : 'RBDY2', 'RBDY3', 'MAILx', 'mecaMAILx' and 'therMAILx'")
    end select

  end function

  !> \brief the model type from its integer id
  function get_model_type_from_id(id)
    implicit none
    integer(kind=4), intent(in) :: id                     !< [in] model type integer id
    character(len=9)            :: get_model_type_from_id !< [return] model type
    !
    character(len=80) :: cout

    get_model_type_from_id = ''

    select case (id)
    case(p_rigid2D)
      get_model_type_from_id = 'RBDY2'
    case(p_rigid3D)
      get_model_type_from_id = 'RBDY3'
    case(p_MAILx)
      get_model_type_from_id = 'MAILx'
    case(p_mecaMAILx)
      get_model_type_from_id = 'mecaMAILx'
    case(p_therMAILx)
      get_model_type_from_id = 'therMAILx'
    case default
      write(cout, '(A,1x,I0)') '[mdl_hdl_parameters::get_model_type_from_id] Unkown model type id:', id
      call logmes(cout)
    end select

  end function

  !> \brief Inform if the model type id is a mechanical model type
  function is_model_meca(type)
    implicit none
    integer(kind=4), intent(in) :: type          !< [in] model type integer identifier
    logical                     :: is_model_meca !< [return] true if type belongs to a mechanical type

    is_model_meca = (type==p_rigid2D) .or. &
                    (type==p_rigid3D) .or. &
                    (type==p_mecaMAILx)
  end function

  !> \brief Inform if the model type id is a thermal model type
  function is_model_ther(type)
    implicit none
    integer(kind=4), intent(in) :: type          !< [in] model type integer identifier
    logical                     :: is_model_ther !< [return] true if type belongs to a thermal model

    is_model_ther = (type==p_therMAILx)
  end function

  !> \brief Get the id of a coordinates field 
  function get_coor_id_from_type(model_type, coor_type)
    implicit none
    integer(kind=4) , intent(in) :: model_type            !< [in] model type id
    character(len=*), intent(in) :: coor_type             !< [in] coordinates field type (ref, begin, detec, now)
    integer(kind=4)              :: get_coor_id_from_type !< [return] integer id
    !
    character(len=120) :: cout
    character(len=41)  :: IAM
    !      12345678901234567890123456789012345678901
    IAM = 'mdl_hdl_parameters::get_coor_id_from_type'

    get_coor_id_from_type = 0

    select case(coor_type)
    case('ref')
      get_coor_id_from_type = p_coor_ref
    case('detec')
      if( is_model_meca(model_type) ) then
        get_coor_id_from_type = p_coor_detec
      else
        write(cout, '(A,A,A)') '[',IAM,'] Cannot get detection coordinates of a non mechanical model'
        call logmes(cout)
      end if
    case('begin')
      if( is_model_meca(model_type) ) then
        get_coor_id_from_type = p_coor_begin
      else
        write(cout, '(A,A,A)') '[',IAM,'] Cannot get beginning coordinates of a non mechanical model'
        call logmes(cout)
      end if
    case('now')
      if( is_model_meca(model_type) ) then
        get_coor_id_from_type = p_coor_
      else
        write(cout, '(A,A,A)') '[',IAM,'] Cannot get current coordinates of a non mechanical model'
        call logmes(cout)
      end if
    case default
      write(cout,'(A,A,A,1x,A)') '[',IAM,'] Unknown coordinates field type:', coor_type
      call logmes(cout)
      write(cout,'(A)'),"Available coordinates field types are : 'ref', 'begin', 'detec' or 'now'"
      call logmes(cout)
    end select

  end function

  !> \brief Get the type of a coordinates field from its integer id
  function get_coor_type_from_id(id)
    implicit none
    integer(kind=4), intent(in) :: id                    !< [in] integer id
    character(len=5)            :: get_coor_type_from_id !< [return] coordinates field type (ref, begin, detec, now)
    !
    character(len=80) :: cout

    get_coor_type_from_id = ''

    select case(id)
    case(p_coor_ref)
      get_coor_type_from_id = 'ref'
    case(p_coor_detec)
      get_coor_type_from_id = 'detec'
    case(p_coor_begin)
      get_coor_type_from_id = 'begin'
    case(p_coor_)
      get_coor_type_from_id = 'now'
    case default
      write(cout,'(A,1x,I0)') '[mdl_hdl_paramters::get_coor_type_from_id] Unknown coordinates type id :', id
      call logmes(cout)
    end select

  end function

  !> \brief Get the id of a nodal field type
  function get_nf_id_from_type(model_type, nf_type)
    implicit none
    integer(kind=4),  intent(in) :: model_type          !< [in] integer id of the type of model
    character(len=*), intent(in) :: nf_type             !< [in] nodal field type (displacement, velocity, temperature)
    integer(kind=4)              :: get_nf_id_from_type !< [return] integer id of the nodal field
    !
    character(len=120) :: cout
    character(len=39)  :: IAM
    !      123456789012345678901234567890123456789
    IAM = 'mdl_hdl_parameters::get_nf_id_from_type'

    get_nf_id_from_type = 0

    select case(nf_type)
    case('displacement')
      if( is_model_meca(model_type) ) then
        get_nf_id_from_type = p_meca_td_X
      else
        write(cout,'(A,A,A)') '[',IAM,'] Cannot get displacement nodal field for a non mechanical model'
        call logmes(cout)
      end if
    case('velocity')
      if( is_model_meca(model_type) ) then
        get_nf_id_from_type = p_meca_td_V
      else
        write(cout,'(A,A,A)') '[',IAM,'] Cannot get velocity nodal field for a non mechanical model'
        call logmes(cout)
      end if
    case('temperature')
      if( is_model_ther(model_type) ) then
        get_nf_id_from_type = p_ther_td_T
      else
        write(cout, '(A,A,A)') '[',IAM,'] Cannot get temperature nodal field for a non thermal model'
        call logmes(cout)
      end if
    case default
      write(cout,'(A,A,A,1x,A)') '[',IAM,'] Unknown nodal field type:', nf_type
      call logmes(cout)
      write(cout,'(A)'),"Available nodal field types are : 'displacement', 'velocity' or 'temperature'"
      call logmes(cout)
    end select

  end function

  !> \brief Get the type of a nodal field from its integer id
  function get_nf_type_from_id(model_id, nf_id)
    implicit none
    integer(kind=4), intent(in) :: model_id            !< [in] integer id of the model
    integer(kind=4), intent(in) :: nf_id               !< [in] integer id of the nodal field
    character(len=12)           :: get_nf_type_from_id !< [return] nodal field name
    !
    character(len=120) :: cout

    get_nf_type_from_id = ''

    if( is_model_meca(model_id) ) then
      select case( nf_id )
      case(p_meca_td_X)
        get_nf_type_from_id = 'displacement'
        return
      case(p_meca_td_V)
        get_nf_type_from_id = 'velocity'
        return
      end select
    end if

    if( is_model_ther(model_id) ) then
      select case( nf_id )
      case(p_ther_td_T)
        get_nf_type_from_id = 'temperature'
        return
      end select
    end if

    write(cout,'(A,1x,I0,1x,A,1x,A)') 'Unable to match nodal field id:', nf_id, &
                                      'with model type:', get_model_type_from_id(model_id)
    call logmes(cout)
  
  end function

  !> \brief Get the id of a elementary rhs contribution type
  function get_erc_id_from_type(model_type, erc_type)
    implicit none
    integer(kind=4),  intent(in) :: model_type           !< [in] integer id of the type of model
    character(len=*), intent(in) :: erc_type             !< [in] elementary rhs contribution type
    integer(kind=4)              :: get_erc_id_from_type !< [return] integer id of the erc
    !
    character(len=120) :: cout
    character(len=40)  :: IAM
    !      1234567890123456789012345678901234567890
    IAM = 'mdl_hdl_parameters::get_erc_id_from_type'

    get_erc_id_from_type = 0

    select case(erc_type)
    case('Fext')
      if( is_model_meca(model_type) ) then
        get_erc_id_from_type = p_meca_td_Fext
      else if( is_model_ther(model_type) ) then
        get_erc_id_from_type = p_ther_td_Fext
      else
        write(cout,'(A,A,A)') '[',IAM,'] Cannot get Fext elementary rhs contribution'
        call logmes(cout)
      end if
    case('Fint')
      if( is_model_meca(model_type) ) then
        get_erc_id_from_type = p_meca_td_Fint
      else if( is_model_ther(model_type) ) then
        get_erc_id_from_type = p_ther_td_Fint
      else
        write(cout,'(A,A,A)') '[',IAM,'] Cannot get Fint elementary rhs contribution'
        call logmes(cout)
      end if
    case default
      write(cout,'(A,A,A,1x,A)') '[',IAM,'] Unknown elementary rhs contribution type:', erc_type
      call logmes(cout)
      write(cout,'(A)'),"Available nodal field types are : 'Fext' or 'Fint'"
      call logmes(cout)
    end select

  end function

  !> \brief Get the type of a elementary rhs contribution from its integer id
  function get_erc_type_from_id(model_id, erc_id)
    implicit none
    integer(kind=4), intent(in) :: model_id             !< [in] integer id of the model
    integer(kind=4), intent(in) :: erc_id               !< [in] integer id of the elementary rhs contribution
    character(len=12)           :: get_erc_type_from_id !< [return] elementary rhs contribution name
    !
    character(len=120) :: cout

    get_erc_type_from_id = ''

    if( is_model_meca(model_id) ) then
      select case( erc_id )
      case(p_meca_td_Fint)
        get_erc_type_from_id = 'Fint'
        return
      case(p_meca_td_Fext)
        get_erc_type_from_id = 'Fext'
        return
      end select
    end if

    if( is_model_ther(model_id) ) then
      select case( erc_id )
      case(p_ther_td_Fint)
        get_erc_type_from_id = 'Fint'
        return
      case(p_ther_td_Fext)
        get_erc_type_from_id = 'Fext'
        return
      end select
    end if

    write(cout,'(A,1x,I0,1x,A,1x,A)') 'Unable to match elementary rhs contribution id:', erc_id, &
                                      'with model type:', get_model_type_from_id(model_id)
    call logmes(cout)
  
  end function

end module mdl_hdl_parameters

