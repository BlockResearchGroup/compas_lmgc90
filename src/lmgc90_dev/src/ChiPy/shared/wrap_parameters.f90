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

module wrap_parameters

  use iso_c_binding

  use parameters, only : get_physic_type_id_from_name            , &
                         get_physic_type_names                   , &
                         get_body_model_id_from_name             , &
                         get_body_model_names                    , &
                         get_contactor_id_from_name              , &
                         get_contactor_names                     , &
                         get_interaction_id_from_name            , &
                         get_interaction_names                   , &
                         get_matrix_storage_id_from_name         , &
                         get_matrix_storage_names                , &
                         get_matrix_shape_id_from_name           , &
                         get_matrix_shape_names                  , &
                         get_generalized_coordinates_id_from_name, &
                         get_generalized_coordinates_names       , &
                         get_surface_energy_status_id_from_name  , &
                         get_surface_energy_status_names         , &
                         get_inter_law_id_from_name              , &
                         get_inter_law_names                     , &
                         get_integrator_id_from_name             , &
                         get_integrator_names                    , &
                         get_node_id_from_name                   , &
                         get_node_names                          , &
                         get_dime_mode_id_from_name              , &
                         get_dime_mode_names                     , &
                         get_body_vector_id_from_name            , &
                         get_body_vector_names                   , &
                         get_contact_status_id_from_name         , &
                         get_contact_status_names                , &
                         check_all_parameters

  implicit none

  public

contains

  function getPhysicTypeId(string) bind(c, name='parameters_getPhysicTypeId')
    implicit none
    integer(c_int) :: getPhysicTypeId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getPhysicTypeId = get_physic_type_id_from_name(fstring)

  end function

  subroutine getPhysicTypeNames(string_out, vector_size, string_size) bind(c, name='parameters_getPhysicTypeNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_physic_type_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getBodyModelId(string) bind(c, name='parameters_getBodyModelId')
    implicit none
    integer(c_int) :: getBodyModelId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getBodyModelId = get_body_model_id_from_name(fstring)

  end function

  subroutine getBodyModelNames(string_out, vector_size, string_size) bind(c, name='parameters_getBodyModelNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_body_model_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getContactorId(string) bind(c, name='parameters_getContactorId')
    implicit none
    integer(c_int) :: getContactorId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getContactorId = get_contactor_id_from_name(fstring)

  end function

  subroutine getContactorNames(string_out, vector_size, string_size) bind(c, name='parameters_getContactorNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_contactor_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getInteractionId(string) bind(c, name='parameters_getInteractionId')
    implicit none
    integer(c_int) :: getInteractionId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getInteractionId = get_interaction_id_from_name(fstring)

  end function

  subroutine getInteractionNames(string_out, vector_size, string_size) bind(c, name='parameters_getInteractionNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_interaction_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getMatrixStorageId(string) bind(c, name='parameters_getMatrixStorageId')
    implicit none
    integer(c_int) :: getMatrixStorageId
    type(c_ptr)   , value :: string
    !
    character(len=8), pointer :: fstring

    call c_f_pointer(string, fstring)

    getMatrixStorageId = get_matrix_storage_id_from_name(fstring)

  end function

  subroutine getMatrixStorageNames(string_out, vector_size, string_size) bind(c, name='parameters_getMatrixStorageNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=8), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 8

    names => get_matrix_storage_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getMatrixShapeId(string) bind(c, name='parameters_getMatrixShapeId')
    implicit none
    integer(c_int) :: getMatrixShapeId
    type(c_ptr)   , value :: string
    !
    character(len=8), pointer :: fstring

    call c_f_pointer(string, fstring)

    getMatrixShapeId = get_matrix_shape_id_from_name(fstring)

  end function

  subroutine getMatrixShapeNames(string_out, vector_size, string_size) bind(c, name='parameters_getMatrixShapeNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=8), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 8

    names => get_matrix_shape_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getGeneralizedCoordinatesId(string) bind(c, name='parameters_getGeneralizedCoordinatesId')
    implicit none
    integer(c_int) :: getGeneralizedCoordinatesId
    type(c_ptr)   , value :: string
    !
    character(len=15), pointer :: fstring

    call c_f_pointer(string, fstring)

    getGeneralizedCoordinatesId = get_generalized_coordinates_id_from_name(fstring)

  end function

  subroutine getGeneralizedCoordinatesNames(string_out, vector_size, string_size) bind(c, name='parameters_getGeneralizedCoordinatesNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=15), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 15

    names => get_generalized_coordinates_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getSurfaceEnergyStatusId(string) bind(c, name='parameters_getSurfaceEnergyStatusId')
    implicit none
    integer(c_int) :: getSurfaceEnergyStatusId
    type(c_ptr)   , value :: string
    !
    character(len=10), pointer :: fstring

    call c_f_pointer(string, fstring)

    getSurfaceEnergyStatusId = get_surface_energy_status_id_from_name(fstring)

  end function

  subroutine getSurfaceEnergyStatusNames(string_out, vector_size, string_size) bind(c, name='parameters_getSurfaceEnergyStatusNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=10), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 10

    names => get_surface_energy_status_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getInterLawId(string) bind(c, name='parameters_getInterLawId')
    implicit none
    integer(c_int) :: getInterLawId
    type(c_ptr)   , value :: string
    !
    character(len=24), pointer :: fstring

    call c_f_pointer(string, fstring)

    getInterLawId = get_inter_law_id_from_name(fstring)

  end function

  subroutine getInterLawNames(string_out, vector_size, string_size) bind(c, name='parameters_getInterLawNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=24), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 24

    names => get_inter_law_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getIntegratorId(string) bind(c, name='parameters_getIntegratorId')
    implicit none
    integer(c_int) :: getIntegratorId
    type(c_ptr)   , value :: string
    !
    character(len=7), pointer :: fstring

    call c_f_pointer(string, fstring)

    getIntegratorId = get_integrator_id_from_name(fstring)

  end function

  subroutine getIntegratorNames(string_out, vector_size, string_size) bind(c, name='parameters_getIntegratorNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=7), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 7

    names => get_integrator_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getNodeId(string) bind(c, name='parameters_getNodeId')
    implicit none
    integer(c_int) :: getNodeId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getNodeId = get_node_id_from_name(fstring)

  end function

  subroutine getNodeNames(string_out, vector_size, string_size) bind(c, name='parameters_getNodeNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_node_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getDimeModeId(string) bind(c, name='parameters_getDimeModeId')
    implicit none
    integer(c_int) :: getDimeModeId
    type(c_ptr)   , value :: string
    !
    character(len=10), pointer :: fstring

    call c_f_pointer(string, fstring)

    getDimeModeId = get_dime_mode_id_from_name(fstring)

  end function

  subroutine getDimeModeNames(string_out, vector_size, string_size) bind(c, name='parameters_getDimeModeNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=10), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 10

    names => get_dime_mode_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getBodyVectorId(string) bind(c, name='parameters_getBodyVectorId')
    implicit none
    integer(c_int) :: getBodyVectorId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getBodyVectorId = get_body_vector_id_from_name(fstring)

  end function

  subroutine getBodyVectorNames(string_out, vector_size, string_size) bind(c, name='parameters_getBodyVectorNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_body_vector_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  function getContactStatusId(string) bind(c, name='parameters_getContactStatusId')
    implicit none
    integer(c_int) :: getContactStatusId
    type(c_ptr)   , value :: string
    !
    character(len=5), pointer :: fstring

    call c_f_pointer(string, fstring)

    getContactStatusId = get_contact_status_id_from_name(fstring)

  end function

  subroutine getContactStatusNames(string_out, vector_size, string_size) bind(c, name='parameters_getContactStatusNames')
    implicit none
    type(c_ptr)    :: string_out
    integer(c_int) :: vector_size, string_size
    !
    character(len=5), dimension(:), pointer :: names

    string_out  = c_null_ptr
    vector_size = 0
    string_size = 5

    names => get_contact_status_names()

    if( associated(names) ) then
      vector_size = size(names)
      string_out  = c_loc(names(1))
    end if

  end subroutine

  !-------------------------------------------------------------------------------------------------------------!

  subroutine checkAll() bind(c, name='parameters_checkAll')
    implicit none

    call check_all_parameters()

  end subroutine

end module
