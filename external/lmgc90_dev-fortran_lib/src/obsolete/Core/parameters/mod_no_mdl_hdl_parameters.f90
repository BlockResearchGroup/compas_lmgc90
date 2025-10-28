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

!>  This module contains empty value to be used by post3D when not using new arch
module mdl_hdl_parameters

  public

  ! map between type of coordinates and integer identifier
  integer(kind=4), parameter :: p_coor_ref    = 0
  integer(kind=4), parameter :: p_coor_begin  = 0

  ! map between type of nodal field and integer identifier
  integer(kind=4), parameter :: p_displacement = 0
  integer(kind=4), parameter :: p_velocity     = 0
  integer(kind=4), parameter :: p_temperature  = 0

  ! normally in mod_integrator_parameters
  integer(kind=4), parameter :: p_meca_td_X = 0
  integer(kind=4), parameter :: p_meca_td_V = 0
  integer(kind=4), parameter :: p_ther_td_T = 0

  public is_model_meca, &
         is_model_ther

  contains

  !> \brief Inform if the model type id is a mechanical model type
  function is_model_meca(type)
    implicit none
    integer(kind=4), intent(in) :: type          !< [in] model type integer identifier
    logical                     :: is_model_meca !< [return] true if type belongs to a mechanical type

    is_model_meca = .false.
  end function

  !> \brief Inform if the model type id is a thermal model type
  function is_model_ther(type)
    implicit none
    integer(kind=4), intent(in) :: type          !< [in] model type integer identifier
    logical                     :: is_model_ther !< [return] true if type belongs to a thermal model

    is_model_ther = .false.
  end function

end module mdl_hdl_parameters

