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

!> define the 3D interaction for mechanics
module inter_meca_3D

  use overall, only: max_internal_tact

  use tact_behaviour, only : T_see, &
                             T_TACT_BEHAV, &
                             get_nb_internal, &
                             init_internal

  use utilities, only : logmes, &
                        faterr

  implicit none

  private

  ! space dimension
  integer(kind=4), parameter, public :: space_dim   = 3
  ! number of dofs of the interaction in local contact frame
  integer(kind=4), parameter :: inter_dim   = 3 


  ! Defines :
  !  - T_interaction
  !  - T_this_adjac
  !  - T_verlet
  !  - T_con
  include 'interaction_type.f90'

end module inter_meca_3D
