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

!> Empty paranoid checks on integer/real array or pointers...
!> to be inlined for even more efficiency
MODULE paranoid_checks

  use utilities, only : faterr

  implicit none

  private

  public paranoid_check_i4     , &
         paranoid_check_i4_ptr , &
         paranoid_check_i4_size, &
         paranoid_check_r8     , &
         paranoid_check_r8_ptr , &
         paranoid_check_r8_size, &
         paranoid_check_c5     , &
         paranoid_check_c5_ptr , &
         paranoid_check_c5_size, &
         paranoid_check_cx     , &
         paranoid_check_cx_ptr , &
         paranoid_check_cx_size
  
contains

!!----------------------------------------------
!! Checks on integers array/pointers
!!----------------------------------------------

  subroutine paranoid_check_i4(where, i4_vector, min_size)
    implicit none
    character(len=*), intent(in) :: where
    integer(kind=4), dimension(:), allocatable, intent(in) :: i4_vector
    integer(kind=4), optional,     intent(in) :: min_size

  end subroutine

  subroutine paranoid_check_i4_ptr(where, i4_pointer, min_size)
    implicit none
    character(len=*), intent(in) :: where
    integer(kind=4), dimension(:), pointer    :: i4_pointer
    integer(kind=4), optional,     intent(in) :: min_size

  end subroutine

  subroutine paranoid_check_i4_size(where, i4_vector, wanted_size)
    implicit none
    character(len=*), intent(in) :: where
    integer(kind=4), dimension(:), intent(in) :: i4_vector
    integer(kind=4), intent(in) :: wanted_size

  end subroutine

!!----------------------------------------------
!! Checks on reals array/pointers
!!----------------------------------------------

  subroutine paranoid_check_r8(where, r8_vector, min_size)
    implicit none
    character(len=*), intent(in) :: where
    real(kind=8), dimension(:), allocatable, intent(in) :: r8_vector
    integer(kind=4), optional,  intent(in) :: min_size

  end subroutine

  subroutine paranoid_check_r8_ptr(where, r8_pointer, min_size)
    implicit none
    character(len=*), intent(in) :: where
    real(kind=8), dimension(:), pointer   :: r8_pointer
    integer(kind=4), optional, intent(in) :: min_size

  end subroutine

  subroutine paranoid_check_r8_size(where, r8_vector, wanted_size)
    implicit none
    character(len=*), intent(in) :: where
    real(kind=8), dimension(:), intent(in) :: r8_vector
    integer(kind=4), intent(in) :: wanted_size

  end subroutine

!!----------------------------------------------
!! Checks on string of size 5 array/pointers
!!----------------------------------------------

  subroutine paranoid_check_c5(where, c5_vector, min_size)
    implicit none
    character(len=*), intent(in) :: where
    character(len=5), dimension(:), allocatable, intent(in) :: c5_vector
    integer(kind=4),  optional,     intent(in) :: min_size

  end subroutine

  subroutine paranoid_check_c5_ptr(where, c5_pointer, min_size)
    implicit none
    character(len=*), intent(in) :: where
    character(len=5), dimension(:), pointer :: c5_pointer
    integer(kind=4), optional, intent(in)   :: min_size

  end subroutine

  subroutine paranoid_check_c5_size(where, c5_vector, wanted_size)
    implicit none
    character(len=*), intent(in) :: where
    character(len=5), dimension(:), intent(in) :: c5_vector
    integer(kind=4), intent(in) :: wanted_size

  end subroutine

!!----------------------------------------------
!! Checks on string of size 128 array/pointers
!!----------------------------------------------

  subroutine paranoid_check_cx(where, cx_vector, min_size)
    implicit none
    character(len=*),   intent(in) :: where
    character(len=128), dimension(:), allocatable, intent(in) :: cx_vector
    integer(kind=4),    optional,     intent(in) :: min_size

  end subroutine

  subroutine paranoid_check_cx_ptr(where, cx_pointer, min_size)
    implicit none
    character(len=*),   intent(in) :: where
    character(len=128), dimension(:), pointer :: cx_pointer
    integer(kind=4),   optional, intent(in)   :: min_size

  end subroutine

  subroutine paranoid_check_cx_size(where, cx_vector, wanted_size)
    implicit none
    character(len=*),   intent(in) :: where
    character(len=128), dimension(:), intent(in) :: cx_vector
    integer(kind=4),   intent(in) :: wanted_size

  end subroutine

end module paranoid_checks
