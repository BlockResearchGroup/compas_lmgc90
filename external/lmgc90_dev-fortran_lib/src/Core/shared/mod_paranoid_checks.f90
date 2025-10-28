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

!> Some paranoid checks on integer/real array or pointers...
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

  !> \brief Check the allocation of an allocatable integer array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_i4(where, i4_vector, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),                           intent(in) :: where
    !> [in] integer array to check
    integer(kind=4), dimension(:), allocatable, intent(in) :: i4_vector
    !> [in] (optional) minimum required size of the array
    integer(kind=4), optional,                  intent(in) :: min_size

    if( .not. allocated(i4_vector) ) then
      call faterr(where,'integer array not allocated')
    end if

    if( present(min_size) ) then
      if( size(i4_vector) < min_size ) then 
        call faterr(where,'integer allocated array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the association of a pointer on an integer array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_i4_ptr(where, i4_pointer, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*), intent(in)           :: where
    !> [in] pointer to check
    integer(kind=4), dimension(:), pointer :: i4_pointer
    !> [in] (optional) minimum required size of the array
    integer(kind=4), optional, intent(in)  :: min_size

    if( .not. associated(i4_pointer) ) then
      call faterr(where,'integer pointer not associated')
    end if

    if( present(min_size) ) then
      if( size(i4_pointer) < min_size ) then 
        call faterr(where,'integer pointer array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the exact size of an integer array
  subroutine paranoid_check_i4_size(where, i4_vector, wanted_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),              intent(in) :: where
    !> [in] integer array to check
    integer(kind=4), dimension(:), intent(in) :: i4_vector
    !> [in] the desired size of the array
    integer(kind=4),               intent(in) :: wanted_size

    if( size(i4_vector) /= wanted_size ) then 
      call faterr(where,'integer array is not of required size')
    end if
  end subroutine

!!----------------------------------------------
!! Checks on reals array/pointers
!!----------------------------------------------

  !> \brief Check the allocation of an allocatable real array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_r8(where, r8_vector, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),                        intent(in) :: where
    !> [in] real array to check
    real(kind=8), dimension(:), allocatable, intent(in) :: r8_vector
    !> [in] (optional) minimum required size of the array
    integer(kind=4), optional,               intent(in) :: min_size

    if( .not. allocated(r8_vector) ) then
      call faterr(where,'real array not allocated')
    end if

    if( present(min_size) ) then
      if( size(r8_vector) < min_size ) then 
        call faterr(where,'real allocated array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the association of a pointer on a real array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_r8_ptr(where, r8_pointer, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*), intent(in)          :: where
    !> [in] pointer to check
    real(kind=8), dimension(:), pointer   :: r8_pointer
    !> [in] (optional) minimum required size of the array
    integer(kind=4), optional, intent(in) :: min_size

    if( .not. associated(r8_pointer) ) then
      call faterr(where,'real pointer not associated')
    end if

    if( present(min_size) ) then
      if( size(r8_pointer) < min_size ) then 
        call faterr(where,'real pointer array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the exact size of a real array
  subroutine paranoid_check_r8_size(where, r8_vector, wanted_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),           intent(in) :: where
    !> [in] real array to check
    real(kind=8), dimension(:), intent(in) :: r8_vector
    !> [in] the desired size of the array
    integer(kind=4),            intent(in) :: wanted_size

    if( size(r8_vector) /= wanted_size ) then 
      call faterr(where,'real array is not of required size')
    end if
  end subroutine

!!----------------------------------------------
!! Checks on string of size 5 array/pointers
!!----------------------------------------------

  !> \brief Check the allocation of an allocatable 5 characters string array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_c5(where, c5_vector, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),                            intent(in) :: where
    !> [in] 5 characters string array to check
    character(len=5), dimension(:), allocatable, intent(in) :: c5_vector
    !> [in] (optional) minimum required size of the array
    integer(kind=4),  optional,                  intent(in) :: min_size

    if( .not. allocated(c5_vector) ) then
      call faterr(where,'string array not allocated')
    end if

    if( present(min_size) ) then
      if( size(c5_vector) < min_size ) then 
        call faterr(where,'string allocated array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the association of a pointer on a 5 characters string array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_c5_ptr(where, c5_pointer, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*), intent(in)            :: where
    !> [in] pointer to check
    character(len=5), dimension(:), pointer :: c5_pointer
    !> [in] (optional) minimum required size of the array
    integer(kind=4), optional, intent(in)   :: min_size

    if( .not. associated(c5_pointer) ) then
      call faterr(where,'string pointer not associated')
    end if

    if( present(min_size) ) then
      if( size(c5_pointer) < min_size ) then 
        call faterr(where,'string pointer array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the exact size of a 5 characters string array
  subroutine paranoid_check_c5_size(where, c5_vector, wanted_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),               intent(in) :: where
    !> [in] the 5 characters array to check
    character(len=5), dimension(:), intent(in) :: c5_vector
    !> [in] the desired size of the array
    integer(kind=4),                intent(in) :: wanted_size

    if( size(c5_vector) /= wanted_size ) then 
      call faterr(where,'string array is not of required size')
    end if
  end subroutine

!!----------------------------------------------
!! Checks on string of size 128 array/pointers
!!----------------------------------------------

  !> \brief Check the allocation of an allocatable 128 characters string array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_cx(where, cx_vector, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),                            intent(in) :: where
    !> [in] 128 characters string array to check
    character(len=128), dimension(:), allocatable, intent(in) :: cx_vector
    !> [in] (optional) minimum required size of the array
    integer(kind=4),    optional,                  intent(in) :: min_size  !< [in] (optional) minimum required size of the array

    if( .not. allocated(cx_vector) ) then
      call faterr(where,'string array not allocated')
    end if

    if( present(min_size) ) then
      if( size(cx_vector) < min_size ) then 
        call faterr(where,'string allocated array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the association of a pointer on a 128 characters string array
  !> Possibly also check that it is of a minimum size
  subroutine paranoid_check_cx_ptr(where, cx_pointer, min_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),   intent(in)            :: where
    !> [in] pointer to check
    character(len=128), dimension(:), pointer :: cx_pointer
    !> [in] (optional) minimum required size of the array
    integer(kind=4),   optional, intent(in)   :: min_size

    if( .not. associated(cx_pointer) ) then
      call faterr(where,'string pointer not associated')
    end if

    if( present(min_size) ) then
      if( size(cx_pointer) < min_size ) then 
        call faterr(where,'string pointer array is not of required minimum size')
      end if
    end if
  end subroutine

  !> \brief Check the exact size of a 128 characters string array
  subroutine paranoid_check_cx_size(where, cx_vector, wanted_size)
    implicit none
    !> [in] name of the function calling the check
    character(len=*),                 intent(in) :: where
    !> [in] the 128 characters string array to check
    character(len=128), dimension(:), intent(in) :: cx_vector
    !> [in] the desired size of the array
    integer(kind=4),                  intent(in) :: wanted_size

    if( size(cx_vector) /= wanted_size ) then 
      call faterr(where,'string array is not of required size')
    end if
  end subroutine

end module paranoid_checks
