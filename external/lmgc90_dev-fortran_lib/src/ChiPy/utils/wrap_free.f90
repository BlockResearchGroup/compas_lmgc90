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
module free

  use iso_c_binding

contains

  subroutine free_ptr_fortran_double_1(array,length) bind(c, name='free_ptrFortranDouble1')
    implicit none
    integer(c_int),  intent(in), value :: length
    type(c_ptr), value :: array
    !
    real(kind=c_double), dimension(:), pointer :: farray

    farray => null()
    call c_f_pointer(cptr=array,fptr=farray,shape=(/length/))

    if( associated(farray) ) deallocate(farray)

  end subroutine

  subroutine free_ptr_fortran_int_1(array,length) bind(c, name='free_ptrFortranInt1')
    implicit none
    integer(c_int),  intent(in), value :: length
    type(c_ptr), value :: array
    !
    integer(kind=c_int), dimension(:), pointer :: farray

    farray => null()
    call c_f_pointer(cptr=array,fptr=farray,shape=(/length/))

    if( associated(farray) ) deallocate(farray)

  end subroutine

  subroutine free_ptr_fortran_double_2(array,nrow,ncol) bind(c, name='free_ptrFortranDouble2')
    implicit none
    integer(c_int),  intent(in), value :: nrow, ncol
    type(c_ptr), value :: array
    !
    real(kind=c_double), dimension(:,:), pointer :: farray

    farray => null()
    call c_f_pointer(cptr=array,fptr=farray,shape=(/ncol,nrow/))

    if( associated(farray) ) deallocate(farray)

  end subroutine

  subroutine free_ptr_fortran_int_2(array,nrow,ncol) bind(c, name='free_ptrFortranInt2')
    implicit none
    integer(c_int),  intent(in), value :: nrow,ncol
    type(c_ptr), value :: array
    !
    integer(kind=c_int), dimension(:,:), pointer :: farray

    farray => null()
    call c_f_pointer(cptr=array,fptr=farray,shape=(/ncol,nrow/))

    if( associated(farray) ) deallocate(farray)

  end subroutine

  subroutine free_ptr_fortran_string(string, length) bind(c, name='free_ptrFortranString')
    implicit none
    type(c_ptr), value :: string
    integer(c_int), intent(in), value :: length
    !
    character(kind=c_char), dimension(:), pointer :: fstring
    character, pointer :: gniii

    fstring => null()
    call c_f_pointer(cptr=string, fptr=fstring, shape=(/length/))

    gniii => fstring(1)
    if( associated(gniii) ) deallocate(gniii)

  end subroutine

end module free
