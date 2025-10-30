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
module wrap_io_hdf5_hl

  use iso_c_binding

  use timer, only : get_new_itimer_ID, &
                    start_itimer     , &
                    stop_itimer

  use lmgc90_hdf5_hl, only : init_h5_hl    , &
                             read_h5_hl    , &
                             write_h5_hl   , &
                             write_l_h5_hl , &
                             set_ugly_fixer, &
                             clean_memory

  implicit none
  
contains
  
  subroutine ioHdf5InitOutFile(cvalue1) bind(c, name='io_hdf5_initOutFile')
    character(c_char), dimension(*) :: cvalue1
    !
    character(len=256) :: cvalue1_f
    integer :: i

    cvalue1_f = ''
    do i=1,len(cvalue1_f)
        if( cvalue1(i) == c_null_char ) exit
        cvalue1_f = cvalue1_f(1:i-1) // cvalue1(i)
    end do
    call init_h5_hl(cvalue1_f)

  end subroutine ioHdf5initOutFile

  subroutine ioHdf5Write() bind(c, name='io_hdf5_write')
    integer, save :: timer_id = 0

    if( timer_id == 0 ) timer_id = get_new_itimer_id('[HDF5] write        ')

    call start_itimer(timer_id)
    call write_h5_hl()
    call stop_itimer(timer_id)

  end subroutine ioHdf5Write

  subroutine ioHdf5WriteLast(cvalue1) bind(c, name='io_hdf5_write_last')
    character(c_char), dimension(*) :: cvalue1
    !
    character(len=256) :: cvalue1_f
    integer :: i
    integer, save :: timer_id = 0

    if( timer_id == 0 ) timer_id = get_new_itimer_id('[HDF5] write last   ')

    cvalue1_f = ''
    do i=1,len(cvalue1_f)
        if( cvalue1(i) == c_null_char ) exit
        cvalue1_f = cvalue1_f(1:i-1) // cvalue1(i)
    end do

    call start_itimer(timer_id)
    call write_l_h5_hl(cvalue1_f)
    call stop_itimer(timer_id)

  end subroutine ioHdf5WriteLast

  subroutine ioHdf5Read(cvalue1, step) bind(c, name='io_hdf5_read')
    implicit none
    character(c_char), dimension(*) :: cvalue1
    integer(kind=c_int), intent(in), value :: step
    !
    character(len=256) :: cvalue1_f
    integer :: i

    cvalue1_f = ''
    do i=1,len(cvalue1_f)
        if( cvalue1(i) == c_null_char ) exit
        cvalue1_f = cvalue1_f(1:i-1) // cvalue1(i)
    end do
    call read_h5_hl(step, cvalue1_f)

  end subroutine ioHdf5Read

  subroutine ioHdf5CleanMemory() bind(c, name='io_hdf5_cleanMemory')
    implicit none

    call clean_memory

  end subroutine

  subroutine ioHdf5FixVersion(version) bind(c, name='io_hdf5_fixVersion')
    implicit none
    integer(kind=c_int), intent(in), value :: version

    call set_ugly_fixer(version)

  end subroutine

end module wrap_io_hdf5_hl
