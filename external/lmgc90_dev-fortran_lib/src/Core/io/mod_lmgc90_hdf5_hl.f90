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

!> Define high level function about hdf5 read/write
module lmgc90_hdf5_hl

  use lmgc90_hdf5, only : ugly_fix            , &
                          open_read_h5        , &
                          open_write_h5       , &
                          read_h5_header      , &
                          read_h5_step_header , &
                          write_h5_header     , &
                          write_h5_step_header, &
                          reset_last_filename , &
                          close_h5            , &
                          version_check

  use h5_interaction, only :  read_h5_Vloc_Rloc_2D, &
                             write_h5_Vloc_Rloc_2D, &
                              read_h5_Vloc_Rloc_3D, &
                             write_h5_Vloc_Rloc_3D

  use h5_mailx, only : write_h5_data_mecaMAILx , &
                       write_h5_data_therMAILx , &
                       write_h5_data_poroMAILx , &
                       write_h5_data_multiMAILx, &
                        read_h5_data_mecaMAILx , &
                        read_h5_data_therMAILx , &
                        read_h5_data_poroMAILx , &
                        read_h5_data_multiMAILx

  use h5_rigid, only :  read_h5_dof_RBDY2, &
                       write_h5_dof_RBDY2, &
                        read_h5_dof_RBDY3, &
                       write_h5_dof_RBDY3

  use h5_format, only :   evolution_id  , &
                          set_all_format, &
                        clean_all_format

  use overall, only : nbDIME

  implicit none

  private :: write_step_

  public ::  init_h5_hl  , &
             read_h5_hl  , &
            write_h5_hl  , &
            write_l_h5_hl, &
            clean_memory , &
            set_ugly_fixer

contains

!------------------------------------------------------------------------

  subroutine init_h5_hl(filename)
    implicit none
    character(len=*), intent(in) :: filename
    !
    integer :: major, minor

    call open_write_h5(major, minor, filename)
    call set_all_format(major, minor)

    call write_h5_header()
    call close_h5( )

  end subroutine

  subroutine write_step_(last)
    implicit none
    logical :: last
    !
    integer :: evol_id


    call write_h5_step_header(last)
    ! store evolution_id and reset
    if( last ) then
      evol_id = evolution_id
      evolution_id = 1
    end if

    ! Write DOF in HDF5 file
    if( nbDIME == 2 ) then
      call write_h5_dof_RBDY2()
    else if ( nbDIME == 3 ) then
      call write_h5_dof_RBDY3()
    end if

    call write_h5_data_mecaMAILx()
    call write_h5_data_therMAILx()
    call write_h5_data_poroMAILx()
    call write_h5_data_multiMAILx()

    ! Write Vloc_Rloc in HDF5 file
    if ( nbDIME == 2 ) then
      call write_h5_Vloc_Rloc_2D()
    else if ( nbDIME == 3 ) then
      call write_h5_Vloc_Rloc_3D()
    end if

    ! put back evolution_id
    if( last ) then
      evolution_id = evol_id
    end if

  end subroutine write_step_

  subroutine write_h5_hl( )
    implicit none
    integer :: major, minor

    ! open file stored by init call
    call open_write_h5(major, minor)
    call set_all_format(major, minor)
    call write_step_(.false.)
    call close_h5()

  end subroutine write_h5_hl


  subroutine write_l_h5_hl(filename)
    implicit none
    character(len=*), intent(in) :: filename
    !
    integer :: major, minor

    call open_write_h5(major, minor, filename, .true.)
    call set_all_format(major, minor)

    call write_h5_header(.true.)
    call write_step_(.true.)

    call close_h5( )

  end subroutine write_l_h5_hl

!------------------------------------------------------------------------

  subroutine read_h5_hl(step, filename)
    implicit none
    integer         , intent(in) :: step
    character(len=*), intent(in) :: filename
    !
    integer :: major, minor, bkp
    logical :: success

    bkp = evolution_id

    call open_read_h5(filename, major, minor)
    call set_all_format(major, minor)

    call version_check(major, minor)

    ! only once ?
    call read_h5_header()

    if( step < 1 ) then
      call read_h5_step_header(evolution_id, success)
    else
      call read_h5_step_header(step, success)
      evolution_id = step
    end if

    ! time step does not exist
    if( .not. success ) then
      call close_h5( )
      return
    end if

    ! internal numbering explicitely set to step
    ! before reading the group itself

    if( nbDIME == 2 ) then
      call read_h5_dof_RBDY2(evolution_id)
    else if( nbDIME == 3 ) then
      call read_h5_dof_RBDY3(evolution_id)
    end if

    call read_h5_data_mecaMAILx(evolution_id)
    call read_h5_data_therMAILx(evolution_id)
    call read_h5_data_poroMAILx(evolution_id)
    call read_h5_data_multiMAILx(evolution_id)

    if( nbDIME == 2 ) then
      call read_h5_Vloc_Rloc_2D(evolution_id)
    else if( nbDIME == 3 ) then
      call read_h5_Vloc_Rloc_3D(evolution_id)
    end if

    call close_h5( )

    evolution_id = bkp

  end subroutine read_h5_hl

!------------------------------------------------------------------------

!------------------------------------------------------------------------

  subroutine clean_memory()

    call reset_last_filename()
    call clean_all_format()

  end subroutine

  subroutine set_ugly_fixer(version)
    implicit none
    integer, intent(in) :: version

    ugly_fix = version

  end subroutine

end module lmgc90_hdf5_hl
