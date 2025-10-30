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
module wrap_interaction_handler

  use ISO_C_BINDING

  use interaction_handler, only : get_nb_interactions      , &
                                  get_nb_rough_interactions, &
                                  get_nb_recup_interactions, &
                                  activate_detection       , &
                                  run_detection            , &
                                  stock_rloc               , &
                                  recup_rloc               , &
                                  write_xxx_vloc_rloc      , &
                                  read_xxx_vloc_rloc       , &
                                  get_ptr_all              , &
                                  clean_memory

  public selectProxTactors!, &
       !  stockRloc        , &
       !  recupRloc        , &
       !  writeLastVlocRloc, &
       !  cleanMemory

  contains
  
  function getNb() bind(c, name='interaction_handler_getNb')
    implicit none
    integer(c_int) :: getNb

    getNb = get_nb_interactions()

  end function

  function getNbRough() bind(c, name='interaction_handler_getNbRough')
    implicit none
    integer(c_int) :: getNbRough

    getNbRough = get_nb_rough_interactions()

  end function

  function getNbRecup() bind(c, name='interaction_handler_getNbRecup')
    implicit none
    integer(c_int) :: getNbRecup

    getNbRecup = get_nb_recup_interactions()

  end function

  subroutine activateDetection(cvalue) bind(c, name='interaction_handler_activateDetection')
    use parameters, only : get_interaction_id_from_name
    implicit none
    character(c_char), dimension(5), intent(in) :: cvalue
    !
    integer(kind=4)  :: inter_id, i
    character(len=5) :: inter_name
    
    inter_name = ''
    do i = 1, 5
       inter_name = inter_name(1:i-1) // cvalue(i)
    end do
    inter_id = get_interaction_id_from_name(inter_name)

    call activate_detection(inter_id,1)

  end subroutine

  !!!-------------------------------------------------------
  subroutine selectProxTactors() bind(c, name='interaction_handler_selectProxTactors')
    implicit none

    call run_detection()

  end subroutine
  !!!--------------------------------------------------

  subroutine stockRloc() bind(c, name='interaction_handler_stockRloc')
    implicit none

    call stock_rloc()

  end subroutine
  !!!--------------------------------------------------
  subroutine recupRloc() bind(c, name='interaction_handler_recupRloc')
    implicit none

    ! must initialize this from verlet in interaction_handler
    call recup_rloc()

  end subroutine
  !!!--------------------------------------------------

  subroutine writeOutVlocRloc() bind(c, name='interaction_handler_writeOutVlocRloc')
    use overall, only : write_vloc_rloc
    implicit none

     if( write_vloc_rloc ) call write_xxx_vloc_rloc(1)

  end subroutine

  subroutine writeLastVlocRloc() bind(c, name='interaction_handler_writeLastVlocRloc')
    implicit none

     call write_xxx_vloc_rloc(2)

  end subroutine

  subroutine displayOutVlocRloc() bind(c, name='interaction_handler_displayOutVlocRloc')
    implicit none

     call write_xxx_vloc_rloc(6)

  end subroutine

  subroutine readIniVlocRloc() bind(c, name='interaction_handler_readIniVlocRloc')
    implicit none

    call read_xxx_Vloc_Rloc

  end subroutine

  subroutine getPtrAll(ptr, dim1, dim2) bind(c, name='interaction_handler_getPtrAll')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1, dim2

    real(kind=8), dimension(:,:), pointer :: all

    all => get_ptr_all()

    if( associated(all) ) then
      ptr  = c_loc(all(1,1))
      dim1 = size(all,1)
      dim2 = size(all,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine getPtrAll

  subroutine cleanMemory() bind(c, name='interaction_handler_cleanMemory')
    implicit none

    call clean_memory()

  end subroutine

end module wrap_interaction_handler
