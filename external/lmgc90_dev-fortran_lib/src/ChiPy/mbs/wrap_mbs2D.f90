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
MODULE wrap_mbs2d

  use iso_c_binding

  use parameters, only : get_contactor_id_from_name

  use mbs2D, only : set_nb, get_nb              , &
                    set_nb_tacty, set_nb_nodes  , &
                    add_tacty                   , &
                    mbs_initialize => initialize, &
                    mbs_finalize   => finalize  , &
                    mbs_increment  => increment , &
                    compute_free_vlocy          , &
                    compute_dof, update_dof     , &
                    get_ptr_coor, get_ptr_coorTT

contains

  ! remplissage / interrogation de la base de donnees

  subroutine setNb(nb) bind(c, name='MBS2D_setNb')
    implicit none
    integer(c_int), intent(in), value :: nb
    !
    call set_nb(nb)

  end subroutine

  function getNb() bind(c, name='MBS2D_getNb')
    implicit none
    integer(c_int) :: getNb
    !
    getNb = get_nb()

  end function

  subroutine setNbTactors(ibdyty,nb) bind(c, name='MBS2D_setNbTactors')
    implicit none
    integer(c_int), intent(in), value :: ibdyty
    integer(c_int), intent(in), value :: nb
    !
    call set_nb_tacty(ibdyty,nb)

  end subroutine

  subroutine setNbNodes(ibdyty,nb) bind(c, name='MBS2D_setNbNodes')
    implicit none
    integer(c_int), intent(in), value :: ibdyty
    integer(c_int), intent(in), value :: nb
    !
    call set_nb_Nodes(ibdyty,nb)

  end subroutine


  subroutine Initialize() bind(c, name="MBS2D_initialize")
    implicit none

    if( get_nb() == 0 ) return

    call mbs_initialize

  end subroutine Initialize

  subroutine Finalize() bind(c, name="MBS2D_finalize")
    implicit none

    if( get_nb() == 0 ) return

    call mbs_finalize

  end subroutine Finalize


  subroutine IncrementStep() bind(c, name='MBS2D_IncrementStep')
    implicit none

    if( get_nb() == 0 ) return

    call mbs_increment

  end subroutine IncrementStep


  subroutine ComputeFreeVelocity() bind(c, name='MBS2D_ComputeFreeVelocity')
    use timer
    implicit none

    integer(kind=4), save :: timer_id = 0

    if( get_nb() == 0 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[MBS2D] comp Free V ')
    call start_itimer(timer_id)

    call compute_free_vlocy

    call stop_itimer(timer_id)

  end subroutine ComputeFreeVelocity

    
  subroutine ComputeDof() bind(c, name='MBS2D_ComputeDof')
    use timer
    implicit none

    integer(kind=4), save :: timer_id = 0

    if( get_nb() == 0 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[MBS2D] comp DOF    ')
    call start_itimer(timer_id)

    call compute_dof

    call stop_itimer(timer_id)

  end subroutine ComputeDof

  subroutine UpdateDof() bind(c, name='MBS2D_UpdateDof')
    use timer
    implicit none

    integer(kind=4), save :: timer_id = 0

    if( get_nb() == 0 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[MBS2D] update DOF  ')
    call start_itimer(timer_id)

    call update_dof

    call stop_itimer(timer_id)

  end subroutine UpdateDof


  subroutine GetPtrCoor(ibdyty,matrix,dim1,dim2) bind(c, name='MBS2D_getPtrCoor')
    implicit none
    integer(c_int), intent(in), value :: ibdyty
    type(c_ptr)                       :: matrix
    integer(c_int)                    :: dim1, dim2
    !
    real(kind=8), dimension(:,:), pointer :: ptr
    
    ptr => get_ptr_coor(ibdyty)
    if( associated(ptr) ) then
      dim1        = size(ptr,1)
      dim2        = size(ptr,2)
      matrix      = c_loc(ptr(1,1))
    else
      dim1        = 0
      dim2        = 0
      matrix      = c_null_ptr
    end if

  end subroutine GetPtrCoor
  
  subroutine GetPtrCoorTT(ibdyty,matrix,dim1,dim2) bind(c, name='MBS2D_getPtrCoorTT')
    implicit none
    integer(c_int), intent(in), value :: ibdyty
    type(c_ptr)                       :: matrix
    integer(c_int)                    :: dim1, dim2
    !
    real(kind=8), dimension(:,:), pointer :: ptr
    
    ptr => get_ptr_coorTT(ibdyty)
    if( associated(ptr) ) then
      dim1        = size(ptr,1)
      dim2        = size(ptr,2)
      matrix      = c_loc(ptr(1,1))
    else
      dim1        = 0
      dim2        = 0
      matrix      = c_null_ptr
    end if

  end subroutine GetPtrCoorTT

  ! les trucs qui ne me plaisent pas

  subroutine addContactor(ibdyty, inodty, itacty, tactype, color, rdata, rsize, idata, isize) bind(c, name='MBS2D_addContactor')
    implicit none
    !> id of mbs
    integer(kind=c_int), intent(in), value :: ibdyty
    !> index of node the new contactor is tied to
    integer(kind=c_int), intent(in), value :: inodty
    !> id of new contactor
    integer(kind=c_int), intent(in), value :: itacty
    !> id of contactor type
    character(c_char), dimension(5), intent(in) :: tactype
    !> color of new contactor
    character(c_char), dimension(5), intent(in) :: color
    !> real data of new contactor
    type(c_ptr), value :: rdata
    !> size of idata array
    integer(kind=c_int), intent(in), value :: rsize
    !> integer data of new contactor
    type(c_ptr), value :: idata
    !> size of idata array
    integer(kind=c_int), intent(in), value :: isize
    !
    integer(c_int), dimension(:), pointer :: ivect
    real(c_double), dimension(:), pointer :: rvect
    character(len=5) :: ctacID, ccolor
    integer(kind=4)  :: i

    ctacID = ''
    ccolor = ''
    do i = 1, 5
       ctacID(i:i) = tactype(i)
       ccolor(i:i) = color(i)
    end do

    ivect => null()
    rvect => null()

    if( isize > 0 ) then
      call c_f_pointer(cptr=idata,fptr=ivect,shape=(/isize/))
    end if
    if( rsize > 0 ) then
      call c_f_pointer(cptr=rdata,fptr=rvect,shape=(/rsize/))
    end if

    i = get_contactor_id_from_name(ctacID)

    call add_tacty(ibdyty, inodty, itacty, i, ccolor, ivect, rvect)

  end subroutine

end module wrap_mbs2d
