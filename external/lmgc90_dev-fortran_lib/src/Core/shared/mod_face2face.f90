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
module face2face

  implicit none

  private

  !> a type to store contact points of contactors facing each others
  type, public :: T_face2face
    logical         :: active !< is the face2face object still active
    integer(kind=4) :: cd     !< id of candidat
    integer(kind=4) :: an     !< id of antagonist
    integer(kind=4) :: an_fac !< face number of antagonist
    integer(kind=4) :: an_pt  !< point number of antagonist
    integer(kind=4) :: isee   !< id of contact law 
    integer(kind=4) :: nb_ctc !< number of contact points
    ! apres je sais pas quoi c'est...
    integer :: iff_cd(4),iff_an(4)
    ! local reduced coordinates (eta,xi,zet) in (-1,+1)
    !f2f barycentric coordinates
    REAL(kind=8),DIMENSION(3,4)::cd_lcoor
    REAL(kind=8),DIMENSION(3,4)::an_lcoor
    ! utilise par l'ancien gestionnaire explicite a la alcan
    !! initial position relative to the brick
    REAL(kind=8),DIMENSION(3,4)::coorcd,cooran     
  end type T_face2face

  public find_candidat_index_in_list, &
         remove_neighbour_from_list

  contains

  !> Get the index of a chosen candidat in a list
  !> The list is an allocated array, the first element
  !> gives the number of candidat stored in the list
  function find_candidat_index_in_list(list, can)
    implicit none
    integer(kind=4), intent(in) :: can             !< [in] candidat number
    integer(kind=4), intent(in) :: list(:)         !< [in] list of candidat
    integer(kind=4) :: find_candidat_index_in_list !< [return] index of the candidat in the list
    !
    integer(kind=4) :: i

    find_candidat_index_in_list = 0
    if( list(1) < 1 ) return
    do i = 2, list(1) + 1
      if( list(i) == can ) then
        find_candidat_index_in_list = i-1
        return
      end if
    end do
  end function

  !> Remove an element from a neighbours list
  subroutine remove_neighbour_from_list(list, i_map, max_size)
    implicit none
    integer(kind=4), intent(in)    :: i_map          !<[in] index in the list of the element to remove
    integer(kind=4), intent(in)    :: max_size       !<[in] size of the list
    integer(kind=4), intent(inout) :: list(max_size) !<[in,out] the neighbours list

    list(1) = list(1) - 1
    list(i_map:max_size-1) = list(i_map+1:max_size)
    list(max_size) = 0
  end subroutine

end module face2face
