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

!> Axis-Aligned Bounding Box detection methods using rTree library
!> \todo : periodicity management.
module aabb_detection

  use parameters

  use overall, only : logmes

  use rough_type, only : T_rough     , &
                         T_link_rough, &
                         add_leaf

  use rtree, only : f_reset_tree2  , &
                    f_add2tree2    , &
                    f_search_in_tree2
                    !f_add2tree3    , &
                    !f_reset_tree3  , &
                    !f_search_in_tree3

  use contactor_2D, only : get_aabbox

  use see_table, only : get_cdan   , &
                        get_cd_data, &
                        get_an_data, &
                        get_option , &
                        get_name

  implicit none

  public aabb_rough_detection

  private auto_search_, &
          fill_tree_  , &
          search_

  contains

  !> \brief Use axis aligned bounding box method to generate rough list
  !> Use rTree library to run the detection
  subroutine aabb_rough_detection(i_see, rough_list, reset_tree)
    implicit none
    !> see table rank used
    integer, intent(in)  :: i_see
    !> linked list in which to add new rough list
    type(T_link_rough), pointer :: rough_list
    !> need to refill the antagonist tree (can be false if antagonist did not changed)
    logical, intent(in)  :: reset_tree
    !
    logical          :: active_opt
    integer          :: cdan, nb_boxes, nb_rough
    integer          :: cdtacty, cdtac_offset
    integer          :: antacty, antac_offset
    character(len=5) :: cdcol  , ancol
    real(kind=8)     :: alert
    !
    real(kind=8), dimension(:,:,:), pointer :: cd_bbox, an_bbox
    !
    character(len=128) :: cout

    call get_cd_data(i_see, cdtacty, cdcol)
    call get_an_data(i_see, antacty, ancol)

    active_opt = get_option(i_see, i_halo, alert)
    cdan = get_cdan(i_see)

    call get_aabbox( antacty, ancol, an_bbox, antac_offset )

    ! if no contactor bounding for contactors of desired color
    if( .not. associated(an_bbox) ) then
      write(cout,*) get_name(i_see), " no antagonist contactor of desired color and body support type"
      call logmes(cout)
      return
    end if

    an_bbox(:,1,:) = an_bbox(:,1,:) - alert*0.5d0
    an_bbox(:,2,:) = an_bbox(:,2,:) + alert*0.5d0

    if( cdtacty == antacty .and. cdcol == ancol ) then

      call auto_search_(rough_list, cdan, i_see, an_bbox, antac_offset, nb_rough)

    else

      if( reset_tree ) then
        call fill_tree_(an_bbox)
      end if

      nb_boxes = size(an_bbox,3)

      call get_aabbox( cdtacty, cdcol, cd_bbox, cdtac_offset )

      ! if no contactor bounding for contactors of desired color
      if( .not. associated(cd_bbox) ) then
        write(cout,*) get_name(i_see), " no candidate contactor of desired color and body support type"
        call logmes(cout)
        return
      end if

      cd_bbox(:,1,:) = cd_bbox(:,1,:) - alert*0.5d0
      cd_bbox(:,2,:) = cd_bbox(:,2,:) + alert*0.5d0

      call search_(rough_list, cdan, i_see, cd_bbox, cdtac_offset, antac_offset, nb_boxes, nb_rough)

    end if

    write(cout,*) get_name(i_see), nb_rough, get_interaction_name_from_id(cdan), " roughly found"
    call logmes(cout)

  end subroutine aabb_rough_detection

  !> Use rtree library to look for intersecting boxes
  subroutine auto_search_(rough_list, cdan, i_see, bbox, offset, nb)
    implicit none
    !> linked list in which to add new rough list
    type(T_link_rough), pointer :: rough_list
    !> contact type id
    integer, intent(in) :: cdan
    !> see table index
    integer, intent(in) :: i_see
    !> list of boxes to check
    real(kind=8), dimension(:,:,:), intent(in) :: bbox
    !> offset of index for correct numbering
    integer, intent(in) :: offset
    !> number of intersection found
    integer, intent(out) :: nb
    !
    integer :: nb_boxes, nb_tact, i_tact, i
    integer, dimension(:), pointer, save :: icdan => null()
    type(T_rough), pointer :: new_rough

    nb = 0
    call f_reset_tree2()

    ! number of antagonist
    nb_tact = size(bbox,3)
    ! but the number of boxes must take into account the periodic antagonists too
    nb_boxes = nb_tact !+ nb_perio
    !nb_perio = sum(abs(xperio(itact))+abs(yperio(itact))) sur itact

    ! will store the list of intersecting boxes: allocate to max needed
    if( associated(icdan) ) then
      if( size(icdan)<nb_boxes+1) then
        deallocate(icdan)
        nullify(icdan)
      end if
    end if

    ! first value: number of intersection
    ! next values: the id of intersected boxes
    if( .not. associated(icdan) ) allocate(icdan(nb_tact))

    ! adding first antagonist
    call f_add2tree2(bbox(:,1,1),bbox(:,2,1),1)

    do i_tact = 2,nb_tact

      ! pretty sure useless... but safety first
      ! and really really slow...
      !icdan = 0

      ! check tact against tree
      call f_search_in_tree2(bbox(:,1,i_tact),bbox(:,2,i_tact),icdan)

      call f_add2tree2(bbox(:,1,i_tact),bbox(:,2,i_tact),i_tact)
      ! adding intersections to rough list
      do i = 2, icdan(1)+1

        allocate(new_rough)

        new_rough%cdan = cdan
        new_rough%cd   = i_tact+offset
        new_rough%an   = icdan(i)+offset
        new_rough%isee = i_see
        new_rough%xperiodic = 0
        new_rough%yperiodic = 0
        new_rough%zperiodic = 0

        ! make sure that cd < an
        if( new_rough%cd > new_rough%an ) then
          new_rough%cd = icdan(i)+offset
          new_rough%an = i_tact+offset
        end if

        call add_leaf(rough_list, new_rough)
        nb = nb+1

      end do

    end do

  end subroutine

  !> Use rtree library to fill the tree
  subroutine fill_tree_(bbox)
    implicit none
    !> list of boxes to store
    real(kind=8), dimension(:,:,:), intent(in) :: bbox
    !
    integer :: nb_tact, i_tact

    call f_reset_tree2()

    nb_tact = size(bbox,3)

    do i_tact = 1, nb_tact
      call f_add2tree2(bbox(:,1,i_tact),bbox(:,2,i_tact),i_tact)
    end do

  end subroutine

  !> Use rtree library to look for intersecting boxes with the ones stored
  subroutine search_(rough_list, cdan, i_see, bbox, shift, shiftt, nb_boxes, nb)
    implicit none
    !> linked list in which to add new rough list
    type(T_link_rough), pointer :: rough_list
    !> contact type id
    integer, intent(in) :: cdan
    !> see table index
    integer, intent(in) :: i_see
    !> list of boxes to check
    real(kind=8), dimension(:,:,:), intent(in) :: bbox
    !> shift of contactor index of bbox
    integer, intent(in) :: shift
    !> shift of contactor index of boxes in tree
    integer, intent(in) :: shiftt
    !> number of boxes in tree
    integer, intent(in) :: nb_boxes
    !> number of intersection added
    integer, intent(out) :: nb
    !
    integer :: nb_tact, i_tact, i
    integer, dimension(:), pointer, save :: icdan => null()
    type(T_rough), pointer :: new_rough

    nb = 0

    ! number of candidat
    nb_tact = size(bbox,3)

    ! will store the list of intersecting boxes: allocate to max needed
    if( associated(icdan) ) then
      if( size(icdan)<nb_boxes+1 ) then
        deallocate(icdan)
        nullify(icdan)
      end if
    end if

    ! first value: number of intersection
    ! next values: the id of intersected boxes
    if( .not. associated(icdan) ) allocate(icdan(nb_boxes+1))

    do i_tact = 1,nb_tact

      ! pretty sure useless... but safety first
      icdan = 0

      ! check tact against tree
      call f_search_in_tree2(bbox(:,1,i_tact),bbox(:,2,i_tact),icdan)

      ! adding intersections to rough list
      do i = 2, icdan(1)+1

        allocate(new_rough)

        new_rough%cdan = cdan
        new_rough%cd   = i_tact+shift
        new_rough%an   = icdan(i)+shiftt
        new_rough%isee = i_see
        new_rough%xperiodic = 0
        new_rough%yperiodic = 0
        new_rough%zperiodic = 0

        ! make sure that cd < an
        if( new_rough%cd > new_rough%an ) then
          new_rough%cd = icdan(i)+shift
          new_rough%an = i_tact+shift
        end if

        call add_leaf(rough_list, new_rough)
        nb = nb+1

      end do

    end do

  end subroutine

end module


