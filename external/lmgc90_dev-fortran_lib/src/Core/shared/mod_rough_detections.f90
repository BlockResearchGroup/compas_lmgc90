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

!> manages 
module rough_detections

  use utilities, only : faterr, logmes

  use anonymous, only : T_object, &
                        set_rank, &
                        set_i4_ptr

  use anonymous_ptr_container, only : T_rough_container       => PTR_CONTAINER              , &
                                      add_object_to_container => add_object_to_ptr_container, &
                                      close_container         => close_ptr_container

  implicit none

  private

  !> A box type for the rough detection with boxes method
  type T_box
    integer(kind=4) :: popul !< the number of bodies in the box
    integer(kind=4), dimension(:), pointer :: which => null() !< the index of the bodies in the box
  end type T_box

  type(T_box), dimension(:,:,:), allocatable :: box !< all the boxes in the 3 directions of space

  !-------------------------------------------------------------------------------------------------
  !am: defining linked lists of indices
  type list_data
     integer(kind=4) :: i4
  end type list_data

  include "linkedlist_type.f90"
  !-------------------------------------------------------------------------------------------------

  !am: sparse srorage of the boxes, based on linked lists
  type T_box_list
    type(linked_list), pointer :: list => null()
  end type T_box_list

  type(T_box_list), dimension(:,:,:), allocatable :: box_lists !< all the boxes in the 3 directions of space

  !am: spasre storage of the boxes in a sparse matrix
  integer(kind=4), dimension(:), allocatable :: box_indices ! indces of the not empty boxes
  type(T_box_list), dimension(:), allocatable :: box_vec

  public boxes_method, &
         boxes_method_lists, &
         boxes_method_sparse, &
         clean_module

  contains

  !> boxes method
  subroutine boxes_method(positions, bdr_radii, alert, rough_list, isXperiodic, xperiode, min_bdr_rad, max_bdr_rad, big_bdr)
    implicit none
    real(kind=8), dimension(:,:), intent(in)    :: positions   !< [in] positions of the objects (nbDIME,nbObjects)
    real(kind=8), dimension(:),   intent(in)    :: bdr_radii   !< [in] boundary radii of the objects
    real(kind=8),                 intent(in)    :: alert       !< [in] alert distance
    type(T_rough_container),      intent(inout) :: rough_list  !< [in,out] the list of rough contact found
    logical                                     :: isXperiodic !< [in] (optional) flag for periodic conditions
    real(kind=8)                                :: xperiode    !< [in] (optional) periode value
    !optional
    real(kind=8), optional                      :: min_bdr_rad !< [in] (optional) minimum boundary radius
    real(kind=8), optional                      :: max_bdr_rad !< [in] (optional) maximum boundary radius
    integer(kind=4), dimension(:), optional     :: big_bdr     !< [in] (optional) list of big boundaries indices
    !
    integer(kind=4) :: i , nb_objs, nb_big_bdr, errare, dime, nb_rough
    integer(kind=4) :: ibox1, ibox1cd, ibox1an, minibox1, maxibox1, old_maxibox1
    integer(kind=4) :: ibox2, ibox2cd, ibox2an, minibox2, maxibox2, old_maxibox2
    integer(kind=4) :: ibox3, ibox3cd, ibox3an, minibox3, maxibox3, old_maxibox3
    integer(kind=4) :: icdpop, ianpop, i_cd, i_an, maxpopul
    real(kind=8)    :: Lbox, lbox_1, norm, adist, min_radius, max_radius
    real(kind=8)    :: Xleft,Xright,Yleft,Yright,Zup,Zdown
    type(T_object)  :: rough_interaction

    logical         :: is_allocated_yet

    real(kind=8), dimension(:),allocatable :: position_tmp

    integer(kind=4), dimension(:), pointer :: i4

    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx

    character(len=100)                        :: cout
  
    i4 => null()
    c5 => null()
    r8 => null()
    cx => null()

    ! on suppose initialement qu'il ne sera pas necessaire de reallouer les tableaux which
    is_allocated_yet = .false.

    dime    = size(positions,1)
    nb_objs = size(positions,2)

    allocate(position_tmp(dime))
    position_tmp = 0.d0

    if (present(big_bdr)) then
      nb_big_bdr=size(big_bdr)
    else
      nb_big_bdr=0
    end if

    WRITE(cout,*) 'nb_big_bdr=', nb_big_bdr
    call logmes(cout)

    ! check size(bdr_radii) == nb_obj

    if( .not. present(min_bdr_rad) ) then
      min_radius = minval(bdr_radii)
    else
      min_radius = min_bdr_rad
    end if
    if( .not. present(max_bdr_rad) ) then
      ! if there's no big boundary, the maximal radius is computed using a standard FORTRAN function
      if (.not. present(big_bdr)) then
        max_radius = maxval(bdr_radii)
      ! else, the big boundaries have to be exlcuded
      else
        max_radius = -1.d24
        do i=1, nb_objs
          if (count(big_bdr == i) /= 0) cycle
          
          max_radius = max(bdr_radii(i), max_radius)
        end do
      end if
    else
      max_radius = max_bdr_rad
    end if

    Lbox   = 1.01D0*(2.D0*max_radius + alert)
    Lbox_1 = 1.D0/Lbox
    norm   = Lbox/min_radius
    
    maxpopul = (1+INT(norm))*(1+INT(norm))
    if( dime == 3) maxpopul = maxpopul*4*(1+INT(norm))

    maxpopul = MIN(maxpopul,nb_objs) ! for each box maxpopul is less than the total number of contactors


    ! Building boxes for quick sorting 
    
    ! Computing maximal boundary radius of contactors and largest box.
    !
    ! The computation of maximal radius of contactors allows to size a grid of boxes,
    ! the purpose of which is quick sorting of candidates to contact. The following
    ! sorting method is reasonnably fast. It is not really efficient when the 
    ! ratio between maximal and minimal radius is too large (>10), since the box 
    ! size becomes too large. Ratio less than 3 or even 5 are fair.
    ! The minimum radius of contactor is also used, in order to estimate the maximal 
    ! number of contactors per box.   
    ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
    ! Since the data file allows, for further generalization purposes, other 
    ! contactors than BDARY (the true boundary), extracting min_radius and 
    ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.
    
    Xleft =  1.D24; Xright = -1.D24
    Yleft =  1.D24; Yright = -1.D24
    if( dime == 3 ) then
      Zup = -1.D24; Zdown =  1.D24
    else
      Zup = 0.D0; Zdown = 0.D0
    end if
    
    ! if there's no big boundary, the maximal box boundaries are computed using standard FORTRAN function
    if (.not. present(big_bdr)) then
      Xleft  = minval(positions(1,:))
      Xright = maxval(positions(1,:))
      Yleft  = minval(positions(2,:))
      Yright = maxval(positions(2,:))
      if ( dime == 3 ) then
        Zup   = maxval(positions(3,:))
        Zdown = minval(positions(3,:))
      end if
    ! else, the big boundaries have to be exlcuded
    else
      do i=1, nb_objs
        if (count(big_bdr == i) /= 0) cycle
        
        Xleft  = MIN(positions(1, i), Xleft )
        Xright = MAX(positions(1, i), Xright)
        Yleft  = MIN(positions(2, i), Yleft )
        Yright = MAX(positions(2, i), Yright)
        if ( dime == 3 ) then
          Zdown = MIN(positions(3, i), Zdown )
          Zup   = MAX(positions(3, i), Zup   )
        end if
      end do
    end if

    !!\todo check for periodicity
    minibox1 = 1; maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
    minibox2 = 1; maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
    minibox3 = 1; maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)

    old_maxibox1 = size(box,dim=1)
    old_maxibox2 = size(box,dim=2)
    old_maxibox3 = size(box,dim=3)

    ! we try to allocate only if necessary
    errare = 0
    if( allocated(box) .and. &
       ( maxibox1 > old_maxibox1 .or. &
         maxibox2 > old_maxibox2 .or. &
         maxibox3 > old_maxibox3       ) ) then
      do ibox1=minibox1, old_maxibox1
        do ibox2=minibox2, old_maxibox2
          do ibox3=minibox3, old_maxibox3
            if ( associated(box(ibox1,ibox2,ibox3)%which) ) deallocate( box(ibox1,ibox2,ibox3)%which )
            nullify(box(ibox1,ibox2,ibox3)%which)
          end do
        end do
      end do
      deallocate(box)

      allocate(box(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)
      ! on indique qu'on devra reallouer les tableaux which
      is_allocated_yet =.true.

    else if (.not. allocated(box)) then
      allocate(box(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)
      ! on indique qu'on devra reallouer les tableaux which
      is_allocated_yet =.true.
    end if    

    if (errare /=0 ) then
      write(cout,'(A,I0,A,I0,A,I0)') ' minibox1=',minibox1,' minibox2=',minibox2,' minibox3=',minibox3
      write(cout,'(A,I0,A,I0,A,I0)') ' maxibox1=',maxibox1,' maxibox2=',maxibox2,' maxibox3=',maxibox3
      write(cout,'(A)') 'error allocating box'
      call faterr('rough_detection::boxes_method',cout)
    end if
    
    do ibox1 = minibox1, maxibox1
      do ibox2 = minibox2, maxibox2
        do ibox3 = minibox3, maxibox3
          box(ibox1,ibox2,ibox3)%popul=0
          ! si necessaire
          if (is_allocated_yet) then
             ! on alloue les tableaux which
             allocate(box(ibox1,ibox2,ibox3)%which(maxpopul),stat=errare)
             if( errare /=0 ) then
                call faterr('[boxes_method]','error in allocating box(1+maxibox1,1+maxibox2,1+maxibox3)%which')
             end if
          end if
          box(ibox1,ibox2,ibox3)%which=0
        end do
      end do
    end do
    
    ! filling boxes with contactors
    ! box(ibox1,ibox2,ibox3)%popul is the number of contactors into the box (ibox1,ibox2,ibox3)
    ! box(ibox1,ibox2,ibox3)%which(ipopul) is the rank of the contactor labelled ipopul in the box
  
    ! filling boxes
    do i = 1, nb_objs
      ! big boundaries are stored in no box
      if (present(big_bdr)) then
        if (count(big_bdr == i) /= 0) cycle
      end if

      ibox1 = 1 + INT((positions(1,i)-Xleft )*Lbox_1)
      ibox2 = 1 + INT((positions(2,i)-Yleft )*Lbox_1)
      if( dime == 3 ) then 
        ibox3 = 1 + INT((positions(3,i)-Zdown )*Lbox_1)
      else
        ibox3 = 1
      end if

      if( ibox1 < minibox1 .or. ibox1 > maxibox1 .or. &
          ibox2 < minibox2 .or. ibox2 > maxibox2 .or. &
          ibox3 < minibox3 .or. ibox3 > maxibox3) then
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
         write(cout,'(A8,I10,A13)') '  body  ', i, ' out of boxes'
         call faterr('rough_detection::boxes_method',cout)
103      format(1X,A10,1X,I5,1X,A10,1X,I5,1X,A10,1x,I5)
      end if
      box(ibox1,ibox2,ibox3)%popul = box(ibox1,ibox2,ibox3)%popul + 1
      box(ibox1,ibox2,ibox3)%which(box(ibox1,ibox2,ibox3)%popul) = i
    end do

    ! Detecting contacts; 
    ! contacts are being detected within a box and immediate surrounding boxes;  
    nb_rough = 0
    
    ! list of matches to examine creation
    ! we deallocate the linked list for the temporary storage of matches candidat/antagonist
    ! we allocate memory each a match candidat/antagonist is found
    do ibox1cd = minibox1, maxibox1  
      do ibox2cd = minibox2, maxibox2
        do ibox3cd = minibox3, maxibox3
          do icdpop = 1, box(ibox1cd,ibox2cd,ibox3cd)%popul
            i_cd = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
            ! box loop investigating antagonist
            do ibox1an = max(minibox1,ibox1cd-1), min(maxibox1,ibox1cd+1)                        
              do ibox2an = max(minibox2,ibox2cd-1), min(maxibox2,ibox2cd+1)                   
                do ibox3an = max(minibox3,ibox3cd-1), min(maxibox3,ibox3cd+1)                   
                  do ianpop = 1, box(ibox1an,ibox2an,ibox3an)%popul
                    i_an = box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                    if( i_an <= i_cd ) cycle
                    ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                    ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                    ! results might be different up to some non significant figures, but when comparing to
                    ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                    adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)

                    if( all( dabs(positions(:,i_cd)-positions(:,i_an)) <= adist ) ) then
                      nb_rough = nb_rough + 1
                      allocate(i4(3)); i4(1) = i_cd; i4(2) = i_an; i4(3) = 0
                      !print *,'rough interaction found between : ', i4
                      call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
                    end if

                  end do
                end do
              end do
            end do
          end do
        end do
      end do
    end do

    !vv&mr: periodic case
    if (isXperiodic)then
       do ibox1cd = maxibox1-1, maxibox1  
          do ibox2cd = minibox2, maxibox2
             do ibox3cd = minibox3, maxibox3
                do icdpop = 1, box(ibox1cd,ibox2cd,ibox3cd)%popul
                   i_cd = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   ! box loop investigating antagonist
                   do ibox1an = minibox1,minibox1+1
                      do ibox2an = max(minibox2,ibox2cd-1), min(maxibox2,ibox2cd+1)                   
                         do ibox3an = max(minibox3,ibox3cd-1), min(maxibox3,ibox3cd+1)                   
                            do ianpop = 1, box(ibox1an,ibox2an,ibox3an)%popul
                               i_an = box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                               
                               adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)
                               position_tmp(:) = positions(:,i_an)

                               position_tmp(1) = position_tmp(1) + xperiode

                               if( all( dabs(positions(:,i_cd)-position_tmp(:)) <= adist ) ) then

                                  nb_rough = nb_rough + 1
                                  
                                  allocate(i4(3)); i4(1) = i_cd; i4(2) = i_an; i4(3) = 1
                                  !print *,'rough interaction found between : ', i4
                                  call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
                               end if
                               
                            end do
                         end do
                      end do
                   end do
                end do
             end do
          end do
       end do
    end if

    ! todo check for periodic conditions
    ! big boundaries treatment
    do i=1, nb_big_bdr
      ! all big boundaries are antagonist
      i_an = big_bdr(i)

      do i_cd=1, nb_objs
        ! avoid auto-contact
        if (i_an == i_cd) cycle
        ! keep only only one interaction in the case of a big boundary/big boundary pair 
        if ((count(big_bdr == i_cd) /= 0) .and. i_an < i_cd ) cycle

        ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
        ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
        ! results might be different up to some non significant figures, but when comparing to
        ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
        adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)

        if( all( dabs(positions(:,i_cd)-positions(:,i_an)) <= adist ) ) then
          nb_rough = nb_rough + 1
          allocate(i4(2)); i4(1) = i_cd; i4(2) = i_an
          !print *,'rough interaction found between : ', i4
          call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
        end if
      end do
    end do 

    call close_container(rough_list)

  end subroutine boxes_method

  !-------------------------------------------------------------------------------------------------
  !am: defining functions necessary to use the linkedlist methods

  subroutine erase_data(data)

     implicit none

     type(list_data), pointer, intent(in) :: data

     ! nothing to do

  end subroutine erase_data

  subroutine display_data(data)

     implicit none

     type(list_data), pointer, intent(in) :: data

     print*, "i4=", data%i4 

  end subroutine display_data

  include "linkedlist_methods.f90"
  !-------------------------------------------------------------------------------------------------

  !> boxes method, using linked lists
  subroutine boxes_method_lists(positions, bdr_radii, alert, rough_list, min_bdr_rad, max_bdr_rad, big_bdr)
    implicit none
    real(kind=8), dimension(:,:), intent(in)    :: positions   !< [in] positions of the objects (nbDIME,nbObjects)
    real(kind=8), dimension(:),   intent(in)    :: bdr_radii   !< [in] boundary radii of the objects
    real(kind=8),                 intent(in)    :: alert       !< [in] alert distance
    type(T_rough_container),      intent(inout) :: rough_list  !< [in,out] the list of rough contact found
    real(kind=8), optional                      :: min_bdr_rad !< [in] (optional) minimum boundary radius
    real(kind=8), optional                      :: max_bdr_rad !< [in] (optional) maximum boundary radius
    integer(kind=4), dimension(:), optional     :: big_bdr     !< [in] (optional) list of big boundaries indices
    !
    integer(kind=4) :: i , nb_objs, nb_big_bdr, errare, dime, nb_rough
    integer(kind=4) :: ibox1, ibox1cd, ibox1an, minibox1, maxibox1, old_maxibox1
    integer(kind=4) :: ibox2, ibox2cd, ibox2an, minibox2, maxibox2, old_maxibox2
    integer(kind=4) :: ibox3, ibox3cd, ibox3an, minibox3, maxibox3, old_maxibox3
    integer(kind=4) :: i_cd, i_an
    real(kind=8)    :: Lbox, lbox_1, adist, min_radius, max_radius
    real(kind=8)    :: Xleft,Xright,Yleft,Yright,Zup,Zdown
    type(T_object)  :: rough_interaction

    type(list_data), pointer :: data
    type(linked_list), pointer :: cd_current, an_current

    !am: the following variable is used to insert objects at the end of the lists, so that interaction numbering will
    !    be the same using this implementation, or the standard one
    type(T_box_list), dimension(:, :, :), allocatable :: list_ends

    integer(kind=4), dimension(:), pointer :: i4

    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx
    character(len=100)                        :: cout
  
    i4 => null()
    c5 => null()
    r8 => null()
    cx => null()

    data => null()

    cd_current => null()
    an_current => null()

    dime    = size(positions,1)
    nb_objs = size(positions,2)

    if (present(big_bdr)) then
      nb_big_bdr=size(big_bdr)
    else
      nb_big_bdr=0
    end if

    WRITE(cout,*) 'nb_big_bdr=', nb_big_bdr
    call logmes(cout)

    ! check size(bdr_radii) == nb_obj

    if( .not. present(min_bdr_rad) ) then
      min_radius = minval(bdr_radii)
    else
      min_radius = min_bdr_rad
    end if
    if( .not. present(max_bdr_rad) ) then
      ! if there's no big boundary, the maximal radius is computed using a standard FORTRAN function
      if (.not. present(big_bdr)) then
        max_radius = maxval(bdr_radii)
      ! else, the big boundaries have to be exlcuded
      else
        max_radius = -1.d24
        do i=1, nb_objs
          if (count(big_bdr == i) /= 0) cycle
          
          max_radius = max(bdr_radii(i), max_radius)
        end do
      end if
    else
      max_radius = max_bdr_rad
    end if

    Lbox   = 1.01D0*(2.D0*max_radius + alert)
    Lbox_1 = 1.D0/Lbox

    ! Building boxes for quick sorting 
    
    ! Computing maximal boundary radius of contactors and largest box.
    !
    ! The computation of maximal radius of contactors allows to size a grid of boxes,
    ! the purpose of which is quick sorting of candidates to contact. The following
    ! sorting method is reasonnably fast. It is not really efficient when the 
    ! ratio between maximal and minimal radius is too large (>10), since the box 
    ! size becomes too large. Ratio less than 3 or even 5 are fair.
    ! The minimum radius of contactor is also used, in order to estimate the maximal 
    ! number of contactors per box.   
    ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
    ! Since the data file allows, for further generalization purposes, other 
    ! contactors than BDARY (the true boundary), extracting min_radius and 
    ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.
    
    Xleft =  1.D24; Xright = -1.D24
    Yleft =  1.D24; Yright = -1.D24
    if( dime == 3 ) then
      Zup = -1.D24; Zdown =  1.D24
    else
      Zup = 0.D0; Zdown = 0.D0
    end if
    
    ! if there's no big boundary, the maximal box boundaries are computed using standard FORTRAN function
    if (.not. present(big_bdr)) then
      Xleft  = minval(positions(1,:))
      Xright = maxval(positions(1,:))
      Yleft  = minval(positions(2,:))
      Yright = maxval(positions(2,:))
      if ( dime == 3 ) then
        Zup   = maxval(positions(3,:))
        Zdown = minval(positions(3,:))
      end if
    ! else, the big boundaries have to be exlcuded
    else
      do i=1, nb_objs
        if (count(big_bdr == i) /= 0) cycle
        
        Xleft  = MIN(positions(1, i), Xleft )
        Xright = MAX(positions(1, i), Xright)
        Yleft  = MIN(positions(2, i), Yleft )
        Yright = MAX(positions(2, i), Yright)
        if ( dime == 3 ) then
          Zdown = MIN(positions(3, i), Zdown )
          Zup   = MAX(positions(3, i), Zup   )
        end if
      end do
    end if

    !!\todo check for periodicity
    minibox1 = 1; maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
    minibox2 = 1; maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
    minibox3 = 1; maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)

    old_maxibox1 = size(box_lists,dim=1)
    old_maxibox2 = size(box_lists,dim=2)
    old_maxibox3 = size(box_lists,dim=3)

    ! we try to allocate only if necessary
    errare = 0
    if( allocated(box_lists) .and. &
       ( maxibox1 > old_maxibox1 .or. &
         maxibox2 > old_maxibox2 .or. &
         maxibox3 > old_maxibox3       ) ) then
      ! N.B. the linked lists were destroyed at the end of the last call of "boxes_method_sparse"
      deallocate(box_lists)

      allocate(box_lists(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)

      do ibox1=minibox1, maxibox1
        do ibox2=minibox2, maxibox2
          do ibox3=minibox3, maxibox3
            box_lists(ibox1,ibox2,ibox3)%list => null()
          end do
        end do
      end do

    else if (.not. allocated(box_lists)) then
      allocate(box_lists(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)

      do ibox1=minibox1, maxibox1
        do ibox2=minibox2, maxibox2
          do ibox3=minibox3, maxibox3
            box_lists(ibox1,ibox2,ibox3)%list => null()
          end do
        end do
      end do
    end if    

    ! prepare list_ends (memory allocation + initialisation)

    allocate(list_ends(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)

    do ibox1=minibox1, maxibox1
      do ibox2=minibox2, maxibox2
        do ibox3=minibox3, maxibox3
          list_ends(ibox1,ibox2,ibox3)%list => null()
        end do
      end do
    end do

    if (errare /=0 ) then
      write(cout,'(A,I0,A,I0,A,I0)') ' minibox1=',minibox1,' minibox2=',minibox2,' minibox3=',minibox3
      write(cout,'(A,I0,A,I0,A,I0)') ' maxibox1=',maxibox1,' maxibox2=',maxibox2,' maxibox3=',maxibox3
      write(cout,'(A)') 'error allocating box'
      call faterr('rough_detection::boxes_method_lists',cout)
    end if
 
    ! filling boxes with contactors
 
    ! filling boxes
    do i = 1, nb_objs
      ! big boundaries are stored in no box
      if (present(big_bdr)) then
        if (count(big_bdr == i) /= 0) cycle
      end if

      ibox1 = 1 + INT((positions(1,i)-Xleft )*Lbox_1)
      ibox2 = 1 + INT((positions(2,i)-Yleft )*Lbox_1)
      if( dime == 3 ) then 
        ibox3 = 1 + INT((positions(3,i)-Zdown )*Lbox_1)
      else
        ibox3 = 1
      end if

      if( ibox1 < minibox1 .or. ibox1 > maxibox1 .or. &
          ibox2 < minibox2 .or. ibox2 > maxibox2 .or. &
          ibox3 < minibox3 .or. ibox3 > maxibox3) then
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
         write(cout,'(A8,I10,A13)') '  body  ', i, ' out of boxes'
         call faterr('rough_detection::boxes_method_lists',cout)
103      format(1X,A10,1X,I5,1X,A10,1X,I5,1X,A10,1x,I5)
      end if

      allocate(data)
      data%i4 = i

      if (.not. associated(box_lists(ibox1,ibox2,ibox3)%list)) then
         ! create a new list
         call list_create( box_lists(ibox1,ibox2,ibox3)%list, data )

         ! initialize the end of the new list
         list_ends(ibox1,ibox2,ibox3)%list => box_lists(ibox1,ibox2,ibox3)%list
      else
         !am: insertion at the head of the list
         !call list_insert_head( box_lists(ibox1,ibox2,ibox3)%list, data )

         !am: insertion at the end of the list

         ! insert the new element
         call list_insert( list_ends(ibox1,ibox2,ibox3)%list, data )
         ! update the end of the list
         list_ends(ibox1,ibox2,ibox3)%list => list_next( list_ends(ibox1,ibox2,ibox3)%list )
      end if
    end do

    ! Detecting contacts; 
    ! contacts are being detected within a box and immediate surrounding boxes;  
    nb_rough = 0
    
    ! list of matches to examine creation
    ! we deallocate the linked list for the temporary storage of matches candidat/antagonist
    ! we allocate memory each a match candidat/antagonist is found
    do ibox1cd = minibox1, maxibox1  
      do ibox2cd = minibox2, maxibox2
        do ibox3cd = minibox3, maxibox3
          cd_current => box_lists(ibox1cd,ibox2cd,ibox3cd)%list
          do while(associated(cd_current))
            data => list_get_data( cd_current )
            i_cd = data%i4
            ! box loop investigating antagonist
            do ibox1an = max(minibox1,ibox1cd-1), min(maxibox1,ibox1cd+1)                        
              do ibox2an = max(minibox2,ibox2cd-1), min(maxibox2,ibox2cd+1)                   
                do ibox3an = max(minibox3,ibox3cd-1), min(maxibox3,ibox3cd+1)                   
                  an_current => box_lists(ibox1an,ibox2an,ibox3an)%list
                  do while(associated(an_current))
                    data => list_get_data( an_current )
                    i_an = data%i4
                    if( i_an > i_cd ) then
                      ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                      ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                      ! results might be different up to some non significant figures, but when comparing to
                      ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                      adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)

                      if( all( dabs(positions(:,i_cd)-positions(:,i_an)) <= adist ) ) then
                        nb_rough = nb_rough + 1
                        allocate(i4(2)); i4(1) = i_cd; i4(2) = i_an
                        !print *,'rough interaction found between : ', i4
                        call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
                      end if
                    end if

                    an_current => list_next( an_current )
                  end do
                end do
              end do
            end do
            cd_current => list_next( cd_current )
          end do
        end do
      end do
    end do

    ! big boundaries treatment
    do i=1, nb_big_bdr
      ! all big boundaries are antagonist
      i_an = big_bdr(i)

      do i_cd=1, nb_objs
        ! avoid auto-contact
        if (i_an == i_cd) cycle
        ! keep only only one interaction in the case of a big boundary/big boundary pair 
        if ((count(big_bdr == i_cd) /= 0) .and. i_an < i_cd ) cycle

        ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
        ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
        ! results might be different up to some non significant figures, but when comparing to
        ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
        adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)

        if( all( dabs(positions(:,i_cd)-positions(:,i_an)) <= adist ) ) then
          nb_rough = nb_rough + 1
          allocate(i4(2)); i4(1) = i_cd; i4(2) = i_an
          !print *,'rough interaction found between : ', i4
          call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
        end if
      end do
    end do 

    call close_container(rough_list)

    ! delete the lists of boundary indices in each box
    do ibox1=minibox1, maxibox1
      do ibox2=minibox2, maxibox2
        do ibox3=minibox3, maxibox3
          if ( associated(box_lists(ibox1,ibox2,ibox3)%list) ) then
             ! destroy the list
             call list_destroy( box_lists(ibox1,ibox2,ibox3)%list )
             ! nullify the head of the list
             box_lists(ibox1,ibox2,ibox3)%list => null()

             ! nullify the end of the list
             list_ends(ibox1,ibox2,ibox3)%list => null()
          end if
        end do
      end do
    end do

    ! dellocate memory used to store the end of the lists
    deallocate(list_ends)

  end subroutine boxes_method_lists

  !> boxes method, using linked lists (sparse storage of the boxes) and a parse storage of the boxes matrix
  !> newer implementation : an array is used to compute the list of not empty boxes, and an optimized mzthod is used
  !> to loop on the not empty candidate boxes during the detection
  subroutine boxes_method_sparse(positions, bdr_radii, alert, rough_list, min_bdr_rad, max_bdr_rad, big_bdr)
    implicit none
    real(kind=8), dimension(:,:), intent(in)    :: positions   !< [in] positions of the objects (nbDIME,nbObjects)
    real(kind=8), dimension(:),   intent(in)    :: bdr_radii   !< [in] boundary radii of the objects
    real(kind=8),                 intent(in)    :: alert       !< [in] alert distance
    type(T_rough_container),      intent(inout) :: rough_list  !< [in,out] the list of rough contact found
    real(kind=8), optional                      :: min_bdr_rad !< [in] (optional) minimum boundary radius
    real(kind=8), optional                      :: max_bdr_rad !< [in] (optional) maximum boundary radius
    integer(kind=4), dimension(:), optional     :: big_bdr     !< [in] (optional) list of big boundaries indices
    !
    integer(kind=4) :: i , nb_objs, nb_big_bdr, errare, dime, nb_rough
    integer(kind=4) :: ibox1, ibox1cd, ibox1an, minibox1, maxibox1, old_maxibox1
    integer(kind=4) :: ibox2, ibox2cd, ibox2an, minibox2, maxibox2, old_maxibox2
    integer(kind=4) :: ibox3, ibox3cd, ibox3an, minibox3, maxibox3, old_maxibox3
    integer(kind=4) :: i_cd, i_an
    real(kind=8)    :: Lbox, lbox_1, adist, min_radius, max_radius
    real(kind=8)    :: Xleft,Xright,Yleft,Yright,Zup,Zdown
    type(T_object)  :: rough_interaction

    integer(kind=4) :: ibox ! index of a box
    integer(kind=4) :: old_nb_boxes ! old number of boxes containing contactors
    integer(kind=4) :: nb_boxes ! number of boxes containing contactors
    integer(kind=4), dimension(:), allocatable :: l_boxes ! list of indices of boxes containing contactors
    type(list_data), pointer :: data
    integer(kind=4) :: ind ! index of the current box index in box_indices
    integer(kind=4) :: tmp(1) ! variable used to compute ind
    integer(kind=4) :: ibox_cd, ibox_an
    integer(kind=4) :: ind_cd, ind_an
    type(linked_list), pointer :: cd_current, an_current

    integer(kind=4) :: r ! variable used to compute ibox1, ibox2, ibox3 from ibox

    !am: the following variable is used to insert objects at the end of the lists, so that interaction numbering will
    !    be the same using this implementation, or the standard one
    type(T_box_list), dimension(:), allocatable :: list_ends

    integer(kind=4), dimension(:), pointer :: i4

    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx

    character(len=100)                        :: cout
  
    i4 => null()
    c5 => null()
    r8 => null()
    cx => null()

    data => null()

    cd_current => null()
    an_current => null()

    dime    = size(positions,1)
    nb_objs = size(positions,2)

    if (present(big_bdr)) then
      nb_big_bdr=size(big_bdr)
    else
      nb_big_bdr=0
    end if

    WRITE(cout,*) 'nb_big_bdr=', nb_big_bdr
    call logmes(cout)

    ! check size(bdr_radii) == nb_obj

    if( .not. present(min_bdr_rad) ) then
      min_radius = minval(bdr_radii)
    else
      min_radius = min_bdr_rad
    end if
    if( .not. present(max_bdr_rad) ) then
      ! if there's no big boundary, the maximal radius is computed using a standard FORTRAN function
      if (.not. present(big_bdr)) then
        max_radius = maxval(bdr_radii)
      ! else, the big boundaries have to be exlcuded
      else
        max_radius = -1.d24
        do i=1, nb_objs
          if (count(big_bdr == i) /= 0) cycle
          
          max_radius = max(bdr_radii(i), max_radius)
        end do
      end if
    else
      max_radius = max_bdr_rad
    end if

    Lbox   = 1.01D0*(2.D0*max_radius + alert)
    Lbox_1 = 1.D0/Lbox

    ! Building boxes for quick sorting 
    
    ! Computing maximal boundary radius of contactors and largest box.
    !
    ! The computation of maximal radius of contactors allows to size a grid of boxes,
    ! the purpose of which is quick sorting of candidates to contact. The following
    ! sorting method is reasonnably fast. It is not really efficient when the 
    ! ratio between maximal and minimal radius is too large (>10), since the box 
    ! size becomes too large. Ratio less than 3 or even 5 are fair.
    ! The minimum radius of contactor is also used, in order to estimate the maximal 
    ! number of contactors per box.   
    ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
    ! Since the data file allows, for further generalization purposes, other 
    ! contactors than BDARY (the true boundary), extracting min_radius and 
    ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.
    
    Xleft =  1.D24; Xright = -1.D24
    Yleft =  1.D24; Yright = -1.D24
    if( dime == 3 ) then
      Zup = -1.D24; Zdown =  1.D24
    else
      Zup = 0.D0; Zdown = 0.D0
    end if
    
    ! if there's no big boundary, the maximal box boundaries are computed using standard FORTRAN function
    if (.not. present(big_bdr)) then
      Xleft  = minval(positions(1,:))
      Xright = maxval(positions(1,:))
      Yleft  = minval(positions(2,:))
      Yright = maxval(positions(2,:))
      if ( dime == 3 ) then
        Zup   = maxval(positions(3,:))
        Zdown = minval(positions(3,:))
      end if
    ! else, the big boundaries have to be exlcuded
    else
      do i=1, nb_objs
        if (count(big_bdr == i) /= 0) cycle
        
        Xleft  = MIN(positions(1, i), Xleft )
        Xright = MAX(positions(1, i), Xright)
        Yleft  = MIN(positions(2, i), Yleft )
        Yright = MAX(positions(2, i), Yright)
        if ( dime == 3 ) then
          Zdown = MIN(positions(3, i), Zdown )
          Zup   = MAX(positions(3, i), Zup   )
        end if
      end do
    end if

    !!\todo check for periodicity
    minibox1 = 1; maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
    minibox2 = 1; maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
    minibox3 = 1; maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)

    ! listing boxes containing contactors
    nb_boxes = 0

    ! N.B. in the worst case, each object is alone in one box
    allocate(l_boxes(nb_objs))

    ! N.B. a box index is greater or equal to 1
    l_boxes = 0

    ! listing boxes
    do i = 1, nb_objs
      ! big boundaries are stored in no box
      if (present(big_bdr)) then
        if (count(big_bdr == i) /= 0) cycle
      end if

      ibox1 = 1 + INT((positions(1,i)-Xleft )*Lbox_1)
      ibox2 = 1 + INT((positions(2,i)-Yleft )*Lbox_1)
      if( dime == 3 ) then 
        ibox3 = 1 + INT((positions(3,i)-Zdown )*Lbox_1)
      else
        ibox3 = 1
      end if

      if( ibox1 < minibox1 .or. ibox1 > maxibox1 .or. &
          ibox2 < minibox2 .or. ibox2 > maxibox2 .or. &
          ibox3 < minibox3 .or. ibox3 > maxibox3) then
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
         write(cout,'(A8,I10,A13)') '  body  ', i, ' out of boxes'
         call faterr('rough_detection::boxes_method_sparse',cout)
103      format(1X,A10,1X,I5,1X,A10,1X,I5,1X,A10,1x,I5)
      end if

      ! N.B. maxibox2 is assumed to be the number of boxes on the Y-axis, and
      ! maxibox3 is assumed to be the number of boxes on the Z-axis 
      ibox = (ibox1 - 1)*maxibox2*maxibox3 + (ibox2 - 1)*maxibox3 + ibox3

      ! insert the current box index in the list of box indices

      ! only new box indices are inserted
      if (any(l_boxes == ibox)) cycle

      ! insert the current box index at the end of the list of box indices
      !nb_boxes = nb_boxes + 1
      !l_boxes(nb_boxes) = ibox

      !am: insert the current box index in the sorted list of boxes permits to preserve
      !    the sweeping order of the boxes used in the standard method

      ! insert the current box index in the sorted list of box indices

      ! the first one is inserted at the head of the list
      if (nb_boxes == 0) then
        nb_boxes = 1
        l_boxes(1) = ibox
        cycle
      end if

      ! if the new box index is greater than box indices in the list, 
      ! it is inserted at the end of the list
      if (ibox > l_boxes(nb_boxes)) then
        nb_boxes = nb_boxes + 1
        l_boxes(nb_boxes) = ibox
        cycle
      end if

      ! from here, we know that the current box index have to be inserted in the list

      ! compute the index where to insert the current box index
      tmp = minloc(l_boxes(1:nb_boxes), l_boxes(1:nb_boxes) > ibox)
      ind = tmp(1)

      ! insert the current box index in the list

      ! shift the box indices greater than the current box index 
      nb_boxes = nb_boxes + 1
      l_boxes(ind + 1:nb_boxes) = l_boxes(ind:nb_boxes - 1)
      ! insert the current box index
      l_boxes(ind) = ibox
    end do

    ! storing not empty boxes

    old_nb_boxes = size(box_indices)

    ! we try to allocate only if necessary
    errare = 0
    ! N.B. box_indices and box_vec are assumed to be allocated at the same time
    if( allocated(box_indices) .and. nb_boxes > old_nb_boxes ) then
      ! N.B. the linked lists were destroyed at the end of the last call of "boxes_method_sparse"
      deallocate(box_indices)
      deallocate(box_vec)

      allocate(box_indices(nb_boxes), stat=errare)
      allocate(box_vec(nb_boxes), stat=errare)

      do i=1, nb_boxes
        box_vec(i)%list => null()
      end do
    else if (.not. allocated(box_indices)) then
      allocate(box_indices(nb_boxes), stat=errare)
      allocate(box_vec(nb_boxes), stat=errare)

      do i=1, nb_boxes
        box_vec(i)%list => null()
      end do
    end if    

    ! prepare list_ends (memory allocation + initialisation)

    allocate(list_ends(nb_boxes), stat=errare)

    do i=1, nb_boxes
      list_ends(i)%list => null()
    end do

    if (errare /=0 ) then
      write(cout,'(A,I0)') 'nb_boxes=', nb_boxes
      write(cout,'(A)') 'error allocating box_indices or box_vec'
      call faterr('rough_detections::boxes_method_sparse',cout)
    end if

    ! filling box indices 
    box_indices(1:nb_boxes) = l_boxes(1:nb_boxes)

    ! destroying l_boxes
    deallocate(l_boxes)

    ! filling boxes with contactors
 
    ! filling boxes
    do i = 1, nb_objs
      ! big boundaries are stored in no box
      if (present(big_bdr)) then
        if (count(big_bdr == i) /= 0) cycle
      end if

      ibox1 = 1 + INT((positions(1,i)-Xleft )*Lbox_1)
      ibox2 = 1 + INT((positions(2,i)-Yleft )*Lbox_1)
      if( dime == 3 ) then 
        ibox3 = 1 + INT((positions(3,i)-Zdown )*Lbox_1)
      else
        ibox3 = 1
      end if

      if( ibox1 < minibox1 .or. ibox1 > maxibox1 .or. &
          ibox2 < minibox2 .or. ibox2 > maxibox2 .or. &
          ibox3 < minibox3 .or. ibox3 > maxibox3) then
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
         write(cout,'(A8,I10,A13)') '  body  ', i, ' out of boxes'
         call faterr('rough_detections::boxes_method_sparse',cout)
      end if

      ! N.B. maxibox2 is assumed to be the number of boxes on the Y-axis, and
      ! maxibox3 is assumed to be the number of boxes on the Z-axis 
      ibox = (ibox1 - 1)*maxibox2*maxibox3 + (ibox2 - 1)*maxibox3 + ibox3

      allocate(data)
      data%i4 = i

      ! paranoid check
      !if (.not. any(box_indices == ibox)) ...

      ! compute the index of ibox in box_indices
      tmp = maxloc(box_indices, box_indices == ibox)
      ind = tmp(1)

      if (.not. associated(box_vec(ind)%list)) then
         ! create a new list
         call list_create( box_vec(ind)%list, data )

         ! initialize the end of the new list
         list_ends(ind)%list => box_vec(ind)%list
      else
         !am: insertion at the head of the list
         !call list_insert_head( box_vec(ind)%list, data )

         !am: insertion at the end of the list

         ! insert the new element
         call list_insert( list_ends(ind)%list, data )
         ! update the end of the list
         list_ends(ind)%list => list_next( list_ends(ind)%list )
      end if
    end do

    ! Detecting contacts; 
    ! contacts are being detected within a box and immediate surrounding boxes;  
    nb_rough = 0

    ! list of matches to examine creation
    ! we deallocate the linked list for the temporary storage of matches candidat/antagonist
    ! we allocate memory each a match candidat/antagonist is found

    ! for each not empty candidate box
    do ind_cd = 1, nb_boxes
      ! getting candidate box index
      ibox_cd = box_indices(ind_cd)       

      ! computing (ibox1cd, ibox2cd, ibox3cd) satisfaying:
      ! ibox_cd = (ibox1cd - 1)*maxibox2*maxibox3 + (ibox2cd - 1)*maxibox3 + ibox3cd)
      ibox1cd = 1 + (ibox_cd - 1) / (maxibox2*maxibox3)

      r = mod(ibox_cd - 1, maxibox2*maxibox3)
      
      ibox2cd = 1 + r / maxibox3
      ibox3cd = 1 + mod(r, maxibox3)

      ! for each candiodate objet in the current candidate box
      cd_current => box_vec(ind_cd)%list
      do while(associated(cd_current))
        data => list_get_data( cd_current )
        i_cd = data%i4
        ! box loop investigating antagonist
        do ibox1an = max(minibox1,ibox1cd-1), min(maxibox1,ibox1cd+1)                        
          do ibox2an = max(minibox2,ibox2cd-1), min(maxibox2,ibox2cd+1)                   
            do ibox3an = max(minibox3,ibox3cd-1), min(maxibox3,ibox3cd+1)                   
              ! compute index of the current antagonist box

              ! N.B. maxibox2 is assumed to be the number of boxes on the Y-axis, and
              ! maxibox3 is assumed to be the number of boxes on the Z-axis 
              ibox_an = (ibox1an - 1)*maxibox2*maxibox3 + (ibox2an - 1)*maxibox3 + ibox3an

              ! if this box is empty, we consider the next one         
              if (.not. any(box_indices == ibox_an)) cycle

              ! compute the index of ibox_an in box_indices
              tmp = maxloc(box_indices, box_indices == ibox_an)
              ind_an = tmp(1)

              ! for each candiodate objet in the current antagonist box
              an_current => box_vec(ind_an)%list
              do while(associated(an_current))
                data => list_get_data( an_current )
                i_an = data%i4
                if( i_an > i_cd ) then
                  ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                  ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                  ! results might be different up to some non significant figures, but when comparing to
                  ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                  adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)

                  if( all( dabs(positions(:,i_cd)-positions(:,i_an)) <= adist ) ) then
                    nb_rough = nb_rough + 1
                    allocate(i4(2)); i4(1) = i_cd; i4(2) = i_an
                    !print *,'rough interaction found between : ', i4
                    call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
                  end if
                end if

                an_current => list_next( an_current )
              end do
            end do
          end do
        end do
        cd_current => list_next( cd_current )
      end do
    end do

    ! big boundaries treatment
    do i=1, nb_big_bdr
      ! all big boundaries are antagonist
      i_an = big_bdr(i)

      do i_cd=1, nb_objs
        ! avoid auto-contact
        if (i_an == i_cd) cycle
        ! keep only only one interaction in the case of a big boundary/big boundary pair 
        if ((count(big_bdr == i_cd) /= 0) .and. i_an < i_cd ) cycle

        ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
        ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
        ! results might be different up to some non significant figures, but when comparing to
        ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
        adist = 0.1005D+01 * alert + bdr_radii(i_cd) + bdr_radii(i_an)

        if( all( dabs(positions(:,i_cd)-positions(:,i_an)) <= adist ) ) then
          nb_rough = nb_rough + 1
          allocate(i4(2)); i4(1) = i_cd; i4(2) = i_an
          !print *,'rough interaction found between : ', i4
          call add_object_to_container(rough_list, nb_rough, c5, i4, r8, cx)
        end if
      end do
    end do 

    call close_container(rough_list)

    ! delete the lists of boundary indices in each box
    do i=1, nb_boxes
      if ( associated(box_vec(i)%list) ) then
         ! destroy the list
         call list_destroy( box_vec(i)%list )
         ! nullify the head of the list
         box_vec(i)%list => null()

         ! nullify the end of the list
         list_ends(i)%list => null()
      end if
    end do

    ! dellocate memory used to store the end of the lists
    deallocate(list_ends)

  end subroutine boxes_method_sparse

  !> \brief free memory allocated within the module
  subroutine clean_module
    implicit none
    integer(kind=4) :: i, j, k
    
    if( allocated(box) ) then
      do i = 1, size(box,3)
        do j = 1, size(box,2)
          do k = 1, size(box,1)
            if( associated(box(k,j,i)%which) ) deallocate(box(k,j,i)%which)
          end do
        end do
      end do

      deallocate(box)
    end if

    ! N.B. the linked lists were destroyed at the end of the last call of "boxes_method_sparse"
    if (allocated(box_lists)) deallocate(box_lists)

    ! N.B. the linked lists were destroyed at the end of the last call of "boxes_method_sparse_vec"
    if (allocated(box_indices)) deallocate(box_indices)
    if (allocated(box_vec)) deallocate(box_vec)
  end subroutine clean_module

end module rough_detections
