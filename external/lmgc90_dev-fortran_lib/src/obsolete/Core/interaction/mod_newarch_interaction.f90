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

!> define the interactions handled by the interaction_handler
module interaction

  use overall

  implicit none

  private

  !------------------------------------------------------------------------  
  type, public :: T_interaction !< Interaction type

    integer(kind=4) :: rank
    integer(kind=4) :: cdan   !< type id of contact
    integer(kind=4) :: icdan  !< index of the interaction

    integer(kind=4) :: icdent !< entity index of candidate
    integer(kind=4) :: ianent !< entity index of antagonist

    integer(kind=4) :: icdbdy !< body index of candidate
    integer(kind=4) :: ianbdy !< body index of antagonist

    integer(kind=4) :: icdtyp !< contactor type of candidate
    integer(kind=4) :: iantyp !< contactor type of antagonist

    integer(kind=4) :: icdtac !< contactor index of candidate
    integer(kind=4) :: iantac !< contactor index of antagonist

    integer(kind=4) :: iadj     !< index of adjacent
    integer(kind=4) :: isee     !< index of see type
    integer(kind=4) :: lawnb    !< index of contact law
    integer(kind=4) :: i_law    !< contact law type
    integer(kind=4) :: periodic !< 1 if periodic contact, else 0

    real(kind=8), dimension(:), allocatable :: vlBEGIN !< local relative velocity at beginnig
    real(kind=8), dimension(:), allocatable :: vl      !< local relative velocity
    real(kind=8), dimension(:), allocatable :: rl      !< local reaction

    real(kind=8), dimension(:), allocatable :: vfree
    real(kind=8), dimension(:), allocatable :: covfree
    real(kind=8), dimension(:), allocatable :: corl

    real(kind=8) :: gapTTbegin !< gap at predicted time (1-theta)*h
    real(kind=8) :: gapTT      !< gap at predicted time (1-theta)*h + h

    real(kind=8), dimension(:)  , allocatable :: coor !< contact locus
    
    real(kind=8), dimension(:,:), allocatable :: uc   !< local frame
    real(kind=8), dimension(:,:), allocatable :: Gcd  !< from local frame to candidat
    real(kind=8), dimension(:,:), allocatable :: Gan  !< from local frame to antagonist

    character(len=5) :: status      !< status at end of time step
    character(len=5) :: statusBEGIN !< status at beginning of time step... what status ?
    character(len=5) :: statuscheck !< statuscheck

    real(kind=8), dimension(:,:), allocatable :: W   !< Delassus matrix
    real(kind=8), dimension(:)  , allocatable :: WW  !< auxiliary diagonal Delassus matrix
    real(kind=8), dimension(:)  , allocatable :: invW!< inverse of WW
    real(kind=8)                              :: det !< det of Delassus matrix
    real(kind=8)                              :: rot !< tangential something
    real(kind=8)                              :: ron !< normal something

    character(len=5) :: forecast !< set to 'acton' (reactions have to be computed)
    real(kind=8)     :: fric  ! this(ik)%fric; friction coefficient for contact ik
    real(kind=8)     :: forward, backward 
    integer(kind=4)  :: ivnish,iskip !ivnish = inoctc, what about ron, rot ?
    integer(kind=4)  :: nbadj,istart
    integer(kind=4), dimension(:), allocatable :: adjjl

    integer(kind=4) :: nb_internal !< do not know
    real(kind=8), dimension(max_internal_tact) :: internal !< do not know
    real(kind=8) :: ws

  end type T_interaction
  
  ! needed for compatibility with current arch
  type T_ptr_interaction 
    type(T_interaction), pointer :: inter => null()
  end type T_ptr_interaction

  type, public :: T_face2face

    integer(kind=4) :: cdan

    integer(kind=4) :: icdtac
    integer(kind=4) :: iantac

    integer(kind=4) :: isee

    integer(kind=4) :: periodic

    integer(kind=4) :: nb_ctc

    type(T_ptr_interaction), dimension(:), allocatable :: ctc

  end type T_face2face

  public initialize_interactions

  public new_interaction    , &
         erase_interaction  , &
         close_interaction  , &
         open_interaction   , &
         set_rank, get_rank , &
         display_interaction

  contains

  !> \brief Empty function for compatibility with current arch
  subroutine initialize_interactions(f2f, nb_ctc)
    implicit none
    !> f2f to initialize
    type(T_face2face), intent(inout) :: f2f
    !> number of contacts for this f2f
    integer(kind=4)  , intent(in)    :: nb_ctc

  end subroutine

  !> \brief interaction constructor \n 
  !> default or copy 
  function new_interaction(other_int)
    implicit none
    type(T_interaction), optional, intent(in) :: other_int       !< [in] (optional) interaction to copy from
    type(T_interaction), pointer              :: new_interaction !< [return] pointer on a new interaction
    !
    integer(kind=4) :: s1, s2
 
    ! allocation of the returned interaction
    new_interaction => null()
    allocate(new_interaction)
      
    if (present(other_int)) then
      new_interaction%rank   = other_int%rank
      new_interaction%cdan   = other_int%cdan
      new_interaction%icdan  = other_int%icdan

      new_interaction%icdent = other_int%icdent
      new_interaction%ianent = other_int%ianent

      new_interaction%iadj   = other_int%iadj
      new_interaction%isee   = other_int%isee
      new_interaction%lawnb  = other_int%lawnb

      new_interaction%gapTTbegin  = other_int%gapTTbegin
      new_interaction%gapTT       = other_int%gapTT

      new_interaction%status      = other_int%status
      new_interaction%statusBEGIN = other_int%statusBEGIN
      new_interaction%statuscheck = other_int%statuscheck

      if( allocated(other_int%vlBEGIN) ) then
        s1 = size(other_int%vlBEGIN)
        allocate(new_interaction%vlBEGIN(s1))
        new_interaction%vlBEGIN(1:s1) = other_int%vlBEGIN(1:s1)
      end if
      if( allocated(other_int%vl) ) then
        s1 = size(other_int%vl)
        allocate(new_interaction%vl(s1))
        new_interaction%vl(1:s1) = other_int%vl(1:s1)
      end if
      if( allocated(other_int%rl) ) then
        s1 = size(other_int%rl)
        allocate(new_interaction%rl(s1))
        new_interaction%rl(1:s1) = other_int%rl(1:s1)
      end if
      if( allocated(other_int%vfree) ) then
        s1 = size(other_int%vfree)
        allocate(new_interaction%vfree(s1))
        new_interaction%vfree(1:s1) = other_int%vfree(1:s1)
      end if
      if( allocated(other_int%covfree) ) then
        s1 = size(other_int%covfree)
        allocate(new_interaction%covfree(s1))
        new_interaction%covfree(1:s1) = other_int%covfree(1:s1)
      end if
      if( allocated(other_int%corl) ) then
        s1 = size(other_int%corl)
        allocate(new_interaction%corl(s1))
        new_interaction%corl(1:s1) = other_int%corl(1:s1)
      end if
      if( allocated(other_int%W) ) then
        s1 = size(other_int%W,1)
        s2 = size(other_int%W,2)
        allocate(new_interaction%W(s1,s2))
        new_interaction%W(1:s1,1:s2) = other_int%W(1:s1,1:s2)
      end if
      if( allocated(other_int%WW) ) then
        s1 = size(other_int%WW)
        allocate(new_interaction%WW(s1))
        new_interaction%WW(1:s1) = other_int%WW(1:s1)
      end if
      if( allocated(other_int%invW) ) then
        s1 = size(other_int%invW)
        allocate(new_interaction%invW(s1))
        new_interaction%invW(1:s1) = other_int%invW(1:s1)
      end if

      new_interaction%det      = other_int%det
      new_interaction%forecast = other_int%forecast
      new_interaction%fric     = other_int%fric
      new_interaction%forward  = other_int%forward
      new_interaction%backward = other_int%backward
      new_interaction%ivnish   = other_int%ivnish
      new_interaction%iskip    = other_int%iskip
      new_interaction%istart   = other_int%istart
      new_interaction%nbadj    = other_int%nbadj

      if( allocated(other_int%adjjl) ) then
        s1 = size(other_int%adjjl)
        allocate(new_interaction%adjjl(s1))
        new_interaction%adjjl(1:s1) = other_int%adjjl(1:s1)
      end if

      new_interaction%nb_internal                   = other_int%nb_internal
      new_interaction%internal(1:max_internal_tact) = other_int%internal(1:max_internal_tact)

    else

      new_interaction%rank   = 0
      new_interaction%cdan   = 0
      new_interaction%icdan  = 0

      new_interaction%icdent = 0
      new_interaction%ianent = 0

      new_interaction%iadj   = 0
      new_interaction%isee   = 0
      new_interaction%lawnb  = 0

      new_interaction%gapTTbegin  = 0.D0
      new_interaction%gapTT       = 0.D0

      new_interaction%status      = 'nknow'
      new_interaction%statusBEGIN = 'nknow'
      new_interaction%statuscheck = 'nknow'

      allocate(new_interaction%coor(nbDIME)) 
      allocate(new_interaction%uc(nbDIME,nbDIME))
      if( nbDIME == 2 ) then
        allocate(new_interaction%Gcd(nbDIME,1))
        allocate(new_interaction%Gan(nbDIME,1))
      else
        allocate(new_interaction%Gcd(nbDIME,nbDIME))
        allocate(new_interaction%Gan(nbDIME,nbDIME))
      end if
      allocate(new_interaction%vlBEGIN(nbDIME))
      allocate(new_interaction%vl(nbDIME))
      allocate(new_interaction%rl(nbDIME))
      allocate(new_interaction%vfree(nbDIME))
      allocate(new_interaction%covfree(nbDIME))
      allocate(new_interaction%corl(nbDIME))
      allocate(new_interaction%W(nbDIME,nbDIME))
      allocate(new_interaction%WW(nbDIME))
      allocate(new_interaction%invW(nbDIME))

      new_interaction%coor(1:nbDIME)         = 0.D0
      new_interaction%uc(1:nbDIME,1:nbDIME)  = 0.D0
      new_interaction%Gcd(1:nbDIME,:)        = 0.D0
      new_interaction%Gan(1:nbDIME,:)        = 0.D0
      new_interaction%vlBEGIN(1:nbDIME)      = 0.D0
      new_interaction%vl(1:nbDIME)           = 0.D0
      new_interaction%rl(1:nbDIME)           = 0.D0
      new_interaction%vfree(1:nbDIME)        = 0.D0
      new_interaction%covfree(1:nbDIME)      = 0.D0
      new_interaction%corl(1:nbDIME)         = 0.D0
      new_interaction%W(1:nbDIME,1:nbDIME)   = 0.D0
      new_interaction%WW(1:nbDIME)           = 0.D0
      new_interaction%invW(1:nbDIME)         = 0.D0

      if( nbDIME == 3 ) new_interaction%W(3,3) = 1.D0
      new_interaction%W(2,2)   = 1.D0
      new_interaction%W(1,1)   = 1.D0
      new_interaction%det      = 1.D0
      new_interaction%forecast = 'acton'
      new_interaction%fric     = 0.D0
      new_interaction%forward  = 0.D0
      new_interaction%backward = 0.D0
      new_interaction%ivnish   = 0
      new_interaction%iskip    = 1
      new_interaction%istart   = 0
      new_interaction%nbadj    = 0

      !nothing to do yet
      !allocate(new_interaction%adjjl(s1))

      new_interaction%nb_internal = 0
      new_interaction%internal(1:max_internal_tact) = 0.D0


    endif
  end function

  !> \brief erase an interaction
  subroutine erase_interaction(interaction)
    implicit none
    type(T_interaction), intent(inout) :: interaction !< [in,out] erase the content of an interaction

    interaction%rank   = 0
    interaction%cdan   = 0
    interaction%icdan  = 0

    interaction%icdent = 0
    interaction%ianent = 0

    interaction%iadj   = 0
    interaction%isee   = 0
    interaction%lawnb  = 0

    interaction%gapTTbegin  = 0.D0
    interaction%gapTT       = 0.D0

    interaction%status      = 'nknow'
    interaction%statusBEGIN = 'nknow'
    interaction%statuscheck = 'nknow'

    if( allocated(interaction%vlBEGIN) ) then
      deallocate(interaction%vlBEGIN)
    end if
    if( allocated(interaction%vl) ) then
      deallocate(interaction%vl)
    end if
    if( allocated(interaction%rl) ) then
      deallocate(interaction%rl)
    end if
    if( allocated(interaction%vfree) ) then
      deallocate(interaction%vfree)
    end if
    if( allocated(interaction%covfree) ) then
      deallocate(interaction%covfree)
    end if
    if( allocated(interaction%corl) ) then
      deallocate(interaction%corl)
    end if
    if( allocated(interaction%W) ) then
      deallocate(interaction%W)
    end if
    if( allocated(interaction%WW) ) then
      deallocate(interaction%WW)
    end if
    if( allocated(interaction%invW) ) then
      deallocate(interaction%invW)
    end if

    if( allocated(interaction%adjjl) ) then
      deallocate(interaction%adjjl)
    end if

    interaction%nb_internal = 0
    interaction%internal(1:max_internal_tact) = 0.D0

  end subroutine
  
  !> \brief close an interaction
  subroutine close_interaction(interaction)
    implicit none
    type(T_interaction), intent(inout) :: interaction !< [in,out] interaction open in input, close on output

    ! nothing to do here, but the subroutine must exist
    ! since it is used in linkedlist.f90 and thus in anonymous_container

  end subroutine

  !> \brief open an interaction
  subroutine open_interaction(interaction)
    implicit none
    type(T_interaction), intent(inout) :: interaction !< [in,out] interaction close in input, open on output

    ! nothing to do here, but the subroutine must exist
    ! since it is used in linkedlist.f90 and thus in anonymous_container

  end subroutine

  ! ***

  !> \brief set rank of an interaction
  subroutine set_rank(interaction,rank)
    implicit none
    type(T_interaction), intent(inout) :: interaction !< [in, out] interaction
    integer(kind=4),     intent(in)    :: rank        !< [in] new rank

    interaction%rank = rank

  end subroutine

  !> \brief get rank of an interaction
  function get_rank(interaction)
    implicit none
    type(T_interaction), intent(in) :: interaction !< [in] interaction
    integer(kind=4)                 :: get_rank    !< [return] rank of interaction

    get_rank = interaction%rank

  end function

  ! ***

  !> \brief display interaction
  !> \todo : decide what to print
  subroutine display_interaction(interaction, ifich)
    implicit none
    type(T_interaction), intent(in) :: interaction !< [in] interaction to display
    integer(kind=4)    , optional   :: ifich       !< [in] (optional) unit number in which to write
    !
    integer(kind=4) :: i, i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,'(A,1x,I0,1x,A,1x,I0)') 'Interaction of type', interaction%cdan, 'number:', interaction%icdan
    write(i_unit,'(A,1x,I0)') 'Candidate  entity:', interaction%icdent
    write(i_unit,'(A,1x,I0)') 'Antagonist entity:', interaction%ianent

    ! would be better to have:
    !write(i_unit,'(A,1x,A,1x,I0)') 'Interaction of type', get_interaction_name_from_id(interaction%icdan),'number:', interaction%icdan
  end subroutine


end module interaction
