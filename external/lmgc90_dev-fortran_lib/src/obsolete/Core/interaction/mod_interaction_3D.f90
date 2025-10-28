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

!> define the 3D interactions handled by the interaction_handler
module interaction_3D

  use parameters

  use overall, only: nbDIME     , &
                     logmes     , &
                     faterr     , &
                     G_nfich    , &
                     read_G_clin, &
                     G_clin     , &
                     H          , &
                     max_internal_tact

  implicit none

  private

  integer(kind=4), public, parameter :: inter_dim = 3

  include 'interaction_type.f90'

  !> number of faces2faces
  integer(kind=4), public :: nb_tact2tacts
  !> tact2tact
  type(T_tact2tact), dimension(:), allocatable, public :: all_t2t

  !> total number of interactions
  integer(kind=4) :: nb_interactions
  !> total number of interactions stocked in old list
  integer(kind=4) :: nb_verlets
  !> total number of recup interactions
  integer(kind=4) :: nb_recup

  !> arrays of interactions
  type(T_ptr_interaction), dimension(:), pointer :: this1, this2

  !> map the cumulated number of adjacent pairs to a candidate contactor
  integer(kind=4), dimension(:), allocatable, public :: vrlt_map
  !> index of these(2) in these(1)
  integer(kind=4), dimension(:), allocatable :: verlet

  !> logical to know if this1 is the real this or if it is the olde one
  logical :: is_one_current = .true.

  !> pointer dedicated to data stock for display
  real(kind=8), dimension(:,:), pointer :: all_this => null()

  contains

  include 'interaction_methods.f90'

  !> \brief Read local velocities and reactions from a file
  subroutine read_ini_Vloc_Rloc(nb_tact, inter_type, tact2bdy)
    use tact_behaviour, only : get_ibehav, get_nb_internal
    implicit none
    !> number of candidat contactors
    integer(kind=4), intent(in) :: nb_tact
    !> type of interactions to read
    integer(kind=4), intent(in) :: inter_type
    !> map from contactor number to local contactor number and body number
    integer(kind=4), dimension(2,nb_tact), intent(in) :: tact2bdy
    !
    logical :: cd_found, an_found
    type(T_ptr_interaction), dimension(:), pointer :: this
    type(T_interaction), pointer :: inter
    integer(kind=4), dimension(:), allocatable :: local_adj_map
    integer(kind=4)  :: nb_read, nb_internal, i_internal, lantac, cdan, i_tact
    character(len=5) :: cdbdy, anbdy, cdtac, antac, behav
    character(len=103) :: cout
    character(len=29)  :: IAM

    IAM = 'mod_interaction::read_ini_Vloc_Rloc'

    inter => null()

    allocate(local_adj_map(nb_tact))
    local_adj_map = 0

    if( is_one_current ) then
      call reallocate_this(this1,nb_read)
      this => this1
    else
      call reallocate_this(this2,nb_read)
      this => this2
    end if

    ! second reading: filling data
    rewind(G_nfich)

    nb_read = 0
    do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'

      nb_read = nb_read + 1
      ! in case of error in reading, 
      if( associated(inter) ) then
        call erase_interaction(inter)
        deallocate(inter)
        nullify(inter)
      end if
      allocate(inter)
      call initialize_interaction(inter)
      inter%cdan = get_interaction_id_from_name(G_clin(9:13))

      if ( .not. read_G_clin()) exit
      if ( .not. read_G_clin()) exit

      !select case (vloc_rloc_format)
      !case ( 1 )
      !  read(G_clin(1:103),'(1X,A5,2X,I7,2X,A5,2X,I7,9X,A5,2X,A5,2X,I7,2X,A5,2X,I7,2X,A5,2X,I7)')  &
      !       cdbdy, icdbdy, cdtac, icdtac, behav, &
      !       anbdy, ianbdy, antac, iantac, sttus, lantac
      !case ( 2 )
        read(G_clin(1:103),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')  &
             cdbdy, inter%icdbdy, cdtac, inter%icdtac, behav, &
             anbdy, inter%ianbdy, antac, inter%iantac, inter%status, lantac
      !end select
      cdan = get_interaction_type_id( get_contactor_id_from_name(cdtac),get_contactor_id_from_name(antac) )
      if( cdan /= inter_type ) cycle
      cd_found = .false.; an_found = .false.
      do i_tact = 1, size(tact2bdy,2)
        if( tact2bdy(1,i_tact) == inter%icdbdy .and. tact2bdy(2,i_tact) == inter%icdtac ) then
          inter%icdtac = i_tact
          cd_found = .true.
        end if
        if( tact2bdy(1,i_tact) == inter%ianbdy .and. tact2bdy(2,i_tact) == inter%iantac ) then
          inter%iantac = i_tact
          an_found = .true.
        end if
        if( cd_found .and. an_found ) exit
      end do
     
      if( .not.(cd_found .and. an_found) ) cycle

      local_adj_map(inter%icdtac) = local_adj_map(inter%icdtac) + 1
      inter%iadj = local_adj_map(inter%icdtac)

      !will need something like i_bdytyp = i_rbdy2
      inter%icdtyp = get_contactor_id_from_name(cdtac)
      inter%iantyp = get_contactor_id_from_name(antac)
      inter%lawnb  = get_ibehav(behav)
      !inter%lantac = lantac

      if( .not. read_G_clin()) exit
      read(G_clin(1:90),'(27X,3(7X,D14.7))') inter%rl(3),inter%rl(1),inter%rl(2)
      !verlt(icdtact)%rl(nb_adj(icdtact)) = rl*H  ! beware the impulse is being stored in verlt
      if( .not. read_G_clin()) exit 
      read(G_clin(1:90),'(27X,3(7X,D14.7))') inter%vl(3),inter%vl(1),inter%vl(2)
      if( .not. read_G_clin()) exit 
      read(G_clin(1:90),'(27X,2(7X,D14.7))') inter%gapTT
      if( .not. read_G_clin()) exit
      if (G_clin(30:34)== 's(1)=') then
         read(G_clin(1:90),'(27X,3(7X,D14.7))') inter%uc(1,3),inter%uc(2,3),inter%uc(3,3)
      else 
         backspace(G_nfich)
      end if
      if (G_clin(30:34)== 't(1)=') then
         read(G_clin(1:90),'(27X,3(7X,D14.7))') inter%uc(1,1),inter%uc(2,1),inter%uc(3,1)
      else 
         backspace(G_nfich)
      end if
      if (G_clin(30:34)== 'n(1)=') then
         read(G_clin(1:90),'(27X,3(7X,D14.7))') inter%uc(1,2),inter%uc(2,2),inter%uc(3,2)
      else 
         backspace(G_nfich)
      end if
      if( .not. read_G_clin()) exit
      if (G_clin(30:34)== 'coo1=') then
         read(G_clin(1:90),'(27X,3(7X,D14.7))') inter%coor(1),inter%coor(2),inter%coor(3)
      else 
         backspace(G_nfich)
      end if

      nb_internal = get_nb_internal(inter%lawnb)
      if (nb_internal /= 0 ) then  
        if( .not. read_G_clin()) exit
        do i_internal = 1, nb_internal
          read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') &
               inter%internal(i_internal)
        end do
      end if

      this(nb_read)%inter => inter
      inter => null()

    end do

    nb_interactions = nb_read

    deallocate(local_adj_map)

 end subroutine read_ini_vloc_rloc

end module interaction_3D
