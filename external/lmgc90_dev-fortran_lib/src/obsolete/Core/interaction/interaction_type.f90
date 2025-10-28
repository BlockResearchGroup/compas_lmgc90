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

! Defines a type of interaction as dimension as a parameter (inter_dim)

  !------------------------------------------------------------------------  
  !> Interaction type
  type, public :: T_interaction ! <inter_dim>

    !== identification part ==!

    !> for new_arch
    integer(kind=4) :: rank

    !> type id of contact
    integer(kind=4) :: cdan
    !> index of the interaction
    integer(kind=4) :: icdan

    !> entity index of candidate
    integer(kind=4) :: icdent
    !> entity index of antagonist
    integer(kind=4) :: ianent

    !> body index of candidate
    integer(kind=4) :: icdbdy
    !> body index of antagonist
    integer(kind=4) :: ianbdy

    !> contactor type of candidate
    integer(kind=4) :: icdtyp
    !> contactor type of antagonist
    integer(kind=4) :: iantyp

    !> contactor index of candidate
    integer(kind=4) :: icdtac
    !> contactor index of antagonist
    integer(kind=4) :: iantac

    !> antagonist contactor support node index
    integer(kind=4) :: ian_al

    !== end identification part ==!


    !> index of adjacent
    integer(kind=4) :: iadj
    !> index of see type
    integer(kind=4) :: isee
    !> index of contact law
    integer(kind=4) :: lawnb
    !> contact law type
    integer(kind=4) :: i_law
    !> 1 if x-periodic contact, else 0
    integer(kind=4) :: xperiodic
    !> 1 if y-periodic contact, else 0
    integer(kind=4) :: yperiodic
    !> 1 if z-periodic contact, else 0
    integer(kind=4) :: zperiodic

    !== geometric detection part ==!

    !> normalized projection of candidat on antagonist (see CLALp)
    real(kind=8) :: cpcd

    !> contact locus
    real(kind=8), dimension(inter_dim)   :: coor
    
    !> local frame (t,n,s)
    real(kind=8), dimension(inter_dim,inter_dim) :: uc
    !> from local frame to candidat
    real(kind=8), dimension(inter_dim,inter_dim) :: Gcd
    !> from local frame to antagonist
    real(kind=8), dimension(inter_dim,inter_dim) :: Gan

    !> gap at predicted time (1-theta)*h
    real(kind=8) :: gapTTbegin
    !> gap at predicted time (1-theta)*h + h
    real(kind=8) :: gapTT

    !> area (length) of contact
    real(kind=8) :: area

    !== end geometric detection part ==!


    !== solver part ==!

    !> local relative velocity at beginnig
    real(kind=8), dimension(inter_dim) :: vlBEGIN
    !> local relative velocity
    real(kind=8), dimension(inter_dim) :: vl
    !> local reaction
    real(kind=8), dimension(inter_dim) :: rl

    real(kind=8), dimension(inter_dim) :: vfree
    real(kind=8), dimension(inter_dim) :: covfree
    real(kind=8), dimension(inter_dim) :: corl


    character(len=5) :: status      !< status at end of time step
    character(len=5) :: statusBEGIN !< status at beginning of time step... what status ?
    character(len=5) :: statuscheck !< statuscheck

    ! computed once during prep_nlgs
    real(kind=8), dimension(inter_dim,inter_dim) :: W   !< Delassus matrix
    real(kind=8), dimension(inter_dim)           :: WW  !< auxiliary diagonal Delassus matrix
    real(kind=8), dimension(inter_dim)           :: invW!< inverse of WW
    real(kind=8)                                 :: det !< det of Delassus matrix
    real(kind=8)                                 :: rot !< tangential something
    real(kind=8)                                 :: ron !< normal something


    character(len=5) :: forecast !< set to 'acton' (reactions have to be computed)
    real(kind=8)     :: fric     ! friction coefficient
    real(kind=8)     :: forward, backward 
    integer(kind=4)  :: ivnish,iskip !ivnish = inoctc, what about ron, rot ?
    integer(kind=4)  :: nbadj,istart
    !> \todo To overestimate instead of allocate
    integer(kind=4), dimension(:), allocatable :: adjjl

    integer(kind=4) :: nb_internal
    real(kind=8), dimension(max_internal_tact) :: internal
    real(kind=8) :: ws

    !== end solver part ==!

  end type T_interaction
  
  type T_ptr_interaction 
    type(T_interaction), pointer :: inter => null()
  end type T_ptr_interaction

  type T_face2face

    !rm : there will probably be more data to store with PRPRx detection

    !> number of contact points
    integer(kind=4) :: nb_ctc

    type(T_ptr_interaction), dimension(:), allocatable :: ctc

  end type T_face2face

  type, public :: T_tact2tact

    !> type id of contact
    integer(kind=4) :: cdan

    !!!> contactor type of candidate
    !!integer(kind=4) :: icdtyp
    !!!> contactor type of antagonist
    !!integer(kind=4) :: iantyp

    !> contactor index of candidate
    integer(kind=4) :: icdtac
    !> contactor index of antagonist
    integer(kind=4) :: iantac

    !> index of see type
    integer(kind=4) :: isee

    !> is x-periodic
    integer(kind=4) :: xperiodic
    !> is y-periodic
    integer(kind=4) :: yperiodic

    !> some more stuff (like inode for CLALp detection)
    integer(kind=4) :: i_param

    !> number of face to face structure
    integer(kind=4) :: nb_f2f

    type(T_face2face), dimension(:), allocatable :: f2f

  end type T_tact2tact

  public new_interaction

  public set_nb_tact2tacts         , &
         add_t2t                   , &
         set_nb_face2faces         , &
         t2t_to_this               , &
         initialize_interactions   , &
         get_nb_interactions       , &
         get_nb_rough_interactions , &
         get_nb_recup_interactions , &
         get_interaction           , &
         get_nb_verlet_interactions, &
         get_verlet_interaction    , &
         stock_rloc                , &
         recup_rloc                , &
         reset_this_for_read       , &
         read_ini_vloc_rloc        , &
         get_ptr_all               , &
         clean_memory_interaction

  private initialize_interaction    , &
          erase_face2faces_in_t2t   , &
          erase_interactions_in_f2f , &
          erase_interactions_in_this, &
          erase_interaction

