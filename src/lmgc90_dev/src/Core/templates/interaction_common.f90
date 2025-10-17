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

!------------------------------------------------------------------------
!> \brief Get the contact law of an interaction
subroutine get_behaviour_( icdan, see, tact_behav )
  implicit none
  !> interaction index
  integer                                         :: icdan
  !> see tables list
  type(T_see)       , dimension( : ), allocatable :: see
  !> contact laws list
  type(T_TACT_BEHAV), dimension( : ), allocatable :: tact_behav
  !
  integer :: i_behav
  logical :: found
  
  character(len=103) :: cout

  found = .false.

  do i_behav = 1, size( tact_behav )

     if ( see( this(icdan)%isee )%behav == tact_behav( i_behav )%behav ) then
        this(icdan)%lawnb       = i_behav
        this(icdan)%nb_internal = get_nb_internal( i_behav )
        this(icdan)%internal    = init_internal( i_behav )
        found = .true.
        exit
     end if

  end do

  if ( .not. found ) then
     write( cout, '(A9,A5,A10,I5,A17)' ) &
          'nickname ', see( this(icdan)%isee )%behav, ' ctact nb ', icdan, &
          ' unknown in lawty'
     call logmes( 'check TACT_BEHAV.DAT in DATBOX' )
     call faterr( con_pedigree%module_name // "::get_behaviour", cout )
  end if

end subroutine get_behaviour_

!------------------------------------------------------------------------
!> \brief Get a reference on 'this', 'verlet' and 'violation' array of the specific module
subroutine get_this(this_inter, verlet_inter, violation_inter, con_inter)
  implicit none
  type(T_interaction), dimension(:)  , pointer :: this_inter
  type(T_verlet)     , dimension(:)  , pointer :: verlet_inter
  real(kind=8)       , dimension(:)  , pointer :: violation_inter
  type(T_con)                        , pointer :: con_inter  

  this_inter      => this
  verlet_inter    => verlt
  violation_inter => violation
  con_inter       => con_pedigree  

end subroutine get_this

integer function get_an_tacty(i_mdl, i_bdy, i_tac)
  implicit none
  !> model id
  integer, intent(in) :: i_mdl
  !> body id
  integer, intent(in) :: i_bdy
  !> contactor id
  integer, intent(in) :: i_tac
  !
  logical :: found

  found = .false.
  do get_an_tacty = 1, size(antact2bdyty,2)
    if( antact2bdyty(1,get_an_tacty) == i_bdy .and. &
        antact2bdyty(2,get_an_tacty) == i_tac .and. &
        antact2bdyty(3,get_an_tacty) == i_mdl       ) then
      found = .true.
      exit
    end if
  end do

  if( .not. found ) then
    get_an_tacty = -1
  end if

end function get_an_tacty

!------------------------------------------------------------------------
!> \brief Redo nb_adj and adjac maps
subroutine redo_nb_adj_( nb_cd )
  implicit none
  !> number of candidate contactors
  integer, intent(in) :: nb_cd
  !
  integer :: icdtac, icdan

  ! in fact redo nb_adj and adjac map
  ! nb_adj is mandatory be able to use stock_rloc
  ! adjac is further needed to be able to write_out_vloc_rloc

  !redo nb_adj
  if (allocated(nb_adj)) deallocate(nb_adj)
  allocate( nb_adj(nb_cd) )

  nb_adj = 0
  do icdan = 1, size(this)
    nb_adj(this(icdan)%icdtac) = nb_adj(this(icdan)%icdtac) + 1
  end do

  !redo adjac
  if (allocated(adjac)) then
    do icdtac = 1, size(adjac)
      if (associated(adjac(icdtac)%icdan))  deallocate(adjac(icdtac)%icdan)
      nullify(adjac(icdtac)%icdan)
    end do
    deallocate(adjac)
  end if

  allocate( adjac(nb_cd) )
  do icdtac = 1, nb_cd
    allocate( adjac(icdtac)%icdan( nb_adj(icdtac) ) )
  end do

  do icdan = 1, size(this)
    adjac( this(icdan)%icdtac)%icdan( this(icdan)%iadj ) = icdan
  end do

end subroutine

!------------------------------------------------------------------------
!> \brief Allocate an element of verlet array
subroutine new_verlet_(icdtac, size, errare)
  implicit none
  !> index of verlet array to allocate
  integer, intent(in)  :: icdtac
  !> size of the allocation
  integer, intent(in)  :: size
  !> error value
  integer, intent(out) :: errare

  allocate( verlt(icdtac)%icdan(size)  , stat=errare )

  allocate( verlt(icdtac)%cdsci(size)  , stat=errare )

  allocate( verlt(icdtac)%anbdy(size)  , stat=errare )
  allocate( verlt(icdtac)%antac(size)  , stat=errare )
  allocate( verlt(icdtac)%anmodel(size), stat=errare )
  allocate( verlt(icdtac)%ansci(size)  , stat=errare )

  allocate( verlt(icdtac)%rlt(size)    , stat=errare )
  allocate( verlt(icdtac)%rln(size)    , stat=errare )

  allocate( verlt(icdtac)%vlt(size)    , stat=errare )
  allocate( verlt(icdtac)%vln(size)    , stat=errare )

  allocate( verlt(icdtac)%gapTT(size)  , stat=errare )
  allocate( verlt(icdtac)%status(size) , stat=errare )

  allocate( verlt(icdtac)%tuc(space_dim ,size), stat=errare )
  allocate( verlt(icdtac)%nuc(space_dim ,size), stat=errare )

  if ( space_dim == 3 ) then
    allocate( verlt(icdtac)%rls(size), stat=errare )
    allocate( verlt(icdtac)%vls(size), stat=errare )
    allocate( verlt(icdtac)%suc(space_dim ,size), stat=errare )
  end if

  allocate( verlt(icdtac)%internal(max_internal_tact,size), stat=errare )

  allocate( verlt(icdtac)%coor(space_dim,size), stat=errare )

  allocate( verlt(icdtac)%icdcoor(space_dim,size), stat=errare)
  allocate( verlt(icdtac)%iancoor(space_dim,size), stat=errare)

  allocate( verlt(icdtac)%id_f_cd(size), stat=errare )
  allocate( verlt(icdtac)%id_f_an(size), stat=errare )


end subroutine new_verlet_

!------------------------------------------------------------------------
!> \brief Deallocate pointers stored by a verlet
subroutine free_verlet_(icdtac)
  implicit none
  !> index of verlet to deallocate
  integer, intent(in) :: icdtac

  if ( associated( verlt(icdtac)%icdan  ) ) deallocate( verlt(icdtac)%icdan  )

  if ( associated( verlt(icdtac)%cdsci  ) ) deallocate( verlt(icdtac)%cdsci  )

  if ( associated( verlt(icdtac)%anbdy  ) ) deallocate( verlt(icdtac)%anbdy  )
  if ( associated( verlt(icdtac)%antac  ) ) deallocate( verlt(icdtac)%antac  )
  if ( associated( verlt(icdtac)%anmodel) ) deallocate( verlt(icdtac)%anmodel)
  if ( associated( verlt(icdtac)%ansci  ) ) deallocate( verlt(icdtac)%ansci  )

  if ( associated( verlt(icdtac)%rlt ) ) deallocate( verlt(icdtac)%rlt )
  if ( associated( verlt(icdtac)%rln ) ) deallocate( verlt(icdtac)%rln )

  if ( associated( verlt(icdtac)%vlt ) ) deallocate( verlt(icdtac)%vlt )
  if ( associated( verlt(icdtac)%vln ) ) deallocate( verlt(icdtac)%vln )

  if ( associated( verlt(icdtac)%gapTT ) ) deallocate( verlt(icdtac)%gapTT )
  if ( associated( verlt(icdtac)%status) ) deallocate( verlt(icdtac)%status)

  if ( associated( verlt(icdtac)%tuc ) ) deallocate( verlt(icdtac)%tuc )
  if ( associated( verlt(icdtac)%nuc ) ) deallocate( verlt(icdtac)%nuc )

  if ( space_dim == 3 ) then
    if ( associated( verlt(icdtac)%rls ) ) deallocate( verlt(icdtac)%rls )
    if ( associated( verlt(icdtac)%vls ) ) deallocate( verlt(icdtac)%vls )
    if ( associated( verlt(icdtac)%suc ) ) deallocate( verlt(icdtac)%suc )
  end if

  if ( associated( verlt(icdtac)%internal ) ) deallocate( verlt(icdtac)%internal )

  if ( associated( verlt(icdtac)%coor ) ) deallocate( verlt(icdtac)%coor )

  if ( associated( verlt(icdtac)%icdcoor ) ) deallocate( verlt(icdtac)%icdcoor )
  if ( associated( verlt(icdtac)%iancoor ) ) deallocate( verlt(icdtac)%iancoor )

  if ( associated( verlt(icdtac)%id_f_cd        ) ) deallocate( verlt(icdtac)%id_f_cd )
  if ( associated( verlt(icdtac)%id_f_an        ) ) deallocate( verlt(icdtac)%id_f_an )

end subroutine

!------------------------------------------------------------------------
!> \brief Nullify pointers stored by a verlet
subroutine nullify_verlet_(icdtac)
  implicit none
  !> index of verlet to nullify
  integer, intent(in) :: icdtac

  nullify( verlt(icdtac)%icdan  )

  nullify( verlt(icdtac)%cdsci  )

  nullify( verlt(icdtac)%anbdy  )
  nullify( verlt(icdtac)%antac  )
  nullify( verlt(icdtac)%anmodel)
  nullify( verlt(icdtac)%ansci  )

  nullify( verlt(icdtac)%rlt )
  nullify( verlt(icdtac)%rln )

  nullify( verlt(icdtac)%vlt )
  nullify( verlt(icdtac)%vln )

  nullify( verlt(icdtac)%gapTT )
  nullify( verlt(icdtac)%status)

  nullify( verlt(icdtac)%tuc )
  nullify( verlt(icdtac)%nuc )

  if ( space_dim == 3 ) then
    nullify( verlt(icdtac)%rls )
    nullify( verlt(icdtac)%vls )
    nullify( verlt(icdtac)%suc )
  end if

  nullify( verlt(icdtac)%internal )

  nullify( verlt(icdtac)%coor )

  nullify( verlt(icdtac)%icdcoor )
  nullify( verlt(icdtac)%iancoor )

  nullify( verlt(icdtac)%id_f_cd )
  nullify( verlt(icdtac)%id_f_an )

end subroutine nullify_verlet_

!------------------------------------------------------------------------



!------------------------------------------------------------------------
!> Release memory ressources of a contactor
subroutine clean_memory_inter_meca_()
  implicit none
  !
  integer(kind=4) :: i

  cdtact2bdyty => null()
  antact2bdyty => null()

  if( allocated( this ) ) deallocate( this )

  if( allocated( adjac ) ) then
     do i = 1, size( adjac )
        if( associated( adjac(i)%icdan ) ) then
           deallocate( adjac(i)%icdan )
        end if
     end do
     deallocate( adjac )
  end if

  if( allocated( nb_adj ) ) deallocate( nb_adj )

  if( allocated( verlt ) ) then
     do i = 1, size( verlt )
       call free_verlet_( i )
       call nullify_verlet_( i )
     end do
     deallocate( verlt )
  end if

  if( allocated( violation ) ) deallocate( violation )

end subroutine clean_memory_inter_meca_

