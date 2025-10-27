!fd revoir lois preGAP, nosldt ...

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
module tact_behaviour

  use overall
  use utilities
  use parameters

  implicit none

  private

  ! tact behaviour type -----------------------------------------------------------

  type,public  :: T_TACT_BEHAV

     character(len=30)  :: lawty                          ! contact law name
     integer            :: ilaw                           ! contact law id (integer id defined in mod_parameters)

     character(len=5)   :: behav                          ! contact law nickname

     integer            :: nb_param                       ! number of parameters
     real(kind=8)    ,pointer, dimension(:) :: param      => null() ! value of parameters
     character(len=5),pointer, dimension(:) :: param_name => null() ! name of parameters

     integer            :: nb_internal

     character(len=internal_tact_comment_length)  :: internal_comment

  end type T_TACT_BEHAV

  !
  ! linked list mechanism to declare tact_behav
  !

  type,public  :: T_link_TACT_BEHAV
     type(T_link_TACT_BEHAV), pointer :: p => null()
     type(T_link_TACT_BEHAV), pointer :: n => null()
     type(T_TACT_BEHAV) :: tb
  end type T_link_TACT_BEHAV

  type(T_link_TACT_BEHAV),pointer :: Root_tb     => null()
  type(T_link_TACT_BEHAV),pointer :: Current_tb  => null()
  type(T_link_TACT_BEHAV),pointer :: Previous_tb => null()

  logical :: is_tb_ll_open=.false.
  integer :: nb_in_tb_ll=0

  type(T_TACT_BEHAV), public, dimension(:), allocatable :: tact_behav

  ! each tact_behav has its pressure extension

  type :: T_pressure

     ! management of post crack pressure
     ! flag
     !     0: no-pressure
     !     1: linear evolution with respect to time (once beta=0) p = p0 + dpdt * (TPS -TPSini)
     !        param(1) p0
     !        param(2) dpdt
     !        param(3) not used
     !        param(4) not used
     !     2: p = (p0 + dp * min(1,TPS-TPSini/tau))*(1-beta)**alpha
     !        param(1) p0
     !        param(2) dp
     !        param(3) tau
     !        param(4) alpha
     !     3: p = (p0 + dp * (1 - exp(-(TPS-TPSini)/tau)))*(1-beta)**alpha
     !        param(1) p0
     !        param(2) dp
     !        param(3) tau
     !        param(4) alpha
     !     4: p = pext * (1-beta)**alpha
     !        param(1) not used
     !        param(2) not used
     !        param(3) not used
     !        param(4) = alpha
     !  beta = taz(1)
     !  pext = taz(3)
     !  TPSini can be the time when contact is fully broken, or when damage starts

     integer :: flag
     real(kind=8),dimension(10) :: param

  end type T_pressure

  type(T_pressure), dimension(:), allocatable :: tact_pressure

  !------------------------------------------------------------------------
  type T_FRICTION_EVOL

     real(kind=8) :: fric
     real(kind=8) :: TPS
     integer      :: step

  end type T_FRICTION_EVOL

  type(T_FRICTION_EVOL),dimension(:),allocatable :: FRICTION_EVOL

  integer      :: FElast,nb_FEvalues
  real(kind=8) :: current_fric
  logical      :: FEVOLflag =.false.,FEVOL_write_flag = .false.

  logical      :: RandomMuIsActive =.false.
  real(kind=8) :: MuRatio = 0.0

  ! see type --------------------------------------------------------------

  type,public :: T_see
     ! contact type id
     integer           :: cdan
     ! candidate body model
     character(len=5)  :: cdbdy
     ! candidate integer body model
     integer           :: id_cdbdy
     ! candidate contactor type
     character(len=5)  :: cdtac
     ! candidate integer contactor type
     integer           :: id_cdtac
     ! candidate color
     character(len=5)  :: cdcol
     ! antagonist body model
     character(len=5)  :: anbdy
     ! antagonist integer body model
     integer           :: id_anbdy
     ! antagonist contactor type
     character(len=5)  :: antac
     ! antagonist integer contactor type
     integer           :: id_antac
     ! antagonist color
     character(len=5)  :: ancol
     ! behaviour law nicknames
     character(len=5)  :: behav
     ! serial number ibehav of law having same nickname than
     ! seety serial number isee
     integer           :: lawnb
     ! alert distance
     real(kind=8)      :: alert
     ! alert distance for the rough stage
     real(kind=8)      :: global_alert

  end type T_see

  type(T_see),public,dimension(:),allocatable  :: see

  !fd new le 05/01/2011
  ! on passe a une gestion de remplissage du tableau tact_behav par liste chainee
  ! ca va permettre de poser des lois depuis python et de simplifier la routine de lecture

  type,public  :: T_link_see
     type(T_link_see), pointer :: p,n
     type(T_see) :: see
  end type T_link_see

  type(T_link_see),pointer                    :: Root_see,Current_see,Previous_see

  !fd gestion linked_list (ll)
  logical :: is_see_ll_open=.false.
  integer :: nb_in_see_ll=0

  real(kind=8),save :: mac_beta_tol=1.d-12

  logical :: itchatche=.false.

  ! rapport des energies de rupture mode 1 / mode 2; par defaut isotrope
  real(kind=8) :: g1overg2=1.D0

  ! rapport des contraintes de rupture mode 1 / mode 2; par defaut isotrope
  real(kind=8) :: s1overs2=1.D0

  ! rapport des seuils plastiques mode 1 / mode 2; par defaut isotrope
  ! ne sert que pour la loi TH et la loi ABP
  real(kind=8) :: d1overd2=1.D0


  !par defaut
  real(kind=8) :: RNcap=1.D+20

  ! dilatancy parameters
  real(kind=8) :: dilatancy_fric=0.d0, dilatancy_height=0.d0

  ! gestion frottement lois CZM
  ! parametre de la loi d'evolution du frottement avant rupture.
  ! 0 = constant, 1 = lineaire (par defaut) , 2,3... degre du polynome ....
  !fd  integer(kind=4) :: czm_initial_friction_power = 1
  real(kind=8) :: czm_initial_friction_power = 1.d0

  !
  real(kind=8) :: global_adist=0.d0

  !!! wrap API
  public read_xxx_tact_behav,write_xxx_tact_behav,clean_out_tact_behav

  !!! internal API
  public indent_isee, &
         get_fric,get_rst,get_coh,get_forcePERgap,get_forcePERstrain,&
         get_gap_tol, get_g0, & ! RGR
         get_prestrain,get_forcePERstrainrate, &
         indent_tact_behav, &
         get_nb_internal,init_internal,write_internal_comment,get_internal_comment,&
         get_isee, &
         get_ibehav,get_behav_name, &
         get_viscosity, &
         get_kwear,& !!! wear
         get_periodic_strain, &
         get_czm,get_fric_CZM, get_fric_CZM_spring,& !raz_CZM, &
         init_CZM   ,prep_CZM   ,iter_CZM   ,updt_CZM  ,& !,post_CZM
         init_CZM_3D,prep_CZM_3D,iter_CZM_3D,updt_CZM_3D, & !,post_CZM_3D
         ! CRITIC
         get_ToverH,get_viscOVERcritvisc, &
         ! fd prendre les lois g0
         get_pregap, & !pta reactivation 02/09/2015
         get_offset, &
         ! Smooth law
         compute_2D_smooth_forces,compute_3D_smooth_forces, &
         ! IQS_BW_CLB
         get_fric_BW,get_alpha_BW,get_threshold_BW,get_TshVlt_BW, get_Frequency_BW,&
         ! fd
         get_fric_cap,&
         !mr
         get_threshold, &
         ! _THER
         get_lambda, &
         !pta &mr
         init_IQS_PLAS,updt_IQS_PLAS, &
         init_ELAS_REP_adapt, & ! PTA sncf 2023
         !
         set_czm_initial_friction, &
         set_halo, &
         get_snmax, &
         open_tact_behav_ll, add_to_tact_behav_ll, close_tact_behav_ll, &
         open_see_ll, add_to_see_ll, sort_see_ll, close_see_ll, &
         get_nb_tact_behav, get_tact_behav , get_tact_behav_name, &
         get_ilaw_list        , &
         tact_behav_info_by_id, &
         tact_behav_info, &
         init_friction_evolution, &
         get_tact_behav_rank_from_name, &
         get_param_rank_from_name, get_param, set_param, &
         ! set_g1overg2,set_s1overs2,set_d1overd2,
         set_RNcap, get_RNcap, &
         set_dilatancy_parameters,get_dilatancy_fric,get_dilatancy_height, &
         set_random_friction, &
         get_nard_coeff, &
         set_parameters_pressure, &
         !fd la merdasse expo spring
         get_czm_expo_spring,get_czm_expo_spring_p,updt_czm_spring,updt_czm_spring_p,updt_czm_spring_3D,updt_czm_spring_p_3D, &
         get_pressure_flag,&
         clean_memory

contains

  ! data are given using a mechanism of linked lists
  ! open_ : create the root of the linked list (ll)
  ! add_  : add a new element to the ll
  ! close_: close the ll, copy it to an array and suppress the ll

  ! used to fill tact_behav and see rules


  !!! -----------------------------
  !> initialize the tact_behav linked list
  !\todo rename as init_tact_behav_ll()
  subroutine open_tact_behav_ll()
    implicit none

    is_tb_ll_open = .true.
    nb_in_tb_ll = 0

    nullify(Root_tb)
    nullify(Current_tb)
    nullify(Previous_tb)

  end subroutine open_tact_behav_ll

  !!! -----------------------------
  !> add and element to tact_behav linked list
  subroutine add_to_tact_behav_ll(name,nickname,r8_v)
    implicit none
    character(len=30) :: name
    character(len=5)  :: nickname

    real(kind=8),dimension(:),pointer     :: r8_v

                            !12345678901234567890123456789012
    character(len=32):: IAM='tact_behav::add_to_tact_behav_ll'

    integer :: id,nb_param,nb_internal
    character(len=internal_tact_comment_length) :: internal_comment
    character(len=5),dimension(:),pointer :: param_name
    character(len=80) :: cout

    if (.not. is_tb_ll_open) then
      call FATERR(IAM,'linked list is closed')
    endif

    param_name => null()

    nb_in_tb_ll = nb_in_tb_ll + 1

    if ( nb_in_tb_ll == 1) then
      allocate(Root_tb)
      Current_tb => Root_tb
      nullify(Root_tb%p)
    else
      allocate(Current_tb)
      Previous_tb%n => Current_tb
    endif

    Current_tb%tb%lawty = name
    Current_tb%tb%behav = nickname
    if (associated(r8_v)) then
      Current_tb%tb%nb_param = size(r8_v)
    else
      Current_tb%tb%nb_param=0
    endif

    call tact_behav_info(name,id,nb_param,param_name,nb_internal,internal_comment)

    if (Current_tb%tb%nb_param /= nb_param) then
      call logmes(name)
      call logmes(nickname)
      write(cout,'(I0)') Current_tb%tb%nb_param
      call logmes(cout)
      write(cout,'(I0)') nb_param
      call logmes(cout)
      call FATERR(IAM,'nb of parameters is wrong')
    endif

    if (associated(r8_v)) then
       allocate(Current_tb%tb%param, source=r8_v)
    else
       Current_tb%tb%param=>null()
    endif

    Current_tb%tb%ilaw=id
    Current_tb%tb%param_name=>param_name
    Current_tb%tb%nb_internal=nb_internal
    Current_tb%tb%internal_comment=internal_comment

    Current_tb%p => Previous_tb
    nullify(Current_tb%n)
    Previous_tb => Current_tb

  end subroutine add_to_tact_behav_ll

  !!! -----------------------------
  !> close the tact_behav linked list, copy it to an array and suppress the ll
  subroutine close_tact_behav_ll()
    implicit none
                            !1234567890123456789012345678901
    character(len=31):: IAM='tact_behav::close_tact_behav_ll'
    integer :: itacty, need_internal

    if (.not. is_tb_ll_open) then
      call FATERR(IAM,'linked list is already closed')
    endif

    is_tb_ll_open = .false.

    ! allocation importante car on check size(tact_behav) plus tard
    if ( allocated(tact_behav) ) deallocate(tact_behav)
    allocate( tact_behav(nb_in_tb_ll) )

    if ( allocated(tact_pressure) ) deallocate(tact_pressure)
    allocate( tact_pressure(nb_in_tb_ll) )

    if (nb_in_tb_ll == 0) return

    need_internal = 0
    do itacty=nb_in_tb_ll,1,-1

      Previous_tb => Current_tb%p
      tact_behav(itacty) = Current_tb%tb
      need_internal = max(need_internal,tact_behav(itacty)%nb_internal)

      deallocate(Current_tb)
      Current_tb => Previous_tb

      tact_pressure(itacty)%flag=0
      tact_pressure(itacty)%param=0.d0

    end do

    nullify(Root_tb)

    call set_need_internal_tact(need_internal)

  end subroutine close_tact_behav_ll

  !!! -----------------------------
  function get_nb_tact_behav()
    implicit none
    integer(kind=4) :: get_nb_tact_behav
                            !12345678901234567890123456789
    character(len=29):: IAM='tact_behav::get_nb_tact_behav'

    get_nb_tact_behav = 0
    if( allocated(tact_behav) ) then
      get_nb_tact_behav = size(tact_behav)
    end if

  end function get_nb_tact_behav

  !!! -----------------------------
  subroutine get_tact_behav(i_tb, lawty, behav, param, nb_param)
    implicit none
    integer(kind=4), intent(in) :: i_tb
    character(len=30) :: lawty                   ! contact law name
    character(len=5)  :: behav                   ! contact law nickname
    integer(kind=4)   :: nb_param                ! number of parameters
    real(kind=8), pointer, dimension(:) :: param ! value of parameters
                            !12345678901234567890123456
    character(len=26):: IAM='tact_behav::get_tact_behav'

    if( .not. allocated(tact_behav) ) then
      call FATERR(IAM,'tact_behav is not allocated')
    endif

    lawty    = tact_behav(i_tb)%lawty
    behav    = tact_behav(i_tb)%behav
    nb_param = tact_behav(i_tb)%nb_param
    param   => tact_behav(i_tb)%param

  end subroutine get_tact_behav

  function get_tact_behav_name(i_tb)
    implicit none
    integer, intent(in) :: i_tb
    character(len=5)    :: get_tact_behav_name

    get_tact_behav_name = tact_behav(i_tb)%behav

  end function

  !!! -----------------------------
  subroutine open_see_ll()
    implicit none

    is_see_ll_open = .true.
    nb_in_see_ll = 0

    nullify(Root_see)
    nullify(Current_see)
    nullify(Previous_see)

  end subroutine open_see_ll

  !!! -----------------------------
  subroutine add_to_see_ll(cdbdy,cdtac,cdcol, &
                           behav,               &
                           anbdy,antac,ancol, &
                           alert,global_alert)
    implicit none

    character(len=5)  :: cdbdy,cdtac,cdcol,anbdy,antac,ancol,behav
    real(kind=8) ::  alert,global_alert

                            !1234567890123456789012345
    character(len=25):: IAM='tact_behav::add_to_see_ll'

    integer :: id,nb_param,nb_internal
    character(len=internal_tact_comment_length) :: internal_comment
    character(len=5),dimension(:),pointer :: param_name

    if (.not. is_see_ll_open) then
      call FATERR(IAM,'linked list is closed')
    endif

    nb_in_see_ll = nb_in_see_ll + 1

    if ( nb_in_see_ll == 1) then
      allocate(Root_see)
      Current_see => Root_see
      nullify(Root_see%p)
    else
      allocate(Current_see)
      Previous_see%n => Current_see
    endif

    Current_see%see%cdbdy = cdbdy
    Current_see%see%cdtac = cdtac
    Current_see%see%cdcol = cdcol
    Current_see%see%anbdy = anbdy
    Current_see%see%antac = antac
    Current_see%see%ancol = ancol
    Current_see%see%behav = behav
    Current_see%see%alert = alert
    Current_see%see%global_alert = global_alert

    Current_see%see%lawnb=0

    Current_see%see%id_cdbdy = get_body_model_id_from_name(cdbdy)
    Current_see%see%id_cdtac = get_contactor_id_from_name(cdtac)
    Current_see%see%id_anbdy = get_body_model_id_from_name(anbdy)
    Current_see%see%id_antac = get_contactor_id_from_name(antac)
    Current_see%see%cdan     = get_interaction_type_id(Current_see%see%id_cdtac,Current_see%see%id_antac)

    Current_see%p => Previous_see
    nullify(Current_see%n)
    Previous_see => Current_see

  end subroutine add_to_see_ll

  !!! -----------------------------
  subroutine close_see_ll()
    implicit none
                            !123456789012345678901234
    character(len=24):: IAM='tact_behav::close_see_ll'
    integer :: isee,ibehav
    character(len=80)   :: cout

    if (.not. is_see_ll_open) then
      call FATERR(IAM,'linked list is already closed')
    endif

    !fd j'ai l'impression que ça dump si liste vide
    if (nb_in_see_ll > 1) then

      call remove_bad_see_links()

      ! Sort the list so that it is ordered along
      ! first the id_antac, second the ancol and with cd==an as first
      call sort_see_ll()

    endif

    is_see_ll_open = .false.

    if ( allocated(see) ) deallocate(see)
    allocate( see(nb_in_see_ll) )

    if (nb_in_see_ll == 0) return

    do isee=nb_in_see_ll,1,-1

      Previous_see => Current_see%p
      see(isee) = Current_see%see

      deallocate(Current_see)
      Current_see => Previous_see
    end do

    nullify(Root_see)

    do isee=1,size(see)
      !print *, see(isee)%cdtac, ' ', see(isee)%cdcol, '    ',see(isee)%antac, ' ', see(isee)%ancol
      do ibehav=1,size(tact_behav)
        if (see(isee)%behav == tact_behav(ibehav)%behav) then
          see(isee)%lawnb=ibehav
          exit
        end if
        cycle
                                         !123456789                   1234567890        12345678901234567
        write(cout,'(A9,A5,A10,I5,A53)') 'nickname ',see(isee)%behav,' seety nb ',isee,' unknown in lawty'
        call LOGMES('check DATBOX/TACT_BEHAV.DAT')
        call FATERR(IAM,cout)
      end do
      cycle
    end do

  end subroutine close_see_ll

  !!! -----------------------------
  subroutine remove_bad_see_links()
    implicit none

    logical :: remove_cell
    type(T_link_see), pointer :: one, two
    character(len=80) :: cout
    character(len=32) :: IAM
          !12345678901234567890123456789012
    IAM = 'tact_behav::remove_bad_see_links'

    ! if it is assumed that 'remove' is called only in close_see_ll
    ! then this test is useless
    if (.not. is_see_ll_open) then
      call faterr(IAM,'cannot sort a closed linked list')
    endif

    one => Root_see
    do while( associated(one) )
      two => one%n
      do while( associated(two) )
        ! check if one/two are the same laws
        if( one%see%id_antac == two%see%id_antac .and. &
            one%see%id_cdtac == two%see%id_cdtac .and. &
            one%see%ancol    == two%see%ancol    .and. &
            one%see%cdcol    == two%see%cdcol    .and. &
            one%see%anbdy    == two%see%anbdy    .and. &
            one%see%cdbdy    == two%see%cdbdy          &
          ) then
          remove_cell = .true.
        else if( one%see%id_antac == two%see%id_cdtac .and. &
                 one%see%id_cdtac == two%see%id_antac .and. &
                 one%see%ancol    == two%see%cdcol    .and. &
                 one%see%cdcol    == two%see%ancol    .and. &
                 one%see%anbdy    == two%see%cdbdy    .and. &
                 one%see%cdbdy    == two%see%anbdy    ) then
          remove_cell = .true.
        else
          remove_cell = .false.
        end if

        if( remove_cell ) then

          nb_in_see_ll = nb_in_see_ll - 1

          ! check if fatal error
          if( one%see%behav /= two%see%behav ) then
            write(cout,'(A,4(1x,A5))') 'Two similar visibilities with different contact law:', &
                                       one%see%cdtac, one%see%cdcol, one%see%antac, one%see%ancol
            call faterr(IAM,cout)
          end if

          write(cout,'(A,4(1x,A5))') 'Removing one of two similar visibilities:', &
                                     one%see%cdtac, one%see%cdcol, one%see%antac, one%see%ancol
          call logmes(cout)


          ! removing second cell
          Previous_see   => two%p
          Previous_see%n => two%n
          if( associated(two%n) ) then
            two%n%p => Previous_see
          else
            Current_see => Previous_see
          end if
          deallocate(two)

          two => Previous_see%n
        else
          two => two%n
        end if
      end do
      one => one%n
    end do

  end subroutine remove_bad_see_links

  !!! -----------------------------
  subroutine sort_see_ll()
    implicit none

    logical :: move_cell
    type(T_link_see), pointer :: ordering, testing
    character(len=80) :: cout
    character(len=23) :: IAM
          !12345678901234567890123
    IAM = 'tact_behav::sort_see_ll'

    ! if it is assumed that 'sort' is called only in close_see_ll
    ! then this test us iseless
    if (.not. is_see_ll_open) then
      call faterr(IAM,'cannot sort a closed linked list')
    endif

    ordering => Root_see%n
    do while( associated(ordering) )

      ! storing next in case of moving cell
      Previous_see => ordering%n

      testing  => Root_see
      do while( .not. associated(ordering,testing) )
        ! checking if cell is to be moved
        ! checking id_antac, then ancol and lastly if cd == an
        if( ordering%see%id_antac < testing%see%id_antac ) then
          move_cell = .true.
        else if(ordering%see%id_antac > testing%see%id_antac) then
          move_cell = .false.
        else
          if( ordering%see%ancol < testing%see%ancol ) then
            move_cell = .true.
          else if( ordering%see%ancol > testing%see%ancol ) then
            move_cell = .false.
          else
            if( ordering%see%ancol==ordering%see%cdcol .and. &
                ordering%see%id_antac==ordering%see%id_cdtac ) move_cell = .true.
          end if
        end if

        if( move_cell ) then
          ! removing ordering cell
          ordering%p%n => ordering%n
          if( associated(ordering%n) ) ordering%n%p => ordering%p
          ! inserting ordering cell before testing cell
          if( associated(testing%p) ) then
            testing%p%n => ordering
            ordering%p => testing%p
          else
            ordering%p => null()
          end if
          ordering%n => testing
          testing%p  => ordering

          ! reset testing for next ordering
          testing => Root_see
          exit
        else
          ! testing next
          testing => testing%n
        end if

      end do

      ! ordering next
      ordering => Previous_see

      !resetting root
      do while( associated(Root_see%p) )
        Root_see => Root_see%p
      end do

    end do

    !resetting current
    do while( associated(Current_see%n) )
      Current_see => Current_see%n
    end do

  end subroutine sort_see_ll

  !!!---------------------------------------------------------------
  subroutine read_xxx_tact_behav(which)

    implicit none

    integer             :: which
    !****
    integer             :: nb_param,iparam,ibehav,isee
    integer             :: errare
    !                           123456789012345678901234567
    character(len=27)   :: IAM='tact_behav::read_behaviours'
    character(len=80)   :: cout
    !
    character(len=30) :: lawty
    character(len=5)  :: behav
    real(kind=8),dimension(:), pointer :: param
    !
    character(len=5) :: cdbdy,cdtac,cdcol,anbdy,antac,ancol
    real(kind=8) :: alert,global_alert

    G_nfich= get_io_unit()

    if (itchatche) call logmes('Entering: '//IAM)

    if (which == 1) then
       open(unit=G_nfich,file=trim(location(in_tact_behav)))
    else if (which == 2) then
       open(unit=G_nfich,file=trim(location(out_tact_behav)))
    else
       call FATERR(IAM,'mismatched which parameter')
    end if
    !
    ! reading behaviour laws
    !

    ibehav=0
    do
      if( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'behav') cycle                                     ! fishing for the keyword 'behav'
      ibehav = ibehav+1
      if( .not. read_G_clin()) exit

      read(G_clin(2: 6),'(A5) ') behav
      read(G_clin(9:38),'(A30)') lawty

      select case(lawty)
              !123456789012345678901234567890
         !!!-----------------------------------------
         case('IQS_CLB                       ')
            nb_param = 1

            !'sfric'

            !IF(ASSOCIATED(param)) DEALLOCATE(param)
            allocate(param(nb_param))

            iparam = 1
            call read_single(behav,param,iparam)

         !!!-----------------------------------------
         case('IQS_CLB_g0                    ')

            nb_param = 1

            !'sfric'

            !IF(ASSOCIATED(param)) DEALLOCATE(param)
            allocate(param(nb_param))

            iparam = 1
            call read_single(behav,param,iparam)

         !!!-----------------------------------------
         case('IQS_MAP                       ')

            nb_param = 1

            !'sfric'

            !IF(ASSOCIATED(param)) DEALLOCATE(param)
            allocate(param(nb_param))

            iparam = 1
            call read_single(behav,param,iparam)

        !!!-----------------------------------------
        case('IQS_CLB_nosldt                ', &
             'IQS_CLB_noslds                ', &
             'GAP_SGR_CLB_nosldt            ', &
             'GAP_SGR_CLB_noslds            ')
          nb_param = 2

          !'sfric','dt___'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

        !!!-----------------------------------------
        case('IQS_CLB_RGR                   ')

          nb_param = 3

          !'T/H  ','gTot ','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('IQS_DS_CLB                    ')

          nb_param = 2

          !'dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)


!!!-----------------------------------------
        case('IQS_WET_DS_CLB                ')

          nb_param = 5
          ! 'cohen','cohet','Wethk','dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)
          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)
          if( .not. read_G_clin()) goto 10
          iparam = 4
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('RST_WET_CLB                   ')

          nb_param = 7

          !'cohen','cohet','restn','restt','Wethk','dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 6
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('IQS_BW_CLB                    ')

          nb_param = 6

          !'Afric','Bfric','Atsld','Btsld','Alpha','VtTsh'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('xQS_WET_DS_CLB                ')

          nb_param = 5

          !'cohen','cohet','Wethk','dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 4
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('TEX_SOL                       ')

          nb_param = 1

          !'sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('TEX_SOL_UNILAT                ')

          nb_param    = 0

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          nullify(param)

!!!-----------------------------------------
        case('GAP_SGR_CLB                   ')

          nb_param = 1

          !'sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

!!!--------- am & pta: loi frottement sec defo/defo defo/rigides avec pre gap
        case('GAP_SGR_CLB_g0                   ')

          nb_param = 1

          !'sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

!!!-pta 20/05/2015--------------------------
        case('preGAP_SGR_CLB                ')

          nb_param = 2

          !'pgap_','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('VEL_SGR_CLB                   ')

          nb_param = 1

          !'sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('GAP_WET_DS_CLB                ')

          nb_param = 5

          !'cohen','cohet','Wethk','dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 4
          call read_double(behav,param,iparam)


!!!-----------------------------------------
        case('GAP_SGR_DS_CLB                ')

          nb_param = 2

          !'dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('VEL_SGR_DS_CLB                ')

          nb_param = 2

          ! 'dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('RST_CLB                       ')

          nb_param = 3

          !'restn','restt','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)


!!!-----------------------------------------
        case('RST_DS_CLB                    ')

          nb_param = 4

          !'restn','restt','dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

!!!-----------------------------------------
!fd TODO a separer
        case('IQS_MOHR_DS_CLB               ', &
             'GAP_MOHR_DS_CLB               ' )

          nb_param = 4

          !'cohen','cohet','dfric','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

!!!-----------------------------------------
!fd TODO a separer
        case('IQS_CAP_MOHR_DS_CLB               ', &
             'GAP_CAP_MOHR_DS_CLB               '    )

          nb_param = 6
          !'cohen','cohet','dfric','sfric','s_ten','s_com'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('ELASTIC_REPELL_CLB            ', &
             'ELASTIC_REPELL_CLB_g0         ', &
             'ELASTIC_REPELL_CLB_adapt      ') ! PTA sncf 2023

          nb_param = 2
          !'F/gap','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 2
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('VISCO_ELASTIC_REPELL_CLB      ')

          nb_param = 3

          !'F/gap','etan','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)


!!!-----------------------------------------
        case('CRITICAL_VOIGT_CLB            ')

          nb_param = 3

          !'T/H  ','v/cv ','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('KV_WET                        ')

          nb_param = 4

          !'F/str','F/sra','cohen','Wethk'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('ELASTIC_REPELL_WET_CLB        ')

          nb_param = 4
          !'F/gap','cohen','Wethk','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 2
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 4
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('VISCO_ELASTIC_REPELL_WET      ')

          nb_param = 5

          !'F/gap','F/sra','cohen','Wethk','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('ELASTIC_ROD                   ')

          nb_param = 2

          !'F/str','prstr'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('VOIGT_ROD                     ')

          nb_param = 3

          !'F/str','prstr','F/sra'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('ELASTIC_WIRE                  ')

          nb_param = 2

          !'F/str','prstr',

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('BRITTLE_ELASTIC_WIRE          ')

          nb_param = 3

          !'F/str','prstr','snmax'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10

          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('RIGID_WIRE                    ')

          nb_param = 0
          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          nullify(param)

!!!-----------------------------------------
        case('VOIGT_WIRE                    ')

          nb_param = 3

          !'F/str','prstr','F/sra'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
       case('MD_JKRs                       ')

          nb_param = 5

          !'kn   ','kt   ','dampn','gamma','sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_single(behav,param,iparam)

!!!-----------------------------------------
!!! Smooth interaction law used in:
!!! Fillot N., Iordanoff I. and Berthier Y., '', 2006
!!!-----------------------------------------
       case('DEM_FIBs                      ')

          nb_param = 3

          !'kn   ','dampn','gamma'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 2
          call read_double(behav,param,iparam)

!!!-----------------------------------------
       case('ELASTIC_WET_NT                ')

          nb_param = 6

          !'kn   ','kt   ','dampn','dampt','adhen','adhet'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('COUPLED_DOF                   ')

          nb_param = 0
          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          nullify(param)

!!!-----------------------------------------
        case('TANGENTIAL_COUPLED_DOF        ')

          nb_param = 0
          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          nullify(param)

!!!-----------------------------------------
        case('NORMAL_COUPLED_DOF            ')

          nb_param = 0
          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          nullify(param)

!!!-----------------------------------------
       case('IQS_STICK                     ', &
            'GAP_SGR_STICK                 ')

          nb_param = 0
          nullify(param)

!!!-----------------------------------------
        case('PLASTIC_COUPLED_DOF           ')

          nb_param = 1

          !'sfric'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('BROKEN_DOF                   ')

          nb_param = 1

          !'thshd'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        !fd for periodic loadings
        case('PERIO_DOF                     ')

          nb_param = 3

          !'E_XX ','E_YY ','E_XY '

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_single(behav,param,iparam)
          iparam = 2
          call read_single(behav,param,iparam)
          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('GAP_SGR_CLB_WEAR              ')

          nb_param = 3

          !'kcdwe','kanwe','sfric'


          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('IQS_SGR_CLB_WEAR              ')

          nb_param = 3

          !'scohe','dcohe','fric ','Wethk'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

!!!-----------------------------------------
! monerie-acary-cangemi cohesive zone model

            !123456789012345678901234567890
        case('MAC_CZM                       ')

          nb_param = 6

          !'dfric','sfric','cn   ','ct   ','visco','dupre'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
            !123456789012345678901234567890
        case('MAC_CZM_nosldt                ')

          nb_param = 7

          !'dfric','sfric','cn   ','ct   ','visco','dupre','dt___'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_single(behav,param,iparam)

!!!-----------------------------------------
            !123456789012345678901234567890
        case('MAC_CZM_noslds                ')

          nb_param = 7

          !'dfric','sfric','cn   ','ct   ','visco','dupre','ds___'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_single(behav,param,iparam)

!!!-----------------------------------------
! monerie-perales cohesive zone model

            !123456789012345678901234567890
       case('MP_CZM                        ')

          nb_param = 5

          !'dfric','sfric','cn   ','ct   ','dupre'

          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_single(behav,param,iparam)

!!!-----------------------------------------
! monerie-perales cohesive zone model

            !123456789012345678901234567890
       case('MP3_CZM                       ')

          nb_param = 6

          !'dfric','sfric','cn   ','ct   ','smax ','dupre'

          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
! monerie-perales cohesive zone model + ther

            !123456789012345678901234567890
       case('MP3_CZM_THER                  ')

          nb_param = 8

          !'dfric','sfric','cn   ','ct   ','smax ','dupre', lambdas, lambdac

          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

!!!-----------------------------------------
! tvergaard-hutchinson cohesive zone model

            !123456789012345678901234567890
       case('TH_CZM                        ', &
            'IQS_TH_CZM                    '   )

          nb_param = 10

          !'dfric', 'sfric', 'cn   ', 'ct   ', 's1    ', 's2   ', 'G1   ', 'G2   ', 'l1    ','l2   '

          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 9
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('IQS_WET_CZM                   ')

          nb_param = 6

          !'cohes','Wethk','cn   ','ct   ','visco','dupre'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('ABP_CZM                       ', &
             'IQS_ABP_CZM                   ')

          nb_param = 11

          ! 'dfric','sfric','cn','ct','s1','s2','J1','J2','du1','du2','Phi'


          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 9
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 11
          call read_single(behav,param,iparam)

!!!-----------------------------------------
!             123456789012345678901234567890
        case('EXPO_CZM                      ', &
             'IQS_EXPO_CZM                  ')

          nb_param = 9

          ! 'dfric','sfric','cn','ct','s1','s2','w1','w2','eta'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 9
          call read_single(behav,param,iparam)
!!!-----------------------------------------
!             123456789012345678901234567890
        case('EXPO_CZM_P                      ', &
             'IQS_EXPO_CZM_P                  ')

          nb_param = 10

          ! 'dfric','sfric','cn','ct','s1','s2','w1','w2','mu_g','eta'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 9
          call read_double(behav,param,iparam)

!!!-----------------------------------------
!             123456789012345678901234567890
       case('EXPO_CZM_SPRING                ', &
            'IQS_EXPO_CZM_SPRING            ')

          nb_param = 11

          ! 'dfric','sfric','cn','ct','s1','s2','w1','w2','eta','k1',k2'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 9
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 10
          call read_double(behav,param,iparam)

!!!-----------------------------------------
!             123456789012345678901234567890
       case('EXPO_CZM_SPRING_P                ', &
            'IQS_EXPO_CZM_SPRING_P           ')

          nb_param = 12

          ! 'dfric','sfric','cn','ct','s1','s2','w1','w2','mu_g','eta','k1',k2'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 9
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 11
          call read_double(behav,param,iparam)


!!!-----------------------------------------

        case('IQS_MAC_CZM                   ', &
             'ER_MAC_CZM                    ')

          nb_param = 6

          !'dfric','sfric','cn   ','ct   ','visco','dupre'


          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('ELASTIC_REPELL_MAC_CZM        ')

          nb_param = 6

          !'F/gap','gcmax','cn   ','ct   ','visco','dupre'


          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
            !123456789012345678901234567890
        case('IQS_MAC_CZM_nosldt            ')

          nb_param = 7

          !'dfric','sfric','cn   ','ct   ','visco','dupre','dt___'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_single(behav,param,iparam)

!!!-----------------------------------------
            !123456789012345678901234567890
        case('IQS_MAC_CZM_noslds            ')

          nb_param = 7

          !'dfric','sfric','cn   ','ct   ','visco','dupre','ds___'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_single(behav,param,iparam)

!!!-----------------------------------------
        case('postGAP_IQS_MAC_CZM           ')

          nb_param = 6

          !'dfric','sfric','cn   ','ct   ','visco','dupre'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('MSMP_CZM                      ')

          nb_param = 6

          !'dfric','sfric','cn   ','ct   ','beta0','dupre'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('MAL_CZM                       ')

          nb_param = 8

          !'dfric','sfric','cn   ','ct   ','S1   ','S2   ','G1   ','G2   '

          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('IQS_MAL_CZM                   ')

          nb_param = 8

          !'dfric','sfric','cn   ','ct   ','S1   ','S2   ','G1   ','G2   '

          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 5
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 7
          call read_double(behav,param,iparam)

!!!-----------------------------------------
        case('LJ_POTENTIAL                  ')

          nb_param = 7

          ! 'sigma','epsil','elect','noycd','noyan','radcd','radan'

          !IF(ASSOCIATED(param)) DEALLOCATE(param)
          allocate(param(nb_param))

          iparam = 1
          call read_double(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 3
          call read_single(behav,param,iparam)

          if( .not. read_G_clin()) goto 10
          iparam = 4
          call read_double(behav,param,iparam)

          iparam = 6
          call read_double(behav,param,iparam)

!!!-----------------------------------------
             !123456789012345678901234567890
        case('IQS_PLAS_CLB                  ')

           nb_param = 6

           ! rnc, roverg, sfric, pfric , dg, dfric

           !IF(ASSOCIATED(param)) DEALLOCATE(param)
           allocate(param(nb_param))

           iparam = 1
           call read_double(behav,param,iparam)

           if( .not. read_G_clin()) goto 10
           iparam = 3
           call read_double(behav,param,iparam)

           if( .not. read_G_clin()) goto 10
           iparam = 5
           call read_double(behav,param,iparam)
!!!-----------------------------------------
             !123456789012345678901234567890
        case('BRITTLE_COATING_CLB           ')

           nb_param = 4

           ! fric, stiffness, smax, g0

           !IF(ASSOCIATED(param)) DEALLOCATE(param)
           allocate(param(nb_param))

           iparam = 1
           call read_single(behav,param,iparam)

           if( .not. read_G_clin()) goto 10
           iparam = 2
           call read_single(behav,param,iparam)

           if( .not. read_G_clin()) goto 10
           iparam = 3
           call read_single(behav,param,iparam)

           if( .not. read_G_clin()) goto 10
           iparam = 4
           call read_single(behav,param,iparam)
!!!-----------------------------------------
             !123456789012345678901234567890
        case('NARD_ROD                      ')

           nb_param = 4

           ! En, nu, s1, s2

           allocate(param(nb_param))

           iparam = 1
           call read_double(behav,param,iparam)

           if( .not. read_G_clin()) goto 10
           iparam = 3
           call read_double(behav,param,iparam)

!!!-----------------------------------------
             !123456789012345678901234567890
        case('GTN_CZM                       ', &
             'GTN2_CZM                      '  )

           nb_param = 17

           !dyfr, stfr, cn, ct,
           !f0  , fc  ,  k,
           !q1  , q2  ,  e,  Y,
           !s0  , Kh  ,  n,
           !fN  , eN  , sN

           allocate(param(nb_param))

           iparam = 1
           call read_double(behav,param,iparam)

           do iparam = 3, 15, 2
               if( .not. read_G_clin()) goto 10
               call read_double(behav,param,iparam)
           end do
           if( .not. read_G_clin()) goto 10
           !iparam = iparam + 2
           call read_single(behav,param,iparam)

!!!-----------------------------------------
             !123456789012345678901234567890
        case('TOSI_CZM                       ', &
             'TOSI_CZM_INCRE                 ')

           nb_param = 16

           !dyfr, stfr,
           !cn,
           !ct,
           !f0, porosite initiale
           !fc  porosite critique
           !n,  voir modele salvo
           !K,
           !R,  constante des gaz parfaits
           !sigma0,
           !Q, energie d'activation
           !Gc1, taux de restitution d'energie mode I
           !Gc2, taux de restitution d'energie mode II/III
           !k_coal, accelerateur coalescente
           !h,
           !n_mol, quantité de mol de gaz par unité de volume

           allocate(param(nb_param))

           iparam = 1
           call read_double(behav,param,iparam)

           do iparam = 3, 15, 2
               if( .not. read_G_clin()) goto 10
               call read_double(behav,param,iparam)
            end do

!!!-----------------------------------------
        case default

          write(cout,'(A6,A30,A8)') 'lawty ',lawty,' unknown'
          call faterr(IAM,cout)

      end select

      call add_to_tact_behav_ll(lawty,behav,param)

    end do
                                   !123456789012345
    write(cout,'(I0,1x,A)') ibehav,'tact law found'
    call LOGMES(cout)
    call LOGMES('--')

    ! reading contactors visibility array see
    ! first reading: sizing contactors visibility array see

    rewind(G_nfich)

    isee=0
    do
      if( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'seety') cycle                                     ! fishing for the keyword 'seety'
      isee = isee+1
      if( .not. read_G_clin()) exit
      if( .not. read_G_clin()) exit

      read(G_clin,50,err=210) cdbdy,cdtac,cdcol, &
                              behav,               &
                              anbdy,antac,ancol, &
                              alert

      !fd on essaie d'initialiser avec la valeur du halo
      global_alert = global_adist

      !fd new 28/10/09 lecture d'une valeur supplementaire optionnelle
      if( read_G_clin() ) then
        if (G_clin(1:1) == '+') then
          read(G_clin(55:68),'(D14.7)',err=210) global_alert
          !print*,'lecture global alert',isee,see(isee)%global_alert
        else
          backspace(G_nfich)
        endif
      else
        backspace(G_nfich)
      endif

      !fd si on n'a rien fait on prend l'infini
      if (global_alert == 0.d0 ) global_alert = 1.d+20

      call add_to_see_ll(cdbdy,cdtac,cdcol, &
                         behav,               &
                         anbdy,antac,ancol, &
                         alert,global_alert)

      !fd
      cycle
                              !1234567890123456789012345567890123
210   write(cout,'(A33,I5)') 'error reading seety data, seety = ',isee
      call LOGMES('check DATBOX/TACT_BEHAV.DAT')
      call FATERR(IAM,cout)
    end do
                                 !1234567890123456
    write(cout,'(I0,1x,A)') isee,'see table found'
    call LOGMES(cout)
    call LOGMES('--')

50  format(1X,A5,2X,A5,2X,A5,2X,A5,2X,A5,2X,A5,2X,A5,5X,1X,D14.7)

    close(G_nfich)

    return

10  write(cout,'(A21,A5)') 'reading error in law ',behav
    call LOGMES('check DATBOX/TACT_BEHAV.DAT')
    call FATERR(IAM,cout)

  end subroutine read_xxx_tact_behav
!!!-----------------------------------------
  subroutine write_xxx_tact_behav(which)

    implicit none
    integer           :: ibehav,isee
    !                         1234567890123456789012345678
    character(len=28) :: IAM='tact_behav::write_behaviours'
    character(len=38) :: clin,clin0
    character(len=80) :: cout
    integer           :: which,nfich,iparam

    nfich= get_io_unit()

    if (which == 1) then       ! rebuild in
       open(unit=nfich,STATUS='REPLACE',file=trim(location(in_tact_behav(:))))
    else if (which == 2) then ! write out
       open(unit=nfich,STATUS='REPLACE',file=trim(location(out_tact_behav(:))))
       call write_comment(nfich)
    else if (which == 3) then  !append out
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_tact_behav(:))))
    else
       call LOGMES('Error '//IAM//': mismatched which parameter')
    end if




    !xxxxxxxxxxxxxxxxxxxxx12345678901234567890123456789012345678
    write(clin0,'(A38)') '                                      '

    do ibehav=1,size(tact_behav)
       write(nfich,'(A13)')'$behav  lawty'

       write(clin,'(1X,A5,2X,A30)') tact_behav(ibehav)%behav,tact_behav(ibehav)%lawty

       select case(tact_behav(ibehav)%ilaw)
       case(i_IQS_CLB, &
            i_IQS_CLB_g0, &
            i_PLASTIC_COUPLED_DOF, &
            i_BROKEN_DOF, &
            i_TEX_SOL, &
            i_GAP_SGR_CLB, &
            i_GAP_SGR_CLB_g0, &
            i_VEL_SGR_CLB)
          call write_single(clin,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_CLB_nosldt, &
            i_IQS_CLB_noslds, &
            i_GAP_SGR_CLB_nosldt, &
            i_GAP_SGR_CLB_noslds )
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_CLB_RGR)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_BW_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_TEX_SOL_UNILAT, &
            i_COUPLED_DOF, &
            i_NORMAL_COUPLED_DOF, &
            i_TANGENTIAL_COUPLED_DOF, &
            i_IQS_STICK, &
            i_GAP_SGR_STICK, &
            i_RIGID_WIRE)

          call write_empty(clin,nfich)

       case(i_IQS_DS_CLB, &
            i_GAP_SGR_DS_CLB, &
            i_preGAP_SGR_CLB, & !pta 20/05/2015
            i_VEL_SGR_DS_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_RST_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_RST_WET_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,6,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_RST_DS_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_WET_DS_CLB, &
            i_xQS_WET_DS_CLB, &
            i_GAP_WET_DS_CLB )
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,4,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_MOHR_DS_CLB, &
            i_GAP_MOHR_DS_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_CAP_MOHR_DS_CLB, &
            i_GAP_CAP_MOHR_DS_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_ELASTIC_REPELL_CLB, &
            i_ELASTIC_REPELL_CLB_g0, &
            i_ELASTIC_REPELL_CLB_adapt) ! PTA sncf 2023
          call write_single(clin,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,2,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_VISCO_ELASTIC_REPELL_CLB)
          call write_double(clin,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_CRITICAL_VOIGT_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_ELASTIC_REPELL_WET_CLB)
          call write_single(clin,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,2,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,4,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_VISCO_ELASTIC_REPELL_WET)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_ELASTIC_ROD, &
            i_ELASTIC_WIRE)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_BRITTLE_ELASTIC_WIRE)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_VOIGT_ROD, &
            i_VOIGT_WIRE)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_MD_JKRs)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_DEM_FIBs)
          call write_single(clin,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,2,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_ELASTIC_WET_NT)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_KV_WET)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_PERIO_DOF)
          call write_single(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,2,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)


       case(i_GAP_SGR_CLB_WEAR)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_SGR_CLB_WEAR)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_WET_CZM)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_MAL_CZM, &
            i_IQS_MAL_CZM )

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_MAC_CZM        , &
            i_IQS_MAC_CZM, &
            i_ER_MAC_CZM, &
            i_postGAP_IQS_MAC_CZM, &
            i_MSMP_CZM, &
            i_ELASTIC_REPELL_MAC_CZM)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_MAC_CZM_nosldt, &
            i_IQS_MAC_CZM_noslds, &
            i_MAC_CZM_nosldt , &
            i_MAC_CZM_noslds)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)


       case(i_ABP_CZM, i_IQS_ABP_CZM)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,9,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,11,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_EXPO_CZM, i_IQS_EXPO_CZM)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,9,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,9,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_EXPO_CZM_SPRING, i_IQS_EXPO_CZM_SPRING)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,9,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,10,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_EXPO_CZM_SPRING_P, i_IQS_EXPO_CZM_SPRING_P)

          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,9,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,11,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_MP_CZM)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_MP3_CZM)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_MP3_CZM_THER)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_TH_CZM,i_IQS_TH_CZM)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,7,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,9,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_LJ_POTENTIAL)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,4,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,6,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_IQS_PLAS_CLB)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,5,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_BRITTLE_COATING_CLB)
          call write_single(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,2,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_single(clin0,4,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_NARD_ROD)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          call write_double(clin0,3,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_GTN_CZM, i_GTN2_CZM)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          do iparam = 3,15,2
            call write_double(clin0,iparam,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          end do
          call write_single(clin0,17,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)

       case(i_TOSI_CZM,i_TOSI_CZM_INCRE)
          call write_double(clin ,1,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
          do iparam = 3,15,2
            call write_double(clin0,iparam,tact_behav(ibehav)%param,tact_behav(ibehav)%param_name,nfich)
         end do

       case default
          write(cout,'(A6,A30)') 'lawty ',tact_behav(ibehav)%lawty
          call FATERR(IAM,cout)
       end select
    end do
    write(nfich,'(A1)') ' '

    ! writing contactors visibility array see
    do isee=1,size(see)

       write(nfich,'(A6)')  '$seety'
       write(nfich,'(A60)') ' cdbdy  cdtac  cdcol  behav  anbdy  antac  ancol       alert'
       write(nfich,50) &
            see(isee)%cdbdy,see(isee)%cdtac,see(isee)%cdcol, &
            see(isee)%behav,                                 &
            see(isee)%anbdy,see(isee)%antac,see(isee)%ancol, &
            see(isee)%alert
       if (see(isee)%global_alert < 1.d20) then
         write(nfich,51 ) see(isee)%global_alert
       endif

       write(nfich,'(A1)') ' '
    end do

    close(nfich)

50  format(1X,A5,2X,A5,2X,A5,2X,A5,2X,A5,2X,A5,2X,A5,6X,D14.7)
51  format('+',5x,2X,5x,2X,5x,2X,5x,2X,5x,2X,5x,2X,5x,6X,D14.7)


    if (itchatche) call logmes('Leaving: '//IAM)


  end subroutine write_xxx_tact_behav
!!!---------------------------------------------------------------------
  subroutine clean_out_tact_behav

    implicit none
    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='REPLACE',file=trim(location(out_tact_behav(:))))
    write(nfich,'(A1)')' '
    close(nfich)

  end subroutine clean_out_tact_behav
!!!------------------------------------------------------------------------
!!!
!!!  CONTACT BEHAVIOURS
!!!
!!!------------------------------------------------------------------------
  subroutine read_single(behav,param,iparam)

    implicit none
    character(len=5) :: behav
    integer :: iparam
    real(kind=8),dimension(:) :: param
    !                           12345678901234567890123
    character(len=23)   :: IAM='tact_behav::read_single'
    character(len=80)   :: cout



    read(G_clin(39:59),'(7X,D14.7)',err=10) param(iparam)

    return

10  write(cout,'(A21,A5)') 'reading error in law ',behav
    call LOGMES('check DATBOX/TACT_BEHAV.DAT')
    call FATERR(IAM,cout)

  end subroutine read_single
!!!------------------------------------------------------------------------
  subroutine read_double(behav,param,iparam)

    implicit none
    character(len=5) :: behav
    integer :: iparam

    real(kind=8),dimension(:) :: param

    !                         12345678901234567890123
    character(len=23) :: IAM='tact_behav::read_double'
    character(len=80) :: cout

    read(G_clin(39:80),'(2(7X,D14.7))',err=10) param(iparam),param(iparam+1)

    return

10  write(cout,'(A21,A5)') 'reading error in law ',behav
    call LOGMES('check DATBOX/TACT_BEHAV.DAT')
    call FATERR(IAM,cout)

  end subroutine read_double

!!!------------------------------------------------------------------------
  subroutine write_empty(clin,nfich)

    implicit none
    integer           :: nfich
    character(len=38) :: clin

    write(nfich,103) clin

103 format(A38)

  end subroutine write_empty
!!!------------------------------------------------------------------------
  subroutine write_single(clin,iparam,param,param_name,nfich)

    implicit none
    integer           :: iparam,nfich
    real(kind=8),dimension(:)      :: param
    character(len=5),dimension(:)  :: param_name
    character(len=38) :: clin

    write(nfich,103) clin,param_name(iparam),param(iparam)

103 format(A38,(2X,A5,D14.7))

  end subroutine write_single
!!!------------------------------------------------------------------------
  subroutine write_double(clin,iparam,param,param_name,nfich)

    implicit none
    integer           :: iparam,nfich
    real(kind=8),dimension(:)      :: param
    character(len=5),dimension(:)  :: param_name
    character(len=38) :: clin

    write(nfich,103) clin, &
         param_name(iparam  ),param(iparam  ),&
         param_name(iparam+1),param(iparam+1)

103 format(A38,2(2X,A5,D14.7))

  end subroutine write_double
!!!------------------------------------------------------------------------
  subroutine write_comment(nfich)

    implicit none
    integer :: nfich
    !                12345678901234567890123456789012345678901234567890123456789012345678901234567
    write(nfich,'(A72)')'! File BEHAVIOUR                                                        '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''$''       preceeds a keyword used in scanning files.     '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''behav''   stands for the nickname of a bulk or           '
    write(nfich,'(A72)')'! contact behaviour law, character(len=5).                              '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''lawty''   stands for the name of a bulk or               '
    write(nfich,'(A72)')'! contact behaviour law, character(len=30).                             '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''seety''   stands for description of a candidate          '
    write(nfich,'(A72)')'! ''cdbdy'' type of body, ''cdtac'' type of contactor, ''cdcol'' color        '
    write(nfich,'(A72)')'! ready to meet with the contact behaviour law ''behav'' an antagonist    '
    write(nfich,'(A72)')'! ''anbdy'' type of body, ''antac'' type of contactor, ''ancol'' color.       '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! Candidate antagonist objects are considered only within some distance '
    write(nfich,'(A72)')'! ''alert''.                                                              '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! STANDARD PACKAGE of contact behaviour laws                            '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! 123456789012345678901234567890:                                       '
    write(nfich,'(A72)')'!                               :                                       '
    write(nfich,'(A72)')'! contact behaviour             :                                       '
    write(nfich,'(A72)')'!                               :                                       '
    write(nfich,'(A72)')'! IQS_CLB                       : Inelastic quasi shock &               '
    write(nfich,'(A72)')'!                               : Coulomb law                           '
    write(nfich,'(A72)')'! IQS_CLB_RGR                   : Inelastic quasi shock &               '
    write(nfich,'(A72)')'!                               : Coulomb law +  Radjai Gap Rescue      '
    write(nfich,'(A72)')'! GAP_SGR_CLB                   : Gap Signorini condition &             '
    write(nfich,'(A72)')'!                               : Coulomb law                           '
    write(nfich,'(A72)')'! VEL_SGR_CLB                   : Velocity Signorini condition &        '
    write(nfich,'(A72)')'!                               : Coulomb law                           '
    write(nfich,'(A72)')'! IQS_DS_CLB                    : Inelastic quasi shock &               '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! GAP_SGR_DS_CLB                : Gap Signorini condition &             '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! VEL_SGR_DS_CLB                : Velocity Signorini condition &        '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! RST_CLB                       : Restitution shock law &               '
    write(nfich,'(A72)')'!                               : Coulomb law                           '
    write(nfich,'(A72)')'! RST_DS_CLB                    : Restitution shock law &               '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! ELASTIC_REPELL_CLB            : Repulsive reaction force proportional '
    write(nfich,'(A72)')'!                               : to penetration gap, vanishing         '
    write(nfich,'(A72)')'!                               : otherwise & Coulomb law               '
    write(nfich,'(A72)')'! GAP_MOHR_DS_CLB               : Mohr-Coulomb law when cohesive, else  '
    write(nfich,'(A72)')'!                               : gap Signorini condition &             '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! IQS_DS_WET_CLB                : rather to be written:                 '
    write(nfich,'(A72)')'! IQS_WET_DS_CLB                : non smooth Lennard Jones attraction   '
    write(nfich,'(A72)')'!                               : law; inelastic quasi shock stands for '
    write(nfich,'(A72)')'!                               : unilaterality &                       '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! GAP_DS_WET_CLB                : rather to be written:                 '
    write(nfich,'(A72)')'! GAP_WET_DS_CLB                : non smooth Lennard Jones attraction   '
    write(nfich,'(A72)')'!                               : law; gap Signorini condition stands   '
    write(nfich,'(A72)')'!                               : for unilaterality &                   '
    write(nfich,'(A72)')'!                               : dynamic static Coulomb law            '
    write(nfich,'(A72)')'! ELASTIC_REPELL_WET_CLB        : non smooth Lennard Jones attraction   '
    write(nfich,'(A72)')'!                               : law; gap Signorini condition stands   '
    write(nfich,'(A72)')'!                               : for unilaterality & Coulomb law       '
    write(nfich,'(A72)')'! ELASTIC_ROD                   : linear elastic rod under traction &   '
    write(nfich,'(A72)')'!                               : compression                           '
    write(nfich,'(A72)')'! VOIGT_ROD                     : linear visco elastic rod under        '
    write(nfich,'(A72)')'!                               : traction & compression                '
    write(nfich,'(A72)')'!                               : (to be used with POINT against POINT) '
    write(nfich,'(A72)')'! ELASTIC_WIRE                  : linear elastic wire under traction &  '
    write(nfich,'(A72)')'!                               : inactive under compression            '
    write(nfich,'(A72)')'!                               : (to be used with POINT against POINT) '
    write(nfich,'(A72)')'! VOIGT_WIRE                    : linear visco lastic wire under        '
    write(nfich,'(A72)')'!                               : traction & inactive under compression '
    write(nfich,'(A72)')'!                               : (to be used with POINT against POINT) '
    write(nfich,'(A72)')'! MD_JKRs                       : Modified JKR Model                    '
    write(nfich,'(A72)')'                                : (to be used with MD + DKDK or DKJC)   '
    write(nfich,'(A72)')'! MAC_CZM                       : Monerie-Acary-Cangemi cohesive zone   '
    write(nfich,'(A72)')'!                               : model                                 '
    write(nfich,'(A72)')'! MP_CZM                        : Monerie-Perales cohesive zone model   '
    write(nfich,'(A72)')'!                               :                                       '
    write(nfich,'(A72)')'! MP3_CZM                       : Monerie-Perales cohesive zone model   '
    write(nfich,'(A72)')'!                               : defined by 3 cohesive parameters      '
    write(nfich,'(A72)')'! TH_CZM                        : Tvergaard-Hutchinson cohesive zone    '
    write(nfich,'(A72)')'!                               : model                                 '
    write(nfich,'(A72)')'! GAP_SGR_CLB_WEAR              : Archard wear model                    '
    write(nfich,'(A72)')'!                               : modified Signorini Coulomb Law        '
    write(nfich,'(A72)')'!                               : (to be used with MAILx)               '
    write(nfich,'(A72)')'! IQS_SGR_CLB_WEAR              : Cohesive wear model                   '
    write(nfich,'(A72)')'!                               : modified Signorini Coulomb Law        '
    write(nfich,'(A72)')'!                               : (to be used with RIGID_2D)            '
    write(nfich,'(A72)')'! COUPLED DOF                   : to impose Un=Ut=0 in a relation       '
    write(nfich,'(A72)')'!                               :                                       '
    write(nfich,'(A72)')'! PERIO DOF                     : to impose a periodic relation         '
    write(nfich,'(A72)')'!                               :   U = E.Y expressed in the local frame'
    write(nfich,'(A72)')'!                               : where E is a strain tensor (given)    '
    write(nfich,'(A72)')'!                               :       Y the periodic vector (comp)    '
    write(nfich,'(A72)')'!                               : (to be used with POINT against POINT) '
    write(nfich,'(A72)')'                                                                        '

  end subroutine write_comment
!!!------------------------------------------------------------------------
  subroutine init_CZM(ibehav,internal,taz)
    ! initialisations faites au premier pas de temps
    implicit none
    integer :: ibehav
    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz)           :: taz

    real(kind=8) :: kc,cn,ct,xc,fc


    select case(tact_behav(ibehav)%ilaw)
    case(i_MAC_CZM, i_IQS_MAC_CZM, i_ER_MAC_CZM,i_ELASTIC_REPELL_MAC_CZM, i_postGAP_IQS_MAC_CZM, &
         i_MAL_CZM, i_IQS_MAL_CZM, &
         i_MSMP_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER, &
         i_TH_CZM,i_IQS_TH_CZM, &
         i_ABP_CZM, i_IQS_ABP_CZM, &
         i_EXPO_CZM, i_IQS_EXPO_CZM &
        )

       !fd interface non endommage

       if (NSTEP == 1) then
          internal=0.d0
          internal(4) = 1.d0
          internal(5) = -99.d0
       end if

       taz(1) = internal(4)
    case(i_EXPO_CZM_SPRING, i_IQS_EXPO_CZM_SPRING)

       !fd interface non endommage

       if (NSTEP == 1) then
          internal=0.d0
          internal(4) = 1.d0
          internal(5) = -99.d0
          internal(8) = 1.d0
          internal(9) = 1.d0 
       end if

       taz(1) = internal(4)

    !!aliboukham
    case(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P, i_EXPO_CZM_SPRING_P, i_IQS_EXPO_CZM_SPRING_P)
       !fd interface non endommage

       if (NSTEP == 1) then
          internal=0.d0
          internal(4) = 1.d0
          internal(5) = -99.d0
       end if

       taz(1) = internal(4)

    case(i_IQS_WET_CZM)
       !mr warning internal(6) is used for thermal anisotropy
       if (NSTEP == 1) then
          internal(1:3)=0.d0
          internal(4) = 1.d0
       end if

       taz(1) = internal(4)

    case(i_TOSI_CZM,i_TOSI_CZM_INCRE)
       !fd interface non endommage

       if (NSTEP == 1) then
         internal=0.d0
         ! valeur de l'endommagement initiale
         internal(4) = 1.d0 - tact_behav(ibehav)%param(5) ! Beta = 1 - fn = 1 - f0
         internal(11)= 1.                                 ! fc   = 1.
       end if

    case default
       call faterr('tact_behav::init_CZM','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

  end subroutine init_CZM
!!!------------------------------------------------------------------------
  subroutine prep_CZM(ibehav,internal,cd_length,ucut,ucun)

    implicit none
    integer      :: ibehav
    real(kind=8) :: cd_length,cn,ct,b,w,ucun,ucut

    real(kind=8),dimension(max_internal_tact) :: internal


    select case(tact_behav(ibehav)%ilaw)
    case(i_MAC_CZM,i_IQS_MAC_CZM,i_ER_MAC_CZM,i_postGAP_IQS_MAC_CZM,i_ELASTIC_REPELL_MAC_CZM,  &
         i_IQS_WET_CZM, &
         i_MSMP_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER, &
         i_MAL_CZM,i_IQS_MAL_CZM, &
         i_TH_CZM,i_IQS_TH_CZM, &
         i_ABP_CZM,i_IQS_ABP_CZM, &
         i_EXPO_CZM, i_IQS_EXPO_CZM, &
         i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)


       internal(1)=cd_length

       !internal(2)=ucut
       !fd on ecrase la valeur update en cours de calcul mais a pourrait etre vire
       internal(3)=ucun

    case(i_EXPO_CZM_SPRING, i_IQS_EXPO_CZM_SPRING, i_EXPO_CZM_SPRING_P, i_IQS_EXPO_CZM_SPRING_P, i_TOSI_CZM,i_TOSI_CZM_INCRE)

       internal(1)=cd_length

       !fd on ne touche pas ces quantites qui sont modifiees lors du update
       !internal(2)=ucut
       !internal(3)=ucun

    case default

       call faterr('tact_behav::pre_CZM','incompatible law '//trim(tact_behav(ibehav)%lawty))

    end select

  end subroutine prep_CZM
!!!------------------------------------------------------------------------
  subroutine iter_CZM(ibehav,internal,taz,k_t,k_n,is_cohesive,radh_t,radh_n,p)

    implicit none
    integer      :: ibehav
    real(kind=8) :: cn,ct,smax,w,d,p,b
    real(kind=8) :: beta,k_t,k_n,radh_t,radh_n,tmp
    logical      :: is_cohesive

    ! abp
    real(kind=8) :: s1,s2,G1,G2,du1,du2,phi

    ! th
    real(kind=8) :: dp1,dp2

    ! expo
    real(kind=8) :: eta, mu_g

    ! tosi
    real(kind=8) :: xx

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz

    is_cohesive = .false.

    select case(tact_behav(ibehav)%ilaw)
    !fd que vient faire wet_czm ?
    case(i_MAC_CZM,i_IQS_MAC_CZM,i_ER_MAC_CZM,i_postGAP_IQS_MAC_CZM,i_ELASTIC_REPELL_MAC_CZM, &
         i_IQS_WET_CZM &
        )

       !fd sig_adh = beta^2 c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm(ibehav,cn,ct,smax,w,b)

          tmp = - internal(1)*beta*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       end if

       p = H*internal(1)*get_pressure(ibehav,beta,internal(5),taz(3))

    case(i_MSMP_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm(ibehav,cn,ct,smax,w,b)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(5),taz(3))

    case(i_MAL_CZM,i_IQS_MAL_CZM)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_mal(ibehav,cn,ct,s1,s2,G1,G2)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(5),taz(3))

    case(i_TH_CZM,i_IQS_TH_CZM )

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_th(ibehav,cn,ct,s1,s2,G1,G2,dp1,dp2)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(5),taz(3))

    case(i_ABP_CZM,i_IQS_ABP_CZM)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_abp(ibehav,cn,ct,s1,s2,G1,G2,du1,du2,phi)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(5),taz(3))

    case(i_EXPO_CZM,i_IQS_EXPO_CZM)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_expo(ibehav,cn,ct,s1,s2,G1,G2,eta)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(5),taz(3))

    case(i_EXPO_CZM_P,i_IQS_EXPO_CZM_P)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_expo_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*(internal(2)-internal(8))
          radh_n = tmp*cn*(internal(3)-internal(9))

         ! print*,'radh_n= ',radh_n,' radh_t= ',radh_t

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0

       endif

       p = H*internal(1)*beta*cn*internal(7)
       ! print*,'Hp= ',p

    case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)

       !fd sig_adh = beta c u
       call faterr('tact_behav::iter_CZM','expo_czm_spring not available in iter_czm')

    case(i_TOSI_CZM)

       call tosi_czm(0.d0,0.d0,0.d0,ibehav,internal,taz,radh_n,radh_t,xx,k_n,k_t,xx,.FALSE.)

       ! cut-off on beta
       if (internal(4) > 0.01 ) then

          is_cohesive=.true.

          tmp = -internal(1)*H

          k_t = tmp*H*k_t
          k_n = tmp*H*k_n

          radh_t = tmp*radh_t
          radh_n = tmp*radh_n

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = -internal(1)*H*radh_n   !Prise en compte de la pression dans les pores

       endif

    case(i_TOSI_CZM_INCRE)

       call tosi_czm_incre(0.d0,0.d0,0.d0,ibehav,internal,taz,H,radh_n,radh_t,xx,k_n,k_t,xx,.FALSE.)

       ! cut-off on beta
       if (internal(4) > 0.01 ) then

          is_cohesive=.true.
          tmp = -internal(1)*H

          k_t = tmp*H*k_t
          k_n = tmp*H*k_n

          radh_t = tmp*radh_t
          radh_n = tmp*radh_n

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = -internal(1)*H*radh_n   !Prise en compte de la pression dans les pores

       endif
    case default
       call faterr('tact_behav::iter_CZM','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

  end subroutine iter_CZM

!!!------------------------------------------------------------------------
  subroutine updt_CZM(ibehav,store,internal,taz,vt,vn)

    implicit none
    integer            :: ibehav

    real(kind=8):: cn,ct,b,w,b0,dc,nd,smax,dt,p,di,dk,di_

    real(kind=8):: dut,dun,ucut,ucun,nucut,nucun,beta,vt,vn,ucun_d,ucun_p,ucut_d,ucut_p,nd_p,dnd_p,ucut_p1,ucut_p2,ucun_p1,ucun_p2,store_,beta_status

    character(len=80) :: cout

    ! pour TH
    ! mode mixte
    real(kind=8) :: dp,du,tmp,dir2
    ! modes pures
    real(kind=8) :: s1,s2,G1,G2,di1,di2,dp1,dp2,du1,du2,C2,GG, &
                    k0,sp1,sp2,si,sp,phi,mu_g,eta,gi,k1,k2,scoh,G_f,phi_f

    ! tri
    real(kind=8) :: xc, fc, xd, f, Xxd, Xxc

    real(kind=8) :: xx1,xx2,xx3,xx4,xx5,xx6

    logical,intent(in) :: store

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz
    !fd calcul de beta stocke dans taz(1) et mise a jour de internal(4)

    dut=H*vt
    dun=H*vn


    select case(tact_behav(ibehav)%ilaw)
    case(i_MAC_CZM,i_IQS_MAC_CZM,i_ER_MAC_CZM,i_postGAP_IQS_MAC_CZM,i_ELASTIC_REPELL_MAC_CZM, &
         i_IQS_WET_CZM &
        )

      ucut  = internal(2) + dut
      ucun  = internal(3) + dun

      nucut = ucut**2
      nucun = ((dabs(ucun)+ucun)*0.5)**2

      call get_czm(ibehav,cn,ct,smax,w,b)

      if ((b + H*( (g1overg2*ct*nucut) + cn*nucun)) == 0.d0) then
        beta = 1.d0
      else
        beta = ((H*w) + (internal(4))*b) /(b + H*( (g1overg2*ct*nucut) + cn*nucun))
      endif

      if (beta < mac_beta_tol) taz(1) = 0.d0

      if (beta <= internal(4) .and. w <= (internal(4)*((g1overg2*ct*nucut) + cn*nucun))) taz(1) = beta

      !fd un beta de coupure positif ?

    case(i_MSMP_CZM)

      ucut  = internal(2) + dut
      ucun  = internal(3) + dun
      nucut = ucut**2
      nucun = ((dabs(ucun)+ucun)*0.5)**2
      nd = dsqrt(nucut+nucun)

      call get_czm(ibehav,cn,ct,smax,w,b)

      dc = dsqrt(0.28945047 * w * ((1.d0/cn) + (1.d0/ct)))

      if (nd < dc) then
         beta = min(internal(4),1.d0)
      else if ( dc <= nd .and. nd < 3.d0*dc) then
         beta = min(internal(4),b0*( ( ((3.d0*dc) - nd) * (2.d0*nd) ) /((dc + nd)*(2.d0*dc))))
      else
         beta = 0.d0
      endif

      taz(1) = beta

    case (i_MP_CZM )

     ucut  = internal(2) + dut
     ucun  = internal(3) + dun
     nucut = ucut**2
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     call get_czm(ibehav,cn,ct,smax,w,b)

     di =  (w/(9.d0 - 4.d0*log(4.d0)) * (1.d0/cn + 1.d0/ct))**0.5
     beta = (3.d0*di - nd) / (di + nd)

     if (beta < 0.d0 .or. nd > 3.d0*di) then

        taz(1) = 0.d0

     else if (beta <= internal(4) .and. nd >= di) then

        taz(1) = beta

     end if

    case (i_MP3_CZM, i_MP3_CZM_THER)

     ucut  = internal(2) + dut
     ucun  = internal(3) + dun
     nucut = ucut**2
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     call get_czm(ibehav,cn,ct,smax,w,b)

     di = ( smax*0.5 ) * ( 1.d0/cn + 1.d0/ct )
     dk = 1.5 * ( w/smax - di*0.5 )

     if (dk == 0.d0) then

        di = 0.d0
        beta = 0.d0

     else

        beta = (di / nd) * (1.d0 - ((nd - di)/dk)**2)

     endif

     if ( nd > ( di + dk )) then

        taz(1) = 0.d0

     else if (beta <= internal(4) .and. nd >= di ) then

        taz(1) = beta

     end if

    case (i_IQS_MAL_CZM,i_MAL_CZM)

      ucut  = internal(2) + dut
      ucun  = internal(3) + dun
      nucut = ucut**2
      nucun = ((dabs(ucun)+ucun)*0.5)**2
      nd = dsqrt(nucut+nucun)

      call get_czm_mal(ibehav,cn,ct,s1,s2,G1,G2)

      if (nucun > tiny(nucun)) then
        dir2 = nucut / nucun
      else
        dir2 = 1d+20
      endif

      di1 = s1/cn
      di2 = s2/ct

      ! la rigidite initiale au carre
      C2 = (cn/(1.d0+dir2)) + (ct*dir2/(1.d0+dir2))
      GG = (G2*cn/(1.d0+dir2)) + (G1*ct*dir2/(1.d0+dir2))

      !dc = 0.5 * smax * ((1.d0/cn) + (1.d0/ct))

      dc = di1*di2*sqrt((1.d0+(dir2))/ ((di2**2) + (dir2)*(di1**2)))

      !dt = 1.5 * ((w/smax) -(0.5 * dc))

      dt = 1.5*(G1*G2 - (0.5 * dc**2 * GG))/(dc * GG)

      if (nd < dc) then
        beta = min(internal(4),1.d0)
      else if ( dc <= nd .and. nd < (dc+dt)) then
        beta = min(internal(4),(dc/nd)*(1.d0 - (((nd - dc)/dt)**2)))
      else
        beta = 0.d0
      endif

      taz(1) = beta

    case(i_TH_CZM,i_IQS_TH_CZM)

      !   ___
      ! /     \
      !  di dp du

      ucut  = internal(2) + dut
      ucun  = internal(3) + dun
      nucut = ucut**2
      nucun = ((dabs(ucun)+ucun)*0.5)**2
      nd = dsqrt(nucut+nucun)

      call get_czm_th(ibehav,cn,ct,s1,s2,G1,G2,dp1,dp2)

      if (nucun > tiny(nucun)) then
        dir2 = nucut / nucun
      else
        dir2 = 1d+20
      endif

      di1 = s1/cn
      di2 = s2/ct

      !di = ( smax * 0.5 ) * ( 1.d0/cn + 1.d0/ct )

      di = di1*di2*sqrt((1+(dir2))/ ((di2**2) + (dir2)*(di1**2)))

      dp = dp1*dp2*sqrt((1+(dir2))/ ((dp2**2) + (dir2)*(dp1**2)))

      tmp =  (cn*G2) + (dir2*ct*G1)

      du = ( (2.d0*G1*G2*(1+dir2)) - ( di*(dp-di)*tmp))/(di*tmp)

      !dk = ( 2.d0 * w / smax ) + ( di - d )

      beta = 1.d0

      if ( nd > du ) then

         beta = 0.d0

      else if ( nd > dp .and. nd <= du ) then

         beta = ( di / nd ) * ( ( du - nd ) / ( du - dp ) )

      else if ( nd > di .and. nd <= dp) then

         beta = di / nd

      end if

      if ( beta <= internal(4) ) then

         taz(1) = beta

      else

         taz(1)= internal(4)

      end if

    case(i_ABP_CZM, i_IQS_ABP_CZM)

     !  /  \
     ! /    ----
     ! 0 di dp du


     ! indices e=i, p=c, u=u

     ucut  = internal(2) + dut
     ucun  = internal(3) + dun

     nucut = (ucut**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     call get_czm_abp(ibehav,cn,ct,s1,s2,G1,G2,du1,du2,phi)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! 2*b/du ; b=w*(1-phi)
     sp1 = 2.d0*G1*(1.d0-phi)/du1
     sp2 = 2.d0*G2*(1.d0-phi)/du2

     ! (2*mu + sp*di)/smax ; mu = w*p
     dp1 = ((2.d0*G1*phi) + (sp1*di1))/s1
     dp2 = ((2.d0*G2*phi) + (sp2*di2))/s2

     ! mixing
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     dp = dp1*dp2*sqrt(1.d0+(dir2))*sqrt((di2**2) + (dir2)*(di1**2))/(di2*dp2 + (dir2*di1*dp1))

     du = ((1.d0+dir2)/dp)*(dp1*du1*dp2*du2)/(dp2*du2+dir2*dp1*du1)

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0+ dir2))

     si = k0 * di

     sp = si * dp / ( di + (phi*du/(1.d0-phi)))


     !print*,'------------------'
     !print*,cn,ct,s1,G1,du1,phi
     !print*,s1,s2,g1,g2,di1,di2,dp1,dp2
     !print*,dir,k0,di,dp,du,si,sp

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > dp .and. nd <= du ) then

        beta = (sp + ((sp/ (dp-du)) * (nd-dp) ) ) /(nd*k0)

     else if ( nd > di .and. nd <= dp) then

        beta = (si + ( ((sp - si)/(dp-di))*(nd-di) ) ) /(nd*k0)

     end if

     !print*,'-----'
     !print*,di,si,k0
     !print*,dp,sp,du
     !print*,nd,beta

     if ( beta <= internal(4) ) then

        taz(1) = beta

      else

         taz(1) = internal(4)

     end if

    case(i_EXPO_CZM, i_IQS_EXPO_CZM)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     ucut  = internal(2) + dut
     ucun  = internal(3) + dun

     nucut = (ucut**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     call get_czm_expo(ibehav,cn,ct,s1,s2,G1,G2,eta)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir2 * di1**2 * G2) ) / ((di2**2) + (dir2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0+ dir2))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     !print*,'------------------'
     !print*,s1,s2,g1,g2,di1,di2,dp1,dp2
     !print*,dir,k0,di,dp,du,si,sp

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/(k0*nd)) * exp(phi*(di-nd))

     end if

     !print*,'-----'
     !print*,di,si,k0
     !print*,dp,sp,du
     !print*,nd,beta

     if ( beta <= internal(4) ) then

        taz(1) = beta

     else

        taz(1) = internal(4)

     end if

    case(i_TOSI_CZM)

       call tosi_czm(dut,dun,0.d0,ibehav,internal,taz,xx1,xx2,xx3,xx4,xx5,xx6,store)
       return

    case(i_TOSI_CZM_INCRE)

       call tosi_czm_incre(vt,vn,0.d0,ibehav,internal,taz,H,xx1,xx2,xx3,xx4,xx5,xx6,store)
       return

    case(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     !! saut de deplacement total
     ucut  = internal(2) + dut
     ucun  = internal(3) + dun

     !! saut de deplacements endommageable
     ucut_d  = ucut - internal(8)
     ucun_d  = ucun - internal(9)

     nucut = (ucut_d**2)
     nucun = ((dabs(ucun_d)+ucun_d)*0.5)**2
     nd = dsqrt(nucut+nucun)


     if (nucun > 1d-14) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     call get_czm_expo_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir2 * di1**2 * G2) ) / ((di2**2) + (dir2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0+ dir2))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     !print*,'------------------'
     !print*,s1,s2,g1,g2,di1,di2,dp1,dp2
     !print*,dir,k0,di,dp,du,si,sp

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/(k0*nd)) * exp(phi*(di-nd))

     end if

     if ( beta <= internal(4) ) then
       taz(1) = beta
       if (beta < 1.d0) then
         !!! calcul saut de deplacement plastique normal
         ! G_f
         G_f=gi/mu_g
         ! phi_f : parametre expo
         phi_f = (k0 * di)/(G_f - (0.5d0 * k0 * di * di))

         if (taz(1)/=0.d0) then
           ! contrainte normal
           scoh=taz(1)*k0*nd
           nd_p=(di-(log(scoh/si)/phi_f) - nd)
           ucun_p=nd_p*cos(atan(dir2))
           ucut_p=nd_p*sin(atan(dir2))
         else
           ucun_p=internal(7)
           ucut_p=internal(6)
         end if

         ! correction du deplacement plastique tangentielle et normal
         ucut_p1=internal(8)+sign(abs(ucut_p-internal(6)),ucut_d)
         ucun_p1=internal(9)+sign(abs(ucun_p-internal(7)),ucun_d)

         if ((internal(4)<1) .and. (0<internal(4)) .and. (internal(6)==0) .and. (internal(7)==0)) then
            scoh=internal(4)*k0*nd
            nd_p=(di-(log(scoh/si)/phi_f) - nd)

            ucun_p2=nd_p*cos(atan(dir2))
            ucut_p2=nd_p*sin(atan(dir2))
            ucut_p1=ucut_p1-sign(abs(ucut_p2),ucut_d)
            ucun_p1=ucun_p1-sign(abs(ucun_p2),ucun_d)
         end if

       else

         !!! calcul saut de deplacement plastique normal
         ! deplacement plastique
         ucun_p  = internal(7)
         ucun_p1 = internal(9)

         ! deplacement plastique
         ucut_p  = internal(6)
         ucut_p1 = internal(8)

       end if

     else

       taz(1) = internal(4)

       !!! calcul saut de deplacement plastique normal
       ! deplacement plastique
       ucun_p  = internal(7)
       ucun_p1 = internal(9)

       ! deplacement plastique
       ucut_p  = internal(6)
       ucut_p1 = internal(8)
     end if

     !!aliboukham
     if( itchatche ) then
       write(cout,'(1x,"updt_czm !!!!!!!!!!!!!!!!!!!!!!!!  end")')
       call LOGMES(cout)
       print*,'beta= ',taz(1)
       write(cout,'(1x,"ut= ",D14.7," un= ",D14.7)') ucut,ucun
       call LOGMES(cout)
       write(cout,'(1x,"ut_p= ",D14.7," un_p= ",D14.7)') ucut_p,ucun_p
       call LOGMES(cout)
     end if

    case default

       call faterr('tact_behav::updt_CZM','incompatible law '//trim(tact_behav(ibehav)%lawty))

    end select

    if (store) then

      internal(4)=taz(1)
      internal(2)=ucut
      internal(3)=ucun
      internal(6)=ucut_p
      internal(7)=ucun_p
      internal(8)=ucut_p1
      internal(9)=ucun_p1

      if (internal(5)==-99.d0 .and. taz(1) < 1.d0) internal(5)=TPS

      !fd pas utile !?

      ! if (taz(1) == 0.d0) then

      !   internal(2)=0.d0
      !   internal(3)=0.d0

      ! end if
    end if

  end subroutine updt_CZM

!!!------------------------------------------------------------------------
  subroutine updt_CZM_spring(ibehav,store,internal,taz,dut,dun,rlt,rln)

    implicit none
    integer     :: ibehav
    real(kind=8):: dut,dun,rlt,rln

    real(kind=8):: cn,ct,ucut,ucun,nucut,nucun,beta,nd,di

    ! pour TH
    ! mode mixte
    real(kind=8) :: dp,du,tmp,dir2
    ! modes pures
    real(kind=8) :: s1,s2,G1,G2,di1,di2,dp1,dp2,du1,du2,C2,GG, &
                    k0,sp1,sp2,si,sp,phi,eta,gi,k1,k2

    logical,intent(in) :: store

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz

                             !123456789012345678901234567
    character(len=27) :: IAM='tact_behav::updt_CZM_spring'

    !fd calcul de beta stocke dans taz(1) et mise a jour de internal(4)

    select case(tact_behav(ibehav)%ilaw)
    case(i_EXPO_CZM_SPRING, i_IQS_EXPO_CZM_SPRING)

     call get_czm_expo_spring(ibehav,cn,ct,s1,s2,G1,G2,eta,k1,k2)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     ! on reconstruit le deplacement de la partie endommageable
     ! rl est une impulsion dont rl/H est une force et k une raideur
     ! rt > 0 [u]<0 compression rl < 0 [u] > 0 traction (c est pourquoi on prend -rl)
     !

     ! calcul de l'allongement endo tangent [ut]-[uet]
     ucut  = internal(2) + internal(6) + dut - (-rlt/(H*internal(1)*(k2*internal(9))))

     ! print*,'[ut] =', internal(2) + internal(6) + dut
     ! print*,'[uet]=',(-rlt/(H*k2))
     ! print*,'[udt]=',ucut

     ! calcul de l'allongement endo normal
     ucun  = dun - (-rln/(H*internal(1)*(k1*internal(9))))

     ! ! print*,'[un] =',dun
     ! print*,'[uen]=',(-rln/(H*k1))
     ! print*,'[udn]=',ucun

     ! <un>_+
     nucut = (ucut**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2

     nd = dsqrt(nucut+nucun)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir2 * di1**2 * G2) ) / ((di2**2) + (dir2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0 + dir2))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     ! print*,'nd= ',nd,' di= ',di,' du= ',du

     ! </fd
     ! print*,'</-----------'
     ! print*,cn,ct,s1,s2,g1,g2,eta
     ! print*,di1,di2
     ! print*,dir,k0,si,di,gi,du,phi
     ! print*,internal(3),internal(2)
     ! print*,dun,dut
     ! print*,(-rln/(H*H*k1)),(-rlt/(H*H*k2))
     ! print*,ucun,ucut,nd
     ! fd/>

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/((k0*internal(8))*nd)) * exp(phi*(di-nd))

     end if

     !</fd
     ! print*,'beta =',beta
     ! print*,'-----------/>'
     ! fd/>

     if ( beta <= internal(4) ) then

        taz(1) = beta

      else

        taz(1) = internal(4)

     end if

    case default
       call faterr('tact_behav::updt_CZM_spring','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

    if (store) then


      ! allongement endo tangent
      internal(2)= ucut
      ! allongement endo normal
      internal(3)= ucun
      ! beta
      internal(4)= taz(1)
      ! declenchement de l'endommagement
      if (internal(5)==-99.d0 .and. taz(1) < 1.d0) internal(5)=TPS
      ! allongement elastique tangent
      internal(6)=-rlt/(H*internal(1)*(internal(9)*k2))


    end if


  end subroutine updt_CZM_spring

 !!!------------------------------------------------------------------------
  subroutine updt_CZM_spring_p(ibehav,store,internal,taz,dut,dun,rlt,rln)

    implicit none
    integer     :: ibehav
    real(kind=8):: dut,dun,rlt,rln

    real(kind=8):: cn,ct,ucut,ucun,nucut,nucun,beta,nd,nd_p,di,ucut_p,ucut_p1,ucun_p,ucun_p1,ucut_d,ucun_d,scoh
    ! real(kind=8):: nd_p2 !!!ab added under test
    real(kind=8):: ucut_p2, ucun_p2 !!!ab added under test

    ! pour TH
    ! mode mixte
    real(kind=8) :: dp,du,tmp,dir
    ! modes pures
    real(kind=8) :: s1,s2,G1,G2,di1,di2,dp1,dp2,du1,du2,C2,GG, &
                    k0,sp1,sp2,si,sp,phi,mu_g,eta,gi,k1,k2,phi_f,G_f

    logical,intent(in) :: store

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz

                             !123456789012345678901234567
    character(len=27) :: IAM='tact_behav::updt_CZM_spring'

    !fd calcul de beta stocke dans taz(1) et mise a jour de internal(4)

    select case(tact_behav(ibehav)%ilaw)
    case(i_EXPO_CZM_SPRING_P, i_IQS_EXPO_CZM_SPRING_P)

     call get_czm_expo_spring_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta,k1,k2)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     ! on reconstruit le deplacement de la partie endommageable
     ! rl est une impulsion dont rl/H est une force et k une raideur
     ! rt > 0 [u]<0 compression rl < 0 [u] > 0 traction (c est pourquoi on prend -rl)
     !

     ! calcul de l'allongement endo tangent [ut]-[uet]
     ucut  = internal(2) + internal(6) + dut - (-rlt/(H*internal(1)*k2))

     ! calcul de l'allongement endo normal
     ucun  = dun - (-rln/(H*internal(1)*k1))

     !! saut de deplacements endommageable
     ucut_d  = ucut - internal(10)
     ucun_d  = ucun - internal(11)

     nucut = (ucut_d**2)
     nucun = ((dabs(ucun_d)+ucun_d)*0.5)**2
     nd = dsqrt(nucut+nucun)


     if (nucun > 1d-14) then
       dir = nucut / nucun
     else
       dir = 1d+20
     endif

     call get_czm_expo_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir**2)) / ((di2**2) + (dir**2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir**2 * di1**2 * G2) ) / ((di2**2) + (dir**2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir*dir)/(1.d0+ dir*dir))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     !print*,'------------------'
     !print*,s1,s2,g1,g2,di1,di2,dp1,dp2
     !print*,dir,k0,di,dp,du,si,sp

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/(k0*nd)) * exp(phi*(di-nd))

     end if

     ! stop

     if ( beta <= internal(4) ) then
        taz(1) = beta
        if (beta < 1.d0) then
          !!! calcul saut de deplacement plastique normal
            ! G_f
            G_f=gi/mu_g
            ! phi_f : parametre expo
            phi_f = (k0 * di)/(G_f - (0.5d0 * k0 * di * di))
            ! deplacement plastique
            if (taz(1)/=0.d0) then
                ! contrainte normale
                scoh=taz(1)*k0*nd
                nd_p=(di-(log(scoh/si)/phi_f) - nd)
                ! print*,scoh,taz(1),nd_p
                ucun_p=nd_p*cos(atan(dir))
                ucut_p=nd_p*sin(atan(dir))
            else
                ! scoh=internal(4)*k0*nd
                ! nd_p=(di-(log(scoh/si)/phi_f) - nd)
                ! nd_p=(di-(log(scoh/si)/phi_f) - nd)
                ! nd_p=0.d0
                ! print*,taz(1)
                ! print*,scoh,internal(4),nd_p
                ! stop
                ucun_p=internal(9)
                ucut_p=internal(8)
            end if
            !!!!! under test
            ! contrainte normale
            ! scoh=internal(4)*k0*nd
            ! deplacement plastique
            ! if (scoh/=0.d0) then
            ! nd_p2=(di-(log(scoh/si)/phi_f) - nd)
            ! else
                ! nd_p2=0.d0
            ! end if
            !!!!!
            ! deplacement plastique
            ! ucun_p=nd_p*cos(atan(dir))
            ! ucun_p=internal(9)+(nd_p-nd_p2)*cos(atan(dir))

            ! deplacement plastique tangentielle
            ! ucut_p=nd_p*sin(atan(dir))
            ! ucut_p=internal(8)+(nd_p-nd_p2)*sin(atan(dir))

            ! correction du deplacement plastique tangentielle et normal
            ucut_p1=internal(10)+sign(abs(ucut_p-internal(8)),ucut_d)
            ucun_p1=internal(11)+sign(abs(ucun_p-internal(9)),ucun_d)

            ! print*, ucun_p1
            ! stop
            if ((internal(4)<1) .and. (0<internal(4)) .and. (internal(8)==0) .and. (internal(9)==0)) then
                scoh=internal(4)*k0*nd
                nd_p=(di-(log(scoh/si)/phi_f) - nd)

                ucun_p2=nd_p*cos(atan(dir))
                ucut_p2=nd_p*sin(atan(dir))
                ucut_p1=ucut_p1-sign(abs(ucut_p2),ucut_d)
                ucun_p1=ucun_p1-sign(abs(ucun_p2),ucun_d)
            end if
        else
            !!! calcul saut de deplacement plastique normal
            ! deplacement plastique
            ucun_p  = internal(9)
            ucun_p1 = internal(11)

            ! deplacement plastique
            ucut_p  = internal(8)
            ucut_p1 = internal(10)


        end if

     else

        taz(1) = internal(4)

        !!! calcul saut de deplacement plastique normal
        ! deplacement plastique
        ucun_p  = internal(9)
        ucun_p1 = internal(11)

        ! deplacement plastique
        ucut_p  = internal(8)
        ucut_p1 = internal(10)
     end if

    case default
       call faterr('tact_behav::updt_CZM_spring_p','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

    if (store) then

      ! allongement endo tangent
      internal(2)= ucut
      ! allongement endo normal
      internal(3)= ucun
      ! beta
      internal(4)= taz(1)
      ! declenchement de l'endommagement
      if (internal(5)==-99.d0 .and. taz(1) < 1.d0) internal(5)=TPS
      ! allongement elastique tangent
      internal(6)=-rlt/(H*internal(1)*k2)
      internal(8)=ucut_p
      internal(9)=ucun_p
      internal(10)=ucut_p1
      internal(11)=ucun_p1

    !! ab added under test
    !  if ((taz(1)<1) .and. (0<taz(1)) .and. (internal(8)==0) .and. (internal(9)==0)) then
    !    print*, 'we are here', taz(1), internal(8), internal(9)
    !    stop
    !    G_f=gi/mu_g
    !    phi_f = (k0 * di)/(G_f - (0.5d0 * k0 * di * di))
    !    scoh=taz(1)*k0*nd
    !    if (scoh/=0.d0) then
           ! nd_p=(di-(log(scoh/si)/phi_f) - nd)
    !    else
           ! nd_p=0.d0
    !    end if

    !    ucun_p=nd_p*cos(atan(dir))
    !    ucut_p=nd_p*sin(atan(dir))
    !    ucut_p1=internal(10)+sign(abs(ucut_p-internal(8)),ucut_d)
    !    ucun_p1=internal(11)+sign(abs(ucun_p-internal(9)),ucun_d)
    !    print*,ucun_p, ucut_p

    !    internal(8)-=ucut_p
    !    internal(9)-=ucun_p
    !    internal(10)-=ucut_p1
    !    internal(11)-=ucun_p1
    !  end if

    end if


  end subroutine updt_CZM_spring_p
!!!------------------------------------------------------------------------

! !!!------------------------------------------------------------------------
!   subroutine raz_CZM(ibehav,internal)

!     implicit none
!     integer :: ibehav
!     real(kind=8),dimension(max_internal_tact) :: internal

!     if (      tact_behav(ibehav)%ilaw.eq.i_MAC_CZM  &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MAC_CZM_nosldt &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MAC_CZM_noslds &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_ER_MAC_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM_nosldt &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM_noslds  &
!          .or. tact_behav(ibehav)%ilaw.eq.i_postGAP_IQS_MAC_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_ELASTIC_REPELL_MAC_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_WET_CZM  &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MSMP_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MP_CZM  &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MP3_CZM  &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MP3_CZM_THER  &
!          .or. tact_behav(ibehav)%ilaw.eq.i_MAL_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAL_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_TH_CZM   &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_TH_CZM   &
!          .or. tact_behav(ibehav)%ilaw.eq.i_ABP_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_ABP_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM &
!          .or. tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM_SPRING &
!          .or. tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM_SPRING &
!        ) then

!        !internal(2:6)=0.d0

!     else
!        call faterr('tact_behav::raz_CZM','incompatible law '//trim(tact_behav(ibehav)%lawty))
!     end if

!   end subroutine raz_CZM
!!!------------------------------------------------------------------------
  subroutine init_CZM_3D(ibehav,internal,taz,dead)

    ! initialisations faites au premier pas de temps
    implicit none
    integer :: ibehav
    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz
    logical, optional :: dead
    logical :: skip

    if (.not. present(dead)) then
       skip=.False.
    else
       skip=dead
    endif

    if (      tact_behav(ibehav)%ilaw.eq.i_MAC_CZM  &
         .or. tact_behav(ibehav)%ilaw.eq.i_MAC_CZM_nosldt &
         .or. tact_behav(ibehav)%ilaw.eq.i_MAC_CZM_noslds &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM_nosldt &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM_noslds  &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_ER_MAC_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MP_CZM  &
         .or. tact_behav(ibehav)%ilaw.eq.i_MP3_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MP3_CZM_THER &
         .or. tact_behav(ibehav)%ilaw.eq.i_TH_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_TH_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MAL_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAL_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_ABP_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_ABP_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM_P &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM_P &
         .or. tact_behav(ibehav)%ilaw .eq. i_EXPO_CZM_SPRING_P &
         .or. tact_behav(ibehav)%ilaw .eq. i_IQS_EXPO_CZM_SPRING_P ) then

       !fd interface non endommage

       if (NSTEP == 1) then
          internal=0.d0
          if (skip) then
            internal(5) = 0.d0
            internal(6) = 0.d0
          else
            internal(5) = 1.d0
            internal(6) = -99.d0
          endif
       end if

       taz(1) = internal(5)

    else if(     tact_behav(ibehav)%ilaw .eq. i_EXPO_CZM_SPRING &
            .or. tact_behav(ibehav)%ilaw .eq. i_IQS_EXPO_CZM_SPRING &
           ) then

       !fd interface non endommage

       if (NSTEP == 1) then
          internal=0.d0
          if (skip) then
            internal(5) = 0.d0
            internal(6) = 0.d0
          else
            internal(5) = 1.d0
            internal(6) = -99.d0
            internal(10) = 1.d0
            internal(11) = 1.d0
          endif
       end if

       taz(1) = internal(5)


    else if (tact_behav(ibehav)%ilaw == i_TOSI_CZM .or. &
             tact_behav(ibehav)%ilaw == i_TOSI_CZM_INCRE) then
       !fd interface non endommage

       if (NSTEP == 1) then
          internal=0.d0
          ! valeur de l'endommagement initiale
          internal(5) = 1.d0 - tact_behav(ibehav)%param(5) ! Beta = 1-fn=1-f0
          internal(13)= 1.                                 ! fc=1.
       end if


    else
       call faterr('tact_behav::init_CZM_3D','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end if

  end subroutine init_CZM_3D
!!!------------------------------------------------------------------------
  subroutine prep_CZM_3D(ibehav,internal,cd_surf,ucut,ucun,ucus)

    implicit none
    integer :: ibehav
    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8):: cd_surf,cn,ct,b,w,ucun,ucut,ucus

    if (      tact_behav(ibehav)%ilaw.eq.i_MAC_CZM  &
         .or. tact_behav(ibehav)%ilaw.eq.i_MAC_CZM_nosldt &
         .or. tact_behav(ibehav)%ilaw.eq.i_MAC_CZM_noslds &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM_nosldt &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM_noslds  &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAC_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_ER_MAC_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MP_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MP3_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MP3_CZM_THER &
         .or. tact_behav(ibehav)%ilaw.eq.i_TH_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_TH_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_MAL_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_MAL_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_ABP_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_ABP_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM &
         .or. tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM_P &
         .or. tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM_P &
       ) then

       internal(1)=cd_surf
       !       internal(2)=ucut
       internal(3)=ucun
       !       internal(4)=ucus
       !fd on considere que saut_de_un= gap-xg0
       if (tact_behav(ibehav)%ilaw.eq.i_EXPO_CZM .or. &
           tact_behav(ibehav)%ilaw.eq.i_IQS_EXPO_CZM ) then


           internal(3) = internal(3) - internal(7)
       end if

    else if ( tact_behav(ibehav)%ilaw .eq. i_EXPO_CZM_SPRING       .or. &
              tact_behav(ibehav)%ilaw .eq. i_IQS_EXPO_CZM_SPRING   .or. &
              tact_behav(ibehav)%ilaw .eq. i_EXPO_CZM_SPRING_P     .or. &
              tact_behav(ibehav)%ilaw .eq. i_IQS_EXPO_CZM_SPRING_P .or. &
              tact_behav(ibehav)%ilaw .eq. i_TOSI_CZM              .or. &
              tact_behav(ibehav)%ilaw .eq. i_TOSI_CZM_INCRE             &
            ) then

           internal(1)=cd_surf

           !fd on ne touche pas ces quantites qui sont modifiees lors du update
           ! internal(2)=ucut
           ! internal(3)=ucun
           ! internal(4)=ucus


    else
       call faterr('tact_behav::prep_CZM_3D','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end if

  end subroutine prep_CZM_3D
!!!------------------------------------------------------------------------
  subroutine iter_CZM_3D(ibehav,internal,taz,k_t,k_n,is_cohesive,radh_t,radh_n,radh_s,p)

    implicit none
    integer     :: ibehav
    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz
    real(kind=8):: cn,ct,b,w,smax,d,p
    real(kind=8):: beta,k_t,k_n,k_s,radh_t,radh_n,radh_s,tmp
    logical :: is_cohesive
                             !12345678901234567890123
    character(len=23) :: IAM='tact_behav::iter_CZM_3D'

    ! abp
    real(kind=8) :: s1,s2,G1,G2,du1,du2,phi

    ! th
    real(kind=8) :: dp1,dp2

    ! expo
    real(kind=8) :: eta, mu_g


    is_cohesive = .false.


    select case(tact_behav(ibehav)%ilaw)
    case(i_MAC_CZM,  &
         i_MAC_CZM_nosldt, &
         i_MAC_CZM_noslds, &
         i_IQS_MAC_CZM_nosldt, &
         i_IQS_MAC_CZM_noslds,  &
         i_IQS_MAC_CZM, &
         i_ER_MAC_CZM)

       !fd sig_adh = beta^2 c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm(ibehav,cn,ct,smax,w,b)

          tmp = - internal(1)*beta*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)
          radh_s = tmp*ct*internal(4)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0
          radh_s = 0.d0
       end if

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case( i_MP_CZM, &
          i_MP3_CZM, &
          i_MP3_CZM_THER)

       !fp sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm(ibehav,cn,ct,smax,w,b)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)
          radh_s = tmp*ct*internal(4)

       else

          k_t = 0.d0
          k_n = 0.d0

          radh_t = 0.d0
          radh_n = 0.d0
          radh_s = 0.d0

       end if

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case(i_IQS_MAL_CZM, i_MAL_CZM )

       !fp sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_mal(ibehav,cn,ct,s1,s2,g1,g2)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)
          radh_s = tmp*ct*internal(4)

       else

          k_t = 0.d0
          k_n = 0.d0

          radh_t = 0.d0
          radh_n = 0.d0
          radh_s = 0.d0

       end if

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case( i_TH_CZM, i_IQS_TH_CZM)

       !fp sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_th(ibehav,cn,ct,s1,s2,g1,g2,dp1,dp2)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)
          radh_s = tmp*ct*internal(4)

       else

          k_t = 0.d0
          k_n = 0.d0

          radh_t = 0.d0
          radh_n = 0.d0
          radh_s = 0.d0

       end if

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case( i_ABP_CZM, i_IQS_ABP_CZM )

       !fp sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_abp(ibehav,cn,ct,s1,s2,G1,G2,du1,du2,phi)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)
          radh_s = tmp*ct*internal(4)

       else

         k_t = 0.d0
         k_n = 0.d0

         radh_t = 0.d0
         radh_n = 0.d0
         radh_s = 0.d0

       end if

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case(i_EXPO_CZM,i_IQS_EXPO_CZM)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_expo(ibehav,cn,ct,s1,s2,G1,G2,eta)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*internal(2)
          radh_n = tmp*cn*internal(3)
          radh_s = tmp*ct*internal(4)

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0
          radh_s = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case(i_EXPO_CZM_P,i_IQS_EXPO_CZM_P)

       !fd sig_adh = beta c u

       beta = taz(1)

       if (beta > mac_beta_tol ) then

          is_cohesive=.true.

          call get_czm_expo_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta)

          tmp = - internal(1)*beta*H

          k_t = tmp*ct*H
          k_n = tmp*cn*H

          radh_t = tmp*ct*(internal(2)-internal(11))
          radh_n = -tmp*cn*internal(9)
          radh_s = tmp*ct*(internal(4)-internal(12))

       else

          k_t=0.d0
          k_n=0.d0

          radh_t = 0.d0
          radh_n = 0.d0
          radh_s = 0.d0

       endif

       p = H*internal(1)*get_pressure(ibehav,beta,internal(6),taz(3))

    case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)

       !fd sig_adh = beta c u
       call faterr('tact_behav::iter_CZM_3D','expo_czm_spring not available in iter_czm')

    case(i_EXPO_CZM_SPRING_P,i_IQS_EXPO_CZM_SPRING_P)

       !fd sig_adh = beta c u
       call faterr('tact_behav::iter_CZM_3D','expo_czm_spring_p not available in iter_czm')

    case(i_TOSI_CZM)

       call tosi_czm(0.d0,0.d0,0.d0,ibehav,internal,taz,radh_n,radh_t,radh_s,k_n,k_t,k_s,.FALSE.)

       if (internal(5) > 0.01 ) then

          is_cohesive=.true.

          tmp = - internal(1)*H

          k_t = tmp*H*k_t
          k_n = tmp*H*k_n
          k_s = tmp*H*k_s

          radh_t = tmp*radh_t
          radh_n = tmp*radh_n
          radh_s = tmp*radh_s

       else

          k_t=0.d0
          k_n=0.d0
          k_s=0.d0

          radh_t = 0.d0
          radh_n = -internal(1)*H*radh_n   !Prise en compte de la pression dans les pores
          radh_s = 0.d0

       endif

    case(i_TOSI_CZM_INCRE)

       call tosi_czm_incre(0.d0,0.d0,0.d0,ibehav,internal,taz,H,radh_n,radh_t,radh_s,k_n,k_t,k_s,.FALSE.)

       if (internal(5) > 0.01 ) then

          is_cohesive=.true.

          tmp = - internal(1)*H

          k_t = tmp*H*k_t
          k_n = tmp*H*k_n
          k_s = tmp*H*k_s

          radh_t = tmp*radh_t
          radh_n = tmp*radh_n
          radh_s = tmp*radh_s

       else

          k_t=0.d0
          k_n=0.d0
          k_s=0.d0

          radh_t = 0.d0
          radh_n = -internal(1)*H*radh_n   !Prise en compte de la pression dans les pores
          radh_s = 0.d0

       endif

    case default
       call faterr('tact_behav::iter_CZM_3D','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

  end subroutine iter_CZM_3D
!!!------------------------------------------------------------------------
  subroutine updt_CZM_3D(ibehav,store,internal,taz,vt,vn,vs)

    implicit none
    integer      :: ibehav
    real(kind=8) :: cn,ct,b,w,smax,d,p,di,dk

    real(kind=8) :: dut,dun,dus,ucut,ucun,ucus,nucut,nucun,beta,C2,GG,vt,vn,vs,ucut_d,ucus_d,ucun_d,ucun_p,ucut_p,ucut_p1,ucut_p2,ucus_p,ucus_p1,ucus_p2,nd_p,ucuts_p,ucun_p1,ucun_p2,ucuts_p2

    logical,intent(in) :: store

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz

    real(kind=8) :: dc,dt,nd

    ! pour TH
    ! mode mixte

    real(kind=8) :: dp,du,tmp,dir2,dirts

    ! modes pures
    real(kind=8) :: s1,s2,G1,G2,di1,di2,dp1,dp2,du1,du2,k0,sp1,sp2,si,sp,phi,scoh,phi_f,G_f

    ! expo
    real(kind=8) :: eta,gi,mu_g

    ! tri
    real(kind=8) :: xc, fc, xd


    real(kind=8) :: xx1,xx2,xx3,xx4,xx5,xx6

                             !12345678901234567890123
    character(len=23) :: IAM='tact_behav::updt_CZM_3D'

    dut=H*vt
    dun=H*vn
    dus=H*vs


    select case(tact_behav(ibehav)%ilaw)
    case( i_MAC_CZM, &
          i_MAC_CZM_nosldt, &
          i_MAC_CZM_noslds, &
          i_IQS_MAC_CZM_nosldt, &
          i_IQS_MAC_CZM_noslds, &
          i_IQS_MAC_CZM, &
          i_ER_MAC_CZM )

       ucut  = internal(2) + dut
       ucun  = internal(3) + dun
       ucus  = internal(4) + dus

       nucut = (ucut**2) + (ucus**2)
       nucun = ((dabs(ucun)+ucun)*0.5)**2

       call get_czm(ibehav,cn,ct,smax,w,b)

       if ((b + H*( g1overg2*ct*nucut + cn*nucun)) == 0.d0) then
         beta = 1.d0
       else
         beta = ((H*w) + (internal(5))*b) /(b + H*( g1overg2*ct*nucut + cn*nucun))
       endif

       if (beta < mac_beta_tol) beta = 0.d0

       if (beta <= internal(5) .and. w <= (internal(5)*(g1overg2*ct*nucut + cn*nucun))) taz(1) = beta

    case (i_IQS_MAL_CZM,i_MAL_CZM)

       ucut  = internal(2) + dut
       ucun  = internal(3) + dun
       ucus  = internal(4) + dus

       nucut = (ucut**2) + (ucus**2)
       nucun = ((dabs(ucun)+ucun)*0.5)**2
       nd = dsqrt(nucut+nucun)

       call get_czm_mal(ibehav,cn,ct,s1,s2,g1,g2)

       if (nucun > tiny(nucun)) then
         dir2 = nucut / nucun
       else
         dir2 = 1d+20
       endif

       di1 = s1/cn
       di2 = s2/ct

       ! la rigidite initiale au carre
       C2 = (cn/(1+dir2)) + (ct*dir2/(1.d0+dir2))
       GG = (G2*cn/(1.d0+dir2)) + (G1*ct*dir2/(1+dir2))

      !dc = 0.5 * smax * ((1.d0/cn) + (1.d0/ct))

       dc = di1*di2*sqrt((1.d0+(dir2))/ ((di2**2) + (dir2)*(di1**2)))

       !dt = 1.5 * ((w/smax) -(0.5 * dc))

       dt = 1.5*(G1*G2 - (0.5 * dc**2 * GG))/(dc * GG)

       if (nd < dc) then
          beta = min(internal(5),1.d0)
       else if ( dc <= nd .and. nd < (dc+dt)) then
          beta = min(internal(5),(dc/nd)*(1.d0 - (((nd - dc)/dt)**2)))
       else
          beta = 0.d0
       endif

       taz(1) = beta

    case (i_MP_CZM )

       ucut  = internal(2) + dut
       ucun  = internal(3) + dun
       ucus  = internal(4) + dus

       nucut = (ucut**2) + (ucus**2)
       nucun = ((dabs(ucun)+ucun)*0.5)**2
       nd = dsqrt(nucut+nucun)

       call get_czm(ibehav,cn,ct,smax,w,b)

       di =  (w/(9.d0 - 4.d0*log(4.d0)) * (1.d0/cn + 1.d0/ct))**0.5
       beta = (3.d0*di - nd) / (di + nd)

       if(beta < 0.d0 .or. nd > 3.d0*di) then

          taz(1) = 0.d0

       else if (beta <= internal(5) .and. nd >= di) then

          taz(1) = beta

       end if

    case (i_MP3_CZM,i_MP3_CZM_THER)

       ucut  = internal(2) + dut
       ucun  = internal(3) + dun
       ucus  = internal(4) + dus

       nucut = (ucut**2) + (ucus**2)
       nucun = ((dabs(ucun)+ucun)*0.5)**2
       nd = dsqrt(nucut+nucun)

       call get_czm(ibehav,cn,ct,smax,w,b)

       di = ( smax*0.5 ) * ( 1.d0/cn + 1.d0/ct )
       dk = 1.5 * ( w/smax - di*0.5 )
       beta = (di / nd) * (1.d0 - ((nd - di)/dk)**2)

       if (dk == 0.d0) then

         di = 0.d0
         beta = 0.d0

       else

         beta = (di / nd) * (1.d0 - ((nd - di)/dk)**2)

       endif

       if (nd > ( di + dk )) then

          taz(1) = 0.d0

       else if (beta <= internal(5) .and. nd >= di ) then

          taz(1) = beta

       end if

    case(i_TH_CZM,i_IQS_TH_CZM)

      !   ___
      ! /     \
      !  di dp du


     ucut  = internal(2) + dut
     ucun  = internal(3) + dun
     ucus  = internal(4) + dus

     nucut = (ucut**2)+(ucus**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     call get_czm_th(ibehav,cn,ct,s1,s2,G1,G2,dp1,dp2)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     di1 = s1/cn
     di2 = s2/ct

     !print*,'------------------'
     !print*,dir,s1,s2,g1,g2,di1,di2,dp1,dp2


     di = di1*di2*sqrt((1.d0+(dir2))/ ((di2**2) + (dir2)*(di1**2)))

     !di = ( smax * 0.5 ) * ( 1.d0/cn + 1.d0/ct )

     dp = dp1*dp2*sqrt((1.d0+(dir2))/ ((dp2**2) + (dir2)*(dp1**2)))

     tmp =  (cn*G2) + (dir2*ct*G1)

     du = ( (2.d0*G1*G2*(1.d0+dir2)) - ( di*(dp-di)*tmp))/(di*tmp)

     !dk = ( 2.d0 * w / smax ) + ( di - d )

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > dp .and. nd <= du ) then

        beta = ( di / nd ) * ( ( du - nd ) / ( du - dp ) )

     else if ( nd > di .and. nd <= dp) then

        beta = di / nd

     end if


     !print*,'-----'
     !print*,nd,di,dp,du,beta


     if( (beta <= internal(5) ) ) then

        taz(1) = beta

     end if


    case(i_ABP_CZM, i_IQS_ABP_CZM)

     !  /  \
     ! /    ----
     ! 0 di dp du


     ! indices e=i, p=c, u=u

     ucut  = internal(2) + dut
     ucun  = internal(3) + dun
     ucus  = internal(4) + dus

     nucut = (ucut**2)+(ucus**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     call get_czm_abp(ibehav,cn,ct,s1,s2,G1,G2,du1,du2,phi)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! 2*b/du ; b=w*(1-phi)
     sp1 = 2.d0*G1*(1.d0-phi)/du1
     sp2 = 2.d0*G2*(1.d0-phi)/du2

     ! (2*mu + sp*di)/smax ; mu = w*phi
     dp1 = ((2.d0*G1*phi) + (sp1*di1))/s1
     dp2 = ((2.d0*G2*phi) + (sp2*di2))/s2

     ! mixing
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     dp = dp1*dp2*sqrt(1.d0+(dir2))*sqrt((di2**2) + (dir2)*(di1**2))/(di2*dp2 + (dir2*di1*dp1))

     du = ((1.d0+dir2)/dp)*(dp1*du1*dp2*du2)/(dp2*du2+dir2*dp1*du1)

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0+ dir2))

     si = k0 * di

     sp = si * dp / ( di + (phi*du/(1.d0-phi)))


     !print*,'------------------'
     !print*,cn,ct,s1,G1,du1,phi
     !print*,s1,s2,g1,g2,di1,di2,dp1,dp2
     !print*,dir,k0,di,dp,du,si,sp

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > dp .and. nd <= du ) then

        beta = (sp + ((sp * (nd-dp) ) / (dp-du))  )/(nd*k0)

     else if ( nd > di .and. nd <= dp) then

        beta = (si + ( ((si - sp)* (nd-di))/(di-dp)) )/(nd*k0)

     end if


     !print*,'-----'
     !print*,nd,di,dp,du,beta


     if( (beta <= internal(5) ) ) then

        taz(1) = beta

     end if

    case(i_EXPO_CZM, i_IQS_EXPO_CZM)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     ucut  = internal(2) + dut
     ucun  = internal(3) + dun
     ucus  = internal(4) + dus

     nucut = (ucut**2)+(ucus**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2
     nd = dsqrt(nucut+nucun)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     call get_czm_expo(ibehav,cn,ct,s1,s2,G1,G2,eta)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir2 * di1**2 * G2) ) / ((di2**2) + (dir2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0+ dir2))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     !print*,'------------------'
     !print*,s1,s2,g1,g2,di1,di2,dp1,dp2
     !print*,dir,k0,di,dp,du,si,sp

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/(k0*nd)) * exp(phi*(di-nd))

     end if

     !print*,'-----'
     !print*,di,si,k0
     !print*,dp,sp,du
     !print*,nd,beta

     if ( beta <= internal(5) ) then

        taz(1) = beta

     else

         taz(1) = internal(5)

     end if

    case(i_TOSI_CZM)

       call tosi_czm(dut,dun,dus,ibehav,internal,taz,xx1,xx2,xx3,xx4,xx5,xx6,store)
       return

    case(i_TOSI_CZM_INCRE)

       call tosi_czm_incre(vt,vn,vs,ibehav,internal,taz,H,xx1,xx2,xx3,xx4,xx5,xx6,store)
       return

    case(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     !! saut de deplacement total
     ucut  = internal(2) + dut
     ucun  = internal(3) + dun
     ucus  = internal(4) + dus

     !! saut de deplacement endommageable
     ucut_d = ucut - internal(11)
     ucun_d = ucun - internal(13)
     ucus_d = ucus - internal(12)

     if (abs(ucut_d) > 1d-14) then
       dirts = abs(ucus_d) / abs(ucut_d)
     else
       dirts = 1d+20
     endif

     nucut = (ucut_d**2)+(ucus_d**2)
     nucun = ((dabs(ucun_d)+ucun_d)*0.5)**2
     nd = dsqrt(nucut+nucun)

     if (nucun > 1d-14) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     call get_czm_expo_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta)

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir2 * di1**2 * G2) ) / ((di2**2) + (dir2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0+ dir2))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/(k0*nd)) * exp(phi*(di-nd))

     end if


     if ( beta <= internal(5) ) then

        taz(1) = beta

        if (beta < 1.d0) then
            !!! calcul saut de deplacement plastique normal
            ! G_f
            G_f=gi/mu_g
            ! phi_f : parametre expo
            phi_f = (k0 * di)/(G_f - (0.5d0 * k0 * di * di))

            if (taz(1)/=0.d0) then
                ! contrainte normale
                scoh=taz(1)*k0*nd
                nd_p=(di-(log(scoh/si)/phi_f) - nd)
                ! deplacement plastique
                ucun_p=nd_p*cos(atan(dir2))

                ! deplacement plastique tangentielle
                ucuts_p=nd_p*sin(atan(dir2))
                ! projection du deplacement plastique tangentielle sur les axes t et s
                ucut_p=ucuts_p*cos(atan(dirts))
                ucus_p=ucuts_p*sin(atan(dirts))
            else
                ucun_p=internal(9)
                ucut_p=internal(8)
                ucus_p=internal(10)
            endif

            ! correction du d?placement plastique tangentielle
            ucun_p1=internal(13)+sign(ucut_p-internal(9),ucun_d)
            ucut_p1=internal(11)+sign(ucut_p-internal(8),ucut_d)
            ucus_p1=internal(12)+sign(ucus_p-internal(10),ucus_d)

            if ((internal(5)<1) .and. (0<internal(5)) .and. (internal(8)==0) .and. (internal(9)==0) .and. (internal(10)==0)) then
                scoh=internal(5)*k0*nd
                nd_p=(di-(log(scoh/si)/phi_f) - nd)
                !!
                ucun_p2  = nd_p*cos(atan(dir2))
                ucuts_p2 = nd_p*sin(atan(dir2))
                ucut_p2  = ucuts_p*cos(atan(dirts))
                ucus_p2  = ucuts_p*sin(atan(dirts))
                !!
                ucut_p1 = ucut_p1-sign(abs(ucut_p2),ucut_d)
                ucus_p1 = ucus_p1-sign(abs(ucus_p2),ucus_d)
                ucun_p1 = ucun_p1-sign(abs(ucun_p2),ucun_d)

            end if

        else

            ! deplacement plastique
            ucun_p=internal(9)
            ucut_p=internal(8)
            ucus_p=internal(10)
            ucut_p1=internal(11)
            ucus_p1=internal(12)
            ucun_p1=internal(13)

        end if

     else

        taz(1) = internal(5)
        ! deplacement plastique
        ucun_p=internal(9)
        ucut_p=internal(8)
        ucus_p=internal(10)
        ucut_p1=internal(11)
        ucus_p1=internal(12)
        ucun_p1=internal(13)

     end if

    case default

       call faterr('tact_behav::updt_CZM_3D','incompatible law '//trim(tact_behav(ibehav)%lawty))

    end select

    if (store) then

       internal(5)=taz(1)
       internal(2)=ucut

       !fd bizarre
       ! !internal(3)=max(0.d0,ucun)
       ! internal(3)=max(0.d0,(ucun - get_dilatancy_height(ibehav,internal)))
       internal(3)=ucun

       !fd xg0 -- debile car saut-de-un a deja ete corrige dans prep
       ! if (tact_behav(ibehav)%ilaw == i_EXPO_CZM) internal(3)=internal(3) - internal(7)

       internal(4)  = ucus
       internal(8)  = ucut_p
       internal(10) = ucus_p
       internal(9)  = ucun_p
       internal(11) = ucut_p1
       internal(12) = ucus_p1
       internal(13) = ucun_p1

       if (internal(6)==-99.d0 .and. taz(1) < 1.d0) internal(6)=TPS

       !fd  pas necessaire
       ! if(taz(1) == 0.d0) then

       !    internal(2)=0.d0
       !    internal(3)=0.d0
       !    internal(4)=0.d0

       ! end if
    end if

  end subroutine updt_CZM_3D
!!!------------------------------------------------------------------------
  subroutine updt_CZM_spring_3D(ibehav,store,internal,taz,dut,dun,dus,rlt,rln,rls)

    implicit none
    integer     :: ibehav
    real(kind=8):: dut,dun,dus,rlt,rln,rls

    real(kind=8):: cn,ct,ucut,ucun,ucus,nucut,nucun,beta,nd,di

    ! pour TH
    ! mode mixte
    real(kind=8) :: dp,du,tmp,dir2
    ! modes pures
    real(kind=8) :: s1,s2,G1,G2,di1,di2,dp1,dp2,du1,du2,C2,GG, &
                    k0,sp1,sp2,si,sp,phi,eta,gi,k1,k2

    logical,intent(in) :: store

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz

                             !123456789012345678901234567
    character(len=27) :: IAM='tact_behav::updt_CZM_spring'

    !fd calcul de beta stocke dans taz(1) et mise a jour de internal(4)

    select case(tact_behav(ibehav)%ilaw)
    case(i_EXPO_CZM_SPRING, i_IQS_EXPO_CZM_SPRING)

     call get_czm_expo_spring(ibehav,cn,ct,s1,s2,G1,G2,eta,k1,k2)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     ! on reconstruit le deplacement de la partie endommageable
     ! rl est une impulsion dont rl/H est une force et k une raideur
     ! rt > 0 [u]<0 compression rl < 0 [u] > 0 traction (c est pourquoi on prend -rl)
     !

     ! calcul de l'allongement endo tangent [ut]-[uet]
     ucut  = internal(2) + internal(7) + dut - (-rlt/(H*internal(1)*(k2*internal(11))))
     ucus  = internal(4) + internal(8) + dus - (-rls/(H*internal(1)*(k2*internal(11))))     

     ! print*,'[ut] =', internal(2) + internal(6) + dut
     ! print*,'[uet]=',(-rlt/(H*k2))
     ! print*,'[udt]=',ucut

     ! calcul de l'allongement endo normal
     ucun  = dun - (-rln/(H*internal(1)*(k1*internal(11))))

     ! ! print*,'[un] =',dun
     ! print*,'[uen]=',(-rln/(H*k1))
     ! print*,'[udn]=',ucun

     ! <un>_+
     nucut = (ucut**2 + ucus**2)
     nucun = ((dabs(ucun)+ucun)*0.5)**2

     nd = dsqrt(nucut+nucun)

     if (nucun > tiny(nucun)) then
       dir2 = nucut / nucun
     else
       dir2 = 1d+20
     endif

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir2)) / ((di2**2) + (dir2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir2 * di1**2 * G2) ) / ((di2**2) + (dir2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir2)/(1.d0 + dir2))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     ! print*,'nd= ',nd,' di= ',di,' du= ',du

     ! </fd
     ! print*,'</-----------'
     ! print*,cn,ct,s1,s2,g1,g2,eta
     ! print*,di1,di2
     ! print*,dir,k0,si,di,gi,du,phi
     ! print*,internal(3),internal(2)
     ! print*,dun,dut
     ! print*,(-rln/(H*H*k1)),(-rlt/(H*H*k2))
     ! print*,ucun,ucut,nd
     ! fd/>

     !stop

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then
        if (internal(10) > 0.) then 
          beta = (si/((k0*internal(10))*nd)) * exp(phi*(di-nd))
        else
          beta = internal(5)
        end if

     end if

     !</fd
     ! print*,'beta =',beta
     ! print*,'-----------/>'
     ! fd/>

     if ( beta <= internal(5) ) then

        taz(1) = beta

      else

        taz(1) = internal(5)

     end if

    case default
       call faterr('tact_behav::updt_CZM_spring','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

    if (store) then

      ! allongement endo tangent
      internal(2)= ucut
      ! allongement endo normal
      internal(3)= ucun
      ! allongement endo tangent
      internal(4)= ucus
      ! beta
      internal(5)= taz(1)
      ! declenchement de l'endommagement
      if (internal(6)==-99.d0 .and. taz(1) < 1.d0) internal(6)=TPS
      ! allongement elastique tangent
      internal(7)=-rlt/(H*internal(1)*internal(11)*k2)
      internal(8)=-rls/(H*internal(1)*internal(11)*k2)      

    end if


  end subroutine updt_CZM_spring_3D
  !!!------------------------------------------------------------------------
  subroutine updt_CZM_spring_p_3D(ibehav,store,internal,taz,dut,dun,dus,rlt,rln,rls)

    implicit none
    integer     :: ibehav
    real(kind=8):: dut,dun,dus,rlt,rln,rls

    real(kind=8):: cn,ct,ucut,ucun,ucus,nucut,nucun,beta,nd,di,ucut_d,ucun_d,ucus_d,ucun_p,ucun_p1,ucun_p2,ucut_p,ucut_p1,ucut_p2,ucus_p,ucus_p1,ucus_p2,ucuts_p,ucuts_p2,nd_p

    ! pour TH
    ! mode mixte
    real(kind=8) :: dp,du,tmp,dir,dirts
    ! modes pures
    real(kind=8) :: s1,s2,G1,G2,di1,di2,dp1,dp2,du1,du2,C2,GG, &
                    k0,sp1,sp2,si,sp,phi,mu_g,eta,gi,k1,k2
    ! plasticit?
    real(kind=8) :: scoh,G_f,phi_f

    logical,intent(in) :: store

    real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz

                             !123456789012345678901234567
    character(len=27) :: IAM='tact_behav::updt_CZM_spring'

    !fd calcul de beta stocke dans taz(1) et mise a jour de internal(4)

    select case(tact_behav(ibehav)%ilaw)
    case(i_EXPO_CZM_SPRING_P, i_IQS_EXPO_CZM_SPRING_P)

     call get_czm_expo_spring_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta,k1,k2)

     !  /  \
     ! /    ----
     ! 0 di     du


     ! indices e=i, p=c, u=u

     ! on reconstruit le deplacement de la partie endommageable
     ! rl est une impulsion dont rl/H est une force et k une raideur
     ! rt > 0 [u]<0 compression rl < 0 [u] > 0 traction (c est pourquoi on prend -rl)
     !

     ! calcul de l'allongement endo tangent [ut]-[uet]
     ucut  = internal(2) + internal(7) + dut - (-rlt/(H*internal(1)*k2))
     ucus  = internal(4) + internal(8) + dus - (-rls/(H*internal(1)*k2))

     ! print*,'[ut] =', internal(2) + internal(6) + dut
     ! print*,'[uet]=',(-rlt/(H*k2))
     ! print*,'[udt]=',ucut

     ! calcul de l'allongement endo normal
     ucun  = dun - (-rln/(H*internal(1)*k1))

     !! saut de deplacement endommageable - saut de deplacement plastiques
     ucut_d = ucut - internal(13)
     ucun_d = ucun - internal(15)
     ucus_d = ucus - internal(14)

     if (abs(ucut_d) > 1d-14) then
       dirts = abs(ucus_d) / abs(ucut_d)
     else
       dirts = 1d+20
     endif

     ! ! print*,'[un] =',dun
     ! print*,'[uen]=',(-rln/(H*k1))
     ! print*,'[udn]=',ucun

     ! <un>_+
     nucut = (ucut_d**2 + ucus_d**2)
     nucun = ((dabs(ucun_d)+ucun_d)*0.5)**2

     nd = dsqrt(nucut+nucun)

     if (nucun > 1d-14) then
       dir = nucut / nucun
     else
       dir = 1d+20
     endif

     ! smax/c
     di1 = s1/cn
     di2 = s2/ct

     ! mixing

     ! di : dep elastique
     di = di1 * di2 * sqrt((1.d0+(dir**2)) / ((di2**2) + (dir**2)*(di1**2)))

     ! gi : energie totale
     gi = ( (di2**2 * G1) + (dir**2 * di1**2 * G2) ) / ((di2**2) + (dir**2)*(di1**2))

     k0 = sqrt((cn*cn + ct*ct*dir*dir)/(1.d0 + dir*dir))

     ! phi : parametre expo
     phi = (k0 * di)/(gi - (0.5d0 * k0 * di * di))

     ! du
     du = di - (log(eta)/phi)

     !
     si = k0 * di

     beta = 1.d0

     if ( nd > du ) then

        beta = 0.d0

     else if ( nd > di .and. nd <= du) then

        beta = (si/(k0*nd)) * exp(phi*(di-nd))

     end if

     !</fd
     ! print*,'beta =',beta
     ! print*,'-----------/>'
     ! fd/>
     if ( beta <= internal(5) ) then

        taz(1) = beta

        if (beta < 1.d0) then
            !!! calcul saut de deplacement plastique normal
            ! G_f
            G_f=gi/mu_g
            ! phi_f : parametre expo
            phi_f = (k0 * di)/(G_f - (0.5d0 * k0 * di * di))

            if (taz(1)/=0.d0) then
                scoh=taz(1)*k0*nd
                nd_p=(di-(log(scoh/si)/phi_f) - nd)

                ! deplacement plastique
                ucun_p=nd_p*cos(atan(dir))
                ! deplacement plastique tangentielle
                ucuts_p=nd_p*sin(atan(dir))
                ! projection du deplacement plastique tangentielle sur les axes t et s
                ucut_p=ucuts_p*cos(atan(dirts))
                ucus_p=ucuts_p*sin(atan(dirts))

            else
                ! deplacement plastique
                ucun_p=internal(12)
                ! projection du deplacement plastique tangentielle sur les axes t et s
                ucut_p=internal(10)
                ucus_p=internal(11)
            endif


            ! correction du d?placement plastique tangentielle
            ucut_p1=internal(13)+sign(ucut_p-internal(10),ucut_d)
            ucus_p1=internal(14)+sign(ucus_p-internal(11),ucus_d)
            ucun_p1=internal(15)+sign(ucun_p-internal(12),ucun_d)

            !! ab :: bloc pour prendre en compte l'effet de l'endo inital sur les d?placements residuels
            if ((internal(5)<1) .and. (0<internal(5)) .and. (internal(10)==0) .and. (internal(11)==0) .and. (internal(12)==0)) then
                scoh=internal(5)*k0*nd
                nd_p=(di-(log(scoh/si)/phi_f) - nd)
                !!
                ucun_p2  = nd_p*cos(atan(dir))
                ucuts_p2 = nd_p*sin(atan(dir))
                ucut_p2  = ucuts_p*cos(atan(dirts))
                ucus_p2  = ucuts_p*sin(atan(dirts))
                !!
                ucut_p1 = ucut_p1-sign(abs(ucut_p2),ucut_d)
                ucus_p1 = ucus_p1-sign(abs(ucus_p2),ucus_d)
                ucun_p1 = ucun_p1-sign(abs(ucun_p2),ucun_d)
            end if

        else


            ! deplacement plastique
            ucun_p  = internal(12)
            ucut_p  = internal(10)
            ucus_p  = internal(11)
            ucut_p1 = internal(13)
            ucus_p1 = internal(14)
            ucun_p1 = internal(15)

        end if

     else

        taz(1) = internal(5)

        ! deplacement plastique
        ucun_p  = internal(12)
        ucut_p  = internal(10)
        ucus_p  = internal(11)
        ucut_p1 = internal(13)
        ucus_p1 = internal(14)
        ucun_p1 = internal(15)

     end if

    case default
       call faterr('tact_behav::updt_CZM_spring_p_3D','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

    if (store) then

      ! allongement endo tangent
      internal(2)= ucut
      ! allongement endo normal
      internal(3)= ucun
      ! allongement endo tangent
      internal(4)= ucus
      ! beta
      internal(5)= taz(1)
      ! declenchement de l'endommagement
      if (internal(6)==-99.d0 .and. taz(1) < 1.d0) internal(6)=TPS
      ! allongement elastique tangent
      internal(7)  = -rlt/(H*internal(1)*k2)
      internal(8)  = -rls/(H*internal(1)*k2)
      internal(10) = ucut_p
      internal(11) = ucus_p
      internal(12) = ucun_p
      internal(13) = ucut_p1
      internal(14) = ucus_p1
      internal(15) = ucun_p1

    end if


  end subroutine updt_CZM_spring_p_3D
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  subroutine get_fric_CZM(ibehav,taz,fric)

    implicit none

    integer       ::  ibehav
    real(kind=8)  ::  fric,beta
    real(kind=8),dimension(max_taz) :: taz
    real(kind=8)  ::  mu_s,mu_d

    real(kind=8) :: d_fric


                             !123456789012345678901234
    character(len=24) :: IAM='tact_behav::get_fric_czm'
    character(len=80) :: cout


    !
    beta = taz(1)

    !fd & va
    !mu_d=tact_behav(ibehav)%param(1)
    !mu_s=tact_behav(ibehav)%param(2)
    !fric=mu_d + beta * (mu_s - mu_d)

    !fd     fric=fric*(1.d0 - beta)

    d_fric = get_dilatancy_fric(ibehav)

    if (d_fric /= 0.d0) then
      fric = beta*d_fric + (1.d0 - beta)*fric
    else
       if (czm_initial_friction_power > 0.d0) then
          fric=fric*((1.d0 - beta)**czm_initial_friction_power)
       else
          ! (1-beta) mu_d + beta mu_s
          fric=(1.d0-beta)*tact_behav(ibehav)%param(1) + beta*tact_behav(ibehav)%param(2)
       endif
    endif

  end subroutine get_fric_CZM

  !!!------------------------------------------------------------------------
  subroutine get_fric_CZM_spring(i_beta_coh_th,ibehav,taz,internal,fric)

    implicit none
    
    integer       ::  ibehav,i_beta_coh_th !!i_beta_coh_th est l'indice du beta_coh_th dans la table des variables internes : pour differencier 2D et 3D
    real(kind=8)  ::  fric,beta
	real(kind=8),dimension(max_internal_tact) :: internal
    real(kind=8),dimension(max_taz) :: taz
    real(kind=8)  ::  mu_s,mu_d

    real(kind=8) :: d_fric


                             !123456789012345678901234
    character(len=24) :: IAM='tact_behav::get_fric_czm_spring'
    character(len=80) :: cout
		
    !
	! beta = taz(1)*internal(i_beta_coh_th8)
	beta = taz(1)*internal(i_beta_coh_th)


    !fd & va
    !mu_d=tact_behav(ibehav)%param(1)
    !mu_s=tact_behav(ibehav)%param(2)
    !fric=mu_d + beta * (mu_s - mu_d)

    !fd     fric=fric*(1.d0 - beta)

    d_fric = get_dilatancy_fric(ibehav)

    if (d_fric /= 0.d0) then
      fric = beta*d_fric + (1.d0 - beta)*fric
    else
       if (czm_initial_friction_power > 0.d0) then
          fric=fric*((1.d0 - beta)**czm_initial_friction_power)
       else
          ! (1-beta) mu_d + beta mu_s
          fric=(1.d0-beta)*tact_behav(ibehav)%param(1) + beta*tact_behav(ibehav)%param(2)
       endif   
    endif

  end subroutine get_fric_CZM_spring

  !!!------------------------------------------------------------------------
  subroutine set_parameters_pressure(ibehav,flag,param)
   implicit none

   integer      :: ibehav,flag
   real(kind=8) :: param(10)

                            !12345678901234567890123456789012345
   character(len=35) :: IAM='tact_behav::set_parameters_pressure'

   integer:: i

   tact_pressure(ibehav)%flag=flag
   tact_pressure(ibehav)%param=param

   if (tact_pressure(ibehav)%flag==2 .or. tact_pressure(ibehav)%flag==3) then
     if (tact_pressure(ibehav)%param(3) < 0.d0) call faterr(IAM,'with saturated laws tau must be positive')
   endif

   ! print*,'set pressure'
   ! do i=1,size(tact_pressure)
   !   print*,tact_pressure(i)%flag
   ! enddo
   ! print*,'--'
 end subroutine set_parameters_pressure

  !!!------------------------------------------------------------------------
  real(kind=8)function get_pressure(ibehav,beta,TPSini,pext)
   implicit none
   integer               :: ibehav
   real(kind=8)          :: beta,TPSini,pext

                            !123456789012345678901234
   character(len=24) :: IAM='tact_behav::get_pressure'

   get_pressure = 0.d0

   if (TPSini >= 0.d0) then

     if (tact_pressure(ibehav)%flag == 0) then
       ! no pressure
     elseif (tact_pressure(ibehav)%flag == 1) then
       ! time evolution: p = p0 + dpdt * (TPS - TPSini)
       if (beta == 0.d0) get_pressure = tact_pressure(ibehav)%param(1) + tact_pressure(ibehav)%param(2) * (TPS-TPSini)
     elseif (tact_pressure(ibehav)%flag == 2) then
       ! saturated linear time evolution p = p0 + dp * min(1,TPS-TPSini/tau) (pression varie entre p0 et p0+dp)
       ! pressure evolution in terms of beta p*(1-beta)**alpha
       get_pressure = tact_pressure(ibehav)%param(1) &
                    + tact_pressure(ibehav)%param(2)*(min(1.d0,((TPS-TPSini)/tact_pressure(ibehav)%param(3))))
       get_pressure = get_pressure*((1.d0-beta)**tact_pressure(ibehav)%param(4))

    elseif (tact_pressure(ibehav)%flag == 3) then
       ! saturated exponential time evolution p = p0 + dpdt * (1 - exp(-(TPS-TPSini)/tau))
       ! pressure evolution in term of beta p*(1-beta)**alpha
       get_pressure = tact_pressure(ibehav)%param(1) &
                    + tact_pressure(ibehav)%param(2)*(1.d0-exp(-(TPS-TPSini)/tact_pressure(ibehav)%param(3)))
       get_pressure = get_pressure*((1.d0-beta)**tact_pressure(ibehav)%param(4))
     elseif(tact_pressure(ibehav)%flag == 4) then
       get_pressure = pext*((1.d0-beta)**tact_pressure(ibehav)%param(4))
     endif
   endif

  end function get_pressure
  !!!------------------------------------------------------------------------
  integer function get_pressure_flag(ibehav)
    implicit none
    integer :: ibehav

    get_pressure_flag =  tact_pressure(ibehav)%flag

  end function get_pressure_flag

  !!!------------------------------------------------------------------------
  subroutine init_IQS_PLAS(ibehav,internal)
    implicit none
    integer(kind=4) :: ibehav
    real(kind=8),dimension(max_internal_tact) :: internal

    select case(tact_behav(ibehav)%ilaw)
    case(i_IQS_PLAS_CLB)
       internal = 0.d0
       internal(2) = tact_behav(ibehav)%param(3)
    case default
       call faterr('tact_behav::init_IQS_PLAS','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

  end subroutine init_IQS_PLAS

  !!!------------------------------------------------------------------------
  subroutine updt_IQS_PLAS(ibehav,internal,rn)
    implicit none
    integer(kind=4) :: ibehav
    real(kind=8)    :: rn,rnb,rnc,roverg

    real(kind=8),dimension(max_internal_tact) :: internal

    select case(tact_behav(ibehav)%ilaw)
    case(i_IQS_PLAS_CLB)
       !!pta 2023/04/03 -> on courcircuite totalement l'update pour etre sur que ca ne foute pas la merde :
       !internal(2) = tact_behav(ibehav)%param(3) !pta 2023/04/03 - on force mu = mu_s
       !internal(3) = 1.0                         !pta 2023/04/03 - on prend glim = 1 m !

       !if ( internal(1).eq.0.d0 ) then
       !   internal(2) = tact_behav(ibehav)%param(3)
       !end if

       !rnc    = tact_behav(ibehav)%param(1)
       !roverg = tact_behav(ibehav)%param(2)

       !!rnb = rnc + roverg*internal(1)
       !rnb = rnc - roverg*internal(1) !pta avec dgb=internal(1) < 0

       !if (rn.gt.rnb) then
       !   !internal(1) = internal(1) + tact_behav(ibehav)%param(5)
       !   if (internal(1) > -internal(3)) then !avec gb <= gbmax=internal(3) = 0.2 sphere encombrement POLYR
       !      internal(1) = internal(1) + tact_behav(ibehav)%param(5)
       !   end if
       !   !                 pfric                       fric + delta_fric
       !   internal(2) = min(tact_behav(ibehav)%param(4),internal(2)+tact_behav(ibehav)%param(6))
       !end if

    case default
       call faterr('tact_behav::updt_IQS_PLAS','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select


  end subroutine updt_IQS_PLAS

  !!!------------------------------------------------------------------------
  !!!------------------------------------------------------------------------
  subroutine init_ELAS_REP_adapt(ibehav,internal)
    implicit none
    integer(kind=4) :: ibehav
    real(kind=8),dimension(max_internal_tact) :: internal

    select case(tact_behav(ibehav)%ilaw)
    case(i_ELASTIC_REPELL_CLB_adapt)
       internal = 0.d0
       internal(1) = tact_behav(ibehav)%param(1)
    case default
       call faterr('tact_behav::init_ELASTIC_REPELL_adapt','incompatible law '//trim(tact_behav(ibehav)%lawty))
    end select

  end subroutine init_ELAS_REP_adapt


  !!!------------------------------------------------------------------------
  !!!
  !!! PUBLIC PART OF THE MODULE
  !!!
  !!!------------------------------------------------------------------------
  function get_isee(cdan, bdycd, colcd, bdyan, colan)
    implicit none
    !> interaction type id
    integer, intent(in) :: cdan
    !> candidate model id
    integer, intent(in) :: bdycd
    !> antaganist model id
    integer, intent(in) :: bdyan
    !> candidate color
    character(len=5) :: colcd
    !> antagonist color
    character(len=5) :: colan
    !> return see table id
    integer :: get_isee
    !
    integer :: isee

    get_isee = 0
    do isee = 1, size(see)

       if ( cdan /= see(isee)%cdan ) cycle
       !print*,'tact ok'
       !print*,'-------'
       !print*,isee,see(isee)%cdcol,see(isee)%ancol
       if ( ((bdycd /= see(isee)%id_cdbdy) .or. (bdyan /= see(isee)%id_anbdy)) .and. &
            ((bdycd /= see(isee)%id_anbdy) .or. (bdyan /= see(isee)%id_cdbdy)) ) cycle
       !print*,'body ok'
       !print*,'-------'
       !print*,isee,see(isee)%cdtac,see(isee)%antac

       if (((colcd /= see(isee)%cdcol) .or. (colan /= see(isee)%ancol)) .and. &
           ((colcd /= see(isee)%ancol) .or. (colan /= see(isee)%cdcol))) cycle

       !print*,'color ok'
       !print*,'-------'

       get_isee = isee

       exit
    end do

  end function get_isee

  !!!------------------------------------------------------------------------
  subroutine indent_isee(isee,ind_color_from,ind_color_to,ind_color_from_to)

    implicit none
    integer          :: isee
    character(len=3) :: ind_color_from,ind_color_to,ind_color_from_to

    see(isee)%cdcol = ind_color_from//see(isee)%cdcol(4:5)
    see(isee)%behav = ind_color_from_to//see(isee)%behav(4:5)
    see(isee)%ancol = ind_color_to//see(isee)%ancol(4:5)

  end subroutine indent_isee

  !!!------------------------------------------------------------------------
  real(kind=8) function get_fric(ibehav,statusBEGIN)

    implicit none
    !                           12345678901234567890
    character(len=20) :: IAM = 'tact_behav::get_fric'
    character(len=80) :: cout
    real(kind=8)      :: x
    integer           :: ibehav
    integer(kind=4)   :: statusBEGIN

    get_fric = 0.D0

    select case(tact_behav(ibehav)%ilaw)

    !!!------------------------------------
    case(i_IQS_CLB,&
         i_IQS_CLB_g0, &
         i_IQS_CLB_nosldt,&
         i_IQS_CLB_noslds,&
         i_GAP_SGR_CLB_nosldt,&
         i_GAP_SGR_CLB_noslds,&
         i_PLASTIC_COUPLED_DOF, &
         i_TEX_SOL, &
         i_GAP_SGR_CLB, &
         i_GAP_SGR_CLB_g0, &
         i_VEL_SGR_CLB, &
         i_BRITTLE_COATING_CLB)
       get_fric = tact_behav(ibehav)%param(1)

    !!!------------------------------------
    case(i_ELASTIC_REPELL_CLB, &
         i_ELASTIC_REPELL_CLB_g0, &
         i_ELASTIC_REPELL_CLB_adapt, & ! PTA sncf 2023
         i_preGAP_SGR_CLB) !pta 20/05/2015
       get_fric = tact_behav(ibehav)%param(2)

    !!!------------------------------------
    case(i_VISCO_ELASTIC_REPELL_CLB)
       get_fric = tact_behav(ibehav)%param(3)

    !!!------------------------------------
    case(i_IQS_CLB_RGR, &
         i_RST_CLB, &
         i_CRITICAL_VOIGT_CLB, &
         i_GAP_SGR_CLB_WEAR, &
         i_IQS_SGR_CLB_WEAR)
       get_fric = tact_behav(ibehav)%param(3)

    !!!------------------------------------
    case(i_ELASTIC_REPELL_WET_CLB, &
         i_IQS_PLAS_CLB)
       get_fric = tact_behav(ibehav)%param(4)

    !!!------------------------------------
    case(i_MD_JKRs)
       get_fric = tact_behav(ibehav)%param(5)

    !!!------------------------------------
    case(i_IQS_DS_CLB)
       get_fric = tact_behav(ibehav)%param(2)
       !if (statusBEGIN.eq.'slifw' .or. statusBEGIN.eq.'slibw') then
       if (statusBEGIN.eq.i_slifw .or. statusBEGIN.eq.i_slibw) then
          get_fric = tact_behav(ibehav)%param(1)
       end if

    !!!------------------------------------
    case(i_IQS_WET_DS_CLB, &
         i_xQS_WET_DS_CLB, &
         i_GAP_WET_DS_CLB)
       get_fric = tact_behav(ibehav)%param(5)
       if (statusBEGIN.eq.i_slifw .or. statusBEGIN.eq.i_slibw .or. &
           statusBEGIN.eq.i_Wslfw .or. statusBEGIN.eq.i_Wslbw) then
          get_fric = tact_behav(ibehav)%param(4)
       end if

    !!!------------------------------------
    case(i_GAP_SGR_DS_CLB, &
         i_VEL_SGR_DS_CLB)
       get_fric = tact_behav(ibehav)%param(2)
       if (statusBEGIN.eq.i_slifw .or. statusBEGIN.eq.i_slibw) then
          get_fric = tact_behav(ibehav)%param(1)
       end if

    !!!------------------------------------
    case(i_RST_DS_CLB)
       get_fric = tact_behav(ibehav)%param(4)
       if (statusBEGIN.eq.i_slifw .or. statusBEGIN.eq.i_slibw) then
          get_fric = tact_behav(ibehav)%param(3)
       end if

    !!!------------------------------------
    case(i_RST_WET_CLB)
       get_fric = tact_behav(ibehav)%param(7)
       if (statusBEGIN.eq.i_slifw .or. statusBEGIN.eq.i_slibw) then
          get_fric = tact_behav(ibehav)%param(6)
       end if

    !!!------------------------------------
    case(i_IQS_MOHR_DS_CLB,i_IQS_CAP_MOHR_DS_CLB, &
         i_GAP_MOHR_DS_CLB,i_GAP_CAP_MOHR_DS_CLB)
       if (statusBEGIN == i_Mstck) then
         if (tact_behav(ibehav)%param(1) > 0.d0) then
           get_fric=tact_behav(ibehav)%param(2)/tact_behav(ibehav)%param(1)
         else
           get_fric=tact_behav(ibehav)%param(4)
         endif
       else if (statusBEGIN == i_stick) then
         get_fric=tact_behav(ibehav)%param(4)
       else if (statusBEGIN == i_slifw .or. statusBEGIN == i_slibw .or. statusBEGIN == i_slide) then
         get_fric=tact_behav(ibehav)%param(3)
       else if (statusBEGIN == i_noctc .or. statusBEGIN == i_nknow) then
         get_fric=tact_behav(ibehav)%param(4)
       end if

    !!!------------------------------------
    case(i_MAC_CZM              , &
         i_MAC_CZM_nosldt       , &
         i_MAC_CZM_noslds       , &
         i_postGAP_IQS_MAC_CZM  , &
         i_MSMP_CZM             , &
         i_MAL_CZM              , &
         i_IQS_MAL_CZM          , &
         i_IQS_MAC_CZM          , &
         i_ER_MAC_CZM           , &
         i_IQS_MAC_CZM_nosldt   , &
         i_IQS_MAC_CZM_noslds   , &
         i_MP_CZM               , &
         i_MP3_CZM              , &
         i_MP3_CZM_THER         , &
         i_TH_CZM               , &
         i_IQS_TH_CZM           , &
         i_ABP_CZM              , &
         i_IQS_ABP_CZM          , &
         i_EXPO_CZM             , &
         i_IQS_EXPO_CZM         , &
         i_EXPO_CZM_P           , &
         i_IQS_EXPO_CZM_P       , &
         i_EXPO_CZM_SPRING      , &
         i_IQS_EXPO_CZM_SPRING  , &
         i_EXPO_CZM_SPRING_P    , &
         i_IQS_EXPO_CZM_SPRING_P, &
         i_TOSI_CZM             , &
         i_TOSI_CZM_INCRE         &
        )

       get_fric = tact_behav(ibehav)%param(2)

       if ( statusBEGIN.eq.i_slifw .or. statusBEGIN.eq.i_slibw .or. &
!            statusBEGIN.eq.'Cslfw' .or. statusBEGIN.eq.'Cslbw' .or. &
            statusBEGIN.eq.i_slide .or. statusBEGIN.eq.i_Cslid ) then
          get_fric = tact_behav(ibehav)%param(1)
       end if

    !!!------------------------------------
    case(i_VISCO_ELASTIC_REPELL_WET)
       get_fric = tact_behav(ibehav)%param(5)

    !!!------------------------------------
    case(i_IQS_WET_CZM, &
         i_ELASTIC_WIRE, &
         i_BRITTLE_ELASTIC_WIRE, &
         i_ELASTIC_ROD, &
         i_VOIGT_WIRE, &
         i_VOIGT_ROD, &
         i_KV_WET, &
         i_DEM_FIBs, &
         i_RIGID_WIRE,&
         i_COUPLED_DOF, &
         i_BROKEN_DOF, &
         i_NORMAL_COUPLED_DOF, &
         i_IQS_BW_CLB, &
         i_ELASTIC_REPELL_MAC_CZM, &
         i_TANGENTIAL_COUPLED_DOF, &
         i_IQS_STICK, i_GAP_SGR_STICK, &
         i_NARD_ROD)

       get_fric = 0.D0

    !!!------------------------------------
    case default
       write(cout,'(A7,A30,A16)') ' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

    if(FEVOLflag)then
       call update_friction_evolution(get_fric)
    end if

    if(RandomMuIsActive)then
       call random_number(x)
       get_fric= get_fric*(1+MuRatio*(2*x-1))
    end if

  end function get_fric

  !!!------------------------------------------------------------------------
  subroutine get_rst(ibehav,tangalrest,normalrest)

    implicit none
    !                         1234567890123456789
    character(len=19) :: IAM='tact_behav::get_rst'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: tangalrest,normalrest

    tangalrest = 0.D0
    normalrest = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_RST_CLB, &
         i_RST_DS_CLB)
       normalrest=tact_behav(ibehav)%param(1)
       tangalrest=tact_behav(ibehav)%param(2)
    case(i_RST_WET_CLB)
       normalrest=tact_behav(ibehav)%param(3)
       tangalrest=tact_behav(ibehav)%param(4)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_rst

  !!!------------------------------------------------------------------------
  subroutine get_coh(ibehav,normalcoh,tangalcoh,Wethk)

    implicit none
    !                         1234567890123456789
    character(len=19) :: IAM='behaviours::get_coh'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: normalcoh
    real(kind=8)      :: tangalcoh
    real(kind=8)      :: Wethk

    normalcoh = 0.D0
    tangalcoh = 0.D0
    Wethk     = 0.D0

    select case(tact_behav(ibehav)%ilaw)
!!!------------------------------------
    case(i_IQS_WET_DS_CLB, &
         i_xQS_WET_DS_CLB, &
         i_GAP_WET_DS_CLB)
       normalcoh = tact_behav(ibehav)%param(1)
       tangalcoh = tact_behav(ibehav)%param(2)
       Wethk     = tact_behav(ibehav)%param(3)
!!!------------------------------------
    case(i_IQS_MOHR_DS_CLB, &
         i_GAP_MOHR_DS_CLB, &
         i_IQS_SGR_CLB_WEAR)   !<---------- argll  fd!
       normalcoh = tact_behav(ibehav)%param(1)
       tangalcoh = tact_behav(ibehav)%param(2)
!!!------------------------------------
    case(i_RST_WET_CLB)
       normalcoh = tact_behav(ibehav)%param(1)
       tangalcoh = tact_behav(ibehav)%param(2)
       Wethk     = tact_behav(ibehav)%param(5)
!!!------------------------------------
    case(i_IQS_CAP_MOHR_DS_CLB, &
         i_GAP_CAP_MOHR_DS_CLB)
!fd on prend le cone tronque ...
       normalcoh = tact_behav(ibehav)%param(5)
       tangalcoh = tact_behav(ibehav)%param(2)
!!!------------------------------------
    case(i_ELASTIC_REPELL_WET_CLB)
       normalcoh = tact_behav(ibehav)%param(2)
       Wethk     = tact_behav(ibehav)%param(3)
!!!------------------------------------
    case(i_IQS_WET_CZM)
       normalcoh = tact_behav(ibehav)%param(1)
       Wethk     = tact_behav(ibehav)%param(2)
!!!------------------------------------
    case(i_KV_WET)
       normalcoh = tact_behav(ibehav)%param(3)
       Wethk     = tact_behav(ibehav)%param(4)
!!!------------------------------------
    case(i_VISCO_ELASTIC_REPELL_WET)
       normalcoh = tact_behav(ibehav)%param(3)
       Wethk     = tact_behav(ibehav)%param(4)
!!!------------------------------------
    case(i_ELASTIC_REPELL_MAC_CZM)  !vhnhu
       Wethk     = tact_behav(ibehav)%param(2) !gcmax
!!!------------------------------------
    case(i_NARD_ROD)
       normalcoh = tact_behav(ibehav)%param(3)
       tangalcoh = tact_behav(ibehav)%param(4)


    case default
!       WRITE(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
!       CALL FATERR(IAM,cout)
    end select

  end subroutine get_coh

!!!------------------------------------------------------------------------
  subroutine get_czm_mal(ibehav,cn,ct,s1,s2,g1,g2)

    implicit none
    !                         12345678901234567890123
    character(len=23) :: IAM='behaviours::get_czm_mal'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,g1,g2

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    g1    = tact_behav(ibehav)%param(7)
    g2    = tact_behav(ibehav)%param(8)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_th(ibehav,cn,ct,s1,s2,w1,w2,dp1,dp2)

    implicit none
    !                         1234567890123456789012
    character(len=22) :: IAM='behaviours::get_czm_th'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,w1,w2
    !> length
    real(kind=8)      :: dp1,dp2

    cn=0.d0;ct=0.d0
    s1=0.d0; s2=0.d0
    w1=0.d0; w2=0.d0
    dp1=0.d0; dp2=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    w1    = tact_behav(ibehav)%param(7)
    w2    = tact_behav(ibehav)%param(8)
    dp1   = tact_behav(ibehav)%param(9)
    dp2   = tact_behav(ibehav)%param(10)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_abp(ibehav,cn,ct,s1,s2,w1,w2,du1,du2,phi)

    implicit none
    !                         12345678901234567890123
    character(len=23) :: IAM='behaviours::get_czm_abp'
    character(len=80) :: cout
    integer           :: ibehav
    !> stiffness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,w1,w2
    !> ultimate length
    real(kind=8)      :: du1,du2
    !> ratio
    real(kind=8)      :: phi

    cn=0.d0;ct=0.d0
    s1=0.d0; s2=0.d0
    w1=0.d0; w2=0.d0
    du1=0.d0; du2=0.d0
    phi=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    w1    = tact_behav(ibehav)%param(7)
    w2    = tact_behav(ibehav)%param(8)
    du1   = tact_behav(ibehav)%param(9)
    du2   = tact_behav(ibehav)%param(10)
    phi   = tact_behav(ibehav)%param(11)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_expo(ibehav,cn,ct,s1,s2,w1,w2,eta)

    implicit none
    !                         123456789012345678901234
    character(len=19) :: IAM='behaviours::get_czm_expo'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,w1,w2
    !> eta
    real(kind=8)      :: eta

    cn=0.d0;ct=0.d0
    s1=0.d0; s2=0.d0
    w1=0.d0; w2=0.d0
    eta=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    w1    = tact_behav(ibehav)%param(7)
    w2    = tact_behav(ibehav)%param(8)
    eta   = tact_behav(ibehav)%param(9)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_expo_p(ibehav,cn,ct,s1,s2,w1,w2,mu_g,eta)

    implicit none
    !                         123456789012345678901234
    character(len=19) :: IAM='behaviours::get_czm_expo'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,w1,w2
    !> eta / mu_g
    real(kind=8)      :: eta,mu_g

    cn=0.d0;ct=0.d0
    s1=0.d0; s2=0.d0
    w1=0.d0; w2=0.d0
    eta=0.d0; mu_g=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    w1    = tact_behav(ibehav)%param(7)
    w2    = tact_behav(ibehav)%param(8)
    mu_g  = tact_behav(ibehav)%param(9)
    eta   = tact_behav(ibehav)%param(10)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_expo_spring(ibehav,cn,ct,s1,s2,w1,w2,eta,k1,k2)

    implicit none
    !                         123456789012345678901234
    character(len=19) :: IAM='behaviours::get_czm_expo'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,w1,w2
    !> eta
    real(kind=8)      :: eta
    !> k1,k2
    real(kind=8)      :: k1,k2

    cn=0.d0;ct=0.d0
    s1=0.d0; s2=0.d0
    w1=0.d0; w2=0.d0
    eta=0.d0
    k1=0.d0; k2=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    w1    = tact_behav(ibehav)%param(7)
    w2    = tact_behav(ibehav)%param(8)
    eta   = tact_behav(ibehav)%param(9)
    k1    = tact_behav(ibehav)%param(10)
    k2    = tact_behav(ibehav)%param(11)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_expo_spring_p(ibehav,cn,ct,s1,s2,w1,w2,mu_g,eta,k1,k2)

    implicit none
    !                         123456789012345678901234
    character(len=19) :: IAM='behaviours::get_czm_expo'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: s1,s2,w1,w2
    !> eta
    real(kind=8)      :: mu_g, eta
    !> k1,k2
    real(kind=8)      :: k1,k2

    cn=0.d0;ct=0.d0
    s1=0.d0; s2=0.d0
    w1=0.d0; w2=0.d0
    mu_g=0.d0; eta=0.d0
    k1=0.d0; k2=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    s1    = tact_behav(ibehav)%param(5)
    s2    = tact_behav(ibehav)%param(6)
    w1    = tact_behav(ibehav)%param(7)
    w2    = tact_behav(ibehav)%param(8)
    mu_g  = tact_behav(ibehav)%param(9)
    eta   = tact_behav(ibehav)%param(10)
    k1    = tact_behav(ibehav)%param(11)
    k2    = tact_behav(ibehav)%param(12)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm_tri(ibehav,cn,ct,xc,fc)

    implicit none
    !                         12345678901234567890123
    character(len=19) :: IAM='behaviours::get_czm_tri'
    character(len=80) :: cout
    integer           :: ibehav
    !> stifness
    real(kind=8)      :: cn,ct
    !>
    real(kind=8)      :: xc,fc

    cn=0.d0;ct=0.d0
    xc=0.d0; fc=0.d0

    cn    = tact_behav(ibehav)%param(3)
    ct    = tact_behav(ibehav)%param(4)
    xc    = tact_behav(ibehav)%param(5)
    fc    = tact_behav(ibehav)%param(6)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_czm(ibehav,cn,ct,smax,w,b)

    implicit none
    !                         1234567890123456789
    character(len=19) :: IAM='behaviours::get_czm'
    character(len=80) :: cout
    integer           :: ibehav
    !> stiffness
    real(kind=8)      :: cn,ct
    !> critical stress and energy
    real(kind=8)      :: smax,w
    !> viscosity for damage evolution
    real(kind=8)      :: b

    cn=0.d0;ct=0.d0
    smax=0.d0; w=0.d0
    b=0.d0

    select case(tact_behav(ibehav)%ilaw)
    case(i_MAC_CZM            , &
         i_MAC_CZM_nosldt     , &
         i_MAC_CZM_noslds     , &
         i_IQS_MAC_CZM        , &
         i_ER_MAC_CZM         , &
         i_IQS_MAC_CZM_nosldt , &
         i_IQS_MAC_CZM_noslds , &
         i_postGAP_IQS_MAC_CZM, &
         i_IQS_WET_CZM        , &
         i_ELASTIC_REPELL_MAC_CZM) !vhnhu

       cn = tact_behav(ibehav)%param(3)
       ct = tact_behav(ibehav)%param(4)
       b  = tact_behav(ibehav)%param(5)
       w  = tact_behav(ibehav)%param(6)

    case(i_MSMP_CZM           )

       cn    = tact_behav(ibehav)%param(3)
       ct    = tact_behav(ibehav)%param(4)
       w     = tact_behav(ibehav)%param(6)

    case(i_MP_CZM              )
       cn = tact_behav(ibehav)%param(3)
       ct = tact_behav(ibehav)%param(4)
       w  = tact_behav(ibehav)%param(5)

    case(i_MP3_CZM,i_MP3_CZM_THER)

       cn   = tact_behav(ibehav)%param(3)
       ct   = tact_behav(ibehav)%param(4)
       smax = tact_behav(ibehav)%param(5)
       w    = tact_behav(ibehav)%param(6)

    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_czm

!!!------------------------------------------------------------------------
  !!!------------------------------------------------------------------------
  subroutine get_czm_tosi(ibehav,cn,ct,f0,fc,n,K,R,sigma0,Q,Gc1,Gc2,k_coal,hdef,n_mol)
    implicit none
    integer     , intent(in)  :: ibehav
    real(kind=8), intent(out) :: cn, ct
    real(kind=8), intent(out) :: f0,fc,n,K,R,sigma0,Q,Gc1,Gc2,k_coal,hdef,n_mol
    !                         123456789012345678901234
    character(len=24) :: IAM='behaviours::get_czm_tosi'
    character(len=80) :: cout

    cn     = tact_behav(ibehav)%param( 3)
    ct     = tact_behav(ibehav)%param( 4)
    f0     = tact_behav(ibehav)%param( 5)
    fc     = tact_behav(ibehav)%param( 6)
    n      = tact_behav(ibehav)%param( 7)
    K      = tact_behav(ibehav)%param(13)
    R      = tact_behav(ibehav)%param( 9)
    sigma0 = tact_behav(ibehav)%param(10)
    Q      = tact_behav(ibehav)%param(16)
    Gc1    = tact_behav(ibehav)%param(11)
    Gc2    = tact_behav(ibehav)%param(12)
    k_coal = tact_behav(ibehav)%param( 8)
    hdef   = tact_behav(ibehav)%param(14)
    n_mol  = tact_behav(ibehav)%param(15)

  end subroutine
  !!!------------------------------------------------------------------------
  subroutine put_czm(ibehav,cn,ct,smax,w,b)
    implicit none
    character(len=19) :: IAM='behaviours::put_czm'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: cn,ct     !> stifness
    real(kind=8)      :: smax,w    !> critical stress and energy
    real(kind=8)      :: b         !> viscosity for damage evolution

    select case(tact_behav(ibehav)%ilaw)
    case(i_MAC_CZM            , &
         i_MAC_CZM_nosldt     , &
         i_MAC_CZM_noslds     , &
         i_IQS_MAC_CZM        , &
         i_ER_MAC_CZM         , &
         i_IQS_MAC_CZM_nosldt , &
         i_IQS_MAC_CZM_noslds , &
         i_postGAP_IQS_MAC_CZM, &
         i_IQS_WET_CZM        , &
         i_ELASTIC_REPELL_MAC_CZM) !vhnhu

       tact_behav(ibehav)%param(3) = cn
       tact_behav(ibehav)%param(4) = ct
       tact_behav(ibehav)%param(5) = b
       tact_behav(ibehav)%param(6) = w

    case default

    end select

  end subroutine put_czm

  !!!------------------------------------------------------------------------
  subroutine get_forcePERgap(ibehav,forcePERgap)

    implicit none
    !                         123456789012345678901234567
    character(len=27) :: IAM='tact_behav::get_forcePERgap'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: forcePERgap

    forcePERgap = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_ELASTIC_REPELL_CLB,  &
         i_ELASTIC_REPELL_CLB_g0, &
         i_ELASTIC_REPELL_CLB_adapt, & !PTA sncf 2023
         i_VISCO_ELASTIC_REPELL_CLB, &
         i_ELASTIC_REPELL_WET_CLB, &
         i_VISCO_ELASTIC_REPELL_WET, &
         i_ELASTIC_REPELL_MAC_CZM)
       forcePERgap = tact_behav(ibehav)%param(1)
    case(i_BRITTLE_COATING_CLB)
       forcePERgap = tact_behav(ibehav)%param(2)
    case(i_ER_MAC_CZM)
       forcePERgap = tact_behav(ibehav)%param(3)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_forcePERgap

  !!!------------------------------------------------------------------------
  subroutine get_ToverH(ibehav,ToverH)

    implicit none
    !                         1234567890123456789012
    character(len=22) :: IAM='tact_behav::get_ToverH'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: ToverH

    ToverH = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_CRITICAL_VOIGT_CLB, &
         i_IQS_CLB_RGR)
       ToverH = tact_behav(ibehav)%param(1)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_ToverH

  !!!------------------------------------------------------------------------
  !!! CRITIC
  !!!------------------------------------------------------------------------
  subroutine get_viscOVERcritvisc(ibehav,vOVERcv)

    implicit none
    !                         123456789012345678901234567890123
    character(len=33) :: IAM='tact_behav::get_TviscOVERcritvisc'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: vOVERcv

    vOVERcv = 1.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_CRITICAL_VOIGT_CLB)
       vOVERcv = tact_behav(ibehav)%param(2)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_viscOVERcritvisc

  !!!------------------------------------------------------------------------
  subroutine get_gap_tol(ibehav,gap_tol)

    implicit none
    !                         12345678901234567890123
    character(len=23) :: IAM='tact_behav::get_gap_tol'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: gap_tol

    gap_tol = 0.D0

    select case(tact_behav(ibehav)%ilaw)
       !   123456789012345678901234567890
    case(i_IQS_CLB_RGR)
       gap_tol = tact_behav(ibehav)%param(2)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_gap_tol

  !!!------------------------------------------------------------------------
  subroutine get_forcePERstrain(ibehav,forcePERstrain)

    implicit none
    !                         123456789012345678901234567890
    character(len=30) :: IAM='tact_behav::get_forcePERstrain'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: forcePERstrain

    forcePERstrain = 0.d0

    select case(tact_behav(ibehav)%ilaw)
    case(i_ELASTIC_ROD, &
         i_ELASTIC_WIRE, &
         i_BRITTLE_ELASTIC_WIRE, &
         i_VOIGT_WIRE, &
         i_VOIGT_ROD, &
         i_KV_WET)
       forcePERstrain = tact_behav(ibehav)%param(1)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_forcePERstrain

  !!!------------------------------------------------------------------------
  subroutine get_prestrain(ibehav,prestrain)

    implicit none
    !                         1234567890123456789012345
    character(len=25) :: IAM='tact_behav::get_prestrain'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: prestrain

    prestrain = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_ELASTIC_ROD, &
         i_ELASTIC_WIRE, &
         i_BRITTLE_ELASTIC_WIRE, &
         i_VOIGT_WIRE, &
         i_VOIGT_ROD)
       prestrain = tact_behav(ibehav)%param(2)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_prestrain

  !!!------------------------------------------------------------------------
  !!!------------------------------------------------------------------------
  subroutine get_snmax(ibehav,snmax)

    implicit none
    !                         123456789012345678901
    character(len=21) :: IAM='tact_behav::get_snmax'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: snmax

    snmax = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_BRITTLE_ELASTIC_WIRE, &
         i_BRITTLE_COATING_CLB)
       snmax = tact_behav(ibehav)%param(3)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_snmax

  !!!------------------------------------------------------------------------
  subroutine get_forcePERstrainrate(ibehav,forcePERstrainrate)

    implicit none
    !                         1234567890123456789012345678901234
    character(len=34) :: IAM='tact_behav::get_forcePERstrainrate'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: forcePERstrainrate

    forcePERstrainrate = 0.d0

    select case(tact_behav(ibehav)%ilaw)
    case(i_VOIGT_WIRE, &
         i_VOIGT_ROD)
       forcePERstrainrate = tact_behav(ibehav)%param(3)
    case(i_KV_WET,i_VISCO_ELASTIC_REPELL_WET)
       forcePERstrainrate = tact_behav(ibehav)%param(2)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_forcePERstrainrate

  !!!------------------------------------------------------------------------
  subroutine get_kwear(ibehav,kcdwear,kanwear)

    implicit none
    integer      :: ibehav
    real(kind=8) :: kcdwear,kanwear

    kcdwear = tact_behav(ibehav)%param(1)
    kanwear = tact_behav(ibehav)%param(2)

  end subroutine get_kwear

  !!!------------------------------------------------------------------------
  subroutine get_periodic_strain(ibehav,E)

    implicit none
    integer                   :: ibehav
    real(kind=8),dimension(3) :: E

    E(1) = tact_behav(ibehav)%param(1)
    E(2) = tact_behav(ibehav)%param(2)
    E(3) = tact_behav(ibehav)%param(3)

  end subroutine get_periodic_strain

  !!!------------------------------------------------------------------------
  subroutine get_viscosity(ibehav,etan,etat)

    character(len=34) :: IAM='tact_behav::get_viscosity'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: etan,etat

    select case(tact_behav(ibehav)%ilaw)
    case(i_VISCO_ELASTIC_REPELL_WET)
       etan = tact_behav(ibehav)%param(2)
       etat = 0.D0
    case(i_VISCO_ELASTIC_REPELL_CLB)
       etan = tact_behav(ibehav)%param(2)
       etat = 0.D0
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_viscosity

  !!!------------------------------------------------------------------------
  subroutine get_threshold(ibehav,threshold)

    implicit none
    !                         1234567890123456789012345
    character(len=25) :: IAM='tact_behav::get_threshold'
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: threshold

    threshold = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_BROKEN_DOF)
       threshold = tact_behav(ibehav)%param(1)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_threshold

  !!!------------------------------------------------------------------------
  subroutine get_lambda(ibehav,lambdas,lambdac)
    implicit none
    !> tact law number
    integer(kind=4), intent(in) :: ibehav
    !> conductivity when interface is not totally broken
    real(kind=8), intent(out) :: lambdas
    !> conductivity when interface is totally broken
    real(kind=8), intent(out) :: lambdac
    !
    character(len=34) :: IAM='tact_behav::get_viscosity'
    character(len=80) :: cout

    select case(tact_behav(ibehav)%ilaw)
    case(i_MP3_CZM_THER)
       lambdas = tact_behav(ibehav)%param(7)
       lambdac = tact_behav(ibehav)%param(8)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_lambda

  !!-pta reactivation 02/09/2015--------------------------------------------
  subroutine get_pregap(ibehav,pre_gap)

   implicit none

                            !1234567890123456789012
   character(len=22) :: IAM='tact_behav::get_pregap'
   character(len=80) :: cout


   integer           :: ibehav
   real(kind=8)      :: pre_gap

   pre_gap=0.d0

   select case(tact_behav(ibehav)%ilaw)
     case(i_preGAP_SGR_CLB)
       pre_gap=tact_behav(ibehav)%param(1)
     case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
                                 !1234567                            1234567890123456
       call FATERR(IAM,cout)

   end select


 end subroutine get_pregap

 !!------------------------------------------------------------------------
 subroutine get_offset(ibehav,offset)

   implicit none

                            !12345678901234567890123
   character(len=23) :: IAM='tact_behav::offset'
   character(len=80) :: cout


   integer           :: ibehav
   real(kind=8)      :: offset


   offset=0.D0

   select case(tact_behav(ibehav)%ilaw)
   case(i_IQS_CLB_nosldt, &
        i_IQS_CLB_noslds, &
        i_GAP_SGR_CLB_nosldt, &
        i_GAP_SGR_CLB_noslds )

     offset=tact_behav(ibehav)%param(2)

   case(i_IQS_MAC_CZM_nosldt , &
        i_IQS_MAC_CZM_noslds , &
        i_MAC_CZM_nosldt , &
        i_MAC_CZM_noslds)

     offset=tact_behav(ibehav)%param(7)

   case default
     write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
                               !1234567                            1234567890123456
     call FATERR(IAM,cout)

   end select


 end subroutine get_offset

 !!------------------------------------------------------------------------
 !> get g0 parameter for a law
 subroutine get_g0(ibehav,g0)
   implicit none
   !> contact law id
   integer(kind=4), intent(in) :: ibehav
   !> g0 parameter of the law
   real(kind=8), intent(out) :: g0
   !
   character(len=14) :: IAM
   character(len=80) :: cout
   !      12345678901234
   IAM = 'tact_behav::g0'

   g0 = 0.d0

   select case(tact_behav(ibehav)%ilaw)
   case(i_BRITTLE_COATING_CLB)

     g0 = tact_behav(ibehav)%param(4)

   case default
     write(cout,'(A,A,A)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
     call faterr(IAM,cout)
   end select

 end subroutine get_g0

 !!!------------------------------------------------------------------------

  subroutine get_Enu(ibehav,Mod_young,Mod_poiss)
    implicit none
    !                         123456789012345678901234567
    character(len=27) :: IAM='tact_behav::get_Enu       '
    character(len=80) :: cout
    integer           :: ibehav
    real(kind=8)      :: Mod_young,Mod_poiss

    Mod_young=0.D0
    Mod_poiss=0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_NARD_ROD)
       Mod_young   = tact_behav(ibehav)%param(1)
       Mod_poiss   = tact_behav(ibehav)%param(2)
    case default
       write(cout,'(A7,A30,A16)')' lawty ',tact_behav(ibehav)%lawty,' not implemented'
       call FATERR(IAM,cout)
    end select

  end subroutine get_Enu

!!------------------------------------------------------------------------
 subroutine indent_tact_behav(ind_color)

    implicit none
    integer          :: ibehav
    character(len=3) :: ind_color

    do ibehav=1,size(tact_behav)
       tact_behav(ibehav)%behav=ind_color//tact_behav(ibehav)%behav(4:5)
    end do

  end subroutine indent_tact_behav
!!!------------------------------------------------------------------------
  integer function get_ibehav(behav)

    implicit none
    !                         1234567890123456789012
    character(len=22) :: IAM='tact_behav::get_ibehav'
    character(len=80) :: cout
    character(len=5)  :: behav
    integer           :: ibehav

    do ibehav=1,size(tact_behav)
       if (behav.eq.tact_behav(ibehav)%behav) then
          get_ibehav=ibehav
          return
       end if
    end do

    write(cout,'(A7,A30,A16)')' lawty ',behav,' not implemented'
    call FATERR(IAM,cout)

  end function get_ibehav

  function get_behav_name(ibehav)
    implicit none
    integer(kind=4)   :: ibehav
    character(len=5)  :: get_behav_name

    get_behav_name = 'none_'
    if( ibehav<=size(tact_behav) .and. ibehav>0 ) then
       get_behav_name = tact_behav(ibehav)%behav
    end if

  end function get_behav_name
!!!------------------------------------------------------------------------
  integer function get_nb_internal(ibehav)

    implicit none
    integer :: ibehav

    get_nb_internal = tact_behav(ibehav)%nb_internal

  end function get_nb_internal
!!!------------------------------------------------------------------------
  function init_internal(ibehav)

    implicit none
    real(kind=8),dimension(max_internal_tact) :: init_internal

    integer :: ibehav

    init_internal = 0.d0

  end function init_internal
!!!------------------------------------------------------------------------
  subroutine write_internal_comment(nfich,ibehav)

    implicit none
    integer :: nfich,ibehav

    write(nfich,'(A)') tact_behav(ibehav)%internal_comment

  end subroutine write_internal_comment
!!!------------------------------------------------------------------------
  function get_internal_comment(ibehav)

    implicit none
    integer :: ibehav
    character(len=internal_tact_comment_length) :: get_internal_comment

    get_internal_comment=tact_behav(ibehav)%internal_comment

  end function get_internal_comment
!!!------------------------------------------------------------------------
  subroutine compute_2D_smooth_forces(ibehav,vltBEGIN,vlnBEGIN,gapTTBEGIN,statusBEGIN, &
                                      internal,reff,meff,status,gapTT,vlt,vln,rlt,rln)

    implicit none
    integer(kind=4), intent(in)  :: ibehav
    real(kind=8)   , intent(in)  :: vltBEGIN,vlnBEGIN,gapTTBEGIN,reff,meff
    integer(kind=4), intent(in)  :: statusBEGIN

    real(kind=8)   , intent(out) :: gapTT,vlt,vln,rlt,rln
    integer(kind=4), intent(out) :: status

    real(kind=8),intent(inout),dimension(max_internal_tact) :: internal

    real(kind=8) :: kn,kt
    real(kind=8) :: gamma,dampn,dampt,epsilon,mu
    real(kind=8) :: dist,surfref,radhs,treshold
    real(kind=8) :: delta

    vln = 0.D0
    vlt = 0.D0

    select case(tact_behav(ibehav)%ilaw)
    case(i_MD_JKRs)

       kn    = tact_behav(ibehav)%param(1)
       kt    = tact_behav(ibehav)%param(2)
       dampn = tact_behav(ibehav)%param(3)
       gamma = tact_behav(ibehav)%param(4)
       mu    = tact_behav(ibehav)%param(5)

!fd comment ca peut etre cohesif si on n'autorise pas de coller !?
!fd il faudrait decaler pour que si c'est affleurant ca donne 0

       if (gapTTBEGIN.ge.0.D0) then
          rln    = 0.D0
          rlt    = 0.D0
          status = i_noctc
       else
          delta=-gapTTBegin

          rln = (kn*delta) - (dampn*dsqrt(meff)*vlnBEGIN) - gamma*dsqrt(delta*reff)

!

!fd ca semble etre la reaction cohesive
!fd mais pourquoi est ce divise par 4 !?
          radhs  = gamma*gamma*reff/(4.*kn)

          rlt=0.d0

!fd ces tests sont vraiment super etranges
!fd          IF (kt*DABS(vltBEGIN) .GT. mu*DABS(-rln+radhs))THEN
          if (kt*DABS(vltBEGIN) .lt. mu*DABS(-rln+radhs))then
             rlt = -kt*dabs(vltBEGIN)*sign(1.d0,vltBEGIN)
             status = i_stick
          else
             rlt = -mu*dabs(-rln+radhs)*sign(1.d0,vltBEGIN)
             status = i_slide
          end if
       end if

!!!
!!!----------------------------------------------------------------------
!!!
    case(i_DEM_FIBs)

       kn    = tact_behav(ibehav)%param(1)
       dampn = tact_behav(ibehav)%param(2)
       gamma = tact_behav(ibehav)%param(3)


    case DEFAULT

    end select

    gapTT= gapTTBEGIN


!    print *,'rln ',rln,'rlt ',rlt


  end subroutine compute_2D_smooth_forces
!!!------------------------------------------------------------------------
  subroutine compute_smooth_potential

    implicit none
    integer           :: ibehav
    real(kind=8)      :: sigma,epsilon,elec,rep,noycd,noyan,raycd,rayan
    real(kind=8)      :: rad,fn,gap


    select case(tact_behav(ibehav)%ilaw)
    case(i_LJ_POTENTIAL)

       sigma   = tact_behav(ibehav)%param(1)
       epsilon = tact_behav(ibehav)%param(2)

       elec  = tact_behav(ibehav)%param(3)
       rep   = tact_behav(ibehav)%param(4)

       noycd = tact_behav(ibehav)%param(5)
       noyan = tact_behav(ibehav)%param(6)

       raycd = tact_behav(ibehav)%param(7)
       rayan = tact_behav(ibehav)%param(8)

       rad = sigma/(noycd+noyan-raycd-rayan-gap)

       fn = -24*epsilon*( 2.0*rad**13 - rep*rad**7 )/sigma + 0.25

    end select

  end subroutine compute_smooth_potential
!!!------------------------------------------------------------------------
  subroutine compute_3D_smooth_forces(ibehav,vlsBEGIN,vltBEGIN,vlnBEGIN,gapTTBEGIN,statusBEGIN, &
                                      internal,reff,meff,status,gapTT,vls,vlt,vln,rls,rlt,rln)

    implicit none
    integer(kind=4), intent(in)  :: ibehav
    real(kind=8)   , intent(in)  :: vlsBEGIN,vltBEGIN,vlnBEGIN,gapTTBEGIN,reff,meff
    integer(kind=4), intent(in)  :: statusBEGIN

    real(kind=8)   , intent(out) :: gapTT,vls,vlt,vln,rls,rlt,rln
    integer(kind=4), intent(out) :: status

    real(kind=8),intent(inout),dimension(max_internal_tact) :: internal

    real(kind=8) :: kn,kt
    real(kind=8) :: gamma,dampn,dampt,epsilon,mu
    real(kind=8) :: dist,surfref,radhs,Vst,treshold

    vln = 0.D0
    vlt = 0.D0
    vls = 0.D0

    select case(tact_behav(ibehav)%ilaw)
!!!-------------------------------------
    case(i_MD_JKRs)

       kn    = tact_behav(ibehav)%param(1)
       kt    = tact_behav(ibehav)%param(2)
       dampn = tact_behav(ibehav)%param(3)
       gamma = tact_behav(ibehav)%param(4)
       mu    = tact_behav(ibehav)%param(5)

       if (gapTTBEGIN.ge.0.D0) then
          rln    = 0.D0
          rlt    = 0.D0
          rls    = 0.D0
          status = i_noctc
       else
          rln = (-kn*gapTTBEGIN) + (dampn*dsqrt(meff)*vlnBEGIN) + gamma*dsqrt(gapTTBEGIN*reff)

          radhs    = gamma*gamma*reff/(4.*kn)
          treshold = mu*DABS(-rln+radhs)

          Vst = vls*vls + vlt*vlt

          if (kt*kt*Vst.gt.treshold*treshold)then
             rlt = kt*dabs(vltBEGIN)*sign(1.d0,vltBEGIN)
             rls = kt*dabs(vlsBEGIN)*sign(1.d0,vlsBEGIN)
             status = i_stick
          else
             rlt = mu*dabs(-rln+radhs)*sign(1.d0,vltBEGIN)
             rls = mu*dabs(-rln+radhs)*sign(1.d0,vlsBEGIN)
             status = i_slide
          end if
       end if

    case(i_DEM_FIBs)

       kn    = tact_behav(ibehav)%param(1)
       dampn = tact_behav(ibehav)%param(2)
       gamma = tact_behav(ibehav)%param(3)

       if (gapTTBEGIN.ge.0.0) then
          rln    = 0.D0
          rlt    = 0.D0
          rls    = 0.D0
          status = i_noctc
       else
          rln = (-kn*gapTTBEGIN) + (dampn*dsqrt(meff)*vlnBEGIN) + gamma
          rlt    = 0.D0
          rls    = 0.D0
          status = i_stick
       end if
    case DEFAULT

    end select

    gapTT= gapTTBEGIN

  end subroutine compute_3D_smooth_forces
!!!------------------------------------------------------------------------
!!! BALLAST WEAR SPECIFIC FRICTION
!!! validation phase please contact M. Renouf at : Mathieu.Renouf@insa-lyon.fr
!!!------------------------------------------------------------------------
  real(kind=8) function get_fric_BW(ibehav,nbc)

    implicit none
    integer          :: ibehav
    character(len=5) :: statusBEGIN
    real(kind=8)     :: nbc

    if ( nbc .eq. 0 ) then
       !mr nbc could not be equal to zero. Its first value should be 1.
       !mr If the case is true a problem is hide somewhere.
       call faterr('tact_behav::get_fric_BW','nbc could not be equal to zero. Its first value should be 1.')
    end if

    get_fric_BW = tact_behav(ibehav)%param(1) + tact_behav(ibehav)%param(2)*log(nbc)

  end function get_fric_BW
!!!------------------------------------------------------------------------
  real(kind=8) function get_threshold_BW(ibehav,nbc)

    implicit none
    integer      :: ibehav
    real(kind=8) :: nbc
    if ( nbc .eq. 0 ) then
       !mr nbc could not be equal to zero. Its first value should be 1.
       !mr If the case is true a problem is hide somewhere.
       call faterr('tact_behav::get_threshold_BW','nbc could not be equal to zero. Its first value should be 1.')
    end if

    get_threshold_BW = tact_behav(ibehav)%param(3) + tact_behav(ibehav)%param(4)*log(nbc)

  end function get_threshold_BW
!!!------------------------------------------------------------------------
  real(kind=8) function get_alpha_BW(ibehav)

    implicit none
    integer :: ibehav

    get_alpha_BW = tact_behav(ibehav)%param(5)

  end function get_alpha_BW
!!!------------------------------------------------------------------------
  real(kind=8) function get_TshVlt_BW(ibehav)

    implicit none
    integer :: ibehav

    get_TshVlt_BW = tact_behav(ibehav)%param(6)

  end function get_TshVlt_BW
!!!------------------------------------------------------------------------
  real(kind=8) function get_Frequency_BW(ibehav)

    implicit none
    integer :: ibehav

    get_Frequency_BW = tact_behav(ibehav)%param(7)

  end function get_Frequency_BW
!!!------------------------------------------------------------------------
  function get_fric_CAP(ibehav,in)

!fd loi de mortier essayant de tenir de la rupture du coulis en compression

    implicit none
                             !123456789012345678901234
    character(len=24) :: IAM='tact_behav::get_fric_CAP'
    character(len=80) :: cout

    integer           ::  ibehav
    real(kind=8)      ::  get_fric_CAP,in,rn,gamma,cohet,rn_c,tan_phi,tan_gamma

    !
    rn = in/H

    tan_phi=tact_behav(ibehav)%param(2)/tact_behav(ibehav)%param(1)
    gamma=0.78539816 - (0.5*atan(tan_phi))
    tan_gamma=tan(gamma)
!faux
!    rn_c = (tact_behav(ibehav)%param(1) + ((1.d0+tan_gamma)*tact_behav(ibehav)%param(6))) / &
!             (tan_phi + tan_gamma)
    rn_c = ((-tact_behav(ibehav)%param(1)*tan_phi) + (tan_gamma*tact_behav(ibehav)%param(6))) / &
             (tan_phi + tan_gamma)


!fd on est oblige de tester en impulsion
!fd probable que pb si choc

    if (rn > rn_c) then
      if (rn > tact_behav(ibehav)%param(6)) then
        get_fric_cap=0.d0
      else
        cohet= tan_gamma*(tact_behav(ibehav)%param(6) - rn)
        get_fric_cap= cohet/tact_behav(ibehav)%param(1)
      endif
    else
      get_fric_cap=tan_phi
    endif
    !
  end function get_fric_CAP
!!!------------------------------------------------------------------------
  subroutine set_czm_initial_friction(p)
     implicit none
     !fd integer(kind=4) :: p
     real(kind=8) :: p

     czm_initial_friction_power = p

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_halo(halo)

    implicit none

    real(kind=8) :: halo

    global_adist = halo

  end subroutine set_halo
!!!------------------------------------------------------------------------
  subroutine get_ilaw_list(ilaw_list)
    implicit none
    integer, dimension(:), pointer :: ilaw_list
    !
    integer :: nb, idx, itact
    integer, dimension(:), allocatable :: ilaws

    nb = size(tact_behav)

    allocate(ilaws(nb))
    ilaws = 0

    idx = 0
    do itact = 1, nb
      if( any( ilaws == tact_behav(itact)%ilaw ) ) then
        cycle
      end if
      idx = idx + 1
      ilaws(idx) = tact_behav(itact)%ilaw
    end do

    allocate(ilaw_list(idx))
    ilaw_list(1:idx) = ilaws(1:idx)

    deallocate(ilaws)

  end subroutine get_ilaw_list

  subroutine tact_behav_info_by_id(id,nb_param,param_name,nb_internal,internal_comment)
    implicit none
    integer :: id,nb_param,nb_internal
    character(len=5),dimension(:),pointer :: param_name
    character(len=internal_tact_comment_length) :: internal_comment
                              !123456789012345678901234567
    character(len=33)  :: IAM='tact_behav::tact_behav_info_by_id'
    character(len=30)  :: name
    character(len=80)  :: cout

    nb_internal      = 0
    internal_comment = '#                                                                          '
    nb_param=0

    select case(id)
!!!-----------------------------------------
    case(i_IQS_CLB)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)    = 'sfric'
!!!-----------------------------------------
    case(i_IQS_CLB_g0)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)    = 'sfric'
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'g_ref         '

!!!-----------------------------------------
    case(i_IQS_MAP)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)    = 'sfric'
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'fric          '

!!!-----------------------------------------
    case(i_IQS_CLB_nosldt)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      param_name(2) = 'dt___'
      !
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'saut_de_ut    '

!!!-----------------------------------------
    case(i_IQS_CLB_noslds)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      param_name(2) = 'ds___'
      !
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'saut_de_us    '

!!!-----------------------------------------
    case(i_GAP_SGR_CLB_nosldt)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      param_name(2) = 'dt___'
      !
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'saut_de_ut    '

!!!-----------------------------------------
    case(i_GAP_SGR_CLB_noslds)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      param_name(2) = 'ds___'
      !
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'saut_de_us    '

!!!-----------------------------------------
    case(i_IQS_CLB_RGR)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'T/H  '
      param_name(2) = 'gTot '
      param_name(3) = 'sfric'

!!!-----------------------------------------
    case(i_IQS_DS_CLB)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'

!!!-----------------------------------------
    case(i_IQS_WET_DS_CLB)
      nb_param = 5
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'Wethk'
      param_name(4) = 'dfric'
      param_name(5) = 'sfric'

!!!-----------------------------------------
    case(i_RST_WET_CLB)
      nb_param = 7
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'restn'
      param_name(4) = 'restt'
      param_name(5) = 'Wethk'
      param_name(6) = 'dfric'
      param_name(7) = 'sfric'

!!!-----------------------------------------
    case(i_IQS_BW_CLB)
      nb_param = 7
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'Afric'
      param_name(2) = 'Bfric'
      param_name(3) = 'Atsld'
      param_name(4) = 'Btsld'
      param_name(5) = 'Alpha'
      param_name(6) = 'VtTsh'
      param_name(6) = 'CFreq'

      nb_internal = 4
      !                          12345678901234
      internal_comment( 2:15) = 'cycle number  '
      internal_comment(17:30) = '              '
      internal_comment(17:30) = '              '

!!!-----------------------------------------
    case(i_xQS_WET_DS_CLB)
      nb_param = 5
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'Wethk'
      param_name(4) = 'dfric'
      param_name(5) = 'sfric'

!!!-----------------------------------------
    case(i_TEX_SOL)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'

!!!-----------------------------------------
    case(i_TEX_SOL_UNILAT)
      nb_param = 0
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      nullify(param_name)
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '

!!!-----------------------------------------
    case(i_GAP_SGR_CLB)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'taille_ele    '

!!!-pta 20/05/2015--------------------------
    case(i_preGAP_SGR_CLB)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'pgap_'
      param_name(2) = 'sfric'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'taille_ele    '

!!!--------- am & pta: loi frottement sec defo/defo defo/rigides avec pre gap
    case(i_GAP_SGR_CLB_g0)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      nb_internal = 2
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '
      internal_comment(17:30) = 'g_ref         '
!!!-----------------------------------------
    case(i_VEL_SGR_CLB)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'
      nb_internal = 1
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '

!!!-----------------------------------------
    case(i_GAP_WET_DS_CLB)
      nb_param = 5
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'Wethk'
      param_name(4) = 'dfric'
      param_name(5) = 'sfric'
      nb_internal = 1
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '

!!!-----------------------------------------
    case(i_GAP_SGR_DS_CLB)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      nb_internal = 1
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '

!!!-----------------------------------------
    case(i_VEL_SGR_DS_CLB)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      nb_internal = 1
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '

!!!-----------------------------------------
    case(i_RST_CLB)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'restn'
      param_name(2) = 'restt'
      param_name(3) = 'sfric'

!!!-----------------------------------------
    case(i_RST_DS_CLB)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'restn'
      param_name(2) = 'restt'
      param_name(3) = 'dfric'
      param_name(4) = 'sfric'

!!!-----------------------------------------

    case(i_IQS_MOHR_DS_CLB)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'dfric'
      param_name(4) = 'sfric'

!!!-----------------------------------------

    case(i_GAP_MOHR_DS_CLB)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'dfric'
      param_name(4) = 'sfric'

!!!-----------------------------------------

    case(i_IQS_CAP_MOHR_DS_CLB)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'dfric'
      param_name(4) = 'sfric'
      param_name(5) = 's_ten'
      param_name(6) = 's_com'

!!!-----------------------------------------
    case(i_GAP_CAP_MOHR_DS_CLB)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'cohet'
      param_name(3) = 'dfric'
      param_name(4) = 'sfric'
      param_name(5) = 's_ten'
      param_name(6) = 's_com'

!!!-----------------------------------------
    case(i_ELASTIC_REPELL_CLB)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'sfric'

!!!-----------------------------------------
    case(i_ELASTIC_REPELL_CLB_g0)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'sfric'
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'g_ref         '

!!!----------------------------------------- PTA sncf 2023
    case(i_ELASTIC_REPELL_CLB_adapt)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'sfric'
      nb_internal      = 1
      !                          12345678901234
      internal_comment( 2:15) = 'F/gap         '

!!!-----------------------------------------
    case(i_VISCO_ELASTIC_REPELL_CLB)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'etan '
      param_name(3) = 'sfric'

!!!-----------------------------------------
    case(i_CRITICAL_VOIGT_CLB)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'T/H  '
      param_name(2) = 'v/cv '
      param_name(3) = 'sfric'
      nb_internal   = 3
      !                           12345678901234
      internal_comment( 2:15)  = 'lagT          '
      internal_comment(17:30)  = 'CVC on = 2    '
      internal_comment(32:45)  = 'lagS          '
!!!-----------------------------------------
    case(i_KV_WET)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/str'
      param_name(2) = 'F/sra'
      param_name(3) = 'cohen'
      param_name(4) = 'Wethk'
      nb_internal = 3
      !                          12345678901234
      internal_comment( 2:15) = '       Status '
      internal_comment(17:30) = '       GapREF '
      internal_comment(32:45) = '       Length '

!!!-----------------------------------------
    case(i_ELASTIC_REPELL_WET_CLB)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'cohen'
      param_name(3) = 'Wethk'
      param_name(4) = 'sfric'

!!!-----------------------------------------
    case(i_VISCO_ELASTIC_REPELL_WET)
      nb_param = 5
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'F/sra'
      param_name(3) = 'cohen'
      param_name(4) = 'Wethk'
      param_name(5) = 'fric '
      !
      nb_internal      = 6

!!!-----------------------------------------
    case(i_ELASTIC_ROD)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/str'
      param_name(2) = 'prstr'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '

!!!-----------------------------------------
    case(i_VOIGT_ROD)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/str'
      param_name(2) = 'prstr'
      param_name(3) = 'F/sra'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '

!!!-----------------------------------------
    case(i_ELASTIC_WIRE)
      nb_param = 2
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/str'
      param_name(2) = 'prstr'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '
!!!-----------------------------------------
    case(i_BRITTLE_ELASTIC_WIRE)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/str'
      param_name(2) = 'prstr'
      param_name(3) = 'snmax'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '

!!!-----------------------------------------
    case(i_RIGID_WIRE)
      nb_param = 0
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '

!!!-----------------------------------------
    case(i_VOIGT_WIRE)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/str'
      param_name(2) = 'prstr'
      param_name(3) = 'F/sra'
      nb_internal = 1
      !                         12345678901234
      internal_comment(2:15) = 'gapREF        '

!!!-----------------------------------------
    case(i_MD_JKRs)
      nb_param = 5
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'kn   '
      param_name(2) = 'kt   '
      param_name(3) = 'dampn'
      param_name(4) = 'gamma'
      param_name(5) = 'sfric'

!!!-----------------------------------------
!!! Smooth interaction law used in:
!!! Fillot N., Iordanoff I. and Berthier Y., '', 2006
!!!-----------------------------------------
    case(i_DEM_FIBs)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'kn   '
      param_name(2) = 'dampn'
      param_name(3) = 'gamma'
      nb_internal = 5
      !                          12345678901234
      internal_comment( 2:15) = 'status        '
      internal_comment(17:30) = 'gapREF        '
      internal_comment(32:45) = 'I0_x          '
      internal_comment(47:60) = 'I0_y          '
      internal_comment(62:75) = 'I0_z          '

!!!-----------------------------------------
    case(i_ELASTIC_WET_NT)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'kn   '
      param_name(2) = 'kt   '
      param_name(3) = 'dampn'
      param_name(4) = 'dampt'
      param_name(5) = 'adhen'
      param_name(6) = 'adhet'
      nb_internal = 3
      !                          12345678901234
      internal_comment( 2:15) = 'Ix ini        '
      internal_comment(17:30) = 'Iy ini        '
      internal_comment(32:45) = 'Iz ini        '

!!!-----------------------------------------
    case(i_COUPLED_DOF)
      nb_param = 0

!!!-----------------------------------------
    case(i_TANGENTIAL_COUPLED_DOF)
      nb_param = 0

!!!-----------------------------------------
    case(i_NORMAL_COUPLED_DOF)
      nb_param = 0

!!!-----------------------------------------
    case(i_IQS_STICK,i_GAP_SGR_STICK)
      nb_param = 0

!!!-----------------------------------------
    case(i_PLASTIC_COUPLED_DOF)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sfric'

!!!-----------------------------------------
    case(i_BROKEN_DOF)
      nb_param = 1
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'thshd'
      nb_internal = 1
      !                          12345678901234
      internal_comment(2 :15) = '       status '

!!!-----------------------------------------
    !fd for periodic loadings
    case(i_PERIO_DOF)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'E_XX '
      param_name(2) = 'E_YY '
      param_name(3) = 'E_XY '

!!!-----------------------------------------
    case(i_GAP_SGR_CLB_WEAR)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'kcdwe'
      param_name(2) = 'kanwe'
      param_name(3) = 'fric '
      nb_internal = 3
      !                          12345678901234
      internal_comment(2 :15) = '       ?????  '
      internal_comment(17:30) = '       ?????  '
      internal_comment(32:45) = '       ?????  '

!!!-----------------------------------------
    case(i_IQS_SGR_CLB_WEAR)
      nb_param = 3
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'scohe'
      param_name(2) = 'dcohe'
      param_name(3) = 'fric '
      param_name(4) = 'Wethk'
      nb_internal = 3
      !                          12345678901234
      internal_comment(2 :15) = '       ?????  '
      internal_comment(17:30) = '       ?????  '
      internal_comment(32:45) = '       ?????  '


!!!-----------------------------------------
    ! monerie-acary-cangemi cohesive zone model
    !fd internal = surf,ucut,ucun,beta

    case(i_MAC_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'

      if (nbdime == 2 ) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbdime == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '

      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------

    case(i_MAC_CZM_nosldt)
      nb_param = 7
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      param_name(7) = 'dt___'

      if (nbdime == 2 ) then
        call faterr(IAM,'Unsupported nbDIME')
      else if (nbdime == 3 ) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------

    case(i_MAC_CZM_noslds)
      nb_param = 7
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      param_name(7) = 'ds___'

      if (nbdime == 2 ) then
        call faterr(IAM,'Unsupported nbDIME')
      else if (nbdime == 3 ) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
    ! monerie-perales cohesive zone model
    !fd internal = surf,ucut,ucun,beta

    case(i_MP_CZM)
      nb_param = 5
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'dupre'

      if (nbdime == 2 ) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbdime == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
     else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
     ! monerie-perales 3 parameters cohesive zone model
     !fd internal = surf,ucut,ucun,beta

    case(i_MP3_CZM)
      nb_param = 6
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'smax '
      param_name(6) = 'dupre'

      if (nbdime == 2 ) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbdime == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
     ! monerie-perales 3 parameters cohesive zone model +ther
     !fd internal = surf,ucut,ucun,beta

    case(i_MP3_CZM_ther)
      nb_param = 8
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'smax '
      param_name(6) = 'dupre'
      param_name(7) = 'lmbds'
      param_name(8) = 'lmbdc'

      if (nbdime == 2 ) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbdime == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
    !fp tvergaard-hutchinson cohesive zone model
    !fp internal = surf,ucut,ucun,beta

    case(i_TH_CZM)
      nb_param = 10
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 's1   '
      param_name(6) = 's2   '
      param_name(7) = 'G1   '
      param_name(8) = 'G2   '
      param_name(9) = 'l1   '
      param_name(10)= 'l2   '
      if (nbdime == 2 ) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbdime == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
    case(i_IQS_TH_CZM)
      nb_param = 10
      allocate(param_name(nb_param))

      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 's1   '
      param_name(6) = 's2   '
      param_name(7) = 'G1   '
      param_name(8) = 'G2   '
      param_name(9) = 'l1   '
      param_name(10)= 'l2   '
      if (nbdime == 2 ) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbdime == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif


!!!-----------------------------------------
    case(i_IQS_WET_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'cohen'
      param_name(2) = 'Wethk'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      nb_internal = 6
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '
      internal_comment(17:30) = 'saut_de_ut    '
      internal_comment(32:45) = 'saut_de_un    '
      internal_comment(47:60) = 'beta          '
      internal_comment(62:75) = 'Cnd anisotrope'

!!!-------------------------------------
    case(i_ELASTIC_REPELL_MAC_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'F/gap'
      param_name(2) = 'gcmax'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      nb_internal = 4
      !                          12345678901234
      internal_comment( 2:15) = 'taille_ele    '
      internal_comment(17:30) = 'saut_de_ut    '
      internal_comment(32:45) = 'saut_de_un    '
      internal_comment(47:60) = 'beta          '

!!!-----------------------------------------
    case(i_IQS_MAC_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      if (nbDIME == 2) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbDIME == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
    !fd loi elastic-repell en compression / mac_czm en traction
    case(i_ER_MAC_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      if (nbDIME == 2) then
        nb_internal = 4
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
      else if (nbDIME == 3) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
    case(i_IQS_MAC_CZM_nosldt)
       nb_param = 7
       !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
       allocate(param_name(nb_param))
       param_name(1) = 'dfric'
       param_name(2) = 'sfric'
       param_name(3) = 'cn   '
       param_name(4) = 'ct   '
       param_name(5) = 'visco'
       param_name(6) = 'dupre'
       param_name(7) = 'dt___'

       if (nbDIME == 2) then
         call faterr(IAM,'Unsupported nbDIME')
       else if (nbDIME == 3) then
         nb_internal = 6
         !                          12345678901234
         internal_comment( 2:15) = 'taille_ele    '
         internal_comment(17:30) = 'saut_de_ut    '
         internal_comment(32:45) = 'saut_de_un    '
         internal_comment(47:60) = 'saut_de_us    '
         internal_comment(62:75) = 'beta          '
         internal_comment(77:90) = 'TPSini        '
       else
         call faterr(IAM,'Unsupported nbDIME')
       endif

!!!-----------------------------------------
    case(i_IQS_MAC_CZM_noslds)
      nb_param = 7
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'
      param_name(7) = 'ds___'

      if (nbDIME == 2) then
        call faterr(IAM,'Unsupported nbDIME')
      else if (nbDIME == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif

!!!-----------------------------------------
    case(i_postGAP_IQS_MAC_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'

      if (nbDIME == 2) then

        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
      else if (nbDIME == 3) then
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_MSMP_CZM)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'visco'
      param_name(6) = 'dupre'

      if (nbDIME == 2) then

        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbDIME == 3) then
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_MAL_CZM)
      nb_param = 8
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'S1   '
      param_name(6) = 'S2   '
      param_name(7) = 'G1   '
      param_name(8) = 'G2   '

      if (nbDIME == 2) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbDIME == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_IQS_MAL_CZM)
      nb_param = 8
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'dfric'
      param_name(2) = 'sfric'
      param_name(3) = 'cn   '
      param_name(4) = 'ct   '
      param_name(5) = 'S1   '
      param_name(6) = 'S2   '
      param_name(7) = 'G1   '
      param_name(8) = 'G2   '
      if (nbDIME == 2) then
        nb_internal = 4
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
      else if (nbDIME == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_ABP_CZM, i_IQS_ABP_CZM)
      nb_param = 11
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)  = 'dfric'
      param_name(2)  = 'sfric'
      param_name(3)  = 'cn   '
      param_name(4)  = 'ct   '
      param_name(5)  = 'S1   '
      param_name(6)  = 'S2   '
      param_name(7)  = 'G1   '
      param_name(8)  = 'G2   '
      param_name(9)  = 'Du1  '
      param_name(10) = 'Du2  '
      param_name(11) = 'Phi  '

      if (nbDIME == 2) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbDIME == 3) then
        nb_internal = 6
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_EXPO_CZM, i_IQS_EXPO_CZM)
      nb_param = 9
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)  = 'dfric'
      param_name(2)  = 'sfric'
      param_name(3)  = 'cn   '
      param_name(4)  = 'ct   '
      param_name(5)  = 'S1   '
      param_name(6)  = 'S2   '
      param_name(7)  = 'G1   '
      param_name(8)  = 'G2   '
      param_name(9)  = 'eta  '

      if (nbDIME == 2) then
        nb_internal = 5
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
      else if (nbDIME == 3) then
        nb_internal = 7
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'saut_de_us    '
        internal_comment(62:75) = 'beta          '
        internal_comment(77:90) = 'TPSini        '
        internal_comment(92:105)= 'g0            '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)
      nb_param = 10
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)  = 'dfric'
      param_name(2)  = 'sfric'
      param_name(3)  = 'cn   '
      param_name(4)  = 'ct   '
      param_name(5)  = 'S1   '
      param_name(6)  = 'S2   '
      param_name(7)  = 'G1   '
      param_name(8)  = 'G2   '
      param_name(9)  = 'mu_g '
      param_name(10) = 'eta  '

      if (nbDIME == 2) then
        nb_internal = 9
        !                          12345678901234
        internal_comment( 2:15)  = 'taille_ele    '
        internal_comment(17:30)  = 'saut_de_ut    '
        internal_comment(32:45)  = 'saut_de_un    '
        internal_comment(47:60)  = 'beta          '
        internal_comment(62:75)  = 'TPSini        '
        internal_comment(77:90)  = 'saut_de_ut_p  '
        internal_comment(92:105) = 'saut_de_un_p  '
        internal_comment(107:120)= 'saut_de_ut_p1 '
        internal_comment(122:135)= 'saut_de_un_p1 '
      else if (nbDIME == 3) then
        nb_internal = 13
        !                          12345678901234
        internal_comment( 2:15)   = 'taille_ele    '
        internal_comment(17:30)   = 'saut_de_ut    '
        internal_comment(32:45)   = 'saut_de_un    '
        internal_comment(47:60)   = 'saut_de_us    '
        internal_comment(62:75)   = 'beta          '
        internal_comment(77:90)   = 'TPSini        '
        internal_comment(92:105)  = 'g0            '
        internal_comment(107:120) = 'saut_de_ut_p  '
        internal_comment(122:135) = 'saut_de_un_p  '
        internal_comment(137:150) = 'saut_de_us_p  '
        internal_comment(152:165) = 'saut_de_ut_p1 '
        internal_comment(167:180) = 'saut_de_us_p1 '
        internal_comment(182:195) = 'saut_de_un_p1 '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_EXPO_CZM_SPRING, i_IQS_EXPO_CZM_SPRING)
      nb_param = 11
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)  = 'dfric'
      param_name(2)  = 'sfric'
      param_name(3)  = 'cn   '
      param_name(4)  = 'ct   '
      param_name(5)  = 'S1   '
      param_name(6)  = 'S2   '
      param_name(7)  = 'G1   '
      param_name(8)  = 'G2   '
      param_name(9)  = 'eta  '
      param_name(10) = 'k1   '
      param_name(11) = 'k2   '

      if (nbDIME == 2) then
        nb_internal = 9
        !                          12345678901234
        internal_comment( 2:15) = 'taille_ele    '
        ! saut deplacement endo
        internal_comment(17:30) = 'saut_de_ut    '
        internal_comment(32:45) = 'saut_de_un    '
        internal_comment(47:60) = 'beta          '
        internal_comment(62:75) = 'TPSini        '
        ! saut deplacement elas
        ! (la partie normale n'est pas conservee car on peut la recalculer avec gaptt)
        internal_comment(77:90) = 'saut_de_ut_e  '
        internal_comment(92:105)= 'g0            '
	! endomagemments thermiques
        internal_comment(107:120) = 'beta_coh_th   '
        internal_comment(122:135) = 'beta_spring_th' 		

      else if (nbDIME == 3) then
        nb_internal = 11
        !                          12345678901234
        internal_comment( 2:15)  = 'taille_ele    '
        ! saut deplacement endo
        internal_comment(17:30)  = 'saut_de_ut    '
        internal_comment(32:45)  = 'saut_de_un    '
        internal_comment(47:60)  = 'saut_de_us    '
        internal_comment(62:75)  = 'beta          '
        internal_comment(77:90)  = 'TPSini        '
        ! saut deplacement elas
        ! (la partie normale n'est pas conservee car on peut la recalculer avec gaptt)
        internal_comment(92:105) = 'saut_de_ut_e  '
        internal_comment(107:120)= 'saut_de_us_e  '
        internal_comment(122:135)= 'g0            '
        ! endomagemments thermiques
        internal_comment(137:150) = 'beta_coh_th   '
        internal_comment(152:165) = 'beta_spring_th' 		

     else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_EXPO_CZM_SPRING_P, i_IQS_EXPO_CZM_SPRING_P)
      nb_param = 12
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1)  = 'dfric'
      param_name(2)  = 'sfric'
      param_name(3)  = 'cn   '
      param_name(4)  = 'ct   '
      param_name(5)  = 'S1   '
      param_name(6)  = 'S2   '
      param_name(7)  = 'G1   '
      param_name(8)  = 'G2   '
      param_name(9)  = 'mu_g '
      param_name(10) = 'eta  '
      param_name(11) = 'k1   '
      param_name(12) = 'k2   '

      if (nbDIME == 2) then
        nb_internal = 11
        !                            12345678901234
        internal_comment( 2:15)   = 'taille_ele    '
        ! saut deplacement endo
        internal_comment(17:30)   = 'saut_de_ut    '
        internal_comment(32:45)   = 'saut_de_un    '
        internal_comment(47:60)   = 'beta          '
        internal_comment(62:75)   = 'TPSini        '
        ! saut deplacement elas
        ! (la partie normale n'est pas conservee car on peut la recalculer avec gaptt)
        internal_comment(77:90)   = 'saut_de_ut_e  '
        internal_comment(92:105)  = 'g0            '
        ! saut deplacement plastiques
        internal_comment(107:120) = 'saut_de_ut_p  '
        internal_comment(122:135) = 'saut_de_un_p  '
        internal_comment(137:150) = 'saut_de_ut_p1 '
        internal_comment(152:165) = 'saut_de_un_p1 '

      else if (nbDIME == 3) then
        nb_internal = 15
        !                            12345678901234
        internal_comment( 2:15)   = 'taille_ele    '
        ! saut deplacement endo
        internal_comment(17:30)   = 'saut_de_ut    '
        internal_comment(32:45)   = 'saut_de_un    '
        internal_comment(47:60)   = 'saut_de_us    '
        internal_comment(62:75)   = 'beta          '
        internal_comment(77:90)   = 'TPSini        '
        ! saut deplacement elas
        ! (la partie normale n'est pas conservee car on peut la recalculer avec gaptt)
        internal_comment(92:105)  = 'saut_de_ut_e  '
        internal_comment(107:120) = 'saut_de_us_e  '
        internal_comment(122:135) = 'g0            '
        ! saut deplacement plastiques
        internal_comment(137:150) = 'saut_de_ut_p  '
        internal_comment(152:165) = 'saut_de_us_p  '
        internal_comment(167:180) = 'saut_de_un_p  '
        internal_comment(182:195) = 'saut_de_ut_p1 '
        internal_comment(197:210) = 'saut_de_us_p1 '
        internal_comment(212:225) = 'saut_de_un_p1 '
     else
        call faterr(IAM,'Unsupported nbDIME')
     endif

!!!-----------------------------------------
    case(i_LJ_POTENTIAL)
      nb_param = 7
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'sigma'
      param_name(2) = 'epsil'
      param_name(3) = 'elect'
      param_name(4) = 'noycd'
      param_name(5) = 'noyan'
      param_name(6) = 'radcd'
      param_name(7) = 'radan'

!!!-----------------------------------------
         !123456789012345678901234567890
    case(i_IQS_PLAS_CLB)
      nb_param = 6
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'rnmax'
      param_name(2) = 'rovrg'
      param_name(3) = 'sfric'
      param_name(4) = 'pfric'
      param_name(5) = 'dgap '
      param_name(6) = 'dfric'

      nb_internal = 3
      !                          12345678901234
      internal_comment( 2:15) = 'pgap          '
      internal_comment(17:30) = 'pfric         '
      internal_comment(32:45) = 'pgap max      '

!!!-----------------------------------------
    case(i_BRITTLE_COATING_CLB)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'fric '
      param_name(2) = 'F/gap'
      param_name(3) = 'snmax'
      param_name(4) = 'g0   '

      nb_internal = 0

!!!-----------------------------------------
    case(i_NARD_ROD)
      nb_param = 4
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name(1) = 'E    '
      param_name(2) = 'nu   '
      param_name(3) = 'S1   '
      param_name(4) = 'S2   '

      nb_internal = 6
      !                          12345678901234
      internal_comment( 2:12) = 'dn         '
      internal_comment(14:24) = 'dt         '
      internal_comment(26:36) = 'ds         '
      internal_comment(38:48) = 'surf       '
      internal_comment(50:60) = 'l0         '
      internal_comment(62:72) = 'Masse      '

!!!-----------------------------------------
    case(i_GTN_CZM, i_GTN2_CZM)
      nb_param = 17
      !IF(ASSOCIATED(param_name)) DEALLOCATE(param_name)
      allocate(param_name(nb_param))
      param_name( 1) = 'dyfr '
      param_name( 2) = 'stfr '
      param_name( 3) = 'cn   '
      param_name( 4) = 'ct   '
      param_name( 5) = 'f0   '
      param_name( 6) = 'fc   '
      param_name( 7) = 'k    '
      param_name( 8) = 'q1   '
      param_name( 9) = 'q2   '
      param_name(10) = 'e    '
      param_name(11) = 'Y    '
      param_name(12) = 's0   '
      param_name(13) = 'Kh   '
      param_name(14) = 'n    '
      param_name(15) = 'fN   '
      param_name(16) = 'eN   '
      param_name(17) = 'sN   '

      !nb_internal = 6
      !!                          12345678901234
      !internal_comment( 2:12) = 'dn         '
      !internal_comment(14:24) = 'dt         '
      !internal_comment(26:36) = 'ds         '
      !internal_comment(38:48) = 'surf       '
      !internal_comment(50:60) = 'l0         '
      !internal_comment(62:72) = 'Masse      '

!!!-----------------------------------------
    case(i_TOSI_CZM)
      nb_param = 16

      allocate(param_name(nb_param))
      param_name( 1) = 'dyfr '
      param_name( 2) = 'stfr '
      param_name( 3) = 'cn   '
      param_name( 4) = 'ct   '
      param_name( 5) = 'f0   '
      param_name( 6) = 'fc   '
      param_name( 7) = 'n    '
      param_name( 8) = 'kcoal'
      param_name( 9) = 'R    '
      param_name(10) = 's0   '
      param_name(11) = 'Gc1  '
      param_name(12) = 'Gc2  '
      param_name(13) = 'K    '
      param_name(14) = 'hdef '
      param_name(15) = 'n_mol'
      param_name(16) = 'Q    '

      if (nbDIME == 2) then
        nb_internal = 11
        !                             12345678901234
        internal_comment( 2:15)   = 'taille_ele    '
        internal_comment(17:30)   = 'saut_de_ut    '
        internal_comment(32:45)   = 'saut_de_un    '
        internal_comment(47:60)   = 'beta          '
        internal_comment(62:75)   = 'saut_de_ut_max'
        internal_comment(77:90)   = 'saut_de_un_max'
        internal_comment(92:105)  = 'coalescence   '
        internal_comment(107:120) = 'tris_coal     '
        internal_comment(122:135) = 'energy1       '
        internal_comment(137:150) = 'energy2       '
        internal_comment(152:165) = 'fc            '
      else if (nbDIME == 3) then
        nb_internal = 13
        !                            12345678901234
        internal_comment(  2: 15) = 'taille_ele    '
        internal_comment( 17: 30) = 'saut_de_ut    '
        internal_comment( 32: 45) = 'saut_de_un    '
        internal_comment( 47: 60) = 'saut_de_us    '
        internal_comment( 62: 75) = 'beta          '
        internal_comment( 77: 90) = 'saut_de_ut_max'
        internal_comment( 92:105) = 'saut_de_un_max'
        internal_comment(107:120) = 'saut_de_us_max'
        internal_comment(122:135) = 'coalescence   '
        internal_comment(137:150) = 'tris_coal     '
        internal_comment(152:165) = 'energy1       '
        internal_comment(167:180) = 'energy2       '
        internal_comment(182:195) = 'fc            '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif
!!!-----------------------------------------
    case(i_TOSI_CZM_INCRE)

      nb_param = 16

      allocate(param_name(nb_param))
      param_name( 1) = 'dyfr '
      param_name( 2) = 'stfr '
      param_name( 3) = 'cn   '
      param_name( 4) = 'ct   '
      param_name( 5) = 'f0   '
      param_name( 6) = 'fc   '
      param_name( 7) = 'n    '
      param_name( 8) = 'kcoal'
      param_name( 9) = 'R    '
      param_name(10) = 's0   '
      param_name(11) = 'Gc1  '
      param_name(12) = 'Gc2  '
      param_name(13) = 'K    '
      param_name(14) = 'hdef '
      param_name(15) = 'n_mol'
      param_name(16) = 'Q    '

      if (nbDIME == 2) then
        nb_internal = 14
        !                            12345678901234
        internal_comment( 2:15)   = 'taille_ele    '
        internal_comment(17:30)   = 'saut_de_ut    '
        internal_comment(32:45)   = 'saut_de_un    '
        internal_comment(47:60)   = 'beta          '
        internal_comment(62:75)   = 'saut_de_ut_max'
        internal_comment(77:90)   = 'saut_de_un_max'
        internal_comment(92:105)  = 'coalescence   '
        internal_comment(107:120) = 'tris_coal     '
        internal_comment(122:135) = 'energy1       '
        internal_comment(137:150) = 'energy2       '
        internal_comment(152:165) = 'fc            '
        internal_comment(167:180) = 'gamma         '
        internal_comment(182:195) = 'Rn_a          '
        internal_comment(197:210) = 'Rt_a          '

      else if (nbDIME == 3) then
        nb_internal = 17
        !                            12345678901234
        internal_comment(  2: 15) = 'taille_ele    '
        internal_comment( 17: 30) = 'saut_de_ut    '
        internal_comment( 32: 45) = 'saut_de_un    '
        internal_comment( 47: 60) = 'saut_de_us    '
        internal_comment( 62: 75) = 'beta          '
        internal_comment( 77: 90) = 'saut_de_ut_max'
        internal_comment( 92:105) = 'saut_de_un_max'
        internal_comment(107:120) = 'saut_de_us_max'
        internal_comment(122:135) = 'coalescence   '
        internal_comment(137:150) = 'tris_coal     '
        internal_comment(152:165) = 'energy1       '
        internal_comment(167:180) = 'energy2       '
        internal_comment(182:195) = 'fc            '
        internal_comment(197:210) = 'gamma         '
        internal_comment(212:225) = 'Rn_a          '
        internal_comment(227:240) = 'Rt_a          '
        internal_comment(242:255) = 'Rs_a          '
      else
        call faterr(IAM,'Unsupported nbDIME')
      endif



!!!-----------------------------------------
    case default
       name = get_inter_law_name_from_id(id)
       write(cout,'(A6,A,A8)') 'lawty ',trim(name),' unknown'
       call faterr(IAM,cout)
       !                           123456789012345678901234567890
    end select

  end subroutine

  subroutine tact_behav_info(name,id,nb_param,param_name,nb_internal,internal_comment)
    implicit none
    character(len=30)::name
    integer :: id,nb_param,nb_internal
    character(len=5),dimension(:),pointer :: param_name
    character(len=internal_tact_comment_length) :: internal_comment
                              !123456789012345678901234567
    character(len=27)  :: IAM='tact_behav::tact_behav_info'
    character(len=80)  :: cout

    id = get_inter_law_id_from_name(name)

    if (id == -99) then
      call logmes(name)
      call faterr(IAM,'Unknown interaction law')
    endif

    call tact_behav_info_by_id(id,nb_param,param_name,nb_internal,internal_comment)

  end subroutine tact_behav_info

!!!------------------------------------------------------------------------
!!! gestion des frictions map ?
!!!------------------------------------------------------------------------
!mr: il faut etre vigilant sur le temps initial qui doit etre a zero.
!   SUBROUTINE init_friction_map

!     IMPLICIT NONE
!     INTEGER           :: imap,ikine
!     CHARACTER(len=26) :: IAM='overall::init_friction_map'
!     CHARACTER(len=39) :: cout

! !!! sizing FRICTION_MAP

!     G_nfich = get_io_unit()
!     OPEN(unit=G_nfich,file=TRIM(location(in_frictionmap(:))))

!     imap  = 0
!     ikine = 0

!     DO
!        IF( .NOT. read_G_clin()) EXIT
!        IF(G_clin(2:6).NE.'kine_') CYCLE
!        ikine = ikine + 1
!     END DO

!     WRITE(cout,'(A6,I10,A39)') 'found ',ikine,' different local values in friction map'

!     CALL LOGMES(cout)

!     nb_FMkine = ikine

!     IF(nb_FMkine.EQ.0)THEN
!        CALL FATERR(IAM,'no local value have been found in Mu_EVOLUTION.DAT file')
!        STOP
!     ELSE
!        ALLOCATE(FMkine(nb_FMkine))
!        DO ikine = 1,nb_FMkine
!           FMkine(ikine)%vlocy = 0.0
!           FMkine(ikine)%force = 0.0
!           NULLIFY(FMkine(ikine)%FRICTION_MAP)
!        END DO
!        ALLOCATE(force2map(nb_FMkine))
!        ALLOCATE(vlocy2map(nb_FMkine))
!     END IF

!     REWIND(G_nfich)

!     ikine = 0

!     DO
!        IF( .NOT. read_G_clin()) EXIT
!        IF(G_clin(2:6).NE.'kine_') CYCLE
!        ikine = ikine + 1
!        READ(G_clin(15:50),'(D14.7,8X,D14.7)') FMkine(ikine)%vlocy,FMkine(ikine)%force
!     END DO

!     force2map = 0
!     vlocy2map = 0

!     force2map(1) = FMkine(1)%force
!     DO ikine=2,nb_FMkine
!        IF(force2map(ikine).LT.

!     END DO


!     FMindex = imap

!     IF (FMindex.NE.0) THEN
!        ALLOCATE(FRICTION_MAP(FMindex))
!        DO imap =1,FMindex
!           FRICTION_MAP(imap)%fric = 0.0
!           FRICTION_MAP(imap)%TPS  = 0.0
!           FRICTION_MAP(imap)%step = 0
!        END DO
!     END IF



! !!! filling FRICTION_MAP

!     imap = 0

!     DO
!        IF( .NOT. read_G_clin()) EXIT

!        imap = imap + 1

!        READ(G_clin,'(2(1X,D14.7))') FRICTION_MAP(imap)%TPS,FRICTION_MAP(imap)%fric
!        FRICTION_MAP(imap)%step = INT(FRICTION_MAP(imap)%TPS/H)+1

!     END DO

!     CLOSE(G_nfich)

!     FMlast = 1

!   END SUBROUTINE init_friction_map
!!!------------------------------------------------------------------------
  subroutine init_friction_evolution
implicit none
    integer           :: imap
    !                         12345678901234567890123456789012
    character(len=26) :: IAM='overall::init_friction_evolution'
    character(len=80) :: cout !len = 80 <--- vhnhu

!!! sizing FRICTION_MAP

    G_nfich = get_io_unit()
    open(unit=G_nfich,file=trim(location(in_frictionevo(:))))

    imap  = 0
    do
       if( .not. read_G_clin()) exit
       imap = imap + 1
    end do

    write(cout,'(A6,I10,A39)') 'found ',imap,' values in friction map'

    call LOGMES(cout)

    nb_FEvalues = imap

    if(nb_FEvalues.eq.0)then
       call FATERR(IAM,'no local value have been found in Mu_EVOLUTION.DAT file')
    else
       allocate(FRICTION_EVOL(nb_FEvalues))
       do imap =1,nb_FEvalues
          FRICTION_EVOL(imap)%fric = 0.0
          FRICTION_EVOL(imap)%TPS  = 0.0
          FRICTION_EVOL(imap)%step = 0
       end do
    end if

!!! filling FRICTION_MAP
    rewind(G_nfich)
    imap = 0

    do
       if( .not. read_G_clin()) exit

       imap = imap + 1

       read(G_clin,'(2(1X,D14.7))') FRICTION_EVOL(imap)%TPS,FRICTION_EVOL(imap)%fric
       FRICTION_EVOL(imap)%step = int(FRICTION_EVOL(imap)%TPS/H)+1

    end do

    close(G_nfich)

    FElast = 1

    FEVOLflag = .true.

    FEVOL_write_flag = .false.  ! .TRUE. if want to write Mu_EVOLUTION.OUT
    if (FEVOL_write_flag) then
       G_nfich = get_io_unit()
       open(unit=G_nfich,STATUS='REPLACE',file=trim(location('OUTBOX/Mu_EVOLUTION.OUT')))
       close(G_nfich)
    end if

  end subroutine init_friction_evolution
!!!------------------------------------------------------------------------
  subroutine update_friction_evolution(fric)

    integer      :: imap,ilast
    real(kind=8) :: fric,alpha,current_fric

    ilast = FElast
    current_fric = FRICTION_EVOL(ilast)%fric
    do imap = FElast,nb_FEvalues

       if ( TPS .le. FRICTION_EVOL(imap)%TPS) then

          if (imap.eq.1)  exit

          alpha = ( TPS -  FRICTION_EVOL(imap-1)%TPS) / &
               (FRICTION_EVOL(imap)%TPS - FRICTION_EVOL(imap-1)%TPS)

          current_fric = FRICTION_EVOL(imap-1)%fric &
               + alpha*(FRICTION_EVOL(imap)%fric-FRICTION_EVOL(imap-1)%fric)

          ilast = imap
          exit

       end if

    end do
    ! we will write the friction's evolution to a file OUTBOX/FRICTION.OUT for test IN-OUT data
    if (FEVOL_write_flag) then
       G_nfich= get_io_unit()
       open(unit=G_nfich,STATUS='OLD',POSITION='APPEND',file=trim(location('OUTBOX/FRICTION.OUT')))
       write(G_nfich,'(2(1X,D14.7))') TPS,current_fric
       close(G_nfich)
    endif

    FElast = ilast
    fric   = current_fric

  end subroutine update_friction_evolution
!!!------------------------------------------------------------------------
  subroutine set_random_friction(ratio)
    implicit none
    real(kind=8) :: ratio

    MuRatio          = ratio
    RandomMuIsActive = .true.

  end subroutine set_random_friction

!<fd obsolete law are now managing mixity
!    2019-02-07 keep intentionally this here for the moment

! !!!------------------------------------------------------------------------
!  subroutine set_g1overg2(val)
!    implicit none
!    real(kind=8)      :: val

!    g1overg2 = val

!  end subroutine
! !!!------------------------------------------------------------------------
!  subroutine set_s1overs2(val)
!    implicit none
!    real(kind=8)      :: val

!    s1overs2 = val

!  end subroutine
! !!!------------------------------------------------------------------------
!  subroutine set_d1overd2(val)
!    implicit none
!    real(kind=8)      :: val

!    d1overd2 = val

!  end subroutine

!fd />

!!!------------------------------------------------------------------------
 subroutine set_RNcap(val)
   implicit none
   real(kind=8)      :: val

   RNcap = val

 end subroutine

!!!------------------------------------------------------------------------
 function get_RNcap()
   implicit none
   real(kind=8) :: get_RNcap

   get_RNcap = RNcap

 end function

!!!------------------------------------------------------------------------
 subroutine set_dilatancy_parameters(fric,height)
   implicit none
   real(kind=8) :: fric,height

   dilatancy_fric = fric
   dilatancy_height = height

 end subroutine

!!!------------------------------------------------------------------------
 function get_dilatancy_fric(ibehav)
   implicit none
   integer :: ibehav

   real(kind=8) :: get_dilatancy_fric

   select case(tact_behav(ibehav)%ilaw)
   case(i_MAC_CZM,i_IQS_MAC_CZM)

     get_dilatancy_fric = dilatancy_fric

  case default

     get_dilatancy_fric = 0.d0

   end select

 end function

!!!------------------------------------------------------------------------
 function get_dilatancy_height(ibehav,internal)
   implicit none
   integer :: ibehav
   real(kind=8),dimension(max_internal_tact) :: internal
   real(kind=8) :: get_dilatancy_height
   ! ****
   real(kind=8) :: nucut

   select case(tact_behav(ibehav)%ilaw)
   case(i_MAC_CZM, i_IQS_MAC_CZM)

     nucut = sqrt((internal(2)**2) + (internal(4)**2))

     get_dilatancy_height = min(get_dilatancy_fric(ibehav)*nucut,dilatancy_height)

   case default

     get_dilatancy_height = 0.d0

   end select

 end function get_dilatancy_height

 !!!------------------------------------------------------------------------

 subroutine get_nard_coeff(ibehav,internal,Ksn,Kst,Kvn,Kvt)

   implicit none

   integer :: ibehav
   real(kind=8),dimension(max_internal_tact) :: internal

   real(kind=8) :: l0,surf
   real(kind=8) :: Ksn,Kst,Kvn,Kvt
   real(kind=8) :: mass_ctc
   real(kind=8) :: Mod_young,coeff_poiss

   CALL get_Enu(ibehav,Mod_young,coeff_poiss)

   surf     = internal(4)
   l0       = internal(5)
   mass_ctc = internal(6)

   Ksn = (Mod_young*surf)/l0

   Kst = Ksn/(2.d0*(1.d0+coeff_poiss))

   Kvn = 2.d0*sqrt(mass_ctc*Ksn)

   Kvt = 2.d0*sqrt(mass_ctc*Kst)

  end subroutine get_nard_coeff




  !!!------------------------------------------------------------------------
  subroutine get_tact_behav_rank_from_name(name,rank)
   implicit none
   character(len=5) :: name
   integer           :: rank
   ! ***
   integer           :: i

   rank = -99

   do i=1,size(tact_behav)
     if (tact_behav(i)%behav  == name) then
       rank = i
       exit
     endif
   enddo
  end subroutine get_tact_behav_rank_from_name

  !!!------------------------------------------------------------------------
  subroutine get_param_rank_from_name(tact_behav_rank,param_name,rank)
   implicit none
   integer           :: tact_behav_rank,rank
   character(len=5) :: param_name
   ! ***
   integer           :: i

   rank=-99
   do i=1,size(tact_behav(tact_behav_rank)%param_name)
     if (tact_behav(tact_behav_rank)%param_name(i)== param_name) then
       rank = i
       exit
     endif
   enddo
  end subroutine get_param_rank_from_name

  !!!------------------------------------------------------------------------
  subroutine get_param(tact_behav_rank,param_rank,val)
   implicit none
   integer           :: tact_behav_rank,param_rank
   real(kind=8)      :: val

   val = tact_behav(tact_behav_rank)%param(param_rank)

  end subroutine get_param

  !!!------------------------------------------------------------------------
  subroutine set_param(tact_behav_rank,param_rank,val)
   implicit none
   integer           :: tact_behav_rank,param_rank
   real(kind=8)      :: val

   tact_behav(tact_behav_rank)%param(param_rank) = val

  end subroutine set_param

  !!!------------------------------------------------------------------------
  subroutine tosi_czm(dut,dun,dus,ibehav,internal,taz,Rn_d_int,Rt_d_int,Rs_d_int,Kn,Kt,Ks,store)
    implicit none
    real(kind=8), intent(in)    :: dut,dun,dus
    integer     , intent(in)    :: ibehav
    real(kind=8), intent(inout) :: internal(max_internal_tact)
    real(kind=8), intent(in)    :: taz(max_taz)
    ! Les composantes du vecteur cohesif max
    real(kind=8), intent(out)   :: Rn_d_int,Rt_d_int,Rs_d_int
    real(kind=8), intent(out)   :: Kn,Kt,Ks
    logical     , intent(in)    :: store

    ! from taz
    !t_t   : Le temps actuel
    !tri_s : La triaxialite en contrainte
    !T : La temperature
    real(kind=8) :: t_t, tri_s, T

    ! le vecteur saut de deplacement
    real(kind=8) :: u(3)
    ! interne
    real(kind=8) :: alpha, beta, gamma, f
    real(kind=8) :: epsi_m, epsi_eq, E
    real(kind=8) :: S_tt, S_ss, S_ts, tri
    real(kind=8) :: R_Un,R_Ut,R_Us,Rn_d,Rt_d,Rs_d
    real(kind=8) :: K_tild, d_un
    !---
    !la porosite actuelle
    real(kind=8) :: fn
    !Un_max,Ut_max,Us_max : Les sauts de déplacement max
    !(relatif à l'histoire du chargement de la zone cohesive)
    real(kind=8) :: Un_max, Ut_max, Us_max
    ! relatif a l apparition de la coalescence de la zone cohesive
    logical      :: coalescence
    ! valeur de la triaxialite en contrainte lors de l'apparition de la coalescence
    real(kind=8) :: tri_coalescence
    real(kind=8) :: en1, en2
    !---
    real(kind=8) :: cn, ct, cs  ! Mpa/m
    real(kind=8) :: f0,fc,n,K,R,sigma0,Q,Gc1,Gc2,betaf
    real(kind=8) :: k_coal,hdef,n_mol
    real(kind=8) :: P
    ! cutoff lorsque t=0
    real(kind=8) :: t_ini, E_ini

    t_ini = 1d-25
    E_ini = 1d-25

    t_t   = taz(4)
    tri_s = taz(5)
    T     = taz(6)

    call get_czm_tosi(ibehav,cn,ct,f0,fc,n,K,R,sigma0,Q,Gc1,Gc2,k_coal,hdef,n_mol)

    cs = ct

    if (nbdime==2) then

      u=(/internal(3)+dun,internal(2)+dut,0.d0/)
      ! S,ut,un,fn,ut_max,un_max,coalescence,tri_s_coal,en,fc

      betaf    = internal(4)
      fn = 1.d0 - betaf
      Ut_max= internal(5)
      Un_max= internal(6)
      Us_max= 0.d0
      if (internal(7) == 0.d0) then
        coalescence=.false.
      else
        coalescence=.true.
      end if
      tri_coalescence=internal(8)
      en1 = internal( 9)
      en2 = internal(10)
      fc  = internal(11)

    else

      u=(/internal(3)+dun,internal(2)+dut,internal(4)+dus/)
      ! S,ut,un,us,fn,ut_max,un_max,us_max,coalescence,tri_s_coal,en

      betaf = internal(5)
      fn = 1.d0 - betaf
      Ut_max= internal(6)
      Un_max= internal(7)
      Us_max= internal(8)
      if (internal(9) == 0.d0) then
        coalescence = .FALSE.
      else
        coalescence = .TRUE.
      endif
      tri_coalescence=internal(10)
      en1 = internal(11)
      en2 = internal(12)
      fc  = internal(13)

    endif

    !---
    if (u(1)<=0) then
      u(1)=0
    end if
    !---
    ! On étend les valeurs du modèle de Salvo pour des T <1100 °C
    if (T<1373.15) then
       T=1373.15
    end if

    Q = MAX(482.d3,(876.d3-0.25d3*T))

    K_tild = K*exp((-Q)/(R*T))

    ! Calcul de la pression dans les pores :
    P = n_mol*R*T/f0

    if (u(1)>Un_max) then
      Un_max=u(1)
    end if
    if (abs(u(2))>Ut_max) then
      Ut_max=abs(u(2))
    end if
    if (abs(u(3))>Us_max) then
      Us_max=abs(u(3))
    end if
    !print*,'Un_max= ',Un_max,' Ut_max= ',Ut_max,' Us_max= ',Us_max

    !Calcul de la triaxialité en taux de déformation :
    if ( coalescence ) then
      tri = tri_coalescence*(1.0d0/3.0d0)*((n*(fn**(-1.d0/n)-1.d0))**((-2.d0*n)/(n+1.d0)))/((1.d0+(2.d0/3.d0)*fn)* &
            (1.d0-fn)**((-2.d0*n)/(n+1.d0)))
    else
      tri = (tri_s)*(1.d0/3.d0)*((n*(fn**(-1.d0/n)-1.d0))**((-2.d0*n)/(n+1.d0)))/((1.d0+(2.d0/3.d0)*fn)*(1.d0-fn)** &
            ((-2.d0*n)/(n+1.d0)))
    end if
    !print *,"tri = ",tri," tri_s = ",tri_s

    !Condition de mauvais fonctionnement du code :
    ! discriminant positif hyp => compute gamma
    if ( ( (Ut_max**2+Us_max**2) /= 0.d0 ) .and. &
         ( 1.d0 + 3.d0*Un_max**2/(Ut_max**2+Us_max**2) < tri**2 ) ) then
      call faterr('tact_behav::tosi_czm', 'negative discriminant, cannot compute gamma')
    end if

    gamma=((2.d0*tri**2+1.d0)/(2.d0*(tri**2-1.d0)))*(Un_max/hdef)- &
            sqrt(tri**2*(9.d0*Un_max**2/hdef**2+3.d0*(1.d0-tri**2)*(Ut_max**2/hdef**2+Us_max**2/hdef**2)))/(2.d0*(tri**2-1.d0))

    !Mise à jour de f:
    f = 1.d0 - (1.d0-f0) * exp((-Un_max/hdef)-2.d0*gamma)

    if( f<fc ) then
      fn = f
    else
      fn=fc+k_coal*(f-fc)
    end if

    if (fn >= 1.d0) then
      fn=0.999d0
    end if

    betaf = 1.d0 - fn

    !Calcul de alpha et beta :

    alpha = (n*(fn**(-1.d0/n)-1.d0))**((-2.d0*n)/(n+1.d0))
    beta  = (1.d0+(2.d0/3.d0)*fn)*(1.d0-fn)**((-2.d0*n)/(n+1.d0))

    !Calcul de R avec prise en compte de la décharge:
    R_Un = 1.d0
    R_Ut = 1.d0
    R_Us = 1.d0

    if (Un_max/= 0.d0) then
      R_Un=u(1)/Un_max
    end if
    if (Ut_max/= 0.d0) then
      R_Ut=abs(u(2))/Ut_max
    end if
    if (Us_max/= 0.d0) then
      R_Us=abs(u(3))/Us_max
    end if
    !print *,"Ratio Un = ",R_Un," Ratio Ut = ",R_Ut

    epsi_m  = Un_max/(3.d0*hdef)+(2.d0/3.d0)*gamma
    epsi_eq = sqrt((4.d0/9.d0)*(((Un_max/hdef)-gamma)**2+(3.d0/4.d0)*Ut_max**2/hdef**2+(3.d0/4.d0)*Us_max**2/hdef**2))
    E       = sqrt((1.d0/alpha)*(3.d0*(epsi_m))**2+(1.d0/beta)*epsi_eq**2)

    !Calcul du vecteur contrainte cohésive avec décharge + pression pores
    Rn_d = R_Un * ( sigma0*asinh(E/(t_t*K_tild+t_ini))*(1.d0/(E+E_ini)) * &
           ( (1.d0/alpha)*((Un_max/hdef)+2*gamma) + (4.d0/(9.d0*beta))*((Un_max/hdef)-gamma) ) - P )


    Rt_d=R_Ut*sigma0*asinh(E/(t_t*K_tild+t_ini))*(1.d0/(E+E_ini))*(1.d0/(3.d0*beta))*(Ut_max/hdef)
    Rs_d=R_Us*sigma0*asinh(E/(t_t*K_tild+t_ini))*(1.d0/(E+E_ini))*(1.d0/(3.d0*beta))*(Us_max/hdef)
    !print*,"Rn_d = ", Rn_d," Rt_d = ", Rt_d," Rs_d = ", Rs_d

    !Tranformation intrinseque
    Rn_d_int=min(Cn*u(1),Rn_d)
    Rt_d_int=sign(min(Ct*abs(u(2)),abs(Rt_d)),u(2))
    Rs_d_int=sign(min(Cs*abs(u(3)),abs(Rs_d)),u(3))

    !print *, "Différence modification intrinseque (Rn_d,Rn_d_int) : ",Rn_d,Rn_d_int

    !Reste du tenseur de cohésion
    S_tt=sigma0*asinh(E/(t_t*K_tild+t_ini))*(1.d0/(E+E_ini))*((1.d0/alpha)*((u(1)/hdef)+2.d0*gamma)+ &
         (2.d0/(9.d0*beta))*(gamma-(u(1)/hdef)))
    S_ss=sigma0*asinh(E/(t_t*K_tild+t_ini))*(1.d0/(E+E_ini))*((1.d0/alpha)*((u(1)/hdef)+2.d0*gamma)+ &
         (2.d0/(9.d0*beta))*(gamma-(u(1)/hdef)))
    S_ts=0.d0

    !Forme de sigma :
    !  | Rn_d_int  Rt_d_int   Rs_d_int |
    !  | Rt_d_int    S_tt       S_ts   |
    !  | Rs_d_int    S_ts       S_ss   |

    ! Rigidité apparente
    !print*,'Un_max= ',Un_max,' Ut_max= ',Ut_max,' Us_max= ',Us_max

    if (Un_max==0.d0) then
      Kn=Cn
    else
      Kn=Rn_d_int/Un_max
    end if

    if (Ut_max==0.d0) then
      Kt=Ct
    else
      Kt=Rt_d_int/Ut_max
    end if

    if (Us_max==0.d0) then
      Ks=Cs
    else
      Ks=Rs_d_int/Us_max
    end if

    !print*,'Kn= ',Kn,' Kt= ',Kt,' Ks= ',Ks

    ! Energy I criterion
    if (en1 < Gc1) then
      if (Rn_d_int==(Cn*u(1))) then
        en1 = en1 + (Rn_d_int+P)*dun
      else
        en1 = en1 + (Rn_d_int+2*P)*dun
      endif
    else
      en1 = Gc1
    end if

    ! Energy II/III criteria

    if (en2 < Gc2) then
       en2 = en2 + Rt_d_int*dut + Rs_d_int*dus
    else
       en2 = Gc2
    end if

    if ( ( en1>=Gc1 .or. en2>=Gc2 ) .and. (.not. coalescence) ) then
      fc=fn
      coalescence = .true.
      tri_coalescence = tri_s
    end if

    if (store) then
      if (nbdime == 2) then
        internal(2)  = u(2)
        internal(3)  = u(1)
        internal(4)  = betaf
        internal(5)  = Ut_max
        internal(6)  = Un_max
        if (coalescence) then
          internal(7)=1.d0
        else
          internal(7)  = 0.d0
        end if
        internal( 8) = tri_coalescence
        internal( 9) = en1
        internal(10) = en2
        internal(11) = fc
      else
        internal(2)  = u(2)
        internal(3)  = u(1)
        internal(4)  = u(3)
        internal(5)  = betaf
        internal(6)  = Ut_max
        internal(7)  = Un_max
        internal(8)  = Us_max
        if (coalescence) then
          internal(9)=1.d0
        else
          internal(9)  = 0.d0
        end if
        internal(10) = tri_coalescence
        internal(11) = en1
        internal(12) = en2
        internal(13) = fc
      endif

      !print*,internal
    endif

  end subroutine tosi_czm

  subroutine tosi_czm_incre(vt,vn,vs,ibehav,internal,taz,dt,Rn_d_int,Rt_d_int,Rs_d_int,Kn,Kt,Ks,store)
    implicit none
    !(vn,vt,vs) : Le vecteur saut de déplacement
    real(kind=8), intent(in)    :: vt,vn,vs
    integer     , intent(in)    :: ibehav
    real(kind=8), intent(inout) :: internal(max_internal_tact)
    real(kind=8), intent(in)    :: taz(max_taz)
    ! pas de temps
    real(kind=8), intent(in)    :: dt
    !Rn_d_int,Rt_d_int,Rs_d_int : Les composantes du vecteur cohésif max
    real(kind=8), intent(out)   :: Rn_d_int,Rt_d_int,Rs_d_int
    real(kind=8), intent(out)   :: Kn,Kt,Ks
    logical     , intent(in)    :: store
    !
    ! triaxialite en contrainte
    real(kind=8) :: tri_s
    real(kind=8) :: u(3)
    ! temperature
    real(kind=8) :: T
    !Un_max,Ut_max,Us_max : Les sauts de déplacement max (relatif à l'histoire du chargement de la zone cohésive)
    real(kind=8) :: alpha, beta, gamma, gammap
    real(kind=8) :: f, epsi_mp, epsi_eqp, Ep
    real(kind=8) :: S_tt,S_ss,S_ts
    real(kind=8) :: tri
    real(kind=8) :: R_Un,R_Ut,R_Us,Rn_d,Rt_d,Rs_d
    real(kind=8) :: K_tild
    real(kind=8) :: ut_a,un_a,us_a,gamma_a
    real(kind=8) :: Rn_a,Rt_a,Rs_a
    !---
    !fn: porosite actuelle
    real(kind=8) :: fn, betaf
    real(kind=8) :: Un_max,Ut_max,Us_max
    ! booléen relatif à l apparition de la coalescence de la zone cohesive
    logical      :: coalescence
    ! valeur de la triaxialité en contrainte lors de l'apparition de la coalescence
    real(kind=8) :: tri_coalescence
    real(kind=8) :: en1, en2
    !---
    real(kind=8) :: cn,ct,cs
    real(kind=8) :: f0,fc,n,K,R,sigma0,Q,Gc1,Gc2
    real(kind=8) :: k_coal,hdef,n_mol,P

    real(kind=8) :: t_ini, E_ini
    integer      :: flag1,flag2,flag3
    real(kind=8) :: vvt,vvn,vvs

    ! cutoff lorsque t=0
    t_ini = 1d-25
    E_ini = 1d-25

    tri_s = taz(5)
    T     = taz(6)

    call get_czm_tosi(ibehav,cn,ct,f0,fc,n,K,R,sigma0,Q,Gc1,Gc2,k_coal,hdef,n_mol)
    cs = ct

    if (nbdime==2) then

      if ( vn == 0.d0 .and. vt == 0.d0) then
        vvn=taz(2)
        vvt=taz(1)
        vvs=0.d0
      else
        vvn=vn
        vvt=vt
        vvs=0.d0
      endif

      u=(/internal(3)+vvn*dt,internal(2)+vvt*dt,0.d0/)

      betaf  = internal(4)
      fn     = 1.d0 - betaf
      Ut_max = internal(5)
      Un_max = internal(6)
      Us_max = 0.d0
      if (internal(7) == 0.d0) then
        coalescence = .false.
      else
        coalescence = .true.
      endif
      tri_coalescence= internal( 8)
      en1            = internal( 9)
      en2            = internal(10)
      fc             = internal(11)
      gamma_a        = internal(12)
      Rn_a           = internal(13)
      Rt_a           = internal(14)
      Rs_a           = 0.d0

    else

      if ( vn == 0.d0 .and. vt == 0.d0 .and. vs == 0.d0) then
         vvn=taz(2)
         vvt=taz(1)
         vvs=taz(3)
      else
         vvn=vn
         vvt=vt
         vvs=vs
      endif

      u = (/internal(3)+vvn*dt,internal(2)+vvt*dt,internal(4)+vvs*dt/)
      !internal  S,ut_a,un_a,us_a,fn,ut_max,un_max,us_max,coalescence,tri_s,en,fc,gamma

      betaf  = internal(5)
      fn     = 1.d0 - betaf
      Ut_max = internal(6)
      Un_max = internal(7)
      Us_max = internal(8)
      if (internal(9) == 0.d0) then
        coalescence=.false.
      else
        coalescence=.true.
      endif
      tri_coalescence=internal(10)
      en1    = internal(11)
      en2    = internal(12)
      fc     = internal(13)
      gamma_a= internal(14)
      Rn_a   = internal(15)
      Rt_a   = internal(16)
      Rs_a   = internal(17)

    endif

    !---
    if (u(1)<=0) then
      u(1) = 0.d0
    end if
    !---

    ! On étend les valeurs du modèle de Salvo pour des T <1100 °C
    if (T<1373.15) then
      T = 1373.15d0
    end if

    Q = MAX(482.d3,(876.d3-0.25d3*T))
    !print*, Q
    K_tild=K*exp((-Q)/(R*T))

    ! Calcul de la pression dans les pores :
    P = n_mol*R*T/f0

    !----------------------------------------------------!
    if (u(1)>Un_max) then
      Un_max=u(1)
      flag1=0
    else
      flag1=1
    end if
    if (abs(u(2))>Ut_max) then
      Ut_max=abs(u(2))
      flag2 =0
    else
      flag2=1
    end if
    if (abs(u(3))>Us_max) then
      Us_max=abs(u(3))
      flag3=0
    else
      flag3=1
    end if

    !Calcul de la triaxialité en taux de déformation :
    if ( coalescence ) then
      tri=tri_coalescence*(1.0d0/3.0d0)*((n*(fn**(-1.d0/n)-1.d0))**((-2.d0*n)/(n+1.d0)))/((1.d0+(2.d0/3.d0)*fn)* &
          (1.d0-fn)**((-2.d0*n)/(n+1.d0)))
    else
      tri = (tri_s)*(1.d0/3.d0)*((n*(fn**(-1.d0/n)-1.d0))**((-2.d0*n)/(n+1.d0)))/((1.d0+(2.d0/3.d0)*fn)*(1.d0-fn)** &
            ((-2.d0*n)/(n+1.d0)))
    end if

    !Condition de bon fonctionnement du code : discriminant positif hyp
    ! if ((Ut_max**2+Us_max**2) == 0.d0 .or. 1.d0+3.d0*Un_max**2/(Ut_max**2+Us_max**2)>=tri**2) then
    if ( (Ut_max**2+Us_max**2) /= 0.d0 .and. &
         (1.d0+3.d0*Un_max**2/(Ut_max**2+Us_max**2)<tri**2) ) then

      call faterr('tact_behav::tosi_czm_incre', 'discriminant negative')
    end if

    !Calcul de gamma
    gammap = ((2.d0*tri**2+1.d0)/(2.d0*(tri**2-1.d0)))*(vvn/hdef)- &
             sqrt(tri**2*(9.d0*vvn**2/hdef**2+3.d0*(1.d0-tri**2)*(vvt**2/hdef**2+vvs**2/hdef**2)))/ &
             (2.d0*(tri**2-1.d0))

    gamma = (gammap*dt+gamma_a)

    !Mise à jour de f:
    f = 1.d0 - (1.d0-f0)*exp((-Un_max/hdef)-2.d0*gamma)

    if (f<fc) then
      fn=f
    else
      fn=fc+k_coal*(f-fc)
    end if

    if (fn >= 1.d0) then
       fn=0.999d0
    end if

    betaf = 1.d0 - fn

    !Calcul de alpha et beta :
    alpha = (n*(fn**(-1.d0/n)-1.d0))**((-2.d0*n)/(n+1.d0))
    beta  = (1.d0+(2.d0/3.d0)*fn)*(1.d0-fn)**((-2.d0*n)/(n+1.d0))

    !print *, 'alpha = ',alpha , 'beta = ',beta
    !Calcul de R avec prise en compte de la décharge:
    R_Un = 1.d0
    R_Ut = 1.d0
    R_Us = 1.d0

    if (Un_max/= 0.d0) then
      R_Un=u(1)/Un_max
    end if
    if (Ut_max/= 0.d0) then
      R_Ut=abs(u(2))/Ut_max
    end if
    if (Us_max/= 0.d0) then
      R_Us=abs(u(3))/Us_max
    end if

    epsi_mp  = vvn/(3.d0*hdef)+(2.d0/3.d0)*gammap
    epsi_eqp = sqrt((4.d0/9.d0)*(((vvn/hdef)-gammap)**2+(3.d0/4.d0)*vvt**2/hdef**2+(3.d0/4.d0)*vvs**2/hdef**2))
    Ep       = sqrt((1.d0/alpha)*(3.d0*(epsi_mp))**2+(1.d0/beta)*epsi_eqp**2)

    !Calcul du vecteur contrainte cohésive + Prise en compte de Ppore
    Rn_d = sigma0*asinh(Ep/(K_tild))*(1.d0/(Ep+E_ini)) * &
           ((1.d0/alpha)*((vvn/hdef)+2*gammap)+(4.d0/(9.d0*beta))*((vvn/hdef)-gammap)) - P
    Rt_d = sigma0*asinh(Ep/(K_tild))*(1.d0/(Ep+E_ini))*(1.d0/(3.d0*beta))*(vvt/hdef)
    Rs_d = sigma0*asinh(Ep/(K_tild))*(1.d0/(Ep+E_ini))*(1.d0/(3.d0*beta))*(vvs/hdef)

    !Tranformation intrinseque+ décharge
    if (flag1==0) then
       Rn_d_int=min(Cn*u(1),Rn_d)
       Rn_a=Rn_d_int
    else
       Rn_d_int=R_Un*Rn_a
    end if

    if (flag2==0) then
       Rt_d_int=sign(min(Ct*abs(u(2)),abs(Rt_d)),u(2))
       Rt_a=Rt_d_int
    else
       Rt_d_int=R_Ut*Rt_a
    end if

    if (flag3==0) then
       Rs_d_int=sign(min(Cs*abs(u(3)),abs(Rs_d)),u(3))
       Rs_a=Rs_d_int
    else
       Rs_d_int=R_Us*Rs_a
    end if

    !Reste du tenseur de cohesion
    S_tt=sigma0*asinh(Ep/(K_tild+t_ini))*(1.d0/(Ep+E_ini))*((1.d0/alpha)*((vvn/hdef)+2.d0*gammap)+ &
         (2.d0/(9.d0*beta))*(gammap-(vvn/hdef)))
    S_ss=sigma0*asinh(Ep/(K_tild+t_ini))*(1.d0/(Ep+E_ini))*((1.d0/alpha)*((vvn/hdef)+2.d0*gammap)+ &
         (2.d0/(9.d0*beta))*(gammap-(vvn/hdef)))
    S_ts=0.d0

    !Forme de sigma :
    !  | Rn_d_int  Rt_d_int   Rs_d_int |
    !  | Rt_d_int    S_tt       S_ts   |
    !  | Rs_d_int    S_ts       S_ss   |

    ! Rigidité apparente
    if (Un_max==0.d0) then
      Kn=Cn
    else
      Kn=Rn_a/Un_max
    end if

    if (Ut_max==0.d0) then
      Kt=Ct
    else
      Kt=sign(Rt_a/Ut_max,u(2))
    end if

    if (Us_max==0.d0) then
      Ks=Cs
    else
      Ks=sign(Rs_a/Us_max,u(3))
    end if

    ! Energy I criterion
    if (en1 < Gc1) then
      if (Rn_d_int==(Cn*u(1))) then
        en1=en1+(Rn_d_int+P)*vvn*dt
      else
        en1=en1+(Rn_d_int+2*P)*vvn*dt
      endif
    else
      en1 = Gc1
    end if

    ! Energy II/III criterion

    if (en2 < Gc2) then
       en2=en2+Rt_d_int*vvt*dt+Rs_d_int*vvs*dt
    else
       en2 = Gc2
    end if

    if ( ( en1>=Gc1 .or. en2>=Gc2 ) .and. (.not. coalescence) ) then
      fc=fn
      coalescence=.true.
      tri_coalescence=tri_s
    end if

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! internal 2D : S, ut, un, fn, Ut_man, Un_max, 0/1 coalescence, tri_coalescence, en, fc, gamma

    if (store) then

      if (nbdime == 2) then

        internal(2)  = u(2)
        internal(3)  = u(1)
        internal(4)  = betaf
        internal(5)  = Ut_max
        internal(6)  = Un_max
        if (coalescence) then
          internal(7) = 1.d0
        else
          internal(7) = 0.d0
        end if
        internal( 8) = tri_coalescence
        internal( 9) = en1
        internal(10) = en2
        internal(11) = fc
        internal(12) = gamma
        internal(13) = Rn_a
        internal(14) = Rt_a

        ! internal 3D : S, ut, un, us, fn, Ut_man, Un_max, Us_max, 0/1 coalescence, tri_coalescence, en, fc, gamma
      else
        internal(2)  = u(2)
        internal(3)  = u(1)
        internal(4)  = u(3)
        internal(5)  = betaf
        internal(6)  = Ut_max
        internal(7)  = Un_max
        internal(8)  = Us_max
        if (coalescence) then
          internal(9) = 1.d0
        else
          internal(9) = 0.d0
        end if
        internal(10) = tri_coalescence
        internal(11) = en1
        internal(12) = en2
        internal(13) = fc
        internal(14) = gamma
        internal(15) = Rn_a
        internal(16) = Rt_a
        internal(17) = Rs_a
      endif

    endif

  end subroutine tosi_czm_incre

  !!!------------------------------------------------------------------------
  subroutine clean_memory()
   implicit none
   integer(kind=4) :: i

   if( allocated(tact_behav) ) then
     do i = 1, size(tact_behav)
       if( associated(tact_behav(i)%param     ) ) deallocate(tact_behav(i)%param     )
       if( associated(tact_behav(i)%param_name) ) deallocate(tact_behav(i)%param_name)
     end do
     deallocate(tact_behav)
     deallocate(tact_pressure)
   end if

   !deallocate linked list ?

   if( allocated(friction_evol) ) deallocate(friction_evol)

   !FElast,nb_FEvalues
   !current_fric
   !FEVOLflag =.false.,FEVOL_write_flag = .false.

   !RandomMuIsActive =.false.
   !MuRatio = 0.0

   if( allocated(see) ) deallocate(see)
   !deallocate linked list ?

   !mac_beta_tol=1.d-12

   !itchatche=.false.

   !par defaut
   !g1overg2=1.D0
   !g1overg2=0.144
   !g1overg2=0.31

   !RNcap=1.D+20

   ! dilatancy parameters
   !dilatancy_fric=0.d0; dilatancy_height=0.d0

   !is_czm_with_initial_friction=.false.

   !global_adist=0.d0
  end subroutine clean_memory

end module tact_behaviour

