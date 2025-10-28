!==========================================================================
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

module interaction_laws_2D

  use parameters

  use utilities, only: logmes, faterr

  use overall, only : H, NSTEP

  use interaction_2D, only: inter_dim, &
                            T_interaction

  implicit none

  private

  !> Type holding a pointer on function working on an interaction
  type interaction_law_behaviours
    procedure(prep_law_proc) , pointer, nopass :: prep_func  => null()
    procedure(iter_law_proc) , pointer, nopass :: iter_func  => null()
    procedure(check_law_proc), pointer, nopass :: check_func => null()
    procedure(post_law_proc) , pointer, nopass :: post_func  => null()
  end type
  
  !> Abstract interface defining the prototype of function working on an interaction
  abstract interface
    subroutine prep_law_proc(inter, is_init)
      use interaction_2D, only : T_interaction
      implicit none
      type(T_interaction) :: inter
      logical, intent(in) :: is_init
    end subroutine
  end interface

  abstract interface
    subroutine iter_law_proc(inter, rlik, vlik, vlfik, Wrlik, WWik, rliki, vliki, stat)
      use interaction_2D, only : T_interaction, inter_dim
      implicit none
      type(T_interaction) :: inter
      real(kind=8), dimension(inter_dim), intent(in)    :: rlik
      real(kind=8), dimension(inter_dim), intent(in)    :: vlik
      real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
      real(kind=8), dimension(inter_dim), intent(in)    :: Wrlik
      real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
      real(kind=8), dimension(inter_dim), intent(out)   :: rliki
      real(kind=8), dimension(inter_dim), intent(inout) :: vliki
      character(len=5), intent(inout) :: stat
    end subroutine
  end interface

  abstract interface
    subroutine check_law_proc(inter, is_init)
      use interaction_2D, only : T_interaction
      implicit none
      type(T_interaction) :: inter
      logical, intent(in) :: is_init
    end subroutine
  end interface

  abstract interface
    subroutine post_law_proc(inter)
      use interaction_2D, only : T_interaction
      implicit none
      type(T_interaction) :: inter
    end subroutine
  end interface

  !> Defining an array of size 4xnb_laws, to store for each interaction law
  !> behaviour in solver (prep, iter, check, post)
  type(interaction_law_behaviours), dimension(total_interaction_law_number), public :: inter_laws

  real(kind=8), parameter :: Oneover2pi = 1.d0/(2.d0*PI_g)
  !real(kind=8), parameter :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)

  public init_inter_laws

  contains

  include 'inc_interaction_law.f90'
  
  subroutine init_inter_laws(inter_dim, diagonal_resolution)
    implicit none
    integer(kind=4), intent(in) :: inter_dim
    logical        , intent(in) :: diagonal_resolution
    !
    integer(kind=4)   :: i_law, i
    character(len=80) :: cout

    do i_law = 1, total_interaction_law_number
      select case( i_law )
      case( i_iqs_clb, i_iqs_ds_clb )
        inter_laws(i_law)%prep_func => iqs_clb_prep
        inter_laws(i_law)%iter_func => mu_clb_solver_2d
      !!case( i_iqs_map )
      !!  inter_laws(i_law)%prep_func => iqs_map_prep_2d
      !!case( i_iqs_clb_g0 )
      !!  inter_laws(i_law)%prep_func => iqs_clb_g0_prep_2d
      !!case( i_iqs_clb_rgr )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => iqs_clb_rgr_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => iqs_clb_rgr_prep
      !!  end if
      !!case( i_rst_clb, i_rst_ds_clb )
      !!  inter_laws(i_law)%prep_func => rst_clb_prep
      !!case( i_rst_wet_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => rst_wet_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => rst_wet_clb_prep
      !!  end if
      case( i_gap_sgr_clb, i_gap_sgr_ds_clb )
        inter_laws(i_law)%prep_func => gap_sgr_clb_prep
        inter_laws(i_law)%iter_func => mu_clb_solver_2d
      !!case( i_iqs_clb_nosldt )
      !!  inter_laws(i_law)%prep_func => iqs_clb_nosldt_prep_2d
      !!case( i_pregap_sgr_clb )
      !!  inter_laws(i_law)%prep_func => pregap_sgr_clb_prep_2d
      !!case( i_vel_sgr_clb )
      !!  inter_laws(i_law)%prep_func => vel_sgr_clb_prep
      !!case( i_gap_wet_ds_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => gap_wet_ds_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => gap_wet_ds_clb_prep
      !!  end if
      !!case( i_iqs_wet_ds_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => iqs_wet_ds_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => iqs_wet_ds_clb_prep
      !!  end if
      !!case( i_xqs_wet_ds_clb )
      !!  inter_laws(i_law)%prep_func => xqs_wet_clb_prep_2d
      !!case( i_iqs_mohr_ds_clb )
      !!  inter_laws(i_law)%prep_func => iqs_mohr_ds_clb_prep
      !!case( i_gap_mohr_ds_clb )
      !!  inter_laws(i_law)%prep_func => gap_mohr_ds_clb_prep_2d
      !!case( i_gap_cap_mohr_ds_clb )
      !!  inter_laws(i_law)%prep_func => gap_cap_mohr_ds_clb_prep_2d
      !!case( i_elastic_repell_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_repell_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_repell_clb_prep
      !!  end if
      !!case( i_critical_voigt_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => critical_voigt_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => critical_voigt_clb_prep
      !!  end if
      !!case( i_elastic_repell_wet_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_repell_wet_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_repell_wet_clb_prep
      !!  end if
      !!case( i_elastic_repell_mac_czm )
      !!  inter_laws(i_law)%prep_func => elastic_repell_mac_czm_prep_2d
      !!case( i_visco_elastic_repell_wet )
      !!  inter_laws(i_law)%prep_func => visco_elastic_repell_wet_prep_2d
      !!case( i_skf_grease )
      !!  inter_laws(i_law)%prep_func => skf_grease_prep_2d
      !!case( i_elastic_rod )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_prep_diag
      !!    inter_laws(i_law)%iter_func => rod_iter_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_prep
      !!    inter_laws(i_law)%iter_func => rod_iter_2d
      !!  end if
      !!case( i_elastic_wire )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_prep
      !!  end if
      !!case( i_brittle_elastic_wire )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => brittle_elastic_wire_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => brittle_elastic_wire_prep
      !!  end if
      !!case( i_voigt_rod )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep
      !!  end if
      !!case( i_voigt_wire )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep
      !!  end if
      !!case( i_tex_sol )
      !!  inter_laws(i_law)%prep_func => tex_sol_prep_2d
      !!case( i_rigid_wire, i_tex_sol_unilat )
      !!  inter_laws(i_law)%prep_func => rigid_wire_prep
      !!case( i_mac_czm, i_msmp_czm, i_mal_czm, i_mp_czm, i_mp3_czm, i_th_czm, i_rld_czm )
      !!  inter_laws(i_law)%prep_func => czm_prep_2d
      !!case( i_iqs_mac_czm, i_iqs_mal_czm )
      !!  inter_laws(i_law)%prep_func => iqs_czm_prep_2d
      !!case( i_iqs_wet_czm )
      !!  inter_laws(i_law)%prep_func => iqs_wet_czm_prep_2d
      !!case( i_postgap_iqs_mac_czm )
      !!  inter_laws(i_law)%prep_func => postgap_iqs_mac_czm_prep_2d
      !!case( i_gap_sgr_clb_wear )
      !!  inter_laws(i_law)%prep_func => gap_sgr_clb_wear_prep_2d
      !!case( i_iqs_bw_clb )
      !!  inter_laws(i_law)%prep_func => iqs_bw_clb_prep_2d
      !!case( i_iqs_sgr_clb_wear )
      !!  inter_laws(i_law)%prep_func => iqs_sgr_clb_wear_prep_2d
      !!case( i_broken_dof )
      !!  inter_laws(i_law)%prep_func => broken_dof_prep_2d
      !!case( i_perio_dof )
      !!  inter_laws(i_law)%prep_func => perio_dof_prep_2d
      !!case( i_coupled_dof, i_plastic_coupled_dof, i_tangential_coupled_dof, i_normal_coupled_dof )
      !!  !nothing to do
      case default
        write(cout,'(A,1x,A)') '[interaction_laws] Cannot init law', get_inter_law_name_from_id(i_law)
        call logmes(cout)
      end select
    end do

  end subroutine 
  
  !===============================================================================!
  ! SOLVER LIST : coulomb_friction                                                !
  !               coupled_dof                                                     !
  !               normal_coupled_dof                                              !
  !               tangential_coupled_dof                                          !
  !               plastic_coupled_dof                                             !
  !               wire                                                            !
  !               czm                                                             !
  !===============================================================================!

  !> \brief Coulomb's friction solver for 2D
  subroutine mu_clb_solver_2d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik
    !
    real(kind=8), dimension(inter_dim,inter_dim) ::WI
    real(kind=8), dimension(inter_dim) ::vvl, DF
    real(kind=8) :: Cforward,Cbackward,FFN

    WI(1,1) = WWik(2,2)
    WI(1,2) =-WWik(1,2)
    WI(2,1) =-WWik(2,1)
    WI(2,2) = WWik(1,1)

    DF(1:2) = matmul(WI(1:2,1:2),vlfik(1:2))
    FFN =-vlfik(2)/WWik(2,2)

    Cforward  = DF(1)+inter%fric*DF(2)
    Cbackward = DF(1)-inter%fric*DF(2)
    
    if (vlfik(2) >= 0.d0) then
      !no contact
      rliki(1:2)  = 0.d0
      statusik  = 'noctc'
    else if (vlfik(2) < 0.d0 .and. Cforward > 0.d0) then
      !sliding forward
      rliki(2)   = FFN/inter%forward
      rliki(1)   =-inter%fric*rliki(2)
      statusik = 'slifw'
    else if (vlfik(2) < 0.d0 .and. Cbackward < 0.d0) then
      !sliding backward      
      rliki(2)   = FFN/inter%backward
      rliki(1)   = inter%fric*rliki(2)
      statusik = 'slibw'
    else if (vlfik(2) < 0.d0 .and. Cforward <= 0.d0 .and. Cbackward >= 0.d0) then
      !sticking      
      rliki(1:2) =-DF(1:2)/inter%det
      statusik = 'stick'
    else
      rliki(1:2) = 0.d0
      statusik = 'vnish'                    
    end if
    
  end subroutine mu_clb_solver_2d

  !> \brief Coupled dof solver for 2D
  subroutine coupled_dof_solver_2d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    rliki(1) = (-WWik(2,2)*vlfik(1) + WWik(1,2)*vlfik(2) ) / inter%det
    rliki(2) = ( WWik(2,1)*vlfik(1) - WWik(1,1)*vlfik(2) ) / inter%det

    statusik = 'stick'

  end subroutine

  !> \brief Normal coupled dof solver for 2D
  subroutine normal_coupled_dof_solver_2d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    rliki(1) = 0.d0
    rliki(2) = -vlfik(2) / WWik(2,2)

    vliki(1) = vlfik(1) + WWik(1,2)*rliki(2)
    vliki(2) = 0.d0

    statusik = 'stick'

  end subroutine

  !> \brief Tangential coupled dof solver for 2D
  subroutine tangential_coupled_dof_solver_2d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    rliki(1) =-vlfik(1) / WWik(1,1)
    rliki(2) = 0.d0

    vliki(1) = 0.d0
    vliki(2) = vlfik(2) + WWik(2,1)*rliki(1)

    statusik = 'stick'

  end subroutine

  !> \brief Plastic coupled dof solver for 2D
  subroutine plastic_coupled_dof_solver_2d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,stat)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: stat

    rliki(1) = (-WWik(2,2)*vlfik(1) + WWik(1,2)*vlfik(2) ) / inter%det
    rliki(2) = ( WWik(2,1)*vlfik(1) - WWik(1,1)*vlfik(2) ) / inter%det

    vliki(:) = 0.d0
    if (rliki(2) < -inter%fric) then
      rliki(2) =-inter%fric
      rliki(1) =-( vlfik(1) + (WWik(1,2)*rliki(2)) ) / WWik(1,1)

      vliki(2) = vlfik(2) + (WWik(2,2)*rliki(2)) + (WWik(2,1)*rliki(1))
    endif

    stat = 'stick'

  end subroutine

  !> \brief CZM solver for 2D
  subroutine czm_solver_2d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,stat)
    use tact_behaviour, only : iter_czm, get_fric_czm
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: stat
    !
    real(kind=8) :: k_t, k_n, Hradh_t, Hradh_n, fricik, detJ, det
    real(kind=8) :: Att, Ant, Atn, Ann, Ttt, Tnt, Ttn, Tnn, ut, un
    logical      :: is_cohesive
    character(len=80) :: cout

    if (stat(1:1) == 'C') then 
       call iter_CZM(inter%lawnb,inter%internal,k_t,k_n,is_cohesive,Hradh_t,Hradh_n)
    else
       is_cohesive = .false.
       Hradh_t = 0.d0
       Hradh_n = 0.d0
    end if

    call get_fric_CZM(inter%lawnb,inter%internal,fricik)

    if (is_cohesive) then

      detJ = 1.d0 /(((1.d0 - (WWik(2,2)*k_n))*(1.d0 - (WWik(1,1)*k_t))) - ((WWik(1,2)*k_n)*(WWik(2,1)*k_t)))
      
      Ann = (1.d0 - (WWik(1,1)*k_t))*detJ
      Att = (1.d0 - (WWik(2,2)*k_n))*detJ
      Ant = WWik(2,1)*k_t*detJ 
      Atn = WWik(1,2)*k_n*detJ 
      
      ut = vlfik(1) + (WWik(1,1) * Hradh_t) !+ (WWtnik * Hradh_n)
      un = vlfik(2) + (WWik(2,1) * Hradh_t) !+ (WWnnik * Hradh_n)
      
      vlfik(1) = (Att * ut) + (Atn * un)
      vlfik(2) = (Ant * ut) + (Ann * un)
      
      Ttt = (Att * WWik(1,1)) + (Atn * WWik(2,1))
      Ttn = (Att * WWik(1,2)) + (Atn * WWik(2,2))
      Tnt = (Ant * WWik(1,1)) + (Ann * WWik(2,1))
      Tnn = (Ant * WWik(1,2)) + (Ann * WWik(2,2))
      
      WWik(1,1) = Ttt; WWik(1,2) = Ttn
      WWik(2,1) = Tnt; WWik(2,2) = Tnn
      
      inter%det = (WWik(1,1)*WWik(2,2))-(WWik(1,2)*WWik(2,1))
      
      inter%forward  = 1.d0 - (inter%fric*WWik(2,1)/WWik(2,2))
      inter%backward = 1.d0 + (inter%fric*WWik(2,1)/WWik(2,2))

    else

      ut = 0.d0
      un = 0.d0

    end if
    
    fricik = inter%fric
    det    = inter%det

    call mu_clb_solver_2d(inter, rlik, vlik, vlfik, Wrlik, WWik, rliki, vliki, stat)

    inter%fric = fricik
    inter%det  = det

    if (is_cohesive) then 
      ut = vlfik(1) + ((WWik(1,1)*rliki(1)) + (WWik(1,2)*rliki(2)))
      un = (vlfik(2) - inter%covfree(2)) + ((WWik(2,1)*rliki(1)) + (WWik(2,2)*rliki(2))) 

      Hradh_t = Hradh_t + (k_t * ut)
      Hradh_n = Hradh_n + (k_n * un)
      
      if ( Hradh_n > 1.d-03 ) then
        write(cout,'(A,D10.3,A,I0)') 'strange cohesive part ',Hradh_n,' for contact ',inter%icdan
        call logmes(cout)
      end if
      
      rliki(1) = rliki(1) + Hradh_t 
      rliki(2) = rliki(2) + Hradh_n 
      
      if (stat == 'noctc') then
        stat='Cstck'
      else if (stat == 'stick') then
        stat='Cstck'
      else if (stat == 'slibw') then
        stat='Cslbw'
      else if (stat == 'slifw') then
        stat='Cslfw'
      end if
    else
      if (stat(1:1) == 'W') then
        rliki(2) = rliki(2) + inter%corl(2)
        call revert_status_wet(stat)
      end if
    end if

  end subroutine czm_solver_2d

end module interaction_laws_2D

