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

!> Handler on 2D interactions for mechanics
module inter_meca_handler_2D

  use parameters

  use overall, only : max_internal_tact, &
                      faterr, H        , &
                      i_real_tactor    , &
                      i_recup_tactor


  use inter_meca_2D, only : T_interaction, &
                            T_verlet, &
                            T_con

  use CLALp, only : get_this_CLALp => get_this  , &
                    check_CLALp                 , &
                    get_nb_CLALp                , &
                    set_nb_CLALp                , &
                    injj_CLALp                  , &
                    prjj_CLALp                  , &
                    vitrad_CLALp                , &
                    nullify_reac_CLALp          , &
                    nullify_vlocy_CLALp         , &
                    get_length_CLALp            , &
                    redo_nb_adj_CLALp           , &
                    print_info_CLALp            , &
                    stock_rloc_CLALp            , &
                    recup_rloc_CLALp            , &
                    recup_rloc_by_position_CLALp, &
                    get_external_pressure_CLALp , &
                    get_an_tacty_CLALp          => get_an_tacty, &
                    get_verlet_tact_lawnb_CLALp => get_verlet_tact_lawnb
  

  use CLJCx, only : get_this_CLJCx => get_this, &
                    check_CLJCx               , &
                    get_nb_CLJCx              , &
                    set_nb_CLJCx              , &
                    injj_CLJCx                , &
                    prjj_CLJCx                , &
                    vitrad_CLJCx              , &
                    nullify_reac_CLJCx        , &
                    nullify_vlocy_CLJCx       , &
                    get_length_CLJCx          , &
                    redo_nb_adj_CLJCx         , &
                    print_info_CLJCx          , &
                    stock_rloc_CLJCx          , &
                    recup_rloc_CLJCx          , &
                    get_an_tacty_CLJCx          => get_an_tacty, &
                    get_verlet_tact_lawnb_CLJCx => get_verlet_tact_lawnb

  use DKALp, only : get_this_DKALp => get_this, &
                    check_DKALp               , &
                    get_nb_DKALP              , &
                    set_nb_DKALP              , &
                    injj_DKALp                , &
                    prjj_DKALp                , &
                    vitrad_DKALp              , &
                    nullify_reac_DKALp        , &
                    nullify_vlocy_DKALp       , &
                    get_length_DKALp          , &
                    redo_nb_adj_DKALp         , &
                    print_info_DKALp          , &
                    stock_rloc_DKALp          , &
                    recup_rloc_DKALp          , &
                    get_an_tacty_DKALp          => get_an_tacty, &
                    get_verlet_tact_lawnb_DKALp => get_verlet_tact_lawnb

  use DKDKL, only : get_this_DKDKL => get_this, &
                    check_DKDKL               , &
                    get_nb_DKDKL              , &
                    set_nb_DKDKL              , &
                    injj_DKDKL                , &
                    prjj_DKDKL                , &
                    vitrad_DKDKL              , &
                    nullify_reac_DKDKL        , &
                    nullify_vlocy_DKDKL       , &
                    get_length_DKDKL          , &
                    redo_nb_adj_DKDKL         , &
                    print_info_DKDKL          , &
                    stock_rloc_DKDKL          , &
                    recup_rloc_DKDKL          , &
                    get_an_tacty_DKDKL          => get_an_tacty, &
                    get_verlet_tact_lawnb_DKDKL => get_verlet_tact_lawnb

  use DKDKx, only : get_this_DKDKx => get_this, &
                    check_DKDKx               , &
                    get_nb_DKDKx              , &
                    set_nb_DKDKx              , &
                    injj_DKDKx                , &
                    prjj_DKDKx                , &
                    vitrad_DKDKx              , &
                    nullify_reac_DKDKx        , &
                    nullify_vlocy_DKDKx       , &
                    get_length_DKDKx          , &
                    update_cohe_DKDKx         , &
                    redo_nb_adj_DKDKx         , &
                    update_fric_DKDKx         , &
                    print_info_DKDKx          , &
                    stock_rloc_DKDKx          , &
                    recup_rloc_DKDKx          , &
                    get_an_tacty_DKDKx          => get_an_tacty, &
                    get_verlet_tact_lawnb_DKDKx => get_verlet_tact_lawnb


  use DKJCx, only : get_this_DKJCx => get_this, &
                    check_DKJCx               , &
                    get_nb_DKJCx              , &
                    set_nb_DKJCx              , &
                    injj_DKJCx                , &
                    prjj_DKJCx                , &
                    vitrad_DKJCx              , &
                    nullify_reac_DKJCx        , &
                    nullify_vlocy_DKJCx       , &
                    get_length_DKJCx          , &
                    update_cohe_DKJCx         , &
                    redo_nb_adj_DKJCx         , &
                    update_fric_DKJCx         , &
                    print_info_DKJCx          , &
                    stock_rloc_DKJCx          , &
                    recup_rloc_DKJCx          , &
                    get_an_tacty_DKJCx          => get_an_tacty, &
                    get_verlet_tact_lawnb_DKJCx => get_verlet_tact_lawnb

  use DKKDx, only : get_this_DKKDx => get_this, &
                    check_DKKDx               , &
                    get_nb_DKKDx              , &
                    set_nb_DKKDx              , &
                    injj_DKKDx                , &
                    prjj_DKKDx                , &
                    vitrad_DKKDx              , &
                    nullify_reac_DKKDx        , &
                    nullify_vlocy_DKKDx       , &
                    get_length_DKKDx          , &
                    update_cohe_DKKDx         , &
                    redo_nb_adj_DKKDx         , &
                    update_fric_DKKDx         , &
                    print_info_DKKDx          , &
                    stock_rloc_DKKDx          , &
                    recup_rloc_DKKDx          , &
                    get_an_tacty_DKKDx          => get_an_tacty, &
                    get_verlet_tact_lawnb_DKKDx => get_verlet_tact_lawnb


  use DKPLx, only : get_this_DKPLx => get_this, &
                    check_DKPLx               , &
                    get_nb_DKPLx              , &
                    set_nb_DKPLx              , &
                    injj_DKPLx                , &
                    prjj_DKPLx                , &
                    vitrad_DKPLx              , &
                    nullify_reac_DKPLx        , &
                    nullify_vlocy_DKPLx       , &
                    get_length_DKPLx          , &
                    redo_nb_adj_DKPLx         , &
                    print_info_DKPLx          , &
                    stock_rloc_DKPLx          , &
                    recup_rloc_DKPLx          , &
                    get_an_tacty_DKPLx          => get_an_tacty, &
                    get_verlet_tact_lawnb_DKPLx => get_verlet_tact_lawnb


  use P2P2L, only : get_this_P2P2L => get_this, &
                    check_P2P2L               , &
                    get_nb_P2P2L              , &
                    set_nb_P2P2L              , &
                    injj_P2P2L                , &
                    prjj_P2P2L                , &
                    vitrad_P2P2L              , &
                    nullify_reac_P2P2L        , &
                    nullify_vlocy_P2P2L       , &
                    get_length_P2P2L          , &
                    redo_nb_adj_P2P2L         , &
                    print_info_P2P2L          , &
                    stock_rloc_P2P2L          , &
                    recup_rloc_P2P2L          , &
                    get_an_tacty_P2P2L          => get_an_tacty, &
                    get_verlet_tact_lawnb_P2P2L => get_verlet_tact_lawnb

  use PLALp, only : get_this_PLALp => get_this, &
                    check_PLALp               , &
                    get_nb_PLALp              , &
                    set_nb_PLALp              , &
                    injj_PLALp                , &
                    prjj_PLALp                , &
                    vitrad_PLALp              , &
                    nullify_reac_PLALp        , &
                    nullify_vlocy_PLALp       , &
                    get_length_PLALp          , &
                    redo_nb_adj_PLALp         , &
                    print_info_PLALp          , &
                    stock_rloc_PLALp          , &
                    recup_rloc_PLALp          , &
                    get_an_tacty_PLALp          => get_an_tacty, &
                    get_verlet_tact_lawnb_PLALp => get_verlet_tact_lawnb

  use PLJCx, only : get_this_PLJCx => get_this , &
                    check_PLJCx                , &
                    get_nb_PLJCx               , &
                    set_nb_PLJCx               , &
                    injj_PLJCx                 , &
                    prjj_PLJCx                 , &
                    vitrad_PLJCx               , &
                    nullify_reac_PLJCx         , &
                    nullify_vlocy_PLJCx        , &
                    get_length_PLJCx           , &
                    redo_nb_adj_PLJCx          , &
                    update_fric_PLJCx          , &
                    print_info_PLJCx           , &
                    stock_rloc_PLJCx           , &
                    recup_rloc_PLJCx           , &
                    get_an_tacty_PLJCx          => get_an_tacty, &
                    get_verlet_tact_lawnb_PLJCx => get_verlet_tact_lawnb

  use PLPLx, only : get_this_PLPLx => get_this , &
                    check_PLPLx                , &
                    get_nb_PLPLx               , &
                    set_nb_PLPLx               , &
                    injj_PLPLx                 , &
                    prjj_PLPLx                 , &
                    vitrad_PLPLx               , &
                    nullify_reac_PLPLx         , &
                    nullify_vlocy_PLPLx        , &
                    update_fric_PLPLx          , &
                    get_length_PLPLx           , &
                    redo_nb_adj_PLPLx          , &
                    print_info_PLPLx           , &
                    stock_rloc_PLPLx           , &
                    recup_rloc_PLPLx           , &
                    recup_rloc_byposition_PLPLx, &
                    get_an_tacty_PLPLx          => get_an_tacty, &
                    get_verlet_tact_lawnb_PLPLx => get_verlet_tact_lawnb

  use PTPT2, only : get_this_PTPT2 => get_this, &
                    check_PTPT2               , &
                    get_nb_PTPT2              , &
                    set_nb_PTPT2              , &
                    injj_PTPT2                , &
                    prjj_PTPT2                , &
                    vitrad_PTPT2              , &
                    nullify_reac_PTPT2        , &
                    nullify_vlocy_PTPT2       , &
                    get_length_PTPT2          , &
                    redo_nb_adj_PTPT2         , &
                    print_info_PTPT2          , &
                    stock_rloc_PTPT2          , &
                    recup_rloc_PTPT2          , &
                    get_an_tacty_PTPT2          => get_an_tacty, &
                    get_verlet_tact_lawnb_PTPT2 => get_verlet_tact_lawnb

  use tact_behaviour, only : get_nb_internal

  implicit none

  private

  !> number of 2D interaction type
  integer, parameter               :: nb_inter_ids = 13
  !> list of 2D interaction type ids
  integer, dimension(nb_inter_ids) :: inter_ids = (/ i_dkdkx, i_dkkdx, &
                                                     i_dkplx, i_dkjcx, &
                                                     i_dkalp, i_dkdkl, &
                                                              i_plplx, &
                                                     i_pljcx, i_plalp, &
                                                     i_ptpt2, i_p2p2l, &
                                                     i_clalp, i_cljcx  /)

  !> the id of the interaction type currently referenced by 'this'
  integer :: active_id = 0
  !$omp threadprivate(active_id)

  !> reference on the 'this' array of an interaction sub-module
  type(T_interaction), dimension(:), pointer :: this_inter   => null()
  !$omp threadprivate(this_inter)

  !> reference on the 'verlet' array of an interaction sub-module
  type(T_verlet)     , dimension(:), pointer :: verlet_inter => null()
  !$omp threadprivate(verlet_inter)

  !> reference on the 'violation' array of an interaction sub-module
  real(kind=8)       , dimension(:), pointer :: violation_inter => null()
  !$omp threadprivate(violation_inter)

  !> reference on the 'con' array of an interaction sub-module
  type(T_con)                      , pointer :: con_inter => null()
  !$omp threadprivate(con_inter)
  
  !> active injj function
  procedure( injj_DKDKx )         , pointer :: injj_inter   => null()
  !$omp threadprivate(injj_inter)

  !> active prjj function
  procedure( prjj_DKDKx )         , pointer :: prjj_inter   => null()
  !$omp threadprivate(prjj_inter)

  !> active vitrad function
  procedure( vitrad_DKDKx )       , pointer :: vitrad_inter => null()
  !$omp threadprivate(vitrad_inter)

  !> active nullify_reac function
  procedure( nullify_reac_DKDKx ) , pointer :: nullify_reac_inter  => null()
  !$omp threadprivate(nullify_reac_inter)

  !> active nullify_vlocy function
  procedure( nullify_vlocy_DKDKx ), pointer :: nullify_vlocy_inter => null()
  !$omp threadprivate(nullify_vlocy_inter)

  !> active get_length function
  procedure( get_length_DKDKx )   , pointer :: get_length_inter    => null()
  !$omp threadprivate(get_length_inter)

  !> active get_an_tacty function
  procedure( get_an_tacty_DKDKx ), pointer :: get_an_tacty_inter => null()
  !$omp threadprivate(get_an_tacty_inter)

  !> active print_info function
  procedure( print_info_DKDKx )   , pointer :: print_info_inter    => null()
  !$omp threadprivate(print_info_inter)

  !> gerby active update_cohe function
  procedure( update_cohe_DKDKx )  , pointer :: update_cohe_inter   => null()
  !$omp threadprivate(update_cohe_inter)

  !> gerby active update_fric function
  procedure( update_fric_DKDKx )  , pointer :: update_fric_inter   => null()
  !$omp threadprivate(update_fric_inter)

  !> gerby active get_verlet_tact_lawnb function
  procedure( get_verlet_tact_lawnb_CLALp )  , pointer :: get_verlet_tact_lawnb_inter   => null()
  !$omp threadprivate(get_verlet_tact_lawnb_inter)
  
  !> gerby active get_verlet_tact_lawnb function
  procedure( get_external_pressure_CLALp )  , pointer :: get_external_pressure_inter   => null()
  !$omp threadprivate(get_external_pressure_inter)
  
  ! private functions for internal plumbery
  private select_this_

  ! api of the module

  ! api for nlgs and wrap
  public get_nb_inters, &
         get_nb_recup

  ! api for nlgs
  public set_loc        , &
         get_rloc       , &
         get_vloc       , &
         get_vlocBEGIN  , &
         get_local_frame, &
         get_internal   , &
         set_internal   , &
         set_internal_bv, &
         inter2ENT     , &
         get_tact_lawnb , &
         injj           , &
         prjj           , &
         vitrad         , &
         nullify_reac   , &
         nullify_vlocy  , &
         get_length     , &
         get_eff        , &
         set_violation  , &
         update_cohe    , &
         update_fric    , &
         print_info     , &
         get_external_pressure
  
  ! api for postpro
  public get_nb_verlets        , &
         get_verlet_size       , &
         get_verlet_adjsz      , &
         this2verlet           , &
         get_gaps              , & ! should be taken from verlet ?
         get_verlet_iantac     , & ! this one is for mp_solver
         get_verlet_lantac     , &
         get_verlet_icdbdy     , &
         get_verlet_ianbdy     , &
         get_verlet_icdbtac    , &
         get_verlet_ianbtac    , &
         get_verlet_gapTT      , &
         get_verlet_tact_lawnb , &
         get_verlet_local_frame, &
         get_verlet_rloc       , &
         get_verlet_vloc       , &
         get_verlet_internal   , &
         set_verlet_internal   , & ! this one is for mp_solver
         compute_rnod

  ! api for chipy
  public get_all,           &
         get_all_idata,     &
         get_all_internal , &
         get_all_tactlawnb, &
         get_idata        , &
         stock_rloc       , &
         recup_rloc       , &
         recup_rloc_by_pos

  ! api for DDM
  public get_icdbdy, &
         get_ianbdy

  ! api for i/o
  public get_ptr_one  , &
         set_nb_inters, &
         redo_nb_adj

contains

  !> Get the 'this' array of a sub-module in current context
  subroutine select_this_( inter_id )
    implicit none
    !> interaction id of the interaction to store in local 'this' array
    integer, intent(in) :: inter_id
    !
                                          !123456789012345678901234567890
    character(len=30), parameter :: IAM = 'inter_handler_2D::select_this_'

    ! paranoid
    ! if( inter_id < 1 .or inter_id > nb_inter_ids ) call faterr(IAM,'wrong index')

    if( inter_id == active_id ) return

    select case( inter_id )
    case( i_clalp )
      call get_this_CLALp(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_CLALp
      prjj_inter          => prjj_CLALp
      vitrad_inter        => vitrad_CLALp
      nullify_reac_inter  => nullify_reac_CLALp
      nullify_vlocy_inter => nullify_vlocy_CLALp
      get_length_inter    => get_length_CLALp
      print_info_inter    => print_info_CLALp
      get_an_tacty_inter  => get_an_tacty_CLALp
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_CLALp
      get_external_pressure_inter => get_external_pressure_CLALp
    case( i_cljcx )
      call get_this_CLJCx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_CLJCx
      prjj_inter          => prjj_CLJCx
      vitrad_inter        => vitrad_CLJCx
      nullify_reac_inter  => nullify_reac_CLJCx
      nullify_vlocy_inter => nullify_vlocy_CLJCx
      get_length_inter    => get_length_CLJCx
      print_info_inter    => print_info_CLJCx
      get_an_tacty_inter  => get_an_tacty_CLJCx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_CLJCx
    case( i_dkalp )
      call get_this_DKALp(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_DKALp
      prjj_inter          => prjj_DKALp
      vitrad_inter        => vitrad_DKALp
      nullify_reac_inter  => nullify_reac_DKALp
      nullify_vlocy_inter => nullify_vlocy_DKALp
      get_length_inter    => get_length_DKALp
      print_info_inter    => print_info_DKALp
      get_an_tacty_inter  => get_an_tacty_DKALp
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_DKALp
    case( i_dkdkl )
      call get_this_DKDKL(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_DKDKL
      prjj_inter          => prjj_DKDKL
      vitrad_inter        => vitrad_DKDKL
      nullify_reac_inter  => nullify_reac_DKDKL
      nullify_vlocy_inter => nullify_vlocy_DKDKL
      get_length_inter    => get_length_DKDKL
      print_info_inter    => print_info_DKDKL
      get_an_tacty_inter  => get_an_tacty_DKDKL
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_DKDKL
    case( i_dkdkx )
      call get_this_DKDKx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_DKDKx
      prjj_inter          => prjj_DKDKx
      vitrad_inter        => vitrad_DKDKx
      nullify_reac_inter  => nullify_reac_DKDKx
      nullify_vlocy_inter => nullify_vlocy_DKDKx
      get_length_inter    => get_length_DKDKx
      print_info_inter    => print_info_DKDKx
      get_an_tacty_inter  => get_an_tacty_DKDKx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_DKDKx
      ! beurk
      update_cohe_inter   => update_cohe_DKDKx
      update_fric_inter   => update_fric_DKDKx
    case( i_dkjcx )
      call get_this_DKJCx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_DKJCx
      prjj_inter          => prjj_DKJCx
      vitrad_inter        => vitrad_DKJCx
      nullify_reac_inter  => nullify_reac_DKJCx
      nullify_vlocy_inter => nullify_vlocy_DKJCx
      get_length_inter    => get_length_DKJCx
      print_info_inter    => print_info_DKJCx
      get_an_tacty_inter  => get_an_tacty_DKJCx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_DKJCx
      ! beurk
      update_cohe_inter   => update_cohe_DKJCx
      update_fric_inter   => update_fric_DKJCx
    case( i_dkkdx )
      call get_this_DKKDx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_DKKDx
      prjj_inter          => prjj_DKKDx
      vitrad_inter        => vitrad_DKKDx
      nullify_reac_inter  => nullify_reac_DKKDx
      nullify_vlocy_inter => nullify_vlocy_DKKDx
      get_length_inter    => get_length_DKKDx
      print_info_inter    => print_info_DKKDx
      get_an_tacty_inter  => get_an_tacty_DKKDx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_DKKDx
      ! beurk
      update_cohe_inter   => update_cohe_DKKDx
      update_fric_inter   => update_fric_DKKDx
    case( i_dkplx )
      call get_this_DKPLx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_DKPLx
      prjj_inter          => prjj_DKPLx
      vitrad_inter        => vitrad_DKPLx
      nullify_reac_inter  => nullify_reac_DKPLx
      nullify_vlocy_inter => nullify_vlocy_DKPLx
      get_length_inter    => get_length_DKPLx
      print_info_inter    => print_info_DKPLx
      get_an_tacty_inter  => get_an_tacty_DKPLx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_DKPLx
    case( i_p2p2l )
      call get_this_P2P2L(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_P2P2L
      prjj_inter          => prjj_P2P2L
      vitrad_inter        => vitrad_P2P2L
      nullify_reac_inter  => nullify_reac_P2P2L
      nullify_vlocy_inter => nullify_vlocy_P2P2L
      get_length_inter    => get_length_P2P2L
      print_info_inter    => print_info_P2P2L
      get_an_tacty_inter  => get_an_tacty_P2P2L
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_P2P2L
    case( i_plalp )
      call get_this_PLALp(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PLALp
      prjj_inter          => prjj_PLALp
      vitrad_inter        => vitrad_PLALp
      nullify_reac_inter  => nullify_reac_PLALp
      nullify_vlocy_inter => nullify_vlocy_PLALp
      get_length_inter    => get_length_PLALp
      print_info_inter    => print_info_PLALp
      get_an_tacty_inter  => get_an_tacty_PLALp
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PLALp
    case( i_pljcx )
      call get_this_PLJCx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PLJCx
      prjj_inter          => prjj_PLJCx
      vitrad_inter        => vitrad_PLJCx
      nullify_reac_inter  => nullify_reac_PLJCx
      nullify_vlocy_inter => nullify_vlocy_PLJCx
      get_length_inter    => get_length_PLJCx
      print_info_inter    => print_info_PLJCx
      get_an_tacty_inter  => get_an_tacty_PLJCx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PLJCx
      !
      update_fric_inter   => update_fric_PLJCx
    case( i_plplx )
      call get_this_PLPLx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PLPLx
      prjj_inter          => prjj_PLPLx
      vitrad_inter        => vitrad_PLPLx
      nullify_reac_inter  => nullify_reac_PLPLx
      nullify_vlocy_inter => nullify_vlocy_PLPLx
      get_length_inter    => get_length_PLPLx
      print_info_inter    => print_info_PLPLx
      get_an_tacty_inter  => get_an_tacty_PLPLx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PLPLx
      !
      update_fric_inter   => update_fric_PLPLx
    case( i_ptpt2 )
      call get_this_PTPT2(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PTPT2
      prjj_inter          => prjj_PTPT2
      vitrad_inter        => vitrad_PTPT2
      nullify_reac_inter  => nullify_reac_PTPT2
      nullify_vlocy_inter => nullify_vlocy_PTPT2
      get_length_inter    => get_length_PTPT2
      print_info_inter    => print_info_PTPT2
      get_an_tacty_inter  => get_an_tacty_PTPT2
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PTPT2
    case default
      call faterr( IAM, 'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine select_this_

  !> Get the number of interactions of a selected type
  integer function get_nb_inters( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_dkdkx )
      get_nb_inters = get_nb_DKDKx(i_real_tactor)
    case( i_dkkdx )
      get_nb_inters = get_nb_DKKDx(i_real_tactor)
    case( i_dkplx )
      get_nb_inters = get_nb_DKPLx(i_real_tactor)
    case( i_dkjcx )
      get_nb_inters = get_nb_DKJCx(i_real_tactor)
    case( i_dkalp )
      get_nb_inters = get_nb_DKALp(i_real_tactor)
    case( i_dkdkl )
      get_nb_inters = get_nb_DKDKL(i_real_tactor)
    case( i_plplx )
      get_nb_inters = get_nb_PLPLx(i_real_tactor)
    case( i_pljcx )
      get_nb_inters = get_nb_PLJCx(i_real_tactor)
    case( i_plalp )
      get_nb_inters = get_nb_PLALp(i_real_tactor)
    case( i_ptpt2 )
      get_nb_inters = get_nb_PTPT2(i_real_tactor)
    case( i_p2p2l )
      get_nb_inters = get_nb_P2P2L(i_real_tactor)
    case( i_clalp )
      get_nb_inters = get_nb_CLALp(i_real_tactor)
    case( i_cljcx )
      get_nb_inters = get_nb_CLJCx(i_real_tactor)
    case default
      call faterr( 'inter_handler_2D::get_nb_inters', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end function get_nb_inters

  !> Get the number of interactions of a selected type
  subroutine set_nb_inters( inter_id, nb_inter )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id
    !> number of interactions
    integer, intent(in) :: nb_inter

    select case( inter_id )
    case( i_dkdkx )
      call set_nb_DKDKx(nb_inter)
    case( i_dkkdx )
      call set_nb_DKKDx(nb_inter)
    case( i_dkplx )
      call set_nb_DKPLx(nb_inter)
    case( i_dkjcx )
      call set_nb_DKJCx(nb_inter)
    case( i_dkalp )
      call set_nb_DKALp(nb_inter)
    case( i_dkdkl )
      call set_nb_DKDKL(nb_inter)
    case( i_plplx )
      call set_nb_PLPLx(nb_inter)
    case( i_pljcx )
      call set_nb_PLJCx(nb_inter)
    case( i_plalp )
      call set_nb_PLALp(nb_inter)
    case( i_ptpt2 )
      call set_nb_PTPT2(nb_inter)
    case( i_p2p2l )
      call set_nb_P2P2L(nb_inter)
    case( i_clalp )
      call set_nb_CLALp(nb_inter)
    case( i_cljcx )
      call set_nb_CLJCx(nb_inter)
    case default
      call faterr( 'inter_handler_2D::set_nb_inters', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine set_nb_inters

  !> Get the number of recup interactions of a selected type
  integer function get_nb_recup( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_dkdkx )
      get_nb_recup = get_nb_DKDKx(i_recup_tactor)
    case( i_dkkdx )
      get_nb_recup = get_nb_DKKDx(i_recup_tactor)
    case( i_dkplx )
      get_nb_recup = get_nb_DKPLx(i_recup_tactor)
    case( i_dkjcx )
      get_nb_recup = get_nb_DKJCx(i_recup_tactor)
    case( i_dkalp )
      get_nb_recup = get_nb_DKALp(i_recup_tactor)
    case( i_dkdkl )
      get_nb_recup = get_nb_DKDKL(i_recup_tactor)
    case( i_plplx )
      get_nb_recup = get_nb_PLPLx(i_recup_tactor)
    case( i_pljcx )
      get_nb_recup = get_nb_PLJCx(i_recup_tactor)
    case( i_plalp )
      get_nb_recup = get_nb_PLALp(i_recup_tactor)
    case( i_ptpt2 )
      get_nb_recup = get_nb_PTPT2(i_recup_tactor)
    case( i_p2p2l )
      get_nb_recup = get_nb_P2P2L(i_recup_tactor)
    case( i_clalp )
      get_nb_recup = get_nb_CLALp(i_recup_tactor)
    case( i_cljcx )
      get_nb_recup = get_nb_CLJCx(i_recup_tactor)
    case default
      call faterr( 'inter_handler_2D::get_nb_recup', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end function get_nb_recup

  !> Redo the adjacence map of a selected type
  subroutine redo_nb_adj( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_dkdkx )
      call redo_nb_adj_DKDKx()
    case( i_dkkdx )
      call redo_nb_adj_DKKDx()
    case( i_dkplx )
      call redo_nb_adj_DKPLx()
    case( i_dkjcx )
      call redo_nb_adj_DKJCx()
    case( i_dkalp )
      call redo_nb_adj_DKALp()
    case( i_dkdkl )
      call redo_nb_adj_DKDKL()
    case( i_plplx )
      call redo_nb_adj_PLPLx()
    case( i_pljcx )
      call redo_nb_adj_PLJCx()
    case( i_plalp )
      call redo_nb_adj_PLALp()
    case( i_ptpt2 )
      call redo_nb_adj_PTPT2()
    case( i_p2p2l )
      call redo_nb_adj_P2P2L()
    case( i_clalp )
      call redo_nb_adj_CLALp()
    case( i_cljcx )
      call redo_nb_adj_CLJCx()
    case default
      call faterr( 'inter_handler_2D::redo_nb_adj', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine redo_nb_adj

  !> Stock from this to verlet
  subroutine stock_rloc( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_dkdkx )
      if( .not. check_DKDKx() ) return
      call stock_rloc_DKDKx()
    case( i_dkkdx )
      if( .not. check_DKKDx() ) return
      call stock_rloc_DKKDx()
    case( i_dkplx )
      if( .not. check_DKPLx() ) return
      call stock_rloc_DKPLx()
    case( i_dkjcx )
      if( .not. check_DKJCx() ) return
      call stock_rloc_DKJCx()
    case( i_dkalp )
      if( .not. check_DKALp() ) return
      call stock_rloc_DKALp()
    case( i_dkdkl )
      if( .not. check_DKDKL() ) return
      call stock_rloc_DKDKL()
    case( i_plplx )
      if( .not. check_PLPLx() ) return
      call stock_rloc_PLPLx()
    case( i_pljcx )
      if( .not. check_PLJCx() ) return
      call stock_rloc_PLJCx()
    case( i_plalp )
      if( .not. check_PLALp() ) return
      call stock_rloc_PLALp()
    case( i_ptpt2 )
      if( .not. check_PTPT2() ) return
      call stock_rloc_PTPT2()
    case( i_p2p2l )
      if( .not. check_P2P2L() ) return
      call stock_rloc_P2P2L()
    case( i_clalp )
      if( .not. check_CLALp() ) return
      call stock_rloc_CLALp()
    case( i_cljcx )
      if( .not. check_CLJCx() ) return
      call stock_rloc_CLJCx()
    case default
      call faterr( 'inter_handler_2D::stock_rloc', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine stock_rloc

  !> Recup from verlet to this
  subroutine recup_rloc( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_dkdkx )
      if( .not. check_DKDKx() ) return
      call recup_rloc_DKDKx()
    case( i_dkkdx )
      if( .not. check_DKKDx() ) return
      call recup_rloc_DKKDx()
    case( i_dkplx )
      if( .not. check_DKPLx() ) return
      call recup_rloc_DKPLx()
    case( i_dkjcx )
      if( .not. check_DKJCx() ) return
      call recup_rloc_DKJCx()
    case( i_dkalp )
      if( .not. check_DKALp() ) return
      call recup_rloc_DKALp()
    case( i_dkdkl )
      if( .not. check_DKDKL() ) return
      call recup_rloc_DKDKL()
    case( i_plplx )
      if( .not. check_PLPLx() ) return
      call recup_rloc_PLPLx()
    case( i_pljcx )
      if( .not. check_PLJCx() ) return
      call recup_rloc_PLJCx()
    case( i_plalp )
      if( .not. check_PLALp() ) return
      call recup_rloc_PLALp()
    case( i_ptpt2 )
      if( .not. check_PTPT2() ) return
      call recup_rloc_PTPT2()
    case( i_p2p2l )
      if( .not. check_P2P2L() ) return
      call recup_rloc_P2P2L()
    case( i_clalp )
      if( .not. check_CLALp() ) return
      call recup_rloc_CLALp()
    case( i_cljcx )
      if( .not. check_CLJCx() ) return
      call recup_rloc_CLJCx()
    case default
      call faterr( 'inter_handler_2D::recup_rloc', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine recup_rloc

  !> Recup from this to verlet by position
  subroutine recup_rloc_by_pos( inter_id, rtol )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id
    !> tolerance for recup
    real(kind=8), intent(in) :: rtol

    select case( inter_id )
    case( i_plplx )
      if( .not. check_PLPLx() ) return
      call recup_rloc_byposition_PLPLx(rtol)
    case( i_clalp )
      if( .not. check_CLALp() ) return
      call recup_rloc_by_position_CLALp(rtol)
    case default
      call faterr( 'inter_handler_2D::recup_rloc_by_pos', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine recup_rloc_by_pos

  !> Put back computed value of local reaction, velocity and gap in this array
  subroutine set_loc(id_inter, icdan, status, vlt, vln, rlt, rln, gap)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> contact status
    integer     , intent(in) :: status
    !> tangential local velocity
    real(kind=8), intent(in) :: vlt
    !> normal local velocity
    real(kind=8), intent(in) :: vln
    !> tangential local reaction
    real(kind=8), intent(in) :: rlt
    !> normal local reaction
    real(kind=8), intent(in) :: rln
    !> gap
    real(kind=8), intent(in) :: gap

    call select_this_(id_inter)

    this_inter(icdan)%status = status
    this_inter(icdan)%vlt    = vlt
    this_inter(icdan)%vln    = vln
    this_inter(icdan)%rlt    = rlt
    this_inter(icdan)%rln    = rln
    this_inter(icdan)%gapTT  = gap

  end subroutine set_loc

  !> Get local reaction and status
  subroutine get_rloc(id_inter, icdan, rlt, rln, status)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> tangential local reaction
    real(kind=8), intent(out) :: rlt
    !> normal local reaction
    real(kind=8), intent(out) :: rln
    !> contact status
    integer     , intent(out) :: status

    call select_this_(id_inter)

    rlt    = this_inter(icdan)%rlt
    rln    = this_inter(icdan)%rln
    status = this_inter(icdan)%status

  end subroutine get_rloc

  !> Get local velocity status 
  subroutine get_vloc(id_inter, icdan, vlt, vln)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> tangential local velocity
    real(kind=8), intent(out) :: vlt
    !> normal local velocity
    real(kind=8), intent(out) :: vln

    call select_this_(id_inter)

    vlt = this_inter(icdan)%vlt
    vln = this_inter(icdan)%vln

  end subroutine get_vloc

  !> Get local velocity status and gap at the beginning of the step
  subroutine get_vlocBEGIN(id_inter, icdan, vltBEGIN, vlnBEGIN, gapBEGIN, statusBEGIN)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> tangential local velocity
    real(kind=8), intent(out) :: vltBEGIN
    !> normal local velocity
    real(kind=8), intent(out) :: vlnBEGIN
    !> gap
    real(kind=8), intent(out) :: gapBEGIN
    !> contact status
    integer     , intent(out) :: statusBEGIN

    call select_this_(id_inter)

    vltBEGIN    = this_inter(icdan)%vltBEGIN
    vlnBEGIN    = this_inter(icdan)%vlnBEGIN
    gapBEGIN    = this_inter(icdan)%gapTTBEGIN
    statusBEGIN = this_inter(icdan)%statusBEGIN

  end subroutine get_vlocBEGIN

  !> Get contact local frame
  subroutine get_local_frame(id_inter, icdan, loc)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> contact locus
    real(kind=8), dimension(6), intent(out) :: loc

    call select_this_(id_inter)

    loc(1:2) = this_inter(icdan)%coor(1:2)
    loc(3:4) = this_inter(icdan)%tuc(1:2)
    loc(5:6) = this_inter(icdan)%nuc(1:2)

  end subroutine get_local_frame

  !> Put some reaction at second member of an algebraic system
  subroutine injj(id_inter, icdan, rtik, rnik, storage)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> tangential local reaction
    real(kind=8), intent(in) :: rtik
    !> normal local reaction
    real(kind=8), intent(in) :: rnik
    !> where to put the reaction
    integer     , intent(in) :: storage

    call select_this_(id_inter)
    call injj_inter(icdan, rtik, rnik, storage)

  end subroutine injj

  !> Get some results from an algebraic system resolution
  subroutine prjj(id_inter, icdan, vtik, vnik, storage)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> tangential local velocity
    real(kind=8), intent(out) :: vtik
    !> normal local velocity
    real(kind=8), intent(out) :: vnik
    !> where to get the velocity
    integer     , intent(in)  :: storage

    call select_this_(id_inter)
    call prjj_inter(icdan, vtik, vnik, storage)

  end subroutine prjj

  !> Get the length associated to the contact
  function get_length(id_inter, icdan)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !
    real(kind=8) :: get_length

    call select_this_(id_inter)
    get_length = get_length_inter(icdan)

  end function get_length

  !> Get the local frame of a verlet interaction
  subroutine get_verlet_local_frame(id_inter, icdtac, iadj, ptc, tuc, nuc)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> contact point locus
    real(kind=8), dimension(2), intent(out) :: ptc
    !> tangent component of local frame
    real(kind=8), dimension(2), intent(out) :: tuc
    !> normal component of local frame
    real(kind=8), dimension(2), intent(out) :: nuc
    call select_this_(id_inter)
    ptc = verlet_inter(icdtac)%coor(:, iadj)
    tuc = verlet_inter(icdtac)%tuc(:, iadj)
    nuc = verlet_inter(icdtac)%nuc(:, iadj)
    
  end subroutine get_verlet_local_frame

  !> Get the local reaction of a verlet interaction
  subroutine get_verlet_rloc(id_inter, icdtac, iadj, i_status, rlt, rln)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> contact status
    integer     , intent(out) :: i_status
    !> tangent reaction
    real(kind=8), intent(out) :: rlt
    !> normal reaction
    real(kind=8), intent(out) :: rln

    call select_this_(id_inter)

    i_status = verlet_inter(icdtac)%status(iadj)

    rlt = verlet_inter(icdtac)%rlt(iadj)
    rln = verlet_inter(icdtac)%rln(iadj)

  end subroutine get_verlet_rloc

  !> Get the local velocity of a verlet interaction
  subroutine get_verlet_vloc(id_inter, icdtac, iadj, vlt, vln)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> tangent velocity
    real(kind=8), intent(out) :: vlt
    !> normal velocity
    real(kind=8), intent(out) :: vln

    call select_this_(id_inter)

    vlt = verlet_inter(icdtac)%vlt(iadj)
    vln = verlet_inter(icdtac)%vln(iadj)

  end subroutine get_verlet_vloc

  !> \brief Get all the verlet interaction of a submodule
  !> Allocate memory and copy
  function get_all( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> coor(2),t(2),n(2),gap,ft,fn
    real(kind=8), dimension(:,:), pointer :: get_all
    !
    integer :: nb_cd, icdtac, iadj, icdan
  
    get_all => null()
  
    call select_this_(inter_id)

    if( .not. associated(verlet_inter) ) return

    nb_cd = size(verlet_inter)

    if( nb_cd <= 0 ) return
  
    ! allocate output array
    icdan = get_nb_verlets( inter_id )
    if( icdan <= 0 ) return

    allocate( get_all(11,icdan) )

    ! filling output array
    do icdtac = 1,nb_cd

      if (verlet_inter(icdtac)%adjsz == 0) cycle
  
      do iadj = 1, verlet_inter(icdtac)%adjsz

        icdan = verlet_inter(icdtac)%icdan(iadj)
         
        get_all(1:2,icdan) = verlet_inter(icdtac)%coor(:,iadj)
        !get_all(3:4,icdan) = verlet_inter(icdtac)%tuc(:,iadj)
        get_all(3  ,icdan) = verlet_inter(icdtac)%nuc(2,iadj)
        get_all(4  ,icdan) =-verlet_inter(icdtac)%nuc(1,iadj)
        get_all(5:6,icdan) = verlet_inter(icdtac)%nuc(:,iadj)
        get_all( 7 ,icdan) = verlet_inter(icdtac)%rlt(iadj)
        get_all( 8 ,icdan) = verlet_inter(icdtac)%rln(iadj)
        get_all( 9 ,icdan) = verlet_inter(icdtac)%vlt(iadj)
        get_all(10 ,icdan) = verlet_inter(icdtac)%vln(iadj)
        get_all(11 ,icdan) = verlet_inter(icdtac)%gaptt(iadj)

      end do

    end do
  
  end function get_all

  !> Print interaction
  subroutine print_info(id_inter, icdan)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan

    call select_this_(id_inter)
    call print_info_inter(icdan)

  end subroutine print_info

  !> Update cohesion value
  !> \TODO: check if still needed (MR and VHN thingy)
  subroutine update_cohe(id_inter, icdan, cohe)
    implicit none
    !> interaction type id
    integer     , intent(in)    :: id_inter
    !> interaction index
    integer     , intent(in)    :: icdan
    !> cohesion value
    real(kind=8), intent(inout) :: cohe

    call select_this_(id_inter)
    call update_cohe_inter(icdan, cohe)

  end subroutine update_cohe

  !> Update friction value
  subroutine update_fric(id_inter, icdan, fric)
    implicit none
    !> interaction type id
    integer     , intent(in)    :: id_inter
    !> interaction index
    integer     , intent(in)    :: icdan
    !> cohesion value
    real(kind=8), intent(inout) :: fric

    call select_this_(id_inter)
    call update_fric_inter(icdan, fric)

  end subroutine update_fric


  subroutine get_external_pressure(id_inter,icdan,pext)
    implicit none
    !> interaction type id
    integer     , intent(in)    :: id_inter
    !> interaction index
    integer     , intent(in)    :: icdan
    !> cohesion value
    real(kind=8), intent(inout) :: pext

    call select_this_(id_inter)
    call get_external_pressure_inter(icdan, pext)

  end subroutine get_external_pressure

  subroutine compute_rnod()
    implicit none
    integer :: i_inter, inter_id, icdan

    ! iIreac is a parameter

    ! first nullify reac for ALL interactions
    do i_inter = 1, nb_inter_ids
      inter_id = inter_ids(i_inter)
      do icdan = 1, get_nb_inters( inter_id )
        call nullify_reac(inter_id, icdan, iIreac)
      end do
    end do

    ! the cumulated sum of reactions on each bodies
    do i_inter = 1, nb_inter_ids
      inter_id = inter_ids(i_inter)
      call select_this_(inter_id)
      do icdan = 1, get_nb_inters( inter_id )
        call injj_inter(icdan, this_inter(icdan)%rlt, this_inter(icdan)%rln, iIreac)
      end do
    end do

  end subroutine compute_rnod


  include 'inter_handler_common.f90'
  
  ! defines the following functions/subroutines :
  !integer function get_nb_verlets(id_inter)
  !subroutine get_internal(id_inter, icdan, internals)
  !subroutine get_gaps(id_inter, icdan, gapTT, gapTTBegin)
  !subroutine set_internal(id_inter, icdan, internals)
  !subroutine set_internal_bv(id_inter, icdan, idx, val)
  !subroutine inter2ENT(id_inter, icdan, icdent, ianent )
  !function get_tact_lawnb(id_inter, icdan)
  !function get_all_tactlawnb(id_inter)  
  !function get_icdbdy(id_inter, icdan)
  !function get_ianbdy(id_inter, icdan)
  !subroutine get_idata(id_inter, icdan, idata)
  !subroutine vitrad(id_inter, icdan, storage, need_full_V)
  !subroutine nullify_reac(id_inter, icdan, storage)
  !subroutine nullify_vlocy(id_inter, icdan, storage)
  !subroutine set_violation(id_inter, icdan, vlton)
  !integer function get_verlet_size(id_inter)
  !integer function get_verlet_adjsz(id_inter, icdtac)
  !subroutine this2verlet(id_inter, icdan, icdtac, iadj)
  !integer function get_verlet_iantac(id_inter, icdtac, iadj)
  !integer function get_verlet_icdbdy(id_inter, icdtac, iadj)
  !integer function get_verlet_ianbdy(id_inter, icdtac, iadj)
  !integer function get_verlet_icdbtac(id_inter, icdtac, iadj)
  !integer function get_verlet_ianbtac(id_inter, icdtac, iadj)
  !real(kind=8) function get_verlet_gapTT(id_inter, icdtac, iadj)
  !integer function get_verlet_tact_lawnb(id_inter, icdtac, iadj)
  !subroutine get_verlet_internal(id_inter, icdtac, iadj, internal)
  !subroutine set_verlet_internal(id_inter, icdtac, iadj, internal)
  !function get_all_internal( inter_id )
  !function get_all_idata( inter_id )
  !subroutine get_eff(id_inter, icdan, meff, reff)
  !function get_ptr_one(id_inter, icdan)



end module inter_meca_handler_2D
