# 1 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Core/contact_3D/mod_inter_handler_3D.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Core/contact_3D/mod_inter_handler_3D.f90"
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

!> Handler on 3D interactions for mechanics
module inter_meca_handler_3D

  use parameters

  use overall, only : max_internal_tact, &
                      faterr, H        , &
                      i_real_tactor    , &
                      i_recup_tactor


  use inter_meca_3D, only : T_interaction, &
                            T_verlet, &
                            T_con

  use CDCDx, only : get_this_CDCDx => get_this, &
                    check_CDCDx               , &
                    get_nb_CDCDx              , &
                    set_nb_CDCDx              , &
                    injj_CDCDx                , &
                    prjj_CDCDx                , &
                    vitrad_CDCDx              , &
                    nullify_reac_CDCDx        , &
                    nullify_vlocy_CDCDx       , &
                    get_surf_CDCDx            , &
                    redo_nb_adj_CDCDx         , &
                    stock_rloc_CDCDx          , &
                    recup_rloc_CDCDx          , &
                    get_an_tacty_CDCDx          => get_an_tacty, &
                    get_verlet_tact_lawnb_CDCDx => get_verlet_tact_lawnb

  use CDPLx, only : get_this_CDPLx => get_this, &
                    check_CDPLx               , &
                    get_nb_CDPLx              , &
                    set_nb_CDPLx              , &
                    injj_CDPLx                , &
                    prjj_CDPLx                , &
                    vitrad_CDPLx              , &
                    nullify_reac_CDPLx        , &
                    nullify_vlocy_CDPLx       , &
                    get_surf_CDPLx            , &
                    redo_nb_adj_CDPLx         , &
                    stock_rloc_CDPLx          , &
                    recup_rloc_CDPLx          , &
                    get_an_tacty_CDPLx          => get_an_tacty, &
                    get_verlet_tact_lawnb_CDPLx => get_verlet_tact_lawnb

  use CSASp, only : get_this_CSASx => get_this  , &
                    check_CSASx                 , &
                    get_nb_CSASx                , &
                    set_nb_CSASx                , &
                    injj_CSASx                  , &
                    prjj_CSASx                  , &
                    vitrad_CSASx                , &
                    nullify_reac_CSASx          , &
                    nullify_vlocy_CSASx         , &
                    get_surf_CSASx              , &
                    redo_nb_adj_CSASx           , &
                    stock_rloc_CSASx            , &
                    recup_rloc_CSASx            , &
                    recup_rloc_by_position_CSASx, &
                    get_external_pressure_CSASx , &
                    get_an_tacty_CSASx          => get_an_tacty, &
                    get_verlet_tact_lawnb_CSASx => get_verlet_tact_lawnb

  use CSPRx, only : get_this_CSPRx => get_this  , &
                    check_CSPRx                 , &
                    get_nb_CSPRx                , &
                    set_nb_CSPRx                , &
                    injj_CSPRx                  , &
                    prjj_CSPRx                  , &
                    vitrad_CSPRx                , &
                    nullify_reac_CSPRx          , &
                    nullify_vlocy_CSPRx         , &
                    get_surf_CSPRx              , &
                    redo_nb_adj_CSPRx           , &
                    stock_rloc_CSPRx            , &
                    recup_rloc_CSPRx            , &
                    recup_rloc_by_position_CSPRx, &
                    get_an_tacty_CSPRx          => get_an_tacty, &
                    get_verlet_tact_lawnb_CSPRx => get_verlet_tact_lawnb

  use PRASp, only : get_this_PRASx => get_this, &
                    check_PRASp               , &
                    get_nb_PRASx              , &
                    set_nb_PRASx              , &
                    injj_PRASx                , &
                    prjj_PRASx                , &
                    vitrad_PRASx              , &
                    nullify_reac_PRASx        , &
                    nullify_vlocy_PRASx       , &
                    get_surf_PRASx            , &
                    redo_nb_adj_PRASx         , &
                    stock_rloc_PRASp          , &
                    recup_rloc_PRASp          , &
                    get_an_tacty_PRASx          => get_an_tacty, &
                    get_verlet_tact_lawnb_PRASx => get_verlet_tact_lawnb

  use PRPLx, only : get_this_PRPLx => get_this, &
                    check_PRPLx               , &
                    get_nb_PRPLx              , &
                    set_nb_PRPLx              , &
                    injj_PRPLx                , &
                    prjj_PRPLx                , &
                    vitrad_PRPLx              , &
                    nullify_reac_PRPLx        , &
                    nullify_vlocy_PRPLx       , &
                    get_surf_PRPLx            , &
                    redo_nb_adj_PRPLx         , &
                    stock_rloc_PRPLx          , &
                    recup_rloc_PRPLx          , &
                    get_an_tacty_PRPLx          => get_an_tacty, &
                    get_verlet_tact_lawnb_PRPLx => get_verlet_tact_lawnb

  use PRPRx, only : get_this_PRPRx => get_this , &
                    check_PRPRx               , &
                    get_nb_PRPRx               , &
                    set_nb_PRPRx               , &
                    injj_PRPRx                 , &
                    prjj_PRPRx                 , &
                    vitrad_PRPRx               , &
                    nullify_reac_PRPRx         , &
                    nullify_vlocy_PRPRx        , &
                    get_surf_PRPRx             , &
                    redo_nb_adj_PRPRx          , &
                    stock_rloc_PRPRx           , &
                    recup_rloc_PRPRx           , &
                    get_external_pressure_PRPRx, &
                    get_an_tacty_PRPRx          => get_an_tacty, &
                    get_verlet_tact_lawnb_PRPRx => get_verlet_tact_lawnb

  use PTPT3, only : get_this_PTPT3 => get_this, &
                    check_PTPT3               , &
                    get_nb_PTPT3              , &
                    set_nb_PTPT3              , &
                    injj_PTPT3                , &
                    prjj_PTPT3                , &
                    vitrad_PTPT3              , &
                    nullify_reac_PTPT3        , &
                    nullify_vlocy_PTPT3       , &
                    get_surf_PTPT3            , &
                    redo_nb_adj_PTPT3         , &
                    stock_rloc_PTPT3          , &
                    recup_rloc_PTPT3          , &
                    get_an_tacty_PTPT3          => get_an_tacty, &
                    get_verlet_tact_lawnb_PTPT3 => get_verlet_tact_lawnb

  use SPCDx, only : get_this_SPCDx => get_this, &
                    check_SPCDx               , &
                    get_nb_SPCDx              , &
                    set_nb_SPCDx              , &
                    injj_SPCDx                , &
                    prjj_SPCDx                , &
                    vitrad_SPCDx              , &
                    nullify_reac_SPCDx        , &
                    nullify_vlocy_SPCDx       , &
                    get_surf_SPCDx            , &
                    redo_nb_adj_SPCDx         , &
                    stock_rloc_SPCDx          , &
                    recup_rloc_SPCDx          , &
                    get_an_tacty_SPCDx          => get_an_tacty, &
                    get_verlet_tact_lawnb_SPCDx => get_verlet_tact_lawnb

  use SPDCx, only : get_this_SPDCx => get_this, &
                    check_SPDCx               , &
                    get_nb_SPDCx              , &
                    set_nb_SPDCx              , &
                    injj_SPDCx                , &
                    prjj_SPDCx                , &
                    vitrad_SPDCx              , &
                    nullify_reac_SPDCx        , &
                    nullify_vlocy_SPDCx       , &
                    get_surf_SPDCx            , &
                    redo_nb_adj_SPDCx         , &
                    stock_rloc_SPDCx          , &
                    recup_rloc_SPDCx          , &
                    get_an_tacty_SPDCx          => get_an_tacty, &
                    get_verlet_tact_lawnb_SPDCx => get_verlet_tact_lawnb

  use SPPLx, only : get_this_SPPLx => get_this, &
                    check_SPPLx               , &
                    get_nb_SPPLx              , &
                    set_nb_SPPLx              , &
                    injj_SPPLx                , &
                    prjj_SPPLx                , &
                    vitrad_SPPLx              , &
                    nullify_reac_SPPLx        , &
                    nullify_vlocy_SPPLx       , &
                    get_surf_SPPLx            , &
                    redo_nb_adj_SPPLx         , &
                    stock_rloc_SPPLx          , &
                    recup_rloc_SPPLx          , &
                    get_an_tacty_SPPLx          => get_an_tacty, &
                    get_verlet_tact_lawnb_SPPLx => get_verlet_tact_lawnb

  use SPPRx, only : get_this_SPPRx => get_this, &
                    check_SPPRx               , &
                    get_nb_SPPRx              , &
                    set_nb_SPPRx              , &
                    injj_SPPRx                , &
                    prjj_SPPRx                , &
                    vitrad_SPPRx              , &
                    nullify_reac_SPPRx        , &
                    nullify_vlocy_SPPRx       , &
                    get_surf_SPPRx            , &
                    redo_nb_adj_SPPRx         , &
                    stock_rloc_SPPRx          , &
                    recup_rloc_SPPRx          , &
                    get_an_tacty_SPPRx          => get_an_tacty, &
                    get_verlet_tact_lawnb_SPPRx => get_verlet_tact_lawnb

  use SPSPx, only : get_this_SPSPx => get_this, &
                    check_SPSPx               , &
                    get_nb_SPSPx              , &
                    set_nb_SPSPx              , &
                    injj_SPSPx                , &
                    prjj_SPSPx                , &
                    vitrad_SPSPx              , &
                    nullify_reac_SPSPx        , &
                    nullify_vlocy_SPSPx       , &
                    get_surf_SPSPx            , &
                    redo_nb_adj_SPSPx         , &
                    stock_rloc_SPSPx          , &
                    recup_rloc_SPSPx          , &
                    get_an_tacty_SPSPx          => get_an_tacty, &
                    get_verlet_tact_lawnb_SPSPx => get_verlet_tact_lawnb

  use tact_behaviour, only : get_nb_internal

  implicit none

  private

  !> number of 2D interaction type
  integer, parameter               :: nb_inter_ids = 13
  !> list of 2D interaction type ids
  integer, dimension(nb_inter_ids) :: inter_ids = (/ i_spspx, i_spcdx, &
                                                     i_spdcx, i_spplx, i_spprx, &
                                                     i_cdcdx, i_cdplx, &
                                                     i_prprx, i_prplx, &
                                                     i_prasp, i_ptpt3, &
                                                     i_csasp, i_csprx /)

  !> the id of the interaction type currently referenced by 'this'
  integer :: active_id = 0
  !$omp threadprivate(active_id)

  !> reference on the 'this' array of an interaction sub-module
  type(T_interaction), dimension(:), pointer :: this_inter   => null()
  !$omp threadprivate(this_inter)

  !> reference on the 'verlet' array of an interaction sub-module
  type(T_verlet)     , dimension(:), pointer :: verlet_inter => null()
  !$omp threadprivate(verlet_inter)

  !> reference ont the 'violation' array of an interaction sub-module
  real(kind=8)       , dimension(:), pointer :: violation_inter => null()
  !$omp threadprivate(violation_inter)

  !> reference on the 'con' array of an interaction sub-module
  type(T_con)                      , pointer :: con_inter => null()
  !$omp threadprivate(con_inter)

  
  !> active injj function
  procedure( injj_SPSPx )         , pointer :: injj_inter   => null()
  !$omp threadprivate(injj_inter)

  !> active prjj function
  procedure( prjj_SPSPx )         , pointer :: prjj_inter   => null()
  !$omp threadprivate(prjj_inter)

  !> active vitrad function
  procedure( vitrad_SPSPx )       , pointer :: vitrad_inter => null()
  !$omp threadprivate(vitrad_inter)

  !> active nullify_reac function
  procedure( nullify_reac_SPSPx ) , pointer :: nullify_reac_inter  => null()
  !$omp threadprivate(nullify_reac_inter)

  !> active nullify_vlocy function
  procedure( nullify_vlocy_SPSPx ), pointer :: nullify_vlocy_inter => null()
  !$omp threadprivate(nullify_vlocy_inter)

  !> active get_surf function
  procedure( get_surf_SPSPx ), pointer :: get_surf_inter    => null()
  !$omp threadprivate(get_surf_inter)
  
  !> active get_an_tacty function
  procedure( get_an_tacty_SPSPx ), pointer :: get_an_tacty_inter => null()
  !$omp threadprivate(get_an_tacty_inter)

  !> gerby active get_verlet_tact_lawnb function
  procedure( get_verlet_tact_lawnb_CSASx )  , pointer :: get_verlet_tact_lawnb_inter   => null()
  !$omp threadprivate(get_verlet_tact_lawnb_inter)

  !> active get_external_pressure function
  procedure( get_external_pressure_CSASx )  , pointer :: get_external_pressure_inter   => null()
  !$omp threadprivate(get_external_pressure_inter)

  ! private functions for internal plumbery
  private select_this_


  ! api of the module

  ! nlgs and wrap part
  public get_nb_inters, &
         get_nb_recup

  ! nlgs part
  public set_loc        , &
         get_rloc       , &
         get_vloc       , &
         get_vlocBEGIN  , &
         get_local_frame, &
         get_internal   , &
         set_internal   , &
         set_internal_bv, &
         inter2ENT      , &
         get_tact_lawnb , &
         injj           , &
         prjj           , &
         vitrad         , &
         nullify_reac   , &
         nullify_vlocy  , &
         get_surf       , &
         get_eff        , &
         set_violation  , &
         get_external_pressure         

  ! postpro part
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

  ! wrap part
  public T_verlet, T_con  , &
         get_ptr_verlet   , &
         get_ptr_con      , &
         get_all          , &
         get_all_idata    , &
         get_all_internal , &
         get_all_tactlawnb, &
         get_idata        , &
         stock_rloc       , &
         recup_rloc       , &
         recup_rloc_by_pos


  ! DDM part
  public get_icdbdy, &
         get_ianbdy

  ! i/o part
  public get_ptr_one, &
         set_nb_inters, &
         redo_nb_adj

contains

  !> Get the 'this' array of a sub-module in current context
  subroutine select_this_( inter_id )
    implicit none
    !> interaction id of the interaction to store in local 'this' array
    integer, intent(in) :: inter_id
    !
                                          !12345678901234567890123456789
    character(len=29), parameter :: IAM = 'inter_handler_3D::select_this'

    ! paranoid
    ! if( inter_id < 1 .or inter_id > nb_inter_ids ) call faterr(IAM,'wrong index')

    if( inter_id == active_id ) return

    select case( inter_id )
    case( i_cdcdx )
      call get_this_CDCDx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_CDCDx
      prjj_inter          => prjj_CDCDx
      vitrad_inter        => vitrad_CDCDx
      nullify_reac_inter  => nullify_reac_CDCDx
      nullify_vlocy_inter => nullify_vlocy_CDCDx
      get_surf_inter      => get_surf_CDCDx
      get_an_tacty_inter  => get_an_tacty_CDCDx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_CDCDx
    case( i_cdplx )
      call get_this_CDPLx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_CDPLx
      prjj_inter          => prjj_CDPLx
      vitrad_inter        => vitrad_CDPLx
      nullify_reac_inter  => nullify_reac_CDPLx
      nullify_vlocy_inter => nullify_vlocy_CDPLx
      get_surf_inter      => get_surf_CDPLx
      get_an_tacty_inter  => get_an_tacty_CDPLx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_CDPLx
    case( i_csasp )
      call get_this_CSASx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_CSASx
      prjj_inter          => prjj_CSASx
      vitrad_inter        => vitrad_CSASx
      nullify_reac_inter  => nullify_reac_CSASx
      nullify_vlocy_inter => nullify_vlocy_CSASx
      get_surf_inter      => get_surf_CSASx
      get_an_tacty_inter  => get_an_tacty_CSASx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_CSASx
      get_external_pressure_inter => get_external_pressure_CSASx
    case( i_csprx )
      call get_this_CSPRx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_CSPRx
      prjj_inter          => prjj_CSPRx
      vitrad_inter        => vitrad_CSPRx
      nullify_reac_inter  => nullify_reac_CSPRx
      nullify_vlocy_inter => nullify_vlocy_CSPRx
      get_surf_inter      => get_surf_CSPRx
      get_an_tacty_inter  => get_an_tacty_CSPRx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_CSPRx
    case( i_prasp )
      call get_this_PRASx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PRASx
      prjj_inter          => prjj_PRASx
      vitrad_inter        => vitrad_PRASx
      nullify_reac_inter  => nullify_reac_PRASx
      nullify_vlocy_inter => nullify_vlocy_PRASx
      get_surf_inter      => get_surf_PRASx
      get_an_tacty_inter  => get_an_tacty_PRASx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PRASx
    case( i_prplx )
      call get_this_PRPLx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PRPLx
      prjj_inter          => prjj_PRPLx
      vitrad_inter        => vitrad_PRPLx
      nullify_reac_inter  => nullify_reac_PRPLx
      nullify_vlocy_inter => nullify_vlocy_PRPLx
      get_surf_inter      => get_surf_PRPLx
      get_an_tacty_inter  => get_an_tacty_PRPLx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PRPLx
    case( i_prprx )
      call get_this_PRPRx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PRPRx
      prjj_inter          => prjj_PRPRx
      vitrad_inter        => vitrad_PRPRx
      nullify_reac_inter  => nullify_reac_PRPRx
      nullify_vlocy_inter => nullify_vlocy_PRPRx
      get_surf_inter      => get_surf_PRPRx
      get_an_tacty_inter  => get_an_tacty_PRPRx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PRPRx
      get_external_pressure_inter => get_external_pressure_PRPRx
    case( i_ptpt3 )
      call get_this_PTPT3(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_PTPT3
      prjj_inter          => prjj_PTPT3
      vitrad_inter        => vitrad_PTPT3
      nullify_reac_inter  => nullify_reac_PTPT3
      nullify_vlocy_inter => nullify_vlocy_PTPT3
      get_surf_inter      => get_surf_PTPT3
      get_an_tacty_inter  => get_an_tacty_PTPT3
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_PTPT3
    case( i_spcdx )
      call get_this_SPCDx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_SPCDx
      prjj_inter          => prjj_SPCDx
      vitrad_inter        => vitrad_SPCDx
      nullify_reac_inter  => nullify_reac_SPCDx
      nullify_vlocy_inter => nullify_vlocy_SPCDx
      get_surf_inter      => get_surf_SPCDx
      get_an_tacty_inter  => get_an_tacty_SPCDx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_SPCDx
    case( i_spdcx )
      call get_this_SPDCx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_SPDCx
      prjj_inter          => prjj_SPDCx
      vitrad_inter        => vitrad_SPDCx
      nullify_reac_inter  => nullify_reac_SPDCx
      nullify_vlocy_inter => nullify_vlocy_SPDCx
      get_surf_inter      => get_surf_SPDCx
      get_an_tacty_inter  => get_an_tacty_SPDCx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_SPDCx
    case( i_spplx )
      call get_this_SPPLx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_SPPLx
      prjj_inter          => prjj_SPPLx
      vitrad_inter        => vitrad_SPPLx
      nullify_reac_inter  => nullify_reac_SPPLx
      nullify_vlocy_inter => nullify_vlocy_SPPLx
      get_surf_inter      => get_surf_SPPLx
      get_an_tacty_inter  => get_an_tacty_SPPLx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_SPPLx
    case( i_spprx )
      call get_this_SPPRx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_SPPRx
      prjj_inter          => prjj_SPPRx
      vitrad_inter        => vitrad_SPPRx
      nullify_reac_inter  => nullify_reac_SPPRx
      nullify_vlocy_inter => nullify_vlocy_SPPRx
      get_surf_inter      => get_surf_SPPRx
      get_an_tacty_inter  => get_an_tacty_SPPRx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_SPPRx
    case( i_spspx )
      call get_this_SPSPx(this_inter, verlet_inter, violation_inter, con_inter)
      injj_inter          => injj_SPSPx
      prjj_inter          => prjj_SPSPx
      vitrad_inter        => vitrad_SPSPx
      nullify_reac_inter  => nullify_reac_SPSPx
      nullify_vlocy_inter => nullify_vlocy_SPSPx
      get_surf_inter      => get_surf_SPSPx
      get_an_tacty_inter  => get_an_tacty_SPSPx
      get_verlet_tact_lawnb_inter => get_verlet_tact_lawnb_SPSPx
    case default
      call faterr( IAM, 'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine select_this_

  !> Get the number of interactions of a selected type
  integer function get_nb_inters( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id
    !

    !fd i_real_tactor means interactions in this
    
    select case( inter_id )
    case( i_cdcdx )
      get_nb_inters = get_nb_CDCDx(i_real_tactor)
    case( i_cdplx )
      get_nb_inters = get_nb_CDPLx(i_real_tactor)
    case( i_csasp )
      get_nb_inters = get_nb_CSASx(i_real_tactor)
    case( i_csprx )
      get_nb_inters = get_nb_CSPRx(i_real_tactor)
    case( i_prasp )
      get_nb_inters = get_nb_PRASx(i_real_tactor)
    case( i_prplx )
      get_nb_inters = get_nb_PRPLx(i_real_tactor)
    case( i_prprx )
      get_nb_inters = get_nb_PRPRx(i_real_tactor)
    case( i_ptpt3 )
      get_nb_inters = get_nb_PTPT3(i_real_tactor)
    case( i_spcdx )
      get_nb_inters = get_nb_SPCDx(i_real_tactor)
    case( i_spdcx )
      get_nb_inters = get_nb_SPDCx(i_real_tactor)
    case( i_spplx )
      get_nb_inters = get_nb_SPPLx(i_real_tactor)
    case( i_spprx )
      get_nb_inters = get_nb_SPPRx(i_real_tactor)
    case( i_spspx )
      get_nb_inters = get_nb_SPSPx(i_real_tactor)
    case default
      call faterr( 'inter_handler_3D::get_nb_inters', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end function get_nb_inters

  !> set the number of interactions of a selected type
  subroutine set_nb_inters( inter_id, nb_inter )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id
    !> number of interactions
    integer, intent(in) :: nb_inter

    select case( inter_id )
    case( i_cdcdx )
      call set_nb_CDCDx(nb_inter)
    case( i_cdplx )
      call set_nb_CDPLx(nb_inter)
    case( i_csasp )
      call set_nb_CSASx(nb_inter)
    case( i_csprx )
      call set_nb_CSPRx(nb_inter)
    case( i_prasp )
      call set_nb_PRASx(nb_inter)
    case( i_prplx )
      call set_nb_PRPLx(nb_inter)
    case( i_prprx )
      call set_nb_PRPRx(nb_inter)
    case( i_ptpt3 )
      call set_nb_PTPT3(nb_inter)
    case( i_spcdx )
      call set_nb_SPCDx(nb_inter)
    case( i_spdcx )
      call set_nb_SPDCx(nb_inter)
    case( i_spplx )
      call set_nb_SPPLx(nb_inter)
    case( i_spprx )
      call set_nb_SPPRx(nb_inter)
    case( i_spspx )
      call set_nb_SPSPx(nb_inter)
    case default
      call faterr( 'inter_handler_3D::set_nb_inters', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine set_nb_inters

  !> Get the number of recup interactions of a selected type
  integer function get_nb_recup( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    !fd i_recup_tactor means interactions recup
    
    select case( inter_id )
    case( i_cdcdx )
      get_nb_recup = get_nb_CDCDx(i_recup_tactor)
    case( i_cdplx )
      get_nb_recup = get_nb_CDPLx(i_recup_tactor)
    case( i_csasp )
      get_nb_recup = get_nb_CSASx(i_recup_tactor)
    case( i_csprx )
      get_nb_recup = get_nb_CSPRx(i_recup_tactor)
    case( i_prasp )
      get_nb_recup = get_nb_PRASx(i_recup_tactor)
    case( i_prplx )
      get_nb_recup = get_nb_PRPLx(i_recup_tactor)
    case( i_prprx )
      get_nb_recup = get_nb_PRPRx(i_recup_tactor)
    case( i_ptpt3 )
      get_nb_recup = get_nb_PTPT3(i_recup_tactor)
    case( i_spcdx )
      get_nb_recup = get_nb_SPCDx(i_recup_tactor)
    case( i_spdcx )
      get_nb_recup = get_nb_SPDCx(i_recup_tactor)
    case( i_spplx )
      get_nb_recup = get_nb_SPPLx(i_recup_tactor)
    case( i_spprx )
      get_nb_recup = get_nb_SPPRx(i_recup_tactor)
    case( i_spspx )
      get_nb_recup = get_nb_SPSPx(i_recup_tactor)
    case default
      call faterr( 'inter_handler_3D::get_nb_recup', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end function get_nb_recup

  !> redo adjacence map of a selected type
  subroutine redo_nb_adj( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_cdcdx )
      call redo_nb_adj_CDCDx()
    case( i_cdplx )
      call redo_nb_adj_CDPLx()
    case( i_csasp )
      call redo_nb_adj_CSASx()
    case( i_csprx )
      call redo_nb_adj_CSPRx()
    case( i_prasp )
      call redo_nb_adj_PRASx()
    case( i_prplx )
      call redo_nb_adj_PRPLx()
    case( i_prprx )
      call redo_nb_adj_PRPRx()
    case( i_ptpt3 )
      call redo_nb_adj_PTPT3()
    case( i_spcdx )
      call redo_nb_adj_SPCDx()
    case( i_spdcx )
      call redo_nb_adj_SPDCx()
    case( i_spplx )
      call redo_nb_adj_SPPLx()
    case( i_spprx )
      call redo_nb_adj_SPPRx()
    case( i_spspx )
      call redo_nb_adj_SPSPx()
    case default
      call faterr( 'inter_handler_3D::redo_nb_adj', &
                   'interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine redo_nb_adj

  !> Stock from this to verlet
  subroutine stock_rloc( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_cdcdx )
      if( .not. check_CDCDx() ) return
      call stock_rloc_CDCDx()
    case( i_cdplx )
      if( .not. check_CDPLx() ) return
      call stock_rloc_CDPLx()
    case( i_csasp )
      if( .not. check_CSASx() ) return
      call stock_rloc_CSASx()
    case( i_csprx )
      if( .not. check_CSPRx() ) return
      call stock_rloc_CSPRx()
    case( i_prasp )
      if( .not. check_PRASp() ) return
      call stock_rloc_PRASp()
    case( i_prplx )
      if( .not. check_PRPLx() ) return
      call stock_rloc_PRPLx()
    case( i_prprx )
      if( .not. check_PRPRx() ) return
      call stock_rloc_PRPRx()
    case( i_ptpt3 )
      if( .not. check_PTPT3() ) return
      call stock_rloc_PTPT3()
    case( i_spcdx )
      if( .not. check_SPCDx() ) return
      call stock_rloc_SPCDx()
    case( i_spdcx )
      if( .not. check_SPDCx() ) return
      call stock_rloc_SPDCx()
    case( i_spplx )
      if( .not. check_SPPLx() ) return
      call stock_rloc_SPPLx()
    case( i_spprx )
      if( .not. check_SPPRx() ) return
      call stock_rloc_SPPRx()
    case( i_spspx )
      if( .not. check_SPSPx() ) return
      call stock_rloc_SPSPx()
    case default
      call faterr( 'inter_handler_3D::stock_rloc', &
                   'interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine stock_rloc

  !> Recup from verlet to this
  subroutine recup_rloc( inter_id )
    implicit none
    !> interaction type id
    integer, intent(in) :: inter_id

    select case( inter_id )
    case( i_cdcdx )
      if( .not. check_CDCDx() ) return
      call recup_rloc_CDCDx()
    case( i_cdplx )
      if( .not. check_CDPLx() ) return
      call recup_rloc_CDPLx()
    case( i_csasp )
      if( .not. check_CSASx() ) return
      call recup_rloc_CSASx()
    case( i_csprx )
      if( .not. check_CSPRx() ) return
      call recup_rloc_CSPRx()
    case( i_prasp )
      if( .not. check_PRASp() ) return
      call recup_rloc_PRASp()
    case( i_prplx )
      if( .not. check_PRPLx() ) return
      call recup_rloc_PRPLx()
    case( i_prprx )
      if( .not. check_PRPRx() ) return
      call recup_rloc_PRPRx()
    case( i_ptpt3 )
      if( .not. check_PTPT3() ) return
      call recup_rloc_PTPT3()
    case( i_spcdx )
      if( .not. check_SPCDx() ) return
      call recup_rloc_SPCDx()
    case( i_spdcx )
      if( .not. check_SPDCx() ) return
      call recup_rloc_SPDCx()
    case( i_spplx )
      if( .not. check_SPPLx() ) return
      call recup_rloc_SPPLx()
    case( i_spprx )
      if( .not. check_SPPRx() ) return
      call recup_rloc_SPPRx()
    case( i_spspx )
      if( .not. check_SPSPx() ) return
      call recup_rloc_SPSPx()
    case default
      call faterr( 'inter_handler_3D::recup_rloc', &
                   'interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
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
    case( i_csasp )
      if( .not. check_CSASx() ) return
      call recup_rloc_by_position_CSASx(rtol)
    case( i_csprx )
      if( .not. check_CSPRx() ) return
      call recup_rloc_by_position_CSPRx(rtol)
    case default
      call faterr( 'inter_handler_3D::recup_rloc_by_pos', &
                   'Interaction id unaivalable: '//get_interaction_name_from_id(inter_id) )
    end select

  end subroutine recup_rloc_by_pos

  !> Put back computed value of local reaction, velocity and gap in this array
  subroutine set_loc(id_inter, icdan, status, vlt, vln, vls, rlt, rln, rls, gap)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> contact status
    integer     , intent(in) :: status
    !> first tangential local velocity
    real(kind=8), intent(in) :: vlt
    !> normal local velocity
    real(kind=8), intent(in) :: vln
    !> second tangential local velocity
    real(kind=8), intent(in) :: vls
    !> first tangential local reaction
    real(kind=8), intent(in) :: rlt
    !> normal local reaction
    real(kind=8), intent(in) :: rln
    !> second tangential local reaction
    real(kind=8), intent(in) :: rls
    !> gap
    real(kind=8), intent(in) :: gap

    call select_this_(id_inter)

    this_inter(icdan)%status = status
    this_inter(icdan)%vlt    = vlt
    this_inter(icdan)%vln    = vln
    this_inter(icdan)%vls    = vls
    this_inter(icdan)%rlt    = rlt
    this_inter(icdan)%rln    = rln
    this_inter(icdan)%rls    = rls
    this_inter(icdan)%gapTT  = gap

  end subroutine set_loc

  !> Get local reaction and status
  subroutine get_rloc(id_inter, icdan, rlt, rln, rls, status)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> first tangential local reaction
    real(kind=8), intent(out) :: rlt
    !> normal local reaction
    real(kind=8), intent(out) :: rln
    !> second tangeentail local reaction
    real(kind=8), intent(out) :: rls
    !> contact status
    integer     , intent(out) :: status

    call select_this_(id_inter)

    rlt    = this_inter(icdan)%rlt
    rln    = this_inter(icdan)%rln
    rls    = this_inter(icdan)%rls
    status = this_inter(icdan)%status

  end subroutine get_rloc

  !> Get local velocity status 
  subroutine get_vloc(id_inter, icdan, vlt, vln, vls)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> first tangential local velocity
    real(kind=8), intent(out) :: vlt
    !> normal local velocity
    real(kind=8), intent(out) :: vln
    !> second tangential local velocity
    real(kind=8), intent(out) :: vls

    call select_this_(id_inter)

    vlt = this_inter(icdan)%vlt
    vln = this_inter(icdan)%vln
    vls = this_inter(icdan)%vls

  end subroutine get_vloc

  !> Get local velocity status and gap at the beginning of the step
  subroutine get_vlocBEGIN(id_inter, icdan, vltBEGIN, vlnBEGIN, vlsBEGIN, gapBEGIN, statusBEGIN)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> first tangential local velocity
    real(kind=8), intent(out) :: vltBEGIN
    !> normal local velocity
    real(kind=8), intent(out) :: vlnBEGIN
    !> second tangential local velocity
    real(kind=8), intent(out) :: vlsBEGIN
    !> gap
    real(kind=8), intent(out) :: gapBEGIN
    !> contact status
    integer     , intent(out) :: statusBEGIN

    call select_this_(id_inter)

    vltBEGIN    = this_inter(icdan)%vltBEGIN
    vlnBEGIN    = this_inter(icdan)%vlnBEGIN
    vlsBEGIN    = this_inter(icdan)%vlsBEGIN
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
    real(kind=8), dimension(12), intent(out) :: loc

    call select_this_(id_inter)

    loc(1 : 3) = this_inter(icdan)%coor(1:3)

    loc(4 : 6) = this_inter(icdan)%tuc(1:3)
    loc(7 : 9) = this_inter(icdan)%nuc(1:3)
    loc(10:12) = this_inter(icdan)%suc(1:3)

  end subroutine get_local_frame

  !> Put some reaction at second member of an algebraic system
  subroutine injj(id_inter, icdan, rtik, rnik, rsik, storage)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> first tangential local reaction
    real(kind=8), intent(in) :: rtik
    !> normal local reaction
    real(kind=8), intent(in) :: rnik
    !> second tangential local reaction
    real(kind=8), intent(in) :: rsik
    !> where to put the reaction
    integer     , intent(in) :: storage

    call select_this_(id_inter)
    call injj_inter(icdan, rsik, rtik, rnik, storage)

  end subroutine injj

  !> Get some results from an algebraic system resolution
  subroutine prjj(id_inter, icdan, vtik, vnik, vsik, storage)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> first tangential local velocity
    real(kind=8), intent(out) :: vtik
    !> normal local velocity
    real(kind=8), intent(out) :: vnik
    !> second tangential local velocity
    real(kind=8), intent(out) :: vsik
    !> where to get the velocity
    integer     , intent(in)  :: storage

    call select_this_(id_inter)
    call prjj_inter(icdan, vsik, vtik, vnik, storage)

  end subroutine prjj

  !> Get the length associated to the contact
  function get_surf(id_inter, icdan)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !
    real(kind=8) :: get_surf

    call select_this_(id_inter)
    get_surf = get_surf_inter(icdan)

  end function get_surf

  !> Get the local frame of a verlet interaction
  subroutine get_verlet_local_frame(id_inter, icdtac, iadj, ptc, tuc, nuc, suc)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> contact point locus
    real(kind=8), dimension(3), intent(out) :: ptc
    !> first tangent component of local frame
    real(kind=8), dimension(3), intent(out) :: tuc
    !> normal component of local frame
    real(kind=8), dimension(3), intent(out) :: nuc
    !> second tangent component of local frame
    real(kind=8), dimension(3), intent(out) :: suc

    call select_this_(id_inter)

    ptc = verlet_inter(icdtac)%coor(:, iadj)
    tuc = verlet_inter(icdtac)%tuc(:, iadj)
    nuc = verlet_inter(icdtac)%nuc(:, iadj)
    suc = verlet_inter(icdtac)%suc(:, iadj)

  end subroutine get_verlet_local_frame

  !> Get the local reaction of a verlet interaction
  subroutine get_verlet_rloc(id_inter, icdtac, iadj, i_status, rlt, rln, rls)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> contact status
    integer     , intent(out) :: i_status
    !> first tangent reaction
    real(kind=8), intent(out) :: rlt
    !> normal reaction
    real(kind=8), intent(out) :: rln
    !> second tangent reaction
    real(kind=8), intent(out) :: rls

    call select_this_(id_inter)

    i_status = verlet_inter(icdtac)%status(iadj)

    rlt = verlet_inter(icdtac)%rlt(iadj)
    rln = verlet_inter(icdtac)%rln(iadj)
    rls = verlet_inter(icdtac)%rls(iadj)

  end subroutine get_verlet_rloc

  !> Get the local velocity of a verlet interaction
  subroutine get_verlet_vloc(id_inter, icdtac, iadj, vlt, vln, vls)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> first tangent velocity
    real(kind=8), intent(out) :: vlt
    !> normal velocity
    real(kind=8), intent(out) :: vln
    !> second tangent velocity
    real(kind=8), intent(out) :: vls

    call select_this_(id_inter)

    vlt = verlet_inter(icdtac)%vlt(iadj)
    vln = verlet_inter(icdtac)%vln(iadj)
    vls = verlet_inter(icdtac)%vls(iadj)

  end subroutine get_verlet_vloc

  function get_ptr_verlet( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> returned pointer
    type(T_verlet), dimension(:), pointer :: get_ptr_verlet

    call select_this_(inter_id)
    get_ptr_verlet => verlet_inter

  end function

  function get_ptr_con( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> returned pointer
    type(T_con), pointer :: get_ptr_con

    call select_this_(inter_id)
    get_ptr_con => con_inter

  end function

  !> \brief Get all the interaction of a submodule
  !> Allocate memory and copy
  function get_all( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> coor(3),t(3),n(3),s(3),gap,ft,fn,fs
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

    allocate( get_all(19,icdan) )
  
    ! filling output array
    do icdtac = 1,nb_cd

      if (verlet_inter(icdtac)%adjsz == 0) cycle
  
      do iadj = 1, verlet_inter(icdtac)%adjsz

        icdan = verlet_inter(icdtac)%icdan(iadj)

        get_all( 1: 3 ,icdan) = verlet_inter(icdtac)%coor(:,iadj)
        get_all( 4: 6 ,icdan) = verlet_inter(icdtac)%tuc(:,iadj)
        get_all( 7: 9 ,icdan) = verlet_inter(icdtac)%nuc(:,iadj)
        get_all(10:12 ,icdan) = verlet_inter(icdtac)%suc(:,iadj)
        get_all(   13 ,icdan) = verlet_inter(icdtac)%rlt(iadj)
        get_all(   14 ,icdan) = verlet_inter(icdtac)%rln(iadj)
        get_all(   15 ,icdan) = verlet_inter(icdtac)%rls(iadj)
        get_all(   16 ,icdan) = verlet_inter(icdtac)%vlt(iadj)
        get_all(   17 ,icdan) = verlet_inter(icdtac)%vln(iadj)
        get_all(   18 ,icdan) = verlet_inter(icdtac)%vls(iadj)
        get_all(   19 ,icdan) = verlet_inter(icdtac)%gaptt(iadj)
      end do

    end do
  
  end function get_all

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
        call injj_inter(icdan, this_inter(icdan)%rls, this_inter(icdan)%rlt, this_inter(icdan)%rln, iIreac)
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
  !integer function get_verlet_tact_lawnb(id_inter, icdtac, iadj) <- grosse merde !
  !subroutine get_verlet_internal(id_inter, icdtac, iadj, internal)
  !subroutine set_verlet_internal(id_inter, icdtac, iadj, internal)
  !function get_all_internal( inter_id )
  !function get_all_idata( inter_id )
  !subroutine get_eff(id_inter, icdan, meff, reff)
  !function get_ptr_one(id_inter, icdan)

end module inter_meca_handler_3D
