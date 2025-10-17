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

!>  This module contains mapping between names and integer (parameter) identifier

! To check the consistency between the parameters and the content of the array
! storing the parameters' name, there is simple automatic test. One must list
! the content of the array and check that the strings are stored in the same order
! than the parameters are added.
!
! In python one can use somehting like:
! >>> import operator
! >>> chipy.Initialize()
! >>> print 'BodyTypes : '
! >>> for t in sorted(chipy.BodyTypes.items(), key=operator.itemgetter(1)):
! >>>   print t
!
! And check that the string are correctly facing the i_string

module parameters

  implicit none

  public

    ! map between physics type and integer identifier
    integer, parameter :: i_mecax = 1         , &
                          i_therx = i_mecax + 1, &
                          i_porox = i_therx + 1, &
                          i_multi = i_porox + 1
    integer, dimension(4), private :: physic_type_id = (/ i_mecax, i_therx, &
                                                          i_porox, i_multi  /)
    character(len=5), dimension(4), target, private :: physic_type = (/ 'mecax', 'therx', &
                                                                        'porox', 'multi' /)

    ! map between body model and integer identifier
    integer(kind=4), parameter :: i_rbdy2 = 1
    integer(kind=4), parameter :: i_rbdy3 = i_rbdy2 + 1
    integer(kind=4), parameter :: i_mailx = i_rbdy3 + 1
    integer(kind=4), parameter :: i_mbs3  = i_mailx + 1
    integer(kind=4), parameter :: i_mbs2  = i_mbs3  + 1

    integer, dimension(i_mbs2), private :: body_model_id = (/ i_rbdy2, i_rbdy3, &
                                                              i_mailx,          &
                                                              i_mbs3 , i_mbs2   /)

    character(len=5), dimension(i_mbs2), target, private :: body_model = (/ 'RBDY2', 'RBDY3', &
                                                                            'MAILx'         , &
                                                                            'MBS3D', 'MBS2D' /)

    ! map between contactor type and an integer identifier

    !contactor on rigid objects
    !2D
    integer(kind=4), parameter :: i_diskx = 1
    integer(kind=4), parameter :: i_xksid = i_diskx + 1
    integer(kind=4), parameter :: i_polyg = i_xksid + 1
    integer(kind=4), parameter :: i_joncx = i_polyg + 1
    integer(kind=4), parameter :: i_pt2dx = i_joncx + 1
    !3D
    integer(kind=4), parameter :: i_spher = i_pt2dx + 1
    integer(kind=4), parameter :: i_cylnd = i_spher + 1
    integer(kind=4), parameter :: i_dnlyc = i_cylnd + 1
    integer(kind=4), parameter :: i_polyr = i_dnlyc + 1
    integer(kind=4), parameter :: i_planx = i_polyr + 1
    integer(kind=4), parameter :: i_pt3dx = i_planx + 1

    ! contactor on deformable object
    !2D
    integer(kind=4), parameter :: i_alpxx = i_pt3dx + 1
    integer(kind=4), parameter :: i_clxxx = i_alpxx + 1
    integer(kind=4), parameter :: i_diskl = i_clxxx + 1
    integer(kind=4), parameter :: i_pt2dl = i_diskl + 1
    !3D
    integer(kind=4), parameter :: i_aspxx = i_pt2dl + 1
    integer(kind=4), parameter :: i_csxxx = i_aspxx + 1

    integer, dimension(17), parameter :: contactor_type_id = (/ i_diskx, i_xksid,                   &
                                                                i_polyg, i_joncx, i_pt2dx, i_spher, &
                                                                i_cylnd, i_dnlyc, i_polyr, i_planx, &
                                                                i_pt3dx, i_alpxx, i_clxxx, i_diskl, &
                                                                i_pt2dl, i_aspxx, i_csxxx          /)

    character(len=5), dimension(17), target, private :: contactor_type = (/ 'DISKx', 'xKSID',                   &
                                                                            'POLYG', 'JONCx', 'PT2Dx', 'SPHER', &
                                                                            'CYLND', 'DNLYC', 'POLYR', 'PLANx', &
                                                                            'PT3Dx', 'ALpxx', 'CLxxx', 'DISKL', &
                                                                            'PT2DL', 'ASpxx', 'CSxxx' /)

    ! map between interaction type and an integer identifier
    integer(kind=4), parameter :: i_dkdkx = 1
    integer(kind=4), parameter :: i_dkkdx = i_dkdkx + 1
    integer(kind=4), parameter :: i_dkplx = i_dkkdx + 1
    integer(kind=4), parameter :: i_dkjcx = i_dkplx + 1
    integer(kind=4), parameter :: i_dkalp = i_dkjcx + 1
    integer(kind=4), parameter :: i_dkdkl = i_dkalp + 1
    integer(kind=4), parameter :: i_plplx = i_dkdkl + 1
    integer(kind=4), parameter :: i_pljcx = i_plplx + 1
    integer(kind=4), parameter :: i_plalp = i_pljcx + 1
    integer(kind=4), parameter :: i_ptpt2 = i_plalp + 1
    integer(kind=4), parameter :: i_p2p2l = i_ptpt2 + 1

    integer(kind=4), parameter :: i_spspx = i_p2p2l + 1
    integer(kind=4), parameter :: i_spcdx = i_spspx + 1
    integer(kind=4), parameter :: i_spdcx = i_spcdx + 1
    integer(kind=4), parameter :: i_spplx = i_spdcx + 1
    integer(kind=4), parameter :: i_cdcdx = i_spplx + 1
    integer(kind=4), parameter :: i_cdplx = i_cdcdx + 1
    integer(kind=4), parameter :: i_prprx = i_cdplx + 1
    integer(kind=4), parameter :: i_prplx = i_prprx + 1
    integer(kind=4), parameter :: i_prasp = i_prplx + 1
    integer(kind=4), parameter :: i_ptpt3 = i_prasp + 1

    integer(kind=4), parameter :: i_clalp = i_ptpt3 + 1
    integer(kind=4), parameter :: i_cljcx = i_clalp + 1
    integer(kind=4), parameter :: i_csasp = i_cljcx + 1
    integer(kind=4), parameter :: i_csprx = i_csasp + 1

    integer(kind=4), parameter :: i_spprx = i_csprx + 1

    ! number of interaction types
    integer(kind=4), parameter :: nb_interaction_types = i_spprx
    integer, dimension(nb_interaction_types) :: interaction_type_id = (/i_dkdkx, i_dkkdx, i_dkplx, i_dkjcx, &
                                                                        i_dkalp, i_dkdkl,                   &
                                                                                 i_plplx, i_pljcx, i_plalp, &
                                                                        i_ptpt2, i_p2p2l, i_spspx, i_spcdx, &
                                                                        i_spdcx, i_spplx,                   &
                                                                        i_cdcdx, i_cdplx, &
                                                                        i_prprx, i_prplx, i_prasp, i_ptpt3, &
                                                                        i_clalp, i_cljcx, i_csasp, i_csprx, &
                                                                        i_spprx                             &
                                                                      /)
    character(len=5), dimension(26), target, private :: interaction_type = (/ 'DKDKx', 'DKKDx', 'DKPLx', 'DKJCx', &
                                                                              'DKALp', 'DKDKL',                   &
                                                                                       'PLPLx', 'PLJCx', 'PLALp', &
                                                                              'PTPT2', 'P2P2L', 'SPSPx', 'SPCDx', &
                                                                              'SPDCx', 'SPPLx',                   &
                                                                              'CDCDx', 'CDPLx',                   &
                                                                              'PRPRx', 'PRPLx', 'PRASp', 'PTPT3', &
                                                                              'CLALp', 'CLJCx', 'CSASp', 'CSPRx', &
                                                                              'SPPRx'                             &
                                                                           /)


    ! map between matrix storage type and integer identifier
    integer(kind=4), public, parameter :: i_diagonal = 1 
    integer(kind=4), public, parameter :: i_sparse   = i_diagonal+ 1
    integer(kind=4), public, parameter :: i_band     = i_sparse  + 1
    integer(kind=4), public, parameter :: i_skyline  = i_band    + 1
    integer(kind=4), public, parameter :: i_full     = i_skyline + 1
    integer(kind=4), public, parameter :: i_exploded = i_full    + 1

    integer         , dimension(6)         :: matrix_storage_id = (/ i_diagonal, i_sparse  ,&
                                                                     i_band    , i_skyline ,&
                                                                     i_full    , i_exploded /)
    character(len=8), dimension(6), target, private:: matrix_storage = (/ 'diagonal', 'sparse__', &
                                                                          'band____', 'skyline_', &
                                                                          'full____', 'exploded' /)

    ! map between matrix shape type and integer identifier
    integer(kind=4), public, parameter :: i_sym = 1 
    integer(kind=4), public, parameter :: i_std = i_sym + 1

    integer         , dimension(2)         :: matrix_shape_id = (/ i_sym, i_std /)
    character(len=8), dimension(2), target, private :: matrix_shape = (/ 'sym_____', 'std_____'/)

    ! map between generalized coordinates type and integer identifier
    integer(kind=4), parameter :: i_natural = 1
    integer(kind=4), parameter :: i_newton_euler_2d = i_natural + 1
    integer(kind=4), parameter :: i_newton_euler_3d = i_natural + 2

    integer          , dimension(3)          :: generalized_coordinates_id = (/ i_natural        , i_newton_euler_2d, &
                                                                                i_newton_euler_3d                     /)
    character(len=15), dimension(3), target, private :: generalized_coordinates = (/ 'natural        ', 'newton_euler_2d', &
                                                                                     'newton_euler_3d' /)

    !map between surface energy status and integer identifier
    !mr: do not permut parameter numbering: i_stationary > i_undefined
    !                                       i_sheared    > i_stationary
    !                                       i_free       > i_sheared
    integer(kind=4), parameter :: i_undefined  = 1
    integer(kind=4), parameter :: i_stationary = i_undefined  + 1
    integer(kind=4), parameter :: i_sheared    = i_stationary + 1
    integer(kind=4), parameter :: i_free       = i_sheared    + 1

    integer          , dimension(4) :: surface_energy_status_id = (/ i_undefined, i_stationary, &
                                                                     i_sheared  , i_free        /)
    character(len=10), dimension(4), target, private :: surface_energy_status = (/ 'undefined ', 'stationary', &
                                                                                   'sheared   ', 'free      '  /)

    ! map between interaction law type and integer identifier
    integer(kind=4), parameter :: &
       i_IQS_CLB                  = 1, & 
       i_IQS_CLB_nosldt           = i_IQS_CLB                  + 1, &
       i_IQS_CLB_noslds           = i_IQS_CLB_nosldt           + 1, &
       i_IQS_CLB_RGR              = i_IQS_CLB_noslds           + 1, &
       i_IQS_DS_CLB               = i_IQS_CLB_RGR              + 1, &
       i_IQS_WET_DS_CLB           = i_IQS_DS_CLB               + 1, &
       i_postGAP_IQS_MAC_CZM      = i_IQS_WET_DS_CLB           + 1, &  !fd
       i_PLASTIC_COUPLED_DOF      = i_postGAP_IQS_MAC_CZM      + 1, &
       i_xQS_WET_DS_CLB           = i_PLASTIC_COUPLED_DOF      + 1, &
       i_TEX_SOL                  = i_xQS_WET_DS_CLB           + 1, &
       i_TEX_SOL_UNILAT           = i_TEX_SOL                  + 1, &
       i_GAP_SGR_CLB              = i_TEX_SOL_UNILAT           + 1, &
       i_VEL_SGR_CLB              = i_GAP_SGR_CLB              + 1, &
       i_GAP_WET_DS_CLB           = i_VEL_SGR_CLB              + 1, &
       i_MSMP_CZM                 = i_GAP_WET_DS_CLB           + 1, & !fd
       i_GAP_SGR_DS_CLB           = i_MSMP_CZM                 + 1, &
       i_VEL_SGR_DS_CLB           = i_GAP_SGR_DS_CLB           + 1, &
       i_RST_CLB                  = i_VEL_SGR_DS_CLB           + 1, &
       i_RST_DS_CLB               = i_RST_CLB                  + 1, &
       i_IQS_MOHR_DS_CLB          = i_RST_DS_CLB               + 1, & !fd loi mohr_ds_clb pour les rigides
       i_MAL_CZM                  = i_IQS_MOHR_DS_CLB          + 1, & !fd
       i_ELASTIC_REPELL_CLB       = i_MAL_CZM                  + 1, &
       i_CRITICAL_VOIGT_CLB       = i_ELASTIC_REPELL_CLB       + 1, &
       i_ELASTIC_REPELL_WET_CLB   = i_CRITICAL_VOIGT_CLB       + 1, &
       i_ELASTIC_ROD              = i_ELASTIC_REPELL_WET_CLB   + 1, &
       i_VOIGT_ROD                = i_ELASTIC_ROD              + 1, &
       i_ELASTIC_WIRE             = i_VOIGT_ROD                + 1, &
       i_VOIGT_WIRE               = i_ELASTIC_WIRE             + 1, &
       i_MD_JKRs                  = i_VOIGT_WIRE               + 1, & ! smooth contact law
       i_ELASTIC_WET_NT           = i_MD_JKRs                  + 1, &
       i_COUPLED_DOF              = i_ELASTIC_WET_NT           + 1, &
       i_TANGENTIAL_COUPLED_DOF   = i_COUPLED_DOF              + 1, &
       i_NORMAL_COUPLED_DOF       = i_TANGENTIAL_COUPLED_DOF   + 1, &
       i_PERIO_DOF                = i_NORMAL_COUPLED_DOF       + 1, &
       i_GAP_SGR_CLB_WEAR         = i_PERIO_DOF                + 1, &
       i_IQS_SGR_CLB_WEAR         = i_GAP_SGR_CLB_WEAR         + 1, &
       i_MAC_CZM                  = i_IQS_SGR_CLB_WEAR         + 1, &
       i_IQS_MAC_CZM              = i_MAC_CZM                  + 1, &
       i_WET_CZM                  = i_IQS_MAC_CZM              + 1, &
       i_GAP_MOHR_DS_CLB          = i_WET_CZM                  + 1, & !fd loi mohr_ds_clb pour les defo
       i_MAC_CZM_nosldt           = i_GAP_MOHR_DS_CLB          + 1, &
       i_MAC_CZM_noslds           = i_MAC_CZM_nosldt           + 1, &
       i_IQS_MAC_CZM_nosldt       = i_MAC_CZM_noslds           + 1, &
       i_IQS_MAC_CZM_noslds       = i_IQS_MAC_CZM_nosldt       + 1, &
       i_ER_MAC_CZM               = i_IQS_MAC_CZM_noslds       + 1, & !fd ER en compression MAC_CZM en traction
       i_IQS_WET_CZM              = i_ER_MAC_CZM               + 1, & !
       i_DEM_FIBs                 = i_IQS_WET_CZM              + 1, & ! smooth contact law
       i_KV_WET                   = i_DEM_FIBs                 + 1, &
       i_VISCO_ELASTIC_REPELL_WET = i_KV_WET                   + 1, &
       i_RIGID_WIRE               = i_VISCO_ELASTIC_REPELL_WET + 1, &
       i_IQS_BW_CLB               = i_RIGID_WIRE               + 1, &
       i_IQS_CAP_MOHR_DS_CLB      = i_IQS_BW_CLB               + 1, & !fd pour une loi mohr_ds_clb un peu trafique
       i_IQS_MAL_CZM              = i_IQS_CAP_MOHR_DS_CLB      + 1, &
       i_GAP_CAP_MOHR_DS_CLB      = i_IQS_MAL_CZM              + 1, & !fd pour une loi mohr_ds_clb un peu trafique
       i_BROKEN_DOF               = i_GAP_CAP_MOHR_DS_CLB      + 1, &    !mr COUPLED DOF WITH A NORMAL TRESHOLD
       i_IQS_CLB_g0               = i_BROKEN_DOF               + 1, &
       i_IQS_MAP                  = i_IQS_CLB_g0               + 1, &
       i_RST_WET_CLB              = i_IQS_MAP                  + 1, &  ! Restitution law with cohesion
       i_LJ_POTENTIAL             = i_RST_WET_CLB              + 1, &  ! Lennard-Jones Potential for MD or Coarse-Grain method
       i_GAP_SGR_CLB_g0           = i_LJ_POTENTIAL             + 1, &  ! am & pta: loi contact frottant defo/defo ou defo/rigide avec pre gap
       i_BRITTLE_ELASTIC_WIRE     = i_GAP_SGR_CLB_g0           + 1, &  !fd un cable elastique qui casse 
       i_ELASTIC_REPELL_CLB_g0    = i_BRITTLE_ELASTIC_WIRE     + 1, &  ! loi elastic avec un gap initial
       i_MP_CZM                   = i_ELASTIC_REPELL_CLB_g0    + 1, &  !fp Monerie-Perales CZM 
       i_MP3_CZM                  = i_MP_CZM                   + 1, &  !fp Monerie-Perales 3 parameters CZM
       i_MP3_CZM_THER             = i_MP3_CZM                  + 1, &  !fp Monerie-Perales 3 parameters CZM + THER      
       i_TH_CZM                   = i_MP3_CZM_THER             + 1, &  !fp Tvergaard Hutchinson CZM
       i_IQS_TH_CZM               = i_TH_CZM                   + 1, &  !fp Tvergaard Hutchinson CZM
       i_postGAP_IQS_CZM          = i_IQS_TH_CZM               + 1, &
       !am: law added to be able to import parameters in nlgs 3D
       i_WET_3C                   = i_postGAP_IQS_CZM          + 1, &
       i_VISCO_ELASTIC_REPELL_CLB = i_WET_3C                   + 1, &  !fd un elastique repell avec viscosite
       i_ELASTIC_REPELL_MAC_CZM   = i_VISCO_ELASTIC_REPELL_CLB + 1, &
       i_IQS_PLAS_CLB             = i_ELASTIC_REPELL_MAC_CZM   + 1, &  ! pta,fd,mr, loi iqs avec partie plastique
       i_ABP_CZM                  = i_IQS_PLAS_CLB             + 1, &  ! fd, marie sauve - stephane morel approximated bilinear perterson 
       i_IQS_ABP_CZM              = i_ABP_CZM                  + 1, &  ! fd, marie sauve - stephane morel approximated bilinear perterson 
       i_BRITTLE_COATING_CLB      = i_IQS_ABP_CZM              + 1, &
       i_IQS_STICK                = i_BRITTLE_COATING_CLB      + 1, &  !fd loi IQS avec frottement infini (collant)
       i_GAP_SGR_STICK            = i_IQS_STICK                + 1, &  !fd loi GAP_SGR  avec frottement infini (collant)          
       i_GAP_SGR_CLB_nosldt       = i_GAP_SGR_STICK            + 1, &
       i_GAP_SGR_CLB_noslds       = i_GAP_SGR_CLB_nosldt       + 1, &
       i_preGAP_SGR_CLB           = i_GAP_SGR_CLB_noslds       + 1, &     ! pta reactivation loi 02/09/2015
       i_NARD_ROD                 = i_preGAP_SGR_CLB           + 1, &
       i_IQS_EXPO_CZM             = i_NARD_ROD                 + 1, &
       i_EXPO_CZM                 = i_IQS_EXPO_CZM             + 1, &
       i_IQS_EXPO_CZM_SPRING      = i_EXPO_CZM                 + 1, &
       i_EXPO_CZM_SPRING          = i_IQS_EXPO_CZM_SPRING      + 1, &
       i_IQS_EXPO_CZM_P           = i_EXPO_CZM_SPRING          + 1, &
       i_EXPO_CZM_P               = i_IQS_EXPO_CZM_P           + 1, &
       i_IQS_EXPO_CZM_SPRING_P    = i_EXPO_CZM_P               + 1, &  
       i_EXPO_CZM_SPRING_P        = i_IQS_EXPO_CZM_SPRING_P    + 1, &  
       i_GTN_CZM                  = i_EXPO_CZM_SPRING_P        + 1, &  ! Cohesive law based on Gurson plasticity model
       i_GTN2_CZM                 = i_GTN_CZM                  + 1, &  ! Incremental version of the GTN_CZM law
       i_TOSI_CZM                 = i_GTN2_CZM                 + 1, &
       i_TOSI_CZM_INCRE           = i_TOSI_CZM                 + 1, &
       i_ELASTIC_REPELL_CLB_adapt = i_TOSI_CZM_INCRE           + 1     ! loi elastic avec stockage de F/gp en variable interne pour pilotage PTA sncf 2023

  integer(kind=4), parameter :: total_interaction_law_number = i_ELASTIC_REPELL_CLB_adapt


  integer          , dimension(total_interaction_law_number) :: interaction_law_type_id = (/ &
      i_IQS_CLB                 , i_IQS_CLB_nosldt          , i_IQS_CLB_noslds          , i_IQS_CLB_RGR             , &
      i_IQS_DS_CLB              , i_IQS_WET_DS_CLB          , i_postGAP_IQS_MAC_CZM     , i_PLASTIC_COUPLED_DOF     , &
      i_xQS_WET_DS_CLB          , i_TEX_SOL                 , i_TEX_SOL_UNILAT          , i_GAP_SGR_CLB             , &
      i_VEL_SGR_CLB             , i_GAP_WET_DS_CLB          , i_MSMP_CZM                , i_GAP_SGR_DS_CLB          , &
      i_VEL_SGR_DS_CLB          , i_RST_CLB                 , i_RST_DS_CLB              , i_IQS_MOHR_DS_CLB         , &
      i_MAL_CZM                 , i_ELASTIC_REPELL_CLB      , i_CRITICAL_VOIGT_CLB      , i_ELASTIC_REPELL_WET_CLB  , &
      i_ELASTIC_ROD             , i_VOIGT_ROD               , i_ELASTIC_WIRE            , i_VOIGT_WIRE              , &
      i_MD_JKRs                 , i_ELASTIC_WET_NT          , i_COUPLED_DOF             , i_TANGENTIAL_COUPLED_DOF  , &
      i_NORMAL_COUPLED_DOF      , i_PERIO_DOF               , i_GAP_SGR_CLB_WEAR        , i_IQS_SGR_CLB_WEAR        , &
      i_MAC_CZM                 , i_IQS_MAC_CZM             , i_WET_CZM                 , i_GAP_MOHR_DS_CLB         , &
      i_MAC_CZM_nosldt          , i_MAC_CZM_noslds          , i_IQS_MAC_CZM_nosldt      , i_IQS_MAC_CZM_noslds      , &
      i_ER_MAC_CZM              , i_IQS_WET_CZM             , i_DEM_FIBs                , i_KV_WET                  , &
      i_VISCO_ELASTIC_REPELL_WET, i_RIGID_WIRE              , i_IQS_BW_CLB              ,                             &
      i_IQS_CAP_MOHR_DS_CLB     , i_IQS_MAL_CZM             , i_GAP_CAP_MOHR_DS_CLB     , i_BROKEN_DOF              , &
      i_IQS_CLB_g0              , i_IQS_MAP                 , i_RST_WET_CLB             , i_LJ_POTENTIAL            , &
      i_GAP_SGR_CLB_g0          , i_BRITTLE_ELASTIC_WIRE    , i_ELASTIC_REPELL_CLB_g0   ,                             &
      i_MP_CZM                  , i_MP3_CZM                 , i_MP3_CZM_THER            , i_TH_CZM                  , &
      i_IQS_TH_CZM              , i_postGAP_IQS_CZM         , i_WET_3C                  , i_VISCO_ELASTIC_REPELL_CLB, &
      i_ELASTIC_REPELL_MAC_CZM  , i_IQS_PLAS_CLB            , i_ABP_CZM                 , i_IQS_ABP_CZM             , &
      i_BRITTLE_COATING_CLB     , i_IQS_STICK               , i_GAP_SGR_STICK           , i_GAP_SGR_CLB_nosldt      , &
      i_GAP_SGR_CLB_noslds      , i_preGAP_SGR_CLB          , i_NARD_ROD                ,                             &
      i_IQS_EXPO_CZM            , i_EXPO_CZM                , i_IQS_EXPO_CZM_SPRING     , i_EXPO_CZM_SPRING         , &
      i_IQS_EXPO_CZM_P          , i_EXPO_CZM_P              , i_IQS_EXPO_CZM_SPRING_P   , i_EXPO_CZM_SPRING_P       , &       
      i_GTN_CZM                 , i_GTN2_CZM                , i_TOSI_CZM                , i_TOSI_CZM_INCRE          , &
      i_ELASTIC_REPELL_CLB_adapt  /)
  
  character(len=24), dimension(total_interaction_law_number), target, private :: interaction_law_type = (/ &
      'IQS_CLB                 ', 'IQS_CLB_nosldt          ', 'IQS_CLB_noslds          ', 'IQS_CLB_RGR             ', &
      'IQS_DS_CLB              ', 'IQS_WET_DS_CLB          ', 'postGAP_IQS_MAC_CZM     ', 'PLASTIC_COUPLED_DOF     ', &
      'xQS_WET_DS_CLB          ', 'TEX_SOL                 ', 'TEX_SOL_UNILAT          ', 'GAP_SGR_CLB             ', &
      'VEL_SGR_CLB             ', 'GAP_WET_DS_CLB          ', 'MSMP_CZM                ', 'GAP_SGR_DS_CLB          ', &
      'VEL_SGR_DS_CLB          ', 'RST_CLB                 ', 'RST_DS_CLB              ', 'IQS_MOHR_DS_CLB         ', &
      'MAL_CZM                 ', 'ELASTIC_REPELL_CLB      ', 'CRITICAL_VOIGT_CLB      ', 'ELASTIC_REPELL_WET_CLB  ', &
      'ELASTIC_ROD             ', 'VOIGT_ROD               ', 'ELASTIC_WIRE            ', 'VOIGT_WIRE              ', &
      'MD_JKRs                 ', 'ELASTIC_WET_NT          ', 'COUPLED_DOF             ', 'TANGENTIAL_COUPLED_DOF  ', &
      'NORMAL_COUPLED_DOF      ', 'PERIO_DOF               ', 'GAP_SGR_CLB_WEAR        ', 'IQS_SGR_CLB_WEAR        ', &
      'MAC_CZM                 ', 'IQS_MAC_CZM             ', 'WET_CZM                 ', 'GAP_MOHR_DS_CLB         ', &
      'MAC_CZM_nosldt          ', 'MAC_CZM_noslds          ', 'IQS_MAC_CZM_nosldt      ', 'IQS_MAC_CZM_noslds      ', &
      'ER_MAC_CZM              ', 'IQS_WET_CZM             ', 'DEM_FIBs                ', 'KV_WET                  ', &
      'VISCO_ELASTIC_REPELL_WET', 'RIGID_WIRE              ', 'IQS_BW_CLB              ',                             &
      'IQS_CAP_MOHR_DS_CLB     ', 'IQS_MAL_CZM             ', 'GAP_CAP_MOHR_DS_CLB     ', 'BROKEN_DOF              ', &
      'IQS_CLB_g0              ', 'IQS_MAP                 ', 'RST_WET_CLB             ', 'LJ_POTENTIAL            ', &
      'GAP_SGR_CLB_g0          ', 'BRITTLE_ELASTIC_WIRE    ', 'ELASTIC_REPELL_CLB_g0   ',                             &
      'MP_CZM                  ', 'MP3_CZM                 ', 'MP3_CZM_THER            ', 'TH_CZM                  ', &
      'IQS_TH_CZM              ', 'postGAP_IQS_CZM         ', 'WET_3C                  ', 'VISCO_ELASTIC_REPELL_CLB', &
      'ELASTIC_REPELL_MAC_CZM  ', 'IQS_PLAS_CLB            ', 'ABP_CZM                 ', 'IQS_ABP_CZM             ', &
      'BRITTLE_COATING_CLB     ', 'IQS_STICK               ', 'GAP_SGR_STICK           ', 'GAP_SGR_CLB_nosldt      ', &
      'GAP_SGR_CLB_noslds      ', 'preGAP_SGR_CLB          ', 'NARD_ROD                ',                             &
      'IQS_EXPO_CZM            ', 'EXPO_CZM                ', 'IQS_EXPO_CZM_SPRING     ', 'EXPO_CZM_SPRING         ', &
      'IQS_EXPO_CZM_P          ', 'EXPO_CZM_P              ', 'IQS_EXPO_CZM_SPRING_P   ', 'EXPO_CZM_SPRING_P       ', &
      'GTN_CZM                 ', 'GTN2_CZM                ', 'TOSI_CZM                ', 'TOSI_CZM_INCRE          ', &
      'ELASTIC_REPELL_CLB_adapt'  /)

  integer(kind=4), parameter :: integrator_moreau  = 1                    , &
                                integrator_gear    = integrator_moreau + 1, &
                                integrator_verlet  = integrator_gear   + 1, &
                                integrator_newmark = integrator_verlet + 1, &
                                integrator_beta2   = integrator_newmark+ 1, &
                                integrator_qs      = integrator_beta2  + 1

  integer         , dimension(6) :: integrator_type_id = (/ integrator_moreau , integrator_gear , integrator_verlet, &
                                                            integrator_newmark, integrator_beta2, integrator_qs     /)
  character(len=7), dimension(6), target, private :: integrator_type = (/ 'MOREAU ', 'GEAR   ', 'VERLET ', &
                                                                          'NEWMARK', 'BETA2  ', 'QS     ' /)
  
  !fd attention la valeur prise par ces parameters est importante
  ! car elle fixe le nombre de ddl du noeud => ne pas toucher !
  integer(kind=4), parameter :: i_NO1xx = 1          , &
                                i_NO2xx = i_NO1xx + 1, &
                                i_NO3xx = i_NO2xx + 1, &
                                i_NO4xx = i_NO3xx + 1, &
                                i_NO5xx = i_NO4xx + 1, &
                                i_NO6xx = i_NO5xx + 1

  integer         , dimension(6) :: node_type_id = (/ i_NO1xx, i_NO2xx, i_NO3xx, &
                                                      i_NO4xx, i_NO5xx, i_NO6xx  /)
  character(len=5), dimension(6), target, private :: node_type = (/ 'NO1xx', 'NO2xx', 'NO3xx', &
                                                                    'NO4xx', 'NO5xx', 'NO6xx' /)

  !> for vloc_rloc format reading... to tiring to choose a name with a meaning
  integer(kind=4), parameter :: header_type_1 = 1, &
                                header_type_2 = 2, &
                                header_type_3 = 3

  !> dimension mode id
  integer(kind=4), parameter :: i_2D_strain = 1              , &
                                i_2D_stress = i_2D_strain + 1, &
                                i_2D_axisym = i_2D_stress + 1, &
                                i_3D        = i_2D_axisym + 1
                                
  integer          , dimension(4) :: dime_mode_type_id = (/ i_2D_strain, i_2D_stress, &
                                                            i_2D_axisym, i_3D          /)
  character(len=10), dimension(4), target, private :: dime_mode_type = (/ '2D PSTRAIN', '2D PSTRESS', &
                                                                          '2D AXI    ', '3D        ' /)

  ! map between bodies vectors and integer identifier
  integer(kind=4), parameter :: iVbeg_ = 1          , &
                                iVfree = iVbeg_ + 1 , &
                                iVaux_ = iVfree + 1 , &
                                iVddm_ = iVaux_ + 1 , &
                                iV____ = iVddm_ + 1 , &
                                iXprev = iV____ + 1 , &
                                iXbeg_ = iXprev + 1 , &
                                iX____ = iXbeg_ + 1 , &
                                iIreac = iX____ + 1 , &
                                iIaux_ = iIreac + 1 , &
                                iReac_ = iIaux_ + 1 , &
                                iRaux_ = iReac_ + 1 , &
                                iRfree = iRaux_ + 1 , &
                                iFint_ = iRfree + 1 , &
                                iFext_ = iFint_ + 1 , &
                                iCoor_ = iFext_ + 1 , &
                                iCoorb = iCoor_ + 1 , &
                                iCoor0 = iCoorb + 1

  integer         , dimension(18) :: vector_type_id = (/ iVbeg_, iVfree, iVaux_, iVddm_, &
                                                         iV____, iXprev, iXbeg_, iX____, &
                                                         iIreac, iIaux_, iReac_, iRaux_, &
                                                         iRfree, iFint_, iFext_, &
                                                         iCoor_, iCoorb, iCoor0 /)
  character(len=5), dimension(18), target, private :: vector_type = (/ 'Vbeg_', 'Vfree', 'Vaux_', 'Vddm_', &
                                                                       'V____', 'Xprev', 'Xbeg_', 'X____', &
                                                                       'Ireac', 'Iaux_', 'Reac_', 'Raux_', &
                                                                       'Rfree', 'Fint_', 'Fext_', &
                                                                       'Coor_', 'Coorb', 'Coor0' /)


  ! status during the contact resolution
  ! map between contact status and integer identifier
  integer(kind=4), parameter :: i_vnish = 1           , &
                                i_nknow = i_vnish + 1 , &
                                i_noctc = i_nknow + 1 , &
                                i_stick = i_noctc + 1 , &
                                i_slide = i_stick + 1 , &
                                i_slibw = i_slide + 1 , &
                                i_slifw = i_slibw + 1 , &
                                i_Cnnow = i_slifw + 1 , & ! C stands for cohesive
                                i_Cnctc = i_Cnnow + 1 , &
                                i_Cstck = i_Cnctc + 1 , &
                                i_Cslid = i_Cstck + 1 , &
                                i_Cslbw = i_Cslid + 1 , &
                                i_Cslfw = i_Cslbw + 1 , &
                                i_Wnnow = i_Cslfw + 1 , & ! W stands for wet
                                i_Wnctc = i_Wnnow + 1 , &
                                i_Wstck = i_Wnctc + 1 , &
                                i_Wslid = i_Wstck + 1 , &
                                i_Wslbw = i_Wslid + 1 , &
                                i_Wslfw = i_Wslbw + 1 , &
                                i_Mnnow = i_Wslfw + 1 , & ! M stands for Mohr-Coulomb
                                i_Mnctc = i_Mnnow + 1 , &
                                i_Mstck = i_Mnctc + 1 , &
                                i_Mslid = i_Mstck + 1 , &
                                i_Mslbw = i_Mslid + 1 , &
                                i_Mslfw = i_Mslbw + 1 , &
                                i_Ennow = i_Mslfw + 1 , & ! E stands for ? (mp_solver)
                                i_Enctc = i_Ennow + 1 , &
                                i_Estck = i_Enctc + 1 , &
                                i_Eslid = i_Estck + 1 , &
                                i_Eslbw = i_Eslid + 1 , &
                                i_Eslfw = i_Eslbw + 1 , &
                                i__RGR_ = i_Eslfw + 1 , &
                                i_Ssstt = i__RGR_ + 1 , & ! cpg 3D for pyramidal friction cone
                                i_Ssptt = i_Ssstt + 1 , & ! t/s p=+ m=-
                                i_Ssmtt = i_Ssptt + 1 , &
                                i_Ssstp = i_Ssmtt + 1 , &
                                i_Ssptp = i_Ssstp + 1 , &
                                i_Ssmtp = i_Ssptp + 1 , &
                                i_Ssstm = i_Ssmtp + 1 , &
                                i_Ssptm = i_Ssstm + 1 , &
                                i_Ssmtm = i_Ssptm + 1 

  integer         , dimension(41) :: contact_status_id = (/ i_vnish, &
                                                            i_nknow, i_noctc, i_stick, i_slide, i_slibw, i_slifw, &
                                                            i_Cnnow, i_Cnctc, i_Cstck, i_Cslid, i_Cslbw, i_Cslfw, &
                                                            i_Wnnow, i_Wnctc, i_Wstck, i_Wslid, i_Wslbw, i_Wslfw, &
                                                            i_Mnnow, i_Mnctc, i_Mstck, i_Mslid, i_Mslbw, i_Mslfw, &
                                                            i_Ennow, i_Enctc, i_Estck, i_Eslid, i_Eslbw, i_Eslfw, &
                                                            i__RGR_, i_Ssstt, i_Ssptt, i_Ssmtt, i_Ssstp, i_Ssptp, &
                                                            i_Ssmtp, i_Ssstm, i_Ssptm, i_Ssmtm                   /)
  character(len=5), dimension(41), target, private :: contact_status = (/ 'vnish', &
                                                                          'nknow', 'noctc', 'stick', 'slide', 'slibw', 'slifw', &
                                                                          'Cnnow', 'Cnctc', 'Cstck', 'Cslid', 'Cslbw', 'Cslfw', &
                                                                          'Wnnow', 'Wnctc', 'Wstck', 'Wslid', 'Wslbw', 'Wslfw', &
                                                                          'Mnnow', 'Mnctc', 'Mstck', 'Mslid', 'Mslbw', 'Mslfw', &
                                                                          'Ennow', 'Enctc', 'Estck', 'Eslid', 'Eslbw', 'Eslfw', &
                                                                          '_RGR_', 'Ssstt', 'Ssptt', 'Ssmtt', 'Ssstp', 'Ssptp', &
                                                                          'Ssmtp', 'Ssstm', 'Ssptm', 'Ssmtm' /)

  private check_parameter_ids_                , &
          parameter_error_                    , &
          check_physic_type_names_            , &
          check_body_model_names_             , &
          check_contactor_names_              , &
          check_interaction_names_            , &
          check_matrix_storage_names_         , &
          check_matrix_shape_names_           , &
          check_generalized_coordinates_names_, &
          check_surface_energy_status_names_  , &
          check_inter_law_names_              , &
          check_integrator_names_             , &
          check_node_names_                   , &
          check_dime_mode_names_              , &
          check_body_vector_names_            , &
          check_contact_status_names_

  public get_index_from_string                   , &
         get_physic_type_id_from_name            , &
         get_physic_type_name_from_id            , &
         get_physic_type_names                   , &
         get_body_model_id_from_name             , &
         get_body_model_name_from_id             , &
         get_body_model_names                    , &
         get_contactor_id_from_name              , &
         get_contactor_name_from_id              , &
         get_contactor_names                     , &
         get_interaction_id_from_name            , &
         get_interaction_name_from_id            , &
         get_interaction_names                   , &
         get_interaction_type_id                 , &
         get_matrix_storage_id_from_name         , &
         get_matrix_storage_name_from_id         , &
         get_matrix_storage_names                , &
         get_matrix_shape_id_from_name           , &
         get_matrix_shape_name_from_id           , &
         get_matrix_shape_names                  , &
         get_generalized_coordinates_id_from_name, &
         get_generalized_coordinates_name_from_id, &
         get_generalized_coordinates_names       , &
         get_surface_energy_status_id_from_name  , &
         get_surface_energy_status_name_from_id  , &
         get_surface_energy_status_names         , &
         get_inter_law_id_from_name              , &
         get_inter_law_name_from_id              , &
         get_inter_law_names                     , &
         get_integrator_id_from_name             , &
         get_integrator_name_from_id             , &
         get_integrator_names                    , &
         get_node_id_from_name                   , &
         get_node_name_from_id                   , &
         get_node_names                          , &
         get_dime_mode_id_from_name              , &
         get_dime_mode_name_from_id              , &
         get_dime_mode_names                     , &
         get_body_vector_id_from_name            , &
         get_body_vector_name_from_id            , &
         get_body_vector_names                   , &
         get_contact_status_id_from_name         , &
         get_contact_status_name_from_id         , &
         get_contact_status_names                , &
         check_all_parameters

  contains

  !> \brief Get the index of a string in a string_array
  !> \todo : correct checks and error message
  function get_index_from_string(string, string_array, array_size)
    implicit none
    !> [in] string to look for in the array
    character(len=*),               intent(in) :: string
    !> [in] string_array to look into
    character(len=*), dimension(*), intent(in) :: string_array
    !> [in] size of the string in the array
    integer(kind=4) :: array_size
    !> [return] index of the string in the array
    integer(kind=4) :: get_index_from_string
    !
    integer(kind=4) :: i

    get_index_from_string = -99

    do i = 1, array_size
      if( trim(string_array(i)) == trim(string) ) then
        get_index_from_string = i
        return
      end if
    end do

  end function

  !> \brief [private] Check there is no error in the filling of parameter id array
  subroutine check_parameter_ids_(id_array, name_array, nb)
    implicit none
    integer         , dimension(nb), intent(in) :: id_array
    character(len=*), dimension(nb), intent(in) :: name_array
    integer                        , intent(in) :: nb
    !
    integer :: i, n, error

    error = 0


    do i = 1, nb

      if( id_array(i) /= i ) then
        write(0,'(A)')             "This is really bad, the parameters module is messed up !"
        write(0,'(A,A,A,I0,A,I0)') "Parameter name ", name_array(i), " of index ", i, &
                                   " is associated with value ", id_array(i)
        stop 1
      end if

    end do

  end subroutine check_parameter_ids_

  !> \brief [private] Write an error message when checking name consistency
  subroutine parameter_error_( param_name )
    implicit none
    character(len=*), intent(in) :: param_name
      write(0,'(A)')   "This is really bad, the parameters module is messed up !"
      write(0,'(A,A)') "Check parameter consistency of ", param_name
      stop 1
  end subroutine parameter_error_

  !---------------------------------------------------------------------------------------!
  !> \brief Get physic type identifier from name
  !> Possible physic types are : mecax, therx, porox, multi
  function get_physic_type_id_from_name(i_type)
    implicit none
    !> [in] physic type
    character(len=5), intent(in) :: i_type
    !> [return] physic id
    integer(kind=4)              :: get_physic_type_id_from_name

    get_physic_type_id_from_name = get_index_from_string(i_type,physic_type,size(physic_type))

  end function

  !> \brief Get physic type name from its integer id
  function get_physic_type_name_from_id(id)
    implicit none
    !> [in] physic id
    integer(kind=4), intent(in) :: id
    !> [return] physic type
    character(len=5)            :: get_physic_type_name_from_id

    if( id > 0 .and. id <= size(physic_type) ) then
      get_physic_type_name_from_id = physic_type(id)
    else
      get_physic_type_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of physic_type
  function get_physic_type_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_physic_type_names

    get_physic_type_names => physic_type

  end function

  !> \brief [private] Check name consistency of physic_type names
  subroutine check_physic_type_names_()
    implicit none

    if( get_physic_type_id_from_name( 'mecax' ) /= i_mecax ) call parameter_error_('mecax')
    if( get_physic_type_id_from_name( 'therx' ) /= i_therx ) call parameter_error_('therx')
    if( get_physic_type_id_from_name( 'porox' ) /= i_porox ) call parameter_error_('porox')
    if( get_physic_type_id_from_name( 'multi' ) /= i_multi ) call parameter_error_('multi')

  end subroutine check_physic_type_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get body model identifier from name
  !> Possible body modles are : RBDY2, RBDY3, MAILx, MBS2D and MBS3D
  function get_body_model_id_from_name(i_model)
    implicit none
    !> [in] body type
    character(len=5), intent(in) :: i_model
    !> [return] body id
    integer(kind=4)              :: get_body_model_id_from_name

    get_body_model_id_from_name = get_index_from_string(i_model, body_model, size(body_model))

  end function

  !> \brief Get body model name from its integer id
  function get_body_model_name_from_id(id)
    implicit none
    !> [in] body id
    integer(kind=4), intent(in) :: id
    !> [return] body type
    character(len=5)            :: get_body_model_name_from_id

    if( id > 0 .and. id <= size(body_model) ) then
      get_body_model_name_from_id = body_model(id)
    else
      get_body_model_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of body_model
  function get_body_model_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_body_model_names

    get_body_model_names => body_model

  end function

  !> \brief [private] Check name consistency of body_model names
  subroutine check_body_model_names_()
    implicit none

    if( get_body_model_id_from_name( 'RBDY2' ) /= i_rbdy2 ) call parameter_error_('RBDY2')
    if( get_body_model_id_from_name( 'RBDY3' ) /= i_rbdy3 ) call parameter_error_('RBDY3')
    if( get_body_model_id_from_name( 'MAILx' ) /= i_mailx ) call parameter_error_('MAILx')
    if( get_body_model_id_from_name( 'MBS3D' ) /= i_mbs3  ) call parameter_error_('MBS3D')
    if( get_body_model_id_from_name( 'MBS2D' ) /= i_mbs2  ) call parameter_error_('MBS2D')

  end subroutine check_body_model_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get integer identifier from contactor type
  !> Contactors possible types are : DISKx, xKSID, POLYG, 
  !> JONCx, PT2Dx, SPHER, CYLND, DNLYC, POLYR, PLANx, PT3Dx
  !> ALpxx, CLxxx, ASpx3, ASpx4, CSxx3, CSxx4, DISKL, PT2DL
  function get_contactor_id_from_name(i_type)
    implicit none
    !> [in] contactor type
    character(len=5), intent(in) :: i_type
    !< [return] integer id
    integer(kind=4)              :: get_contactor_id_from_name

    get_contactor_id_from_name = get_index_from_string(i_type,contactor_type,size(contactor_type))
    !print *,'[get_contactor_id_from_name] : unknown contactor type : ', type

  end function

  !> \brief Get contactor type from its integer id
  function get_contactor_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] contactor type
    character(len=5)            :: get_contactor_name_from_id

    if( id > 0 .and. id <= size(contactor_type) ) then
      get_contactor_name_from_id = contactor_type(id)
    else
      !print *,'[get_contactor_name_from_id] : unknown contactor type identifier : ', id
      get_contactor_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of contactor_type
  function get_contactor_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_contactor_names

    get_contactor_names => contactor_type

  end function

  !> \brief [private] Check name consistency of contactor names
  subroutine check_contactor_names_()
    implicit none

    if( get_contactor_id_from_name('DISKx') /= i_diskx ) call parameter_error_('DISKx')
    if( get_contactor_id_from_name('xKSID') /= i_xksid ) call parameter_error_('xKSID')
    if( get_contactor_id_from_name('POLYG') /= i_polyg ) call parameter_error_('POLYG')
    if( get_contactor_id_from_name('JONCx') /= i_joncx ) call parameter_error_('JONCx')
    if( get_contactor_id_from_name('PT2Dx') /= i_pt2dx ) call parameter_error_('PT2Dx')
    if( get_contactor_id_from_name('SPHER') /= i_spher ) call parameter_error_('SPHER')
    if( get_contactor_id_from_name('CYLND') /= i_cylnd ) call parameter_error_('CYLND')
    if( get_contactor_id_from_name('DNLYC') /= i_dnlyc ) call parameter_error_('DNLYC')
    if( get_contactor_id_from_name('POLYR') /= i_polyr ) call parameter_error_('POLYR')
    if( get_contactor_id_from_name('PLANx') /= i_planx ) call parameter_error_('PLANx')
    if( get_contactor_id_from_name('PT3Dx') /= i_pt3dx ) call parameter_error_('PT3Dx')
    if( get_contactor_id_from_name('ALpxx') /= i_alpxx ) call parameter_error_('ALpxx')
    if( get_contactor_id_from_name('CLxxx') /= i_clxxx ) call parameter_error_('CLxxx')
    if( get_contactor_id_from_name('DISKL') /= i_diskl ) call parameter_error_('DISKL')
    if( get_contactor_id_from_name('PT2DL') /= i_pt2dl ) call parameter_error_('PT2DL')
    if( get_contactor_id_from_name('ASpxx') /= i_aspxx ) call parameter_error_('ASpxx')
    if( get_contactor_id_from_name('CSxxx') /= i_csxxx ) call parameter_error_('CSxxx')

  end subroutine check_contactor_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get integer identifier from interaction type
  !> Contactors possible types are : DKDKx, DKKDx, DKPLx,
  !> DKJCx, DKALp, DKDKL, PLPLx, PLJCx,
  !> PLALp, PTPT2, P2P2L, SPSPx, SPCDx, SPDCx, SPPLx, SPPRx, CDCDx,
  !> CDPLx, PRPRx, PRPLx, PRASp, PTPT3, CLALp, CLJCx, CSASp,
  !> CSPRx,
  function get_interaction_id_from_name(i_type)
    implicit none
    !> [in] interaction type
    character(len=5), intent(in) :: i_type
    !> [return] integer id
    integer(kind=4) :: get_interaction_id_from_name

    get_interaction_id_from_name = get_index_from_string(i_type,interaction_type,size(interaction_type))
    !print *,'[get_interaction_id_from_name] : unknown interaction type : ', type

  end function

  !> \brief Get interaction type from its integer id
  function get_interaction_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] interaction type
    character(len=5) :: get_interaction_name_from_id

    if( id > 0 .and. id <= size(interaction_type) ) then
      get_interaction_name_from_id = interaction_type(id)
    else
      get_interaction_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of interaction_type
  function get_interaction_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_interaction_names

    get_interaction_names => interaction_type

  end function

  !> \brief [private] Check name consistency of interaction names
  subroutine check_interaction_names_()
    implicit none

    if( get_interaction_id_from_name('DKDKx') /= i_dkdkx ) call parameter_error_('DKDKx')
    if( get_interaction_id_from_name('DKKDx') /= i_dkkdx ) call parameter_error_('DKKDx')
    if( get_interaction_id_from_name('DKPLx') /= i_dkplx ) call parameter_error_('DKPLx')
    if( get_interaction_id_from_name('DKJCx') /= i_dkjcx ) call parameter_error_('DKJCx')
    if( get_interaction_id_from_name('DKALp') /= i_dkalp ) call parameter_error_('DKALp')
    if( get_interaction_id_from_name('DKDKL') /= i_dkdkl ) call parameter_error_('DKDKL')
    if( get_interaction_id_from_name('PLPLx') /= i_plplx ) call parameter_error_('PLPLx')
    if( get_interaction_id_from_name('PLJCx') /= i_pljcx ) call parameter_error_('PLJCx')
    if( get_interaction_id_from_name('PLALp') /= i_plalp ) call parameter_error_('PLALp')
    if( get_interaction_id_from_name('PTPT2') /= i_ptpt2 ) call parameter_error_('PTPT2')
    if( get_interaction_id_from_name('P2P2L') /= i_p2p2l ) call parameter_error_('P2P2L')
    if( get_interaction_id_from_name('SPSPx') /= i_spspx ) call parameter_error_('SPSPx')
    if( get_interaction_id_from_name('SPCDx') /= i_spcdx ) call parameter_error_('SPCDx')
    if( get_interaction_id_from_name('SPDCx') /= i_spdcx ) call parameter_error_('SPDCx')
    if( get_interaction_id_from_name('SPPLx') /= i_spplx ) call parameter_error_('SPPLx')
    if( get_interaction_id_from_name('SPPRx') /= i_spprx ) call parameter_error_('SPPRx')    
    if( get_interaction_id_from_name('CDCDx') /= i_cdcdx ) call parameter_error_('CDCDx')
    if( get_interaction_id_from_name('CDPLx') /= i_cdplx ) call parameter_error_('CDPLx')
    if( get_interaction_id_from_name('PRPRx') /= i_prprx ) call parameter_error_('PRPRx')
    if( get_interaction_id_from_name('PRPLx') /= i_prplx ) call parameter_error_('PRPLx')
    if( get_interaction_id_from_name('PRASp') /= i_prasp ) call parameter_error_('PRASp')
    if( get_interaction_id_from_name('PTPT3') /= i_ptpt3 ) call parameter_error_('PTPT3')
    if( get_interaction_id_from_name('CLALp') /= i_clalp ) call parameter_error_('CLALp')
    if( get_interaction_id_from_name('CLJCx') /= i_cljcx ) call parameter_error_('CLJCx')
    if( get_interaction_id_from_name('CSASp') /= i_csasp ) call parameter_error_('CSASp')
    if( get_interaction_id_from_name('CSPRx') /= i_csprx ) call parameter_error_('CSPRx')

  end subroutine check_interaction_names_

  !> \brief Get the id of the contact type from the id of the candidat and the antagonist contactors
  function get_interaction_type_id( cd_geo, an_geo )
    implicit none
    !> [in] contactor candidat geometric id
    integer(kind=4), intent(in) :: cd_geo
    !> [in] contactor antagonis geometric id
    integer(kind=4), intent(in) :: an_geo
    !> [return] contact type id
    integer(kind=4) :: get_interaction_type_id

    select case( cd_geo )
    case( i_diskx )
      select case( an_geo )
      case( i_diskx )
        get_interaction_type_id = i_dkdkx; return
      case( i_xksid )
        get_interaction_type_id = i_dkkdx; return
      case( i_polyg )
        get_interaction_type_id = i_dkplx; return
      case( i_joncx )
        get_interaction_type_id = i_dkjcx; return
      case( i_alpxx )
        get_interaction_type_id = i_dkalp; return
      case( i_diskl )
        get_interaction_type_id = i_dkdkl; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_xksid )
      select case( an_geo )
      case( i_diskx )
        get_interaction_type_id = i_dkdkx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_polyg )
      select case( an_geo )
      case( i_diskx )
        get_interaction_type_id = i_dkplx; return
      case( i_polyg )
        get_interaction_type_id = i_plplx; return
      case( i_joncx )
        get_interaction_type_id = i_pljcx; return
      case( i_alpxx )
        get_interaction_type_id = i_plalp; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_joncx )
      select case( an_geo )
      case( i_diskx )
        get_interaction_type_id = i_dkjcx; return
      case( i_polyg )
        get_interaction_type_id = i_pljcx; return
      case( i_clxxx )
        get_interaction_type_id = i_cljcx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_pt2dx )
      select case( an_geo )
      case( i_pt2dx )
        get_interaction_type_id = i_ptpt2; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_pt2dl )
      select case( an_geo )
      case( i_pt2dl )
        get_interaction_type_id = i_p2p2l; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_spher )
      select case( an_geo )
      case( i_spher )
        get_interaction_type_id = i_spspx; return
      case( i_cylnd )
        get_interaction_type_id = i_spcdx; return
      case( i_dnlyc )
        get_interaction_type_id = i_spdcx; return
      case( i_planx )
        get_interaction_type_id = i_spplx; return
      case( i_polyr )
        get_interaction_type_id = i_spprx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_cylnd )
      select case( an_geo )
      case( i_spher )
        get_interaction_type_id = i_spcdx; return
      case( i_cylnd )
        get_interaction_type_id = i_cdcdx; return
      case( i_planx )
        get_interaction_type_id = i_cdplx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_dnlyc )
      select case( an_geo )
      case( i_spher )
        get_interaction_type_id = i_spdcx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_polyr )
      select case( an_geo )
      case( i_polyr )
        get_interaction_type_id = i_prprx; return
      case( i_planx )
        get_interaction_type_id = i_prplx; return
      case( i_spher )
        get_interaction_type_id = i_spprx; return
      case( i_aspxx )
        get_interaction_type_id = i_prasp; return
      case( i_csxxx )
        get_interaction_type_id = i_csprx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_planx )
      select case( an_geo )
      case( i_spher )
        get_interaction_type_id = i_spplx; return
      case( i_cylnd )
        get_interaction_type_id = i_cdplx; return
      case( i_polyr )
        get_interaction_type_id = i_prplx; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_pt3dx )
      select case( an_geo )
      case( i_pt3dx )
        get_interaction_type_id = i_ptpt3; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_alpxx )
      select case( an_geo )
      case( i_diskx )
        get_interaction_type_id = i_dkalp; return
      case( i_polyg )
        get_interaction_type_id = i_plalp; return
      case( i_clxxx )
        get_interaction_type_id = i_clalp; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_aspxx )
      select case( an_geo )
      case( i_polyr )
        get_interaction_type_id = i_prasp; return
      case( i_csxxx )
        get_interaction_type_id = i_csasp; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_clxxx )
      select case( an_geo )
      case( i_joncx )
        get_interaction_type_id = i_cljcx; return
      case( i_alpxx )
        get_interaction_type_id = i_clalp; return
      case default
        print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case( i_csxxx )
      select case( an_geo )
      case( i_polyr )
        get_interaction_type_id = i_csprx; return
      case( i_aspxx )
        get_interaction_type_id = i_csasp; return
      case default
        !print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
      end select
    case default
        !print *,'[get_interaction_type_id] : contact detection not implemented for these contactors : ', cd_geo, an_geo
        get_interaction_type_id = -99
    end select

  end function

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get matrix storage type integer id
  !> Matrix storage possible types are : dense, sparse, expolded
  function get_matrix_storage_id_from_name(storage)
    implicit none
    !> [in] matrix storage type
    character(len=8), intent(in) :: storage
    !> [return] integer id
    integer(kind=4)              :: get_matrix_storage_id_from_name
    !
    character(len=80) :: cout
    character(len=43) :: IAM
    !      1234567890123456789012345678901234567890123
    IAM = 'parameters::get_matrix_storage_id_from_type'

    get_matrix_storage_id_from_name = get_index_from_string(storage,matrix_storage,size(matrix_storage))
    !write(cout,'(A,1x,A)') 'unknown storage :', storage
    !write(*,'['//IAM//']::'//cout)

  end function

  !> \brief Get matrix storage type from integer id
  !> Matrix storage possible types are : dense, sparse, expolded
  function get_matrix_storage_name_from_id(storage_id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: storage_id
    !> [return] matrix storage type
    character(len=8)            :: get_matrix_storage_name_from_id 
    !
    character(len=80) :: cout
    character(len=43) :: IAM
    !      1234567890123456789012345678901234567890123
    IAM = 'parameters::get_matrix_storage_name_from_id'

    if( storage_id > 0 .and. storage_id <= size(matrix_storage) ) then
      get_matrix_storage_name_from_id = matrix_storage(storage_id)
    else
      get_matrix_storage_name_from_id = 'xxxxxxxx'
    end if

  end function

  !> \brief Get list of parameter names of matrix_storage
  function get_matrix_storage_names()
    implicit none
    character(len=8), dimension(:), pointer :: get_matrix_storage_names

    get_matrix_storage_names => matrix_storage

  end function

  !> \brief [private] Check name consistency of matrix_storage names
  subroutine check_matrix_storage_names_()
    implicit none

    if( get_matrix_storage_id_from_name('diagonal') /= i_diagonal  ) call parameter_error_('diagonal')
    if( get_matrix_storage_id_from_name('sparse__') /= i_sparse    ) call parameter_error_('sparse__')
    if( get_matrix_storage_id_from_name('band____') /= i_band      ) call parameter_error_('band____')
    if( get_matrix_storage_id_from_name('skyline_') /= i_skyline   ) call parameter_error_('skyline_')
    if( get_matrix_storage_id_from_name('full____') /= i_full      ) call parameter_error_('full____')
    if( get_matrix_storage_id_from_name('exploded') /= i_exploded  ) call parameter_error_('exploded')

  end subroutine check_matrix_storage_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get matrix shape type integer id
  !> Matrix shape possible types are : symetric or full
  function get_matrix_shape_id_from_name(i_shape)
    implicit none
    !> [in] matrix shape type
    character(len=8), intent(in) :: i_shape
    !> [return] integer id
    integer(kind=4)              :: get_matrix_shape_id_from_name 
    !
    character(len=80) :: cout
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = 'parameters::get_matrix_shape_id_from_type'

    get_matrix_shape_id_from_name = get_index_from_string(i_shape,matrix_shape,size(matrix_shape))
    !write(cout,'(A,1x,A)') 'unknown shape :', shape
    !write(*,'['//IAM//']::'//cout)

  end function

  !> \brief Get matrix shape type from integer id
  !> Matrix shape possible types are : symetric or full
  function get_matrix_shape_name_from_id(shape_id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: shape_id
    !> [return] matrix shape type
    character(len=8)            :: get_matrix_shape_name_from_id 
    !
    character(len=80) :: cout
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = 'parameters::get_matrix_shape_name_from_id'

    if( shape_id > 0 .and. shape_id <= size(matrix_shape) ) then
      get_matrix_shape_name_from_id = matrix_shape(shape_id)
    else
      get_matrix_shape_name_from_id = 'xxxxxxxx'
    end if

  end function

  !> \brief Get list of parameter names of matrix_shape
  function get_matrix_shape_names()
    implicit none
    character(len=8), dimension(:), pointer :: get_matrix_shape_names

    get_matrix_shape_names => matrix_shape

  end function

  !> \brief [private] Check name consistency of matrix_shape names
  subroutine check_matrix_shape_names_()
    implicit none

    if( get_matrix_shape_id_from_name('sym_____') /= i_sym  ) call parameter_error_('sym_____')
    if( get_matrix_shape_id_from_name('std_____') /= i_std  ) call parameter_error_('std_____')

  end subroutine check_matrix_shape_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get the integer id of a generalized coordinates type
  !> Possible generalized coordinates type are : natural, newton_euler_2d
  !> or newton_euler_3d
  function get_generalized_coordinates_id_from_name(i_type)
    implicit none
    !> [in] generalized coordinates type
    character(len=15), intent(in) :: i_type
    !> [return] integer id
    integer(kind=4)               :: get_generalized_coordinates_id_from_name

    get_generalized_coordinates_id_from_name = get_index_from_string(i_type,generalized_coordinates,size(generalized_coordinates))
    !print *,'[get_generalized_coordinates_type_id] : unknown type : ', type

  end function

  !> \brief Get the generalized coordinates type from integer id
  function get_generalized_coordinates_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] generalized coordinates type
    character(len=15)           :: get_generalized_coordinates_name_from_id

    if( id > 0 .and. id <= size(generalized_coordinates) ) then
      get_generalized_coordinates_name_from_id = generalized_coordinates(id)
    else
      get_generalized_coordinates_name_from_id = 'xxxxxxxxxxxxxxx'
    end if

  end function

  !> \brief Get list of parameter names of generalized_coordinates
  function get_generalized_coordinates_names()
    implicit none
    character(len=15), dimension(:), pointer :: get_generalized_coordinates_names

    get_generalized_coordinates_names => generalized_coordinates

  end function

  !> \brief [private] Check name consistency of generalized_coordinates names
  subroutine check_generalized_coordinates_names_()
    implicit none

    if( get_generalized_coordinates_id_from_name('natural        ') /= i_natural          ) call parameter_error_('natural'        )
    if( get_generalized_coordinates_id_from_name('newton_euler_2d') /= i_newton_euler_2d  ) call parameter_error_('newton_euler_2d')
    if( get_generalized_coordinates_id_from_name('newton_euler_3d') /= i_newton_euler_3d  ) call parameter_error_('newton_euler_3d')

  end subroutine check_generalized_coordinates_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get integer identifier from a surface energy status name
  function get_surface_energy_status_id_from_name(i_type)
    implicit none
    !> [in] interaction law type name
    character(len=10), intent(in) :: i_type
    !> [return] integer id
    integer(kind=4)               :: get_surface_energy_status_id_from_name

    get_surface_energy_status_id_from_name = get_index_from_string(i_type,surface_energy_status,size(surface_energy_status))

  end function

  !> \brief Get surface energy status name from its integer id
  function get_surface_energy_status_name_from_id(id)
    implicit none
    !> [in] node id
    integer(kind=4), intent(in) :: id
    !> [return] interaction law name
    character(len=10)           :: get_surface_energy_status_name_from_id

    if( id > 0 .and. id <= size(surface_energy_status) ) then
      get_surface_energy_status_name_from_id = surface_energy_status(id)
    else
      get_surface_energy_status_name_from_id = 'xxxxxxxxxx'
    end if

  end function

  !> \brief Get list of parameter names of surface_energy_status
  function get_surface_energy_status_names()
    implicit none
    character(len=10), dimension(:), pointer :: get_surface_energy_status_names

    get_surface_energy_status_names => surface_energy_status

  end function

  !> \brief [private] Check name consistency of surface_energy_status names
  subroutine check_surface_energy_status_names_()
    implicit none

    if( get_surface_energy_status_id_from_name('undefined ') /= i_undefined   ) call parameter_error_('undefined' )
    if( get_surface_energy_status_id_from_name('stationary') /= i_stationary  ) call parameter_error_('stationary')
    if( get_surface_energy_status_id_from_name('sheared   ') /= i_sheared     ) call parameter_error_('sheared'   )
    if( get_surface_energy_status_id_from_name('free      ') /= i_free        ) call parameter_error_('free'      )

  end subroutine check_surface_energy_status_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get integer identifier from an interaction law name
  function get_inter_law_id_from_name(i_type)
    implicit none
    !> [in] interaction law type name
    character(len=24), intent(in) :: i_type
    !> [return] integer id
    integer(kind=4)               :: get_inter_law_id_from_name

    get_inter_law_id_from_name = get_index_from_string(i_type,interaction_law_type,size(interaction_law_type))

  end function

  !> \brief Get interaction law name from its integer id
  function get_inter_law_name_from_id(id)
    implicit none
    !> [in] node id
    integer(kind=4), intent(in) :: id
    !> [return] interaction law name
    character(len=24)           :: get_inter_law_name_from_id 

    if( id > 0 .and. id < size(interaction_law_type) ) then
      get_inter_law_name_from_id = interaction_law_type(id)
    else
      get_inter_law_name_from_id = 'xxxxxxxxxxxxxxxxxxxxxxxx'
    end if

  end function

  !> \brief Get list of parameter names of inter_law
  function get_inter_law_names()
    implicit none
    character(len=24), dimension(:), pointer :: get_inter_law_names

    get_inter_law_names => interaction_law_type

  end function

  !> \brief [private] Check name consistency of inter_law names
  subroutine check_inter_law_names_()
    implicit none

    if( get_inter_law_id_from_name('IQS_CLB                  ') /= i_IQS_CLB                    ) call parameter_error_('IQS_CLB'                  )
    if( get_inter_law_id_from_name('IQS_CLB_nosldt           ') /= i_IQS_CLB_nosldt             ) call parameter_error_('IQS_CLB_nosldt'           )
    if( get_inter_law_id_from_name('IQS_CLB_noslds           ') /= i_IQS_CLB_noslds             ) call parameter_error_('IQS_CLB_noslds'           )
    if( get_inter_law_id_from_name('IQS_CLB_RGR              ') /= i_IQS_CLB_RGR                ) call parameter_error_('IQS_CLB_RGR'              )
    if( get_inter_law_id_from_name('IQS_DS_CLB               ') /= i_IQS_DS_CLB                 ) call parameter_error_('IQS_DS_CLB'               )
    if( get_inter_law_id_from_name('IQS_WET_DS_CLB           ') /= i_IQS_WET_DS_CLB             ) call parameter_error_('IQS_WET_DS_CLB'           )
    if( get_inter_law_id_from_name('postGAP_IQS_MAC_CZM      ') /= i_postGAP_IQS_MAC_CZM        ) call parameter_error_('postGAP_IQS_MAC_CZM'      )
    if( get_inter_law_id_from_name('PLASTIC_COUPLED_DOF      ') /= i_PLASTIC_COUPLED_DOF        ) call parameter_error_('PLASTIC_COUPLED_DOF'      )
    if( get_inter_law_id_from_name('xQS_WET_DS_CLB           ') /= i_xQS_WET_DS_CLB             ) call parameter_error_('xQS_WET_DS_CLB'           )
    if( get_inter_law_id_from_name('TEX_SOL                  ') /= i_TEX_SOL                    ) call parameter_error_('TEX_SOL'                  )
    if( get_inter_law_id_from_name('TEX_SOL_UNILAT           ') /= i_TEX_SOL_UNILAT             ) call parameter_error_('TEX_SOL_UNILAT'           )
    if( get_inter_law_id_from_name('GAP_SGR_CLB              ') /= i_GAP_SGR_CLB                ) call parameter_error_('GAP_SGR_CLB'              )
    if( get_inter_law_id_from_name('VEL_SGR_CLB              ') /= i_VEL_SGR_CLB                ) call parameter_error_('VEL_SGR_CLB'              )
    if( get_inter_law_id_from_name('GAP_WET_DS_CLB           ') /= i_GAP_WET_DS_CLB             ) call parameter_error_('GAP_WET_DS_CLB'           )
    if( get_inter_law_id_from_name('MSMP_CZM                 ') /= i_MSMP_CZM                   ) call parameter_error_('MSMP_CZM'                 )
    if( get_inter_law_id_from_name('GAP_SGR_DS_CLB           ') /= i_GAP_SGR_DS_CLB             ) call parameter_error_('GAP_SGR_DS_CLB'           )
    if( get_inter_law_id_from_name('VEL_SGR_DS_CLB           ') /= i_VEL_SGR_DS_CLB             ) call parameter_error_('VEL_SGR_DS_CLB'           )
    if( get_inter_law_id_from_name('RST_CLB                  ') /= i_RST_CLB                    ) call parameter_error_('RST_CLB'                  )
    if( get_inter_law_id_from_name('RST_DS_CLB               ') /= i_RST_DS_CLB                 ) call parameter_error_('RST_DS_CLB'               )
    if( get_inter_law_id_from_name('IQS_MOHR_DS_CLB          ') /= i_IQS_MOHR_DS_CLB            ) call parameter_error_('IQS_MOHR_DS_CLB'          )
    if( get_inter_law_id_from_name('MAL_CZM                  ') /= i_MAL_CZM                    ) call parameter_error_('MAL_CZM'                  )
    if( get_inter_law_id_from_name('ELASTIC_REPELL_CLB       ') /= i_ELASTIC_REPELL_CLB         ) call parameter_error_('ELASTIC_REPELL_CLB'       )
    if( get_inter_law_id_from_name('CRITICAL_VOIGT_CLB       ') /= i_CRITICAL_VOIGT_CLB         ) call parameter_error_('CRITICAL_VOIGT_CLB'       )
    if( get_inter_law_id_from_name('ELASTIC_REPELL_WET_CLB   ') /= i_ELASTIC_REPELL_WET_CLB     ) call parameter_error_('ELASTIC_REPELL_WET_CLB'   )
    if( get_inter_law_id_from_name('ELASTIC_ROD              ') /= i_ELASTIC_ROD                ) call parameter_error_('ELASTIC_ROD'              )
    if( get_inter_law_id_from_name('VOIGT_ROD                ') /= i_VOIGT_ROD                  ) call parameter_error_('VOIGT_ROD'                )
    if( get_inter_law_id_from_name('ELASTIC_WIRE             ') /= i_ELASTIC_WIRE               ) call parameter_error_('ELASTIC_WIRE'             )
    if( get_inter_law_id_from_name('VOIGT_WIRE               ') /= i_VOIGT_WIRE                 ) call parameter_error_('VOIGT_WIRE'               )
    if( get_inter_law_id_from_name('MD_JKRs                  ') /= i_MD_JKRs                    ) call parameter_error_('MD_JKRs'                  )
    if( get_inter_law_id_from_name('ELASTIC_WET_NT           ') /= i_ELASTIC_WET_NT             ) call parameter_error_('ELASTIC_WET_NT'           )
    if( get_inter_law_id_from_name('COUPLED_DOF              ') /= i_COUPLED_DOF                ) call parameter_error_('COUPLED_DOF'              )
    if( get_inter_law_id_from_name('TANGENTIAL_COUPLED_DOF   ') /= i_TANGENTIAL_COUPLED_DOF     ) call parameter_error_('TANGENTIAL_COUPLED_DOF'   )
    if( get_inter_law_id_from_name('NORMAL_COUPLED_DOF       ') /= i_NORMAL_COUPLED_DOF         ) call parameter_error_('NORMAL_COUPLED_DOF'       )
    if( get_inter_law_id_from_name('PERIO_DOF                ') /= i_PERIO_DOF                  ) call parameter_error_('PERIO_DOF'                )
    if( get_inter_law_id_from_name('GAP_SGR_CLB_WEAR         ') /= i_GAP_SGR_CLB_WEAR           ) call parameter_error_('GAP_SGR_CLB_WEAR'         )
    if( get_inter_law_id_from_name('IQS_SGR_CLB_WEAR         ') /= i_IQS_SGR_CLB_WEAR           ) call parameter_error_('IQS_SGR_CLB_WEAR'         )
    if( get_inter_law_id_from_name('MAC_CZM                  ') /= i_MAC_CZM                    ) call parameter_error_('MAC_CZM'                  )
    if( get_inter_law_id_from_name('IQS_MAC_CZM              ') /= i_IQS_MAC_CZM                ) call parameter_error_('IQS_MAC_CZM'              )
    if( get_inter_law_id_from_name('WET_CZM                  ') /= i_WET_CZM                    ) call parameter_error_('WET_CZM'                  )
    if( get_inter_law_id_from_name('GAP_MOHR_DS_CLB          ') /= i_GAP_MOHR_DS_CLB            ) call parameter_error_('GAP_MOHR_DS_CLB'          )
    if( get_inter_law_id_from_name('MAC_CZM_nosldt           ') /= i_MAC_CZM_nosldt             ) call parameter_error_('MAC_CZM_nosldt'           )
    if( get_inter_law_id_from_name('MAC_CZM_noslds           ') /= i_MAC_CZM_noslds             ) call parameter_error_('MAC_CZM_noslds'           )
    if( get_inter_law_id_from_name('IQS_MAC_CZM_nosldt       ') /= i_IQS_MAC_CZM_nosldt         ) call parameter_error_('IQS_MAC_CZM_nosldt'       )
    if( get_inter_law_id_from_name('IQS_MAC_CZM_noslds       ') /= i_IQS_MAC_CZM_noslds         ) call parameter_error_('IQS_MAC_CZM_noslds'       )
    if( get_inter_law_id_from_name('ER_MAC_CZM               ') /= i_ER_MAC_CZM                 ) call parameter_error_('ER_MAC_CZM'               )
    if( get_inter_law_id_from_name('IQS_WET_CZM              ') /= i_IQS_WET_CZM                ) call parameter_error_('IQS_WET_CZM'              )
    if( get_inter_law_id_from_name('DEM_FIBs                 ') /= i_DEM_FIBs                   ) call parameter_error_('DEM_FIBs'                 )
    if( get_inter_law_id_from_name('KV_WET                   ') /= i_KV_WET                     ) call parameter_error_('KV_WET'                   )
    if( get_inter_law_id_from_name('VISCO_ELASTIC_REPELL_WET ') /= i_VISCO_ELASTIC_REPELL_WET   ) call parameter_error_('VISCO_ELASTIC_REPELL_WET' )
    if( get_inter_law_id_from_name('RIGID_WIRE               ') /= i_RIGID_WIRE                 ) call parameter_error_('RIGID_WIRE'               )
    if( get_inter_law_id_from_name('IQS_BW_CLB               ') /= i_IQS_BW_CLB                 ) call parameter_error_('IQS_BW_CLB'               )
    if( get_inter_law_id_from_name('IQS_CAP_MOHR_DS_CLB      ') /= i_IQS_CAP_MOHR_DS_CLB        ) call parameter_error_('IQS_CAP_MOHR_DS_CLB'      )
    if( get_inter_law_id_from_name('IQS_MAL_CZM              ') /= i_IQS_MAL_CZM                ) call parameter_error_('IQS_MAL_CZM'              )
    if( get_inter_law_id_from_name('GAP_CAP_MOHR_DS_CLB      ') /= i_GAP_CAP_MOHR_DS_CLB        ) call parameter_error_('GAP_CAP_MOHR_DS_CLB'      )
    if( get_inter_law_id_from_name('BROKEN_DOF               ') /= i_BROKEN_DOF                 ) call parameter_error_('BROKEN_DOF'               )
    if( get_inter_law_id_from_name('IQS_CLB_g0               ') /= i_IQS_CLB_g0                 ) call parameter_error_('IQS_CLB_g0'               )
    if( get_inter_law_id_from_name('IQS_MAP                  ') /= i_IQS_MAP                    ) call parameter_error_('IQS_MAP'                  )
    if( get_inter_law_id_from_name('RST_WET_CLB              ') /= i_RST_WET_CLB                ) call parameter_error_('RST_WET_CLB'              )
    if( get_inter_law_id_from_name('LJ_POTENTIAL             ') /= i_LJ_POTENTIAL               ) call parameter_error_('LJ_POTENTIAL'             )
    if( get_inter_law_id_from_name('GAP_SGR_CLB_g0           ') /= i_GAP_SGR_CLB_g0             ) call parameter_error_('GAP_SGR_CLB_g0'           )
    if( get_inter_law_id_from_name('BRITTLE_ELASTIC_WIRE     ') /= i_BRITTLE_ELASTIC_WIRE       ) call parameter_error_('BRITTLE_ELASTIC_WIRE'     )
    if( get_inter_law_id_from_name('ELASTIC_REPELL_CLB_g0    ') /= i_ELASTIC_REPELL_CLB_g0      ) call parameter_error_('ELASTIC_REPELL_CLB_g0'    )
    if( get_inter_law_id_from_name('MP_CZM                   ') /= i_MP_CZM                     ) call parameter_error_('MP_CZM'                   )
    if( get_inter_law_id_from_name('MP3_CZM                  ') /= i_MP3_CZM                    ) call parameter_error_('MP3_CZM'                  )
    if( get_inter_law_id_from_name('MP3_CZM_THER             ') /= i_MP3_CZM_THER               ) call parameter_error_('MP3_CZM_THER'             )
    if( get_inter_law_id_from_name('TH_CZM                   ') /= i_TH_CZM                     ) call parameter_error_('TH_CZM'                   )
    if( get_inter_law_id_from_name('IQS_TH_CZM               ') /= i_IQS_TH_CZM                 ) call parameter_error_('IQS_TH_CZM'               )
    if( get_inter_law_id_from_name('postGAP_IQS_CZM          ') /= i_postGAP_IQS_CZM            ) call parameter_error_('postGAP_IQS_CZM'          )
    if( get_inter_law_id_from_name('WET_3C                   ') /= i_WET_3C                     ) call parameter_error_('WET_3C'                   )
    if( get_inter_law_id_from_name('VISCO_ELASTIC_REPELL_CLB ') /= i_VISCO_ELASTIC_REPELL_CLB   ) call parameter_error_('VISCO_ELASTIC_REPELL_CLB' )
    if( get_inter_law_id_from_name('ELASTIC_REPELL_MAC_CZM   ') /= i_ELASTIC_REPELL_MAC_CZM     ) call parameter_error_('ELASTIC_REPELL_MAC_CZM'   )
    if( get_inter_law_id_from_name('IQS_PLAS_CLB             ') /= i_IQS_PLAS_CLB               ) call parameter_error_('IQS_PLAS_CLB'             )
    if( get_inter_law_id_from_name('ABP_CZM                  ') /= i_ABP_CZM                    ) call parameter_error_('ABP_CZM'                  )
    if( get_inter_law_id_from_name('IQS_ABP_CZM              ') /= i_IQS_ABP_CZM                ) call parameter_error_('IQS_ABP_CZM'              )
    if( get_inter_law_id_from_name('BRITTLE_COATING_CLB      ') /= i_BRITTLE_COATING_CLB        ) call parameter_error_('BRITTLE_COATING_CLB'      )
    if( get_inter_law_id_from_name('IQS_STICK                ') /= i_IQS_STICK                  ) call parameter_error_('IQS_STICK'                )
    if( get_inter_law_id_from_name('GAP_SGR_STICK            ') /= i_GAP_SGR_STICK              ) call parameter_error_('GAP_SGR_STICK'            )
    if( get_inter_law_id_from_name('GAP_SGR_CLB_nosldt       ') /= i_GAP_SGR_CLB_nosldt         ) call parameter_error_('GAP_SGR_CLB_nosldt'       )
    if( get_inter_law_id_from_name('GAP_SGR_CLB_noslds       ') /= i_GAP_SGR_CLB_noslds         ) call parameter_error_('GAP_SGR_CLB_noslds'       )
    if( get_inter_law_id_from_name('preGAP_SGR_CLB           ') /= i_preGAP_SGR_CLB             ) call parameter_error_('preGAP_SGR_CLB'           )
    if( get_inter_law_id_from_name('NARD_ROD                 ') /= i_NARD_ROD                   ) call parameter_error_('NARD_ROD'                 )
    if( get_inter_law_id_from_name('IQS_EXPO_CZM             ') /= i_IQS_EXPO_CZM               ) call parameter_error_('IQS_EXPO_CZM'             )
    if( get_inter_law_id_from_name('EXPO_CZM                 ') /= i_EXPO_CZM                   ) call parameter_error_('EXPO_CZM'                 )    
    if( get_inter_law_id_from_name('IQS_EXPO_CZM_P           ') /= i_IQS_EXPO_CZM_P             ) call parameter_error_('IQS_EXPO_CZM_P'           )
    if( get_inter_law_id_from_name('EXPO_CZM_P               ') /= i_EXPO_CZM_P                 ) call parameter_error_('EXPO_CZM_P'               )
    if( get_inter_law_id_from_name('IQS_EXPO_CZM_SPRING      ') /= i_IQS_EXPO_CZM_SPRING        ) call parameter_error_('IQS_EXPO_CZM_SPRING'      )
    if( get_inter_law_id_from_name('EXPO_CZM_SPRING          ') /= i_EXPO_CZM_SPRING            ) call parameter_error_('EXPO_CZM_SPRING'          )
    if( get_inter_law_id_from_name('IQS_EXPO_CZM_SPRING_P    ') /= i_IQS_EXPO_CZM_SPRING_P      ) call parameter_error_('IQS_EXPO_CZM_SPRING_P'    )
    if( get_inter_law_id_from_name('EXPO_CZM_SPRING_P        ') /= i_EXPO_CZM_SPRING_P          ) call parameter_error_('EXPO_CZM_SPRING_P'        )
    if( get_inter_law_id_from_name('GTN_CZM                  ') /= i_GTN_CZM                    ) call parameter_error_('GTN_CZM'                  )
    if( get_inter_law_id_from_name('GTN2_CZM                 ') /= i_GTN2_CZM                   ) call parameter_error_('GTN2_CZM'                 )
    if( get_inter_law_id_from_name('TOSI_CZM                 ') /= i_TOSI_CZM                   ) call parameter_error_('TOSI_CZM'                 )
    if( get_inter_law_id_from_name('TOSI_CZM_INCRE           ') /= i_TOSI_CZM_INCRE             ) call parameter_error_('TOSI_CZM_INCRE'           )

  end subroutine check_inter_law_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get integer identifier from integrator type
  !> Possible integrator types are : MOREAU, GEAR, VERLET, NEWMARK, BETA2
  function get_integrator_id_from_name(i_type)
    implicit none
    !> [in] integrator type
    character(len=7), intent(in) :: i_type
    !> [return] integer id
    integer(kind=4)              :: get_integrator_id_from_name

    get_integrator_id_from_name = get_index_from_string(i_type,integrator_type,size(integrator_type))
    !print *,'[get_integrator_id_from_name] : unknown integrator type : ', trim(type)

  end function

  !> \brief Get integrator type from its integer id
  function get_integrator_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] integrator type
    character(len=7)            :: get_integrator_name_from_id

    if( id > 0 .and. id <= size(integrator_type) ) then
      get_integrator_name_from_id = integrator_type(id)
    else
      !fd débile get_integrator_name_from_id = 'xxxxxxx'
      print *,'get_integrator_name_from_id','unknown ID'
      stop 1 
    end if

  end function

  !> \brief Get list of parameter names of integrator
  function get_integrator_names()
    implicit none
    character(len=7), dimension(:), pointer :: get_integrator_names

    get_integrator_names => integrator_type

  end function

  !> \brief [private] Check name consistency of integrator names
  subroutine check_integrator_names_()
    implicit none

    if( get_integrator_id_from_name('MOREAU ') /= integrator_moreau   ) call parameter_error_('MOREAU ')
    if( get_integrator_id_from_name('GEAR   ') /= integrator_gear     ) call parameter_error_('GEAR   ')
    if( get_integrator_id_from_name('VERLET ') /= integrator_verlet   ) call parameter_error_('VERLET ')
    if( get_integrator_id_from_name('NEWMARK') /= integrator_newmark  ) call parameter_error_('NEWMARK')
    if( get_integrator_id_from_name('BETA2  ') /= integrator_beta2    ) call parameter_error_('BETA2  ')
    if( get_integrator_id_from_name('QS     ') /= integrator_qs       ) call parameter_error_('QS     ')

  end subroutine check_integrator_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get node identifier from node type
  !> Possible node types are : NO1xx,NO2xx,NO3xx,NO4xx,NO5xx,NO6xx
  function get_node_id_from_name(i_type)
    implicit none
    !> [in] node type
    character(len=5), intent(in) :: i_type
    !> [return] node id
    integer(kind=4)              :: get_node_id_from_name 

    get_node_id_from_name = get_index_from_string(i_type,node_type,size(node_type))
    !print *,'[get_node_id_from_name] : unknown node type : ', trim(type)

  end function

  !> \brief Get node type from its integer id
  function get_node_name_from_id(id)
    implicit none
    !> [in] node id
    integer(kind=4), intent(in) :: id
    !> [return] node type
    character(len=7)            :: get_node_name_from_id

    if( id > 0 .and. id <= size(node_type) ) then
      get_node_name_from_id = node_type(id)
    else
      get_node_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of node
  function get_node_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_node_names

    get_node_names => node_type

  end function

  !> \brief [private] Check name consistency of node names
  subroutine check_node_names_()
    implicit none

    if( get_node_id_from_name('NO1xx') /= i_NO1xx  ) call parameter_error_('NO1xx')
    if( get_node_id_from_name('NO2xx') /= i_NO2xx  ) call parameter_error_('NO2xx')
    if( get_node_id_from_name('NO3xx') /= i_NO3xx  ) call parameter_error_('NO3xx')
    if( get_node_id_from_name('NO4xx') /= i_NO4xx  ) call parameter_error_('NO4xx')
    if( get_node_id_from_name('NO5xx') /= i_NO5xx  ) call parameter_error_('NO5xx')
    if( get_node_id_from_name('NO6xx') /= i_NO6xx  ) call parameter_error_('NO6xx')

  end subroutine check_node_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Get integer identifier from dime mod
  !> Possible dime mod name are : PSTRESS, PSTRAIN, AXI
  function get_dime_mode_id_from_name(dime_mode)
    use utilities, only: get_io_logmes
    implicit none
    !> [in] dime mode
    character(len=10), intent(in) :: dime_mode
    !> [return] integer id
    integer(kind=4)               :: get_dime_mode_id_from_name

    get_dime_mode_id_from_name = get_index_from_string(dime_mode,dime_mode_type,size(dime_mode_type))
    !write(get_io_logmes(),'(A)') '[get_dime_mode_id_from_name] : unknown dime mode type : '// dime_mode

  end function

  !> \brief Get dime mode type from its integer id
  function get_dime_mode_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] dime_mode type
    character(len=10)           :: get_dime_mode_name_from_id

    if( id > 0 .and. id <= size(dime_mode_type) ) then
      get_dime_mode_name_from_id = dime_mode_type(id)
    else
      !print *,'[get_body_vector_name_from_id] : unknown body vector identifier : ', id
      get_dime_mode_name_from_id = 'xxxxxxxxxx'
    end if

  end function
  
  !> \brief Get list of parameter names of dime_mode
  function get_dime_mode_names()
    implicit none
    character(len=10), dimension(:), pointer :: get_dime_mode_names

    get_dime_mode_names => dime_mode_type

  end function

  !> \brief [private] Check name consistency of dime_mode names
  subroutine check_dime_mode_names_()
    implicit none

    if( get_dime_mode_id_from_name('2D PSTRAIN') /= i_2D_strain  ) call parameter_error_('2D PSTRAIN')
    if( get_dime_mode_id_from_name('2D PSTRESS') /= i_2D_stress  ) call parameter_error_('2D PSTRESS')
    if( get_dime_mode_id_from_name('2D AXI    ') /= i_2D_axisym  ) call parameter_error_('2D AXI    ')
    if( get_dime_mode_id_from_name('3D        ') /= i_3D         ) call parameter_error_('3D        ')

  end subroutine check_dime_mode_names_

  !----------------------------------------------------------------------------------------------------------------!

  ! Get integer identifier from body vector name
  function get_body_vector_id_from_name(body_vector_name)
    implicit none
    !> [in] body_vector_name
    character(len=5), intent(in) :: body_vector_name
    !> [return] integer id
    integer(kind=4)              :: get_body_vector_id_from_name

    get_body_vector_id_from_name = get_index_from_string(body_vector_name,vector_type,size(vector_type))
    !print *,'[get_body_vector_id_from_name] : unknown body vector name : ', body_vector_name

  end function
  
  ! Get body vector name from integer identifier 
  function get_body_vector_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] body vector name
    character(len=5)            :: get_body_vector_name_from_id

    if( id > 0 .and. id <= size(vector_type) ) then
      get_body_vector_name_from_id = vector_type(id)
    else
      !print *,'[get_body_vector_name_from_id] : unknown body vector identifier : ', id
      get_body_vector_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of body_vector
  function get_body_vector_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_body_vector_names

    get_body_vector_names => vector_type

  end function

  !> \brief [private] Check name consistency of vector_type names
  subroutine check_body_vector_names_()
    implicit none

    if( get_body_vector_id_from_name('Vbeg_') /= iVbeg_  ) call parameter_error_('Vbeg_')
    if( get_body_vector_id_from_name('Vfree') /= iVfree  ) call parameter_error_('Vfree')
    if( get_body_vector_id_from_name('Vaux_') /= iVaux_  ) call parameter_error_('Vaux_')
    if( get_body_vector_id_from_name('Vddm_') /= iVddm_  ) call parameter_error_('Vddm_')
    if( get_body_vector_id_from_name('V____') /= iV____  ) call parameter_error_('V____')
    if( get_body_vector_id_from_name('Xprev') /= iXprev  ) call parameter_error_('Xprev')
    if( get_body_vector_id_from_name('Xbeg_') /= iXbeg_  ) call parameter_error_('Xbeg_')
    if( get_body_vector_id_from_name('X____') /= iX____  ) call parameter_error_('X____')
    if( get_body_vector_id_from_name('Ireac') /= iIreac  ) call parameter_error_('Ireac')
    if( get_body_vector_id_from_name('Iaux_') /= iIaux_  ) call parameter_error_('Iaux_')
    if( get_body_vector_id_from_name('Reac_') /= iReac_  ) call parameter_error_('Reac_')
    if( get_body_vector_id_from_name('Raux_') /= iRaux_  ) call parameter_error_('Raux_')
    if( get_body_vector_id_from_name('Rfree') /= iRfree  ) call parameter_error_('Rfree')
    if( get_body_vector_id_from_name('Fint_') /= iFint_  ) call parameter_error_('Fint_')
    if( get_body_vector_id_from_name('Fext_') /= iFext_  ) call parameter_error_('Fext_')
    if( get_body_vector_id_from_name('Coor_') /= iCoor_  ) call parameter_error_('Coor_')
    if( get_body_vector_id_from_name('Coorb') /= iCoorb  ) call parameter_error_('Coorb')
    if( get_body_vector_id_from_name('Coor0') /= iCoor0  ) call parameter_error_('Coor0')

  end subroutine check_body_vector_names_

  !----------------------------------------------------------------------------------------------------------------!

  ! Get integer identifier from contact status name
  function get_contact_status_id_from_name(contact_status_name)
    implicit none
    !> [in] contact_status_name
    character(len=5), intent(in) :: contact_status_name
    !> [return] integer id
    integer(kind=4)              :: get_contact_status_id_from_name

    get_contact_status_id_from_name = get_index_from_string(contact_status_name,contact_status,size(contact_status))
    !print *,'[get_contact_status_id_from_name] : unknown body vector name : ', contact_status_name

  end function

  ! Get contact status name from integer identifier 
  function get_contact_status_name_from_id(id)
    implicit none
    !> [in] integer id
    integer(kind=4), intent(in) :: id
    !> [return] body vector name
    character(len=5)            :: get_contact_status_name_from_id

    if( id > 0 .and. id <= size(contact_status) ) then
      get_contact_status_name_from_id = contact_status(id)
    else
      !print *,'[get_contact_status_name_from_id] : unknown body vector identifier : ', id
      get_contact_status_name_from_id = 'xxxxx'
    end if

  end function

  !> \brief Get list of parameter names of contact_status
  function get_contact_status_names()
    implicit none
    character(len=5), dimension(:), pointer :: get_contact_status_names

    get_contact_status_names => contact_status

  end function

  !> \brief [private] Check name consistency of contact_status names
  subroutine check_contact_status_names_()
    implicit none

    if( get_contact_status_id_from_name('vnish') /= i_vnish  ) call parameter_error_('vnish')
    if( get_contact_status_id_from_name('nknow') /= i_nknow  ) call parameter_error_('nknow')
    if( get_contact_status_id_from_name('noctc') /= i_noctc  ) call parameter_error_('noctc')
    if( get_contact_status_id_from_name('stick') /= i_stick  ) call parameter_error_('stick')
    if( get_contact_status_id_from_name('slide') /= i_slide  ) call parameter_error_('slide')
    if( get_contact_status_id_from_name('slibw') /= i_slibw  ) call parameter_error_('slibw')
    if( get_contact_status_id_from_name('slifw') /= i_slifw  ) call parameter_error_('slifw')
    if( get_contact_status_id_from_name('Cnnow') /= i_Cnnow  ) call parameter_error_('Cnnow')
    if( get_contact_status_id_from_name('Cnctc') /= i_Cnctc  ) call parameter_error_('Cnctc')
    if( get_contact_status_id_from_name('Cstck') /= i_Cstck  ) call parameter_error_('Cstck')
    if( get_contact_status_id_from_name('Cslid') /= i_Cslid  ) call parameter_error_('Cslid')
    if( get_contact_status_id_from_name('Cslbw') /= i_Cslbw  ) call parameter_error_('Cslbw')
    if( get_contact_status_id_from_name('Cslfw') /= i_Cslfw  ) call parameter_error_('Cslfw')
    if( get_contact_status_id_from_name('Wnnow') /= i_Wnnow  ) call parameter_error_('Wnnow')
    if( get_contact_status_id_from_name('Wnctc') /= i_Wnctc  ) call parameter_error_('Wnctc')
    if( get_contact_status_id_from_name('Wstck') /= i_Wstck  ) call parameter_error_('Wstck')
    if( get_contact_status_id_from_name('Wslid') /= i_Wslid  ) call parameter_error_('Wslid')
    if( get_contact_status_id_from_name('Wslbw') /= i_Wslbw  ) call parameter_error_('Wslbw')
    if( get_contact_status_id_from_name('Wslfw') /= i_Wslfw  ) call parameter_error_('Wslfw')
    if( get_contact_status_id_from_name('Mnnow') /= i_Mnnow  ) call parameter_error_('Mnnow')
    if( get_contact_status_id_from_name('Mnctc') /= i_Mnctc  ) call parameter_error_('Mnctc')
    if( get_contact_status_id_from_name('Mstck') /= i_Mstck  ) call parameter_error_('Mstck')
    if( get_contact_status_id_from_name('Mslid') /= i_Mslid  ) call parameter_error_('Mslid')
    if( get_contact_status_id_from_name('Mslbw') /= i_Mslbw  ) call parameter_error_('Mslbw')
    if( get_contact_status_id_from_name('Mslfw') /= i_Mslfw  ) call parameter_error_('Mslfw')
    if( get_contact_status_id_from_name('Ennow') /= i_Ennow  ) call parameter_error_('Ennow')
    if( get_contact_status_id_from_name('Enctc') /= i_Enctc  ) call parameter_error_('Enctc')
    if( get_contact_status_id_from_name('Estck') /= i_Estck  ) call parameter_error_('Estck')
    if( get_contact_status_id_from_name('Eslid') /= i_Eslid  ) call parameter_error_('Eslid')
    if( get_contact_status_id_from_name('Eslbw') /= i_Eslbw  ) call parameter_error_('Eslbw')
    if( get_contact_status_id_from_name('Eslfw') /= i_Eslfw  ) call parameter_error_('Eslfw')
    if( get_contact_status_id_from_name('_RGR_') /= i__RGR_  ) call parameter_error_('_RGR_')
    if( get_contact_status_id_from_name('Ssstt') /= i_Ssstt  ) call parameter_error_('Ssstt')
    if( get_contact_status_id_from_name('Ssptt') /= i_Ssptt  ) call parameter_error_('Ssptt')
    if( get_contact_status_id_from_name('Ssmtt') /= i_Ssmtt  ) call parameter_error_('Ssmtt')
    if( get_contact_status_id_from_name('Ssstp') /= i_Ssstp  ) call parameter_error_('Ssstp')
    if( get_contact_status_id_from_name('Ssptp') /= i_Ssptp  ) call parameter_error_('Ssptp')
    if( get_contact_status_id_from_name('Ssmtp') /= i_Ssmtp  ) call parameter_error_('Ssmtp')
    if( get_contact_status_id_from_name('Ssstm') /= i_Ssstm  ) call parameter_error_('Ssstm')
    if( get_contact_status_id_from_name('Ssptm') /= i_Ssptm  ) call parameter_error_('Ssptm')
    if( get_contact_status_id_from_name('Ssmtm') /= i_Ssmtm  ) call parameter_error_('Ssmtm')

  end subroutine check_contact_status_names_

  !----------------------------------------------------------------------------------------------------------------!

  !> \brief Run a consistency check of all parameters id with their associated name_array
  subroutine check_all_parameters()
    implicit none

    call check_parameter_ids_( body_model_id       , body_model       , size(body_model)       )
    call check_parameter_ids_( contactor_type_id  , contactor_type  , size(contactor_type)  )
    call check_parameter_ids_( interaction_type_id, interaction_type, size(interaction_type))
    call check_parameter_ids_( matrix_storage_id  , matrix_storage  , size(matrix_storage)  )
    call check_parameter_ids_( matrix_shape_id    , matrix_shape    , size(matrix_shape)  )

    call check_parameter_ids_( generalized_coordinates_id  , &
                             generalized_coordinates     , &
                             size(generalized_coordinates) )
    call check_parameter_ids_( surface_energy_status_id    , &
                             surface_energy_status       , &
                             size(surface_energy_status)   )
    call check_parameter_ids_( interaction_law_type_id     , &
                             interaction_law_type        , &
                             size(interaction_law_type)    )
    call check_parameter_ids_( integrator_type_id, integrator_type, size(integrator_type))
    call check_parameter_ids_( node_type_id      , node_type      , size(node_type)      )
    call check_parameter_ids_( dime_mode_type_id , dime_mode_type , size(dime_mode_type) )
    call check_parameter_ids_( vector_type_id    , vector_type    , size(vector_type)    )
    call check_parameter_ids_( contact_status_id , contact_status , size(contact_status) )


    call check_body_model_names_()
    call check_contactor_names_()
    call check_interaction_names_()
    call check_matrix_storage_names_()
    call check_matrix_shape_names_()
    call check_generalized_coordinates_names_()
    call check_surface_energy_status_names_()
    call check_inter_law_names_()
    call check_integrator_names_()
    call check_node_names_()
    call check_dime_mode_names_()
    call check_body_vector_names_()
    call check_contact_status_names_()

  end subroutine check_all_parameters

end module parameters

