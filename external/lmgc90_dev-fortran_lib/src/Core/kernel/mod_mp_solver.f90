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
module MP_SOLVER
! REFLEXION A REPRENDRE SUR QUI PORTE QUOI: TACTY ou BDYTY
  !!****h* LMGC90.CORE/MP_SOLVER
  !! NAME
  !!  module MP_SOLVER
  !! AUTHOR
  !!   M. Renouf (e-mail: Mathieu.Renouf@insa-lyon.fr)
  !! PURPOSE
  !!   This module is dedicated to Thermal Electrical Discrete Element  model
  !!   Contact the author to obtain the module.
  !!****

  use parameters
  use utilities
  use overall
  use BULK_BEHAVIOUR
  use TACT_BEHAVIOUR
  use a_DOF

  use RBDY2
  use DISKx, only: diskx2bdyty , &
                   get_nb_diskx, &
                   get_radius_diskx
  use POLYG, only: polyg2bdyty , &
                   get_nb_polyg
  use JONCx, only: joncx2bdyty , &
                   get_nb_joncx
  use xKSID, only: xksid2bdyty , &
                   get_nb_xksid

  use DKDKx, only: DKDKx2DISKx
  use DKJCx, only: DKJCx2DISKx, &
                   DKJCx2JONCx
  use DKKDx, only: DKKDx2DISKx, &
                   DKKDx2xKSID
  use PLPLx, only: PLPLx2POLYG, &
                   get_type_PLPLx
  use PLJCx, only: PLJCx2POLYG, &
                   PLJCx2JONCx, &
                   get_type_PLJCx
  use CLALp, only: CLALp2CLxxx, &
                   CLALp2ALpxx


  use CLxxx
  use ALpxx

  use inter_meca_handler_2D, only : get_nb_inters         , &
                                    get_rloc              , &
                                    get_vloc              , &
                                    get_vlocBEGIN         , &
                                    get_internal          , &
                                    get_tact_lawnb        , &
                                    get_eff               , &
                                    get_nb_verlets        , &
                                    get_verlet_adjsz      , &
                                    get_verlet_iantac     , &
                                    get_verlet_rloc       , &
                                    get_verlet_internal   , &
                                    set_verlet_internal   , &
                                    get_verlet_local_frame, &
                                    this2verlet

  implicit none

  private

  logical :: electro_model = .false.
  logical :: thermo_model  = .false.

  !----------------------------------------------------------------
  !* iheat = 1 : diff
  !* iheat = 2 : conv
  !*
  !* ibound = 1 : adia
  !* ibound = 2 : line
  !* ibound = 3 : 1D
  !* ibound = 4 : 2D
  !-----------------------------------------------------------------!

  type T_THERMAL_MODEL
     
     logical         :: lconv,init
     integer(kind=4) :: iheat,ibound,ilkine,ildiff,igdiff
     integer(kind=4) :: ilcond
     real(kind=8)    :: thickness
     real(kind=8)    :: T0,Alert,Gcond,GTemp

  end type T_THERMAL_MODEL
  
  type(T_THERMAL_MODEL) :: MODtherm
  real(kind=8),allocatable,dimension(:,:)   :: TBULK,TBULKi

  !-----------------------------------------------------------------!

  integer(kind=4),parameter :: i_h_diff = 1, i_h_conv = 2
  integer(kind=4),parameter :: i_d_adia = 1, i_d_line = 2, i_d_1D__ = 3, i_d_2D__ = 4
  integer(kind=4),parameter :: i_lkine_no = 0 , i_lkine_dvn = 1 , i_lkine_dvt = 2 , i_lkine_all = 3
  integer(kind=4),parameter :: i_ldiff_none = 0 , i_ldiff_Hertz = 1 , i_ldiff_Cylnd = 2
  integer(kind=4),parameter :: i_gdiff_discrete = 0 , i_gdiff_continue = 1
  !
  ! FOR EXTRA MODELS
  !
  integer(kind=4),parameter :: i_upxxx = 1 , i_downx = 2 , i_right = 3 , i_leftx = 4 , i_default = 0
  !
  !-----------------------------------------------------------------!
  type T_ELECTRO_MODEL
     
     integer(kind=4) :: current,tension,local

     logical      :: init,oxide,sliding,breakdown
     real(kind=8) :: A,B,Omega,Phi
     real(kind=8) :: Coxi
     real(kind=8) :: breakdown_threshold,sliding_threshold,breakdown_var
     
     real(kind=8)    :: TOL,NLTOL
     integer(kind=4) :: itermax
     integer(kind=4) :: NLitermax

  end type T_ELECTRO_MODEL

  type(T_ELECTRO_MODEL) :: MODelec

  integer(kind=4),parameter :: i_c_cons = 1, i_c_line = 2, i_c_alte = 3, i_c_vanish = 0
  integer(kind=4),parameter :: i_t_cons = 1, i_t_line = 2, i_t_alte = 3, i_t_vanish = 0
  integer(kind=4),parameter :: i_l_allno = 1, i_l_Hertz = 2, i_l_Holm_Hertz = 3, i_l_Holm_plast = 4, i_l_Tekaya = 5

  !-----------------------------------------------------------------!
  type T_NODES

     integer(kind=4) :: ID_RBDY2,ID_TACTY

     !* thermal purposes

     logical         :: AM_I_TH_BND,AM_I_TH_SRC !
     integer(kind=4) :: iTHsource,iTHbound      !
     real(kind=8)    :: alpha                   ! alpha : 1/(rho*Cp*V)
     real(kind=8)    :: T                       ! T     : Current temperature
     real(kind=8)    :: Tini                    ! Tini  : Initial temperature
     real(kind=8)    :: dT                      ! T     : Current temperature velocity
     real(kind=8)    :: dTini                   ! Tini  : Initial temperature velocity

     !* electrical purposes

     logical         :: AM_I_EL_BND,AM_I_EL_SRC !
     integer(kind=4) :: iELsource,iELbound      !
     real(kind=8)    :: Idriv,Vdriv             ! 
     real(kind=8)    :: V,Vik,Ic                ! Electrical strengh

     integer(kind=4)                        :: iadj,nbadj
     real(kind=8)                           :: Cii
     integer(kind=4), pointer, dimension(:) :: adj
     real(kind=8), pointer, dimension(:)    :: Cij

  end type T_NODES

  type(T_NODES),dimension(:),allocatable :: ELnodes,THnodes

  !-----------------------------------------------------------------
  integer(kind=4) :: nb_NODES = 0

  !-----------------------------------------------------------------
  !> \brief Verlet structure for oxided contacts in electrical model
  !>        used to recup last electrical state.
  type T_VERLET

     integer(kind=4)                      :: nbadj,iadj   !> number of adjacent branches
     integer(kind=4)                      :: itype 
     integer(kind=4),pointer,dimension(:) :: adj          !> adjacent branches
     logical,pointer,dimension(:)         :: oxided       !> oxided flag
     real(kind=8),pointer,dimension(:)    :: threshold    !> electrical treshold
     real(kind=8),pointer,dimension(:)    :: UI           !> electrical power

  end type T_VERLET

  type(T_VERLET),allocatable,dimension(:) :: ElectricalNodes

  !-----------------------------------------------------------------

  type MULTI_NODE

     integer(kind=4) :: CD,AN
     real(kind=8)    :: AREA

  end type MULTI_NODE

  type(MULTI_NODE),dimension(:),allocatable :: MultiNodes

  integer(kind=4) :: nb_MLT_NODES = 0

  !------------------------------------------------------------------
  type T_EXTRA_NODES

     integer(kind=4)                     :: ifirst,ilast,idirection
     real(kind=8),dimension(2)           :: internal
     real(kind=8)                        :: T,thickness,sgnB
     real(kind=8)                        :: length,DX,ALPHA,ADT

     integer(kind=4)                     :: NX,NY
     real(kind=8),dimension(:),pointer   :: TSOURCE,TPROFIL
     real(kind=8),dimension(:,:),pointer :: TBULK,TBULKi

  end type T_EXTRA_NODES

  type(T_EXTRA_NODES),dimension(:),allocatable :: TH_SRC_NODES,TH_BND_NODES
  type(T_EXTRA_NODES),dimension(:),allocatable :: EL_BND_NODES

  integer(kind=4) :: nb_TH_BOUNDS = 0 , nb_TH_SOURCES = 0
  integer(kind=4) :: nb_EL_BOUNDS = 0

  !-----------------------------------------------------------------!
  !> \brief Definition of branche type for electro model. 
  type T_BRANCHE
     
     integer(kind=4) :: Active
     integer(kind=4) :: icd,ian,itype
     real(kind=8)    :: rln,vlt,Ctot,U,UI,Coxi,I
     real(kind=8)    :: threshold,Calpha,reff
     logical         :: oxided,oxi

  end type T_BRANCHE
  
  type(T_BRANCHE),allocatable,dimension(:) :: Branches  

  !-----------------------------------------------------------------

  type T_Flux

     real(kind=8)    :: Qij_c ! conduction
     real(kind=8)    :: Qij_s ! generation

  end type T_Flux

  type(T_Flux),allocatable,dimension(:) :: LocalFlux_DKDKx,LocalFlux_DKJCx,LocalFlux_DKKDx, &
                                           LocalFlux_PLPLx,LocalFlux_PLJCx

  !-----------------------------------------------------------------

  integer(kind=4) :: nb_RBDY2 = 0 , nb_DISKx = 0 , nb_POLYG = 0 , nb_JONCx = 0 , nb_xKSID = 0, nb_GRAIN = 0

  integer(kind=4),allocatable,dimension(:) :: diskx2nodes,joncx2nodes,polyg2nodes,xksid2nodes
  integer(kind=4),allocatable,dimension(:) :: nodes2rbdy2

  integer(kind=4) :: nb_CDAN = 0 ,  nb_DKDKx = 0 , nb_DKJCx = 0
  integer(kind=4) :: nb_DKKDx = 0 , nb_PLPLx = 0 , nb_PLJCx = 0

  integer(kind=4),allocatable,dimension(:) :: dkdkx2branches,dkjcx2branches,dkkdx2branches

  integer(kind=4),allocatable,dimension(:) :: randomlist

  !------------------------------------------------------------------

  real(kind=8)    :: E_ERR
  integer(kind=4) :: iter , NLiter ,Noxide
  integer(kind=4) :: Isig = 1, nb_OXID=0, nb_changed_OXID = 0 , nb_SLDOXI = 0

  real(kind=8) :: V0, Vtmp = 0.D0, Itmp = 0.D0, OMEGA
  real(kind=8) :: Req, Cmean, Pmean , Pmax, Pmin , Iinc = 1.0

  real(kind=8) :: GLOBAL_DV2,GLOBAL_PV,GLOBAL_DPV,GLOBAL_QIJ,GLOBAL_AREA,GLOBAL_AQIJ,GLOBAL_QRIJ

  real(kind=8),allocatable,dimension(:,:) :: CondMat

  logical :: first_time=.true.
  logical :: FREE_BOUNDARY=.false.
  
  !jr
  real(kind=8)    :: ThBeta,MeanC = 4.0,ConvGen = 1.0, ConvCond = 0.0
  integer(kind=4) :: coordination
  integer(kind=4),allocatable,dimension(:) :: coordinance

  public &
       active_recup, &
       get_branches_values, &
       get_electro_info, &
       get_global_thermal_variable, &
       get_heat_bound_dims, &
       get_heat_bound_MNODES, &
       get_heat_bound_TNODES, &
       get_heat_bound_PROFILE, &
       get_nb_heat_bounds, &
       get_oxided_tactor, &
       get_write_mp_values, &
       init_mp_solver, &
       init_free_boundary_mp_solver, &
       read_in_mp_behaviour_mp_solver, &
       read_ini_mp_values_mp_solver, &
       solve_electro1G, &
       solve_nl_electro1G, &
       solve_thermo_mp_solver, &
       update_conductivity_mp_solver, &
       update_thermo_mp_solver, &
       write_out_mp_behaviour_mp_solver, &
       write_xxx_mp_values_mp_solver,&
       !
       compute_flux_mailx, &
       init_thermal_conductivity_mp_solver, &
       get_local_flux, &
       set_heat_generation_factor, &
       set_heat_conduction_continue_factor

  ! used in thermal solver
  public comp_thermal_eff_value

contains

!!!**************************************************************************************
!!!*
!!!*  Thermal and electrical values are related to contactors and not to bodies.
!!!*  If the body have only one contactor, node and body are the same, obviously.
!!!*  On the contrary, i.e. the body has several contactors l'echelle thermique
!!!*  et calculer constante pour un seul contactor. ainsi il peut y avoir des gradients
!!!*  thermiques.
!!!*  Cependant cette methodologie entraine un probleme lors de la diffusioin thermique
!!!*  En effet, la structure actuelle se base sur les contact ie qu'il y aura diffusion si
!!!*  il y a contact. Il ne faut alors pas oublie les multi-tactors qui vont echanger de la
!!!*  chaleur si ils sont geomtriquement proche.
!!!*
!!!**************************************************************************************
  !> \brief initailisation of multiphysic strutures
  subroutine init_mp_solver
    implicit none
    integer(kind=4) :: inode,ibdyty

    !mr : 2014.02.20
    !     the electrical problem need to be computed on each RBDY2 and not on each tactor.
    !     Thus a distinction should be done between the electrical problem and the electrical
    !     one and two structures should be defined.
    !     Such separation will make easier the thermo-electrical coupling.

    nb_RBDY2  = get_nb_RBDY2()

    nb_DISKx  = get_nb_DISKx()
    nb_JONCx  = get_nb_JONCx()
    nb_POLYG  = get_nb_POLYG()
    nb_xKSID  = get_nb_xKSID()

    nb_NODES = nb_DISKx + nb_JONCx + nb_POLYG + nb_xKSID 

    if( nb_NODES .eq. 0 ) then
       call LOGMES('  @ No THERMAL NODES in this problem')
       stop
    end if

    ! Electrical part
    if (allocated(ELnodes)) deallocate(ELnodes)
    allocate(ELnodes(nb_RBDY2))

    if (allocated(randomlist)) deallocate(randomlist)
    allocate(randomlist(nb_RBDY2))


    do ibdyty = 1,nb_RBDY2

       randomlist(ibdyty) = ibdyty
       ELnodes(ibdyty)%ID_RBDY2 = 0
       ELnodes(ibdyty)%ID_TACTY = 0

       ! thermal parameter
       ELnodes(ibdyty)%AM_I_TH_SRC = .false.
       ELnodes(ibdyty)%AM_I_TH_BND = .false.

       ELnodes(ibdyty)%iTHsource = 0
       ELnodes(ibdyty)%iTHbound  = 0

       ELnodes(ibdyty)%alpha = 0.D0
       ELnodes(ibdyty)%T     = 0.D0
       ELnodes(ibdyty)%Tini  = 0.D0
       ELnodes(ibdyty)%dT    = 0.D0
       ELnodes(ibdyty)%dTini = 0.D0

       !electrical parameter
       ELnodes(ibdyty)%AM_I_EL_SRC = .false.
       ELnodes(ibdyty)%AM_I_EL_BND = .false.

       ELnodes(ibdyty)%iELsource = 0
       ELnodes(ibdyty)%iELbound  = 0

       ELnodes(ibdyty)%V     = 0.D0
       ELnodes(ibdyty)%Ic    = 0.D0
       ELnodes(ibdyty)%Vik   = 0.D0
       ELnodes(ibdyty)%Idriv = 0.D0
       ELnodes(ibdyty)%Vdriv = 0.D0
       ELnodes(ibdyty)%iadj  = 0
       ELnodes(ibdyty)%nbadj = 0
       ELnodes(ibdyty)%Cii   = 0.0

       nullify(ELnodes(ibdyty)%adj)
       nullify(ELnodes(ibdyty)%Cij)       

    end do

    ! Thermal part
    if (allocated(THnodes)) deallocate(THnodes)
    allocate(THnodes(nb_NODES))

    do inode = 1,nb_NODES

       THnodes(inode)%ID_RBDY2 = 0
       THnodes(inode)%ID_TACTY = 0

       ! thermal 
       THnodes(inode)%AM_I_TH_SRC = .false.
       THnodes(inode)%AM_I_TH_BND = .false.

       THnodes(inode)%iTHsource = 0
       THnodes(inode)%iTHbound  = 0

       THnodes(inode)%alpha = 0.D0
       THnodes(inode)%T     = 0.D0
       THnodes(inode)%Tini  = 0.D0
       THnodes(inode)%dT    = 0.D0
       THnodes(inode)%dTini = 0.D0

       !electrical
       THnodes(inode)%AM_I_EL_SRC = .false.
       THnodes(inode)%AM_I_EL_BND = .false.

       THnodes(inode)%iELsource = 0
       THnodes(inode)%iELbound  = 0

       THnodes(inode)%V     = 0.D0
       THnodes(inode)%Ic    = 0.D0
       THnodes(inode)%Vik   = 0.D0
       THnodes(inode)%Idriv = 0.D0
       THnodes(inode)%Vdriv = 0.D0
       THnodes(inode)%iadj  = 0
       THnodes(inode)%nbadj = 0
       THnodes(inode)%Cii   = 0.0
       nullify(THnodes(inode)%adj)
       nullify(THnodes(inode)%Cij)       

    end do

    if (.not.allocated(diskx2nodes)) allocate(diskx2nodes(nb_DISKx))
    if (.not.allocated(joncx2nodes)) allocate(joncx2nodes(nb_JONCx))
    if (.not.allocated(xksid2nodes)) allocate(xksid2nodes(nb_xKSID))
    if (.not.allocated(polyg2nodes)) allocate(polyg2nodes(nb_POLYG))

    if (.not.allocated(nodes2rbdy2)) allocate(nodes2rbdy2(nb_NODES))

    diskx2nodes = 0.D0
    joncx2nodes = 0.D0
    xksid2nodes = 0.D0
    polyg2nodes = 0.D0

    nodes2rbdy2 = 0

    ! initialization thermal and electrical models

    MODtherm%lconv     = .FALSE.
    MODtherm%init      = .FALSE.
    MODtherm%iheat     = i_h_diff
    MODtherm%ibound    = i_d_adia
    MODtherm%ilkine    = i_lkine_all
    MODtherm%ildiff    = i_ldiff_none
    MODtherm%igdiff    = i_gdiff_discrete
    MODtherm%ilcond    = 0
    MODtherm%thickness = 0.d0
    MODtherm%T0        = 0.d0
    MODtherm%Alert     = 0.d0
    MODtherm%Gcond     = 0.d0
    MODtherm%GTemp     = 0.d0

    MODelec%current = i_c_vanish
    MODelec%tension = i_t_vanish
    MODelec%local   = i_l_allno
    MODelec%init    = .FALSE.
    MODelec%oxide   = .FALSE.
    MODelec%sliding = .FALSE.
    MODelec%breakdown = .FALSE.
    MODelec%A       = 0.d0
    MODelec%B       = 0.d0
    MODelec%Omega   = 0.d0
    MODelec%Phi     = 0.d0
    MODelec%Coxi    = 0.d0
    MODelec%breakdown_threshold = 0.d0
    MODelec%sliding_threshold   = 0.d0
    MODelec%breakdown_var       = 0.d0
    MODelec%TOL       = 0.d0
    MODelec%NLTOL     = 0.d0
    MODelec%itermax   = 0.d0
    MODelec%NLitermax = 0.d0

    !jr case

    nb_GRAIN = nb_DISKx + nb_POLYG

    if(nb_GRAIN .ne. 0) then 
       if(.not.allocated(coordinance)) allocate(coordinance(nb_GRAIN))
    end if

  end subroutine init_mp_solver
!!!------------------------------------------------------
  !> \brief read
  subroutine read_in_mp_behaviour_mp_solver
    implicit none
    integer(kind=4) :: inode,itact,ibdyty

    G_nfich = get_io_unit()

    open(unit=G_nfich,file=trim(location(in_mpdem(:))))
    call read_mp_behaviour
    close(G_nfich)

    ! electrical part
    do ibdyty = 1,nb_RBDY2
       ELnodes(ibdyty)%ID_RBDY2 = ibdyty
    end do


    ! thermal part
    inode = 0

    do itact = 1,nb_DISKx
       inode = inode + 1
       diskx2nodes(itact) = inode
       THnodes(inode)%ID_RBDY2 = diskx2bdyty(1,itact)
       THnodes(inode)%ID_TACTY = diskx2bdyty(2,itact)

       nodes2rbdy2(inode) = THnodes(inode)%ID_RBDY2
    end do

    do itact = 1,nb_JONCx
       inode = inode + 1
       joncx2nodes(itact) = inode
       THnodes(inode)%ID_RBDY2 = joncx2bdyty(1,itact)
       THnodes(inode)%ID_TACTY = joncx2bdyty(2,itact)

       nodes2rbdy2(inode) = THnodes(inode)%ID_RBDY2
    end do

    do itact = 1,nb_POLYG
       inode = inode + 1
       polyg2nodes(itact) = inode
       THnodes(inode)%ID_RBDY2 = polyg2bdyty(1,itact)
       THnodes(inode)%ID_TACTY = polyg2bdyty(2,itact)

       nodes2rbdy2(inode) = THnodes(inode)%ID_RBDY2
    end do

    do itact = 1,nb_xKSID
       inode = inode + 1
       xksid2nodes(itact) = inode
       THnodes(inode)%ID_RBDY2 = xksid2bdyty(1,itact)
       THnodes(inode)%ID_TACTY = xksid2bdyty(2,itact)

       nodes2rbdy2(inode) = THnodes(inode)%ID_RBDY2
    end do

    if (electro_model) then
       call logmes('  @ ELECTRICAL MODEL DEFINED')
       call init_behav_electrical_solver
    endif

    if (thermo_model)  then
       call logmes('  @ THERMAL MODEL DEFINED')
       call init_behav_thermal_solver
    end if

  end subroutine read_in_mp_behaviour_mp_solver
!!!-----------------------------------------------------------    
  subroutine read_mp_behaviour
    implicit none
    integer(kind=4)    :: err
    integer(kind=4)    :: imodel,iTHsource,iTHbound,iELbound
    character(len=35)  :: IAM
    character(len=30)  :: e_clin
    character(len=29)  :: cout
    character(len=1)   :: signe

    IAM = 'mod_mp_solver::read_mp_behaviour'

    ! first read model
    err    = 0
    imodel = 0

    do
       
       if( .not. read_G_clin()) exit

       if (G_clin(2:6) .ne. 'model') cycle

       select case(G_clin(9:13))

!!!****ELECTRO CASE********************************

       case('elec_')
          electro_model = .true.
          imodel = imodel + 1
          !*curnt*************************************
          if( .not. read_G_clin()) then
             call LOGMES('You specified a elec_ model but no keyword are defined')
             call LOGMES('Check your MP_DEM.DAT file')
             stop
          end if
          if ((G_clin(15:19) .ne. 'curnt').and.(G_clin(15:19) .ne. 'tensn')) then
             call LOGMES('keyword curnt or tensn are not defined')
             stop
          end if
          select case(G_clin(15:19))
          case('curnt')
             select case(G_clin(22:26))
             case('const')
                !* I = A
                MODelec%current = i_c_cons
                read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%A
                if(err.ne.0)then
                   call LOGMES('Error during reading current value: a real value is missing.')
                   stop
                end if
             case('linea')
                !* I = A*MIN(1.,B*t)
                MODelec%current = i_c_line
                read(G_clin(21:50),'(D14.7,2X,D14.7)',iostat=err) MODelec%A,MODelec%B
                if(err.ne.0)then
                   call LOGMES('Error during reading current values: two real values are missing.')
                   stop
                end if
             case('alter')
                !* I = A + B*COS(OMEGA*t+Phi)
                MODelec%current = i_c_alte
                read(G_clin(21:84),'(4(D14.7,2X))',iostat=err) MODelec%A,MODelec%B,MODelec%Omega,MODelec%Phi
                if(err.ne.0)then
                   call LOGMES('Error during reading current values: four real values are missing.')
                   stop
                end if
             case DEFAULT
                call LOGMES('current type is not defined')
                call LOGMES('choose one in : cons/line/alte')
                stop
             end select
          case('tensn')
             select case(G_clin(22:26))
             case('const')
                !* U = A
                MODelec%tension = i_t_cons
                read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%A
                if(err.ne.0)then
                   call LOGMES('Error during reading tension value: a real value is missing.')
                   stop
                end if
             case('linea')
                !* U = A*MIN(1.,B*t)
                MODelec%tension = i_t_line
                read(G_clin(21:50),'(D14.7,2X,D14.7)',iostat=err) MODelec%A,MODelec%B
                if(err.ne.0)then
                   call LOGMES('Error during reading tension values: two real values are missing.')
                   stop
                end if
             case('alter')
                !* U = A + B*COS(OMEGA*t+Phi)
                MODelec%tension = i_t_alte
                read(G_clin(21:84),'(4(D14.7,2X))',iostat=err) MODelec%A,MODelec%B,MODelec%Omega,MODelec%Phi
                if(err.ne.0)then
                   call LOGMES('Error during reading tension values: four real values are missing.')
                   stop
                end if
             case DEFAULT
                call LOGMES('tension type is not defined')
                call LOGMES('choose one in : cons/line/alte')
                stop
             end select
          end select

          !*local*************************************
          if( .not. read_G_clin()) then
             call LOGMES('keywords are missing. Complete the electrical model.')
             stop
          end if
          if (G_clin(15:19) .ne. 'local') then
             call LOGMES('keyword local not defined')
             stop
          else
             select case(G_clin(22:26))
             case('hertz')
                MODelec%local = i_l_Hertz
             case('allno')
                MODelec%local = i_l_allno
             case('HolmH')
                MODelec%local = i_l_Holm_Hertz
             case('HolmP')
                MODelec%local = i_l_Holm_plast
             case('Tkaya')
                MODelec%local = i_l_Tekaya
             case DEFAULT
                call LOGMES('local type is not defined')
                call LOGMES('choose one in : hertz/allno/HolmH/HolmP/Tkaya')
                stop
             end select
             !* iter_ ********************
             if( .not. read_G_clin()) then
                call LOGMES('keyword missing')
                stop
             end if
             if (G_clin(22:26) .ne. 'iter_') then
                call LOGMES('keyword iter_ missing')
                stop
             else
                read(G_clin(29:35),'(I7)',iostat=err) MODelec%itermax
                if(err.ne.0)then
                   call LOGMES('Error during reading itermax value')
                   stop
                end if
             end if
             !* tol__ ********************
             if( .not. read_G_clin()) then
                call LOGMES('keyword missing')
                stop
             end if
             if (G_clin(22:26) .ne. 'tol__') then
                call LOGMES('keyword tol__ missing')
                stop
             else
                read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%TOL
                if(err.ne.0)then
                   call LOGMES('Error during reading tol value')
                   stop
                end if
             end if
          end if

          !*oxide*************************************
          if( .not. read_G_clin()) then
             call LOGMES('keyword missing')
             stop
          end if
          if (G_clin(15:19) .ne. 'oxide') then
             call LOGMES('keyword oxide not defined')
             stop
          else
             select case(G_clin(22:24))
             case('yes')
                MODelec%oxide = .true.
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'Cond_') then
                   call LOGMES('keyword Cond_ not defined')
                   stop
                else
                   read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%Coxi
                   if(err.ne.0)then
                      call LOGMES('Error during reading Coxi value')
                      stop
                   end if
                end if
                !* iter_ ********************
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'iter_') then
                   call LOGMES('keyword iter_ missing')
                   stop
                else
                   read(G_clin(29:35),'(I7)',iostat=err) MODelec%NLitermax
                   if(err.ne.0)then
                      call LOGMES('Error during reading NLitermax value')
                      stop
                   end if
                end if
                !* tol__ ********************
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'tol__') then
                   call LOGMES('keyword tol__ missing')
                   stop
                else
                   read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%NLTOL
                   if(err.ne.0)then
                      call LOGMES('Error during reading NLtol value')
                      stop
                   end if
                end if
             case('no ')
                MODelec%oxide = .false.
             case DEFAULT
                call LOGMES('problem for the oxide definition')
                stop
             end select
          end if

          !*brkdw*************************************
          if( .not. read_G_clin()) then
             call LOGMES('keyword missing')
             stop
          end if
          if (G_clin(15:19) .ne. 'brkdw') then
             call LOGMES('keyword brkdw not defined')
             stop
          else
             select case(G_clin(22:24))
             case('yes')
                MODelec%breakdown = .true.
                !---------------------------------
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'thsld') then
                   call LOGMES('keyword thsld missing')
                   stop
                else
                   read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%breakdown_threshold
                   if(err.ne.0)then
                      call LOGMES('Error during reading value')
                      stop
                   end if
                end if
                !---------------------------------
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'var__') then
                   call LOGMES('keyword var__ missing')
                   stop
                else
                   read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%breakdown_var
                   if(err.ne.0)then
                      call LOGMES('Error during reading value')
                      stop
                   end if
                end if

                !----------------------------------
             case('no ')
                MODelec%breakdown = .false.
             case DEFAULT
             end select
          end if
          !*sldng*************************************
          if( .not. read_G_clin()) then
             call LOGMES('keyword missing')
             stop
          end if
          if (G_clin(15:19) .ne. 'sldng') then
             call LOGMES('keyword sldng not defined')
             stop
          else
             select case(G_clin(22:24))
             case('yes')
                MODelec%sliding = .true.
                !---------------------------------
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'thsld') then
                   call LOGMES('keyword thsld missing')
                   stop
                else
                   read(G_clin(28:41),'(D14.7)',iostat=err) MODelec%sliding_threshold
                   if(err.ne.0)then
                      call LOGMES('Error during reading sliding threshold value')
                      stop
                   end if
                end if
                !----------------------------------
             case('no ')
                MODelec%sliding = .false.
             case DEFAULT
             end select
          end if

!!!****THERMO CASE**********************************

       case('therm')

          thermo_model = .true.
          imodel = imodel + 1

          !* T0___
          if( .not. read_G_clin()) exit
          if (G_clin(15:19) .ne. 'T0___') then
             call LOGMES('keyword T0___ not defined')
             stop
          else
             read(G_clin(21:34),'(D14.7)',iostat=err) MODtherm%T0
             if(err.ne.0)then
                call LOGMES('Error during reading value')
                stop
             end if
          end if

          !* alert
          if( .not. read_G_clin()) exit
          if (G_clin(15:19) .ne. 'alert') then
             call LOGMES('keyword alert not defined')
             stop
          else
             read(G_clin(21:34),'(D14.7)',iostat=err) MODtherm%Alert
             if(err.ne.0)then
                call LOGMES('Error during reading value')
                stop
             end if
          end if

          !* ldiff************************************
          if( .not. read_G_clin()) exit
          if (G_clin(15:19) .ne. 'ldiff') then
             call LOGMES('keyword ldiff not defined')
             stop
          else
             select case(G_clin(22:26))
             case('Hertz')
                MODtherm%ildiff = i_ldiff_Hertz
             case('Cylnd')
                MODtherm%ildiff = i_ldiff_Cylnd
             case DEFAULT
                call LOGMES('ldiff case not implemented')
                stop
             end select

             select case(G_clin(28:35))
             case('continue')
                MODtherm%igdiff = i_gdiff_continue
             case('discrete')
                MODtherm%igdiff = i_gdiff_discrete
             case DEFAULT
                call LOGMES('gdiff is not defined (discrete or continue)')
                call LOGMES('default value is used (discrete)')
                MODtherm%igdiff = i_gdiff_discrete
             end select
          end if

          !* lconv************************************
          if( .not. read_G_clin()) exit
          if (G_clin(15:19) .ne. 'lconv') then
             call LOGMES('keyword lconv not defined')
             stop
          else
             select case(G_clin(22:24))
             case('yes')

                MODtherm%lconv = .true.
                !---------------------------------
                if( .not. read_G_clin()) then
                   call LOGMES('keyword missing')
                   stop
                end if
                if (G_clin(22:26) .ne. 'Gcond') then
                   call LOGMES('keyword Gcond missing')
                   stop
                else
                   read(G_clin(28:62),'(D14.7,8X,D14.7)',iostat=err) MODtherm%Gcond,MODtherm%GTemp
                   if(err.ne.0)then
                      call LOGMES('Error during reading sliding threshold value')
                      stop
                   end if
                end if
                !----------------------------------

             case('no ')
                MODtherm%lconv = .false.
             case DEFAULT
                call LOGMES('lconv case not implemented')
                stop
             end select
          end if

          !*lkine*************************************
          if( .not. read_G_clin()) exit
          if (G_clin(15:19) .ne. 'lkine') then
             call LOGMES('keyword lkine not defined')
             stop
          else
             select case(G_clin(22:24))
             case('all')
                MODtherm%ilkine = i_lkine_all
             case('dvt')
                MODtherm%ilkine = i_lkine_dvt
             case('dvn')
                MODtherm%ilkine = i_lkine_dvn
             case('no ')
                MODtherm%ilkine = i_lkine_no
             case DEFAULT
                call LOGMES('lkine case not implemented')
                stop
             end select
          end if

          !*bound*************************************
          if( .not. read_G_clin()) exit
          if (G_clin(15:19) .ne. 'bound') then
             call LOGMES('keyword bound not defined')
             stop
          else
             select case(G_clin(22:25))
             case('adia')
                MODtherm%ibound = i_d_adia
             case('line')
                MODtherm%ibound = i_d_line
             case('1D__')
                MODtherm%ibound = i_d_1D__
             case('2D__')
                MODtherm%ibound = i_d_2D__
             case DEFAULT                  !12345678901                 1234567890123456
                write(cout,'(A11,A4,A16)') 'bound case ',G_clin(22:25),' not implemented'
                call LOGMES(cout)
                stop
             end select
          end if

!!!****DEFAULT CASE*********************************
       case DEFAULT
          call LOGMES('model not implemented')
          stop
       end select
    end do

    rewind(G_nfich)

    write(6,'(1X,I2,1X,A13)') imodel,' models found'

    ! second read size sources and bounds

    iTHsource = 0
    iTHbound  = 0
    iELbound  = 0

    do
       if( .not. read_G_clin()) exit
       select case(G_clin(2:7))

       case('source')
          select case(G_clin(10:14))
          case('therm')
             iTHsource = iTHsource + 1
          case DEFAULT
             stop
          end select

       case('bounds')
          select case(G_clin(10:14))
          case('therm')
             iTHbound = iTHbound + 1
          case('elec_')
             iELbound = iELbound + 1
          case DEFAULT
             stop
          end select
          
       case DEFAULT

       end select
    end do
    
    rewind(G_nfich)
    
    write(6,'(1X,I3,1X,A25)') iTHsource,' thermal sources found  '
    write(6,'(1X,I3,1X,A25)') iTHbound, ' thermal bounds found   '
    write(6,'(1X,I3,1X,A25)') iELbound, ' electrical bounds found'

    nb_TH_SOURCES = iTHsource
    nb_TH_BOUNDS  = iTHbound
    nb_EL_BOUNDS  = iELbound

    !* Allocate bounds and sources *

    if ( allocated(TH_SRC_NODES) ) deallocate(TH_SRC_NODES) 
    allocate(TH_SRC_NODES(nb_TH_SOURCES))

    if ( allocated(TH_BND_NODES) ) deallocate(TH_BND_NODES) 
    allocate(TH_BND_NODES(nb_TH_BOUNDS))

    if ( allocated(EL_BND_NODES) ) deallocate(EL_BND_NODES) 
    allocate(EL_BND_NODES(nb_EL_BOUNDS))

    !* ************************** *

    do iTHsource = 1,nb_TH_SOURCES
       TH_SRC_NODES(iTHsource)%ifirst    = 0
       TH_SRC_NODES(iTHsource)%ilast     = 0
       TH_SRC_NODES(iTHsource)%internal  = 0.D0
       TH_SRC_NODES(iTHsource)%T         = 0.D0
       TH_SRC_NODES(iTHsource)%sgnB      = 0.D0
       TH_SRC_NODES(iTHsource)%thickness = 0.D0
    end do

    do iTHbound = 1,nb_TH_BOUNDS

       TH_BND_NODES(iTHbound)%ifirst    = 0
       TH_BND_NODES(iTHbound)%ilast     = 0
       TH_BND_NODES(iTHbound)%internal  = 0.D0
       TH_BND_NODES(iTHbound)%thickness = 0.D0
       TH_BND_NODES(iTHbound)%length    = 0.D0
       TH_BND_NODES(iTHbound)%T         = 0.D0
       TH_BND_NODES(iTHbound)%sgnB      = 0.D0
       TH_BND_NODES(iTHbound)%NX        = 0
       TH_BND_NODES(iTHbound)%NY        = 0
       nullify(TH_BND_NODES(iTHbound)%TSOURCE)
       nullify(TH_BND_NODES(iTHbound)%TPROFIL)
       nullify(TH_BND_NODES(iTHbound)%TBULK)
       nullify(TH_BND_NODES(iTHbound)%TBULKi)

    end do

    do iELbound = 1,nb_EL_BOUNDS
       EL_BND_NODES(iELbound)%ifirst    = 0
       EL_BND_NODES(iELbound)%ilast     = 0
       EL_BND_NODES(iELbound)%internal  = 0.D0
       EL_BND_NODES(iELbound)%thickness = 0.D0
       EL_BND_NODES(iELbound)%length    = 0.D0
       EL_BND_NODES(iELbound)%sgnB      = 0.D0
       EL_BND_NODES(iELbound)%T         = 0.D0
    end do

    !* ************************** *

    iTHsource = 0
    iTHbound  = 0
    iELbound  = 0

    do
       if( .not. read_G_clin()) exit

       select case(G_clin(2:7))
       case('source')

          select case(G_clin(10:14))
          case('therm')
             iTHsource = iTHsource + 1
          
             if( .not. read_G_clin() )then
                call LOGMES('Value missing')
                stop
             end if
             !********
             if (G_clin(19:20) .eq. 'to') then
                read(G_clin(10:29),'(I7,6X,I7)') TH_SRC_NODES(iTHsource)%ifirst,TH_SRC_NODES(iTHsource)%ilast
             else
                read(G_clin(10:16),'(I7)') TH_SRC_NODES(iTHsource)%ifirst
                TH_SRC_NODES(iTHsource)%ilast = TH_SRC_NODES(iTHsource)%ifirst
             end if
             !********
             read(G_clin(37:50),'(D14.7)') TH_SRC_NODES(iTHsource)%T
             !********
          case DEFAULT
             stop
          end select

       case('bounds')
          
          select case(G_clin(10:14))
          case('therm')
             iTHbound = iTHbound + 1
             select case(G_clin(16:16))
             case('U')
                TH_BND_NODES(iTHbound)%idirection = i_upxxx
             case('D')
                TH_BND_NODES(iTHbound)%idirection = i_downx
             case('R')
                TH_BND_NODES(iTHbound)%idirection = i_right
             case('L')
                TH_BND_NODES(iTHbound)%idirection = i_leftx
             case DEFAULT
                TH_BND_NODES(iTHbound)%idirection = i_default
             end select

             if( .not. read_G_clin() )then
                call LOGMES('Value missing')
                stop
             end if
             !********
             if (G_clin(19:20) .eq. 'to') then
                read(G_clin(10:29),'(I7,6X,I7)') TH_BND_NODES(iTHbound)%ifirst,TH_BND_NODES(iTHbound)%ilast
             else
                read(G_clin(10:16),'(I7)') TH_BND_NODES(iTHbound)%ifirst
                TH_BND_NODES(iTHbound)%ilast = TH_BND_NODES(iTHbound)%ifirst
             end if
             !********
             read(G_clin(37:74),'(D14.7,7X,D14.7)') TH_BND_NODES(iTHbound)%T, &
                                                    TH_BND_NODES(iTHbound)%thickness
             if( .not. read_G_clin() )then
                call LOGMES('Value missing')
                stop
             end if

             read(G_clin(37:74),'(D14.7,7X,D14.7)') TH_BND_NODES(iTHbound)%length, &
                                                    TH_BND_NODES(iTHbound)%ALPHA

             !********
          case('elec_')
             iELbound = iELbound + 1

             if( .not. read_G_clin() )then
                call LOGMES('Value missing')
                stop
             end if
             !********

             read(G_clin(10:16),'(I7)') EL_BND_NODES(iELbound)%ifirst
             EL_BND_NODES(iELbound)%ilast = EL_BND_NODES(iELbound)%ifirst
             
             !********

             read(G_clin(38:38),'(A1)') signe
             if (signe.eq.'+') then
                if(MODelec%current.eq.i_c_vanish)then
                   EL_BND_NODES(iELbound)%sgnB = 1.D0 ! tension drive boundary
                else
                   EL_BND_NODES(iELbound)%sgnB = 1.D0 ! current drive boundary
                end if
             else if(signe.eq.'-')then
                if(MODelec%current.eq.i_c_vanish)then
                   EL_BND_NODES(iELbound)%sgnB =-1.D0 ! tension drive boundary
                else
                   EL_BND_NODES(iELbound)%sgnB =-1.D0 ! current drive boundary
                end if
             end if
             !********
          case DEFAULT
             stop
          end select

       case DEFAULT
          
       end select

    end do

    rewind(G_nfich)

  end subroutine read_mp_behaviour
!!!-----------------------------------------------------------
  subroutine read_ini_mp_values_mp_solver(step)
    implicit none
    integer(kind=4), intent(in) :: step
    !
    integer(kind=4) :: ibdyty,inode
    character(len=103) :: cout
    character(len=33)  :: IAM
    
    IAM = 'mod_mp_solver::read_ini_mp_values'

    G_nfich=get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_mpv(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_mpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_mpv(:))))
    end if

    do
       if ( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'nodty') cycle
       read(G_clin(9:15),'(I7)') inode
       ibdyty = nodes2rbdy2(inode)
       read(G_clin(23:57),'(D14.7,7X,D14.7)') ELnodes(ibdyty)%V,THnodes(inode)%Tini
       THnodes(inode)%T = THnodes(inode)%Tini
       cycle
    end do

    close(G_nfich)    

    ! electrical model
    do ibdyty =1,nb_RBDY2
       call put_electric_potentiel(ibdyty,ELnodes(ibdyty)%V)
    end do

    ! thermal model
    do inode =1,nb_NODES
       call put_thermal_value(THnodes(inode)%ID_RBDY2,THnodes(inode)%ID_TACTY,THnodes(inode)%Tini)
    end do

  end subroutine read_ini_mp_values_mp_solver
!!!--------------------------------------------------------------------------------------
  subroutine write_xxx_mp_values_mp_solver(which)
    implicit none
    integer, intent(in) :: which
    !
    integer(kind=4) :: nfich,ibdyty,inode
    character(len=103) :: cout
    CHARACTER(len=38)  :: IAM
    
    IAM = 'mod_mp_solver_3D::write_xxx_mp_values'

    select case(which)
    case(1)
       nfich = get_io_unit()
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',&
            file=trim(location(out_mpv(:))))
    case(2)
       nfich = get_io_unit()
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',&
            file=trim(location(last_mpv(:))))
    case(6)
       nfich = which
    end select

    do inode=1,nb_NODES
       ibdyty = nodes2rbdy2(inode)
       write(nfich,'(A6,2X,I7,2(2X,A5,D14.7))') '$nodty',inode,'PotE=',ELnodes(ibdyty)%V,'Temp=',THnodes(inode)%Tini
       write(nfich,'(A6)') '      '
    end do

    if( nfich /= 6 ) close(nfich)

  end subroutine write_xxx_mp_values_mp_solver
!!!--------------------------------------------------------------------------------------
  subroutine init_behav_electrical_solver
    implicit none
    integer(kind=4)   :: ibdyty,itacty,itact,inode,ibound
    character(len=30) :: cout
    real(kind=8)      :: Concd,rho,avrd,Hspe,TMP,TCdown

    Cmean = 0.D0

    do ibdyty=1,nb_RBDY2

       ELnodes(ibdyty)%AM_I_EL_BND = .false.
       ELnodes(ibdyty)%iELbound  = 0

       do ibound=1,nb_EL_BOUNDS

          if (ibdyty.ne.EL_BND_NODES(ibound)%ifirst) cycle

          Elnodes(ibdyty)%AM_I_EL_BND = .true.
          ELnodes(ibdyty)%iELbound  = ibound

          exit

       end do

       call put_electric_potentiel(ibdyty,ELnodes(ibdyty)%V)

       Cmean = Cmean + get_elec_cond(ibdyty)

    end do

    Cmean = Cmean/real(nb_RBDY2,8)
    
    if (MODelec%oxide) then

       if ( allocated(ElectricalNodes) ) deallocate(ElectricalNodes)
       allocate(ElectricalNodes(nb_RBDY2))

       do ibdyty = 1,nb_RBDY2
          ElectricalNodes(ibdyty)%nbadj = 0
          ElectricalNodes(ibdyty)%iadj  = 0
          nullify(ElectricalNodes(ibdyty)%adj)
          nullify(ElectricalNodes(ibdyty)%oxided)
          nullify(ElectricalNodes(ibdyty)%threshold)
          nullify(ElectricalNodes(ibdyty)%UI)
       end do

    end if

  end subroutine init_behav_electrical_solver
!!!-----------------------------------------------------------
  subroutine init_behav_thermal_solver
    implicit none
    integer(kind=4)   :: ibdyty,itacty,itact,inode,isource,ibound
    character(len=30) :: cout
    logical           :: FLAG
    real(kind=8)      :: Concd,rho,avrd,Hspe,TMP,TCdown

    integer(kind=4)           :: icdbdy,ianbdy,icdtac,iantac
    integer(kind=4)           :: icdnode,iannode,imltnode
    real(kind=8)              :: rcd,ran,reff,dist
    real(kind=8),dimension(3) :: coorcd,cooran,sep

    inode = 0

    ! first : thermal bound and source case

    do inode=1,nb_NODES

       ibdyty = THnodes(inode)%ID_RBDY2

       do isource=1,nb_TH_SOURCES

          if (ibdyty.lt.TH_SRC_NODES(isource)%ifirst) cycle
          if (ibdyty.gt.TH_SRC_NODES(isource)%ilast) cycle
          
          THnodes(inode)%AM_I_TH_SRC = .true.
          THnodes(inode)%iTHsource   = isource
          exit

       end do

       do ibound=1,nb_TH_BOUNDS

          if (ibdyty.lt.TH_BND_NODES(ibound)%ifirst) cycle
          if (ibdyty.gt.TH_BND_NODES(ibound)%ilast) cycle

          THnodes(inode)%AM_I_TH_BND = .true.
          THnodes(inode)%iTHbound = ibound

          TH_BND_NODES(ibound)%NX = TH_BND_NODES(ibound)%NX + 1

          exit

       end do

    end do

    do ibound=1,nb_TH_BOUNDS
       if (TH_BND_NODES(ibound)%NX.eq.0) cycle
       allocate(TH_BND_NODES(ibound)%TSOURCE(TH_BND_NODES(ibound)%NX))
       TH_BND_NODES(ibound)%NX      = 0
       TH_BND_NODES(ibound)%TSOURCE = 0
    end do

    do inode=1,nb_NODES

       ibdyty = THnodes(inode)%ID_RBDY2

       do ibound=1,nb_TH_BOUNDS

          if (ibdyty.lt.TH_BND_NODES(ibound)%ifirst) cycle
          if (ibdyty.gt.TH_BND_NODES(ibound)%ilast) cycle

          THnodes(inode)%AM_I_TH_BND = .true.
          THnodes(inode)%iTHbound = ibound

          TH_BND_NODES(ibound)%NX = TH_BND_NODES(ibound)%NX + 1
          TH_BND_NODES(ibound)%TSOURCE(TH_BND_NODES(ibound)%NX) = inode
          exit

       end do

    end do

    do ibound=1,nb_TH_BOUNDS
       call INIT_BULK_MATRIX(ibound)
    end do
    
    !second : node case

    do inode = 1,nb_NODES

       ibdyty = THnodes(inode)%ID_RBDY2
       itacty = THnodes(inode)%ID_TACTY

       rho  = get_rho(get_bulk_behav_number_RBDY2(ibdyty,1))
       Hspe = get_Hspe(get_bulk_behav_number_RBDY2(ibdyty,1))
       avrd = get_avr_radius_tacty(ibdyty,itacty)

       TMP = rho*Hspe*avrd*avrd*PI_g

       THnodes(inode)%alpha = 0.D0
       if ( TMP .gt. 1.D-16 ) THnodes(inode)%alpha = H/TMP

       if (THnodes(inode)%AM_I_TH_SRC) then
          THnodes(inode)%Tini  = TH_SRC_NODES(THnodes(inode)%iTHsource)%T
          THnodes(inode)%T     = THnodes(inode)%Tini
          THnodes(inode)%dTini = 0.D0
          THnodes(inode)%dT    = 0.D0
       else
          THnodes(inode)%Tini  = MODtherm%T0
          THnodes(inode)%T     = MODtherm%T0
          THnodes(inode)%dTini = 0.D0
          THnodes(inode)%dT    = 0.D0
       end if

       call put_thermal_value(ibdyty,itacty,THnodes(inode)%Tini)

    end do

    !* third: multi-node case

    !* a) sizing vector

    imltnode = 0
    
    do icdnode = 1,nb_NODES

       icdbdy = THnodes(icdnode)%ID_RBDY2
       icdtac = THnodes(icdnode)%ID_TACTY

       do iannode = icdnode+1,nb_NODES 

          ianbdy = THnodes(iannode)%ID_RBDY2

          if(icdbdy.ne.ianbdy) cycle
          !* MULTI-TACTOR CASE

          iantac = THnodes(iannode)%ID_TACTY

          coorcd = get_coor(icdbdy,icdtac)
          cooran = get_coor(ianbdy,iantac)

          sep  = coorcd - cooran
          dist = sep(1)*sep(1) + sep(2)*sep(2)

          if (sqrt(dist) .gt. MODtherm%alert) cycle
          
          imltnode = imltnode + 1

       end do

    end do

    !* 
    nb_MLT_NODES = imltnode

    if (nb_MLT_NODES.eq.0) then
       call LOGMES('Warning!')
       call LOGMES('No MULTI-NODE according to the definition of MODtherm%alert')
    else
       write(6,'(I7,A18)') nb_MLT_NODES,' MULTI-NODES found' 
       if (allocated(MultiNodes)) deallocate(MultiNodes)
       allocate(MultiNodes(nb_MLT_NODES))
       do imltnode = 1,nb_MLT_NODES
          MultiNodes(imltnode)%CD   = 0
          MultiNodes(imltnode)%AN   = 0
          MultiNodes(imltnode)%Area = 0.D0
       end do
    end if
    !*

    imltnode = 0

    do icdnode = 1,nb_NODES

       icdbdy = THnodes(icdnode)%ID_RBDY2
       icdtac = THnodes(icdnode)%ID_TACTY

       do iannode = icdnode+1,nb_NODES 

          ianbdy = THnodes(iannode)%ID_RBDY2
          iantac = THnodes(iannode)%ID_TACTY

          if(icdbdy.ne.ianbdy) cycle
          !* MULTI-TACTOR CASE

          coorcd = get_coor(icdbdy,icdtac)
          cooran = get_coor(ianbdy,iantac)

          sep  = coorcd - cooran
          dist = sep(1)*sep(1) + sep(2)*sep(2)

          if (sqrt(dist) .gt. MODtherm%alert) cycle

          imltnode = imltnode + 1

          MultiNodes(imltnode)%CD = icdnode
          MultiNodes(imltnode)%AN = iannode

          rcd = get_avr_radius_tacty(icdbdy,icdtac)
          ran = get_avr_radius_tacty(ianbdy,iantac)

          reff = (rcd+ran)*0.5
          MultiNodes(imltnode)%Area = 2.0*reff

       end do

    end do

  end subroutine init_behav_thermal_solver
!!!--------------------------------------------------------------------------------------
  subroutine solve_electro1G
    implicit none
    integer(kind=4) :: inode,icdan,ibdyty,itacty,inet,iadj,nbadj,ibound
    integer(kind=4) :: icdnode,iannode,icdtac,iantac,adjsz,ial1,ial2,iik
    real(kind=8)    :: rlt,rln,ra
    real(kind=8)    :: Concd,Conan,Conct,Conby,Ctot,Cii,Condini
    real(KIND=8)    :: Resct,Resby,Tup,duk
    real(KIND=8)    :: Pot,Itot,Apot,PotUPxxx,PotDOWNx
    integer(kind=4) :: status

    logical :: inbound

    Req   = 0.0

    nb_CDAN = 0
    nb_DKDKx = get_nb_inters( i_dkdkx )
    nb_CDAN = nb_CDAN + nb_DKDKx

    if (allocated(dkdkx2branches)) deallocate(dkdkx2branches)
    allocate(dkdkx2branches(nb_DKDKx))
    dkdkx2branches = 0

    nb_DKJCx = get_nb_inters( i_dkjcx )
    nb_CDAN = nb_CDAN + nb_DKJCx

    if (allocated(dkjcx2branches)) deallocate(dkjcx2branches)
    allocate(dkjcx2branches(nb_DKJCx))
    dkjcx2branches = 0

    nb_DKKDx = get_nb_inters( i_dkkdx )
    nb_CDAN = nb_CDAN + nb_DKKDx

    if (allocated(dkkdx2branches)) deallocate(dkkdx2branches)
    allocate(dkkdx2branches(nb_DKKDx))
    dkkdx2branches = 0

    if ( nb_CDAN == 0 ) return 

    if ( allocated(Branches) ) deallocate(Branches)
    allocate(Branches(nb_CDAN))
!!!*
    Conan = 0.0
    Concd = 0.0
!!!*
    inet  = 0

    do ibdyty = 1,nb_RBDY2
       Elnodes(ibdyty)%iadj = 0
    end do

    select case(MODelec%local)
    case(i_l_allno,i_l_Hertz,i_l_Holm_Hertz,i_l_Holm_plast,i_l_Tekaya)
       call fill_active_contact(inet)
    case default
       print*,'local model not defined'
       stop
    end select

    if ( inet == 0 ) then
       call logmes('warning no active network case')
       return
    end if

!!! matrix construction --------------------------------------------------

    call assemble_conductivity_matrix()

!!! Initialization --------------------------------------------------------

!!!* current *

    if(MODelec%current.ne.i_c_vanish)then

       select case(MODelec%current)
       case(i_c_cons)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_c_line)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*min(1.D0,MODelec%B*TPS)
             ELnodes(ibdyty)%Idriv = ELnodes(ibdyty)%Idriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_c_alte)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*sin(MODelec%Omega*TPS+MODelec%Phi)
             ELnodes(ibdyty)%Idriv = ELnodes(ibdyty)%Idriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case default
          stop       
       end select

    else if(MODelec%tension.ne.i_t_vanish)then

       select case(MODelec%tension)
       case(i_t_cons)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Vdriv = 0.5*MODelec%A*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_t_line)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Vdriv = MODelec%A*min(1.D0,MODelec%B*TPS)
             ELnodes(ibdyty)%Vdriv = ELnodes(ibdyty)%Vdriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_t_alte)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Vdriv = MODelec%A*sin(MODelec%Omega*TPS+MODelec%Phi)
             ELnodes(ibdyty)%Vdriv = ELnodes(ibdyty)%Vdriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case default
          stop       
       end select
    end if
!!!
!!!* Potential *
!!!
    if (MODelec%init) then
       do ibdyty = 1,nb_RBDY2
          if(ELnodes(ibdyty)%AM_I_EL_BND) cycle
          ELnodes(ibdyty)%V = get_electric_potentiel(ibdyty)
       end do
    else
       do ibdyty = 1,nb_RBDY2
          if(ELnodes(ibdyty)%AM_I_EL_BND) cycle
          ELnodes(ibdyty)%V = 0.0
       end do
    end if
!!!
    do ibdyty = 1,nb_RBDY2/2

       call random_number(ra)
       ial1 = int(ra*nb_RBDY2)+1
       ial1 = min(ial1,nb_RBDY2)
       ial1 = max(1,ial1)

       call random_number(ra)
       ial2 = int(ra*nb_RBDY2)+1
       ial2 = min(ial2,nb_RBDY2)
       ial2 = max(1,ial2)

       iik = randomlist(ial1)
       randomlist(ial1) = randomlist(ial2)
       randomlist(ial2) = iik

    end do
!!!
!!! start iteration --------------------------------------------------------
!!! Current drive
!!!
    if(MODelec%current.ne.i_c_vanish) then

       iter = 0
    
       do while ( iter < MODelec%itermax )

          iter = iter + 1
          E_ERR = 0.D0
          
          do ibdyty = 1,nb_RBDY2

             icdnode = randomlist(ibdyty)

             ibound  = ELnodes(icdnode)%iELbound
             inbound = .false.

             if (ibound.ne.0) then
                if(EL_BND_NODES(ibound)%sgnB.eq.1.d0) inbound = .true. 
             end if

             if(inbound)then
                ELnodes(icdnode)%V = 0.d0
             else

                if ( abs(ELnodes(icdnode)%Cii) .ge. 1.D-14 ) then
                   
                   Itot  = ELnodes(icdnode)%Idriv
                   nbadj = ELnodes(icdnode)%nbadj
                
                   do iadj = 1,nbadj
                      iannode = ELnodes(icdnode)%adj(iadj)
                      Itot = Itot - ELnodes(iannode)%V*ELnodes(icdnode)%Cij(iadj)
                   end do
                   
                   Pot = Itot/ELnodes(icdnode)%Cii
                   
                   if( abs(Pot) > 1.D-16 ) then
                      Apot = abs( (ELnodes(icdnode)%V-Pot )/Pot)
                   else
                      Apot = abs( ELnodes(icdnode)%V-Pot )
                   end if
                   
                   ELnodes(icdnode)%V = Pot
                   E_ERR = max( E_ERR , Apot )
                   
                end if
             end if
          end do
          
          if ( E_ERR < MODelec%TOL ) exit
          
       end do

    end if

!!! Tension drive

    if(MODelec%tension.ne.i_t_vanish) then

       iter = 0
    
       do while ( iter < MODelec%itermax ) 
       
          iter = iter + 1
          E_ERR = 0.D0

          do ibdyty = 1,nb_RBDY2

             icdnode = randomlist(ibdyty)

             if(ELnodes(icdnode)%AM_I_EL_BND)then
                ELnodes(icdnode)%V = ELnodes(icdnode)%Vdriv
             else

                if ( abs(ELnodes(icdnode)%Cii) .ge. 1.D-14 ) then
                   
                   Itot  = 0.d0
                   nbadj = ELnodes(icdnode)%nbadj
                   
                   do iadj = 1,nbadj
                      iannode = ELnodes(icdnode)%adj(iadj)
                      Itot = Itot - ELnodes(iannode)%V*ELnodes(icdnode)%Cij(iadj)
                   end do
                   
                   Pot = Itot/ELnodes(icdnode)%Cii
                   
                   if( abs(Pot) > 1.D-16 ) then
                      Apot = abs( (ELnodes(icdnode)%V-Pot )/Pot)
                   else
                      Apot = abs( ELnodes(icdnode)%V-Pot )
                   end if
                   
                   ELnodes(icdnode)%V = Pot
                   E_ERR = max( E_ERR , Apot )
                   
                end if
             end if
          end do
          
          if ( E_ERR < MODelec%TOL ) exit
          
       end do

    end if

!!! end of iteration ---------------------------------------------------------

    do icdnode=1,nb_RBDY2
       
       call put_electric_potentiel(icdnode,ELnodes(icdnode)%V )
       
       Itot  = ELnodes(icdnode)%Idriv
       nbadj = ELnodes(icdnode)%nbadj
       
       do iadj = 1,nbadj
          iannode = ELnodes(icdnode)%adj(iadj)
          Itot = Itot + ELnodes(iannode)%V*ELnodes(icdnode)%Cij(iadj)
       end do
       
       ELnodes(icdnode)%Ic = Itot + ELnodes(icdnode)%Cii*ELnodes(icdnode)%V
       
       call put_electric_current(icdnode,ELnodes(icdnode)%Ic) 

    end do

    if(MODelec%current.ne.i_c_vanish)then

       select case(MODelec%current)
       case(i_c_cons)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
             call put_electric_current(ibdyty,ELnodes(ibdyty)%Idriv) 
          end do
       case(i_c_line)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*min(1.D0,MODelec%B*TPS)
             ELnodes(ibdyty)%Idriv = ELnodes(ibdyty)%Idriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
             call put_electric_current(ibdyty,ELnodes(ibdyty)%Idriv) 
          end do
       case(i_c_alte)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*sin(MODelec%Omega*TPS+MODelec%Phi)
             ELnodes(ibdyty)%Idriv = ELnodes(ibdyty)%Idriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
             call put_electric_current(ibdyty,ELnodes(ibdyty)%Idriv) 
          end do
       case default
          stop       
       end select
    end if

    do icdan = 1,nb_CDAN
       
       icdnode = Branches(icdan)%icd
       iannode = Branches(icdan)%ian

       Branches(icdan)%U = ELnodes(icdnode)%V - ELnodes(iannode)%V
       Branches(icdan)%I = Branches(icdan)%U*Branches(icdan)%Ctot
          
    end do

  end subroutine solve_electro1G
!!!--------------------------------------------------------------------------------------
  subroutine solve_nl_electro1G
    implicit none
    integer(kind=4) :: inode,icdan,ibdyty,inet,iadj,nbadj,itacty
    integer(kind=4) :: icdnode,iannode,icdtac,iantac,adjsz,nb_RECUP
    integer(kind=4) :: NL_ERR

    real(kind=8)    :: rlt,rln,vlt,vln,X
    real(kind=8)    :: Concd,Conan,Conct,Conby,Ctot,Cii,Condini,Calpha
    real(KIND=8)    :: Resct,Resby,Tup,duk
    real(kind=8)    :: gapTT,gap,reff,meff
    real(KIND=8)    :: Pot,Itot,Apot,PotUPxxx,PotDOWNx

    integer(kind=4) :: status

    nb_CDAN = 0
    nb_DKDKx = get_nb_inters( i_dkdkx )
    nb_CDAN = nb_CDAN + nb_DKDKx

    nb_DKJCx = get_nb_inters( i_dkjcx )
    nb_CDAN = nb_CDAN + nb_DKJCx

    nb_DKKDx = get_nb_inters( i_dkkdx )
    nb_CDAN = nb_CDAN + nb_DKKDx

    if ( nb_CDAN == 0 ) return 

    if (allocated(dkdkx2branches)) deallocate(dkdkx2branches)
    allocate(dkdkx2branches(nb_DKDKx))
    dkdkx2branches = 0

    if (allocated(dkjcx2branches)) deallocate(dkjcx2branches)
    allocate(dkjcx2branches(nb_DKJCx))
    dkjcx2branches = 0

    if (allocated(dkkdx2branches)) deallocate(dkkdx2branches)
    allocate(dkkdx2branches(nb_DKKDx))
    dkkdx2branches = 0

    if ( allocated(Branches) ) deallocate(Branches)
    allocate(Branches(nb_CDAN))

    Conan = 0.0
    Concd = 0.0
    inet  = 0

    do ibdyty = 1,nb_RBDY2
       ELnodes(ibdyty)%iadj = 0
    end do

    !--------------------------------------------------------------
    select case(MODelec%local)
    case(i_l_allno,i_l_Hertz,i_l_Holm_Hertz,i_l_Holm_plast,i_l_Tekaya)
       call fill_active_contact(inet)
    case default
       print*,'local model not defined'
       stop
    end select

    ! Initialisation ----------------------------------------------

    NL_ERR = max(1,int(nb_CDAN*MODelec%NLTOL))

    !write(6,'(A16,I5)') 'NL Error value: ',NL_ERR

    ! current

    if(MODelec%current.ne.i_c_vanish)then
       select case(MODelec%current)
       case(i_c_cons)
          do ibdyty = 1,nb_RBDY2
             ELnodes(ibdyty)%Idriv = 0.D0
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_c_line)
          do ibdyty = 1,nb_RBDY2
             ELnodes(ibdyty)%Idriv = 0.D0
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*min(1.D0,MODelec%B*TPS)
             ELnodes(ibdyty)%Idriv = ELnodes(ibdyty)%Idriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_c_alte)
          do ibdyty = 1,nb_RBDY2
             ELnodes(ibdyty)%Idriv = 0.D0
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Idriv = MODelec%A*sin(MODelec%Omega*TPS+MODelec%Phi)
             ELnodes(ibdyty)%Idriv = ELnodes(ibdyty)%Idriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case DEFAULT
          stop       
       end select
    else if(MODelec%tension.ne.i_t_vanish)then

       select case(MODelec%tension)
       case(i_t_cons)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Vdriv = MODelec%A*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_t_line)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Vdriv = MODelec%A*min(1.D0,MODelec%B*TPS)
             ELnodes(ibdyty)%Vdriv = ELnodes(ibdyty)%Vdriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case(i_t_alte)
          do ibdyty = 1,nb_RBDY2
             if(.not.ELnodes(ibdyty)%AM_I_EL_BND) cycle
             ELnodes(ibdyty)%Vdriv = MODelec%A*sin(MODelec%Omega*TPS+MODelec%Phi)
             ELnodes(ibdyty)%Vdriv = ELnodes(ibdyty)%Vdriv*EL_BND_NODES(ELnodes(ibdyty)%iELbound)%sgnB
          end do
       case DEFAULT
          stop       
       end select
    end if

    !write(6,'(A18,I5)') 'Network branches: ',inet

    nb_RECUP  = 0
    nb_SLDOXI = 0

    if (first_time) then
       do icdan = 1, nb_CDAN
          vlt = Branches(icdan)%vlt
          if ( abs(vlt) .gt. MODelec%sliding_threshold ) then
             Branches(icdan)%oxided   = .false.
             Branches(icdan)%oxi      = .false.
          else
             Branches(icdan)%oxided   = .true.
             Branches(icdan)%oxi      = .true.
             Branches(icdan)%threshold = 0.75*MODelec%breakdown_threshold*Branches(icdan)%reff*Branches(icdan)%rln/H
          end if
       end do
    else
       do icdan = 1, nb_CDAN
          
          icdnode = Branches(icdan)%icd
          iannode = Branches(icdan)%ian
          
          nbadj = ElectricalNodes(icdnode)%nbadj
          vlt   = Branches(icdan)%vlt
          
          if ( abs(vlt) .gt. MODelec%sliding_threshold ) then
             if (Branches(icdan)%oxided) nb_SLDOXI = nb_SLDOXI + 1
             Branches(icdan)%oxided   = .false.
             Branches(icdan)%oxi      = .false.
          else
             Branches(icdan)%oxided = .true.
             
             !Modification en fonction de la discussion avec Daniel: R*I^2 = a^3*Cp(Tf-T0)

             Branches(icdan)%threshold = 0.75*MODelec%breakdown_threshold*Branches(icdan)%reff*Branches(icdan)%rln/H
             
             do iadj = 1,nbadj
                if(iannode.eq.ElectricalNodes(icdnode)%adj(iadj))then
                   Branches(icdan)%oxided   = ElectricalNodes(icdnode)%oxided(iadj)
                   Branches(icdan)%threshold = ElectricalNodes(icdnode)%threshold(iadj)
                   nb_RECUP = nb_RECUP + 1
                   exit
                end if
             end do
          end if
       end do
    end if
    
    if ( inet == 0 ) return

    !write(*,*) ' @ NB ELECTRO RECUP : ',nb_RECUP
    !write(*,*) ' @ NB OXIDED SLIDING: ',nb_SLDOXI
    
! matrix construction ------------------------------------------------

    call assemble_conductivity_matrix()
    
! Initialization -----------------------------------------------------

    if (MODelec%init) then
       do ibdyty = 1,nb_RBDY2
          ELnodes(ibdyty)%V = get_electric_potentiel(ibdyty)
       end do
    else
       do ibdyty = 1,nb_RBDY2
          ELnodes(ibdyty)%V = 0.0
       end do
    end if

! Start non linear iteration -------------------------------------------

    NLiter = 0

    do ibdyty = 1,nb_RBDY2
       ELnodes(ibdyty)%Vik = ELnodes(ibdyty)%V
    end do

    do 

       NLiter = NLiter + 1
       
       if( NLiter > MODelec%NLitermax ) exit

!!!---- Start linear iteration -------------------------------------------

       iter   = 0

       do while ( iter < MODelec%itermax )
          
          iter = iter + 1          
          E_ERR = 0.D0
          
          do icdnode = 1,nb_RBDY2
             
             if ( abs(ELnodes(icdnode)%Cii) >= 1.D-14 ) then
                
                Itot  = ELnodes(icdnode)%Idriv
                nbadj = ELnodes(icdnode)%nbadj

                do iadj = 1,nbadj
                   iannode = ELnodes(icdnode)%adj(iadj)
                   Itot    = Itot - ELnodes(iannode)%Vik*ELnodes(icdnode)%Cij(iadj)
                end do
                
                Pot = Itot/ELnodes(icdnode)%Cii
                
                if( abs(Pot) > 1.D-16 ) then
                   Apot = abs( (ELnodes(icdnode)%Vik-Pot )/Pot)
                else
                   Apot = abs( ELnodes(icdnode)%Vik-Pot )
                end if
                
                ELnodes(icdnode)%Vik = Pot
                E_ERR = max( E_ERR , Apot )

             end if
             
          end do

          if ( E_ERR < MODelec%TOL ) exit
          
       end do

!!!---- stop linear iteration ---------------------------------------------------

       Noxide = 0
       Pmean  = 0
       Pmax   =-1.D+24
       Pmin   = 1.D+24
 
       do ibdyty = 1,nb_RBDY2
          ELnodes(ibdyty)%iadj = 0
       end do

       do icdan = 1,nb_CDAN

          icdnode = Branches(icdan)%icd
          iannode = Branches(icdan)%ian
          
          ELnodes(icdnode)%iadj = ELnodes(icdnode)%iadj + 1
          ELnodes(iannode)%iadj = ELnodes(iannode)%iadj + 1
          
          if (Branches(icdan)%Active == 1) then
             
             Branches(icdan)%U  = ELnodes(iannode)%Vik - ELnodes(icdnode)%Vik
             Branches(icdan)%UI = Branches(icdan)%U*Branches(icdan)%U*Branches(icdan)%Calpha

             Pmean = Pmean + Branches(icdan)%UI
             Pmax  = max(Pmax,Branches(icdan)%UI)
             Pmin  = min(Pmin,Branches(icdan)%UI)

             if (Branches(icdan)%oxided) then
                if (Branches(icdan)%oxi) then
                   if ( Branches(icdan)%UI .ge. Branches(icdan)%threshold ) then 
                      Noxide = Noxide + 1
                      Branches(icdan)%oxi   = .false.
                      ELnodes(icdnode)%Cii = ELnodes(icdnode)%Cii &
                           - Branches(icdan)%Coxi + Branches(icdan)%Ctot
                      ELnodes(iannode)%Cii = ELnodes(iannode)%Cii &
                           - Branches(icdan)%Coxi + Branches(icdan)%Ctot
                      
                      ELnodes(icdnode)%Cij(ELnodes(icdnode)%iadj)  = -Branches(icdan)%Ctot
                      ELnodes(iannode)%Cij(ELnodes(iannode)%iadj)  = -Branches(icdan)%Ctot
                      Branches(icdan)%Calpha = Branches(icdan)%Ctot
                   end if
                else
                   if (.not. Branches(icdan)%oxi) then
                      if ( Branches(icdan)%UI .lt. Branches(icdan)%threshold ) then 
                         Noxide = Noxide + 1
                         Branches(icdan)%oxi   = .true.
                         ELnodes(icdnode)%Cii = ELnodes(icdnode)%Cii &
                              + Branches(icdan)%Coxi - Branches(icdan)%Ctot
                         ELnodes(iannode)%Cii = ELnodes(iannode)%Cii &
                              + Branches(icdan)%Coxi - Branches(icdan)%Ctot
                         
                         ELnodes(icdnode)%Cij(ELnodes(icdnode)%iadj)  = -Branches(icdan)%Coxi
                         ELnodes(iannode)%Cij(ELnodes(iannode)%iadj)  = -Branches(icdan)%Coxi
                         Branches(icdan)%Calpha = Branches(icdan)%Coxi
                      end if
                   end if
                end if
             end if
          end if
       end do

       if ( Noxide .le. NL_ERR .and. &
            E_ERR  .le. MODelec%TOL ) exit
       
    end do
    
! stop non linear iteration ---------------------------------------------------

    do ibdyty=1,nb_RBDY2
       call put_electric_potentiel(ibdyty,ELnodes(ibdyty)%Vik)
       ELnodes(ibdyty)%V = ELnodes(ibdyty)%Vik
    end do

! Stock information for next time step

! clean struture ---------------------------------------------------------
    
    if (first_time) then
       first_time = .false.
       do ibdyty = 1,nb_RBDY2
          ElectricalNodes(ibdyty)%iadj  = 0
          ElectricalNodes(ibdyty)%nbadj = 0
       end do
    else
       do ibdyty = 1,nb_RBDY2
          if(associated(ElectricalNodes(ibdyty)%adj))       deallocate(ElectricalNodes(ibdyty)%adj)
          if(associated(ElectricalNodes(ibdyty)%oxided))    deallocate(ElectricalNodes(ibdyty)%oxided)
          if(associated(ElectricalNodes(ibdyty)%threshold)) deallocate(ElectricalNodes(ibdyty)%threshold)
          if(associated(ElectricalNodes(ibdyty)%UI))        deallocate(ElectricalNodes(ibdyty)%UI)
          nullify(ElectricalNodes(ibdyty)%adj)
          nullify(ElectricalNodes(ibdyty)%oxided)
          nullify(ElectricalNodes(ibdyty)%threshold)
          nullify(ElectricalNodes(ibdyty)%UI)
          ElectricalNodes(ibdyty)%iadj  = 0
          ElectricalNodes(ibdyty)%nbadj = 0
       end do
    end if
    
!!! resize structure -------------------------------------------------------

    nb_OXID = 0
    nb_changed_OXID = 0

    do icdan = 1,nb_CDAN
       icdnode = Branches(icdan)%icd
       ElectricalNodes(icdnode)%iadj = ElectricalNodes(icdnode)%iadj + 1

!fd faux au niveau du langage je pense que c'est pas autorise de tester des logiques
!fd IF (Branches(icdan)%oxided .NE. Branches(icdan)%oxi) nb_changed_OXID = nb_changed_OXID + 1
!mr
       if ((    Branches(icdan)%oxided     .and.     Branches(icdan)%oxi)    .or. &
           ((.not. Branches(icdan)%oxided) .and. (.not. Branches(icdan)%oxi))     &
          ) nb_changed_OXID = nb_changed_OXID + 1

       Branches(icdan)%oxided = Branches(icdan)%oxi
       if (Branches(icdan)%oxided) nb_OXID = nb_OXID + 1

    end do

    do ibdyty=1,nb_RBDY2
       nbadj = ElectricalNodes(ibdyty)%iadj
       ElectricalNodes(ibdyty)%nbadj = nbadj 
       ElectricalNodes(ibdyty)%iadj  = 0
       if (nbadj .ne. 0) then
          allocate(ElectricalNodes(ibdyty)%adj(nbadj))
          allocate(ElectricalNodes(ibdyty)%oxided(nbadj))
          allocate(ElectricalNodes(ibdyty)%threshold(nbadj))
          allocate(Electricalnodes(ibdyty)%UI(nbadj))
          ElectricalNodes(ibdyty)%threshold = 0.0
          ElectricalNodes(ibdyty)%oxided   = .false.
          ElectricalNodes(ibdyty)%adj      = 0
          ElectricalNodes(ibdyty)%UI       = 0.0
       else
          nullify(ElectricalNodes(ibdyty)%adj)
          nullify(ElectricalNodes(ibdyty)%oxided)
          nullify(ElectricalNodes(ibdyty)%threshold)
          nullify(ElectricalNodes(ibdyty)%UI)
       end if
    end do

!!! fill structure ---------------------------------------------------------

    do icdan = 1,nb_CDAN
       icdnode = Branches(icdan)%icd
       iannode = Branches(icdan)%ian
       ElectricalNodes(icdnode)%iadj = ElectricalNodes(icdnode)%iadj + 1
       ElectricalNodes(icdnode)%adj(ElectricalNodes(icdnode)%iadj)       = iannode
       ElectricalNodes(icdnode)%oxided(ElectricalNodes(icdnode)%iadj)    = Branches(icdan)%oxided
       ElectricalNodes(icdnode)%threshold(ElectricalNodes(icdnode)%iadj) = Branches(icdan)%threshold
       ElectricalNodes(icdnode)%UI(ElectricalNodes(icdnode)%iadj)        = Branches(icdan)%UI
    end do

    return

  end subroutine solve_nl_electro1G
!-------------------------------------------------------------------------
  subroutine fill_active_contact_aux_( id_inter, icdan, icdtac, iantac, itact, contactor1_2bdyty, &
                                       contactor2_2bdyty, inter_2branches, Branches, ELnodes, inet )

    implicit none

    integer( kind = 4 )                                  :: id_inter
    integer( kind = 4 )                                  :: icdan
    integer( kind = 4 )                                  :: icdtac
    integer( kind = 4 )                                  :: iantac
    integer( kind = 4 )                                  :: itact
    integer( kind = 4 ), dimension( : , : ), allocatable :: contactor1_2bdyty
    integer( kind = 4 ), dimension( : , : ), allocatable :: contactor2_2bdyty
    integer( kind = 4 ), dimension( : ), allocatable :: inter_2branches
    type(T_BRANCHE)    , dimension( : ), allocatable :: Branches
    type(T_NODES)      , dimension( : ), allocatable :: ELnodes
    integer( kind = 4 )                              :: inet

    ! Local variables
    integer( kind = 4 ) :: icdnode
    integer( kind = 4 ) :: iannode
    real( kind = 8 )    :: rln
    real( kind = 8 )    :: rlt
    integer( kind = 4 ) :: status
    real( kind = 8 )    :: vlt
    real( kind = 8 )    :: vln
    real( kind = 8 )    :: reff
    real( kind = 8 )    :: meff

    Branches( icdan )%itype = id_inter
    inter_2branches( itact ) = icdan

    icdnode = contactor1_2bdyty( 1, icdtac )
    iannode = contactor2_2bdyty( 1, iantac )

    Branches( icdan )%icd = icdnode
    Branches( icdan )%ian = iannode

    ELnodes( icdnode )%iadj = ELnodes( icdnode )%iadj + 1
    ELnodes( iannode )%iadj = ELnodes( iannode )%iadj + 1

    call get_rloc( id_inter, itact, rlt, rln, status )
    call get_vloc( id_inter, itact, vlt, vln )

    Branches( icdan )%rln    = rln
    Branches( icdan )%vlt    = vlt
    Branches( icdan )%Active = 0
    Branches( icdan )%oxided = .false.
    Branches( icdan )%oxi    = .false.
    Branches( icdan )%I      = 0.d0

    call get_eff( id_inter, itact, meff, reff )
    Branches( icdan )%reff = reff

    !/todo reflechir au cas cohesif
    if ( abs(rln) > 1.D-16 ) then
       Branches( icdan )%Active = 1
       inet = inet + 1
    end if

  end subroutine fill_active_contact_aux_
!-------------------------------------------------------------------------
  subroutine fill_active_contact(inet)
    implicit none
    integer(kind=4) :: inet,icdan,icdnode,iannode
    integer(kind=4) :: icdtac,iantac,itact
    integer(kind=4) :: status
    real(kind=8)    :: rln,rlt,vlt,vln,reff,meff

    icdan = 0

    do itact = 1, nb_DKDKx

       icdan = icdan + 1
       
       call DKDKx2DISKx( itact, icdtac, iantac )

       call fill_active_contact_aux_( i_dkdkx, icdan, icdtac, iantac, itact, diskx2bdyty, diskx2bdyty, &
                                      dkdkx2branches, Branches, ELnodes, inet )

    end do

    do itact = 1, nb_DKJCx

       icdan = icdan + 1

       call DKJCx2DISKx( itact, icdtac )
       call DKJCx2JONCx( itact, iantac )

       call fill_active_contact_aux_( i_dkjcx, icdan, icdtac, iantac, itact, diskx2bdyty, joncx2bdyty, &
                                      dkjcx2branches, Branches, ELnodes, inet )

    end do

    do itact = 1, nb_DKKDx

       icdan = icdan + 1

       call DKKDx2DISKx( itact, icdtac )
       call DKKDx2xKSID( itact, iantac )

       call fill_active_contact_aux_( i_dkkdx, icdan, icdtac, iantac, itact, diskx2bdyty, xksid2bdyty, &
                                      dkkdx2branches, Branches, ELnodes, inet )

    end do

  end subroutine fill_active_contact
!-------------------------------------------------------------------------
  subroutine assemble_conductivity_matrix
    implicit none
    integer(kind=4) :: ibdyty,inode,icdan,inet,icdnode,iannode
    integer(kind=4) :: adjsz
    real(kind=8)    :: Ctot,Conby,Concd,Conan,Conct,Calpha
    real(kind=8)    :: rln,CndTH,CndEL,Eeff

    do ibdyty = 1,nb_RBDY2
       
       adjsz = ELnodes(ibdyty)%iadj
       ELnodes(ibdyty)%nbadj = adjsz
       
       if (associated(ELnodes(ibdyty)%adj)) deallocate(ELnodes(ibdyty)%adj)
       if (associated(ELnodes(ibdyty)%Cij)) deallocate(ELnodes(ibdyty)%Cij)

       if ( adjsz /= 0 ) then
          allocate(ELnodes(ibdyty)%adj(adjsz))
          allocate(ELnodes(ibdyty)%Cij(adjsz))
       else
          nullify(ELnodes(ibdyty)%adj)
          nullify(ELnodes(ibdyty)%Cij)
       end if
       
       ELnodes(ibdyty)%Cii  = 0.0
       ELnodes(ibdyty)%iadj = 0

    end do

    inet = 0

    do icdan = 1,nb_CDAN

       icdnode = Branches(icdan)%icd
       iannode = Branches(icdan)%ian

       Ctot = 0.0

       if (Branches(icdan)%Active .eq. 1) then
          
          inet = inet + 1

          call comp_electrical_eff_value(icdnode,iannode,CndEL,Eeff)
          
          select case(MODelec%local)
          case(i_l_allno) 

             Ctot = CndEL

          case(i_l_Hertz)

             rln = abs(Branches(icdan)%rln)
             Conct = (rln/H)**(0.33333)
             if ( (CndEL + Conct) > 1.D-18 ) Ctot = CndEL*Conct/(CndEL+Conct)

          case(i_l_Holm_Hertz)

             rln = abs(Branches(icdan)%rln)
             Conct = (0.75*(rln/H)*Branches(icdan)%reff/Eeff)**(0.33333)
             Ctot = 2*Conct*CndEL

          case(i_l_Holm_plast)

             rln = abs(Branches(icdan)%rln)
             Conct = ((rln/H)/(PI_g*Eeff))**(0.5)
             Ctot = 2*Conct*CndEL

          case(i_l_Tekaya)

             rln = abs(Branches(icdan)%rln)
             Conct = (0.75d0*(rln/H)*Branches(icdan)%reff/Eeff)**(0.33333)
             Ctot = 0.75d0*PI_g*(Conct**4)*CndEL/(Branches(icdan)%reff**3)

          case default
             print*,'local model not defined'
             stop
          end select
                    
       end if

       Branches(icdan)%Ctot = Ctot
       Branches(icdan)%U    = 0.0
       Branches(icdan)%UI   = 0.0
       Branches(icdan)%oxi  = Branches(icdan)%oxided
       
       if (Branches(icdan)%oxided) then
          Branches(icdan)%Coxi     = Ctot*MODelec%Coxi/(Ctot+MODelec%Coxi)
          Calpha = Branches(icdan)%Coxi
       else
          Calpha = Branches(icdan)%Ctot
       end if
       !print*,icdan,Branches(icdan)%Ctot,Branches(icdan)%oxided

       Branches(icdan)%Calpha = Calpha
       
       ELnodes(icdnode)%iadj = ELnodes(icdnode)%iadj + 1
       ELnodes(iannode)%iadj = ELnodes(iannode)%iadj + 1
       
       ELnodes(icdnode)%Cii = ELnodes(icdnode)%Cii + Calpha
       ELnodes(iannode)%Cii = ELnodes(iannode)%Cii + Calpha
       
       ELnodes(icdnode)%adj(ELnodes(icdnode)%iadj) = iannode
       ELnodes(iannode)%adj(ELnodes(iannode)%iadj) = icdnode
       
       ELnodes(icdnode)%Cij(ELnodes(icdnode)%iadj)  = -Calpha
       ELnodes(iannode)%Cij(ELnodes(iannode)%iadj)  = -Calpha
       
    end do

  end subroutine assemble_conductivity_matrix
!!!--------------------------------------------------------
!!!--------------------------------------------------------
  subroutine solve_thermo_mp_solver_aux( id_inter, icdan, iantac, icdtac, contact1_2nodes, &
                                         contact2_2nodes, internal, LocalFlux_inter )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: iantac
    integer( kind = 4 ) :: icdtac
    integer( kind = 4 ), allocatable, dimension( : )    :: contact1_2nodes
    integer( kind = 4 ), allocatable, dimension( : )    :: contact2_2nodes
    real( kind = 8 )   , dimension( max_internal_tact ) :: internal
    type(T_Flux)       , allocatable, dimension( : )    :: LocalFlux_inter

    ! Local variables
    real( kind = 8 )    :: meff
    real( kind = 8 )    :: reff
    integer( kind = 4 ) :: lawnb
    integer( kind = 4 ) :: ilaw
    integer( kind = 4 ) :: icdnode
    integer( kind = 4 ) :: iannode
    integer( kind = 4 ) :: status
    real( kind = 8 )    :: vln
    real( kind = 8 )    :: vlt
    real( kind = 8 )    :: vlnBEGIN
    real( kind = 8 )    :: vltBEGIN
    real( kind = 8 )    :: gapBEGIN
    real( kind = 8 )    :: rlt
    real( kind = 8 )    :: rln
    real( kind = 8 )    :: Qij

    call get_eff( id_inter, icdan, meff, reff )

    !mr: Todo check if it is correct!
    coordination = get_verlet_adjsz( id_inter, iantac ) + get_verlet_adjsz( id_inter, icdtac ) !jr

    icdnode = contact1_2nodes(icdtac)
    iannode = contact2_2nodes(iantac)

    call get_vlocBEGIN(id_inter, icdan, vltBEGIN, vlnBEGIN, gapBEGIN, status)
    call get_vloc( id_inter, icdan, vlt, vln )
    call get_rloc(id_inter, icdan, rlt, rln, status)
       
!!!### GENERATION
    lawnb = get_tact_lawnb( id_inter, icdan )
    ilaw  = tact_behav( lawnb )%ilaw
    call get_internal( id_inter, icdan, internal)
    call comp_generation_part( ilaw, icdnode, iannode, status, &
                               rln, rlt, vln, vlt, vlnBEGIN, vltBEGIN, internal, Qij )

    LocalFlux_inter( icdan )%Qij_s = Qij
!!!### DIFFUSION

    call comp_diffusive_part( ilaw, lawnb, icdnode, iannode, rln, reff, internal, Qij )

    LocalFlux_inter( icdan )%Qij_c = Qij
    
!!!### #########
  end subroutine solve_thermo_mp_solver_aux
!!!--------------------------------------------------------
!!!--------------------------------------------------------
  subroutine solve_thermo_mp_solver
    implicit none
    integer(kind=4) :: ik,ibdyty,inode,icdan,icdtac,iantac,ibound,iadj
    integer(kind=4) :: icdnode,iannode,itact,itacty,setnb
    integer(kind=4) :: nb_tacty
    integer(kind=4) :: lawnb,ibehav,ilaw
    real(kind=8)    :: Qij,meff,reff,CndTH,CndEL,ETHeff
    real(kind=8)    :: rlt,rln,vln,vlnBEGIN,vlt,vltBEGIN
    integer(kind=4) :: status
    logical         :: FLAG

    real(kind=8),dimension(max_internal_tact) :: internal

!!!--------------------------------------------------------
!    PRINT*,' *** INITIALISATION ***'

    GLOBAL_DV2 = 0.D0
    GLOBAL_PV  = 0.D0
    GLOBAL_DPV = 0.D0
    GLOBAL_QIJ = 0.D0
    GLOBAL_AREA = 0.D0
    GLOBAL_AQIJ = 0.D0

    internal = 0.d0

    if(MODtherm%init)then
       do inode=1,nb_NODES
          THnodes(inode)%Tini = THnodes(inode)%T
          THnodes(inode)%dT   = 0.D0
       end do
    else
       do inode=1,nb_NODES
          THnodes(inode)%Tini = 0.D0
          THnodes(inode)%T    = 0.D0
          THnodes(inode)%dT   = 0.D0
       end do
    end if
    
    nb_CDAN = 0

    nb_DKDKx = get_nb_inters( i_dkdkx )
    nb_CDAN = nb_CDAN + nb_DKDKx

    nb_DKJCx = get_nb_inters( i_dkjcx )
    nb_CDAN = nb_CDAN + nb_DKJCx

    nb_DKKDx = get_nb_inters( i_dkkdx )
    nb_CDAN = nb_CDAN + nb_DKKDx

    nb_PLPLx = get_nb_inters( i_plplx )
    nb_CDAN = nb_CDAN + nb_PLPLx

    nb_PLJCx = get_nb_inters( i_pljcx )
    nb_CDAN = nb_CDAN + nb_PLJCx

    !mr only in continue case
    if(MODtherm%igdiff.eq.i_gdiff_continue) call compute_average_contacts()

    if (allocated(LocalFlux_DKDKx)) deallocate(LocalFlux_DKDKx)
    if (allocated(LocalFlux_DKJCx)) deallocate(LocalFlux_DKJCx)
    if (allocated(LocalFlux_DKKDx)) deallocate(LocalFlux_DKKDx)
    if (allocated(LocalFlux_PLPLx)) deallocate(LocalFlux_PLPLx)
    if (allocated(LocalFlux_PLJCx)) deallocate(LocalFlux_PLJCx)
    
    if(nb_DKDKx .ne. 0) then 
       allocate(LocalFlux_DKDKx(nb_DKDKx))
       do icdan=1,nb_DKDKx
          LocalFlux_DKDKx(icdan)%Qij_c = 0.d0
          LocalFlux_DKDKx(icdan)%Qij_s = 0.d0
       end do
    end if

    if(nb_DKJCx .ne. 0) then 
       allocate(LocalFlux_DKJCx(nb_DKJCx))
       do icdan=1,nb_DKJCx
          LocalFlux_DKJCx(icdan)%Qij_c = 0.d0
          LocalFlux_DKJCx(icdan)%Qij_s = 0.d0
       end do
    end if
    
    if(nb_DKKDx .ne. 0) then 
       allocate(LocalFlux_DKKDx(nb_DKKDx))
       do icdan=1,nb_DKKDx
          LocalFlux_DKKDx(icdan)%Qij_c = 0.d0
          LocalFlux_DKKDx(icdan)%Qij_s = 0.d0
       end do
    end if
    
    if(nb_PLPLx .ne. 0) then 
       allocate(LocalFlux_PLPLx(nb_PLPLx))
       do icdan=1,nb_PLPLx
          LocalFlux_PLPLx(icdan)%Qij_c = 0.d0
          LocalFlux_PLPLx(icdan)%Qij_s = 0.d0
       end do
    end if
    
    if(nb_PLJCx .ne. 0) then 
       allocate(LocalFlux_PLJCx(nb_PLJCx))
       do icdan=1,nb_PLJCx
          LocalFlux_PLJCx(icdan)%Qij_c = 0.d0
          LocalFlux_PLJCx(icdan)%Qij_s = 0.d0
       end do
    end if

!!!* Diffusion ******************************

    do icdan = 1, nb_DKDKx
       
       call DKDKx2DISKx( icdan, icdtac, iantac )
       !print*,icdan,icdtac,iantac
       call solve_thermo_mp_solver_aux( i_dkdkx, icdan, iantac, icdtac, diskx2nodes, &
                                        diskx2nodes, internal, LocalFlux_DKDKx       )
     
    end do

    do icdan = 1, nb_DKJCx

       call DKJCx2DISKx( icdan, icdtac )
       call DKJCx2JONCx( icdan, iantac )

       call solve_thermo_mp_solver_aux( i_dkjcx, icdan, iantac, icdtac, diskx2nodes, &
                                        joncx2nodes, internal, LocalFlux_DKJCx       )

    end do

    do icdan = 1, nb_DKKDx

       call DKKDx2DISKx( icdan, icdtac )
       call DKKDx2xKSID( icdan, iantac )

       call solve_thermo_mp_solver_aux( i_dkkdx, icdan, iantac, icdtac, diskx2nodes, &
                                        xksid2nodes, internal, LocalFlux_DKKDx       )
       
    end do

    do icdan = 1, nb_PLPLx

       call PLPLx2POLYG( icdan, icdtac, iantac )

       call solve_thermo_mp_solver_aux( i_plplx, icdan, iantac, icdtac, polyg2nodes, &
                                        polyg2nodes, internal, LocalFlux_PLPLx       )
     
    end do

    do icdan = 1, nb_PLJCx

       call PLJCx2POLYG( icdan, icdtac )
       call PLJCx2JONCx( icdan, iantac )

       call solve_thermo_mp_solver_aux( i_pljcx, icdan, iantac, icdtac, polyg2nodes, &
                                        joncx2nodes, internal, LocalFlux_PLJCx       )
     
    end do

!!!---------------------------------------------------------
!!! Multi-tactors treatment 

    do inode=1,nb_MLT_NODES

       icdnode = MultiNodes(inode)%CD
       iannode = MultiNodes(inode)%AN

       call comp_thermal_eff_value(icdnode,iannode,CndTH,ETHeff)

       Qij = 2.0*CndTH*( THnodes(iannode)%Tini - THnodes(icdnode)%Tini )*MultiNodes(inode)%Area

       THnodes(icdnode)%dT = THnodes(icdnode)%dT + Qij
       THnodes(iannode)%dT = THnodes(iannode)%dT - Qij

    end do

!!!---------------------------------------------------------
!!! BOUNDARY CONDITION

    select case(MODtherm%ibound)
    !!!* ADIABATIC BEHAVIOUR *
    case(i_d_adia)
       !* NOTHING TO DO

    !!!* LINEAR EVOLUTION IN THE FIRST BODY THICKNESS *
    case(i_d_line)

       do ibound=1,nb_TH_BOUNDS

          do inode=1,TH_BND_NODES(ibound)%NX

             icdnode = TH_BND_NODES(ibound)%TSOURCE(inode)

             reff    = get_avr_radius_tacty(THnodes(icdnode)%ID_RBDY2,THnodes(icdnode)%ID_TACTY)
             CndTH  = get_therm_cond(THnodes(icdnode)%ID_RBDY2,THnodes(icdnode)%ID_TACTY)

             Qij = 2.0*CndTH*reff*( TH_BND_NODES(ibound)%T - THnodes(inode)%Tini )/TH_BND_NODES(ibound)%thickness


             !fd correction bug inode -> icdnode
             
             THnodes(icdnode)%dT = THnodes(inode)%dT + Qij          

             GLOBAL_AQIJ = GLOBAL_AQIJ + Qij

          end do

       end do

!!!* D. RICHARD APPROACH *
    case(i_d_1D__)
      do ibound=1,nb_TH_BOUNDS
          call COMP_T_BULK(ibound)
       end do       
!!!* M. RENOUF APPROACH *
    case(i_d_2D__)
       do ibound=1,nb_TH_BOUNDS
          call COMP_T_BULK(ibound)
       end do
    case default
       call LOGMES('Boundary conditions not found')
       call LOGMES('Please, correct the ELECTRO.DAT file.')
       stop
    end select

!!!---------------------------------------------------------
    if(FREE_BOUNDARY)then
       do inode=1,nb_NODES
          
          call comp_convexion_part(inode,Qij)

          THnodes(inode)%dT = THnodes(inode)%dT + Qij          
          
          GLOBAL_AQIJ = GLOBAL_AQIJ + Qij

       end do
    end if
!!!---------------------------------------------------------
    !call update_specific_heat !jr: specific heat updating function of temperature

    do inode=1,nb_NODES
       THnodes(inode)%dT = THnodes(inode)%dT*THnodes(inode)%alpha
    end do

!!!---------------------------------------------------------
!!! Note: The time step H is already take into account in the alpha
!    PRINT*,' *** SOLUTION ***'

    do inode=1,nb_NODES

       ibdyty = THnodes(inode)%ID_RBDY2
       itacty = THnodes(inode)%ID_TACTY

       if(THnodes(inode)%AM_I_TH_SRC) then
          THnodes(inode)%T     = TH_SRC_NODES(THnodes(inode)%iTHsource)%T
       else
          THnodes(inode)%T     = THnodes(inode)%Tini + (1-THETA)*THnodes(inode)%dTini + THETA*THnodes(inode)%dT
          THnodes(inode)%dTini = THnodes(inode)%dT
       end if

       call put_thermal_value(ibdyty,itacty,THnodes(inode)%T)

    end do

  end subroutine solve_thermo_mp_solver
!!!--------------------------------------------------------------------------------------
  subroutine update_conductivity_mp_solver

    implicit none


  end subroutine update_conductivity_mp_solver
!!!--------------------------------------------------------------------------------------
  subroutine update_thermo_mp_solver

    implicit none

  end subroutine update_thermo_mp_solver
!!!--------------------------------------------------------------------------------------
  logical function get_write_mp_values(fantome)

    implicit none
    integer(kind=4),optional :: fantome

    get_write_mp_values = write_mpv
    
  end function get_write_mp_values
!!!--------------------------------------------------------------------------------------
  subroutine get_global_thermal_variable(GPV,GDPV,GDV2,GQIJ,GQRIJ,GAQIJ,GA)

    implicit none
    integer(kind=4) :: NA
    real(kind=8)    :: GPV,GDPV,GDV2,GQIJ,GQRIJ,GAQIJ,GA

    GPV  = GLOBAL_PV  
    GDPV = GLOBAL_DPV 
    GDV2 = GLOBAL_DV2   !jr : global generated heat flux
    GQIJ = GLOBAL_QIJ   !jr : global conductive heat flux
    GAQIJ = GLOBAL_AQIJ !jr : global heat flux exchanged with boundaries
    GQRIJ = GLOBAL_QRIJ !jr : global radiative heat flux

    NA = max(1,nb_CDAN)
    GA   = GLOBAL_AREA/real(NA,8) !jr : global surface exchange

  end subroutine get_global_thermal_variable
!!!--------------------------------------------------------------------------------------
  logical function get_oxided_tactor(icdan)

    implicit none

    integer(kind=4) :: icdan

    if ( allocated(Branches) ) then
       get_oxided_tactor = Branches(icdan)%oxided
    else
       get_oxided_tactor = .true.
    end if

  end function get_oxided_tactor
!!!--------------------------------------------------------------------------------------
  subroutine get_electro_info(it,err,NLit,Cm,nbo,nbco,nbso)

    implicit none

    integer(kind=4)      :: it,NLit,nbo,nbco,nbso
    real(kind=8) :: err,NLerr,Cm

    it    = iter 
    err   = E_ERR
    NLit  = NLiter

    Cm    = Cmean

    nbo   = nb_OXID
    nbco  = nb_changed_OXID
    nbso  = nb_SLDOXI

  end subroutine get_electro_info
!!!--------------------------------------------------------------------------------------
  subroutine active_recup(FLAG)

    implicit none
    character(len=1) :: FLAG

    select case(FLAG)
    case('T')
       MODtherm%init = .true.
    case('P')
       MODelec%init = .true.
    case DEFAULT
    end select

  end subroutine active_recup
!!!--------------------------------------------------------------------------------------
  subroutine write_out_mp_behaviour_mp_solver
    implicit none
    integer(kind=4) :: inode,itact,nfich

    nfich = get_io_unit()

    open(unit=nfich,file=trim(location(out_mpdem(:))))

    if(electro_model)then
       !fd write(nfich,'(A14)') '$model  elec_:'
       write(nfich,'(A14)') '$model  elec_ '       
       select case(MODelec%current)
       case(i_c_cons)              !12345678901
          write(nfich,'(14X,A11)') 'curnt: cons'
          write(nfich,'(18X,1(2X,D14.7))') MODelec%A
       case(i_c_line)
          write(nfich,'(14X,A11)') 'curnt: line'
          write(nfich,'(18X,2(2X,D14.7))') MODelec%A,MODelec%B
       case(i_c_alte)
          write(nfich,'(14X,A11)') 'curnt: alte'
          write(nfich,'(18X,4(2X,D14.7))') MODelec%A,MODelec%B,MODelec%Omega,MODelec%Phi
       end select
       select case(MODelec%local)
       case(i_l_Hertz)             !123456789012
          write(nfich,'(14X,A12)') 'local: Hertz'
       case(i_l_allno)
          write(nfich,'(14X,A12)') 'local: allno'
       case(i_l_Holm_Hertz)
          write(nfich,'(14X,A12)') 'local: HolmH'
       case(i_l_Holm_plast)
          write(nfich,'(14X,A12)') 'local: HolmP'
       case(i_l_Tekaya)
          write(nfich,'(14X,A12)') 'local: Tkaya'
       end select
       write(nfich,'(21X,A6,I7)')    'iter_:',MODelec%itermax
       write(nfich,'(21X,A6,D14.7)') 'tol__:',MODelec%TOL
       if (MODelec%oxide) then     !1234567890
          write(nfich,'(14X,A10)') 'oxide: yes'
          write(nfich,'(21X,A6,D14.7)') 'Cond_:',MODelec%Coxi
          write(nfich,'(21X,A6,I7)')    'iter_:',MODelec%NLitermax
          write(nfich,'(21X,A6,D14.7)') 'tol__:',MODelec%NLTOL
       else                        
          write(nfich,'(14X,A10)') 'oxide: no '
       end if
       if (MODelec%breakdown) then !1234567890
          write(nfich,'(14X,A10)') 'brkdw: yes'
          write(nfich,'(21X,A6,D14.7)') 'brkth:',MODelec%breakdown_threshold
          write(nfich,'(21X,A6,D14.7)') 'var__:',MODelec%breakdown_var
       else                        
          write(nfich,'(14X,A10)') 'brkth: no '
       end if
       if (MODelec%sliding) then   !1234567890
          write(nfich,'(14X,A10)') 'sldng: yes'
          write(nfich,'(21X,A6,D14.7)') 'sldth:',MODelec%sliding_threshold
       else                        
          write(nfich,'(14X,A10)') 'sldng: no '
       end if
    end if

    if(thermo_model)then

       !fd write(nfich,'(A14)') '$model  therm:'
       write(nfich,'(A14)') '$model  therm '
       write(nfich,'(14X,A6,D14.7)') 'T0___:',MODtherm%T0
       write(nfich,'(14X,A6,D14.7)') 'alert:',MODtherm%Alert

       !fd manque ecriture gdiff !!
       
       select case(MODtherm%ildiff)
       !fd correction format   
       case(i_ldiff_none)          !123456789012
          write(nfich,'(14X,A12)') 'ldiff: none '
       case(i_ldiff_Hertz)
          write(nfich,'(14X,A12)') 'ldiff: Hertz'
       case(i_ldiff_Cylnd)
          write(nfich,'(14X,A12)') 'ldiff: Cylnd'
       end select

       if (MODtherm%lconv) then    !1234567890
          write(nfich,'(14X,A10)') 'lconv: yes'
          !fd correction format et ecriture          
          write(nfich,'(14X,A13,D14.7,A8,D14.7)') '       Gcond:',MODtherm%Gcond,'  GTemp:',MODtherm%GTemp
       else                        
          write(nfich,'(14X,A10)') 'lconv: no '
       end if

       select case(MODtherm%ilkine)
       case(i_lkine_all)           !1234567890
          write(nfich,'(14X,A10)') 'lkine: all'
       case(i_lkine_dvn)
          write(nfich,'(14X,A10)') 'lkine: dvn'
       case(i_lkine_dvt)
          write(nfich,'(14X,A10)') 'lkine: dvt'
       case(i_lkine_no)
          write(nfich,'(14X,A10)') 'lkine: no '
       end select

       select case(MODtherm%ibound)
       case(i_d_adia)              !12345678901
          write(nfich,'(14X,A11)') 'bound: adia'
       case(i_d_line)
          write(nfich,'(14X,A11)') 'bound: line'
       case(i_d_1D__)
          write(nfich,'(14X,A11)') 'bound: 1D__'
       case(i_d_2D__)
          write(nfich,'(14X,A11)') 'bound: 2D__'
       end select
    end if

    do inode = 1,nb_TH_SOURCES
       write(nfich,'(A1)') ' '
       write(nfich,'(A14)') '$source  therm'
       write(nfich,'(9X,I7,2X,A2,2X,I7,2X,A5,D14.7)') &
            TH_SRC_NODES(inode)%ifirst,'to',TH_SRC_NODES(inode)%ilast,'T0__=',TH_SRC_NODES(inode)%T
    end do

    do inode = 1,nb_TH_BOUNDS
       write(nfich,'(A1)') ' '

       select case(TH_BND_NODES(inode)%idirection)
       case(i_upxxx)           !1234567890123456
          write(nfich,'(A16)') '$bounds  therm U'
       case(i_downx)
          write(nfich,'(A16)') '$bounds  therm D'
       case(i_right)
          write(nfich,'(A16)') '$bounds  therm R'
       case(i_leftx)
          write(nfich,'(A16)') '$bounds  therm L'
       case DEFAULT
          write(nfich,'(A16)') '$bounds  therm  '
       end select

       write(nfich,'(9X,I7,2X,A2,2X,I7,2X,A5,D14.7,2X,A5,D14.7)') &
            TH_BND_NODES(inode)%ifirst,'to',TH_BND_NODES(inode)%ilast, &
            'T0__=',TH_BND_NODES(inode)%T,'Thck=',TH_BND_NODES(inode)%thickness
       write(nfich,'(31X,A5,D14.7,2X,A5,D14.7)') &
            'lengh=',TH_BND_NODES(inode)%length,'Alph=',TH_BND_NODES(inode)%ALPHA
    end do

    do inode = 1,nb_EL_BOUNDS
       write(nfich,'(A1)') ' '
       write(nfich,'(A14)') '$bounds  elec_'
       write(nfich,'(9X,I7,2X,A2,2X,I7,2X,A5,D8.1)') &
            EL_BND_NODES(inode)%ifirst,'to',EL_BND_NODES(inode)%ilast,'Isgn=',EL_BND_NODES(inode)%sgnB
    end do

    close(nfich)

  end subroutine write_out_mp_behaviour_mp_solver
!!!------------------------------------------------------
  subroutine comp_thermal_eff_value(icd,ian,condTH,E)

    implicit none
    integer(kind=4)  :: icd,ian,ibdyty,itacty,ibehav
    real(kind=8)     :: condTH,E
    real(kind=8)     :: Ean,NUan,Ecd,NUcd
    real(kind=8)     :: concd,conan

    condTH = 0.d0

    ibdyty = THnodes(icd)%ID_RBDY2
    itacty = THnodes(icd)%ID_TACTY

    ibehav = get_bulk_behav_number_RBDY2(ibdyty,1)
    call get_equivalent_mat_prop(ibehav,Ecd,NUcd)
    !print*,ibehav,Ecd,NUcd
    
    ibdyty = THnodes(ian)%ID_RBDY2
    itacty = THnodes(ian)%ID_TACTY
    
    ibehav = get_bulk_behav_number_RBDY2(ibdyty,1)
    call get_equivalent_mat_prop(ibehav,Ean,NUan)
    !print*,ibehav,Ean,NUan
    
    if ( Ecd + Ean < 1.D-16 ) then
       E = 0.0
    else
       E = Ecd*Ean/(Ecd*(1-NUan*NUan) + Ean*(1-NUcd*NUcd))
    end if

    ibdyty = THnodes(icd)%ID_RBDY2
    itacty = THnodes(icd)%ID_TACTY
    concd  = get_therm_cond(ibdyty,itacty)
    
    ibdyty = THnodes(ian)%ID_RBDY2
    itacty = THnodes(ian)%ID_TACTY
    
    conan  = get_therm_cond(ibdyty,itacty)
    
    if ( concd + conan < 1.D-16 ) then
       condTH = 0.0
    else
       condTH = 2.0*concd*conan/(concd + conan)
    end if

  end subroutine comp_thermal_eff_value
!!!------------------------------------------------------
  subroutine comp_electrical_eff_value(icd,ian,condEL,E)
    implicit none
    integer(kind=4)  :: icd,ian,ibdyty,ibehav
    real(kind=8)     :: condEL,E
    real(kind=8)     :: Ean,NUan,Ecd,NUcd
    real(kind=8)     :: concd,conan

    condEL = 0.d0

    ibehav = get_bulk_behav_number_RBDY2(icd,1)
    call get_equivalent_mat_prop(ibehav,Ecd,NUcd)

    ibehav = get_bulk_behav_number_RBDY2(ian,1)
    call get_equivalent_mat_prop(ibehav,Ean,NUan)

    if ( Ecd + Ean < 1.D-16 ) then
       E = 0.0
    else
       E = Ecd*Ean/(Ecd*(1-NUan*NUan) + Ean*(1-NUcd*NUcd))
    end if

    concd  = get_elec_cond(icd)
    
    conan  = get_elec_cond(ian)
    
    if ( concd + conan < 1.D-16 ) then
       condEL = 0.0
    else
       condEL = 2.0*concd*conan/(concd + conan)
    end if

  end subroutine comp_electrical_eff_value
!!!------------------------------------------------------
  subroutine comp_diffusive_part(ilaw,lawnb,icd,ian,rln,reff,internal,Qij)

    implicit none
    integer(kind=4) :: ilaw,lawnb,icd,ian
    real(kind=8)    :: rln,reff,AREA
    real(kind=8)    :: cohn,coht,dw,Qij,ETHeff,CndTH,CndEL

    real(kind=8),dimension(max_internal_tact) :: internal

    call comp_thermal_eff_value(icd,ian,CndTH,ETHeff)
    !jr & mr: anisotropic conductivity
    if(internal(4).ne.0)then
       CndTH = internal(6)
    end if

    call get_coh(lawnb,cohn,coht,dw)

    !mr: check with CZM
    select case(MODtherm%igdiff)
    case(i_gdiff_continue)
       select case(ilaw)
       case(i_IQS_MAC_CZM,i_MAC_CZM,i_IQS_WET_CZM)
          if(internal(4).eq.0.0)then
             select case(MODtherm%ildiff)
             case(i_ldiff_Hertz)
                AREA = ConvCond*(0.5d0*PI_g)/MeanC &
                     + (1.d0-ConvCond)*(3.0d0*reff*max(0.D0,rln/H+cohn)/(4.0*ETHeff))**0.3333333D+00
             case(i_ldiff_Cylnd)
                AREA = ConvCond*(0.5d0*PI_g)/MeanC &
                     + (1.d0-ConvCond)*(4.0d0*reff*max(0.D0,rln/H+cohn)/(PI_g*ETHeff))**0.5
             case DEFAULT
                stop
             end select
          else
             if(rln.lt.0.0) then
                AREA = (0.5d0*PI_g*internal(4))/MeanC
             else 
                AREA = (0.5d0*PI_g*internal(4))/MeanC &
                     + (1-internal(4))*((4.0*reff*max(0.D0,rln/H+cohn)/(PI_g*ETHeff))**0.5)
             end if
          end if
       case default
          AREA = 0.5d0*PI_g/MeanC
       end select

    case(i_gdiff_discrete)

       select case(MODtherm%ildiff)
       case(i_ldiff_Hertz)
          AREA = ConvCond*(0.5d0*PI_g)/MeanC &
               + (1.d0-ConvCond)*(3.0d0*reff*max(0.D0,rln/H+cohn)/(4.0d0*ETHeff))**0.3333333D+00
       case(i_ldiff_Cylnd)
          AREA = ConvCond*(0.5d0*PI_g)/MeanC &
               +  (1-ConvCond)*(4.0d0*reff*max(0.D0,rln/H+cohn)/(PI_g*ETHeff))**0.5
       case DEFAULT
          stop
       end select

    end select

    !call update_conductivity_mp_solver !jr : thermal conductivity updating fonction of temperature

    Qij = 2.0*CndTH*( THnodes(ian)%Tini - THnodes(icd)%Tini )*AREA

    !print*,reff,ETHeff,rln
    !print*,CndTH,AREA,Qij,THnodes(ian)%Tini,THnodes(icd)%Tini

    THnodes(icd)%dT = THnodes(icd)%dT + Qij
    THnodes(ian)%dT = THnodes(ian)%dT - Qij

    GLOBAL_QIJ  = GLOBAL_QIJ  + abs(Qij)
    GLOBAL_AREA = GLOBAL_AREA + AREA/(2.0*reff)

  end subroutine comp_diffusive_part
!!!------------------------------------------------------
  !jr : radiative flux
  subroutine comp_radiation_part(ibdytyan,ibdytycd) 

    implicit none
    integer(kind=4) :: ibdytyan,ibdytycd
    real(kind=8) :: raycd,rayan,emiss_an,emiss_cd,emiss,sigmab
    real(kind=8) :: A,B,W,X,Y,Z,F,Qrij,delta,dist,area
    real(kind=8),dimension(2) :: coorcd, cooran

    raycd = get_radius_DISKx(ibdytycd)
    rayan = get_radius_DISKx(ibdytyan)

    coorcd = get_coor(ibdytycd,0) !jr : coordinates
    cooran = get_coor(ibdytyan,0)

    delta = SQRT((coorcd(1)-cooran(1))**2 + (coorcd(2)-cooran(2))**2)
    dist = delta-raycd-rayan+1e-16 !jr : length between two grains
    
    B = raycd/rayan !jr : view factor's terms
    A = 1 + B + (dist/rayan)
    W = PI_g + SQRT(A*A-(B+1)**2)
    X = - SQRT(A*A-(B-1)**2)
    Y = (B-1)*ACOS((B-1)/A)
    Z = -(B+1)*ACOS((B+1)/A)

    emiss_an = 0.8
    emiss_cd = 0.9

    emiss = (emiss_an * emiss_cd)/(1-((1-emiss_an)*(1-emiss_cd))) !jr : emissivity for two grey surfaces
    sigmab = 5.67e-14 !jr : Stefan-Boltzmann constant (g/mm/ms)
    area = PI_g*rayan*rayan
    F = (1/(2.d0*PI_g))*(W+X+Y+Z) !jr : view factor for parallel axis of two cylinders
        
    Qrij = sigmab*emiss*area*F*(THnodes(ibdytyan)%Tini**(4)-THnodes(ibdytycd)%Tini**(4))
    
    THnodes(ibdytycd)%dT = THnodes(ibdytycd)%dT + Qrij
    THnodes(ibdytyan)%dT = THnodes(ibdytyan)%dT - Qrij

    GLOBAL_QRIJ  = GLOBAL_QRIJ  + abs(Qrij)

  end subroutine comp_radiation_part
!!!------------------------------------------------------
  subroutine comp_generation_part(ilaw,icd,ian,status, &
                                  rln,rlt,vln,vlt,vlni,vlti,internal,dv2)
    implicit none
    integer(kind=4) :: icd,ian,ilaw
    real(kind=8)    :: rln,rlt,vln,vlt,vlni,vlti
    real(kind=8)    :: dv2,Un,Ut,QEij,QPij

    integer(kind=4) :: status
    real(kind=8),dimension(max_internal_tact) :: internal

    dv2 = 0.0
    QEij = 0.D0
    QPij = 0.D0

    if ((status.eq.i_noctc).or.(status.eq.i_vnish)) return

    ! Dissipated power: M*L*L/T*T*T
    ! dv2 = 0.5*meff*ABS((vln*vln+vlt*vlt)-(vlnBEGIN*vlnBEGIN+vltBEGIN*vltBEGIN))/H
    
    ! kinetic energy theory: the system is isolated 
    ! => the power dissipated in the system equals the sum of the power of each contact
    

    select case(MODtherm%ilkine)

    case(i_lkine_all)
       select case(ilaw)
       case(i_IQS_CLB,i_IQS_DS_CLB, &
            i_RST_CLB,i_RST_DS_CLB)
          Un = ((1-THETA)*vlni) + (THETA* vln)
          Ut = ((1-THETA)*vlti) + (THETA* vlt)
          dv2 = -((Un*rln)+(Ut*rlt))/H
       case(i_IQS_WET_DS_CLB)
          if(rln.gt.0.d0) then             !jr
             Un = ((1-THETA)*vlni) + (THETA* vln)
             Ut = ((1-THETA)*vlti) + (THETA* vlt)
             dv2 = -((Un*rln)+(Ut*rlt))/H
          end if
       case(i_IQS_MAC_CZM,i_MAC_CZM)
          if(internal(4).eq.0.d0)then
             Un = ((1-THETA)*vlni) + (THETA* vln)
             Ut = ((1-THETA)*vlti) + (THETA* vlt)
             dv2 = -((Un*rln)+(Ut*rlt))/H
          end if
       case(i_IQS_WET_CZM)
          !jr : no dependance between temperature and cohesion
          if((internal(4).eq.0.d0).and.(rln.gt.0.d0)) then
             Un = ((1-THETA)*vlni) + (THETA* vln)
             Ut = ((1-THETA)*vlti) + (THETA* vlt)
             dv2 = -((Un*rln)+(Ut*rlt))/H
          end if
       !case(i_ELASTIC_REPELL_CLB)

       !case(i_ELASTIC_REPELL_WET_CLB)

       case default
          CALL LOGMES('warning! heat generation computed with an unsupported contact law')
          Un = ((1-THETA)*vlni) + (THETA* vln)
          Ut = ((1-THETA)*vlti) + (THETA* vlt)
          dv2 = -((Un*rln)+(Ut*rlt))/H
       end select
  
       dv2 = ConvGen * dv2
       
       THnodes(icd)%dT = THnodes(icd)%dT + dv2*0.5
       THnodes(ian)%dT = THnodes(ian)%dT + dv2*0.5

    case(i_lkine_dvn)

       Un = ((1-THETA)*vlni) + (THETA* vln)
       
       dv2 = -(Un*rln)/H

       THnodes(icd)%dT = THnodes(icd)%dT + dv2*0.5
       THnodes(ian)%dT = THnodes(ian)%dT + dv2*0.5

    case(i_lkine_dvt)

       Ut = ((1-THETA)*vlti) + (THETA* vlt)

       dv2 = -(Ut*rlt)/H

       THnodes(icd)%dT = THnodes(icd)%dT + dv2*0.5
       THnodes(ian)%dT = THnodes(ian)%dT + dv2*0.5

    case(i_lkine_no)

       dv2 = 0.0

    end select

    ! call elasto_plastic_dissipation(QEij,QPij,internal,lawnb,icd,ian)
    ! dv2 = dv2 + QEij + QPij

    GLOBAL_PV   = GLOBAL_PV   + abs(rln*vln/H) + abs(rlt*vlt/H)
    GLOBAL_DPV  = GLOBAL_DPV  + abs(rln*(vln-vlni)/H) + abs(rlt*(vlt-vlti)/H)
    GLOBAL_DV2  = GLOBAL_DV2  + dv2

  end subroutine comp_generation_part
!!!------------------------------------------------------
  subroutine comp_convexion_part(inode,QIJ)
    implicit none
    integer(kind=4) :: inode
    integer(kind=4) :: ibdyty,itacty
    real(kind=8)    :: QIJ,radius,condik,AREA

    ibdyty = THnodes(inode)%ID_RBDY2
    itacty = THnodes(inode)%ID_TACTY

    QIJ = 0.D0

    if (.not.IS_IN_THE_FREE_BOUNDARY(ibdyty,itacty)) return

    radius  = get_avr_radius_tacty(ibdyty,itacty)
    condik  = get_therm_cond(ibdyty,itacty)

    AREA = PI_g*radius

    QIJ = MODtherm%Gcond*AREA*(MODtherm%GTemp-THnodes(inode)%Tini)

  end subroutine comp_convexion_part
!!!------------------------------------------------------
  subroutine get_local_flux(icdan,type,Qij_c,Qij_s)
    implicit none
    integer(kind=4) :: icdan,type
    real(kind=8)    :: Qij_c,Qij_s
    
    select case(type)
    case(i_dkdkx)
       Qij_c = LocalFlux_DKDKx(icdan)%Qij_c
       Qij_s = LocalFlux_DKDKx(icdan)%Qij_s
    case(i_dkjcx)
       Qij_c = LocalFlux_DKJCx(icdan)%Qij_c
       Qij_s = LocalFlux_DKJCx(icdan)%Qij_s
    case(i_dkkdx)
       Qij_c = LocalFlux_DKKDx(icdan)%Qij_c
       Qij_s = LocalFlux_DKKDx(icdan)%Qij_s
    case(i_plplx)
       Qij_c = LocalFlux_PLPLx(icdan)%Qij_c
       Qij_s = LocalFlux_PLPLx(icdan)%Qij_s
    case(i_pljcx)
       Qij_c = LocalFlux_PLJCx(icdan)%Qij_c
       Qij_s = LocalFlux_PLJCx(icdan)%Qij_s
    case default
       Qij_c = 0.d0 
       Qij_s = 0.d0
    end select

  end subroutine get_local_flux
!!!------------------------------------------------------
   subroutine INIT_BULK_MATRIX(ibound)

     implicit none
     integer(kind=4) :: ibound,NX,NY
     real(kind=8)    :: CST,DX,ALPHA,DX2,HTMP

                              !12345678901234567890
     character(len=20)::IAM = 'MP::INIT_BULK_MATRIX'

     NX = TH_BND_NODES(ibound)%NX
     !PRINT*,'enter in INIT_BULK_MATRIX'
     if ( NX.eq.0) then
        print*,'WARNING!'
        print*,'BOUND ',ibound,' DO NOT HAVE NODE'
        stop
     else
        DX = TH_BND_NODES(ibound)%length/real(NX,8)
        TH_BND_NODES(ibound)%DX = DX
     end if

     NY = int(TH_BND_NODES(ibound)%thickness/DX)

     TH_BND_NODES(ibound)%NY = NY

     select case(MODtherm%ibound)
     case(i_d_1D__)

        allocate(TH_BND_NODES(ibound)%TBULK(NX,NY))
        allocate(TH_BND_NODES(ibound)%TBULKi(NX,NY))
        allocate(TH_BND_NODES(ibound)%TPROFIL(NY))

        HTMP = 0.5*DX*DX/TH_BND_NODES(ibound)%ALPHA
        
!        print*,' HTMP/H = ',HTMP,'/',H

        if ( H .gt. HTMP ) then
           call LOGMES('The time step is larger than the maximal value of the thermal step')
           call Faterr(IAM,'Reduce time step')
        end if

        TH_BND_NODES(ibound)%ADT = TH_BND_NODES(ibound)%ALPHA*H/(DX*DX)

        TH_BND_NODES(ibound)%TBULK   = TH_BND_NODES(ibound)%T
        TH_BND_NODES(ibound)%TBULKi  = TH_BND_NODES(ibound)%T
        TH_BND_NODES(ibound)%TPROFIL = TH_BND_NODES(ibound)%T

     case(i_d_2D__)

        allocate(TH_BND_NODES(ibound)%TBULK(NX,NY))
        allocate(TH_BND_NODES(ibound)%TBULKi(NX,NY))
        allocate(TH_BND_NODES(ibound)%TPROFIL(NY))

        HTMP = 0.25*DX*DX/(TH_BND_NODES(ibound)%ALPHA*H)

        print*,' Stabiliy scheme ratio: ',HTMP

        if ( HTMP .lt. 1.0 ) then
           call LOGMES('The stability condition is not preserved')
           call faterr(IAM,'Reduce time step')
           stop
        end if

        TH_BND_NODES(ibound)%ADT = TH_BND_NODES(ibound)%ALPHA*H/(DX*DX)

        TH_BND_NODES(ibound)%TBULK   = TH_BND_NODES(ibound)%T
        TH_BND_NODES(ibound)%TBULKi  = TH_BND_NODES(ibound)%T
        TH_BND_NODES(ibound)%TPROFIL = TH_BND_NODES(ibound)%T

     case DEFAULT

        nullify(TH_BND_NODES(ibound)%TBULK)
        nullify(TH_BND_NODES(ibound)%TBULKi)
        nullify(TH_BND_NODES(ibound)%TPROFIL)

     end select

   end subroutine INIT_BULK_MATRIX
!!!------------------------------------------------------
   subroutine COMP_T_BULK(ibound)

     implicit none
     integer(kind=4) :: inode,ibound,ibdyty,itacty
     integer(kind=4) :: NX,NY,IX,IY
     real(kind=8)    :: DX,DX2,DX2_1,ADT,DIAG,QIJ,concd,DELTAT

     NX  = TH_BND_NODES(ibound)%NX
     NY  = TH_BND_NODES(ibound)%NY

     DX    = TH_BND_NODES(ibound)%DX
     DX2   = DX*DX
     DX2_1 = 1.D0/DX2
     ADT   = TH_BND_NODES(ibound)%ALPHA*H

     select case(MODtherm%ibound)
        !********************!
     case(i_d_1D__)

        !* LIMIT CONDITIONS *!
        
        DIAG = 1.d0 - 2.d0*ADT/DX2

        TH_BND_NODES(ibound)%TBULK(1,1) = 0 

        do IX = 1,NX
           inode = TH_BND_NODES(ibound)%TSOURCE(IX)
           TH_BND_NODES(ibound)%TBULK(1,1) = TH_BND_NODES(ibound)%TBULK(1,1) + THnodes(inode)%Tini
        end do

        TH_BND_NODES(ibound)%TBULK(1,1)  = TH_BND_NODES(ibound)%TBULK(1,1) / real(NX,8)
        TH_BND_NODES(ibound)%TBULK(1,NY) = TH_BND_NODES(ibound)%T     

        !* BULK *!
        
        do IY = 2,NY-1

           TH_BND_NODES(ibound)%TBULK(1,IY) &
                = DIAG*TH_BND_NODES(ibound)%TBULK(1,IY) &
                + ADT*DX2_1*(TH_BND_NODES(ibound)%TBULK( 1,IY+1) &
                +            TH_BND_NODES(ibound)%TBULK( 1,IY-1))
        end do

        do IX = 2,NX
           do IY = 1,NY
              TH_BND_NODES(ibound)%TBULK(IX,IY) = TH_BND_NODES(ibound)%TBULK(1,IY)
           end do
        end do

        TH_BND_NODES(ibound)%TBULKi = TH_BND_NODES(ibound)%TBULK

        !* THERMAL GRADIENT COMPUTATION *!

        do IX = 1,NX
           inode = TH_BND_NODES(ibound)%TSOURCE(IX)
           
           DELTAT = TH_BND_NODES(ibound)%TBULK(1,2) - THnodes(inode)%Tini 
           
           ibdyty = THnodes(inode)%ID_RBDY2
           itacty = THnodes(inode)%ID_TACTY
           
           concd  = get_therm_cond(ibdyty,itacty)
           
           QIJ = concd*DELTAT!*DX/DX
           
           THnodes(inode)%dT = THnodes(inode)%dT + Qij
           
           GLOBAL_AQIJ = GLOBAL_AQIJ + Qij
        end do

     case(i_d_2D__)

        !* LIMIT CONDITIONS *!
        DIAG = 1.d0 - 4.d0*ADT/DX2

        do IX = 1,NX
           inode = TH_BND_NODES(ibound)%TSOURCE(IX)
           TH_BND_NODES(ibound)%TBULK(IX,1) = THnodes(inode)%Tini
        end do

        TH_BND_NODES(ibound)%TBULK(1:NX,NY) = TH_BND_NODES(ibound)%T     

        !PRINT*,TH_BND_NODES(ibound)%TBULK(:,NY)

        !* BULK *!

        do IY = 2,NY-1

           TH_BND_NODES(ibound)%TBULK(1,IY) &
                = DIAG*TH_BND_NODES(ibound)%TBULK(1,IY) &
                + ADT*DX2_1*(TH_BND_NODES(ibound)%TBULK( 2,IY  ) &
                +            TH_BND_NODES(ibound)%TBULK(NX,IY  ) &
                +            TH_BND_NODES(ibound)%TBULK( 1,IY+1) &
                +            TH_BND_NODES(ibound)%TBULK( 1,IY-1))
           
           do IX = 2,NX-1
              TH_BND_NODES(ibound)%TBULK(IX,IY) &
                   = DIAG*TH_BND_NODES(ibound)%TBULK(IX,IY) &
                   + ADT*DX2_1*(TH_BND_NODES(ibound)%TBULK(IX+1,IY  ) &
                   +            TH_BND_NODES(ibound)%TBULK(IX-1,IY  ) &
                   +            TH_BND_NODES(ibound)%TBULK(IX  ,IY+1) &
                   +            TH_BND_NODES(ibound)%TBULK(IX  ,IY-1))
           end do
           
           TH_BND_NODES(ibound)%TBULK(NX,IY) &
                = DIAG*TH_BND_NODES(ibound)%TBULK(NX,IY) &
                + ADT*DX2_1*(TH_BND_NODES(ibound)%TBULK(1   ,IY  ) &
                +            TH_BND_NODES(ibound)%TBULK(NX-1,IY  ) &
                +            TH_BND_NODES(ibound)%TBULK(NX  ,IY+1) &
                +            TH_BND_NODES(ibound)%TBULK(NX  ,IY-1))
           
        end do

        TH_BND_NODES(ibound)%TBULKi = TH_BND_NODES(ibound)%TBULK

        !* THERMAL GRADIENT COMPUTATION

        do IX = 1,NX
           
           inode = TH_BND_NODES(ibound)%TSOURCE(IX)
           
           DELTAT = TH_BND_NODES(ibound)%TBULK(IX,2) - THnodes(inode)%Tini 
           
           ibdyty = THnodes(inode)%ID_RBDY2
           itacty = THnodes(inode)%ID_TACTY
           
           concd  = get_therm_cond(ibdyty,itacty)
           
           QIJ = concd*DELTAT!*DX/DX
           
           THnodes(inode)%dT = THnodes(inode)%dT + Qij
           
           GLOBAL_AQIJ = GLOBAL_AQIJ + Qij
           
        end do

     case DEFAULT
        
     end select

   end subroutine COMP_T_BULK
!!!------------------------------------------------------
   integer(kind=4) function get_nb_HEAT_bounds()
     implicit none

     get_nb_HEAT_bounds = nb_TH_BOUNDS

   end function get_nb_HEAT_bounds
!!!------------------------------------------------------
   subroutine get_HEAT_bound_dims(ibound,NX,NY)
     implicit none
     integer(kind=4) :: ibound,NX,NY
     
     NX  = TH_BND_NODES(ibound)%NX
     NY  = TH_BND_NODES(ibound)%NY

   end subroutine get_HEAT_bound_dims
!!!------------------------------------------------------
   subroutine GET_HEAT_BOUND_MNODES(ibound,MNODES)
     implicit none
     integer(kind=4)           :: ibound,inode,icd,IX,IY,NX,NY
     integer(kind=4)           :: ID_RBDY2,ID_TACTY
     real(kind=8)              :: DX,X0,Y0
     real(kind=8),dimension(2) :: coor
 
     real(kind=8),pointer,dimension(:,:) :: MNODES
     
     NX  = TH_BND_NODES(ibound)%NX
     NY  = TH_BND_NODES(ibound)%NY
     DX  = TH_BND_NODES(ibound)%DX

     X0 = 0.D0
     Y0 = 0.D0

     do inode = 1,TH_BND_NODES(ibound)%NX
        icd = TH_BND_NODES(ibound)%TSOURCE(inode)
        ID_RBDY2 = THnodes(icd)%ID_RBDY2
        ID_TACTY = THnodes(icd)%ID_TACTY
        coor = get_coor(ID_RBDY2,ID_TACTY)
        X0 = coor(1) + X0
        Y0 = coor(2) + Y0
     end do
     
     X0 = X0/real(TH_BND_NODES(ibound)%NX,8)
     Y0 = Y0/real(TH_BND_NODES(ibound)%NX,8)
     !PRINT*,'direction: ',TH_BND_NODES(ibound)%idirection
     !PRINT*,DX,NX
     select case(TH_BND_NODES(ibound)%idirection)
     case(i_upxxx)

        X0 = X0 - TH_BND_NODES(ibound)%length*0.5

        do IY = 1,NY
           do IX = 1,NX
              MNODES(1,IX + NX*(IY-1)) = X0 + DX*(IX-1) 
              MNODES(2,IX + NX*(IY-1)) = Y0 + DX*(IY-1)
           end do
        end do
     
     case(i_downx)

        X0 = X0 - TH_BND_NODES(ibound)%length*0.5

        do IY = 1,NY
           do IX = 1,NX
              MNODES(1,IX + NX*(IY-1)) = X0 + DX*(IX-1) 
              MNODES(2,IX + NX*(IY-1)) = Y0 - DX*(IY-1)
           end do
        end do

     case(i_leftx)

        Y0 = Y0 - TH_BND_NODES(ibound)%length*0.5

        do IY = 1,NY
           do IX = 1,NX
              MNODES(1,IX + NX*(IY-1)) = X0 - DX*(IX-1)
              MNODES(2,IX + NX*(IY-1)) = Y0 + DX*(IY-1) 
           end do
        end do

     case(i_right)

        Y0 = Y0 - TH_BND_NODES(ibound)%length*0.5

        do IY = 1,NY
           do IX = 1,NX
              MNODES(1,IX + NX*(IY-1)) = X0 + DX*(IX-1)
              MNODES(2,IX + NX*(IY-1)) = Y0 + DX*(IY-1) 
           end do
        end do
     end select

   end subroutine GET_HEAT_BOUND_MNODES
!!!------------------------------------------------------
   subroutine GET_HEAT_BOUND_TNODES(ibound,TNODES)
     implicit none
     integer(kind=4) :: ibound,IX,IY,NX,NY
     real(kind=8),pointer,dimension(:) :: TNODES
     
     NX  = TH_BND_NODES(ibound)%NX
     NY  = TH_BND_NODES(ibound)%NY

!     PRINT*,ibound,NX,NY

     do IY = 1,NY
        do IX = 1,NX
           TNODES(IX + NX*(IY-1)) = TH_BND_NODES(ibound)%TBULKi(IX,IY)
        end do
     end do

   end subroutine GET_HEAT_BOUND_TNODES
!!!------------------------------------------------------
   subroutine GET_HEAT_BOUND_PROFILE(ibound,TPROFIL)
     implicit none
     integer(kind=4) :: ibound,IX,IY,NX,NY
     real(kind=8),pointer,dimension(:) :: TPROFIL

     NX  = TH_BND_NODES(ibound)%NX
     NY  = TH_BND_NODES(ibound)%NY

     select case(MODtherm%ibound)
     case(i_d_1D__)

        do IY = 1,NY
           TPROFIL(IY) = TH_BND_NODES(ibound)%TBULKi(1,IY)
        end do

     case(i_d_2D__)

        do IY = 1,NY
           TPROFIL(IY) = 0.D0
           do IX = 1,NX
              TPROFIL(IY) = TPROFIL(IY) + TH_BND_NODES(ibound)%TBULKi(IX,IY)
           end do
           TPROFIL(IY) = TPROFIL(IY) / real(NX,8)
        end do

     case default

     end select

   end subroutine GET_HEAT_BOUND_PROFILE
!!!------------------------------------------------------
   subroutine init_free_boundary_mp_solver
     implicit none
     FREE_BOUNDARY = .true.

   end subroutine init_free_boundary_mp_solver
!!!------------------------------------------------------
   subroutine compute_flux_mailx
     implicit none
     integer(kind=4) :: icdan,nb_CLALp
     integer(kind=4) :: iclxxx,ialpxx
     real(kind=8)    :: rlt,rln
     real(kind=8)    :: flux,Tcd,Tan
     integer(kind=4) :: status

     nb_CLALp = get_nb_inters( i_CLALp )
     
     do icdan=1,nb_CLALp

        call get_rloc( i_CLALp, icdan, rlt, rln, status)

        if (status.eq.i_noctc) cycle

        iclxxx = CLALp2CLxxx(icdan)
        ialpxx = CLALp2ALpxx(icdan)

        !call get_Temp_CLxxx(iclxxx,iVfree,Tcd)
        !call get_Temp_ALpxx(ialpxx,iVfree,Tan)

        flux = (Tcd-Tan)*rln

     end do

   end subroutine compute_flux_mailx
!!!------------------------------------------------------
   subroutine init_thermal_conductivity_mp_solver

     implicit none
     integer      :: icdan,icdtac,iantac,ibdyty,itacty,adjsz,iadj,iblmty,ibehav
     real(kind=8) :: cdPTC,cdSTC,cdTnx,cdTny
     real(kind=8) :: anPTC,anSTC,anTnx,anTny
     real(kind=8) :: inx,iny,acs,bsn
     real(kind=8) :: Condcd,Condan,Condeff

     real(kind=8),dimension(2) :: coor,tuc,nuc
     real(kind=8),dimension(max_internal_tact) :: internal

     character(len=3)   :: thmodel

     nb_DKDKx = get_nb_verlets(i_dkdkx)

     !print*,'start mp_solver::init_thermal_conductivity_mp_solver'

     if(nb_DKDKx.ne.0) then
        print*,'nb DKDKx:',nb_DKDKx
        do icdtac=1,nb_DISKx
           adjsz = get_verlet_adjsz(i_dkdkx, icdtac)
           do iadj=1,adjsz
              print*,'get_verlet_internal',i_dkdkx,icdtac
              call get_verlet_internal(i_dkdkx, icdtac,iadj,internal)
              iantac = get_verlet_iantac(i_dkdkx, icdtac, iadj)

              print*,'get_verlet_local_frame'
              !if(internal(4).eq.0) cycle
              call get_verlet_local_frame(i_dkdkx, icdtac, iadj, coor, tuc, nuc) 

              ibdyty = diskx2bdyty(1,icdtac)
              itacty = diskx2bdyty(2,icdtac)
        
              !mr : we assume that iblmty = 1
              iblmty = 1
              ibehav = get_bulk_behav_number_RBDY2(ibdyty,iblmty)

              thmodel = get_Tmodel(ibehav)

              print*,'select case cd'

              select case(thmodel)
              case('iso') !jr : isotropic case
                 Condcd = get_therm_cond(ibdyty,itacty) 
              case('ani') !jr : anisotropic case  
                 call get_ani_therm_cond(ibdyty,itacty,cdPTC,cdSTC,cdTnx,cdTny) 
                 acs = cdPTC*abs(cdTnx*nuc(1)+cdTny*nuc(2))
                 bsn = cdSTC*sqrt(1-(abs(cdTnx*nuc(1)+cdTny*nuc(2)))**2)
                 Condcd = sqrt(acs*acs+bsn*bsn)
              end select
         
              ibdyty = diskx2bdyty(1,iantac)
              itacty = diskx2bdyty(2,iantac)

              !mr : we assume that iblmty = 1
              iblmty = 1
              ibehav = get_bulk_behav_number_RBDY2(ibdyty,iblmty)

              thmodel = get_Tmodel(ibehav)

              print*,'select case an'
              
              select case(thmodel)
              case('iso') !jr : isotropic case
                 Condan = get_therm_cond(ibdyty,itacty)
              case('ani') !jr : anisotropic case
                 call get_ani_therm_cond(ibdyty,itacty,anPTC,anSTC,anTnx,anTny)
                 acs = anPTC*abs(anTnx*nuc(1)+anTny*nuc(2))
                 bsn = anSTC*sqrt(1-(abs(anTnx*nuc(1)+anTny*nuc(2)))**2)
                 Condan = sqrt(acs*acs+bsn*bsn)
              end select

              Condeff = (2.0*Condcd*Condan)/(Condcd + Condan)
              internal(6) = Condeff
              print*,'set_verlet_internal'
              call set_verlet_internal(i_dkdkx, icdtac, iadj, internal)
           end do

        end do
     end if

!     nb_DKJCx = get_nb_DKJCx()
    
!     nb_DKKDx = get_nb_DKKDx()
    
!     nb_PLPLx = get_nb_PLPLx()
    
!     nb_PLJCx = get_nb_PLJCx()
     
     print*, 'end'

   end subroutine init_thermal_conductivity_mp_solver
!!!------------------------------------------------------
! jr : average contact number of middle for calculation of contact conductance in case of beta != 0

   subroutine compute_average_contacts()
     implicit none
     integer         :: icdan,icdtac,iantac,iadj
     integer         :: cdan_type
     real(kind=8)    :: val0,val3,rn,rt
     integer(kind=4) :: status

     integer( kind = 4 ) :: id_inter

     coordinance=0
  
     id_inter       =  i_dkdkx

     do icdan=1,nb_DKDKx
        call this2verlet( id_inter, icdan, icdtac, iadj )
        iantac = get_verlet_iantac( id_inter, icdtac, iadj )
        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
        if(abs(rn).lt.1.d-16) cycle 
        coordinance(icdtac) = coordinance(icdtac) + 1
        coordinance(iantac) = coordinance(iantac) + 1
     end do
     

     id_inter       =  i_dkkdx

     do icdan=1,nb_DKKDx
        call this2verlet( id_inter, icdan, icdtac, iadj )
        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
        if(abs(rn).lt.1.d-16) cycle 
        coordinance(icdtac) = coordinance(icdtac) + 1
     end do
    
     id_inter       =  i_dkjcx

     do icdan=1,nb_DKJCx
        call this2verlet( id_inter, icdan, icdtac, iadj )
        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
        if(abs(rn).lt.1.d-16) cycle 
        coordinance(icdtac) = coordinance(icdtac) + 1
     end do

     id_inter       =  i_plplx

     do icdan=1,nb_PLPLx
        call this2verlet( id_inter, icdan, icdtac, iadj )
        iantac = get_verlet_iantac( id_inter, icdtac, iadj )
        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
        if(abs(rn).lt.1.d-16) cycle 
        cdan_type = get_type_PLPLx(icdan)
        if(cdan_type.eq.0)then
           coordinance(nb_DISKx+icdtac) = coordinance(nb_DISKx+icdtac) + 1
           coordinance(nb_DISKx+iantac) = coordinance(nb_DISKx+iantac) + 1
        else
           coordinance(nb_DISKx+icdtac) = coordinance(nb_DISKx+icdtac) + 0.5
           coordinance(nb_DISKx+iantac) = coordinance(nb_DISKx+iantac) + 0.5
        end if
     end do

     id_inter       =  i_pljcx

     do icdan=1,nb_PLJCx
        call this2verlet( id_inter, icdan, icdtac, iadj )
        iantac = get_verlet_iantac( id_inter, icdtac, iadj )
        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
        if(abs(rn).lt.1.d-16) cycle 
        cdan_type = get_type_PLJCx(icdan)
        if(cdan_type.eq.0)then
           coordinance(nb_DISKx+icdtac) = coordinance(nb_DISKx+icdtac) + 1
        else
           coordinance(nb_DISKx+icdtac) = coordinance(nb_DISKx+icdtac) + 0.5
        end if
     end do

     MeanC = 0.d0

     if(nb_GRAIN.ne.0) then 
        val0  = sum(coordinance)
        val3  = 1.D0/real(nb_GRAIN,8)
        MeanC = val0*val3
     end if

   end subroutine compute_average_contacts
!-----------------------------------------
   subroutine get_branches_values(type,itact,vector)
     implicit none
     character(len=5)          :: type
     integer                   :: ibranche,itact
     real(kind=8),dimension(4) :: vector

     select case(type)
     case('DKDKx')
        ibranche = dkdkx2branches(itact)
     case('DKJCx')
        ibranche = dkjcx2branches(itact)
     case('DKKDx')
        ibranche = dkkdx2branches(itact)
     end select

     vector(1) = Branches(ibranche)%U
     vector(2) = Branches(ibranche)%I
     vector(3) = Branches(ibranche)%Ctot
     if(Branches(ibranche)%oxided)then
        vector(4) = 1.
     else
        vector(4) = 0. 
     end if

   end subroutine get_branches_values
!-----------------------------------------
!jr : calculation of specific heat in function of temperature
   subroutine update_specific_heat

     implicit none

     integer      :: inode,ibdyty,itacty
     real(kind=8) :: TMP,avrd,rho,Hspe

     do inode = 1,nb_NODES

       ibdyty = THnodes(inode)%ID_RBDY2
       itacty = THnodes(inode)%ID_TACTY

       rho  = get_rho(get_bulk_behav_number_RBDY2(ibdyty,1))
       Hspe = 338.3d0*((THnodes(inode)%T-273.15d0)**0.25)         !jr : experimental analytic law
       avrd = get_avr_radius_tacty(ibdyty,itacty)

       TMP = rho*Hspe*avrd*avrd*PI_g

       THnodes(inode)%alpha = 0.D0
       if ( TMP .gt. 1.D-16 ) THnodes(inode)%alpha = H/TMP

       call put_therm_sheat(ibdyty,itacty,Hspe)

     end do

   end subroutine update_specific_heat

!!!------------------------------------------------------
!jr : calculation of thermal diffusivities in function of temperature
   subroutine update_thermal_diffusivity(inode,ibdyty,itacty,alpha_rtheta,alpha_z)

     implicit none

     integer      :: inode,ibdyty,itacty
     real(kind=8) :: alpha_rtheta,alpha_z

     alpha_rtheta = 3.53*1.D-1*((THnodes(inode)%T)**(-0.451))  !jr : experimental analytic law
     alpha_z = 7.63*1.D-2*((THnodes(inode)%T)**(-0.378))              
     call put_therm_diffu(ibdyty,itacty,alpha_rtheta,alpha_z)

   end subroutine update_thermal_diffusivity

!!!------------------------------------------------------
!jr : heat generation by elasto-plastic strain
!   subroutine elasto_plastic_dissipation(QEij,QPij,internal,lawnb,icd,ian)!
!
!     implicit none
!     integer      :: icd,ian,lawnb
!     real(kind=8) :: TQcoeff,dilatation,QEij,QPij,stock,damage,rupture,mean
!     real(kind=8) :: GLOBAL_stock,GLOBAL_damage,GLOBAL_rupture
!     
!     real(kind=8),dimension(max_internal_tact) :: internal
!  
!     TQcoeff = 0.9
!     dilatation = 2*1.D-6
!     mean = (THnodes(icd)%dT + THnodes(ian)%dT)/2
!     
!     call get_energy_czm(lawnb,internal,stock,damage,rupture,GLOBAL_stock,GLOBAL_damage,GLOBAL_rupture) 
!
!     QEij = -dilatation*mean*(stock/H)              !jr : heat source from elastic strain 
!     
!     QPij = TQcoeff*(damage/H)                      !jr : heat source by plastic strain!
!
!   end subroutine elasto_plastic_dissipation
!!!------------------------------------------------------------------------
  subroutine set_heat_generation_factor(cvgen)
    implicit none
    real(kind=8) :: cvgen

    ConvGen = cvgen

  end subroutine set_heat_generation_factor
!!!------------------------------------------------------------------------
  subroutine set_heat_conduction_continue_factor(cvcond)
    implicit none
    real(kind=8) :: cvcond
    
    ConvCond = cvcond

  end subroutine set_heat_conduction_continue_factor
!!!------------------------------------------------------------------------


end module MP_SOLVER

