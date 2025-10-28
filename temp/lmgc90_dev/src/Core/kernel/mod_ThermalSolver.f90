module ThermalSolver

  use a_system

  use parameters
  use utilities
  use overall
  use BULK_BEHAVIOUR
  use TACT_BEHAVIOUR
  use a_DOF

  use RBDY2
  use DISKx
  use POLYG
  use JONCx
  use xKSID

  use inter_meca_handler_2D, only : get_nb_inters, &
                                    get_tact_lawnb

  implicit none

  type T_THERMAL_MODEL
     
     logical          :: lconv,init
     integer(kind=4)  :: iheat,ibound,ilkine,ildiff,igdiff
     integer(kind=4)  :: ilcond
     real(kind=8)     :: thickness
     real(kind=8)     :: T0,Alert,Gcond,GTemp

     integer(kind=4) :: LocDiff  ! cylnd, Hertz
     integer(kind=4) :: GloDiff  ! discr, cont_
     integer(kind=4) :: LocGene  ! no___,all__, norm_, tang_

     integer(kind=4) :: Boundary ! adia_, 1D___, 2D___

  end type T_THERMAL_MODEL
  
  type(T_THERMAL_MODEL) :: Tmodel

  ! nodal ddl's 

  !TYPE(T_nodty),DIMENSION(:),POINTER :: nodty    ! ID of the nodty storing the nodal deg of freedom ...
  !INTEGER,DIMENSION(:),POINTER :: nodty2M_nodty  ! NUM in the MAILx Data BAse i.e. inodty for the M_bdyty(iM_bdyty)


  !REAL(kind=8),DIMENSION(:),POINTER :: Tbegin,T,Tlast  ! Tbegin   temperature at the beginning of time step;
                                                     ! T        temperature during the current time step;

  !REAL(kind=8),DIMENSION(:),POINTER :: Taux

  !REAL(kind=8),DIMENSION(:),POINTER :: Fext,Fint ! Fext     external force, external momentum ...
                                                 !
                                                 ! Fint     internal force, internal momentum ...

  !REAL(kind=8),DIMENSION(:),POINTER :: residu

  ! part concerning the driven dof's of the bdyty

     
  !INTEGER                                 :: nb_temp_driven_dof
  !TYPE(T_driven_dof),DIMENSION(:),POINTER :: temp_driven_dof 
  !REAL(kind=8),DIMENSION(:),POINTER       :: Tdriv
  !fd new 
  !logical,dimension(:),pointer            :: is_Tdriv_active
  
  ! pour simplifier la vie avec le g_system
  !integer(kind=4),DIMENSION(:),POINTER    :: drvdofs
  !REAL(kind=8),DIMENSION(:),POINTER       :: drvvalues
  
  !INTEGER                                 :: nb_flux_driven_dof
  !TYPE(T_driven_dof),DIMENSION(:),POINTER :: flux_driven_dof 
  !REAL(kind=8),DIMENSION(:),POINTER       :: Fdriv
  
  ! matrix  
  !TYPE (G_matrix)               :: KT        ! matrix
  !REAL(kind=8),DIMENSION(:),POINTER   :: RHS       ! vector
  
  !integer                             :: nb_nodes
  !INTEGER                           :: nbdof     ! total number of dof of a bdyty
  
  !INTEGER ,DIMENSION(:),POINTER     :: ccdof     ! iccdof = bdyty(ibdyty)%ccdof(inodty)+1 for thermal analysis
  ! the comments below stand for idof running from 1 to N_DOF_by_NODE
  !INTEGER ,DIMENSION(:),POINTER     :: nodnb     ! inodty = bdyty(ibdyty)%nodnb(iccdof)
  ! and to dof:
  !INTEGER ,DIMENSION(:),POINTER     :: dofnb     ! idof = bdyty(ibdyty)%dofnb(iccdof)
  ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
  ! T_force_driven, with similar meanings.
  
  !INTEGER,DIMENSION(:),POINTER      :: perm,inv_perm ! node numbering
  !type(G_system) :: g_sys

  type T_NODES

     integer(kind=4) :: ID_RBDY2,ID_TACTY,ID_MAILx

     !* thermal purposes

     real(kind=8)    :: alpha                   ! alpha : (rho*Cp*V)
     real(kind=8)    :: T                       ! T     : Current temperature
     real(kind=8)    :: Tini                    ! Tini  : Initial temperature
     real(kind=8)    :: dT                      ! T     : Current temperature velocity
     real(kind=8)    :: dTini                   ! Tini  : Initial temperature velocity

     real(kind=8)    :: Hth,Hthini

     real(kind=8)    :: area,rho,cond,Hpse

  end type T_NODES

  type(T_NODES),dimension(:),allocatable :: mpNODES

  integer(kind=4) :: nb_DDL = 0
  integer(kind=4) :: nb_RBDY2 = 0 , nb_DISKx = 0 , nb_POLYG = 0 , nb_JONCx = 0 , nb_xKSID = 0

  integer(kind=4),allocatable,dimension(:) :: diskx2nodes,joncx2nodes,polyg2nodes,xksid2nodes

  integer(kind=4) :: nb_CDAN = 0 ,  nb_DKDKx = 0 , nb_DKJCx = 0
  integer(kind=4) :: nb_DKKDx = 0 , nb_PLPLx = 0 , nb_DKPLx = 0 , nb_PLJCx = 0

  ! temporaire a mettre dans mod_parameter

  integer(kind=4),parameter :: i_h_diff = 1, i_h_conv = 2
  integer(kind=4),parameter :: i_Boundary_adia = 1 , i_Boundary_line = 2 , i_Boundary_1D   = 3 , i_Boundary_2D = 4
  integer(kind=4),parameter :: i_LocGene_none  = 0 , i_LocGene_norm  = 1 , i_LocGene_tang  = 2 , i_LocGene_all = 3
  integer(kind=4),parameter :: i_LocDiff_none  = 0 , i_LocDiff_Hertz = 1 , i_LocDiff_Cylnd = 2
  integer(kind=4),parameter :: i_GloDiff_discr = 0 , i_GloDiff_cont  = 1

  !

  public &
       init_ThermalSolver, &
       compute_DOF_ThermalSolver, &
       assemb_LHS_ThermalSolver, &
       assemb_RHS_ThermalSolver, &
       set_boundary_conditions

contains

  subroutine load_models()
    implicit none
    integer(kind=8) :: i
    
    ! default model          ! available
    Tmodel%LocDiff  = i_LocDiff_Cylnd ! cylnd, Hertz
    Tmodel%GloDiff  = i_GloDiff_discr ! discr, cont_
    Tmodel%LocGene  = i_LocGene_all   ! no___,all__, norm_, tang_

    Tmodel%Boundary = i_Boundary_adia ! adia, 1D___, 2D___

    

  end subroutine load_models

  subroutine init_ThermalSolver()
    ! read tactor data base and create meshless models in therMAILx
    implicit none
    integer(kind=4) :: inode,itact,ibdyty,itacty
    real(kind=8)    :: area,Hspe,Cond,rho
    real(kind=8)    :: TMP
 
    ! 1. compter le nombre de noeuds (ddl) nb_ddl = nb_noeud*cst
    ! 2. creer la/les maps correspondantes entre les ddl locaux et les ddl des autres modeles
    ! 3. creer le Gsystem dynamique direct sparse ou iteratif
    ! 4. attribuer materiau aux noeuds (rho, C, k, ...) 

    ! 1. compter le nombre de noeuds (ddl) nb_ddl = nb_noeud*cst

    ! 1.1 deformable part

    nb_DDL = 0

    ! 1.2 rigid part

    nb_DISKx  = get_nb_DISKx()
    nb_DDL    = nb_DDL + nb_DISKx

    nb_JONCx  = get_nb_JONCx()
    nb_DDL    = nb_DDL + nb_DISKx

    nb_POLYG  = get_nb_POLYG()
    nb_DDL    = nb_DDL + nb_DISKx

    nb_xKSID  = get_nb_xKSID()
    nb_DDL    = nb_DDL + nb_xKSID

    if( nb_DDL .eq. 0 ) then
       call LOGMES(' @ no multi-physical DDL have been detected')
       stop
    end if

    if (allocated(mpNODES)) deallocate(mpNODES)
    allocate(mpNODES(nb_DDL))

    do inode = 1,nb_DDL

       mpNODES(inode)%ID_MAILx = 0 
       mpNODES(inode)%ID_RBDY2 = 0
       mpNODES(inode)%ID_TACTY = 0

       mpNODES(inode)%alpha = 0.D0
       mpNODES(inode)%cond  = 0.D0
       mpNODES(inode)%Hpse  = 0.D0
       mpNODES(inode)%rho   = 0.D0
       mpNODES(inode)%area  = 0.D0

       mpNODES(inode)%T     = 0.D0
       mpNODES(inode)%Tini  = 0.D0
       mpNODES(inode)%dT    = 0.D0
       mpNODES(inode)%dTini = 0.D0

       mpNODES(inode)%Hthini = 0.D0
       mpNODES(inode)%Hth    = 0.D0

    end do

    ! 2. creer la/les maps correspondantes entre les ddl locaux et les ddl des autres modeles

    if (.not.allocated(diskx2nodes)) allocate(diskx2nodes(nb_DISKx))
    if (.not.allocated(joncx2nodes)) allocate(joncx2nodes(nb_JONCx))
    if (.not.allocated(xksid2nodes)) allocate(xksid2nodes(nb_xKSID))
    if (.not.allocated(polyg2nodes)) allocate(polyg2nodes(nb_POLYG))

    diskx2nodes = 0.D0
    joncx2nodes = 0.D0
    xksid2nodes = 0.D0
    polyg2nodes = 0.D0

    inode = 0

    do itact = 1,nb_DISKx
       inode = inode + 1
       diskx2nodes(itact) = inode
       mpNODES(inode)%ID_RBDY2 = diskx2bdyty(1,itact)
       mpNODES(inode)%ID_TACTY = diskx2bdyty(2,itact)
    end do

    do itact = 1,nb_JONCx
       inode = inode + 1
       joncx2nodes(itact) = inode
       mpNODES(inode)%ID_RBDY2 = joncx2bdyty(1,itact)
       mpNODES(inode)%ID_TACTY = joncx2bdyty(2,itact)
    end do

    do itact = 1,nb_POLYG
       inode = inode + 1
       polyg2nodes(itact) = inode
       mpNODES(inode)%ID_RBDY2 = polyg2bdyty(1,itact)
       mpNODES(inode)%ID_TACTY = polyg2bdyty(2,itact)
    end do

    do itact = 1,nb_xKSID
       inode = inode + 1
       xksid2nodes(itact) = inode
       mpNODES(inode)%ID_RBDY2 = xksid2bdyty(1,itact)
       mpNODES(inode)%ID_TACTY = xksid2bdyty(2,itact)
    end do

    ! 3. ....

    ! 4. attribuer materiau aux noeuds (rho, C, k, ...) 

    do inode = 1,nb_DDL

       ibdyty = mpNODES(inode)%ID_RBDY2
       itacty = mpNODES(inode)%ID_TACTY

       !rho  = get_rho(get_bulk_behav_number_RBDY2(ibdyty,1))  ! 1 pour iblmty
       !Hspe = get_Hspe(get_bulk_behav_number_RBDY2(ibdyty,1)) ! 1 pour iblmty
       area = get_area_tacty(ibdyty,itacty)
       cond = get_therm_cond(ibdyty,itacty)

       TMP = rho*Hspe*area

       mpNODES(inode)%alpha = rho*Hspe*area

    end do

  end subroutine init_ThermalSolver
!
!  subroutine increment_ThermalSolver
!    implicit none
!    !fait dans thermMAILx! 
!
!  end subroutine increment_ThermalSolver
!
  subroutine compute_DOF_ThermalSolver()
    implicit none
    ! Calcul (Delta T) : T = Tbegin + (Delta T)
    ! Pousse T dans les modeles (update_dof est fait dans therMAILx)

  end subroutine compute_DOF_ThermalSolver
!
  subroutine assemb_LHS_ThermalSolver()
    implicit none
    integer(kind=4)             :: icdan,icdtac,iantac
    integer(kind=4)             :: icdnode,iannode
    real(kind=8)                :: Hcdan
    real(kind=8),dimension(2,2) :: mat_ele

    ! 1. boucler sur therMAILx puis boucle sur ces elements pour recuperer les Matrices 
    ! Elementaire que l'on assemble dans le GSystem Dynamique
    !
    ! 2. boucle sur les interactions 
    !    (remonter les noeuds supports + info geometrique (section)
    !     calcul parametres effectifs
    !     creer la matrice elementaire (appel a therMAILx)
    !     assemblage dans GSystem Dynamique

    nb_CDAN = 0

    nb_DKDKx = get_nb_inters( i_dkdkx )
    nb_CDAN = nb_CDAN + nb_DKDKx

    nb_DKJCx = get_nb_inters( i_dkjcx )
    nb_CDAN = nb_CDAN + nb_DKJCx

    nb_DKKDx = get_nb_inters( i_dkkdx )
    nb_CDAN = nb_CDAN + nb_DKKDx

    nb_PLPLx = get_nb_inters( i_plplx )
    nb_CDAN = nb_CDAN + nb_PLPLx

    nb_DKPLx = get_nb_inters( i_dkplx )
    nb_CDAN = nb_CDAN + nb_DKPLx

    nb_PLJCx = get_nb_inters( i_pljcx )
    nb_CDAN = nb_CDAN + nb_PLJCx

    do icdan = 1, nb_DKDKx

       call DKDKx2DISKx(icdan,icdtac,iantac)

       icdnode = diskx2nodes(icdtac)
       iannode = diskx2nodes(iantac)

       ! a envoyer a therMAILx 
       ! mpNODES(icdnode)%alpha
       ! a envoyer a therMAILx 
       ! mpNODES(iannode)%alpha

       ! a envoyer a therMAILx 
       Hcdan = get_Hth(icdan,icdnode,iannode)
       
    end do

    do icdan = 1, nb_DKJCx

       call DKJCx2DISKx(icdan,icdtac)
       call DKJCx2JONCx(icdan,iantac)

       icdnode = diskx2nodes(icdtac)
       iannode = joncx2nodes(iantac)

       ! a envoyer a therMAILx 
       ! mpNODES(icdnode)%alpha
       ! a envoyer a therMAILx 
       ! mpNODES(iannode)%alpha

       ! a envoyer a therMAILx 
       Hcdan = get_Hth(icdan,icdnode,iannode)

    end do

    do icdan = 1, nb_DKKDx

       call DKJCx2DISKx(icdan,icdtac)
       call DKKDx2xKSID(icdan,iantac)

       icdnode = diskx2nodes(icdtac)
       iannode = xksid2nodes(iantac)

       ! a envoyer a therMAILx 
       ! mpNODES(icdnode)%alpha
       ! a envoyer a therMAILx 
       ! mpNODES(iannode)%alpha

       ! a envoyer a therMAILx 
       Hcdan = get_Hth(icdan,icdnode,iannode)

    end do

    do icdan = 1, nb_PLPLx

       call PLPLx2POLYG(icdan,icdtac,iantac)

       icdnode = polyg2nodes(icdtac)
       iannode = polyg2nodes(iantac)

       ! a envoyer a therMAILx 
       ! mpNODES(icdnode)%alpha
       ! a envoyer a therMAILx 
       ! mpNODES(iannode)%alpha

       ! a envoyer a therMAILx 
       Hcdan = get_Hth(icdan,icdnode,iannode)

    end do

    do icdan = 1, nb_DKPLx

  !     call DKPLx2DISKx(icdan,icdtac)
 !      call DKPLx2POLYG(icdan,iantac)

       icdnode = diskx2nodes(icdtac)
       iannode = polyg2nodes(iantac)

       ! a envoyer a therMAILx 
       ! mpNODES(icdnode)%alpha
       ! a envoyer a therMAILx 
       ! mpNODES(iannode)%alpha

       ! a envoyer a therMAILx 
       Hcdan = get_Hth(icdan,icdnode,iannode)

    end do

    do icdan = 1, nb_PLJCx

       call PLJCx2POLYG(icdan,icdtac)
       call PLJCx2JONCx(icdan,iantac)

       icdnode = polyg2nodes(icdtac)
       iannode = joncx2nodes(iantac)

       ! a envoyer a therMAILx 
       ! mpNODES(icdnode)%alpha
       ! a envoyer a therMAILx 
       ! mpNODES(iannode)%alpha

       ! a envoyer a therMAILx 
       Hcdan = get_Hth(icdan,icdnode,iannode)

    end do

  end subroutine assemb_LHS_ThermalSolver
!
  subroutine assemb_RHS_ThermalSolver()
    implicit none
    ! 1. boucler sur therMAILx puis boucle sur ces elements pour recuperer les seconds 
    ! membres que l'on assemble dans le GSystem Dynamique
    !
    ! 2. boucle sur les interactions 
    !    (remonter les noeuds supports + info geometrique (section)
    !     calcul parametres effectifs
    !     creer les seconds membres elementaires (appel a therMAILx)
    !     assemblage dans GSystem Dynamique

  end subroutine assemb_RHS_ThermalSolver
!
  subroutine create_boundary_conditions()!(inode,lprimal,ldual)
    ! declare les conditions limites sur le ddl d'un noeud primal ou dual
    implicit none
    integer :: inode
    

  end subroutine create_boundary_conditions
!
  subroutine set_boundary_conditions()
    ! applique la valeur a la condition limite
    implicit none

  end subroutine set_boundary_conditions
!
  subroutine relax_boundary_conditions()!(inode,)
    ! desactive la condition limite
    implicit none

  end subroutine relax_boundary_conditions
!
!............................................................
!
  real(kind=8) function get_Hth(icdan,icd,ian)
    use mp_solver, only : comp_thermal_eff_value
    ! calcule le H conduction propre a chaque contact
    implicit none
    integer(kind=4) :: icdan,icd,ian
    integer(kind=4) :: ilaw
    real(kind=8)    :: reff,AREA
    real(kind=8)    :: cohn,coht,dw,Qij,ETHeff,Coneff,CndEL
    real(kind=8)    :: rlt,rln,vln,vlt,vlnBEGIN,vltBEGIN
    integer(kind=4) :: status

    real(kind=8),dimension(max_internal_tact) :: internal

    get_Hth = 0.d0

    ilaw = get_tact_lawnb(i_dkdkx, icdan)

    call get_internal_DKDKx(icdan,internal)
    call get_vloc_DKDKx(icdan,vln,vlt,vlnBEGIN,vltBEGIN)
    call get_loc_DKDKx(icdan,rlt,rln,status)

    call comp_eff_value(icd,ian,Coneff,CndEL,ETHeff)

!jr & mr: anisotropic conductivity
    if(internal(4).ne.0)then
       Coneff = internal(6)
    end if

    call get_coh(ilaw,cohn,coht,dw)

!mr: check with CZM
    select case(Tmodel%GloDiff)
    case(i_GloDiff_cont)

       select case(ilaw)
       case(i_IQS_MAC_CZM,i_MAC_CZM,i_IQS_WET_CZM)
          if(internal(4).eq.0.0)then
             select case(Tmodel%LocDiff)
             case(i_LocDiff_Hertz)
                AREA = (3.0*reff*max(0.D0,rln/H+cohn)/(4.0*ETHeff))**0.3333333D+00
             case(i_LocDiff_Cylnd)
                AREA = (8.0*reff*max(0.D0,rln/H+cohn)/(PI_g*ETHeff))**0.25
             case DEFAULT
                stop
             end select
          else
             AREA = reff*internal(4)
          end if
       case default
          AREA = PI_g/16.
       end select

    case(i_GloDiff_discr)

       select case(Tmodel%LocDiff)
       case(i_LocDiff_Hertz)
          AREA = (3.0*reff*max(0.D0,rln/H+cohn)/(4.0*ETHeff))**0.3333333D+00
       case(i_LocDiff_Cylnd)
          AREA = (4.0*reff*max(0.D0,rln/H+cohn)/(PI_g*ETHeff))**0.25
       case DEFAULT
          stop
       end select

    end select

    get_Hth = 2.0*Coneff*AREA

  end function get_Hth

end module ThermalSolver
