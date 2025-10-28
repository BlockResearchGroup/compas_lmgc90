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

module MAILx_type

  use a_dof, only : T_nodty, &
                    T_driven_dof

  use a_system, only : g_system

  use a_multiEF, only : nb_eo

  implicit none

  private

!------------------------------------------------------------------------------

  type :: T_mecablmty 
 
     ! number of mecanical bulk element (in mod_a_mecaEF.f90).
     integer(kind=4) :: blmnb
     ! serial number (in BEHAVIOURS.DAT) of law with nickname behav in body
     integer(kind=4) :: lawnb
     ! serial number (in MODELS.DAT) of the model
     integer(kind=4) :: mdlnb
     ! table de connectivite avec la numerotation locale
     integer(kind=4), dimension( : ), pointer :: NODES => null()
     ! new 22/05/06
     ! serial number of the property set in model
     ! modified by fd 13/08/09
     ! one needs a ppset by rig gp instead of a ppset by ele
     ! mass gp are managed without ppset
     integer(kind=4), dimension( : ), pointer :: ppsnb => null()

     ! number of dof of the element nodes
     integer(kind=4) :: n_dof_by_node
     
     ! correspondance between local and global dof
     integer, dimension( : ), pointer :: edof2gdof => null()

     ! elementary stiffness
     real(kind=8), dimension( : , : ), pointer :: stiffness => null()
     ! elementary mass matrix
     real(kind=8), dimension( : , : ), pointer :: mass      => null()
     ! elementary damping matrix
     real(kind=8), dimension( : , : ), pointer :: damping   => null()
     ! elementary external forces vector
     real(kind=8), dimension( : , : ), pointer :: Fext      => null()
     ! elementary internal forces vector
     real(kind=8), dimension( : , : ), pointer :: Fint      => null()
     ! elementary internal forces vector
     real(kind=8), dimension( : )    , pointer :: ttFint    => null()

     !fd passe dans le g_system
     ! elementary internal forces vector
     real(kind=8), dimension( : )    , pointer :: RHSloc    => null()
 
  end type T_mecablmty

!------------------------------------------------------------------------------

  type, public :: T_mecaMAILx

     ! blmty
     type( T_mecablmty ), dimension( : ), pointer :: blmty         => null()

     ! mapping between mecaMAILx and MAILx element rank
     integer(kind=4)    , dimension( : ), pointer :: blmty2M_blmty => null()

     ! nodal ddl's 

     ! nodty storing the nodal deg of freedom ...
     type( T_nodty ), dimension( : ), pointer :: nodty => null()

     ! mapping between mecaMAILx and MAILx node rank
     integer(kind=4), dimension( : ), pointer :: nodty2M_nodty => null()

     ! Nodal fields (Velocity and displacement) managed by the integrators: MOREAU or BETA2

     ! For MOREAU (implicit)     
     ! Vbegin  velocity at beginning of the current time step (known)
     ! V       velocity at the end of the current time step (unknown)
     ! Vlast   last value of the velocity at the end of the current time step during NR iteration (known)
     ! Xbegin  displacement at beginning of time step with respect to reference coordinates (known) 
     ! X       displacement at the end of the current time step with respect to reference coordinates (unknown)

     ! For BETA2 (explicit)
     ! Vbegin  velocity at beginning of the current time step (known)
     ! V       velocity at the end of the current time step (unknown)
     !         during the step V contains a working velocity V = X-Xbegin/H ! Xprev   displacement at beginning of time step with respect to reference coordinates (known)
     ! Xbegin  displacement at the end of the current time step with respect to reference coordinates (known)
     ! X       displacement at the end of the next time step with respect to reference coordinates (unknown)


     real(kind=8), dimension( : ), pointer :: Vbegin => null()
     real(kind=8), dimension( : ), pointer :: V      => null()
     real(kind=8), dimension( : ), pointer :: Vlast  => null()
     real(kind=8), dimension( : ), pointer :: Xprev  => null()
     real(kind=8), dimension( : ), pointer :: Xbegin => null()
     real(kind=8), dimension( : ), pointer :: X      => null()

     ! Fext: external force, external momentum ...
     real(kind=8), dimension( : ), pointer :: Fext => null()
     ! Fint: internal force, internal momentum ...
     real(kind=8), dimension( : ), pointer :: Fint => null()
     
     logical :: is_reac_modified = .true.
     ! Ireac: resulting reaction impulses, resulting reaction impulse momentum
     real(kind=8), dimension( : ), pointer :: IReac => null()
     ! Iaux: storage resulting reaction impulse, storage resulting reaction impulse momentum
     real(kind=8), dimension( : ), pointer :: Iaux  => null()
     ! Vfree: free velocity no reaction acting;
     real(kind=8), dimension( : ), pointer :: Vfree => null()
     ! Vaux: velocity provisional vector;
     real(kind=8), dimension( : ), pointer :: Vaux  => null()
     ! Vddm: Velocity for DDM_SCHWARTZ, contains result of dV from reac of aother domains
     real(kind=8), dimension( : ), pointer :: Vddm  => null()

     real(kind=8), dimension( : ), pointer :: residu   => null()
     real(kind=8), dimension( : ), pointer :: Finert   => null()
     real(kind=8), dimension( : ), pointer :: momentum => null()

     ! le nombre de noeuds du maillage
     integer(kind=4) :: nb_nodes

     ! iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
     integer(kind=4), dimension( : ), pointer :: ccdof => null()

     ! the comments below stand for idof running from 1 to N_DOF_by_NODE

     ! total number of dof of a bdyty
     integer(kind=4) :: nbdof

     ! In a reverse way, in the body i_bdyty, the index iccdof corresponds
     ! to node:
     ! inodty = bdyty(ibdyty)%nodnb(iccdof)
     integer(kind=4), dimension( : ), pointer :: nodnb => null()
     ! and to dof:
     ! idof = bdyty(ibdyty)%dofnb(iccdof)
     integer(kind=4), dimension( : ), pointer :: dofnb => null()

     ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
     ! T_force_driven, with similar meanings.


     ! contact detection configuration
     real(kind=8), dimension( : , : ), pointer :: coorTT  => null()
     real(kind=8), dimension( : )    , pointer :: RcoorTT => null()

     ! part concerning the driven dof's of the bdyty
     integer(kind=4)                               :: nb_vlocy_driven_dof
     type( T_driven_dof ), dimension( : ), pointer :: vlocy_driven_dof    => null()
     real(kind=8)        , dimension( : ), pointer :: Vdriv               => null()
     real(kind=8)        , dimension( : ), pointer :: VdrivBeg            => null()
     real(kind=8)        , dimension( : ), pointer :: Xdriv               => null()

     ! pour simplifier la vie avec le g_system
     integer(kind=4), dimension( : ), pointer :: drvdofs   => null()
     real(kind=8)   , dimension( : ), pointer :: drvvalues => null()
     integer(kind=4), dimension( : ), pointer :: drvstatus => null()

     integer(kind=4)                               :: nb_force_driven_dof
     type( T_driven_dof ), dimension( : ), pointer :: force_driven_dof => null()
     real(kind=8)        , dimension( : ), pointer :: Fdriv            => null()
     real(kind=8)        , dimension( : ), pointer :: FdrivBeg         => null()

     real(kind=8)        , dimension( : ), pointer :: RHS => null()

     real(kind=8)        , dimension( : ), pointer :: Xwear => null()
     real(kind=8)        , dimension( : ), pointer :: Vwear => null()
     
     ! due to fd
     ! precondensation
     ! lorsque lineaire HPP on calcule w condensee sur les noeuds supports de contact
     ! pb ne supporte pas les rotations 

     ! le corps est il precondense
     logical :: is_precon = .false.
     ! les noeuds precon
     integer(kind=4), dimension( : )    , pointer :: nodes_precon => null()
      ! la matrice
     real(kind=8)   , dimension( : , : ), pointer :: W_precon     => null()
     ! la matrice transposee
     !real(kind=8), dimension(:,:), pointer :: W_precon_T => null()
     ! un vecteur auxiliaire
     real(kind=8)   , dimension( : )    , pointer :: Vaux_precon  => null()
     ! la map des ddl precon vers les ddl globaux
     integer(kind=4), dimension( : )    , pointer :: p2g          => null()
     integer(kind=4), dimension( : )    , pointer :: g2p          => null()
     ! le nombre de ddl concernes par le precon
     integer(kind=4) :: nbdof_precon
     ! la matrice W precondensee doit elle etre sauvee !PTA 22/03/2013
     logical :: saved_precon_W = .false.

     ! due to fd & pt 
     ! coro
     ! lorsque lineaire HPP on calcule un w en separant mouvement de corps rigide de la partie deformation 
     ! autorise les grandes rotations
     !
     ! attention lorqu'on utilise se formalisme modifie on decrit tous les modeles mass,kt,V,X,... dans le repere principale d'inertie
     ! a chaque fois qu'on applique une force ou qu'on recupere une vitesse il faut tourner les choses
     !
     logical :: is_rigid       = .false.
     logical :: skip_defo_comp = .false.
     logical :: is_coro        = .false.
     ! 

     ! coordonnees du maillage dans le repere principal d'inertie
     real(kind=8), dimension( : , : ), allocatable :: cooref_local
     
     ! maps
     real(kind=8), dimension( : , : ), allocatable :: R2D
     real(kind=8), dimension( : , : ), allocatable :: D2R
     ! position centre d inertie reference reference
     real(kind=8), dimension( : )    , allocatable :: cooref_G
     ! position centre d inertie debut pas
     real(kind=8), dimension( : )    , allocatable :: coorbegin_G
     ! reperes d inertie begin (begin), current et tt
     real(kind=8), dimension( : , : ), allocatable :: LocalFrameIni
     real(kind=8), dimension( : , : ), allocatable :: Localframe
     real(kind=8), dimension( : , : ), allocatable :: LocalFrameTT

     integer(kind=4) :: nbdofR
     ! vitesse de corps rigides
     real(kind=8), dimension( : ), allocatable :: RV
     ! vitesse de corps rigides debut
     real(kind=8), dimension( : ), allocatable :: RVbegin
     ! vitesse de corps rigides debut
     real(kind=8), dimension( : ), allocatable :: RVaux
     ! vitesse libre de corps rigides
     real(kind=8), dimension( : ), allocatable :: RVfree
     ! translations de corps rigides
     real(kind=8), dimension( : ), allocatable :: RX
     real(kind=8), dimension( : ), allocatable :: RXbegin
     real(kind=8), dimension( : ), allocatable :: RX_TT
     ! increment de deplacement de corps rigides
     real(kind=8), dimension( : ), allocatable :: RIreac
     real(kind=8), dimension( : ), allocatable :: RIaux
     real(kind=8), dimension( : ), allocatable :: RFext
     real(kind=8), dimension( : ), allocatable :: RFint

     ! fd a virer
     real(kind=8), dimension( : ), allocatable :: dep_R_TT
     ! matrice pour le cas rigid
     ! (masse,inertie) du rigide dans le rep d'inertie
     real(kind=8), dimension( : ), allocatable :: mR
     real(kind=8), dimension( : ), allocatable :: inv_mR

     integer(kind=4)                 :: nb_RV_driven
     integer(kind=4), dimension( 6 ) :: RV_driven_dof
     real(kind=8)   , dimension( 6 ) :: RV_driven

     ! energy
     real(kind=8) :: E_pot
     real(kind=8) :: E_cin
     real(kind=8) :: E_def
     ! work
     real(kind=8) :: W_pot
     real(kind=8) :: W_cin
     real(kind=8) :: W_def
     real(kind=8) :: W_ddl
     real(kind=8) :: W_con
     ! power
     real(kind=8) :: P_pot
     real(kind=8) :: P_cin
     real(kind=8) :: P_def
     real(kind=8) :: P_ddl
     real(kind=8) :: P_con
     !
     logical :: visible     = .true.
     logical :: is_periodic = .false.
     integer(kind=4), dimension( : ), pointer :: periodicnode => null()

     type( G_system ) :: g_sys

     ! Elements visibility. Used for element erosion.
     integer     , dimension(:), pointer :: eviz => null()

     ! Elements energy. Used for element erosion.
     real(kind=8), dimension(:), pointer :: elem_energy => null()

  end type T_mecaMAILx
  
!------------------------------------------------------------------------------

  type :: T_poroblmty 
 
     integer                          :: blmnb                ! number of thermal bulk element (in mod_a_therEF.f90).      
     integer, dimension( : ), pointer :: NODES                ! table de connectivite avec la numerotation locale
     integer                          :: lawnb                ! serial number (in BEHAVIOURS.DAT) of law with nickname behav in body; 
     integer                          :: mdlnb                ! serial number (in MODELS.DAT) of the model
     
     integer, dimension( : ), pointer :: ppsnb                ! serial number of the property set in model ! new 22/05/06
                                                          ! modified by fd 13/08/09 
                                                          ! one needs a ppset by rig gp instead of a ppset by ele
                                                          ! mass gp are managed without ppset
     
     integer, dimension( : ), pointer :: n_dof_by_node       ! number of dof of the node
     integer                          :: meca_n_dof_by_node  ! number of dof par node meca
     integer                          :: ther_n_dof_by_node  ! number of dof par node ther
     integer                          :: meca_node           ! number of node meca
     integer                          :: ther_node           ! number of node ther
     integer                          :: ndof                ! nombre de dof total
     integer                          :: ndof_meca           ! nombre de dof de meca
     integer                          :: ndof_ther           ! nombre de dof de ther
     
     integer, dimension( : ), pointer :: edof2gdof  ! correspondance between local and global dof
     integer, dimension( : ), pointer :: meca2poro  ! correspondance between mecadof and porodof
     integer, dimension( : ), pointer :: ther2poro  ! correspondance between therdof and porodof
     
     real(kind=8), dimension( : , : ), pointer :: stiffness  ! elementary stifness matrix
     real(kind=8), dimension( : , : ), pointer :: mass       ! elementary mass matrix
     real(kind=8), dimension( : , : ), pointer :: damping    ! elementary damping matrix
     !real(kind=8),dimension(:,:),pointer :: KTloc         ! 
     real(kind=8), dimension( : , : ), pointer :: Fext       ! elementary external fluxes vector
     real(kind=8), dimension( : , : ), pointer :: Fint       ! elementary internal fluxes vector
     !am : ajout des vecteurs de flux externes elementaires, dans la
     ! configuration milieu 
     real(kind=8), dimension( : ), pointer :: ttFext  ! elementray external fluxes vector
     real(kind=8), dimension( : ), pointer :: ttFint  ! elementray internal fluxes vector
     real(kind=8), dimension( : ), pointer :: RHSloc  ! elementary internal forces vector
     
  end type T_poroblmty
  
!------------------------------------------------------------------------------

  type, public :: T_poroMAILx
     
     integer :: bdyty2M_bdyty
     
     ! blmty
     
     type( T_poroblmty ), dimension( : ), pointer :: blmty 
     
     integer, dimension( : ), pointer :: blmty2M_blmty  ! NUM in the MAILx Data BAse i.e. iblmty for the M_bdyty(iM_bdyty)
     
     ! nodal ddl's 

     type( T_nodty ), dimension( : ), pointer :: nodty  ! ID of the nodty storing the nodal deg of freedom ...
     integer, dimension( : ), pointer :: nodty2M_nodty  ! NUM in the MAILx Data BAse i.e. inodty for the M_bdyty(iM_bdyty)


     real(kind=8), dimension( : ), pointer :: Vbegin  ! Ubegin   dof at the beginning of time step;
                                                      ! U        dof during the current time step;
     real(kind=8), dimension( : ), pointer :: V       ! Ubegin   dof at the beginning of time step;
                                                      ! U        dof during the current time step;
     real(kind=8), dimension( : ), pointer :: Vlast   ! Ubegin   dof at the beginning of time step;
                                                      ! U        dof during the current time step;

     real(kind=8), dimension( : ), pointer :: V_ALE_begin  ! V_ALE_beg    ALE at the beginning of time step;
                                                           ! V_ALE        ALE during the current time step;
     real(kind=8), dimension( : ), pointer :: V_ALE  ! V_ALE_beg    ALE at the beginning of time step;
                                                     ! V_ALE        ALE during the current time step;

     integer(kind=4), dimension( : ), pointer :: Mask_ALE     ! Masque       Connaissance d'un DDL fluide ou solide ALE;
     integer(kind=4), dimension( : ), pointer :: Mask_No_ALE  ! Masque       Connaissance d'un DDL fluide ou solide ALE;
                                                    
     
     integer(kind=4), dimension( : , : ), pointer :: Mask_P2U ! Masque de l'evaluation de la pression sur les noeuds milieux;
     
     real(kind=8), dimension( : ), pointer :: Xbegin
     real(kind=8), dimension( : ), pointer :: X  ! Xbegin   displacement, angular change ... 
                                                 !          at beginning of time step
                                                 !          with respect to reference coordinates;
                                                 ! X        displacement, angular change ...
                                                 !          while iterating 
                                                 !          with respect to reference coordinates;
                                                 ! values X are set to Xbegin+H*Vbegin
                                                 ! at first step iteration, allowing a single theta method iteration 
                                                 ! constant linear systems, see INCREMENT;

     real(kind=8), dimension( : ), pointer :: Fext  ! Fext     external force, external momentum ...
     real(kind=8), dimension( : ), pointer :: Fint  ! Fint     internal force, internal momentum ...

     real(kind=8), dimension( : ), pointer :: Ireac ! Ireac    resulting reaction impulses, resulting reaction impulse momentum
     real(kind=8), dimension( : ), pointer :: Iaux  ! Raux     storage resulting reaction impulses, storage resulting reaction impulse momentum

     real(kind=8), dimension( : ), pointer :: Vfree  ! Vfree    free velocity no reaction acting;
     real(kind=8), dimension( : ), pointer :: Vaux   ! Vaux     velocity provisional vector;
     real(kind=8), dimension( : ), pointer :: residu
     real(kind=8), dimension( : ), pointer :: Finert

     integer                          :: nb_nodes    ! le nombre de noeuds du maillage

     integer ,dimension( : ), pointer :: ccdof       ! iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
                                                     ! the comments below stand for idof running from 1 to N_DOF_by_NODE 
     integer                          :: nbdof       ! total number of dof of a bdyty
     integer                          :: nbdof_meca  ! total number of dof of a bdyty
     

                                                    ! In a reverse way, in the body i_bdyty, the index iccdof corresponds
                                                   ! to node:
     integer, dimension( : ), pointer :: nodnb     ! inodty = bdyty(ibdyty)%nodnb(iccdof)
                                                   ! and to dof:
     integer, dimension( : ), pointer :: dofnb     ! idof = bdyty(ibdyty)%dofnb(iccdof)
                                                   ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
                                                   ! T_force_driven, with similar meanings.

     ! contact detection configuration
     real(kind=8), dimension( : , : ), pointer :: coorTT
     real(kind=8), dimension( : )    , pointer :: RcoorTT
     
     ! part concerning the driven dof's of the bdyty

     
     integer                                       :: nb_poro_driven_dof
     type( T_driven_dof ), dimension( : ), pointer :: poro_driven_dof 
     real(kind=8)        , dimension( : ), pointer :: Tdriv
     real(kind=8)        , dimension( : ), pointer :: Vdriv
     real(kind=8)        , dimension( : ), pointer :: VdrivBeg
     real(kind=8)        , dimension( : ), pointer :: Xdriv
     !fd new 
     logical, dimension( : ), pointer :: is_Tdriv_active
     
     integer                                       :: nb_flux_driven_dof
     type( T_driven_dof ), dimension( : ), pointer :: flux_driven_dof 
     real(kind=8)        , dimension( : ), pointer :: Fdriv
     real(kind=8)        , dimension( : ), pointer :: FdrivBeg

     ! pour simplifier la vie avec le g_system
     integer(kind=4), dimension( : ), pointer :: drvdofs
     real(kind=8)   , dimension( : ), pointer :: drvvalues
     
     ! matrix  
     !type (G_matrix)                     :: KT        ! matrix
     real(kind=8), dimension( : ), pointer :: RHS  ! vector
     
     !integer,dimension(:),pointer      :: perm,inv_perm ! node numbering

     ! due to fd
     ! precondensation
     ! lorsque lineaire HPP on calcule w condensee sur les noeuds supports de contact
     ! pb ne supporte pas les rotations 

     logical                                   :: is_precon               ! le corps est il precondense
     integer     , dimension( : )    , pointer :: nodes_precon  => null() ! les noeuds precon
     real(kind=8), dimension( : , : ), pointer :: W_precon      => null() ! la matrice 
     real(kind=8), dimension( : , : ), pointer :: W_precon_T    => null() ! la matrice transposee 
     real(kind=8), dimension( : )    , pointer :: Vaux_precon   => null() ! un vecteur auxiliaire 
     integer     , dimension( : )    , pointer :: p2g           => null() ! la map des ddl precon vers les ddl globaux
     integer     , dimension( : )    , pointer :: g2p           => null() ! la map des ddl precon vers les ddl globaux
     integer                                   :: nbdof_precon            ! le nombre de ddl concernes par le precon
     
     integer, dimension( : ), pointer :: periodicnode
     logical :: visible
     logical :: is_periodic
    
     type( G_system ) :: g_sys

  end type T_poroMAILX

!------------------------------------------------------------------------------

  type :: T_thermblmty 
 
     integer                          :: blmnb          ! number of thermal bulk element (in mod_a_therEF.f90).      
     integer, dimension( : ), pointer :: NODES          ! table de connectivite avec la numerotation locale
     integer                          :: lawnb          ! serial number (in BEHAVIOURS.DAT) of law with nickname behav in body; 
     integer                          :: mdlnb          ! serial number (in MODELS.DAT) of the model
     integer                          :: n_dof_by_node  ! number of dof of the element nodes
     
     integer, dimension( : ), pointer :: ppsnb          ! serial number of the property set in model ! new 22/05/06
                                                    ! modified by fd 13/08/09 
                                                    ! one needs a ppset by rig gp instead of a ppset by ele
                                                    ! mass gp are managed without ppset

     integer     , dimension( : ), pointer :: edof2gdof  ! correspondance between local and global dof
     
     real(kind=8), dimension( : , : ), pointer :: conductivity  ! elementary conductivity
     real(kind=8), dimension( : , : ), pointer :: convection    ! elementary convection
     real(kind=8), dimension( : , : ), pointer :: capacity      ! elementary capacity
     real(kind=8), dimension( : , : ), pointer :: capacity_supg ! elementary capacity
     !real(kind=8),dimension(:,:),pointer :: KTloc         ! 
     real(kind=8), dimension( : , : ), pointer :: Fext          ! elementary external fluxes vector
     real(kind=8), dimension( : , : ), pointer :: Fint          ! elementary internal fluxes vector
     real(kind=8), dimension( : )    , pointer :: ttFint        ! elementary internal fluxes vector
     !am : ajout des vecteurs de flux externes elementaires, dans la
     ! configuration milieu 
     real(kind=8), dimension( : )    , pointer :: ttFext        ! elementray external fluxes vector
     real(kind=8), dimension( : )    , pointer :: RHSloc        ! elementary internal forces vector
     
  end type T_thermblmty
  
!------------------------------------------------------------------------------

  type, public :: T_therMAILx
     
     integer :: bdyty2M_bdyty
     
     ! blmty
     
     type( T_thermblmty ), dimension( : ), pointer :: blmty
     integer             , dimension( : ), pointer :: blmty2M_blmty  ! NUM in the MAILx Data BAse i.e. iblmty for the M_bdyty(iM_bdyty)
     
     ! nodal ddl's 

     type( T_nodty ), dimension( : ), pointer :: nodty    ! ID of the nodty storing the nodal deg of freedom ...
     integer        , dimension( : ), pointer :: nodty2M_nodty  ! NUM in the MAILx Data BAse i.e. inodty for the M_bdyty(iM_bdyty)


     real(kind=8), dimension( : ), pointer :: Tbegin  ! Tbegin   temperature at the beginning of time step;
     real(kind=8), dimension( : ), pointer :: T       ! T        temperature during the current time step;
     real(kind=8), dimension( : ), pointer :: Tlast

     real(kind=8), dimension( : ), pointer :: Taux

     real(kind=8), dimension( : ), pointer :: Fext  ! Fext     external force, external momentum ...
     real(kind=8), dimension( : ), pointer :: Fint  ! Fint     internal force, internal momentum ...

     real(kind=8), dimension( : ), pointer :: residu

     ! part concerning the driven dof's of the bdyty

     
     integer                                       :: nb_temp_driven_dof
     type( T_driven_dof ), dimension( : ), pointer :: temp_driven_dof 
     real(kind=8)        , dimension( : ), pointer :: Tdriv
     !fd new 
     logical             , dimension( : ), pointer :: is_Tdriv_active
     
     ! pour simplifier la vie avec le g_system
     integer(kind=4), dimension( : ), pointer :: drvdofs
     real(kind=8)   , dimension( : ), pointer :: drvvalues
     
     integer                                       :: nb_flux_driven_dof
     type( T_driven_dof ), dimension( : ), pointer :: flux_driven_dof 
     real(kind=8)        , dimension( : ), pointer :: Fdriv
     
     ! matrix  
     !type (G_matrix)               :: KT        ! matrix
     real(kind=8), dimension( : ), pointer :: RHS  ! vector
     
     integer :: nb_nodes
     integer :: nbdof     ! total number of dof of a bdyty
     
     integer, dimension( : ), pointer :: ccdof  ! iccdof = bdyty(ibdyty)%ccdof(inodty)+1 for thermal analysis
                                                ! the comments below stand for idof running from 1 to N_DOF_by_NODE
     integer, dimension( : ), pointer :: nodnb  ! inodty = bdyty(ibdyty)%nodnb(iccdof)
                                                ! and to dof:
     integer, dimension( : ), pointer :: dofnb  ! idof = bdyty(ibdyty)%dofnb(iccdof)
                                                ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
                                                ! T_force_driven, with similar meanings.
     
     !integer,dimension(:),pointer      :: perm,inv_perm ! node numbering
     type( G_system ) :: g_sys

  end type T_therMAILX

!------------------------------------------------------------------------------

  type :: T_multiblmty 

     !> number of bulk element
     integer(kind=4) :: blmnb
     !> serial number (in BEHAVIOURS.DAT) of law with nickname behav in body; 
     integer(kind=4) :: lawnb
     !> serial number (in MODELS.DAT) of the model
     integer(kind=4) :: mdlnb
     !> connectivity with local numbering
     integer(kind=4), dimension( : ), pointer :: NODES
     
     !> serial number of the property set in model
     integer(kind=4) :: ppsnb
     
     !> map between local and global dof indices
     integer(kind=4), dimension( : ), pointer :: edof2gdof

     !> map between operator id and field rank
     integer(kind=4), dimension( nb_eo ) :: eo2fr

     !> elementary stiffness matrix
     real(kind=8), dimension( : , : ), pointer :: stiffness
     !> elementary mass matrix
     real(kind=8), dimension( : , : ), pointer :: mass
     !> elementary damping matrix
     real(kind=8), dimension( : , : ), pointer :: damping
     
     !> elementary external fluxes vector (at beginning and current step)
     real(kind=8), dimension( : , : ), pointer :: Fext
     !> elementary internal fluxes vector (at beginning and current step)
     real(kind=8), dimension( : , : ), pointer :: Fint
     
     !> elementary internal forces vector to pass to G_sys
     real(kind=8), dimension( : )    , pointer :: RHSloc
     
  end type T_multiblmty
  
!------------------------------------------------------------------------------

  type, public :: T_multiMAILx
     
     !> map to MAILx body numbering
     integer(kind=4) :: bdyty2M_bdyty
     
     !> bulk elements
     type( T_multiblmty ), dimension( : ), pointer :: blmty 
     
     !> NUM in the MAILx database i.e. iblmty for the M_bdyty(iM_bdyty)
     integer(kind=4)     , dimension( : ), pointer :: blmty2M_blmty
     
     ! nodal ddl's 
     !> ID of the nodty storing the nodal degrees of freedom
     type( T_nodty ), dimension( : ), pointer :: nodty
     !> NUM in the MAILx database i.e. inodty for the M_bdyty(iM_bdyty)
     integer(kind=4), dimension( : ), pointer :: nodty2M_nodty

     !> Degrees of freedom values at the current and beginning of time step (in that order)
     real(kind=8), dimension( : , : ), pointer :: Dofs
     !> Degrees of freedom values at the previous iteration of current time step
     real(kind=8), dimension( : )    , pointer :: DofsLast

     !rm : ?
     ! V_ALE_beg    ALE at the beginning of time step;
     ! V_ALE        ALE during the current time step;
     !REAL(kind=8),DIMENSION(:),POINTER :: V_ALE_begin,V_ALE
     !                                                      
     ! Masque       Connaissance d'un DDL fluide ou solide ALE;
     !INTEGER(kind=4),DIMENSION(:),POINTER      :: Mask_ALE      
     ! Masque       Connaissance d'un DDL fluide ou solide ALE;
     !INTEGER(kind=4),DIMENSION(:),POINTER      :: Mask_No_ALE   
     !                                               
     !
     ! Masque de l'evaluation de la pression sur les noeuds milieux;
     !INTEGER(kind=4),DIMENSION(:,:),POINTER :: Mask_P2U 

     !> displacement at while iterating and at beginning of time step 
     !> (in that order) with respect to reference coordinates;
     !> values X are set to Xbegin+H*Vbegin at first step iteration,
     !> allowing a single theta method iteration 
     real(kind=8), dimension( : , : , : ), pointer :: X

     !> external forces, momentum
     real(kind=8), dimension( : ), pointer :: Fext
     !> internal forces/flux from stiffness
     real(kind=8), dimension( : ), pointer :: Fint
     !> internal forces/flux from damping
     real(kind=8), dimension( : ), pointer :: Fdmp
     !> resulting reaction forces, momentum (impulses)
     real(kind=8), dimension( : ), pointer :: Ireac
     !> storage of resulting reaction forces, momentum (impulses)
     real(kind=8), dimension( : ), pointer :: Iaux

     !> Degrees of freedom no reaction acting
     real(kind=8), dimension( : ), pointer :: DofsFree
     !> Degrees of freedom provisional vector
     real(kind=8), dimension( : ), pointer :: DofsAux

     !> Residue
     real(kind=8), dimension( : ), pointer :: residu
     !> Inertial fluxes/forces
     real(kind=8), dimension( : ), pointer :: Fdyn

     !> number of nodes of the mesh
     integer(kind=4) :: nb_nodes

     !> node to global dof index  map
     !> iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
     integer(kind=4), dimension( : )    , pointer :: ccdof
     !> node and physic index to dof size map
     integer(kind=4), dimension( : , : ), pointer :: ccsize

     !> total number of dofs of a bdyty
     integer(kind=4) :: nbdof

     !> global dof index to node map
     !> inodty = bdyty(ibdyty)%nodnb(iccdof)
     integer(kind=4), dimension( : ), pointer :: nodnb
     !> global dof index to local dof index map
     !> idof = bdyty(ibdyty)%dofnb(iccdof)
     integer(kind=4), dimension( : ), pointer :: dofnb

     !> contact detection configuration (coordinates)
     real(kind=8), dimension( : , : ), pointer :: coorTT
     
     !> number of primal driven dofs
     integer(kind=4) :: nb_primal_driven_dofs
     !> primal driven dofs
     type( T_driven_dof ), dimension( : ), allocatable :: primal_drvdofs
     !> number of dual driven dofs
     integer(kind=4) :: nb_dual_driven_dofs
     !> dual driven dofs
     type( T_driven_dof ), dimension( : ), allocatable :: dual_drvdofs
     ! pour simplifier la vie avec le g_system
     integer(kind=4)     , dimension( : ), pointer     :: drvdofs
     real(kind=8)        , dimension( : ), pointer     :: drvvalues
     

     !INTEGER,DIMENSION(:),POINTER :: periodicnode
     logical :: visible!,is_periodic
    
     !> general system for resolution
     type( G_system ) :: g_sys
     !> storing rigth hand side of the system of equations
     real(kind=8), dimension( : ), pointer :: RHS

     !> mask to compute edge values of dofs (see Mask_P2U of poroMAILx)
     integer(kind=4), dimension( : , : ), allocatable :: mask

     !> Elements visibility. Used for element erosion.
     integer(kind=4), dimension( : ), pointer :: eviz        => null()
     !> Elements energy. Used for element erosion.
     real(kind=8)   , dimension( : ), pointer :: el_energy   => null()
     !> Elements jacobian. Used for element erosion.
     real(kind=8)   , dimension( : ), pointer :: el_jacobian => null()


     integer(kind=4), dimension( : ), pointer :: flying_nodes

  end type T_multiMAILX

end module MAILx_type
