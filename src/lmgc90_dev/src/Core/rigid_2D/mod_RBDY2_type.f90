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


module RBDY2_type

  use a_DOF, only : T_nodty, &
                    T_driven_dof

  implicit none

  private

  ! -----------------------------------------------------------
  ! list of bulk elements blmty selected to construct body type
  ! -----------------------------------------------------------

  !---------------------------------------------------------------------- 
  ! standard bulk element
  type T_PLAIN

     ! average radius       mass = Umass*pi*avr_radius**2
     real(kind=8) :: avr_radius
     ! gyration radius      inertia momentum = mass*sgyr_radius**2
     real(kind=8) :: gyr_radius
     real(kind=8) :: area

     ! total thermal deformation
     real(kind=8), dimension(:)  , pointer :: dilat     => null()
     ! increment of thermal deformation
     real(kind=8), dimension(:)  , pointer :: inc_dilat => null()
     ! coordonnees de reference des vertex
     real(kind=8), dimension(:,:), pointer :: cooref    => null()

     ! 1./demi_longueur_x et 1./demi_longueur_y (du travail de porc)
     real(kind=8) :: iHalfLx
     ! 1./demi_longueur_x et 1./demi_longueur_y (du travail de porc)
     real(kind=8) :: iHalfLy

  end type T_PLAIN
  !----------------------------------------------------------------------- 

  ! generic bulk element type --------------------------------------------
  type T_blmty 

     character(len=5) :: blmID  ! name of bulk element                          
     character(len=5) :: behav  ! nickname of bulk material
     integer          :: lawnb  ! serial number (in BEHAVIOURS.DAT) of law with nickname behav in body;
     type( T_PLAIN )  :: PLAIN  ! standard bulk element

  end type T_blmty

  ! --------------------------------------------------------
  ! list of contactors tacty selected to construct body type
  ! --------------------------------------------------------

  !------------------------------------------------------------------------
  ! TYPE T_BDARY : candidate or antagonist contactor boundary
  ! 
  !              - DATA real data
  !                - DISKx,xKSID : radius  
  !                - JONCx       : ax1,ax2
  !                - POLYG       : coordinates of the vertex
  !                - PT2D        : coor1,coor2 
  !                - OTHER       : NULL
  !              - idata 
  !                - POLYG      : nb of vertices
  !                - OTHER      : NULL
  !              - shift coordinates of the boundary barycenter with respect to solid inertial center
  !              - rdg   equivalent radius
  !              - grdg  gyration radius
  !
  ! add for multiphysics applications
  !              - IdTh     : thermal node identifiant
  !              - WS,WSini : surface energy
  !              - T        : surface temperature
  !              - T        : Thermal conductivity
  !------------------------------------------------------------------------
  type T_BDARY

     real(kind=8)   , dimension( : ), pointer :: data  => null()
     integer(kind=4), dimension( : ), pointer :: idata => null()
     real(kind=8)                             :: area
     real(kind=8)   , dimension( 2 )          :: shift
     real(kind=8)                             :: rdg,grdg  
     !
     integer(kind=4)                          :: IdTh    
     !REAL(kind=8)                            :: WS,WSini
     !mr: 3/5/12
     !The surface energy variable becomes a vector
     real(kind=8), dimension( : ), pointer    :: WS       => null()
     real(kind=8), dimension( : ), pointer    :: WSini    => null()
     real(kind=8), dimension( : ), pointer    :: WStime   => null()
     integer     , dimension( : ), pointer    :: WSstatus => null()

     real(kind=8)                             :: T,TCond,TCondini
     real(kind=8)                             :: PTcond,STcond,Tnx,Tny
     real(kind=8)                             :: alpha_rtheta, alpha_z !jr
     real(kind=8)                             :: Hspe                  !jr
     real(kind=8)                             :: betai
     logical                                  :: BOUNDARY

  end type T_BDARY

  ! generic contactor type ------------------------------------------------

  type T_tacty 

     character(len=5) :: tacID
     character(len=5) :: color
     type( T_BDARY )  :: BDARY  ! standard contactor (the boundary)           

  end type T_tacty

  !------------------------------------------------------------------------  

  ! ------------------------------------
  ! defining the generic body type RBDY2
  ! ------------------------------------

  ! generic body type ----------------------------------------------------- 

  type, public :: T_RBDY2

     character(len=5)                         :: bdyID

     !fd stupid, it can be only one blmty in a RBDY2 !!!

     type( T_blmty ), dimension( : ), pointer :: blmty => null()


     type( T_nodty )                          :: nodty
     type( T_tacty ), dimension( : ), pointer :: tacty => null()

     ! nodes are arrays where dof are stored. Several kinds of nodes 
     ! are used. The specy of each node inodty for each body ibdyty, 
     ! is hiden in bdyty(ibdyty)%nodty
     ! In 2D rigid bodies, specy NO3xx is essentially used. In this specy
     ! three degrees of freedom are stored for each node,
     ! the mechanical meaning of it beeing described below.
     ! In order to avoid calling recursively too many pointers, nodes
     ! data are concatenated in a single list with index iccdof.
     ! It avoids also defining collection of node types since the data are
     ! merely dof datas. 
     ! The dof idofty of node inodty of body ibdyty has index
     ! iccdof = bdyty(ibdyty)%ccdof(inodty)+idof.
     ! In a reverse way, in the body ibdyty, the index iccdof corresponds
     ! to node inodty = bdyty(ibdyty)%nodnb(iccdof) and 
     ! to dof    idof = bdyty(ibdyty)%dofnb(iccdof) .
     ! The comments below stand for idof running from 1 to 3.

     ! The symbols nodnb and dofnb are also used in T_vlocy_driven and
     ! T_force_driven, with similar meanings.


     !o   When a body owns several nodes, the variables to be used are:
     !o   integer                         ::  nodnb        ! inodty = bdyty(ibdyty)%nodnb(iccdof)
     !o   integer ,dimension(:),pointer   ::  dofnb        !   idof = bdyty(ibdyty)%dofnb(iccdof)
     !o   integer ,dimension(:),pointer   ::  ccdof        ! iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
     !o   Rigid bodies own a single node, thus  nodnb=1 dofnb=1 ccdof=0, and the above variables are superfluous.


     !> cooref(1),cooref(2),        body center cordinates 
     !>                             in reference configuration;
     !> cooref(3),                  angular value of body
     !>                             in reference configuration;
     real(kind=8), dimension( : ), pointer :: cooref => null()
     real(kind=8), dimension( : ), pointer :: coor   => null()

     ! Vbegin(1),Vbegin(2),        velocity of the center
     !                             at beginning of time step;
     ! Vbegin(3),                  angular velocity
     !                             at beginning of time step;
     ! V(1),V(2),                  velocity of the center
     !                             while iterating; 
     ! V(3),                       angular velocity
     !                             while iterating;
     ! values V(1),V(2),V(3), are set to Vbegin(1),Vbegin(2),Vbegin(3),
     ! at first step iteration, see INCREMENT;        
     real(kind=8), dimension( : ), pointer :: Vbegin => null()
     real(kind=8), dimension( : ), pointer :: V      => null()

     ! Xbegin(1),Xbegin(2),        displacement of the center 
     !                             at beginning of time step
     !                             with respect to reference coordinates;
     ! Xbegin(3),                  angular change
     !                             at beginning of time step 
     !                             with respect to reference rotation; 
     ! X(1),X(2),                  displacement of the center
     !                             while iterating 
     !                             with respect to reference coordinates;
     ! X(3),                       angular change
     !                             while iterating
     !                             with respect to reference rotation;
     ! values X(1),X(2),X(3), are set to 
     ! Xbegin(1)+H*Vbegin(1),Xbegin(2)+H*Vbegin(2),Xbegin(3)+H*Vbegin(3),
     ! at first step iteration, allowing a single theta method iteration 
     ! constant linear systems, see INCREMENT;
     real(kind=8), dimension( : ), pointer :: Xbegin => null()
     real(kind=8), dimension( : ), pointer :: X      => null()

     ! Vfree(1),Vfree(2),          free velocity of the center 
     !                             no reaction acting;
     ! Vfree(3),                   free angular velocity
     !                             no reaction acting;  
     ! Vaux(1),Vaux(2),            velocity of the center
     !                             provisional vector;
     ! Vaux(3),                    angular velocity
     !                             provisional vector;     
     real(kind=8), dimension( : ), pointer :: Vfree => null()
     real(kind=8), dimension( : ), pointer :: Vaux  => null()

     ! Fext(1),Fext(2),            external force
     !                             applied to the center;
     ! Fext(3),                    external momentum
     !                             applied to the center;
     ! Fint(1),Fint(2),            internal force
     !                             applied to the center;
     ! Fext(3),                    internal momentum
     !                             applied to the center;
     real(kind=8), dimension( : ), pointer :: Fext => null()
     real(kind=8), dimension( : ), pointer :: Fint => null()

     ! Ireac(1),Ireac(2),          resulting reaction impulses
     !                             applied to the center;
     ! Ireac(3),                   resulting reaction impulse momentum
     !                             applied to the center;
     ! Iaux(1),Iaux(2),            storage resulting reaction impulses
     !                             applied to the center;
     ! Iaux(3),                    storage resulting reaction impulse
     !                             momentum applied to the center;
     real(kind=8), dimension( : ), pointer :: Ireac => null()
     real(kind=8), dimension( : ), pointer :: Iaux  => null()

     ! mass(1),mass(2),            mass of body;
     ! mass(3),                    inertia momentum;
     real(kind=8), dimension( : ), pointer :: mass => null()

     ! inv_mass(1),inv_mass(2),    inverse of mass;
     ! inv_mass(3),                inverse of inertia momentum; 
     real(kind=8), dimension( : ), pointer :: inv_mass => null()

     ! imposed velocities
     type( T_driven_dof ), dimension( : ), pointer :: vlocy_driven_dof => null()
     ! imposed forces
     type( T_driven_dof ), dimension( : ), pointer :: force_driven_dof => null()
     ! total number of driven velocities
     integer                                       :: nb_vlocy_driven_dof
     ! total number of driven forces
     integer                                       :: nb_force_driven_dof
     ! values of driven velocities and displacement velocities
     real(kind=8), dimension( : ), pointer         :: Vdriv => null()
     ! values of driven velocities and displacement velocities
     real(kind=8), dimension( : ), pointer         :: Xdriv => null()
     ! values of driven forces
     real(kind=8), dimension( : ), pointer         :: Fdriv => null()
     logical                                       :: visible
     logical                                       :: r2m
!!! > md,dem > !!!
     real(kind=8), dimension( : ), pointer :: Abegin => null()
     real(kind=8), dimension( : ), pointer :: A      => null()
     real(kind=8), dimension( : ), pointer :: Bbegin => null()
     real(kind=8), dimension( : ), pointer :: B      => null()
     real(kind=8), dimension( : ), pointer :: Cbegin => null()
     real(kind=8), dimension( : ), pointer :: C      => null()
!!! < md,dem < !!!

     real(kind=8) :: T
     integer      :: periode ! 0 unchanged
                             ! 1 from right to left
                             !-1 from left to right
     real(kind=8) :: EPot
     real(kind=8) :: ECond
     real(kind=8) :: ECondini
     real(kind=8) :: ECur
     real(kind=8) :: Talpha

     ! Equivalent stress tensor
     real(kind=8), dimension( 2, 2 ) :: SIGMA

  end type T_RBDY2

end module RBDY2_type
