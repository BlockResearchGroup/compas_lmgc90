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

module RBDY3_type

  use a_DOF, only : T_nodty, &
                    T_driven_dof

  implicit none

  private

  ! -------------------------------------------------------------
  ! T_PLAIN - standard bulk element
  !
  !  list of bulk elements blmty selected to construct body type
  !
  !  avr_radius : mass = Umass*pi*avr_radius**2
  !  I1,I2,I3   : inertia terms in principal inertial frame    
  !  volume     :   
  ! -------------------------------------------------------------

  type T_PLAIN
     real(kind=8) :: avr_radius
     real(kind=8) :: volume
     real(kind=8) :: I1
     real(kind=8) :: I2
     real(kind=8) :: I3
  end type T_PLAIN

  ! -------------------------------------------------------------
  ! T_blmty - generic bulk element type 
  !
  !  blmID : name of bulk element 
  !  behav : nickname of bulk material
  !  lawnb : serial number (in BEHAVIOURS.DAT) of law with nickname behav in body
  !  PLAIN : standard bulk element 
  ! -------------------------------------------------------------

  type T_blmty 
     character(len=5) :: blmID
     character(len=5) :: behav
     integer          :: lawnb
     type( T_PLAIN )  :: PLAIN
  end type T_blmty
  
  ! --------------------------------------------------------
  ! T_BDARY - candidate or antagonist contactor boundary
  !  list of contactors tacty selected to construct body type
  !

  ! --------------------------------------------------------
  
  type T_BDARY
     !>  idata : anonymous real data space necessary to define the boundary
     integer     , dimension( : ), pointer :: idata => null()
     !>  DATA  : anonymous real data space necessary to define the boundary
     real(kind=8), dimension( : ), pointer :: DATA => null()
     !>  shift : distance from a contactor center to the center of inertia  
     real(kind=8), dimension( 3 )          :: shift
     !> volume: volume of the contactor ; given by read_BDARY_xxx 
     real(kind=8)                          :: volume
     !> principal inertia               ; given by read_BDARY_xxx
     real(kind=8)                          :: I1
     real(kind=8)                          :: I2
     real(kind=8)                          :: I3
     !> average radius
     real(kind=8)                          :: rdg
     !> inertia frame of the contactor with respect to the rbdy3 local frame
     !> Is identity except for PLANx (?)
     real(kind=8), dimension( 3, 3 )       :: EmbededFrame

     ! for multiphysics applications
     real(kind=8)                          :: WS        ! Surface energy
     real(kind=8)                          :: WSini     ! Surface energy
     real(kind=8)                          :: TCond     ! Thermal and Electrical conductivity
     real(kind=8)                          :: ECond     ! Thermal and Electrical conductivity
     real(kind=8)                          :: TCondini  ! Thermal and Electrical conductivity
     real(kind=8)                          :: ECondini  ! Thermal and Electrical conductivity
     real(kind=8)                          :: T
     real(kind=8)                          :: EPot

     logical                               :: BOUNDARY

  end type T_BDARY

  ! --------------------------------------------------------
  ! T_tacty - generic contactor type
  !
  !  tacID :
  !  color :
  !  BDARY : standard contactor (the boundary) 
  ! --------------------------------------------------------

  type T_tacty  

     integer          :: tacID
     character(len=5) :: color
     type( T_BDARY )  :: BDARY

  end type T_tacty

  ! --------------------------------------------------------
  ! T_RBDY3 - generic body type
  !
  ! nodes are arrays where dof are stored. Several kinds of nodes 
  ! are used. The specy of each node inodty for each body ibdyty, 
  ! is hiden in bdyty(ibdyty)%nodty.
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
  ! T_force_driven, with similar meanings.  !
  !
  ! cooref   : body center cordinates 
  ! Vbegin   : Vbegin(1:3) velocity of the center at beginning of time step
  !            Vbegin(4:6) angular velocity at beginning of time step
  ! V        : V(1:3) velocity of the center while iterating
  !            V(4:6) angular velocity while iterating
  ! Xbegin   : Xbegin(1:3) displacement of the center at beginning of time step
  ! X        : X(1:3) displacement of the center while iterating with respect to reference coordinates
  !            X is set to Xbegin + H*THETA*Vbegin
  ! Vfree    : provisional vector no reaction acting
  !            Vfree(4:6) free angular velocity no reaction acting 		 
  ! Vaux     : Vaux(1:3) velocity of the center provisional vector
  !            Vaux(4:6) angular velocity
  ! Fext(1),Fext(2),Fext(3),       external force
  !                                applied to the center;
  ! Fext(4),Fext(5),Fext(6),       external momentum
  !                                applied to the center;
  ! Fint(1),Fint(2),Fint(3),       internal force
  !                                applied to the center;
  ! Fext(4),Fint(5),Fint(6)        internal momentum
  ! Ireac(1),Ireac(2),Ireac(3),    resulting reaction impulses
  !                                applied to the center;
  ! Ireac(4),Ireac(5),IReac(6),    resulting reaction impulses momentum
  !                                applied to the center;
  ! Iaux(1),Iaux(2),Iaux(3),       storage resulting reaction impulses
  !                                applied to the center;
  ! Iaux(4),Iaux(5),Iaux(6),       storage resulting reaction impulses momentum

  type, public :: T_RBDY3 
     
     character(len=5)                         :: bdyID
     
     type( T_blmty ), dimension( : ), pointer :: blmty => null()
     type( T_nodty )                          :: nodty
     type( T_tacty ), dimension( : ), pointer :: tacty => null()

     !fd map qui ce corps renvoie une liste (type de contacteur, rang dans le tableau de contacteur de ce type)  
     !todo: mettre cette liste dans le module entity
     integer, dimension( : , : ), pointer :: bdyty2tacty => null()

     real(kind=8), dimension( 6 ) :: cooref
     real(kind=8), dimension( 6 ) :: Vbegin
     real(kind=8), dimension( 6 ) :: V
     real(kind=8), dimension( 6 ) :: Xbegin
     real(kind=8), dimension( 6 ) :: X
     real(kind=8), dimension( 6 ) :: Vfree
     real(kind=8), dimension( 6 ) :: Vaux
     real(kind=8), dimension( 6 ) :: Fext
     real(kind=8), dimension( 6 ) :: Fint
     real(kind=8), dimension( 6 ) :: Ireac
     real(kind=8), dimension( 6 ) :: Iaux

     real(kind=8), dimension( 6 ) :: mass
!     real(kind=8),dimension(3)   :: I   
!     real(kind=8),dimension(3)   :: inv_I
     real(kind=8), dimension( 6 ) :: inv_mass
     
     real(kind=8), dimension( 3, 3 ) :: LocalFrameIni
     real(kind=8), dimension( 3, 3 ) :: LocalFrameRef

     real(kind=8), dimension( 3, 3 ) :: LocalFrame

     !fd detection configuration
     real(kind=8), dimension( 3 )    :: coorTT
     real(kind=8), dimension( 3, 3 ) :: LocalFrameTT

     real(kind=8) :: area

     type( T_driven_dof ), dimension( : ), pointer :: vlocy_driven_dof => null()  ! imposed velocities
     type( T_driven_dof ), dimension( : ), pointer :: force_driven_dof => null()  ! imposed forces
     integer                                       :: nb_vlocy_driven_dof   ! total number of driven velocities        
     integer                                       :: nb_force_driven_dof   ! total number of driven forces
     real(kind=8), dimension( : ), pointer :: Vdriv => null()  ! values of driven velocities and displacement velocities
     real(kind=8), dimension( : ), pointer :: Xdriv => null()  ! values of driven velocities and displacement velocities
     real(kind=8), dimension( : ), pointer :: Fdriv => null()                 ! values of driven forces
     logical                               :: visible 
     integer                               :: visibleID             ! PTA: map pour la gestion de la renumerotation des corps
                                                                    ! dans le cas de l'utilisation des mecanismes d'invisibilite...
  
     real(kind=8), dimension( 6 ) :: Abegin
     real(kind=8), dimension( 6 ) :: A
     real(kind=8), dimension( 6 ) :: Bbegin
     real(kind=8), dimension( 6 ) :: B
     real(kind=8), dimension( 6 ) :: Cbegin
     real(kind=8), dimension( 6 ) :: C

     integer :: xperiode
     integer :: yperiode    

     !pb
     integer :: thread

     real(kind=8) :: T
     real(kind=8) :: Talpha

  end type T_RBDY3

  !------------------------------------------------------------------------ 

end module RBDY3_type
