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
MODULE postpro
  
  use overall, only : H, TPS, NSTEP    , &
                      nbDIME, PI_g     , &
                      max_internal_tact, &
                      location, in_post, &
                      post_processing  , &
                      nlgs_solver2D    , &
                      cpg_solver       , &
                      delete_part_of_file


  USE utilities
  USE tact_behaviour
  use parameters
  USE BULK_BEHAVIOUR
  
  USE nlgs ! resolution
  USE cpg

  USE MP_SOLVER
  
  USE RBDY2
  USE DISKx, only : DISKx2bdyty , &
                    get_nb_DISKx, &
                    get_radius_DISKx
  USE JONCx, only : JONCx2bdyty , &
                    get_nb_JONCx
  USE PT2Dx, only : PT2Dx2bdyty , &
                    get_nb_PT2Dx
  USE POLYG, only : T_polyg     , &
                    POLYG2bdyty , &
                    get_nb_POLYG, &
                    get_l_POLYG
  USE xKSID, only : xKSID2bdyty , &
                    get_nb_xKSID, &
                    get_radius_xKSID
  USE CLxxx
  USE ALpxx

  USE MAILx                                                               
  USE mecaMAILx

  use inter_meca_handler_2D, only : get_nb_verlets        , &
                                    get_rloc              , &
                                    get_vloc              , &
                                    get_vlocBEGIN         , &
                                    get_gaps              , & ! beurk
                                    this2verlet           , &
                                    get_verlet_size       , &
                                    get_verlet_adjsz      , &
                                    get_verlet_iantac     , &
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
                                    get_nb_inters

  USE GEOMPACK

  IMPLICIT NONE
  
  PRIVATE
  

!!! The number of command is a global variable and must be changed here

  INTEGER :: NB_COMMANDS = 40

!!! pseudo hash_map 

  INTEGER,PARAMETER :: &
       i_AVERAGE_VELOCITY_EVOLUTION  =  1 , &
       i_SPECIES_KINETIC_ENERGY      =  2 , &
       i_SNAPSHOT_SAMPLE             =  3 , &
       i_MP_SNAPSHOT_SAMPLE          =  4 , &
       i_NORMAL_CONTACT_DISTRIBUTION =  5 , &
       i_CONTACT_FORCE_DISTRIBUTION  =  6 , &
       i_COORDINATION_NUMBER         =  7 , &
       i_BODY_TRACKING               =  8 , &
       i_MP_VALUE_TRACKING           =  9 , &
       i_TORQUE_EVOLUTION            = 10 , &
       i_DENSE_SAMPLE_COMPACITY      = 11 , & ! 2D ONLY - voir DISPLAY TENSORS
       i_DOUBLET_INTERACTIONS        = 12 , &
       i_3D_EXTRUSION                = 13 , & ! 2D ONLY
       i_QUASI_SLIDING_CONTACT       = 14 , &
       i_NEW_MECAx_SETS              = 15 , &
       i_Fint_EVOLUTION              = 16 , &
       i_Dep_EVOLUTION               = 17 , &
       i_NEW_DIST_SETS               = 18 , &
       i_MAILx_DIST                  = 19 , &
       i_NEW_RIGID_SETS              = 20 , &
       i_NEW_BOUNDED_SETS            = 21 , &
       i_DISSIPATED_ENERGY           = 22 , &
       i_SOLVER_INFORMATIONS         = 23 , &
       i_DRY_CONTACT_NATURE          = 24 , &
       i_WET_CONTACT_NATURE          = 25 , &
       i_KINETIC_ENERGY              = 26 , &
       i_VIOLATION_EVOLUTION         = 27 , &
       i_PLPLx_ANALYSIS              = 28 , & ! 2D ONLY
       i_THERMAL_EVOLUTION           = 29 , &
       i_NETWORK_EVOLUTION           = 30 , &
       i_CREATE_TEXT_DATA            = 31 , & ! 2D ONLY
       i_ELECTRO_EVOLUTION           = 32 , &
       i_CLOUD_ANALYSIS              = 33 , & ! NOT YET USED
       i_DISPLAY_TENSORS             = 34 , & ! NOT YET USED
       i_COMPACITY_EVOLUTION         = 35 , & ! TO MERGE WITH DENSE_SAMPLE_COMPACITY
       i_HEAT_BOUND_PROFILE          = 36 , &
       i_CLxxx_ANALYSIS              = 37 , &
       i_SNAP_SURFACE_ENERGIE_SECTOR = 38 , & ! MR&VHN GET SECTOR ENERGIE SURFACE
       i_CZM_ENERGY_EVOLUTION        = 39 , & ! JR : GET CZM ENERGY
       i_ENERGY_SNAPSHOT_SAMPLE      = 40
!!!*

  !------------------------------------------------------------------------
  TYPE T_identity_command
     ! name of the command     
     CHARACTER(len=30) :: name
     ! true if the command is used (declared POSTPRO.DAT file)     
     LOGICAL           :: used
     ! execution frequency     
     INTEGER           :: step
     ! number of files managed by the command     
     INTEGER           :: nb_files
     ! list of the io_unit related to the files managed by the command           
     INTEGER,DIMENSION(:),pointer :: io_unit => null()
     
  END TYPE T_identity_command
  
  TYPE(T_identity_command),DIMENSION(:),ALLOCATABLE :: PostCommand
  
!!!--------------------------------------------------------------------------------------------------------
!!! Module variables for each command used.
!!! If needed you can add your own variables at the end of the following.
!!!
!!! WARNING: some names of files are changing each time they are written,
!!!          the file index are managed as module variables to permit clean memory
!!!          to reset them if multiple run are done  
!!!--------------------------------------------------------------------------------------------------------

!!! average velocity evolution --------------------

  CHARACTER(len=5) :: AVcolor

!!! bodies tracking -------------------------------
  
  INTEGER                          :: nb_tracking
  INTEGER,DIMENSION(:),ALLOCATABLE :: TrackingID

!!! mp value tracking -------------------------------
  
  INTEGER                          :: nb_mp_tracking
  INTEGER,DIMENSION(:),ALLOCATABLE :: bMPTrackingID,tMPTrackingID

!!! compacity evolution ---------------------------

  CHARACTER(len=5)             :: BodyBehav
  CHARACTER(len=12)            :: CMPCT_MODEL
  INTEGER                      :: ID_LEFTx,ID_RIGHT,ID_DOWNx,ID_UPxxx
  INTEGER                      :: ID_INTER,ID_EXTER
  INTEGER                      :: compacity_model
  INTEGER,PARAMETER            :: icmp_smooth_box=0,  &
                                  icmp_rough_box  =1, &
                                  icmp_couette    =2, &
                                  icmp_cluster_box=3, &
                                  icmp_selection  =4

  ! dense compacity managing interior object
  integer                      :: i_inter, interID
  
  ! TYPE T_identity_BODY
     
  !    INTEGER          :: number
  !    CHARACTER(len=5) :: TYPE
     
  ! END TYPE T_identity_BODY
  
  ! TYPE(T_identity_BODY)        :: R_body,L_body,D_body


!!! contact forces distribution -------------------
  
  INTEGER                               :: Fsect
  
  ! global since allocated once
  INTEGER,DIMENSION(:),ALLOCATABLE      :: Fnumber,FNnumber
  
  ! file index
  INTEGER                               :: cfd_unit = 0
  
  !                                1234567890123456789012345678901234567890123456
  CHARACTER(len=46) :: cfd_name1 ! POSTPRO/CONTACT_FORCE_DISTRIBUTION_0000000.DAT  

!!! coordination number ---------------------------

  ! global since allocated once
  INTEGER,DIMENSION(:),ALLOCATABLE :: cplus,cmoins,ctotal
  
!!! dissipated energy -----------------------------
  
  REAL(kind=8) :: dissipated_energy = 0.D0
  
!!! doublet interactions --------------------------
  
  INTEGER,DIMENSION(2)             :: doubletbdyty
  CHARACTER(len=5)                 :: doublet_type

!!! species kinetic energy ------------------------

  INTEGER :: nb_species

  TYPE SPECIES
     REAL(kind=8)     :: KE
     REAL(kind=8)     :: DE
     REAL(kind=8)     :: P
     REAL(kind=8)     :: PE
     CHARACTER(len=5) :: behav
  END TYPE SPECIES

  TYPE(SPECIES),DIMENSION(:),ALLOCATABLE  :: SKE

!!! normal contact distribution -------------------

  ! file index
  INTEGER :: ncd_unit = 0
  
  INTEGER                                 :: Nsect,DNsect
  
  ! global since allocated once
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: C_ORI,SC_ORI,WC_ORI
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: T_ORI,ST_ORI,WT_ORI

  !                                132456789013245678901234567890123456789
  CHARACTER(len=39) :: ncd_name1 ! POSTPRO/NORMAL_CONTACT_DIST_0000000.DAT
  CHARACTER(len=27) :: ncd_name2 ! POSTPRO/P2THETA_0000000.DAT 
  
!!! snapshot sample -------------------------------
  
  CHARACTER(len=33)                       :: body_snap_name
  CHARACTER(len=36)                       :: contact_snap_name
  INTEGER                                 :: snap_unit=0
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: body_data_snapshot

!!! energy snapshot sample -------------------------------
  
  CHARACTER(len=35)                       :: energy_snap_name
  ! file index
  INTEGER                                 :: esnap_unit=0

!!! torque evolution ------------------------------
  
  INTEGER                          :: nb_torque
  INTEGER,DIMENSION(:),ALLOCATABLE :: TorqueID 
  
!!! 3D EXTRUSION    --------------------------------
  
  REAL(kind=8) :: deep,factor
  
!!! EVOLUTION CONTACT RT~muRN    --------------------------------
  
  REAL(kind=8) :: fraction

!!!  NEW RIGID SETS ---------------------------------

  TYPE T_rigid_set
     INTEGER :: size
     INTEGER,DIMENSION(:),POINTER :: list
  END TYPE T_rigid_set

  TYPE(T_rigid_set), DIMENSION(:), ALLOCATABLE :: RBDY2_set
  INTEGER :: nb_RBDY2_sets

!!!  NEW BOUNDED SETS ---------------------------------

  TYPE T_bounded_set
     INTEGER :: idmin,idmax
  END TYPE T_bounded_set

  TYPE(T_bounded_set), DIMENSION(:), ALLOCATABLE :: bounded_set
  INTEGER :: nb_bounded_sets

!!! CLxxx set
  integer :: nb_clxxx_set
  integer,dimension(:,:),allocatable:: clxxx_set

!!! SURFACE_ENERGIE SETS MR&VHN
  
  !TYPE T_sector_set
  !   INTEGER :: WSstatus
  !   REAL(kind=8) :: WS,WStime
  !END TYPE T_sector_set
  !INTEGER,DIMENSION(:),ALLOCATABLE :: SectorID 
  !TYPE(T_sector_set), DIMENSION(:,:), ALLOCATABLE :: sector_set
  INTEGER :: nb_WS,ws_unit=0,nb_WSsect
  CHARACTER(len=20)                :: ws_name
  
!!!-------------------------------------------------

  REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: HEATBOUNDS
  REAL(kind=8), DIMENSION(:), POINTER       :: HEATvector

  INTEGER :: HB_NY,nb_HB
  INTEGER :: hb_unit = 0

  !-------------------------------------12345678901234567890123456789012345678
  CHARACTER(len=38) :: heat_bound_name='POSTPRO/HEAT_BOUND_PROFILE_0000000.DAT'

  !-------------------------------------------------

  INTEGER      :: i_window = 0,nb_INSIDE=0
  REAL(kind=8) :: WdwX,WdwY,X0,Y0,WdwR,WdwR2,TransX,TransY
  LOGICAL      :: Winside =.FALSE.
  REAL(kind=8) :: marea,WdwX0,WdwY0

  LOGICAL,DIMENSION(:),ALLOCATABLE        :: BodyWindow
    
  !-------------------------------------------------
  
  INTEGER           :: nb_DISKx,nb_JONCx,nb_PT2Dx,nb_xKSID,nb_POLYG,nb_GRAIN, &
                       nb_CLxxx,nb_ALpxx
  INTEGER           :: nb_DKDKx,nb_DKJCx,nb_DKKDx,nb_PLPLx,nb_PLJCx, &
                       nb_DKPLx,nb_CLJCx,nb_CLALp,nb_DKALp
  INTEGER           :: nb_MAILx,nb_RBDY2
  !-------------------------------------------------

  REAL(kind=8), parameter :: dpi = 2.d0*PI_g
  
  !-----------bidouillages pour les corps mailles----
    
  INTEGER nb_mecax_sets,nb_dist_sets
  
  TYPE T_MECAx
     INTEGER                            :: iMECAx,nb_nodes
     INTEGER, DIMENSION(:),POINTER      :: nodes
  END TYPE T_MECAx
  
  TYPE T_set
     INTEGER                            :: nb_MECAx
     TYPE(T_MECAx),DIMENSION(:),POINTER :: DATA
  END TYPE T_set
  
  TYPE(T_set), DIMENSION(:), ALLOCATABLE :: MECAx_set
  
  TYPE(T_set), DIMENSION(:), ALLOCATABLE :: DIST_set
  
  !------- fin bidouillage ----

  real(kind=8) :: tol_rn =1d-5

  
  PUBLIC &
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &
       postpro_during_computation, &
       flush_during_computation  , &
       compute_kinetic_energy

  PUBLIC &
       circular_selection_postpro, &
       selection_translation_postpro

  public clean_memory_postpro

CONTAINS

!!!---------------------------------------------------------------------------------------
!!! If you introduce new command, scratch the old message and put a little
!!! description of your add
!!!---------------------------------------------------------------------------------------
  SUBROUTINE messages_for_users

    IMPLICIT NONE

    CALL LOGMES('  @ POSTPRO v2:')
    CALL LOGMES('  @ Refer to User Guide for more details')
    CALL LOGMES('  @               * * * ')

  END SUBROUTINE messages_for_users
  !!!-----------------------------------------------------------------------
  
  function init_postpro_command()
    
    !!****u* CORE.POSTPRO/init_postpro_command
    !! NAME
    !!  subroutine init_postpro_command
    !! PURPOSE
    !!  Increment the list of command and define their attributes
    !!  When a new command is add, you must define:
    !!
    !!  1 - increment the parameter NB_COMMANDS
    !!  2 - add the name and all information after the last previous command
    !!****
    implicit none
    integer :: init_postpro_command

    integer :: inum, err
    character(len=80) :: cout

    IF (ALLOCATED(PostCommand)) DEALLOCATE(PostCommand)
    ALLOCATE(PostCommand(NB_COMMANDS))
    
    DO inum = 1,NB_COMMANDS
       NULLIFY(PostCommand(inum)%io_unit)
       PostCommand(inum)%used     = .FALSE.
       PostCommand(inum)%nb_files  = 0
    END DO
    
    inum = 0

    !#01#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_AVERAGE_VELOCITY_EVOLUTION)%name  = 'AVERAGE VELOCITY EVOLUTION    '

    !#02#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_SPECIES_KINETIC_ENERGY)%name      = 'SPECIES KINETIC ENERGY        '

    !#03#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_SNAPSHOT_SAMPLE)%name             = 'SNAPSHOT SAMPLE               '

    !#04#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_MP_SNAPSHOT_SAMPLE)%name          = 'MP SNAPSHOT SAMPLE            '

    !#05#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NORMAL_CONTACT_DISTRIBUTION)%name = 'NORMAL CONTACT DISTRIBUTION   '

    !#06#-------------------------------------------------------
    ! 
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_CONTACT_FORCE_DISTRIBUTION)%name  = 'CONTACT FORCE DISTRIBUTION    '

    !#07#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_COORDINATION_NUMBER)%name         = 'COORDINATION NUMBER           '

    !#08#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_BODY_TRACKING)%name               = 'BODY TRACKING                 '

    !#09#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_MP_VALUE_TRACKING)%name           = 'MP VALUE TRACKING             '

    !#10#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_TORQUE_EVOLUTION)%name            = 'TORQUE EVOLUTION              '

    !#11#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DENSE_SAMPLE_COMPACITY)%name      = 'DENSE SAMPLE COMPACITY        '

    !#12#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DOUBLET_INTERACTIONS)%name        = 'DOUBLET INTERACTIONS          '

    !#13#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_3D_EXTRUSION)%name                = '3D EXTRUSION                  '

    !#14#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_QUASI_SLIDING_CONTACT)%name       = 'QUASI SLIDING CONTACT         '

    !#15#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_MECAx_SETS)%name              = 'NEW MECAx SETS                '

    !#16#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_Fint_EVOLUTION)%name              = 'Fint EVOLUTION                '

    !#17#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_Dep_EVOLUTION)%name               = 'Dep EVOLUTION                 '
 
    !#18#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_DIST_SETS)%name               = 'NEW DIST SETS                '
 
    !#19#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_MAILx_DIST)%name                  = 'MAILx DIST                    '

    !#20#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_RIGID_SETS)%name              = 'NEW RIGID SETS                '

    !#21#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_BOUNDED_SETS)%name            = 'NEW BOUNDED SETS              '

    !#22#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DISSIPATED_ENERGY)%name           = 'DISSIPATED ENERGY             '

    !#23#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_SOLVER_INFORMATIONS)%name         = 'SOLVER INFORMATIONS           '

    !#24#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DRY_CONTACT_NATURE)%name          = 'DRY CONTACT NATURE            '

    !#25#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_WET_CONTACT_NATURE)%name          = 'WET CONTACT NATURE            '

    !#26#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_KINETIC_ENERGY)%name              = 'KINETIC ENERGY                '

    !#27#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_VIOLATION_EVOLUTION)%name         = 'VIOLATION EVOLUTION           '

    !#28#-------------------------------------------------------
    ! 
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_PLPLx_ANALYSIS)%name              = 'PLPLx ANALYSIS                '
    
    !#29#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_THERMAL_EVOLUTION)%name           = 'THERMAL EVOLUTION             '

    !#30#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NETWORK_EVOLUTION)%name           = 'NETWORK EVOLUTION             '

    !#31#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_CREATE_TEXT_DATA)%name            = 'CREATE TEXT DATA              '
    
    !#32#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_ELECTRO_EVOLUTION)%name           = 'ELECTRO EVOLUTION             '
 
    !#33#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_CLOUD_ANALYSIS)%name              = 'CLOUD ANALYSIS                '
 
    !#34#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DISPLAY_TENSORS)%name             = 'DISPLAY TENSORS               '

    !#35#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_COMPACITY_EVOLUTION)%name         = 'COMPACITY EVOLUTION           '

    !#36#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_HEAT_BOUND_PROFILE)%name          = 'HEAT BOUND PROFILE            '

    !#37#-------------------------------------------------------
    inum=inum+1
    !                                                 123456789012345678901234567890
    PostCommand(i_CLxxx_ANALYSIS)%name             = 'CLxxx ANALYSIS                '

    !#38#-------------------------------------------------------
    !MR&VHN
    inum=inum+1
    !                                                 123456789012345678901234567890
    PostCommand(i_SNAP_SURFACE_ENERGIE_SECTOR)%name= 'SURFACE ENERGIE SECTOR        '
    !####-------------------------------------------------------

    !#39#-------------------------------------------------------
    !JR
    inum=inum+1
    !                                                 123456789012345678901234567890
    PostCommand(i_CZM_ENERGY_EVOLUTION)%name        = 'CZM ENERGY EVOLUTION          '
    !#40#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_ENERGY_SNAPSHOT_SAMPLE)%name      = 'ENERGY SNAPSHOT SAMPLE        '
    !####-------------------------------------------------------

    IF (inum .NE. NB_COMMANDS) THEN
       write(cout,'(A)') 'There is some internal mismatch in mod_postpro'
       call logmes(cout)
       write(cout,'(A)') 'The number of command does not fit the existing number of command'
       call faterr('postpro_2D::init_postpro_command',cout)
    END IF

    
    ! file closed in start_postpro_command
    init_postpro_command = get_io_unit()
    open( unit=init_postpro_command, file=trim(location(in_post)), &
          status='OLD', iostat=err )
    
    if ( err > 0 ) then
       call faterr('postpro_2D::init_postpro_command','file POSTPRO.DAT cannot be found')
    end if

  end function init_postpro_command
  !!!-----------------------------------------------------------------------

  subroutine start_postpro(nfich, restart)
    !!****u* CORE.POSTPRO/start_postpro
    !! NAME
    !!  subroutine start_postpro
    !! PURPOSE
    !!  Reading file POSTPRO.DAT to size the command structure.
    !!  Determine if the commands are kwown, read command parameters and 
    !!  determine if the command must be run before, during or after computation 
    !! USES
    !!  POSTPRO/reading_data
    !!  POSTPRO/initialize_selection
    !!****
    implicit none
    integer, intent(in) :: nfich
    integer, intent(in) :: restart
    !
    integer             :: i, j, k, l
    integer             :: nfich_loc, clin_test, err
    character(len=30)   :: clin
    character(len=80)   :: cout

    character(len=22), parameter :: IAM='postpro::start_postpro'

    !rm: adds his boarfitude
    cfd_unit   = max(0,restart-1)
    ncd_unit   = max(0,restart-1)
    snap_unit  = max(0,restart-1)
    esnap_unit = max(0,restart-1)
    hb_unit    = max(0,restart-1)
    ws_unit    = max(0,restart-1)

    nb_RBDY2 = get_nb_RBDY2()
    nb_MAILx = get_nb_MAILx()

    nb_DISKx = get_nb_DISKx()
    nb_JONCx = get_nb_JONCx()
    nb_xKSID = get_nb_xKSID()
    nb_PT2Dx = get_nb_PT2Dx()
    nb_POLYG = get_nb_POLYG()
    nb_CLxxx = get_nb_CLxxx()
    nb_ALpxx = get_nb_ALpxx()

    nb_GRAIN = nb_DISKx+nb_POLYG

    !!! Check if problem occur during opening file ---------------------

    nfich_loc = get_io_unit()

    OPEN(UNIT=nfich_loc,FILE=trim(location('POSTPRO/POSTPRO.OUT')),STATUS='REPLACE',IOSTAT=err)
    
    IF (err.GT.0) THEN
       CALL FATERR(IAM,'POSTPRO directory not found')
    END IF
    
    CLOSE(nfich_loc)

    !!! start reading POSTPRO.DAT file ----------------------------------
    
    DO
       clin_test=0 

       READ(nfich,'(A30)',iostat=err) clin

       IF (err.GT.0) THEN
          CALL FATERR(IAM,'error during reading of file POSTPRO.DAT')
       END IF

       IF (INDEX(clin,'END').EQ.1) EXIT

       DO i=1,SIZE(PostCommand)

          IF ( PostCommand(i)%name .NE. CLIN ) CYCLE
             
          PostCommand(i)%used = .TRUE.
          PostCommand(i)%step = 0

          clin_test=1
             
          READ(nfich,'(A30)',iostat=err) CLIN
          IF (err.GT.0) THEN
             CALL FATERR(IAM,'error during reading of file POSTPRO.DAT')
          END IF
          IF (CLIN(1:4) .NE.'STEP') THEN
             WRITE(cout,'(A38,A30)') '@ Keyword STEP is missing for command ',PostCommand(i)%name
             call faterr('postpro_2D::start_postpro',cout)
          END IF
          
          
          READ(CLIN(5:30),*) PostCommand(i)%step

          call reading_data(i, nfich, restart)
          
          EXIT

       END DO

       IF (INDEX(clin,'END').EQ.1) EXIT

       IF (clin_test.EQ.0) THEN
          IF ((clin(1:4).NE.'STEP') .AND.(clin(1:1).NE.'#')) THEN
             WRITE(cout,'(A11,A30,A16)') ' @ Command ',clin,' does not exist'
             CALL LOGMES(cout)
             CALL LOGMES(' @ or does not match with the command names below:')
             CALL LOGMES(' ')
             DO i=1,SIZE(PostCommand)
                WRITE(cout,'(A5,i2,A2,A30)') ' @ n°',i,': ',PostCommand(i)%name
                CALL LOGMES(cout)
             END DO
             call faterr('postpro_2D::start_postpro','read log')
          END IF
       END IF
    END DO
    
    ! file opened in init_postpro_command
    CLOSE(nfich)

    !!! ---------------------

    IF (.NOT. ALLOCATED(BodyWindow)) ALLOCATE(BodyWindow(nb_RBDY2))
    BodyWindow = .TRUE.

    !IF (post_processing) CALL initialize_selection
    IF (i_window /= 0) CALL initialize_selection    

  END SUBROUTINE start_postpro
  !!!--------------------------------------------------------

  SUBROUTINE close_postpro_files

    IMPLICIT NONE
    
    INTEGER :: i,j
    
    DO i=1,SIZE(PostCommand)

       IF (.NOT.PostCommand(i)%used) CYCLE

       ! due to strange behaviour os SIZE function
       ! we have to test the pointer... (SIZE gives
       ! back 1 when io_unit is not associated with g95/fortran
       if( associated(PostCommand(i)%io_unit) ) then
         DO j=1,SIZE(PostCommand(i)%io_unit)
            CLOSE(PostCommand(i)%io_unit(j))
         END DO
       end if

    END DO
    
  END SUBROUTINE close_postpro_files
  !!!---------------------------------------------------------------------------------

  subroutine reading_data(ICNAME, nfich, restart)

    !!  read parameters of each command and initialize corresponding data

    implicit none

    ! *** input
    integer, intent(in) :: ICNAME
    integer, intent(in) :: nfich
    integer, intent(in) :: restart
    
    ! *** local
    CHARACTER(len=5)  :: color,behav
    character(len=6)  :: file_position
    character(len=7)  :: file_status
    character(len=30) :: clin
    character(len=40) :: file_name, file_name1, file_name2
    integer           :: err, nb_files, nfich0, ibdyty, iblmty, imodel

    integer           :: i, j, k, NX, NY

    character(len=80) :: cout
    character(len=24) :: IAM
          !123456789012345678901234
    IAM = 'postpro_2D::reading_data'

    !------------1234567890123456789012345678901234567890
    file_name = '                                        '

    if( restart > 0 ) then
      file_status   = 'OLD'
      file_position = 'APPEND'
    else
      file_status   = 'REPLACE'
      file_position = 'ASIS'
    end if

    SELECT CASE(ICNAME)
!!! #1
!!!--------------------------------------       
!!! COMMAND AVERAGE_VELOCITY_EVOLUTION
!!!--------------------------------------       
    CASE(i_AVERAGE_VELOCITY_EVOLUTION)

       READ(nfich,'(A5)') AVcolor

       nb_files=1
       PostCommand(ICNAME)%nb_files = nb_files
       ALLOCATE(PostCommand(ICNAME)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/AVERAGE_VELOCITY_EVOLUTION.DAT  '

       nfich0 = get_io_unit()
       PostCommand(ICNAME)%io_unit(1)=nfich0
       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #2
!!! --------------------------------------
!!! COMMAND SPECIES_KINETIC_ENERGY
!!! --------------------------------------
    CASE(i_SPECIES_KINETIC_ENERGY)

       READ(nfich,'(I5)') nb_species
       IF(.NOT.ALLOCATED(SKE)) ALLOCATE(SKE(nb_species))
       DO i=1,nb_species
          READ(nfich,'(A5)') SKE(i)%behav
          SKE(i)%KE = 0.D0
          SKE(i)%DE = 0.D0
          SKE(i)%P  = 0.D0
          SKE(i)%PE = 0.D0
       END DO

       nb_files = nb_species
       PostCommand(i_SPECIES_KINETIC_ENERGY)%nb_files = nb_files
       ALLOCATE(PostCommand(i_SPECIES_KINETIC_ENERGY)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/XXXXX_KINETIC_ENERGY.DAT        '
       
       DO i = 1,nb_files
          IF ( i .LT. 1000) THEN
             WRITE(file_name(9:13),'(A5)') SKE(i)%behav
          ELSE
             CALL LOGMES(' @ WARNING : Torque number exeded')
             CALL LOGMES(' @ Only the 100th first ones will be checked')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_SPECIES_KINETIC_ENERGY)%io_unit(i)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

!!! #3
!!! --------------------------------------
!!! COMMAND SNAPSHOT_SAMPLE
!!! --------------------------------------
    CASE(i_SNAPSHOT_SAMPLE)

       IF(.NOT.ALLOCATED(body_data_snapshot)) ALLOCATE(body_data_snapshot(11,nb_RBDY2))
!!!-------------------------123456789012345678901234567890123456
       body_snap_name    = 'POSTPRO/BODY_SNAPSHOT_0000000.DAT'
       contact_snap_name = 'POSTPRO/CONTACT_SNAPSHOT_0000000.DAT'

       nb_files = 2
       PostCommand(i_SNAPSHOT_SAMPLE)%nb_files = nb_files

       ALLOCATE(PostCommand(i_SNAPSHOT_SAMPLE)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_SNAPSHOT_SAMPLE)%io_unit(i) = nfich0
       END DO

       ! Opening and closing files occuring during the computation

!!! #4
!!! --------------------------------------
!!! COMMAND MP_SNAPSHOT_SAMPLE
!!! --------------------------------------
    CASE(i_MP_SNAPSHOT_SAMPLE)

       IF(.NOT.ALLOCATED(body_data_snapshot)) ALLOCATE(body_data_snapshot(17,nb_RBDY2))
!!!-------------------------123456789012345678901234567890123456
       body_snap_name    = 'POSTPRO/BODY_SNAPSHOT_0000000.DAT'
       contact_snap_name = 'POSTPRO/CONTACT_SNAPSHOT_0000000.DAT'

       nb_files=2
       PostCommand(i_MP_SNAPSHOT_SAMPLE)%nb_files = nb_files
       ALLOCATE(PostCommand(i_MP_SNAPSHOT_SAMPLE)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_MP_SNAPSHOT_SAMPLE)%io_unit(i) = nfich0
       END DO

       ! Opening and closing files occuring during the computation

!!! #5
!!! --------------------------------------
!!! COMMAND NORMAL_CONTACT_DISTRIBUTION
!!! --------------------------------------
    CASE(i_NORMAL_CONTACT_DISTRIBUTION)

       READ(nfich,'(I5)') Nsect
       DNsect = 2*Nsect

       IF ( .NOT. ALLOCATED(C_ORI) )  ALLOCATE( C_ORI(DNsect) )
       IF ( .NOT. ALLOCATED(SC_ORI) ) ALLOCATE( SC_ORI(DNsect) )
       IF ( .NOT. ALLOCATED(WC_ORI) ) ALLOCATE( WC_ORI(DNsect) )

       IF ( .NOT. ALLOCATED(T_ORI) )  ALLOCATE( T_ORI(DNsect) )
       IF ( .NOT. ALLOCATED(ST_ORI) ) ALLOCATE( ST_ORI(DNsect) )
       IF ( .NOT. ALLOCATED(WT_ORI) ) ALLOCATE( WT_ORI(DNsect) )

!!!-----------------123456789012345678901234567890123456789
       ncd_name2 = 'POSTPRO/P2THETA_0000000.DAT' 
       ncd_name1 = 'POSTPRO/NORMAL_CONTACT_DIST_0000000.DAT'

       nb_files = 2
       PostCommand(i_NORMAL_CONTACT_DISTRIBUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_NORMAL_CONTACT_DISTRIBUTION)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_NORMAL_CONTACT_DISTRIBUTION)%io_unit(i) = nfich0
       END DO

       ! Opening and closing files occuring during the computation
      
!!! #6 
!!! --------------------------------------
!!! COMMAND CONTACT_FORCE_DISTRIBUTION
!!! --------------------------------------
    CASE(i_CONTACT_FORCE_DISTRIBUTION)
       
       READ(nfich,'(I5)') Fsect

       IF(.NOT.ALLOCATED(Fnumber))  ALLOCATE(Fnumber(Fsect))
       IF(.NOT.ALLOCATED(FNnumber)) ALLOCATE(FNnumber(Fsect))

!!!-----------------1234567890123456789012345678901234567890123456
       cfd_name1 = 'POSTPRO/CONTACT_FORCE_DISTRIBUTION_0000000.DAT'
       

       nb_files=1
       PostCommand(i_CONTACT_FORCE_DISTRIBUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_CONTACT_FORCE_DISTRIBUTION)%io_unit(nb_files))
       
       nfich0 = get_io_unit()
       PostCommand(i_CONTACT_FORCE_DISTRIBUTION)%io_unit(1)=nfich0

       ! Opening and closing files occuring during the computation

!!! #7
!!! --------------------------------------
!!! COMMAND COORDINATION_NUMBER
!!! --------------------------------------
    CASE(i_COORDINATION_NUMBER)

       IF(nb_RBDY2 /= 0) THEN 
          IF(.NOT.ALLOCATED(cplus))  ALLOCATE( cplus(nb_RBDY2))
          IF(.NOT.ALLOCATED(ctotal)) ALLOCATE(ctotal(nb_RBDY2))
          IF(.NOT.ALLOCATED(cmoins)) ALLOCATE(cmoins(nb_RBDY2))
       END IF
       
       ! IF(nb_GRAIN .NE. 0) THEN 
       !    IF(.NOT.ALLOCATED(cplus))  ALLOCATE( cplus(nb_GRAIN))
       !    IF(.NOT.ALLOCATED(ctotal)) ALLOCATE(ctotal(nb_GRAIN))
       !    IF(.NOT.ALLOCATED(cmoins)) ALLOCATE(cmoins(nb_GRAIN))
       ! END IF

       nb_files=1
       PostCommand(i_COORDINATION_NUMBER)%nb_files = nb_files
       ALLOCATE(PostCommand(i_COORDINATION_NUMBER)%io_unit(nb_files))
       
       nfich0 = get_io_unit()
       PostCommand(i_COORDINATION_NUMBER)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/COORDINATION_NUMBER.DAT         '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )
       
!!! #8
!!! --------------------------------------
!!! COMMAND BODY_TRACKING
!!! --------------------------------------
    CASE(i_BODY_TRACKING)

       READ(nfich,'(I7)') nb_tracking
       ALLOCATE(TrackingID(nb_tracking))

       DO i=1,nb_tracking
          READ(nfich,'(I7)') TrackingID(i)
       END DO

       nb_files = nb_tracking
       PostCommand(i_BODY_TRACKING)%nb_files = nb_files
       ALLOCATE(PostCommand(i_BODY_TRACKING)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/BODY_0000000.DAT                '
       
       DO i = 1,nb_files
          
          IF (i.LT.10000000) THEN
             WRITE(file_name(14:20),'(I7.7)') TrackingID(i)
          ELSE 
             CALL LOGMES(' @ Number of tracked bodies exceeded')
             EXIT
          END IF
          nfich0 = get_io_unit()
          PostCommand(i_BODY_TRACKING)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO
      
!!! #9 
!!! --------------------------------------
!!! COMMAND MP_VALUE_TRACKING
!!! --------------------------------------
    CASE(i_MP_VALUE_TRACKING)

       READ(nfich,'(I7)') nb_mp_tracking
       ALLOCATE(bMPTrackingID(nb_mp_tracking))
       ALLOCATE(tMPTrackingID(nb_mp_tracking))

       DO i=1,nb_mp_tracking
          READ(nfich,'(I7,1X,I3)') bMPTrackingID(i),tMPTrackingID(i)
       END DO

       nb_files = nb_mp_tracking
       PostCommand(i_MP_VALUE_TRACKING)%nb_files = nb_files
       ALLOCATE(PostCommand(i_MP_VALUE_TRACKING)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/MP_VALUES_0000000_0000000.DAT   '

       DO i = 1,nb_files
          
          IF (i.LT.10000000) THEN
             WRITE(file_name(19:33),'(I7.7,A1,I7.7)') bMPTrackingID(i),'.',tMPTrackingID(i)
          ELSE 
             CALL LOGMES(' @ Number of tracked bodies exeded')
             EXIT
          END IF
          nfich0 = get_io_unit()
          PostCommand(i_MP_VALUE_TRACKING)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

!!! #10
!!! --------------------------------------
!!! COMMAND TORQUE_EVOLUTION
!!! --------------------------------------
    CASE(i_TORQUE_EVOLUTION)

       READ(nfich,'(I7)') nb_torque
       ALLOCATE(TorqueID(nb_torque))

       DO i=1,nb_torque
          READ(nfich,'(I7)') TorqueID(i)
       END DO

       nb_files=nb_torque
       PostCommand(i_TORQUE_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_TORQUE_EVOLUTION)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/TORQUE_0000000.DAT              '
       
       DO i = 1,nb_files
          IF ( i .LT. 10000000) THEN
             WRITE(file_name(16:22),'(I7.7)') TorqueID(i)
          ELSE
             CALL LOGMES(' @ WARNING : Torque number exceeded')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_TORQUE_EVOLUTION)%io_unit(i)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO
       
!!! #11
!!! --------------------------------------
!!! COMMAND DENSE_SAMPLE_COMPACITY
!!! --------------------------------------
    CASE(i_DENSE_SAMPLE_COMPACITY)

       err = 0
       DO
          i_inter = 0
          READ(nfich,'(A30)',iostat=err) clin

          IF (err.NE.0) EXIT
          ! si necessaire on retire un disque ou polygone INTERieur; utile pour couette          
          IF ( 'INTER' .EQ. clin(1:5) ) THEN
             READ(clin(7:13),'(I7)',iostat=err) InterID
             i_inter = 1
          END IF
          EXIT
       END DO

       nb_files = 3
       PostCommand(i_DENSE_SAMPLE_COMPACITY)%nb_files = nb_files
       ALLOCATE(PostCommand(i_DENSE_SAMPLE_COMPACITY)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_DENSE_SAMPLE_COMPACITY)%io_unit(i)=nfich0

          !------------------------1234567890123456789012345678901234567890
          IF (i.EQ.1) file_name = 'POSTPRO/DS_COMPACITY.DAT                '
          IF (i.EQ.2) file_name = 'POSTPRO/DS_TENSORS.DAT                  '
          IF (i.EQ.3) file_name = 'POSTPRO/DS_PRINCIPAL_TENSORS.DAT        '
          
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO
 
!!! #12
!!! --------------------------------------
!!! COMMAND DOUBLET_INTERACTIONS
!!! --------------------------------------
    CASE(i_DOUBLET_INTERACTIONS)

       READ(nfich,'(A5)') doublet_type
       READ(nfich,'(I7)') doubletbdyty(1)
       READ(nfich,'(I7)') doubletbdyty(2)
       
       nb_files=1
       PostCommand(i_DOUBLET_INTERACTIONS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_DOUBLET_INTERACTIONS)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_DOUBLET_INTERACTIONS)%io_unit(1) = nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/DOUBLET_INTERACTIONS.DAT        '
 
       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )
   
!!! #13
!!! --------------------------------------
!!! COMMAND 3D_EXTRUSION
!!! --------------------------------------
    CASE(i_3D_EXTRUSION)

       READ(nfich,*) deep 
       READ(nfich,*) factor

       nb_files=1
       PostCommand(i_3D_EXTRUSION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_3D_EXTRUSION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_3D_EXTRUSION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/3D_EXTRUSION_SAMPLE.DAT         '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )
       
!!! #14
!!! --------------------------------------
!!! COMMAND QUASI_SLIDING_CONTACT
!!! --------------------------------------
    CASE(i_QUASI_SLIDING_CONTACT)

       READ(nfich,'(ES14.7)') fraction

       nb_files=1
       PostCommand(i_QUASI_SLIDING_CONTACT)%nb_files = nb_files
       ALLOCATE(PostCommand(i_QUASI_SLIDING_CONTACT)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_QUASI_SLIDING_CONTACT)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/QUASI_SLIDING_CONTACT.DAT       '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )
       
!!! #15
!!! --------------------------------------
!!! COMMAND NEW_MECAx_SETS
!!! --------------------------------------
    CASE(i_NEW_MECAx_SETS)

       READ(nfich,'(I7)') nb_MECAx_sets
       
       IF (ALLOCATED(MECAx_set)) DEALLOCATE(MECAx_set)
       ALLOCATE(MECAx_set(nb_MECAx_sets))
       
       DO i=1,nb_MECAx_sets
          READ(nfich,'(I7)') MECAx_set(i)%nb_MECAx
          ALLOCATE(MECAx_set(i)%data(MECAx_set(i)%nb_MECAx))
          DO j=1,MECAx_set(i)%nb_MECAx
             
             READ(nfich,'(I7,1x,I7)') MECAx_set(i)%data(j)%iMECAx,MECAx_set(i)%data(j)%nb_nodes
             ALLOCATE(MECAx_set(i)%data(j)%nodes(MECAx_set(i)%data(j)%nb_nodes))
             DO k=1,MECAx_set(i)%data(j)%nb_nodes
                READ(nfich,'(I7)') MECAx_set(i)%data(j)%nodes(k)
             END DO
          END DO
       END DO

!!! #16
!!! --------------------------------------
!!! COMMAND Fint_EVOLUTION
!!! --------------------------------------
    CASE(i_Fint_EVOLUTION)   

       nb_files=nb_MECAx_sets
       
       PostCommand(i_Fint_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_Fint_EVOLUTION)%io_unit(nb_files))
       
       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/Fint_0000000.DAT                '

       DO i=1,nb_MECAx_sets
          IF (i.LT.10000000) THEN
             WRITE(file_name(14:20),'(I7.7)') i
          ELSE
             write(cout,'(A)') 'There is some internal limitation in mod_postpro'
             call logmes(cout)
             write(cout,'(A)') 'You cannot compute Fint evolution of more than 9999999 sets'
             call faterr(IAM,cout)
          END IF
          nfich0 = get_io_unit()
          PostCommand(i_Fint_EVOLUTION)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

!!! #17
!!! --------------------------------------
!!! COMMAND Dep_EVOLUTION
!!! --------------------------------------
    CASE(i_Dep_EVOLUTION)

       nb_files = nb_MECAx_sets

       PostCommand(i_Dep_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_Dep_EVOLUTION)%io_unit(nb_files))
       
       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/Dep_0000000.DAT                 '
       
       DO i=1,nb_MECAx_sets
          IF (i.LT.10000000) THEN
             WRITE(file_name(13:19),'(I7.7)') i
          ELSE
             write(cout,'(A)') 'There is some internal limitation in mod_postpro'
             call logmes(cout)
             write(cout,'(A)') 'You cannot compute Dep evolution of more than 9999999 sets'
             call faterr(IAM,cout)
          ENDIF
          nfich0 = get_io_unit()
          PostCommand(i_Dep_EVOLUTION)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

!!! #18
!!! --------------------------------------
!!! COMMAND NEW_DIST_SETS
!!! --------------------------------------
    CASE(i_NEW_DIST_SETS)

       READ(nfich,'(I7)') nb_dist_sets
       
       IF (ALLOCATED(dist_set)) DEALLOCATE(dist_set)
       ALLOCATE(dist_set(nb_dist_sets))
       
       DO i=1,nb_dist_sets
          dist_set(i)%nb_MECAx=2
          ALLOCATE(dist_set(i)%data(dist_set(i)%nb_MECAx))
          dist_set(i)%data(1)%nb_nodes=1
          ALLOCATE(dist_set(i)%data(1)%nodes(1))
          dist_set(i)%data(2)%nb_nodes=1
          ALLOCATE(dist_set(i)%data(2)%nodes(1))
          READ(nfich,*) dist_set(i)%data(1)%iMECAx,dist_set(i)%data(1)%nodes(1),&
               dist_set(i)%data(2)%iMECAx,dist_set(i)%data(2)%nodes(1)
       END DO

!!! #19
!!! --------------------------------------
!!! COMMAND MAILx_dist
!!! --------------------------------------
    CASE(i_MAILx_dist)

       nb_files=1
       PostCommand(i_MAILx_dist)%nb_files = nb_files
       ALLOCATE(PostCommand(i_MAILx_dist)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/MAILx_dist.DAT                  '
 
       nfich0 = get_io_unit()
       PostCommand(i_MAILx_dist)%io_unit(1) = nfich0

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #20
!!! --------------------------------------
!!! COMMAND NEW_RIGID_SETS
!!! --------------------------------------
    CASE(i_NEW_RIGID_SETS)

       READ(nfich,'(I7)') nb_RBDY2_sets
       
       IF (ALLOCATED(RBDY2_set)) DEALLOCATE(RBDY2_set)
       ALLOCATE(RBDY2_set(nb_RBDY2_sets))
       
       DO i=1,nb_RBDY2_sets
          READ(nfich,'(I7)') RBDY2_set(i)%size
          ALLOCATE(RBDY2_set(i)%list(RBDY2_set(i)%size))
          DO j =1,RBDY2_set(i)%size
             READ(nfich,'(I7)') RBDY2_set(i)%list(j)
          END DO
       END DO

       nb_files = 2*nb_RBDY2_sets
       PostCommand(i_NEW_RIGID_SETS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_NEW_RIGID_SETS)%io_unit(nb_files))

       !-------------1234567890123456789012345678901234567890
       file_name1 = 'POSTPRO/EVOL_RIGID_SET_0000000.DAT      '
       file_name2 = 'POSTPRO/REAC_RIGID_SET_0000000.DAT      '

       DO i = 1,nb_RBDY2_sets

          IF ( i .LT. 10000000) THEN
             WRITE(file_name1(24:30),'(I7.7)') i
             WRITE(file_name2(24:30),'(I7.7)') i
          ELSE
             CALL LOGMES(' @ WARNING : Rigid set number exceeded')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_NEW_RIGID_SETS)%io_unit(2*i-1) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name1)))
          open( unit=nfich0, file=trim(location(file_name1)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_RIGID_SETS)%io_unit(2*i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name2)))
          open( unit=nfich0, file=trim(location(file_name2)), status=trim(file_status), &
                position=trim(file_position) )

       END DO

!!! #21
!!! --------------------------------------
!!! COMMAND NEW_BOUNDED_SETS
!!! --------------------------------------
    CASE(i_NEW_BOUNDED_SETS)

       READ(nfich,'(I7)') nb_bounded_sets
       
       IF (ALLOCATED(bounded_set)) DEALLOCATE(bounded_set)
       ALLOCATE(bounded_set(nb_bounded_sets))
       
       DO i=1,nb_bounded_sets
          READ(nfich,'(I7)') bounded_set(i)%idmin
          READ(nfich,'(I7)') bounded_set(i)%idmax
       END DO

       nb_files = 2*nb_bounded_sets
       PostCommand(i_NEW_BOUNDED_SETS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_NEW_BOUNDED_SETS)%io_unit(nb_files))

       !-------------1234567890123456789012345678901234567890
       file_name1 = 'POSTPRO/EVOL_BOUNDED_SET_0000000.DAT   '
       file_name2 = 'POSTPRO/REAC_BOUNDED_SET_0000000.DAT   '
       !           1234567890123456789012345
       
       DO i = 1,nb_bounded_sets
          IF ( i .LT. 10000000) THEN
             WRITE(file_name1(26:32),'(I7.7)') i
             WRITE(file_name2(26:32),'(I7.7)') i
          ELSE
             CALL LOGMES(' @ WARNING : set number exceeded')
             CALL LOGMES(' @ Only the 10000000th first ones will be checked')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_NEW_BOUNDED_SETS)%io_unit(2*i-1) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name1)))
          open( unit=nfich0, file=trim(location(file_name1)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_BOUNDED_SETS)%io_unit(2*i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name2)))
          open( unit=nfich0, file=trim(location(file_name2)), status=trim(file_status), &
                position=trim(file_position) )

       END DO

!!! #22
!!! --------------------------------------
!!! COMMAND DISSIPATED_ENERGY
!!! --------------------------------------
    CASE(i_DISSIPATED_ENERGY)

       nb_files=1

       PostCommand(i_DISSIPATED_ENERGY)%nb_files = nb_files
       ALLOCATE(PostCommand(i_DISSIPATED_ENERGY)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_DISSIPATED_ENERGY)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/DISSIPATED_ENERGY.DAT           '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #23
!!! --------------------------------------
!!! COMMAND SOLVER_INFORMATIONS
!!! --------------------------------------
    CASE(i_SOLVER_INFORMATIONS)

       nb_files=1

       PostCommand(i_SOLVER_INFORMATIONS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_SOLVER_INFORMATIONS)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_SOLVER_INFORMATIONS)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/SOLVER_INFORMATIONS.DAT         '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #24
!!! --------------------------------------
!!! COMMAND DRY_CONTACT_NATURE
!!! --------------------------------------
    CASE(i_DRY_CONTACT_NATURE)

       nb_files=1

       PostCommand(i_DRY_CONTACT_NATURE)%nb_files = nb_files
       ALLOCATE(PostCommand(i_DRY_CONTACT_NATURE)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_DRY_CONTACT_NATURE)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/DRY_CONTACT_NATURE.DAT          '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #25
!!! --------------------------------------
!!! COMMAND WET_CONTACT_NATURE
!!! --------------------------------------
    CASE(i_WET_CONTACT_NATURE)

       nb_files=1

       PostCommand(i_WET_CONTACT_NATURE)%nb_files = nb_files
       ALLOCATE(PostCommand(i_WET_CONTACT_NATURE)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_WET_CONTACT_NATURE)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/WET_CONTACT_NATURE.DAT          '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )


!!! #26
!!! --------------------------------------
!!! COMMAND KINETIC_ENERGY
!!! --------------------------------------
    CASE(i_KINETIC_ENERGY)

       nb_files=1

       PostCommand(i_KINETIC_ENERGY)%nb_files = nb_files
       ALLOCATE(PostCommand(i_KINETIC_ENERGY)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_KINETIC_ENERGY)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/KINETIC_ENERGY.DAT              '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #27
!!! --------------------------------------
!!! COMMAND VIOLATION_EVOLUTION
!!! --------------------------------------
    CASE(i_VIOLATION_EVOLUTION)

       nb_files=1

       PostCommand(i_VIOLATION_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_VIOLATION_EVOLUTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_VIOLATION_EVOLUTION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/VIOLATION_EVOLUTION.DAT         '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #28
!!! --------------------------------------
!!! COMMAND PLPLx_ANALYSIS
!!! --------------------------------------
    CASE(i_PLPLx_ANALYSIS)

       nb_files=1

       PostCommand(i_PLPLx_ANALYSIS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_PLPLx_ANALYSIS)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_PLPLx_ANALYSIS)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/PLPLx_ANALYSIS.DAT              '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #29
!!! --------------------------------------
!!! COMMAND THERMAL_EVOLUTION
!!! --------------------------------------
    CASE(i_THERMAL_EVOLUTION)

       nb_files=1

       PostCommand(i_THERMAL_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_THERMAL_EVOLUTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_THERMAL_EVOLUTION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/THERMAL_EVOLUTION.DAT           '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #30
!!! --------------------------------------
!!! COMMAND NETWORK_EVOLUTION
!!! --------------------------------------
    CASE(i_NETWORK_EVOLUTION)

       nb_files=1

       PostCommand(i_NETWORK_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_NETWORK_EVOLUTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_NETWORK_EVOLUTION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/NETWORK_EVOLUTION.DAT           '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #31
!!! --------------------------------------
!!! COMMAND CREATE_TEXT_DATA
!!! --------------------------------------
    CASE(i_CREATE_TEXT_DATA)

       nb_files=4

       PostCommand(i_CREATE_TEXT_DATA)%nb_files = nb_files
       ALLOCATE(PostCommand(i_CREATE_TEXT_DATA)%io_unit(nb_files))

       DO i=1,nb_files

          nfich0 = get_io_unit()
          PostCommand(i_CREATE_TEXT_DATA)%io_unit(i)=nfich0

          !------------1234567890123456789012345678901234567890
          IF (i.EQ.1)  file_name='POSTPRO/SAVE_BODIES.DAT      '
          IF (i.EQ.2)  file_name='POSTPRO/SAVE_DOF.DAT         '
          IF (i.EQ.3)  file_name='POSTPRO/SAVE_CONTACTS.DAT    '
          IF (i.EQ.4)  file_name='POSTPRO/SAVE_DATA.DAT        '

          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )

       END DO

!!! #32
!!! --------------------------------------
!!! COMMAND ELECTRO_EVOLUTION
!!! --------------------------------------
    CASE(i_ELECTRO_EVOLUTION)

       nb_files=1

       PostCommand(i_ELECTRO_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_ELECTRO_EVOLUTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_ELECTRO_EVOLUTION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/ELECTRO_EVOLUTION               '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #33
!!! --------------------------------------
!!! COMMAND CLOUD_ANALYSIS
!!! --------------------------------------
    CASE(i_CLOUD_ANALYSIS)

       nb_files = 10

       PostCommand(i_CLOUD_ANALYSIS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_CLOUD_ANALYSIS)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_CLOUD_ANALYSIS)%io_unit(i) = nfich0
          
          !------------------------1234567890123456789012345678901234567890
          IF(i.EQ. 1) file_name = 'POSTPRO/CLOUD_DKDKx.DAT                 '
          IF(i.EQ. 2) file_name = 'POSTPRO/CLOUD_DKJCx.DAT                 '
          IF(i.EQ. 3) file_name = 'POSTPRO/CLOUD_DKPLx.DAT                 '
          IF(i.EQ. 4) file_name = 'POSTPRO/CLOUD_DKKDx.DAT                 '
          IF(i.EQ. 7) file_name = 'POSTPRO/CLOUD_PLPLx.DAT                 '
          IF(i.EQ. 8) file_name = 'POSTPRO/CLOUD_PLJCx.DAT                 '
!          IF(i.EQ. 9) file_name = 'POSTPRO/CLOUD_DKALp.DAT                 '
          IF(i.EQ.10) file_name = 'POSTPRO/CLOUD_CLALp.DAT                 '

          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

!!! #34
!!! --------------------------------------
!!! COMMAND DISPLAY_TENSORS
!!! --------------------------------------
    CASE(i_DISPLAY_TENSORS)

       nb_files = 1

       PostCommand(i_DISPLAY_TENSORS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_DISPLAY_TENSORS)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_DISPLAY_TENSORS)%io_unit(1) = nfich0
          
       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/DISPLAY_TENSORS.DAT             '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! # 35
!!! --------------------------------------
!!! COMMAND COMPACITY_EVOLUTION
!!! --------------------------------------
    CASE(i_COMPACITY_EVOLUTION)

       READ(nfich,'(A5)') BodyBehav    ! materiau des particules
       READ(nfich,'(A12)') CMPCT_MODEL

       SELECT CASE(CMPCT_MODEL)
          !  123456789012
       CASE('SMOOTH BOX  ')
          compacity_model = icmp_smooth_box
          READ(nfich,'(I7)') ID_LEFTx
          READ(nfich,'(I7)') ID_RIGHT
          READ(nfich,'(I7)') ID_DOWNx
          READ(nfich,'(I7)') ID_UPxxx
          !  123456789012
       CASE('ROUGH BOX   ')
          compacity_model = icmp_rough_box
          READ(nfich,'(I7)') ID_LEFTx
          READ(nfich,'(I7)') ID_RIGHT
          READ(nfich,'(I7)') ID_DOWNx
          READ(nfich,'(I7)') ID_UPxxx
          !  123456789012
       CASE('COUETTE     ')
          compacity_model = icmp_couette
          READ(nfich,'(I7)') ID_INTER
          READ(nfich,'(I7)') ID_EXTER
          !  123456789012
       CASE('CLUSTER BOX ')
          compacity_model = icmp_cluster_box
          READ(nfich,'(I7)') ID_LEFTx
          READ(nfich,'(I7)') ID_RIGHT
          READ(nfich,'(I7)') ID_DOWNx
          READ(nfich,'(I7)') ID_UPxxx
          !  123456789012          
       CASE('SELECTION   ')
          compacity_model = icmp_selection
       CASE('DENSE SAMPLE')
          call faterr(IAM,'  @ Keyword DENSE SAMPLE not yet available')
       CASE DEFAULT
          write(cout,'(A,I5,A)') '  @ Keyword ',CMPCT_MODEL,' not yet available'
          call faterr(IAM,cout)
       END SELECT

       nb_files = 1

       PostCommand(i_COMPACITY_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_COMPACITY_EVOLUTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_COMPACITY_EVOLUTION)%io_unit(1) = nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/COMPACITY_EVOLUTION.DAT         '
       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

       
!!! #36
!!! --------------------------------------
!!! COMMAND HEAT_BOUND_PROFILE
!!! --------------------------------------
    CASE(i_HEAT_BOUND_PROFILE)

       nb_files=1

       PostCommand(i_HEAT_BOUND_PROFILE)%nb_files = nb_files
       ALLOCATE(PostCommand(i_HEAT_BOUND_PROFILE)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_HEAT_BOUND_PROFILE)%io_unit(1)=nfich0

       !------------------12345678901234567890123456789012345678
       heat_bound_name = 'POSTPRO/HEAT_BOUND_PROFILE_0000000.DAT'

       nb_HB = get_nb_HEAT_bounds()

       HB_NY = 0

       DO i = 1,nb_HB
          CALL get_HEAT_bound_dims(i,NX,NY)
          HB_NY = MAX(HB_NY,NY)
       END DO

       ALLOCATE(HEATBOUNDS(nb_HB,HB_NY))
       HEATBOUNDS = 0.D0
       ALLOCATE(HEATvector(HB_NY))
       HEATvector = 0.D0

       ! already made at the previous function call
       ! hb_unit = restart-1


    CASE(i_CLxxx_ANALYSIS)

       READ(nfich,*) nb_CLxxx_set

       IF (ALLOCATED(CLxxx_set)) DEALLOCATE(CLxxx_set)
       ALLOCATE(CLxxx_set(2,nb_CLxxx_set))
       
       DO i=1,nb_CLxxx_set
         READ(nfich,*) CLxxx_set(1,i),CLxxx_set(2,i)
       END DO

       nb_files = nb_CLxxx_set
       PostCommand(i_CLxxx_ANALYSIS)%nb_files = nb_CLxxx_set
       ALLOCATE(PostCommand(i_CLxxx_ANALYSIS)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/CLxxx_0000000_ANALYSIS.DAT      '
       
       DO i = 1,nb_files
          IF ( i .LT. 10000000) THEN
             WRITE(file_name(15:21),'(I7.7)') i
          ELSE
             CALL LOGMES(' @ WARNING : CLxxx set number exceeded')
             CALL LOGMES(' @ Only the 10000000th first ones will be checked')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_CLxxx_ANALYSIS)%io_unit(i)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

!#38 i_SNAP_SURFACE_ENERGIE_SECTOR 
!MR&VHN

   CASE(i_SNAP_SURFACE_ENERGIE_SECTOR)

       !READ(nfich,*) nb_WS
       !IF (ALLOCATED(SectorID)) DEALLOCATE(SectorID)
       !ALLOCATE(SectorID(nb_WS))
       
       !DO i=1,nb_WS
       !   READ(nfich,*) SectorID(i)
       !END DO

       nb_WSsect = get_surface_sectors()
 
       !IF (ALLOCATED(sector_set)) DEALLOCATE(sector_set)
       !ALLOCATE(sector_set(nb_WSsect,nb_RBDY2))
       !ALLOCATE(sector_set(nb_WSsect,nb_WS))

       nb_files = 1
       PostCommand(i_SNAP_SURFACE_ENERGIE_SECTOR)%nb_files = nb_files
       ALLOCATE(PostCommand(i_SNAP_SURFACE_ENERGIE_SECTOR)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       ws_name   = 'POSTPRO/WS_0000000.DAT'
          
       nfich0 = get_io_unit()
       PostCommand(i_SNAP_SURFACE_ENERGIE_SECTOR)%io_unit(1)=nfich0

!!! #39
!!! --------------------------------------
!!! COMMAND CZM ENERGY EVOLUTION
!!! --------------------------------------
    CASE(i_CZM_ENERGY_EVOLUTION)

       nb_files=1

       PostCommand(i_CZM_ENERGY_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_CZM_ENERGY_EVOLUTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_CZM_ENERGY_EVOLUTION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/CZM_ENERGY_EVOLUTION.DAT        '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

!!! #40
!!! --------------------------------------
!!! COMMAND ENERGY_SNAPSHOT_SAMPLE
!!! --------------------------------------
    CASE(i_ENERGY_SNAPSHOT_SAMPLE)

!!!---------------------------12345678901234567890123456789012345
       energy_snap_name    = 'POSTPRO/ENERGY_SNAPSHOT_0000000.DAT'

       nb_files = 1
       PostCommand(i_ENERGY_SNAPSHOT_SAMPLE)%nb_files = nb_files

       ALLOCATE(PostCommand(i_ENERGY_SNAPSHOT_SAMPLE)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_ENERGY_SNAPSHOT_SAMPLE)%io_unit(i) = nfich0
       END DO

       ! Opening and closing files occuring during the computation

!!! --------------------------------------
    CASE DEFAULT
       call faterr(IAM,'Unknown command in POSTPRO.DAT')
    END SELECT

  END SUBROUTINE reading_data

  !!!--------------------------------------------------------------------------------------
  ! *subroutine postpro_during_c:
  !
  !   subroutine lancant les subroutines qui doivent être lancé au cours du calcul
  !
  !  RAJOUT COMMANDE: 1) mettre un cas nveau selon nom de commande et l'appel à la procédure utilisée
  !
  !--------------------------------------------------------------------------------------
  SUBROUTINE postpro_during_computation
    !!****u* CORE.POSTPRO/postpro_during_computation
    !! NAME
    !! subroutine postpro_during_computation
    !! PURPOSE
    !!  execute POSTPRO.DAT commands which occur during computation
    !! USES
    !!  POSTPRO/update_selection
    !!  POSTPRO/bodies_monitoring
    !!  POSTPRO/torque_evolution
    !!  POSTPRO/dense_sample_compacity
    !!  POSTPRO/snapshot
    !!  POSTPRO/body_tracking
    !!  POSTPRO/comp_dissipated_energy
    !!  POSTPRO/kinetic_energy
    !!  POSTPRO/species_kinetic_energy
    !!  POSTPRO/solver_informations
    !!  POSTPRO/dry_contact_nature
    !!  POSTPRO/wet_contact_nature
    !!  POSTPRO/coordination_number
    !!  POSTPRO/average_velocity_evolution
    !!  POSTPRO/normal_contact_distribution
    !!  POSTPRO/contact_force_distribution
    !!  POSTPRO/violation_evolution
    !!  POSTPRO/doublet_interactions
    !!  POSTPRO/PLPLx_analysis
    !!  POSTPRO/quasi_sliding_contact
    !!  POSTPRO/Fint_EVOLUTION
    !!  POSTPRO/Dep_EVOLUTION
    !!  POSTPRO/network_evolution
    !!  POSTPRO/electro_evolution
    !!  POSTPRO/create_text_data
    !!  POSTPRO/save_data
    !!****

    IMPLICIT NONE
  
    INTEGER :: ic
    
    nb_DKDKx = get_nb_inters( i_dkdkx )
    nb_DKJCx = get_nb_inters( i_dkjcx )
    nb_DKKDx = get_nb_inters( i_dkkdx )
    nb_PLPLx = get_nb_inters( i_plplx )
    nb_PLJCx = get_nb_inters( i_pljcx )
    nb_DKPLx = get_nb_inters( i_dkplx )
    nb_CLJCx = get_nb_inters( i_cljcx )
    nb_CLALp = get_nb_inters( i_clalp )
    nb_DKALp = get_nb_inters( i_dkalp )

!!!
!!! For post processing
!!!    

    !fd IF(post_processing) CALL update_selection
    IF(i_window /= 0) CALL update_selection    

!!! mr: Commands are classed according to the alphabetical order.
!!!     Please respect this order if you add a new command.

    DO ic = 1,NB_COMMANDS
       
       IF(.NOT.PostCommand(ic)%used) CYCLE
       IF (MODULO(Nstep,PostCommand(ic)%step) .NE. 0) CYCLE
       
       SELECT CASE(ic)

       CASE(i_AVERAGE_VELOCITY_EVOLUTION)
          CALL average_velocity_evolution

       CASE(i_BODY_TRACKING)
          CALL BODY_TRACKING(1)

       CASE(i_CLOUD_ANALYSIS)
          CALL cloud_analysis

       CASE(i_COMPACITY_EVOLUTION)
          CALL compacity_evolution

       CASE(i_CONTACT_FORCE_DISTRIBUTION)
          CALL contact_force_distribution

       CASE(i_COORDINATION_NUMBER)
          CALL coordination_number

       CASE(i_CREATE_TEXT_DATA)
          CALL create_text_data
          CALL save_data       

       CASE(i_DENSE_SAMPLE_COMPACITY)
          CALL dense_sample_compacity

       CASE(i_Dep_EVOLUTION)
          CALL Dep_EVOLUTION

       CASE(i_DISPLAY_TENSORS)
          CALL display_tensors

       CASE(i_DISSIPATED_ENERGY)
          CALL comp_dissipated_energy

       CASE(i_DOUBLET_INTERACTIONS)
          CALL doublet_interactions

       CASE(i_DRY_CONTACT_NATURE)
          CALL dry_contact_nature

       CASE(i_ELECTRO_EVOLUTION)
          CALL electro_evolution

       CASE(i_Fint_EVOLUTION)
          CALL Fint_EVOLUTION

       CASE(i_HEAT_BOUND_PROFILE)
          CALL heat_bound_profile

       CASE(i_KINETIC_ENERGY)
          CALL kinetic_energy

       CASE(i_MAILx_dist)
          CALL mailx_dist

       CASE(i_MP_SNAPSHOT_SAMPLE)
          CALL snapshot_sample(.TRUE.)

       CASE(i_ENERGY_SNAPSHOT_SAMPLE)
          CALL energy_snapshot_sample

       CASE(i_MP_VALUE_TRACKING)
          CALL mp_value_tracking

       CASE(i_NETWORK_EVOLUTION)
          CALL network_evolution

       CASE(i_NEW_BOUNDED_SETS)
          CALL BODY_TRACKING(3)
          CALL TORQUE_EVOLUTION(3)

       CASE(i_NEW_RIGID_SETS)
          CALL BODY_TRACKING(2)
          CALL TORQUE_EVOLUTION(2)

       CASE(i_NORMAL_CONTACT_DISTRIBUTION)
          CALL normal_contact_distribution

       CASE(i_PLPLx_ANALYSIS)
          CALL PLPLx_analysis

       CASE(i_QUASI_SLIDING_CONTACT)
          CALL quasi_sliding_contact

       CASE(i_SNAPSHOT_SAMPLE)
          CALL snapshot_sample(.FALSE.)

       CASE(i_SOLVER_INFORMATIONS)
          CALL solver_informations

       CASE(i_SPECIES_KINETIC_ENERGY)
          CALL species_kinetic_energy

       CASE(i_THERMAL_EVOLUTION)
          CALL thermal_evolution

       CASE(i_TORQUE_EVOLUTION)
          CALL TORQUE_EVOLUTION(1)

       CASE(i_VIOLATION_EVOLUTION)
          CALL violation_evolution

       CASE(i_WET_CONTACT_NATURE)
          CALL wet_contact_nature

       CASE(i_3D_EXTRUSION)
          CALL extrusion

       CASE(i_CLxxx_ANALYSIS)
          CALL clxxx_analysis

       CASE(i_SNAP_SURFACE_ENERGIE_SECTOR)  !MR&VHN
          CALL WS_sector_evolution

       CASE(i_CZM_ENERGY_EVOLUTION)         !JR
          CALL czm_energy_evolution

       CASE DEFAULT

       END SELECT
    END DO

  END SUBROUTINE postpro_during_computation

  subroutine flush_during_computation
    implicit none
    integer :: i, j, io, nb_io
    logical :: ok

    do i =1, NB_COMMANDS

        if ( PostCommand(i)%used ) then
            if( associated(PostCommand(i)%io_unit) ) then
              nb_io = size( PostCommand(i)%io_unit )
            else
              nb_io = 0
            end if
            do j = 1, nb_io
                io = PostCommand(i)%io_unit(j)
                INQUIRE(UNIT=io, OPENED=ok)
                if( ok ) then
                  call flush( io )
                end if
            end do
        end if
    end do

  end subroutine flush_during_computation

!!!------------------------------------------------------------------------------------------------
!!!
!!! POSTPROCESSING SUBROUTINES
!!!
!!!------------------------------------------------------------------------------------------------

  !!!------------------------------------------------------------------------------------------------
  !!! #10
  !!!-----------------------------------------------------------------------------------
  SUBROUTINE TORQUE_EVOLUTION(iFLAG)

    IMPLICIT NONE

    REAL(kind=8),DIMENSION(3) :: REAC,Fext
    INTEGER                   :: i,j,ibdyty,iFLAG
    INTEGER                   :: nfich
    LOGICAL                   :: visible

    SELECT CASE(iFLAG)

    !!! TORQUE EVOLUTION 
    CASE(1)
       DO i=1,nb_torque

          ibdyty = TorqueID(i)
          REAC = get_REAC(ibdyty)
          Fext = get_Fext(ibdyty)

          nfich = PostCommand(i_TORQUE_EVOLUTION)%io_unit(i)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),Fext(1),Fext(2),Fext(3)

       END DO
         
    !!! NEW RIGID SETS
    CASE(2)
       DO i=1,nb_RBDY2_sets
          
          REAC = 0.D0
          Fext = 0.D0

          DO j=1, RBDY2_set(i)%size

             ibdyty = RBDY2_set(i)%list(j)
             visible = get_visible(ibdyty)

             IF (.NOT. visible) CYCLE

             REAC = REAC + get_REAC(ibdyty)
             Fext = Fext + get_Fext(ibdyty)

          END DO

          nfich = PostCommand(i_NEW_RIGID_SETS)%io_unit(2*i)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),Fext(1),Fext(2),Fext(3)

       END DO
       
    !!! NEW BOUNDED SETS
    CASE(3)
       DO i=1,nb_bounded_sets
          
          REAC = 0.D0
          Fext = 0.D0

          DO ibdyty = bounded_set(i)%idmin,bounded_set(i)%idmax

             visible = get_visible(ibdyty)
             IF (.NOT. visible) CYCLE

             REAC = REAC + get_REAC(ibdyty)
             Fext = Fext + get_Fext(ibdyty)

          END DO

          nfich = PostCommand(i_NEW_BOUNDED_SETS)%io_unit(2*i)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),Fext(1),Fext(2),Fext(3)

       END DO
    END SELECT

  END SUBROUTINE TORQUE_EVOLUTION

  !!!-----------------------------------------------------------------------------------
  !!! #11
  !!!-----------------------------------------------------------------------------------

  !fd le nom de cette routine est inadapte et
  !fd display_tensors fait la meme chose (excepte le calcul de la surface du domaine)
  !fd faire du menage ...

  
  SUBROUTINE dense_sample_compacity

    IMPLICIT NONE

    integer :: nfich,nb_CDAN,icdan
    integer :: i, icdtac, iadj, nb_adj, verlet_size
    integer  :: itact, ibdy
    
    integer :: nb_Tri, it1, it2, it3

    real(kind=8), dimension(2) :: Ipoint, tuc, nuc
    real(kind=8), dimension(2) :: coor1,coor2,coor3

    real(kind=8) :: AREA, PAREA
    real(kind=8) :: s11,s12,s21,s22,f11,f12,f21,f22
    real(kind=8) :: sig1,sig2,tex1,tex2
    real(kind=8) :: DET,TRA,DELTA

    REAL(kind=8),DIMENSION(2,2)  :: SIGMA,TEXT
    integer     , dimension(5)                :: inters_id    

    integer     , dimension(:)  , allocatable :: NodeIndex
    real(kind=8), dimension(:,:), allocatable :: Nodes
    integer     , dimension(:,:), allocatable :: TriangleNumber, TriangleNodes  

    character(len=80) ::cout

    inters_id = (/ i_dkdkx, i_dkjcx, i_dkkdx, i_plplx, i_pljcx /)

    nb_CDAN = 0
    do i = 1, size(inters_id)
      nb_CDAN = nb_CDAN + get_nb_verlets( inters_id(i) )
    end do

    if (nb_CDAN == 0) return

    if (allocated(Nodes)) deallocate(Nodes)
    allocate( Nodes(2, nb_CDAN) )

    ! on parcourt tous les contacts
    
    SIGMA = 0.d0
    TEXT  = 0.d0
    icdan = 0
    
    do i = 1, size(inters_id)
      id_inter = inters_id(i)

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          icdan = icdan + 1

          call get_verlet_local_frame( id_inter, icdtac, iadj, Ipoint, tuc, nuc )

          Nodes(1:2, icdan) = Ipoint(1:2)

          ! ne remonte que les contribution de dkdk et plpl
          call compute_SIGMA( id_inter, icdtac, iadj, H, SIGMA )

          ! ne remonte que les contribution de dkdk et plpl
          if (id_inter == i_dkdkx .or. id_inter == i_plplx) then   
            TEXT(1, 1:2) = TEXT(1, 1:2) + nuc(1) * nuc(1:2)
            TEXT(2, 1:2) = TEXT(2, 1:2) + nuc(2) * nuc(1:2)
          endif
         
        end do

      end do

    end do

    !fd calcul de la surface du domaine en triangulant le domaine occupe par les points de contact
    
    IF (ALLOCATED(NodeIndex)) DEALLOCATE(NodeIndex)
    ALLOCATE(NodeIndex(nb_CDAN))

    IF (ALLOCATED(TriangleNodes)) DEALLOCATE(TriangleNodes)
    ALLOCATE(TriangleNodes(3,2*nb_CDAN))

    IF (ALLOCATED(TriangleNumber)) DEALLOCATE(TriangleNumber)
    ALLOCATE(TriangleNumber(3,2*nb_CDAN))
       
    NodeIndex      = 0
    TriangleNodes  = 0
    TriangleNumber = 0
       
    DO icdan = 1,nb_CDAN
       NodeIndex(icdan) = icdan
    END DO
       
    nb_Tri = 0
       
    CALL dtris2 ( nb_CDAN , Nodes , NodeIndex , nb_Tri , TriangleNodes , TriangleNumber )

    AREA  = 0.d0
    
    DO i = 1,nb_TRI
       
       it1 = TriangleNodes(1,i)
       it2 = TriangleNodes(2,i)
       it3 = TriangleNodes(3,i)
          
       coor1 = Nodes(:,it1)
       coor2 = Nodes(:,it2)
       coor3 = Nodes(:,it3)
          
       AREA  = AREA + 0.5*( & 
            ( ( coor2(1)-coor1(1) )*( coor3(2)-coor1(2) ) ) &
            - &
            ( ( coor2(2)-coor1(2) )*( coor3(1)-coor1(1) ) ) )
          
    END DO

    ! le volume occupe par les particules dans le domaine
    
    PAREA = 0.d0

    ! si une particule interieure 
    if (i_inter == 1) then    
      DO itact=1,nb_DISKx
         ibdy = diskx2bdyty(1,itact)
         if (ibdy /= InterID) PAREA = PAREA + get_area(ibdy)
      END DO

      DO itact=1,nb_POLYG
         ibdy=polyg2bdyty(1,itact)
         if (ibdy /= InterID) PAREA = PAREA + get_area(ibdy)
      END DO

      AREA = AREA - get_area(InterID)      
    else
      DO itact=1,nb_DISKx
         PAREA = PAREA + get_area(diskx2bdyty(1,itact))
      END DO

      DO itact=1,nb_POLYG
         PAREA = PAREA + get_area(polyg2bdyty(1,itact))
      END DO
    endif
       
    IF ( AREA < 1.D-16) THEN
       write(cout,'(A)') ' @ Warning: Problem during the computation of the area'
       call logmes(cout)
       write(cout,'(A)') ' @ The computation should be stop'
       call faterr('postpro_2D::dense_sample_compacity',cout)
    END IF


    !
    
    SIGMA = SIGMA/AREA
    TEXT  = TEXT/REAL(nb_CDAN,8)

    s11 = SIGMA(1,1)
    s12 = SIGMA(1,2)
    s21 = SIGMA(2,1)
    s22 = SIGMA(2,2)

    IF(s12 < 1.D-16)THEN
       IF (s11 >= s22) THEN
          sig1 = s11
          sig2 = s22
       ELSE
          sig1 = s22
          sig2 = s11
       END IF
    ELSE
       DET   = s11*s22-s12*s21
       TRA   = s11+s22
       DELTA = TRA*TRA-4*DET
       IF(DELTA >= 0)THEN
          sig1  = (TRA+SQRT(DELTA))*0.5
          sig2  = (TRA-SQRT(DELTA))*0.5
       ELSE
          sig1  = 0.D0
          sig2  = 0.D0
       END IF
    END IF

    f11 = TEXT(1,1)
    f12 = TEXT(1,2)
    f21 = TEXT(2,1)
    f22 = TEXT(2,2)

    IF(f12 < 1.D-16)THEN
       IF (s11 >= s22) THEN
          tex1 = f11
          tex2 = f22
       ELSE
          tex1 = f22
          tex2 = f11
       END IF
    ELSE
       DET   = f11*f22-f12*f21
       TRA   = f11+f22
       DELTA = TRA*TRA-4*DET
       IF(DELTA >= 0)THEN
          tex1  = (TRA+SQRT(DELTA))*0.5
          tex2  = (TRA-SQRT(DELTA))*0.5
       ELSE
          tex1  = 0.D0
          tex2  = 0.D0
       END IF
    END IF

    nfich = PostCommand(i_DENSE_SAMPLE_COMPACITY)%io_unit(1)
    WRITE(nfich,'(ES15.8,3(1X,ES14.7))') TPS,PAREA/AREA,PAREA,AREA

    nfich = PostCommand(i_DENSE_SAMPLE_COMPACITY)%io_unit(2)
    WRITE(nfich,'(ES15.8,8(1X,ES14.7))') TPS,s11,s12,s21,s22,f11,f12,f21,f22
    
    nfich = PostCommand(i_DENSE_SAMPLE_COMPACITY)%io_unit(3)
    IF (ABS(sig1+sig2).LT.1.D-16) THEN
       WRITE(nfich,'(ES15.8,8(1X,ES14.7))') TPS,sig1,sig2,0.d0           ,0.5*(sig1-sig2),0.d0                   ,&
                                            tex1,tex2,tex1-tex2
    ELSE
       WRITE(nfich,'(ES15.8,8(1X,ES14.7))') TPS,sig1,sig2,0.5*(sig1+sig2),0.5*(sig1-sig2),(sig1-sig2)/(sig1+sig2),&
                                            tex1,tex2,tex1-tex2
    END IF


  END SUBROUTINE dense_sample_compacity

  !!!------------------------------------------------------------------------------
  !!! #3 AND #5
  !!!------------------------------------------------------------------------------

  !fd virer MP dans la couche MP
  
  SUBROUTINE snapshot_sample(FLAG)

    IMPLICIT NONE

    integer                   :: status
    LOGICAL                   :: FLAG
    INTEGER                   :: icdbdy,ianbdy,iadj,icdan,itacty,nb_tacty
    INTEGER                   :: nfich1, nfich2,nb_CDAN
    REAL(kind=8)              :: RlocN,RlocT,area,vn,vt,WS,Ti,TCond,Ei,ECond
    REAL(kind=8),DIMENSION(2) :: Ipoint,tuc,nuc,Fik,rcd,ran
    REAL(kind=8),DIMENSION(3) :: coor,Vbegin

    REAL(kind=8),DIMENSION(max_internal_tact) :: internal

    integer :: i, nb_adj, icdtac, verlet_size, id_inter
    integer, dimension(7) :: inters_id
    character(len=5) :: module_name

    snap_unit = snap_unit + 1

    IF(FLAG)THEN
       nfich1 = PostCommand(i_MP_SNAPSHOT_SAMPLE)%io_unit(1)
       nfich2 = PostCommand(i_MP_SNAPSHOT_SAMPLE)%io_unit(2)
    ELSE
       nfich1 = PostCommand(i_SNAPSHOT_SAMPLE)%io_unit(1)
       nfich2 = PostCommand(i_SNAPSHOT_SAMPLE)%io_unit(2)
    END IF

    body_data_snapshot = 0.D0

    
    !!! BODY FILE
    WRITE(body_snap_name(23:29),'(I7.7)') snap_unit

    !!! CONTACT FILE
    WRITE(contact_snap_name(26:32),'(I7.7)') snap_unit

    OPEN(unit=nfich2,file=trim(location(contact_snap_name)),status='REPLACE')
    OPEN(unit=nfich1,file=trim(location(body_snap_name)),status='REPLACE')

    inters_id = (/ i_dkdkx, i_dkjcx, i_dkkdx, i_clalp, &
                   i_plplx, i_pljcx, i_cljcx           /)
    nb_CDAN = 0
    do i = 1, 7
      nb_CDAN = nb_CDAN + get_nb_verlets( inters_id(i) )
    end do

    WRITE(nfich2,'(I7)') nb_CDAN

    DO icdbdy=1,nb_RBDY2

       coor   = get_coor(icdbdy,0)
       Vbegin = get_Vbegin(icdbdy)
       area   = get_area(icdbdy)

       body_data_snapshot(1:2,icdbdy) = coor(1:2)
       body_data_snapshot(3,icdbdy)   = area

       body_data_snapshot(4:6,icdbdy) = Vbegin(1:3)
       body_data_snapshot(  7,icdbdy) = 0.0
       body_data_snapshot(  8,icdbdy) = 0.0
       body_data_snapshot(  9,icdbdy) = 0.0
       body_data_snapshot( 10,icdbdy) = 0.0
       body_data_snapshot( 11,icdbdy) = 0.0

    END DO

    IF(FLAG)THEN
       DO icdbdy=1,nb_RBDY2
          body_data_snapshot(12:17,icdbdy) = 0.0

          TCond = 0.D0
          Ti    = 0.D0
          ECond = 0.D0
          Ei    = 0.D0
          WS    = 0.D0

          nb_tacty = get_nb_tacty(icdbdy)
       
          DO itacty=1,nb_tacty
             TCond = TCond + get_thermal_value(icdbdy,itacty)
             Ti    = Ti    + get_therm_cond(icdbdy,itacty)
             !WS    = WS    + get_average_WS(icdbdy,itacty)
          END DO

          ECond = get_elec_cond(icdbdy)
          Ei    = get_electric_potentiel(icdbdy)
       
          TCond = TCond/REAL(nb_tacty,8)
          Ti    = Ti/REAL(nb_tacty,8)
          WS    = WS/REAL(nb_tacty,8)

          body_data_snapshot(12,icdbdy) = Ti
          body_data_snapshot(13,icdbdy) = TCond
          body_data_snapshot(14,icdbdy) = Ei
          body_data_snapshot(15,icdbdy) = ECond
          body_data_snapshot(16,icdbdy) = WS
       END DO
    END IF

    do i = 1, 7
 
      id_inter    = inters_id(i)
      module_name = get_interaction_name_from_id( id_inter )


      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      icdan = 0
      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          icdan = icdan + 1
          !call get_verlet_antac( id_inter, icdtac, iadj, ianbdy )

          icdbdy = get_verlet_icdbdy(id_inter, icdtac)
          ianbdy = get_verlet_ianbdy(id_inter, icdtac, iadj)

          call get_verlet_local_frame( id_inter, icdtac, iadj, Ipoint, tuc, nuc )
          call get_verlet_rloc( id_inter, icdtac, iadj, status, RlocT, RlocN )
          call get_verlet_vloc( id_inter, icdtac, iadj, vt, vn )

          internal = 0.d0
          call get_verlet_internal( id_inter, icdtac, iadj, internal )

          Fik = ( RlocN ) * nuc + ( RlocT ) * tuc

          write( nfich2, '(1X,A5,3(1X,I7),10(1X,ES14.7))' ) module_name, icdan, &
                                                            icdbdy, ianbdy, RlocN, RlocT, &
                                                            vn, vt, internal(1:6)

          if( id_inter == i_dkdkx ) then

            rcd(1:2) = body_data_snapshot(1:2,icdbdy) - Ipoint(1:2)
                   
            body_data_snapshot( 7,icdbdy) = body_data_snapshot( 7,icdbdy) + rcd(1)*Fik(1)
            body_data_snapshot( 8,icdbdy) = body_data_snapshot( 8,icdbdy) + rcd(1)*Fik(2)
            body_data_snapshot( 9,icdbdy) = body_data_snapshot( 9,icdbdy) + rcd(2)*Fik(1)
            body_data_snapshot(10,icdbdy) = body_data_snapshot(10,icdbdy) + rcd(2)*Fik(2)
            
            ran(1:2) = body_data_snapshot(1:2,ianbdy)-Ipoint(1:2)
            
            body_data_snapshot( 7,ianbdy) = body_data_snapshot( 7,ianbdy) - ran(1)*Fik(1)
            body_data_snapshot( 8,ianbdy) = body_data_snapshot( 8,ianbdy) - ran(1)*Fik(2)
            body_data_snapshot( 9,ianbdy) = body_data_snapshot( 9,ianbdy) - ran(2)*Fik(1)
            body_data_snapshot(10,ianbdy) = body_data_snapshot(10,ianbdy) - ran(2)*Fik(2)
            
            body_data_snapshot(11,icdbdy) = body_data_snapshot(11,icdbdy) + 1
            body_data_snapshot(11,ianbdy) = body_data_snapshot(11,ianbdy) + 1

          else if( id_inter == i_dkjcx .or. id_inter == i_dkkdx ) then

            rcd(1:2) = body_data_snapshot(1:2,icdbdy) - Ipoint(1:2)
            
            body_data_snapshot( 7,icdbdy) = body_data_snapshot( 7,icdbdy) + rcd(1)*Fik(1)
            body_data_snapshot( 8,icdbdy) = body_data_snapshot( 8,icdbdy) + rcd(1)*Fik(2)
            body_data_snapshot( 9,icdbdy) = body_data_snapshot( 9,icdbdy) + rcd(2)*Fik(1)
            body_data_snapshot(10,icdbdy) = body_data_snapshot(10,icdbdy) + rcd(2)*Fik(2)
            
            body_data_snapshot(11,icdbdy) = body_data_snapshot(11,icdbdy) + 1

          end if

        end do
      end do
    end do

    IF(nb_RBDY2 /= 0)THEN
       IF(FLAG)THEN
          DO icdbdy=1,nb_RBDY2
             WRITE(nfich1,'(17(1X,ES14.7))') body_data_snapshot(1:17,icdbdy)
          END DO
       ELSE
          DO icdbdy=1,nb_RBDY2
             WRITE(nfich1,'(11(1X,ES14.7))') body_data_snapshot(1:11,icdbdy)
          END DO
       END IF
    END IF

    CLOSE(nfich1)
    CLOSE(nfich2)

  END SUBROUTINE snapshot_sample
  
  !!!--------------------------------------------------------------------------------------
  !!! # 29
  !!!--------------------------------------------------------------------------------------

  !fd a virer dans la couche MP
  
  SUBROUTINE thermal_evolution

    IMPLICIT NONE
    INTEGER      :: nfich,icdbdy,itacty,nb_tacty
    REAL(kind=8) :: GPV,GDPV,GDV2,GQIJ,GA,GQRIJ,GAQIJ,TWS,WS,Tav,TTav

    CALL get_global_thermal_variable(GPV,GDPV,GDV2,GQIJ,GQRIJ,GAQIJ,GA)
    nfich = PostCommand(i_THERMAL_EVOLUTION)%io_unit(1)

    TWS = 0.D0
    TTav = 0.D0 !jr : average temperature

    DO icdbdy=1,nb_RBDY2
       WS = 0.D0
       nb_tacty = get_nb_tacty(icdbdy)
       DO itacty=1,nb_tacty
       !WS    = WS    + get_average_WS(icdbdy,itacty)
       END DO
       WS    = WS/REAL(nb_tacty,8)
       TWS = TWS + WS
    END DO

    DO icdbdy=1,nb_RBDY2
       Tav = 0.D0
       nb_tacty = get_nb_tacty(icdbdy)
       DO itacty=1,nb_tacty
          Tav = Tav + get_thermal_value(icdbdy,itacty)
       END DO
       Tav = Tav/REAL(nb_tacty,8)
       TTav = TTav + Tav
    END DO

    IF (nb_RBDY2.NE.0) TWS = TWS/REAL(nb_RBDY2,8)
    IF (nb_RBDY2.NE.0) TTav = TTav/REAL(nb_RBDY2,8)

    WRITE(nfich,'(ES15.8,9(1X,ES14.7))') TPS,TTav,GPV,GDPV,GDV2,GQIJ,GQRIJ,GA,TWS,GAQIJ

  END SUBROUTINE thermal_evolution
  
  !!!--------------------------------------------------------------------------------------
  !!! # 8
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE BODY_TRACKING(iFLAG)

    IMPLICIT NONE
    
    INTEGER                   :: i,j,ibdyty,iFLAG
    REAL(kind=8),DIMENSION(3) :: COOR,V,X
    INTEGER                   :: nfich,nb
    LOGICAL                   :: visible

    SELECT CASE(iFLAG)
       
    !!! BODY TRACKING
    CASE(1)
       DO i=1,nb_tracking
          ibdyty = TrackingID(i)
          COOR = get_coor(ibdyty,0)
          V    = get_V(ibdyty)
          X    = get_X(ibdyty)
          nfich = PostCommand(i_BODY_TRACKING)%io_unit(i)
          WRITE(nfich,'(ES15.8,9(1X,ES14.7))') TPS,coor(1),coor(2),coor(3), &
                                               X(1),X(2),X(3),V(1),V(2),V(3)
       END DO
       
    !!! NEW RIGID SETS
    CASE(2)
       DO i=1, nb_RBDY2_sets
          
          COOR  = 0.D0
          X     = 0.D0
          V     = 0.D0
          
          nb    = 0
          
          DO j=1,RBDY2_set(i)%size
             
             ibdyty  = RBDY2_set(i)%list(j)
             VISIBLE = get_visible(ibdyty)
             IF (.NOT. VISIBLE) CYCLE
             
             COOR  = COOR + get_coor(ibdyty, 0)
             X     = X    + get_X(ibdyty)
             V     = V    + get_V(ibdyty)

             nb = nb + 1

          END DO

          COOR  = COOR/REAL(nb,8)
          X     = X/REAL(nb,8)
          V     = V/REAL(nb,8)

          nfich = Postcommand(i_NEW_RIGID_SETS)%io_unit(2*i-1)
          WRITE(nfich,'(ES15.8,9(1X,ES14.7))') TPS,coor(1),coor(2),coor(3), &
                                               X(1),X(2),X(3),V(1),V(2),V(3)
       END DO
       
       !!! NEW BOUNDED SETS
       CASE(3)
       DO i=1, nb_bounded_sets
          
          COOR  = 0.D0
          X     = 0.D0
          V     = 0.D0
          
          nb    = 0

          DO ibdyty = bounded_set(i)%idmin,bounded_set(i)%idmax
          
             visible = get_visible(ibdyty)
             IF (.NOT. VISIBLE) CYCLE

             COOR  = COOR + get_coor(ibdyty, 1)
             X     = X    + get_X(ibdyty)
             V     = V    + get_V(ibdyty)

             nb = nb + 1

          END DO

          COOR  = COOR/REAL(nb,8)
          X     = X/REAL(nb,8)
          V     = V/REAL(nb,8)

          nfich = Postcommand(i_NEW_BOUNDED_SETS)%io_unit(2*i-1)
          WRITE(nfich,'(ES15.8,9(1X,ES14.7))') TPS,coor(1),coor(2),coor(3), &
                                               X(1),X(2),X(3),V(1),V(2),V(3)
       END DO

    END SELECT

  END SUBROUTINE BODY_TRACKING
  
  !!!--------------------------------------------------------------------------------------
  !!! # 9
  !!!--------------------------------------------------------------------------------------

  !fd a virer dans la couche MP
  
  SUBROUTINE MP_Value_Tracking

    IMPLICIT NONE
    INTEGER      :: i,ibdyty,itacty
    INTEGER      :: nfich
    REAL(kind=8) :: TC,TT,WS,EP,EC

    DO i =1,nb_mp_tracking

       ibdyty = bMPTrackingID(i)
       itacty = tMPTrackingID(i)

       TT = get_thermal_value(ibdyty,itacty)
       TC = get_therm_cond(ibdyty,itacty)
!       WS = get_average_WS(ibdyty,itacty)
       EP = get_electric_potentiel(ibdyty)
       EC = get_elec_cond(ibdyty)

       nfich = PostCommand(i_MP_VALUE_TRACKING)%io_unit(i)
       WRITE(nfich,'(ES15.8,5(1X,ES14.7))') TPS,TC,TT,WS,EP,EC

    END DO

  END SUBROUTINE MP_Value_Tracking
  
  !!!----------------------------------------------------------------------------
  !!! # 22
  !!!----------------------------------------------------------------------------
  subroutine comp_dissipated_energy_aux( id_inter, ik,  energy )
    implicit none

    integer( kind = 4 )                  :: id_inter
    integer( kind = 4 )                  :: ik
    real( kind = 8 )   , intent( inout ) :: energy

    ! Local variables
    real( kind = 8 )    :: Rn
    real( kind = 8 )    :: Rt
    real( kind = 8 )    :: Vn
    real( kind = 8 )    :: Vt
    real( kind = 8 )    :: VnBegin
    real( kind = 8 )    :: VtBegin
    real( kind = 8 )    :: GapBegin
    real( kind = 8 )    :: Un
    real( kind = 8 )    :: Ut
    integer( kind = 4 ) :: status
    integer( kind = 4 ) :: StatusBegin

    call get_rloc( id_inter, ik, Rt, Rn, status )
    if ( status == i_noctc ) return
    if ( status == i_vnish ) return
    call get_vloc( id_inter, ik, Vt, Vn )
    call get_vlocBEGIN( id_inter, ik, VtBegin, VnBegin, GapBegin, StatusBegin )
    Un = ( 0.5d0 * VnBegin ) + ( 0.5d0 * Vn )
    Ut = ( 0.5d0 * VtBegin ) + ( 0.5d0 * Vt )
    energy = energy - ( ( Un * Rn ) + ( Ut * Rt ) )

  end subroutine comp_dissipated_energy_aux
  !!!----------------------------------------------------------------------------
  !!!----------------------------------------------------------------------------
  SUBROUTINE comp_dissipated_energy()

    IMPLICIT NONE

    INTEGER         :: ik,nfich
    REAL(kind=8)    :: energy

    energy = 0.0D0
    
    DO ik=1,nb_DKDKx
       call comp_dissipated_energy_aux( i_dkdkx, ik, energy )
    END DO
    
    DO ik=1,nb_DKJCx
       call comp_dissipated_energy_aux( i_dkjcx, ik, energy )
    END DO

    DO ik=1,nb_DKKDx
       call comp_dissipated_energy_aux( i_dkkdx, ik, energy )
    END DO
    
    DO ik=1,nb_DKPLx
       call comp_dissipated_energy_aux( i_dkplx, ik, energy )
    END DO

    DO ik=1,nb_PLPLx
       call comp_dissipated_energy_aux( i_plplx, ik, energy )
    END DO

    DO ik=1,nb_PLJCx
       call comp_dissipated_energy_aux( i_pljcx, ik, energy )
    END DO

    DO ik=1,nb_CLALp
       call comp_dissipated_energy_aux( i_clalp, ik, energy )
    END DO
    
    DO ik=1,nb_CLJCx
       call comp_dissipated_energy_aux( i_cljcx, ik, energy )
    END DO

    DO ik=1,nb_DKALp
       call comp_dissipated_energy_aux( i_dkalp, ik, energy )
    END DO

    dissipated_energy = dissipated_energy + energy

    nfich = PostCommand(i_DISSIPATED_ENERGY)%io_unit(1)
    WRITE(nfich,'(ES15.8,2(1X,ES14.7))') TPS,dissipated_energy,energy
    
  END SUBROUTINE comp_dissipated_energy
  
  !!!--------------------------------------------------------------------------------------
  !!! # 26
  !!!--------------------------------------------------------------------------------------

  FUNCTION compute_kinetic_energy()

    IMPLICIT NONE

    INTEGER                   :: ibdyty,itacty
    ! kinetic energy
    REAL(kind=8)              :: KE
    REAL(kind=8)              :: compute_kinetic_energy
    REAL(kind=8)              :: mass,gyr
    REAL(kind=8),DIMENSION(3) :: coor,I
    REAL(kind=8),DIMENSION(6) :: V,Reac,X
    REAL(kind=8)              :: Eg_pot,Eg_cin,Eg_def

    ! energy
    KE     = 0.D0
    mass   = 0.D0
    V      = 0.D0

    Eg_pot = 0.D0
    Eg_cin = 0.D0
    Eg_def = 0.D0


    !!! partie rigides

    DO ibdyty=1,nb_RBDY2

       IF( .NOT. BodyWindow(ibdyty) ) CYCLE

       mass = get_mass(ibdyty)
       gyr  = get_gyr_radius(ibdyty)

       V    = get_V(ibdyty)
       X    = get_X(ibdyty)

       Reac(1:3) = get_Reac(ibdyty) + get_Fext(ibdyty)

       KE   = KE + 0.5*mass*( (V(1)*V(1) + V(2)*V(2) ) + (V(3)*V(3)*gyr*gyr))

    END DO
    
    !!! partie deformable

    call compute_energy_mecaMAILx(Eg_cin,Eg_def,Eg_pot)
    KE = KE + Eg_cin

    compute_kinetic_energy = KE

  END FUNCTION compute_kinetic_energy

  SUBROUTINE kinetic_energy

    IMPLICIT NONE

  
    ! kinetic energy
    REAL(kind=8)  :: KE, Wpot, Wext
    ! dissipated energy (cumulated) and 
    REAL(kind=8),save :: DE=0.D0, RWext=0.d0
    
    INTEGER                   :: ibdyty,ID_RBDY2,ID_TACTY
    REAL(kind=8)              :: mass,gyr
    REAL(kind=8),DIMENSION(3) :: V,Reac,X
    INTEGER                   :: nfich

    REAL(kind=8) :: Eg_pot,Eg_cin,Eg_def,Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con

    Eg_pot = 0.D0
    Eg_cin = 0.D0
    Eg_def = 0.D0

    nfich = PostCommand(i_KINETIC_ENERGY)%io_unit(1) 
    
    KE    = 0.D0  ! kinetic energy
    Wext  = 0.D0  ! work of external forces
    Wpot  = 0.D0  ! work of gravity

    mass   = 0.D0
    V = 0.D0

    !!! partie rigides

    DO ibdyty=1,nb_RBDY2

       IF( .NOT. BodyWindow(ibdyty) ) CYCLE

       mass = get_mass(ibdyty)
       gyr  = get_gyr_radius(ibdyty) 
      
       V    = get_V(ibdyty)
       X    = get_X(ibdyty)

       Reac = get_Reac(ibdyty) + get_Fext(ibdyty)

       KE   = KE + 0.5*mass*( (V(1)*V(1) + V(2)*V(2) ) + (V(3)*V(3)*gyr*gyr))
       Wpot = Wpot + mass*( X(1)*grav1 + X(2)*grav2 )
       RWext = RWext + H*(Reac(1)*V(1) + Reac(2)*V(2) + Reac(3)*V(3))

    END DO
    
    !!! partie deformable 

    call compute_energy_mecaMAILx(Eg_cin,Eg_def,Eg_pot)

    !print*,Eg_cin,Eg_def,Eg_pot

    call compute_work_mecaMAILx(Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con)

    !print*,Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con

    KE = KE + Eg_cin
    Wext = RWext + Wg_ddl + Wg_con
    Wpot = Wpot + Wg_pot

    ! dissipation (travail (depense) - energie (stocke))
    ! pour le moment
    DE = DE + Wg_def - Eg_def
  
    ! kinetic,internal,external,potential,dissipated
    WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,KE,KE+Eg_Def,-Wext,-Wpot,DE,KE+Wg_def+Wext+Wpot
    
  END SUBROUTINE kinetic_energy

  !!!--------------------------------------------------------------------------------------
  !!! # 2
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE species_kinetic_energy

    IMPLICIT NONE
    
    INTEGER                   :: is,ibdyty
    REAL(kind=8)              :: mass,gyr
    REAL(kind=8),DIMENSION(3) :: V,Reac,coor
    INTEGER                   :: nfich
    CHARACTER(len=5)          :: behav

    nfich = PostCommand(i_SPECIES_KINETIC_ENERGY)%io_unit(1) 
    
    DO is =1,nb_species
       SKE(is)%KE = 0.D0
       SKE(is)%P  = 0.D0
       SKE(is)%PE = 0.D0
    END DO

    mass   = 0.D0
    V      = 0.D0

    DO ibdyty=1,nb_RBDY2

       call get_behav(ibdyty,1,behav)

       DO is =1,nb_species
          IF ( behav == SKE(is)%behav )THEN
             V    = get_V(ibdyty)
             mass = get_mass(ibdyty)
             coor = get_coor(ibdyty,0)
             gyr  = get_gyr_radius(ibdyty)
             Reac = get_Reac(ibdyty) + get_Fext(ibdyty)

             SKE(is)%KE = SKE(is)%KE + 0.5*mass*( (V(1)*V(1) + V(2)*V(2) ) + V(3)*V(3)*gyr*gyr)
             SKE(is)%PE = SKE(is)%PE + mass*( coor(1)*grav1 + coor(2)*grav2 ) 
             SKE(is)%P  = SKE(is)%P  + Reac(1)*V(1) + Reac(2)*V(2) + Reac(3)*V(3)

             EXIT
          END IF
       END DO
       
    END DO

    DO is =1,nb_species
       SKE(is)%DE = (SKE(is)%KE + SKE(is)%PE - SKE(is)%DE)/H
       nfich = PostCommand(i_SPECIES_KINETIC_ENERGY)%io_unit(is)
       WRITE(nfich,'(ES15.8,5(1X,ES14.7))') TPS,SKE(is)%KE,SKE(is)%PE,SKE(is)%KE+SKE(is)%PE,SKE(is)%DE,SKE(is)%P
       SKE(is)%DE = SKE(is)%KE+SKE(is)%PE
    END DO


  END SUBROUTINE species_kinetic_energy

  !!!--------------------------------------------------------------------------------------
  !!! # 23
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE solver_informations
  
    IMPLICIT NONE
    
    INTEGER      :: compteur,contacts,nfich
    REAL(kind=8) :: err1,err2,err3

    IF(nlgs_solver2D)THEN
       CALL get_nlgs2D_loop(compteur,err1,err2,err3,contacts)
    !ELSE IF(new_int_nlgs_solver)THEN
    !   CALL new_int_get_nlgs_loop(compteur,err1,err2,err3,contacts)
    ELSE IF(cpg_solver)THEN
       CALL get_cpg_loop(compteur,err1,err2,err3,contacts)
    END IF

    nfich = PostCommand(i_SOLVER_INFORMATIONS)%io_unit(1)
    WRITE(nfich,'(ES15.8,1X,I8,3(1X,ES14.7),1X,I8)') TPS,compteur,err1,err2,err3,contacts
    
  END SUBROUTINE solver_informations
  
  !!!--------------------------------------------------------------------------------------
  !!! # 24
  !!!--------------------------------------------------------------------------------------

  !fd mettre les statuts en concordance avec les modifs du solveur 
  
  SUBROUTINE dry_contact_nature

    IMPLICIT NONE
    
    INTEGER :: noctc,Wslide,Sslide,Sstick,Wstick
    INTEGER :: nfich

    noctc  = 0
    Wslide = 0
    Sslide = 0
    Sstick = 0
    Wstick = 0

    IF(nlgs_solver2D)THEN
       CALL get_nlgs2D_contact_status(noctc,Wslide,Sslide,Wstick,Sstick)
    ELSE IF(cpg_solver)THEN
       CALL get_cpg_contact_status(noctc,Wslide,Wstick)
    END IF

    nfich = PostCommand(i_DRY_CONTACT_NATURE)%io_unit(1) 

    WRITE(nfich,'(ES15.8,5(1X,I8))') TPS,noctc,Wstick+Sstick,Wslide+Sslide
    
  END SUBROUTINE dry_contact_nature
  
  !!!----------------------------------------------------------------------------
  !!! # 25
  !!!----------------------------------------------------------------------------


  !!!----------------------------------------------------------------------------
  subroutine wet_contact_nature_aux( id_inter, nb_inter, &
       nb_Wnctc, nb_Wstck, nb_slide )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    integer( kind = 4 ) :: nb_Wnctc
    integer( kind = 4 ) :: nb_Wstck
    integer( kind = 4 ) :: nb_slide 

    ! Local variables
    integer( kind = 4 ) :: icdan
    real( kind = 8 )    :: rt
    real( kind = 8 )    :: rn
    integer( kind = 4 ) :: status

    do icdan = 1, nb_inter
       !call get_loc_inter_( id_inter, icdan, rt, rn, status )
       select case( status )
       case( i_Wnctc )
          nb_Wnctc = nb_Wnctc + 1
       case( i_Wstck )
          nb_Wstck = nb_Wstck + 1
       case( i_Wslbw )
          nb_slide = nb_slide + 1
       case( i_Wslfw )
          nb_slide = nb_slide + 1
       end select
    end do

  end subroutine wet_contact_nature_aux

  
  SUBROUTINE wet_contact_nature

    IMPLICIT NONE
    
    INTEGER          :: icdan,nb_Wstck,nb_slide,nb_Wnctc
    CHARACTER(len=5) :: status
    REAL(kind=8)     :: rn,rt
    INTEGER          :: nfich
  
    nb_Wstck = 0 
    nb_slide = 0
    nb_Wnctc = 0

    call wet_contact_nature_aux( i_dkdkx, nb_DKDKx, nb_Wnctc, nb_Wstck, nb_slide )

    call wet_contact_nature_aux( i_dkjcx, nb_DKJCx, nb_Wnctc, nb_Wstck, nb_slide )
    
    call wet_contact_nature_aux( i_dkkdx, nb_DKKDx, nb_Wnctc, nb_Wstck, nb_slide )

    call wet_contact_nature_aux( i_dkplx, nb_DKPLx, nb_Wnctc, nb_Wstck, nb_slide )

    call wet_contact_nature_aux( i_dkjcx, nb_DKJCx, nb_Wnctc, nb_Wstck, nb_slide )

    call wet_contact_nature_aux( i_plplx, nb_PLPLx, nb_Wnctc, nb_Wstck, nb_slide )
    
    nfich = PostCommand(i_WET_CONTACT_NATURE)%io_unit(1)
    WRITE(nfich,'(ES15.8,3(1X,I16))') TPS,nb_Wstck,nb_slide,nb_Wnctc

  END SUBROUTINE wet_contact_nature
  

  !!!--------------------------------------------------------------------------------------
  !!! # 7
  !!!--------------------------------------------------------------------------------------

  !fd pour ne compter que sur les cd 
  subroutine coordination_number_aux_dk(id_inter,tol_rn)

    implicit none

    integer(kind=4) :: id_inter
    real(kind=8)    :: tol_rn
    
    ! Local variables
    integer(kind=4) :: verlet_size,nb_adj    
    integer(kind=4) :: icdtac
    integer(kind=4) :: iadj
    integer(kind=4) :: id_rbdy2    
    real(kind=8)    :: rn
    real(kind=8)    :: rt
    integer(kind=4) :: status

    ! on ne compte que ce qui concerne les diskx

    verlet_size = get_verlet_size(id_inter)
    if( verlet_size == 0 ) return

    do icdtac = 1, verlet_size
       
      nb_adj = get_verlet_adjsz(id_inter, icdtac)
       
      do iadj = 1, nb_adj

        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
         
        if (status == i_noctc) cycle
        if (status == i_vnish) cycle

        ID_RBDY2 = diskx2bdyty(1,icdtac)
        IF ( BodyWindow(ID_RBDY2) ) then
          IF (rn > tol_rn) THEN
            cplus(id_rbdy2) = cplus(id_rbdy2) + 1
            ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
          else if (rn < -tol_rn) then
            cmoins(id_rbdy2) =  cmoins(id_rbdy2) + 1            
            ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
          endif   
        ENDIF
      end do
    enddo
   
  end subroutine coordination_number_aux_dk
  
  !!!--------------------------------------------------------------------------------------
  subroutine coordination_number_aux_pl(id_inter,tol_rn)

    implicit none

    integer( kind = 4 ) :: id_inter
    real(kind=8)        :: tol_rn
    
    ! Local variables
    integer(kind=4)     :: verlet_size,nb_adj,id_rbdy2
    integer( kind = 4 ) :: icdtac
    integer( kind = 4 ) :: iadj
    real( kind = 8 )    :: rn
    real( kind = 8 )    :: rt
    integer( kind = 4 ) :: status

    ! on ne compte que ce qui concerne les polyg

    verlet_size = get_verlet_size(id_inter)
    if( verlet_size == 0 ) return

    do icdtac = 1, verlet_size
       
      nb_adj = get_verlet_adjsz(id_inter, icdtac)
       
      do iadj = 1, nb_adj

        call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
         
        if (status == i_noctc) cycle
        if (status == i_vnish) cycle

        ID_RBDY2 = polyg2bdyty(1,icdtac)
        IF ( BodyWindow(ID_RBDY2) ) then
          IF (rn > tol_rn) THEN
            cplus(id_rbdy2) = cplus(id_rbdy2) + 1
            ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
          else if (rn < -tol_rn) then
            cmoins(id_rbdy2) =  cmoins(id_rbdy2) + 1            
            ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
          endif   
        ENDIF
      end do
    enddo

  end subroutine coordination_number_aux_pl

  !!!--------------------------------------------------------------------------------------
  SUBROUTINE coordination_number

    IMPLICIT NONE
    INTEGER         :: ID_RBDY2
    INTEGER         :: icdtac,iantac,iadj
    INTEGER         :: nb_adj,verlet_size
    REAL(kind=8)    :: rn,rt
    INTEGER         :: nfich
    integer(kind=4) :: status

    integer( kind = 4 ) :: id_inter,nbp
    
    ! sans les zeros contact
    integer(kind=4) :: nbc0t,nbc0m,nbc0p,nbp0
    ! sans les zeros et uns contact
    integer(kind=4) :: nbc1t,nbc1m,nbc1p,nbp1
    
    ! size array is equal to nb_POLYG + nb_DISKx

    cplus  = 0
    cmoins = 0
    ctotal = 0

    !dkdkx
    
    id_inter = i_dkdkx
    verlet_size = get_verlet_size(id_inter)
    
    if ( verlet_size /= 0 ) then

      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )

          if (status == i_noctc) cycle
          if (status == i_vnish) cycle

          ID_RBDY2 = diskx2bdyty(1,icdtac)
          IF ( BodyWindow(ID_RBDY2) ) then
            IF (rn > tol_rn) THEN
              cplus(id_rbdy2) = cplus(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            else if (rn < -tol_rn) then
              cmoins(id_rbdy2) =  cmoins(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1               
            endif   
          ENDIF

          id_rbdy2 = get_verlet_ianbdy( id_inter, icdtac, iadj )

          !fd ne tape pas dans la bonne liste
          ! iantac = get_verlet_iantac( id_inter, icdtac, iadj )
          ! ID_RBDY2 = diskx2bdyty(1,iantac)


          !print*,id_inter,icdtac,iadj,id_rbdy2
          
          IF ( BodyWindow(ID_RBDY2) ) then
            IF (rn > tol_rn) THEN
              cplus(id_rbdy2) = cplus(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            else IF (rn < -tol_rn) then
              cmoins(id_rbdy2) = cmoins(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            endif   
          endif 
        enddo
      enddo
    endif     

    !les cas ou on ne compte que les dk qui sont cd 
    
    call coordination_number_aux_dk(i_dkkdx,tol_rn)

    call coordination_number_aux_dk(i_dkjcx,tol_rn)

    
    ! dkpl
    id_inter = i_dkplx    
    verlet_size = get_verlet_size(id_inter)
    
    if ( verlet_size /= 0 ) then
      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )

          if (status == i_noctc) cycle
          if (status == i_vnish) cycle

          ID_RBDY2 = diskx2bdyty(1,icdtac)        
          IF ( BodyWindow(ID_RBDY2) ) then
            IF (rn > tol_rn) THEN
              cplus(id_rbdy2) = cplus(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            ELSE IF (rn < -tol_rn) THEN
              cmoins(id_rbdy2) =  cmoins(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            endif   
          END IF
         
          id_rbdy2 = get_verlet_ianbdy( id_inter, icdtac, iadj )

          ! iantac = get_verlet_iantac( id_inter, icdtac, iadj )
          ! ID_RBDY2 = polyg2bdyty(1,iantac)
          
          IF ( BodyWindow(ID_RBDY2) ) then
            IF (rn > tol_rn) THEN
              cplus(id_rbdy2) = cplus(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            ELSE IF (rn < -tol_rn) THEN
              cmoins(id_rbdy2) =  cmoins(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            END IF
          endif
        enddo  
      END DO
    endif
   
    !!! plplx
    id_inter = i_plplx
    verlet_size = get_verlet_size(id_inter)
    
    if( verlet_size /= 0 ) then
      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn )
           
          if (status == i_noctc) cycle
          if (status == i_vnish) cycle

          ID_RBDY2 = polyg2bdyty(1,icdtac)
          IF ( BodyWindow(ID_RBDY2) ) then
            IF (rn > tol_rn) THEN
              cplus(id_rbdy2) = cplus(id_rbdy2) + 1 
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            else if (rn < -tol_rn) then
              cmoins(id_rbdy2) =  cmoins(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            endif   
          ENDIF

          id_rbdy2 = get_verlet_ianbdy( id_inter, icdtac, iadj )

          ! iantac = get_verlet_iantac( id_inter, icdtac, iadj )
          ! ID_RBDY2 = polyg2bdyty(1,iantac)
          
          IF ( BodyWindow(ID_RBDY2) ) then
            IF (rn > tol_rn) THEN
              cplus(id_rbdy2) = cplus(id_rbdy2) + 1            
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            else IF (rn < -tol_rn) then
              cmoins(id_rbdy2) = cmoins(id_rbdy2) + 1
              ctotal(id_rbdy2) = ctotal(id_rbdy2) + 1
            endif   
          endif 
        enddo
      enddo
    endif
   
    call coordination_number_aux_pl( i_pljcx,tol_rn)

    !!!-----------------------
    
    IF (nb_GRAIN /= 0) THEN


       ! nbc0 = SUM(ctotal)
       ! nbc1 = SUM(cplus)
       ! nbc2 = SUM(cmoins)
       ! val3 = 1.D0/REAL(nb_GRAIN,8)

       
       !fd on compte les grains actifs et on ne gere pas les clusters !!

       nbc0t=0
       nbc0p=0
       nbc0m=0
       nbp0 =0
       
       nbc1t=0
       nbc1p=0
       nbc1m=0
       nbp1 =0
       
       do icdtac=1,size(ctotal)
         if (ctotal(icdtac) > 0) then
           nbc0t=nbc0t+ctotal(icdtac)
           nbc0p=nbc0p+cplus(icdtac)
           nbc0m=nbc0m+cmoins(icdtac)            
           nbp0 =nbp0+1
           if (ctotal(icdtac) > 1) then
             nbc1t=nbc1t+ctotal(icdtac) 
             nbc1p=nbc1p+cplus(icdtac)
             nbc1m=nbc1m+cmoins(icdtac)            
             nbp1 =nbp1+1
           endif   
         endif  
       enddo
      
       nfich = PostCommand(i_COORDINATION_NUMBER)%io_unit(1)

       ! ct: contact actifs rn/=0, c+ contact rn>0, c- contacts rn<0
       ! N: nb particules, N-N0 nb particules sans flottantes, N-N0-N1 nb particules sans flottantes et 1 contact

       if (i_window == 0) then
         nbp=nb_rbdy2
       else
         nbp=nb_inside  
       endif
       !                                    N   N-N0 2ct   2c+   2c-   N-N0-N1 2ct-N1 2c+-N1 2c--N1
       WRITE(nfich,'(ES15.8,9(1x,I7))') TPS,nbp,nbp0,nbc0t,nbc0p,nbc0m,nbp1   ,nbc1t ,nbc1p ,nbc1m

      END IF
    
  END SUBROUTINE coordination_number

  !!!--------------------------------------------------------------------------------------
  !!! # 1
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE average_velocity_evolution

    IMPLICIT NONE
    
    REAL(kind=8),DIMENSION(3) :: Vbegin,Vmean
    INTEGER                   :: ibdyty,nb_IN,ID_RBDY2,ID_TACTY
    CHARACTER(len=5)          :: color
    
    INTEGER                   :: nfich

    nb_IN = 0
    Vmean = 0.D0

    DO ibdyty=1,nb_RBDY2
       
       IF( .NOT. BodyWindow(ibdyty) ) CYCLE

       color  = get_color(ibdyty,1)

       IF ( color == AVcolor ) THEN
          
          Vbegin = get_V(ibdyty)
       
          Vmean  = Vmean + Vbegin
          
          nb_IN = nb_IN + 1
       END IF

    END DO
    
    IF( nb_IN /= 0 ) Vmean = Vmean/REAL(nb_IN,8)
       
    nfich = PostCommand(i_AVERAGE_VELOCITY_EVOLUTION)%io_unit(1)

    WRITE(nfich,'(ES15.8,4(1X,ES14.7))') TPS,Vmean(1),Vmean(2),Vmean(3),SQRT(Vmean(1)*Vmean(1)+Vmean(2)*Vmean(2))
    
  END SUBROUTINE average_velocity_evolution
  
  !!!-----------------------------------------------------------------------------------
  !!! # 5
  !!!-----------------------------------------------------------------------------------
  !!! SUBROUTINE normal_contact_distribution
  !!!
  !!! nb_STrac : number of contact with a normal force larger than the mean normal force in traction 
  !!! nb_WTrac : number of contact with a normal force less than the mean normal force in traction
  !!!
  !!! nb_SComp : number of contact with a normal force larger than the mean normal force in compression
  !!! nb_WComp : number of contact with a normal force less than the mean normal force in compression
  !!!
  !!!-----------------------------------------------------------------------------------
  SUBROUTINE normal_contact_distribution

    IMPLICIT NONE
    
    INTEGER :: i,j,icdan,iadj,icdtac,nb_adj,ik,ikk,ick,itk
    INTEGER :: nfich,nb_STrac,nb_SComp,nb_WTrac,nb_WComp,nb_CDAN,nb_iCDAN,nb_Trac,nb_Comp

    REAL(kind=8) :: MEAN_T_RLN,MEAN_C_RLN

    REAL(kind=8)                            :: sect,isect,rln,rlt
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: C_TUC,C_NUC,C_COOR
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: T_NUC,T_TUC,T_COOR

    REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: C_RLOC,T_RLOC

    integer, dimension(7) :: inters_id
    integer :: id_inter, status, verlet_size

    real(kind=8),dimension(2) :: coor,tuc,nuc

    
    MEAN_T_RLN = 0.D0
    MEAN_C_RLN = 0.D0

    nb_STrac = 0
    nb_SComp = 0
    nb_WTrac = 0
    nb_WComp = 0
    nb_Trac = 0
    nb_Comp = 0

    inters_id = (/ i_dkdkx, i_dkjcx,                   &
                   i_dkkdx, i_dkplx, i_plplx, i_pljcx, &
                   i_clalp                             /)

    nb_CDAN = 0
    do i = 1,size(inters_id)
      nb_CDAN = nb_CDAN + get_nb_verlets( inters_id(i) )
    end do

    IF (nb_CDAN == 0) RETURN

    ncd_unit = ncd_unit + 1
    
    WRITE( ncd_name1(29:35),'(I7.7)') ncd_unit
    WRITE( ncd_name2(17:23),'(I7.7)') ncd_unit

    sect   = PI_g/REAL(Nsect,8)
    isect  = 1.0D+00/sect

    !!!----------------------------------------

    IF (ALLOCATED(C_TUC)) DEALLOCATE(C_TUC)
    ALLOCATE(C_TUC(2,nb_CDAN))
    
    IF (ALLOCATED(C_NUC)) DEALLOCATE(C_NUC)
    ALLOCATE(C_NUC(2,nb_CDAN))
    
    IF (ALLOCATED(C_COOR)) DEALLOCATE(C_COOR)
    ALLOCATE(C_COOR(2,nb_CDAN))

    !!!----------------------------------------

    IF (ALLOCATED(T_TUC)) DEALLOCATE(T_TUC)
    ALLOCATE(T_TUC(2,nb_CDAN))
    
    IF (ALLOCATED(T_NUC)) DEALLOCATE(T_NUC)
    ALLOCATE(T_NUC(2,nb_CDAN))
    
    IF (ALLOCATED(T_COOR)) DEALLOCATE(T_COOR)
    ALLOCATE(T_COOR(2,nb_CDAN))

    !!!----------------------------------------
    
    IF (ALLOCATED(T_RLOC)) DEALLOCATE(T_RLOC)
    ALLOCATE(T_RLOC(nb_CDAN))

    IF (ALLOCATED(C_RLOC)) DEALLOCATE(C_RLOC)
    ALLOCATE(C_RLOC(nb_CDAN))

    C_ORI  = 0.D0
    SC_ORI = 0.D0
    WC_ORI = 0.D0
    C_COOR = 0.D0

    T_ORI  = 0.D0
    ST_ORI = 0.D0
    WT_ORI = 0.D0
    T_COOR = 0.D0

    ick       = 0
    itk       = 0
    iadj      = 0
    icdtac    = 0

    do i = 1, size(inters_id)
 
      id_inter = inters_id(i)

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

         nb_adj = get_verlet_adjsz(id_inter, icdtac)

         do iadj = 1, nb_adj

            call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln )

            if (status == i_noctc) cycle
            if (status == i_vnish) cycle
            
            call get_verlet_local_frame(id_inter, icdtac, iadj, coor, tuc, nuc)

            Winside = .FALSE.
            SELECT CASE(i_window)
            CASE(0)
               Winside = .TRUE.
            CASE(1,2)
               Winside = am_i_inside(COOR(1),COOR(2))
            CASE default
            END SELECT

            if (.not. Winside) cycle

            ! on ejecte les grains a force nulle volontairement car ils ne participent pas
            
            if ( rln > tol_rn ) then
               ick = ick + 1
               C_rloc(ick) = rln
               c_coor(1:2, ick) = coor
               c_tuc(1:2, ick) = tuc
               c_nuc(1:2, ick) = nuc
            else if (rln < -tol_rn) then
               itk = itk + 1
               T_rloc(itk) = rln
               t_coor(1:2, itk) = coor
               t_tuc(1:2, itk) = tuc
               t_nuc(1:2, itk) = nuc
            end if

         end do
      end do

    end do

    nb_Trac = itk
    nb_Comp = ick

    DO ick = 1,nb_Comp
       MEAN_C_RLN = MEAN_C_RLN + C_rloc(ick)
    END DO
    MEAN_C_RLN = MEAN_C_RLN/REAL(MAX(1,nb_Comp),8)

    DO itk = 1,nb_Trac
       MEAN_T_RLN = MEAN_T_RLN + T_rloc(itk)
    END DO
    MEAN_T_RLN = MEAN_T_RLN/REAL(MAX(1,nb_Trac),8)

    !!!
    !!! COMPRESSION FORCES (RLN > 0)
    !!!
    
    DO ick=1,nb_Comp

       ! fait avant
       ! Winside = .FALSE.
       ! SELECT CASE(i_window)
       ! CASE(0)
       !    Winside = .TRUE.
       ! CASE(1,2)
       !    Winside = am_i_inside(C_COOR(1,ick),C_COOR(2,ick))
       ! CASE default
       ! END SELECT

       ! IF (Winside) THEN

          IF (C_NUC(2,ick) > 0.D0) THEN

             j = INT(ACOS(C_NUC(1,ick))*isect) + 1
             
             C_ORI(j)       = C_ORI(j) + 1
             C_ORI(j+Nsect) = C_ORI(j+Nsect) + 1

             IF (C_rloc(ick) > MEAN_C_RLN) THEN

                SC_ORI(j)       = SC_ORI(j) + 1
                SC_ORI(j+Nsect) = SC_ORI(j+Nsect) + 1

                nb_SComp = nb_SComp + 1

             ELSE

                WC_ORI(j)       = WC_ORI(j) + 1
                WC_ORI(j+Nsect) = WC_ORI(j+Nsect) + 1

                nb_WComp = nb_WComp + 1

             END IF

          ELSE IF (C_NUC(2,ick) < 0.D0) THEN

             ! on considere -nuc
             
             j = INT(ACOS(-C_NUC(1,ick))*isect) + 1

             C_ORI(j)       = C_ORI(j) + 1
             C_ORI(j+Nsect) = C_ORI(j+Nsect) + 1

             IF (C_rloc(ick) > MEAN_C_RLN) THEN

                SC_ORI(j)       = SC_ORI(j) + 1
                SC_ORI(j+Nsect) = SC_ORI(j+Nsect) + 1

                nb_SComp = nb_SComp + 1

             ELSE

                WC_ORI(j)       = WC_ORI(j) + 1
                WC_ORI(j+Nsect) = WC_ORI(j+Nsect) + 1

                nb_WComp = nb_WComp + 1

             END IF
             
             !fd erreur constatee le 18/02/2009
             !fd a cause d'erreur d'arrondie on peut avoir un j qui 
             !fd est inferieur ou egal a nsect d'ou l'ajout du max
             !fd si ensuite ca plante
             
             ! j = max(NSECT,INT((DPI-ACOS(C_NUC(1,ick)))*isect)) + 1

             ! j = min(j,2*nsect)
             
             ! C_ORI(j)       = C_ORI(j) + 1
             ! C_ORI(j-Nsect) = C_ORI(j-Nsect) + 1

             ! IF (C_rloc(ick) > MEAN_C_RLN) THEN

             !    SC_ORI(j)       = SC_ORI(j) + 1
             !    SC_ORI(j-Nsect) = SC_ORI(j-Nsect) + 1

             !    nb_SComp = nb_SComp + 1

             ! ELSE

             !    WC_ORI(j)       = WC_ORI(j) + 1
             !    WC_ORI(j-Nsect) = WC_ORI(j-Nsect) + 1

             !    nb_WComp = nb_WComp + 1

             ! END IF

          ELSE

             C_ORI(1)       = C_ORI(1) + 1
             C_ORI(1+Nsect) = C_ORI(1+Nsect) + 1

             IF (C_rloc(ick) > MEAN_C_RLN) THEN

                SC_ORI(1)       = SC_ORI(1) + 1
                SC_ORI(1+Nsect) = SC_ORI(1+Nsect) + 1

                nb_SComp = nb_SComp + 1

             ELSE

                WC_ORI(1)       = WC_ORI(1) + 1
                WC_ORI(1+Nsect) = WC_ORI(1+Nsect) + 1

                nb_WComp = nb_WComp + 1

             END IF
          END IF
       ! END IF
    END DO
    
    !!!
    !!! TRACTION FORCES (RLN < 0)
    !!!

    DO itk=1,nb_Trac

       ! Winside = .FALSE.
       ! SELECT CASE(i_window)
       ! CASE(0)
       !    Winside = .TRUE.
       ! CASE(1,2)
       !    Winside = am_i_inside(T_COOR(1,itk),T_COOR(2,itk))
       ! CASE default
          
       ! END SELECT

       ! IF (Winside) THEN

          IF (T_NUC(2,itk).GT.0.D0) THEN

             j = INT(ACOS(T_NUC(1,itk))*isect) + 1
             
             T_ORI(j)       = T_ORI(j) + 1
             T_ORI(j+Nsect) = T_ORI(j+Nsect) + 1

             IF (T_rloc(itk) < MEAN_T_RLN) THEN

                ST_ORI(j)       = ST_ORI(j) + 1
                ST_ORI(j+Nsect) = ST_ORI(j+Nsect) + 1

                nb_STrac = nb_STrac + 1

             ELSE

                WT_ORI(j)       = WT_ORI(j) + 1
                WT_ORI(j+Nsect) = WT_ORI(j+Nsect) + 1

                nb_WTrac = nb_WTrac + 1

             END IF

          ELSE IF (T_NUC(2,itk).LT. 0.D0) THEN
             ! on considere -nuc
             
             j = INT(ACOS(-T_NUC(1,itk))*isect) + 1

             T_ORI(j)       = T_ORI(j) + 1
             T_ORI(j+Nsect) = T_ORI(j+Nsect) + 1

             IF (T_rloc(ick) < MEAN_T_RLN) THEN

                ST_ORI(j)       = ST_ORI(j) + 1
                ST_ORI(j+Nsect) = ST_ORI(j+Nsect) + 1

                nb_STrac = nb_STrac + 1

             ELSE

                WT_ORI(j)       = WT_ORI(j) + 1
                WT_ORI(j+Nsect) = WT_ORI(j+Nsect) + 1

                nb_WTrac = nb_WTrac + 1

             END IF
              
             ! !fd erreur constatee le 18/02/2009
             ! !fd a cause d'erreur d'arrondie on peut avoir un j qui 
             ! !fd est inferieur ou egal a nsect d'ou l'ajout du max
             ! !fd si ensuite ca plante

             ! j = max(NSECT,INT((DPI-ACOS(T_NUC(1,itk)))*isect)) + 1

             ! T_ORI(j)       = T_ORI(j) + 1
             ! T_ORI(j-Nsect) = T_ORI(j-Nsect) + 1

             ! IF (T_rloc(itk).LT.MEAN_T_RLN) THEN

             !    ST_ORI(j)       = ST_ORI(j) + 1
             !    ST_ORI(j-Nsect) = ST_ORI(j-Nsect) + 1

             !    nb_STrac = nb_STrac + 1

             ! ELSE

             !    WT_ORI(j)       = WT_ORI(j) + 1
             !    WT_ORI(j-Nsect) = WT_ORI(j-Nsect) + 1

             !    nb_WTrac = nb_WTrac + 1

             ! END IF

          ELSE

             T_ORI(1)       = T_ORI(1) + 1
             T_ORI(1+Nsect) = T_ORI(1+Nsect) + 1

             IF (T_rloc(itk) < MEAN_T_RLN) THEN

                ST_ORI(1)       = ST_ORI(1) + 1
                ST_ORI(1+Nsect) = ST_ORI(1+Nsect) + 1

                nb_STrac = nb_STrac + 1

             ELSE

                WT_ORI(1)       = WT_ORI(1) + 1
                WT_ORI(1+Nsect) = WT_ORI(1+Nsect) + 1

                nb_WTrac = nb_WTrac + 1

             END IF
          END IF
       ! END IF
    END DO

!!! NORMALISATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     C_ORI =  C_ORI/(2.*REAL(max(1,nb_Comp),8))
    WC_ORI = WC_ORI/(2.*REAL(max(1,nb_WComp),8))
    SC_ORI = SC_ORI/(2.*REAL(max(1,nb_SComp),8))

     T_ORI =  T_ORI/(2.*REAL(max(1,nb_Trac),8))
    WT_ORI = WT_ORI/(2.*REAL(max(1,nb_WTrac),8))
    ST_ORI = ST_ORI/(2.*REAL(max(1,nb_STrac),8))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nfich = PostCommand(i_NORMAL_CONTACT_DISTRIBUTION)%io_unit(2)

    OPEN(unit=nfich,file=ncd_name2,status='REPLACE')

    DO j=1,Nsect
       WRITE(nfich,'(7(1X,ES14.7))') sect*(j-0.5-Nsect),&
            isect*C_ORI(Nsect+j),isect*SC_ORI(Nsect+j),isect*WC_ORI(Nsect+j),&
            isect*T_ORI(Nsect+j),isect*ST_ORI(Nsect+j),isect*WT_ORI(Nsect+j)
    END DO
    
    DO j=1,Nsect
       WRITE(nfich,'(7(1X,ES14.7))') sect*(j-0.5), &
            isect*C_ORI(j),isect*SC_ORI(j),isect*WC_ORI(j),&
            isect*T_ORI(j),isect*ST_ORI(j),isect*WT_ORI(j)
    END DO

    CLOSE(nfich)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nfich = PostCommand(i_NORMAL_CONTACT_DISTRIBUTION)%io_unit(1)


    OPEN(unit=nfich,file=ncd_name1,status='REPLACE')

    DO i=1,DNsect
       WRITE(nfich,'(12(1X,ES14.7))') COS(sect*(i-1))* C_ORI(i) ,SIN(sect*(i-1))* C_ORI(i), & 
                                      COS(sect*(i-1))*WC_ORI(i) ,SIN(sect*(i-1))*WC_ORI(i), &
                                      COS(sect*(i-1))*SC_ORI(i) ,SIN(sect*(i-1))*SC_ORI(i), &
                                      COS(sect*(i-1))* T_ORI(i) ,SIN(sect*(i-1))* T_ORI(i), & 
                                      COS(sect*(i-1))*WT_ORI(i) ,SIN(sect*(i-1))*WT_ORI(i), &
                                      COS(sect*(i-1))*ST_ORI(i) ,SIN(sect*(i-1))*ST_ORI(i)

       WRITE(nfich,'(12(1X,ES14.7))') COS(sect*i)* C_ORI(i) ,SIN(sect*i)* C_ORI(i), & 
                                      COS(sect*i)*WC_ORI(i) ,SIN(sect*i)*WC_ORI(i), &
                                      COS(sect*i)*SC_ORI(i) ,SIN(sect*i)*SC_ORI(i), &
                                      COS(sect*i)* T_ORI(i) ,SIN(sect*i)* T_ORI(i), & 
                                      COS(sect*i)*WT_ORI(i) ,SIN(sect*i)*WT_ORI(i), &
                                      COS(sect*i)*ST_ORI(i) ,SIN(sect*i)*ST_ORI(i)
    END DO
    
    WRITE(nfich,'(12(1X,ES14.7))') C_ORI(1),0.,WC_ORI(1),0.,SC_ORI(1),0.,T_ORI(1),0.,WT_ORI(1),0.,ST_ORI(1),0.

    CLOSE(nfich)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    DEALLOCATE(C_NUC)
    DEALLOCATE(C_TUC)
    DEALLOCATE(C_COOR)
    DEALLOCATE(C_RLOC)

    DEALLOCATE(T_NUC)
    DEALLOCATE(T_TUC)
    DEALLOCATE(T_COOR)
    DEALLOCATE(T_RLOC)

  END SUBROUTINE normal_contact_distribution

  !!!--------------------------------------------------------------------------------------
  !!! # 6
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE contact_force_distribution

    IMPLICIT NONE

    INTEGER      :: icdtac,nb_adj,iadj,incdan
    INTEGER      :: ik,nb_CDAN,icdan,alpha,nfich,nb_act
    REAL(kind=8) :: rln,rlt,rls
    REAL(kind=8) :: Rmag,Rmean,Rmax,Rnmean,Rnmax,Rnmin

    integer :: status, i, id_inter, verlet_size
    integer, dimension(7) :: inters_id
   
    REAL(kind=8),DIMENSION(:),ALLOCATABLE :: cfdRmag,cfdRn
    
    nfich = PostCommand(i_CONTACT_FORCE_DISTRIBUTION)%io_unit(1) 

    cfd_unit = cfd_unit + 1

    IF( cfd_unit < 10000000)THEN
       WRITE(cfd_name1(36:42),'(I7.7)') cfd_unit
    ELSE
       CALL LOGMES(' @ The number of files is exceeded')
       CALL LOGMES(' @ The contact cannot be executed anymore')
       RETURN
    END IF

    inters_id = (/ i_dkdkx, i_dkjcx,                   &
                   i_dkkdx, i_dkplx, i_plplx, i_pljcx, &
                   i_clalp                             /)

    nb_CDAN = 0
    do i = 1, size(inters_id)
      nb_CDAN = nb_CDAN + get_nb_verlets( inters_id(i) )
    end do

    Rmean  = 0.D0
    Rnmean = 0.D0
    Rmax   =-1.0D+24
    Rnmax  =-1.0D+24
    Rnmin  = 1.0D+24
    icdan  = 0
    incdan = 0

    IF(nb_CDAN ==0) RETURN
    
    ALLOCATE(cfdRmag(nb_CDAN))


    ALLOCATE(cfdRn(nb_CDAN))

    do i = 1,size(inters_id)

      id_inter = inters_id(i)

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

           call get_verlet_rloc(id_inter, icdtac, iadj, status, rlt, rln)

           Rmag = sqrt( rln*rln + rlt*rlt )
           if ( Rmag > epsilon(1.d0)) then
              icdan = icdan+1
              cfdRmag(icdan) = Rmag
              Rmean = Rmean + Rmag
              Rmax  = MAX(Rmax,Rmag) 
           end if
           if ( ABS(rln) > epsilon(1.d0)) then
              incdan = incdan+1
              cfdRn(incdan) = rln
              Rnmean = Rnmean + rln
              Rnmax  = MAX(Rnmax,rln) 
              Rnmin  = MIN(Rnmin,rln) 
           end if
        end do

      end do

    end do
   
    nb_act = icdan

    IF ( nb_act /= 0 ) Rmean = Rmean/REAL(nb_act,8)
    IF ( Rmax == 0.D0) Rmax = 1.D0

    IF (Rmean.EQ.0.D0) RETURN
    
    Fnumber = 0
   
    DO icdan=1,nb_act
       Rmag = cfdRmag(icdan)
       ik = INT(Fsect*Rmag/Rmax) + 1
       ik = MIN(Fsect,ik)
       Fnumber(ik) = Fnumber(ik) + 1
    END DO

    ! la force normale 
    
    nb_act = incdan
    IF ( nb_act /= 0 ) Rnmean = Rnmean/REAL(nb_act,8)
    
    Rnmax = Rnmax - Rnmin
    IF ( Rnmax == 0.D0) Rnmax = 1.D0
    
    FNnumber = 0
   
    DO icdan=1,nb_act
       rln = cfdRn(icdan) - Rnmin
       ik = INT(Fsect*rln/Rnmax) + 1
       ik = MIN(Fsect,ik)
       FNnumber(ik) = FNnumber(ik) + 1
    END DO

    OPEN(unit=nfich,file=trim(location(cfd_name1)),status= 'REPLACE')
    
    DO ik = 1,Fsect
       WRITE(nfich,'(1X,ES14.7,1X,I6,1X,ES14.7,1X,I6)') &
            (Rmax/Rmean)*(ik-0.5)/REAL(Fsect,8),Fnumber(ik), &
            (Rnmax/Rnmean)*(ik-0.5)/REAL(Fsect,8)-Rnmin/Rnmean,FNnumber(ik)
    END DO

    CLOSE(nfich)

    DEALLOCATE(cfdRmag)
    DEALLOCATE(cfdRn)

    
  END SUBROUTINE contact_force_distribution
  
  !!!-----------------------------------------------------------------------------------
  !!! # 27
  !!!-----------------------------------------------------------------------------------

  !mr TO DO: adapt for post-data.

  subroutine violation_evolution_aux( id_inter, nb_inter, max_gapBegin, total_gapBegin, max_gap, total_gap )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    real( kind = 8)     :: max_gapBegin
    real( kind = 8)     :: total_gapBegin
    real( kind = 8)     :: max_gap,total_gap

    ! Local variables
    integer( kind = 4 )  :: icdan
    real( kind = 8 )     :: gapBegin
    real( kind = 8 )     :: gap

    do icdan = 1, nb_inter
       call get_gaps( id_inter, icdan, gapBegin, gap )

       gapBegin = MIN( 0.D0, gapBegin )
       gap      = MIN( 0.D0, gap )

       total_gapBegin = total_gapBegin - gapBegin     
       total_gap      = total_gap      - gap     

       max_gapBegin = MAX( max_gapBegin,-gapBegin )
       max_gap      = MAX( max_gap, -gap )
    end do

  end subroutine violation_evolution_aux

  SUBROUTINE violation_evolution

    IMPLICIT NONE
    
    INTEGER      :: icdan,nb_CDAN
    REAL(kind=8) :: gapBegin,gap
    REAL(kind=8) :: max_gapBegin,total_gapBegin,max_gap,total_gap
    
    INTEGER      :: nfich

    total_gapBegin = 0.D0
    max_gapBegin   = 0.D0

    total_gap = 0.D0
    max_gap   = 0.D0
    
    call violation_evolution_aux( i_dkdkx, nb_DKDKx, max_gapBegin, total_gapBegin, max_gap, total_gap )

    call violation_evolution_aux( i_dkjcx, nb_DKJCx, max_gapBegin, total_gapBegin, max_gap, total_gap )

    call violation_evolution_aux( i_dkkdx, nb_DKKDx, max_gapBegin, total_gapBegin, max_gap, total_gap )

    call violation_evolution_aux( i_plplx, nb_PLPLx, max_gapBegin, total_gapBegin, max_gap, total_gap )

    call violation_evolution_aux( i_pljcx, nb_PLJCx, max_gapBegin, total_gapBegin, max_gap, total_gap )

    nb_CDAN = nb_PLJCx + nb_PLPLx + nb_DKJCx + nb_DKDKx + nb_DKKDx

    !! deformables 

    call violation_evolution_aux( i_clalp, nb_CLALp, max_gapBegin, total_gapBegin, max_gap, total_gap )

    call violation_evolution_aux( i_dkalp, nb_DKALp, max_gapBegin, total_gapBegin, max_gap, total_gap )

    call violation_evolution_aux( i_cljcx, nb_CLJCx, max_gapBegin, total_gapBegin, max_gap, total_gap )

    nb_CDAN = nb_CDAN + nb_CLALp + nb_DKALp + nb_CLJCx

    nb_CDAN = MAX(nb_CDAN,1)

    nfich = PostCommand(i_VIOLATION_EVOLUTION)%io_unit(1) 

    WRITE(nfich,'(ES15.8,4(1X,ES14.7))') TPS,total_gapBegin/REAL(nb_CDAN,8),total_gap/REAL(nb_CDAN,8),max_gapBegin,max_gap
   
  END SUBROUTINE violation_evolution

  !!!----------------------------------------------
  !!! # 12
  !!!----------------------------------------------

  subroutine doublet_interactions_aux( id_inter, nb_inter, contact1_bdyty, contact2_bdyty, &
                                       gapTT, rln, rlt, vln, vlt )
    
    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    integer( kind = 4 ), dimension( : , : ), allocatable :: contact1_bdyty
    integer( kind = 4 ), dimension( : , : ), allocatable :: contact2_bdyty

    real( kind = 8 ), intent( out ) :: gapTT
    real( kind = 8 ), intent( out ) :: rln
    real( kind = 8 ), intent( out ) :: rlt
    real( kind = 8 ), intent( out ) :: vln
    real( kind = 8 ), intent( out ) :: vlt
    
    ! Local variables
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: icdtac
    integer( kind = 4 ) :: iadj
    integer( kind = 4 ) :: iantac
    integer( kind = 4 ) :: status

    do icdan = 1, nb_inter
       call this2verlet( id_inter, icdan, icdtac, iadj )
       iantac = get_verlet_iantac( id_inter, icdtac, iadj )

       IF ( ( doubletbdyty( 1 ) == contact1_bdyty( 1, icdtac ) ) .AND. &
            ( doubletbdyty( 2 ) == contact2_bdyty( 1, iantac ) ) ) THEN
          gapTT = get_verlet_gapTT( id_inter, icdtac, iadj )
          call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln )
          call get_verlet_vloc( id_inter, icdtac, iadj, vlt, vln )
          exit
       END IF
    END DO    
    
  end subroutine doublet_interactions_aux
  
  SUBROUTINE doublet_interactions

    IMPLICIT NONE
    
    REAL(kind=8)    :: gapTT,rln,rlt,vln,vlt
    INTEGER         :: nfich
    integer(kind=4) :: status

    integer ( kind = 4 ) :: id_inter

    gapTT = 0.D0
    rln   = 0.D0
    rlt   = 0.D0
    vln   = 0.D0
    vlt   = 0.D0

    SELECT CASE(doublet_type)
    CASE('DKDKx')

       id_inter = i_dkdkx

       call doublet_interactions_aux( id_inter, nb_DKDKx, diskx2bdyty, diskx2bdyty, &
            gapTT, rln, rlt, vln, vlt )

    CASE('DKJCx')

       id_inter = i_dkjcx

       call doublet_interactions_aux( id_inter, nb_DKJCx, diskx2bdyty, joncx2bdyty, &
            gapTT, rln, rlt, vln, vlt )

    CASE('PLPLx')

       id_inter = i_plplx

       call doublet_interactions_aux( id_inter, nb_DKJCx, polyg2bdyty, polyg2bdyty, &
            gapTT, rln, rlt, vln, vlt )

    CASE('PLJCx')

       id_inter = i_pljcx

       call doublet_interactions_aux( id_inter, nb_DKJCx, polyg2bdyty, joncx2bdyty, &
            gapTT, rln, rlt, vln, vlt )

    END SELECT
    
    nfich = PostCommand( i_DOUBLET_INTERACTIONS )%io_unit( 1 )
    WRITE( nfich, '(ES15.8,5(1X,ES14.7))' ) TPS, gapTT, rln / H, rlt / H, vln, vlt
    
  END SUBROUTINE doublet_interactions

  !!!--------------------------------------------------------------------------------------
  !!! # 28
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE PLPLx_analysis

    IMPLICIT NONE

    INTEGER  :: nfich
    INTEGER  :: ctc_double,ctc_simple
    
    call faterr('postpro_2D::plplx_analysis','not re-implemented')
    nfich = PostCommand(i_PLPLx_ANALYSIS)%io_unit(1) 
    
    !CALL get_contact_dist_PLPLx(ctc_simple,ctc_double)
    
    WRITE(nfich,'(ES15.8,1X,I8,1X,I8)') TPS,ctc_simple,ctc_double
    
  END SUBROUTINE PLPLx_analysis
  
  !!!----------------------------------------------------------------------------
  !!! # 14
  !!!----------------------------------------------------------------------------
  SUBROUTINE quasi_sliding_contact

    IMPLICIT NONE
    
    INTEGER         :: lawnb,nb_limit,nb_actif,nb_CDAN
    REAL(kind=8)    :: rln,rlt,fric,normT,treshold_sup,treshold_inf
    integer(kind=4) :: statusBEGIN
    INTEGER         :: nfich

    integer :: inters_id(5), i, id_inter, icdtac, nb_adj, iadj, verlet_size
 
    nb_limit = 0
    nb_actif = 0

    nb_CDAN  = 0

    inters_id = (/ i_plplx, i_pljcx, i_dkdkx, i_dkjcx, &
                   i_dkkdx                             /)

    do i = 1, size(inters_id)

      id_inter = inters_id(i)

      nb_CDAN = nb_CDAN + get_nb_verlets( id_inter )

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

           lawnb = get_verlet_tact_lawnb(id_inter, icdtac, iadj)

           call get_verlet_rloc( id_inter, icdtac, iadj, statusBEGIN, rlt, rln )

           fric = get_fric( lawnb, statusBEGIN )

           if (dabs(rln) < tol_rn) cycle

           nb_actif = nb_actif + 1

           normT = ABS( rlt )

           treshold_sup = fric * rln
           treshold_inf = fric * rln * ( 1.0 - fraction )

           if ( normT < treshold_inf ) cycle
           if ( normT > treshold_sup ) cycle

           nb_limit = nb_limit + 1
        end do

      end do

    end do

    nb_actif = MAX( nb_actif, 1 )

    nfich = PostCommand(i_QUASI_SLIDING_CONTACT)%io_unit(1) 
    WRITE(nfich,'(ES15.8,1X,I10,1X,ES14.7)') TPS,nb_limit,nb_limit/REAL(nb_actif,8)
   
  END SUBROUTINE quasi_sliding_contact
  
  !!!--------------------------------------------------------------------------------
  !!! # 21
  !!!--------------------------------------------------------------------------------
  SUBROUTINE compute_bounded_sets

    IMPLICIT NONE

    REAL(kind=8),DIMENSION(3) :: reac,V
    INTEGER                   :: i,j,nb
    INTEGER                   :: nfich

    DO i=1,nb_bounded_sets
       reac = 0.D0
       V    = 0.D0
       nb   = 0
       DO j = bounded_set(i)%idmin,bounded_set(i)%idmax
          reac = reac + get_reac(j)
          V    = V    + get_V(j)
          nb = nb + 1
       END DO
       V    = V/REAL(nb,8)
       nfich = PostCommand(i_NEW_BOUNDED_SETS)%io_unit(i)
       WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,reac(1),reac(2),reac(3),V(1),V(2),V(3)
    END DO

  END SUBROUTINE compute_bounded_sets

  !!!--------------------------------------------------------------------------------
  !!! # 33
  !!!--------------------------------------------------------------------------------

  subroutine cloud_analysis_aux( id_inter, nb_inter )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter

    ! Local variables
    integer( kind = 4 ) :: ik
    real( kind = 8 )    :: rlt
    real( kind = 8 )    :: rln
    integer( kind = 4 ) :: status
    integer( kind = 4 ) :: nfich

    do ik = 1, nb_inter
       call get_rloc( id_inter, ik, rlt, rln, status )
       nfich = PostCommand( i_CLOUD_ANALYSIS )%io_unit( 1 )
       write( nfich, '(2(1X,ES14.7))' ) rlt, rln
    end do

  end subroutine cloud_analysis_aux
  !!!--------------------------------------------------------------------------------  
  SUBROUTINE cloud_analysis

    IMPLICIT NONE
    REAL(kind=8)    :: rlt,rln
    INTEGER         :: ik,nfich
    integer(kind=4) :: status

    call cloud_analysis_aux( i_dkdkx, nb_DKDKx )

    call cloud_analysis_aux( i_dkjcx, nb_DKJCx )

    call cloud_analysis_aux( i_dkplx, nb_DKPLx )

    call cloud_analysis_aux( i_dkkdx, nb_DKKDx )

    call cloud_analysis_aux( i_plplx, nb_PLPLx )

    call cloud_analysis_aux( i_pljcx, nb_PLJCx )

    call cloud_analysis_aux( i_clalp, nb_CLALp )

  END SUBROUTINE cloud_analysis
  

  !!!------------------------------------------------------------------
  !!! # 34
  !!!------------------------------------------------------------------

  subroutine compute_SIGMA(id_inter, icdtac, iadj, H, SIGMA)
    implicit none
    integer     , intent(in) :: id_inter, icdtac, iadj
    real(kind=8), intent(in) :: H
    real(kind=8), dimension(2,2), intent(inout) :: SIGMA
    ! Local variables
    integer :: ID_RBDY2, status !fd, ID_TACTY
    real(kind=8), dimension(2) :: Fik, Lik, ptc, tuc, nuc
    real(kind=8), dimension(3) :: coorcd,cooran
    real(kind=8) :: rln, rlt

    !TODO: should test body type support and switch between RBDY2/MBD2D/MAILx

    !fd sigma est initialise au dessus

    
    !fd par ce test on vire la contribution des contacts qui sont au bord du domaine
    !fd ne contribuent que les contacts dans le domaine

    if( id_inter /= i_dkdkx .and. id_inter/=i_plplx .and. id_inter/=i_dkplx ) return

    call get_verlet_local_frame(id_inter, icdtac, iadj, ptc, tuc, nuc )
    call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln )

    Fik = ( rln * nuc + rlt * tuc )

    ID_RBDY2 = get_verlet_icdbdy( id_inter, icdtac)

    !fd les bras de levier sont par rapport au centre d'inertie

    coorcd = get_coor( ID_RBDY2, 0 )    

    ID_RBDY2 = get_verlet_ianbdy( id_inter, icdtac, iadj)

    cooran   = get_coor( ID_RBDY2, 0 )

    Lik = coorcd(1:2) - cooran(1:2)

    SIGMA(1, 1:2) =  SIGMA(1, 1:2) + Lik(1) * Fik(1:2)
    SIGMA(2, 1:2) =  SIGMA(2, 1:2) + Lik(2) * Fik(1:2)

  end subroutine compute_SIGMA

  SUBROUTINE display_tensors

    IMPLICIT NONE

    integer :: i, nfich, nb_CDAN
    integer :: iadj, icdtac, itact, nb_adj, verlet_size

    real(kind=8), dimension(2) :: Ipoint,tuc,nuc

    real(kind=8) :: DET,TRA,DELTA
    real(kind=8) :: f11,f12,f21,f22
    real(kind=8) :: s11,s12,s21,s22
    real(kind=8) :: sig1,sig2,tex1,tex2

    REAL(kind=8),DIMENSION(2,2)  :: SIGMA,TEXT
    
    integer, dimension(7) :: inters_id


    itact = 0
    SIGMA = 0
    TEXT  = 0

    inters_id = (/ i_dkdkx, i_dkjcx, i_dkkdx,          &
                            i_plplx, i_pljcx, i_clalp, &
                   i_cljcx                             /)

    nb_CDAN = 0
    do i = 1, size(inters_id)
      nb_CDAN = nb_CDAN + get_nb_verlets( inters_id(i) )
    end do

    if (nb_CDAN == 0) return

    do i = 1, size(inters_id)
      id_inter = inters_id(i)

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

         nb_adj = get_verlet_adjsz(id_inter, icdtac)

         do iadj = 1, nb_adj

           itact = itact + 1

           call get_verlet_local_frame( id_inter, icdtac, iadj, Ipoint, tuc, nuc )

           if( id_inter /= i_clalp .or. id_inter /= i_cljcx ) then
              call compute_SIGMA(id_inter, icdtac, iadj, H, SIGMA )
           end if

           TEXT(1, 1:2) =  TEXT(1, 1:2) + nuc(1) * nuc(1:2)
           TEXT(2, 1:2) =  TEXT(2, 1:2) + nuc(2) * nuc(1:2)

        end do

      end do
    end do

    !!! calcul vecteur et mode propre
    
    SIGMA = SIGMA
    TEXT  = TEXT/REAL(nb_CDAN,8)

    s11 = SIGMA(1,1)
    s12 = SIGMA(1,2)
    s21 = SIGMA(2,1)
    s22 = SIGMA(2,2)

    IF(s12 < epsilon(1.d0))THEN
       IF (s11 >= s22) THEN
          sig1 = s11
          sig2 = s22
       ELSE
          sig1 = s22
          sig2 = s11
       END IF
    ELSE
       DET   = s11*s22-s12*s21
       TRA   = s11+s22
       DELTA = TRA*TRA-4*DET
       IF(DELTA >= 0)THEN
          sig1  = (TRA+SQRT(DELTA))*0.5
          sig2  = (TRA-SQRT(DELTA))*0.5
       ELSE
          sig1  = 0.D0
          sig2  = 0.D0
       END IF
    END IF

    f11 = TEXT(1,1)
    f12 = TEXT(1,2)
    f21 = TEXT(2,1)
    f22 = TEXT(2,2)

    IF(f12 < epsilon(1.d0))THEN
       IF (s11 >= s22) THEN
          tex1 = f11
          tex2 = f22
       ELSE
          tex1 = f22
          tex2 = f11
       END IF
    ELSE
       DET   = f11*f22-f12*f21
       TRA   = f11+f22
       DELTA = TRA*TRA-4*DET
       IF(DELTA >= 0)THEN
          tex1  = (TRA+SQRT(DELTA))*0.5
          tex2  = (TRA-SQRT(DELTA))*0.5
       ELSE
          tex1  = 0.D0
          tex2  = 0.D0
       END IF
    END IF

    nfich = PostCommand(i_DISPLAY_TENSORS)%io_unit(1)

    WRITE(nfich,'(ES15.8,12(1X,ES14.7))') TPS,s11,s12,s21,s22,f11,f12,f21,f22,sig1,sig2,tex1,tex2

  END SUBROUTINE display_tensors

  !!!------------------------------------------------------------------
  !!! # 30  
  !!!------------------------------------------------------------------
  SUBROUTINE network_evolution
  
    IMPLICIT NONE
    
    INTEGER :: nct,nweak,nstrong
    INTEGER :: nfich

    IF(nlgs_solver2D)THEN
       CALL get_nlgs2D_network_change(nct,nweak,nstrong)
    END IF

    nfich = PostCommand(i_NETWORK_EVOLUTION)%io_unit(1)
    WRITE(nfich,'(ES15.8,3(1X,I6))') TPS,nct,nweak,nstrong

  END SUBROUTINE network_evolution

  !!!------------------------------------------------------------------
  !!! # 32  
  !!!------------------------------------------------------------------

  !fd a virer dans la couche MP 
  
  !!!--------------------------------------------------------------------------------------
  SUBROUTINE electro_evolution
  
    IMPLICIT NONE
    
    INTEGER      :: nfich
    INTEGER      :: iter,NLiter,nbo,nbco,nbso
    REAL(kind=8) :: err,NLerr,Cm

    nfich = PostCommand(i_ELECTRO_EVOLUTION)%io_unit(1) 
    
    CALL get_electro_info(iter,err,NLiter,Cm,nbo,nbco,nbso)
    
    WRITE(nfich,'(ES15.8,2(1X,I7,1X,ES14.7),3(1X,I7))') TPS,iter,err,NLiter,Cm,nbo,nbco,nbso
    
  END SUBROUTINE electro_evolution
  !!!--------------------------------------------------------------------------------------


  !!!------------------------------------------------------------------
  !!! # 13  
  !!!------------------------------------------------------------------

  SUBROUTINE extrusion
    
    IMPLICIT NONE
    INTEGER                   :: nfich,nb_bodies
    INTEGER                   :: ibdyty,itacty,nb_tacty,ibdy,i,itact
    INTEGER                   :: nb_faces,nb_vertex_3D
    REAL(kind=8),DIMENSION(3) :: vect 
    CHARACTER(len=5)          :: tactID
    TYPE(T_POLYG)             :: Bodie_polyg
    CHARACTER(len=5)          :: tactID_3D,color,clin
    CHARACTER(len=70)         :: clin3
    REAL(kind=8),DIMENSION(2) :: axes
    REAL(kind=8)              :: radius
    

    !print*, 'deep=',deep,'factor=',factor

    nfich=PostCommand(i_3D_extrusion)%io_unit(1) 

    nb_bodies=0

    DO ibdyty=1,nb_POLYG
       nb_bodies=nb_bodies+1
       Bodie_polyg  = get_l_polyg(ibdyty)
       nb_vertex_3D = 2*Bodie_polyg%nb_vertex
       nb_faces      = 4*Bodie_polyg%nb_vertex-4
       tactID_3D='POLYR';
       itacty=1
       color = get_color(polyg2bdyty(1,ibdyty),polyg2bdyty(2,itacty))
       itact=1
       vect=0.D0
       clin='NO6xx'
       WRITE(nfich,'(A6)')       '$bdyty'
       WRITE(nfich,'(1X,A5,I7)') 'RBDY3',nb_bodies
       WRITE(nfich,'(A6)')       '$blmty'
       WRITE(nfich,'(A72)')       ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
       
       WRITE(nfich,'(A6)') '$nodty'
       WRITE(nfich,105) clin,itact,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
       vect=0.D0
       WRITE(nfich,'(A6)') '$tacty'
       WRITE(nfich,104) tactID_3D,itact,'color',color,'nb_vertex=',nb_vertex_3D,'nb_faces=',nb_faces
       
       DO i=1,Bodie_polyg%nb_vertex
          vect(1)=Bodie_polyg%vertex(1,i)
          vect(2)=0.D0
          vect(3)=Bodie_polyg%vertex(2,i)
          WRITE(nfich,131) 'coo1=',vect(1)*factor,'coo2=',vect(2)*factor,'coo3=',vect(3)*factor
       ENDDO
       DO i=1,Bodie_polyg%nb_vertex
          vect(1)=Bodie_polyg%vertex(1,i)
          vect(2)=deep
          vect(3)=Bodie_polyg%vertex(2,i)
          WRITE(nfich,131) 'coo1=',vect(1)*factor,'coo2=',vect(2),'coo3=',vect(3)*factor
          !print*,VeCT
       ENDDO
       DO i=1,Bodie_polyg%nb_vertex-1
          WRITE(nfich,132) 'ver1=',i,'ver2=',Bodie_polyg%nb_vertex+i,'ver3=',Bodie_polyg%nb_vertex+i+1
          WRITE(nfich,132) 'ver1=',i,'ver2=',Bodie_polyg%nb_vertex+i+1,'ver3=',i+1
       ENDDO
       i=1
       WRITE(nfich,132) 'ver1=',Bodie_polyg%nb_vertex,'ver2=',2*Bodie_polyg%nb_vertex,'ver3=',i
       WRITE(nfich,132) 'ver1=',2*Bodie_polyg%nb_vertex,'ver2=',i,'ver3=',Bodie_polyg%nb_vertex+1
       DO i=2,Bodie_polyg%nb_vertex-1
          itact=1 
          WRITE(nfich,132) 'ver1=',itact,'ver2=',i,'ver3=',i+1
       ENDDO
       DO i=Bodie_polyg%nb_vertex+2,2*Bodie_polyg%nb_vertex-1
          itact=Bodie_polyg%nb_vertex+1
          WRITE(nfich,132) 'ver1=',itact,'ver2=',i,'ver3=',i+1
       ENDDO
       WRITE(nfich,'(A6)') '$$$$$$'
    ENDDO
    
    DO ibdyty=1,nb_DISKx
       nb_bodies=nb_bodies+1
       itacty=1
       vect(1:3) = get_coor(diskx2bdyty(1,ibdyty),diskx2bdyty(2,itacty))
       itacty=1
       color     = get_color(diskx2bdyty(1,ibdyty),diskx2bdyty(2,itacty))
       radius=get_radius_DISKx(ibdyty)
       clin='NO6xx'
       WRITE(nfich,'(A72)') '$bdyty                                                                  '
       WRITE(nfich,101)'RBDY3',nb_bodies
       WRITE(nfich,'(A6)') '$blmty'
       WRITE(nfich,'(A72)') ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
       WRITE(nfich,'(A6)') '$nodty'
       vect(3)=0.D0
       itact=1
       WRITE(nfich,105)clin,itact,'coo1=',vect(1)*factor,'coo2=',vect(3)*factor,'coo3=',vect(2)*factor
       vect=0.D0
       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
       WRITE(nfich,'(A6)') '$tacty'
       !1234567890123456789012345678901234567890123456789012345678901234567890
       clin3='                                                                      '
       clin3(1:5)='SPHER'
       WRITE(clin3(10:12),'(I3)') 1
       clin3(15:19)='color'
       clin3(22:26)=color
       clin3(29:34)='byrd='
       WRITE(clin3(34:47),'(ES14.7)') radius*factor
       WRITE(nfich,*) clin3
       WRITE(nfich,'(A6)') '$$$$$$'
    ENDDO
    
    DO ibdyty=1,nb_xKSID
       nb_bodies=nb_bodies+1
       itacty=1
       vect(1:3) = get_coor(xksid2bdyty(1,ibdyty),xksid2bdyty(2,itacty))
       itacty=1
       color     = get_color(xksid2bdyty(1,ibdyty),xksid2bdyty(2,itacty))
       radius=get_radius_xKSID(ibdyty)
       clin='NO6xx'
       WRITE(nfich,'(A72)') '$bdyty                                                                  '
       WRITE(nfich,101)'RBDY3',nb_bodies
       WRITE(nfich,'(A6)') '$blmty'
       WRITE(nfich,'(A72)') ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
       WRITE(nfich,'(A6)') '$nodty'
       vect(3)=0.D0
       itact=1
       WRITE(nfich,105)clin,itact,'coo1=',vect(1)*factor,'coo2=',vect(3)*factor,'coo3=',vect(2)*factor
       vect=0.D0
       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
       WRITE(nfich,'(A6)') '$tacty'
       !1234567890123456789012345678901234567890123456789012345678901234567890
       clin3='                                                                      '
       clin3(1:5)='CYLDx'
       WRITE(clin3(10:12),'(I3)') 1
       clin3(15:19)='color'
       clin3(22:26)='BLEUx'
       clin3(29:34)='High='
       WRITE(clin3(34:47),'(ES14.7)') deep
       clin3(50:55)='Byrd='
       WRITE(clin3(55:68),'(ES14.7)') radius*factor
       WRITE(1,*) clin3
       WRITE(1,'(A6)')'$$$$$$'
    ENDDO
    
    DO ibdyty=1,nb_JONCx
       nb_bodies=nb_bodies+1
       itacty=1
       vect(1:3) = get_coor(joncx2bdyty(1,ibdyty),joncx2bdyty(2,itacty))
       color = get_color(joncx2bdyty(1,ibdyty),joncx2bdyty(2,itacty))
       CALL get_data(joncx2bdyty(1,ibdyty),joncx2bdyty(2,itacty),axes)
       clin='NO6xx'
       !1234567890123456789012345678901234567890123456789012345678901234567890
       clin3='                                                                      '
       clin3(2:6)='PLANx'
       WRITE(clin3(10:12),'(I3)') 1
       clin3(15:19)='color'
       clin3(22:26)=color
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)'RBDY3',nb_bodies
       WRITE(nfich,'(A6)') '$blmty'
       WRITE(nfich,'(A72)') ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
       WRITE(nfich,'(A6)') '$nodty'
       vect(3)=0.D0
       WRITE(nfich,105)clin,itact,'coo1=',vect(1)*factor,'coo2=',vect(3)*factor,'coo3=',vect(2)*factor
       vect=0.D0
       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
       WRITE(nfich,'(A6)') '$tacty'
       vect(1)=axes(1)*factor
       vect(2)=4.D0*deep
       vect(3)=axes(2)*factor
       WRITE(nfich,106) clin3(1:27),'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
       vect=0.D0;
       WRITE(nfich,'(A6)')'$$$$$$'
    END DO
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7,4X,A9,I7)
131 FORMAT(27X,3(2X,A5,ES14.7))
132 FORMAT(27X,3(2X,A5,I7,7X))
105 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,ES14.7))
101 FORMAT(1X,A5,2X,I5)
103 FORMAT(27X,3(2X,A5,ES14.7))
106 FORMAT(A27,3(2X,A5,ES14.7))

  END SUBROUTINE extrusion
  !!!--------------------------------------------------------------------------------


  !!!------------------------------------------------------------------
  !!! # 16  
  !!!------------------------------------------------------------------

  SUBROUTINE Fint_EVOLUTION

    IMPLICIT NONE

    INTEGER                   :: nfich,i,j,k

    REAL(kind=8) :: tReac(2),tFint(2),tFinert(2),tFext(2),tRes(2)
    
    REAL(kind=8) :: reac(2),fint(2),Finert(2),fext(2),res(2),momentum(2)

    DO i=1,nb_MECAx_sets
       
       tReac   = 0.d0
       tFint   = 0.d0
       tFinert = 0.d0
       tFext   = 0.d0
       tRes    = 0.d0
       
       DO j=1,MECAx_set(i)%nb_MECAx
          
          DO k=1,MECAx_set(i)%data(j)%nb_nodes
             
             CALL get_nodal_Forces_mecaMAILx(MECAx_set(i)%data(j)%iMECAx,&
                                             MECAx_set(i)%data(j)%nodes(k),2,&
                                             reac,fint,finert,fext,res,momentum)


             tReac =tReac + reac
             tFint=tFint +fint
             tFinert=tFinert +finert
             tFext=tFext +fext
             tRes=tRes + res
          END DO
       END DO

       nfich = PostCommand(i_Fint_EVOLUTION)%io_unit(i)
       WRITE(nfich,'(ES15.8,10(1x,ES14.7))') TPS,tReac(1),tReac(2), &
                                             tFint(1),tFint(2), &
                                             tFinert(1),tFinert(2), &
                                             tFext(1),tFext(2), &
                                             tRes(1),tRes(2)  

    END DO

  END SUBROUTINE Fint_EVOLUTION
  !!!--------------------------------------------------------------------------------

  !!!------------------------------------------------------------------
  !!! # 17  
  !!!------------------------------------------------------------------  
  SUBROUTINE Dep_EVOLUTION

    IMPLICIT NONE
    
    INTEGER      :: nfich,i,j,k,nb_nodes
    REAL(kind=8) :: mean_u(2),mean_v(2)
    REAL(kind=8) :: u(2),v(2)

    DO i=1,nb_MECAx_sets
       mean_u = 0.d0
       mean_v = 0.d0

       nb_nodes=0
       
       DO j=1,MECAx_set(i)%nb_MECAx
          
          DO k=1,MECAx_set(i)%data(j)%nb_nodes
             
             CALL get_nodal_displacements_mecaMAILx(MECAx_set(i)%data(j)%iMECAx, &
                  MECAx_set(i)%data(j)%nodes(k),2,u,v)

             mean_u = mean_u + u
             mean_v = mean_v + v

             nb_nodes=nb_nodes+1
             
          END DO
       END DO


       nfich = PostCommand(i_Dep_EVOLUTION)%io_unit(i)
       WRITE(nfich,'(ES15.8,4(1x,ES14.7))') TPS,mean_u(1)/nb_nodes,mean_u(2)/nb_nodes, &
                                            mean_v(1)/nb_nodes,mean_v(2)/nb_nodes
    END DO
    
  END SUBROUTINE Dep_EVOLUTION
  !!!--------------------------------------------------------------------------------


  !!!------------------------------------------------------------------
  !!! # 19  
  !!!------------------------------------------------------------------

  !fd a quoi sert cette merde !!
  
  SUBROUTINE MAILx_dist

    IMPLICIT NONE
    
    INTEGER      :: nfich,i,ibdya,ibdyb,inoda,inodb
    REAL(kind=8),DIMENSION(nbDIME) :: coora,coorb
    REAL(kind=8) :: dist
    
    nfich = PostCommand(i_MAILx_DIST)%io_unit(1)
    WRITE(nfich,'(1(1x,ES15.8))') TPS
    
    DO i=1,nb_dist_sets
       
       ibdya = dist_set(i)%data(1)%iMECAx
       inoda = dist_set(i)%data(1)%nodes(1)
       coora = get_coor_nodty_MAILx(ibdya,inoda)
       
       ibdyb = dist_set(i)%data(2)%iMECAx
       inodb = dist_set(i)%data(2)%nodes(1)
       coorb = get_coor_nodty_MAILx(ibdyb,inodb) 
       
       dist=SQRT(DOT_PRODUCT(coora-coorb,coora-coorb))
       
       WRITE(nfich,'(I7,1x,I7,1x,ES14.7)') ibdya,ibdyb,dist
       
    END DO

  END SUBROUTINE MAILx_dist
  !!!------------------------------------------------------------------------------


  !!!------------------------------------------------------------------
  !!! # 37  
  !!!------------------------------------------------------------------

  !fd a quoi ça sert !?
  
  SUBROUTINE CLxxx_analysis

    IMPLICIT NONE
    
    INTEGER         :: i,ibdyty,itacty,icdan,icdtac,iadj
    REAL(kind=8)    :: gapTT,rln,rlt,vln,vlt
    REAL(kind=8),DIMENSION(max_internal_tact) :: internal
    integer(kind=4) :: status
    INTEGER         :: nfich

    do i=1,nb_clxxx_set

      ibdyty = clxxx_set(1,i)
      itacty = clxxx_set(2,i)

      gapTT    = 0.d0
      rln      = 0.d0
      rlt      = 0.d0
      vln      = 0.d0
      vlt      = 0.d0
      internal = 0.d0

      do icdan = 1, get_nb_verlets( i_clalp )

        call this2verlet(id_inter, icdan, icdtac, iadj)

        IF ( (ibdyty == clxxx2bdyty(1,icdtac)) .AND. &
             (itacty == clxxx2bdyty(2,icdtac)) ) THEN
           gapTT = get_verlet_gapTT( id_inter, icdtac, iadj )
           call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln )
           call get_verlet_vloc( id_inter, icdtac, iadj, vlt, vln )
           call get_verlet_internal( id_inter, icdtac, iadj, internal )
           EXIT
        END IF

      END DO

      nfich = PostCommand(i_CLxxx_ANALYSIS)%io_unit(i)
      WRITE(nfich,'(ES15.8,25(1X,ES14.7))') TPS,gapTT,rln,rlt,vln,vlt,internal(1:max_internal_tact)
    
    ENDDO
  END SUBROUTINE
  

  !!!-------------------- 
  !!! # 31
  !!!--------------------

  !fd a virer
  
  !------------------------------------------------------------------------------
  ! CREATE TEXT DATA:
  ! subroutine for creating files in order to use special post-processing
  ! created by G. Saussine and F. Radjai
  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  subroutine create_text_data_aux( id_inter, nb_inter )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter

    ! Local variables
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: icdent
    integer( kind = 4 ) :: ianent
    integer( kind = 4 ) :: icdtac
    integer( kind = 4 ) :: iadj
    real( kind = 8 ), dimension( 2 ) :: coor_ctc
    real( kind = 8 ), dimension( 2 ) :: t
    real( kind = 8 ), dimension( 2 ) :: n
    integer( kind = 4 ) :: status
    real( kind = 8 )    :: rn
    real( kind = 8 )    :: rt
    integer( kind = 4 ) :: cttype
    integer( kind = 4 ) :: nfich

    call faterr( 'postpro_2D::create_text_data_aux','not re-implemented')
    do icdan = 1, nb_PLPLx

       !call inter_2ENT( id_inter, icdan, icdent, ianent )
       !call this2verlet( id_inter, icdan, icdtac, iadj )
       !call get_local_frame_inter_( id_inter, icdtac, iadj, coor_ctc, t, n )
       !call get_local_reac_inter_( id_inter, icdtac, iadj, status, rn, rt )
       !cttype = get_contact_type(id_inter, icdan)

       nfich  = PostCommand( i_CREATE_TEXT_DATA )%io_unit( 3 )
       write( nfich, '(1X,I10,1X,I10,1X,I10,1X,6(1x,ES12.5))' ) &
            icdent, ianent, cttype, n(1), n(2), coor_ctc(1), &
            coor_ctc(2), rn, rt

    end do

  end subroutine create_text_data_aux

  SUBROUTINE create_text_data

    IMPLICIT NONE
    INTEGER                   :: nfich,nb_vertex
    INTEGER                   :: ibdyty,itacty=1,i_tactID,k,icdan,nb_CDAN
    CHARACTER(len=5)          :: tactID,status
    REAL(kind=8),DIMENSION(3) :: coor,Vbegin
    REAL(kind=8),DIMENSION(2) :: axes,t,n,coor_ctc
    TYPE(T_POLYG)             :: Body_polyg
    INTEGER                   :: iadj,cttype,icdtac,icdent,ianent
    REAL(kind=8)              :: rn,rt

    ! Creation of file SAVE_BODIES.DAT: indice grains,type,rayon équivalent,nombre de sommets, coordonnée (x1,y1) ...(xn,yn)
    ! Creation of file SAVE_DOF.DAT : indice grains,x,y,rot,vx,vy,vrot
    
    ! First line 
    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(1)
    WRITE(nfich,'(1x,I10,1X,I10,1X,I10,1X,I10,1X,I10)') Nstep,nb_POLYG,nb_JONCx,nb_PLPLx,nb_PLJCx
    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(2)
    WRITE(nfich,'(1x,I10,1X,I10,1X,I10,1X,I10,1X,I10)') Nstep,nb_POLYG,nb_JONCx,nb_PLPLx,nb_PLJCx
    
    DO ibdyty=1,nb_RBDY2
       tactID = get_tacID(ibdyty,itacty)
       SELECT CASE(tactID)
       CASE('POLYG')
          i_tactID = 1   ! pour postraitement les polygones ont par défaut contacteur 1
          Body_polyg=get_l_polyg(ibdyty)
          coor   = get_coor(ibdyty,itacty)
          Vbegin = get_Vbegin(ibdyty)
          nb_vertex = Body_polyg%nb_vertex      
          ! ecriture des fichiers particles
          nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(1)
          WRITE(nfich,'(I4,1X,I2,1X,ES12.5,1X,I2,99(1x,ES12.5))') ibdyty,i_tactID,Body_polyg%outer_radius,Body_polyg%nb_vertex, &
               (Body_polyg%vertex(1,k),Body_polyg%vertex(2,k),k=1,Body_polyg%nb_vertex)
          ! ecriture degree_of_freedom
          nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(2)
          WRITE(nfich,'(I4,1X,6(1x,ES12.5))') ibdyty,coor(1:3),Vbegin(1:3)  
       CASE default
          CYCLE 
       END SELECT
    ENDDO
    DO ibdyty=1,nb_RBDY2
       tactID = get_tacID(ibdyty,itacty)
       SELECT CASE(tactID)
       CASE('JONCx')
          i_tactID = 2
          coor     = get_coor(ibdyty,itacty)
          Vbegin = get_Vbegin(ibdyty)
          CALL get_data(ibdyty,itacty,axes)
          ! ecriture des fichiers particles
          nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(1)
          WRITE(nfich,'(1X,I4,1X,I2,5(1x,ES12.5))') ibdyty,i_tactID,axes(1),axes(2)
          ! ecriture degree_of_freedom
          nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(2)
          WRITE(nfich,'(I4,1X,6(1x,ES12.5))') ibdyty,coor(1:3),Vbegin(1:3) 
          
       CASE default
          CYCLE 
       END SELECT
       
    ENDDO
    
    ! blanc line delimiter
    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(1)
    WRITE(nfich,*)
    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(2)
    WRITE(nfich,*)
    
    ! Creation of file CONTACTS.DAT: 
    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(3)
    WRITE(nfich,'(1x,I10,1X,I10,1X,I10,1X,I10,1X,I10)') Nstep,nb_POLYG,nb_JONCx,nb_PLPLx,nb_PLJCx

    call create_text_data_aux( i_plplx, nb_PLPLx )

    call create_text_data_aux( i_pljcx, nb_PLJCx )

    ! blanc line delimiter
    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(3)
    WRITE(nfich,*)
  END SUBROUTINE create_text_data

  !--------------------------------------------------------------------------------
  SUBROUTINE save_data
    
    IMPLICIT NONE
    
    CHARACTER(len=30) :: Glin
    INTEGER           :: nfich,isee,ibehav,ibdyty
    integer(kind=4)   :: statusBEGIN
    REAL(kind=8)      :: restn,restt

    nfich = PostCommand(i_CREATE_TEXT_DATA)%io_unit(4)

    WRITE(nfich+3,'(I10,1x,A5)')   nb_RBDY2,'CORPS'
    WRITE(nfich+3,'(I10,1x,A5)')   nb_POLYG,'POLYG'
    WRITE(nfich+3,'(I10,1x,A5)')   nb_JONCx,'JONCx'
    WRITE(nfich+3,'(10000(1x,ES12.5))')  (get_mass(ibdyty),ibdyty=1,nb_RBDY2)
    WRITE(nfich+3,'(10000(1x,ES12.5))')  (get_gyr_radius(ibdyty),ibdyty=1,nb_RBDY2)
    
    WRITE(nfich+3,'(ES12.5,1x,A5)') H,'delta'
    !  write(nfich+3,'(1x,A5')          'ITER:'
    WRITE(nfich+3,'(3(I10,1x))')      1,1,1!NRFZZZ(1:NBPZZZ)
    
    !  write(nfich+3,'(1x,A5')          'ALERT'
    DO isee=1,SIZE(see)
       IF((see(isee)%cdtac) == (see(isee)%antac))THEN
          WRITE(nfich+3,'(E12.5,1x,A5)') see(isee)%alert,'PLPLx'
       ELSE
          WRITE(nfich+3,'(E12.5,1x,A5)') see(isee)%alert,'PLJCx'
       ENDIF
    ENDDO
    
    !  write(nfich+3,'(1x,A5')          'FRIC:'
    DO isee=1,SIZE(see)
       IF((see(isee)%cdtac) == (see(isee)%antac))THEN
          WRITE(nfich+3,'(ES12.5,1x,A5)') get_fric(see(isee)%lawnb,statusBEGIN),'PLPLx'
       ELSE
          WRITE(nfich+3,'(ES12.5,1x,A5)') get_fric(see(isee)%lawnb,statusBEGIN),'PLJCx'
       ENDIF
    ENDDO
    
    !  write(nfich+3,'(1x,A5')          'RESTI'
    DO isee=1,SIZE(see)
       IF((see(isee)%cdtac) == (see(isee)%antac))THEN
          CALL get_rst(see(isee)%lawnb,restt,restn)
          WRITE(nfich+3,'(ES12.5,1x,A5)') restn,'PLPLx'
       ELSE
          CALL get_rst(see(isee)%lawnb,restt,restn)
          WRITE(nfich+3,'(ES12.5,1x,A5)') restn,'PLJCx'
       ENDIF
    ENDDO
    
    WRITE(nfich+3,'(2(ES12.5,1x),A5)') grav1,grav2,'GRAVI'
    
  END SUBROUTINE save_data

  !!!-------------------- 
  !!! # 31
  !!!--------------------
  
  !!!---------------------------------------------------------------------
  SUBROUTINE compacity_evolution

    IMPLICIT NONE
    
    INTEGER                   :: nfich,ibdyty,iblmty,imodel
    INTEGER                   :: nb_cells,nb_models
    CHARACTER(len=5)          :: behav
    REAL(kind=8)              :: XMIN,XMAX,YMIN,YMAX,BoxArea,AREA,RADIUS(1)
    REAL(kind=8),DIMENSION(2) :: AXES
    REAL(kind=8),DIMENSION(3) :: XLEFTx,XRIGHT,XDOWNx,XUPxxx
    character(len=80)         :: cout
    ! computed for once during ? should change if visible/invisible ?
    REAL(kind=8)              :: RBodyAREA,DBodyArea,BBodyArea

    
    RBodyArea = 0.D0
    DO ibdyty=1,nb_RBDY2
      if (BodyBehav == 'all__') then
        if (BodyWindow(ibdyty)) RBodyArea = RBodyArea + get_area(ibdyty)
      else   
        iblmty = 1 
        CALL get_behav(ibdyty,iblmty,behav)
        IF (behav == BodyBehav .and. BodyWindow(ibdyty)) RBodyArea = RBodyArea + get_area(ibdyty)
      endif  
    END DO
    

    DBodyArea = 0.D0
    DO ibdyty=1,nb_MAILx
       nb_cells = get_nb_cell_MAILx(ibdyty)
       
       DO iblmty=1,nb_cells
          
          nb_models = get_nb_model_MAILx(ibdyty,iblmty)
          
          DO imodel = 1,nb_models

             if (BodyBehav /= 'all__') then
               behav = get_behav_MAILx(ibdyty,iblmty,imodel)
               IF (behav .NE. BodyBehav) CYCLE
             endif  

             CALL comp_cell_area(ibdyty,iblmty,AREA)
             
             DBodyArea = DBodyArea + AREA
             
             EXIT
          END DO
       END DO
    END DO

    BBodyArea=0.d0
    SELECT CASE(compacity_model)
    CASE(icmp_smooth_box)

       BBodyArea = BBodyArea + get_area(ID_LEFTx)
       BBodyArea = BBodyArea + get_area(ID_RIGHT)
       BBodyArea = BBodyArea + get_area(ID_DOWNx)       
       BBodyArea = BBodyArea + get_area(ID_UPxxx)
       
       XLEFTx = get_coor(ID_LEFTx,0)
       XRIGHT = get_coor(ID_RIGHT,0)
       XDOWNx = get_coor(ID_DOWNx,0)
       XUPxxx = get_coor(ID_UPxxx,0)

       CALL get_data(ID_LEFTx,1,AXES)
       XMIN = XLEFTx(1) + AXES(2)

       CALL get_data(ID_RIGHT,1,AXES)
       XMAX = XRIGHT(1) - AXES(2)

       CALL get_data(ID_DOWNx,1,AXES)
       YMIN = XDOWNx(2) + AXES(2)

       CALL get_data(ID_UPxxx,1,AXES)
       YMAX = XUPxxx(2) - AXES(2)

       BoxArea = (XMAX-XMIN)*(YMAX-YMIN)

    CASE(icmp_rough_box)

       BBodyArea = BBodyArea + get_area(ID_LEFTx)
       BBodyArea = BBodyArea + get_area(ID_RIGHT)
       BBodyArea = BBodyArea + get_area(ID_DOWNx)       
       BBodyArea = BBodyArea + get_area(ID_UPxxx)
       
       XLEFTx = get_coor(ID_LEFTx,0)
       XRIGHT = get_coor(ID_RIGHT,0)
       XDOWNx = get_coor(ID_DOWNx,0)
       XUPxxx = get_coor(ID_UPxxx,0)

       XMIN = XLEFTx(1)
       XMAX = XRIGHT(1)
       YMIN = XDOWNx(2)
       YMAX = XUPxxx(2)

       BoxArea = (XMAX-XMIN)*(YMAX-YMIN)

    CASE(icmp_couette)

       BBodyArea = BBodyArea + get_area(ID_INTER)
       BBodyArea = BBodyArea + get_area(ID_EXTER)
       
       CALL get_data(ID_INTER,1,AXES)
       XMIN = AXES(2)
       CALL get_data(ID_EXTER,1,AXES)
       XMAX = AXES(2)

       BoxArea = PI_g*( XMAX*XMAX - XMIN*XMIN )

    CASE(icmp_cluster_box)

       BBodyArea = BBodyArea + get_area(ID_LEFTx)
       BBodyArea = BBodyArea + get_area(ID_RIGHT)
       BBodyArea = BBodyArea + get_area(ID_DOWNx)       
       BBodyArea = BBodyArea + get_area(ID_UPxxx)

       ! fd pourquoi 1 ??
       XLEFTx = get_coor(ID_LEFTx,1)
       XRIGHT = get_coor(ID_RIGHT,1)
       XDOWNx = get_coor(ID_DOWNx,1)
       XUPxxx = get_coor(ID_UPxxx,1)

       CALL get_data(ID_LEFTx,1,RADIUS)
       XMIN = XLEFTx(1) + RADIUS(1)

       CALL get_data(ID_RIGHT,1,RADIUS)
       XMAX = XRIGHT(1) - RADIUS(1)

       CALL get_data(ID_DOWNx,1,RADIUS)
       YMIN = XDOWNx(2) + RADIUS(1)

       CALL get_data(ID_UPxxx,1,RADIUS)
       YMAX = XUPxxx(2) - RADIUS(1)

       BoxArea = (XMAX-XMIN)*(YMAX-YMIN)

    CASE(icmp_selection)
       BBodyArea = 0.d0
       BoxArea   = Pi_g*WdwR2
    CASE DEFAULT

    END SELECT

    nfich = PostCommand(i_COMPACITY_EVOLUTION)%io_unit(1)

    IF(BoxArea > epsilon(1.d0))THEN 
       WRITE(nfich,'(ES15.8,3(1X,ES14.7))') TPS,(RBodyArea+DBodyArea-BBodyArea)/BoxArea,RBodyArea+DBodyArea-BBodyArea,BoxArea
    ELSE
       write(cout,'(A,I5,A)') '@ At step ',Nstep,'area is less than 1D-16'
       call logmes(cout)
       call logmes('@ No computation for this step')
       write(cout,*) XMIN,XMAX
       call logmes(cout)       
       write(cout,*) YMIN,YMAX
       call logmes(cout)
       RETURN
    ENDIF
    
  END SUBROUTINE compacity_evolution
  
  !!!-----------------------------------------------------------------------------------
  !!! # 36
  !!!--------------------------------------------------------------------------------------

  !fd a virer dans couche MP
  
  SUBROUTINE heat_bound_profile
  
    IMPLICIT NONE
    
    INTEGER :: iy,ibound
    INTEGER :: nfich
    
    hb_unit = hb_unit + 1
    
    nfich = PostCommand(i_HEAT_BOUND_PROFILE)%io_unit(1)

    WRITE(heat_bound_name(28:34),'(I7.7)') hb_unit
    OPEN(unit=nfich,file=trim(location(heat_bound_name)),status='REPLACE')

    DO ibound=1,nb_HB
       HEATvector = 0.D0
       CALL GET_HEAT_BOUND_PROFILE(ibound,HEATvector)
       HEATBOUNDS(ibound,1:HB_NY) = HEATvector(1:HB_NY)
    END DO

    SELECT CASE(nb_HB)
    CASE(1)
       DO iy=1,HB_NY
          WRITE(nfich,'(1X,ES14.7)') HEATBOUNDS(1,iy)
       END DO
    CASE(2)
       DO iy=1,HB_NY
          WRITE(nfich,'(2(1X,ES14.7))') HEATBOUNDS(1,iy),HEATBOUNDS(2,iy)
       END DO
    CASE(3)
       DO iy=1,HB_NY
          WRITE(nfich,'(3(1X,ES14.7))') HEATBOUNDS(1,iy),HEATBOUNDS(2,iy),HEATBOUNDS(3,iy)
       END DO
    CASE(4)
       DO iy=1,HB_NY
          WRITE(nfich,'(4(1X,ES14.7))') HEATBOUNDS(1,iy),HEATBOUNDS(2,iy),HEATBOUNDS(3,iy),HEATBOUNDS(4,iy)
       END DO
    CASE DEFAULT
       
    END SELECT

    CLOSE(nfich)

  END SUBROUTINE heat_bound_profile
  
  !!!-----------------------------------------------------------------------------------
  !!! # 38 GET ENERGIE SURFACE PER SECTOR 
  !!! MR&VHN
  !!!-----------------------------------------------------------------------------------

  !fd a virer dans la couche MP

  
  SUBROUTINE WS_sector_evolution
    implicit none
    INTEGER(kind=4)           :: ibdyty,itacty,iblmty,ibehav,isect,WSstatus,nb_tacty
    REAL(kind=8)              :: WS,WStime
    INTEGER                   :: nfich,i

    nfich = PostCommand(i_SNAP_SURFACE_ENERGIE_SECTOR)%io_unit(1)

    ws_unit = ws_unit + 1
    WRITE(ws_name(12:18),'(I7.7)') ws_unit
    OPEN(unit=nfich,file=trim(location(ws_name)),status='REPLACE')

    DO ibdyty=1,nb_RBDY2
       !ibdyty = SectorID(i)
       iblmty = 1
       !ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
       nb_tacty = get_nb_tacty(ibdyty)

       DO itacty=1,nb_tacty
          DO isect=1,nb_WSsect

             !sector_set(isect,ibdyty)%WS       = get_WS(ibdyty,itacty,isect)
             !sector_set(isect,ibdyty)%WStime   = get_WStime(ibdyty,itacty,isect)
             !sector_set(isect,ibdyty)%WSstatus = get_WSstatus(ibdyty,itacty,isect)
             WS       = get_WS(ibdyty,itacty,isect)
             WStime   = get_WStime(ibdyty,itacty,isect)
             WSstatus = get_WSstatus(ibdyty,itacty,isect)

             WRITE(nfich,'(3(1x,I7),2(1x,ES14.7),(1x,I7))') ibdyty,itacty,isect,WS,WStime,WSstatus
          END DO          
       END DO

    END DO
    CLOSE(nfich)
  END SUBROUTINE WS_sector_evolution

  !!!-----------------------------------------------------------------------------------
  !!! # 39 
  !!!-----------------------------------------------------------------------------------
  
  subroutine czm_energy_evolution_aux( id_inter, nb_inter, &
       StoredEnergy, DamageEnergy, FailureEnergy, CohesionEnergy )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter

    real( kind = 8 ) :: StoredEnergy
    real( kind = 8 ) :: DamageEnergy
    real( kind = 8 ) :: FailureEnergy
    real( kind = 8 ) :: CohesionEnergy

    ! Local variables
    integer( kind = 4 ) :: icdan
    real( kind = 8 )    :: stored  
    real( kind = 8 )    :: damage  
    real( kind = 8 )    :: failure 
    real( kind = 8 )    :: cohesion

    do icdan = 1, nb_inter

       !call get_CZM_energy_inter_( id_inter, icdan, stored, damage, failure, cohesion )

       StoredEnergy   = StoredEnergy   + stored
       DamageEnergy   = DamageEnergy   + damage
       FailureEnergy  = FailureEnergy  + failure
       CohesionEnergy = CohesionEnergy + cohesion

    end do

  end subroutine czm_energy_evolution_aux
  
  !!!-----------------------------------------------------------------------------------
  subroutine czm_energy_evolution
    implicit none
    integer(kind=4) :: nfich,icdan
    real(kind=8)    :: StoredEnergy,DamageEnergy,FailureEnergy,CohesionEnergy
    real(kind=8)    :: stored,damage,failure,cohesion

    call faterr( 'postpro_2D::czm_energy_evolution','not re-implemented')

    call czm_energy_evolution_aux( i_dkdkx, nb_DKDKx, &
         StoredEnergy, DamageEnergy, FailureEnergy, CohesionEnergy )

    call czm_energy_evolution_aux( i_plplx, nb_PLPLx, &
         StoredEnergy, DamageEnergy, FailureEnergy, CohesionEnergy )

    nfich = PostCommand(i_CZM_ENERGY_EVOLUTION)%io_unit(1)

    WRITE(nfich,'(ES15.8,4(1X,ES14.7))') TPS,StoredEnergy,DamageEnergy,FailureEnergy,CohesionEnergy

  END SUBROUTINE czm_energy_evolution
  !!!-----------------------------------------------------------------------------------

  !!!-----------------------------------------------------------------------------------
  !!! # 40 
  !!!-----------------------------------------------------------------------------------
  subroutine energy_snapshot_sample_aux( id_inter, nb_inter, nfich )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    integer( kind = 4 ) :: nfich

    ! Local variables
    integer( kind = 4 ) :: icdan
    real( kind = 8 )    :: stored  
    real( kind = 8 )    :: damage  
    real( kind = 8 )    :: failure 
    real( kind = 8 )    :: cohesion
    real( kind = 8 )    :: Qij_c
    real( kind = 8 )    :: Qij_s

    do icdan = 1, nb_inter

       !call get_CZM_energy_inter_( id_inter, icdan, stored, damage, failure, cohesion )

       call get_local_flux( icdan, id_inter, Qij_c, Qij_s )

       write( nfich, '(1X,A5,1X,I7,6(1X,ES14.7))' ) &
            get_interaction_name_from_id( id_inter ), &
            icdan, Qij_c, Qij_s, stored, damage, failure, cohesion

    end do

  end subroutine energy_snapshot_sample_aux

  !!!-----------------------------------------------------------------------------------
  SUBROUTINE energy_snapshot_sample
    IMPLICIT NONE
    integer(kind=4) :: nb_CDAN,icdan,nfich
    real(kind=8)    :: Qij_c,Qij_s
    real(kind=8)    :: stored,damage,failure,cohesion

    call faterr( 'postpro_2D::energy_snapshot_sample','not re-implemented')
    esnap_unit = esnap_unit + 1

    nfich = PostCommand(i_ENERGY_SNAPSHOT_SAMPLE)%io_unit(1)

    WRITE(energy_snap_name(24:29),'(I6.6)') esnap_unit
   
    OPEN(unit=nfich,file=trim(location(energy_snap_name)),status='REPLACE')

    nb_CDAN = nb_DKDKx + nb_DKJCx + nb_DKKDx + nb_CLALp + nb_PLPLx + nb_PLJCx + nb_CLJCx

    WRITE(nfich,'(I7)') nb_CDAN
        
    call energy_snapshot_sample_aux( i_dkdkx, nb_DKDKx, nfich )

    DO icdan = 1,nb_DKJCx

       !call get_DKDKx_CZM_energy(icdan,stored,damage,failure,cohesion)
       stored   = 0.d0
       damage   = 0.d0
       failure  = 0.d0
       cohesion = 0.d0
       
       call get_local_flux(icdan,i_dkjcx,Qij_c,Qij_s)

       WRITE(nfich,'(1X,A5,1X,I7,6(1X,ES14.7))') 'DKJCx',icdan,Qij_c,Qij_s,stored,damage,failure,cohesion

    END DO

    DO icdan = 1,nb_DKKDx

       !call get_DKKDx_CZM_energy(icdan,stored,damage,failure,cohesion)
       stored   = 0.d0
       damage   = 0.d0
       failure  = 0.d0
       cohesion = 0.d0
       
       call get_local_flux(icdan,i_dkkdx,Qij_c,Qij_s)

       WRITE(nfich,'(1X,A5,1X,I7,6(1X,ES14.7))') 'DKKDx',icdan,Qij_c,Qij_s,stored,damage,failure,cohesion

    END DO

    do icdan = 1,nb_CLALp
       !call get_CLALp_CZM_energy(icdan,stored,damage,failure,cohesion)
       stored   = 0.d0
       damage   = 0.d0
       failure  = 0.d0
       cohesion = 0.d0
       
       !call get_local_flux(icdan,i_dkkdx,Qij_c,Qij_s)
       Qij_c = 0.d0
       Qij_s = 0.d0

       WRITE(nfich,'(1X,A5,1X,I7,6(1X,ES14.7))') 'CLALp',icdan,Qij_c,Qij_s,stored,damage,failure,cohesion
    end do

    do icdan = 1,nb_CLJCx
       !call get_CLALp_CZM_energy(icdan,stored,damage,failure,cohesion)
       stored   = 0.d0
       damage   = 0.d0
       failure  = 0.d0
       cohesion = 0.d0
       
       !call get_local_flux(icdan,i_dkkdx,Qij_c,Qij_s)
       Qij_c = 0.d0
       Qij_s = 0.d0

       WRITE(nfich,'(1X,A5,1X,I7,6(1X,ES14.7))') 'CLJCx',icdan,Qij_c,Qij_s,stored,damage,failure,cohesion
    end do

    DO icdan = 1,nb_PLJCx

       !call get_PLJCx_CZM_energy(icdan,stored,damage,failure,cohesion)
       stored   = 0.d0
       damage   = 0.d0
       failure  = 0.d0
       cohesion = 0.d0
       
       call get_local_flux(icdan,i_pljcx,Qij_c,Qij_s)

       WRITE(nfich,'(1X,A5,1X,I7,6(1X,ES14.7))') 'PLJCx',icdan,Qij_c,Qij_s,stored,damage,failure,cohesion

    END DO

    call energy_snapshot_sample_aux( i_plplx, nb_plplx, nfich )

    CLOSE(nfich)

  END SUBROUTINE energy_snapshot_sample


  !!!------------------------------------------------------------------------------
  !!!
  !!! PART OF LMGC90 iterative post_processing using sample windows
  !!!
  !!!------------------------------------------------------------------------------

  !fd called through chipy
  
  SUBROUTINE circular_selection_postpro(X,Y,R)

    IMPLICIT NONE
    
    REAL(kind=8) :: X,Y,R

    WdwX = X
    WdwY = Y
    WdwR = R
    WdwR2 = WdwR*WdwR
    i_window = 1

  END SUBROUTINE circular_selection_postpro
  !!!------------------------------------------------------------------------------
  
  SUBROUTINE selection_translation_postpro(X,Y)

    IMPLICIT NONE

    REAL(kind=8) :: X,Y

    IF(i_window .NE. 1)THEN
       call faterr('postpro_2D::selection_translation','You must define first the circular selection')
    END IF

    TransX = X
    TransY = Y
    WdwX = WdwX0
    WdwY = WdwY0
    i_window = 2

  END SUBROUTINE selection_translation_postpro
  !!!------------------------------------------------------------------------------

  LOGICAL FUNCTION am_i_inside(X,Y)

    IMPLICIT NONE
    
    REAL(kind=8) :: X,Y
    
    am_i_inside = .FALSE.

    IF ( ( ( X - WdwX )*( X - WdwX )  &
         + ( Y - WdwY )*( Y - WdwY ) ) <= WdwR2 ) am_i_inside = .TRUE.
  
  END FUNCTION am_i_inside
!!!---------------------------------------------------------------------
  
  SUBROUTINE initialize_selection

    IMPLICIT NONE

    INTEGER                   :: i,irbdy2,it1,it2,it3
    REAL(kind=8)              :: rzone,Xmin,Xmax,Ymin,Ymax
    REAL(kind=8),DIMENSION(2) :: coorcd,coor1,coor2,coor3
    integer                   :: nb_TRIANGLE
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: CoorXY
    INTEGER,DIMENSION(:,:),ALLOCATABLE      :: TriNodes,TriNbrg
    INTEGER,DIMENSION(:),ALLOCATABLE        :: IndexXY
    REAL(kind=8)                            :: CellArea0
    REAL(kind=8)                            :: dX0,dY0
    
    SELECT CASE(i_window)
    CASE(0)
    CASE(1,2)

       BodyWindow = .FALSE.

       marea = 0.D0
       irbdy2 = 0
     
       Xmin = 1.D+20
       Xmax =-1.D+20
       Ymin = 1.D+20
       Ymax =-1.D+20
     
       DO i = 1,nb_RBDY2

          coorcd = get_coor(i,1)
          BodyWindow(i) = am_i_inside(coorcd(1),coorcd(2))
          
          IF ( BodyWindow(i) ) THEN 
             marea = marea + get_area(i)
             irbdy2 = irbdy2+1
          END IF
       END DO
     
       nb_INSIDE = irbdy2

       ALLOCATE(CoorXY(2,nb_INSIDE))
       ALLOCATE(IndexXY(nb_INSIDE))
       ALLOCATE(TriNodes(3,nb_INSIDE*2))
       ALLOCATE(TriNbrg(3,nb_INSIDE*2))
       
       irbdy2   = 0
       TriNodes = 0
       TriNbrg  = 0
       CoorXY   = 0
       
       DO i = 1,nb_RBDY2
          
          IF ( .NOT. BodyWindow(i) ) CYCLE 
          coorcd = get_coor(i,1)
          Xmin = MIN(Xmin,coorcd(1))
          Xmax = MAX(Xmax,coorcd(1))
          Ymin = MIN(Ymin,coorcd(2))
          Ymax = MAX(Ymax,coorcd(2))
          irbdy2 = irbdy2+1
          CoorXY(1:2,irbdy2) = coorcd(1:2)
          IndexXY(irbdy2) = irbdy2
       END DO
       
       nb_TRIANGLE = 0
       
       CALL dtris2 ( nb_INSIDE , CoorXY , IndexXY , nb_TRIANGLE , TriNodes , TriNbrg )
       
       irbdy2    = 0
       CellArea0 = 0
       dX0 = Xmax-Xmin
       dY0 = Ymax-Ymin
       
       DO i = 1,nb_TRIANGLE
          
          it1 = TriNodes(1,i)
          it2 = TriNodes(2,i)
          it3 = TriNodes(3,i)
          
          coor1 = coorXY(:,it1)
          coor2 = coorXY(:,it2)
          coor3 = coorXY(:,it3)
          
          CellArea0  = CellArea0 + 0.5*( & 
               ( ( coor2(1)-coor1(1) )*( coor3(2)-coor1(2) ) ) &
               - &
               ( ( coor2(2)-coor1(2) )*( coor3(1)-coor1(1) ) ) )
          
       END DO

       deALLOCATE(CoorXY)
       deALLOCATE(IndexXY)
       deALLOCATE(TriNodes)
       deALLOCATE(TriNbrg)

    END SELECT
    
  END SUBROUTINE initialize_selection
  !!!--------------------------------------------------------------------------------------
  
  SUBROUTINE update_selection

    IMPLICIT NONE

    SELECT CASE(i_window)
    CASE(0)
      return
    CASE(1)
    CASE(2)
       WdwX = WdwX0 + TransX*TPS
       WdwY = WdwY0 + TransY*TPS
    CASE default
    END SELECT

    call initialize_selection
    
  END SUBROUTINE update_selection
  !!!--------------------------------------------------------------------------------------

  subroutine clean_memory_postpro()
    implicit none
    integer(kind=4) :: i, j

    cfd_unit   = 0
    ncd_unit   = 0
    snap_unit  = 0
    esnap_unit = 0
    hb_unit    = 0
    ws_unit    = 0

    dissipated_energy = 0.D0

    if( allocated(PostCommand) ) then
      do i = 1, size(PostCommand)
        if( associated(postCommand(i)%io_unit) ) then
          deallocate(postCommand(i)%io_unit)
          nullify(postCommand(i)%io_unit)
        end if
      end do
      deallocate(PostCommand)
    end if
  
    if( allocated(TrackingID)    ) deallocate(TrackingID)
    if( allocated(bMPTrackingID) ) deallocate(bMPTrackingID)
    if( allocated(tMPTrackingID) ) deallocate(tMPTrackingID)
    nb_tracking    = 0
    nb_mp_tracking = 0

    if( allocated(Fnumber)  )  deallocate(Fnumber)
    if( allocated(FNnumber) )  deallocate(FNnumber)

    if( allocated(cplus)  )  deallocate(cplus)
    if( allocated(cmoins) )  deallocate(cmoins)
    if( allocated(ctotal) )  deallocate(ctotal)
  
    if( allocated(body_data_snapshot) )  deallocate(body_data_snapshot)
  
    if( allocated(SKE) )  deallocate(SKE)
    nb_species = 0

    if( allocated(C_ORI)  ) deallocate(C_ORI)
    if( allocated(SC_ORI) ) deallocate(SC_ORI)
    if( allocated(WC_ORI) ) deallocate(WC_ORI)
    if( allocated(T_ORI)  ) deallocate(T_ORI)
    if( allocated(ST_ORI) ) deallocate(ST_ORI)
    if( allocated(WT_ORI) ) deallocate(WT_ORI)

    if( allocated(clxxx_set) ) deallocate(clxxx_set)
    nb_clxxx_set = 0

    if(allocated(TorqueID) )  deallocate(TorqueID)
    nb_torque = 0
  
    if( allocated(RBDY2_set) ) then
      do i = 1, size(RBDY2_set)
        if( associated(RBDY2_set(i)%list) ) then
          deallocate(RBDY2_set(i)%list)
          nullify(RBDY2_set(i)%list)
        end if
      end do
      deallocate(RBDY2_set)
      nb_RBDY2_sets = 0
    end if

    if( allocated(bounded_set) ) then
      deallocate(bounded_set)
      nb_bounded_sets = 0
    end if

    if( allocated(HEATBOUNDS)  ) deallocate(HEATBOUNDS)
    nb_HB = 0
    if( associated(HEATvector) ) then
      deallocate(HEATvector)
      nullify(HEATvector)
    end if
    HB_NY = 0

    if( allocated(BodyWindow) ) deallocate(BodyWindow)

    if( allocated(MECAx_set) ) then
      do i = 1, size(MECAx_set)
        if( associated(MECAx_set(i)%DATA) ) then
          do j = 1, size(MECAx_set(i)%DATA)
            deallocate(MECAx_set(i)%DATA(j)%nodes)
            nullify(MECAx_set(i)%DATA(j)%nodes)
          end do
          deallocate(MECAx_set(i)%DATA)
          nullify(MECAx_set(i)%DATA)
        end if
      end do
      nb_mecax_sets = 0
      deallocate(MECAx_set)
    end if

    if( allocated(DIST_set) ) then
      do i = 1, size(DIST_set)
        if( associated(DIST_set(i)%DATA) ) then
          do j = 1, size(DIST_set(i)%DATA)
            deallocate(DIST_set(i)%DATA(j)%nodes)
            nullify(DIST_set(i)%DATA(j)%nodes)
          end do
          deallocate(DIST_set(i)%DATA)
          nullify(DIST_set(i)%DATA)
        end if
      end do
      nb_dist_sets = 0
      deallocate(DIST_set)
    end if

  end subroutine

END MODULE postpro

