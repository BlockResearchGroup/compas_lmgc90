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

!fd todo
!fd il faut que tout attaque verlet et pas this


MODULE postpro_3D

  use overall, only : H, TPS, NSTEP    , &
                      nbDIME, THETA    , &
                      location, in_post, &
                      i_real_tactor    , &
                      i_recup_tactor   , &
                      i_rough_tactor   , &
                      i_verlet_tactor  , &
                      nlgs_solver3D    , &
                      cpg_solver       , &
                      delete_part_of_file


  USE utilities
  USE bulk_behaviour
  USE tact_behaviour
  USE parameters

  USE RBDY3
  
  USE SPHER
  USE PLANx
  USE DNLYC
  USE CYLND
  USE POLYR

  USE SPSPx
  USE SPCDx, only: SPCDx2SPHER
  USE SPDCx, only: SPDCx2SPHER
  USE SPPLx, only: SPPLx2SPHER, &
                   SPPLx2PLANx
  USE PRPLx, only: PRPLx2POLYR
  USE PRPRx, only: PRPRx2POLYR , &
                   get_nb_PRPRx, &
                   pair_reaction_PRPRx
  use CSPRx, only: get_nb_CSPRx

  USE PTPT3, only: PTPT32PT3Dx
  
  USE nlgs_3D
  !use nlgs_new_int_3D, only: new_int_get_nlgs_loop => get_nlgs_loop
  USE cpg_3D
  USE mp_solver_3D

  USE MAILx                                                               
  USE mecaMAILx

  use inter_meca_handler_3D, only : get_nb_inters         , &
                                    get_nb_verlets        , &
                                    get_rloc              , &
                                    get_vloc              , &
                                    get_vlocBEGIN         , &
                                    get_gaps              , &
                                    get_tact_lawnb        , &
                                    this2verlet           , & ! beurk
                                    get_verlet_size       , &
                                    get_verlet_adjsz      , &
                                    get_verlet_iantac     , &
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
                                    get_verlet_internal
  
  IMPLICIT NONE

  PRIVATE

  INTEGER,PARAMETER :: &
       i_AVERAGE_VELOCITY_EVOLUTION  =  1 , &
       i_SPECIES_KINETIC_ENERGY      =  2 , &
       i_SNAPSHOT_SAMPLE             =  3 , &
       i_MP_SNAPSHOT_SAMPLE          =  4 , &
       i_NORMAL_CONTACT_DISTRIBUTION =  5 , & ! NOT YET USED
       i_CONTACT_FORCE_DISTRIBUTION  =  6 , &
       i_COORDINATION_NUMBER         =  7 , &
       i_BODY_TRACKING               =  8 , &
       i_MP_VALUE_TRACKING           =  9 , &
       i_TORQUE_EVOLUTION            = 10 , &
       i_TRIAXIAL_COMPACITY          = 11 , & !3D ONLY
       i_DOUBLET_INTERACTIONS        = 12 , &
       !*************************************
       i_QUASI_SLIDING_CONTACT       = 13 , &
       i_NEW_MECAx_SETS              = 14 , & 
       i_Fint_EVOLUTION              = 15 , & 
       i_Dep_EVOLUTION               = 16 , & 
       i_NEW_DIST_SETS               = 17 , & 
       i_MAILx_DIST                  = 18 , & 
       i_NEW_RIGID_SETS              = 19 , &
       i_NEW_BOUNDED_SETS            = 20 , &
       i_DISSIPATED_ENERGY           = 21 , &
       i_SOLVER_INFORMATIONS         = 22 , &
       i_DRY_CONTACT_NATURE          = 23 , &
       i_WET_CONTACT_NATURE          = 24 , &
       i_KINETIC_ENERGY              = 25 , &
       i_VIOLATION_EVOLUTION         = 26 , &
       i_INTER_ANALYSIS              = 27 , & !3D ONLY
       i_THERMAL_EVOLUTION           = 28 , &
       i_NETWORK_EVOLUTION           = 29 , &
       !*************************************
       i_ELECTRO_EVOLUTION           = 30 , &
       i_CLOUD_ANALYSIS              = 31 , &
       i_DISPLAY_TENSORS             = 32 , &
       i_HEAT_BOUND_PROFILE          = 33 , &
       !*************************************       
       i_VISIBILITY_STATE            = 34 , & !PTA 3D only
       i_PRxxx_DETECTION             = 35 , & !PTA 3D only
       !*************************************       
       i_DOUBLETS_TORQUE_EVOLUTION   = 36     !fd 

  !!! The number of command 

  INTEGER, parameter :: NB_COMMANDS = 36

  TYPE T_identity_command

     ! name            : name of the command
     CHARACTER(len=30) :: name
     ! used            : true if command called in file POSTPRO.DAT
     LOGICAL           :: used
     ! step            : execution period     
     INTEGER           :: step
     ! nb_files : number of files related to the command     
     INTEGER           :: nb_files
     ! files_io_unit   : list of io_unit related to the command           
     INTEGER,DIMENSION(:),POINTER :: io_unit  => null()
     character(len=40),DIMENSION(:),POINTER :: file_name => null()
     
  END TYPE T_identity_command

  TYPE(T_identity_command),DIMENSION(nb_commands) :: PostCommand
  
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

  !!! body tracking -------------------------------
  
  INTEGER                          :: nb_tracking
  INTEGER,DIMENSION(:),ALLOCATABLE :: TrackingID

  !!! mp value tracking -------------------------------
  
  INTEGER                          :: nb_mp_tracking
  INTEGER,DIMENSION(:),ALLOCATABLE :: bMPTrackingID,tMPTrackingID

  !!! contact forces distribution -------------------
  
  INTEGER                               :: Fsect
  INTEGER,DIMENSION(:),ALLOCATABLE      :: Fnumber,FNnumber
  INTEGER                               :: cfd_unit = 0
  CHARACTER(len=46)                     :: cfd_name1

  !!! Coordination Number ---------------------------
  
  INTEGER,DIMENSION(:),ALLOCATABLE :: cplus,cmoins,ctotal
  
  !!! dissipated energy -----------------------------
  
  REAL(kind=8) :: dissipated_energy = 0.D0

  !!! Display tensors --------------------------------
  
  !!! kinetic energy --------------------------------
  
  REAL(kind=8) :: KE = 0.D0, DE = 0.D0, PE = 0.D0

  !!! snapshot sample -------------------------------

  CHARACTER(len=31)                       :: body_snap_name
  CHARACTER(len=34)                       :: contact_snap_name
  INTEGER                                 :: snap_unit = 0
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: body_data_snapshot

  !!! inter analysis -------------------------------
  !                                123456789012345678901234567890123456789
  CHARACTER(len=38) :: snap_inter='POSTPRO/SNAP_INTER_ANALYSIS_0000000.DAT'
  INTEGER           :: nb_snap_inter,snap_inter_unit=0

  !!! species kinetic energy ------------------------

  INTEGER :: nb_species

  TYPE SPECIES
     REAL(kind=8)     :: KE
     REAL(kind=8)     :: DE
     REAL(kind=8)     :: P
     REAL(kind=8)     :: PE
     CHARACTER(len=5) :: name
  END TYPE SPECIES

  TYPE(SPECIES),DIMENSION(:),ALLOCATABLE  :: SKE

  !!! torque evolution ------------------------------
  
  INTEGER                          :: nb_torque
  INTEGER,DIMENSION(:),ALLOCATABLE :: TorqueID 
  
  !!! doublet interactions --------------------------
  
  INTEGER,DIMENSION(2)             :: doubletbdyty
  CHARACTER(len=5)                 :: doublet_type

  !-EVOLUTION CONTACT RT~muRN    --------------------------------
  
  REAL(kind=8) :: fraction

  !!! Triaxial compacity

  INTEGER                   :: iXmin,iXmax,iYmin,iYmax,iZmin,iZmax
  INTEGER                   :: run=0
  REAL(kind=8)              :: volume0
  REAL(kind=8),DIMENSION(6) :: size_planx

  !!!- NEW RIGID SETS ---------------------------------

  TYPE T_rigid_set
     INTEGER :: size
     INTEGER,DIMENSION(:),POINTER :: list => null()
  END TYPE T_rigid_set

  TYPE(T_rigid_set), DIMENSION(:), ALLOCATABLE :: RBDY3_set
  INTEGER :: nb_RBDY3_sets=0

  !!!- NEW BOUNDED SETS ---------------------------------

  TYPE T_bounded_set
     INTEGER :: idmin,idmax
  END TYPE T_bounded_set

  TYPE(T_bounded_set), DIMENSION(:), ALLOCATABLE :: bounded_set
  INTEGER :: nb_bounded_sets

  !!!- HEAT BOUND PROFILE ------------------------------------------------

  REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: HEATBOUNDS
  REAL(kind=8), DIMENSION(:), POINTER       :: HEATvector => null()

  INTEGER :: HB_NY,nb_HB
  INTEGER :: hb_unit = 0
  
  !-------------------------------------12345678901234567890123456789012345678
  CHARACTER(len=37) :: heat_bound_name='POSTPRO/HEAT_BOUND_PROFILE_0000000.DAT'

  !!!----------------------------------------------------
  ! gestion des sets pour la visu 
  !   du deplacement/vitesse/etc des noeuds d un maillage 
  !   des forces aux noeuds d'un maillage 
  !   des distances entre noeuds 

  INTEGER nb_mecax_sets,nb_dist_sets
  
  TYPE T_MECAx
     
     INTEGER                       :: iMECAx,nb_nodes
     INTEGER, DIMENSION(:),POINTER :: nodes => null()
     
  END TYPE T_MECAx
  
  TYPE T_set
     INTEGER ::nb_MECAx
     TYPE(T_MECAx),DIMENSION(:),POINTER :: DATA => null()
  END TYPE T_set
  
  TYPE(T_set), DIMENSION(:), ALLOCATABLE :: MECAx_set
  
  TYPE(T_set), DIMENSION(:), ALLOCATABLE :: DIST_set


  !!! doublets torque evolution ------------------------------
  
  INTEGER                            :: nb_dte
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: dte 

  !!!----------------------------------------------------

  INTEGER :: nb_GRAIN, nb_SPHER, nb_PLANx, nb_DNLYC, nb_POLYR, nb_CYLND
  INTEGER :: nb_CDANx, nb_SPSPx, nb_SPPLx, nb_SPCDx, nb_SPDCx, nb_PRPRx, nb_PRPLx, nb_PTPT3, nb_CDCDx
  INTEGER :: nb_RBDY3, nb_mecaMAILx

  !!!----------------------------------------------------

  PUBLIC &
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &
       postpro_during_computation, &
       flush_during_computation  , &
       compute_kinetic_energy, &
       get_rbdy3_princ_stress

  public clean_memory_postpro_3D


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
    !!  Increment the list of command and define their attributes
    !!  When a new command is added, you must:
    !!  1 - increment the parameter NB_COMMANDS
    !!  2 - add the name and all information after the last previous command
    !!****
    implicit none
    integer :: init_postpro_command

    integer :: inum, err
    character(len=80) :: cout
    
    !!! NB_COMMANDS is defined in the global variable
    
    DO inum = 1,NB_COMMANDS
       NULLIFY(PostCommand(inum)%io_unit)
       PostCommand(inum)%used     = .FALSE.
       PostCommand(inum)%nb_files  = 0
       NULLIFY(PostCommand(inum)%file_name)
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
    PostCommand(i_TRIAXIAL_COMPACITY)%name          = 'TRIAXIAL COMPACITY            '

    !#12#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DOUBLET_INTERACTIONS)%name        = 'DOUBLET INTERACTIONS          '

    !#13#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_QUASI_SLIDING_CONTACT)%name       = 'QUASI SLIDING CONTACT         '

    !#14#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_MECAx_SETS)%name              = 'NEW MECAx SETS                '

    !#15#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_Fint_EVOLUTION)%name              = 'Fint EVOLUTION                '

    !#16#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_Dep_EVOLUTION)%name               = 'Dep EVOLUTION                 '
 
    !#17#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_DIST_SETS)%name               = 'NEW DIST SETS                '
 
    !#18#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_MAILx_DIST)%name                  = 'MAILx DIST                    '

    !#19#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_RIGID_SETS)%name              = 'NEW RIGID SETS                '

    !#20#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NEW_BOUNDED_SETS)%name            = 'NEW BOUNDED SETS              '

    !#21#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DISSIPATED_ENERGY)%name           = 'DISSIPATED ENERGY             '

    !#22#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_SOLVER_INFORMATIONS)%name         = 'SOLVER INFORMATIONS           '

    !#23#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DRY_CONTACT_NATURE)%name          = 'DRY CONTACT NATURE            '

    !#24#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_WET_CONTACT_NATURE)%name          = 'WET CONTACT NATURE            '

    !#25#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_KINETIC_ENERGY)%name              = 'KINETIC ENERGY                '

    !#26#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_VIOLATION_EVOLUTION)%name         = 'VIOLATION EVOLUTION           '

    !#27#-------------------------------------------------------
    ! 
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_INTER_ANALYSIS)%name              = 'INTER ANALYSIS                '
    
    !#28#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_THERMAL_EVOLUTION)%name           = 'THERMAL EVOLUTION             '

    !#29#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_NETWORK_EVOLUTION)%name           = 'NETWORK EVOLUTION             '

    !#30#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_ELECTRO_EVOLUTION)%name           = 'ELECTRO EVOLUTION             '
 
    !#31#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_CLOUD_ANALYSIS)%name              = 'CLOUD ANALYSIS                '
 
    !#32#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DISPLAY_TENSORS)%name             = 'DISPLAY TENSORS               '

    !#33#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_HEAT_BOUND_PROFILE)%name          = 'HEAT BOUND PROFILE            '

    !#34#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_VISIBILITY_STATE)%name            = 'VISIBILITY STATE              '

    !#35#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_PRxxx_DETECTION)%name             = 'PRxxx DETECTION               '

    !#36#-------------------------------------------------------
    inum=inum+1
    !                                                  123456789012345678901234567890
    PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%name   = 'DOUBLETS TORQUE EVOLUTION     '

    IF (inum .NE. NB_COMMANDS) THEN
       write(cout,'(A)') 'There is some internal mismatch in mod_postpro'
       write(cout,'(A)') 'The number of command does not fit the existing number of command'
       call faterr('postpro_3D::init_postpro_command',cout)
    END IF

    ! file closed in start_postpro_command
    init_postpro_command = get_io_unit()
    open( unit=init_postpro_command, file=trim(location(in_post)), &
          status='OLD', iostat=err )
    
    if ( err > 0 ) then
       call faterr('postpro_3D::init_postpro_command','file POSTPRO.DAT cannot be found')
    end if
    
  end function init_postpro_command

  !!!-----------------------------------------------------------------------
  subroutine start_postpro(nfich, restart)
    !! PURPOSE
    !!  Reading file POSTPRO.DAT to size the command structure.
    !!  Determine if the commands are kwown, read command parameters and 
    !!  determine if the command must be run before, during or after computation 
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
    cfd_unit        = max(0,restart-1)
    snap_unit       = max(0,restart-1)
    snap_inter_unit = max(0,restart-1)
    hb_unit         = max(0,restart-1)

    nb_RBDY3 = get_nb_RBDY3()
    
    nb_SPHER = get_nb_SPHER()
    nb_PLANx = get_nb_PLANx()
    nb_DNLYC = get_nb_DNLYC()
    nb_CYLND = get_nb_CYLND()
    nb_POLYR = get_nb_POLYR()

    nb_mecaMAILx = get_nb_mecaMAILx()

    nb_GRAIN = nb_SPHER+nb_POLYR

    !!! Check if problem occur during opening file ---------------------

    nfich_loc = get_io_unit()

    OPEN(UNIT=nfich_loc,FILE=TRIM(location('POSTPRO/POSTPRO.OUT')),STATUS='REPLACE',IOSTAT=err)
    
    IF (err.GT.0) THEN
       CALL FATERR(IAM,'POSTPRO directory not found')
    END IF
    
    CLOSE(nfich_loc)

    OPEN(UNIT=nfich,FILE=TRIM(location(in_post)),STATUS='OLD',IOSTAT=err)
    
    IF (err.GT.0) THEN
       CALL FATERR(IAM,'file POSTPRO.DAT cannot be found')
    END IF

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
             call faterr(IAM,cout)
          END IF
          
          READ(CLIN(5:30),*) PostCommand(i)%step
             
          call reading_data(i, nfich, restart)

          EXIT
          
       END DO
       !
       IF (INDEX(clin,'END').EQ.1) EXIT

       IF (clin_test.EQ.0) THEN
          IF ((clin(1:4).NE.'STEP') .AND.(clin(1:1).NE.'#')) THEN
             WRITE(cout,'(A11,A30,A16)') ' @ Command ',clin,' does not exist'
             CALL LOGMES(cout)
             CALL LOGMES(' @ or does not match with the command names below:')
             CALL LOGMES(' ')
             DO i=1,SIZE(PostCommand)
                WRITE(cout,'(A5,i2,A2,A30)') ' @ n�',i,': ',PostCommand(i)%name
                CALL LOGMES(cout)
             END DO
             call faterr(IAM,'read log')
          END IF
       END IF
    END DO
    
    ! file opened in init_postpro_command
    CLOSE(nfich)

    !!! ---------------------

    !IF (.NOT. ALLOCATED(BodyWindow)) ALLOCATE(BodyWindow(nb_RBDY3))
    !BodyWindow = .TRUE.
    !IF (post_processing) CALL initialize_selection

  END SUBROUTINE start_postpro

  !!!--------------------------------------------------------
  SUBROUTINE close_postpro_files

    IMPLICIT NONE
    
    INTEGER :: i,j
   
    DO i=1,SIZE(PostCommand)

       IF (.NOT.PostCommand(i)%used) CYCLE

       DO j=1, PostCommand(i)%nb_files
          if (PostCommand(i)%io_unit(j) /= 0 ) CLOSE(PostCommand(i)%io_unit(j))
       END DO

    END DO
    
  END SUBROUTINE close_postpro_files

  !!!---------------------------------------------------------------------------------
  subroutine reading_data(ICNAME, nfich, restart)
    !! INPUT
    !!  ic      command identifiant
    !!  nfich   file number
    !! PURPOSE
    !!  read parameters of each command and initialize corresponding data
    !!****
    implicit none
    integer, intent(in) :: ICNAME
    integer, intent(in) :: nfich
    integer, intent(in) :: restart
    !
    character(len=6)  :: file_position
    character(len=7)  :: file_status
    character(len=30) :: clin
    character(len=40) :: file_name, file_name1, file_name2, file_name3, file_name4
    integer           :: i, j, k, NX, NY, NZ
    integer           :: err, nb_files, nfich0, itacty

    REAL(kind=8),DIMENSION(3) :: DATA
    character(len=80) :: cout
    character(len=24) :: IAM
          !123456789012345678901234
    IAM = 'postpro_3D::reading_data'

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
       PostCommand(i_AVERAGE_VELOCITY_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_AVERAGE_VELOCITY_EVOLUTION)%io_unit(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/AVERAGE_VELOCITY_EVOLUTION.DAT  '

       nfich0 = get_io_unit()
       PostCommand(i_AVERAGE_VELOCITY_EVOLUTION)%io_unit(1)=nfich0
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
          READ(nfich,'(A5)') SKE(i)%name
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
             WRITE(file_name(9:13),'(A5)') SKE(i)%name
          ELSE
             CALL LOGMES(' @ WARNING : Species number exceeded')
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

       IF(.NOT.ALLOCATED(body_data_snapshot)) ALLOCATE(body_data_snapshot(20,nb_RBDY3))
       !!!------------------123456789012345678901234567890123456
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

       IF(.NOT.ALLOCATED(body_data_snapshot)) ALLOCATE(body_data_snapshot(26,nb_RBDY3))
       !!!------------------123456789012345678901234567890123456
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

       !!! NOT YET SUPPORT
       call faterr(IAM,'NORMAL CONTACT DISTRIBUTION command not yet supported')

    !!! #6 
    !!! --------------------------------------
    !!! COMMAND CONTACT_FORCE_DISTRIBUTION
    !!! --------------------------------------
    CASE(i_CONTACT_FORCE_DISTRIBUTION)
       
       !READ(nfich,'(I5)') Fsect
       READ(nfich,*) Fsect

       IF(.NOT.ALLOCATED(Fnumber))  ALLOCATE(Fnumber(Fsect))
       IF(.NOT.ALLOCATED(FNnumber)) ALLOCATE(FNnumber(Fsect))

       !!!----------1234567890123456789012345678901234567890123456
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

       IF(nb_GRAIN .NE. 0) THEN 
          IF(.NOT.ALLOCATED(cplus))  ALLOCATE( cplus(nb_GRAIN))
          IF(.NOT.ALLOCATED(ctotal)) ALLOCATE(ctotal(nb_GRAIN))
          IF(.NOT.ALLOCATED(cmoins)) ALLOCATE(cmoins(nb_GRAIN))
       END IF

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
       ALLOCATE(PostCommand(i_BODY_TRACKING)%file_name(nb_files))

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/BODY_0000000.DAT                '
       
       DO i = 1,nb_files
          
          IF (i.LT.100000) THEN
             WRITE(file_name(14:20),'(I7.7)') TrackingID(i)
          ELSE 
             CALL LOGMES(' @ Number of tracked bodies exceeded')
             CALL LOGMES(' @ Run postpro with the 100th first only')
             EXIT
          END IF

          nfich0 = get_io_unit()

          PostCommand(i_BODY_TRACKING)%io_unit(i)=0
          PostCommand(i_BODY_TRACKING)%file_name(i)=' '
          PostCommand(i_BODY_TRACKING)%file_name(i)=file_name

          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
          close(nfich0)
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
             WRITE(file_name(19:33),'(I7.7,A1,I7.7)') bMPTrackingID(i),'_',tMPTrackingID(i)
          ELSE 
             CALL LOGMES(' @ Number of tracked bodies exceeded')
             EXIT
          END IF
          nfich0 = get_io_unit()
          PostCommand(i_MP_VALUE_TRACKING)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=TRIM(location(file_name)), status=trim(file_status), &
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

       nb_files=3*nb_torque

       PostCommand(i_TORQUE_EVOLUTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_TORQUE_EVOLUTION)%io_unit(nb_files))

       !-------------1234567890123456789012345678901234567890
       file_name1 = 'POSTPRO/REAC_0000000.DAT                '
       file_name2 = 'POSTPRO/Fext_0000000.DAT                '
       file_name3 = 'POSTPRO/Fine_0000000.DAT                '

       DO i = 1,nb_torque
          IF ( i .LT. 10000000) THEN
             WRITE(file_name1(14:20),'(I7.7)') TorqueID(i)
             WRITE(file_name2(14:20),'(I7.7)') TorqueID(i)
             WRITE(file_name3(14:20),'(I7.7)') TorqueID(i)
          ELSE
             CALL LOGMES(' @ WARNING : Torque number exceeded')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_TORQUE_EVOLUTION)%io_unit(3*i-2)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name1)))
          open( unit=nfich0, file=trim(location(file_name1)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_TORQUE_EVOLUTION)%io_unit(3*i-1)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name2)))
          open( unit=nfich0, file=trim(location(file_name2)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_TORQUE_EVOLUTION)%io_unit(3*i)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name3)))
          open( unit=nfich0, file=trim(location(file_name3)), status=trim(file_status), &
                position=trim(file_position) )

       END DO

    !!! #11
    !!! --------------------------------------
    !!! COMMAND TRIAXIAL_COMPACITY
    !!! --------------------------------------
    CASE(i_TRIAXIAL_COMPACITY)

       READ(nfich,'(I5)') iXmin
       READ(nfich,'(I5)') iXmax
       READ(nfich,'(I5)') iYmin
       READ(nfich,'(I5)') iYmax
       READ(nfich,'(I5)') iZmin
       READ(nfich,'(I5)') iZmax

       volume0 = 0.D0

       DO i=1,nb_SPHER
          volume0 = volume0 + get_volume(spher2bdyty(1,i))
       END DO
       DO i=1,nb_POLYR
          volume0 = volume0 + get_volume(polyr2bdyty(1,i))
       END DO
       IF(volume0.EQ.0)THEN
          write(cout,'(A)') ' @ WARNING: particle volume seems to be equal to zero'
          write(cout,'(A)') ' @ Command TRIAXIAL COMPACITY can not work'
          call faterr(IAM,cout)
       END IF

       !mr must be extented to multi-tactors

       itacty=1

       CALL get_data(iXMin,itacty,DATA)
       size_planx(1) = MINVAL(DATA)
       CALL get_data(iXMax,itacty,DATA)
       size_planx(2) = MINVAL(DATA)
       CALL get_data(iYMin,itacty,DATA)
       size_planx(3) = MINVAL(DATA)
       CALL get_data(iYMax,itacty,DATA)
       size_planx(4) = MINVAL(DATA)
       CALL get_data(iZMin,itacty,DATA)
       size_planx(5) = MINVAL(DATA)
       CALL get_data(iZMax,itacty,DATA)
       size_planx(6) = MINVAL(DATA)

       nb_files = 1
       PostCommand(i_TRIAXIAL_COMPACITY)%nb_files = nb_files
       ALLOCATE(PostCommand(i_TRIAXIAL_COMPACITY)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_TRIAXIAL_COMPACITY)%io_unit(i)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/TRIAXIAL_COMPACITY.DAT          '
       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )
 
    !!! #12
    !!! --------------------------------------
    !!! COMMAND DOUBLET_INTERACTION
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
    !!! COMMAND QUASI_SLIDING_CONTACT
    !!! --------------------------------------
    CASE(i_QUASI_SLIDING_CONTACT)

       !READ(nfich,'(ES14.7)') fraction
       READ(nfich,*) fraction

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
       
    !!! #14
    !!! --------------------------------------
    !!! COMMAND NEW_MECAx_SETS
    !!! --------------------------------------
    CASE(i_NEW_MECAx_SETS)

       READ(nfich,'(I7)') nb_MECAx_sets
       
       IF (ALLOCATED(MECAx_set)) DEALLOCATE(MECAx_set)
       ALLOCATE(MECAx_set(nb_MECAx_sets))
       !       
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

    !!! #15
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
             write(cout,'(A)') 'You cannot compute Fint evolution of more than 999 sets'
             call faterr(IAM,cout)
         END IF
          nfich0 = get_io_unit()
          PostCommand(i_Fint_EVOLUTION)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

    !!! #16
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
             write(cout,'(A)') 'You cannot compute Dep evolution of more than 999 sets'
             call faterr(IAM,cout)
          ENDIF
          nfich0 = get_io_unit()
          PostCommand(i_Dep_EVOLUTION)%io_unit(i) = nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

    !!! #17
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

    !!! #18
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

    !!! #19
    !!! --------------------------------------
    !!! COMMAND NEW_RIGID_SETS
    !!! --------------------------------------
    CASE(i_NEW_RIGID_SETS)

       READ(nfich,'(I7)') nb_RBDY3_sets

       IF (ALLOCATED(RBDY3_set)) DEALLOCATE(RBDY3_set)
       ALLOCATE(RBDY3_set(nb_RBDY3_sets))
       
       DO i=1,nb_RBDY3_sets
          READ(nfich,'(I7)') RBDY3_set(i)%size
          ALLOCATE(RBDY3_set(i)%list(RBDY3_set(i)%size))
          DO j =1,RBDY3_set(i)%size
             READ(nfich,'(I7)') RBDY3_set(i)%list(j)
          END DO
       END DO

       nb_files = 4*nb_RBDY3_sets

       PostCommand(i_NEW_RIGID_SETS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_NEW_RIGID_SETS)%io_unit(nb_files))

       !-------------1234567890123456789012345678901234567890
       file_name1 = 'POSTPRO/EVOL_RIGID_SET_0000000.DAT      '
       file_name2 = 'POSTPRO/REAC_RIGID_SET_0000000.DAT      '
       file_name3 = 'POSTPRO/Fext_RIGID_SET_0000000.DAT      '
       file_name4 = 'POSTPRO/Fine_RIGID_SET_0000000.DAT      '

       DO i = 1,nb_RBDY3_sets

          IF ( i .LT. 10000000) THEN
             WRITE(file_name1(24:30),'(I7.7)') i
             WRITE(file_name2(24:30),'(I7.7)') i
             WRITE(file_name3(24:30),'(I7.7)') i
             WRITE(file_name4(24:30),'(I7.7)') i
          ELSE
             CALL LOGMES(' @ WARNING : rigid set number exceeded')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i-3)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name1)))
          open( unit=nfich0, file=trim(location(file_name1)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i-2)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name2)))
          open( unit=nfich0, file=trim(location(file_name2)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i-1)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name3)))
          open( unit=nfich0, file=trim(location(file_name3)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name4)))
          open( unit=nfich0, file=trim(location(file_name4)), status=trim(file_status), &
                position=trim(file_position) )

       END DO

    !!! #20
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

       nb_files = 4*nb_bounded_sets

       PostCommand(i_NEW_BOUNDED_SETS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_NEW_BOUNDED_SETS)%io_unit(nb_files))

       !-------------1234567890123456789012345678901234567890
       file_name1 = 'POSTPRO/EVOL_BOUNDED_SET_0000000.DAT    '
       file_name2 = 'POSTPRO/REAC_BOUNDED_SET_0000000.DAT    '
       file_name3 = 'POSTPRO/Fext_BOUNDED_SET_0000000.DAT    '
       file_name4 = 'POSTPRO/Fine_BOUNDED_SET_0000000.DAT    '
       
       DO i = 1,nb_bounded_sets
          IF ( i .LT. 10000000) THEN
             WRITE(file_name1(26:32),'(I7.7)') i
             WRITE(file_name2(26:32),'(I7.7)') i
             WRITE(file_name3(26:32),'(I7.7)') i
             WRITE(file_name4(26:32),'(I7.7)') i
          ELSE
             CALL LOGMES(' @ WARNING : bounded set number exceeded')
             CALL LOGMES(' @ Only the 100000th first ones will be checked')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_NEW_BOUNDED_SETS)%io_unit(4*i-3)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name1)))
          open( unit=nfich0, file=trim(location(file_name1)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_BOUNDED_SETS)%io_unit(4*i-2)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name2)))
          open( unit=nfich0, file=trim(location(file_name2)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_BOUNDED_SETS)%io_unit(4*i-1)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name3)))
          open( unit=nfich0, file=trim(location(file_name3)), status=trim(file_status), &
                position=trim(file_position) )

          nfich0 = get_io_unit()
          PostCommand(i_NEW_BOUNDED_SETS)%io_unit(4*i)=nfich0
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name4)))
          open( unit=nfich0, file=trim(location(file_name4)), status=trim(file_status), &
                position=trim(file_position) )

       END DO

    !!! #21
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

    !!! #22
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

    !!! #23
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

    !!! #24
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


    !!! #25
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

    !!! #26
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

    !!! #27
    !!! --------------------------------------
    !!! COMMAND INTER_ANALYSIS
    !!! --------------------------------------
    CASE(i_INTER_ANALYSIS)

       nb_files=1

       PostCommand(i_INTER_ANALYSIS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_INTER_ANALYSIS)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_INTER_ANALYSIS)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/INTER_ANALYSIS.DAT              '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

    !!! #28
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

    !!! #29
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

    !!! #30
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
       file_name = 'POSTPRO/ELECTRO_EVOLUTION.DAT           '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

    !!! #31
    !!! --------------------------------------
    !!! COMMAND CLOUD_ANALYSIS
    !!! --------------------------------------
    CASE(i_CLOUD_ANALYSIS)

       nb_files = 6

       PostCommand(i_CLOUD_ANALYSIS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_CLOUD_ANALYSIS)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_CLOUD_ANALYSIS)%io_unit(i) = nfich0
          
          !-----------------------1234567890123456789012345678901234567890
          IF(i.EQ.1) file_name = 'POSTPRO/CLOUD_SPSPx.DAT                 '
          IF(i.EQ.2) file_name = 'POSTPRO/CLOUD_SPPLx.DAT                 '
          IF(i.EQ.3) file_name = 'POSTPRO/CLOUD_SPCDx.DAT                 '
          IF(i.EQ.4) file_name = 'POSTPRO/CLOUD_SPDCx.DAT                 '
          IF(i.EQ.5) file_name = 'POSTPRO/CLOUD_PRPRx.DAT                 '
          IF(i.EQ.6) file_name = 'POSTPRO/CLOUD_PRPLx.DAT                 '

          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

    !!! #32
    !!! --------------------------------------
    !!! COMMAND DISPLAY_TENSORS
    !!! --------------------------------------
    CASE(i_DISPLAY_TENSORS)

       nb_files = 2

       PostCommand(i_DISPLAY_TENSORS)%nb_files = nb_files
       ALLOCATE(PostCommand(i_DISPLAY_TENSORS)%io_unit(nb_files))

       DO i=1,nb_files
          nfich0 = get_io_unit()
          PostCommand(i_DISPLAY_TENSORS)%io_unit(i) = nfich0
          
          !-----------------------1234567890123456789012345678901234567890
          IF(i.EQ.1) file_name = 'POSTPRO/DISPLAY_STRESS_TENSOR.DAT       '
          IF(i.EQ.2) file_name = 'POSTPRO/DISPLAY_FABRIC_TENSOR.DAT       '

          if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
          open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
                position=trim(file_position) )
       END DO

    !!! #33
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
          CALL get_HEAT_bound_dims(i,NX,NY,NZ)
          HB_NY = MAX(HB_NY,NY)
       END DO

       ALLOCATE(HEATBOUNDS(nb_HB,HB_NY))
       HEATBOUNDS = 0.D0
       ALLOCATE(HEATvector(HB_NY))
       HEATvector = 0.D0

       ! already made at the previous function call
       ! hb_unit = restart-1

    !!! #34
    !!! --------------------------------------
    !!! COMMAND VISIBILITY_STATE
    !!! --------------------------------------
    CASE(i_VISIBILITY_STATE)

       nb_files=1

       PostCommand(i_VISIBILITY_STATE)%nb_files = nb_files
       ALLOCATE(PostCommand(i_VISIBILITY_STATE)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_VISIBILITY_STATE)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/VISIBILITY_STATE.DAT            '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

    !!! #35
    !!! --------------------------------------
    !!! COMMAND PRxxx_DETECTION
    !!! --------------------------------------
    CASE(i_PRxxx_DETECTION)

       nb_files=1

       PostCommand(i_PRxxx_DETECTION)%nb_files = nb_files
       ALLOCATE(PostCommand(i_PRxxx_DETECTION)%io_unit(nb_files))

       nfich0 = get_io_unit()
       PostCommand(i_PRxxx_DETECTION)%io_unit(1)=nfich0

       !------------1234567890123456789012345678901234567890
       file_name = 'POSTPRO/PRxxx_DETECTION.DAT            '

       if( restart > 0 ) call delete_part_of_file(trim(location(file_name)))
       open( unit=nfich0, file=trim(location(file_name)), status=trim(file_status), &
             position=trim(file_position) )

    !!! #36
    !!! --------------------------------------
    !!! COMMAND DOUBLETS_TORQUE_EVOLUTION
    !!! --------------------------------------
    CASE(i_DOUBLETS_TORQUE_EVOLUTION)

       READ(nfich,*) nb_dte
       !print*,nb_dte
       ALLOCATE(dte(2,nb_dte))
       DO i=1,nb_dte
          READ(nfich,*) dte(1,i),dte(2,i)
          !print*,i,dte(1,i),dte(2,i)
       END DO

       PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%nb_files = nb_dte
       ALLOCATE(PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%io_unit(nb_dte))
       ALLOCATE(PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%file_name(nb_dte))

       !-------------1234567890123456789012345678901234567890
       file_name1 = 'POSTPRO/DTE_0000000_0000000.DAT         '

       DO i = 1,nb_dte
          IF ( i .LT. 10000000) THEN
             WRITE(file_name1(13:19),'(I7.7)') dte(1,i)
             WRITE(file_name1(21:27),'(I7.7)') dte(2,i)
          ELSE
             CALL LOGMES(' @ WARNING : Doublets Torque number exceeded')
             CALL LOGMES(' @ Only the 100th first ones will be checked')
             EXIT
          END IF
          
          nfich0 = get_io_unit()
          PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%io_unit(i)=0
          PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%file_name(i)=' '
          PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%file_name(i)=file_name1
          if( restart > 0 ) call delete_part_of_file(trim(location(file_name1)))
          open( unit=nfich0, file=trim(location(file_name1)), status=trim(file_status), &
                position=trim(file_position) )
          close(nfich0)

       END DO

    !!! --------------------------------------
    CASE DEFAULT
       call faterr(IAM,'Unknown command in POSTPRO.DAT')
    END SELECT

  END SUBROUTINE reading_data

  !!--------------------------------------------------------------------------------------
  SUBROUTINE postpro_during_computation
    
    IMPLICIT NONE
  
    INTEGER :: ic

    nb_SPSPx = get_nb_inters( i_SPSPx )
    nb_SPPLx = get_nb_inters( i_SPPLx )
    nb_SPCDx = get_nb_inters( i_SPCDx )
    nb_PRPRx = get_nb_inters( i_PRPRx )
    nb_PRPLx = get_nb_inters( i_PRPLx )
    nb_PTPT3 = get_nb_inters( i_PTPT3 )
    nb_CDCDx = get_nb_inters( i_CDCDx )

    nb_CDANx = nb_SPSPx+nb_SPPLx+nb_SPCDx+nb_PRPRx+nb_PRPLx

    !!! For post processing
    !!! IF(post_processing) CALL update_selection

    !!! mr: Commands are classed according to the alphabetical order.
    !!!     Please respect this order if you add a new command.

    DO ic = 1,NB_COMMANDS
       
       IF(.NOT.PostCommand(ic)%used) CYCLE
       IF (PostCommand(ic)%step == 0) CYCLE
       IF (MODULO(Nstep,PostCommand(ic)%step) .NE. 0) CYCLE

       SELECT CASE(ic)

       CASE(i_AVERAGE_VELOCITY_EVOLUTION)
          CALL average_velocity_evolution

       CASE(i_BODY_TRACKING)
          CALL BODY_TRACKING(1)

       CASE(i_CLOUD_ANALYSIS)
          CALL cloud_analysis

       CASE(i_CONTACT_FORCE_DISTRIBUTION)
          CALL contact_force_distribution

       CASE(i_COORDINATION_NUMBER)
          CALL coordination_number

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

       CASE(i_PRxxx_DETECTION)
          CALL prxxx_detection

       CASE(i_INTER_ANALYSIS)
          CALL INTER_analysis

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

       CASE(i_TRIAXIAL_COMPACITY)
          CALL triaxial_compacity

       CASE(i_VIOLATION_EVOLUTION)
          CALL violation_evolution

       CASE(i_VISIBILITY_STATE)
          CALL visibility_state

       CASE(i_WET_CONTACT_NATURE)
          CALL wet_contact_nature

       CASE(i_DOUBLETS_TORQUE_EVOLUTION)
          CALL doublets_torque_evolution

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

  !!!--------------------------------------------------------------------------------------------
  !!! *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** 
  !!!--------------------------------------------------------------------------------------------  
  !!!--------------------------------------------------------------------------------------------
  !!!
  !!! HERE START SUBROUTINES FOR POSTPROCESSING PURPOSES
  !!!
  !!!--------------------------------------------------------------------------------------------
  !!!--------------------------------------------------------------------------------------------
  !!! *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** 
  !!!--------------------------------------------------------------------------------------------
  SUBROUTINE BODY_TRACKING(iFLAG)

    IMPLICIT NONE
    
    INTEGER                   :: iFLAG
    INTEGER                   :: i,j,ibdyty,nb
    REAL(kind=8),DIMENSION(6) :: V
    REAL(kind=8),DIMENSION(3) :: coor, X
    INTEGER                   :: nfich
    LOGICAL                   :: visible


    SELECT CASE(iFLAG)

    !!! BODY TRACKING
    CASE(1)

       DO i=1,nb_tracking

          ibdyty = TrackingID(i)

          COOR = get_coor(ibdyty,0)
          V    = get_V(ibdyty)
          X    = get_X(ibdyty)

          nfich = get_io_unit()
          OPEN(unit=nfich,file=TRIM(location(PostCommand(i_BODY_TRACKING)%file_name(i))),status='OLD',POSITION='APPEND')

          WRITE(nfich,'(ES15.8,12(1X,ES14.7))') TPS,coor(1),coor(2),coor(3),X(1),X(2),X(3), &
                                                V(1),V(2),V(3),V(4),V(5),V(6)

          close(nfich)
       END DO

    !!! NEW RIGID SETS
    CASE(2)    
       DO i=1, nb_RBDY3_sets
          COOR  = 0.D0
          X     = 0.D0
          V     = 0.D0
          
          nb    = 0
          
          DO j=1,RBDY3_set(i)%size
          
             ibdyty  = RBDY3_set(i)%list(j)
             visible = get_visible(ibdyty)
             IF (.NOT. VISIBLE) CYCLE

             COOR  = COOR + get_coor(ibdyty, 1)
             X     = X    + get_X(ibdyty)
             V     = V    + get_V(ibdyty)

             nb = nb + 1

          END DO

          if (nb/=0) then
             COOR  = COOR/REAL(nb,8)
             X     = X/REAL(nb,8)
             V     = V/REAL(nb,8)
          end if

          nfich = Postcommand(i_NEW_RIGID_SETS)%io_unit(4*i-3)

          WRITE(nfich,'(ES15.8,12(1X,ES14.7))') TPS,coor(1),coor(2),coor(3),X(1),X(2),X(3), &
                                                V(1),V(2),V(3),V(4),V(5),V(6)
                                         
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

          nfich = Postcommand(i_NEW_BOUNDED_SETS)%io_unit(4*i-3)
          WRITE(nfich,'(ES15.8,12(1X,ES14.7))') TPS,coor(1),coor(2),coor(3),X(1),X(2),X(3), &
                                                V(1),V(2),V(3),V(4),V(5),V(6)
       END DO
    END SELECT

  END SUBROUTINE BODY_TRACKING

  !!!-------------------------------------------------------------------------------------------- 
  !!! *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** LMGC90 *** 
  !!!--------------------------------------------------------------------------------------------
  SUBROUTINE TORQUE_EVOLUTION(iFLAG)

    IMPLICIT NONE
    
    INTEGER                   :: iFLAG
    INTEGER                   :: i,j,ibdyty,nfich
    REAL(kind=8),DIMENSION(6) :: REAC,Fext,Fine
    LOGICAL                   :: visible


    SELECT CASE(iFLAG)

    !!! TORQUE EVOLUTION 
    CASE(1)

       DO i=1,nb_torque
          ibdyty = TorqueID(i)
          REAC  = get_REAC(ibdyty)
          Fext  = get_Fext(ibdyty)
          Fine  = get_Finertia(ibdyty)

          nfich = PostCommand(i_TORQUE_EVOLUTION)%io_unit(3*i-2)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),REAC(4),REAC(5),REAC(6)

          nfich = PostCommand(i_TORQUE_EVOLUTION)%io_unit(3*i-1)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,Fext(1),Fext(2),Fext(3),Fext(4),Fext(5),Fext(6)

          nfich = PostCommand(i_TORQUE_EVOLUTION)%io_unit(3*i)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,Fine(1),Fine(2),Fine(3),Fine(4),Fine(5),Fine(6)

       END DO

    !!! NEW RIGID SETS
    CASE(2)

       DO i=1, nb_RBDY3_sets

          REAC = 0.D0
          Fext = 0.D0
          Fine = 0.D0

          DO j=1, RBDY3_set(i)%size

             ibdyty = RBDY3_set(i)%list(j)
             visible = get_visible(ibdyty)

             IF (.NOT. visible) CYCLE

             REAC = REAC + get_REAC(ibdyty)
             Fext = Fext + get_Fext(ibdyty)
             Fine = Fine + get_Finertia(ibdyty)

          END DO

          nfich = PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i-2)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),REAC(4),REAC(5),REAC(6)

          nfich = PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i-1)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,Fext(1),Fext(2),Fext(3),Fext(4),Fext(5),Fext(6)

          nfich = PostCommand(i_NEW_RIGID_SETS)%io_unit(4*i)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,Fine(1),Fine(2),Fine(3),Fine(4),Fine(5),Fine(6)

       END DO

    !!! NEW BOUNDED SETS
    CASE(3)
       
       DO i=1,nb_bounded_sets

          REAC = 0.D0
          Fext = 0.D0
          Fine = 0.D0

          DO ibdyty = bounded_set(i)%idmin,bounded_set(i)%idmax
             
             VISIBLE = get_visible(ibdyty)
             IF(.NOT.VISIBLE) CYCLE

             REAC = REAC + get_REAC(ibdyty)
             Fext = Fext + get_Fext(ibdyty)
             Fine = Fine + get_Finertia(ibdyty)

          END DO
          
          nfich = Postcommand(i_NEW_BOUNDED_SETS)%io_unit(4*i-2)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),REAC(4),REAC(5),REAC(6)

          nfich = Postcommand(i_NEW_BOUNDED_SETS)%io_unit(4*i-1)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,Fext(1),Fext(2),Fext(3),Fext(4),Fext(5),Fext(6)

          nfich = Postcommand(i_NEW_BOUNDED_SETS)%io_unit(4*i)
          WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,Fine(1),Fine(2),Fine(3),Fine(4),Fine(5),Fine(6)

       END DO

    END SELECT

  END SUBROUTINE TORQUE_EVOLUTION

  !!!------------------------------------------------------
  SUBROUTINE solver_informations
  
    IMPLICIT NONE
    
    INTEGER      :: compteur,contacts,nfich
    REAL(kind=8) :: err1,err2,err3

    compteur = 0
    err1 = 0.d0
    err2 = 0.d0
    err3 = 0.d0
    contacts = 0

    IF(nlgs_solver3D)THEN
       CALL get_nlgs3D_loop(compteur,err1,err2,err3,contacts)
    ELSE IF(cpg_solver)THEN
       CALL get_cpg_loop(compteur,err1,err2,err3,contacts)
    END IF

    nfich = PostCommand(i_SOLVER_INFORMATIONS)%io_unit(1)
    WRITE(nfich,'(ES15.8,1X,I8,3(1X,ES14.7),1X,I8)') TPS,compteur,err1,err2,err3,contacts
    
  END SUBROUTINE solver_informations

  !!!-------------------------------------------------------------------------------------
  subroutine quasi_sliding_contact_aux( id_inter, nb_inter, fraction, nb_limit )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    real( kind = 8 )    :: fraction
    integer( kind = 4 ) :: nb_limit

    ! Local variables
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: lawnb
    real( kind = 8 )    :: fric
    integer( kind = 4 ) :: statusBEGIN
    real( kind = 8 )    :: rls
    real( kind = 8 )    :: rlt
    real( kind = 8 )    :: rln
    real( kind = 8 )    :: normT
    real( kind = 8 )    :: treshold_sup
    real( kind = 8 )    :: treshold_inf

    do icdan = 1, nb_inter
       
       lawnb = get_tact_lawnb( id_inter, icdan )
       
       fric = get_fric( lawnb, statusBEGIN )
       call get_rloc( id_inter, icdan, rlt, rln, rls, statusBEGIN )
       
       normT = SQRT( rls*rls + rlt*rlt )
       
       treshold_sup = fric * rln
       treshold_inf = fric * rln * ( 1. - fraction )
       
       if ( normT < treshold_inf ) CYCLE
       if ( normT > treshold_sup ) CYCLE
       
       nb_limit = nb_limit + 1
       
    end do

  end subroutine quasi_sliding_contact_aux

  !!!-------------------------------------------------------------------------------------
  SUBROUTINE quasi_sliding_contact

    IMPLICIT NONE
    
    INTEGER         :: icdan,lawnb,nb_limit,nb_CDAN
    REAL(kind=8)    :: rln,rlt,rls,fric,normT,treshold_sup,treshold_inf
    integer(kind=4) :: statusBEGIN
    INTEGER         :: nfich
 
    nb_limit = 0

    call quasi_sliding_contact_aux( i_prprx, nb_PRPRx, fraction, nb_limit )

    call quasi_sliding_contact_aux( i_prplx, nb_PRPLx, fraction, nb_limit )

    call quasi_sliding_contact_aux( i_spspx, nb_SPSPx, fraction, nb_limit )

    call quasi_sliding_contact_aux( i_spplx, nb_SPPLx, fraction, nb_limit )

    call quasi_sliding_contact_aux( i_spcdx, nb_SPCDx, fraction, nb_limit )

    call quasi_sliding_contact_aux( i_spdcx, nb_SPDCx, fraction, nb_limit )

    nb_CDAN = nb_SPSPx + nb_SPPLx + nb_SPCDx + nb_SPDCx + nb_PRPRx + nb_PRPLx

    nb_CDAN = MAX(nb_CDAN,1)

    nfich = PostCommand(i_QUASI_SLIDING_CONTACT)%io_unit(1) 
    WRITE(nfich,'(ES15.8,1X,I10,1x,ES14.7)') TPS,nb_limit,nb_limit/REAL(nb_CDAN,8)
   
  END SUBROUTINE quasi_sliding_contact

  !!!--------------------------------------------------------------------------------------
  subroutine coordination_number_aux_( id_inter, nb_inter )
    implicit none
    integer, intent(in) :: id_inter
    integer, intent(in) :: nb_inter
    ! Local variables
    integer      :: icdan
    integer      :: icdtac, iantac, icdtmp, iantmp
    integer      :: iadj
    real(kind=8) :: rt, rn, rs
    integer      :: status
    integer      :: cd_ct,cd_cm,cd_cp,an_ct,an_cm,an_cp

    integer :: i
    
    icdtmp = 0
    iantmp = 0
    cd_cp=0
    cd_cm=0
    cd_ct=0
    an_cp=0
    an_cm=0
    an_ct=0

    do icdan = 1, nb_inter
       
       call this2verlet( id_inter, icdan, icdtac, iadj )
       call get_verlet_rloc( id_inter, icdtac, iadj, status, rt, rn, rs )
       iantac = get_verlet_lantac( id_inter, icdtac, iadj )

       if (id_inter==i_prprx .or. id_inter==i_prplx) then       
         !!! For contact between polyhedron there is the case of multiple contacts.

         ! si on change de set de contact on actualise les tableaux
         if ((icdtmp /=0 .and. iantmp /= 0) .and. (icdtac /= icdtmp .and. iantac /= iantmp )) then
           ctotal(icdtmp) = ctotal(icdtmp) + min(1,cd_ct)
           cplus(icdtmp)  = cplus(icdtmp)  + min(1,cd_cp)
           cmoins(icdtmp) = cmoins(icdtmp) + min(1,cd_cm)

           ctotal(iantmp) = ctotal(iantmp) + min(1,an_ct)
           cplus(iantmp)  = cplus(iantmp)  + min(1,an_cp)
           cmoins(iantmp) = cmoins(iantmp) + min(1,an_cm)
          
           icdtmp = 0
           iantmp = 0
           
         end if

         ! si nouveau set on initialise
         if (icdtmp == 0 .and. iantmp == 0) then
           icdtmp = icdtac
           iantmp = iantac
           cd_cp=0
           cd_cm=0
           cd_ct=0
           an_cp=0
           an_cm=0
           an_ct=0
         endif


         ! on ajoute
         cd_ct = cd_ct+1
         if ( rn .GT. 0.D0 ) then
           cd_cp = cd_cp + 1
         else if ( rn .LT. 0.D0 ) then
           cd_cm = cd_cm + 1
         end if

         if (id_inter==i_prprx) then
           an_ct = an_ct + 1
           if ( rn .GT. 0.D0 ) then
             an_cp = an_cp + 1
           else if ( rn .LT. 0.D0 ) then
             an_cm = an_cm + 1
           end if
         endif
       else 
         !! contact pour les spheres         

         ctotal(icdtac) = ctotal(icdtac) + 1

         if ( rn .GT. 0.D0 ) then
           cplus(icdtac) = cplus(icdtac) + 1
         else if ( rn .LT. 0.D0 ) then
           cmoins(icdtac) =  cmoins(icdtac) + 1
         end if

         if( id_inter==i_spspx ) then
           ctotal(iantac) = ctotal(iantac) + 1
           if ( rn .GT. 0.D0 ) then
             cplus(iantac) = cplus(iantac) + 1
           else if ( rn .LT. 0.D0 ) then
             cmoins(iantac) =  cmoins(iantac) + 1
           end if
         end if

       endif  

    end do

    !!! For contact between polyhedron there is the case of multiple contacts.
    
    if (id_inter==i_prprx .or. id_inter==i_prplx) then       
      if( nb_inter > 0 ) then
          ctotal(icdtmp) = ctotal(icdtmp) + min(1,cd_ct)
          cplus(icdtmp)  = cplus(icdtmp)  + min(1,cd_cp)
          cmoins(icdtmp) = cmoins(icdtmp) + min(1,cd_cm)

          ctotal(iantmp) = ctotal(iantmp) + min(1,an_ct)
          cplus(iantmp)  = cplus(iantmp)  + min(1,an_cp)
          cmoins(iantmp) = cmoins(iantmp) + min(1,an_cm)
      end if
    end if

  end subroutine coordination_number_aux_
  !
  SUBROUTINE coordination_number

    IMPLICIT NONE
    
    INTEGER      :: nfich
    REAL(kind=8) :: val0,val1,val2,val3
    integer      :: i,tp,tm,tt
    
    integer( kind = 4 ) :: id_inter

    IF (nb_GRAIN.NE.0) THEN
    
      cplus  = 0
      cmoins = 0
      ctotal = 0

      ! print*,'SPSPx'
      call coordination_number_aux_( i_spspx, nb_SPSPx )

      ! print*,'SPPLx'
      call coordination_number_aux_( i_spplx, nb_SPPLx )

      ! print*,'SPCDx'
      call coordination_number_aux_( i_spcdx, nb_SPCDx )
      
      ! print*,'SPDCx'      
      call coordination_number_aux_( i_spdcx, nb_SPDCx )

      ! print*,'PRPRx'
      call coordination_number_aux_( i_prprx, nb_PRPRx )

      ! print*,'PRPLx'      
      call coordination_number_aux_( i_prplx, nb_PRPLx )

      !!!-----------------------

      ! on compte les grains qui ont des contacts ...
      tp=0
      tm=0
      tt=0
      do i=1,nb_grain
        if (cplus(i) /= 0 .or. cmoins(i) /= 0) tt=tt+1
        if (cplus(i) /= 0) then
           tp=tp+1
        endif   
        if (cmoins(i) /= 0) then
           tm=tm+1
        endif   
      enddo   

      ! on calcule la coordination moyenne sum(nb contact "actifs")/nb_grains actifs
      if (tp /= 0 ) then
        val1 = real(SUM(cplus))/real(tp)
      else
        val1 = 0.d0
      endif

      if (tm /= 0 ) then 
        val2 = real(SUM(cmoins))/real(tm)
      else
        val2 = 0.d0
      endif

      if (tt /= 0) then
        !fd un peu foireux 
        val3 = real(SUM(cplus) + SUM(cmoins)) / real(tt) 
      else
        val3 = 0.d0   
      endif

      ! info geometrique
      val0 = real(SUM(ctotal))/real(nb_grain)
      
      nfich = PostCommand(i_COORDINATION_NUMBER)%io_unit(1)
      WRITE(nfich,'(ES15.8,4(1X,ES14.7))') TPS,val0,val1,val2,val3

      !print*,'fd CN ',nb_grain,tt,tp,tm,SUM(ctotal),SUM(cplus),SUM(cmoins)

      
    endif

    
  END SUBROUTINE coordination_number

  !!!----------------------------------------------------------------------------
  subroutine snapshot_sample_aux( id_inter, nb_inter, contactor1_2bdyty, contactor2_2bdyty, H, nfich2, module_name, &
       body_data_snapshot )

    implicit none

    integer( kind = 4 )  :: id_inter
    integer( kind = 4 )  :: nb_inter
    integer( kind = 4 ), dimension( : , : ), pointer :: contactor1_2bdyty
    integer( kind = 4 ), dimension( : , : ), pointer :: contactor2_2bdyty
    real( kind = 8 )     :: H
    integer( kind = 4 )  :: nfich2
    character( len = 5 ) :: module_name
    real( kind = 8 ), dimension( : , : ), allocatable :: body_data_snapshot
    
    ! Local variables
    integer :: icdan, icdtac, iadj, nb_adj
    integer :: icdbdy, ianbdy
    integer :: icd, ian, status, verlet_size
    real(kind=8), dimension(3) :: Ipoint
    real(kind=8), dimension(3) :: tuc, nuc, suc
    real(kind=8)               :: RlocT, RlocN, RlocS
    real(kind=8)               :: vt, vn, vs
    real(kind=8), dimension(6) :: internal
    real(kind=8), dimension(3) :: Fik, rcd, ran

    verlet_size = get_verlet_size(id_inter)
    if( verlet_size == 0 ) return

    icdan = 0
    do icdtac = 1, verlet_size

      nb_adj = get_verlet_adjsz(id_inter, icdtac)

      do iadj = 1, nb_adj

        icdan = icdan + 1

        icdbdy = get_verlet_icdbdy(id_inter, icdtac)
        ianbdy = get_verlet_ianbdy(id_inter, icdtac, iadj)

        call get_verlet_local_frame( id_inter, icdtac, iadj, Ipoint, tuc, nuc, suc )
        call get_verlet_rloc( id_inter, icdtac, iadj, status, RlocT, RlocN, RlocS )
        call get_verlet_vloc( id_inter, icdtac, iadj, vt, vn, vs )

        internal = 0.d0
        call get_verlet_internal( id_inter, icdtac, iadj, internal )

        icd = contactor1_2bdyty( 1, icdbdy )
        ian = contactor2_2bdyty( 1, ianbdy )
       
        Fik = (RlocN*nuc) + (RlocT*tuc) + (RlocS*suc)
        
        rcd(1:3) = body_data_snapshot(1:3,icd) - Ipoint(1:3)
        
        body_data_snapshot(11,icd) = body_data_snapshot(11,icd) + rcd(1)*Fik(1)
        body_data_snapshot(12,icd) = body_data_snapshot(12,icd) + rcd(1)*Fik(2)
        body_data_snapshot(13,icd) = body_data_snapshot(13,icd) + rcd(1)*Fik(3)
        body_data_snapshot(14,icd) = body_data_snapshot(14,icd) + rcd(2)*Fik(1)
        body_data_snapshot(15,icd) = body_data_snapshot(15,icd) + rcd(2)*Fik(2)
        body_data_snapshot(16,icd) = body_data_snapshot(16,icd) + rcd(2)*Fik(3)
        body_data_snapshot(17,icd) = body_data_snapshot(17,icd) + rcd(3)*Fik(1)
        body_data_snapshot(18,icd) = body_data_snapshot(18,icd) + rcd(3)*Fik(2)
        body_data_snapshot(19,icd) = body_data_snapshot(19,icd) + rcd(3)*Fik(3)

        ran(1:3) = body_data_snapshot(1:3,ian) - Ipoint(1:3)
        
        body_data_snapshot(11,ian) = body_data_snapshot(11,ian) - ran(1)*Fik(1)
        body_data_snapshot(12,ian) = body_data_snapshot(12,ian) - ran(1)*Fik(2)
        body_data_snapshot(13,ian) = body_data_snapshot(13,ian) - ran(1)*Fik(3)
        body_data_snapshot(14,ian) = body_data_snapshot(14,ian) - ran(2)*Fik(1)
        body_data_snapshot(15,ian) = body_data_snapshot(15,ian) - ran(2)*Fik(2)
        body_data_snapshot(16,ian) = body_data_snapshot(16,ian) - ran(2)*Fik(3)
        body_data_snapshot(17,ian) = body_data_snapshot(17,ian) - ran(3)*Fik(1)
        body_data_snapshot(18,ian) = body_data_snapshot(18,ian) - ran(3)*Fik(2)
        body_data_snapshot(19,ian) = body_data_snapshot(19,ian) - ran(3)*Fik(3)

        body_data_snapshot(20,icd) = body_data_snapshot(20,icd) + 1
        body_data_snapshot(20,ian) = body_data_snapshot(20,ian) + 1
  
        WRITE(nfich2,'(1X,A5,3(1X,I7),15(1X,ES14.7))') module_name,icdan,icd,ian,Ipoint,RlocN,RlocT,RlocS,vn,vt,vs,internal(1:6)

      end do
    end do

  end subroutine snapshot_sample_aux
  !!!----------------------------------------------------------------------------
  !!  create a snapshot of mechanical informations of the sample in two files:
  !!  - The body_snapshot file contains information for bodies
  !!  - The contact snapshot file contains information for contacts
  !!  The active contacts are: SPSPx, SPPLx, SPCDx, SPDCx, PRPRx and PRPLx.
  !!****
  SUBROUTINE snapshot_sample(FLAG)

    IMPLICIT NONE

    LOGICAL                     :: FLAG
    integer                     :: i, ibdyty, icdbdy, ianbdy, icd, ian, nb_cdan
    integer                     :: iadj, nbadj, icdan, itacty, nb_tacty
    integer                     :: ID_RBDY3, ID_TACTY, nfich1, nfich2
    REAL(kind=8)                :: RlocN,RlocT,RlocS,volume,vn,vt,vs,WS,Ti,TCond,Ei,ECond
    REAL(kind=8),DIMENSION(1)   :: radius
    REAL(kind=8),DIMENSION(3)   :: Ipoint,suc,tuc,nuc,Fik,rcd,ran
    REAL(kind=8),DIMENSION(3)   :: coor
    REAL(kind=8),DIMENSION(6)   :: Vbegin,internal
    integer     , dimension(6)  :: inters_id

    IF (nb_RBDY3.EQ.0) RETURN

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

    OPEN(unit=nfich1,file=TRIM(location(body_snap_name)),status='REPLACE')
    OPEN(unit=nfich2,file=TRIM(location(contact_snap_name)),status='REPLACE')

    nb_CDAN = nb_SPSPx + nb_SPPLx + nb_SPCDx + nb_SPDCx &
            + nb_PRPRx + nb_PRPLx

    inters_id = (/ i_spspx, i_spplx, i_spcdx, i_spdcx, &
                   i_prprx, i_prplx                    /)
    nb_CDAN = 0
    do i = 1, 6
      nb_CDAN = nb_CDAN + get_nb_verlets( inters_id(i) )
    end do

    WRITE(nfich2,'(I7)') nb_CDAN
    !!!
    DO icdbdy=1,nb_RBDY3

       coor   = get_coor(icdbdy,0)
       Vbegin = get_Vbegin(icdbdy)
       volume = get_volume(icdbdy)

       body_data_snapshot(1:3,icdbdy) = coor(1:3)
       body_data_snapshot(4,icdbdy)   = volume

       body_data_snapshot(5:10,icdbdy) = Vbegin(1:6)

       body_data_snapshot(11:20,icdbdy) = 0.0

    END DO
    !
    !mr TO DO ADD MP VALUE
    !
    IF(FLAG)THEN
       DO icdbdy=1,nb_RBDY3
          body_data_snapshot(21:26,icdbdy) = 0.0

          TCond = 0.D0
          Ti    = 0.D0
          ECond = 0.D0
          Ei    = 0.D0
          WS    = 0.D0

          nb_tacty = get_nb_tacty(icdbdy)

          DO itacty=1,nb_tacty
             Ti    = Ti    + get_thermal_value(icdbdy,itacty)
             TCond = TCond + get_therm_cond(icdbdy,itacty)
             ECond = ECond + get_elec_cond(icdbdy,itacty)
             Ei    = Ei    + get_electric_potentiel(icdbdy,itacty)
             WS    = WS    + get_WS(icdbdy,itacty)
          END DO
       
          TCond = TCond/REAL(nb_tacty,8)
          Ti    = Ti/REAL(nb_tacty,8)
          ECond = ECond/REAL(nb_tacty,8)
          Ei    = Ei/REAL(nb_tacty,8)
          WS    = WS/REAL(nb_tacty,8)

          body_data_snapshot(21,icdbdy) = Ti
          body_data_snapshot(22,icdbdy) = TCond
          body_data_snapshot(23,icdbdy) = Ei
          body_data_snapshot(24,icdbdy) = ECond
          body_data_snapshot(25,icdbdy) = WS

       END DO
    END IF


    call snapshot_sample_aux( i_spspx, nb_SPSPx, spher2bdyty, spher2bdyty, H, nfich2, 'SPSPx', &
                              body_data_snapshot )
    call snapshot_sample_aux( i_spplx, nb_SPPLx, spher2bdyty, planx2bdyty, H, nfich2, 'SPPLx', &
                              body_data_snapshot )
    call snapshot_sample_aux( i_spcdx, nb_SPCDx, spher2bdyty, cylnd2bdyty, H, nfich2, 'SPCDx', &
                              body_data_snapshot )
    call snapshot_sample_aux( i_spdcx, nb_SPDCx, spher2bdyty, dnlyc2bdyty, H, nfich2, 'SPDCx', &
                              body_data_snapshot )
    call snapshot_sample_aux( i_prprx, nb_PRPRx, polyr2bdyty, polyr2bdyty, H, nfich2, 'PRPRx', &
                              body_data_snapshot )
    call snapshot_sample_aux( i_prplx, nb_PRPLx, spher2bdyty, planx2bdyty, H, nfich2, 'PRPLx', &
                              body_data_snapshot )

    IF(nb_RBDY3 /= 0)THEN
       IF(FLAG)THEN
          DO icdbdy=1,nb_RBDY3
             WRITE(nfich1,'(26(1X,ES14.7))') body_data_snapshot(1:26,icdbdy)
          END DO
       ELSE
          DO icdbdy=1,nb_RBDY3
             WRITE(nfich1,'(20(1X,ES14.7))') body_data_snapshot(1:20,icdbdy)
          END DO
       END IF
    END IF

    CLOSE(nfich1)
    CLOSE(nfich2)

  END SUBROUTINE snapshot_sample

  !!!-----------------------------------------------------------------------------------
  SUBROUTINE species_kinetic_energy

    IMPLICIT NONE

    INTEGER                   :: is,ibdyty
    REAL(kind=8)              :: mass,gyr
    REAL(kind=8),DIMENSION(6) :: V,Reac
    REAL(kind=8),DIMENSION(3) :: coor,I
    INTEGER                   :: nfich
    CHARACTER(len=5)          :: color

    nfich = PostCommand(i_SPECIES_KINETIC_ENERGY)%io_unit(1) 
    
    DO is =1,nb_species
       SKE(is)%KE = 0.D0
       SKE(is)%P  = 0.D0
       SKE(is)%PE = 0.D0
    END DO

    mass   = 0.D0
    V      = 0.D0

    DO ibdyty=1,nb_RBDY3

       color  = get_color(ibdyty,1)

       DO is =1,nb_species
          IF ( color == SKE(is)%name )THEN
             V    = get_V(ibdyty)
             mass = get_mass(ibdyty)
             coor = get_coor(ibdyty,0)
             
             I    = get_inertia_tensor(ibdyty)

             Reac = get_Reac(ibdyty) + get_Fext(ibdyty)

             SKE(is)%KE = SKE(is)%KE + 0.5*mass*(V(1)*V(1) + V(2)*V(2) + V(3)*V(3)) &
                                     + 0.5*(I(1)*V(4)*V(4) + I(2)*V(5)*V(5) + I(3)*V(6)*V(6))
             SKE(is)%PE = SKE(is)%PE + mass*( coor(1)*grav1 + coor(2)*grav2 + coor(3)*grav3)
             SKE(is)%P  = SKE(is)%P  + Reac(1)*V(1) + Reac(2)*V(2) + Reac(3)*V(3) &
                                     + Reac(4)*V(4) + Reac(5)*V(5) + Reac(6)*V(6)

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

  !!!-----------------------------------------------------------------------------------
  SUBROUTINE compute_bounded_sets

    IMPLICIT NONE
    REAL(kind=8)              :: POWER
    REAL(kind=8),DIMENSION(6) :: reac,V
    REAL(kind=8),DIMENSION(3) :: X
    INTEGER                   :: i,j,nb
    INTEGER                   :: nfich
    LOGICAL                   :: visible

    DO i=1,nb_bounded_sets

       reac  = 0.D0
       V     = 0.D0
       X     = 0.D0
       nb    = 0
       POWER = 0.D0

       DO j = bounded_set(i)%idmin,bounded_set(i)%idmax

          VISIBLE = get_visible(j)
          IF(.NOT.VISIBLE) CYCLE

          REAC  = reac + get_reac(j)
          V     = V    + get_V(j)
          POWER = REAC(1)*V(1) + REAC(2)*V(2) + REAC(3)*V(3)
          nb = nb + 1

       END DO

       V     = V/REAL(nb,8)
       X     = X/REAL(nb,8)


       nfich = Postcommand(i_NEW_BOUNDED_SETS)%io_unit(i)
       WRITE(nfich,'(ES15.8,7(1X,ES14.7))') TPS,REAC(1),REAC(2),REAC(3),V(1),V(2),V(3),POWER

    END DO

  END SUBROUTINE compute_bounded_sets

  !!!-----------------------------------------------------------------------------------
  subroutine inter_analysis_aux_( id_inter, nb_inter, nfich )
    implicit none
    integer, intent(in) :: id_inter, nb_inter, nfich
    !
    integer :: ik, iadj, status
    integer :: icdtac, iantac
    integer :: icdbdy, ianbdy
    real(kind=8) :: Rtik, Rnik, Rsik
    real(kind=8) :: gapini, gap
    real(kind=8), dimension(3) :: tik, nik, sik, coorik, Fik

    do ik = 1, nb_inter

       call this2verlet( id_inter, ik, icdtac, iadj )
       call get_verlet_rloc( id_inter, icdtac, iadj, status, Rtik, Rnik, Rsik )
       call get_verlet_local_frame( id_inter, icdtac, iadj, coorik, tik, nik, sik )

       call get_gaps( id_inter, ik, gapini, gap )

       icdbdy = get_verlet_icdbdy(id_inter, icdtac)
       ianbdy = get_verlet_ianbdy(id_inter, icdtac, iadj)
       iantac = get_verlet_iantac(id_inter, icdtac, iadj)

       Rsik = Rsik
       Rtik = Rtik
       Rnik = Rnik
       Fik = Rsik*sik + Rtik*tik + Rnik*nik

       write(nfich,'(A5,4(1X,I5),11(1X,ES14.7))') get_interaction_name_from_id(id_inter), &
                                                & icdbdy, icdtac, ianbdy, iantac, &
                                                & Rsik, Rtik, Rnik, coorik(1:3),  &
                                                & Fik(1:3), gapini, gap
    end do
 
  end subroutine inter_analysis_aux_

  SUBROUTINE inter_analysis

    IMPLICIT NONE

    INTEGER                   :: nfich
    INTEGER                   :: ik,icdbdy,ianbdy,icdtac,iantac,iadj
    REAL(kind=8)              :: Rsik,Rtik,Rnik,gapini,gap
    REAL(kind=8),DIMENSION(3) :: coorik,sik,tik,nik,Fik
    CHARACTER(len=5)          :: statusik

    integer :: id_inter

    snap_inter_unit = snap_inter_unit + 1

    IF(snap_inter_unit.GT.9999999)THEN
       RETURN
    ELSE
       WRITE(snap_inter(29:35),'(I7.7)') snap_inter_unit
    END IF

    nfich = PostCommand(i_INTER_ANALYSIS)%io_unit(1)
    OPEN(unit=nfich,file=TRIM(location(snap_inter)),status='REPLACE')
    
    WRITE(nfich,'(3(1X,I7))') nb_SPSPx,nb_SPPLx,nb_PTPT3
    
    IF (nb_SPSPx+nb_SPPLx+nb_PTPT3.EQ.0) THEN 
       CLOSE(nfich)
       RETURN
    END IF

    call inter_analysis_aux_( i_spspx, nb_SPSPx, nfich )
    call inter_analysis_aux_( i_spplx, nb_SPPLx, nfich )
    call inter_analysis_aux_( i_ptpt3, nb_PTPT3, nfich )
  
    CLOSE(nfich)

  END SUBROUTINE inter_analysis

  !!!-----------------------------------------------------------------------------------
  subroutine comp_dissipated_energy_aux( id_inter, nb_inter, energy )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    real( kind = 8 )    :: energy

    ! Local variables
    integer( kind = 4 ) :: ik
    real( kind = 8 )    :: rls
    real( kind = 8 )    :: rlt
    real( kind = 8 )    :: rln
    integer( kind = 4 ) :: status
    real( kind = 8 )    :: vls
    real( kind = 8 )    :: vlt
    real( kind = 8 )    :: vln
    real( kind = 8 )    :: vlsBEGIN
    real( kind = 8 )    :: vltBEGIN
    real( kind = 8 )    :: vlnBEGIN
    real( kind = 8 )    :: gap
    real( kind = 8 )    :: TT
    real( kind = 8 )    :: UMTT
    real( kind = 8 )    :: us
    real( kind = 8 )    :: ut
    real( kind = 8 )    :: un

    TT   = THETA
    UMTT = 1.0 - THETA

    DO ik = 1, nb_inter

       call get_rloc( id_inter, ik, rlt, rln, rls, status )
       IF (status .EQ. i_noctc) CYCLE
       IF (status .EQ. i_vnish) CYCLE
       CALL get_vloc( id_inter, ik, vlt, vln, vls )
       CALL get_vlocBEGIN( id_inter, ik, vltBEGIN, vlnBEGIN, vlsBEGIN, gap, status )

       un = UMTT * vlnBegin + TT * vln
       ut = UMTT * vltBegin + TT * vlt
       us = UMTT * vlsBegin + TT * vls

       energy = energy - ( un*rln +  ut*rlt + us*rls )

    END DO

  end subroutine comp_dissipated_energy_aux

  !!!-----------------------------------------------------------------------------------
  SUBROUTINE comp_dissipated_energy

    IMPLICIT NONE

    INTEGER         :: ik,nfich
    REAL(kind=8)    :: rln,rlt,rls,vln,vlt,vls,vlnBEGIN,vltBEGIN,vlsBEGIN
    REAL(kind=8)    :: un,ut,us,normR,normV,gap,TT,UMTT
    integer(kind=4) :: status

    !- dissipated energy -----------------------------

    REAL(kind=8),SAVE :: energy = 0.D0

    TT   = THETA
    UMTT = 1.0 - THETA
    energy = 0.0D0

    call comp_dissipated_energy_aux( i_spspx, nb_SPSPx, energy )

    call comp_dissipated_energy_aux( i_spplx, nb_SPPLx, energy )

    call comp_dissipated_energy_aux( i_spcdx, nb_SPCDx, energy )

    call comp_dissipated_energy_aux( i_prprx, nb_PRPRx, energy )

    call comp_dissipated_energy_aux( i_prplx, nb_PRPLx, energy )

    dissipated_energy = dissipated_energy + energy

    nfich = PostCommand(i_DISSIPATED_ENERGY)%io_unit(1)

    WRITE(nfich,'(ES15.8,2(1X,ES14.7))') TPS,dissipated_energy,energy
           
  END SUBROUTINE comp_dissipated_energy

  !!!-PTA--------------------------------------
  FUNCTION compute_kinetic_energy()

    IMPLICIT NONE
    INTEGER                   :: ibdyty,itacty
    REAL(kind=8)              :: compute_kinetic_energy
    REAL(kind=8)              :: mass
    REAL(kind=8),DIMENSION(3) :: coor,I
    REAL(kind=8),DIMENSION(6) :: V,Reac
    
    KE        = 0.D0
    mass      = 0.D0
    V    = 0.D0

    ! pour les corps deformables:
    KE = KE + compute_kinetic_energy_mecaMAILx()

    ! pour les rigides:
    DO ibdyty=1,nb_RBDY3
       
       V          = get_V(ibdyty)
       mass       = get_mass(ibdyty)
       Reac       = get_Reac(ibdyty) + get_Fext(ibdyty)
       coor       = get_coor(ibdyty,1)
       I          = get_inertia_tensor(ibdyty)
       KE         = KE + 0.5*mass*(V(1)*V(1) + V(2)*V(2) + V(3)*V(3)) &
                       + 0.5*(I(1)*V(4)*V(4) + I(2)*V(5)*V(5) + I(3)*V(6)*V(6))
    END DO

    compute_kinetic_energy = KE

  END FUNCTION compute_kinetic_energy

  !!!------------------------------------------
  SUBROUTINE kinetic_energy

    IMPLICIT NONE

    ! kinetic energy, potential energy, work of external/internal forces 
    REAL(kind=8)  :: KE,Wext,Wpot
    ! dissipated energy (cumulated)
    REAL(kind=8),save :: DE=0.D0, RWext=0.d0


    INTEGER                   :: ibdyty,itacty
    REAL(kind=8)              :: mass,Puissance
    REAL(kind=8),DIMENSION(3) :: X,I
    REAL(kind=8),DIMENSION(6) :: V,Reac
    INTEGER                   :: nfich

    REAL(kind=8) :: Eg_cin,Eg_def,Eg_pot,Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con

    Eg_pot = 0.D0
    Eg_cin = 0.D0
    Eg_def = 0.D0

    nfich = PostCommand(i_KINETIC_ENERGY)%io_unit(1) 

    mass   = 0.D0
    V      = 0.D0
    
    KE     = 0.D0
    Wext   = 0.D0  
    Wpot   = 0.D0  

    DO ibdyty=1,nb_RBDY3

       mass       = get_mass(ibdyty)       
       I          = get_inertia_tensor(ibdyty)

       X          = get_X(ibdyty)
       V          = get_V(ibdyty)

       Reac       = get_Reac(ibdyty) + get_Fext(ibdyty)

       KE         = KE   + 0.5*mass*(V(1)*V(1) + V(2)*V(2) + V(3)*V(3)) &
                         + 0.5*(I(1)*V(4)*V(4) + I(2)*V(5)*V(5) + I(3)*V(6)*V(6))
       RWext      = RWext + H*(Reac(1)*V(1) + Reac(2)*V(2) + Reac(3)*V(3) &
                          +    Reac(4)*V(4) + Reac(5)*V(5) + Reac(6)*V(6))
       Wpot       = Wpot + mass*( X(1)*grav1 + X(2)*grav2 + X(3)*grav3)
    END DO
    
    call compute_energy_mecaMAILx(Eg_cin,Eg_def,Eg_pot)

    call compute_work_mecaMAILx(Wg_cin,Wg_def,Wg_pot,Wg_ddl,Wg_con)

    KE = KE + Eg_cin
    Wext = RWext + Wg_ddl + Wg_con
    Wpot = Wpot + Wg_pot

    ! dissipation <- a revoir

    DE = DE + Wg_def - Eg_def

    ! kinetic,external,potential,dissipated
    WRITE(nfich,'(ES15.8,5(1X,ES14.7))') TPS,KE,KE+Eg_Def,-Wext,-Wpot,DE

           
  END SUBROUTINE kinetic_energy

  !!!----------------------------------------------------------------------------
  subroutine violation_evolution_aux( id_inter, nb_inter, &
       total_gapBegin, max_gapBegin, total_gap, max_gap )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    real( kind = 8 )    :: total_gapBegin
    real( kind = 8 )    :: max_gapBegin
    real( kind = 8 )    :: total_gap
    real( kind = 8 )    :: max_gap

    ! Local variables
    integer( kind = 4 ) :: icdan
    real( kind = 8 )    :: gapBegin
    real( kind = 8 )    :: gap

    do icdan = 1, nb_inter
       call get_gaps( id_inter, icdan, gapBegin, gap )

       gapBegin = min( 0.D0, gapBegin )
       gap      = min( 0.D0, gap )

       total_gapBegin = total_gapBegin - gapBegin     
       total_gap      = total_gap      - gap     

       max_gapBegin  = max( max_gapBegin, -gapBegin )
       max_gap       = max( max_gap, -gap )
    end do

  end subroutine violation_evolution_aux

  !!!----------------------------------------------------------------------------
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
    
    call violation_evolution_aux( i_spspx, nb_SPSPx, &
         total_gapBegin, max_gapBegin, total_gap, max_gap )

    call violation_evolution_aux( i_spplx, nb_SPPLx, &
         total_gapBegin, max_gapBegin, total_gap, max_gap )

    call violation_evolution_aux( i_spcdx, nb_SPCDx, &
         total_gapBegin, max_gapBegin, total_gap, max_gap )

    call violation_evolution_aux( i_prprx, nb_PRPRx, &
         total_gapBegin, max_gapBegin, total_gap, max_gap )

    call violation_evolution_aux( i_prplx, nb_PRPLx, &
         total_gapBegin, max_gapBegin, total_gap, max_gap )

    call violation_evolution_aux( i_cdcdx, nb_CDCDx, &
         total_gapBegin, max_gapBegin, total_gap, max_gap )

    nb_CDAN = nb_PRPLx + nb_PRPRx + nb_SPSPx + nb_SPPLx + nb_SPCDx + nb_CDCDx
    nb_CDAN = MAX( nb_CDAN, 1 )

    nfich = PostCommand(i_VIOLATION_EVOLUTION)%io_unit(1) 

    WRITE(nfich,'(ES15.8,4(1X,ES14.7))') TPS,total_gapBegin/REAL(nb_CDAN,8),total_gap/REAL(nb_CDAN,8),max_gapBegin,max_gap
   
  END SUBROUTINE violation_evolution

  !!! ---------------------------------------------------
  subroutine cloud_analysis_aux( id_inter, nb_inter, i_CLOUD_ANALYSIS )
    
    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    integer( kind = 4 ) :: i_CLOUD_ANALYSIS

    ! Local variables
    integer( kind = 4 )  :: ik
    real( kind = 8 )     :: rls
    real( kind = 8 )     :: rlt
    real( kind = 8 )     :: rln
    integer( kind = 4 )  :: status
    integer( kind = 4 )  :: nfich

    do ik = 1, nb_inter
       call get_rloc( id_inter, ik, rlt, rln, rls, status )
       nfich = PostCommand( i_CLOUD_ANALYSIS )%io_unit( 1 )
       write( nfich, '(3(1X,ES14.7))' ) rls, rlt, rln
    end do
    
  end subroutine cloud_analysis_aux

  !!! ---------------------------------------------------
  SUBROUTINE cloud_analysis

    IMPLICIT NONE
    REAL(kind=8)     :: rls,rlt,rln
    INTEGER          :: ik,nfich
    integer(kind=4)  :: status

    call cloud_analysis_aux( i_spspx, nb_SPSPx, i_CLOUD_ANALYSIS )
    
    call cloud_analysis_aux( i_spplx, nb_SPPLx, i_CLOUD_ANALYSIS )

    call cloud_analysis_aux( i_spcdx, nb_SPCDx, i_CLOUD_ANALYSIS )
    
    call cloud_analysis_aux( i_spdcx, nb_SPDCx, i_CLOUD_ANALYSIS )

    call cloud_analysis_aux( i_prprx, nb_PRPRx, i_CLOUD_ANALYSIS )

    call cloud_analysis_aux( i_prplx, nb_PRPLx, i_CLOUD_ANALYSIS )

  END SUBROUTINE cloud_analysis
  
  !!!----------------------------------------------------
  subroutine contact_force_distribution_aux( id_inter, nb_contactor, &
       icdan, incdan, cfdRmag, cfdRn, Rmean, Rmax, Rnmean, Rnmax, Rnmin )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_contactor
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: incdan
    real( kind = 8 )    :: Rmean
    real( kind = 8 )    :: Rmax
    real( kind = 8 )    :: Rnmean
    real( kind = 8 )    :: Rnmax
    real( kind = 8 )    :: Rnmin

    real( kind = 8 )   , dimension(:) :: cfdRmag
    real( kind = 8 )   , dimension(:) :: cfdRn
    
    ! Local variables
    integer :: icdtac, nb_adj, iadj, status
    real(kind=8) :: rls
    real(kind=8) :: rlt
    real(kind=8) :: rln
    real(kind=8) :: Rmag


    
    do icdtac = 1, nb_contactor
       nb_adj = get_verlet_adjsz( id_inter, icdtac )

       do iadj = 1, nb_adj
          call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln, rls )
          Rmag = SQRT( rln*rln + rlt*rlt + rls*rls )

          if ( Rmag > 1.D-16 ) then
             icdan = icdan + 1
             cfdRmag( icdan ) = Rmag
             Rmean = Rmean + Rmag
             Rmax  = MAX( Rmax, Rmag ) 
          end if

          if ( ABS( rln ) > 1.D-16 ) then
             incdan = incdan + 1
             cfdRn( incdan ) = rln
             Rnmean = Rnmean + rln
             Rnmax  = MAX( Rnmax, rln ) 
             Rnmin  = MIN( Rnmin, rln ) 
          end if

       end do

    end do

  end subroutine contact_force_distribution_aux

  !!!----------------------------------------------------
  SUBROUTINE contact_force_distribution

    IMPLICIT NONE

    INTEGER      :: icdtac,nb_adj,iadj,incdan
    INTEGER      :: ik,icdan,alpha,nfich,nb_act
    REAL(kind=8) :: rln,rlt,rls
    REAL(kind=8) :: Rmag,Rmean,Rmax,Rnmean,Rnmax,Rnmin

    integer :: status, i, id_inter, verlet_size
    integer, dimension(6) :: inters_id

    REAL(kind=8),DIMENSION(:),ALLOCATABLE :: cfdRmag,cfdRn
    
   
    Rmean  = 0.D0
    Rnmean = 0.D0
    Rmax   =-1.0D+24
    Rnmax  =-1.0D+24
    Rnmin  = 1.0D+24
    icdan  = 0
    incdan = 0

    IF(nb_CDANx ==0) RETURN

    nfich = PostCommand(i_CONTACT_FORCE_DISTRIBUTION)%io_unit(1) 

    cfd_unit = cfd_unit + 1

    IF( cfd_unit < 10000000)THEN
       WRITE(cfd_name1(36:42),'(I7.7)') cfd_unit
    ELSE
       CALL LOGMES(' @ The number of files is exeeded')
       CALL LOGMES(' @ The contact cannot be executed anymore')
       RETURN
    END IF

    OPEN(unit=nfich,file=TRIM(location(cfd_name1)),status= 'REPLACE')

    ALLOCATE(cfdRmag(nb_CDANx))

    
    ALLOCATE(cfdRn(nb_CDANx))

    inters_id = (/ i_spspx, i_spplx, i_spcdx, i_spdcx, &
                   i_prprx, i_prplx                    /)


    do i = 1, size(inters_id)
      id_inter = inters_id(i)

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln, rls )
          Rmag = SQRT( rln*rln + rlt*rlt + rls*rls )

          if ( Rmag > 1.D-16 ) then
             icdan = icdan + 1
             cfdRmag( icdan ) = Rmag
             Rmean = Rmean + Rmag
             Rmax  = MAX( Rmax, Rmag ) 
          end if

          if ( ABS( rln ) > 1.D-16 ) then
             incdan = incdan + 1
             cfdRn( incdan ) = rln
             Rnmean = Rnmean + rln
             Rnmax  = MAX( Rnmax, rln ) 
             Rnmin  = MIN( Rnmin, rln ) 
          end if

        end do
      end do
    end do

    nb_act = icdan
    IF ( nb_act /= 0 ) Rmean = Rmean/REAL(nb_act,8)
    IF ( Rmax == 0.D0) Rmax = 1.D0
    
    Fnumber = 0
   
    DO icdan=1,nb_act
       Rmag = cfdRmag(icdan)
       ik = INT(Fsect*Rmag/Rmax) + 1
       ik = MIN(Fsect,ik)
       Fnumber(ik) = Fnumber(ik) + 1
    END DO
   
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

    DO ik = 1,Fsect
       WRITE(nfich,'(1X,ES14.7,1X,I6,1X,ES14.7,1X,I6)') &
            (Rmax/Rmean)*(ik-0.5)/REAL(Fsect,8),Fnumber(ik), &
            (Rnmax/Rnmean)*(ik-0.5)/REAL(Fsect,8)-Rnmin/Rnmean,FNnumber(ik)
    END DO

    CLOSE(nfich)

    DEALLOCATE(cfdRmag)
    DEALLOCATE(cfdRn)    

    
  END SUBROUTINE contact_force_distribution
  
  !!!----------------------------------------------------
  subroutine display_tensors_aux_compute_Fik_coorik_nik( id_inter, ik, H, Fik, coorik, nik )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: ik
    real( kind = 8 )    :: H
    real( kind = 8 ), dimension( 3 ) :: Fik
    real( kind = 8 ), dimension( 3 ) :: coorik

    ! Local variables
    integer      :: icdtac, iadj, status
    real(kind=8) :: Rnik
    real(kind=8) :: Rtik
    real(kind=8) :: Rsik
    real(kind=8), dimension(3) :: tik, nik, sik

    call this2verlet( id_inter, ik, icdtac, iadj )
    call get_verlet_rloc( id_inter, icdtac, iadj, status, Rtik, Rnik, Rsik )
    call get_verlet_local_frame( id_inter, icdtac, iadj, coorik, tik, nik, sik )

    Fik = ( Rnik*nik + Rtik*tik + Rsik*sik )

  end subroutine display_tensors_aux_compute_Fik_coorik_nik

  !!!----------------------------------------------------
  subroutine display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    implicit none

    real( kind = 8 ), dimension( 3 )    :: Fik
    real( kind = 8 ), dimension( 3 )    :: Lik
    real( kind = 8 ), dimension( 3 )    :: nik
    real( kind = 8 ), dimension( 3, 3 ) :: SIGMA0 
    real( kind = 8 ), dimension( 3, 3 ) :: TEXT0

    SIGMA0( 1, 1 : 3 ) = Fik( 1 ) * Lik( 1 : 3 ) + SIGMA0( 1, 1 : 3 )
    SIGMA0( 2, 1 : 3 ) = Fik( 2 ) * Lik( 1 : 3 ) + SIGMA0( 2, 1 : 3 )
    SIGMA0( 3, 1 : 3 ) = Fik( 3 ) * Lik( 1 : 3 ) + SIGMA0( 3, 1 : 3 )

    TEXT0( 1, 1 : 3 )  = nik( 1 ) * nik( 1 : 3 ) + TEXT0( 1, 1 : 3 )
    TEXT0( 2, 1 : 3 )  = nik( 2 ) * nik( 1 : 3 ) + TEXT0( 2, 1 : 3 )
    TEXT0( 3, 1 : 3 )  = nik( 3 ) * nik( 1 : 3 ) + TEXT0( 3, 1 : 3 )

  end subroutine display_tensors_aux_compute_sigma0_text0

  !!!----------------------------------------------------
  SUBROUTINE display_tensors
    
    IMPLICIT NONE

    INTEGER                   :: ik,icdtac,iantac,iadj
    REAL(kind=8)              :: Rnik,Rtik,Rsik
    REAL(kind=8),DIMENSION(3) :: sik,tik,nik,Fik,Lik,coorik
    REAL(kind=8),DIMENSION(3) :: coorcd,cooran
    integer(kind=4)           :: statusik
    INTEGER                   :: nfich

    REAL(kind=8),DIMENSION(3,3) :: TEXT0,SIGMA0 
  

    SIGMA0 = 0.D0
    TEXT0  = 0.D0

    DO ik=1,nb_SPSPx

       call display_tensors_aux_compute_Fik_coorik_nik( i_spspx, ik, H, Fik, coorik, nik )

       CALL SPSPx2SPHER(ik,icdtac,iantac)

       coorcd = get_coor(spher2bdyty(1,icdtac),spher2bdyty(2,icdtac))
       cooran = get_coor(spher2bdyty(1,iantac),spher2bdyty(2,iantac))
       
       Lik = coorcd(1:3)-cooran(1:3)

       call display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    ENDDO

    DO ik=1,nb_SPPLx

       call display_tensors_aux_compute_Fik_coorik_nik( i_spplx, ik, H, Fik, coorik, nik )

       CALL SPPLx2SPHER(ik,icdtac)
       coorcd = get_coor(spher2bdyty(1,icdtac),spher2bdyty(2,icdtac))       
       Lik = coorcd(1:3)-coorik(1:3)

       call display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    ENDDO

    DO ik=1,nb_SPCDx

       call display_tensors_aux_compute_Fik_coorik_nik( i_spcdx, ik, H, Fik, coorik, nik )

       CALL SPCDx2SPHER(ik,icdtac)
       coorcd = get_coor(spher2bdyty(1,icdtac),spher2bdyty(2,icdtac))       
       Lik = coorcd(1:3)-coorik(1:3)

       call display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    ENDDO

    DO ik=1,nb_SPDCx

       call display_tensors_aux_compute_Fik_coorik_nik( i_spdcx, ik, H, Fik, coorik, nik )

       CALL SPDCx2SPHER(ik,icdtac)
       coorcd = get_coor(spher2bdyty(1,icdtac),spher2bdyty(2,icdtac))
       Lik = coorcd(1:3)-coorik(1:3)

       call display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    ENDDO

    DO ik=1,nb_PRPRx

       call display_tensors_aux_compute_Fik_coorik_nik( i_prprx, ik, H, Fik, coorik, nik )

       CALL PRPRx2POLYR(ik,icdtac,iantac)
       coorcd = get_coor(polyr2bdyty(1,icdtac),polyr2bdyty(2,icdtac))
       cooran = get_coor(polyr2bdyty(1,iantac),polyr2bdyty(2,iantac))
       Lik = coorcd(1:3)-cooran(1:3)

       call display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    ENDDO

    DO ik=1,nb_PRPLx

       call display_tensors_aux_compute_Fik_coorik_nik( i_prplx, ik, H, Fik, coorik, nik )

       CALL PRPLx2POLYR(ik,icdtac)
       coorcd = get_coor(polyr2bdyty(1,icdtac),polyr2bdyty(2,icdtac))
       Lik = coorcd(1:3)-coorik(1:3)
       
       call display_tensors_aux_compute_sigma0_text0( Fik, Lik, nik, SIGMA0, TEXT0 )

    ENDDO
    
    IF (nb_CDANx.NE.0) TEXT0 = TEXT0/REAL(nb_CDANx,8)
    
    nfich = PostCommand(i_DISPLAY_TENSORS)%io_unit(1)
    WRITE(nfich,'(ES15.8,9(1X,ES14.7))') TPS,SIGMA0(1,:),SIGMA0(2,:),SIGMA0(3,:)

    nfich = PostCommand(i_DISPLAY_TENSORS)%io_unit(2)
    WRITE(nfich,'(ES15.8,9(1X,ES14.7))') TPS,TEXT0(1,:),TEXT0(2,:),TEXT0(3,:)
    
  END SUBROUTINE display_tensors

  !!!----------------------------------------------------
  SUBROUTINE dry_contact_nature
    
    IMPLICIT NONE
    
    INTEGER :: stick,noctc,slide,nfich
    
    IF(nlgs_solver3D)THEN
       CALL get_nlgs3D_contact_status(noctc,stick,slide)
    ENDIF
    
    IF(cpg_solver)THEN
       CALL get_cpg_contact_status(noctc,stick,slide)
    ENDIF

    nfich = PostCommand(i_DRY_CONTACT_NATURE)%io_unit(1)

    WRITE(nfich,'(1X,ES15.8,3(1X,I6))') TPS,noctc,stick,slide
    
  END SUBROUTINE dry_contact_nature

  !!!---------------------------------------------------------------------------
  SUBROUTINE network_evolution
    
    IMPLICIT NONE
    
    INTEGER :: nct,nweak,nstrong
    INTEGER :: nfich
    
    nct = 0; nweak=0; nstrong=0

    IF(nlgs_solver3D)THEN
       CALL get_nlgs3D_network_change(nct,nweak,nstrong)
    ENDIF
    IF(cpg_solver)THEN 
       CALL get_cpg_network_change(nct,nweak,nstrong)
    ENDIF

    nfich = PostCommand(i_NETWORK_EVOLUTION)%io_unit(1)
    WRITE(nfich,'(ES15.8,3(1X,I6))') TPS,nct,nweak,nstrong
    
  END SUBROUTINE network_evolution

  !!!---------------------------------------------------------------------------
  SUBROUTINE average_velocity_evolution

    IMPLICIT NONE

    REAL(kind=8),DIMENSION(6) :: V,Vmean
    INTEGER                   :: ibdyty,nb_IN,nfich
    CHARACTER(len=5)          :: color
    
    nb_IN = 0
    Vmean = 0.D0
    
    DO ibdyty=1,nb_RBDY3
       
!       IF( .NOT. BodyWindow(ibdyty) ) CYCLE

       color  = get_color(ibdyty,1)

       IF ( color .EQ. AVcolor ) THEN
          
          V   = get_V(ibdyty)
       
          Vmean = Vmean + V
          
          nb_IN = nb_IN + 1
       END IF

    END DO

    IF( nb_IN .NE. 0 ) Vmean = Vmean/REAL(nb_IN,8)

    nfich = PostCommand(i_AVERAGE_VELOCITY_EVOLUTION)%io_unit(1)
    WRITE(nfich,'(ES15.8,7(1X,ES14.7))') TPS,Vmean(1),Vmean(2),Vmean(3),Vmean(4),Vmean(5),Vmean(6), &
                                        SQRT(Vmean(1)*Vmean(1)+Vmean(2)*Vmean(2)+Vmean(3)*Vmean(3))

  END SUBROUTINE average_velocity_evolution

  !!!---------------------------------------------------------------------------
  SUBROUTINE triaxial_compacity

    IMPLICIT NONE
    INTEGER                   :: nfich
    REAL(kind=8),DIMENSION(3) :: coor 
    REAL(kind=8)              :: BoxVolume
    REAL(kind=8)              :: Xmin,Xmax,Ymin,Ymax,Zmin,Zmax
    
    coor = get_coor(iXmin,0); Xmin = coor(1)+size_planx(1)
    coor = get_coor(iXmax,0); Xmax = coor(1)-size_planx(2)
    coor = get_coor(iYmin,0); Ymin = coor(2)+size_planx(3)
    coor = get_coor(iYmax,0); Ymax = coor(2)-size_planx(4)
    coor = get_coor(iZmin,0); Zmin = coor(3)+size_planx(5)
    coor = get_coor(iZmax,0); Zmax = coor(3)-size_planx(6)
    
    BoxVolume = (Xmax-Xmin)*(Ymax-Ymin)*(Zmax-Zmin)
    
    nfich = PostCommand(i_TRIAXIAL_COMPACITY)%io_unit(1)    

    WRITE(nfich,'(ES15.8,3(1X,ES14.7))') tps,Volume0/BoxVolume,Volume0,BoxVolume
    
  END SUBROUTINE triaxial_compacity

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

  !!!---------------------------------------------------------------------------
  SUBROUTINE dep_evolution

    IMPLICIT NONE
     INTEGER      :: nfich,i,j,k,nb_nodes
    REAL(kind=8) :: mean_u(3),mean_v(3)
    REAL(kind=8) :: u(3),v(3)

    DO i=1,nb_MECAx_sets
       mean_u = 0.d0
       mean_v = 0.d0

       nb_nodes=0
       
       DO j=1,MECAx_set(i)%nb_MECAx
          
          DO k=1,MECAx_set(i)%data(j)%nb_nodes
             
             CALL get_nodal_displacements_mecaMAILx(MECAx_set(i)%data(j)%iMECAx, &
                  MECAx_set(i)%data(j)%nodes(k),3,u,v)

             mean_u = mean_u + u
             mean_v = mean_v + v

             nb_nodes=nb_nodes+1
             
          END DO
       END DO

       nfich = PostCommand(i_Dep_EVOLUTION)%io_unit(i)
       WRITE(nfich,'(ES15.8,6(1x,ES14.7))') TPS,mean_u(1)/nb_nodes,mean_u(2)/nb_nodes,mean_u(3)/nb_nodes, &
                                            mean_v(1)/nb_nodes,mean_v(2)/nb_nodes,mean_v(3)/nb_nodes

    END DO


  END SUBROUTINE dep_evolution
  
  !!!---------------------------------------------------------------------------
  SUBROUTINE fint_evolution

    IMPLICIT NONE

    INTEGER                   :: nfich,i,j,k

    REAL(kind=8) :: tReac(3),tFint(3),tFinert(3),tFext(3),tRes(3),tmomentum(3)
    
    REAL(kind=8) :: reac(3),fint(3),Finert(3),fext(3),res(3),momentum(3)

    DO i=1,nb_MECAx_sets
       
       tReac   = 0.d0
       tFint   = 0.d0
       tFinert = 0.d0
       tFext = 0.d0
       tRes    = 0.d0
       tmomentum = 0.d0
       
       DO j=1,MECAx_set(i)%nb_MECAx
          
          DO k=1,MECAx_set(i)%data(j)%nb_nodes
             
             CALL get_nodal_Forces_mecaMAILx(MECAx_set(i)%data(j)%iMECAx,&
                                             MECAx_set(i)%data(j)%nodes(k),3,&
                                             reac,fint,finert,fext,res,momentum)

             tReac =tReac + reac
             tFint=tFint +fint
             tFinert=tFinert +finert
             tFext=tFext +fext
             tRes=tRes + res
             tmomentum = tmomentum + momentum 
          END DO
       END DO

       nfich = PostCommand(i_Fint_EVOLUTION)%io_unit(i)
       WRITE(nfich,'(ES15.8,18(1x,ES14.7))') TPS,tReac(1),tReac(2),tReac(3), &
                                             tFint(1),tFint(2),tFint(3), &
                                             tFinert(1),tFinert(2),tFinert(3), &
                                             tFext(1),tFext(2),tFext(3), &
                                             tRes(1),tRes(2),tRes(3), &
                                             tmomentum(1:3)  

    END DO


  END SUBROUTINE fint_evolution

  !!!---------------------------------------------------------------------------
  SUBROUTINE thermal_evolution

    IMPLICIT NONE
    INTEGER      :: nfich,icdbdy,itacty,nb_tacty
    REAL(kind=8) :: GPV,GDPV,GDV2,GQIJ,GA,GAQIJ,TWS,WS

    CALL get_global_3D_thermal_variable(GPV,GDPV,GDV2,GQIJ,GAQIJ,GA)
    nfich = PostCommand(i_THERMAL_EVOLUTION)%io_unit(1)

    TWS = 0.D0

    DO icdbdy=1,nb_RBDY3
       WS = 0.D0
       nb_tacty = get_nb_tacty(icdbdy)
       DO itacty=1,nb_tacty
          WS    = WS    + get_WS(icdbdy,itacty)
       END DO
       WS    = WS/REAL(nb_tacty,8)
       TWS = TWS + WS
    END DO

    IF (nb_RBDY3.NE.0) TWS = TWS/REAL(nb_RBDY3,8)

    WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,GPV,GDPV,GDV2,GQIJ,GA,TWS

  END SUBROUTINE thermal_evolution
  
  !!!---------------------------------------------------------------------------
  SUBROUTINE wet_contact_nature

    IMPLICIT NONE

  END SUBROUTINE wet_contact_nature
  
  !!!---------------------------------------------------------------------------
  SUBROUTINE normal_contact_distribution

    IMPLICIT NONE

  END SUBROUTINE normal_contact_distribution

  !!!---------------------------------------------------------------------------
  !fd cette routine n est pas propre car elle ne gere pas les points de contact multiples
  subroutine doublet_interactions_aux_( id_inter, nb_inter, contact1_bdyty, contact2_bdyty, &
                                        gapTT, rlt, rln, rls, vlt, vln, vls )
    implicit none

    integer, intent(in) :: id_inter, nb_inter
    integer, dimension(:,:), intent(in) :: contact1_bdyty, contact2_bdyty

    real(kind=8), intent(out) :: gapTT
    real(kind=8), intent(out) :: rlt, rln, rls
    real(kind=8), intent(out) :: vlt, vln, vls
    
    ! Local variables
    integer :: icdan
    integer :: icdtac
    integer :: iadj
    integer :: iantac
    integer :: status

    do icdan = 1, nb_inter
       call this2verlet( id_inter, icdan, icdtac, iadj )
       iantac = get_verlet_iantac( id_inter, icdtac, iadj )

       IF ( ( doubletbdyty( 1 ) == contact1_bdyty( 1, icdtac ) ) .AND. &
            ( doubletbdyty( 2 ) == contact2_bdyty( 1, iantac ) ) ) THEN

          gapTT = get_verlet_gapTT( id_inter, icdtac, iadj )

          call get_verlet_rloc( id_inter, icdtac, iadj, status, rlt, rln, rls )
          call get_verlet_vloc( id_inter, icdtac, iadj, vlt, vln, vls )

          exit
       END IF
    END DO    
    
  end subroutine doublet_interactions_aux_
  !
  SUBROUTINE doublet_interactions

    IMPLICIT NONE

    INTEGER          :: icdan,icdtac,iantac,iadj
    REAL(kind=8)     :: gapTT,rln,rlt,rls,vln,vlt,vls
    INTEGER          :: nfich
                             !12345678901234567890123456789012
    character(len=32) :: IAM='postpro_3D::doublet_interactions'
    
    gapTT = 0.D0

    rln = 0.D0
    rlt = 0.D0
    rls = 0.D0
    vln = 0.D0
    vlt = 0.D0
    vls = 0.D0

    SELECT CASE(doublet_type)
    CASE('SPSPx')
       call doublet_interactions_aux_( i_spspx, nb_SPSPx, spher2bdyty, spher2bdyty, &
                                       gapTT, rlt, rln, rls, vlt, vln, vls )
    CASE('SPPLx')
       call doublet_interactions_aux_( i_spplx, nb_SPSPx, spher2bdyty, planx2bdyty, &
                                       gapTT, rlt, rln, rls, vlt, vln, vls )

!fd c'est de la connerie vu qu'on a potentiellement plusieurs points de contact
!   avec des normales differentes 
!!$    CASE('PRPRx')
!!$       DO icdan=1,nb_PRPRx
!!$          CALL this2verlt_PRPRx(icdan,icdtac,iadj)
!!$          CALL get_antac_PRPRx(icdtac,iadj,iantac)
!!$
!!$          IF ( (doubletbdyty(1) == polyr2bdyty(icdtac,1)).AND.(doubletbdyty(2) == polyr2bdyty(iantac,1)) ) THEN
!!$             gapTT = get_gapTT_PRPRx(icdtac,iadj)
!!$             CALL get_local_reac_PRPRx(icdtac,iadj,rls,rlt,rln)
!!$             CALL get_local_vloc_PRPRx(icdtac,iadj,vls,vlt,rln)
!!$             EXIT
!!$          END IF
!!$       END DO
!!$    CASE('PRPLx')
!!$       DO icdan=1,nb_PRPLx
!!$          CALL this2verlt_PRPLx(icdan,icdtac,iadj)
!!$          CALL get_antac_PRPLx(icdtac,iadj,iantac)
!!$
!!$          IF ( (doubletbdyty(1) == polyr2bdyty(icdtac,1)).AND.(doubletbdyty(2) == planx2bdyty(iantac,1)) ) THEN
!!$             gapTT = get_gapTT_PRPLx(icdtac,iadj)
!!$             CALL get_local_reac_PRPLx(icdtac,iadj,rls,rlt,rln)
!!$             CALL get_local_vloc_PRPLx(icdtac,iadj,vls,vlt,rln)
!!$             EXIT
!!$          END IF
!!$       END DO
    CASE DEFAULT
      call logmes(doublet_type)
      call faterr(IAM,'unsupported doublet type')
    END SELECT
    
    nfich = PostCommand(i_DOUBLET_INTERACTIONS)%io_unit(1)
    WRITE(nfich,'(ES15.8,7(1X,ES14.7))') TPS,gapTT,rln,rlt,rls,vln,vlt,vls

  END SUBROUTINE doublet_interactions

  !!!---------------------------------------------------------------------------
  !> computes reac on a body due to contact with an other one (bdyty)
  SUBROUTINE doublets_torque_evolution

    IMPLICIT NONE


    INTEGER          :: nfich
                            !1234567890123456789012345678901234567
    character(len=37):: IAM='postpro_3D::doublets_torque_evolution'
    
    integer :: i,i1,i2
 
    integer, dimension(:,:), pointer :: bdyty2tacty_1, bdyty2tacty_2

    real(kind=8),dimension(6) :: local_reac, raux
    character(len=80) :: cout

    DO i=1,nb_dte

       nfich = get_io_unit()
       OPEN(unit=nfich,file=TRIM(location(PostCommand(i_DOUBLETS_TORQUE_EVOLUTION)%file_name(i))),status='OLD',POSITION='APPEND')

      ! on boucle sur les contacteurs des deux objets
      ! pour chaque couple on teste si un contact existe via verlet
      ! si il existe on calcule la resultante sur 

      bdyty2tacty_1 => get_ptr_bdyty2tacty_rbdy3(dte(1,i))    
      bdyty2tacty_2 => get_ptr_bdyty2tacty_rbdy3(dte(2,i))    

      if (.not. associated(bdyty2tacty_1)) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'dte',i,'body',dte(1,i)
        call logmes(cout)
        call FATERR(IAM,'body without contactors')
      endif

      if (.not. associated(bdyty2tacty_2)) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'dte',i,'body',dte(2,i)
        call logmes(cout)
        call FATERR(IAM,'body without contactors')
      endif

      !print*,'objet ', dte(1,i),size(bdyty2tacty_1,dim=2)
      !print*,bdyty2tacty_1(1,:)
      !print*,bdyty2tacty_1(2,:)

      !print*,'objet ', dte(2,i),size(bdyty2tacty_2,dim=2)
      !print*,bdyty2tacty_2(1,:)
      !print*,bdyty2tacty_2(2,:)
 

     local_reac = 0.d0
      do i1=1,size(bdyty2tacty_1,dim=2)
        do i2=1,size(bdyty2tacty_2,dim=2)
          if (bdyty2tacty_1(1,i1) == i_polyr .or. bdyty2tacty_1(1,i2) == i_polyr) then
            !print*,'on calcule la reaction entre ',bdyty2tacty_1(2,i1),' et ',bdyty2tacty_2(2,i2)
            call pair_reaction_PRPRx(dte(1,i),bdyty2tacty_1(2,i1),i1,dte(2,i),bdyty2tacty_2(2,i2),i2,raux)
            !print*,raux
            local_reac = local_reac + raux
          endif
        enddo
      enddo    

      WRITE(nfich,'(ES15.8,6(1X,ES14.7))') TPS,local_reac

      close(nfich)

    enddo

  END SUBROUTINE doublets_torque_evolution
  
  !!!---------------------------------------------------------------------------
  SUBROUTINE mailx_dist

    IMPLICIT NONE
    
    INTEGER      :: nfich,i,ibdya,ibdyb,inoda,inodb
    REAL(kind=8),DIMENSION(nbDIME) :: coora,coorb
    REAL(kind=8) :: dist
    
    nfich = PostCommand(i_MAILx_DIST)%io_unit(1)
    WRITE(nfich,'(1(1x,E14.7))') TPS
    
    DO i=1,nb_dist_sets
       
       ibdya = dist_set(i)%data(1)%iMECAx
       inoda = dist_set(i)%data(1)%nodes(1)
       coora = get_coor_nodty_MAILx(ibdya,inoda)
       
       ibdyb = dist_set(i)%data(2)%iMECAx
       inodb = dist_set(i)%data(2)%nodes(1)
       coorb = get_coor_nodty_MAILx(ibdyb,inodb) 
       
       dist=SQRT(DOT_PRODUCT(coora-coorb,coora-coorb))
       
       WRITE(nfich,'(I7,1x,I7,1x,E14.7)') ibdya,ibdyb,dist
       
    END DO

  END SUBROUTINE mailx_dist
  
  !!!---------------------------------------------------------------------------
  SUBROUTINE mp_value_tracking

    IMPLICIT NONE
    INTEGER      :: i,ibdyty,itacty
    INTEGER      :: nfich
    REAL(kind=8) :: TC,TT,WS,EP,EC

    DO i =1,nb_mp_tracking

       ibdyty = bMPTrackingID(i)
       itacty = tMPTrackingID(i)

       TT = get_thermal_value(ibdyty,itacty)
       TC = get_therm_cond(ibdyty,itacty)
       WS = get_WS(ibdyty,itacty)
       EP = get_electric_potentiel(ibdyty,itacty)
       EC = get_elec_cond(ibdyty,itacty)

       nfich = PostCommand(i_MP_VALUE_TRACKING)%io_unit(i)
       WRITE(nfich,'(ES15.8,5(1X,ES14.7))') TPS,TC,TT,WS,EP,EC

    END DO

  END SUBROUTINE mp_value_tracking
  
  !!!---------------------------------------------------------------------------
  SUBROUTINE heat_bound_profile
  
    IMPLICIT NONE
    
    INTEGER :: iy,ibound
    INTEGER :: nfich
    
    hb_unit = hb_unit + 1
    
    nfich = PostCommand(i_HEAT_BOUND_PROFILE)%io_unit(1)

    WRITE(heat_bound_name(28:34),'(I7.7)') hb_unit
    OPEN(unit=nfich,file=TRIM(location(heat_bound_name)),status='REPLACE')

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
  SUBROUTINE visibility_state

    IMPLICIT NONE
    
    !VARIABLES LOCALES  
    INTEGER  :: nfich                ! numero du fichier
    INTEGER  :: nb_RBDY3_visible     ! nombre de rigides visibles
    INTEGER  :: nb_mecaMAILx_visible ! nombre de mailles meca visibles
     
    INTEGER  :: ibdyty                ! indice de boucle sur les corps

    nb_RBDY3_visible=0
    DO ibdyty=1,nb_RBDY3
       IF (get_visible(ibdyty)) nb_RBDY3_visible = nb_RBDY3_visible + 1
    END DO

    nb_mecaMAILx_visible=0
    DO ibdyty=1,nb_mecaMAILx
       IF (get_visible_mecaMAILx(ibdyty)) nb_mecaMAILx_visible = nb_mecaMAILx_visible + 1
    END DO

    nfich = PostCommand(i_VISIBILITY_STATE)%io_unit(1)
    WRITE(nfich,'(ES15.8,4(1X,ES14.7))') TPS,real(nb_RBDY3_visible,8),real(nb_RBDY3,8), &
                                         real(nb_mecaMAILx_visible,8),real(nb_mecaMAILx,8)

  END SUBROUTINE visibility_state

  !!!------------------------------------------------------
  SUBROUTINE prxxx_detection

    IMPLICIT NONE
    INTEGER  :: nfich                ! numero du fichier
    INTEGER  :: nb_rough_PRPRx,nb_rough_CSPRx
    INTEGER  :: nb_verlet_PRPRx,nb_verlet_CSPRx
    INTEGER  :: nb_PRPRx,nb_CSPRx
    INTEGER  :: nb_recup_PRPRx,nb_recup_CSPRx

    nb_rough_PRPRx  = get_nb_PRPRx( i_rough_tactor )
    nb_verlet_PRPRx = get_nb_PRPRx( i_verlet_tactor )
    nb_PRPRx        = get_nb_PRPRx( i_real_tactor )
    nb_recup_PRPRx  = get_nb_PRPRx( i_recup_tactor )

    nb_rough_CSPRx  = get_nb_CSPRx( i_rough_tactor )
    nb_verlet_CSPRx = get_nb_CSPRx( i_verlet_tactor )
    nb_CSPRx        = get_nb_CSPRx( i_real_tactor )
    nb_recup_CSPRx  = get_nb_CSPRx( i_recup_tactor )

    nfich = PostCommand(i_PRxxx_DETECTION)%io_unit(1)
    WRITE(nfich,'(ES15.8,8(1X,ES14.7))') TPS, &
         real(nb_rough_PRPRx,8), real(nb_verlet_PRPRx,8), real(nb_PRPRx,8), real(nb_recup_PRPRx,8), &
         real(nb_rough_CSPRx,8), real(nb_verlet_CSPRx,8), real(nb_CSPRx,8), real(nb_recup_CSPRx,8)

  END SUBROUTINE prxxx_detection

  !!!------------------------------------------------------
  subroutine clean_memory_postpro_3D()
    implicit none
    integer(kind=4) :: i, j

    cfd_unit = 0
    dissipated_energy = 0.D0
    KE = 0.D0; DE = 0.D0; PE = 0.D0

    snap_unit = 0
    snap_inter_unit=0

    run=0

    hb_unit = 0


    do i = 1, nb_commands
      if( associated(postCommand(i)%io_unit) ) then
        deallocate(postCommand(i)%io_unit)
        nullify(postCommand(i)%io_unit)
      end if
      if( associated(postCommand(i)%file_name) ) then
        deallocate(postCommand(i)%file_name)
        nullify(postCommand(i)%file_name)
      end if
    end do
  
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

    if( allocated(TorqueID) )  deallocate(TorqueID)
    nb_torque = 0
  
    if( allocated(RBDY3_set) ) then
      do i = 1, size(RBDY3_set)
        if( associated(RBDY3_set(i)%list) ) then
          deallocate(RBDY3_set(i)%list)
          nullify(RBDY3_set(i)%list)
        end if
      end do
      deallocate(RBDY3_set)
      nb_RBDY3_sets = 0
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
      deallocate(MECAx_set)
      nb_mecax_sets = 0
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
      deallocate(DIST_set)
      nb_dist_sets = 0
    end if
  
    if( allocated(dte) ) deallocate(dte)
    nb_dte = 0

  end subroutine

  !!!------------------------------------------------------------------------ 
  function get_rbdy3_princ_stress()
    use algebra, only : diagonalise33
    implicit none
    real(kind=8)   ,dimension(:,:),pointer     :: get_rbdy3_princ_stress
    !
    integer        ,dimension(4)               :: inters_id
    integer(kind=4)                            :: i,id_inter,verlet_size
    integer                                    :: icdtac,nb_adj,iadj,status
    integer                                    :: icdbdy,ianbdy
    real(kind=8)                               :: Rtik,Rnik,Rsik
    real(kind=8)   ,dimension(3)               :: tik, nik, sik
    real(kind=8)   ,dimension(3)               :: Fik,Lik
    real(kind=8)   ,dimension(3)               :: coorik
    REAL(kind=8)   ,dimension(3)               :: coorcd,cooran
    real(kind=8)   ,dimension(3)               :: Ip
    real(kind=8)   ,dimension(3,3)             :: Fp     
    real(kind=8)   ,dimension(:,:,:),allocatable :: stress
    
    get_rbdy3_princ_stress => null()

    if( nb_rbdy3 <= 0 ) return

    allocate( get_rbdy3_princ_stress(12,nb_rbdy3) )

    get_rbdy3_princ_stress = 0.d0

    allocate( stress(3,3,nb_rbdy3) )

    stress = 0.d0
    
    inters_id = (/ i_spspx, i_spplx, i_prprx, i_prplx /)

    ! calcul de la contribution des forces
    
    do i = 1, size(inters_id)

      id_inter = inters_id(i)

      verlet_size = get_verlet_size(id_inter)
      if( verlet_size == 0 ) cycle

      do icdtac = 1, verlet_size

        if (i == 1 .or. i == 2) then   
          icdbdy = spher2bdyty(1,icdtac)
        else if (i == 3 .or. i==4) then
          icdbdy = polyr2bdyty(1,icdtac)
        endif
       
        coorcd = get_coor(icdbdy,0) 

        nb_adj = get_verlet_adjsz(id_inter, icdtac)

        do iadj = 1, nb_adj

          call get_verlet_rloc( id_inter, icdtac, iadj, status, Rtik, Rnik, Rsik )
          
          call get_verlet_local_frame( id_inter, icdtac, iadj, coorik, tik, nik, sik )

          Fik = ( Rnik*nik + Rtik*tik + Rsik*sik )

          Lik = coorik(1:3)-coorcd(1:3)

          stress(1,1:3,icdbdy) = stress(1,1:3,icdbdy) + (Fik(1)*Lik(1:3)) 
          stress(2,1:3,icdbdy) = stress(2,1:3,icdbdy) + (Fik(2)*Lik(1:3)) 
          stress(3,1:3,icdbdy) = stress(3,1:3,icdbdy) + (Fik(3)*Lik(1:3)) 

          if (i==1 .or. i==3) then          
            ianbdy = get_verlet_ianbdy( id_inter, icdtac, iadj )
            cooran = get_coor(ianbdy,0)
          
            Lik = coorik(1:3)-cooran(1:3)
            
            stress(1,1:3,ianbdy) = stress(1,1:3,ianbdy) - (Fik(1)*Lik(1:3)) 
            stress(2,1:3,ianbdy) = stress(2,1:3,ianbdy) - (Fik(2)*Lik(1:3)) 
            stress(3,1:3,ianbdy) = stress(3,1:3,ianbdy) - (Fik(3)*Lik(1:3)) 
          endif

        enddo 
      enddo 
    enddo

    ! on divise par le volume de la particule    
    do i=1,nb_rbdy3
      stress(:,:,i) = stress(:,:,i) /get_volume(i)
    enddo

    ! on diagonalise et on stocke
    do i=1,nb_rbdy3
      call diagonalise33(stress(:,:,i),Ip,Fp)
      get_rbdy3_princ_stress(1:3,i)=Ip
      get_rbdy3_princ_stress(4:6,i)=Fp(:,1)
      get_rbdy3_princ_stress(7:9,i)=Fp(:,2)
      get_rbdy3_princ_stress(10:12,i)=Fp(:,3)
    enddo        
    
    deallocate(stress)
   
  end function get_rbdy3_princ_stress


  
END MODULE postpro_3D
