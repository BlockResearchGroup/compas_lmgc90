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
module overall

  !!****h* LMGC90.CORE/OVERALL
  !! NAME
  !!  module OVERALL
  !! PURPOSE
  !!  This modulus contains names of data (DAT) and results (OUT) standard files
  !!  and commands used thoroughly.
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****
  
  use utilities

  use parameters

!!!----------------------------------------------
!!!
!!! Defining some version data
!!!
!!! character(len=9) , public, parameter :: release
!!! character(len=40), public, parameter :: git_branch
!!! character(len=40), public, parameter :: git_revision
!!!
!!!----------------------------------------------
include "version.f90"

!!!----------------------------------------------
!!!
!!! Defining data and results standard file names
!!!
!!!----------------------------------------------

  character(len=200) :: WORKING_DIRECTORY='./'

  !                                                  !123456789012345678
  !  CHARACTER(len=18) ::  in_chic_command         = 'DATBOX/COMMAND.DAT'        ! command file;
  !  CHARACTER(len=18) :: out_chic_command         = 'OUTBOX/COMMAND.OUT'        ! rewritten displayed file;
  
  !                                         12345678901234567                           
  character(len=17) ::  in_bodies        = 'DATBOX/BODIES.DAT'         ! defining bodies file;
  character(len=17) :: out_bodies        = 'OUTBOX/BODIES.OUT'         ! rewritten file;

  !                                         123456789012345678901
  character(len=21) ::  in_bulk_behav    = 'DATBOX/BULK_BEHAV.DAT'     ! bulk behaviour and contact laws file;
  character(len=21) :: out_bulk_behav    = 'OUTBOX/BULK_BEHAV.OUT'     ! rewritten file;
  character(len=21) ::  in_tact_behav    = 'DATBOX/TACT_BEHAV.DAT'     ! bulk behaviour and contact laws file;
  character(len=21) :: out_tact_behav    = 'OUTBOX/TACT_BEHAV.OUT'     ! rewritten file;
  
  !                                         12345678901234567
  character(len=17) ::  in_models        = 'DATBOX/MODELS.DAT'         ! bulk FE model file;
  character(len=17) :: out_models        = 'OUTBOX/MODELS.OUT'         ! rewritten file;

  !                                         123456789012345678  
  character(len=18) ::  in_driven_dof    = 'DATBOX/DRV_DOF.DAT'        ! driven degrees of freedom file; 
  character(len=18) :: out_driven_dof    = 'OUTBOX/DRV_DOF.OUT'        ! rewritten file;
                           
  !                                         1234567890123456789012345  ! keep long for post    
  character(len=25) :: in_dof            = 'DATBOX/DOF.INI           ' ! initial degrees of freedom file;
  character(len=25) :: in_Vloc_Rloc      = 'DATBOX/Vloc_Rloc.INI     ' ! initial local variables file;


  !                                         1234567890123456789012345                            
  character(len=25) :: last_dof          = 'OUTBOX/DOF.LAST'           ! last degrees of freedom file;
  character(len=26) :: last_Rnod         = 'OUTBOX/Rnod.LAST'          ! last generalized reaction forces file;
  character(len=31) :: last_Vloc_Rloc    = 'OUTBOX/Vloc_Rloc.LAST'     ! last reaction local variables file;

                                                  !1234567890123456789012345                            
  character(len=15) :: last_gpv          = 'OUTBOX/GPV.LAST'           ! last gauss points values file;
  character(len=24) :: out_gpv           = 'OUTBOX/GPV.OUT'            ! out gauss points values file;
  character(len=14) :: in_gpv            = 'DATBOX/GPV.INI'            ! last gauss points values file;

!!! here the over length is aimed to add the number of the file if one use the STEP keyword 

                                                  !1234567890123456789012345                            
  character(len=24) :: out_dof           = 'OUTBOX/DOF.OUT'            ! current degrees of freedom file;
  character(len=25) :: out_Rnod          = 'OUTBOX/Rnod.OUT'           ! current generalized reaction forces file;
  character(len=30) :: out_Vloc_Rloc     = 'OUTBOX/Vloc_Rloc.OUT'      ! current reaction local variables file;

! 
                                                  !1234567890123456              
  character(len=16) :: out_timer         = 'OUTBOX/TIMER.OUT'          ! time by subroutine data file;

!                                          !12345678901234567890123456781                                                        
  character(len=18) :: in_post           = 'DATBOX/POSTPRO.DAT'        ! contain post-treatment command.             
  character(len=17) :: in_mpdem          = 'DATBOX/MP_DEM.DAT'         ! contains information for MPDEM problem
  character(len=17) :: out_mpdem         = 'OUTBOX/MP_DEM.OUT'         ! contains information for MPDEM problem
  character(len=20) :: in_mpv            = 'DATBOX/MP_VALUES.INI'
  character(len=26) :: out_mpv           = 'OUTBOX/MP_VALUES.OUT.     '
  character(len=21) :: last_mpv          = 'OUTBOX/MP_VALUES.LAST'

  !                                            1234567890123456789012345678901234                                                      
  character(len=31) :: post_body_sample     = 'POSTPRO/BODY_SNAPSHOT000000.DAT'
  character(len=34) :: post_contact_sample  = 'POSTPRO/CONTACT_SNAPSHOT000000.DAT'

  character(len=17) :: sample_bodies     = 'PREBOX/BODIES.DAT'
  character(len=18) :: sample_driven_dof = 'PREBOX/DRV_DOF.DAT'
  character(len=14) :: sample_dof        = 'PREBOX/DOF.INI'

  character(len=17) :: in_sample         = 'DATBOX/SAMPLE.DAT'         ! generate a simulation sample.

!!!mr: experimental
  character(len=23) :: in_frictionmap    = 'DATBOX/FRICTION_MAP.DAT'   ! 
  character(len=23) :: in_frictionevo    = 'DATBOX/Mu_EVOLUTION.DAT'   ! 

!  CHARACTER(len=7 ) :: post_body_sample  = 'ZOB.ZOB'   ! 
!  CHARACTER(len=7 ) :: post_contact_sample  = 'BOZ.BOZ'   ! 




!!! internal values for file access

  integer, save :: io_in_dof, io_out_dof, io_last_dof
  integer, save :: io_in_Vloc_Rloc ! am & pta
  integer, save :: io_last_Vloc_Rloc ! am & pta
  integer, save :: io_out_Vloc_Rloc ! am & pta
  integer       :: record_size_Vloc_Rloc ! am & pta
  integer       :: rec_number ! am & pta
  
  integer           :: Nstep=0                                                ! step counter;
  real(kind=8)      :: TPS=0.D0                                               ! current time;
  real(kind=8)      :: TPSbegin=0.D0                                          ! time at the beginning of a time step;
  real(kind=8)      :: TPSendall=0.D0                                         ! final time
  real(kind=8)      :: H=1.D0                                                 ! time step;
  logical           :: is_H_set=.FALSE.

  !> DOF.OUT file number
  integer(kind=4)  :: current_fic_write_DOF       = 0
  !> VlocRloc.OUT file number
  integer(kind=4)  :: current_fic_write_Vloc_Rloc = 0
  !> GPV.OUT file number
  integer(kind=4)  :: current_fic_write_gpv       = 0
  !> MP_VALUES.OUT file number
  integer(kind=4)  :: current_fic_write_mpv       = 0

!!!
!!! gestion du pas automatique
!!!
  real(kind=8)      :: oldH=0.d0                                              ! time step at the previous step;
  real(kind=8)      :: newH=0.d0                                              ! time step;
  real(kind=8)      :: H_max=1.D-24                                           ! fake largest time step;
  real(kind=8)      :: H_min=1.D+24                                           ! fake smallest time step;

  !----------------------------------------------------------------------
  ! bulk_itergood : threshold to assume good convergence
  ! bulk_iterbad  : threshold to assume bad convergence
  ! numincconv    : number of increments where we study if there is a bad 
  !
  !fd faudrait mettre du bulk partout ...
  
  integer           :: bulk_itermax=50,bulk_iter
  real(kind=8)      :: critconv
  integer           :: bulk_itergood=10
  integer           :: bulk_iterbad=30
  integer           :: numincconv=3

  ! flag showing if the local solver converge ----------------------------
  logical           :: conv_contact=.true.


  ! Dimension of the problem: 1D, 2D or 3D -------------------------------
  ! Default value is 2D
                                 !1234567890
  character(len=10), private :: DIME ='2D        '
  integer           :: nbDIME   = 2
  integer           :: dime_mod = i_2D_strain

  ! Chat flag ------------------------------------------------------------
  logical           :: itchache=.false.

  !-----------------------------------------------------------------------
  integer,private   :: ii,l_ii,iv
  character(len=20) :: zeros='00000000000000000000'
  character(len=9)  :: numfic


  ! flags associated to written files associated to the keyword STEP ------

  logical           :: write_Rnod = .false., write_DOF =.false., write_Vloc_Rloc = .false.,&
                       write_gpv_actif = .false. , write_mpv =.false.

  real(kind=8),private :: DTPS_write_Rnod, DTPS_write_DOF, DTPS_write_Vloc_Rloc, DTPS_write_gpv

!!! Variables pour calculer le temps passe dans un increment

  real(kind=8),private :: time_ini=0.d0,time_end=0.d0

  ! internal values for data reading -----------------------------------

  integer,parameter :: isskip=0,ifound=1,inomor=99 

  ! internal values for inter selection -------------------------------

  integer,parameter :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , &
                       i_real_tactor  = 4

! internal values for database access ----------------------------------

  integer,parameter :: iV____e_invM_t_Ireac = 1, &     ! V____=m-1Ireac
                       iVaux_e_invM_t_Ireac = 2, &     ! Vaux_=m-1Ireac
                       iVaux_e_invM_t_Iaux_ = 3, &     ! Vaux_=m-1Iaux_
                       iVaux_e_invM_t_Iaux_p_Vfree = 4 ! Vaux_=m-1Iaux_ + raz ddl bloques


  integer,parameter :: iTaux_=1


  ! map between contact type and integer identifier
  integer(kind=4), parameter :: NOINT = 0
  integer(kind=4), parameter :: INTRF = 1


  ! Flag for the solver used for the contact problem resolution.

  logical :: nlgs_solver2D   =.false., cpg_solver=.false., new_int_nlgs_solver=.false.
  logical :: nlgs_solver3D =.false.  
!!!

  logical :: smooth_method = .false.  ! flag for smooth method

  ! Parameter for the time integrator used for the mechanical resolution 
  ! INTEGRATOR_MOREAU | integrator_gear | integrator_verlet | integrator_newmark | INTEGRATOR_BETA2 | INTEGRATOR_QS 
  
  integer           :: M_INTEGRATOR_ID = 0

  ! THETA METHOD parameters 
  ! THETA   : for the mechanical part

  real(kind=8)      :: THETA                                            
 
  ! explicit beta2 ; for beta diffusive scheme

  real(kind=8)      :: beta2                 
  real(kind=8)      :: c1_beta2, c2_beta2, c3_beta2, c4_beta2


  ! THETA_t : for the thermal part
  
  integer           :: T_INTEGRATOR_ID = 0
  real(kind=8)      :: THETA_t
  
  ! MD parameters -------------------------------------------------

  real(kind=8),parameter :: md_gear0 = 0.211111111111 ! md_gear0 = 19.d0/90.d0
  real(kind=8),parameter :: md_gear1 = 0.75           ! md_gear1 = 0.75d0
  real(kind=8),parameter :: md_gear3 = 0.5            ! md_gear3 = 0.5d0
  real(kind=8),parameter :: md_gear4 = 0.083333333333 ! md_gear4 = 1.d0/12.d0
  
  real(kind=8) :: c1, c2, c3, c4
  real(kind=8) :: cr,cv,cb,cc
  
  ! gestion des variables pour le contact

  ! velocity weights 
  logical           :: is_contactdetectionconfiguration_defined=.false.
  real(kind=8)      :: vw_b,vw_e                                            

  integer, parameter :: max_internal_tact=19

  integer, parameter :: internal_tact_comment_length=max_internal_tact*15

  integer, private   :: need_internal_tact = 0

  
  !fd rendre ce paramettre dynamique ? 
  ! lois czm: beta, ep (loi spring) ,pext, b,bp,g1,g2
  ! tosi czm : 4: t_t, 5: tri_s, 6: T
  integer, parameter :: max_taz=7
  
  logical,public :: xxl_check=.false.     ! To deal with large number of particles 
                                          ! it replaces the I5 format by an I10 format !
  ! variables for postprocessing---------------------------------------------

  integer :: i_post_increment
  integer :: i_post_first , i_post_last
  logical :: post_processing = .false. 
  !
  logical :: FIRST_RUN=.true.,RUN_TACTOR=.false.
  !


  !</ fd managing entity
  
  integer,private   :: nb_ENTITY = 0

  logical,private :: is_entity_initialized = .false.

  ! list of all mechanical models involved in the simulation 
  type T_ENTITY
     ! number of adjacent entities to the current entity computed during contact detection
     integer                      :: nb
     ! number of adjacent entities to the current entity computed by the contact solver 
     integer                      :: ik
     ! list of the entities
     integer,dimension(:),pointer :: list
     ! a flag to say if it is clamped (0==no)
     integer                      :: is_clamped
  end type T_ENTITY

  type(T_ENTITY),dimension(:),allocatable :: entity

  !fd />

  !fd 
  logical :: with_experimental_dev =.false. 

  !fd new 16/06/09 
  logical :: is_externalFEM=.false.

                                                                                    
  !vv parametre version enrichie PA
  real(kind=8) :: d1_eta = 0.d0

  logical :: BetaiComputation_flag = .false.
  logical :: MpComputation_flag = .false.

  ! Constant parameters values
  real(kind=8), parameter :: PI_g = 3.14159265358979323846

!!! wrap API

  public SetWorkingDirectory

  public Write_out_bodies_Ol, &
       Clean_out_bodies, &
       Clean_in_bodies, &
       Write_out_driven_dof_Ol, &
       Read_in_dof_Ol, &
       Read_snapshot_sample, &
       Write_xxx_dof_Ol, &
       Read_in_Vloc_Rloc_Ol, &
       Write_xxx_Vloc_Rloc_Ol,&
       Read_in_gpv_Ol, &
       Write_xxx_gpv_Ol,&
       Display_prox_tactors_Ol,&
       Write_xxx_Rnod_Ol,&
       time_increment, &
       Updt_time_begin, & 
       Set_newton_tolerance, &
       Set_newton_maxloop, &
       Set_newton_badloop, &
       Set_newton_goodloop, &
       Set_newton_rate_step, & 
       Set_newton_loop, &
       Incre_newton_loop, &
       Check_Newton_Convergence, &
       Compute_newton_time_step, &
       DISPLAY_TIME, &
       Clean_writing_flags, &
       Get_NSTEP, &
       Init_dimension,&
       Set_time_step, &
       Init_gear_integrator, &
       Init_theta_integrator, &
       init_verlet_integrator, &
       Init_CN_integrator, &
       Init_beta2_integrator, &
       !get_integrator_id, &
       get_explicit_beta, &
       Set_Contact_Detection_Configuration, &
       Set_initial_step, &
       Set_initial_time, &
       Set_final_time, &
       Set_min_time_step, &
       Set_max_time_step, &
       Acti_large_computation, &
       set_run_contactor, &
       TACTOR_RUNNING, &
       update_post_data_ol, &
       init_post_data_ol, &
       Read_in_mp_values_Ol, &
       Write_xxx_mp_values_Ol, &
       add_nb_ENTITY, &
       Init_EntityList, &
       Create_EntityList, &
       Free_EntityList, &
       Clean_EntityList, &
       is_EntityList_Initialized, &
       set_clamped_status_ENTITY, &
       get_status_ENTITY, &
       get_time , &
       get_time_step , &
       set_with_experimental_dev, get_with_experimental_dev, &
       set_is_externalFEM, get_is_externalFEM,&
       get_theta, &
       set_need_internal_tact, &
       get_need_internal_tact, &
       delete_part_of_file, &
       overall_clean_memory

!!! internal API
  
  public  location
 
contains

!!!------------------------------------------------------------------------
  subroutine Write_out_bodies_Ol(skip_header)

    implicit none  
    integer :: nfich  

    ! this logical allows to avoid header writing
    logical, intent(in), optional :: skip_header

    logical :: write_header

    write_header = .true.

    ! header writin is avoid iff skip_header is present and skip_header is "true"
    if (present(skip_header)) then
       write_header = .not. skip_header
    end if

    nfich = get_io_unit()

    open(unit=nfich,STATUS='REPLACE',file=trim(location(out_bodies(:))))

    if (write_header) then
       !                123456789012345678901234567890123456789012345678901234567890123456789012
       write(nfich,'(A72)')'! File BODIES                                                           '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A70)')'! The symbol   ''$''      preceeds a keyword used in scanning files.    '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A68)')'! the symbol   ''bdyty''  stands for ''body type data''.                ' 
       write(nfich,'(A72)')'! These data are distributed according to some species.                 '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A68)')'! the specy    ''blmty''  stands for ''bulk element type data'',        '
       write(nfich,'(A72)')'! i.e. part or total bulk geometric description,                        '
       write(nfich,'(A72)')'! and bulk behaviour laws;                                              '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A68)')'! the specy    ''nodty''  stands for ''node type data'',                '
       write(nfich,'(A72)')'! i.e. degrees of freedom data;                                         '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A68)')'! the specy    ''tacty''  stands for ''contactor type data'';           '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A70)')'! the keyword  ''$$$$$$'' ends a body record.                           '
       write(nfich,'(A72)')'!                                                                       '
       write(nfich,'(A70)')'                                                                        '
    end if

    close(nfich)

  end subroutine Write_out_bodies_Ol
!!!------------------------------------------------------------------------   
  subroutine Clean_out_bodies

    implicit none
    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='REPLACE',file=trim(location(out_bodies(:))))
    write(nfich,'(A1)')' '
    close(nfich)

  end subroutine Clean_out_bodies
!!!------------------------------------------------------------------------
  subroutine Clean_in_bodies 
    
    implicit none
    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='REPLACE',file=trim(location(out_bodies(:))))
    write(nfich,'(A1)')' '
    close(nfich)

  end subroutine Clean_in_bodies
!!!------------------------------------------------------------------------
  subroutine Write_out_driven_dof_Ol

    implicit none
    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='REPLACE',file=trim(location(out_driven_dof(:))))
    
    write(nfich,'(A5)')'     '
    !pta old fashion: write(nfich,'(A9)')'! DRV_DOF'
    write(nfich,'(A5)')'! DOF'
    write(nfich,'(A5)')'     '
    
    close(nfich)
   
  end subroutine Write_out_driven_dof_Ol

  !> \brief Read header of a DOF file
  !> If num is 0 : read DATBOX/DOF.INI
  !> If num is >0: read OUTBOX/DOF.OUT.num
  !> If num is <0: read OUTBOX/DOF.LAST and
  !> next write will be num
  subroutine Read_in_dof_Ol(num)
    implicit none
    integer(kind=4), intent(in) :: num
    !
    integer(kind=4)   :: lc 
    character(len=9)  :: numfic 
    character(72)     :: clin !12345678901234567890
    character(len=20) :: IAM ='overall::Read_in_dof'
   
    G_nfich = get_io_unit()

    if( num==0 ) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
      current_fic_write_DOF = 0
    else if( num>0 ) then
      current_fic_write_DOF = num
      write(numfic,'(I9)') num
      numfic = adjustl(numfic)
      write(out_DOF(15:24),'(A10)') '.'//numfic(1:9)
      lc = len_trim(out_dof)
      open(unit=G_nfich,file=trim(location(out_dof(1:lc))))
    else 
      open(unit=G_nfich,file=trim(location(last_dof(:))))
      current_fic_write_DOF =-num-1
    end if

    do    
       if( .not. read_G_clin()) then
          call FATERR(IAM,'keyword ''steps'' not found')
       end if
       ! fishing for the keyword 'steps'
       if  (G_clin(2:6) .ne. 'steps') cycle       

       read(G_clin( 9:15),'(I7)') Nstep
       read(G_clin(35:48),'(D14.7)') TPSbegin

       exit
    end do
    close(G_nfich)

    ! time at the beginning of the time step is initialized to initial time
    ! time at the end of the time step is initialized to initial time

    TPS = TPSbegin

    write(6,'(A1)') ' '         !123456789012345678901234567890123456       1234567
    write(6,'(A36,D14.7,A7,I7)')'      Starting computation at time= ',TPS,'Step= ',Nstep
    write(6,'(A1)') ' '

    return 
   
  end subroutine Read_in_dof_Ol
!!!------------------------------------------------------------------------
  subroutine Write_xxx_dof_Ol(which, skip_header)

    implicit none
    integer          ::  nfich,which
    integer          :: lc 
    character(len=9) :: numfic 

    ! this logical allows to avoid header writing
    logical, intent(in), optional :: skip_header

    logical :: write_header

    write_header = .true.

    ! header writin is avoid iff skip_header is present and skip_header is "true"
    if (present(skip_header)) then
       write_header = .not. skip_header
    end if

    nfich = get_io_unit()

    if (which == 1) then
       current_fic_write_DOF = current_fic_write_DOF + 1
       write(numfic,'(I9)') current_fic_write_DOF
       numfic = adjustl(numfic)
       write(out_DOF(15:24),'(A10)') '.'//numfic(1:9)
       
       write_DOF = .true.
       lc = len_trim(out_dof)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(out_dof(1:lc))))
    else if (which == 2) then
       lc = len_trim(last_dof)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(last_dof(1:lc))))
    else
       nfich=6
    end if
  
    if (write_header) then
       write(nfich,'(A5)')'     '
       write(nfich,'(A5)')'! DOF'
       write(nfich,'(A5)')'     '
       
       write(nfich,'(A6,2X,I7,14X,A5,D14.7)')'$steps',Nstep,'time=',TPS
       write(nfich,'(A1)')' '
       write(nfich,'(A72)') '!-----------------------------------------------------------------------'
    end if
    
    if (nfich /= 6) close(nfich)
   
  end subroutine Write_xxx_dof_Ol
!!!------------------------------------------------------------------------

  !> \brief Read header of a MP_VALUES file
  !> If num is 0 : read DATBOX/MP_VALUES.INI
  !> If num is >0: read OUTBOX/MP_VALUES.OUT.num
  !> If num is <0: read OUTBOX/MP_VALUES.LAST
  !> and the next written file will be OUTBOX/MP_VALUES.OUT.num
  subroutine Read_in_mp_values_Ol(num)
    implicit none
    integer(kind=4), intent(in) :: num
    !
    integer(kind=4)   :: lc
    character(len=9)  :: numfic
    character(72)     :: clin !12345678901234567890123456
    character(len=26) :: IAM ='overall::Read_in_mp_values'

    G_nfich = get_io_unit()

    if( num==0 ) then
      open(unit=G_nfich,file=trim(location(in_mpv(:))))
      current_fic_write_mpv = 0
    else if( num>0 ) then
      current_fic_write_mpv = num
      write(numfic,'(I9)') num
      numfic = adjustl(numfic)
      write(out_mpv(21:26),'(A6)') '.'//numfic(1:5)
      lc = len_trim(out_mpv)
      open(unit=G_nfich,file=trim(location(out_mpv(1:lc))))
    else
      open(unit=G_nfich,file=trim(location(last_mpv(:))))
      current_fic_write_mpv = -num-1
    end if

    do
       if( .not. read_G_clin()) then
          call FATERR(IAM,'keyword ''steps'' not found')
       end if
       ! fishing for the keyword 'steps'
       if  (G_clin(2:6) .ne. 'steps') cycle

       read(G_clin( 9:15),'(I7)') Nstep
       read(G_clin(35:48),'(D14.7)') TPSbegin

       exit
    end do
    close(G_nfich)

    ! time at the beginning of the time step is initialized to initial time
    ! time at the end of the time step is initialized to initial time

    TPS = TPSbegin

    write(6,'(A1)') ' '         !123456789012345678901234567890123456       1234567
    write(6,'(A36,D14.7,A7,I7)')'      Starting computation at time= ',TPS,'Step= ',Nstep
    write(6,'(A1)') ' '

    return

  end subroutine Read_in_mp_values_Ol

  subroutine Write_xxx_mp_values_Ol(which)
    implicit none
    integer, intent(in) :: which
    !
    integer          ::  nfich
    integer          :: lc
    character(len=9) :: numfic

    nfich = get_io_unit()

    if (which == 1) then
       write_mpv = .true.
       current_fic_write_mpv = current_fic_write_mpv + 1
       write(numfic,'(I5)') current_fic_write_mpv
       numfic = adjustl(numfic)
       write(out_mpv(21:26),'(A6)') '.'//numfic(1:5)

       lc = len_trim(out_mpv)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(out_mpv(1:lc))))
    else if (which == 2) then
       lc = len_trim(last_mpv)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(last_mpv(1:lc))))
    else
       nfich=6
    end if

    write(nfich,'(A5)')  '     '
    write(nfich,'(A13)') '! MP VALUES'
    write(nfich,'(A5)')  '     '
    
    write(nfich,'(A6,2X,I7,14X,A5,D14.7)')'$steps',Nstep,'time=',TPS
    write(nfich,'(A1)') ' '
    write(nfich,'(A72)') '!-----------------------------------------------------------------------'

    if (nfich /= 6) close(nfich)

  end subroutine Write_xxx_mp_values_Ol
!!!------------------------------------------------------------------------
  !> \brief Read header of a VlocRloc file
  !> If num is 0 : read DATBOX/VlocRloc.INI
  !> If num is >0: read OUTBOX/VlocRloc.OUT.num
  !> If num is <0: read OUTBOX/VlocRloc.LAST
  !> and next file written will be OUTBOX/VlocRloc.out.num
  subroutine Read_in_Vloc_Rloc_Ol(num)
    implicit none
    integer(kind=4), intent(in) :: num
    !
    integer(kind=4)   :: l_Nstep, lc
    real(kind=8)      :: l_TPSbegin, tol_TPSbegin
    character(len=9)  :: numfic 
    character(len=34) :: IAM='overall::Read_in_Vloc_Rloc'
    character(len=154):: cout

    tol_TPSbegin = 1.d-03

    G_nfich= get_io_unit()

    if( num==0 ) then
      current_fic_write_Vloc_Rloc = 0
      open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))
    else if( num>0 ) then
      current_fic_write_Vloc_Rloc = num
      write(numfic,'(I9)') num
      numfic = adjustl(numfic)
      write(out_Vloc_Rloc(21:30),'(A10)') '.'//numfic(1:9)
      lc = len_trim(out_vloc_rloc)
      open(unit=G_nfich,file=trim(location(out_Vloc_Rloc(1:lc))))
    else
      current_fic_write_Vloc_Rloc = -num-1
      open(unit=G_nfich,file=trim(location(last_Vloc_Rloc(:))))
    end if

    do    
       if( .not. read_G_clin()) then
          call FATERR(IAM,'keyword ''steps'' not found')
       end if
       ! fishing for the keyword 'steps'
       if  (G_clin(2:6) .ne. 'steps') cycle
       read(G_clin( 9:15),'(I7)') l_Nstep
       read(G_clin(35:48),'(D14.7)') l_TPSbegin
       exit
    end do
    close(G_nfich)
    
    !IF (l_TPSbegin .NE. TPSbegin .OR. l_Nstep .NE. Nstep) THEN
    if (abs(l_TPSbegin -TPSbegin) > tol_TPSbegin*H .or. l_Nstep .ne. Nstep) then
       write(cout,'(A72)') 'There is an inconsistancy in the definition of steps and/or time       '
       write(cout,'(A72)') 'Check DATBOX/DOF.INI and DATBOX/Vloc_RLoc.INI                          '
       call faterr(IAM,cout)
    end if
    
    return 
 
  end subroutine Read_in_Vloc_Rloc_Ol
!!!------------------------------------------------------------------------
  subroutine Write_xxx_Vloc_Rloc_Ol(which, skip_header)

    implicit none
    integer          :: nfich,which
    integer          :: lc 
    character(len=9) :: numfic 

    ! this logical allows to avoid header writing
    logical, intent(in), optional :: skip_header

    logical :: write_header

    write_header = .true.

    ! header writing is avoid iff skip_header is present and skip_header is "true"
    if (present(skip_header)) then
       write_header = .not. skip_header
    end if

    nfich = get_io_unit()

    if (which == 1) then
       write_Vloc_Rloc=.true.
       current_fic_write_Vloc_Rloc = current_fic_write_Vloc_Rloc + 1
       write(numfic,'(I9)') current_fic_write_Vloc_Rloc
       numfic = adjustl(numfic)
       write(out_Vloc_Rloc(21:30),'(A10)') '.'//numfic(1:9)
       
       lc = len_trim(out_Vloc_Rloc)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(out_Vloc_Rloc(1:lc))))
    else if (which == 2) then
       lc = len_trim(last_Vloc_Rloc)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(last_Vloc_Rloc(1:lc))))
    else
       nfich = 6
    end if
 
    if (write_header) then 
       write(nfich,'(A5)')'     '
       write(nfich,'(A11)')'! Vloc_Rloc'
       write(nfich,'(A6)')'      '
       
       write(nfich,'(A6,2X,I7,14X,A5,D14.7)')'$steps',Nstep,'time=',TPS
       write(nfich,'(A1)')' '
       write(nfich,'(A72)') '!-----------------------------------------------------------------------'
    end if    

    if (nfich /= 6) close(nfich)

  end subroutine Write_xxx_Vloc_Rloc_Ol
!!!------------------------------------------------------------------------
  !> \brief Read header of a GPV file
  !> If num is 0 : read DATBOX/GPV.INI
  !> If num is >0: read OUTBOX/GPV.OUT.num
  !> If num is <0: read OUTBOX/GPV.LAST
  !> and next written file will be OUTBOX/GPV.OUT.num
  subroutine Read_in_gpv_Ol(num)
    implicit none
    integer(kind=4), intent(in) :: num
    !
    real(kind=8)      :: l_TPSbegin
    integer(kind=4)   :: lc, l_Nstep   
    character(len=9)  :: numfic 
    character(len=28) :: IAM='overall::Read_in_gpv'

    G_nfich = get_io_unit()

    if( num==0 ) then
      current_fic_write_gpv = 0
      open(unit=G_nfich,file=trim(location(in_gpv(:))))
    else if( num>0 ) then
      current_fic_write_gpv = num
      write(numfic,'(I9)') num
      numfic = adjustl(numfic)
      write(out_gpv(15:24),'(A10)') '.'//numfic(1:9)
      lc = len_trim(out_gpv)
      open(unit=G_nfich,file=trim(location(out_gpv(1:lc))))
    else
      current_fic_write_gpv = -num-1
      open(unit=G_nfich,file=trim(location(last_gpv(:))))
    end if

    do    
       if( .not. read_G_clin()) then
          call FATERR(IAM,'keyword ''steps'' not found')
       end if
       if  (G_clin(2:6) /= 'steps') cycle       ! fishing for the keyword 'steps'
       read(G_clin(9:15),'(I7)') l_Nstep
       read(G_clin(35:48),'(D14.7)') l_TPSbegin
       exit
    end do
    close(G_nfich)
    
    if (l_TPSbegin .ne. TPSbegin .or. l_Nstep .ne. Nstep) then
       write(6,'(A43)') ' l_TPS_BEGIN | TPS_BEGIN | l_NSTEP | NSTEP '
       write(6,'(2(1X,D14.7),2(1X,I7))') l_TPSbegin,TPSbegin,l_Nstep,Nstep
       write(6,'(A72)') 'There is an inconsistancy in the definition of steps and/or time       '
       write(6,'(A72)') 'Check DATBOX/DOF.INI and DATBOX/GPV.INI                                '
       call faterr(IAM,'read log')
    end if

    return 
   
  end subroutine Read_in_gpv_Ol
!!!------------------------------------------------------------------------
  subroutine Write_xxx_gpv_Ol(which)

    implicit none
    integer          :: nfich,which
    integer          :: lc 
    character(len=9) :: numfic 

    nfich = get_io_unit()

    if (which == 1) then
       write_gpv_actif=.true.
       current_fic_write_gpv = current_fic_write_gpv + 1
       write(numfic,'(I9)') current_fic_write_gpv
       numfic = adjustl(numfic)
       write(out_gpv(15:24),'(A10)') '.'//numfic(1:9)
       lc = len_trim(out_gpv)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(out_gpv(1:lc))))
    else if (which == 2) then
       lc = len_trim(last_gpv)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(last_gpv(1:lc))))
    else
       nfich = 6
    end if
  
    write(nfich,'(A5)')'     '
    write(nfich,'(A20)')'! Gauss Point Values'
    write(nfich,'(A6)')'      '
    
    write(nfich,'(A6,2X,I7,14X,A5,D14.7)')'$steps',Nstep,'time=',TPS
    write(nfich,'(A1)')' '
    write(nfich,'(A72)') '!-----------------------------------------------------------------------'
    
    if (nfich /= 6) close(nfich)

  end subroutine Write_xxx_gpv_Ol
!!!------------------------------------------------------------------------
  subroutine Display_prox_tactors_Ol

    implicit none
    
    write(6,'(A13)')'! PROX_TACTORS'
    write(6,'(A6)')'      '
    write(6,'(A6,2X,I7,14X,A5,D14.7)')'$steps',Nstep,'time=',TPS
    write(6,'(A1)')' '
    write(6,'(A72)') '!-----------------------------------------------------------------------'

  end subroutine Display_prox_tactors_Ol
!!!------------------------------------------------------------------------
  subroutine Write_xxx_Rnod_Ol(which)

    implicit none
    integer          :: nfich,which
    integer          :: lc 
    integer,save     :: current_fic_write_Rnod=0
    character(len=9) :: numfic 

    nfich = get_io_unit()

    if (which == 1) then
       write_Rnod = .true.
       current_fic_write_Rnod = current_fic_write_Rnod + 1
       write(numfic,'(I9)') current_fic_write_Rnod
       numfic = adjustl(numfic)
       write(out_Rnod(16:25),'(A10)') '.'//numfic(1:9)
       lc = len_trim(out_Rnod)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(out_Rnod(1:lc))))
    else if (which == 2) then
       lc = len_trim(last_Rnod)
       open(unit=nfich,STATUS='REPLACE',file=trim(location(last_Rnod(1:lc))))
    else
       nfich = 6
    end if
    
    write(nfich,'(A5)')'     '
    write(nfich,'(A6)')'! Rnod'
    write(nfich,'(A5)')'     '
    write(nfich,'(A6,2X,I7,14X,A5,D14.7)')'$steps',Nstep,'time=',TPS
    write(nfich,'(A1)')' '
    write(nfich,'(A72)') '!-----------------------------------------------------------------------'
    
    if (nfich /= 6) close(nfich)
   
  end subroutine Write_xxx_Rnod_Ol
!!!------------------------------------------------------------------------

  !fd les entites sont la pour fournir un numero de modele mecanique unique au solveur de contact
  !fd ce qui est necessaire a la creation d'une table de connectivite des contacts. 
  !fd  * a la lecture des modeles (rbdy2|rbdy3, mailx, mbs2|mbs3) ils s'abonnent par type a la liste d'entite (add_nb_Entity) et
  !fd    ils stockent le debut du rang de leur type dans cette liste 
  !fd  * a l'initialisation du contact compute box on cree le tableau d'entite vide (Init_EntityList)
  !fd  * lors de la detection de contact on stocke qpour une entite le nombre d'entites voisines 
  !fd  * lors de la preparation de la resolution on construit la "list" des contact pour chaque entite; "ik" est un compteur interne
  !fd  * ajout du statut is_clamped le 22 octobre 2018 pour essayer d'ameliorer la performance de nlgs
  !rm  * il faut que cette fonction soit appelee après les LoadTactors
  
  subroutine Init_EntityList

    implicit none
    integer           :: ient
    character(len=25) :: IAM='overall::Init_EntityList'

    if ( .not. is_entity_initialized) then
      if (nb_entity == 0) then
         call logmes('make sure that there are some xxxxx_LoadTactors calls that work')
         call FATERR(IAM,'No mechanical entity declared !!')
      end if

      if(allocated(entity)) deallocate(entity)
      allocate(entity(nb_ENTITY))

      do ient=1,nb_ENTITY
        nullify(entity(ient)%list)
        entity(ient)%ik=0
        entity(ient)%is_clamped=0
      enddo

      is_entity_initialized = .True.
     
    endif

    do ient=1,nb_ENTITY
      entity(ient)%nb = 0
    enddo

  end subroutine Init_EntityList
!!!------------------------------------------------------------------------
  subroutine Create_EntityList

    implicit none
    integer :: ient,errare
    character(len=25) :: IAM='overall::create_EntityList'

    do ient=1,nb_ENTITY
       if(associated(entity(ient)%list)) deallocate(entity(ient)%list)
       
       allocate(entity(ient)%list(entity(ient)%nb),stat=errare)
       if (errare /= 0) call FATERR(IAM,'error allocating entity(ient)%list')

       entity(ient)%list=0
       entity(ient)%ik=0
    end do


  end subroutine
!!!------------------------------------------------------------------------
  subroutine Free_EntityList

    implicit none
    integer :: ient

    do ient=1,nb_ENTITY
      if (associated(entity(ient)%list)) then
        deallocate(entity(ient)%list)
        nullify(entity(ient)%list)
      end if
      entity(ient)%ik = 0
    end do
    
  end subroutine Free_EntityList
!!!------------------------------------------------------------------------
  ! this is separate from clean_memory because
  ! of Xper charms refinement feature
  subroutine Clean_EntityList()
    implicit none

    ! entity is allocated only if contact happens
    if (allocated(entity)) then
      call Free_EntityList()
      deallocate(entity)
    endif

    is_entity_initialized = .false.
    nb_ENTITY = 0

  end subroutine

  logical function is_EntityList_Initialized()
    implicit none
    is_EntityList_Initialized = is_entity_initialized
  end function

!!!------------------------------------------------------------------------
  integer function get_nb_ENTITY()

    implicit none

    get_nb_ENTITY = nb_ENTITY

  end function get_nb_ENTITY
!!!------------------------------------------------------------------------
  subroutine add_nb_ENTITY(nb)

    implicit none
    integer :: nb
    
    nb_ENTITY = nb_ENTITY + nb
    
  end subroutine add_nb_ENTITY
!!!------------------------------------------------------------------------
  subroutine set_clamped_status_ENTITY(ient)

    implicit none
    integer :: ient

    ! in case you ask : initialization cannot be done
    ! here because this function may can be called from
    ! inside an openMP loop.
    if ( .not. is_entity_initialized) then
        call logmes('try to use "overall_InitEntityList" after your "LoadTactors" command',.True.)
        call faterr('overall::set_clamped_status_ENTITY', &
                    'trying to set entity status but list not initialized')
    end if

    entity(ient)%is_clamped=1
    
  end subroutine set_clamped_status_ENTITY
!!!------------------------------------------------------------------------
  integer function get_status_ENTITY(ient)

    implicit none
    integer :: ient
    
    get_status_ENTITY = entity(ient)%is_clamped
    
  end function get_status_ENTITY
!!!------------------------------------------------------------------------
  subroutine set_run_contactor(Nstep_rough_seek)

    implicit none
    integer :: Nstep_rough_seek

    RUN_TACTOR = .false.

    if ( FIRST_RUN .or. modulo(Nstep,Nstep_rough_seek) == 0) then 
       FIRST_RUN = .false.
       RUN_TACTOR = .true.
    end if
    
  end subroutine set_run_contactor
!!!------------------------------------------------------------------------
  subroutine time_increment
    use parameters, only : INTEGRATOR_BETA2

    implicit none
      
    time_ini = time_end
    call cpu_time(time_end)

    !fd gestion de l'adaptation du pas de temps 

    oldH=H
    if (newH /= 0.d0) then
       H=newH
       newH=0.d0
    endif

    if ( M_INTEGRATOR_ID == INTEGRATOR_BETA2 ) then

      !fd il faudrait un test cfl pour savoir si ce h va bien ....

      c1_beta2 = 2.d0 / (1.d0 + 2.d0* beta2)
      c2_beta2 = 2.d0 * (1.d0 - beta2 ) / H
      c3_beta2 = 2.d0 * beta2 - 1.d0
      c4_beta2 = 2.d0 * beta2 / H

    endif

    TPS   = TPSbegin + H
    Nstep = Nstep + 1 
       
  end subroutine time_increment
!!!------------------------------------------------------------------------
  subroutine Updt_time_begin

    implicit none

    TPSbegin = TPS 

  end subroutine Updt_time_begin
!!!------------------------------------------------------------------------
  subroutine Set_newton_tolerance(reel)
  
    implicit none
    real(kind=8) :: reel

    critconv=reel

  end subroutine Set_newton_tolerance
!!!------------------------------------------------------------------------
  subroutine set_newton_maxloop(val)

    implicit none
    integer :: val

    bulk_itermax = val

  end subroutine set_newton_maxloop
!!!------------------------------------------------------------------------
  subroutine Set_newton_badloop(val)

    implicit none
    integer :: val

    bulk_iterbad = val

  end subroutine Set_newton_badloop
!!!------------------------------------------------------------------------
  subroutine Set_newton_goodloop(val)
  
    implicit none
    integer :: val

    bulk_itergood = val

  end subroutine Set_newton_goodloop
!!!------------------------------------------------------------------------
  subroutine Set_newton_rate_step(val)
  
    implicit none
    integer :: val

    numincconv = val

  end subroutine Set_newton_rate_step
!!!------------------------------------------------------------------------
  subroutine Set_newton_loop(entier)
  
    implicit none
    integer :: entier

    bulk_iter = entier

  end subroutine Set_newton_loop
!!!------------------------------------------------------------------------
  subroutine Incre_newton_loop
  
    implicit none

    character(len=80):: cout

    bulk_iter = bulk_iter + 1

    call logmes('   ')
    write(cout,'(A,I0)') '  @    finishing Newton-Raphson iteration: ',bulk_iter
    call logmes(cout)
    call logmes('   ')
    
  end subroutine Incre_newton_loop

!!!------------------------------------------------------------------------
  subroutine check_newton_convergence(value,iconv)
  
    implicit none
    ! =0 CV, =1 on ne sait pas, =2 DV
    integer :: iconv
    real(kind=8) :: value

    iconv = 1

    if (value < critconv ) then
     iconv=0 
    endif

    if (iconv /= 0 .and. bulk_iter == bulk_itermax) then
      ICONV=2
    endif

  end subroutine

!!!------------------------------------------------------------------------
  subroutine Compute_newton_time_step(ReDoStep,StopComputation)

    implicit none
    integer      :: kbonconv=0,kmauvconv=0
    real(KIND=8) :: testemps,Ttot,arretH,Hcurrent
    ! ReDoStep,StopComputation : 0 if yes else 1
    integer      :: ReDoStep, StopComputation 
    character(len=80) :: cout
                             !123456789012345678901234
    character(len=24) :: IAM='compute_newton_time_step'

    ! par defaut:
    !  bulk_max = 50 ; bulk_itergood = 10 ; bulk_iterbad = 30 ; numincconv = 3

    if (bulk_itergood > bulk_iterbad) call FATERR(IAM,'itergood larger than iterbad')       
    
    ReDoStep=1
    StopComputation=1

    Hcurrent  = H
    Ttot    = TPSendall
    
    if (Nstep == 1) then
       kbonconv = 0
       kmauvconv= 0
    end if
    !
    ! le nb d'iter atteint le nb max ...
    !

    if (.not. conv_contact) call LOGMES('WARNING: The contact has diverged')

    if (bulk_iter == bulk_itermax) then

       call LOGMES (          '  @    Newton iteration limit have been reached')
       write(cout,'(A,I0)')   '  @    Iteration number ',bulk_itermax
       call LOGMES(cout)

       newH = H / DSQRT(2.D0)
       
       write(cout,'(A,D12.5)')'  @    Restart increment with a smaller time step ',newH  
       call LOGMES(cout)

       call logmes(' ')
       
       Nstep = Nstep - 1
       
       ReDoStep  = 0
       kbonconv  = 0
       kmauvconv = 0

       !fd pourquoi ?return

    else
       if (bulk_iter <= bulk_itergood) then
          ! GOOD CONVERGENCE CASE
          kbonconv  = kbonconv + 1
          kmauvconv = 0
       else if ( bulk_iter >= bulk_iterbad ) then
          ! BAD CONVERGENCE CASE
          kbonconv  = 0
          kmauvconv = kmauvconv + 1
       else 
          ! STANDARD CASE
          kbonconv  = 0
          kmauvconv = 0
       end if
       
       ! Check if one of the bounds has been reached
       if (kbonconv == numincconv) then
          ! Good bound reached
          newH = H * DSQRT(2.D0)
          kbonconv  = 0
          kmauvconv = 0
          if (newH .ge. H_max) newH = H_max
       else if (kmauvconv == numincconv) then
          ! Bad bound reached
          newH = H / DSQRT(2.D0)
          kbonconv  = 0
          kmauvconv = 0
       else
          newH=H
       end if
    end if
    
    arretH = 1.D-04 * Hcurrent
    
    if ( (TPS+newH) - Ttot  > arretH) newH = Ttot - TPS

    !depend du probleme
    if (newH < H_min) then    
       if (dabs(Ttot - (TPS+newH)) <= arretH) then
          call LOGMES('  @    Final Time reached')
          call LOGMES('  @    End of simulation')
          StopComputation=0
       else
          call LOGMES('  @    Time step too small')
          call LOGMES('  @    The simulation has to be stopped')
          StopComputation=0
       end if
       call logmes(' ')
    else
      write(cout,'(A,D12.5)') '  @    Time step for new increment',newH  
      call LOGMES(cout)
      call logmes(' ')
    end if
    
  end subroutine Compute_newton_time_step
!!!------------------------------------------------------------------------
  subroutine DISPLAY_TIME

    implicit none
    character(len=103) :: cout

    write(cout,800) time_end-time_ini
800 format(' ',' @ ','Elapsed time in the previous step,           =',D14.7)
    call LOGMES(cout)
    call LOGMES(   '    ')
    call LOGMES(   '  +++++                                                    +++++')
    call LOGMES(   '    ')
    write(cout,777) Nstep
777 format(' ',' @ ','step = ',I7)                                       
    call LOGMES(cout)
    write(cout,778) TPSbegin
778 format(' ',' @ ','Time at the beginning of time step, TPSbegin =',D14.7)
    call LOGMES(cout)
    write(cout,779) TPS
779 format(' ',' @ ','Time at the end of time step,       TPS      =',D14.7)
    call LOGMES(cout)

  end subroutine DISPLAY_TIME
!!!------------------------------------------------------------------------
  subroutine Clean_writing_flags
  
    implicit none

    write_DOF       = .false. 
    write_Rnod      = .false.
    write_Vloc_Rloc = .false.
    write_gpv_actif = .false.
    write_mpv       = .false.

  end subroutine Clean_writing_flags
!!!------------------------------------------------------------------------
  integer function Get_NSTEP()

    implicit none
    
    Get_NSTEP = NSTEP
    
  end function Get_NSTEP
!!!------------------------------------------------------------------------
  subroutine Init_dimension(chaine)
  
    implicit none
    character(len=29) :: IAM='overall::initialize_dimension'
    character(len=10), intent(in) :: chaine

    DIME = chaine

    if (DIME(1:2).eq.'2D') then
       nbDIME = 2
    else if (DIME(1:2).eq.'3D') then
       nbDIME = 3
    else
       call FATERR(IAM,'DIME unexpected')
    end if

    dime_mod = get_dime_mode_id_from_name(DIME)

  end subroutine Init_dimension
!!!------------------------------------------------------------------------
  subroutine Set_time_step(reel)

    implicit none
    real(kind=8) :: reel
    logical :: is_first_time = .true.

    newH = reel
    !fd fait dans time_increment   H    = reel

    !fd au premier passage on est oblige d initialiser H
    if (is_first_time) then
      H=newH       
      is_first_time=.false.
      ! on donne une valeur au pas de temps min si pas fait 
      if ( H < H_min ) H_min = H
      ! on donne une valeur au pas de temps max si pas fait 
      if ( H > H_max ) H_max = H
    endif

    is_H_set = .TRUE.

  end subroutine Set_time_step
!!!------------------------------------------------------------------------
  subroutine Init_gear_integrator
    use parameters, only : INTEGRATOR_GEAR
  
    implicit none
                             !123456789012345678901
    character(len=21) :: IAM='Integrator::init_gear'     
    
    if (.not. is_H_set) call faterr(IAM,'H is not set')

    smooth_method = .true.
    M_INTEGRATOR_ID = INTEGRATOR_GEAR

    c1 = H
    c2 = c1*H*0.5
    c3 = c2*H/3.d0
    c4 = c3*H*0.25
    
    cr = md_gear0*H*H*0.5d0
    cv = md_gear1*H*0.5d0
    cb = md_gear3*3.d0/H
    cc = md_gear4*12.d0/(H*H)

  end subroutine Init_gear_integrator
!!!------------------------------------------------------------------------
  subroutine Init_verlet_integrator
    use parameters, only : INTEGRATOR_VERLET
  
    implicit none
                             !12345678901234567890123
    character(len=23) :: IAM='Integrator::init_verlet'     
    
    if (.not. is_H_set) call faterr(IAM,'H is not set')
  
    smooth_method = .true.
    M_INTEGRATOR_ID = INTEGRATOR_VERLET

  end subroutine Init_verlet_integrator
!!!------------------------------------------------------------------------
  subroutine Init_theta_integrator(reel)
    use parameters, only : INTEGRATOR_MOREAU

    implicit none
    real(kind=8) :: reel
                             !1234567890123456789012
    character(len=22) :: IAM='Integrator::init_theta'     
    
    if (.not. is_H_set) call faterr(IAM,'H is not set')
    
    theta = reel
    M_INTEGRATOR_ID = INTEGRATOR_MOREAU

  end subroutine Init_theta_integrator
 !!!------------------------------------------------------------------------ 
  subroutine Init_beta2_integrator(reel)
    use parameters, only : INTEGRATOR_BETA2

    implicit none
    real(kind=8), intent(in) :: reel
                             !1234567890123456789012
    character(len=22) :: IAM='Integrator::init_beta2'     
    
    if (.not. is_H_set) call faterr(IAM,'H is not set')

    beta2 = reel
    
    c1_beta2 = 2.d0 / (1.d0 + 2.d0 * beta2)
    c2_beta2 = 2.d0 * (1.d0 - beta2) / H 
    c3_beta2 = 2.d0 * beta2 - 1.d0
    c4_beta2 = 2.d0 * beta2 / H 

    M_INTEGRATOR_ID = INTEGRATOR_BETA2

  end subroutine Init_beta2_integrator 
 !!!------------------------------------------------------------------------ 
  subroutine Init_qs_integrator()
    use parameters, only : INTEGRATOR_QS

    implicit none

    character(len=22) :: IAM='Integrator::init_qs'     
    
    if (.not. is_H_set) call faterr(IAM,'H is not set')
    ! if (H /= 1.d0) call faterr(IAM,'H must be equal to 1.') 
    
    M_INTEGRATOR_ID = INTEGRATOR_QS

  end subroutine Init_QS_integrator 
!!!------------------------------------------------------------------------
  subroutine get_theta(my_theta)
    implicit none
    real(kind=8) :: my_theta

    my_theta = theta

  end subroutine
!------------------------------------------------------------------------
  subroutine Init_CN_integrator(reel)
    use parameters, only : INTEGRATOR_MOREAU

    implicit none
    real(kind=8) :: reel

    !!!fd a voir le micmac th / meca ..

    T_INTEGRATOR_ID = INTEGRATOR_MOREAU
    theta_t = reel

  end subroutine Init_CN_integrator

!!!------------------------------------------------------------------------
  ! subroutine get_integrator_id(my_integrator)
  !   implicit none
  !   integer :: my_integrator

  !   my_integrator = INTEGRATOR_ID

  ! end subroutine
!!!------------------------------------------------------------------------
  subroutine get_explicit_beta(my_beta)
    implicit none
    real(kind=8) :: my_beta

    my_beta = beta2

  end subroutine 
!!!------------------------------------------------------------------------

  subroutine Set_initial_step(entier)

    implicit none
    integer(kind=4) :: entier

    NStep = entier

  end subroutine Set_initial_step
!!!------------------------------------------------------------------------
  subroutine Set_initial_time(reel)

    implicit none
    real(kind=8) :: reel

    TPS = reel
    TPSbegin = reel

  end subroutine Set_initial_time
!!!------------------------------------------------------------------------
  subroutine Set_final_time(reel)

    implicit none
    real(kind=8) :: reel

    TPSendall = reel

  end subroutine Set_final_time
!!!------------------------------------------------------------------------
  subroutine Set_min_time_step(reel)

    implicit none
    real(kind=8) :: reel

    H_min = reel

  end subroutine Set_min_time_step
!!!------------------------------------------------------------------------
  subroutine Set_max_time_step(reel)
  
    implicit none
    real(kind=8) :: reel

    H_max = reel

  end subroutine Set_max_time_step
!!!------------------------------------------------------------------------
  subroutine Acti_large_computation
    
    implicit none
    xxl_check = .true.
  end subroutine Acti_large_computation
!!!------------------------------------------------------------------------
  subroutine init_post_data_ol(ifirst,ilast)
    
    implicit none
    integer :: ifirst,ilast

    i_post_first     = ifirst
    i_post_last      = ilast
    i_post_increment = i_post_first
    post_processing  = .true.
    
  end subroutine init_post_data_ol
!!!------------------------------------------------------------------------ 
  subroutine update_post_data_ol(info)
    implicit none
    integer :: info
    !                      1234567890123456789012345678901234
    post_body_sample    = 'POSTPRO/BODY_SNAPSHOT000000.DAT'
    post_contact_sample = 'POSTPRO/CONTACT_SNAPSHOT000000.DAT'

    in_dof       = 'OUTBOX/DOF.OUT.          '! current degrees of freedom file;
    in_Vloc_Rloc = 'OUTBOX/Vloc_Rloc.OUT.    '! current reaction local variables file;
    
    write(post_body_sample(22:27),'(I6.6)') i_post_increment
    write(post_contact_sample(25:30),'(I6.6)') i_post_increment

    write(in_dof(16:19),'(I4.4)') i_post_increment
    write(in_Vloc_Rloc(22:25),'(I4.4)') i_post_increment
 
    info = 1
    
    i_post_increment = i_post_increment + 1
    
    if ( i_post_increment > i_post_last ) then
       info = 0
       write(6,'(1X,A3,A25)') ' @ ',in_dof
       write(6,'(1X,A3,A25)') ' @ ',in_Vloc_Rloc
       return
    end if

  end subroutine update_post_data_ol
!!!------------------------------------------------------------------------
  real(kind=8) function get_time()

    implicit none

    get_time = TPS

  end function get_time
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_time_step()

    implicit none

    get_time_step = H

  end function get_time_step
!!!------------------------------------------------------------------------ 
  function location(in)

    implicit none
    character(len=*)  :: in
    character(len=250):: location
    
    location = trim(WORKING_DIRECTORY)//trim(in)

  end function location
!!!------------------------------------------------------------------------ 
  subroutine Set_Working_Directory(chaine)

    implicit none
    character(len=*):: chaine
    
    if( chaine( len(chaine):len(chaine) ) /= '/' ) then
      WORKING_DIRECTORY=trim(chaine)//'/'
    else
      WORKING_DIRECTORY=chaine
    end if
    
  end subroutine Set_Working_Directory
!!!------------------------------------------------------------------------ 
  subroutine set_with_experimental_dev()

    implicit none
    
    with_experimental_dev=.true.
    
  end subroutine set_with_experimental_dev
!!!------------------------------------------------------------------------ 
  logical function get_with_experimental_dev()

    implicit none
    
    get_with_experimental_dev = with_experimental_dev

  end function get_with_experimental_dev
!!!------------------------------------------------------------------------ 
  subroutine set_is_externalFEM

    implicit none
    
    is_externalFEM =.true.
    
  end subroutine 
!!!------------------------------------------------------------------------ 
  logical function get_is_externalFEM()

    implicit none
    
    get_is_externalFEM = is_externalFEM

  end function 
!!!------------------------------------------------------------------------  
  subroutine Set_Contact_Detection_Configuration(r8_1,r8_2)
    implicit none 
    real(kind=8) :: r8_1,r8_2

    is_contactdetectionconfiguration_defined=.true.

    vw_b = r8_1
    vw_e = r8_2 

  end subroutine
!!!------------------------------------------------------------------------  
  subroutine set_enrichissement_parameter(d1_eta_)
    implicit none
    real(kind=8), intent(in) :: d1_eta_
    d1_eta = d1_eta_
  end subroutine set_enrichissement_parameter  
!!!------------------------------------------------------------------------  

  subroutine set_need_internal_tact(nb_int)
    implicit none
    integer, intent(in) :: nb_int

    need_internal_tact = nb_int

  end subroutine

  integer function get_need_internal_tact()
    implicit none

    get_need_internal_tact  = need_internal_tact

  end function

!!!------------------------------------------------------------------------  
  subroutine delete_part_of_file(fname)
    implicit none
    character(len=*), intent(in) :: fname
    !
    integer      :: f_unit, tmp_unit, ios
    real(kind=8) :: current_time
    !
    integer, parameter :: max_line_len = 25*16 ! should be overkill
    character(len=max_line_len) :: buffer

    open(NEWUNIT=f_unit  , STATUS="OLD"    , FORM="FORMATTED", ACTION="READ" , POSITION="REWIND", FILE=fname)
    open(NEWUNIT=tmp_unit, STATUS="SCRATCH", FORM="FORMATTED")

    ! copy fname to tmp until first double record is above TPSbegin
    read(f_unit, '(A)', IOSTAT=ios) buffer
    do while( ios == 0 )
        read(buffer(2:15), '(D14.7)', IOSTAT=ios) current_time
        if( current_time > TPSbegin ) exit
        write(tmp_unit, '(A)') trim(buffer)
        read(f_unit, '(A)', IOSTAT=ios) buffer
    end do

    ! rewrite file only if last time read < TPSbegin
    if( ios==0 ) then

        ! rewind tmp and close fname
        close(f_unit)
        rewind(tmp_unit)

        ! copy back from tmp to fname
        open(NEWUNIT=f_unit, STATUS="REPLACE", FORM="FORMATTED", ACTION="WRITE", POSITION="REWIND", FILE=fname)
        read(tmp_unit, '(A)', IOSTAT=ios) buffer
        do while( ios == 0 )
            write(f_unit, '(A)') trim(buffer)
            read(tmp_unit, '(A)', IOSTAT=ios) buffer
        end do

    end if

    ! close properly
    close(f_unit)
    close(tmp_unit)

  end subroutine delete_part_of_file
!!!------------------------------------------------------------------------  

  subroutine overall_clean_memory
    implicit none

    WORKING_DIRECTORY='./'
    Nstep     = 0
    TPS       = 0.d0
    TPSbegin  = 0.d0
    TPSendall = 0.d0
    H         = 1.d0
    is_H_set  = .false.

    current_fic_write_DOF       = 0
    current_fic_write_Vloc_Rloc = 0
    current_fic_write_gpv       = 0
    current_fic_write_mpv       = 0

    newH  = 0.d0
    H_max = 1.d-24
    H_min = 1.d+24

    bulk_itermax  = 50
    bulk_itergood = 10
    bulk_iterbad  = 30
    numincconv    = 3

    conv_contact = .true.

    DIME     ='2D        '
    nbDIME   = 2
    dime_mod = i_2D_strain

    itchache = .false.

    write_Rnod      = .false.
    write_DOF       =.false.
    write_Vloc_Rloc = .false.
    write_gpv_actif = .false.
    write_mpv       = .false.

    time_ini = 0.d0
    time_end = 0.d0

    nlgs_solver2D       = .false.
    cpg_solver          = .false.
    new_int_nlgs_solver = .false.
    nlgs_solver3D       = .false.

    smooth_method = .false.

    M_INTEGRATOR_ID = 0
    T_INTEGRATOR_ID = 0

    is_contactdetectionconfiguration_defined = .false.

    xxl_check = .false.

    post_processing = .false.
    FIRST_RUN  = .true.
    RUN_TACTOR = .false.
    
    call Clean_EntityList()

    with_experimental_dev = .false.
    is_externalFEM        = .false.

    d1_eta = 0.d0

    BetaiComputation_flag = .false.
    MpComputation_flag    = .false.

    need_internal_tact    = 0

  end subroutine overall_clean_memory

end module overall

