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
module BULK_BEHAVIOUR

  use overall
  use utilities
  use parameters

  implicit none

  private

  
  real(kind=8),public   :: grav1,grav2,grav3   ! components of gravity


  ! utilise pour SPHV,COCO,density
  type :: T_r8_PARAM
      integer           :: type  ! =0 valeur constante , =1  field
      real(kind=8)      :: val
      character(len=30) :: field      
  end type T_r8_PARAM

  ! used for dilation
  type :: T_3r8_PARAM
      integer                    :: type  ! =0 valeur constante , =1  field
      real(kind=8),dimension(3)  :: val
      character(len=30)          :: field      
  end type T_3r8_PARAM
  
  ! utilise pour elas_coeff
  type :: T_vect_PARAM
      integer                               :: type  ! =0 valeur constante , =1  vfield
      real(kind=8),dimension(21)            :: val
      character(len=30)                     :: vfield
      integer                               :: vsize
  end type T_vect_PARAM

  ! fd new le 27/02/08

  ! type :: T_USER_PARAM
  !     character(LEN=50) :: name
  !     integer           :: type  ! =0 integer , =1  real
  !     integer           :: ival
  !     real(kind=8)      :: rval
  ! end type T_USER_PARAM

  ! bulk behaviour type ------------------------------------------------------------

  type ::  T_BULK_BEHAV

     character(len=30)  :: lawty                ! bulk behaviour law name              
     character(len=5)   :: behav                ! bulk behaviour law nickname 
   

     
     type(T_r8_param)            :: Umass       ! mass      
                                                !
     integer                     :: elas_modele ! 0: rigide 1: elas|Neo Hookeen|hyper 2: Mooney-Rivlin 3: HartSmith
     
     integer                     :: anisotropie ! 0: isotrope, 1: orthotrope, 2: anisotrope

     !real(kind=8), dimension(21) :: elas_coeff  ! elas_modele = 1
     type(T_vect_param)          :: elas_coeff  ! elas_modele = 1
                                                !   anisotropy
                                                !     iso:   coeff(1)=Young, coeff(2)=poisson   
                                                !     ortho: Young11, Young22, Young33
                                                !            Poisson12,Poisson13,Poisson23
                                                !            G12,G13,G23
                                                !     ani  : les 21 coeff
                                                ! elas_modele = 2
                                                !    Mooney-Rivlin: a1,a2,eps
                                                ! elas_modele = 3
                                                !    HartSmith    : a1

   ! critere
   integer                     :: critere       ! type de critère: 0 pas, 1 VM, 2 HILL
   real(kind=8), dimension(10) :: crit_coeff    ! VM:   none
                                                ! HILL:
   ! ecrouissage isotrope
   integer                     :: iso_hard      ! ecrouissage isotrope: 0 pas, 1 swift, 2 line, 3 ...                    
   real(kind=8), dimension(80) :: isoh_coeff    ! swift   : C0,EPS0,n
                                                ! lineaire: SIG0,H
                                                ! Hollomon: H,n
                                                ! Expo    : SIG0,SIG_inf,n
                                                ! PT by PT: EPS(i),SIG(i)
   ! ecrouissage cinematique
   integer                     :: cine_hard     ! ecrouissage cinematique: 0 pas, 1
   real(kind=8),dimension(1)   :: cinh_coeff    ! le beta

   ! visco plasticite
   integer                     :: visco_plas    ! visco plasticite: 0 pas, 1
   real(kind=8),dimension(3)   :: vplas_coeff   ! contrainte_de_reference,taux_de_deformation_de_reference,exposant

   ! visco elasticite
   integer                     :: visco_elas    ! visco elasticite: 0 pas, 1 kelvin-voigt 
   real(kind=8),dimension(21)  :: velas_coeff   ! visco_elas = 1
                                                !   anisotropy
                                                !     iso:   coeff(1)=Young, coeff(2)=poisson   
                                                !     ortho: Young11, Young22, Young33
                                                !            Poisson12,Poisson13,Poisson23
                                                !            G12,G13,G23
                                                !     ani  : les 21 coeff

!todo comment definir le rep d'anisotropie ? 


   ! couplage thermique
   integer                     :: therm_cpl     ! 0: no 1: yes

   ! real(kind=8)                :: dilatation(3) ! pour l'orthotropie
   type(T_3r8_param)           :: dilatation ! vecteur pour l'orthotropie   
   
   real(kind=8)                :: T_ref_meca    ! eps_th = dilatation*(T-T_ref_meca) 


   ! parametres thermiques
   integer                     :: therm_effect  ! 0: no 1:yes
   type(T_r8_param)            :: specific_capacity
   type(T_r8_param)            :: conductivity_coefficient
   type(T_r8_param)            :: biot_coefficient
   type(T_r8_param)            :: external_flux_coefficient

   ! parametres thermiques supplementaires pour la formulation variationnelle
   integer                     :: therm_varia   ! 0: no 1:yes
   real(kind=8)                :: T_ref_ther
   real(kind=8)                :: T_ini_ther

   ! coupled mecanical electrical behaviour for rigid body 
   real(kind=8)                :: ECond
   real(kind=8)                :: Eeq,NUeq        ! equivalent young modulus and poisson ratio
   character(len=3)            :: Tmodel          ! thermal model (iso/ani) 
   !iso case:
   real(kind=8)                :: TCond,Hspe      ! thermal conductivity and specific heat
   !ani case:
   real(kind=8)                :: PTCond,STCond   ! Primal and secondary thermal conductivity
   real(kind=8)                :: Tnx,Tny         ! Normal direction of principal value
   !
   real(kind=8)                :: Tmelt,Tvar      ! Melting point and Temperature variation
   real(kind=8)                :: WS,WSvar,Eratio
   integer                     :: WSilaw

   real(kind=8)                :: WSmax,WSmin,ContTime,ActIner

   ! liste des fields ->  une grosse merde pour gagner du temps (a revoir) 
   ! il faudrait que ca soit allouable dynamiquement

   integer :: nb_fields=0
   character(len=30),dimension(5) :: field_name   ! Il existe COCO, SPHV, BIOT, EX_F, density
                                                  ! pour      Conductivity, Capacity, Biot, External Flux, masse volumique
   integer :: nb_vfields=0
   character(len=30),dimension(1) :: vfield_name   ! Il existe elas_coeff


   ! flag permettant de savoir si ce materiau a deja ete charge dans une bibli externe

   logical                     :: is_external

   ! user_mat gerer comme une liste de user_parameter

   logical                                 :: IS_A_USER_MAT
   character(LEN=50)                       :: USER_MAT_FILE_NAME
   ! type(T_USER_PARAM),dimension(:),pointer :: USER_MAT_PARAM


   ! behaviour of discrete FE
   real(kind=8),dimension(:),pointer :: discrete_stiffness,discrete_viscosity,discrete_mass

   ! joint (tout dans 1 vecteur)        
   integer                           :: joint_param_type
   real(kind=8),dimension(:),pointer :: joint_param
   
   
 end type T_BULK_BEHAV

 type(T_BULK_BEHAV), dimension(:),allocatable :: bulk_behav

!----------------------------------------------------------------------

 integer :: nb_bulk_behav=0

 integer,parameter :: i_ludinglaw = 1, i_thibautlaw = 2, i_renouflaw = 3, i_renouflaw2 = 4, i_nhulaw = 5

 logical :: itchatche = .false.

! --------------------------
! subroutines set to private
! --------------------------

! none

! -------------------------
! subroutines set to public
! -------------------------

! wrap API

 public read_in_bulk_behav,write_out_bulk_behav, &
        read_out_bulk_behav,clean_out_bulk_behav, &
        append_out_bulk_behav,write_in_bulk_behav, &
        set_nb_bulks,set_bulk, &
        get_gravity,set_gravity, &
        set_conductivity, &
        set_biot, &
        set_capacity, &
        set_external_flux, &
        set_solid_density

 public get_bulk_behav
        

! routine public a lmgc90

 public is_ELAS,is_ELAS_PLAS, get_elas_model,get_visco_elas_model,get_plas_critere, &
        get_bulk_behav_nb, get_rho, get_Tref_meca,&
        get_elas_coeff,get_plas_coeff, get_visco_elas_coeff, &
        indent_bulk_behav,get_nb_bulk_behav,get_bulk_behav_ID, &
        get_sphv_type, get_coco_type, get_biot_type,get_external_flux_type,&
        get_sphv, get_coco, get_biot, get_external_flux, & 
        get_sphv_field_name, get_coco_field_name, get_biot_field_name, get_external_flux_field_name, & 
        get_Tref_ther, get_Tini_ther,get_dilatation,&
        get_ECond,get_TCond,get_surface_energy,get_surface_energy_WS, &
        Initialize_WS, compute_WSvsT, compute_WSvsTime, &
        get_AniTCond, &
        get_Tmodel, &
        get_is_external , set_is_external, &
        get_therm_cpl,get_therm_effect,get_therm_varia,get_Hspe, &
        get_is_a_user_mat,get_user_mat_filename,& !get_user_mat_param_size,get_user_mat_param_fields, &
        get_equivalent_mat_prop, &
        get_bulk_field_nb, get_bulk_field_name, & 
        get_discrete_stiffness, &
        get_discrete_viscosity, &
        get_discrete_mass     , &
        get_joint_param     , &
        get_joint_param_size, &
        get_joint_param_type, &        
        clean_memory, &
        get_rho_type, &
        get_elas_coeff_type


contains

!!!------------------------------------------------------------------------
  subroutine read_in_bulk_behav
    
    implicit none
    logical :: exist

    ! new : one can feed the bulk_behav trough subroutines,
    ! reading the file is not mandatory

    inquire(file=trim(location(in_bulk_behav(:))),EXIST=exist)
    if (.not. exist) then
      call logmes('Warning: '//trim(location(in_bulk_behav(:)))//' missing')
      return
    endif

    G_nfich = get_io_unit()
    !print *,'trying to open unit : ', G_nfich, ' on filename ', trim(location(in_bulk_behav(:)))

    open(unit=G_nfich,file=trim(location(in_bulk_behav(:))))
    call read_behaviours
    close(G_nfich)
    
  end subroutine read_in_bulk_behav
!!!------------------------------------------------------------------------
  subroutine write_out_bulk_behav
    
    implicit none

    G_nfich = get_io_unit()
    open(unit=G_nfich,STATUS='REPLACE',file=trim(location(out_bulk_behav(:))))
    call write_behaviours
    close(G_nfich)
    
  end subroutine write_out_bulk_behav
!!!------------------------------------------------------------------------
  subroutine read_out_bulk_behav
    
    implicit none

    G_nfich = get_io_unit()
    open(unit=G_nfich,file=trim(location(out_bulk_behav(:))))
    call read_behaviours
    close(G_nfich)
    
  end subroutine read_out_bulk_behav
!!!------------------------------------------------------------------------
  subroutine clean_out_bulk_behav
    
    implicit none
    
    G_nfich = get_io_unit()
    open(unit=G_nfich,STATUS='REPLACE',file=trim(location(out_bulk_behav(:))))
    write(G_nfich,'(A1)')' '
    close(G_nfich)
    
  end subroutine clean_out_bulk_behav
!!!------------------------------------------------------------------------
  subroutine append_out_bulk_behav

    implicit none

    G_nfich = get_io_unit()
    open(unit=G_nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_bulk_behav(:))))
    call write_behaviours
    close(G_nfich)
    
  end subroutine append_out_bulk_behav
!!!------------------------------------------------------------------------
  subroutine write_in_bulk_behav

    implicit none

    G_nfich = get_io_unit()
    open(unit=G_nfich,STATUS='REPLACE',file=trim(location(in_bulk_behav(:))))
    call write_behaviours
    close(G_nfich)

  end subroutine write_in_bulk_behav
!!!------------------------------------------------------------------------
!!!
!!! PRIVATE PART OF THE MODULE BULK BEHAVIOURS 
!!! START IN/OUT
!!!----------------------------------------------------------------------------------------------
  subroutine read_behaviours

    implicit none   

    integer             :: ibehav,isee,errare
    !                           123456789012345678901234567
    character(len=27)   :: IAM='bulk_behav::read_behaviours'
    character(len=80)   :: cout

    type(T_BULK_BEHAV)  :: zero_mat   

    if (itchatche) call LOGMES('Entering : '//IAM)

    ! on genere un materiau initialise a 0


    zero_mat%umass%type         = 0
    zero_mat%umass%val          = 0.D0
    ! 
    zero_mat%elas_modele        = 0
    zero_mat%anisotropie        = 0
    zero_mat%elas_coeff%type    = 0
    zero_mat%elas_coeff%val     = 0.d0
    !
    zero_mat%critere = 0
    !
    zero_mat%iso_hard = 0
    zero_mat%isoh_coeff = 0.D0
    !
    zero_mat%cine_hard = 0
    zero_mat%cinh_coeff = 0.D0
    !
    zero_mat%visco_plas  = 0
    zero_mat%vplas_coeff = 0.D0
    !
    zero_mat%visco_elas  = 0
    zero_mat%velas_coeff = 0.D0
    !
    zero_mat%therm_cpl  = 0
    zero_mat%dilatation%type = 0
    zero_mat%dilatation%val = 0.D0    
    zero_mat%T_ref_meca = 0.D0
    !
    zero_mat%therm_effect                   = 0
    zero_mat%specific_capacity%type         = 0
    zero_mat%specific_capacity%val          = 0.D0
    zero_mat%conductivity_coefficient%type  = 0
    zero_mat%conductivity_coefficient%val   = 0.D0
    zero_mat%biot_coefficient%type          = 0
    zero_mat%biot_coefficient%val           = 0.D0
    zero_mat%external_flux_coefficient%type = 0
    zero_mat%external_flux_coefficient%val  = 0.D0
    !
    zero_mat%therm_varia = 0
    zero_mat%T_ref_ther  = 0.D0
    zero_mat%T_ini_ther  = 0.D0
    !
    zero_mat%ECond  = 0.D0

    zero_mat%Tmodel = 'xxx'
    zero_mat%TCond  = 0.D0
    zero_mat%PTCond = 0.D0
    zero_mat%STCond = 0.D0
    zero_mat%Tnx    = 0.D0
    zero_mat%Tny    = 0.D0
    zero_mat%Hspe   = 0.D0

    zero_mat%Tvar   = 0.D0
    zero_mat%TMelt  = 0.D0
    zero_mat%WS     = 0.D0
    zero_mat%WSvar  = 0.D0
    zero_mat%Eratio = 0.D0
    zero_mat%WSilaw = 0 
    !
    zero_mat%WSmax  = 0.D0
    zero_mat%WSmin  = 0.D0
    zero_mat%ContTime= 0.D0
    zero_mat%ActIner= 0.D0
    ! 
    zero_mat%is_external=.false.
    !
    zero_mat%is_a_user_mat=.false.
    zero_mat%user_mat_file_name=' '
    ! nullify(zero_mat%user_mat_param)

    nullify(zero_mat%discrete_stiffness, &
            zero_mat%discrete_viscosity, &
            zero_mat%discrete_mass)

    zero_mat%joint_param_type = 0
    nullify(zero_mat%joint_param)

    !
    !   
    ! reading gravity
    !
    errare=1
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'gravy') cycle                                     ! fishing for the keyword 'gravy' 
       if( .not. read_G_clin()) exit
       read(G_clin(25:38),'(D14.7)') grav1
       read(G_clin(46:59),'(D14.7)') grav2
       read(G_clin(67:80),'(D14.7)') grav3
       errare=0
       exit
    end do
    if (errare /= 0) then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')
       call FATERR(IAM,'error reading gravy')
    end if

    !
    ! reading behaviour laws
    !

    errare = 1

    ! first reading: sizing array of behaviour laws 
    rewind(G_nfich)
    ibehav = 0
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'behav') cycle                                     ! fishing for the keyword 'behav'
       ibehav = ibehav+1
       cycle
    end do

    write(cout,'(I0,1x,A)') ibehav,'bulk materials found'
    call LOGMES(cout)
    
    ! attention on suppose qu'on ne passe qu'une fois ici !!!
    nb_bulk_behav = ibehav

    if (ibehav == 0) then
       call LOGMES('WARNING: if you are not using an external modeling tool bulk material is missing !')  
       call LOGMES('--')

       return
    end if
    call LOGMES('--')

    IF(ALLOCATED(bulk_behav)) DEALLOCATE(bulk_behav)

    allocate(bulk_behav(ibehav),stat=errare)

    if (errare /= 0) then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')
       call FATERR(IAM,'error allocating laws')
    end if
        
    !initialisation
    !fd je ne sais pas si on peut initialiser un pointeur en lui copiant un pointeur nullify ... a voir

    do ibehav = 1,size(bulk_behav)
       bulk_behav(ibehav) = zero_mat
    enddo

    ! second reading: filling in data

    rewind(G_nfich)
    ibehav=0
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'behav') cycle                                     ! fishing for the keyword 'behav'
       ibehav = ibehav+1                    
       if( .not. read_G_clin()) exit
       read(G_clin(2: 6),'(A5) ') bulk_behav(ibehav)%behav
       read(G_clin(9:38),'(A30)') bulk_behav(ibehav)%lawty

       !PRINT*,bulk_behav(ibehav)%behav, bulk_behav(ibehav)%lawty

       select case(bulk_behav(ibehav)%lawty)
          !     123456789012345678901234567890
       case('MATERIAL_POINT                ', &
            'RIGID                         ')
          call read_UMASS(ibehav) 
       case('THERMO_RIGID                  ')
          call read_UMASS(ibehav)
          call read_RIGID_THERM(ibehav)
       case('THERMO_CHEMICAL_RIGID         ')
          call read_UMASS(ibehav)
          call read_RIGID_THERM(ibehav)
          call read_WS(ibehav)
      case('CHEMICAL_RIGID                 ')
          call read_UMASS(ibehav)
          call read_WS(ibehav)
       case('ELECTRO_RIGID                 ')
          call read_UMASS(ibehav)
          call read_RIGID_ELEC(ibehav)
          !CALL read_WS(ibehav)
       case('THERMO_ELECTRO_RIGID          ')
          call read_UMASS(ibehav)
          call read_RIGID_THERM(ibehav)
          call read_RIGID_ELEC(ibehav)
          call read_WS(ibehav)
       case('ELAS                          ')
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)
       case('ELAS_DILA                     ')                    
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)                 
          call read_CPLT(ibehav)
       case('VISCO_ELAS                    ')
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)                 
          call read_VISCO(ibehav)                 
       case('ELAS_PLAS                     ')
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)                 
          call read_PLAS(ibehav)                 
       case('THERMO_ELAS                   ')                    
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)                 
          call read_CPLT(ibehav)
          call read_THERM(ibehav)
       case('THERMO_ELAS_VARIA             ')                    
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)                 
          call read_CPLT(ibehav)
          call read_THERM(ibehav)
          call read_VARIA(ibehav)
       case('PORO_ELAS                      ')
          call read_UMASS(ibehav)       
          call read_ELAS(ibehav)
          call read_THERM(ibehav)
          call read_BIOT(ibehav)
       case('USER_MAT                      ')                    
          call read_UMASS(ibehav)       
          call read_UMAT(ibehav)         
       case('DISCRETE                      ')                    
          call read_discrete(ibehav)       
       case('JOINT_ELAS                    ')                    
          call read_joint(ibehav,0)       
       case('JOINT_MC                      ')                    
          call read_joint(ibehav,1)       
       case('JOINT_FCZM                    ')                    
          call read_joint(ibehav,2)       

       case default
          write(cout,'(A6,A30,A8)') 'lawty ',bulk_behav(ibehav)%lawty,' unknown'
          call LOGMES(cout)
          !                          123456789012345678901234567890
          bulk_behav(ibehav)%behav ='     '
          bulk_behav(ibehav)%lawty ='unknown                       '  
          
       end select
       cycle
    end do

    if (itchatche) call LOGMES('Leaving : '//IAM)

  end subroutine read_behaviours
!!!------------------------------------------------------------------------
  subroutine write_behaviours
 
    implicit none
    integer :: ibehav,isee
    !                           1234567890123456789012345678
    character(len=28)   :: IAM='bulk_behav::write_behaviours'
    character(len=80)   :: cout
   
    call write_comment(nfich=G_nfich)

    write(G_nfich,'(A6)') '$gravy'
    write(G_nfich,'(17X,3(2X,A5,D14.7))')'grv1=',grav1,'grv2=',grav2,'grv3=',grav3
    write(G_nfich,'(A1)')' '                                
   
    if (nb_bulk_behav == 0) return

    ! writing behaviour laws                     
    do ibehav=1,nb_bulk_behav

       !PRINT*,bulk_behav(ibehav)%lawty

       write(G_nfich,'(A13)')'$behav  lawty'
       select case(bulk_behav(ibehav)%lawty)
          !  123456789012345678901234567890
       case('MATERIAL_POINT                ')
          call write_UMASS(ibehav,nfich=G_nfich)
       case('RIGID                         ')
          call write_UMASS(ibehav,nfich=G_nfich)
       case('THERMO_RIGID                  ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_RIGID_THERM(ibehav,nfich=G_nfich)
       case('CHEMICAL_RIGID                ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_WS(ibehav,nfich=G_nfich)
       case('THERMO_CHEMICAL_RIGID         ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_RIGID_THERM(ibehav,nfich=G_nfich)
          call write_WS(ibehav,nfich=G_nfich)
       case('ELECTRO_RIGID                 ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_RIGID_ELEC(ibehav,nfich=G_nfich)
          !call write_WS(ibehav,nfich=G_nfich)
       case('THERMO_ELECTRO_RIGID          ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_RIGID_THERM(ibehav,nfich=G_nfich)
          call write_RIGID_ELEC(ibehav,nfich=G_nfich)
          call write_WS(ibehav,nfich=G_nfich)
       case('ELAS                          ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_ELAS(ibehav,nfich=G_nfich)
       case('ELAS_DILA                      ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_ELAS(ibehav,nfich=G_nfich)
          call write_CPLT(ibehav,nfich=G_nfich)
       case('VISCO_ELAS                    ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_ELAS(ibehav,nfich=G_nfich)
          call write_VISCO(ibehav,nfich=G_nfich)
       case('ELAS_PLAS                     ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_ELAS(ibehav,nfich=G_nfich)
          call write_PLAS(ibehav,nfich=G_nfich)
       case('THERMO_ELAS                   ')
          call write_UMASS(ibehav,nfich=G_nfich)       
          call write_ELAS(ibehav,nfich=G_nfich)
          call write_CPLT(ibehav,nfich=G_nfich)
          call write_THERM(ibehav,nfich=G_nfich)
       case('THERMO_ELAS_VARIA             ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_ELAS(ibehav,nfich=G_nfich)
          call write_CPLT(ibehav,nfich=G_nfich)
          call write_THERM(ibehav,nfich=G_nfich)
          call write_VARIA(ibehav,nfich=G_nfich)
       case('USER_MAT                      ')
          call write_UMASS(ibehav,nfich=G_nfich)
          call write_UMAT(ibehav,nfich=G_nfich)
       case('PORO_ELAS                     ')                    
          call write_UMASS(ibehav,nfich=G_nfich)       
          call write_ELAS(ibehav,nfich=G_nfich)
          call write_THERM(ibehav,nfich=G_nfich)
          call write_BIOT(ibehav,nfich=G_nfich)
       case('DISCRETE                      ')                    
          call write_discrete(ibehav,nfich=G_nfich)       
       case('JOINT_ELAS                    ')                    
          call write_joint(ibehav,0,nfich=G_nfich)       
       case('JOINT_MC                      ')                    
          call write_joint(ibehav,1,nfich=G_nfich)       
       case('JOINT_FCZM                    ')                    
          call write_joint(ibehav,2,nfich=G_nfich)       
       case('unknown                       ')
       case default
          write(cout,'(A6,A30)') 'lawty ',bulk_behav(ibehav)%lawty
          call FATERR(IAM,cout)
       end select

       write(G_nfich,'(A1)') ' '

    end do
    
  end subroutine write_behaviours
!!!------------------------------------------------------------------------
  subroutine read_UMASS(ibehav)
    !!****f* BULK_BEHAVIOUR/read_UMASS
    !! NAME
    !!  read_UMASS
    !! SYNOPSIS
    !!  read_UMASS(ibehav)
    !! INPUTS
    !!  ibehav :: behaviour identifiant
    !! PURPOSE
    !!  the command scan the file BULK_BEHAV.DAT and recup the value of the density
    !!  of bodies. The command jump to column 39 and read 7 characters and then the 
    !!  density value (D14.7 format)
    !!****
    implicit none   

    integer :: ibehav
    !                         1234567890123456789012
    character(len=22) :: IAM='behaviours::read_UMASS' 
    character(len=80) :: cout

    if (G_clin(46:50) /= 'field') then
      bulk_behav(ibehav)%Umass%type=0       
      read(G_clin(39:59),'(7X,D14.7)',err=10) bulk_behav(ibehav)%Umass%val
    else
      bulk_behav(ibehav)%Umass%type=1
      bulk_behav(ibehav)%Umass%field='density'
      bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
      bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='density'
    endif  
    return   
    
10  write(cout,'(A,A)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
 
  end subroutine read_UMASS
!!!------------------------------------------------------------------------
  subroutine write_UMASS(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich

    if (bulk_behav(ibehav)%Umass%type==0)then
    
      write(nfich,101) &
           bulk_behav(ibehav)%behav, &
           bulk_behav(ibehav)%lawty, &
           'Umas=', bulk_behav(ibehav)%Umass%val
    else      
      write(nfich,102) &
           bulk_behav(ibehav)%behav, &
           bulk_behav(ibehav)%lawty, &
           'Umas=','field'
    endif
   
101 format(1X,A5,2X,A30,2X,A5,D14.7)
102 format(1X,A5,2X,A30,2X,A5,A5)    
          
  end subroutine write_UMASS
!!!------------------------------------------------------------------------
  subroutine read_RIGID_ELEC(ibehav)

    implicit none   

    integer :: ibehav
    !                         123456789012345678901234567
    character(len=27) :: IAM='behaviours::read_RIGID_ELEC' 
    character(len=80) :: cout
    
    if( .not. read_G_clin()) goto 10
    read(G_clin(39:59),'(7X,D14.7)',err = 10) bulk_behav(ibehav)%ECond

    if( .not. read_G_clin()) goto 10
    read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%Eeq,bulk_behav(ibehav)%NUeq

    return   
    
10  write(cout,'(A,A)') 'reading error in law ',bulk_behav(ibehav)%behav

    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
  
  end subroutine read_RIGID_ELEC
!!!------------------------------------------------------------------------
  subroutine write_RIGID_ELEC(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich
    
    write(nfich,102) 'ECnd=',bulk_behav(ibehav)%ECond
    write(nfich,103) 'Eel_=',bulk_behav(ibehav)%Eeq  ,'NUel=',bulk_behav(ibehav)%NUeq

103 format(38X,2X,A5,D14.7,2X,A5,D14.7)
102 format(38X,2X,A5,D14.7)
   
  end subroutine write_RIGID_ELEC
!!!------------------------------------------------------------------------
  subroutine read_RIGID_THERM(ibehav)

    implicit none   

    integer :: ibehav
    !                         1234567890123456789012345678
    character(len=28) :: IAM='behaviours::read_RIGID_THERM'
    character(len=80) :: cout

    if( .not. read_G_clin()) goto 10
    read(G_clin(41:43),'(A3)',err = 10) bulk_behav(ibehav)%Tmodel

    select case(bulk_behav(ibehav)%Tmodel)
    case('iso')
       if( .not. read_G_clin()) goto 10
       !fd read(G_clin(39:59),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%TCond
       read(G_clin(39:59),'(1(7X,D14.7))',err = 10) bulk_behav(ibehav)%TCond       
    case('ani')
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%PTCond,bulk_behav(ibehav)%STCond
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%Tnx,bulk_behav(ibehav)%Tny
    case default
       call faterr(IAM,'undefined model '//bulk_behav(ibehav)%Tmodel)
    end select

    if( .not. read_G_clin()) goto 10
    !fd read(G_clin(39:59),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%Hspe
    read(G_clin(39:59),'(1(7X,D14.7))',err = 10) bulk_behav(ibehav)%Hspe    

    if( .not. read_G_clin()) goto 10
    read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%Eeq,bulk_behav(ibehav)%NUeq

    return   

10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav

    call LOGMES('check DATBOX/BULK_BEHAV.DAT',.true.)
    call FATERR(IAM,cout)
  
  end subroutine read_RIGID_THERM
!!!------------------------------------------------------------------------
  subroutine write_RIGID_THERM(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich

    !fd write(nfich,'(38X,A3,A1)') bulk_behav(ibehav)%Tmodel,':'
    write(nfich,'(40X,A3,A1)') bulk_behav(ibehav)%Tmodel,':'    
    select case(bulk_behav(ibehav)%Tmodel)
    case('iso')
       write(nfich,103) 'TCnd=',bulk_behav(ibehav)%TCond
    case('ani')
       write(nfich,102) 'PrTC=',bulk_behav(ibehav)%PTCond,'ScTC=',bulk_behav(ibehav)%STCond
       write(nfich,102) 'PDnx=',bulk_behav(ibehav)%Tnx,   'PDny=',bulk_behav(ibehav)%Tny
    end select

    write(nfich,103) 'Hspe=',bulk_behav(ibehav)%Hspe
    write(nfich,102) 'Eeq_=',bulk_behav(ibehav)%Eeq  ,'NUeq=',bulk_behav(ibehav)%NUeq

102 format(38X,2X,A5,D14.7,2X,A5,D14.7)
103 format(38X,2X,A5,D14.7)
    
  end subroutine write_RIGID_THERM
!!!------------------------------------------------------------------------
  subroutine read_WS(ibehav)

    implicit none   

    integer          :: ibehav
    character(len=5) :: WSlaw
    !                         1234567890123456789
    character(len=19) :: IAM='behaviours::read_WS' 
    character(len=80) :: cout

    if( .not. read_G_clin()) goto 10
    read(G_clin(47:51),'(A5)',err = 10) WSlaw 

    select case(WSlaw)
    case('LMM05') ! Luding Manestberger Mullers 2005
       bulk_behav(ibehav)%WSilaw = i_ludinglaw
       !PRINT*,'HERE HERE'

       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%WS,bulk_behav(ibehav)%WSvar
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%TMelt,bulk_behav(ibehav)%TVar

    case('REN12') 
       bulk_behav(ibehav)%WSilaw = i_renouflaw

       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%WSmax,bulk_behav(ibehav)%WSmin
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(1(7X,D14.7))',err = 10) bulk_behav(ibehav)%ContTime
    !vhn  
    case('REN13') 
       bulk_behav(ibehav)%WSilaw = i_renouflaw2

       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%WSmax,bulk_behav(ibehav)%WSmin
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(1(7X,D14.7))',err = 10) bulk_behav(ibehav)%ContTime

    case('NHU14') 
       bulk_behav(ibehav)%WSilaw = i_nhulaw

       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%WSmax,bulk_behav(ibehav)%WSmin
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%ContTime,bulk_behav(ibehav)%ActIner
    
    case DEFAULT
       print*,G_clin
       print*,G_clin(1:5),G_clin(57:61)
       call faterr(IAM,'Model not defined in CHEMICAL RIGID BEHAV')
    end select

    return   
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav

    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
  
  end subroutine read_WS
!!!------------------------------------------------------------------------
  subroutine write_WS(ibehav,nfich)

    implicit none
    integer          :: ibehav,nfich
    character(len=5) :: WSlaw

    WSlaw = '_____'
    !PRINT*,ibehav,bulk_behav(ibehav)%WSilaw
    select case(bulk_behav(ibehav)%WSilaw)
    case(i_ludinglaw)
       WSlaw = 'LMM05'
       write(nfich,103) 'WSlw=',WSlaw
       write(nfich,102) 'WS__=',bulk_behav(ibehav)%WS,   'WSvr=',bulk_behav(ibehav)%WSvar
       write(nfich,102) 'TMlt=',bulk_behav(ibehav)%TMelt,'Tvar=',bulk_behav(ibehav)%Tvar
       write(nfich,'(A1)')' '
    case(i_renouflaw)
       WSlaw = 'REN12'
       write(nfich,103) 'WSlw=',WSlaw
       write(nfich,102) 'WSmx=',bulk_behav(ibehav)%WSmax,   'WSmn=',bulk_behav(ibehav)%WSmin
       write(nfich,102) 'CtTi=',bulk_behav(ibehav)%ContTime
       write(nfich,'(A1)')' '
    case(i_renouflaw2)
       WSlaw = 'REN13'
       write(nfich,103) 'WSlw=',WSlaw
       write(nfich,102) 'WSmx=',bulk_behav(ibehav)%WSmax,   'WSmn=',bulk_behav(ibehav)%WSmin
       write(nfich,102) 'CtTi=',bulk_behav(ibehav)%ContTime
       write(nfich,'(A1)')' '
    case(i_nhulaw)
       WSlaw = 'NHU14'
       write(nfich,103) 'WSlw=',WSlaw
       write(nfich,102) 'WSmx=',bulk_behav(ibehav)%WSmax,   'WSmn=',bulk_behav(ibehav)%WSmin
       write(nfich,102) 'CtTi=',bulk_behav(ibehav)%ContTime,'AcTi=',bulk_behav(ibehav)%ActIner
       write(nfich,'(A1)')' '
    case DEFAULT
       call LOGMES('Model not defined in THERMO RIGID BEHAV')
!       STOP
    end select

102 format(38X,2X,A5,D14.7,2X,A5,D14.7)
103 format(38X,2X,A5,1X,A5)
104 format(38X,2X,A5,D14.7)

  end subroutine write_WS
!!!------------------------------------------------------------------------
  subroutine read_ELAS(ibehav)

    implicit none   
    integer :: i,ibehav
    !                         123456789012345678901
    character(len=21) :: IAM='behaviours::read_ELAS' 
    character(len=80) :: cout
    character(len=5)  :: option
    character(len=13) :: modele,anisotropie

    !  lecture du type d elasticite
  
    if( .not. read_G_clin()) goto 10
    read(G_clin(41:45),'(A5)') option
    if (option /= 'elas:') then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'You should specify the elas option (standard | MooneyRivlin | HartSmith)')
    end if
    read(G_clin(47:59),'(A13)') modele

    select case (modele)
       !        1234567890123
    case('standard     ')
       bulk_behav(ibehav)%elas_modele=1
    case('MooneyRivlin ')
       bulk_behav(ibehav)%elas_modele=2
    case('HartSmith    ')
       bulk_behav(ibehav)%elas_modele=3
    case('Signorini    ')
       bulk_behav(ibehav)%elas_modele=4
    case default
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'The elas option is no yet supported')
    end select

    if( .not. read_G_clin()) goto 10
    read(G_clin(41:45),'(A5)') option
    if (option /= 'ani_:') then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'You should specify the ani_ option (isotropic | orthotropic | anisotropic)')
    end if
    read(G_clin(47:59),'(A13)') anisotropie

    select case (bulk_behav(ibehav)%elas_modele)
    case(1) ! lecture standard
      select case (anisotropie)
         !        1234567890123
      case('isotropic    ')
         bulk_behav(ibehav)%anisotropie=0
         if( .not. read_G_clin()) goto 10
         if (G_clin(46:50) /= 'field') then
           bulk_behav(ibehav)%elas_coeff%type=0 
           read(G_clin(39:80),'(2(7X,D14.7))',err=10) bulk_behav(ibehav)%elas_coeff%val(1:2)
         else
           bulk_behav(ibehav)%elas_coeff%type=1
           bulk_behav(ibehav)%elas_coeff%vfield='elas_coeff'
           bulk_behav(ibehav)%elas_coeff%vsize=2
           bulk_behav(ibehav)%nb_vfields = bulk_behav(ibehav)%nb_vfields + 1 
           bulk_behav(ibehav)%vfield_name(bulk_behav(ibehav)%nb_vfields)='elas_coeff'
         endif           
      case('orthotropic  ')
         ! fd voir comment tester cas field     
         bulk_behav(ibehav)%anisotropie=1
         if( .not. read_G_clin()) goto 10
         bulk_behav(ibehav)%elas_coeff%type=0          
         read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%elas_coeff%val(1:3)
         if( .not. read_G_clin()) goto 10
         read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%elas_coeff%val(4:6)
         if( .not. read_G_clin()) goto 10
         read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%elas_coeff%val(7:9)
      case('anisotropic  ')
         ! fd voir comment tester cas field     
         bulk_behav(ibehav)%anisotropie=2
         do i=1,7
            if( .not. read_G_clin()) goto 10
            if (i==1) bulk_behav(ibehav)%elas_coeff%type=0                             
            read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%elas_coeff%val((i-1)*3+1:(i-1)*3+3)
         end do
      case default
         write(cout,'(A10,A13,A8)') 'parameter ',anisotropie,' unknown'
         call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
         call FATERR(IAM,cout)
      end select
    case(2,3,4)
      call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
      call FATERR(IAM,'Not implemented yet')
    case default
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'You specify the type of elasticity parameter')
    end select
    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
    
  end subroutine read_ELAS
!!!------------------------------------------------------------------------
  subroutine write_ELAS(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich,i
    !                         1234567890123456789012
    character(len=22) :: IAM='behaviours::write_ELAS' 
    character(len=80) :: cout

    select case (bulk_behav(ibehav)%elas_modele)
    case(1)
       write(nfich,'(38X,2X,A5,1x,A13)') 'elas:','standard     '   
    case default
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'unknown or unsupported elastic material')
    end select

    select case (bulk_behav(ibehav)%anisotropie)
    case(0)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'ani_:',' isotropic    '
       if (bulk_behav(ibehav)%elas_coeff%type == 0) then 
         write(nfich,102) 'EYng=',bulk_behav(ibehav)%elas_coeff%val(1),'EPss=',bulk_behav(ibehav)%elas_coeff%val(2)
       else
         write(nfich,104) 'EYng=','field','EPss=','field'
       endif   
    case(1)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'ani_:',' orthotropic  '
       write(nfich,103) 'EY11=',bulk_behav(ibehav)%elas_coeff%val(1), &
                        'EY22=',bulk_behav(ibehav)%elas_coeff%val(2), &
                        'EY33=',bulk_behav(ibehav)%elas_coeff%val(3)
       write(nfich,103) 'EP12=',bulk_behav(ibehav)%elas_coeff%val(4), &
                        'EP13=',bulk_behav(ibehav)%elas_coeff%val(5), &
                        'EP23=',bulk_behav(ibehav)%elas_coeff%val(6)
       write(nfich,103) 'G12_=',bulk_behav(ibehav)%elas_coeff%val(7), &
                        'G13_=',bulk_behav(ibehav)%elas_coeff%val(8), &
                        'G23_=',bulk_behav(ibehav)%elas_coeff%val(9)
    case(2)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'ani_:',' anisotropic  '
       do i=1,7
          write(nfich,103) '     ',bulk_behav(ibehav)%elas_coeff%val((i-1)*3+1),&
                           '     ',bulk_behav(ibehav)%elas_coeff%val((i-1)*3+2),&
                           '     ',bulk_behav(ibehav)%elas_coeff%val((i-1)*3+3)
       end do
    case default
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%anisotropie,' unknown'
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    write(nfich,'(A1)')' '
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
102 format(38X,2(2X,A5,D14.7))
103 format(38X,3(2X,A5,D14.7))
104 format(38X,2(2X,A5,A5))    
  end subroutine write_ELAS
!!!------------------------------------------------------------------------
  subroutine read_CPLT(ibehav)

    implicit none   

    integer :: ibehav
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::read_CPLT' 
    character(len=80) :: cout
    
    character(len=4)  :: bbtype
    character(len=13) :: anisotropie
    integer           :: i
    
    if( .not. read_G_clin()) goto 10
    
    read(G_clin(41:44),'(A4)') bbtype
    
    if (bbtype /= 'cplt') then
       call faterr(IAM,'BULK_BEHAV.DAT unreadable')
    endif
    
    bulk_behav(ibehav)%therm_cpl=1
    
    select case (bulk_behav(ibehav)%anisotropie)
    case(0) !'isotropic    '
      if( .not. read_G_clin()) goto 10
      if (G_clin(46:50) /= 'field') then
        bulk_behav(ibehav)%dilatation%type=0                
        read(G_clin(39:59),'((7X,D14.7))',err=10) bulk_behav(ibehav)%dilatation%val(1)
      else
        bulk_behav(ibehav)%dilatation%type=1
        bulk_behav(ibehav)%dilatation%field='dilation'
        bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
        bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='dilation'
      endif  
    case(1) !'orthotropic  '
      if( .not. read_G_clin()) goto 10
      read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%dilatation%val(1:3)
!    CASE(2) !'anisotropic  '
!      CALL LOGMES('check DATBOX/BULK_BEHAV.DAT')       
!      STOP
    case default
      write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%anisotropie,' unknown'
      call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
      call FATERR(IAM,cout)
    end select

    if( .not. read_G_clin()) goto 10
    read(G_clin(39:80),'(1(7X,D14.7))',err=10) bulk_behav(ibehav)%T_ref_meca
    
    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
    
  end subroutine read_CPLT
!!!------------------------------------------------------------------------
  subroutine write_CPLT(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich,i
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::write_CPLT' 
    character(len=80) :: cout
    
    if (bulk_behav(ibehav)%therm_cpl /=1) return
    
    write(nfich,'(38X,2X,A5)') 'cplt:'
    select case (bulk_behav(ibehav)%anisotropie)
    case(0)                                  
       if (bulk_behav(ibehav)%dilatation%type==0)then
         write(nfich,101) 'Dila=',bulk_behav(ibehav)%dilatation%val(1)
       else
         write(nfich,102) 'Dila=','field'
       endif
    case(1)    
      write(nfich,103) 'Dil1=',bulk_behav(ibehav)%dilatation%val(1), &
                       'Dil2=',bulk_behav(ibehav)%dilatation%val(2), &
                       'Dil3=',bulk_behav(ibehav)%dilatation%val(3)
    case DEFAULT
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%anisotropie,' unknown'
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    write(nfich,101)'Tref=',bulk_behav(ibehav)%T_ref_meca
    write(nfich,'(A1)')' '
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
101 format(38X,1(2X,A5,D14.7))
102 format(38X,1(2X,A5,A5))    
103 format(38X,3(2X,A5,D14.7))
    
  end subroutine write_CPLT
!!!------------------------------------------------------------------------
  subroutine read_THERM(ibehav)

    implicit none   

    integer :: ibehav
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::read_THERM' 
    character(len=80) :: cout
    
    character(len=4)  :: bbtype
    character(len=13) :: anisotropie
    integer           :: i
    
    if( .not. read_G_clin()) goto 10
    
    read(G_clin(41:44),'(A4)') bbtype
    
    if (bbtype /= 'ther') then
       call faterr(IAM,'BULK_BEHAV.DAT unreadable')
    endif
    
    bulk_behav(ibehav)%therm_effect = 1
    
    if( .not. read_G_clin()) goto 10
    if (G_clin(46:50) /= 'field') then
      bulk_behav(ibehav)%specific_capacity%type=0
      read(G_clin(46:59),'(D14.7)',err=10) bulk_behav(ibehav)%specific_capacity%val
    else
      bulk_behav(ibehav)%specific_capacity%type=1
      bulk_behav(ibehav)%specific_capacity%field='SPHV'
      bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
      bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='SPHV'
    endif

    if( .not. read_G_clin()) goto 10
    if (G_clin(46:50) /= 'field') then
      bulk_behav(ibehav)%conductivity_coefficient%type=0
      read(G_clin(46:59),'(D14.7)',err=10) bulk_behav(ibehav)%conductivity_coefficient%val
    else
      bulk_behav(ibehav)%conductivity_coefficient%type=1
      bulk_behav(ibehav)%conductivity_coefficient%field='COCO'
      bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 

      bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='COCO'
    endif

    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
  
  end subroutine read_THERM
!!!------------------------------------------------------------------------
  subroutine write_THERM(ibehav,nfich)
    
    implicit none
    integer :: ibehav,nfich,i
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::write_ELAS' 
    character(len=80) :: cout
    
    if (bulk_behav(ibehav)%therm_effect /= 1) return
    
    write(nfich,'(38X,2X,A5)') 'ther:'

    if (bulk_behav(ibehav)%specific_capacity%type==0)then
       write(nfich,101) 'SPHV=',bulk_behav(ibehav)%specific_capacity%val
    else
       write(nfich,104) 'SPHV=','field'
    endif

    if (bulk_behav(ibehav)%conductivity_coefficient%type==0)then
       write(nfich,101) 'COCO=',bulk_behav(ibehav)%conductivity_coefficient%val
    else
       write(nfich,104) 'COCO=','field' 
    endif
    
    write(nfich,'(A1)')' '
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
101 format(38X,1(2X,A5,D14.7))
102 format(38X,2(2X,A5,D14.7))
103 format(38X,3(2X,A5,D14.7))
104 format(38X,2X,A5,A5)
    
  end subroutine write_THERM
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  subroutine read_VARIA(ibehav)
    
    implicit none   

    integer :: ibehav
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::read_VARIA' 
    character(len=80) :: cout
    
    character(len=4)  :: bbtype
    character(len=13) :: anisotropie
    integer           :: i
    
    if( .not. read_G_clin()) goto 10
    
    read(G_clin(41:44),'(A4)') bbtype
    
    if (bbtype /= 'varf') then
       call faterr(IAM,'BULK_BEHAV.DAT unreadable')
    endif
    
    bulk_behav(ibehav)%therm_varia = 1
    
    if( .not. read_G_clin()) goto 10
    read(G_clin(39:80),'(2(7X,D14.7))',err=10) bulk_behav(ibehav)%T_ref_ther,bulk_behav(ibehav)%T_ini_ther
    
    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    !123456789012345678901
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
    
  end subroutine read_VARIA
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  subroutine write_VARIA(ibehav,nfich)
    
    implicit none
    integer :: ibehav,nfich,i
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::write_ELAS' 
    character(len=80) :: cout
    
    if (bulk_behav(ibehav)%therm_varia /= 1) return
    
    write(nfich,'(38X,2X,A5)') 'varf:'
    write(nfich,102) 'Trth=',bulk_behav(ibehav)%T_ref_ther,'Tith=',bulk_behav(ibehav)%T_ini_ther
    
    write(nfich,'(A1)')' '
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
101 format(38X,1(2X,A5,D14.7))
102 format(38X,2(2X,A5,D14.7))
103 format(38X,3(2X,A5,D14.7))
    
  end subroutine write_VARIA
!!!------------------------------------------------------------------------
  subroutine read_PLAS(ibehav)
    
    implicit none   

    integer :: ibehav
    !                            123456789012345678901
    character(len=21) :: IAM='behaviours::read_PLAS' 
    character(len=80) :: cout
    
    character(len=13) :: chaine
    integer           :: i
    
    !
    ! le critère de plasticite
    !
    if( .not. read_G_clin()) goto 10
    read(G_clin(47:59),'(A13)') chaine
    select case (chaine)
       !          1234567890123
    case('none         ')
       bulk_behav(ibehav)%critere=0
       return
    case('Von-Mises    ')
       bulk_behav(ibehav)%critere=1
       
       
       !     case('Hill         ')
       !       bulk_behav(ibehav)%critere=2
       !
       ! prévoir la lecture des coefficients !!
       !
       
    case default
       write(cout,'(A10,A13,A8)') 'parameter ',chaine,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    !
    ! écrouissage isotrope
    !
    if( .not. read_G_clin()) goto 10
    read(G_clin(47:59),'(A13)') chaine
    select case (chaine)
       !          1234567890123
    case('none         ')
       bulk_behav(ibehav)%iso_hard=0
    case('Swift        ')
       bulk_behav(ibehav)%iso_hard=1
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%isoh_coeff(1:3)
    case('linear       ')
       bulk_behav(ibehav)%iso_hard=2
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err=10) bulk_behav(ibehav)%isoh_coeff(1:2)
    case('Hollomon     ')
       bulk_behav(ibehav)%iso_hard=3
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err=10) bulk_behav(ibehav)%isoh_coeff(1:2)
       !
       ! prévoir la lecture des coefficients !!
       !
    case default
       write(cout,'(A10,A13,A8)') 'parameter ',chaine,' unknown'
       !1234567890        12345678
       call LOGMES('You should specify the isoh option (none|Swift|linear|Hollomon)')       
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    !
    ! écrouissage cinematique
    !
    if( .not. read_G_clin()) goto 10
    read(G_clin(47:59),'(A13)') chaine
    select case (chaine)
       !          1234567890123
    case('none         ')
       bulk_behav(ibehav)%cine_hard=0
    case('linear         ')
       bulk_behav(ibehav)%cine_hard=1
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:59),'(7X,D14.7)',err=10) bulk_behav(ibehav)%cinh_coeff(1)
    case default
       write(cout,'(A10,A13,A8)') 'parameter ',chaine,' unknown'
       !1234567890        12345678
       call LOGMES('You should specify the cinh option (none|linear)')       
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    !
    ! visco plasticite
    !
    if( .not. read_G_clin()) goto 10
    read(G_clin(47:59),'(A13)') chaine
    select case (chaine)
       !          1234567890123
    case('none         ')
       bulk_behav(ibehav)%visco_plas=0
    case('power_law    ')
       bulk_behav(ibehav)%visco_plas=1
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%vplas_coeff(1:3)
    case default
       write(cout,'(A10,A13,A8)') 'parameter ',chaine,' unknown'
       !1234567890        12345678
       call LOGMES('You should specify the cinh option (none|linear)')       
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
    
  end subroutine read_PLAS
!!!------------------------------------------------------------------------
  subroutine write_PLAS(ibehav,nfich)

    implicit none

    integer :: ibehav,nfich,i
    character(len=27) :: IAM='behaviours::write_ELAS_PLAS' 
    character(len=80) :: cout
    
    select case (bulk_behav(ibehav)%critere)
       
    case(0)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'crit:',' none         '
       write(nfich,'(A1)')' '
       return
    case(1)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'crit:',' Von-Mises    '
    case default
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%critere,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    select case (bulk_behav(ibehav)%iso_hard)
    case(0)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'isoh:',' none         '
    case(1)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'isoh:',' Swift        '
       write(nfich,103) 'C0__=',bulk_behav(ibehav)%isoh_coeff(1),'EPS0=',bulk_behav(ibehav)%isoh_coeff(2),&
            'n___=',bulk_behav(ibehav)%isoh_coeff(3)
    case(2)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'isoh:',' linear       '
       write(nfich,102) 'SIG0=',bulk_behav(ibehav)%isoh_coeff(1),'K___=',bulk_behav(ibehav)%isoh_coeff(2)
    case(3)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'isoh:',' Hollomon     '
       write(nfich,102) 'K___=',bulk_behav(ibehav)%isoh_coeff(1),'n___=',bulk_behav(ibehav)%isoh_coeff(2)
    case default
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%iso_hard,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    select case (bulk_behav(ibehav)%cine_hard)
    case(0)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'cinh:',' none         '
    case(1)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'cinh:',' linear       '
       write(nfich,101) 'K___=',bulk_behav(ibehav)%cinh_coeff(1)
    case default
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%cine_hard,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    select case (bulk_behav(ibehav)%visco_plas)
    case(0)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'vpla:',' none         '
    case(1)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'vpla:',' power_law    '
       write(nfich,103) 'Sref=',bulk_behav(ibehav)%vplas_coeff(1),'Dref=',bulk_behav(ibehav)%vplas_coeff(2),&
            'n___=',bulk_behav(ibehav)%vplas_coeff(3)
    case default
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%cine_hard,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    write(nfich,'(A1)')' '
    return
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
101 format(38X,1(2X,A5,D14.7))
102 format(38X,2(2X,A5,D14.7))
103 format(38X,3(2X,A5,D14.7))
    
  end subroutine write_PLAS
!!!------------------------------------------------------------------------
  subroutine read_VISCO(ibehav)
    
    implicit none   
    integer :: ibehav
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::read_VISCO' 
    character(len=80) :: cout
    
    character(len=5)  :: option
    
    character(len=13) :: modele
    character(len=13) :: anisotropy
    integer           :: i
    
    !  lecture du type d elasticite
    
    if( .not. read_G_clin()) goto 10
    read(G_clin(41:45),'(A5)') option
    if (option /= 'visc:') then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'You should specify the visc option (none|KelvinVoigt)')
    endif
    read(G_clin(47:59),'(A13)') modele
    
    select case (modele)
       !        1234567890123
    case('none         ')
       bulk_behav(ibehav)%visco_elas=0
    case('KelvinVoigt  ')
       bulk_behav(ibehav)%visco_elas=1
    case default
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'The value of the visc option is no yet supported')
    end select
    
    if( .not. read_G_clin()) goto 10
    read(G_clin(41:45),'(A5)') option
    if (option /= 'ani_:') then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'You should specify the ani_ option (isotropic | orthotropic | anisotropic)')
    endif
    read(G_clin(47:59),'(A13)') anisotropy
    
    select case (anisotropy)
       !        1234567890123
    case('isotropic    ')
       bulk_behav(ibehav)%anisotropie=0
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:80),'(2(7X,D14.7))',err=10) bulk_behav(ibehav)%velas_coeff(1:2)
    case('orthotropic  ')
       bulk_behav(ibehav)%anisotropie=1
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%velas_coeff(1:3)
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%velas_coeff(4:6)
       if( .not. read_G_clin()) goto 10
       read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%velas_coeff(7:9)
    case('anisotropic  ')
       bulk_behav(ibehav)%anisotropie=2
       do i=1,7
          if( .not. read_G_clin()) goto 10
          read(G_clin(39:101),'(3(7X,D14.7))',err=10) bulk_behav(ibehav)%velas_coeff((i-1)*3+1:(i-1)*3+3)
       end do
    case default
       write(cout,'(A10,A13,A8)') 'parameter ',anisotropy,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    !123456789012345678901
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
    
  end subroutine read_VISCO
!!!------------------------------------------------------------------------
  subroutine write_VISCO(ibehav,nfich)
    
    implicit none
    integer :: ibehav,nfich,i
    !                            12345678901234567890123
    character(len=23) :: IAM='behaviours::write_VISCO' 
    character(len=80) :: cout
    
    select case (bulk_behav(ibehav)%visco_elas)
    case(0)
       !1234567890123
       write(nfich,'(38X,2X,A5,1x,A13)') 'visc:','none         '   
       
    case(1)
       !1234567890123
       write(nfich,'(38X,2X,A5,1x,A13)') 'visc:','KelvinVoigt  '   
    case default
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,'unknown or unsupported elastic material')
    end select
    
    select case (bulk_behav(ibehav)%anisotropie)
       !          12345678901234
    case(0)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'ani_:',' isotropic    '
       write(nfich,102) 'EYng=',bulk_behav(ibehav)%velas_coeff(1),'EPss=',bulk_behav(ibehav)%velas_coeff(2)
    case(1)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'ani_:',' orthotropic  '
       write(nfich,103) 'EY11=',bulk_behav(ibehav)%velas_coeff(1),'EY22=',bulk_behav(ibehav)%velas_coeff(2),&
            'EY33=',bulk_behav(ibehav)%velas_coeff(3)
       write(nfich,103) 'EP12=',bulk_behav(ibehav)%velas_coeff(4),'EP13=',bulk_behav(ibehav)%velas_coeff(5),&
            'EP23=',bulk_behav(ibehav)%velas_coeff(6)
       write(nfich,103) 'G12_=',bulk_behav(ibehav)%velas_coeff(7),'G13_=',bulk_behav(ibehav)%velas_coeff(8),&
            'G23_=',bulk_behav(ibehav)%velas_coeff(9)
    case(2)                                  !12345678901234
       write(nfich,'(38X,2X,A5,A14)') 'ani_:',' anisotropic  '
       do i=1,7
          write(nfich,103) '     ',bulk_behav(ibehav)%velas_coeff((i-1)*3+1),&
               '     ',bulk_behav(ibehav)%velas_coeff((i-1)*3+2),&
               '     ',bulk_behav(ibehav)%velas_coeff((i-1)*3+3)
       end do
    case default
       write(cout,'(A10,I5,A8)') 'parameter ',bulk_behav(ibehav)%anisotropie,' unknown'
       !1234567890        12345678
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
       call FATERR(IAM,cout)
    end select
    
    write(nfich,'(A1)')' '
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
102 format(38X,2(2X,A5,D14.7))
103 format(38X,3(2X,A5,D14.7))
    
  end subroutine write_VISCO
!!!------------------------------------------------------------------------
  subroutine read_UMAT(ibehav)
    !!****f* BULK_BEHAVIOUR/read_UMAT
    !! NAME
    !!  read_UMAT
    !! SYNOPSIS
    !!  read_UMAT(ibehav)
    !! INPUTS
    !!  ibehav :: behaviour identifiant
    !! PURPOSE
    !!  
    !!****
    implicit none   

    integer :: ibehav
    !                         1234567890123456789012
    character(len=21) :: IAM='behaviours::read_UMAT' 
    character(len=80) :: cout

    integer :: i,io_unit
    logical :: test=.false.
    character(len=103) :: cin,file_name
    character(len=50)  :: cvalue1,cvalue2,cvalue3,cvalue4

    if( .not. read_G_clin()) goto 20
    
    bulk_behav(ibehav)%IS_A_USER_MAT=.true.

    read(G_clin,*) file_name
    bulk_behav(ibehav)%USER_MAT_file_name = trim(file_name)


    print *,'Trying to read user material file: ', &
           trim(location('DATBOX/'//trim(file_name)))

    inquire(FILE=trim(location('DATBOX/'//trim(file_name))),EXIST=test)

    if (.not. test) then
      call LOGMES('check DATBOX/BULK_BEHAV.DAT')       
      call FATERR(IAM,'The UMAT file doesn''t exist !?')
    endif

!     io_unit=get_io_unit()
!     open(UNIT=io_unit,FILE=trim(location('DATBOX/'//trim(file_name))))

!     i=0
!     do
!       read(io_unit,'(A103)',end=10) cin 
!       if (cin(1:1) == '!' .or. cin(1:1) == '#' .or. cin(1:1) == ' ') cycle 
!       i=i+1
!       cycle      
!  10   test=.false.
!       exit
!     end do   

!     allocate(bulk_behav(ibehav)%USER_MAT_PARAM(i))

!     rewind(io_unit)

!     i=0
!     do    
!       read(io_unit,'(A103)',end=11) cin 
!       if (cin(1:1) == '!' .or. cin(1:1) == '#' .or. cin(1:1) == ' ') cycle 
!       i=i+1
!       cvalue1=' ';cvalue2=' ';cvalue3=' ';cvalue4=' '
!       read(cin,*) cvalue1,cvalue2,cvalue3,cvalue4

!       bulk_behav(ibehav)%USER_MAT_PARAM(i)%name=trim(cvalue1)
!       if (trim(cvalue4) == '(real)') then
!         bulk_behav(ibehav)%USER_MAT_PARAM(i)%type=1
!         read(cvalue3,*) bulk_behav(ibehav)%USER_MAT_PARAM(i)%rval
!       else if (trim(cvalue4) == '(integer)') then
!         bulk_behav(ibehav)%USER_MAT_PARAM(i)%type=0
!         read(cvalue3,*) bulk_behav(ibehav)%USER_MAT_PARAM(i)%ival
!       endif
!       cycle
!  11   test=.false.
!       exit
!     enddo

!     close(IO_UNIT)

! !!$    print*,bulk_behav(ibehav)%USER_MAT_file_NAME
! !!$
! !!$    do i=1,size(bulk_behav(ibehav)%USER_PARAM)
! !!$      if (bulk_behav(ibehav)%USER_PARAM(i)%type == 0) then
! !!$        print*,bulk_behav(ibehav)%USER_PARAM(i)%name,bulk_behav(ibehav)%USER_PARAM(i)%ival
! !!$      else if (bulk_behav(ibehav)%USER_PARAM(i)%type == 1) then
! !!$        print*,bulk_behav(ibehav)%USER_PARAM(i)%name,bulk_behav(ibehav)%USER_PARAM(i)%rval
! !!$      endif
! !!$    enddo

    return   
    
20  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
   
  end subroutine read_UMAT
!!!------------------------------------------------------------------------
  subroutine write_UMAT(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich
    
    write(nfich,102) bulk_behav(ibehav)%USER_MAT_file_name

102 format(38X,1(2X,A50))

  end subroutine write_UMAT
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  subroutine read_discrete(ibehav)

    implicit none   

    integer :: ibehav
    !                         1234567890123456789012345
    character(len=25) :: IAM='behaviours::read_discrete'
    character(len=80) :: cout

    allocate(bulk_behav(ibehav)%discrete_stiffness(nbdime) , &
             bulk_behav(ibehav)%discrete_viscosity(nbdime) , &
             bulk_behav(ibehav)%discrete_mass(nbdime))

    bulk_behav(ibehav)%discrete_stiffness = 0.d0
    bulk_behav(ibehav)%discrete_viscosity = 0.d0
    bulk_behav(ibehav)%discrete_mass = 0.d0

    select case (nbdime) 
    case (2)
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%discrete_mass(1),&
                                                   bulk_behav(ibehav)%discrete_mass(2)
      if( .not. read_G_clin()) goto 10
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%discrete_stiffness(1), &
                                                   bulk_behav(ibehav)%discrete_stiffness(2)
      if( .not. read_G_clin()) goto 10
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10) bulk_behav(ibehav)%discrete_viscosity(1), &
                                                   bulk_behav(ibehav)%discrete_viscosity(2)
    case(3)
      read(G_clin(39:101),'(3(7X,D14.7))',err = 10) bulk_behav(ibehav)%discrete_mass(1), &
                                                    bulk_behav(ibehav)%discrete_mass(2), &
                                                    bulk_behav(ibehav)%discrete_mass(3)
      if( .not. read_G_clin()) goto 10
      read(G_clin(39:101),'(3(7X,D14.7))',err = 10) bulk_behav(ibehav)%discrete_stiffness(1), &
                                                    bulk_behav(ibehav)%discrete_stiffness(2), &
                                                    bulk_behav(ibehav)%discrete_stiffness(3)
      if( .not. read_G_clin()) goto 10
      read(G_clin(39:101),'(3(7X,D14.7))',err = 10) bulk_behav(ibehav)%discrete_viscosity(1), &
                                                    bulk_behav(ibehav)%discrete_viscosity(2), &
                                                    bulk_behav(ibehav)%discrete_viscosity(3)
    case default
      write(cout,'(A,1x,I0)') 'Space dimension',nbdime
      call logmes(cout)
      call FATERR(IAM,'Sorry unsupported space dimension')
    end select

    return   
10  write(cout,'(A,A)') 'reading error in law ',bulk_behav(ibehav)%behav

    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
  
  end subroutine
!!!------------------------------------------------------------------------
  subroutine write_discrete(ibehav,nfich)

    implicit none
    integer :: ibehav,nfich
    !                         12345678901234567890123456
    character(len=26) :: IAM='behaviours::write_discrete'
    
    select case (nbdime) 
    case (2)
      write(nfich,101)  bulk_behav(ibehav)%behav, bulk_behav(ibehav)%lawty, &
                       'm1  =',bulk_behav(ibehav)%discrete_mass(1)     , &
                       'm2  =',bulk_behav(ibehav)%discrete_mass(2)
      write(nfich,102) 'k1  =',bulk_behav(ibehav)%discrete_stiffness(1), &
                       'k2  =',bulk_behav(ibehav)%discrete_stiffness(2)
      write(nfich,102) 'c1  =',bulk_behav(ibehav)%discrete_viscosity(1), &
                       'c2  =',bulk_behav(ibehav)%discrete_viscosity(2)
    case(3)
      write(nfich,103)  bulk_behav(ibehav)%behav, bulk_behav(ibehav)%lawty, &
                       'm1  =',bulk_behav(ibehav)%discrete_mass(1)     , &
                       'm2  =',bulk_behav(ibehav)%discrete_mass(2)     , &
                       'm3  =',bulk_behav(ibehav)%discrete_mass(3)
      write(nfich,104) 'k1  =',bulk_behav(ibehav)%discrete_stiffness(1), &
                       'k2  =',bulk_behav(ibehav)%discrete_stiffness(2), &
                       'k3  =',bulk_behav(ibehav)%discrete_stiffness(3)
      write(nfich,104) 'c1  =',bulk_behav(ibehav)%discrete_viscosity(1), &
                       'c2  =',bulk_behav(ibehav)%discrete_viscosity(2), &
                       'c3  =',bulk_behav(ibehav)%discrete_viscosity(3)
    case default
      call FATERR(IAM,'Sorry unsupported space dimension')
    end select

101 format(1X,A5,2X,A30,2(2X,A5,D14.7))
102 format(38X,2(2X,A5,D14.7))
103 format(1X,A5,2X,A30,3(2X,A5,D14.7))
104 format(38X,3(2X,A5,D14.7))

    
  end subroutine

!!!------------------------------------------------------------------------

  subroutine read_joint(ibehav,jtype)

    implicit none   

    !jtype = 0 elas 
    !jtype = 1 Mohr-Coulomb
    !jtype = 2 FCZM
    
    integer :: ibehav,jtype
    !                         1234567890123456789012
    character(len=22) :: IAM='behaviours::read_joint'
    character(len=80) :: cout

    if (jtype == 0) then    
       allocate(bulk_behav(ibehav)%joint_param(3))
      bulk_behav(ibehav)%joint_param=0.d0
    else if (jtype == 1) then
      allocate(bulk_behav(ibehav)%joint_param(9))
      bulk_behav(ibehav)%joint_param=0.d0
    else if (jtype == 2) then
      allocate(bulk_behav(ibehav)%joint_param(15))
      bulk_behav(ibehav)%joint_param=0.d0
    else
      call faterr(IAM,'unknown type of joint material')
    endif

    !kt,knc,knt      
    read(G_clin(39:101),'(3(7X,D14.7))',err = 10)   bulk_behav(ibehav)%joint_param(1), &
                                                    bulk_behav(ibehav)%joint_param(2), &
                                                    bulk_behav(ibehav)%joint_param(3)         
    if (jtype==1) then
      !kncc,ec 
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10)  bulk_behav(ibehav)%joint_param(4), &
                                                    bulk_behav(ibehav)%joint_param(5)
      !ftrc
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:59),'((7X,D14.7))',err = 10)   bulk_behav(ibehav)%joint_param(6)
      !phi,C,zmu
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:101),'(3(7X,D14.7))',err = 10) bulk_behav(ibehav)%joint_param(7), &
                                                    bulk_behav(ibehav)%joint_param(8), &
                                                    bulk_behav(ibehav)%joint_param(9)

    else if (jtype==2) then
      !kncc,ec   
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10)  bulk_behav(ibehav)%joint_param(4), &
                                                    bulk_behav(ibehav)%joint_param(5)
      !phi,zmu 
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10)  bulk_behav(ibehav)%joint_param(6), &
                                                    bulk_behav(ibehav)%joint_param(7)
      !pf,pd 
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:80),'(2(7X,D14.7))',err = 10)  bulk_behav(ibehav)%joint_param(8), &
                                                    bulk_behav(ibehav)%joint_param(9)
      !ct,s2,G2 
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:101),'(3(7X,D14.7))',err = 10) bulk_behav(ibehav)%joint_param(10), &
                                                    bulk_behav(ibehav)%joint_param(11), &
                                                    bulk_behav(ibehav)%joint_param(12)      
      !cn,s1,G1 
      if ( .not. read_G_clin()) call faterr(IAM,'missing line')
      read(G_clin(39:101),'(3(7X,D14.7))',err = 10) bulk_behav(ibehav)%joint_param(13), &
                                                    bulk_behav(ibehav)%joint_param(14), &
                                                    bulk_behav(ibehav)%joint_param(15)      

    endif
    
    return
    
10  write(cout,'(A,A)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
  
  end subroutine read_joint
  !!!------------------------------------------------------------------------
  subroutine write_joint(ibehav,jtype,nfich)

    implicit none
    integer :: ibehav,jtype,nfich
    !                         12345678901234567890123456
    character(len=26) :: IAM='behaviours::write_joint'
    
    write(nfich,101)  bulk_behav(ibehav)%behav, bulk_behav(ibehav)%lawty, &
                       'kt  =',bulk_behav(ibehav)%joint_param(1)     , &
                       'knc =',bulk_behav(ibehav)%joint_param(2)     , & 
                       'knt =',bulk_behav(ibehav)%joint_param(3)    
    if (jtype==1) then
        write(nfich,102) 'kncc=',bulk_behav(ibehav)%joint_param(4)   , &
                         'ec  =',bulk_behav(ibehav)%joint_param(5)
        write(nfich,105) 'ftrc=',bulk_behav(ibehav)%joint_param(6)
        write(nfich,104) 'phi =',bulk_behav(ibehav)%joint_param(7)   , &
                         'C   =',bulk_behav(ibehav)%joint_param(8)   , &
                         'zmu =',bulk_behav(ibehav)%joint_param(9)             
    else if (jtype==2) then
        write(nfich,102) 'kncc=',bulk_behav(ibehav)%joint_param(4)   , &
                         'ec  =',bulk_behav(ibehav)%joint_param(5)
        write(nfich,102) 'phi =',bulk_behav(ibehav)%joint_param(6)   , &
                         'zmu =',bulk_behav(ibehav)%joint_param(7)
        write(nfich,102) 'pf  =',bulk_behav(ibehav)%joint_param(8)   , &
                         'pd  =',bulk_behav(ibehav)%joint_param(9)
        write(nfich,104) 'ct  =',bulk_behav(ibehav)%joint_param(10)  , &
                         's2  =',bulk_behav(ibehav)%joint_param(11)  , &
                         'G2  =',bulk_behav(ibehav)%joint_param(12)
        write(nfich,104) 'cn  =',bulk_behav(ibehav)%joint_param(13)  , &
                         's1  =',bulk_behav(ibehav)%joint_param(14)  , &
                         'G1  =',bulk_behav(ibehav)%joint_param(15)
      endif

101 format(1X,A5,2X,A30,3(2X,A5,D14.7))
102 format(38X,2(2X,A5,D14.7))
103 format(1X,A5,2X,A30,3(2X,A5,D14.7))
104 format(38X,3(2X,A5,D14.7))
105 format(38X,(2X,A5,D14.7))
    
  end subroutine

!!!------------------------------------------------------------------------
  subroutine write_comment(nfich)
 
    implicit none
    integer :: nfich
    !                12345678901234567890123456789012345678901234567890123456789012345678901234567
    write(nfich,'(A72)')'! File BULK_BEHAV                                                       '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''$''       preceeds a keyword used in scanning files.   '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''behav''   stands for the nickname of a bulk behaviour  '
    write(nfich,'(A72)')'! law, character(len=5).                                                '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! The symbol   ''lawty''   stands for the name of a dedicated reading   '
    write(nfich,'(A72)')'! filter of a bulk behaviour law, character(len=30).                    '                             
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! STANDARD FILTERS of bulk laws                                         '
    write(nfich,'(A72)')'!                                                                       '
    write(nfich,'(A72)')'! 123456789012345678901234567890:                                       '
    write(nfich,'(A72)')'!                               :                                       '
    write(nfich,'(A72)')'! bulk behaviour                :                                       '           
    write(nfich,'(A72)')'!                               :                                       ' 
    write(nfich,'(A72)')'! RIGID                         : Rigid body                            '
    write(nfich,'(A72)')'! ELAS                          : Deformable elastic body               '  
    write(nfich,'(A72)')'                                                                        ' 
 
  end subroutine write_comment
!!!----------------------------------------------------------------------------------------------
!!! GET FUNCTION
!!!----------------------------------------------------------------------------------------------
  integer function get_bulk_behav_nb(behav)
    
    implicit none
    
    integer          :: ibehav
    character(len=5) :: behav
    
    get_bulk_behav_nb = 0
    
    do ibehav=1,size(bulk_behav)
       if (behav == bulk_behav(ibehav)%behav) then
          get_bulk_behav_nb=ibehav
          return
       end if
    end do
    
  end function get_bulk_behav_nb
!!!------------------------------------------------------------------------
  integer function get_nb_bulk_behav(fantome)

    implicit none
    
    integer,optional  :: fantome
    
    get_nb_bulk_behav = nb_bulk_behav
    
  end function get_nb_bulk_behav
!!!------------------------------------------------------------------------
  character(len=5) function get_bulk_behav_ID(ibehav)

    implicit none

    integer  :: ibehav
    
    get_bulk_behav_ID = bulk_behav(ibehav)%behav
    
  end function get_bulk_behav_ID
!!!------------------------------------------------------------------------
  logical function is_ELAS(ibehav)

    implicit none

    integer :: ibehav
  
    is_ELAS=.false.

    if ( bulk_behav(ibehav)%lawty == 'ELAS                          ' .or. &
         bulk_behav(ibehav)%lawty == 'ELAS_PLAS                     ' .or. &
         bulk_behav(ibehav)%lawty == 'THERMO_ELAS                   ')  is_ELAS = .true.

  end function is_ELAS
!!!------------------------------------------------------------------------
  logical function is_ELAS_PLAS(ibehav)

    implicit none

    integer :: ibehav
  
    is_ELAS_PLAS=.false.

    !fd debile
    if ( bulk_behav(ibehav)%lawty == 'ELAS_PLAS                     ' .and. &
         bulk_behav(ibehav)%critere /= 0 )  is_ELAS_PLAS = .true.

  end function is_ELAS_PLAS
!!!------------------------------------------------------------------------
  real(kind=8) function get_rho(ibehav)

    implicit none

    integer      :: ibehav
    !  
    get_rho=bulk_behav(ibehav)%Umass%val
    !
  end function get_rho
!!!------------------------------------------------------------------------
  integer function get_rho_type(ibehav)

    implicit none

    integer      :: ibehav
    !  
    get_rho_type=bulk_behav(ibehav)%Umass%type
    !
  end function get_rho_type
!!!------------------------------------------------------------------------
  subroutine get_elas_coeff(ibehav,anisotropie,elas_coeff)

    implicit none

    integer                     :: ibehav,anisotropie
    real(kind=8), dimension(21) :: elas_coeff
    !  
    anisotropie  = bulk_behav(ibehav)%anisotropie
    elas_coeff(:)= bulk_behav(ibehav)%elas_coeff%val(:)

  end subroutine get_elas_coeff
!!!------------------------------------------------------------------------
  integer function get_elas_coeff_type(ibehav)

    implicit none

    integer      :: ibehav
    !  
    get_elas_coeff_type=bulk_behav(ibehav)%elas_coeff%type
    !
  end function get_elas_coeff_type
!!!------------------------------------------------------------------------
  subroutine get_visco_elas_coeff(ibehav,anisotropie,visco_elas_coeff)

    implicit none

    integer                     :: ibehav,anisotropie
    real(kind=8), dimension(21) :: visco_elas_coeff
    !  
    anisotropie  = bulk_behav(ibehav)%anisotropie
    visco_elas_coeff(:)= bulk_behav(ibehav)%velas_coeff(:)

  end subroutine get_visco_elas_coeff
!!!------------------------------------------------------------------------
  subroutine get_plas_coeff(ibehav,anisotropie, &
                                 crit_coeff, &
                                 iso_hard,isoh_coeff, &
                                 cine_hard,cinh_coeff,&
                                 visco_plas,vplas_coeff)
    implicit none

    integer                     :: ibehav,anisotropie
!    REAL(kind=8), DIMENSION(21) :: elas_coeff
!    INTEGER                     :: critere
    integer                     :: iso_hard,cine_hard
    real(kind=8), dimension(10) :: crit_coeff
    real(kind=8), dimension(80) :: isoh_coeff
    real(kind=8), dimension(1)  :: cinh_coeff
    integer                     :: visco_plas
    real(kind=8), dimension(3)  :: vplas_coeff    

    anisotropie  = bulk_behav(ibehav)%anisotropie
!    elas_coeff(:)= bulk_behav(ibehav)%elas_coeff(:)

!    critere      = bulk_behav(ibehav)%critere
    crit_coeff(:)= bulk_behav(ibehav)%crit_coeff(:)

    iso_hard     = bulk_behav(ibehav)%iso_hard
    isoh_coeff(:)= bulk_behav(ibehav)%isoh_coeff(:)

    cine_hard = bulk_behav(ibehav)%cine_hard
    cinh_coeff= bulk_behav(ibehav)%cinh_coeff(1)

    visco_plas = bulk_behav(ibehav)%visco_plas
    vplas_coeff(:)= bulk_behav(ibehav)%vplas_coeff(:)

    return

  end subroutine get_plas_coeff
!!!------------------------------------------------------------------------
  function get_dilatation(ibehav)

    implicit none

    real(kind=8),dimension(3) :: get_dilatation  
    integer      :: ibehav

    get_dilatation=bulk_behav(ibehav)%dilatation%val

  end function get_dilatation
!!!------------------------------------------------------------------------
  integer function get_dilatation_type(ibehav)

    implicit none

    integer      :: ibehav
    !  
    get_dilatation_type=bulk_behav(ibehav)%dilatation%type
    !
  end function get_dilatation_type
!!!------------------------------------------------------------------------
  real(kind=8) function get_Tref_meca(ibehav)

    implicit none
    integer      :: ibehav

    get_Tref_meca=bulk_behav(ibehav)%T_ref_meca
    
  end function get_Tref_meca
!!!-------------------------------------------------------------------------
  integer function get_sphv_type(ibehav)

    implicit none

    integer      :: ibehav
      
    get_sphv_type=bulk_behav(ibehav)%specific_capacity%type
    
  end function 
!!!------------------------------------------------------------------------
  integer function get_coco_type(ibehav)

    implicit none

    integer      :: ibehav

    get_coco_type=bulk_behav(ibehav)%conductivity_coefficient%type

  end function
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_sphv(ibehav)

    implicit none

    integer      :: ibehav
      
    get_sphv=bulk_behav(ibehav)%specific_capacity%val
    
  end function get_sphv
!!!------------------------------------------------------------------------
  real(kind=8) function get_coco(ibehav)

    implicit none

    integer      :: ibehav

    get_coco=bulk_behav(ibehav)%conductivity_coefficient%val

  end function get_coco
!!!------------------------------------------------------------------------ 
  character(len=30) function get_sphv_field_name(ibehav)

    implicit none

    integer      :: ibehav
      
    get_sphv_field_name=bulk_behav(ibehav)%specific_capacity%field
    
  end function 
!!!------------------------------------------------------------------------
  character(len=30) function get_coco_field_name(ibehav)

    implicit none

    integer      :: ibehav

    get_coco_field_name=bulk_behav(ibehav)%conductivity_coefficient%field

  end function
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_Tref_ther(ibehav)

    implicit none

    integer      :: ibehav

    get_Tref_ther=bulk_behav(ibehav)%T_ref_ther

  end function get_Tref_ther
!!!------------------------------------------------------------------------
  real(kind=8) function get_Tini_ther(ibehav)

    implicit none

    integer      :: ibehav
    get_Tini_ther=bulk_behav(ibehav)%T_ini_ther
    
  end function get_Tini_ther
!!!------------------------------------------------------------------------
  subroutine indent_bulk_behav(ind_color)

    implicit none
    integer          :: ibehav
    character(len=3) :: ind_color
    
    do ibehav=1,size(bulk_behav)
       bulk_behav(ibehav)%behav=ind_color//bulk_behav(ibehav)%behav(4:5)
    end do
    
  end subroutine indent_bulk_behav
!!!------------------------------------------------------------------------
  real(kind=8) function get_ECond(ibehav)

    implicit none
    integer :: ibehav

    get_ECond = bulk_behav(ibehav)%ECond
    
  end function get_ECond
!!!------------------------------------------------------------------------
  character(len=3) function get_Tmodel(ibehav)

    implicit none
    integer :: ibehav

    get_Tmodel = bulk_behav(ibehav)%Tmodel
    
  end function get_Tmodel
!!!------------------------------------------------------------------------
  real(kind=8) function get_TCond(ibehav)

    implicit none
    integer :: ibehav

    get_TCond = bulk_behav(ibehav)%TCond
    
  end function get_TCond
!!!------------------------------------------------------------------------
  subroutine get_AniTCond(ibehav,PTcond,STcond,Tnx,Tny)

    implicit none
    integer      :: ibehav
    real(kind=8) :: PTcond,STcond,Tnx,Tny

    PTCond = bulk_behav(ibehav)%PTCond
    STCond = bulk_behav(ibehav)%STCond
    Tnx = bulk_behav(ibehav)%Tnx
    Tny = bulk_behav(ibehav)%Tny
    
  end subroutine get_AniTCond
!!!------------------------------------------------------------------------
  real(kind=8) function get_Hspe(ibehav)

    implicit none
    integer :: ibehav

    get_Hspe = bulk_behav(ibehav)%Hspe
    
  end function get_Hspe
!!!------------------------------------------------------------------------
  subroutine get_equivalent_mat_prop(ibehav,Eeq,NUeq)

    implicit none
    integer      :: ibehav
    real(kind=8) :: Eeq,NUeq

    Eeq  = bulk_behav(ibehav)%Eeq
    NUeq = bulk_behav(ibehav)%NUeq
    
  end subroutine get_equivalent_mat_prop
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_surface_energy(ibehav)
    
    implicit none
    integer :: ibehav

    get_surface_energy = bulk_behav(ibehav)%WS

  end function get_surface_energy
!!!------------------------------------------------------------------------ 
!!vhn
  subroutine get_surface_energy_WS(ibehav,WSmax,WSmin,ContTime,ActIner)
    
    implicit none
    integer :: ibehav,flag
    real(kind=8) ::WSmin,WSmax,ContTime,ActIner

    WSmax = bulk_behav(ibehav)%WSmax
    WSmin = bulk_behav(ibehav)%WSmin
    ContTime = bulk_behav(ibehav)%ContTime
    ActIner = bulk_behav(ibehav)%ActIner

  end subroutine get_surface_energy_WS
!!!------------------------------------------------------------------------ 
  subroutine Initialize_WS(ibehav,WS)
    implicit none
    integer      :: ibehav
    real(kind=8) :: WS

    WS = bulk_behav(ibehav)%WS
    
  end subroutine Initialize_WS
!!!------------------------------------------------------------------------ 
  subroutine compute_WSvsT(ibehav,WS,T)
    implicit none
    integer      :: ibehav
    real(kind=8) :: WS,T

    select case(bulk_behav(ibehav)%WSilaw)
    case(i_ludinglaw)
       WS = bulk_behav(ibehav)%WS + bulk_behav(ibehav)%WSvar*tanh((bulk_behav(ibehav)%TMelt-T)/bulk_behav(ibehav)%Tvar)
    case(i_thibautlaw)
       ! ....
    case(i_renouflaw)
       ! ....
    case DEFAULT
       call LOGMES('In bulk_behav::compute_WS: no model have been defined')
       WS = 0.0
    end select

  end subroutine compute_WSvsT
!!!------------------------------------------------------------------------
!!!MR&VHN WSini - WS before update
  subroutine compute_WSvsTime(ibehav,WSstatus,WS,WStime,WSini)
    implicit none
    integer(kind=4),intent(in) :: ibehav,WSstatus
    real(kind=8),intent(out)   :: WS
    real(kind=8),intent(inout) :: WStime
    real(kind=8),intent(in)    :: WSini
    real(kind=8)               :: DWS

    WS  = 0.d0
    DWS = 0.d0

    select case(bulk_behav(ibehav)%WSilaw)
    case(i_ludinglaw)
       !nothing to do yet
    case(i_renouflaw)
       select case(WSstatus)
       case(i_undefined)
          !WStime = WStime
          WS     = bulk_behav(ibehav)%WSmax
       case(i_free)
          WStime = WStime + H
          DWS    = bulk_behav(ibehav)%WSmax - bulk_behav(ibehav)%WSmin
          WStime = min(WStime,bulk_behav(ibehav)%ContTime) !MR&VHN
          WS     = bulk_behav(ibehav)%WSmax - DWS*WStime/bulk_behav(ibehav)%ContTime
       case(i_sheared)
          WStime = 0.d0
          WS     = bulk_behav(ibehav)%WSmax
       case(i_stationary)
          !WStime = WStime
          WS     = WSini
       end select
!!vhn
    case(i_renouflaw2)
       select case(WSstatus)
       case(i_undefined)
          !WStime = WStime
          WS     = bulk_behav(ibehav)%WSmax
       case(i_free)
          WStime = WStime + H
          DWS    = ABS(bulk_behav(ibehav)%WSmax - bulk_behav(ibehav)%WSmin)
          WStime = min(WStime,bulk_behav(ibehav)%ContTime) !MR&VHN
          WS     = (bulk_behav(ibehav)%WSmax)-DWS*(1-exp(-1.D0*6*WStime/bulk_behav(ibehav)%ContTime))
       case(i_sheared)
          WStime = 0.d0
          WS     = bulk_behav(ibehav)%WSmax
       case(i_stationary)
          !WStime = WStime
          WS     = WSini
       end select
    case(i_nhulaw)
       select case(WSstatus)
       case(i_undefined)
          !WStime = WStime
          WS     = bulk_behav(ibehav)%WSmax
       case(i_free)
          WStime = WStime + H
          DWS    = bulk_behav(ibehav)%WSmax - bulk_behav(ibehav)%WSmin
          WStime = min(WStime,bulk_behav(ibehav)%ContTime) !MR&VHN
          WS     = bulk_behav(ibehav)%WSmax - DWS*WStime/bulk_behav(ibehav)%ContTime
       case(i_sheared)
          WStime = WStime - bulk_behav(ibehav)%ActIner
          DWS    = bulk_behav(ibehav)%WSmax - bulk_behav(ibehav)%WSmin
          WStime = max(WStime,0.d0) !MR&VHN
          WS     = bulk_behav(ibehav)%WSmax - DWS*WStime/bulk_behav(ibehav)%ContTime
       case(i_stationary)
          !WStime = WStime
          WS     = WSini
       end select
    case default

    end select

  end subroutine compute_WSvsTime
!!!------------------------------------------------------------------------
  integer function get_elas_model(ibehav)

    implicit none
    integer :: ibehav
!  
    get_elas_model=bulk_behav(ibehav)%elas_modele
!
  end function get_elas_model
!------------------------------------------------------------------------ 
  integer function get_visco_elas_model(ibehav)

    implicit none
    integer :: ibehav
  
    get_visco_elas_model=bulk_behav(ibehav)%visco_elas

  end function get_visco_elas_model
!!!------------------------------------------------------------------------ 
  integer function get_plas_critere(ibehav)

    implicit none
    integer :: ibehav
!  
    get_plas_critere=bulk_behav(ibehav)%critere
!
  end function get_plas_critere
!!!------------------------------------------------------------------------ 
  integer function get_therm_cpl(ibehav)

    implicit none
    integer :: ibehav
!  
    get_therm_cpl=bulk_behav(ibehav)%therm_cpl
!
  end function get_therm_cpl
!!!------------------------------------------------------------------------ 
  integer function get_therm_effect(ibehav)

    implicit none
    integer :: ibehav
    !  
    get_therm_effect=bulk_behav(ibehav)%therm_effect
    !
  end function get_therm_effect
!!!------------------------------------------------------------------------ 
  integer function get_therm_varia(ibehav)

    implicit none
    integer :: ibehav
!  
    get_therm_varia=bulk_behav(ibehav)%therm_varia
    !
  end function get_therm_varia
!!!------------------------------------------------------------------------ 
  logical function get_is_external(ibehav)

    implicit none
    integer :: ibehav

    get_is_external=bulk_behav(ibehav)%is_external
    
  end function get_is_external
!!!------------------------------------------------------------------------
  subroutine set_is_external(ibehav)

    implicit none
    integer :: ibehav

    bulk_behav(ibehav)%is_external=.true.

  end subroutine set_is_external
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------ 
  logical function get_is_a_user_mat(ibehav)

    implicit none
    integer :: ibehav

    get_is_a_user_mat=bulk_behav(ibehav)%is_a_user_mat
    
  end function get_is_a_user_mat
!!!------------------------------------------------------------------------

! !!!------------------------------------------------------------------------ 
!   integer function get_user_mat_param_size(ibehav)

!     implicit none
!     integer :: ibehav

!     get_user_mat_param_size=size(bulk_behav(ibehav)%user_mat_param)
    
!   end function get_user_mat_param_size
! !!!------------------------------------------------------------------------
! !!!------------------------------------------------------------------------ 
!   subroutine get_user_mat_param_fields(ibehav,i,name,type,ival,rval)

!     implicit none
!     integer           :: ibehav,i,type,ival
!     character(LEN=50) :: name
!     real(KIND=8)      :: rval

!     name = bulk_behav(ibehav)%user_mat_param(i)%name
!     type = bulk_behav(ibehav)%user_mat_param(i)%type
!     ival=0;rval=0.D0
!     if (type == 0) then
!       ival=bulk_behav(ibehav)%user_mat_param(i)%ival 
!     else
!       rval=bulk_behav(ibehav)%user_mat_param(i)%rval
!     endif

!   end subroutine get_user_mat_param_fields
! !!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------ 
  character(len=80) function get_user_mat_filename(ibehav)

    implicit none
    integer :: ibehav

    get_user_mat_filename = trim(location('DATBOX/'//trim(bulk_behav(ibehav)%USER_MAT_file_name)))
    
  end function get_user_mat_filename


!!!------------------------------------------------------------------------
!------------------------------------------------------------------------
 integer function get_bulk_field_nb(ibehav)
  implicit none
  integer :: ibehav

  get_bulk_field_nb = bulk_behav(ibehav)%nb_fields


 end function 
!------------------------------------------------------------------------------
 character(len=30) function get_bulk_field_name(ibehav,if)
  implicit none
  integer :: ibehav,if

  get_bulk_field_name=bulk_behav(ibehav)%field_name(if)

 end function 
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
  subroutine set_nb_bulks(i4)

    implicit none
    integer :: i4,errare,ibehav
                            !123456789012
    character(len=12):: IAM='set_nb_bulks'
    type(T_BULK_BEHAV)  :: zero_mat   

    ! on genere un materiau initialise a 0

    zero_mat%umass%type = 0
    zero_mat%umass%val = 0.D0
    !
    zero_mat%elas_modele = 0
    zero_mat%anisotropie = 0
    zero_mat%elas_coeff%type = 0
    zero_mat%elas_coeff%val  = 0.D0
    !
    zero_mat%critere = 0
    !
    zero_mat%iso_hard = 0
    zero_mat%isoh_coeff = 0.D0
    !
    zero_mat%cine_hard = 0
    zero_mat%cinh_coeff = 0.D0
    !
    zero_mat%visco_plas  = 0
    zero_mat%vplas_coeff = 0.D0
    !
    zero_mat%visco_elas  = 0
    zero_mat%velas_coeff = 0.D0
    !
    zero_mat%therm_cpl  = 0
    zero_mat%dilatation%type = 0
    zero_mat%dilatation%val = 0.D0    
    zero_mat%T_ref_meca = 0.D0
    !
    zero_mat%therm_effect             = 0
    zero_mat%specific_capacity%type       = 0
    zero_mat%specific_capacity%val        = 0.D0
    zero_mat%conductivity_coefficient%val = 0.D0
    !
    zero_mat%therm_varia = 0
    zero_mat%T_ref_ther  = 0.D0
    zero_mat%T_ini_ther  = 0.D0
    !
    zero_mat%ECond  = 0.D0
    zero_mat%TCond  = 0.D0
    zero_mat%Tvar   = 0.D0
    zero_mat%TMelt  = 0.D0
    zero_mat%WS     = 0.D0
    zero_mat%WSvar  = 0.D0
    zero_mat%Eratio = 0.D0
    zero_mat%WSilaw = 0 
    ! 
    zero_mat%is_external=.false.
    !
    zero_mat%is_a_user_mat=.false.
    zero_mat%user_mat_file_name=' '
    ! nullify(zero_mat%user_mat_param)

    nullify(zero_mat%discrete_stiffness, &
            zero_mat%discrete_viscosity, &
            zero_mat%discrete_mass)
    
    nullify(zero_mat%joint_param)

    nb_bulk_behav = i4

    if (allocated(bulk_behav)) then
      call faterr(IAM,'bulk_behav already allocated')
    endif

    allocate(bulk_behav(i4),stat=errare)

    if (errare /= 0) then
       call LOGMES('check DATBOX/BULK_BEHAV.DAT')
       call FATERR(IAM,'error allocating laws')
    end if

    do ibehav = 1,i4
       bulk_behav(ibehav) = zero_mat
    enddo

  end subroutine 
!!!------------------------------------------------------------------------
  subroutine set_bulk(i4,behav)
    implicit none
    integer :: i4
    character(len=5) :: behav

    bulk_behav(i4)%behav=behav

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_gravity(r8_vect)
    implicit none
    real(kind=8),dimension(3) :: r8_vect

    grav1 = r8_vect(1)
    grav2 = r8_vect(2)
    grav3 = r8_vect(3)

  end subroutine
!!!------------------------------------------------------------------------
  function get_gravity()
    implicit none
    real(kind=8),dimension(3) :: get_gravity

    get_gravity(1) = grav1
    get_gravity(2) = grav2
    get_gravity(3) = grav3

  end function

!!!------------------------------------------------------------------------
  subroutine get_discrete_mass(i4,mass)
    implicit none
    integer :: i4
    real(kind=8),dimension(nbdime) :: mass

    mass = bulk_behav(i4)%discrete_mass

  end subroutine
!!!------------------------------------------------------------------------
  subroutine get_discrete_stiffness(i4,stiffness)
    implicit none
    integer :: i4
    real(kind=8),dimension(nbdime) :: stiffness

    stiffness = bulk_behav(i4)%discrete_stiffness

  end subroutine
!!!------------------------------------------------------------------------
  subroutine get_discrete_viscosity(i4,viscosity)
    implicit none
    integer :: i4
    real(kind=8),dimension(nbdime) :: viscosity

    viscosity = bulk_behav(i4)%discrete_viscosity

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_joint_param_size(i4,sz)
    implicit none
    integer :: i4
    integer :: sz

    sz = size(bulk_behav(i4)%joint_param)

  end subroutine

!!!------------------------------------------------------------------------
  subroutine get_joint_param(i4,param)
    implicit none
    integer :: i4
    real(kind=8),dimension(:) :: param

    param = bulk_behav(i4)%joint_param

  end subroutine
!!!------------------------------------------------------------------------
  integer function get_joint_param_type(i4)
    implicit none
    integer :: i4

    get_joint_param_type= bulk_behav(i4)%joint_param_type

    return
  end function
!!!------------------------------------------------------------------------
  subroutine write_BIOT(ibehav,nfich)
    
    implicit none
    integer :: ibehav,nfich,i
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::write_BIOT' 
    character(len=80) :: cout
    
    if (bulk_behav(ibehav)%therm_effect/=1) return
    
    write(nfich,'(38X,2X,A5)') 'cpl_:'

    if (bulk_behav(ibehav)%biot_coefficient%type==0)then
       write(nfich,101) 'BIOT=',bulk_behav(ibehav)%biot_coefficient%val
    else
       write(nfich,104) 'BIOT=','field' !,bulk_behav(ibehav)%conductivity_coefficient%field
    endif
    
!old fashion    WRITE(nfich,102) 'SPHV=',bulk_behav(ibehav)%specific_capacity,'COCO=',bulk_behav(ibehav)%conductivity_coefficient
    
    write(nfich,'(A1)')' '
    
100 format(1X,A5,2X,A30,2X,A5,D14.7)
101 format(38X,1(2X,A5,D14.7))
102 format(38X,2(2X,A5,D14.7))
103 format(38X,3(2X,A5,D14.7))
    
104 format(38X,2X,A5,A5)
  end subroutine write_BIOT
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  subroutine read_BIOT(ibehav)

    implicit none   

    integer :: ibehav
    !                            1234567890123456789012
    character(len=22) :: IAM='behaviours::read_BIOT' 
    character(len=80) :: cout
    
    character(len=4)  :: bbtype
    character(len=13) :: anisotropie
    integer           :: i
    
    if( .not. read_G_clin()) goto 10
    
    read(G_clin(41:44),'(A4)') bbtype

    if (bbtype /= 'cpl_') then
       call faterr(IAM,'BULK_BEHAV.DAT unreadable')
    endif
    
    if( .not. read_G_clin()) goto 10
    if (G_clin(46:50) /= 'field') then
      bulk_behav(ibehav)%biot_coefficient%type=0
      read(G_clin(46:59),'(D14.7)',err=10) bulk_behav(ibehav)%biot_coefficient%val
    else

      bulk_behav(ibehav)%biot_coefficient%type=1
      !read(G_clin(52:),*) bulk_behav(ibehav)%conductivity_coefficient%field
      bulk_behav(ibehav)%biot_coefficient%field='BIOT'
      bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 

      bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='BIOT'
    endif

!old fashion    READ(G_clin(39:80),'(2(7X,D14.7))',err=10) bulk_behav(ibehav)%specific_capacity,bulk_behav(ibehav)%conductivity_coefficient

    return
    
10  write(cout,'(A21,A5)') 'reading error in law ',bulk_behav(ibehav)%behav
    call LOGMES('check DATBOX/BULK_BEHAV.DAT')
    call FATERR(IAM,cout)
  
  end subroutine read_BIOT

!!!------------------------------------------------------------------------
  integer function get_external_flux_type(ibehav)

    implicit none

    integer      :: ibehav

    get_external_flux_type=bulk_behav(ibehav)%external_flux_coefficient%type

  end function
  
!!!------------------------------------------------------------------------
  integer function get_biot_type(ibehav)

    implicit none

    integer      :: ibehav

    get_biot_type=bulk_behav(ibehav)%biot_coefficient%type

  end function
  
!!!------------------------------------------------------------------------
  real(kind=8) function get_external_flux(ibehav)

    implicit none

    integer      :: ibehav

    get_external_flux=bulk_behav(ibehav)%external_flux_coefficient%val

  end function get_external_flux
  
!!!------------------------------------------------------------------------
  real(kind=8) function get_biot(ibehav)

    implicit none

    integer      :: ibehav

    get_biot=bulk_behav(ibehav)%biot_coefficient%val

  end function get_biot
!!!------------------------------------------------------------------------
  character(len=30) function get_external_flux_field_name(ibehav)

    implicit none

    integer      :: ibehav

    get_external_flux_field_name=bulk_behav(ibehav)%external_flux_coefficient%field

  end function
  
!!!------------------------------------------------------------------------
  character(len=30) function get_biot_field_name(ibehav)

    implicit none

    integer      :: ibehav

    get_biot_field_name=bulk_behav(ibehav)%biot_coefficient%field

  end function
!!!------------------------------------------------------------------------
! DA : pour appliquer des valeurs sur des loi de comportement

  subroutine set_conductivity(nickname, i4, r8)
    
    implicit none
    
    integer          :: i4
    real(kind=8)     :: r8
    character(len=5) :: nickname
    
    integer          :: ibehav

    ibehav = get_bulk_behav_nb(nickname)

    if (i4 /= 0) then
		bulk_behav(ibehav)%conductivity_coefficient%type = 1
		bulk_behav(ibehav)%conductivity_coefficient%field='COCO'
		bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
		bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='COCO'
    else
		bulk_behav(ibehav)%conductivity_coefficient%type = i4
		bulk_behav(ibehav)%conductivity_coefficient%val  = r8
    endif

  end subroutine

  subroutine set_capacity(nickname, i4, r8)
    
    implicit none
    
    integer          :: i4
    real(kind=8)     :: r8
    character(len=5) :: nickname
    
    integer          :: ibehav

    ibehav = get_bulk_behav_nb(nickname)
    
    if (i4 /= 0) then
		bulk_behav(ibehav)%specific_capacity%type = 1
		bulk_behav(ibehav)%specific_capacity%field='SPHV'
		bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
		bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='SPHV'
    else
		bulk_behav(ibehav)%specific_capacity%type = i4
		bulk_behav(ibehav)%specific_capacity%val  = r8
    endif
    
  end subroutine
  
  subroutine set_biot(nickname, i4, r8)
    
    implicit none
    
    integer          :: i4
    real(kind=8)     :: r8
    character(len=5) :: nickname
    
    integer          :: ibehav

	ibehav = get_bulk_behav_nb(nickname)

	if (i4 /= 0) then
		bulk_behav(ibehav)%biot_coefficient%type = 1
		bulk_behav(ibehav)%biot_coefficient%field='BIOT'
		bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
		bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='BIOT'
    else
		bulk_behav(ibehav)%biot_coefficient%type = i4
		bulk_behav(ibehav)%biot_coefficient%val  = r8
    endif

  end subroutine

  subroutine set_external_flux(nickname, i4, r8)
    
    implicit none
    
    integer          :: i4
    real(kind=8)     :: r8
    character(len=5) :: nickname
    
    integer          :: ibehav

	ibehav = get_bulk_behav_nb(nickname)

	if (i4 /= 0) then
		bulk_behav(ibehav)%external_flux_coefficient%type = 1
		bulk_behav(ibehav)%external_flux_coefficient%field='EX_F'
		bulk_behav(ibehav)%nb_fields = bulk_behav(ibehav)%nb_fields + 1 
		bulk_behav(ibehav)%field_name(bulk_behav(ibehav)%nb_fields)='EX_F'
    else
		bulk_behav(ibehav)%external_flux_coefficient%type = i4
		bulk_behav(ibehav)%external_flux_coefficient%val  = r8
    endif

  end subroutine
  
  subroutine set_solid_density(nickname, r8)
    
    implicit none
    
    real(kind=8)     :: r8
    character(len=5) :: nickname
    
    integer          :: ibehav
    
                            !12345678901234567890123456789
    character(len=29):: IAM='bulk_behav::set_solid_density'
    
    ibehav = get_bulk_behav_nb(nickname)
    
    if (bulk_behav(ibehav)%Umass%type /= 0) call faterr(IAM,'not possible - density is a field') 

    bulk_behav(ibehav)%Umass%val = r8

  end subroutine  
  
  ! introspection des materiaux

  subroutine get_bulk_behav(i_bb, lawty, behav)
    implicit none
    integer(kind=4), intent(in) :: i_bb
    character(len=30) :: lawty                   ! bulk law name              
    character(len=5)  :: behav                   ! bulk law nickname 

                            !12345678901234567890123456
    character(len=26):: IAM='bulk_behav::get_bulk_behav'

    if( .not. allocated(bulk_behav) ) then
      call FATERR(IAM,'bulk_behav is not allocated') 
    endif

    lawty    = bulk_behav(i_bb)%lawty
    behav    = bulk_behav(i_bb)%behav

  end subroutine

  subroutine clean_memory()
    implicit none
    integer(kind=4) :: i

    nb_bulk_behav = 0
    if( allocated(bulk_behav) ) then
      do i = 1, size(bulk_behav)
        ! if( associated(bulk_behav(i)%USER_MAT_PARAM)     ) deallocate(bulk_behav(i)%USER_MAT_PARAM)
        if( associated(bulk_behav(i)%discrete_stiffness) ) deallocate(bulk_behav(i)%discrete_stiffness)
        if( associated(bulk_behav(i)%discrete_viscosity) ) deallocate(bulk_behav(i)%discrete_viscosity)
        if( associated(bulk_behav(i)%discrete_mass     ) ) deallocate(bulk_behav(i)%discrete_mass     )
        if( associated(bulk_behav(i)%joint_param       ) ) deallocate(bulk_behav(i)%joint_param )        
      end do
      deallocate(bulk_behav)
    end if

  end subroutine

end module BULK_BEHAVIOUR

