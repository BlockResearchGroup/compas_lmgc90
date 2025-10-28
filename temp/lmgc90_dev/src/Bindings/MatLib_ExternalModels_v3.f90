module MatLibExternalModels

! additional module in order to use the MatLib modelling library

use overall, only : nbDIME

use utilities, only : logmes, &
                      faterr

use bulk_behaviour, only : get_bulk_behav_ID      , &
                           get_rho                , &
                           get_sphv               , &
                           get_coco               , &
                           get_elas_coeff         , &
                           get_elas_model         , &
                           get_visco_elas_model   , &
                           get_visco_elas_coeff   , &
                           get_plas_critere       , &
                           get_plas_coeff         , &
                           get_therm_cpl          , &
                           get_dilatation         , &
                           get_tref_meca          , &
                           get_tref_ther          , &
                           get_tini_ther          , &
                           get_therm_varia        , &
                           get_therm_effect       , &
                           get_is_a_user_mat      , &
                           get_user_mat_param_size, &
                           get_user_mat_param_fields

use models, only : modelz               , &
                   get_nb_models        , &
                   get_nb_ppsets        , &
                   get_ppset_value      , &
                   get_eleop_id         , &
                   get_eleop_value      , &
                   get_eleop_value_bypps, &
                   get_nb_external_variables

 implicit none
 private

! map between lmgc90 ppset and matlib ppset 

 integer,dimension(:),allocatable :: external_ppset

! bavardage

 logical :: itchatche = .FALSE.

! wrap
 public init_external_models,check_external_ppset, clean_memory

! internal API

 public compute_external_pg,set_ortho_frame

 contains
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine init_External_Models
   implicit none
   integer :: ibehav,imodel,ippset,id
   character(len=5) :: isext
                             !123456789012345678901234567 
   character(len=27)  :: IAM='models::init_external_model'

   if (itchatche) call logmes('Entering : '//IAM)

   !fd on pousse les modeles dans la librairie externe

   do imodel=1,get_nb_models()
     id = get_eleop_id(imodel,'isext') 
     if ( id /= 0) then
       isext = get_eleop_value(imodel,id) 
       if (isext == 'yes__') then     
         call push_model(imodel)
       endif
     endif
   enddo

   if (itchatche) call logmes('Leaving : '//IAM)

 end subroutine
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine push_model(imodel)

!
! declarations pour la matlib

! Dans le conteneur des variables externes on trouve les gradients et les flux.
! L'operateur tangent a donc sa taille  
!
! Dans le conteneur des variables internes ont trouve des variables internes ET
! des variables cachees necessaires a la matlib
!
! le modele elastique standard prend en compte la dilatation dans l'absolue il
! doit permettre un couplage faible 
!
! le modele thermoelastique permet les couplages forts
!

  implicit none

  integer   :: imodel

!  logical,save :: is_first_time=.true.
  integer*4 :: i,imodelz,itmp,zdim,lchaine
  
  character(len=80) :: chaine

  character(len=5)  :: kinematic,formulation,material,anisotropy
  character(len=5)  :: cplth

                            !123456789012345678901234567 
  character(len=27)  :: IAM='models::load_external_model'

  if (itchatche) call logmes('Entering : '//IAM)

!fd on initialise les modeles de MatLib une seule fois

!  if (is_first_time) then
!    print*,'on charge le dico'
!    call f_load_model_dictionary
!    is_first_time = .false.
!  endif

!fd la dimension d'espace du probleme !!

  zdim=int(nbDIME,4)
  imodelz=int(imodel,4)

  if (itchatche) print*,'dimension: ',zdim,' num de modele: ',imodelz

  if (modelz(imodel)%is_a_user_model) then
    chaine=trim(modelz(imodel)%user_model_name)
    lchaine=len_trim(modelz(imodel)%user_model_name)
  else

    ! attention ici POROx ne fair gerer que la partie mecanique le reste est rajoute apres.

    if ((modelz(imodel)%mdlty == 'MECAx') .OR. (modelz(imodel)%mdlty == 'POROx')) then

      kinematic =get_eleop_value(imodel,'kine_')        
      anisotropy=get_eleop_value(imodel,'aniso')
      material  =get_eleop_value(imodel,'mater')

      if (itchatche) print*,kinematic,' ',anisotropy,' ',material

      if (kinematic == 'small') then

        if (anisotropy == 'iso__') then
  
          select case(material)
          case('elas_')
                   !12345678901234567890
            chaine='ISOTROPIC_ELASTICITY'
            lchaine=20
          case('kvisc')
                   
            !v2 chaine='ISOTROPIC_KV_VISCOELASTICITY'
            !lchaine=28
                   !123456789012345678901234567890123
            chaine='ISOTROPIC_KELVIN_VISCO_ELASTICITY'
            lchaine=33
          case('J2iso')
                   !123456789012345678901234567890
            chaine='LINEAR_ISOTROPIC_J2_PLASTICITY'
            lchaine=30
          case('J2mix')
                   !12345678901234567890123456
            chaine='LINEAR_MIXED_J2_PLASTICITY'
            lchaine=26
          case('elasd')
                   !123456789012345678901234567890123456
            chaine='ISOTROPIC_THERMO_DILATANT_ELASTICITY'
            lchaine=36
          case default
            call FATERR(IAM,'unsupported iso__ material')        
          end select
        else if (anisotropy == 'ortho') then
          select case(material)
          case('elas_')
                   !1234567890123456789012
            chaine='ORTHOTROPIC_ELASTICITY'
            lchaine=22
          case('elasd')

            call logMES('ORTHOTROPIC_DILATANT_ELASTICITY')
            call FATERR(IAM,'unsupported ortho material yet')        

                   !1234567890123456789012345678901
            chaine='ORTHOTROPIC_DILATANT_ELASTICITY'
            lchaine=31
          case default
            call FATERR(IAM,'unsupported ortho material')        
          end select
        else
          call FATERR(IAM,'unsupported anisotropy yet')        
        endif

      else if (kinematic == 'large') then

        formulation=get_eleop_value(imodel,'form_')

        if (itchatche) print*,formulation

        if (formulation == 'UpdtL') then
          call FATERR(IAM,'unsupported formulation, check MODELS.DAT')        
        else if (formulation == 'TotaL') then

          if (anisotropy == 'iso__') then

            select case(material)
            case('neoh_')
                     !1234567890
              chaine='NEOHOOKEAN'
              lchaine=10
            case ('hyper')
                     !12345678901234567890123456
              chaine='ISOTROPIC_HYPER_ELASTICITY'
              lchaine=26
            case ('hyp_d')
                      !123456789012345678901234567890123456789012
               chaine='ISOTROPIC_THERMO_DILATANT_HYPER_ELASTICITY'
               lchaine=42
            case ('kvisc')


               call logMES('ISOTROPIC_KV_VISCOHYPERELASTICITY')
               call FATERR(IAM,'unsupported ortho material yet')        

                     !123456789012345678901234567890123
              chaine='ISOTROPIC_KV_VISCOHYPERELASTICITY'
              lchaine=33
            case ('J2iso')
                     !1234567890123456789012345678901234567
              chaine='LINEAR_ISOTROPIC_J2_FINITE_PLASTICITY'
              lchaine=37
            case default
              call FATERR(IAM,'unsupported mechanical model')        
            end select
          else
            call FATERR(IAM,'unsupported anisotropy')        
          endif
        else
          call FATERR(IAM,'unsupported formulation')        
        endif
      else 
        call FATERR(IAM,'you should specify the kine_ option, check MODELS.DAT')        
      endif

    else if (modelz(imodel)%mdlty == 'THERM') then

      formulation=get_eleop_value(imodel,'form_')

      select case(formulation)
      case('class')  ! classical formulation
                 !123456789012345678901234567
        chaine = 'LINEAR_ISOTROPIC_CONDUCTION'
        lchaine=27  
      case('stdvf')  ! standard variational formulation
                     !12345678901234567890123456789012
        chaine = 'ISOTROPIC_VARIATIONAL_CONDUCTION'
        lchaine=32
      case('linvf')  ! linearized variational formulation
                     !123456789012345678901234567890123456789
        chaine = 'LINEAR_ISOTROPIC_VARIATIONAL_CONDUCTION'
        lchaine=39
      case default
        call FATERR(IAM,'unsupported thermal model')        
      end select
    
    else if (modelz(imodel)%mdlty == 'THMEC') then

      kinematic =get_eleop_value(imodel,'kine_')        
      anisotropy=get_eleop_value(imodel,'aniso')
      material  =get_eleop_value(imodel,'mater')

      if (kinematic == 'small') then
        if (anisotropy == 'iso__') then

          select case(material)
          case('elas_')
                  !12345678901234567890123456
            chaine='ISOTROPIC_THERMOELASTICITY'
            lchaine=26
          case('kvisc')
                   !1234567890123456789012345678901234
            chaine='ISOTROPIC_KV_THERMOVISCOELASTICITY'
            lchaine=34
          case ('J2iso')
                   !12345678901234567890123456789012345
            chaine='LINEAR_ISOTROPIC_J2_THERMOPLASTICITY'
            lchaine=35
          case default
            call FATERR(IAM,'unsupported mechanical model')        
          end select
        else
          call FATERR(IAM,'unsupported anisotropy')        
        endif

      else if (kinematic == 'large') then

        formulation=get_eleop_value(imodel,'form_')

        if (formulation == 'UpdtL') then
          call FATERR(IAM,'unsupported formulation, check MODELS.DAT')        
        else if (formulation == 'TotaL') then
          if (anisotropy == 'iso__') then
            select case(material)
            case ('hyper')
                     !1234567890123456789012345678901
              chaine='ISOTROPIC_THERMOHYPERELASTICITY'
              lchaine=31
            case ('kvisc')
                     !123456789012345678901234567890123457890
              chaine='ISOTROPIC_KV_THERMOVISCOHYPERELASTICITY'
              lchaine=40
            case default
              call FATERR(IAM,'unsupported mechanical model')        
            end select
          else
            call FATERR(IAM,'unsupported anisotropy')        
          endif
        else
          call FATERR(IAM,'unsupported formulation')        
        endif
      else 
        call FATERR(IAM,'you should specify the kine_ option, check MODELS.DAT')        
      endif

    else 
      call FATERR(IAM,'you should specify a known model, check MODELS.DAT')        
    endif
  endif

  !** c'est parti on charge les modeles MatLib

  if (itchatche) print*,'new:',chaine,lchaine,zdim
  call f_new_constitutive_model(imodelz,chaine,lchaine,zdim)
 
  if (itchatche) print*,'ok'

  call f_n_external_variables(imodelz,itmp)
  modelz(imodel)%nb_external_variables = itmp      

  if (itchatche) print*,'nb external ',itmp
 
  call f_n_ext_variables_bundled(imodelz,itmp)
  modelz(imodel)%nb_external_variables_bundled=itmp
  if( associated(modelz(imodel)%ext_descriptor) ) deallocate(modelz(imodel)%ext_descriptor)
  allocate(modelz(imodel)%ext_descriptor(itmp))

  if (itchatche) print*,'nb external bundled ',itmp

  do i=1,modelz(imodel)%nb_external_variables_bundled
    call f_type_external_variable(imodelz,i,itmp)
    modelz(imodel)%ext_descriptor(i)%type=itmp
    !
    call f_index_external_variable(imodelz,i+1,itmp)
    modelz(imodel)%ext_descriptor(i)%size=itmp
    !
    call f_index_external_variable(imodelz,i,itmp)
    modelz(imodel)%ext_descriptor(i)%index=itmp-1
    modelz(imodel)%ext_descriptor(i)%size=modelz(imodel)%ext_descriptor(i)%size-itmp
    !
    call f_label_external_variable(imodelz,i,chaine,lchaine)
    lchaine=lchaine+1
                                                     
    modelz(imodel)%ext_descriptor(i)%label_primal=' '
    modelz(imodel)%ext_descriptor(i)%label_primal(1:lchaine)=chaine(1:lchaine)

    if (itchatche) print*,i,modelz(imodel)%ext_descriptor(i)%label_primal(1:lchaine),lchaine

    !
    call f_label_external_force(imodelz,i,chaine,lchaine)
    lchaine=lchaine+1
                                                           
    modelz(imodel)%ext_descriptor(i)%label_dual=' '
    modelz(imodel)%ext_descriptor(i)%label_dual(1:lchaine)=chaine(1:lchaine)

    if (itchatche) print*,i,modelz(imodel)%ext_descriptor(i)%label_dual(1:lchaine),lchaine

  enddo

  call f_n_internal_variables(imodelz,itmp)
  modelz(imodel)%nb_internal_variables = itmp      

  if (itchatche) print*,'nb internal ',itmp

  call f_n_int_variables_bundled(imodelz,itmp)
  modelz(imodel)%nb_internal_variables_bundled=itmp
  if( associated(modelz(imodel)%int_descriptor) ) deallocate(modelz(imodel)%int_descriptor)
  allocate(modelz(imodel)%int_descriptor(itmp))

  if (itchatche) print*,'nb internal bundled ',itmp

  do i=1,modelz(imodel)%nb_internal_variables_bundled
    call f_type_internal_variable(imodelz,i,itmp)
    modelz(imodel)%int_descriptor(i)%type=itmp
    !
    call f_index_internal_variable(imodelz,i+1,itmp)
    modelz(imodel)%int_descriptor(i)%size=itmp
    !
    call f_index_internal_variable(imodelz,i,itmp)
    modelz(imodel)%int_descriptor(i)%index=itmp-1
    modelz(imodel)%int_descriptor(i)%size=modelz(imodel)%int_descriptor(i)%size-itmp
    !
    call f_label_internal_variable(imodelz,i,chaine,lchaine)
    lchaine=lchaine+1
                                                      
    modelz(imodel)%int_descriptor(i)%label_primal =' '
    modelz(imodel)%int_descriptor(i)%label_primal(1:lchaine)=chaine(1:lchaine)

    if (itchatche) print*,i,modelz(imodel)%int_descriptor(i)%label_primal(1:lchaine),lchaine

!fd y en a pas
!      call f_label_internal_force(imodelz,i,chaine,lchaine)
!                                                             !123456789012345678901234567890 
!      modelz(imodel)%int_descriptor(i)%label_dual(1:lchaine)='                              '
!      modelz(imodel)%int_descriptor(i)%label_dual(1:lchaine)=chaine(1:lchaine)
  enddo


   if (itchatche) call logmes('Leaving : '//IAM)

 end subroutine push_model
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine check_external_ppset
   implicit none
   integer :: ibehav,imodel,ippset,nb_ppsets,iext_ppset
   character(len=5) :: isext
                             !1234567890123456789012345678 
   character(len=28)  :: IAM='models::check_external_ppset'

   if (itchatche) call logmes('Entering : '//IAM)
  
   nb_ppsets = get_nb_ppsets()
   if (nb_ppsets == 0) return

   if( allocated(external_ppset) ) deallocate(external_ppset)
   allocate(external_ppset(nb_ppsets))
   external_ppset = 0

   !fd on charge les materiaux
   iext_ppset = 0
   do ippset=1,get_nb_ppsets()
     isext = get_eleop_value_bypps(ippset,'isext')
     if (isext == 'yes__') then
      iext_ppset = iext_ppset + 1 
      external_ppset(ippset)=iext_ppset

      call get_ppset_value(ippset,imodel,ibehav)

!fd deja fait  call push_external_model(imodel)

!fd old version
!      call push_external_behaviour(ibehav)
!      call check_external_model(ibehav,imodel)

!fd new version on a maintenant un set de parametres materiau par pg
!fd ce qui va permettre de les faires varier avec la T par exemple

      call push_behaviour(ippset)
      call check_ppset(ippset)

     endif
   end do 

   if (itchatche) call logmes('Leaving : '//IAM)

 end subroutine check_external_ppset
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine push_behaviour(ippset)

   !fd attention new ibehav est remplace par le numero de ppset externe 
   !
   implicit none   

                            !123456789012345678901234567890123456789
   character(len=30) :: IAM='ExternalModels::push_behaviour'

   integer          :: ibehav,ippset,iext_ppset,inull

   real(kind=8)     :: rho
   real(kind=8)     :: alpha(3),Tref_meca,Tref_ther,Tini_ther
   real(kind=8)     :: sphv,coco

   integer                     :: anisotropy
   real(kind=8),dimension(21)  :: coeff
   INTEGER                     :: iso_hard,cine_hard,visco_plas
   REAL(kind=8), DIMENSION(10) :: crit_coeff
   REAL(kind=8), DIMENSION(80) :: isoh_coeff
   REAL(kind=8), DIMENSION(1)  :: cinh_coeff
   REAL(kind=8), DIMENSION(3)  :: vplas_coeff

   character(len=80) :: chaine
   integer*4         :: lchaine,iext_ppset_z

   logical           :: is_external


   !fd  user mat

   integer           :: size,type,isz,ival
   character(len=50) :: name
   real(kind=8)      :: rval


   if (itchatche) call LOGMES('Entering : '//IAM)

   iext_ppset = external_ppset(ippset)

   !  pour recuperer le num de behav
   call get_ppset_value(ippset,inull,ibehav)

   ! connerie de conversion
   iext_ppset_z=int(iext_ppset,4)


   if (itchatche) print*,'ippset=',ippset,'iext_ppset=',iext_ppset
   if (itchatche) print*,'ibehav=',ibehav

   !fd
   chaine(1:5)=get_bulk_behav_ID(ibehav)
   lchaine=5
   call f_new_material_data(iext_ppset_z,chaine,lchaine)

          !123456789012 
   chaine='MASS_DENSITY'
   lchaine=12
   rho=get_rho(ibehav)

   call f_add_double_property(iext_ppset_z,chaine,lchaine,rho)
     
   !fd passage d'un user_mat 

   if ( get_is_a_user_mat(ibehav) ) then
      if (itchatche) call LOGMES('Loading a user mat')

      size = get_user_mat_param_size(ibehav)

      do isz=1,size

        call get_user_mat_param_fields(ibehav,isz,name,type,ival,rval)

        if (type == 0) then

          chaine=trim(name)
          lchaine=len_trim(name)

          call f_add_integer_property(iext_ppset_z,chaine,lchaine,ival)

        else if (type == 1) then

          chaine=trim(name)
          lchaine=len_trim(name)

          call f_add_double_property(iext_ppset_z,chaine,lchaine,rval)

        end if

      enddo

     return
   endif

   !fd chargement de l'elasticite

   select case (get_elas_model(ibehav))
   case(0) ! rigide
     call LOGMES(IAM//'rigid material unsupported')        

   case(1) !elastique|neohookeen|hyper
   
     call get_elas_coeff(ibehav,anisotropy,coeff)

     select case (anisotropy)

     case(0) ! isotrope
             !1234567890123 
       chaine='YOUNG_MODULUS'         
       lchaine=13
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))

              !1234567890123456789 
       chaine='POISSON_COEFFICIENT'         
       lchaine=19
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))

     case(1) ! orthotrope
                !123456789012345 
       chaine='YOUNG_MODULUS_1'         
       lchaine=15
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))
              !123456789012345 
       chaine='YOUNG_MODULUS_2'         
       lchaine=15
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))
              !123456789012345 
       chaine='YOUNG_MODULUS_3'         
       lchaine=15
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(3))
              !1234567890123456789012 
       chaine='POISSON_COEFFICIENT_12'         
       lchaine=22
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(4))
              !1234567890123456789012 
       chaine='POISSON_COEFFICIENT_13'         
       lchaine=22
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(5))
              !1234567890123456789012 
       chaine='POISSON_COEFFICIENT_23'         
       lchaine=22
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(6))
           !1234567890123456
       chaine='SHEAR_MODULUS_12'         
       lchaine=16
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(7))
              !1234567890123456
       chaine='SHEAR_MODULUS_13'         
       lchaine=16
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(8))
              !1234567890123456
       chaine='SHEAR_MODULUS_23'         
       lchaine=16
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(9))
     case default ! autres  
       call FATERR(IAM,'unsupported anisotropy')        
     end select
   case(4) ! signorini
     stop
   case default ! autres  
     call FATERR(IAM,'unsupported elastic model')        
   end select

   !fd chargement de la visco elasticite

   if (get_visco_elas_model(ibehav) /=0) then

     call get_visco_elas_coeff(ibehav,anisotropy,coeff)

     select case (anisotropy)
     case(0) ! isotrope
              !123456789012345678901 
       chaine='VISCOUS_YOUNG_MODULUS'         
       lchaine=21
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))

              !123456789012345678901234567 
       chaine='VISCOUS_POISSON_COEFFICIENT'         
       lchaine=27
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))
     case(1) ! orthotrope

              !123456789012345678901234 
       chaine='VISCOUS_YOUNG_MODULUS_1'         
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))

              !123456789012345678901234 
       chaine='VISCOUS_YOUNG_MODULUS_2'         
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))

              !123456789012345678901234 
       chaine='VISCOUS_YOUNG_MODULUS_3'         
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(3))

              !123456789012345678901234567890 
       chaine='VISCOUS_POISSON_COEFFICIENT_12'         
       lchaine=30
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(4))

              !123456789012345678901234567890 
       chaine='VISCOUS_POISSON_COEFFICIENT_13'         
       lchaine=30
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(5))

              !123456789012345678901234567890 
       chaine='VISCOUS_POISSON_COEFFICIENT_23'         
       lchaine=30
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(6))

              !123456789012345678901234
       chaine='VISCOUS_SHEAR_MODULUS_12'         
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(7))

              !123456789012345678901234
       chaine='VISCOUS_SHEAR_MODULUS_13'         
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(8))

              !123456789012345678901234
       chaine='VISCOUS_SHEAR_MODULUS_23'         
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coeff(9))

     case default ! autres  
       call FATERR(IAM,'unsupported anisotropy')        
     end select
   endif

!fd chargement de la (visco)plasticite

   if (get_plas_critere(ibehav) /= 0 ) then

       if (get_plas_critere(ibehav) == 2) then
         call FATERR(IAM,'unsupported plastic criterium')        
       endif

       call get_plas_coeff(ibehav,anisotropy,crit_coeff, &
                           iso_hard,isoh_coeff, &
                           cine_hard,cinh_coeff, &
                           visco_plas,vplas_coeff)


       if (iso_hard /=0 .and. iso_hard /= 2) then
         call FATERR(IAM,'unsupported isotropic hardening')        
       endif

              !1234567890123456 
       chaine='YIELD_IN_TENSION'
       lchaine=16
       call f_add_double_property(iext_ppset_z,chaine,lchaine,isoh_coeff(1))

              !12345678901234567 
       chaine='HARDENING_MODULUS'
       lchaine=17
       call f_add_double_property(iext_ppset_z,chaine,lchaine,isoh_coeff(2))

              !123456789012345678901234567 
       chaine='KINEMATIC_HARDENING_MODULUS' 
       lchaine=27
       call f_add_double_property(iext_ppset_z,chaine,lchaine,cinh_coeff(1))

              !1234567890123456 
       chaine='REFERENCE_STRESS'
       lchaine=16
       call f_add_double_property(iext_ppset_z,chaine,lchaine,vplas_coeff(1))

              !123456789012345678901 
       chaine='REFERENCE_STRAIN_RATE' 
       lchaine=21
       call f_add_double_property(iext_ppset_z,chaine,lchaine,vplas_coeff(2))

              !123456789012345678901234 
       chaine='RATE_DEPENDENCY_EXPONENT'
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,vplas_coeff(3))
     endif

     if (get_therm_cpl(ibehav) /= 0) then

       alpha=get_dilatation(ibehav)       
       Tref_meca=get_Tref_meca(ibehav)

!       print*,alpha,Tref_meca

       call get_elas_coeff(ibehav,anisotropy,coeff)

       select case (anisotropy)

       case(0) ! isotrope

                !123456789012345678901234567890
         chaine='THERMAL_DILATATION_COEFFICIENT'
         lchaine=30
         call f_add_double_property(iext_ppset_z,chaine,lchaine,alpha(1))
       case(1)
                !12345678901234567890123456789012
         chaine='THERMAL_DILATATION_COEFFICIENT_1'
         lchaine=32
         call f_add_double_property(iext_ppset_z,chaine,lchaine,alpha(1))
                !12345678901234567890123456789012
         chaine='THERMAL_DILATATION_COEFFICIENT_2'
         lchaine=32
         call f_add_double_property(iext_ppset_z,chaine,lchaine,alpha(2))
                !12345678901234567890123456789012
         chaine='THERMAL_DILATATION_COEFFICIENT_3'
         lchaine=32
         call f_add_double_property(iext_ppset_z,chaine,lchaine,alpha(3))

       case default
         call FATERR(IAM,'unsupported anisotropy')        
       end select
              !123456789012345678901
       chaine='INITIAL_TEMPERATURE'
       lchaine=19
       call f_add_double_property(iext_ppset_z,chaine,lchaine,Tref_meca)

       !         !123456789012345678901
       !chaine='REFERENCE_TEMPERATURE'
       !lchaine=21
       !call f_add_double_property(iext_ppset_z,chaine,lchaine,Tref_meca)

     endif

     if (get_therm_effect(ibehav) /= 0) then

       sphv = get_sphv(ibehav)
       coco = get_coco(ibehav)       

              !12345678901234567
       chaine='SPECIFIC_CAPACITY'
!!       chaine='VOLUMIC_CAPACITY'
       lchaine=16
       call f_add_double_property(iext_ppset_z,chaine,lchaine,sphv)

              !123456789012345678901234
       chaine='CONDUCTIVITY_COEFFICIENT'
       lchaine=24
       call f_add_double_property(iext_ppset_z,chaine,lchaine,coco)

     endif

     if (get_therm_varia(ibehav) /= 0) then

       Tref_ther = get_Tref_ther(ibehav)
       Tini_ther = get_Tini_ther(ibehav)

       if (get_therm_cpl(ibehav) == 0) then
                !123456789012345678901
         chaine='REFERENCE_TEMPERATURE'
         lchaine=21
         call f_add_double_property(iext_ppset_z,chaine,lchaine,Tref_ther)
       else
         if (Tref_meca /= Tref_ther) then
           call FATERR(IAM,'Tref in cplt: should be the same as Tref in varf')        
         endif

       endif

              !1234567890123456789
       chaine='INITIAL_TEMPERATURE'
       lchaine=19
       call f_add_double_property(iext_ppset_z,chaine,lchaine,Tini_ther)

       if (Tref_meca /= Tini_ther) then
           call FATERR(IAM,'Tref in cplt: should be the same as Tini in varf:')        
         endif

     endif

   if (itchatche) call LOGMES('Leaving : '//IAM)

 end subroutine push_behaviour
!------------------------------------------------------------------------ 
 subroutine check_ppset(ippset)

   implicit none

   integer :: ippset,iext_ppset,imodel,inull
   integer*4 :: iext_ppset_z,imodel_z,lchaine
   character(len=80) :: chaine

                            !123456789012345678901234567
   character(len=27) :: IAM='ExternalModels::check_ppset'

   if (itchatche) call LOGMES('Entering : '//IAM)

   iext_ppset = external_ppset(ippset)

   !  pour recuperer le num de behav
   call get_ppset_value(ippset,imodel,inull)

   iext_ppset_z = int(iext_ppset,4)
   imodel_z=int(imodel,4)

   if (itchatche) then
     print*,'On lie et on teste ',iext_ppset_z,imodel_z
     chaine=' '
     lchaine=0
   else
     chaine='null'
     lchaine=4
   endif
   call f_check_properties(iext_ppset_z,imodel_z,chaine,lchaine)


   if (itchatche) call LOGMES('Leaving : '//IAM)

 end subroutine check_ppset
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 SUBROUTINE compute_external_pg(ppsnb,extP_lbl,extP_len,ivalue, &
                                extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,H,calcD)

  use timer, only: get_new_etimer_ID, &
                   start_etimer     , &
                   stop_etimer

  ! zone de stockage: gradient,flux,internal,operateur tangent
  
  real(kind=8),dimension(:)  :: GRAD0,FLUX0,INTERNAL0
  real(kind=8),dimension(:)  :: GRAD1,FLUX1,INTERNAL1
  real(kind=8),dimension(:),allocatable :: De
  real(kind=8),dimension(:,:),pointer   :: D
  real(kind=8) :: H
  
  ! parametres externes
  
  character(len=30),dimension(:) :: extP_lbl
  integer*4        ,dimension(:) :: extP_len
  real(kind=8)     ,dimension(:) :: extP_val
  integer*4                      :: extP_nb, calcD
  
  !fd 
  integer   :: inull,ppsnb  ,mdlnb  ,nb_external
  integer*4 ::       ppsnb_z,mdlnb_z,ivalue
  
  !fd 
  integer, save :: timer_id = 0
  !$omp threadprivate(timer_id)

                                                   !12345678901234567890
  if( timer_id == 0 ) timer_id = get_new_etimer_ID('[MATLIB] compute pg ')
  call start_etimer(timer_id)

  call get_ppset_value(ppsnb,mdlnb,inull)
  
  nb_external = get_nb_external_variables(mdlnb)

  ppsnb_z = int(external_ppset(ppsnb),4)  
  mdlnb_z = int(mdlnb,4)

  allocate(De(nb_external*nb_external))
  De = 0.d0

  !print*,ppsnb_z,mdlnb_z
  !print*,extP_lbl,extP_len
  !print*,ivalue
  !print*,extP_val,extP_nb
  !print*,GRAD0
  !print*,FLUX0

  call f_update_state(ppsnb_z,mdlnb_z,extP_lbl,extP_len,ivalue,extP_val,extP_nb, &
                      GRAD0,FLUX0,INTERNAL0, &
                      GRAD1,FLUX1,INTERNAL1, &
                      H,De,calcD)

  !print*,GRAD1
  !print*,FLUX1

  if (  calcD == 1 ) then
    IF(ASSOCIATED(D)) THEN ; DEALLOCATE(D) ; NULLIFY(D) ; ENDIF
    allocate(D(nb_external,nb_external))
    D = reshape(De,(/ nb_external,nb_external/))
  endif

  deallocate(De)

  call stop_etimer(timer_id)

 END SUBROUTINE

 subroutine set_ortho_frame(ippset,frame)
   implicit none

   integer     ,intent(in) :: ippset
   real(kind=8),intent(in) :: frame(3,3)

   real(kind=8) :: vec1(3),vec2(3)
   integer*4    :: ppsnb_z,mdlnb_z
   integer      :: mdlnb,inull

   call get_ppset_value(ippset,mdlnb,inull)

   ppsnb_z = int(external_ppset(ippset),4)  
   mdlnb_z = int(mdlnb,4)

   !print*,'---ortho frame ----'
   !print*,ppsnb_z,mdlnb_z
   !print*,frame(1:3,1)
   !print*,frame(1:3,2)
 
!   vec1 = frame(1:3,1); vec2 = frame(1:3,2)
   vec1 = frame(1,1:3); vec2 = frame(2,1:3)
   call f_rotate_properties(ppsnb_z,mdlnb_z,vec1,vec2)

 end subroutine

 subroutine clean_memory()
   implicit none
   if( allocated(external_ppset) ) deallocate(external_ppset)
 end subroutine

end module
