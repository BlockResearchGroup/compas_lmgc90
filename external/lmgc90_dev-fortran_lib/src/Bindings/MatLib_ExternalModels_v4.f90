!===========================================================================
!
! Copyright 2000-2023 CNRS-UM.
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
                             get_user_mat_filename
  
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

  public push_model,push_behaviour,check_ppset,compute_external_gp,set_ortho_frame

 contains
   
!------------------------------------------------------------------------
  subroutine push_model(imodel,itchatche)

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
  logical   :: itchatche

  integer*4 :: i,imodelz,itmp,zdim,lchaine
  
  character(len=80) :: chaine

  character(len=5)  :: kinematic,formulation,material,anisotropy
  character(len=5)  :: cplth

                            !123456789012345678901234567 
  character(len=27)  :: IAM='models::load_external_model'

  if (itchatche) call logmes('Entering : '//IAM)

  zdim=int(nbDIME,4)
  imodelz=int(imodel,4)

  if (itchatche) print*,'dimension: ',zdim,' num de modele: ',imodelz

  if (modelz(imodel)%is_a_user_model) then
    chaine=trim(modelz(imodel)%user_model_name)
    lchaine=len_trim(modelz(imodel)%user_model_name)
  else

    ! attention ici POROx ne gere que la partie mecanique le reste est rajoute apres.

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
  call f_matlib_new_constitutive_model(imodelz,chaine,lchaine,zdim)
 
  if (itchatche) print*,'ok'

  ! partie qui concerne les variables "externes" : gradients et flux
  ! rq: il peut y avoir des variables regroupees avec les variables classiques (bundled) 
  
  call f_matlib_n_external_variables(imodelz,itmp)
  modelz(imodel)%nb_external_variables = itmp      
  if (itchatche) print*,'nb external ',itmp
 
  call f_matlib_n_ext_variables_bundled(imodelz,itmp)
  modelz(imodel)%nb_external_variables_bundled=itmp
  if (itchatche) print*,'nb external bundled ',itmp  

  ! on va stocker les noms des variables externes
  if( associated(modelz(imodel)%ext_descriptor) ) deallocate(modelz(imodel)%ext_descriptor)
  allocate(modelz(imodel)%ext_descriptor(itmp))

  do i=1,modelz(imodel)%nb_external_variables_bundled
    call f_matlib_type_external_variable(imodelz,i,itmp)
    modelz(imodel)%ext_descriptor(i)%type=itmp
    !
    call f_matlib_index_external_variable(imodelz,i+1,itmp)
    modelz(imodel)%ext_descriptor(i)%size=itmp
    !
    call f_matlib_index_external_variable(imodelz,i,itmp)
    modelz(imodel)%ext_descriptor(i)%index=itmp-1
    modelz(imodel)%ext_descriptor(i)%size=modelz(imodel)%ext_descriptor(i)%size-itmp
    !
    call f_matlib_label_external_variable(imodelz,i,chaine,lchaine)
    lchaine=lchaine+1
                                                     
    modelz(imodel)%ext_descriptor(i)%label_primal=' '
    modelz(imodel)%ext_descriptor(i)%label_primal(1:lchaine)=chaine(1:lchaine)

    if (itchatche) print*,i,modelz(imodel)%ext_descriptor(i)%label_primal(1:lchaine),lchaine

    !
    call f_matlib_label_external_force(imodelz,i,chaine,lchaine)
    lchaine=lchaine+1
                                                           
    modelz(imodel)%ext_descriptor(i)%label_dual=' '
    modelz(imodel)%ext_descriptor(i)%label_dual(1:lchaine)=chaine(1:lchaine)

    if (itchatche) print*,i,modelz(imodel)%ext_descriptor(i)%label_dual(1:lchaine),lchaine

  enddo

  ! partie qui concerne les variables "internes" du modele
  ! rq: il peut y avoir des variables regroupees avec les variables classiques (bundled) 
  
  call f_matlib_n_internal_variables(imodelz,itmp)
  modelz(imodel)%nb_internal_variables = itmp      
  if (itchatche) print*,'nb internal ',itmp

  call f_matlib_n_int_variables_bundled(imodelz,itmp)
  modelz(imodel)%nb_internal_variables_bundled=itmp
  if (itchatche) print*,'nb internal bundled ',itmp
  
  if( associated(modelz(imodel)%int_descriptor) ) deallocate(modelz(imodel)%int_descriptor)
  allocate(modelz(imodel)%int_descriptor(itmp))

  do i=1,modelz(imodel)%nb_internal_variables_bundled
    call f_matlib_type_internal_variable(imodelz,i,itmp)
    modelz(imodel)%int_descriptor(i)%type=itmp
    !
    call f_matlib_index_internal_variable(imodelz,i+1,itmp)
    modelz(imodel)%int_descriptor(i)%size=itmp
    !
    call f_matlib_index_internal_variable(imodelz,i,itmp)
    modelz(imodel)%int_descriptor(i)%index=itmp-1
    modelz(imodel)%int_descriptor(i)%size=modelz(imodel)%int_descriptor(i)%size-itmp
    !
    call f_matlib_label_internal_variable(imodelz,i,chaine,lchaine)
    lchaine=lchaine+1
                                                      
    modelz(imodel)%int_descriptor(i)%label_primal =' '
    modelz(imodel)%int_descriptor(i)%label_primal(1:lchaine)=chaine(1:lchaine)

    if (itchatche) print*,i,modelz(imodel)%int_descriptor(i)%label_primal(1:lchaine),lchaine

    !fd y en a pas
    ! call f_matlib_label_internal_force(imodelz,i,chaine,lchaine)
    !                                                             !123456789012345678901234567890 
    ! modelz(imodel)%int_descriptor(i)%label_dual(1:lchaine)='                              '
    ! modelz(imodel)%int_descriptor(i)%label_dual(1:lchaine)=chaine(1:lchaine)
  enddo

  if (itchatche) then
  
    print*,'-------------------------------------------'

    print*,'model : ',imodel
    print*,' -- number of external variables : ', modelz(imodel)%nb_external_variables_bundled
    do i=1,modelz(imodel)%nb_external_variables_bundled
      print*,' --- ',trim(modelz(imodel)%ext_descriptor(i)%label_primal),' | ',trim(modelz(imodel)%ext_descriptor(i)%label_dual)
    enddo
    print*,' -- number of internal variables : ', modelz(imodel)%nb_internal_variables_bundled  
    do i=1,modelz(imodel)%nb_internal_variables_bundled
      print*,' --- ',trim(modelz(imodel)%int_descriptor(i)%label_primal) !,modelz(imodel)%int_descriptor(i)%label_dual
    enddo
  
    print*,'-------------------------------------------'
    
  endif 

  if (itchatche) call logmes('Leaving : '//IAM)

 end subroutine push_model
!------------------------------------------------------------------------

!------------------------------------------------------------------------
 subroutine push_behaviour(iext_ppset,ibehav,itchatche,density,elas_coeff)

   !fd attention new ibehav est remplace par le numero de ppset externe 
   !
   implicit none   

                            !123456789012345678901234567890123456789
   character(len=37) :: IAM='MatLib_ExternalModels::push_behaviour'

   integer          :: ibehav,iext_ppset
   logical          :: itchatche

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


   real(kind=8),optional :: density,elas_coeff(2) 
   
   !fd  user mat

   integer           :: size,type,isz,ival
   character(len=50) :: name
   real(kind=8)      :: rval


   if (itchatche) call LOGMES('Entering : '//IAM)


   ! connerie de conversion
   iext_ppset_z=int(iext_ppset,4)


   if (itchatche) print*,'iext_ppset=',iext_ppset
   if (itchatche) print*,'ibehav=',ibehav
     
   !fd passage d'un user_mat 

   if ( get_is_a_user_mat(ibehav) ) then
      if (itchatche) call LOGMES('Loading a user mat')

      chaine = get_user_mat_filename(ibehav)
      lchaine=len_trim(chaine)

      call f_matlib_new_material_data_from(iext_ppset_z,chaine,lchaine)

     return
   endif

   ! ----
   
   !fd
   chaine(1:5)=get_bulk_behav_ID(ibehav)
   lchaine=5
   call f_matlib_new_material_data(iext_ppset_z,chaine,lchaine)

          !123456789012 
   chaine='MASS_DENSITY'
   lchaine=12
   if (present(density)) then
      rho=density
   else   
      rho=get_rho(ibehav)
   endif
   
   call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,rho)

   
   !fd chargement de l'elasticite

   select case (get_elas_model(ibehav))
   case(0) ! rigide
     call LOGMES(IAM//'rigid material unsupported')        

   case(1) !elastique|neohookeen|hyper

     if (present(elas_coeff)) then
       coeff=0.d0    
       coeff(1:2)=elas_coeff
       anisotropy = 0      
     else    
       call get_elas_coeff(ibehav,anisotropy,coeff)
     endif
     
     select case (anisotropy)

     case(0) ! isotrope
             !1234567890123 
       chaine='YOUNG_MODULUS'         
       lchaine=13
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))

              !1234567890123456789 
       chaine='POISSON_COEFFICIENT'         
       lchaine=19
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))

     case(1) ! orthotrope
                !123456789012345 
       chaine='YOUNG_MODULUS_1'         
       lchaine=15
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))
              !123456789012345 
       chaine='YOUNG_MODULUS_2'         
       lchaine=15
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))
              !123456789012345 
       chaine='YOUNG_MODULUS_3'         
       lchaine=15
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(3))
              !1234567890123456789012 
       chaine='POISSON_COEFFICIENT_12'         
       lchaine=22
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(4))
              !1234567890123456789012 
       chaine='POISSON_COEFFICIENT_13'         
       lchaine=22
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(5))
              !1234567890123456789012 
       chaine='POISSON_COEFFICIENT_23'         
       lchaine=22
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(6))
           !1234567890123456
       chaine='SHEAR_MODULUS_12'         
       lchaine=16
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(7))
              !1234567890123456
       chaine='SHEAR_MODULUS_13'         
       lchaine=16
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(8))
              !1234567890123456
       chaine='SHEAR_MODULUS_23'         
       lchaine=16
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(9))
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
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))

              !123456789012345678901234567 
       chaine='VISCOUS_POISSON_COEFFICIENT'         
       lchaine=27
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))
     case(1) ! orthotrope

              !123456789012345678901234 
       chaine='VISCOUS_YOUNG_MODULUS_1'         
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(1))

              !123456789012345678901234 
       chaine='VISCOUS_YOUNG_MODULUS_2'         
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(2))

              !123456789012345678901234 
       chaine='VISCOUS_YOUNG_MODULUS_3'         
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(3))

              !123456789012345678901234567890 
       chaine='VISCOUS_POISSON_COEFFICIENT_12'         
       lchaine=30
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(4))

              !123456789012345678901234567890 
       chaine='VISCOUS_POISSON_COEFFICIENT_13'         
       lchaine=30
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(5))

              !123456789012345678901234567890 
       chaine='VISCOUS_POISSON_COEFFICIENT_23'         
       lchaine=30
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(6))

              !123456789012345678901234
       chaine='VISCOUS_SHEAR_MODULUS_12'         
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(7))

              !123456789012345678901234
       chaine='VISCOUS_SHEAR_MODULUS_13'         
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(8))

              !123456789012345678901234
       chaine='VISCOUS_SHEAR_MODULUS_23'         
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coeff(9))

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
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,isoh_coeff(1))

              !12345678901234567 
       chaine='HARDENING_MODULUS'
       lchaine=17
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,isoh_coeff(2))

              !123456789012345678901234567 
       chaine='KINEMATIC_HARDENING_MODULUS' 
       lchaine=27
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,cinh_coeff(1))

              !1234567890123456 
       chaine='REFERENCE_STRESS'
       lchaine=16
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,vplas_coeff(1))

              !123456789012345678901 
       chaine='REFERENCE_STRAIN_RATE' 
       lchaine=21
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,vplas_coeff(2))

              !123456789012345678901234 
       chaine='RATE_DEPENDENCY_EXPONENT'
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,vplas_coeff(3))
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
         call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,alpha(1))
       case(1)
                !12345678901234567890123456789012
         chaine='THERMAL_DILATATION_COEFFICIENT_1'
         lchaine=32
         call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,alpha(1))
                !12345678901234567890123456789012
         chaine='THERMAL_DILATATION_COEFFICIENT_2'
         lchaine=32
         call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,alpha(2))
                !12345678901234567890123456789012
         chaine='THERMAL_DILATATION_COEFFICIENT_3'
         lchaine=32
         call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,alpha(3))

       case default
         call FATERR(IAM,'unsupported anisotropy')        
       end select
              !123456789012345678901
       chaine='INITIAL_TEMPERATURE'
       lchaine=19
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,Tref_meca)

       !         !123456789012345678901
       !chaine='REFERENCE_TEMPERATURE'
       !lchaine=21
       !call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,Tref_meca)

     endif

     if (get_therm_effect(ibehav) /= 0) then

       sphv = get_sphv(ibehav)
       coco = get_coco(ibehav)       

              !12345678901234567
       chaine='SPECIFIC_CAPACITY'
!!       chaine='VOLUMIC_CAPACITY'
       lchaine=16
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,sphv)

              !123456789012345678901234
       chaine='CONDUCTIVITY_COEFFICIENT'
       lchaine=24
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,coco)

     endif

     if (get_therm_varia(ibehav) /= 0) then

       Tref_ther = get_Tref_ther(ibehav)
       Tini_ther = get_Tini_ther(ibehav)

       if (get_therm_cpl(ibehav) == 0) then
                !123456789012345678901
         chaine='REFERENCE_TEMPERATURE'
         lchaine=21
         call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,Tref_ther)
       else
         if (Tref_meca /= Tref_ther) then
           call FATERR(IAM,'Tref in cplt: should be the same as Tref in varf')        
         endif

       endif

              !1234567890123456789
       chaine='INITIAL_TEMPERATURE'
       lchaine=19
       call f_matlib_add_double_property(iext_ppset_z,chaine,lchaine,Tini_ther)

       if (Tref_meca /= Tini_ther) then
           call FATERR(IAM,'Tref in cplt: should be the same as Tini in varf:')        
         endif

     endif

   if (itchatche) call LOGMES('Leaving : '//IAM)

 end subroutine push_behaviour
!------------------------------------------------------------------------  

!------------------------------------------------------------------------ 
 subroutine check_ppset(iext_ppset,imodel,itchatche)

   implicit none

   integer :: iext_ppset,imodel
   logical :: itchatche
   integer*4 :: iext_ppset_z,imodel_z,lchaine
   character(len=80) :: chaine

                            !123456789012345678901234567
   character(len=27) :: IAM='ExternalModels::check_ppset'

   if (itchatche) call LOGMES('Entering : '//IAM)

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
   call f_matlib_check_properties(iext_ppset_z,imodel_z,chaine,lchaine)


   if (itchatche) call LOGMES('Leaving : '//IAM)

 end subroutine check_ppset
!------------------------------------------------------------------------
 
!------------------------------------------------------------------------
 SUBROUTINE compute_external_gp(ppsnb,mdlnb,extP_lbl,extP_len,ivalue, &
                                extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,H,calcD)

  use timer, only: get_new_etimer_ID, &
                   start_etimer     , &
                   stop_etimer
  
  
  ! zone de stockage: gradient,flux,internal,operateur tangent
  
  real(kind=8),dimension(:)             :: GRAD0,FLUX0,INTERNAL0
  real(kind=8),dimension(:)             :: GRAD1,FLUX1,INTERNAL1
  real(kind=8),dimension(:),allocatable :: De
  real(kind=8),dimension(:,:),pointer   :: D
  real(kind=8) :: H
  
  ! parametres externes
  
  character(len=30),dimension(:) :: extP_lbl
  integer(kind=4)  ,dimension(:) :: extP_len
  real(kind=8)     ,dimension(:) :: extP_val
  integer(kind=4)                :: extP_nb, calcD
  
  !fd 
  integer         :: ppsnb  ,mdlnb  ,nb_external
  integer(kind=4) :: ppsnb_z,mdlnb_z,ivalue
  
  !fd 
  integer, save :: timer_id = 0
  !$omp threadprivate(timer_id)
                                                   !12345678901234567890
  if( timer_id == 0 ) timer_id = get_new_etimer_ID('[MATLIB] compute pg ')
  call start_etimer(timer_id)

  
  nb_external = get_nb_external_variables(mdlnb)

  ppsnb_z = int(ppsnb,4)  
  mdlnb_z = int(mdlnb,4)

  allocate(De(nb_external*nb_external))
  De = 0.d0

  call f_matlib_update_state(ppsnb_z,mdlnb_z,extP_lbl,extP_len,ivalue,extP_val,extP_nb, &
                      GRAD0,FLUX0,INTERNAL0, &
                      GRAD1,FLUX1,INTERNAL1, &
                      H,De,calcD)
  
  if (  calcD == 1 ) then
    IF(ASSOCIATED(D)) THEN ; DEALLOCATE(D) ; NULLIFY(D) ; ENDIF
    allocate(D(nb_external,nb_external))
    D = reshape(De,(/ nb_external,nb_external/))
  endif

  deallocate(De)

  call stop_etimer(timer_id)

 END SUBROUTINE
 !------------------------------------------------------------------------
 
 !------------------------------------------------------------------------
 subroutine set_ortho_frame(ppsnb,mdlnb,frame)
   implicit none

   integer     ,intent(in) :: ppsnb,mdlnb
   real(kind=8),intent(in) :: frame(3,3)

   real(kind=8)            :: vec1(3),vec2(3)
   integer(kind=4)         :: ppsnb_z,mdlnb_z

   ppsnb_z = int(ppsnb,4)  
   mdlnb_z = int(mdlnb,4)

   vec1 = frame(1,1:3); vec2 = frame(2,1:3)
   call f_matlib_rotate_properties(ppsnb_z,mdlnb_z,vec1,vec2)

 end subroutine


end module MatLibExternalModels

