!===========================================================================
!
! COPYRIGHT 2000-2025 CNRS-UM.
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
MODULE models

!todo passage en private, URGENT !!

  USE overall
  USE utilities
 
  use bulk_behaviour, only : get_nb_bulk_behav, &
                             get_coco         , &
                             get_elas_coeff   , &
                             get_joint_param  , &
                             get_joint_param_size

  IMPLICIT NONE

  ! a cause de modelz ... a torcher private
  ! fd 21/04/08
  TYPE T_model_options
     CHARACTER(len=5 ),DIMENSION(:),POINTER ::ID
     CHARACTER(len=30),DIMENSION(:),POINTER ::value
  END TYPE T_model_options
  
  TYPE T_model_r_param
     CHARACTER(len=5 ),DIMENSION(:),POINTER ::ID
     real(kind=8),DIMENSION(:),POINTER ::value
  END TYPE T_model_r_param


  TYPE T_model_descriptor
     ! type des variables stockees (0 inconnu, 1 scalaire, 2 vecteur, &
     !                              3 tenseur sym, 4 tenseur nonsym)   
     INTEGER :: TYPE
     ! index 
     INTEGER :: index
     ! size
     INTEGER :: size
     ! labels variable primale & dual
     CHARACTER(len=80) :: label_primal, label_dual
  END TYPE T_model_descriptor
  
  TYPE T_model
     CHARACTER(len=5)      :: model ! nickname
     CHARACTER(len=5)      :: mdlty ! MECAx, THERMx, ...
     CHARACTER(len=5)      :: ID    ! T3xxx, ...
     TYPE(T_model_options) :: optns ! options
     
     ! taille du conteneur de stockage pour variables externes & internes
     INTEGER               :: nb_external_variables
     INTEGER               :: nb_internal_variables   
     
     ! nombre de variables a proprement parler
     INTEGER               :: nb_external_variables_bundled
     INTEGER               :: nb_internal_variables_bundled   
     
     ! descriptions des variables
     TYPE(T_model_descriptor), DIMENSION(:),POINTER:: ext_descriptor
     TYPE(T_model_descriptor), DIMENSION(:),POINTER:: int_descriptor
     
     !fd on donne explicitement le nom du modele
     LOGICAL           :: is_a_user_model
     CHARACTER(LEN=50) :: user_model_name

     !fd gestion des fields

     integer :: nb_fields
     character(len=30),dimension(:),pointer:: field_name 

     integer :: nb_vfields, vfield_max_size
     character(len=30),dimension(:),pointer:: vfield_name 

  END TYPE T_model
  
  TYPE(T_model),DIMENSION(:),ALLOCATABLE :: modelz
  
  !fd new 22/05/06 
  !fd gestion acces model+behav par numero unique
  !fd va permettre de mieux profiter de matlib quand materiaux couples

  INTEGER,DIMENSION(:,:),ALLOCATABLE :: check_ppset_map

  TYPE T_ppset 
   INTEGER :: mdlnb,lawnb
  END TYPE T_ppset 

  TYPE(T_ppset),DIMENSION(:),ALLOCATABLE :: ppset

  TYPE T_link_ppset                                 ! liste chainée pour attribution ppset
    TYPE(T_link_ppset), POINTER :: p                ! pointeur sur le precedent
    TYPE(T_ppset)               :: val              ! les valeurs
    TYPE(T_link_ppset), POINTER :: n                ! pointeur sur le suivant
  END TYPE T_link_ppset
 
  TYPE(T_link_ppset),POINTER                    :: Root,Current,Previous

  INTEGER :: nb_ppset=0


  INTERFACE get_eleop_value
    MODULE PROCEDURE get_eleop_value_by_name, & 
                     get_eleop_value_by_id
  END INTERFACE


  ! wrap API
  
  PUBLIC read_models,write_models,init_models,store_ppset
  
  ! internal API
  
  PUBLIC get_nb_models,get_nb_external_variables,get_nb_internal_variables,get_nb_internal_variables_bundled, &
         get_eleop_id, get_eleop_value, get_ppset_nb,  &
         get_external_field_nb, get_external_field_name, &
         get_external_nb_vfield, get_external_vfield_name, get_external_vfield_max_size, &
         set_nb_models, set_model, get_model_nb, &
         clean_memory

! meca

  PUBLIC D_SOLID_ISO, comp_stress, D_SOLID_SHB, comp_stress_shb
  PUBLIC D_JOINT, comp_stress_joint_elas, comp_stress_joint_MC


! ther 

  PUBLIC COCO_ISO

CONTAINS

!------------------------------------------------------------------------
SUBROUTINE read_models

   IMPLICIT NONE

   INTEGER :: imodel,ioptns
   INTEGER :: errare,itest,vsize
   CHARACTER(len=5) :: ctempo
   CHARACTER(len=103) :: cout
 !                             1234567890123456789 
   CHARACTER(len=19)  :: IAM='models::read_models'

  ! first reading the models file

   G_nfich = get_io_unit()
   OPEN(unit=G_nfich,file=TRIM(location(in_models(:))))  

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! first reading: sizing array of models  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   imodel=0
   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) == 'model') imodel=imodel+1    ! fishing for the keyword 'model'
   ENDDO
   REWIND(G_nfich)

    WRITE(cout,'(I0,1x,A)') imodel,'models found'
    CALL LOGMES(cout)
    CALL LOGMES('--')


   ALLOCATE(modelz(imodel),stat=errare)
   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating model')
   END IF

   !print*,'nb models:',imodel

   imodel=0
   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) == 'model') THEN   ! fishing for the keyword 'model'
       imodel=imodel+1    

       !fd 16/06/04 nouveau pour la gestion dans la matlib
       modelz(imodel)%nb_external_variables_bundled=0
       modelz(imodel)%nb_internal_variables_bundled=0
       NULLIFY(modelz(imodel)%ext_descriptor,modelz(imodel)%int_descriptor)

       !fd gestion fields 28/01/09
       modelz(imodel)%nb_fields=-1
       NULLIFY(modelz(imodel)%field_name)

       modelz(imodel)%nb_vfields=-1
       NULLIFY(modelz(imodel)%vfield_name)

       !fd 16/04/08 adaptation pour lecture u_mdl
       DO 
         IF( .NOT. read_G_clin()) EXIT
         IF(G_clin(2:6) == 'model') EXIT
         IF(G_clin(2:6) /= '     ') modelz(imodel)%model=G_clin(2:6)
         IF(G_clin(2:6) == '     ' .AND. G_clin(9:13) /= '     ') THEN
           modelz(imodel)%mdlty=G_clin(9:13)
           modelz(imodel)%ID   =G_clin(16:20)
           IF (G_clin(23:27) == '    ') THEN 
             ioptns=0
             NULLIFY(modelz(imodel)%optns%ID)
             NULLIFY(modelz(imodel)%optns%value)
           ELSE
             modelz(imodel)%is_a_user_model=.FALSE.
             if (G_clin(23:27) == 'u_mdl') then
               !fd apres  modelz(imodel)%is_a_user_model=.TRUE.
               !fd apres  read(G_clin(30:79),*) modelz(imodel)%user_model_name
               ioptns=0
             else
               ioptns=1
             endif
             !fd a la peche a la suite des options
             DO 
               IF( .NOT. read_G_clin()) EXIT
               IF(G_clin(2:6) == 'model') EXIT
               IF(G_clin(2:6) == '     ' .AND. G_clin(9:13) /= '     ') THEN
                 CALL FATERR(IAM,'error reading models')
               ENDIF
               IF (G_clin(23:27) /= '    ') ioptns=ioptns+1
             ENDDO
             BACKSPACE(G_nfich)

             ALLOCATE(modelz(imodel)%optns%ID(ioptns),stat=errare)
             IF (errare /= 0) THEN
               CALL FATERR(IAM,'error allocating modelz%optns%ID')
             END IF

             ALLOCATE(modelz(imodel)%optns%value(ioptns),stat=errare)
             IF (errare /= 0) THEN
               CALL FATERR(IAM,'error allocating modelz%optns%value')
             END IF
           ENDIF 

           !print*,'model',imodel,'nb options',ioptns

         ENDIF
       ENDDO
       BACKSPACE(G_nfich)
     ENDIF
   ENDDO
   REWIND(G_nfich)

   !fd 16/04/08 adaptation pour lecture u_mdl
   imodel=0
   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) == 'model') THEN
       imodel=imodel+1    ! fishing for the keyword 'model'
       DO 
         IF( .NOT. read_G_clin()) EXIT
         IF(G_clin(2:6) == 'model') EXIT
         IF(G_clin(2:6) == '     ' .AND. G_clin(9:13) /= '     ') THEN
           ioptns=0
           IF (G_clin(23:27) == '    ') THEN 
             ioptns=0
           ELSE
             modelz(imodel)%is_a_user_model=.FALSE.
             if (G_clin(23:27) == 'u_mdl') then
               modelz(imodel)%is_a_user_model=.TRUE.
               read(G_clin(30:79),*) modelz(imodel)%user_model_name
               ioptns=0
             else
               ioptns=1
               modelz(imodel)%optns%ID(ioptns)   =G_clin(23:27)
               !fd 21/04/08  modelz(imodel)%optns%value(ioptns)=G_clin(30:34)
               modelz(imodel)%optns%value(ioptns)=G_clin(30:59)
             endif
             modelz(imodel)%vfield_max_size = 0
             DO 
               IF( .NOT. read_G_clin()) EXIT
               IF(G_clin(2:6) == 'model') EXIT
               IF(G_clin(2:6) == '     ' .AND. G_clin(9:13) /= '     ') EXIT
               IF (G_clin(23:27) /= '    ') THEN
                 ioptns=ioptns+1
                 modelz(imodel)%optns%ID(ioptns)   =G_clin(23:27)
                 !fd 21/04/08 modelz(imodel)%optns%value(ioptns)=G_clin(30:34)
                 modelz(imodel)%optns%value(ioptns)=G_clin(30:59)
                 
                 if (G_clin(61:65) /= '    ') then
                   read(G_clin(61:65),'(I5)') vsize
                   modelz(imodel)%vfield_max_size = max(modelz(imodel)%vfield_max_size,vsize)
                 end if
               ENDIF
             ENDDO
             BACKSPACE(G_nfich)
           ENDIF 
         ENDIF
       ENDDO
       BACKSPACE(G_nfich)
     ENDIF
   ENDDO
   CLOSE(G_nfich)

   !rm : ugly loop to change ext_f option name
   do imodel = 1, size(modelz)
     if (.not. associated(modelz(imodel)%optns%ID)) cycle
     do ioptns = 1, size(modelz(imodel)%optns%ID)
       if( modelz(imodel)%optns%ID(ioptns) == 'ext_f' ) then
           modelz(imodel)%optns%ID(ioptns) = 'extsf'
       end if
     end do
   end do
 END SUBROUTINE read_models
!------------------------------------------------------------------------ 
 SUBROUTINE write_models

   IMPLICIT NONE

   INTEGER :: imodel,ioptns
   INTEGER :: errare,itest
   CHARACTER(len=5) :: ctempo
   CHARACTER(len=103) :: cout

   IF (.NOT. ALLOCATED(modelz)) RETURN

   G_nfich = get_io_unit()
   OPEN(unit=G_nfich,file=TRIM(location(out_models(:))))  

   DO imodel=1,SIZE(modelz)

     WRITE(G_nfich,10)
10   FORMAT('$model  mdlty  finel  eleop  value  ')
     WRITE(G_nfich,11) modelz(imodel)%model
11   FORMAT(1x,A5,'                      ')
     IF ( .NOT. ASSOCIATED(modelz(imodel)%optns%ID)) THEN
         WRITE(G_nfich,12) modelz(imodel)%mdlty,modelz(imodel)%ID,'     ','     '
       ELSE
         !fd 16/04/08 adaptation pour ecriture u_mdl
         if (modelz(imodel)%is_a_user_model) then
           WRITE(G_nfich,13) modelz(imodel)%mdlty,modelz(imodel)%ID, &
                             'u_mdl',modelz(imodel)%user_model_name
           DO ioptns=1,SIZE(modelz(imodel)%optns%ID)
             if( modelz(imodel)%optns%ID(ioptns) == 'extvf' ) then
               write(G_nfich,14) '     ','     ', &
                                 modelz(imodel)%optns%ID(ioptns),modelz(imodel)%optns%value(ioptns),modelz(imodel)%vfield_max_size
             else
               write(G_nfich,12) '     ','     ', &
                                 modelz(imodel)%optns%ID(ioptns),modelz(imodel)%optns%value(ioptns)
             end if
           ENDDO

         else
           ioptns=1
           WRITE(G_nfich,12) modelz(imodel)%mdlty,modelz(imodel)%ID, &
                             modelz(imodel)%optns%ID(ioptns),modelz(imodel)%optns%value(ioptns)
           DO ioptns=2,SIZE(modelz(imodel)%optns%ID)
             if( modelz(imodel)%optns%ID(ioptns) == 'extvf' ) then
               write(G_nfich,14) '     ','     ', &
                                 modelz(imodel)%optns%ID(ioptns),modelz(imodel)%optns%value(ioptns),modelz(imodel)%vfield_max_size
             else
               write(G_nfich,12) '     ','     ', &
                                 modelz(imodel)%optns%ID(ioptns),modelz(imodel)%optns%value(ioptns)
             end if
           ENDDO
         endif
       ENDIF
   ENDDO
   CLOSE(G_nfich)

 12  FORMAT(1x,5x,2x,A5,2x,A5,2x,A5,2x,A30)
 13  FORMAT(1x,5x,2x,A5,2x,A5,2x,A5,2x,A50)
 14  FORMAT(1x,5x,2x,A5,2x,A5,2x,A5,2x,A30,1x,I5)

 END SUBROUTINE write_models
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE init_models

   IMPLICIT NONE

   INTEGER :: imodel
   INTEGER :: errare
   CHARACTER(len=5) :: kinematic,formulation,isext,discrete,mater
   CHARACTER(len=103) :: cout
 !                             1234567890123456789 
   CHARACTER(len=19)  :: IAM='models::init_models'
   integer :: id

   ! some preliminaries dimensionings  

   DO imodel=1,SIZE(modelz)

     !fd pour les elements geres par une librairie EF externe
     if (modelz(imodel)%ID(1:3) == 'EXT' .or. &
         modelz(imodel)%ID(1:3) == 'Rxx'     ) then
         modelz(imodel)%nb_external_variables=0
         modelz(imodel)%nb_internal_variables=0
         cycle
     endif

     ! pour un modele is_ext c'est gere dans Bindings

     isext = get_eleop_value(imodel,'isext') 

     IF (isext /= 'no___') THEN        
       write(cout,'(A,1x,I0,1x,A)') 'model:',imodel,'is external'
       call logmes(cout)
       cycle
     ENDIF

     id = get_eleop_id(imodel,'discr') 
     if ( id /= 0) then
       if (get_eleop_value(imodel,id) == 'yes__') then
         modelz(imodel)%nb_external_variables=0
         modelz(imodel)%nb_internal_variables=0
         cycle
       endif
     endif

     SELECT CASE(modelz(imodel)%mdlty)
       CASE('MECAx')

         ! joint element 
         if (modelz(imodel)%ID(1:1) == 'J') then

           if (modelz(imodel)%ID(5:5) == '2') then 
              if (nbdime /= 2) call faterr(IAM,'bad dim')              
               ! nb_external = taille du vecteur contrainte
               modelz(imodel)%nb_external_variables=2
               ! nb_internal = taille des variables internes au pg
               mater = get_eleop_value(imodel,'mater')
               if (mater == 'JELAS') then
                 ! coefint + Q 
                 modelz(imodel)%nb_internal_variables=1+nbdime*nbdime
               else if (mater == 'J__MC') then
                 ! nb_internal = taille des variables internes au pg
                 ! 3 (defo plastique) + 4 (var interne) + coefint + Q
                 modelz(imodel)%nb_internal_variables=8+nbdime*nbdime
               else if (mater == 'JFCZM') then
                 ! nb_internal = taille des variables internes au pg
                 ! 3 (defo plastique) + 4 (var) + coefint + Q
                 modelz(imodel)%nb_internal_variables=7+nbdime*nbdime
               else  
                 call faterr(IAM,'unknown meterial')
               endif   
           else if (modelz(imodel)%ID(5:5) == '3') then 
             if (nbdime /= 3) call faterr(IAM,'bad dim') 
               ! nb_external = taille du vecteur contrainte
               modelz(imodel)%nb_external_variables=3
               ! nb_internal = taille des variables internes au pg
               mater = get_eleop_value(imodel,'mater')
               if (mater == 'JELAS') then
                 ! Q 
                 modelz(imodel)%nb_internal_variables=nbdime*nbdime
               else if (mater == 'J__MC') then
                 ! nb_internal = taille des variables internes au pg
                 ! 3 (defo plastique) + 4 (var) + Q
                 modelz(imodel)%nb_internal_variables=7+nbdime*nbdime
               else if (mater == 'JFCZM') then
                 ! nb_internal = taille des variables internes au pg
                 ! 3 (defo plastique) + 4 (var) + Q
                 modelz(imodel)%nb_internal_variables=7+nbdime*nbdime
               else  
                 call faterr(IAM,'unknown material')
              endif   
           endif
           cycle
         endif
         
         SELECT CASE (nbDIME)
           CASE(2) 
             kinematic = get_eleop_value(imodel,'kine_')
             IF (kinematic == 'small') THEN

               ! nb_external = taille du vecteur contrainte
               modelz(imodel)%nb_external_variables=4
               ! nb_internal = taille des variables internes au pg
               modelz(imodel)%nb_internal_variables=0

             ELSE IF (kinematic == 'large') THEN
               formulation=get_eleop_value(imodel,'form_')
               IF (formulation == 'UpdtL') THEN
                 ! nb_external = taille du vecteur contrainte
                 modelz(imodel)%nb_external_variables=4
                 ! nb_internal = taille des variables internes au pg
                 ! on prevoit le max ... a gerer !!
                 modelz(imodel)%nb_internal_variables=6
               ELSE
                 modelz(imodel)%nb_external_variables=0
                 modelz(imodel)%nb_internal_variables=0
                 !
                 call logMes('Unknown form_ option for LMGC90 standalone')
                 call logMes('Either understood by external models or check MODELS.DAT')
                 call FATERR(IAM,'unknown form_ option') 
               ENDIF
             ELSE
               call logMes('Unknown kine_ option Check MODELS.DAT') 
               call FATERR(IAM,'unknown kine_ option') 
             ENDIF
           CASE(3)
             kinematic =get_eleop_value(imodel,'kine_') 
             IF (kinematic == 'small') THEN
               ! nb_external = taille du vecteur contrainte
               modelz(imodel)%nb_external_variables=6
               ! nb_internal = taille des variables internes au pg
               modelz(imodel)%nb_internal_variables=0
             ELSE IF (kinematic == 'large') THEN
               formulation=get_eleop_value(imodel,'form_')
               IF (formulation == 'UpdtL') THEN
                 ! nb_external = taille du vecteur contrainte
                 modelz(imodel)%nb_external_variables=6
                 ! nb_internal = taille des variables internes au pg
                 modelz(imodel)%nb_internal_variables=0
               ELSE
                 !
                 call logMes('Unknown form_ option for LMGC90 standalone')
                 call logMes('Either understood by external models or check MODELS.DAT')
                 call FATERR(IAM,'unknown form_ option') 
                 !
                 modelz(imodel)%nb_external_variables=0
                 ! nb_internal = taille des variables internes au pg
                 modelz(imodel)%nb_internal_variables=0
               ENDIF
             ELSE
               call logMes('Unknown kine_ option Check MODELS.DAT') 
               call FATERR(IAM,'unknown kine_ option') 
             ENDIF
           CASE default
             CALL FATERR(IAM,'unsupported dimension')        
           END SELECT

       CASE('POROx')

         ! il faut etre prudent avec POROx car nb_external et nb_internal sont la uniquement pour gerer
         ! l espace memore au point de Gauss. 

         SELECT CASE (nbDIME)
           CASE(2) 
             kinematic = get_eleop_value(imodel,'kine_')
             IF (kinematic == 'small') THEN

               ! nb_external = taille du vecteur contrainte + flux
               modelz(imodel)%nb_external_variables=4 !+2 <-fd non implicite 
               ! nb_internal = taille des variables internes au pg
               modelz(imodel)%nb_internal_variables=0

             ELSE IF (kinematic == 'large') THEN
               formulation=get_eleop_value(imodel,'form_')
               IF (formulation == 'UpdtL') THEN
                 ! nb_external = taille du vecteur contrainte
                 modelz(imodel)%nb_external_variables=4 !+2 <-fd non implicite
                 ! nb_internal = taille des variables internes au pg
                 ! on prevoit le max ... a gerer !!
                 modelz(imodel)%nb_internal_variables=6+3
               ELSE
                 modelz(imodel)%nb_external_variables=4 !+2 <- non implicite
                 modelz(imodel)%nb_internal_variables=6+3
                 !
                 call logmes('Unknown form_ option for LMGC90 standalone')
                 call logmes('Either understood by external models') 
                 call faterr(IAM,'Or check MODELS.DAT') 
               ENDIF
             ELSE
               call logmes('Unknown kine_ option')
               call faterr(IAM,'Check MODELS.DAT') 
             ENDIF
           CASE(3)
             kinematic =get_eleop_value(imodel,'kine_') 
             IF (kinematic == 'small') THEN
               ! nb_external = taille du vecteur contrainte
               modelz(imodel)%nb_external_variables=6 !+3 <- non implicite
               ! nb_internal = taille des variables internes au pg
               modelz(imodel)%nb_internal_variables=0
             ELSE IF (kinematic == 'large') THEN
               formulation=get_eleop_value(imodel,'form_')
               IF (formulation == 'UpdtL') THEN
                 ! nb_external = taille du vecteur contrainte
                 modelz(imodel)%nb_external_variables=6 !+3 <- non implicit
                 ! nb_internal = taille des variables internes au pg
                 modelz(imodel)%nb_internal_variables=0
               ELSE
                 modelz(imodel)%nb_external_variables=6 !+3 <- non implicit
                 ! nb_internal = taille des variables internes au pg
                 modelz(imodel)%nb_internal_variables=6+3
                 !
                 call logmes('Unknown form_ option for LMGC90 standalone')
                 call logmes('Either understood by external models') 
                 call faterr(IAM,'Or check MODELS.DAT') 
                 !
               ENDIF
             ELSE
               call logmes('Unknown kine_ option')
               call faterr(IAM,'Check MODELS.DAT') 
             ENDIF
           CASE default
             CALL FATERR(IAM,'unsupported dimension')        
           END SELECT

       CASE('MULTI')
         select case (nbDIME)
         case(2) 
           kinematic = get_eleop_value(imodel,'kine_')
           if (kinematic == 'small') then
             modelz(imodel)%nb_external_variables = 4+2+2
             modelz(imodel)%nb_internal_variables = 0

           else if (kinematic == 'large') then
             formulation=get_eleop_value(imodel,'form_')
             !IF (formulation == 'UpdtL') THEN
             !  modelz(imodel)%nb_external_variables=4+2
             !  modelz(imodel)%nb_internal_variables=6+3
             !ELSE
             !  modelz(imodel)%nb_external_variables=4+2
             !  modelz(imodel)%nb_internal_variables=6+3
             !  !
             !  call logmes('Unknown form_ option for LMGC90 standalone')
             !  call logmes('Either understood by external models') 
             !  call faterr(IAM,'Or check MODELS.DAT') 
             !ENDIF
             call faterr(IAM,'No large def yet for multi. Check MODELS.DAT') 
           else
             call logmes('Unknown kine_ option')
             call faterr(IAM,'Check MODELS.DAT') 
           end if
         case(3)
           kinematic =get_eleop_value(imodel,'kine_') 
           if (kinematic == 'small') then
             modelz(imodel)%nb_external_variables=6+3+3
             modelz(imodel)%nb_internal_variables=0
           else if (kinematic == 'large') then
             call faterr(IAM,'No large def yet for multi. Check MODELS.DAT') 
             !formulation=get_eleop_value(imodel,'form_')
             !IF (formulation == 'UpdtL') THEN
             !  modelz(imodel)%nb_external_variables=6+3
             !  modelz(imodel)%nb_internal_variables=0
             !ELSE
             !  modelz(imodel)%nb_external_variables=6+3
             !  modelz(imodel)%nb_internal_variables=6+3
             !  !
             !  call logmes('Unknown form_ option for LMGC90 standalone')
             !  call logmes('Either understood by external models') 
             !  call faterr(IAM,'Or check MODELS.DAT') 
             !  !
             !ENDIF
           else
             call logmes('Unknown kine_ option')
             call faterr(IAM,'Check MODELS.DAT') 
           end if
         case default
           call faterr(IAM,'unsupported dimension')        
         end select

       CASE('THERM')
         formulation=get_eleop_value(imodel,'form_')
         SELECT CASE(formulation)
         CASE('class')  ! classical formulation
           !fd attention le gradient a toujours une dimension de 3 !!

           modelz(imodel)%nb_external_variables=3
           modelz(imodel)%nb_internal_variables=0
         CASE('discr') ! discrete formulation

           modelz(imodel)%nb_external_variables=3
           modelz(imodel)%nb_internal_variables=0

         CASE default
           !
           call logMes('Unknown form_ option for LMGC90 standalone')
           call logMes('Either understood by external models or check MODELS.DAT')
           call FATERR(IAM,'unknown form_ option') 
         END SELECT
       CASE default
         CALL FATERR(IAM,'unknown model')        
       END SELECT
   END DO

   CALL init_ppset

 END SUBROUTINE init_models
!------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_models(fantome)
  IMPLICIT NONE
  INTEGER,OPTIONAL :: fantome

  get_nb_models=SIZE(modelz)

 END FUNCTION get_nb_models
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_external_variables(imodel)
  IMPLICIT NONE
  INTEGER :: imodel

  get_nb_external_variables=modelz(imodel)%nb_external_variables

 END FUNCTION get_nb_external_variables
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_internal_variables(imodel)
  IMPLICIT NONE
  INTEGER :: imodel

  get_nb_internal_variables=modelz(imodel)%nb_internal_variables

 END FUNCTION get_nb_internal_variables
!------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_internal_variables_bundled(imodel)
  IMPLICIT NONE
  INTEGER :: imodel

  get_nb_internal_variables_bundled=modelz(imodel)%nb_internal_variables_bundled

 END FUNCTION get_nb_internal_variables_bundled
!------------------------------------------------------------------------------
 FUNCTION get_eleop_id(imodel,eleop)
  IMPLICIT NONE
  INTEGER :: imodel,get_eleop_id
  CHARACTER(len=5)  :: eleop
  !****
  INTEGER :: i
                           !12345678012345678901
  CHARACTER(len=21) :: IAM='models::get_eleop_id'

  get_eleop_id=0

  IF (ASSOCIATED(modelz(imodel)%optns%ID)) THEN
    DO i=1,SIZE(modelz(imodel)%optns%ID)
      IF (modelz(imodel)%optns%ID(i) == eleop) THEN
        get_eleop_id=i
        EXIT
      ENDIF
    ENDDO
  ENDIF

 END FUNCTION get_eleop_id
!------------------------------------------------------------------------------
 FUNCTION get_eleop_value_by_name(imodel,eleop)
  IMPLICIT NONE
  INTEGER                   :: imodel
  CHARACTER(len=5),optional :: eleop
  CHARACTER(len=5)          :: get_eleop_value_by_name
  ! ***
  INTEGER :: i
  LOGICAL :: exist
  CHARACTER(len=30) :: value
                           !1234567890123456789012345678901
  CHARACTER(len=31) :: IAM='models::get_eleop_value_by_name'

  exist=.FALSE.

  IF (ASSOCIATED(modelz(imodel)%optns%ID)) THEN
     DO i=1,SIZE(modelz(imodel)%optns%ID)
      IF (modelz(imodel)%optns%ID(i) == eleop) THEN
        exist=.TRUE.
        value=modelz(imodel)%optns%value(i)
        get_eleop_value_by_name=value(1:5)
        EXIT
      ENDIF
    ENDDO
  ENDIF

  IF (.NOT. exist) THEN
    CALL LOGMES('The option '//eleop//' is not defined')
    CALL LOGMES('You should check the MODELS.DAT file')
    CALL FATERR(IAM,'undefined option')
  ENDIF

 END FUNCTION get_eleop_value_by_name
!------------------------------------------------------------------------------
 FUNCTION get_eleop_value_by_id(imodel,id)
  IMPLICIT NONE
  INTEGER          :: imodel
  integer          :: id 
  CHARACTER(len=5) :: get_eleop_value_by_id
  ! ***
  INTEGER :: i
  LOGICAL :: exist
  CHARACTER(len=30) :: value
                           !12345678901234567890123456789
  CHARACTER(len=29) :: IAM='models::get_eleop_value_by_id'

  if (id == 0) then
    CALL LOGMES('The id is not defined')
    CALL LOGMES('You should check the MODELS.DAT file')
    CALL FATERR(IAM,'undefined option')
  endif

  value=modelz(imodel)%optns%value(id)
  get_eleop_value_by_id=value(1:5)

 END FUNCTION get_eleop_value_by_id


!> ppset initialization
!> 
!------------------------------------------------------------------------------
 SUBROUTINE init_ppset
   IMPLICIT NONE

! PRINT*,'INIT PPSET',get_nb_bulk_behav(),get_nb_models()

   if( allocated(check_ppset_map) ) deallocate(check_ppset_map)
   ALLOCATE(check_ppset_map(get_nb_bulk_behav(),get_nb_models()))
   check_ppset_map=0

   NULLIFY(Root)
   NULLIFY(Current)
   NULLIFY(Previous)

   nb_ppset = 0
! PRINT*,'Initialisation ppset'


 END SUBROUTINE init_ppset
!------------------------------------------------------------------------------
 INTEGER FUNCTION get_ppset_nb(use_existing_ppset,imodel,ibehav)
  IMPLICIT NONE
  LOGICAL :: use_existing_ppset
  INTEGER :: imodel,ibehav
  CHARACTER(len=5) :: isext

  if (use_existing_ppset .and. check_ppset_map(ibehav,imodel) /= 0 ) then 
    get_ppset_nb = check_ppset_map(ibehav,imodel) 
    return
  endif

  nb_ppset = nb_ppset+1

  IF ( nb_ppset == 1) THEN
    ALLOCATE(Root)
    Current => Root
    NULLIFY(Root%p)
  ELSE
    ALLOCATE(Current)
    Previous%n => Current
  ENDIF

  Current%val%mdlnb  = imodel
  Current%val%lawnb  = ibehav

  Current%p => Previous
  NULLIFY(Current%n)
  Previous => Current

!fd pour le moment on n'alloue pas tout dans MatLib
!fd on n'alloue qu'une fois par paire
!fd
!  PRINT*,'in PPSET',imodel,ibehav

  IF (check_ppset_map(ibehav,imodel) == 0) THEN
    check_ppset_map(ibehav,imodel) = nb_ppset
  ENDIF

  get_ppset_nb=nb_ppset

! PRINT*,'allocation du ppset ',nb_ppset,' pour ',imodel,ibehav

 END FUNCTION get_ppset_nb
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 SUBROUTINE store_ppset
   IMPLICIT NONE
   INTEGER :: i
   character(len=80) :: cout

   if( allocated(ppset) ) deallocate(ppset)

   ALLOCATE(ppset(nb_ppset))

   DO i=nb_ppset,1,-1
      
! on recupere

     ppset(i)%mdlnb  = Current%val%mdlnb
     ppset(i)%lawnb  = Current%val%lawnb
      
! on rembobine de 1

     Previous => Current%p
     DEALLOCATE(Current)
     Current => Previous
   END DO

   NULLIFY(Root)

   write(cout,'(I0,1x,A)') nb_ppset,'ppset are stored'
   call logmes(cout)
   call logmes('--')

 END SUBROUTINE store_ppset
!-----------------------------------------------------------------------------
!------------------------------------------------------------------------------
 FUNCTION get_eleop_value_bypps(ppsnb,eleop)
   IMPLICIT NONE
   INTEGER :: ppsnb,mdlnb 
   CHARACTER(len=5) :: eleop,get_eleop_value_bypps
   CHARACTER(len=30):: value

   mdlnb=ppset(ppsnb)%mdlnb
   
   value = get_eleop_value(mdlnb,eleop)
   get_eleop_value_bypps = value(1:5)

 END FUNCTION get_eleop_value_bypps
!------------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_ppsets(fantome)
  IMPLICIT NONE
  INTEGER,OPTIONAL :: fantome

  get_nb_ppsets=nb_ppset

 END FUNCTION get_nb_ppsets
!------------------------------------------------------------------------------
  SUBROUTINE get_ppset_value(ippset,imodel,ibehav)
    IMPLICIT NONE
    INTEGER :: ippset,imodel,ibehav

    imodel = ppset(ippset)%mdlnb
    ibehav = ppset(ippset)%lawnb

  END SUBROUTINE get_ppset_value
!------------------------------------------------------------------------
 INTEGER FUNCTION get_external_field_nb(imodel)
  IMPLICIT NONE
  INTEGER :: i,imodel

  if (modelz(imodel)%nb_fields < 0) then
    get_external_field_nb = 0
   
    IF (ASSOCIATED(modelz(imodel)%optns%ID)) THEN
      DO i=1,SIZE(modelz(imodel)%optns%ID)
        IF (modelz(imodel)%optns%ID(i) == 'ext_f' .or. &
            modelz(imodel)%optns%ID(i) == 'extsf') get_external_field_nb = get_external_field_nb + 1
      ENDDO
    ENDIF
    modelz(imodel)%nb_fields = get_external_field_nb
  else
    get_external_field_nb = modelz(imodel)%nb_fields
  endif
  if (get_external_field_nb == 0) return

  if (associated(modelz(imodel)%field_name) .and. &
      get_external_field_nb /= size(modelz(imodel)%field_name)) then
    deallocate(modelz(imodel)%field_name)
    nullify(modelz(imodel)%field_name)
  endif

  if (.not. associated(modelz(imodel)%field_name)) then
    allocate(modelz(imodel)%field_name(get_external_field_nb))
                                !123456789012345678901234567890
    modelz(imodel)%field_name = 'undefined                     '
  endif


 END FUNCTION get_external_field_nb
!------------------------------------------------------------------------
 !> Get the number of external vector field of a model
 integer(kind=4) function get_external_nb_vfield(imodel)
  implicit none
  !> model index
  integer(kind=4), intent(in) :: imodel
  !
  integer(kind=4) :: i

  if (modelz(imodel)%nb_vfields < 0) then
    get_external_nb_vfield = 0
   
    if (associated(modelz(imodel)%optns%ID)) then
      do i = 1, size(modelz(imodel)%optns%ID)
        if (modelz(imodel)%optns%ID(i) == 'extvf') get_external_nb_vfield = get_external_nb_vfield + 1
      end do
    end if
    modelz(imodel)%nb_vfields = get_external_nb_vfield
  else
    get_external_nb_vfield = modelz(imodel)%nb_vfields
  endif
  if (get_external_nb_vfield == 0) return

  if (associated(modelz(imodel)%vfield_name) .and. &
      get_external_nb_vfield /= size(modelz(imodel)%vfield_name)) then
    deallocate(modelz(imodel)%vfield_name)
    nullify(modelz(imodel)%vfield_name)
  endif

  if (.not. associated(modelz(imodel)%vfield_name)) then
    allocate(modelz(imodel)%vfield_name(get_external_nb_vfield))
                                 !123456789012345678901234567890
    modelz(imodel)%vfield_name = 'undefined                     '
  endif


 end function get_external_nb_vfield
!------------------------------------------------------------------------------
 FUNCTION get_external_field_name(imodel,if)
  IMPLICIT NONE
  INTEGER :: i,imodel,if,j
  CHARACTER(len=30)  :: get_external_field_name
  !
  character(len=80)  :: cout

  !fd si pas defini on cherche
                                       !123456789012345678901234567890
  if (modelz(imodel)%field_name(if) == 'undefined                     ') then
    j=0
    IF (ASSOCIATED(modelz(imodel)%optns%ID)) THEN
      DO i=1,SIZE(modelz(imodel)%optns%ID)
        IF (modelz(imodel)%optns%ID(i) == 'extsf') THEN
          j = j + 1
          if (if == j) then
            get_external_field_name=modelz(imodel)%optns%value(i)
            modelz(imodel)%field_name(if) = get_external_field_name 
            EXIT
          endif
        ENDIF
      ENDDO
    ENDIF

    IF ( j == 0) THEN

      write(cout,'(A,I0)') 'Problem getting the external field ',j
      write(cout,'(A)') 'Check MODELS.DAT                    '
      call faterr('models::get_external_field_name',cout)

    ENDIF

  !fd sinon on va directement
  else
    get_external_field_name=modelz(imodel)%field_name(if)
  endif

 END FUNCTION get_external_field_name
!------------------------------------------------------------------------------!
 !> Get the name of an external vector field
 function get_external_vfield_name(imodel,if)
  implicit none
  !> model index
  integer(kind=4), intent(in) :: imodel
  !> vector field index
  integer(kind=4), intent(in) :: if
  !> vector field name
  character(len=30)  :: get_external_vfield_name
  !
  integer(kind=4)   :: i,j
  character(len=80) :: cout

  !fd si pas defini on cherche
                                        !123456789012345678901234567890
  if (modelz(imodel)%vfield_name(if) == 'undefined                     ') then
    j = 0
    if (associated(modelz(imodel)%optns%ID)) then
      do i = 1, size(modelz(imodel)%optns%ID)
        if (modelz(imodel)%optns%ID(i) == 'extvf') THEN
          j = j + 1
          if (if == j) then
            get_external_vfield_name=modelz(imodel)%optns%value(i)
            modelz(imodel)%vfield_name(if) = get_external_vfield_name 
            exit
          end if
        end if
      end do
    end if

    if (j == 0) then
      write(cout,'(A,I0)') 'Problem getting the external vector field ',j
      write(cout,'(A)') 'Check MODELS.DAT                    '
      call faterr('models::get_external_vfield_name',cout)
    end if

  !fd sinon on va directement
  else
    get_external_vfield_name = modelz(imodel)%vfield_name(if)
  end if

 end function get_external_vfield_name

 !> Get the maximum size of external vector field
 integer(kind=4) function get_external_vfield_max_size(imodel)
  implicit none
  !> model index
  integer(kind=4), intent(in) :: imodel

  get_external_vfield_max_size = modelz(imodel)%vfield_max_size

 end function

!------------------------------------------------------------------------------!
!  Calcul de la matrice de comportement elastique D pour les elements
!                        isoparametriques
!
! 2D   D(3,3)
! AXI  D(4,4)
! 3D   D(6,6)
!------------------------------------------------------------------------------!
SUBROUTINE D_SOLID_ISO(ppsnb,D,my_ec)

 IMPLICIT NONE

 INTEGER                         :: ppsnb
 REAL(KIND=8),POINTER            :: D(:,:)
 real(kind=8),optional           :: my_ec(2) 
 
 INTEGER                         :: mdlnb,lawnb
 INTEGER                         :: anisotropie
!
! modifier cette taille qui est mise en dur
!
 REAL(kind=8),DIMENSION(21) :: elas_coeff             

 REAL(KIND=8)               :: YOUNG,PS


! Variables locales
 REAL(KIND=8)               :: C1,C2,C3,C4


 mdlnb=ppset(ppsnb)%mdlnb
 lawnb=ppset(ppsnb)%lawnb

 if (present(my_ec)) then
   elas_coeff=0.d0
   elas_coeff(1:2)=my_ec(1:2)
   anisotropie=0
 else   
   CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)
 endif   
!
 IF (anisotropie > 0) THEN 
   call faterr('models::D_SOLID_ISO','materiau non supporte')
 ENDIF

 YOUNG=elas_coeff(1)
 PS=elas_coeff(2)

!! print*,'Young=',young,'Poisson=',PS


! Initialisations des nouveaux pointeurs
IF(ASSOCIATED(D)) THEN ; DEALLOCATE(D) ; NULLIFY(D) ; ENDIF

 SELECT CASE(DIME_mod)
   CASE(i_2D_stress) ! Contraintes planes
      C1=YOUNG/(1.D0-PS*PS) ; C2=C1*PS ; C3=0.5D0*C1*(1.D0-PS)
      ALLOCATE(D(3,3))
      D=RESHAPE( (/  C1   ,  C2   , 0.D0  ,  &
                     C2   ,  C1   , 0.D0  ,  &
                    0.D0  , 0.D0  ,  C3      /),(/3,3/) )

    CASE(i_2D_strain) ! Deformations planes
      C1= YOUNG / ( (1.D0-2.D0*PS)*(1.D0+PS) ) ; C2= C1*(1.D0-PS)
      C3= C1*PS ; C4= 0.5D0*C1*(1.D0-2.D0*PS)
      ALLOCATE(D(3,3))
      D=RESHAPE( (/  C2  ,  C3  , 0.D0 , &
                     C3  ,  C2  , 0.D0 , &
                    0.D0 , 0.D0 ,  C4    /),(/3,3/) )

    CASE(i_2D_axisym) ! 2D axisymetrique
      C1= YOUNG/( (1.D0-2.D0*PS)*(1.D0+PS) ) ; C2= C1*(1.D0-PS)
      C3= C1*PS ; C4= 0.5D0*C1*(1.D0-2.D0*PS)
      ALLOCATE(D(4,4))
      D=RESHAPE( (/  C2  ,  C3  , 0.D0 ,  C3  , &
                     C3  ,  C2  , 0.D0 ,  C3  , &
                    0.D0 , 0.D0 ,  C4  , 0.D0 , &
                     C3  ,  C3  , 0.D0 ,  C2    /), (/4,4/) )

    CASE(i_3D) ! 3D
      C1= YOUNG/( (1.D0-2.D0*PS)*(1.D0+PS) ) ; C2= C1*(1.D0-PS)
      C3= C1*PS ; C4= 0.5D0*C1*(1.D0-2.D0*PS)
      ALLOCATE(D(6,6))
      D=RESHAPE( (/  C2  ,  C3  ,  C3  , 0.D0 , 0.D0 , 0.D0 , &
                     C3  ,  C2  ,  C3  , 0.D0 , 0.D0 , 0.D0 , &
                     C3  ,  C3  ,  C2  , 0.D0 , 0.D0 , 0.D0 , &
                    0.D0 , 0.D0 , 0.D0 ,  C4  , 0.D0 , 0.D0 , &
                    0.D0 , 0.D0 , 0.D0 , 0.D0 ,  C4  , 0.D0 , &
                    0.D0 , 0.D0 , 0.D0 , 0.D0 , 0.D0 ,  C4   /), (/6,6/) )

    CASE DEFAULT
      call faterr('models::D_SOLID_ISO','unknown DIME : '//get_dime_mode_name_from_id(DIME_mod))
 END SELECT

END SUBROUTINE D_SOLID_ISO

!------------------------------------------------------------------------------!
!  Calcul de la matrice de comportement elastique D pour les elements
!                        isoparametriques de type SHB coque
!
! 3D   D(6,6)
!------------------------------------------------------------------------------!
SUBROUTINE D_SOLID_SHB(ppsnb,D)

 IMPLICIT NONE

 INTEGER                         :: ppsnb,mdlnb,lawnb
 INTEGER                         :: anisotropie
!
! modifier cette taille qui est mise en dur
!
 REAL(kind=8),DIMENSION(21) :: elas_coeff             

 REAL(KIND=8)               :: YOUNG,PS
 REAL(KIND=8),POINTER       :: D(:,:)

! Variables locales
 REAL(KIND=8)               :: C1,C2,C3


 mdlnb=ppset(ppsnb)%mdlnb
 lawnb=ppset(ppsnb)%lawnb

 CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)
!
 IF (anisotropie > 0) THEN 
   call faterr('models::D_SOLID_SHB','materiau non supporte')
 ENDIF

 YOUNG=elas_coeff(1)
 PS=elas_coeff(2)

!~ print*,'Young=',young,'Poisson=',PS

! Initialisations des nouveaux pointeurs
IF(ASSOCIATED(D)) THEN ; DEALLOCATE(D) ; NULLIFY(D) ; ENDIF

C1= YOUNG/((1.D0 - PS*PS)) ; C2= C1 * PS
C3= 0.5D0*C1*(1.D0 - PS)
ALLOCATE(D(6,6))
D=RESHAPE( (/  C1  ,  C2  , 0.D0  , 0.D0 , 0.D0 , 0.D0 , &
               C2  ,  C1  , 0.D0  , 0.D0 , 0.D0 , 0.D0 , &
              0.D0 , 0.D0 , YOUNG , 0.D0 , 0.D0 , 0.D0 , &
              0.D0 , 0.D0 , 0.D0  ,  C3  , 0.D0 , 0.D0 , &
              0.D0 , 0.D0 , 0.D0  , 0.D0 ,  C3  , 0.D0 , &
              0.D0 , 0.D0 , 0.D0  , 0.D0 , 0.D0 ,  C3   /), (/6,6/) )


END SUBROUTINE D_SOLID_SHB

!------------------------------------------------------------------------------!
!  Calcul de la matrice de comportement elastique D pour les elements joints
!
! 2D   D(2,2)
! 3D   D(3,3)
!------------------------------------------------------------------------------!

SUBROUTINE D_JOINT(ppsnb,D,my_ec)

 IMPLICIT NONE

 INTEGER                         :: ppsnb
 REAL(KIND=8),POINTER            :: D(:,:)
 real(kind=8),optional           :: my_ec(3) 
 
 INTEGER                         :: mdlnb,lawnb,sz
!
! modifier cette taille qui est mise en dur
!
 
 REAL(kind=8),DIMENSION(:),allocatable :: joint_param
 real(kind=8)              :: kt,kn

 mdlnb=ppset(ppsnb)%mdlnb
 lawnb=ppset(ppsnb)%lawnb

 call get_joint_param_size(lawnb,sz)
 allocate(joint_param(sz)) 
 
 if (present(my_ec)) then
   joint_param=0.d0
   joint_param(1:2)=my_ec(1:2)
 else   
   CALL get_joint_param(lawnb,joint_param)
 endif   

 ! on prend le kn en compression ...
 
 kt=joint_param(1)
 kn=joint_param(2)

   
 ! Initialisations des nouveaux pointeurs
 IF(ASSOCIATED(D)) THEN ; DEALLOCATE(D) ; NULLIFY(D) ; ENDIF

 SELECT CASE(DIME_mod)
   CASE(i_2D_stress,i_2D_strain,i_2D_axisym) 
      ALLOCATE(D(2,2))
      D=RESHAPE( (/  kt   , 0.D0  ,  &
                    0.D0  ,  kn /),(/2,2/) )

   CASE(i_3D) ! 3D
      ALLOCATE(D(3,3))
      D=RESHAPE( (/ kt   , 0.D0 , 0.D0 , &
                    0.D0 ,  kt  , 0.D0 , &
                    0.D0 , 0.D0 ,  kn   /), (/3,3/) )
    CASE DEFAULT
      call faterr('models::D_SOLID_JOINT','unknown DIME : '//get_dime_mode_name_from_id(DIME_mod))
 END SELECT

END SUBROUTINE D_JOINT

!------------------------------------------------------------------------
SUBROUTINE comp_stress(ppsnb,Eloc,Sloc,my_ec)
  IMPLICIT NONE
  INTEGER                   :: ppsnb,mdlnb,lawnb
  REAL(kind=8),DIMENSION(:) :: Eloc,Sloc
  real(kind=8),optional     :: my_ec(2)
  ! matrice de comportement  
  REAL(KIND=8) , POINTER    :: D(:,:)      
  INTEGER                   :: anisotropie
  !
  !fd modifier cette taille qui est mise en dur ?
  !
  REAL(kind=8),DIMENSION(21):: elas_coeff             

  REAL(KIND=8)              :: YOUNG,PS
 
  D => null()

  if (present(my_ec )) then
    CALL D_SOLID_ISO(ppsnb,D,my_ec)
    YOUNG=my_ec(1)
    PS=my_ec(2)
    anisotropie=0 
  else
    CALL get_ppset_value(ppsnb,mdlnb,lawnb)    
    CALL D_SOLID_ISO(ppsnb,D)
    CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)
    YOUNG=elas_coeff(1)
    PS=elas_coeff(2)
  endif
 
  IF (anisotropie > 0) THEN 
    call faterr('models::comp_stress','materiau non supporte')
  ENDIF

  ! S = D Eps 
  Sloc(1:SIZE(D,dim=1)) = MATMUL(D,Eloc(1:SIZE(D,dim=1)))  

  ! cas des deformations et contraintes planes

   SELECT CASE(DIME_mod)
     CASE(i_2D_stress)
       Sloc(4)=0.D0
       Eloc(4)=-PS/YOUNG*(Sloc(1) +Sloc(2))
     CASE(i_2D_strain)
       Sloc(4)=PS*(Sloc(1) +Sloc(2)) 
       Eloc(4)=0.D0
   END SELECT

   deallocate(D)
   nullify(D)

END SUBROUTINE comp_stress

!------------------------------------------------------------------------
SUBROUTINE comp_stress_shb(ppsnb,Eloc,Sloc)
  IMPLICIT NONE
  INTEGER :: ppsnb,mdlnb,lawnb
  REAL(kind=8),DIMENSION(:) :: Eloc,Sloc 
  REAL(KIND=8) , POINTER :: D(:,:)      ! matrice de comportement
  INTEGER                         :: anisotropie
  !
  ! modifier cette taille qui est mise en dur
  !
  REAL(kind=8),DIMENSION(21)      :: elas_coeff             

  REAL(KIND=8)               :: YOUNG,PS
 
  D => null()

  CALL get_ppset_value(ppsnb,mdlnb,lawnb)

  CALL D_SOLID_SHB(ppsnb,D)

  CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)
  !
  IF (anisotropie > 0) THEN 
    call faterr('models::comp_stress','materiau non supporte')
  ENDIF

  YOUNG=elas_coeff(1)
  PS=elas_coeff(2)

  Sloc(1:SIZE(D,dim=1)) = MATMUL(D,Eloc(1:SIZE(D,dim=1)))  ! S = D Eps 

   deallocate(D)
   nullify(D)

END SUBROUTINE comp_stress_shb

!------------------------------------------------------------------------
SUBROUTINE comp_stress_joint_elas(ppsnb,Eloc,Sloc,my_ec)

  !elastic model for joint
  
  IMPLICIT NONE
  INTEGER                   :: ppsnb,mdlnb,lawnb,sz
  REAL(kind=8),DIMENSION(:) :: Eloc,Sloc
  real(kind=8),optional     :: my_ec(3)

  !
  !fd modifier cette taille qui est mise en dur ?
  !
  REAL(kind=8),DIMENSION(:), allocatable:: joint_param             

  CALL get_ppset_value(ppsnb,mdlnb,lawnb)
  call get_joint_param_size(lawnb,sz)
  allocate(joint_param(sz)) 
  
  if (present(my_ec )) then
    joint_param(1:3)=my_ec(1:3)
  else          
    CALL get_joint_param(lawnb,joint_param)
  endif
 
  ! S = D Eps 

  SELECT CASE(DIME_mod)
  CASE(i_2D_stress,i_2D_strain,i_2D_axisym)
        Sloc(1) = joint_param(1)*Eloc(1)
        if (Eloc(2) > 0.d0) then
           Sloc(2) = joint_param(3)*Eloc(2)
        else   
           Sloc(2) = joint_param(2)*Eloc(2)
        endif   
  CASE(i_3D)
        Sloc(1) = joint_param(1)*Eloc(1)
        Sloc(2) = joint_param(1)*Eloc(2)
        if (Eloc(3) > 0.d0) then
           Sloc(3) = joint_param(3)*Eloc(3)
        else   
           Sloc(3) = joint_param(2)*Eloc(3)
        endif   
  END SELECT

  deallocate(joint_param)
   
END SUBROUTINE comp_stress_joint_elas

SUBROUTINE comp_stress_joint_MC(ppsnb,E0,S0,I0,E1,S1,I1,bavard,my_ec)

  ! Mohr-Coulomb model for joint
  
  IMPLICIT NONE
  INTEGER                   :: ppsnb,mdlnb,lawnb,sz
  REAL(kind=8),DIMENSION(:) :: E0,S0,I0
  REAL(kind=8),DIMENSION(:) :: E1,S1,I1
  REAL(kind=8),DIMENSION(3) :: E0_,S0_
  REAL(kind=8),DIMENSION(3) :: E1_,S1_
  logical                   :: bavard
  real(kind=8),optional     :: my_ec(9)
  real(kind=8)              :: DEFP(3)
                                   !123456789012345678901234567
  character(len=27)         :: IAM='models:comp_stress_joint_MC'
  !
  !fd modifier cette taille qui est mise en dur ?
  !
  REAL(kind=8),DIMENSION(:),allocatable:: joint_param             

  ! print*,'E0',E0
  ! print*,'E1',E1
  
  CALL get_ppset_value(ppsnb,mdlnb,lawnb)
  call get_joint_param_size(lawnb,sz)
  allocate(joint_param(sz)) 
  
  if (present(my_ec)) then
    joint_param(1:9)=my_ec(1:9)
  else
    CALL get_joint_param(lawnb,joint_param)
  endif
 
  ! S = D Eps 

  SELECT CASE(DIME_mod)
  CASE(i_2D_stress,i_2D_strain,i_2D_axisym)
        !fd pour le 2D on fait comme si on était en 3D avec la première composante en t nulle
        E0_=0.
        E0_(2:3)=E0
        E1_=0.
        E1_(2:3)=E1        
        S0_=0.
        S0_(2:3)=S0
        S1_=0.
        !call faterr(IAM,'MC not implemented for 2D problems')
        call MC(joint_param,S0_,I0(1:3),I0(4:7),E1_-E0_,S1_,DEFP,I1(4:7),bavard)
        I1(1:3) = I0(1:3) + DEFP(1:3) 
        S1=S1_(2:3)
        
  CASE(i_3D)

        call MC(joint_param,S0,I0(1:3),I0(4:7),E1-E0,S1,DEFP,I1(4:7),bavard)
        
        I1(1:3) = I0(1:3) + DEFP(1:3)

        ! print*,'I0   ',I0(1:3)
        ! print*,'DEFP ',DEFP(1:3)
        ! print*,'I1   ',I1(1:3)
        
  END SELECT

  deallocate(joint_param)
  
END SUBROUTINE comp_stress_joint_MC

subroutine MC(joint_param,SIG0,EPIN0,VAR0,DEPST,SIGF,DEFP,VARF,bavard)
    
!    Args:
!
!     SIG0: contrainte initiale
!     EPIN0: deformation inelastique initiale
!     VAR0: variables internes initiales  
!           VAR0(1) deformation plastique equivalente
!           VAR0(2) EPOUN (defo plastique due a l'ouverture seule) <-fd incre de defo plastique 
!           VAR0(3) STAT (état du joint)
!                     = 0 joint sain comprimé
!                     = 1 joint sain traction
!                     = 2 joint cassé comprimé
!                     = 3 joint cassé en traction 
!           VAR0(4) LAM1 (multiplicateur plastique en traction)
!     DEPST: increment de deformation
!     kt: rigidité tangentielle
!     ks: rigidité tangentielle
!     knc: premiere rigidité normale en compression
!     knt: premiere rigidité normale en traction 
!     kncc: deuxième rigidité normaleen compression
!     ECN: deformation a transition entre rigidités normales
!     C: cohesion
!     PHI: angle du cone de Mohr-Coulomb
!     ZMU: angle de dilantance
!     FTRC: limite en traction

!    Returns:

!     SIGF: contrainte finale
!     DEFP: increment de deformation inelastique
!     VARF: variables internes finales

    implicit none

    logical :: bavard,first_time=.TRUE.

    ! parametres materiaux
    real(kind=8) :: joint_param(9)
    real(kind=8) :: kt,ks,knc,knt,kncc,ECN,C,PHI,ZMU,FTRC
    real(kind=8) :: COTMU

    
    ! contrainte initiale
    real(kind=8) :: SIG0(3)
    ! deformation inelastique initiale
    real(kind=8) :: EPIN0(3)
    ! variables internes initiales  
    real(kind=8) :: VAR0(4)
    ! increment de deformation    
    real(kind=8) :: DEPST(3)
    ! sortie contrainte, def plastique, variables internes
    real(kind=8) :: SIGF(3),DEFP(3),VARF(4)
    real(kind=8) :: DEPFN,SIGSEI

    ! def inelastique
    real(kind=8) :: EPSS1,EPSS2,EPSN1
    ! def elastique tangente
    real(kind=8) :: DEFS10,DEFS20,DEFN0
    ! def totale pas N
    real(kind=8) :: EPS1N,EPS2N,EPSNN
    ! pred def total pas N+1 
    real(kind=8) :: EP1NP1,EP2NP1,EPNNP1
    ! def elas pas N+1 it 0
    real(kind=8) :: EE1P10,EE2P10,EENP10
    ! contrainte pas N+1 it 0
    real(kind=8) :: S1NP10,S2NP10,SNNP10

    real(kind=8) :: DELEP1,DELEP2,DELEPN
    real(kind=8) :: DEPEQ,ESPEQ

    
    real(kind=8) :: PRESP,PRESM
    real(kind=8) :: PREEP,PREEM
    real(kind=8) :: PRECIS,PRECIN
    real(kind=8) :: EPSEQ,EPOUN,STAT,XLAM1,RTRAC
    
    integer      :: iter,maxite

    ! 
    real(kind=8) :: Cprime,Cseconde
    real(kind=8) :: CRICIS,CRINOR,CRICIP,ABSTAU,CRICC

    ! derivee F suivant sigma
    real(kind=8) :: DFIDS1,DFIDS2,DFIDSN

    ! derivee G suivant sigma
    real(kind=8) :: DENO,DGIDS1,DGIDS2,DGIDSN,DZGIDS

    !
    real(kind=8) :: HODF1,HODF2,HODFN
    real(kind=8) :: HODG1,HODG2,HODGN 
    real(kind=8) :: FIFI,FIGI,GIFI,GIGI,DET
    real(kind=8) :: DLAM1,DLAM2,DEFPN
    real(kind=8) :: DEPS1,DEPS2,DEPSN
    real(kind=8) :: S1NP11,S2NP11,SNNP11
    
    real(kind=8) :: delam1,pro1
                             !1234679012
    character(len=12) :: IAM='models::MC'

    if (bavard) print*,'-----------'

    !1  2   3   4    5  6    7   8 9
    !kt,knc,knt,kncc,ec,ftrc,phi,C,zmu
    
    kt   = joint_param(1)
    ks   = joint_param(1)
    knc  = joint_param(2)
    knt  = joint_param(3)
    kncc = joint_param(4)
    ECN  = joint_param(5)
    C    = joint_param(8)
    PHI  = joint_param(7)*pi_g/180.d0
    ZMU  = joint_param(9)*pi_g/180.d0
    FTRC = joint_param(6)
    
    if (FTRC > (C/tan(phi))) then
      print*,'FTRC      = ',FTRC
      print*,'C/tan(phi)= ',C/tan(phi)
      call faterr(IAM,'FTRC greater than C/tan(phi)')
    endif   

    ! contrainte en fin de pente EC
    sigsei = -ECN*knc
    
    ! if (bavard) print*,'DEPS',DEPST

    !parametres pour le test de nullite des contraintes et déformations

    PRESP = 1D-8
    PRESM = -1D-8

    PREEP = 1D-10
    PREEM = -1D-10

    ! nombre maximum d'itérations

    MAXITE = 1000

    ! précision pour les itérations internes
    PRECIS = 1D-8
    PRECIN = -1D-8


    ! incrément de déformation (DEPS)
    DEPS1 = DEPST(1)
    DEPS2 = DEPST(2)
    DEPSN = DEPST(3)

    if (BAVARD) print*,"DEPST= ",DEPST
    
    ! contraintes finales
    SIGF = 0.d0

    ! incrément de déformations inélastiques final (DEFP)
    DEFP = 0.d0

    ! chargement des variables de déformation !fd a voir
    DEFPN = 0.d0

    ! cas sans dilatance
    if (ZMU == 0.d0) then
        COTMU = 0.D0
    else
        COTMU = 1.D0 / tan(ZMU)
    endif
     
    !######### CALCUL DE L'ETAT DE CONTRAINTES ET DE DEFORMATIONS AU PAS PRECEDENT ###########          
    ! DEFORMATIONS

    if (BAVARD) print*,'EPIN0= ',EPIN0
    
    ! déformations inélastiques 
    EPSS1 = EPIN0(1)
    EPSS2 = EPIN0(2)
    EPSN1 = EPIN0(3)

    ! déformations élastiques tangente
    DEFS10 = SIG0(1) / kt
    DEFS20 = SIG0(2) / ks

    ! pente EC ou pente EF pour le calcul de DEFN0
    if (SIG0(3) < sigsei) then
      DEFN0 = (-ECN) + (SIG0(3)-sigsei)/kncc
    else if (SIG0(3) >= sigsei .and. SIG0(3) <= 0.d0) then
      DEFN0 = SIG0(3)/knc
    else if (SIG0(3) > 0.d0) then
      ! correction pour joint avec résistance en traction
      DEFN0 = SIG0(3)/knt
    endif
      
    ! déformations totales au pas N
    EPS1N = EPSS1 + DEFS10
    EPS2N = EPSS2 + DEFS20
    EPSNN = EPSN1 + DEFN0

    ! estimation de la déformation totale au pas N+1
    EP1NP1 = EPS1N + DEPS1
    EP2NP1 = EPS2N + DEPS2
    EPNNP1 = EPSNN + DEPSN

    ! epsilon élastique au pas N+1 à l'itération 0
    EE1P10 = DEFS10 + DEPS1
    EE2P10 = DEFS20 + DEPS2
    EENP10 = DEFN0  + DEPSN

    ! Correction pour joint cassé en fermeture
    if (DEPSN < PREEP .and. abs(SIG0(3)) <= PRESP) then 
      if (EPNNP1 > PREEM ) then
        ! fermeture entièrement plastique
        EENP10 = 0.d0
      else if (EPNNP1 < PREEM .and. EPSNN > PREEM) then
        ! passage d'ouvert à fermé
        EENP10 = EPSNN + DEPSN 
      endif
    endif 

    ! sigma au pas N+1 iteration 0
    S1NP10 = kt * EE1P10
    S2NP10 = ks * EE2P10
	
    if (EENP10 > 0.d0) then 
      SNNP10 = knt * EENP10        
    else
       if (EENP10 >= -ECN) then 
         ! compression/traction pente EC
         SNNP10 = knc * EENP10        
       else if (EENP10 < -ECN) then
         ! compression pente EF 
         SNNP10 = sigsei + (EENP10 + ECN) * kncc
       endif   
    endif 
       
    if (bavard) print*,'EPNNP1= ',EPNNP1
    if (bavard) print*,'EPSN1= ',EPSN1,'DEFN0= ',DEFN0
    if (bavard) print*,'SNP10 = ',S1NP10,S2NP10,SNNP10    
    !######### VARIABLES INTERNES ############################################

    EPSEQ = VAR0(1)
    EPOUN = VAR0(2)
    STAT  = VAR0(3)
    XLAM1 = VAR0(4)
    
    ! mise à 0 de la limite en traction si jeu ou joint cassé

    RTRAC = FTRC

    ! Calcul des critères de rupture si joint pas encore cassé

    if (STAT < 2) then  
      Cprime = C - RTRAC * tan(PHI)
      Cseconde = C - RTRAC * (tan(PHI) + (COTMU))
      CRICIS = sqrt( (S1NP10 ** 2) + (S2NP10 ** 2) )  + SNNP10 * tan(PHI) - C
      CRINOR = SNNP10 - RTRAC 

      if ((CRICIS > PRECIS) .or. (CRINOR > PRECIS)) then
        ! rupture du joint, changement d'état 
        STAT = 2
      endif

    endif 		 
	
    ! Cas joint déjà cassé
	
    if (STAT >= 2) then
      ! critère plastique de Coulomb 
      RTRAC = 0.d0
	  C = 0.d0

    !  Pas de dilatance si joint cassé et ouvert au pas précédent
      if ((EPNNP1 > PRESP) .and. (EPSNN > PRESP)) then
	    ZMU = 0.d0
	    COTMU = 0.d0
      endif

    endif	

    !######### ETUDE DE L'ECOULEMENT SELON DIFFERENTS CRITERES ##############

    Cprime = C - RTRAC * tan(PHI)
    Cseconde = C - RTRAC * (tan(PHI) + (COTMU))

    CRICIS = sqrt( (S1NP10 ** 2) + (S2NP10 ** 2) ) + SNNP10 * tan(PHI) - C
    CRINOR = SNNP10 - RTRAC

    ! cas dilatance nulle, CRICIP = CRINOR
    if (ZMU == 0.d0) then
      ! sans dilatence 
      CRICIP = CRINOR
    else
      ! avec dilatence  
      CRICIP = - sqrt( (S1NP10 ** 2) + (S2NP10 ** 2) ) + SNNP10 * (COTMU) + Cseconde
    endif

    ABSTAU = sqrt( (S1NP10 ** 2) + (S2NP10 ** 2) ) - Cprime

    ! if (bavard) print*,FRTC,C,tan(PHI),tan(zMU),cotmu,cprime,cseconde
    
    if (bavard) print*,'CRICIS= ',CRICIS
    if (bavard) print*,'CRINOR= ',CRINOR
    if (bavard) print*,'ABSTAU= ',ABSTAU
    
    !######### CAS I LES DEUX CRITERES SONT VERIFIES ########################

    if ( CRICIS <= PRECIS .and. CRINOR <= PRECIS ) then

      ! CHARGEMENT DES VALEURS DE CONTRAINTES, DEFORMATIONS, VARIABLES INTERNES #

      if (BAVARD) then
          print*,'passage N° 1'
          print*,'les deux critères sont vérifiés'
          print*,'convergence à l itération 0'
      endif    

      SIGF(1) = S1NP10
      SIGF(2) = S2NP10
      SIGF(3) = SNNP10

      ! domaine élastique pas d'incrément de déformation plastique
      
      DEFP(1) = 0.d0
      DEFP(2) = 0.d0
      DEFP(3) = EPNNP1 - EENP10 - EPSN1
      
    endif

    !######### CAS II LES DEUX CRITERES SONT VIOLES ######################
    if (CRICIP > PRECIS .and. ABSTAU > PRECIS) then

      ! ECOULEMENT SELON LES CRITERES F ET G #

      ITER = 1
      DELAM1 = 0.d0

      do while (abs(CRICIS) > PRECIS .or. abs(CRINOR) > PRECIS) 

          ! nbre d'itérations maximal
          if (ITER > MAXITE) then
              print*,'les deux critères sont violés'
              print*,"Nombre max d itérations dépassé"
              call faterr('MC','erreur')
          endif
           
          ! messages d'itération

          if (BAVARD) then 
              print*,"passage N° 2"
              print*,"les deux critères sont violés"
              print*,'itération', ITER
          endif

          ! calcul des correcteurs plastiques #

          ! dérivée du critère F suivant sigma
          DFIDS1 = 0.d0
          DFIDS2 = 0.d0
          DFIDSN = 1.d0

          ! dérivée du critère G suivant sigma
          DENO = sqrt(S1NP10 ** 2 + S2NP10 ** 2)
          DGIDS1 = S1NP10 / DENO
          DGIDS2 = S2NP10 / DENO
          DZGIDS = tan(ZMU)
          DGIDSN = tan(PHI)

          ! matrice de Hook * DFI/DSIGMA
          HODF1 = kt  * DFIDS1
          HODF2 = ks  * DFIDS2
          HODFN = knt * DFIDSN

          ! matrice de Hook * DGI/DSIGMA
          HODG1 = kt  * DGIDS1
          HODG2 = ks  * DGIDS2
          HODGN = knt * DZGIDS

          FIFI = (DFIDS1 * HODF1) + (DFIDS2 * HODF2) + (DFIDSN * HODFN)
          FIGI = (DFIDS1 * HODG1) + (DFIDS2 * HODG2) + (DFIDSN * HODGN)
          GIFI = (DGIDS1 * HODF1) + (DGIDS2 * HODF2) + (DGIDSN * HODFN)
          GIGI = (DGIDS1 * HODG1) + (DGIDS2 * HODG2) + (DGIDSN * HODGN)

          ! résolution du système pour trouver les correcteurs plast.

          DET = (FIFI * GIGI) + (FIGI * GIFI)

          if (abs(DET) <= PRECIS) then
              print*,'système indéterminé (F G), itération', ITER
              call faterr('MC','erreur')
          endif    

          DLAM1 = (CRINOR * GIGI) - (CRICIS * FIGI)
          DLAM1 = DLAM1 / DET
          DELAM1 = DELAM1 + DLAM1
          RTRAC = 0.d0

          DLAM2 = (CRICIS * FIFI) - (CRINOR * GIFI)
          DLAM2 = DLAM2 / DET

          ! calcul du nouvel état de contraintes

          S1NP11 = S1NP10 - (kt * ((DLAM1 * DFIDS1) + (DLAM2 * DGIDS1)))
          S2NP11 = S2NP10 - (ks * ((DLAM1 * DFIDS2) + (DLAM2 * DGIDS2)))
          SNNP11 = SNNP10 - (knt * ((DLAM1 * DFIDSN) + (DLAM2 * DZGIDS)))

          ! mise à jour des valeurs dans la boucle
          S1NP10=S1NP11
          S2NP10=S2NP11
          SNNP10=SNNP11
          ITER = ITER + 1
          
          ! vérification de la convergence
          CRICIS = sqrt( (S1NP11 ** 2) + (S2NP11 ** 2) ) + SNNP11 * tan(PHI) - C
          CRINOR = SNNP11 - RTRAC

      end do    

      ! les deux critères sont vérifiés #

      ! message nbre d'itérations
      if (BAVARD) then
          print*,'passage N° 5 - 2'
          print*,'les deux critères sont vérifiés'
          print*,'convergence à l itération', ITER
      endif

      ! chargement des valeurs de contraintes, déformations, et variables internes
      SIGF(1) = S1NP11
      SIGF(2) = S2NP11
      SIGF(3) = SNNP11

      DEFP(1) = EP1NP1 - (SIGF(1)/kt) - EPSS1
      DEFP(2) = EP2NP1 - (SIGF(2)/ks) - EPSS2

      if (abs(SIGF(3)) <= PRESP) then
          ! defo plast. au pas N+1 - defo plast. au pas N
          DEFP(3) = EPNNP1 - EPSN1
      else
          ! le joint se ferme élastiquement en fermeture
          DEFP(3) = 0.d0
      endif
       
      ! mise à jour multiplicateur plastique en traction
      XLAM1 = XLAM1 + DELAM1
    endif

    !######### CAS III LE CRITERE D'EFFORT NORMAL EST VIOLE  ######################
    if (ABSTAU < PRECIS .and. CRINOR > PRECIS) then

      ! ECOULEMENT SELON LE CRITERE F #

      ITER = 1
      DELAM1 = 0.d0

      do while (abs(CRINOR) > PRECIS)

        ! nbre d'itérations maximal
        if (ITER > MAXITE) then
          print*,'le critère normal est violé'
          print*,"Nombre max d itérations dépassé"
          call faterr('MC','erreur')
        endif
          
        ! messages d'itération
        if (BAVARD) then
          print*,"passage N° 3"
          print*,"le critère de l effort normal est violé"
          print*,'itération', ITER
        endif

        ! calcul du correcteur plastique #

        ! dérivée de F suivant sigma
        DFIDS1 = 0.d0
        DFIDS2 = 0.d0
        DFIDSN = 1.d0

        ! matrice de Hook * DFI/DSIGMA
        HODF1 = kt  * DFIDS1
        HODF2 = ks  * DFIDS2
        HODFN = knt * DFIDSN

        FIFI = (DFIDS1 * HODF1) + (DFIDS2 * HODF2) + (DFIDSN * HODFN)

        ! résolution de l'équation pour trouver DLAM1

        if (abs(FIFI) <= PRECIS) then
          print*,'système indéterminé (F), itération', ITER
          stop !call faterr('MC')
        endif
          
        DLAM1 = CRINOR / FIFI
        DELAM1 = DELAM1 + DLAM1
        RTRAC = 0.d0

        ! calcul du nouvel état de contraintes

        S1NP11 = S1NP10 - (kt * DLAM1 * DFIDS1)
        S2NP11 = S2NP10 - (ks * DLAM1 * DFIDS2)
        SNNP11 = SNNP10 - (knt * DLAM1 * DFIDSN)

        ! mise à jour des valeurs dans la boucle
        S1NP10=S1NP11
        S2NP10=S2NP11
        SNNP10=SNNP11
        
        ITER = ITER + 1

        ! vérification de la convergence
        CRICIS = sqrt( (S1NP11 ** 2) + (S2NP11 ** 2) ) + SNNP11 * tan(PHI) - C
        CRINOR = SNNP11 - RTRAC

      end do

      ! les deux critères sont vérifiés #

      ! message nbre d'itérations
      if (BAVARD) then
        print*,'passage N° 5 - 3'
        print*,'les deux critères sont vérifiés'
        print*,'convergence à l itération', ITER
      endif
        
      ! chargement des valeurs de contraintes, déformations,et variables internes
      SIGF(1) = S1NP11
      SIGF(2) = S2NP11
      SIGF(3) = SNNP11

      DEFP(1) = EP1NP1 - (SIGF(1)/kt) - EPSS1
      DEFP(2) = EP2NP1 - (SIGF(2)/ks) - EPSS2

      if (abs(SIGF(3)) <= PRESP) then
        !defo plast. au pas N+1 - defo plast. au pas N
        DEFP(3) = EPNNP1 - EPSN1
      else
        ! le joint se ferme élastiquement en fermeture
        DEFP(3) = 0.d0
      endif
        
      ! mise à jour multiplicateur plastique en traction
      XLAM1 = XLAM1 + DELAM1
    endif

    !######### CAS IV LE CRITERE DE CISAILLEMENT EST VIOLE  ######################

    ! critère de cisaillement violé
    ! critère de l'effort normal vérifié

    if (CRICIS > PRECIS .and. CRICIP <= PRECIS) then

      ! écoulement selon le critère G à l'itération ITER 

      ITER = 1

      do while (abs(CRICIS) > PRECIS)

        ! vérification du nombre max d'itérations
        if (ITER > MAXITE) then
          print*,"Le critère de cisaillement est violé"
          print*,"Nombre max d itérations dépassé"
          call faterr('MC','erreur')
        endif
       
        ! messages d'itération
        if (BAVARD) then
          print*,"passage N° 4"
          print*,"critère de cisaillement violé"
          print*,'itération', ITER
        endif
       
        ! calcul du correcteur plastique

        ! dérivée de G suivant sigma
        DENO = sqrt(S1NP10 ** 2 + S2NP10 ** 2)
        DGIDS1 = S1NP10 / DENO
        DGIDS2 = S2NP10 / DENO
        DZGIDS = tan(ZMU)
        DGIDSN = tan(PHI)

        ! matrice de Hook * DGI/DSIGMA
        HODG1 = kt * DGIDS1
        HODG2 = ks * DGIDS2
        
        if (SNNP10 < sigsei) then
          HODGN = kncc * DZGIDS
        else
          HODGN = knc * DZGIDS
        endif
       
        GIGI = (DGIDS1 * HODG1) +  (DGIDS2 * HODG2) + (DGIDSN * HODGN)

        ! Résolution de l'équation pour trouver le correcteur plastique DLAM2
        if (abs(GIGI) <= PRECIS) then
          print*,'Système indéterminé (G), itération', ITER
          call faterr('MC','erreur')
        endif 
          
        DLAM2 = CRICIS / GIGI

        ! Calcul du nouvel état de contraintes
        S1NP11=S1NP10 - (kt *  (DLAM2*DGIDS1) )
        S2NP11=S2NP10 - (ks *  (DLAM2*DGIDS2) )
        
        if (SNNP10 < sigsei) then
          SNNP11=SNNP10 - (kncc *  (DLAM2*DZGIDS) )
        else
          SNNP11=SNNP10 - (knc *  (DLAM2*DZGIDS) )
        endif

        ! if (SNNP11 >= 0.):
        !   SNNP11 = 0.

        ! mise à jour des valeurs dans la boucle
        S1NP10=S1NP11
        S2NP10=S2NP11
        SNNP10=SNNP11
        ITER = ITER + 1

        ! vérification de la convergence
        CRICIS = sqrt( (S1NP11 ** 2) + (S2NP11 ** 2) ) + SNNP11 * tan(PHI) - C
        CRINOR = SNNP11 - RTRAC

      enddo

      ! les deux critères sont vérifiés

      ! message nbre d'itérations
      if (BAVARD) then
        print*,'passage N° 5 - 4'
        print*,'les deux critères sont vérifiés'
        print*,'convergence à l itération', ITER
      endif 
        
      ! chargement des valeurs de contraintes, déformations et variables internes
      SIGF(1) = S1NP11
      SIGF(2) = S2NP11
      SIGF(3) = SNNP11

      DEFP(1) = EP1NP1 - (SIGF(1)/kt) - EPSS1
      DEFP(2) = EP2NP1 - (SIGF(2)/ks) - EPSS2

      if (abs(SIGF(3)) <= PRESP) then
        ! defo plast. au pas N+1 - defo plast. au pas N
        DEFP(3) = EPNNP1 - EPSN1
      else
        ! le joint se ferme élastiquement en fermeture
        DEFP(3) = 0.d0 
      endif
     
    endif
        
    !######### CALCUL VARIABLES INTERNES + SORTIES ###################
      
    ! calcul des variables internes DEPEQ, STAT, EPOUN

    ! delta epsilon plastique entre N et N+1
    DELEP1 = DEFP(1)
    DELEP2 = DEFP(2)
    DELEPN = DEFP(3)

    ! produit contracté pour le calcul de la déformation plastique équivalente
    PRO1 = (DELEP1 ** 2) + (DELEP2 ** 2) + (DELEPN ** 2)
    DEPEQ = sqrt(2/3 * PRO1)
    EPSEQ = EPSEQ+DEPEQ
    
    ! calcul de la def normale due à l'ouverture seule
    if (abs(SIGF(3)) <= PRESP) then
      EPOUN = DELEPN
    else
      EPOUN = 0.d0
    endif
   
    ! état du joint: 
    !   STAT = 0   joint sain et comprimé
    !   STAT = 1   joint sain en traction
    !   STAT = 2   joint cassé comprimé
    !   STAT = 3   joint cassé ouvert

 
    if (STAT >= 2) then
      ! joint cassé
      if (SIGF(3) > PRESM) then
        ! ouvert
        STAT = 3
      else
        ! compression
        STAT = 2
      endif
    else
      ! joint sain
      if (SIGF(3) > PRESP) then
        ! traction
        STAT = 1
      else
        ! compression
        STAT = 0
      endif
    endif
        
    VARF=(/ EPSEQ,EPOUN,STAT,XLAM1 /) 

    if (bavard) print*,'EPIN1= ',EPIN0+DEFP
    if (bavard) print*,'SIGF= ',SIGF    
    if (bavard) print*,'VARF= ',VARF
    if (bavard) print*,'-----'
    
end subroutine MC

SUBROUTINE comp_stress_joint_FCZM(ppsnb,E0,S0,I0,E1,S1,I1,bavard,my_ec)

  ! Mohr-Coulomb model for joint
  
  IMPLICIT NONE
  INTEGER                   :: ppsnb,mdlnb,lawnb,sz
  REAL(kind=8),DIMENSION(:) :: E0,S0,I0
  REAL(kind=8),DIMENSION(:) :: E1,S1,I1
  REAL(kind=8),DIMENSION(3) :: E0_,S0_
  REAL(kind=8),DIMENSION(3) :: E1_,S1_
  logical                   :: bavard
  real(kind=8),optional     :: my_ec(15)
  real(kind=8)              :: DEFP(3)
                                   !12345678901234567890123456789
  character(len=29)         :: IAM='models:comp_stress_joint_FCZM'


  REAL(kind=8),DIMENSION(:),allocatable:: joint_param


  ! print*,'E0',E0
  ! print*,'E1',E1
  
  CALL get_ppset_value(ppsnb,mdlnb,lawnb)
  call get_joint_param_size(lawnb,sz)
  allocate(joint_param(sz)) 
  
  if (present(my_ec)) then
    joint_param(1:15)=my_ec(1:15)
  else
    CALL get_joint_param(lawnb,joint_param)
  endif
 
  ! S = D Eps 

  SELECT CASE(DIME_mod)
  CASE(i_2D_stress,i_2D_strain,i_2D_axisym)
        E0_=0.
        E0_(2:3)=E0
        E1_=0.
        E1_(2:3)=E1
        S0_=0.
        S0_(2:3)=S0
        S1_=0.
        call FCZM_FLP(joint_param,S0_,I0(1:3),I0(4:7),E1_-E0_,S1_,DEFP,I1(4:7),bavard)
        !call faterr(IAM,'MC not implemented for 2D problems')
        I1(1:3) = I0(1:3) + DEFP(1:3)
        S1=S1_(2:3)
  CASE(i_3D)

        ! print*,'E0 ',E0 
        ! print*,'E1 ',E1 
     
        call FCZM_FLP(joint_param,S0,I0(1:3),I0(4:7),E1-E0,S1,DEFP,I1(4:7),bavard)
        !call FCZM(joint_param,S0,I0(1:3),I0(4:7),E1-E0,S1,DEFP,I1(4:7),bavard)     
        
        I1(1:3) = I0(1:3) + DEFP(1:3)

        ! print*,'I0   ',I0(1:3)
        ! print*,'DEFP ',DEFP(1:3)
        ! print*,'I1   ',I1(1:3)
        
  END SELECT

  deallocate(joint_param)

  
end subroutine comp_stress_joint_FCZM

subroutine FCZM(joint_param,SIG0,EPIN0,VAR0,DEPST,SIGF,DEFP,VARF,bavard)

! version fd en jachère
  
!    Args:
!
!     SIG0: contrainte initiale
!     EPIN0: deformation inelastique initiale
!     VAR0: variables internes initiales  
!     DEPST: increment de deformation

!    Returns:

!     SIGF: contrainte finale
!     DEFP: increment de deformation inelastique
!     VARF: variables internes finales


    logical :: bavard 
  
    ! contrainte initiale
    real(kind=8) :: SIG0(3)
    ! deformation inelastique initiale
    real(kind=8) :: EPIN0(3)
    ! variables internes initiales  
    !      VAR0(1) deformation plastique equivalente
    !      VAR0(2) XLAM1 (limite elastique en cisaillement)
    !      VAR0(3) ENDO
    !      VAR0(4) pas utilise
    real(kind=8) :: VAR0(4)
    ! increment de deformation    
    real(kind=8) :: DEPST(3)
    ! sortie contrainte, def plastique, variables internes
    real(kind=8) :: SIGF(3),DEFP(3),VARF(4)
    real(kind=8) :: DEPFN,SIGSEI

    ! def inelastique
    real(kind=8) :: EPSS1,EPSS2,EPSN1
    ! def elastique tangente
    real(kind=8) :: DEFS10,DEFS20,DEFN0
    ! def totale pas N
    real(kind=8) :: EPS1N,EPS2N,EPSNN
    ! pred def total pas N+1 
    real(kind=8) :: EP1NP1,EP2NP1,EPNNP1
    ! def elas pas N+1 it 0
    real(kind=8) :: EE1P10,EE2P10,EENP10
    ! contrainte pas N+1 it 0
    real(kind=8) :: S1NP10,S2NP10,SNNP10

    real(kind=8) :: DELEP1,DELEP2,DELEPN
    real(kind=8) :: DEPEQ,ESPEQ

    
    real(kind=8) :: PRESP,PRESM
    real(kind=8) :: PREEP,PREEM
    real(kind=8) :: PRECIS,PRECIN
    real(kind=8) :: EPSEQ,XLAM1

    
    integer      :: iter,maxite

    ! 
    real(kind=8) :: Cprime,Cseconde
    real(kind=8) :: CRICIS,CRINOR,CRICIP,ABSTAU,CRICC

    ! derivee F suivant sigma
    real(kind=8) :: DFIDS1,DFIDS2,DFIDSN

    ! derivee G suivant sigma
    real(kind=8) :: DENO,DGIDS1,DGIDS2,DGIDSN,DZGIDS

    !
    real(kind=8) :: HODF1,HODF2,HODFN
    real(kind=8) :: HODG1,HODG2,HODGN 
    real(kind=8) :: FIFI,FIGI,GIFI,GIGI,DET
    real(kind=8) :: DLAM1,DLAM2,DEFPN
    real(kind=8) :: DEPS1,DEPS2,DEPSN
    real(kind=8) :: S1NP11,S2NP11,SNNP11
    
    real(kind=8) :: delam1,pro1

    real(kind=8) :: B,alpha,beta,damage,ENERN,ENERS,ENTOT,EPS0,AGI1,AGI2,DAMN,DAMN10,DAMN11,&
                    DATEST,DDELT,DELN,DELNP1,DELTA0,DELTA1,DGAM1,DGAM2,&
                    DLIN,DLIN10,DLINP1,ECOHN,ECOHS,G1IN,G1IN10,G1INP1,G2IN,G2IN10,G2INP1,&
                    GA1N,GA1NP1,GA2N,GA2NP1,GAMMA0,GAMMA1,PHI0,PRICIS,RNI,RNM,RNMF,RSI, &
                    RSM,ECN,SIGMN,SIGN1,SIGN10,SIGN11,SIGNP1,T1FN1,T1FN10,T1FN11,T1FNP1,T2FN1,T2FN10,&
                    T2FN11,T2FNP1,TA1N,TA1N10,TA1NP1,TA2N,TA2N10,TA2NP1,TANMU,TANMU1,TANPHI,&
                    TAPHI1,TESTEND,ZMU0

    real(kind=8) :: joint_param(15)
    
    integer      :: ITEND,NTEST

    real(kind=8) :: PDAM

    
                             !123467901234
    character(len=14) :: IAM='models::FCZM'


    !fd 
    real(kind=8) :: T1MN10,T2MN10,T1MN11,T2MN11
    
    !parametres pour le test de nullite des contraintes et déformations
    PRESP = 1D-6
    PRESM = -1D-6

    ! nombre maximum d'itérations
    MAXITE = 500

    ! précision pour les itérations internes
    PRECIS = 1D-8
    PRECIN = -1D-8

!
!----------Chargement des caractéristiques mécaniques

! 1  2   3   4    5  6   7   8  9  10 11 12 13 14 15
! kt,knc,knt,kncc,ec,phi,zmu,pf,pd,ct,s2,G2,cn,s1,G1
    
!
!   rigidité tangentielle du mortier
    RSM    = joint_param(1)
!   rigidité tangentielle de l'interface
    RSI    = joint_param(10)
!   première rigidité normale du mortier
    RNM    = joint_param(2)
!   seconde rigidité normale du mortier
    RNMF   = joint_param(4)
!   rigidité normale de l'interface
    RNI    = joint_param(13)
!   déformation de transition de rigidité normale pour le mortier
    ECN    = - joint_param(5)
!   angle de frottement maximal
    PHI0   = joint_param(6)*pi_g/180.
!   angle de dilatance maximal
    ZMU0   = joint_param(7)*pi_g/180.
!   Paramètres d'endommagement:
!   seuil d'endommagement en traction pure (dep n critique)
    DELTA0 = joint_param(14)/joint_param(13)
!   seuil d'endommagement en cisaillement pur (dep t critique)
    GAMMA0 = joint_param(11)/joint_param(10)
!   énergie de cohésion en traction pure
    ECOHN  = joint_param(15)
!   énergie de cohésion en cisaillement pur
    ECOHS  = joint_param(12)
!   paramètres d'activation du frottement
    alpha  = joint_param(8)
!   paramètre d'activation de la dilatance
    beta   = joint_param(9)

    !
    !---Chargement des variables internes
    !
    !   déformation équivalente
    EPSEQ  = VAR0(1)
    !   limite élastique en cisaillement 
    XLAM1  = VAR0(2)
    !   endommagement 
    DAMN   = VAR0(3)

    !
    !---Précision endommagement
    !
    PDAM = PRESP / max(RSI*GAMMA0 , RNI*DELTA0)

    !
    !---Incrément de déformation
    !
    DGAM1 = DEPST(1)
    DGAM2 = DEPST(2)
    DDELT = DEPST(3)
    !
    !---Contraintes finales
    !
    SIGF(1) = 0.D0
    SIGF(2) = 0.D0
    SIGF(3) = 0.D0
    !
    !---Incrément de déformations final à l'interface (DEFP)
    !
    DEFP(1) = 0.D0
    DEFP(2) = 0.D0
    DEFP(3) = 0.D0    
    !
    !
    !   CALCUL DES ETATS DE DEPLACEMENTS AUX PAS N et N+1
    !

    !
    !---Déformations de l'interface au pas N (allongement du ressort endommageable)
    !
    G1IN = EPIN0(1)
    G2IN = EPIN0(2)
    DLIN = EPIN0(3)
    !
    !---Etat de contraintes dans le joint au pas N (== mortier == interface)
    !
    TA1N  = SIG0(1)
    TA2N  = SIG0(2)
    SIGMN = SIG0(3)
    !
    !---Déplacements dans le joint au pas N
    !
    !   déplacement tangentiel (mortier + interface)
    GA1N = TA1N / RSM + G1IN 
    GA2N = TA2N / RSM + G2IN
    !
    !   déplacement normal, plusieurs pentes
    DELN = SIGMN / RNM + DLIN 
    if ( DELN < ECN ) then
      DELN = ECN + ( SIGMN - (ECN*RNM) )/RNMF 
    endif
    !
    !---Déplacements au pas N+1 dans le joint
    !   on cherche à savoir comment ça va se répartir entre mortier et interface (effet endo et frottement)
    !
    GA1NP1 = GA1N + DGAM1
    GA2NP1 = GA2N + DGAM2
    DELNP1 = DELN + DDELT
    
    !
    !   CALCUL ITERATIF DES DEPLACEMENTS D'INTERFACE ET DE L'ENDOMMAGEMENT
    !
    !
    !---initialisation de la boucle sur l'endommagement
    !

    !fd
    G1IN10=G1IN
    G2IN10=G2IN
    
    DAMN10 = DAMN
    DAMN11 = -1.D0
    !
    TESTEND = DAMN10 - DAMN11 
    ITEND = 1
    !
    !do while ( abs(TESTEND) > PDAM )
    do while ( abs(TESTEND) > 1e-9 )       

      if ( ITEND > MAXITE ) then
        write(6,*) 'probleme sur la boucle de calcul de l endommagement'
        write(6,*) 'Nombre max d itérations dépassé'
        call faterr('FCZM','erreur') 
      endif
     
      print*,'------------'
      if (itend == 1) then
         print*,'dep ressort initial',G1IN,G2IN,DLIN
         print*,'dep total',GA1NP1,GA2NP1,DELNP1
      endif
      !
      !   DEPLACEMENT NORMAL D'INTERFACE
      !
      !   ici les choses sont simples car ressort et contact ne travaillent jamais ensemble
       
      if ( DELNP1 > 0.D0 ) then
        ! Cas traction ; 2 ressorts en serie (on retrouve DLIN10=DELNP1 si DAMN10 == 1)
        DLIN10 = DELNP1 * RNM / ( (1.d0-DAMN10)*RNI + RNM)
      else
        ! Cas compression ; pas d'allongement de l'interface
        DLIN10 = 0.D0
      endif

      !   
      !   CALCUL DE LA CONTRAINTE DE FROTTEMENT DE L'INTERFACE
      !
      !   la force tangentielle dans l'interface est la somme de la force du ressort et du "patin frottant"
      !   ici on cherche la contribution du frottement pour en déduire la contribution du ressort 

      !
      !---Contrainte de l'interface complet au pas N+1 (== contrainte mortier)
      !
      
      ! fd cette proposition de FLP ne me semble pas correcte, car l'increment de déformation
      ! n'est pas totalement transformé en deformation du mortier 
      
      ! !-----Estimation de la contrainte dans le mortier au pas N+1
      ! !
      ! TA1N10 = TA1N + DGAM1*RSM
      ! TA2N10 = TA2N + DGAM2*RSM
      ! !
      ! !-----Estimation contrainte de frottement
      ! !
      ! T1FN1 = TA1N10 - G1IN * (1.d0-DAMN)*RSI
      ! T1FN10 = T1FN1
      ! T2FN1 = TA2N10 - G2IN * (1.d0-DAMN)*RSI
      ! T2FN10 = T2FN1

      ! fd on se sert de la dernière estimation de l'allongement de l'interface pour deduire l'allongement du mortier
      !    et en deduire l'effort dans le mortier ;
      !    par équilibre on recupere celle dans l'interface et on retire la force dans le ressort d'interface
      !    pour avoir la contribution du frottement

      T1MN10 = (GA1NP1-G1IN10)*RSM
      T2MN10 = (GA2NP1-G2IN10)*RSM
      
      T1FN1 = T1MN10 - (G1IN10 * (1.d0-DAMN10)*RSI)
      T1FN10 = T1FN1
      T2FN1 = T2MN10 - (G2IN10 * (1.d0-DAMN10)*RSI)
      T2FN10 = T2FN1

      !
      !---Contrainte normale au pas N+1
      !
      if (DELNP1 >= ECN) then
        SIGN1 = (DELNP1 - DLIN10) * RNM
      else
        SIGN1 = ECN * RNM + (DELNP1 - ECN) * RNMF
      endif
      SIGN10 = SIGN1

      ! fd
      ! maintenant on va projeter cette contrainte totale de l'interface sur les critères pour avoir une estimation
      ! de la contribution du contact-frottant et en deduire la contribution des efforts dans les ressorts et ainsi
      ! calculer l'evolution de l'endommagement.
      ! Ici c'est le critère de Coulomb classique (pas de cohésion)
      !
      print*,'it endo ',itend
      print*,'force esti',T1FN10,T2FN10,SIGN10
      
      !
      !---frottement
      !
      if (alpha == 0.d0) then
        TANPHI = tan(PHI0)
      else
        TANPHI = tan(PHI0) * DAMN10**alpha
      endif   
      !
      !---dilatance
      !
      if ( DELNP1 > 0.D0 ) then
        ! traction 
        TANMU = 0.D0
      else
        ! compression
        if (beta==0.d0) then 
          TANMU = tan(ZMU0)
        else
           TANMU = tan(ZMU0) * DAMN10**beta
        endif   
      endif
      !
      !-Calcul du critère de cisaillement
      !
      CRICIS = sqrt((T1FN10**2) + (T2FN10**2)) + SIGN10*TANPHI
      !
      !-Critère écoulement normal
      !
      if ( abs(TANMU) > PRESP ) then
        CRINOR = SIGN10/TANMU - sqrt((T1FN10**2)+(T2FN10**2))
      else
        CRINOR = SIGN10
      endif 

      !
      !-Aiguillage selon les différents critères
      !
      
      if ((CRICIS <= PRECIS) .and. (CRINOR <= PRECIN)) then
        !
        ! Tous les critères sont vérifiés on est dans le cone => compression/collant
        !
         
        print*,'I'
         
        !
        ! contrainte normale
        ! 
        SIGNP1 = SIGN10
        !
        ! contraintes finales de frottement
        !
        T1FNP1 = T1FN10
        T2FNP1 = T2FN10

        ! l'allongement est bloqué
        if ( abs(DAMN10 - 1.D0) > PDAM) then
          G1IN10 = (T1MN10 - T1FNP1) / ((1.d0-DAMN10) * RSI)
          G2IN10 = (T2MN10 - T2FNP1) / ((1.d0-DAMN10) * RSI)
        else      
          G1IN10 = GA1NP1 - T1FNP1/RSM !G1IN
          G2IN10 = GA2NP1 - T2FNP1/RSM !G2IN
        endif
       
        ! deplacement normal interface déjà calculé DLIN10 = 0.D0        

                
      else if ((CRICIS > PRECIS) .and. (CRINOR <= PRECIN)) then
        !
        ! Cisaillement violé , compression respecté => compression/glissement avec frottement
        !
        
        print*,'II'

        !
        ! initialisation boucle écoulement
        ITER = 1
        !
        do while (abs(CRICIS) > PRECIS)
          !
          ! vérification du nombre max itération
          if ( ITER > MAXITE ) then
	    write(6,*) 'le critère de cisaillement est violé'
            write(6,*) 'Nombre max d itérations dépassé'
            call faterr('FCZM','erreur') 
          endif
          !
          ! calcul du correcteur plastique
          !
          ! dérivée de G suivant sigma
          !
          DENO = sqrt( (T1FN10**2) + (T2FN10**2) )
          DGIDS1 = T1FN10 / DENO
          DGIDS2 = T2FN10 / DENO
          DZGIDS = TANMU
          DGIDSN = TANPHI
          !
          ! rigidité * DGI/DSIGMA
          !
          HODG1 = DGIDS1 * RSM
          HODG2 = DGIDS2 * RSM
          !
          if ( DELNP1 < ECN ) then
            HODGN = RNMF * DZGIDS
          else
            HODGN = RNM * DZGIDS
          endif
          !
          GIGI = (DGIDS1*HODG1)+(DGIDS2*HODG2)+(DGIDSN*HODGN)
          !
          ! Résolution équation pour trouver DLAM2, correcteur plastique
          !
          if ( abs(GIGI) <= PRECIS) then
             write(6,*) 'système indéterminé (F G),itération',ITER
             call faterr('FCZM','erreur')
          endif
          !
          DLAM2 = CRICIS / GIGI
          !
          ! Calcul du nouvel état de contraintes
          !
          T1FN11 = T1FN10 - ((DLAM2*DGIDS1) * RSM)
          T2FN11 = T2FN10 - ((DLAM2*DGIDS2) * RSM)
          !
          if (DELNP1 < ECN) then
            SIGN11 = SIGN10 - (RNMF * (DLAM2*DZGIDS))
          else
            SIGN11 = SIGN10 - (RNM * (DLAM2*DZGIDS))
          endif
          !
          ! Mise à jour des valeurs dans la boucle
          T1FN10 = T1FN11
          T2FN10 = T2FN11
          SIGN10 = SIGN11
          ITER = ITER + 1
          !
          ! nouveau test de convergence
          !
          CRICIS=sqrt((T1FN11**2)+(T2FN11**2))+SIGN11*TANPHI
          !
        enddo
        !
        ! compression cisaillement vérifié
        !
        
        !
        ! chargement des valeurs de contraintes de frottement
        !
        T1FNP1 = T1FN10
        T2FNP1 = T2FN10
        !
        ! contrainte normale
        !
        SIGNP1 = SIGN10


        ! on a trouvé RT dans le patin
        ! la force dans le ressort = force mortier - RT
        !
        ! déplacement tangentiel d'interface
        ! 
        
        if ( abs(DAMN10 - 1.D0) > PDAM) then
          G1IN10 = (T1MN10 - T1FNP1) / ((1.d0-DAMN10) * RSI)
          G2IN10 = (T2MN10 - T2FNP1) / ((1.d0-DAMN10) * RSI)
        else
          G1IN10 = GA1NP1 - (T1FNP1/RSM)
          G2IN10 = GA2NP1 - (T2FNP1/RSM) 
        endif
        
      else
        !
        ! Les deux critères sont violés --> traction + glissement sans frottement
        !
         
        print*,'III'
         
        !
        ! contraintes finales de frottement
        ! 
        T1FNP1 = 0.D0
        T2FNP1 = 0.D0

        ! 
        ! contrainte normale (la reaction dans le ressort d'interface)
        !
        SIGNP1 = SIGN10

        ! on a RT = 0 dans le patin, tout passe dans le ressort
        ! 
        !
        ! déplacement tangentiel d'interface
        ! 

        if ( abs(DAMN10 - 1.D0) > PDAM) then        
          G1IN10 = T1MN10 / ((1.d0-DAMN10) * RSI)
          G2IN10 = T2MN10 / ((1.d0-DAMN10) * RSI)
        else
          G1IN10 = GA1NP1
          G2IN10 = GA2NP1
          !DELNP1 = DELN + DDELT
        endif

        
      endif	

      print*,'force frottante',T1FNP1,T2FNP1,SIGNP1
      
      print*,G1IN10,G2IN10,DLIN10
      
      !
      !
      ! MISE A JOUR DE L'ENDOMMAGEMENT	
      !
      if ( abs(DAMN10 - 1.D0) <= PDAM) then

        DAMN11 = 1.D0

      else
        !
        !-Déplacements équivalents à l'interface
        !
        ! déplacement tangentiel pur
        GAMMA1 = sqrt(G1IN10**2 + G2IN10**2)
        ! déplacement normal pur
        DELTA1 = DLIN10
        ! déplacement mixte
        EPSEQ = sqrt(DLIN10**2 + G1IN10**2 + G2IN10**2)
        !
        !-calcul nouvel endommagement
        !
        DAMN11 = endo(DELTA0,GAMMA0,ECOHN,ECOHS,RNI,RSI,DELTA1,GAMMA1,EPSEQ,DAMN)

        ! if (DAMN11 > DAMN10) then
        !   a=0.8d0
        !   DAMN11 = (1.d0-a)*DAMN10+a*DAMN11
        ! endif 
        
        print*,DAMN11

        !
        !-Limiteur d'endommagement (dans le cas de compression cisaillement)
        ! la methode utilisee precedemment va surevaluer l'endommagement
        ! on reapplique la methode en partant du nouvel endommagement trouve et
        ! si on trouve un nouvel endommagement plus petit que le precedent on itere en diminuant l'endommagement
        !

        !fd me semble plus legitime
        !fd  if ((SIGN1 < PRECIN) .and. (DAMN11 > DAMN10)) then
        if ((SIGNP1 < PRECIN) .and. (DAMN11 > DAMN10)) then           
          NTEST = 0
          ! Boucle adoucissement 
          do while (NTEST < 1) 
            !
            ! actualisation du frottement et de la dilatance
            !
            if (alpha == 0.d0) then 
               TAPHI1 = tan(PHI0)
            else   
               TAPHI1 = tan(PHI0)*(DAMN11**alpha)
            endif
            if (beta == 0.d0) then
              TANMU1 = tan(ZMU0)
            else
              TANMU1 = tan(ZMU0)*(DAMN11**beta)
            endif   
            !
            ! on reinitialise l'estimation de la contrainte dans l'interface
            !
            T1FN10 = T1MN10 - (G1IN10 * (1.d0-DAMN11)*RSI)
            T2FN10 = T2MN10 - (G2IN10 * (1.d0-DAMN11)*RSI)
            SIGN10 = SIGN1
            !
            ! calcul d'un nouveau critère de cisaillement
            !
            CRICIS = sqrt((T1FN10**2)+(T2FN10**2)) + SIGN10*TAPHI1

            if (CRICIS > PRECIS) then
              !
              ! initialisation boucle écoulement
              ! 
              ITER = 1
              do while (abs(CRICIS) > PRECIS)
                !
                !   vérification du nombre max itération
                ! 
                if ( ITER > MAXITE ) then
        	  write(6,*) 'le critère de cisaillement est violé'
                  write(6,*) 'Nombre max d itérations dépassé'
                  call faterr('FCZM','erreur') 
                endif
                !
                ! calcul du correcteur plastique
                !
                ! dérivée de G suivant sigma
                DENO = sqrt( (T1FN10**2) + (T2FN10**2) )
                DGIDS1 = T1FN10 / DENO
                DGIDS2 = T2FN10 / DENO
                DZGIDS = TANMU1
                DGIDSN = TAPHI1
                !
                ! rigidité * DGI/DSIGMA
                !
                HODG1 = DGIDS1 * RSM
                HODG2 = DGIDS2 * RSM

                if ( DELNP1 < ECN ) then
                  HODGN = RNMF * DZGIDS
                else
                  HODGN = RNM * DZGIDS
                endif

                GIGI = (DGIDS1*HODG1)+(DGIDS2*HODG2)+(DGIDSN*HODGN)
                !
                ! Résolution équation pour trouver DLAM2, correcteur plastique
                !
                if ( abs(GIGI) <= PRECIS) then
                  write(6,*) 'système indéterminé (F G),itération',ITER
                  call faterr('FCZM','erreur')
                endif

                DLAM2 = CRICIS / GIGI
                !
                ! calcul du nouvel état de contraintes
                !
                T1FN11 = T1FN10 - ((DLAM2*DGIDS1) * RSM)
                T2FN11 = T2FN10 - ((DLAM2*DGIDS2) * RSM)

                if (DELNP1 < ECN) then
                  SIGN11 = SIGN10 - (RNMF * (DLAM2*DZGIDS))
                else
                  SIGN11 = SIGN10 - (RNM * (DLAM2*DZGIDS))
                endif
                !
                ! Mise à jour des valeurs dans la boucle
                !
                T1FN10 = T1FN11
                T2FN10 = T2FN11
                SIGN10 = SIGN11
                ITER = ITER + 1
                !
                ! Nouveau critère
                !
                CRICIS=sqrt((T1FN11**2)+(T2FN11**2))+SIGN11*TAPHI1

              enddo
              !
              ! chargement des valeurs de contraintes de frottement
              !
              T1FNP1 = T1FN10
              T2FNP1 = T2FN10
              !
              ! contrainte normale
              !
              SIGNP1 = SIGN10

              ! on a trouvé RT dans le patin
              ! la force dans le ressort = force mortier - RT
              ! 
              !
              ! déplacement tangentiel d'interface
              ! 

              G1IN10 = (T1MN10 - T1FNP1) / ((1.d0-DAMN11) * RSI)
              G2IN10 = (T2MN10 - T2FNP1) / ((1.d0-DAMN11) * RSI)

              print*,G1IN10,G2IN10,DLIN10

              
              !	
              ! déplacement tangentiel pur
              !
              GAMMA1 = sqrt(G1IN10**2 + G2IN10**2)
              ! déplacement normal pur
              DELTA1 = DLIN10
              ! déplacement mixte
              EPSEQ = sqrt(DLIN10**2 + G1IN10**2 + G2IN10**2)		
              ! calcul de l'endommagement dû à cette déformation d'interface
              DATEST = endo(DELTA0,GAMMA0,ECOHN,ECOHS,RNI,RSI,DELTA1,GAMMA1,EPSEQ,DAMN)

              print*,'c',DATEST
              
            else
              DATEST = DAMN10
            endif
            !
            ! comparaison des endommagements
            !
            if ( (DAMN11 - DATEST) > PRECIS) then
               DAMN11 = (DAMN11+DAMN10)/2.d0
               print*,'n',DAMN11
            else
              NTEST = 1
            endif

          enddo
          
        endif

      endif
      !
      !-Test fin de boucle et dichotomie
      !
      TESTEND = DAMN11 - DAMN10

      print*,'testend ',testend

      DAMN10 = DAMN11

      ITEND = ITEND + 1

      print*,'-----'
            
    enddo 
    !
    !-Déplacements finaux d'interface
    !
    G1INP1 = G1IN10
    G2INP1 = G2IN10
    DLINP1 = DLIN10
    !
    !
    ! SORTIES:   ETAT DE CONTRAINTES FINAL
    !            INCREMENT DE DEPLACEMENT D'INTERFACE ET VARIABLES INTERNES
    !
    !
    !-contraintes finales
    !
    !
    TA1NP1 = (GA1NP1 - G1INP1) * RSM
    TA2NP1 = (GA2NP1 - G2INP1) * RSM

    SIGF(1) = TA1NP1
    SIGF(2) = TA2NP1
    SIGF(3) = SIGNP1 
    !
    !-Incrément de déplacement d'interface
    !
    DEFP(1) = G1INP1 - G1IN
    DEFP(2) = G2INP1 - G2IN
    DEFP(3) = DLINP1 - DLIN
    !
    !-Variables internes finales
    !
    VARF(1) = EPSEQ
    !
    ! limite élastique en cisaillement
    !
    if (SIGNP1 < 0.D0) then
      XLAM1 = TANPHI * SIGNP1
    else
      XLAM1 = 0.D0
    endif

    VARF(2) = XLAM1

    VARF(3) = DAMN10 

    VARF(4) = 0.d0
    
    
end subroutine FCZM


subroutine FCZM_FLP(joint_param,SIG0,EPIN0,VAR0,DEPST,SIGF,DEFP,VARF,bavard)

!    Args:
!
!     SIG0: contrainte initiale
!     EPIN0: deformation inelastique initiale
!     VAR0: variables internes initiales  
!     DEPST: increment de deformation

!    Returns:

!     SIGF: contrainte finale
!     DEFP: increment de deformation inelastique
!     VARF: variables internes finales


    logical :: bavard 
  
    ! contrainte initiale
    real(kind=8) :: SIG0(3)
    ! deformation inelastique initiale
    real(kind=8) :: EPIN0(3)
    ! variables internes initiales  
    !      VAR0(1) deformation plastique equivalente
    !      VAR0(2) XLAM1 (limite elastique en cisaillement)
    !      VAR0(3) ENDO
    !      VAR0(4) pas utilise
    real(kind=8) :: VAR0(4)
    ! increment de deformation    
    real(kind=8) :: DEPST(3)
    ! sortie contrainte, def plastique, variables internes
    real(kind=8) :: SIGF(3),DEFP(3),VARF(4)
    real(kind=8) :: DEPFN,SIGSEI

    ! def inelastique
    real(kind=8) :: EPSS1,EPSS2,EPSN1
    ! def elastique tangente
    real(kind=8) :: DEFS10,DEFS20,DEFN0
    ! def totale pas N
    real(kind=8) :: EPS1N,EPS2N,EPSNN
    ! pred def total pas N+1 
    real(kind=8) :: EP1NP1,EP2NP1,EPNNP1
    ! def elas pas N+1 it 0
    real(kind=8) :: EE1P10,EE2P10,EENP10
    ! contrainte pas N+1 it 0
    real(kind=8) :: S1NP10,S2NP10,SNNP10

    real(kind=8) :: DELEP1,DELEP2,DELEPN
    real(kind=8) :: DEPEQ,ESPEQ

    
    real(kind=8) :: PRESP,PRESM
    real(kind=8) :: PREEP,PREEM
    real(kind=8) :: PRECIS,PRECIN
    real(kind=8) :: EPSEQ,XLAM1

    
    integer      :: iter,maxite

    ! 
    real(kind=8) :: Cprime,Cseconde
    real(kind=8) :: CRICIS,CRINOR,CRICIP,ABSTAU,CRICC

    ! derivee F suivant sigma
    real(kind=8) :: DFIDS1,DFIDS2,DFIDSN

    ! derivee G suivant sigma
    real(kind=8) :: DENO,DGIDS1,DGIDS2,DGIDSN,DZGIDS

    !
    real(kind=8) :: HODF1,HODF2,HODFN
    real(kind=8) :: HODG1,HODG2,HODGN 
    real(kind=8) :: FIFI,FIGI,GIFI,GIGI,DET
    real(kind=8) :: DLAM1,DLAM2,DEFPN
    real(kind=8) :: DEPS1,DEPS2,DEPSN
    real(kind=8) :: S1NP11,S2NP11,SNNP11
    
    real(kind=8) :: delam1,pro1

    real(kind=8) :: B,alpha,beta,damage,ENERN,ENERS,ENTOT,EPS0,AGI1,AGI2,DAMN,DAMN10,DAMN11,&
                    DATEST,DDELT,DELN,DELNP1,DELTA0,DELTA1,DGAM1,DGAM2,&
                    DLIN,DLIN10,DLINP1,ECOHN,ECOHS,G1IN,G1IN10,G1INP1,G2IN,G2IN10,G2INP1,&
                    GA1N,GA1NP1,GA2N,GA2NP1,GAMMA0,GAMMA1,PHI0,PRICIS,RNI,RNM,RNMF,RSI, &
                    RSM,ECN,SIGMN,SIGN1,SIGN10,SIGN11,SIGNP1,T1FN1,T1FN10,T1FN11,T1FNP1,T2FN1,T2FN10,&
                    T2FN11,T2FNP1,TA1N,TA1N10,TA1NP1,TA2N,TA2N10,TA2NP1,TANMU,TANMU1,TANPHI,&
                    TAPHI1,TESTEND,ZMU0

    real(kind=8) :: joint_param(15)
    
    integer      :: ITEND,NTEST

    real(kind=8) :: PDAM

    
                             !123467901234
    character(len=14) :: IAM='models::FCZM'


    !parametres pour le test de nullite des contraintes et déformations

    PRESP = 1D-6
    PRESM = -1D-6

    ! nombre maximum d'itérations

    MAXITE = 1500

    ! précision pour les itérations internes
    PRECIS = 1D-8
    PRECIN = -1D-8

!
!----------Chargement des caractéristiques mécaniques

! 1  2   3   4    5  6   7   8  9  10 11 12 13 14 15
! kt,knc,knt,kncc,ec,phi,zmu,pf,pd,ct,s2,G2,cn,s1,G1
    
!
!   rigidité tangentielle du mortier
    RSM    = joint_param(1)
!   rigidité tangentielle de l'interface
    RSI    = joint_param(10)
!   première rigidité normale du mortier
    RNM    = joint_param(2)
!   seconde rigidité normale du mortier
    RNMF   = joint_param(4)
!   rigidité normale de l'interface
    RNI    = joint_param(13)
!   déformation de transition de rigidité normale pour le mortier
    ECN    = - joint_param(5)
!   angle de frottement maximal
    PHI0   = joint_param(6)*pi_g/180.
!   angle de dilatance maximal
    ZMU0   = joint_param(7)*pi_g/180.
!   Paramètres d'endommagement:
!   seuil d'endommagement en traction pure (dep n critique)
    DELTA0 = joint_param(14)/joint_param(13)
!   seuil d'endommagement en cisaillement pur (dep t critique)
    GAMMA0 = joint_param(11)/joint_param(10)
!   énergie de cohésion en traction pure
    ECOHN  = joint_param(15)
!   énergie de cohésion en cisaillement pur
    ECOHS  = joint_param(12)
!   paramètres d'activation du frottement
    alpha  = joint_param(8)
!   paramètre d'activation de la dilatance
    beta   = joint_param(9)

    !
    !---Chargement des variables internes
    !
    !   déformation équivalente
    EPSEQ  = VAR0(1)
    !   limite élastique en cisaillement 
    XLAM1  = VAR0(2)
    !   endommagement 
    DAMN   = VAR0(3)

    !
    !---Précicision endommagement
    !
    PDAM = PRESP / max(RSI*GAMMA0 , RNI*DELTA0)

    !
    !---Incrément de déformation
    !
    DGAM1 = DEPST(1)
    DGAM2 = DEPST(2)
    DDELT = DEPST(3)
    !
    !---Contraintes finales
    !
    SIGF(1) = 0.D0
    SIGF(2) = 0.D0
    SIGF(3) = 0.D0
    !
    !---Incrément de déformations final à l'interface (DEFP)
    !
    DEFP(1) = 0.D0
    DEFP(2) = 0.D0
    DEFP(3) = 0.D0    
    !
    !
    !   CALCUL DES ETATS DE DEPLACEMENTS AUX PAS N et N+1
    !

    !
    !---Déformations de l'interface au pas N (allongement du ressort endommageable)
    !
    G1IN = EPIN0(1)
    G2IN = EPIN0(2)
    DLIN = EPIN0(3)
    !
    !---Etat de contraintes dans le joint au pas N (== mortier == interface)
    !
    TA1N  = SIG0(1)
    TA2N  = SIG0(2)
    SIGMN = SIG0(3)
    !
    !---Déplacements dans le joint au pas N
    !
    !   déplacement tangentiel (mortier + interface)
    GA1N = TA1N / RSM + G1IN 
    GA2N = TA2N / RSM + G2IN
    !
    !   déplacement normal, plusieurs pentes
    DELN = SIGMN / RNM + DLIN 
    if ( DELN < ECN ) then
      DELN = ECN + ( SIGMN - (ECN*RNM) )/RNMF 
    endif
    !
    !---Déplacements au pas N+1 dans le joint
    !   on cherche à savoir comment ça va se répartir entre mortier et interface (effet endo et frottement)
    !
    GA1NP1 = GA1N + DGAM1
    GA2NP1 = GA2N + DGAM2
    DELNP1 = DELN + DDELT
    
    !
    !   CALCUL ITERATIF DES DEPLACEMENTS D'INTERFACE ET DE L'ENDOMMAGEMENT
    !
    !
    !---initialisation de la boucle sur l'endommagement
    !

    !fd
    G1IN10=G1IN
    G2IN10=G2IN
    
    DAMN10 = DAMN
    DAMN11 = -1.D0
    !
    TESTEND = DAMN10 - DAMN11 
    ITEND = 1
    !
    do while ( abs(TESTEND) > PDAM ) 
      !
      !   DEPLACEMENT NORMAL D'INTERFACE
      !
      !   ici les choses sont simples car ressort et contact ne travaillent jamais ensemble
       
      if ( DELNP1 > 0.D0 ) then
        ! Cas traction ; 2 ressorts en serie         
        DLIN10 = DELNP1 * RNM / ( (1.d0-DAMN10)*RNI + RNM)
      else
        ! Cas compression ; pas d'allongement de l'interface
        DLIN10 = 0.D0
      endif

      !   
      !   CALCUL DE LA CONTRAINTE DE FROTTEMENT DE L'INTERFACE
      !
      !   la force tangentielle dans l'interface est la somme de la force du ressort et du "patin frottant"
      !   ici on cherche la contribution du frottement pour en déduire la contribution du ressort 

      !
      !---Contrainte de l'interface complet au pas N+1 (== contrainte mortier)
      !
      
      ! fd cette proposition de FLP ne me semble pa correcte, car l'increment de déformation
      ! n'est pas totalement transformé en deformation du mortier 
      
      ! !-----Estimation de la contrainte dans le mortier au pas N+1
      ! !
      ! TA1N10 = TA1N + DGAM1*RSM
      ! TA2N10 = TA2N + DGAM2*RSM
      ! !
      ! !-----Estimation contrainte de frottement
      ! !
      ! T1FN1 = TA1N10 - G1IN * (1.d0-DAMN)*RSI
      ! T1FN10 = T1FN1
      ! T2FN1 = TA2N10 - G2IN * (1.d0-DAMN)*RSI
      ! T2FN10 = T2FN1

      ! fd on se sert de la dernière estimation de l'allongement de l'interface pour
      !    deduire l'effort dans le mortier et par action/reaction celle dans l'interface
      
      T1FN1 = (GA1NP1-G1IN10)*RSM 
      T1FN10 = T1FN1 - (G1IN10 * (1.d0-DAMN10)*RSI)
      T2FN1 = (GA2NP1-G2IN10)*RSM 
      T2FN10 = T2FN1 - (G2IN10 * (1.d0-DAMN10)*RSI)

      !
      !---Contrainte normale au pas N+1
      !
      if (DELNP1 >= ECN) then
        SIGN1 = (DELNP1 - DLIN10) * RNM
      else
        SIGN1 = ECN * RNM + (DELNP1 - ECN) * RNMF
      endif
      SIGN10 = SIGN1

      ! fd
      ! maintenant on va projeter cette contrainte totale de l'interface sur les critères pour avoir une estimation
      ! de la contribution du contact-frottant et en deduire la contribution des efforts dans les ressorts et ainsi
      ! calculer l'evolution de l'endommagement.
      ! Ici c'est le critère de Coulomb classique (pas de cohésion)
      !
       
      !
      !---frottement
      !
      if (alpha == 0.d0) then
        TANPHI = tan(PHI0)
      else
        TANPHI = tan(PHI0) * DAMN10**alpha
      endif   
      !
      !---dilatance
      !
      if ( DELNP1 > 0.D0 ) then
        ! traction 
        TANMU = 0.D0
      else
        ! compression
        if (beta==0.d0) then 
          TANMU = tan(ZMU0)
        else
           TANMU = tan(ZMU0) * DAMN10**beta
        endif   
      endif
      !
      !-Calcul du critère de cisaillement
      !
      CRICIS = sqrt((T1FN10**2) + (T2FN10**2)) + SIGN10*TANPHI
      !
      !-Critère écoulement normal
      !
      if ( abs(TANMU) > PRESP ) then
        CRINOR = SIGN10/TANMU - sqrt((T1FN10**2)+(T2FN10**2))
      else
        CRINOR = SIGN10
      endif 

      !
      !-Aiguillage selon les différents critères
      !
      
      if ((CRICIS <= PRECIS) .and. (CRINOR <= PRECIN)) then
        !
        ! Tous les critères sont vérifiés on est dans le cone => compression/collant
        !
         
        ! print*,'I'
         
        !
        ! contrainte normale
        ! 
        SIGNP1 = SIGN10
        !
        ! contraintes finales de frottement
        !
        T1FNP1 = T1FN10
        T2FNP1 = T2FN10
        
      else if ((CRICIS > PRECIS) .and. (CRINOR <= PRECIN)) then
        !
        ! Cisaillement violé , compression respecté => compression/glissement avec frottement
        !
        
        ! print*,'II'

        !
        ! initialisation boucle écoulement
        ITER = 1
        !
        do while (abs(CRICIS) > PRECIS)
          !
          ! vérification du nombre max itération
          if ( ITER > MAXITE ) then
	    write(6,*) 'le critère de cisaillement est violé'
            write(6,*) 'Nombre max d itérations dépassé'
            call faterr('FCZM','erreur') 
          endif
          !
          ! calcul du correcteur plastique
          !
          ! dérivée de G suivant sigma
          !
          DENO = sqrt( (T1FN10**2) + (T2FN10**2) )
          DGIDS1 = T1FN10 / DENO
          DGIDS2 = T2FN10 / DENO
          DZGIDS = TANMU
          DGIDSN = TANPHI
          !
          ! rigidité * DGI/DSIGMA
          !
          HODG1 = DGIDS1 * RSM
          HODG2 = DGIDS2 * RSM
          !
          if ( DELNP1 < ECN ) then
            HODGN = RNMF * DZGIDS
          else
            HODGN = RNM * DZGIDS
          endif
          !
          GIGI = (DGIDS1*HODG1)+(DGIDS2*HODG2)+(DGIDSN*HODGN)
          !
          ! Résolution équation pour trouver DLAM2, correcteur plastique
          !
          if ( abs(GIGI) <= PRECIS) then
             write(6,*) 'système indéterminé (F G),itération',ITER
             call faterr('FCZM','erreur')
          endif
          !
          DLAM2 = CRICIS / GIGI
          !
          ! Calcul du nouvel état de contraintes
          !
          T1FN11 = T1FN10 - ((DLAM2*DGIDS1) * RSM)
          T2FN11 = T2FN10 - ((DLAM2*DGIDS2) * RSM)
          !
          if (DELNP1 < ECN) then
            SIGN11 = SIGN10 - (RNMF * (DLAM2*DZGIDS))
          else
            SIGN11 = SIGN10 - (RNM * (DLAM2*DZGIDS))
          endif
          !
          ! Mise à jour des valeurs dans la boucle
          T1FN10 = T1FN11
          T2FN10 = T2FN11
          SIGN10 = SIGN11
          ITER = ITER + 1
          !
          ! nouveau test de convergence
          !
          CRICIS=sqrt((T1FN11**2)+(T2FN11**2))+SIGN11*TANPHI
          !
        enddo
        !
        ! compression cisaillement vérifié
        !
        
        !
        ! chargement des valeurs de contraintes de frottement
        !
        T1FNP1 = T1FN10
        T2FNP1 = T2FN10
        !
        ! contrainte normale
        !
        SIGNP1 = SIGN10
        
      else
        !
        ! Les deux critères sont violés --> traction + glissement sans frottement
        !
         
        ! print*,'III'
         
        !
        ! contraintes finales de frottement
        ! 
        T1FNP1 = 0.D0
        T2FNP1 = 0.D0

        ! 
        ! contrainte normale (la reaction dans le ressort d'interface)
        !
        SIGNP1 = SIGN10

      endif	
      !
      !
      !   DEPLACEMENT TANGENTIEL DE L'INTERFACE
      !
      !

      !
      if ( abs(DAMN10 - 1.D0) > PDAM) then

        ! old way que je ne m'explique pas         

        ! deformation du ressort equivalente à la contribution du frottement 
        AGI1 = T1FNP1 / ((1.d0-DAMN10) * RSI)
        AGI2 = T2FNP1 / ((1.d0-DAMN10) * RSI)
           
        ! if (DAMN10 > 0.d0) print*,itend,DAMN10,AGI1,AGI2,GA1NP1,GA2NP1
           
        !
        ! déplacement tangentiel d'interface
        !
         
        G1IN10 = RSM*(GA1NP1 + AGI1)/(RSM + (1.D0-DAMN10)*RSI) 
        G1IN10 = G1IN10 - AGI1
        G2IN10 = RSM*(GA2NP1 + AGI2)/(RSM + (1.D0-DAMN10)*RSI)
        G2IN10 = G2IN10 - AGI2

      else
        !
        ! déplacement tangentiel d'interface
        ! 
        G1IN10 = GA1NP1 - T1FNP1/RSM
        G2IN10 = GA2NP1 - T2FNP1/RSM 

      endif
      !
      !   MISE A JOUR DE L'ENDOMMAGEMENT	
      !
      if ( abs(DAMN10 - 1.D0) <= PDAM) then

        DAMN11 = 1.D0

      else
        !
        !-Déplacements équivalents à l'interface
        !
        ! déplacement tangentiel pur
        GAMMA1 = sqrt(G1IN10**2 + G2IN10**2)
        ! déplacement normal pur
        DELTA1 = DLIN10
        ! déplacement mixte
        EPSEQ = sqrt(DLIN10**2 + G1IN10**2 + G2IN10**2)
        !
        !-calcul nouvel endommagement
        !
        DAMN11 = endo(DELTA0,GAMMA0,ECOHN,ECOHS,RNI,RSI,DELTA1,GAMMA1,EPSEQ,DAMN)

        TESTEND = DAMN11 - DAMN10
        
        !
        !-Limiteur d'endommagement (dans le cas de compression cisaillement)
        ! la methode utilisee precedemment va surevaluer l'endommagement
        ! on reapplique la methode en partant du nouvel endommagement trouve et
        ! si on trouve un nouvel endommagement plus petit que le precedent on itere en diminuant l'endommagement
        !

        !fd me semble plus legitime
        !fd  if ((SIGN1 < PRECIN) .and. (DAMN11 > DAMN10)) then
        if ((SIGNP1 < PRECIN) .and. (DAMN11 > DAMN10)) then           
          NTEST = 0
          ! Boucle adoucissement 
          do while (NTEST < 1) 
            !
            ! actualisation du frottement et de la dilatance
            !
            if (alpha == 0.d0) then 
               TAPHI1 = tan(PHI0)
            else   
               TAPHI1 = tan(PHI0)*(DAMN11**alpha)
            endif
            if (beta == 0.d0) then
              TANMU1 = tan(ZMU0)
            else
              TANMU1 = tan(ZMU0)*(DAMN11**beta)
            endif   
            !
            ! on reinitialise l'estimation de la contrainte dans l'interface
            !
            T1FN10 = T1FN1 - (G1IN10 * (1.d0-DAMN11)*RSI)
            T2FN10 = T2FN1 - (G2IN10 * (1.d0-DAMN11)*RSI)
            SIGN10 = SIGN1
            !
            ! calcul d'un nouveau critère de cisaillement
            !
            CRICIS = sqrt((T1FN10**2)+(T2FN10**2)) + SIGN10*TAPHI1

            if (CRICIS > PRECIS) then
              !
              ! initialisation boucle écoulement
              ! 
              ITER = 1
              do while (abs(CRICIS) > PRECIS)
                !
                !   vérification du nombre max itération
                ! 
                if ( ITER > MAXITE ) then
		  write(6,*) 'le critère de cisaillement est violé'
                  write(6,*) 'Nombre max d itérations dépassé'
                  call faterr('FCZM','erreur') 
                endif
                !
                ! calcul du correcteur plastique
                !
                ! dérivée de G suivant sigma
                DENO = sqrt( (T1FN10**2) + (T2FN10**2) )
                DGIDS1 = T1FN10 / DENO
                DGIDS2 = T2FN10 / DENO
                DZGIDS = TANMU1
                DGIDSN = TAPHI1
                !
                ! rigidité * DGI/DSIGMA
                !
                HODG1 = DGIDS1 * RSM
                HODG2 = DGIDS2 * RSM

                if ( DELNP1 < ECN ) then
                  HODGN = RNMF * DZGIDS
                else
                  HODGN = RNM * DZGIDS
                endif

                GIGI = (DGIDS1*HODG1)+(DGIDS2*HODG2)+(DGIDSN*HODGN)
                !
                ! Résolution équation pour trouver DLAM2, correcteur plastique
                !
                if ( abs(GIGI) <= PRECIS) then
                  write(6,*) 'système indéterminé (F G),itération',ITER
                  call faterr('FCZM','erreur')
                endif

                DLAM2 = CRICIS / GIGI
                !
                ! calcul du nouvel état de contraintes
                !
                T1FN11 = T1FN10 - ((DLAM2*DGIDS1) * RSM)
                T2FN11 = T2FN10 - ((DLAM2*DGIDS2) * RSM)

                if (DELNP1 < ECN) then
                  SIGN11 = SIGN10 - (RNMF * (DLAM2*DZGIDS))
                else
                  SIGN11 = SIGN10 - (RNM * (DLAM2*DZGIDS))
                endif
                !
                ! Mise à jour des valeurs dans la boucle
                !
                T1FN10 = T1FN11
                T2FN10 = T2FN11
                SIGN10 = SIGN11
                ITER = ITER + 1
                !
                ! Nouveau critère
                !
                CRICIS=sqrt((T1FN11**2)+(T2FN11**2))+SIGN11*TAPHI1

              enddo
              !
              ! chargement des valeurs de contraintes de frottement
              !
              T1FNP1 = T1FN10
              T2FNP1 = T2FN10
              !
              ! contrainte normale
              !
              SIGNP1 = SIGN10
              !
              AGI1 = T1FNP1 / ((1.d0-DAMN11) * RSI)
              AGI2 = T2FNP1 / ((1.d0-DAMN11) * RSI)
              !
              ! déplacement tangentiel d'interface
              !
              G1IN10 = (GA1NP1 + AGI1)/(RSM + (1.d0-DAMN11)*RSI) 
              G1IN10 = G1IN10 * RSM - AGI1
              G2IN10 = (GA2NP1 + AGI2)/(RSM + (1.d0-DAMN11)*RSI)
              G2IN10 = G2IN10 * RSM - AGI2
              !	
              ! déplacement tangentiel pur
              !
              GAMMA1 = sqrt(G1IN10**2 + G2IN10**2)
              ! déplacement normal pur
              DELTA1 = DLIN10
              ! déplacement mixte
              EPSEQ = sqrt(DLIN10**2 + G1IN10**2 + G2IN10**2)		
              ! calcul de l'endommagement dû à cette déformation d'interface
              DATEST = endo(DELTA0,GAMMA0,ECOHN,ECOHS,RNI,RSI,DELTA1,GAMMA1,EPSEQ,DAMN)

            else
              DATEST = DAMN10
            endif
            !
            ! comparaison des endommagements
            !
            if ( (DAMN11 - DATEST) > PRECIS) then
              DAMN11 = (DAMN11+DAMN10)/2.d0
            else
	      NTEST = 1
            endif

          enddo

        endif

      endif
      !
      !-Test fin de boucle et dichotomie
      !
      TESTEND = DAMN11 - DAMN10

      DAMN10 = DAMN11

      ITEND = ITEND + 1

    enddo 
    !
    !-Déplacements finaux d'interface
    !
    G1INP1 = G1IN10
    G2INP1 = G2IN10
    DLINP1 = DLIN10
    !
    !
    ! SORTIES:   ETAT DE CONTRAINTES FINAL
    !            INCREMENT DE DEPLACEMENT D'INTERFACE ET VARIABLES INTERNES
    !
    !
    !-contraintes finales
    !
    !
    TA1NP1 = (GA1NP1 - G1INP1) * RSM
    TA2NP1 = (GA2NP1 - G2INP1) * RSM

    SIGF(1) = TA1NP1
    SIGF(2) = TA2NP1
    SIGF(3) = SIGNP1 
    !
    !-Incrément de déplacement d'interface
    !
    DEFP(1) = G1INP1 - G1IN
    DEFP(2) = G2INP1 - G2IN
    DEFP(3) = DLINP1 - DLIN
    !
    !-Variables internes finales
    !
    VARF(1) = EPSEQ
    !
    ! limite élastique en cisaillement
    !
    if (SIGNP1 < 0.D0) then
      XLAM1 = TANPHI * SIGNP1
    else
      XLAM1 = 0.D0
    endif

    VARF(2) = XLAM1

    VARF(3) = DAMN10 

    VARF(4) = 0.d0
    
    
end subroutine FCZM_FLP


real(kind=8) FUNCTION ENDO(DELTA0,GAMMA0,ECOHN,ECOHS,RNI,RSI,DELTA1,GAMMA1,EPSEQ,DAMN)
!
!     
!             CALCUL VARIABLE D'ENDOMMAGEMENT TYPE MAZAR 
!               MODELE JOINT ENDOMMAGEABLE PLASTIQUE
!
! ENTREES
!     DAMN            = ENDOMMAGEMENT DE L'INTERFACE AU PAS PRECEDENT
!     DELTA0          = LIMITE ELASTIQUE EN TRACTION
!     GAMMA0          = LIMITE ELASTIQUE EN CISAILLEMENT
!     ECOHN           = ENERGIE DE COHESION EN TRACTION
!     ECOHS           = ENERGIE DE COHESION EN CISAILLEMENT
!     RNI             = RIGIDITE NORMALE DE L'INTERFACE
!     RSI             = RIGIDITE TANGENTIELLE DE L'INTERFACE
!     DELTA1          = DEPLACEMENT NORMAL DE l'INTERFACE
!     GAMMA1          = DEPLACEMENT TANGENTIEL DE l'INTERFACE
!     EPSEQ           = DEPLACEMENT TOTAL DE L'INTERFACE  
!
! SORTIE
!     ENDO            = VARIABLE D'ENDOMMAGEMENT ACTUALISEE
!
!
      IMPLICIT NONE
      real(kind=8) :: DELTA0,GAMMA0,ECOHN,ECOHS,RNI,RSI,DELTA1,GAMMA1,EPSEQ,DAMN
      real(kind=8) :: PRECIS
      real(kind=8) :: B,beta,DAMAGE,ENERN,ENERS,ENTOT,EPS0,SIG0

!-----Paramètre de test de nullité
!
      PRECIS = 1.D-10
!
!-----Trop peu de déplacement dans l'interface
!
      if ( abs(EPSEQ) < PRECIS) then
         DAMAGE = 0.D0
      else
!
!---------Aiguillage mode d'endommagement
!
         if (abs(GAMMA1) < PRECIS) then
!          mode traction pur
!          write(*,*) 'endo traction pure'
!
!          seuil d'endommagement
           EPS0 = DELTA0
!
!          critère d'endommagement
           B = (2.d0 * DELTA0)/(2.d0*ECOHN/RNI - DELTA0**2)

         else if (abs(DELTA1) < PRECIS) then
!          mode cisaillement pur
!          write(*,*) 'endo cisaillement pur'
!
!          seuil d'endommagement
           EPS0 = GAMMA0
!
!          critère d'endommagement
           B = (2.d0 * GAMMA0)/(2.d0*ECOHS/RSI - GAMMA0**2)
            
         else
!          mode mixte
!          write(*,*) 'endo mixte'
!
!          ratio de mixité
           beta = GAMMA1 / DELTA1
!
!          seuil d'endommagement
           EPS0 = sqrt((1.d0+beta**2)/(GAMMA0**2+(beta**2)*(DELTA0**2)))
	   EPS0 = EPS0*DELTA0*GAMMA0
!
!          contrainte de limite d'élasticité
           SIG0 = (RNI**2+(beta**2)*(RSI**2))
           SIG0 = sqrt(SIG0/(GAMMA0**2+(beta**2)*(DELTA0**2)))
           SIG0 = SIG0*DELTA0*GAMMA0 
!
!          énergie de cohésion liée à la traction
           ENERN = (ECOHN*GAMMA0**2)/(GAMMA0**2+(beta**2)*(DELTA0**2))
!
!          énergie de cohésion liée au cisaillement
           ENERS = ECOHS*(beta**2)*(DELTA0**2)
           ENERS = ENERS/(GAMMA0**2+(beta**2)*(DELTA0**2))
!
!          énergie totale de cohésion
           ENTOT = ENERN + ENERS
!
!          critère d'endommagement
           B = (2.d0 * SIG0)/(2.d0*ENTOT - EPS0*SIG0)
            
         endif
!
!----------Calcul de l'endommagement
!
         DAMAGE = 1.D0 - (EPS0/EPSEQ) * exp( - B * (EPSEQ - EPS0) ) 
!
      endif
!
!----------Sortie: valeur max de l'histoire de l'endo
!
      ENDO = MAX( 0.D0 , DAMN , DAMAGE )
!
      RETURN

END function ENDO
    

  
!------------------------------------------------------------------------
! THERx part
!------------------------------------------------------------------------
!!$ !HARA!TER(len=5) FUN!TION get_capacity_storage_THERx(imodel)
!!$  IMPLI!IT NONE
!!$  INTEGER :: imodel,i
!!$  LOGI!AL :: exist
!!$
!!$  exist=.FALSE.
!!$
!!$  IF (ASSO!IATED(modelz(imodel)%optns%ID)) THEN
!!$    DO i=1,SIZE(modelz(imodel)%optns%ID)
!!$      IF (modelz(imodel)%optns%ID(i) == 'cstrg') THEN
!!$        exist=.TRUE.
!!$        get_capacity_storage_THERx=modelz(imodel)%optns%value(i)
!!$        EXIT
!!$      ENDIF
!!$    ENDDO
!!$  ENDIF
!!$
!!$  IF (.NOT. exist) THEN
!!$
!!$    PRINT*,'You should specify the cstrg option (storage scheme)'
!!$    PRINT*,'!heck MODELS.DAT                                    '
!!$    PRINT*,'Default is taken as cstrg lump_                     '
!!$
!!$    get_capacity_storage_THERx='lump_'
!!$
!!$  ENDIF
!!$
!!$ END FUN!TION get_capacity_storage_THERx
!!$!------------------------------------------------------------------------
!!$!------------------------------------------------------------------------
!!$ !HARA!TER(len=5) FUN!TION get_cplm__THERx(imodel)
!!$  IMPLI!IT NONE
!!$  INTEGER :: imodel,i
!!$  LOGI!AL :: exist
!!$
!!$  exist=.FALSE.
!!$
!!$  IF (ASSO!IATED(modelz(imodel)%optns%ID)) THEN
!!$    DO i=1,SIZE(modelz(imodel)%optns%ID)
!!$      IF (modelz(imodel)%optns%ID(i) == 'cplm_') THEN
!!$        exist=.TRUE.
!!$        get_cplm__THERx=modelz(imodel)%optns%value(i)
!!$        EXIT
!!$      ENDIF
!!$    ENDDO
!!$  ENDIF
!!$
!!$  IF (.NOT. exist) THEN
!!$
!!$    PRINT*,'You should specify the cplm_ option'
!!$    PRINT*,'!heck MODELS.DAT                   '
!!$    PRINT*,'Default is taken as cplm_ no___    '
!!$
!!$    get_cplm__THERx='no___'
!!$
!!$  ENDIF
!!$
!!$ END FUN!TION get_cplm__THERx
!!$!------------------------------------------------------------------------
!!$!------------------------------------------------------------------------
!!$ !HARA!TER(len=5) FUN!TION get_formulation_THERx(imodel)
!!$  IMPLI!IT NONE
!!$  INTEGER :: imodel,i
!!$  LOGI!AL :: exist
!!$
!!$  exist=.FALSE.
!!$
!!$  IF (ASSO!IATED(modelz(imodel)%optns%ID)) THEN
!!$    DO i=1,SIZE(modelz(imodel)%optns%ID)
!!$      IF (modelz(imodel)%optns%ID(i) == 'form_') THEN
!!$        exist=.TRUE.
!!$        get_formulation_THERx=modelz(imodel)%optns%value(i)
!!$        EXIT
!!$      ENDIF
!!$    ENDDO
!!$  ENDIF
!!$
!!$  IF (.NOT. exist) THEN
!!$
!!$    PRINT*,'You need to specify the form_ option'
!!$    PRINT*,'!heck MODELS.DAT                    '
!!$
!!$    STOP
!!$
!!$  ENDIF
!!$ END FUN!TION get_formulation_THERx
!!$!------------------------------------------------------------------------------!
!  !alcul de la matrice de conductivite isoparametrique
!
! 2D   D(2,2)
! 3D   D(3,3)
!------------------------------------------------------------------------------!
SUBROUTINE COCO_ISO(ppsnb,D,coco)

 IMPLICIT NONE

 INTEGER                         :: ppsnb,mdlnb,lawnb
 
 REAL(KIND=8),POINTER       :: D(:,:)
 REAL(KIND=8),OPTIONAL      :: coco
 REAL(KIND=8)               :: conductivity
 logical :: existe
 
 existe = present(coco)
 
 ! Initialisations des nouveaux pointeurs
 IF(ASSOCIATED(D)) THEN ; DEALLOCATE(D) ; NULLIFY(D) ; ENDIF
 
 if (existe) then
   conductivity = coco
 else  
   CALL get_ppset_value(ppsnb,mdlnb,lawnb)
   conductivity = get_coco(lawnb)
 endif

 ALLOCATE(D(3,3))
 D=RESHAPE( (/  conductivity , 0.D0  , 0.D0  ,  &
                0.D0 ,  conductivity , 0.D0  ,  &
                0.D0 , 0.D0  ,  conductivity  /),(/3,3/) )    
END SUBROUTINE COCO_ISO

!------------------------------------------------------------------------
SUBROUTINE comp_flux(ppsnb,Gloc,Floc,coco)
  IMPLICIT NONE
  INTEGER                         :: ppsnb
  REAL(kind=8),DIMENSION(:) :: Gloc,Floc 
  REAL(KIND=8) , POINTER :: D(:,:)      ! matrice de comportement
  REAL(KIND=8),OPTIONAL      :: coco
  !
  ! modifier cette taille qui est mise en dur
  !
  REAL(kind=8),DIMENSION(21) :: elas_coeff             
  REAL(KIND=8)               :: conductivity

  nullify(D)  

  CALL COCO_ISO(ppsnb,D,coco)

  Floc(1:SIZE(D,dim=1)) = MATMUL(D,Gloc(1:SIZE(D,dim=1)))

  deallocate(D)
  nullify(D)

END SUBROUTINE comp_flux
!------------------------------------------------------------------------
! API pour creation modele a la main
!------------------------------------------------------------------------
SUBROUTINE set_nb_models(i4)
  implicit none
  integer :: i4,errare
  !                          123456789012345678901 
  CHARACTER(len=21)  :: IAM='models::set_nb_models'


  if (allocated(modelz) ) then 
    CALL FATERR(IAM,'modelz already allocated')
  endif
 
  ALLOCATE(modelz(i4),stat=errare)
  IF (errare /= 0) THEN
    CALL FATERR(IAM,'error allocating modelz')
  END IF

end subroutine

SUBROUTINE set_model(i4,model,mdlty,ID)
  implicit none
  integer :: i4
  !                          12345678901234567
  CHARACTER(len=17)  :: IAM='models::set_model'
  !
  CHARACTER(len=5)      :: model ! nickname
  CHARACTER(len=5)      :: mdlty ! MECAx, THERMx, ...
  CHARACTER(len=5)      :: ID    ! T3xxx, ...

  modelz(i4)%model=model
  modelz(i4)%mdlty=mdlty
  modelz(i4)%ID   =ID

  nullify(modelz(i4)%optns%ID,modelz(i4)%optns%value)

  modelz(i4)%nb_external_variables=0
  modelz(i4)%nb_internal_variables=0

  modelz(i4)%nb_external_variables_bundled=0
  modelz(i4)%nb_internal_variables_bundled=0

  nullify(modelz(i4)%ext_descriptor,modelz(i4)%int_descriptor)

  modelz(i4)%is_a_user_model=.false.

  modelz(i4)%nb_fields=0
  nullify(modelz(i4)%field_name)

END SUBROUTINE

INTEGER FUNCTION get_model_nb(model)
  
  IMPLICIT NONE
  
  INTEGER          :: imodelz
  CHARACTER(len=5) :: model
  
  get_model_nb = 0
  
  do imodelz = 1, size(modelz)
     if (model == modelz(imodelz)%model) then
        get_model_nb = imodelz
        return
     end if
  end do
  
end function get_model_nb

subroutine clean_memory()
  implicit none
  integer(kind=4) :: i, j

  if( allocated(modelz) ) then
    do i = 1, size(modelz)
      if( associated(modelz(i)%optns%ID)       ) deallocate(modelz(i)%optns%ID)
      if( associated(modelz(i)%optns%value)    ) deallocate(modelz(i)%optns%value)
      if( associated(modelz(i)%ext_descriptor) ) deallocate(modelz(i)%ext_descriptor)
      if( associated(modelz(i)%int_descriptor) ) deallocate(modelz(i)%int_descriptor)
      if( associated(modelz(i)%field_name)     ) deallocate(modelz(i)%field_name)
      if( associated(modelz(i)%vfield_name)    ) deallocate(modelz(i)%vfield_name)
    end do
    deallocatE(modelz)
  end if
  
  if( allocated(check_ppset_map) ) deallocate(check_ppset_map)
  if( allocated(ppset) ) deallocate(ppset)

  ! do something on linked list ?

  nb_ppset = 0

end subroutine

!> Get the largest number of external/internal variables for a given physics
subroutine get_max_field_sizes(mdlty, nb_external, nb_internal)
  implicit none
  !> phyics type (1:meca, 2:ther, 3:poro, 4:multi)
  integer, intent(in)  :: mdlty
  !> number of external variables
  integer, intent(out) :: nb_external
  !> number of internal variables
  integer, intent(out) :: nb_internal
  !
  integer :: i_model

  nb_external = 0
  nb_internal = 0

  if( .not. allocated(modelz) ) return

  select case( mdlty )
  case( 1 )

    do i_model = 1, size(modelz)
      if( modelz(i_model)%mdlty /= 'MECAx' ) cycle
      nb_external = max(nb_external, modelz(i_model)%nb_external_variables)
      nb_internal = max(nb_internal, modelz(i_model)%nb_internal_variables)
    end do

  case( 2 )

    do i_model = 1, size(modelz)
      if( modelz(i_model)%mdlty /= 'THERM' ) cycle
      nb_external = max(nb_external, modelz(i_model)%nb_external_variables)
      nb_internal = max(nb_internal, modelz(i_model)%nb_internal_variables)
    end do

  case( 3 )

    do i_model = 1, size(modelz)
      if( modelz(i_model)%mdlty /= 'POROx' ) cycle
      nb_external = max(nb_external, modelz(i_model)%nb_external_variables)
      nb_internal = max(nb_internal, modelz(i_model)%nb_internal_variables)
    end do

  case( 4 )

    do i_model = 1, size(modelz)
      if( modelz(i_model)%mdlty /= 'MULTI' ) cycle
      nb_external = max(nb_external, modelz(i_model)%nb_external_variables)
      nb_internal = max(nb_internal, modelz(i_model)%nb_internal_variables)
    end do

  end select
  
end subroutine get_max_field_sizes

END MODULE models  

