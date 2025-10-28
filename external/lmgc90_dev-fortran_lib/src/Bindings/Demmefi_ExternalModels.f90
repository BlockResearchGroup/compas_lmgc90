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

module DemmefiExternalModels

  use Demmefi, only : mats, endo3d

  use overall, only : nbDIME

  use utilities, only : logmes, faterr, get_io_unit

  use models, only : modelz               , &
                     get_nb_models        , &
                     get_nb_ppsets        , &
                     get_ppset_value      , &
                     get_eleop_id         , &
                     get_eleop_value      , &
                     get_eleop_value_bypps, &
                     get_nb_external_variables

  implicit none

  public push_model,push_behaviour,check_ppset,set_nb_ppsets,clean_memory,compute_external_gp

  contains

! wrapper
!------------------------------------------------------------------------
  subroutine push_model(imodel,itchatche)
    implicit none
    integer :: imodel
    logical :: itchatche

    integer :: itmp,i,lchaine
    character(len=80) :: chaine

    !description des variables internes
    character(len=4),dimension(22) :: idescriptor
    integer         ,dimension(22) :: itype
    integer         ,dimension(22) :: isize

                              !1234567890123456789012345678901234
    character(len=34)  :: IAM='Demmefi_ExternalModels::push_model'
    
    if (nbDIME /= 3) call faterr(IAM,'model available only in 3D')

    if (itchatche) call logmes('Entering : '//IAM)
    
    if (itchatche) print*,' num de modele: ',imodel
    
    ! partie qui concerne les variables "externes" : gradients et flux
    ! rq: ici il n'y a qu'un variable dans gradient et flux

    itmp=6
    modelz(imodel)%nb_external_variables = itmp
    if (itchatche) print*,'nb external ', itmp

    ! on va stocker les noms des variables externes
    if( associated(modelz(imodel)%ext_descriptor) ) deallocate(modelz(imodel)%ext_descriptor)
    allocate(modelz(imodel)%ext_descriptor(itmp))
    
    itmp=1
    modelz(imodel)%nb_external_variables_bundled = itmp 
    if (itchatche) print*,'nb external bundled ', itmp

    do i=1,modelz(imodel)%nb_external_variables_bundled
      itmp=3 ! tenseur symetrique
      modelz(imodel)%ext_descriptor(i)%type=itmp
      !
      itmp=6
      modelz(imodel)%ext_descriptor(i)%size=itmp
      !
      modelz(imodel)%ext_descriptor(i)%index=0
      modelz(imodel)%ext_descriptor(i)%size=6
      !
      chaine='Stress'
      lchaine=6
                                                     
      modelz(imodel)%ext_descriptor(i)%label_primal=' '
      modelz(imodel)%ext_descriptor(i)%label_primal(1:lchaine)=chaine(1:lchaine)

      if (itchatche) print*,i,modelz(imodel)%ext_descriptor(i)%label_primal(1:lchaine),lchaine

      !
      chaine='Strain'
      lchaine=6
                                                           
      modelz(imodel)%ext_descriptor(i)%label_dual=' '
      modelz(imodel)%ext_descriptor(i)%label_dual(1:lchaine)=chaine(1:lchaine)

      if (itchatche) print*,i,modelz(imodel)%ext_descriptor(i)%label_dual(1:lchaine),lchaine

    enddo

    ! partie qui concerne les variables "internes" du modele
    ! rq: il peut y avoir des variables regroupees avec les variables classiques (bundled) 

    ! Description des variables internes :
    ! - Les scalaires :
    ! 'PPAS' indicateur premier pas (1 si premier pas passé sinon 0)
    ! 'CDP ' Cdp actuel
    ! 'PC'   pression de consolidation de CC
    ! 'PT'   tension limite de CC
    ! 'WPL0' ouverture de fissure maxi actuelle
    ! 'ERGF' erreur sur la dissipation de traction pour ne pas avoir de snap back
    ! 'DTPP' endo prepic traction
    ! 'DCPP' endo prepic compression
    ! 'DPEQ' deformation equivalente de cisaillement de DP
    ! 'DCOM' endo maxi compression
    ! 'DTRA' deformation equivalente de cisaillement de DP
    ! 'RTHC' effet de la temperature sur les parametres de compression
    ! 'RTHT' effet de la temperature sur les parametres de traction
    ! 'RTHE' effet de la temperature sur les parametres d elasticite
    ! 'TMAX' temperature maximale atteinte
    ! - Les tenseurs : 
    ! 'SEM' 1 contrainte effective matrice              
    ! 'EPE' 2 deformations elastiques de la matrice           
    ! 'EPT' 3 deformations plastique de traction de la matrice            
    ! 'EPC' 4 deformations plastique de cisaillement de la matrice           
    ! 'EPP' 5 deformations plastique d effondrement de porosite de la matrice           
    ! 'WPL' 6 ouverture de fissure
    ! 'WPX' 7 ouverture maxi

    idescriptor=(/ 'PPAS','CDP ','PC  ','PT  ','WPL0','ERGF','DTPP','DCPP', &
                   'DPEQ','DCOM','DTRA','RTHC','RTHT','RTHE','TMAX',        &
                   'SEM ','EPE ','EPT ','EPC ','EPP ','WPL ','WPX '         /)

    itype      =(/ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,                           &   
                   3,3,3,3,3,3,3                                            /)

    isize      =(/ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,                           &   
                   6,6,6,6,6,6,6                                            /)
    
    itmp=15+7*6 ! taille totale
    modelz(imodel)%nb_internal_variables = itmp      
    if (itchatche) print*,'nb internal ',itmp

    itmp=15+7 ! nb de variables contenues
    modelz(imodel)%nb_internal_variables_bundled=itmp
    if (itchatche) print*,'nb internal bundled ',itmp
  
    if( associated(modelz(imodel)%int_descriptor) ) deallocate(modelz(imodel)%int_descriptor)
    allocate(modelz(imodel)%int_descriptor(itmp))
    
    do i=1,modelz(imodel)%nb_internal_variables_bundled

      modelz(imodel)%int_descriptor(i)%type=itype(i)
      !
      modelz(imodel)%int_descriptor(i)%size=isize(i)
      !
      modelz(imodel)%int_descriptor(i)%index=i-1
      !
      chaine=idescriptor(i)
      lchaine=4
                                                      
      modelz(imodel)%int_descriptor(i)%label_primal =' '
      modelz(imodel)%int_descriptor(i)%label_primal(1:lchaine)=chaine(1:lchaine)

      if (itchatche) print*,i,modelz(imodel)%int_descriptor(i)%label_primal(1:lchaine),lchaine

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
        print*,' --- ',trim(modelz(imodel)%int_descriptor(i)%label_primal)
      enddo
  
      print*,'-------------------------------------------'
    
    endif 

    if (itchatche) call logmes('Leaving : '//IAM)

  end subroutine push_model
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine set_nb_ppsets(nb,itchatche)
    implicit none 
    integer :: nb
    logical :: itchatche
                              !1234567890123456789012
    character(len=22)  :: IAM='Demmefi::set_nb_ppsets'

    if (itchatche) call logmes('Entering : '//IAM)
    
    ! on aura un mat par ppset ; tant pis si il y a des doublons
    
    if ( allocated(mats) ) deallocate(mats)
    allocate(mats(nb))

    if (itchatche) call logmes('Leaving : '//IAM)
    
  end subroutine set_nb_ppsets  
!------------------------------------------------------------------------
  
!------------------------------------------------------------------------
  subroutine push_behaviour(ppsnb,itchatche,rho,filename)
    implicit none
    integer           :: ppsnb
    logical           :: itchatche
    real(kind=8)      :: rho
    character(len=80) :: filename
    
                              !12345678901234567890123
    character(len=23)  :: IAM='Demmefi::push_behaviour'

    if (itchatche) call logmes('Entering : '//IAM)
    
    mats(ppsnb)%values(3)=rho
    mats(ppsnb)%filename=filename
    mats(ppsnb)%type=0
    
    if (itchatche) call logmes('Leaving : '//IAM)
    
  end subroutine push_behaviour
!------------------------------------------------------------------------  

!------------------------------------------------------------------------  
  subroutine check_ppset(ppsnb,imodel,itchatche)
    implicit none
    integer :: ppsnb,imodel
    logical :: itchatche

    integer :: nfich,err,i
    character(len=5) :: name
    character(len=80):: cout 
    character(len=10):: toto
    
                              !12345678901234567890
    character(len=20)  :: IAM='Demmefi::check_ppset'

    if (itchatche) call logmes('Entering : '//IAM)
    
   ! values
   !  1- E moduled de Young
   !  2- nu coefficint de poisson
   !  3- rho masse volumique
   !  4- alpha coefficient de dilatation thermique
   !  5- RT  resistance a la traction       
   !  6- EPT deformation au pic de traction        
   !  7- GFT Energie de fissuration de traction        
   !  8- REF contrainte de refermeture     
   !  9- GFR Energie de refermeture de traction        
   ! 10- RC resistance a la compression        
   ! 11- EPC deformation au pic de compression        
   ! 12- DCPK endomagement au pic de compression
   ! 13- DELT coeff de confinement dans Druker Prager
   ! 14- BETA Dilatance pour Druker Prager
   ! 15- EKDC deformation caracteristique pour l endo de compression
   ! 16- PORO porosite initiale de la matrice
   ! 17- MCC module d ecrouissage initial pour Cam Clay
   ! 18- PFCC pente initiale de la courbe de consolidation pour CamClay
   ! 19- TT0C debut de reduction thermique pour Rc debut
   ! 20- TT1C debut de reduction thermique pour Rc milieu
   ! 21- MTTC debut de reduction thermique pour Rc exposant
   ! 22- PTTC debut de reduction thermique pour Rc exposant
   ! 23- TT0T debut de reduction thermique pour Rc debut
   ! 24- TT1T debut de reduction thermique pour Rc milieu
   ! 25- MTTT debut de reduction thermique pour Rc exposant
   ! 26- PTTT debut de reduction thermique pour Rc exposant
   ! 27- TT0E debut de reduction thermique pour Rc debut
   ! 28- TT1E debut de reduction thermique pour Rc milieu
   ! 29- MTTE debut de reduction thermique pour Rc exposant
   ! 30- PTTE debut de reduction thermique pour Rc exposant
   ! 31- ALTC coeff de flambage bielletes comprimees
   ! 32- PPCC pression de preconsolidation à porosité initiale

    mats(ppsnb)%names=(/ 'E    ','nu   ','rho  ','alpha','RT   ','EPT  ','GFT  ','REF  ','GFR  ','RC   ', &
                         'EPC  ','DCPK ','DELT ','BETA ','EKDC ','PORO ','MCC  ','PFCC ','TT0C ','TT1C ', &
                         'MTTC ','PTTC ','TT0T ','TT1T ','MTTT ','PTTT ','TT0E ','TT1E ','MTTE ','PTTE ', &
                         'ALTC ','PPCC ' /)
    
    !lecture du fichier materiau

    nfich=get_io_unit()

    OPEN(UNIT=nfich,FORM='FORMATTED',STATUS='OLD',FILE=mats(ppsnb)%filename,IOSTAT=err)
   
    IF (err > 0) THEN
      call faterr(IAM,'error while opening '//mats(ppsnb)%filename)
   ENDIF

   READ(nfich,'(A)') name
   DO i=1,32
     if (i == 3) cycle
     if (i==4) then
       READ(nfich,'(A,1x,A)') name,toto
       if (toto(2:6)=='field') then
         mats(ppsnb)%type(i)=1
       else
         backspace(nfich)  
         READ(nfich,'(A,1x,D12.5)') name,mats(ppsnb)%values(i)
       endif   
     else 
       READ(nfich,'(A,1x,D12.5)') name,mats(ppsnb)%values(i)
          
       if (itchatche) then
         write(cout,'(A,1x,D12.5)') name,mats(ppsnb)%values(i)
         call logmes(cout)
       endif
     endif   
     if (name /= mats(ppsnb)%names(i)) call faterr(IAM,'problem while reading file')
   ENDDO 

   close(nfich) 

!   if (itchatche) then
     do i=1,31
       print*,mats(ppsnb)%names(i),mats(ppsnb)%values(i)
     enddo
!   endif    
   ! fd remonter rho !!

   
   if (itchatche) call logmes('Leaving : '//IAM)
   
  end subroutine
!------------------------------------------------------------------------

  
!------------------------------------------------------------------------
  subroutine compute_external_gp(ppsnb,imodel,extP_lbl,extP_len,ivalue, &
                                extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,H,calcD,xe3d)

    ! zone de stockage: gradient,flux,internal,operateur tangent
  
    real(kind=8),dimension(:)             :: GRAD0,FLUX0,INTERNAL0
    real(kind=8),dimension(:)             :: GRAD1,FLUX1,INTERNAL1
    real(kind=8),dimension(:),allocatable :: De
    real(kind=8),dimension(:,:),pointer   :: D
    real(kind=8)                          :: H
    
    ! parametres externes
  
    character(len=30),dimension(:) :: extP_lbl
    integer(kind=4)  ,dimension(:) :: extP_len
    real(kind=8)     ,dimension(:) :: extP_val
    integer(kind=4)                :: extP_nb, calcD
  
    !fd 
    integer                        :: ppsnb,imodel,ibehav,ivalue,ierr
    real(kind=8),dimension(:,:)    :: xe3d

    real(kind=8)                   :: young,ps,C1,C2,C3,C4,alpha_ini,alpha_fin

    !fd dilatation thermique
    real(kind=8),dimension(6)      :: grad_th,grad_th_ini
    
                              !1234567890123456789012345678
    character(len=28)  :: IAM='Demmefi::compute_external_gp'


    ! print*,'grad0     ',grad0
    ! print*,'flux0     ',flux0
    ! print*,'internal0 ',internal0
    
    if (calcD == 1 ) then
    
      YOUNG=mats(ppsnb)%values(1)
      PS   =mats(ppsnb)%values(2)
    
      C1= YOUNG/( (1.D0-2.D0*PS)*(1.D0+PS) ) ; C2= C1*(1.D0-PS)
      C3= C1*PS ; C4= 0.5D0*C1*(1.D0-2.D0*PS)
      D=RESHAPE( (/  C2  ,  C3  ,  C3  , 0.D0 , 0.D0 , 0.D0 , &
                     C3  ,  C2  ,  C3  , 0.D0 , 0.D0 , 0.D0 , &
                     C3  ,  C3  ,  C2  , 0.D0 , 0.D0 , 0.D0 , &
                    0.D0 , 0.D0 , 0.D0 ,  C4  , 0.D0 , 0.D0 , &
                    0.D0 , 0.D0 , 0.D0 , 0.D0 ,  C4  , 0.D0 , &
                    0.D0 , 0.D0 , 0.D0 , 0.D0 , 0.D0 ,  C4   /), (/6,6/) )

    endif  

    ! calcul de l'increment de deformation thermique 
    grad_th=0.d0     
    grad_th_ini=0.d0

    if (mats(ppsnb)%type(4)== 0) then
       alpha_ini=mats(ppsnb)%values(4)
       alpha_fin=mats(ppsnb)%values(4)
    else
       if (size(extP_val) /= 4) call faterr(IAM,'alpha field is missing') 
       alpha_ini= extP_val(3)
       alpha_fin= extP_val(4)
    endif
      

    grad_th(1) = alpha_fin * (extP_val(2) - 20.d0)
    grad_th(2) = grad_th(1)
    grad_th(3) = grad_th(1)

    grad_th_ini(1) = alpha_ini * (extP_val(1) - 20.d0)
    grad_th_ini(2) = grad_th_ini(1)
    grad_th_ini(3) = grad_th_ini(1)
 
      
    call endo3d(mats(ppsnb)%values, &        !XMAT : tableau des paramètres matériau
                size(mats(ppsnb)%values), &  !NMAT : nombre de paramètres matériau
                flux0, &                     !sig0 : Etat de contrainte initial
                flux1, &                     !sigf : Etat de contrainte final
                grad1-grad0-(grad_th-grad_th_ini), &  !deps : Incrément de déformation envoyé par le code en soustrayant la déformation thermique. Les termes diagonaux sont des gammas.
                6, &                         !nstrs : Nombre de contraintes dans le tenseur des contraintes = 6
                internal0, &                 !VAR0 : Variables internes intiales
                internal1, &                 !VARF : Variables internes finales
                size(internal1), &           !NVARI : Nombre de variables internes
                4, &                         !nbelas3d : Nombre de paramètres élastiques
                extP_val(1), &               !teta1 : Température en début de pas
                extP_val(2), &               !teta2 : Température en fin de pas
                H, &                         !dt : Incrément de temps
                ierr, &                      !ierr1 : (entier) si = 1 ça plante sinon =0 
                .TRUE., &                    !iso : (logique) si loi d'élasticité est isotrope = true
                1, &                         !mfr : (entier) numero de formulation EF. 1 == massif
                2, &                         !ifou : formulation. 2==massif 
                0, &                         !istep : (entier) = 0,1,2 ou 3 qui gère le non local. Mettre =0
                grad0-grad_th_ini, &         !epst0 : Déformation totale initiale (sans la thermique) 
                grad1-grad_th, &             !epstf : Déformation totale finale (sans la thermique)
                27, &                        !NBRFLU3D : Nombre de variables envoyés dans endo3d
                0, &                         !NBSUPP3D : Nombre de variables en plus pour des développements
                31, &                        !NMAT1 : Position dans la table des matériaux (spécifique Castem) NMAT1=NBELAS3D+NBRFLU3D+NBSUPP3D+NBRENF3D*NBPARR3D
                34, &                        !NMAT2 : Position dans la table des matériaux (spécifique Castem) NMAT2=NMAT1+NBRTAIL3D+3
                0, &                         !NVARFLU3D : Pour développement
                0, &                         !NVARSUP3D : Pour développement
                15, &                        !NBVSCAL : Nombre de variables scalaires
                7, &                         !NBVTENS : Nombre de variables tenseur                       
                6, &                         !NBVPTENS : Nombre de variables par tenseur
                size(xe3d,dim=2), &          !NBNMAX3D : Nombre maximum de noeuds pour 1 élément fini
                size(xe3d,dim=2), &          !NBNB3D : Nombre de noeuds de l'élément calculé  <- a gerer si tetra
                3, &                         !IDIMB3D : Dimension de l'élément fini en cours de calcul
                xe3d, &                      !XE3D : Coordonnées des noeuds pour l'élément (3,nbnmax3d)
                20.d0)                       !tetaref : Température de référence du code


    ! print*,'grad1     ',grad1
    ! print*,'flux1     ',flux1
    ! print*,'internal1 ',internal1
    
    if (ierr > 0) call faterr(IAM,'pb with endo3d') 

    
  end subroutine    
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine clean_memory
    implicit none
    
    if ( allocated(mats) ) deallocate(mats)
    
  end subroutine clean_memory
end module DemmefiExternalModels
