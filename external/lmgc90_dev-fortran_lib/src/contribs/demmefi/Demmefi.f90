!===========================================================================
!
! Copyright 2000-2025 CNRS.
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

module Demmefi

  implicit none

  private

  ! PARAMETRES MATERIAU 
  type T_mat
   ! filename
   character(len=80) :: filename
   ! values
   !  1- E moduled de Young
   !  2- nu coefficient de poisson
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
   ! 32  PPCC pression de preconsolidation à porosité initiale

   real(kind=8)     :: values(32)
   character(len=5) :: names(32)
   integer          :: type(32) ! to manage fields (0 constant , 1 field)

  end type T_mat


  Type(T_mat), dimension(:), allocatable, public :: mats

  public endo3d

  contains

!------------------------------------------------------------------------  

! routines coming from A. Sellier - LMDC  
!------------------------------------------------------------------------  

    
  subroutine endo3d(XMAT,NMAT,sig0,sigf,deps,nstrs, &
                    VAR0,VARF,NVARI,nbelas3d,teta1,teta2,dt,ierr1,iso,mfr,ifou, &
                    istep,epst0,epstf,NBRFLU3D,NBSUPP3D, &
                    NMAT1,NMAT2,NVARFLU3D,NVARSUP3D,NBVSCAL,NBVTENS,NBVPTENS, &
                    NBNMAX3D,NBNB3D,IDIMB3D,XE3D,tetaref)

!     calcul de l ecoulement elasto-plastique: Sellier mars 2014
!     istep pour la phase non locale : juillet 2015
!     actualisation DTT : aout 2018
!     decouplage du fluage pour faire plastendo3d : avril 2021

!     08/05/2020 : hypothese non locale 
!     istep=0 : calcul local
!     istep=1 : preparation du 1er passge non local
!     istep=2 : dernier passage non local effectue
!     istep=3 : passage non local intermediaire en non local iteratif

!     tables de dimension fixe pour resolution des sytemes lineaires 


      implicit none
    
!     *** declaration pour le non local ********************************
!      include './declare_non_local3d.h'

!     precision
      real(kind=8), parameter ::  precision3d=1.0d-8

!     nombre maxi de sous iterations
      integer,parameter :: itermax=4000,iprint=3000 !!ali boukham : itermax was set to 500 previously iprint to 100

!     endommagement maxi
      real(kind=8),parameter :: dmaxi=1.d0-precision3d

!     methode de prise en compte de l endo post pic de cisaillement
      logical,parameter :: orthodp=.false.
            
!     variable logique pour matrice init isotrope
      logical :: iso
       
!     decalage Gauss pour la plasticite en cas de fluage  nf0
!     et decalage de la taille nmat1, ifou numero de la formulation    
      integer ::  nf0,nmat1,nmat2,ifou,istep
      
!     *** nombre de parametres materiaux *******************************
      integer ::  NBRFLU3D,NBSUPP3D,NBRENF3D

!     *** nombre de vari scalaire et pseudovecteur *********************
      integer NBVSCAL,NBVTENS,NBVPTENS         

!     compteur associees
      integer :: iscal,itens,icomp3d      
      
!     *** nombre de variables internes ********************************* 
!     nombre de variables internes
      integer :: NVARFLU3D,NVARSUP3D     
     
!     **** tableau des coordonnees des neouds de l element ************* 
      integer NBNMAX3D,NBNB3D,IDIMB3D
      real(kind=8) ::  xe3d(3,NBNMAX3D)
!     methode de calcul de la taille des elements
      logical,parameter ::  Mjacob=.false.,Mnoeuds=.true.
      
!     *** declaration des variables externes ***************************
      integer      :: nmat,nstrs,NVARI,nbelas3d,ierr1,mfr,nv3d
      real(kind=8) :: xmat(nmat),sig0(nstrs),sigf(nstrs),deps(nstrs)
      integer      :: IVALMAT3D(nmat)
      real(kind=8) :: epst0(nstrs),epstf(nstrs)
      real(kind=8) :: var0(NVARI),varf(NVARI)
      real(kind=8) :: dt,teta1,teta2,tetaref      

!<fd commenter car combinatoire et pas systeme (discuté AS)
      
!     *** tableau pour la resolution des systemes lineaires ************
!     taille maxi du systeme lineaire
      integer,parameter :: ngf=65
      real(kind=8)      :: XN(ngf),BN(ngf),ANN(ngf,(ngf+1))
      integer           :: ipzero(ngf)

!     variable de controle d erreur dans methode de gauss
      integer :: errgauss

!fd >

      
!     **** variables locales a plastendo3d *****************************
!     criteres
      integer,parameter :: ncr=5
      logical           :: actif(ncr)
      real(kind=8)      :: fcr(ncr)
      
!     parametres materiaux donnes      
      real(kind=8) ::  young,nu,rt,ept,eept,gft,ref,gfr,rc,epc,delta,beta,dcpp,dcpk
      real(kind=8) :: ekdc,poro,pcc,mcc,ppcc,pfcc

!     coeff elastique
      real(kind=8) :: MK,MG      

!     variables generales      
      integer :: i,j,k,l
!     indicateur premier passage
      logical :: ppas
!     coeff pour theta methode
      real(kind=8),parameter :: theta=0.5d0

!     cohesions de DP
      real(kind=8) :: Cdp,Cdp_min,Cdp_max,Rcmin
!     pression de consolidation de CC
      real(kind=8) :: Ptmax,Pt,Pcmin,Pc
!     indicateur d endommagement prepic
      logical :: logdtpp,logdcpp
!     resistance effective et endommagement prepic
      real(kind=8) :: dtpp,dtpp0,dtppmax,dcppmax,rteff,rceff 
!     invariant de la deformation plastique de DP
      real(kind=8) :: EPLDPV,EPLDPD 
!     invariant de la deformation plastique de CC
      real(kind=8) :: EPLCCV,EPCCD       
!     deformations elastique et plastique de compression
      real(kind=8) :: EEPC,EPLPIC,EDPPIC
!     module d ecrouissage
      real(kind=8) :: Hcc,Hdp
!     vecteur de deformations mecaniques
      real(kind=8) :: deps6(6),epst06(6),epstf6(6)
!     matrice deps
      real(kind=8) :: deps33(3,3),deps3(3),vdeps33(3,3),vdeps33t(3,3)
!     contraintes effectives
      real(kind=8) :: dsig3(3),dsig6(6),sigef33(3,3),sigef3(3),vsigef33(3,3)
!     contraintes bornées utilisees dans DP et CC      
      real(kind=8) :: sigdp3(3)
!     tenseurs variables internes DP et CC debut de pas
      real(kind=8) :: EPLDP6(6),EPLCC6(6)
!     fin de passage
      real(kind=8) :: epldp6f(6),eplcc6f(6)
!     matrice des tailles
      real(kind=8) :: t33(3,3),n33(3,3) 
!     plasticite de traction
      real(kind=8) :: eplr60(6),eplr6f(6),eplr33(3,3),eplr3(3)
!     gestion des ouvertures de fissures
      real(kind=8):: wplt06(6),wplt6(6),wpltx06(6)
      real(kind=8):: wpltx6(6),wpl3(3),vwpl33(3,3),vwpl33t(3,3)
      real(kind=8):: wplx3(3),vwplx33(3,3),vwplx33t(3,3)
!     contraintes finales
      real(kind=8):: sigef6(6),sigf6d(6),sigf6(6),sigt6(6),sigt6d(6)
!     pseudo tenseur auxiliaire
      real(kind=8):: xp6(6),xp33(3,3),vxp33(3,3),vxp33t(3,3),xp3(3)
      real(kind=8):: aux3(3) 
!     logique pour reduire l increment
      logical log_reduc,ecoul 
      real(kind=8):: reduc
      integer iter
!     endommagement
      real(kind=8):: umdt
!     contrainte hydrique
      real(kind=8):: sigp 
!     base ouverture des fissures
      real(kind=8):: depr3(3),vdepr33(3,3),vdepr33t(3,3),dir3(3),long3(3)
!     increment def plastique traction 
      real(kind=8):: deplr3(3),deplr6(6)     
!     increment def plastique dp
      real(kind=8):: depldp3(3),depldp6(6) 
!     increment def plastique cc
      real(kind=8):: deplcc3(3),deplcc6(6)
!     increment de def elastique pour retour radial
      real(kind=8):: depse3(3),depse6(6)
!     multiplicateurs plastiques
      real(kind=8):: lr1,lr2,lr3,ldp,lcc 
!     parametre de la loi de reduction thermique pour rc,rt,e 
!     ordre defini dans idvic et idvar4
      real(kind=8):: tt0(3),tt1(3),mtt(3),ptt(3),red(3)
      integer ntt
      parameter (ntt=4) 
!     prise en compte variation module due a theta
      real(kind=8):: dx,dMK,dMG 
!     compte sur la boucle de reduction d increment plastique
      integer jter
!     exposant endommagement prepic
      real(kind=8):: m1,dcpp0,dpic,spic,smin,s 
!     endommagements de traction-refermeture      
      real(kind=8):: dtra,dt3(3),ref3(3),wkt,wkr
!     calcul de la matrice d endo orthotrope      
      real(kind=8):: sn3(3),dt66(6,6)
!     partition des contraintes effectives
      real(kind=8):: sigf3(3),vsigf33(3,3),vsigf33t(3,3)
      real(kind=8):: sigft6(6),sigfc6(6),sigfc3(3),sigft3(3)
      real(kind=8):: sigft6p(6),sigft6d(6),sigft6dp(6)
!     refermeture des fissures
      real(kind=8):: dr66(6,6)
      real(kind=8):: sigfc6p(6),sigfc6dp(6),sigfc6d(6)
      real(kind=8):: sigf61(6),sigft61(6)
      real(kind=8):: sigf62(6),sigft62(6)
!     flambage des bielttes comprimees
      real(kind=8):: dct3(3)
!     coeff de couplage endo traction/flambage bielettes de compression
      real(kind=8):: altc,dctmax
!     endo ortho de DP
      real(kind=8):: dcc3(3),dcdp,dcom,dcc,dccmax     
!     indicateur de mise a zero du reducteur de retour plastique
      logical log_reduc_zero,log_err      
 
      real(kind=8) :: dvtp,R1,R2,R3
      
!     ***** declaration parametres materiaux des renforts repartis *****
!     include './declare_renfort3d.h'
!     ! ne rien déclarer apres cet include qui contient des commandes !
      
!***********************************************************************
!     initialisation des parametres 

!     ** indicateur de premier passage 
      if (var0(1) /= 1.d0) then
         ppas=.true.
      else
         ppas=.false.
      end if 

!     initialisation de la variable de controle d erreur
      ierr1=0

!     *********** chargement des parametres materiaux depuis xmat ******

!     Module d young                            
      young=xmat(1)  
!     coefficient de Poisson      
      nu=xmat(2)
!     calcul des autres coeffs elastiques
      MK=young/(3.d0*(1.d0-2.0d0*nu))
      MG=young/(2.d0*(1.d0+nu))      
!     resistance a la traction      
      rt=xmat(nbelas3d+1)
!     deformation au pic de traction      
      ept=xmat(nbelas3d+2)
      if (ept == 0.d0) then
           ept=rt/young 
      end if 
!     energie de fissuration en traction directe
      gft=xmat(nbelas3d+3)
      if (gft == 0.d0) then
         print*,'Il manque l energie de fissuration GFT ds plasendo3d '
         ierr1=1
      end if       
!     seuil pour la refermeture des fissures
      ref=xmat(nbelas3d+4)
      if(ref.lt.(precision3d*rt)) then
         ref=rt*precision3d
      end if      
!     energie de fissuration pour l endo de traction
      gfr=xmat(nbelas3d+5)  
      if(gfr== 0.d0) then
         print*,'Il manque l energie de refermeture GFR ds plastendo3d'
         ierr1=1
      end if 
!     resistance en compression-par cisaillement
      rc=xmat(nbelas3d+6)
!     deformation au pic de compression
      epc=xmat(nbelas3d+7)
!     endommagement au pic de compression
      dcpk=xmat(nbelas3d+8)      
!     coeff drucker prager
      delta=xmat(nbelas3d+9)         
!     coeff dilatance de cisaillement
      beta=xmat(nbelas3d+10)          
!     deformation caracteristique endo de compression
      ekdc=xmat(nbelas3d+11)
      if(ekdc == 0.d0) then
         print*,'il manque EKDC dans plastendo3d'
         ierr1=1
      end if
!     porosite initiale     
      poro=xmat(nbelas3d+12)       
!     Module d ecrouissage initial pour Cam Clay
      mcc=xmat(nbelas3d+13) 
!     recup des caracteristiques pour la reduc thermique
      do i=1,3
        tt0(i)=xmat(nbelas3d+14+(i-1)*ntt+1)      
        tt1(i)=xmat(nbelas3d+14+(i-1)*ntt+2)
        mtt(i)=xmat(nbelas3d+14+(i-1)*ntt+3) 
        ptt(i)=xmat(nbelas3d+14+(i-1)*ntt+4)
!       print*,tt0(i),tt1(i),mtt(i),ptt(i)          
      end do
!     coeff de couplage endo traction/endo compression     
      altc=xmat(nbelas3d+27)
!     pression de préconsolidation (initiale) pour camclay
      ppcc=xmat(nbelas3d+28) 
!     pour eviter une singulariute sur le gradient de CC iso avec pt=pc
!     il faut PPCC>RT
      if(ppcc.lt.rt*(1.d0+precision3d)) then
        print*,'Dans endo3d il faut PPCC>Rt'
        ierr1=1
      endif
!     pression de fin de consolidation pour CamClay 
      pfcc=xmat(nbelas3d+14)        

      ! print*,'-------------------'
      ! print*,young,nu,rt,ept,gft,ref,gfr,rc,epc,dcpk,delta,beta,ekdc,poro,mcc,kcc,altc
      ! print*,'-------------------'
      
!***********************************************************************
!     initialisation des variables internes finales 
!***********************************************************************

      ! print*,'-------------------'      
      ! print*,NBVSCAL,NBVTENS,NBVPTENS
      ! print*,'-------------------'
      
      do iscal=1,NBVSCAL
         varf(iscal)=var0(iscal)
      end do
      do itens=1,NBVTENS
        do ICOMP3D=1,NBVPTENS
             nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D
             varf(nv3d)=var0(nv3d)
        end do
      end do
      
!***********************************************************************
!      prise en compte de la temperature 
!***********************************************************************


!     *** reduction des proprietes mecaniquess ************************* 

      ! actualisation de la temperature maximale atteinte
      varf(15)=max(var0(15),teta2)

      ! calcul de la reduction fin de pas  
      do i=1,3  
        call thermred3d(teta2,tt0(i),tt1(i),mtt(i),ptt(i),red(i))
        !print*,'red(',i,')=',red(i)
        ! le coeff de reduction thermique ne peut pas reaugmenter 
        if(.not. ppas) then       
           red(i)=min(red(i),var0(11+i))
        end if
        varf(11+i)=red(i)
      end do      
      ! ppt meca effectees par la temperature
      ! compression et parametres associes    
      rc=rc*red(1)
      ! traction et parametres associes      
      rt=rt*red(2)
      gft=gft*red(2)      
      ref=ref*red(2)
      gfr=gfr*red(2)
      ! module d young et deformations associees      
      young=young*red(3) 

!     ** prise en compte de la variation du module *********************

      ! variation du coeff de reduction du module
      dx=varf(14)-var0(14)
      ! rajout de dE*epse si le module varie       
      if ((abs(dx).gt.precision3d).and.(.not.ppas)) then
          ! le module d Young a varie
          ! recup def elastique pas precedent
          ITENS=2        
          do ICOMP3D=1,6
            nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
            xp6(ICOMP3D)=var0(nv3d)
          end do
          ! modification de la contrainte du a la variation du module
          dMK=dx*MK
          dMG=dx*MG
          ! diagonalisation du vecteur des deformations elastiques
          call x6x33(xp6,xp33)
          if(isnan(xp33(1,1))) then
            print*,'Pb1 dans endo3d', xp33
            ierr1=1
            return
          end if
          call b3d_valp33(xp33,xp3,vxp33)
          ! increment de contrainte associees          
          call DSDED3D(dsig3,xp3,dMK,dMG)
          ! passage en base fixe
          call CHREP36(dsig3,dsig6,vxp33)
          ! mise a jour des contraintes
          ITENS=1 
          do ICOMP3D=1,6
             nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
             varf(nv3d)=varf(nv3d)+dsig6(ICOMP3D)
             sigef6(ICOMP3D)=varf(nv3d)
             ! print*,'dans endo3d A sigef(',icomp3d,')=',sigef6(icomp3d)
          end do 
      end if
       
      ! actualisation des coeffs elastiques
      MK=young/(3.d0*(1.d0-2.0d0*nu))
      MG=young/(2.d0*(1.d0+nu))  
      
!     ***********test de coherence des donnees pour la matrice *********

!     ** existance d un endommagement pre pic en traction **************

      if (ept.gt.((rt/young)*(1.d0+precision3d)))then
        ! il existe un endommagement pre pic en traction      
        logdtpp=.true.
        ! endo pre pic maxi
        dtppmax=1.d0-rt/(young*ept)        
        rteff=rt/(1.d0-dtppmax)
      else
        logdtpp=.false.
        dtppmax=0.d0
        ept=rt/young
        rteff=rt
      end if
      ! on continue avec rteff      
      rt=rteff
      ! deformation elastique au pic de traction
      EEPT=rt/young 

!fd pas utilise ?
! !     pas de plasticite pre pic en traction
!       EPPT=0.d0


!     deformation au pic de traction
      EPT=EEPT      
      
!     ** existance d un endommagement pre pic en compression ***********

!     ** verif validite de la dilatance
      if(beta.gt.(sqrt(3.d0)*(1.d0-precision3d))) then
        print*,'Dilatance beta excessive dans plastendo3d'
        ierr1=1
      end if
      
!     ** verif validite de l angle de frottement
      if(delta.gt.(sqrt(3.d0)*(1.d0-precision3d))) then
        print*,'Coeff de confinement excessif dans plastendo3d'
        ierr1=1
      end if  

      if(dcpk.gt.precision3d) then
!       on veut un endo compression pre pic      
        if(epc.gt.((rc/young)*(1.d0+precision3d))) then
!         endo pre pic compression possible
          logdcpp=.true.        
          if(dcpk.le.(1.d0-rc/(young*epc)))then
!           l endommagement prepic de compression est conserve
            dcppmax=dcpk            
          else
!           l endommagement pre pic est trop grand , on le limite
            dcppmax=1.d0-rc/(young*epc) 
          end if
          rceff=rc/(1.d0-dcppmax)
        else
!         endo compression impossible
!          print*,'dcpp,Rc, et EPC sont incompatibles'
!          ierr1=1
          logdcpp=.false.
          dcppmax=0.d0
          epc=rc/young
          rceff=rc        
        end if
      else
!       pas d endo de compression
        logdcpp=.false.
        dcppmax=0.d0
        epc=rc/young
        rceff=rc
      end if 
!     deformation elastique au pic de compression
      EEPC=rceff/young
!     deformation plastique au pic de compression
      EPLPIC=EPC-EEPC 
!     deformation plastique equivalente au pic de compression
      EDPPIC= 0.3D1 * eplpic / (sqrt(0.3D1) - beta)
!     on continue avec rceff
      rc=rceff      
                 
!     *** initialisation des parametres calculees a partir des donnees *

!     cohesion finale de drucker prager pour atteindre Rc
      Cdp_max=rc*(1.d0/sqrt(3.d0)-delta/3.d0)
!     cohesion initiale de Drucker prager pour activer Rankine en
!     avant d'activer DP    
      Cdp_min = Rt * delta     

!     coherence Rt, Rc pour que les Rankine et DP puissent êtres actifs     
      if((Cdp_min*(1.d0+precision3d)).gt.Cdp_max) then
        print*,'Rt/Rc trop grand dans plastendo3d pour activer DP'
        print*,'il faut:  Rt/Rc< (sqrt(3)-delta)/(sqrt(3)+2*delta)'
        ierr1=1
      end if 

!     *** initialisation des pressions de consolidation ****************

      if(ppcc.lt.precision3d*rt) then
        print*,'dans ENDO3D'
        print*,'La pression de preconsolidation ne peut pas etre nulle'
        print*,'il faut PPCC=',PPCC,'>> ',precision3d*rt
        ierr1=1
        return
      else if (pfcc.lt.ppcc*(1.d0+precision3d)) then
        print*,'dans ENDO3D'
        print*,'La pression de fin de consolidation ne peut pas '
        print*,'etre inferieure a la pression de preconsolidation'
        print*,'il faut PFCC=',PFCC,'>> ',PPCC
        ierr1=1
        return       
      end if
     

!     ********** parametres pour le non local **************************

!     include './initialise_non_local3d.h'      
      
!     *** bilan analyse des donnees ************************************
      if(ierr1.eq.1) then
         print*,'Incoherence dans les donnees de plastendo3d '
         return
      end if 
  
!***********************************************************************
!     chargement increment de deformation imposee et deformation finale
!***********************************************************************
      if (nstrs.lt.6) then
        do i=1,nstrs
          deps6(i)=deps(i)
          epstf6(i)=epstf(i)
          epst06(i)=epst0(i)             
        end do
        do i=nstrs+1,6
          deps6(i)=0.d0
          epstf6(i)=0.d0
          epst06(i)=0.d0
        end do
      else
        do i=1,6
          deps6(i)=deps(i)
          epstf6(i)=epstf(i)
          epst06(i)=epst0(i)
        end do      
      end if
!     passage en epsilon
      do i=4,6
        deps6(i)=0.5d0*deps6(i)
        epstf6(i)=0.5d0*epstf6(i)
        epst06(i)=0.5d0*epst06(i)         
      end do
!     mise a jour deformation elastique dans les varf
      ITENS=2
      do ICOMP3D=1,6
        nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
        varf(nv3d)=varf(nv3d)+deps6(ICOMP3D)
      end do    

       
!***********************************************************************
!      tir elastique
!***********************************************************************

!     ** directions principales de l increment de deformation *********
!     diagonalisation du vecteur des d eps 
      call x6x33(deps6,deps33)
      if(isnan(deps33(1,1))) then
        print*,'Pb2 dans endo3d', deps33
        ierr1=1
        return
      end if
     
      call b3d_valp33(deps33,deps3,vdeps33)       

      ! loi elastique 
      call DSDED3D(dsig3,deps3,MK,MG)
      
      if (isnan(dsig3(1))) then
        print*,'Pb2-1 lors du tir elastique dans endo3d'
        ierr1=1
        return
      end if          

      ! passage en base fixe
      call CHREP36(dsig3,dsig6,vdeps33)

      ! mise a jour des contraintes
      ITENS=1 
      do ICOMP3D=1,6
        nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
        varf(nv3d)=varf(nv3d)+dsig6(ICOMP3D)
        sigef6(ICOMP3D)=varf(nv3d)
        !print*,'dans endo3d A sigef(',icomp3d,')=',sigef6(icomp3d)
      end do
                
!***********************************************************************
!      criteres de plasticité et ecoulements 
!***********************************************************************

      
!     ******************************************************************
!     recuperation deformation plastique debut et fin de de pas pour
!     controle de refermeture 
      itens=3 
      do ICOMP3D=1,6
         nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
         eplr60(ICOMP3D)=var0(nv3d)
         eplr6f(ICOMP3D)=eplr60(ICOMP3D)
      end do 
!     recuperation des deformation debut et fin de pas pour dp
      itens=4 
      do ICOMP3D=1,6
         nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
         epldp6(ICOMP3D)=var0(nv3d)
         epldp6f(ICOMP3D)=epldp6(ICOMP3D)
      end do       

!     **********debut des sous iterations locales *********************
      iter=0

!xxx mettre un do

      ! mise a jour num iteration plastique
10    iter=iter+1
!     reinitialisation du compteur sur la boucle de reduction de la
!     taille de l increment plastique
      jter=0
      
!     ******************************************************************
!     actualisation des parametres ecrouissables des criteres
!     ****************************************************************** 

!     ***** cohesion de DP (norme du deviateur) ************************

!     deformation plastique equivalente de DP debut de sous iteration     
      EPLDPD=varf(9)
!     actualisation de la cohesion de DP
      if(EDPPIC.gt.precision3d) then
!       il existe un ecrouissage pre pic en compression
        call ecroup3d(epldpd,edppic,Cdp_min,Cdp_max,Cdp,Hdp)
      else
!       cohesion max directement      
        Cdp=Cdp_max        
!       ecrouissage pre pic neglige            
        Hdp=0.d0
      endif     
!     stockage de la nouvelle cohesion
      varf(2)=Cdp
!     actualisation de la resistance a la compression pour DP
      Rc=Cdp/(1.d0/sqrt(3.d0)-delta/3.d0)
!      if(ppas) then
!         print*,'EPLDPD',EPLDPD
!         print*,'rcmin',Cdp_min/(1.d0/sqrt(3.d0)-delta/3.d0)       
!         print*,'rcmax',Cdp_max/(1.d0/sqrt(3.d0)-delta/3.d0)
!         print*,'Hdp',hdp
!         read*
!      end if  
       
!     **** pressions limites pour cam clay *****************************

!     prise en compte de la deformation volumique de CamClay
      EPLCCV=0.d0
      ITENS=5

      do ICOMP3D=1,6
         nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D
         EPLCC6(ICOMP3D)=varf(nv3d)
         if (ICOMP3D.le.3) then
           EPLCCV=EPLCCV+EPLCC6(ICOMP3D)
         endif   
      end do      
      ! actualisation pc et pt  en fonction de la variation de volume
      call PCCD3D(precision3d,poro,EPLCCV,ppcc,pfcc,pc,rt,pt)

      ! print*,'----'
      ! print*,poro,eplccv,pc,pt
      ! print*,'----'
      
      if(isnan(pc).or.isnan(pt)) then
         print*,'Pb4 dans endo3: calcul de pc et pt de CamClay'
         print*,'precision3d,poro,EPLCCV,ppcc,pfcc,pc,rt,pt'
         print*,precision3d,poro,EPLCCV,ppcc,pfcc,pc,rt,pt
         ierr1=1
         return
      else
         varf(3)=pc
         varf(4)=pt
      endif   
!     actualisation du module tangent de CamClay
!     (si EPLCCV mis a zero : on garde un ecrouissage constant) 
      call HCCD3D(pfcc,ppcc,poro,EPLCCV,Hcc)
     
!     **** tir elastique ***********************************************

!      diagonalisation du vecteur des contraintes effectives
       call x6x33(sigef6,sigef33)
       if (isnan(sigef33(1,1))) then         
         print*,'Pb3 dans endo3d sigef6',sigef6
         print*,'iter',iter,' jter',jter
         do i=1,ncr        
           write(*,'(a9,i2,1x,a9,l1,1x,a4,i1,a2,e10.3)') 'critere:',i,'activite:',actif(i),'fcr(',i,'):',fcr(i)   
         end do
         do itens=1,7
           do ICOMP3D=1,6
             nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
             print*,'Itenseur',itens,'v(',icomp3d,')=',varf(nv3d)
           end do  
         end do
         ierr1=1
         return
       endif

       call b3d_valp33(sigef33,sigef3,vsigef33)
       ! deformation plastique normale de Rankine dans la base principales
       call chrep6(eplr6f,vsigef33,.false.,xp6) 
       do i=1,3
         eplr3(i)=xp6(i)
       end do

!      **** test activite des criteres ****

       call CRITENDO3D(sigef3,precision3d,Rt,Rc,Ref,eplr3,sigdp3,Cdp,delta,Mcc,pt,pc,ncr,actif,fcr,ppcc,pfcc,poro,eplccv)

       !fd pas utile
       if(ierr1.eq.1) return  
       
!      affichage des causes de sous iterations     
       if (iter.ge.iprint) then       
         print*,'Endo3d sous iteration locale:',iter
         do i=1,ncr
             write(*,'(a9,i2,1x,a9,l1,1x,a4,i1,a2,e10.3)') 'critere:',i,'activite:',actif(i),'fcr(',i,'):',fcr(i)
         end do
         if (iter.eq.(itermax+1)) then
             print*,'Nbr maxi de sous iterations locales'
             print*,'atteint dans endo3d: ', iter-1
             ierr1=1
             return
         end if
       end if     
!      test necessite de l ecoulement     
20     ecoul=.false.
       do i=1,ncr
            ecoul=ecoul.or.actif(i)
       end do
       if ((jter.ge.iprint).and.ecoul) then
            print*,'Criteres residuels pour iteration ',iter             
            do i=1,ncr
                if ((iter.ge.iprint).or.(jter.ge.iprint)) then
                    print*,'dans endo3d actif(',i,')=',actif(i)            
                    write(*,'(a9,i2,1x,a9,l1,1x,a4,i1,a2,e10.3)') 'critere:',i,'activite:',actif(i),'fcr(',i,'):',fcr(i)               
                end if
            end do
       end if

!      print*,'dans endo3d ecoul:',ecoul

!      reinitialisation du log_reduc
       log_reduc=.false.
!      retour le cas echeant
       if(ecoul) then
!           initialisation des increments
            do i=1,3
                deplr3(i)=0.d0
                depldp3(i)=0.d0
                deplcc3(i)=0.d0 
            end do                
          
!           choix de l ecoulement en fonction des criteres actifs
            if(actif(1).and.actif(2).and.actif(3)) then 
!               tri-traction-refermeture
                call EPLRR3D(MG,MK,sigef3(1),sigef3(2),sigef3(3),fcr(1),fcr(2),fcr(3),deplr3,3)
!               print*,'endo3d: R1 R2 R3', deplr3(1),deplr3(2),deplr3(2)           
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0
                do i=1,3
                    depldp3(i)=0.d0
                    deplcc3(i)=0.d0
                end do 
            else if (actif(1).and.actif(2).and.actif(4)) then
!               2 Rankine et et un DP
!               choix des contraintes limite pour DP
                if(fcr(1).gt.0.d0) then
                   R1=Rt
                else
                   R1=-Ref
                end if
                if(fcr(2).gt.0.d0) then
                   R2=Rt
                else
                   R2=-Ref
                end if                
!               calcul des multiplicateurs
                call LR12LDP3D(MK,MG,beta,delta,sigef3(1),sigef3(2),sigef3(3),Hdp,fcr(1), &
                               R1,fcr(2),R2,fcr(4),lr1,lr2,ldp,precision3d,log_err)
                if(log_err) then
                    !on resoud par familles de criteres
                    if(0.5d0*(abs(fcr(1))+abs(fcr(2))).gt.fcr(4)) then
                        actif(1)=.true.
                        actif(2)=.true.
                        actif(4)=.false.
                    else
                        actif(1)=.false.
                        actif(2)=.false.                 
                        actif(4)=.true.
                    end if
                    !on recommence                 
                    goto 20                 
                end if                 

!               print*,'endo3d: R1 R2 et DP ',lr1, lr2 ,ldp      

!               calcul de l ecoulement de DP                   
                call EPLDP3D(R1,R2,sigef3(3),beta,ldp,depldp3)
!               ecoulement R1 et R2                
                deplr3(1)=lr1
                deplr3(2)=lr2
                deplr3(3)=0.d0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc) 
!               pas  cc
                lr3=0.d0
                lcc=0.d0
                do i=1,3
                    deplcc3(i)=0.d0
                end do 
            else if (actif(2).and.actif(3).and.actif(4)) then
!               2 Rankine et et un DP
!               choix des contraintes limite pour DP
                if(fcr(2).gt.0.) then
                   R2=Rt
                else
                   R2=-Ref
                end if
                if(fcr(3).gt.0.) then
                   R3=Rt
                else
                   R3=-Ref
                end if                
!               calcul des multiplicateurs
                call LR12LDP3D(MK,MG,beta,delta,sigef3(2),sigef3(3),sigef3(1), &
                               Hdp,fcr(2),R2,fcr(3),R3,fcr(4),lr2,lr3,ldp,precision3d,log_err)
                if(log_err) then
                    ! on resoud par familles de criteres
                    if(0.5d0*(abs(fcr(2))+abs(fcr(3))).GT.fcr(4)) then
                        actif(2)=.true.
                        actif(3)=.true.
                        actif(4)=.false.
                    else
                        actif(2)=.false.
                        actif(3)=.false.                 
                        actif(4)=.true.
                    end if
                    ! on recommence                 
                    goto 20
                end if      

!               print*,'endo3d: R2 R3 et DP ',lr2, lr3 ,ldp     
!               calcul de l ecoulement de DP
                call EPLDP3D(R2,R3,sigef3(1),beta,ldp,aux3)
                depldp3(1)=aux3(3)
                depldp3(2)=aux3(1)
                depldp3(3)=aux3(2)
!               ecoulement R1 et R2                
                deplr3(1)=0.d0
                deplr3(2)=lr2
                deplr3(3)=lr3                                
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc) 
!               pas  cc
                lcc=0.d0
                lr1=0.d0

            else if (actif(1).and.actif(3).and.actif(4)) then
!               2 Rankine et et un DP
!               choix des contraintes limite pour DP
                if(fcr(1).gt.0.) then
                   R1=Rt
                else
                   R1=-Ref
                end if
                if(fcr(3).gt.0.) then
                   R3=Rt
                else
                   R3=-Ref
                end if                
!               calcul des multiplicateurs
                call LR12LDP3D(MK,MG,beta,delta,sigef3(1),sigef3(3),sigef3(2), &
                               Hdp,fcr(1),R1,fcr(3),R3,fcr(4),lr1,lr3,ldp,precision3d,log_err)
                if(log_err) then
                    ! on resoud par familles de criteres
                    if(0.5d0*(abs(fcr(1))+abs(fcr(3))).GT.fcr(4)) then
                        actif(1)=.true.
                        actif(3)=.true.
                        actif(4)=.false.
                    else
                        actif(1)=.false.
                        actif(3)=.false.                 
                        actif(4)=.true.
                    end if
                    ! on recommance                 
                    goto 20                 
                end if     
                
!               print*,'endo3d: R1 R3 et DP ',lr1, lr3 ,ldp       
!               calcul de l ecoulement de DP
                call EPLDP3D(R1,R3,sigef3(2),beta,ldp,aux3)
                depldp3(1)=aux3(1)
                depldp3(2)=aux3(3)
                depldp3(3)=aux3(2)
!               ecoulement R1 et R3                
                deplr3(1)=lr1
                deplr3(2)=0.d0
                deplr3(3)=lr3                
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc
                lcc=0.d0
                lr2=0.d0

            else if (actif(1).and.actif(2)) then
!               bi-traction-refermeture       
                call EPLRR3D(MG,MK,sigef3(1),sigef3(2),sigef3(3),fcr(1),fcr(2),fcr(3),deplr3,2)
!                print*,'endo3d: R1 et R2  ', deplr3(1),deplr3(2)      
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0

            else if (actif(1).and.actif(3)) then
!               bi-traction-refermeture       
                call EPLRR3D(MG,MK,sigef3(1),sigef3(3),sigef3(2),fcr(1),fcr(3),fcr(2),aux3,2)
!                print*,'endo3d: R1 et R3  ', aux3(1),aux3(2) 
                deplr3(1)=aux3(1)     
                deplr3(2)=aux3(3)            
                deplr3(3)=aux3(2)
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0

            else if (actif(2).and.actif(3)) then
!               bi-traction-refermeture       
                call EPLRR3D(MG,MK,sigef3(2),sigef3(3),sigef3(1),fcr(2),fcr(3),fcr(1),aux3,2)
!                print*,'endo3d: R2 et R3  ', aux3(1),aux3(2)      
                deplr3(1)=aux3(3)
                deplr3(2)=aux3(1)        
                deplr3(3)=aux3(2)
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0

            else if (actif(1).and.actif(4)) then 
!               1 rankine et dp actifs
!               choix de la contrainte limite pour DP
                if(fcr(1).gt.0.) then
                   R1=Rt
                else
                   R1=-Ref
                end if
!               multiplicateurs plastiques                
                call LR1LDP3D(MK,MG,beta,delta,sigef3(1),sigef3(2),sigef3(3),Hdp,fcr(1),R1,fcr(4),lr1,ldp,precision3d,log_err)
                if(log_err) then
                    if(abs(fcr(1)).gt.fcr(4)) then
                        actif(1)=.true.
                        actif(4)=.false.
                    else
                        actif(1)=.false.
                        actif(4)=.true.
                    end if
                    goto 20
                end if     
                
!               print*,'endo3d: R1 et DP ',lr1 ,ldp
!               calcul de l ecoulement de DP
                call EPLDP3D(R1,sigef3(2),sigef3(3),beta,ldp,aux3)
                depldp3(1)=aux3(1)
                depldp3(2)=aux3(2)
                depldp3(3)=aux3(3)
!               ecoulement R1 et R2                
                deplr3(1)=lr1
                deplr3(2)=0.d0
                deplr3(3)=0.d0                 
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc) 
!               pas  cc
                lcc=0.d0
                
            else if (actif(2).and.actif(4)) then
!               1 rankine et dp actifs
!               choix de la contrainte limite pour DP
                if(fcr(2).gt.0.) then
                   R2=Rt
                else
                   R2=-Ref
                end if
!               multiplicateurs plastiques                
                call LR1LDP3D(MK,MG,beta,delta,sigef3(2),sigef3(1),sigef3(3),Hdp,fcr(2),R2,fcr(4),lr2,ldp,precision3d,log_err)
                if(log_err) then
                    if(abs(fcr(2)).gt.fcr(4)) then
                        actif(2)=.true.
                        actif(4)=.false.
                    else
                        actif(2)=.false.
                        actif(4)=.true.
                    end if
                    goto 20
                end if
                
!               print*,'endo3d: R4 et DP ',lr2 ,ldp
!               calcul de l ecoulement de DP
                call EPLDP3D(R2,sigef3(1),sigef3(3),beta,ldp,aux3)
                depldp3(1)=aux3(2)
                depldp3(2)=aux3(1)                
                depldp3(3)=aux3(3)
!               ecoulement R1 et R2                
                deplr3(1)=0.d0
                deplr3(2)=lr2
                deplr3(3)=0.d0                 
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc
                lcc=0.d0

            else if (actif(3).and.actif(4)) then
!               1 rankine et dp actifs
!               choix de la contrainte limite pour DP
                if(fcr(3).gt.0.) then
                   R3=Rt
                else
                   R3=-Ref
                end if
!               multiplicateurs plastiques                
                call LR1LDP3D(MK,MG,beta,delta,sigef3(3),sigef3(2),sigef3(1),Hdp,fcr(3),R3,fcr(4),lr3,ldp,precision3d,log_err)
                if(log_err) then
                    if(abs(fcr(3)).gt.fcr(4)) then
                        actif(3)=.true.
                        actif(4)=.false.
                    else
                        actif(3)=.false.
                        actif(4)=.true.
                    end if
                    goto 20
                end if
                
!               print*,'endo3d: R3 et DP ',lr3 ,ldp
!               calcul de l ecoulement de DP
                call EPLDP3D(R3,sigef3(2),sigef3(1),beta,ldp,aux3)
                depldp3(1)=aux3(3) 
                depldp3(2)=aux3(2)                
                depldp3(3)=aux3(1)
!               ecoulement R1 et R2                
                deplr3(1)=0.d0
                deplr3(2)=0.d0
                deplr3(3)=lr3               
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc
                lcc=0.d0

            else if (actif(4).and.actif(5)) then
!               DP et CC actifs 
!               calcul de derivee de dcc=d(fcc) / d(lcc)
                call DCCD3D(pt,pc,Mcc,Hcc,sigef3(1),sigef3(2),sigef3(3),dcc)
!               calcul des multiplicateurs   
                call LCCLDPD3D(Mcc,MK,MG,beta,delta,pt,pc,sigef3(1),sigef3(2),sigef3(3), &
                               dcc,Hdp,lcc,ldp,fcr(5),fcr(4),precision3d,log_err)
                if(log_err) then
                   if(fcr(5).gt.fcr(4)) then
                      actif(5)=.true.
                      actif(4)=.false.
                   else
                      actif(5)=.false.
                      actif(4)=.true.
                   end if
                   goto 20
                end if
                
!               ecoulement de DP
                call EPLDP3D(sigef3(1),sigef3(2),sigef3(3),beta,ldp,depldp3) 
!               ecoulement de CC
                call EPLCC3D(pt,pc,Mcc,sigef3(1),sigef3(2),sigef3(3),lcc,deplcc3)
                print*,'endo3d: DP et CC ',ldp ,lcc
!               test de proprosité non negative                   
                call TESTCC3D(poro,EPLCCV,deplcc3,precision3d,log_reduc,reduc)  
                if(reduc.lt.precision3d) then
                    actif(5)=.false.
                    fcr(5)=0.d0
                    lcc=0.d0
                    log_reduc=.true.
                    do i=1,3
                       deplcc3(i)=0.d0
                    end do
                end if      

            else if (actif(1)) then
                call EPLRR3D(MG,MK,sigef3(1),sigef3(2),sigef3(3),fcr(1),fcr(2),fcr(3),aux3,1)
!                print*,'endo3d: R1 seul ', aux3(1)      
                deplr3(1)=aux3(1)
                deplr3(2)=aux3(2)        
                deplr3(3)=aux3(3)
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0

            else if (actif(2)) then
                call EPLRR3D(MG,MK,sigef3(2),sigef3(1),sigef3(3),fcr(2),fcr(1),fcr(3),aux3,1)
!                print*,'endo3d: R2 seul ', aux3(1)  
                deplr3(1)=aux3(2)
                deplr3(2)=aux3(1)        
                deplr3(3)=aux3(3)
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0

            else if (actif(3)) then
                call EPLRR3D(MG,MK,sigef3(3),sigef3(1),sigef3(2),fcr(3),fcr(1),fcr(2),aux3,1)
!                print*,'endo3d: R3 seul ', aux3(1)  
                deplr3(1)=aux3(2)
                deplr3(2)=aux3(3)        
                deplr3(3)=aux3(1)
!               verif eplr3 final > 0
                call TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
!               pas  cc ni dp
                lcc=0.d0
                ldp=0.d0

            else if (actif(4)) then
!               Drucker Prager seul
                call LDPD3D(fcr(4),MK,MG,beta,delta,Hdp,ldp)
!                print*,'endo3d: DP seul ',ldp                
!               ecoulement de DP
                call EPLDP3D(sigef3(1),sigef3(2),sigef3(3),beta,ldp,depldp3)
!               pas de rankine ni de cc
200             format(a8,i1,a2,e10.3)
                lcc=0.d0

              else if (actif(5)) then

                ! CamClay seul
                ! calcul de derivee de dcc=d(fcc) / d(lcc)
                call DCCD3D(pt,pc,Mcc,Hcc,sigef3(1),sigef3(2),sigef3(3),dcc)

                ! multiplicateur plastique     
                call LCCD3D(pt,pc,Mcc,sigef3(1),sigef3(2),sigef3(3),MK,MG,dcc,fcr(5),lcc,precision3d)         
                !print*,'endo3d: CC seul ',lcc
                !print*,pt,pc,Mcc,sigef3(1),sigef3(2),sigef3(3),MK,MG,dcc,fcr(5),lcc

                ! ecoulement cc                
                call EPLCC3D(pt,pc,Mcc,sigef3(1),sigef3(2),sigef3(3),lcc,deplcc3)
                ! do i=1,3
                !    print*,'deplcc3(',i,')=',deplcc3(i)
                ! end do                   

                ! test de porosité non negative

                call TESTCC3D(poro,EPLCCV,deplcc3,precision3d,log_reduc,reduc)
                if (reduc.lt.precision3d) then
                  actif(5)=.false.
                  fcr(5)=0.d0
                  lcc=0.d0
                  log_reduc=.true.
                  do i=1,3
                    deplcc3(i)=0.d0
                  end do
                end if                  

                ! print*,'----'
                ! print*,poro,EPLCCV,deplcc3
                ! print*,'----'
            else
                print*,'configuration imprevue dans endo3d'
                do i=1,5
                   print*,'critere:',i,actif(i),' fcr(',i,')=',fcr(i)
                end do
                ierr1=1
                return                
            end if
!           read*
!           test de dissipation sur les criteres CC et DP
            if(reduc.lt.precision3d) then
                log_reduc_zero=.true.                
            else
                log_reduc_zero=.false.
            end if
            if(((ldp.lt.-precision3d).and.actif(4)) .or.((lcc.lt.-precision3d).and.actif(5))) then
!                print*,' Endo3d multiplicateurs  negatif',ldp,lcc
!               on indique qu il ne faut pas garder ce retour
                log_reduc=.true.
!               on met le retour a zero
                reduc=0.d0 
!               indicateur associe                
                log_reduc_zero=.true.                               
!               on desactive les critere fautifs
                if ((ldp.lt.-precision3d).and.actif(4)) then   
!                  print*,'ldp',ldp
                   actif(4)=.false.                 
                else if ((lcc.lt.-precision3d).and.actif(5)) then                      
!                  print*,'lcc',lcc
                   actif(5)=.false.
                else
                   print*,'pb6 dans endo3d'
                   ierr1=1
                   return                   
                end if                                   
            end if                 
                
!           modification de l ecoulement en cas de reduction de la refermeture
            if(log_reduc) then 
              do i=1,3
                 deplr3(i)=reduc*deplr3(i)
                 depldp3(i)=reduc*depldp3(i)                     
                 deplcc3(i)=reduc*deplcc3(i)                  
              end do
!             reduction des multiplicateurs possiblement utilises apres              
              lcc=lcc*reduc              
              ldp=ldp*reduc 
!             traitement suivant la reduction              
!             on recommence l ecoulement sans les criteres desactives
!             dans testref3d
              jter=jter+1
              if(jter.ge.iprint) then
                  print*,'sous iteration endo3d'
                  print*,'reduction a la refermeture:',jter  
              end if                
              if (jter.eq.itermax) then
                  print*,'Itermax Reduction a la refermeture atteint'
                   print*,'Criteres residuels pour itertation ',iter
                   do i=1,ncr                
                     write(*,'(a9,i2,1x,a9,l1,1x,a4,i1,a2,e10.3)') 'critere:',i,'activite:',actif(i),'fcr(',i,'):',fcr(i)
                   end do                     
                   
                   ierr1=1
                   return
              else
!               nouvelle iteration 
!               print*,'log_reduc_zero=',log_reduc_zero
                if (log_reduc_zero) then 
!                 pas de plasticité, on recommence en eliminant les
!                 criteres non dissipatifs  sans modifier la contrainte 
                  goto 20
                end if
              end if
           end if
           
            ! mise a jour des deformations plastiques de traction
            call CHREP36(deplr3,deplr6,vsigef33)
            itens=3 
            do ICOMP3D=1,6
                nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
                varf(nv3d)=varf(nv3d)+deplr6(ICOMP3D)
                eplr6f(ICOMP3D)=varf(nv3d)
            end do             

            ! mise a jour des deformations plastiques de dp
            call CHREP36(depldp3,depldp6,vsigef33)
            itens=4 
            do ICOMP3D=1,6
                nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
                varf(nv3d)=varf(nv3d)+depldp6(ICOMP3D)
                epldp6f(ICOMP3D)=varf(nv3d)
            end do 
            ! deformation equivalente de dp
            epldpd=epldpd+ldp
            varf(9)=epldpd           

            ! mise a jour des deformations plastiques de cc
            call CHREP36(deplcc3,deplcc6,vsigef33)
            itens=5 
            do ICOMP3D=1,6
                nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
                varf(nv3d)=varf(nv3d)+deplcc6(ICOMP3D)
                eplcc6f(ICOMP3D)=varf(nv3d)
            end do             

            ! mise a jour def elastique dans les varf
            log_err=.false.
            do i=1,3
               depse3(i)=-(deplr3(i)+depldp3(i)+deplcc3(i))
               if(isnan(depse3(i))) then
                  log_err=.true.
                  print*,'Dans endo3d pb5-0'
                  print*,'deplr3(i),depldp3(i),deplcc3(i)'
                  print*, deplr3(i),depldp3(i),deplcc3(i)
               end if
            end do
            if (log_err) then
                print*,'Endo3d Pb5 a l iter:',iter
                do i=1,ncr
                    write(*,'(a9,i2,1x,a9,l1,1x,a4,i1,a2,e10.3)') 'critere:',i,'activite:',actif(i),'fcr(',i,'):',fcr(i)
                end do
                ierr1=1
                return
            end if                     
            ! mise a jour des deformations
            call chrep36(depse3,depse6,vsigef33)
            ITENS=2
            do ICOMP3D=1,6
                nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
                varf(nv3d)=varf(nv3d)+depse6(ICOMP3D)
            end do            

            ! mise a jour des contraintes
            call DSDED3D(dsig3,depse3,MK,MG) 

            ! retour en base fixe
            call CHREP36(dsig3,dsig6,vsigef33)

            ! mise a jour des contraintes
            ITENS=1
            do ICOMP3D=1,6
              nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D
              varf(nv3d)=varf(nv3d)+dsig6(ICOMP3D)
              sigef6(ICOMP3D)=varf(nv3d)
              !print*,'dans endo3d B sigef(',icomp3d,')=',sigef6(icomp3d)                
            end do
           
            ! on teste a nouveau les criteres       
            goto 10
       else
!           aucun des criteres n est actif
            continue
       end if

       ! recuperation des contraintes effectives convergees
30     ITENS=1 
       do ICOMP3D=1,6
         nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
         sigef6(ICOMP3D)=varf(nv3d)
         ! print*,'dans endo3d B sigef(',icomp3d,')=',sigef6(icomp3d)                
       end do              
         
       
!      fin des procedures elasto-plastiques                 
!***********************************************************************
        

!***********************************************************************
!       ouvertures de fissure debut de pas 
!***********************************************************************

        itens=6
        do ICOMP3D=1,6
          nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
          wplt06(ICOMP3D)=var0(nv3d)
        end do
!       ouvertures de fissure maxi debut de pas
        itens=7
        do ICOMP3D=1,6
          nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
          wpltx06(ICOMP3D)=var0(nv3d)
        end do 
!       base principale des ouvertures de fissures
        call basefiss3d(eplr6f,eplr60,depr3,vdepr33,vdepr33t)
!       longueurs de l element dans cette base
        do i=1,3
           do l=1,3
            dir3(l)=vdepr33(l,i)
           end do
           call LNOEUD3D(long3(i),xe3d,NBNMAX3D,NBNB3D,dir3)
        end do           
!       mise a jour des ouvertures de fissures
        call wfiss3d(depr3,long3,vdepr33t,wplt06,wplt6,vwpl33,vwpl33t,wpl3,wpltx06,wpltx6,vwplx33,vwplx33t,wplx3)
        itens=6
        do ICOMP3D=1,6
          nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
          varf(nv3d)=wplt6(ICOMP3D)
        end do
!       ouvertures maxi de fissure fin de pas
        itens=7
        do ICOMP3D=1,6
          nv3d=NBVSCAL+(ITENS-1)*NBVPTENS+ICOMP3D      
          varf(nv3d)=wpltx6(ICOMP3D)
        end do      
!       ouverture maxi actuelle
        varf(5)=max(wpl3(1),wpl3(2),wpl3(3)) 


!***********************************************************************
!       endommagements
!***********************************************************************

!       **** partition du tenseur des contraintes **********************

        call partition3d(sigef6,sigf3,vsigf33,vsigf33t,sigft6,sigfc6,sigfc3,sigft3) 

!       *** endommagement prepic de compression ************************

        if(logdcpp) then
!           endo pre pic de compression possible
            dcpp0=var0(8)      
            dpic=dcppmax
            spic=Cdp_max
            smin=0.d0        
!           contrainte equivalente de DP        
            s=fcr(4)+Cdp 
            if((epldpd.lt.edppic).and.(edppic.ge.precision3d)) then
!              endommagement pre pic lineaire
               dcpp=dpic * ((s - smin) / (spic - smin)) 
            else
!              pas de plasticite pre pic, on utilise le critere DP en 
!              contrainte effective pour l endo pre pic
!              exposant de la loi d endommagement
               m1=(-0.1D1 + dpic) / spic / dpic * (-spic + smin)
               dcpp=dpic * ((s - smin) / (spic - smin)) ** m1
            end if
            dcpp=max(dcpp,dcpp0)
         else
            dcpp=0.d0
            dcpp0=0.d0            
         end if
         varf(8)=dcpp
        
!       *** endommagement prepic de traction  **************************

        if(logdtpp) then
!           endo pre pic de compression possible
            dtpp0=var0(7)      
            dpic=dtppmax
            spic=rteff
            smin=0.d0        
!           contrainte equivalente de DP        
            s=max(sigef3(1),sigef3(2),sigef3(3),0.d0)
!           exposant de la loi d endommagement
            m1=(-0.1D1 + dpic) / spic / dpic * (-spic + smin)
!           endommagement pre pic tension            
            dtpp=dpic * ((s - smin) / (spic - smin)) ** m1
            dtpp=max(dtpp,dtpp0)
         else
            dtpp=0.d0  
         end if
         varf(7)=dtpp  

!       *** endommagement complet de traction **************************

!       ouverture caracteristique pour l' endommagement de traction
        wkt=gft/rt
        dtra=0.d0
        do i=1,3
!           endo localise base principale de fissuration       
            dt3(i)=wplx3(i)*(wplx3(i)+2.d0*wkt)/(wkt+wplx3(i))**2
!           borne de dt a dmaxi
            dt3(i)=min(dmaxi,dt3(i))
!           endommagement global de traction pour affichage             
            dtra=max(dtra,dt3(i))
!           indice de fissure
            sn3(i)=1.d0/(1.d0-dt3(i))            
        end do
!       matrice d endommagement de traction incluant l endo pre pic
        call b3d_d66(nu,sn3,dt66,.false.,.false.)
!       passage  des contraintes effectives dans la base prin des endo
        call chrep6(sigft6,vwplx33,.false.,sigft6p)  
!       application du tenseur d endommagement 
        do i=1,6
          sigft6dp(i)=sigft6p(i)
          do j=1,6
             sigft6dp(i)=sigft6dp(i)-dt66(i,j)*sigft6p(j)
          end do          
        end do
!       retour des contraintes positives en base fixe
        call chrep6(sigft6dp,vwplx33t,.false.,sigft6d)
!       reconstruction du tenseur des contraintes endommage 
        do i=1,6
!           prise en compte de l endo iso prepic de traction        
            sigf61(i)=sigfc6(i)+sigft6d(i)*(1.d0-dtpp)
        end do
        varf(11)=1.d0-(1.d0-dtra)*(1.d0-dtpp)        
!       Nouvelle partition du tenseur des contraintes endommagés 
        call partition3d(sigf61,sigf3,vsigf33,vsigf33t,sigft61,sigfc6,sigfc3,sigft3)        

!       *** fonction de refermeture ************************************

!       ouverture caracteristique pour l' endommagement de traction
        wkr=gfr/ref
        do i=1,3
!           endo localise base principale de fissuration       
            ref3(i)=1.d0-wpl3(i)*(wpl3(i)+2.d0*wkr)/(wkr+wpl3(i))**2
!           ajustement borne  mini            
            ref3(i)=max(ref3(i),0.d0)
!           ajustement borne maxi            
            ref3(i)=min(ref3(i),1.d0)
!           indice de refermeture
            sn3(i)=1.d0/ref3(i)            
        end do         
!       matrice des fonctions de refermeture
        call b3d_d66(nu,sn3,dr66,.false.,.false.)
!       passage  des contraintes effectives dans la base prin des endo
        call chrep6(sigfc6,vwplx33,.false.,sigfc6p)  
!       application du tenseur de refermeture aux contraintes negatives
        do i=1,6
          sigfc6dp(i)=sigfc6p(i)
          do j=1,6
             sigfc6dp(i)=sigfc6dp(i)-dr66(i,j)*sigfc6p(j)
          end do          
        end do
!       retour des contraintes positives en base fixe
        call chrep6(sigfc6dp,vwplx33t,.false.,sigfc6d)
!       reconstruction du tenseur des contraintes avec refermetures
        do i=1,6
            sigf62(i)=sigfc6d(i)+sigft61(i)
        end do
!       Nouvelle partition du tenseur des contraintes endommagés 
        call partition3d(sigf62,sigf3,vsigf33,vsigf33t,sigft62,sigfc6,sigfc3,sigft3)         

!       *** endommagement de flambage des bielettes comprimees *********

        call buckling3d(altc,dt3,dct3,vwplx33,vwplx33t,sigfc6)
        dctmax=max(dct3(1),dct3(2),dct3(3))

        
!       *** endommagement de cisaillement ******************************

!       dilatance transverse au pic de l essai uniaxial
        if(orthodp) then
            dvtp = eplpic *(0.2D1*beta+sqrt(0.3D1))/(sqrt(0.3D1)-beta)
        else
            dvtp = 0.3D1 * beta * eplpic / (sqrt(0.3D1) - beta)
        end if
!       endo de cisaillement initial
        dcdp=1.d0-(1.d0-var0(10))/(1.d0-dcpp0)
!       actualisation endo de cisaillement        
        call damdp3d(precision3d,ekdc,dvtp,epldp6f,dcc3,sigfc6,sigft62,dcdp,orthodp)
        if(orthodp) then
            dccmax=max(dct3(1),dct3(2),dct3(3))       
        else
            dccmax=dcdp
        end if
!       prise en compte endommagement isotrope prepic de compression 
        do i=1,6
!             sigf6(i)=(sigft62(i)+sigfc6(i))*(1.d0-dcpp)
             sigf6(i)=sigft62(i)+sigfc6(i)*(1.d0-dcpp)
        end do
!       actualisation endo de cisaillement complet        
        dcom=1.d0-(1.d0-dccmax)*(1.d0-dcpp)
        varf(10)=dcom
        
        
!***********************************************************************        
!       traitement des erreurs         
        if(ierr1.eq.1) then
             print*,'pb lors du calcul des endommagements dans endo3d'
             return
        end if
          
   
!***********************************************************************        
!       affectation dans le tableau de sortie des contraintes
!       et prise  encompte des contribution des renforts
        do i=1,nstrs
          sigf(i)=sigf6(i)
          !print*,'sigf(',i,')=',sigf(i)
        end do 
        
!       indicateur de fin de 1er pas 
        if(ppas .and. ((istep.eq.2).or.(istep.eq.0))) then     
             varf(1)=1.d0
        else
            if (ppas) then
              varf(1) = 0.d0
            else
              varf(1) = var0(1)
            end if
        end if

!*******traitement erreur residuelle************************************
        if(ierr1.eq.1) then        
             return 
        end if 
        
        return

end subroutine endo3d
      
!***********************************************************************      
      
!***********************************************************************      
subroutine thermred3d(t,t0,t1,m,p,f)

! reduction par effet thermique
! (A.Sellier 2021/04/26)
  implicit none 
      
  real(kind=8) :: t,t0,t1,f,p,m
  real(kind=8) :: t7,t8
      
  if (m .ne. 0) then
    if ((t .ge. t0).and.(t1 .gt. t0)) then
      t7 = ((t - t0) / (t1 - t0)) ** m
      t8 = exp(-t7)
      f = p + t8 * (0.1D1 - p)
    else
      f=1.d0
    end if
  else
    f=1.d0
  end if

end subroutine thermred3d

!***********************************************************************      
      
!***********************************************************************      

subroutine x6x33(x6,x33)
  !    passage vecteur 6 matrice 33 symetrique
  implicit none
  real(kind=8) :: x6(6),x33(3,3)
  integer i,k,l
  do i=1,6
    call indice0(i,k,l)
    x33(k,l)=x6(i)
    if(k .ne. l) x33(l,k)=x33(k,l)
  end do

end subroutine x6x33

!***********************************************************************      
      
!***********************************************************************      

subroutine x33x6(x33,x6)
  ! passage matrice 33 symetrique vecteur 6
  implicit none
  real(kind=8) :: x6(6),x33(3,3)
  integer      :: i,k,l
  do i=1,6
    call indice0(i,k,l)
    if (i.le.3) then
      x6(i)=x33(k,l)
    else
      x6(i)=0.5*(x33(k,l)+x33(l,k))
    end if
  end do

end subroutine x33x6

!***********************************************************************      
      
!***********************************************************************      

subroutine b3d_valp33(x33,x3,v33)
  !A.Sellier jeu. 02 sept. 2010 18:13:24 CEST 
  !diagonalisagion 33 a partir de jacobi + quelques controles declarations externes
  implicit none
  real(kind=8) :: x33(3,3),x3(3),v33(3,3),y33(3,3)
  ! declarations locales      
  real(kind=8) :: xmax,depsv
  integer      :: i,j,k,l
  real(kind=8),parameter :: un=1.d0,epsv=1.d-6
  real(kind=8) :: eps3(3),eps1,xn

  ! une matrice est consideree comme deja diagonale si test 20 verifie
  ! fonction de la prescision relative epsv
  ! epsv est aussi utilise   dans jacob3 pour decider si deux valeurs
  ! propres petites par rapport a la troisieme peuvent etre consideree
  ! comme des doubles, ce qui evite de rechercher des vecteurs propres
  ! avec une matrice mal conditonnee
  ! enfin epsv est utilise en fin de programme pour tester si la matrice
  ! de passage fonctionne correctement, pour cela on verifie si la propriete
  ! vt*v est verifiee  a epsv pres hors diagonale si ce n est pas le cas
  ! on affiche un message non bloquant

  real(kind=8) :: v33t(3,3)
  real(kind=8) :: u33(3,3),u033(3,3),dif33(3,3)
  real(kind=8) :: xmax1      
  logical :: vpmultiple,erreur,diago,ordre
  integer :: imin,imax,imoy      

  !     epsv*d(1) valeur en dessous la quelle un terme hors diagonale est negligee
  !     lors du calcul des vecteurs propres
  vpmultiple=.false.
  diago=.false.
  ordre=.true.
  
!     print*
!     call affiche33(x33)
!-----------------------------------------------------------------------
!     normalisation du tenseur avant digonalisation
!      print*,'avant normalisation ds b3d_valp33'
!      call affiche33(x33)
!      xmax1=1.0d-8
!      do i=1,3
!         do j=1,3
!            xmax1=dmax1(xmax1,dabs(x33(i,j)))
!         end do
!      end do
!      do i=1,3
!          do j=1,3
!            x33(i,j)=x33(i,j)/xmax1
!          end do
!      end do
!      print*,'apres normalisation ds b3d_valp33'
!      call affiche33(x33)
!----------------------------------------------------------------------- 

!     STOCKAGE de x33 dans y33 pour diagonalisation par jacobi iterative
!     car cette methode ecrit sur y33
      do i=1,3
         do j=1,3
            y33(i,j)=x33(i,j)
         end do
      end do
      call b3d_jacobi(y33,v33,x3)

!     controle des normes de v33
      do i=1,3
        xn=dsqrt(v33(1,i)**2+v33(2,i)**2+v33(3,i)**2)
!        print*,'norme v(',i,')=',xn
        if(dabs(xn-1.d0).gt.1.d-4) then
            print*,'vecteur propre anormal ds b3d_valp33'
            call affiche33(x33)
!           print*,'remplacement par vi v vj'
            call indice1(i,k,l)
            v33(1,i)=v33(2,k)*v33(3,l)-v33(3,k)*v33(2,l)
            v33(2,i)=v33(3,k)*v33(1,l)-v33(1,k)*v33(3,l)
            v33(3,i)=v33(1,k)*v33(2,l)-v33(2,k)*v33(1,l) 
            xn=dsqrt(v33(1,i)**2+v33(2,i)**2+v33(3,i)**2)
            do k=1,3
               v33(k,i)=v33(k,i)/xn
            end do
            call affiche33(v33)
!           read*          
        end if
      end do
      
      
!     **verif produit scalaire entre v1 et v2*****
20    eps1=0.d0
      do i=1,3
       eps1=eps1+v33(i,1)*v33(i,2)
      end do
!      print*,'valp33 v1.v2=',eps1
      if(dabs(eps1).gt.1.d-4) then
!         print*,'erreur produit scalaire v1 v2 av :',eps1
!        test produit scalaire v1 v3         
         eps1=0.d0
         do i=1,3
           eps1=eps1+v33(i,1)*v33(i,3)
         end do
         if(dabs(eps1).gt.1.d-4) then
!            print*,'erreur produit scalaire v1 v3 av :',eps1 
!           correction de v1
            do i=1,3
              v33(i,1)=v33(i,1)-eps1*v33(i,3)
            end do
!           renormalisation
            xn=dsqrt(v33(1,1)**2+v33(2,1)**2+v33(3,1)**2)
            do i=1,3
               v33(i,1)=v33(i,1)/xn
            end do               
!            erreur=.true.
!            goto 10
         end if
!        v1 et v3 etant orthogonaux on reconstruit v2 par produit
!        vectoriel
         v33(1,2)=v33(2,1)*v33(3,3)-v33(3,1)*v33(2,3)
         v33(2,2)=v33(3,1)*v33(1,3)-v33(1,1)*v33(3,3)
         v33(3,2)=v33(1,1)*v33(2,3)-v33(2,1)*v33(1,3)
      else
!      test produit scalaire v2 v3
       eps1=0.d0
       do i=1,3
        eps1=eps1+v33(i,2)*v33(i,3)
       end do 
!       print*,'valp33 v2.v3=',eps1
       if(dabs(eps1).gt.1.d-4) then
!         print*,'erreur produit scalaire v2 v3 av :',eps1
!        test produit scalaire v1 v3         
         eps1=0.d0
         do i=1,3
           eps1=eps1+v33(i,1)*v33(i,3)
         end do
         if(dabs(eps1).gt.1.d-4) then
!            print*,'erreur produit scalaire v1 v3 av :',eps1         
!           correction de v3
            do i=1,3
              v33(i,3)=v33(i,3)-eps1*v33(i,1)
            end do
!           renormalisation
            xn=dsqrt(v33(1,3)**2+v33(2,3)**2+v33(3,3)**2)
            do i=1,3
               v33(i,3)=v33(i,3)/xn
            end do 
         end if
!        v1 et v3 etant orthogonaux on reconstruit v2 par produit
!        vectoriel
         v33(1,2)=v33(2,1)*v33(3,3)-v33(3,1)*v33(2,3)
         v33(2,2)=v33(3,1)*v33(1,3)-v33(1,1)*v33(3,3)
         v33(3,2)=v33(1,1)*v33(2,3)-v33(2,1)*v33(1,3)
        else
!        test du produit scalaire entre v1 v3
         eps1=0.d0
         do i=1,3
          eps1=eps1+v33(i,1)*v33(i,3)
         end do 
!        print*,'valp33 v3.v1=',eps1
         if(dabs(eps1).gt.1.d-4) then
!           print*,'erreur produit scalaire v3 v1 av :',eps1
!          test produit scalaire v3 v2         
           eps1=0.d0
           do i=1,3
             eps1=eps1+v33(i,2)*v33(i,3)
           end do
           if(dabs(eps1).gt.1.d-4) then
!            print*,'erreur produit scalaire v3 v2 av :',eps1         
!           correction de v3 avec v2
            do i=1,3
              v33(i,3)=v33(i,3)-eps1*v33(i,2)
            end do
!           renormalisation
            xn=dsqrt(v33(1,3)**2+v33(2,3)**2+v33(3,3)**2)
            do i=1,3
               v33(i,3)=v33(i,3)/xn
            end do 
!           v2 et v3 etant orthogonaux on reconstruit v1 par produit
!           vectoriel
            v33(1,1)=v33(2,2)*v33(3,3)-v33(3,2)*v33(2,3)
            v33(2,1)=v33(3,2)*v33(1,3)-v33(1,2)*v33(3,3)
            v33(3,1)=v33(1,2)*v33(2,3)-v33(2,2)*v33(1,3)             
           end if 
        end if        
       end if
      end if

      do i=1,3
       do j=1,3
        v33t(i,j)=v33(j,i)
       end do
      end do 
    
! ***********************************************
 
!     verif validite des matrice de passage
!     (on change de nase une matrice unitaire)
      erreur=.false.
      call matmat3d(v33,v33t,3,3,3,u33)
!      print*,'Image matrice identite apres correction'
!      call affiche33(u33)      
      do i=1,3
       do j=1,3
        if (i.eq.j)then
         if( dabs(u33(i,i)-un) .gt. dsqrt(epsv))then
          erreur=.true.
          goto 10
         endif
        else
         if( dabs(u33(i,j)) .gt. dsqrt(epsv))then
          erreur=.true.
          goto 10
         endif
        end if
       end do
      end do 

      !     affichage des variables en cas de pb de diagonalisation  

10    if(erreur)then
       print*,'_______________________________'
       print*,'pb vecteur propre ds b3d_valp33'
       print*,'matrice deja digonale :',diago
       print*,'ordre :',ordre
       print*,'vp multiple :',vpmultiple
       print*,'matrice a diagonaliser :'
       call affiche33(x33)  
       print*,'valeurs propres:',x3(1),x3(2),x3(3)
       print*,'x1-x2',x3(1)-x3(2)
       print*,'x2-x3',x3(2)-x3(3) 
       print*,'|',epsv,'*x(1)|',dabs(epsv * x3(1))     
       print*,'|',epsv,'*x(2)|',dabs(epsv * x3(2))              
       print*,'matrice de passage:'
       call affiche33(v33)
       print*,'image matrice identite:'
       call affiche33(u33)  
       print*,'valeurs propres:',x3(1),x3(2),x3(3) 
       print*,'matrice de passage:'
       call affiche33(v33)       
!      read* 
      end if
     
      return

end subroutine b3d_valp33

! ***************************************************************************************

! ***************************************************************************************      

! ----------------------------------------------------------------------------
! Numerical diagonalization of 3x3 matrcies
! Copyright (C) 2006  Joachim Kopp
! ----------------------------------------------------------------------------
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
subroutine b3d_jacobi(A,Q,W)
! ----------------------------------------------------------------------------
! Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
! matrix A using the Jacobi algorithm.
! The upper triangular part of A is destroyed during the calculation,
! the diagonal elements are read but not destroyed, and the lower
! triangular elements are not referenced at all.
! ----------------------------------------------------------------------------
! Parameters:
!   A: The symmetric input matrix
!   Q: Storage buffer for eigenvectors
!   W: Storage buffer for eigenvalues
! ----------------------------------------------------------------------------
!     .. Arguments ..
      implicit none
      real(kind=8) :: A(3,3),W(3),Q(3,3)
      real(kind=8) :: B(3,3)
      
!     .. Parameters ..
      integer,parameter :: N=3

!     .. Local Variables ..
      real(kind=8) :: SD, SO
      real(kind=8) :: S, C, T
      real(kind=8) :: G, H, Z, THETA
      real(kind=8) :: THRESH
      integer      :: I, X, Y, R


      B=A

      ! do x=1,n
      !  do y=1,n
      !   b(x,y)=a(x,y)
      !  end do
      ! end do
      
!     Initialize Q to the identitity matrix
!     --- This loop can be omitted if only the eigenvalues are desired ---
      
      DO 10 X = 1, N
        Q(X,X) = 1.0D0
        DO 11, Y = 1, X-1
          Q(X, Y) = 0.0D0
          Q(Y, X) = 0.0D0
   11   CONTINUE
   10 CONTINUE

!     Initialize W to diag(A)
      DO 20 X = 1, N
        W(X) = A(X, X)
   20 CONTINUE

!     Calculate SQR(tr(A))  
      SD = 0.0D0
      DO 30 X = 1, N
        SD = SD + ABS(W(X))
   30 CONTINUE
      SD = SD**2

!     Main iteration loop
      DO 40 I = 1, 50
!       Test for convergence
        SO = 0.0D0
        DO 50 X = 1, N
          DO 51 Y = X+1, N
            SO = SO + ABS(A(X, Y))
   51     CONTINUE
   50   CONTINUE
        IF (SO .EQ. 0.0D0) THEN
!         print*,'convergence jacobi en ',I,' iterations'
          RETURN
        END IF

        IF (I .LT. 4) THEN
          THRESH = 0.2D0 * SO / N**2
        ELSE
          THRESH = 0.0D0
        END IF

!       Do sweep
        DO 60 X = 1, N
          DO 61 Y = X+1, N
            G = 100.0D0 * ( ABS(A(X, Y)) )
            IF ( I .GT. 4 .AND. (ABS(W(X)) + G) .EQ. ABS(W(X)) .AND. ABS(W(Y)) + G .EQ. ABS(W(Y)) ) THEN
              A(X, Y) = 0.0D0
            ELSE IF (ABS(A(X, Y)) .GT. THRESH) THEN
!            Calculate Jacobi transformation
              H = W(Y) - W(X)
              IF ( ABS(H) + G .EQ. ABS(H) ) THEN
                T = A(X, Y) / H
              ELSE
                THETA = 0.5D0 * H / A(X, Y)
                IF (THETA .LT. 0.0D0) THEN
                  T = -1.0D0 / (SQRT(1.0D0 + THETA**2) - THETA)
                ELSE
                  T = 1.0D0 / (SQRT(1.0D0 + THETA**2) + THETA)
                END IF
              END IF

              C = 1.0D0 / SQRT( 1.0D0 + T**2 )
              S = T * C
              Z = T * A(X, Y)
              
!             Apply Jacobi transformation
              A(X, Y) = 0.0D0
              W(X)    = W(X) - Z
              W(Y)    = W(Y) + Z
              DO 70 R = 1, X-1
                T       = A(R, X)
                A(R, X) = C * T - S * A(R, Y)
                A(R, Y) = S * T + C * A(R, Y)
   70         CONTINUE
              DO 80, R = X+1, Y-1
                T       = A(X, R)
                A(X, R) = C * T - S * A(R, Y)
                A(R, Y) = S * T + C * A(R, Y)
   80         CONTINUE
              DO 90, R = Y+1, N
                T       = A(X, R)
                A(X, R) = C * T - S * A(Y, R)
                A(Y, R) = S * T + C * A(Y, R)
   90         CONTINUE

!             Update eigenvectors
!             --- This loop can be omitted if only the eigenvalues are desired ---
              DO 100, R = 1, N
                T       = Q(R, X)
                Q(R, X) = C * T - S * Q(R, Y)
                Q(R, Y) = S * T + C * Q(R, Y)
  100         CONTINUE
            END IF
   61     CONTINUE
   60   CONTINUE
   40 CONTINUE

      PRINT *, "B3D_JACOBI: No convergence."
      print*,'A'
      call affiche33(B)
      print*,'W'
      do i=1,3
        print*,W(i)
      end do
      print*,'Q'
      call affiche33(Q)
      return      

End subroutine
    
!**************************************************************************

!**************************************************************************

subroutine affiche33(v)
  implicit none
  real(kind=8) :: v(3,3)
  integer      :: j,k
  do j=1,3
    write(*,'(1x,3e10.3)') (v(j,k),k=1,3)
  end do

end subroutine affiche33
      
!**************************************************************************

!**************************************************************************************
subroutine indice0(i,k,l)
  ! correspondance entre les  indices en representation pseudo-vecteur (i) et tenseur (k,l)
  implicit none
  integer :: i,k,l
  if (i.le.3) then
    k=i
    l=i
  end if
  if (i.eq.4) then
    k=1
    l=2
  end if
  if (i.eq.5) then
    k=1
    l=3
  end if
  if (i.eq.6) then
    k=2
    l=3
   end if

 end subroutine indice0
!************************************************************************************** 

!**************************************************************************
subroutine indice1(i,k,l)
  ! indices complementaires dans une matrice carre
  implicit none
  integer :: i,k,l
  select case(i)
  case(1)
    k=2
    l=3
  case(2)
    k=1
    l=3
  case(3)
    k=1
    l=2
  end select

end subroutine indice1
!**************************************************************************

!*************************************************************************
subroutine transpos1(xt,x,jdim)
  ! transposition d une matrice
  implicit none
  integer      :: jdim,i,j
  real(kind=8) :: xt(jdim,jdim),x(jdim,jdim)
  do i=1,jdim
    do j=1,jdim
      xt(i,j)=x(j,i)
    end do
  end do 

end subroutine transpos1
!**************************************************************************

!**************************************************************************
SUBROUTINE MATMAT3d(A,B,NL,NC1,NC2,C)                                       
  IMPLICIT NONE 

  integer      :: NL,NC1,NC2,I,J,K
  real(kind=8) :: A(NL,nc1),B(NC1,nc2),C(NL,nc2),XX                                        
  DO I=1,NL                                                               
    DO J=1,NC2                                                              
      XX= 0.D0                                                                  
      DO K=1,NC1                                                              
        XX = A(I,K)*B(K,J) + XX                                                   
      ENDDO
      C(I,J)=XX                                                                 
    ENDDO
  ENDDO
 
END SUBROUTINE MATMAT3d

!**************************************************************************

!**************************************************************************

SUBROUTINE DSDED3D(dS3,deps3,K,G)
  !     pressions limites de CamClay ds=E*epse
  !     (A.Sellier 2021/04/23)
  implicit none
      
  real(kind=8) :: dS3(3),deps3(3),K,G
  real(kind=8) :: t3,t4,t9,t11
  
  t3 = 0.2D1 * deps3(2)
  t4 = 0.2D1 * deps3(3)
  t9 = K * (deps3(1) + deps3(2) + deps3(3))
  t11 = 0.2D1 * deps3(1)
  dS3(1) = G * (0.4D1 * deps3(1) - t3 - t4) / 0.3D1 + t9
  dS3(2) = G * (-t11 + 0.4D1 * deps3(2) - t4) / 0.3D1 + t9
  dS3(3) = G * (-t11 - t3 + 0.4D1 * deps3(3)) / 0.3D1 + t9
            
  !print*,'dans dsded3d:',dS3,deps3,K,G      

end SUBROUTINE DSDED3D

!**************************************************************************

!**************************************************************************

SUBROUTINE  CHREP36(sig3,sig6,v33)
  ! passage base prin->base fixe
  ! (A.Sellier 2021/04/24)
  implicit none 
      
  real(kind=8) :: sig3(3),sig6(6),v33(3,3)
  integer      :: i,j,k,l
  real(kind=8) :: v3(3),b33(3,3,3)
  real(kind=8) :: sig33(3,3)
  
  ! tenseur d orientation      
  do i=1,3
    do j=1,3
      v3(j)=v33(j,i)
    end do
    do l=1,3
      do k=1,3
        b33(i,l,k)=v3(l)*v3(k)
      end do
    end do
  end do
 
  ! tenseur physique      
  do i=1,3
    do j=1,3
      sig33(i,j)=0.d0
    end do
  end do
  do i=1,3
    do l=1,3
      do k=l,3
        sig33(l,k)=sig33(l,k)+sig3(i)*b33(i,l,k)
      end do
    end do
  end do

  ! stockage pseudo vecteur      
  do i=1,3
    sig6(i)=sig33(i,i)
  end do
  sig6(4)=sig33(1,2)
  sig6(5)=sig33(1,3)
  sig6(6)=sig33(2,3)
      
end SUBROUTINE CHREP36

!**************************************************************************

!**************************************************************************
subroutine ecroup3d(epldpd,edppic,Cdp_min,Cdp_max,Cdp,Hdp)
     
  ! sous programme de traitement de l ecrouissage parabolique pre pic
  ! Alain SELLIER 2021/05/05

  implicit none
      
  real(kind=8) :: epldpd,edppic,Cdp_min,Cdp_max,Cdp,Hdp
  real(kind=8) :: CdpM,cg,cg1,ldppic,ldp      
  real(kind=8) :: t1,t2,t4,t7
  
  ! variable dp equivalente = integrale du multiplicateur      
  ldp=epldpd    
  ldppic = edppic

  if (ldppic .gt. 0.d0) then
    if(ldp .lt. ldppic) then      
      ! cohesion
      cg=Cdp_min
      CdpM=Cdp_max
      t1 = CdpM - cg
      t2 = ldp ** 2
      t7 = ldppic ** 2
      Cdp =0.1D1/t7 *(0.2D1 *ldppic * ldp * t1+ t7 * cg - t2 * t1)      
      ! module d ecrouissage
      t4 = ldppic ** 2
      Hdp = -0.2D1 / t4 * (ldp - ldppic) * (CdpM - cg)
    else      
      Cdp=Cdp_max
      Hdp=0.d0      
    end if
  else      
    Cdp=Cdp_max
    Hdp=0.d0      
  end if
      
end subroutine ecroup3d

!**************************************************************************

!**************************************************************************

SUBROUTINE PCCD3D(precision3d,poro0,epccv,ppcc,pfcc,pc,rt,pt)
  ! pressions limites de CamClay 
  ! (A.Sellier 2021/04/23)
  implicit none

  real(kind=8) :: precision3d,poro0,epccv,ppcc,pfcc,pc,rt,pt
  real(kind=8) :: t8,pc1,pc2  

  ! pression de consolidation actuelle
  if (epccv.le.0.d0) then 
    ! consolidation      
    if (epccv+poro0*(1.d0-2.d0*precision3d).gt.0.d0) then 
      ! la pression de consolidation peut évoluer      
      t8 = (pfcc / ppcc) ** (epccv / poro0 / (-0.1D1 + poro0 + epccv))
      pc1 = ppcc * t8
      pc2=max(ppcc,pc,pc1)
      pc=min(pfcc,pc2)
      if (isnan(pc))then
        print*,'pb dans pccd3d appele par endo3d'
      end if
    else
      ! on a atteint la pression limite
      pc=pfcc
    end if
  else
    ! deconsolidation interdite en dessous valeur initiale      
    pc=ppcc
  end if       
      
  ! pression limite de traction triaxiale constante
  pt = -rt

end SUBROUTINE PCCD3D

!**************************************************************************

!**************************************************************************

SUBROUTINE HCCD3D(pfcc,ppcc,poro0,epccv,Hcc)
  ! Module d ecrouissage plastique tangent pour CamClay 
  ! (A.Sellier 2021/04/23)
  implicit none
      
  real(kind=8) :: pfcc,ppcc,poro0,epccv,Hcc
  real(kind=8) :: t2,t3,t5,t8,t12,t14

  ! module de depart
  t2 = pfcc / ppcc
  t3 = 0.1D1 / poro0
  t5 = -0.1D1 + poro0 + epccv
  t8 = t2 ** (0.1D1 / t5 * t3 * epccv)
  t12 = t5 ** 2
  t14 = log(t2)
  Hcc = -ppcc * t14 / t12 * t3 * (-0.1D1 + poro0) * t8

end SUBROUTINE HCCD3D

!**************************************************************************

!**************************************************************************
      
subroutine chrep6(x6,vp33,controle,xp6)
      
  ! changement de base d'un pseudo vecteur de deformation
  ! contenant des gama si controle=vrai
      
  implicit none
      
  ! variables externes           
  real(kind=8) :: x6(6),vp33(3,3),xp6(6)
  logical      :: controle
      
  ! variables locales
  integer :: j
  real(kind=8) :: x33(3,3),xp33(3,3)
            

  ! chargement et passage des deformations

  if (controle) then   
    ! gama->epsilon             
    do j=4,6
      x6(j)=0.5d0*x6(j)
    end do
  end if
      
  call x6x33(x6,x33)         
  call chrep3d(xp33,x33,vp33)
  call x33x6(xp33,xp6)
      
  if (controle) then
    ! epsilon->gama si controle         
    do j=4,6
      x6(j)=2.d0*x6(j)
      xp6(j)=2.d0*xp6(j)
    end do
  end if

end subroutine chrep6

!**************************************************************************

!**************************************************************************

subroutine chrep3d(M,A,P)

  !     changement de repère d'une matrice 3*3
  !     M = P'AP  où P'=transposée(P)
  !     M, P, A sont des matrices 3*3

  implicit none
  real(kind=8) :: M(3,3),A(3,3),P(3,3)
  real(kind=8) :: TP(3,3),R(3,3)
  integer      :: i,j

  ! on calcule la transposée de P => TP
  do i=1,3
    do j=1,3
      TP(i,j)=P(j,i)
      M(i,j)=0.D0
    enddo
  enddo
 
  call matmat3d(A,P,3,3,3,R)
  call matmat3d(TP,R,3,3,3,M)

end subroutine chrep3d

!**************************************************************************

!**************************************************************************

SUBROUTINE CRITENDO3D(sigef3,precision3d,Rt,Rc,ref,ept3, &
                      sigdp3,Cdp,delta,M,pt,pc,ncr,actif,fcr, &
                      ppcc,pfcc,poro,eplccv)

  ! test des criteres pour endo3d 
  ! 1: rankine direction 1
  ! 2: rankine direction 2
  ! 3: rankine direction 3
  ! 4: Drucker Prager
  ! 5: CamClay
  ! (A.Sellier 2021/04/26)

  implicit none

  real(kind=8) :: sigef3(3),precision3d,Rt,Rc,sigdp3(3),M,pt,pc,ref,Cdp,delta
  integer      :: ncr      
  real(kind=8) :: fcr(ncr),pcr
  logical      :: actif(ncr)
  real(kind=8) :: taueq,press,sigd3(3),ept3(3),cg,Ptran,ppcc,pfcc,poro,eplccv
      
  integer      :: i
  real(kind=8) :: t1,t2,t4,t6,t7,t8,t9,t10,t12,t13,t14,t15,t20,t23,t24,t36,t39,t43,t49,t50
  real(kind=8) :: t16,t17,t26,t27,t38,t41,t45,t51,t52      
  !     ******** criteres de Rankine *************************************

  do i=1,3
    if (sigef3(i).ge.0.) then
      fcr(i)=sigef3(i)-Rt
      if (fcr(i).gt.(precision3d*Rt)) then
        ! le critere est actif
        actif(i)=.true. 
      else
        actif(i)=.false.
      end if
    else if (ept3(i).gt.precision3d) then
      !refermeture de fissure possible
      fcr(i)=sigef3(i)+ref
      if (fcr(i).lt.(-precision3d*ref)) then
        ! refermeture
        actif(i)=.true.
      else
        actif(i)=.false.
        fcr(i)=0.d0
      end if
    else
      actif(i)=.false.
      fcr(i)=0.d0
    end if
  end do

  !     ********* citere de Drucker Prager *******************************  
   
  if (.not.(actif(1).and.actif(2).and.actif(3))) then
     
    ! pression
    press=0.d0
    do i=1,3 
      sigdp3(i)=sigef3(i)
      press=press-sigdp3(i)
    end do
    press=press/3.d0
    ! cisaillement      
    taueq=0.d0
    do i=1,3
      sigd3(i)=sigdp3(i)+press
      taueq=taueq+(sigd3(i))**2
    end do
    taueq=sqrt(taueq/2.d0)
    fcr(4)=taueq-(Cdp+delta*press)
    if (fcr(4).ge.(precision3d*Cdp)) then
      !print*,'fcr(4),precision3d*Cdp',fcr(4),precision3d*Cdp
      actif(4)=.true.
    else
      actif(4)=.false.
      ! ne pas mettre le critere a zero car il sera utilise
      ! pour l endo pre pic            
    end if
  else
    ! les trois rankine sont actifs      
    ! on desactive les autres criteres pour cette iteration      
    actif(4)=.false. 
    ! ne pas mettre le critere a zero car il sera utilise
    ! pour l endo pre pic          
  end if
      
  !     ****** critere de CamClay **************************************** 

  if ((.not.actif(1)).and.(.not.actif(2)).and.(.not.actif(3)).and.((poro+eplccv).gt.(3.d0*poro*precision3d)))then       
    ! calcul du critere de camclay
      t4 = M ** 2
      t6 = taueq ** 2
      t9 = (pc - pt) ** 2
      cg = t9 * (t4 * (press - pc) * (press - pt) + t6) / 0.4D1


       ! print*,'----'
       ! print*,press,cg,precision3d*M*rt
       ! print*,'----'



    if (cg.ge.precision3d*M*rt) then
      ! CamClay est actif        
      actif(5)=.true.
      fcr(5)=cg
      ! calcul de la pression de transition entre DP et CC
      t1 = M ** 2
      t2 = Rt * t1
      t6 = sqrt(0.3D1)
      t10 = delta ** 2
      t13 = t1 ** 2
      t14 = Rt ** 2
      t20 = pc ** 2
      t23 = t6 * t1
      t24 = Rc ** 2
      t36 = t24 * t1
      t39 = Rc * t1
      t43 = pc * t10
      t49 = 0.36D2 * Rc * Rt * delta * t23 - 0.36D2 * Rc * delta * pc * t23 + &
            0.18D2 * pc * Rt * t13 - 0.36D2 * t10 * Rt * t39 + 0.24D2 * delta * t24 * t23 - &
            0.12D2 * t10 * t36 + 0.9D1 * t14 * t13 + 0.9D1 * t20 * t13 + 0.108D3 * t43 * t2 + &
            0.36D2 * t43 * t39 - 0.36D2 * t36
      t50 = sqrt(t49)
      Ptran = -0.1D1 / (t1 + 0.3D1 * t10) &
              * (0.6D1 * delta * Rc * t6 - 0.6D1 * t10 * Rc - 0.3D1 * pc * t1 + 0.3D1 * t2 - t50) &
              / 0.6D1




      if (.not.(isnan(ptran))) then
        ! il existe une pression de transition
        if (press.lt.ptran) then
          ! desactivation de CamClay
          actif(5)=.false.
          fcr(5)=0.d0
        end if            
      end if
      ! on vient tester le depassement des pression limites
      if ( (pc.ge.(PFCC*(1.d0+precision3d))) .or. &
           (pc.le.(PPCC*(1.d0-precision3d)))) then
        ! on doit rester dans la zone de consolidation possible donc on empeche l ecoulement
         actif(5)=.false.
         fcr(5)=0.d0
      else
        ! on autorise pas la deconsolidation
        if (press.lt.((pc+pt)*(1.d0+precision3d)*0.5d0)) then
          actif(5)=.false.
          fcr(5)=0.d0
        end if
      end if         
    else
      ! cc pas actif
      actif(5)=.false.
      fcr(5)=0.d0 
    end if 
  else
    ! au moins un des rankine est actif on enleve CamClay       
    actif(5)=.false.
    fcr(5)=0.d0      
  end if


end SUBROUTINE CRITENDO3D

!**************************************************************************

!**************************************************************************

SUBROUTINE EPLRR3D(G,K,s1,s2,s3,f1,f2,f3,Vdep,numcas)
  ! Ecoulement plastique Trois Rankine (A.Sellier 2021/04/22)
  implicit none
  
  real(kind=8) :: G,K,s1,s2,s3,f1,f2,f3,Vdep(3)
  integer      :: numcas,ierr1


!fd que faire de ierr1

  ierr1=0
  
  if (numcas.eq.1) then
    ! cas 1-3
    Vdep(1) = 0.3D1 * f1 / (0.4D1 * G + 0.3D1 * K)
    Vdep(2) = 0.d0
    Vdep(3) = 0.d0 
      
  else if (numcas.eq.2) then
    ! cas 1-2
    Vdep(1) = (0.4D1 * G * f1 + 0.2D1 * G * f2 + 0.3D1 * K * f1 - 0.3D1 * K * f2) &
              / G / (G + 0.3D1 * K) / 0.4D1
    Vdep(2) = (0.2D1 * G * f1 + 0.4D1 * G * f2 - 0.3D1 * K * f1 + 0.3D1 * K * f2) &
              / G / (G + 0.3D1 * K) / 0.4D1
    Vdep(3) = 0.d0
      
  else if(numcas.eq.3) then
    ! cas 1-1     
    Vdep(1) = (0.2D1 * G * f1 + 0.2D1 * G * f2 + 0.2D1 * G * f3 + 0.6D1 * K * f1 - 0.3D1 * K * f2 - 0.3D1 * K * f3) &
               / G / K / 0.18D2
    Vdep(2) = (0.2D1 * G * f1 + 0.2D1 * G * f2 + 0.2D1 * G * f3 - 0.3D1 * K * f1 + 0.6D1 * K * f2 - 0.3D1 * K * f3) &
               / G / K / 0.18D2
    Vdep(3) = (0.2D1 * G * f1 + 0.2D1 * G * f2 + 0.2D1 * G * f3 - 0.3D1 * K * f1 - 0.3D1 * K * f2 + 0.6D1 * K * f3) &
               / G / K / 0.18D2

 else
    print*,'incoherence dans eplrr3d cas:',numcas
    ierr1=1
 end if
 
end SUBROUTINE EPLRR3D

!**************************************************************************

!**************************************************************************
      

subroutine TESTCC3D(poro0,EPLCCV,deplcc3,precision3d,log_reduc,reduc)
  
 ! test refermeture excisse lors de l ecoulement et calcul du coeff de reduction de retour radial

 implicit none 
  
 real(kind=8) :: poro0,EPLCCV,deplcc3(3),precision3d,reduc
 logical      :: log_reduc
      
 real(kind=8)::  dporo,poro1

 ! test consolidation maxi non dépassée
 dporo=deplcc3(1)+deplcc3(2)+deplcc3(3)
 poro1=poro0+EPLCCV+dporo
 ! print*,'ds testcc3d',poro0,poro1,EPLCCV,dporo
 ! read*
 if (poro1.lt.(poro0*precision3d)) then
   ! on arrive a la limite de la consolidation possible on reduit le retour pour ne pas avoir de poro nulle
   if (abs(dporo).gt.(precision3d*poro0)) then
     reduc=-(poro0+EPLCCV)/dporo
   else
     reduc=-(poro0+EPLCCV)/(precision3d*poro0)
   end if
   log_reduc=.true.
   print*,'Testcc3d reduction  increment plastique CC',reduc
   ! print*,poro0,EPLCCV,deplcc3,precision3d,log_reduc,reduc
   ! read*
 else
   reduc=1.d0        
 end if

end subroutine TESTCC3D

!**************************************************************************

!**************************************************************************

subroutine TESTREFD3D(fcr,eplr3,deplr3,actif,ncr,precision3d,log_reduc,reduc)
      
  ! test refermeture excisse lors de l coulement et calcul du coeff
  ! de reduction de retour radial
      
  implicit none 
      
  integer      :: ncr
  logical      :: actif(ncr),actif_prec(ncr)
  real(kind=8) :: fcr(ncr),eplr3(3),deplr3(3),precision3d
  logical      :: log_reduc
  real(kind=8) :: reduc,aux
  integer      :: i

  log_reduc=.false.
  reduc=1.d0
  do i=1,3
    if (actif(i)) then
      if ((eplr3(i)+deplr3(i)).lt.0.d0) then
        log_reduc=.true.
        aux=-eplr3(i)/deplr3(i)
        if (isnan(aux))then
          print*,'Pb dans testref3d, reduc = NaN, mis a zero'
          print*,i,eplr3(i),deplr3(i)
          aux=0.d0
        end if  
        ! print*,'testrefd3d reduc(',i,')=',aux
        if (aux.le.precision3d) then
          ! print*,'pb dans testrefd3d'
          ! print*,'reduc',reduc
          reduc=0.d0
          ! on desactive le critere pour la prochaine iteration
          actif(i)=.false.
          fcr(i)=0.d0                   
          ! print*,'direction',i
          ! print*,'eplr3(i)',eplr3(i)
          ! print*,'deplr3(i)',deplr3(i)
          ! read*
        else
          reduc=min(reduc,aux)
        end if
      end if
    end if
  end do
  ! if(log_reduc)then
  !   print*,'testref3d reduc',reduc
  ! end if
        
end subroutine TESTREFD3D

!**************************************************************************

!**************************************************************************
        
SUBROUTINE LR12LDP3D(K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,R2,f3,lr1,lr2,ldp,precision3d,log_err)
  ! Multiplicateur plastique Rankine 1 et 2 et Drucker Prager en 3 couples
  ! (A.Sellier 2021/04/22)
  implicit none 

  real(kind=8) :: K,G,b,d,R1,R2,s1,s2,s3,Hdp,f1,f2,f3,lr1,lr2,ldp,precision3d
  logical      :: log_err
  real(kind=8) :: DD(3,3),denom
  real(kind=8) :: t2,t4,t5,t9,t13,t14,t16,t17,t18,t19,t29,t36,t40,t45,t46,t51
  real(kind=8) :: d11,d12,d13,d21,d22,d23,d31,d32,d33

  log_err=.false.

  t2 = -0.4D1 / 0.3D1 * G - K
  t4 = -K + 0.2D1 / 0.3D1 * G
  t5 = R1 ** 2
  t9 = R2 ** 2
  t13 = s3 ** 2
  t14 = 0.3D1 * t13
  t16 = (0.3D1 * t5 + 0.3D1 * R1 * (-R2 - s3) + 0.3D1 * t9 - 0.3D1 * R2 * s3 + t14)
  if (t16.gt.0.d0) then
    t16=sqrt(t16)
  else
    !cprint*,'Pb dans lr12ldp3d t16<0'
    log_err=.true.
    goto 10
  end if
  t17 = 0.1D1 / t16
  t18 = b * K
  t19 = t16 * t18
  t29 = 0.2D1 * R2
  t36 = s1 ** 2
  t40 = s2 ** 2
  t45 = (0.3D1 * t36 + 0.3D1 * (-s2 - s3) * s1 + 0.3D1 * t40 - 0.3D1 * s2 * s3 + t14)
  if (t45.gt.0.d0) then
    t45=sqrt(t45)
  else
    ! print*,'Pb dans lr12ldp3d t45<0'
    log_err=.true. 
    goto 10        
  end if     
  t46 = t45 * d * K
  t51 = 0.1D1 / t45
  
  DD(1,1) = t2
  DD(1,2) = t4
  DD(1,3) = -0.4D1 / 0.3D1 * (0.3D1 / 0.4D1 * t19 + 0.3D1 / 0.2D1 * (R1 - R2 / 0.2D1 - s3 / 0.2D1) * G) * t17
  DD(2,1) = t4
  DD(2,2) = t2
  DD(2,3) = (-t19 + (R1 - t29 + s3) * G) * t17
  DD(3,1) = t51 * (-t46 + (-0.2D1 * s1 + s2 + s3) * G)
  DD(3,2) = t51 * (-t46 + G * (s1 - 0.2D1 * s2 + s3))
  DD(3,3) = 0.3D1 / 0.2D1 * t17 * t51 * (0.2D1 / 0.3D1 * t16 * t45 * (-d * t18 - Hdp) + (-0.2D1 * t13 + (s2 + R1 + R2 + s1) * s3 + &
           (-0.2D1 * R1 + R2) * s1 + (R1 - t29) * s2) * G)              
  d11=dd(1,1)
  d12=dd(1,2)
  d13=dd(1,3)
  d21=dd(2,1)
  d22=dd(2,2)
  d23=dd(2,3)
  d31=dd(3,1)
  d32=dd(3,2)
  d33=dd(3,3)      
  denom=(d11 * d22 * d33 - d11 * d23 * d32 - d12 * d21 * d33 + d12 * d23 * d31 + d13 * d21 * d32 - d13 * d22 * d31)     
  
  if (abs(denom).lt.precision3d) then
    log_err=.true.
    goto 10
  else
    lr1 = -(d12 * d23 * f3 - d12 * d33 * f2 - d13 * d22 * f3 + d13 * d32 * f2 + d22 * d33 * f1 - d23 * d32 * f1) /denom 
    lr2 = (d11 * d23 * f3 - d11 * d33 * f2 - d13 * d21 * f3 + d13 * d31 * f2 + d21 * d33 * f1 - d23 * d31 * f1) / denom
    ldp = -(d11 * d22 * f3 - d11 * d32 * f2 - d12 * d21 * f3 + d12 * d31 * f2 + d21 * d32 * f1 - d22 * d31 * f1) / denom 
  end if
10 if (log_err.or.isnan(lr1).or.isnan(lr2).or.isnan(ldp)) then  
     ! print*,'Pb de resolution dans lr12ldp3d'
     ! print*,'t16',t16
     ! print*,'t45',t45
     ! print*,'denom=',denom
     ! print*,'K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,R2,f3,lr1,lr2,ldp,prec'
     ! print*,K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,R2,f3,lr1,lr2,ldp,precision3d
     log_err=.true.
     lr1=0.d0
     lr2=0.d0
     ldp=0.d0
  end if

  ! print*,'denom',denom
  ! print*,'f',f1,f2,f3
  ! print*,lr1,lr2,ldp     
     
  ! attention: mettre s1=R1, s2=R2 dans le gradient (Vdep)     
  

end SUBROUTINE LR12LDP3D

!**************************************************************************

!**************************************************************************
        
SUBROUTINE EPLDP3D(s1,s2,s3,b,ldp,Vdep)
  ! Ecoulement plastique Drucker Prager (A.Sellier 2021/04/22)
  implicit none

  real(kind=8) :: s1,s2,s3,b,ldp,Vdep(3)
  real(kind=8) :: t4,t7,t9,t12,t15
     
  t4 = s1 ** 2
  t7 = s2 ** 2
  t9 = s3 ** 2
  t12 = sqrt(0.3D1) * (-s1 * s2 - s1 * s3 - s2 * s3 + t4 + t7 + t9) ** (-0.1D1 / 0.2D1) / 0.3D1
  t15 = b / 0.3D1
  Vdep(1) = (t12 * (0.2D1 * s1 - s2 - s3) / 0.2D1 + t15) * ldp
  Vdep(2) = (-t12 * (s1 - 0.2D1 * s2 + s3) / 0.2D1 + t15) * ldp
  Vdep(3) = (-t12 * (s1 + s2 - 0.2D1 * s3) / 0.2D1 + t15) * ldp
     
end SUBROUTINE EPLDP3D

!**************************************************************************

!**************************************************************************

SUBROUTINE LR1LDP3D(K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,lr1,ldp,precision3d,log_err)
  ! Multiplicateur plastique Rankine1 et Drucker Prager couples
  ! (A.Sellier 2021/04/22)
  implicit none 
      
  real(kind=8) :: K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,lr1,ldp,precision3d
  logical      :: log_err
  real(kind=8) :: CC(2,2),denom 
  real(kind=8) :: t3,t5,t7,t8,t10,t11,t12,t14,t15,t16,t19,t20,t27,t31,t32
  

  t3 = R1 ** 2
  t5 = -s2 - s3
  t7 = s2 ** 2
  t8 = 0.3D1 * t7
  t10 = 0.3D1 * s2 * s3
  t11 = s3 ** 2
  t12 = 0.3D1 * t11
  t14 = (0.3D1 * R1 * t5 - t10 + t12 + 0.3D1 * t3 + t8)
  if (t14.ge.0.) then
    t14=sqrt(t14)
   else
    print*,'Pb dans lr1ldp3d t14<0'
    log_err=.true. 
    goto 10        
  end if         
  t15 = 0.1D1 / t14
  t16 = b * K
  t19 = s2 / 0.2D1
  t20 = s3 / 0.2D1
  t27 = s1 ** 2
  t31 = (0.3D1 * s1 * t5 - t10 + t12 + 0.3D1 * t27 + t8)
  if (t31.gt.0.d0) then
    t31=sqrt(t31)
  else
    ! print*,'Pb dans lr1ldp3d t14<0'
    log_err=.true. 
    goto 10        
  end if      
  t32 = 0.1D1 / t31
  
  CC(1,1) = -0.4D1 / 0.3D1 * G - K
  CC(1,2) = -0.4D1 / 0.3D1 * (0.3D1 / 0.4D1 * t14 * t16 + 0.3D1 &
       / 0.2D1 * (R1 - t19 - t20) * G) * t15
  CC(2,1) = -0.3D1 * (t31 * d * K / 0.3D1 + 0.2D1 / 0.3D1 * (s1 - t19 - t20) * G) * t32
  CC(2,2) = -0.3D1 * t15 * (t14 * t31 * (d * t16 + Hdp) / 0.3D1 + &
       G * (t7 + (-R1 / 0.2D1 - s1 / 0.2D1 - s3) * s2 + t11 + &
       s3 * (-R1 - s1) / 0.2D1 + R1 * s1)) * t32
  
  denom=CC(1,1) * CC(2,2) - CC(1,2) * CC(2,1)

  if (abs(denom).lt.precision3d) then
    log_err=.true.
    goto 10
  else
    lr1 = (CC(1,2) * f2 - CC(2,2) * f1) / denom
    ldp = -(CC(1,1) * f2 - CC(2,1) * f1) / denom      
  end if
      
10 if (log_err.or.isnan(lr1).or.isnan(ldp)) then
     ! print*,'Pb1 dans lr1ldpd3d'
     ! print*,'K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,lr1,ldp'
     ! print*,K,G,b,d,s1,s2,s3,Hdp,f1,R1,f2,lr1,ldp
     lr1=0.d0
     ldp=0.d0
     log_err=.true.
  end if
      
  ! attention remplacer s1=R1  dans le gradient de DP( Vdep )    

end SUBROUTINE LR1LDP3D
!**************************************************************************


!**************************************************************************
SUBROUTINE DCCD3D(pt0,pc0,M,Hcc,s1,s2,s3,dcc)
  ! part de dfcc/dlcc due a l ecrouissage seul dans CamClay
  ! (A.Sellier 2021/04/22)
  implicit none

  real(kind=8) :: pt0,pc0,d,M,Hcc,s1,s2,s3,dcc
!  real(kind=8) :: t6,t12,t17,t20,t22,t34
  real(kind=8) :: t1,t3,t4,t8,t9,t10,t11,t13,t15,t18,t20,t22,t25,t26,t27,t29

!  t6 = pc0 + pt0
!  t12 = M ** 2
!  t17 = s1 ** 2
!  t20 = s2 ** 2
!  t22 = s3 ** 2
!  t34 = t6 ** 2
!  dcc = -t12 * t34 * (0.2D1 * s1 + 0.2D1 * s2 + 0.2D1 * s3 + 0.3D1 *pc0 + 0.3D1 * pt0) * Hcc * &
!       (0.3D1 / 0.4D1 * t12 * (pt0 + s1 / 0.3D1 + s2 / 0.3D1 + s3 / 0.3D1) * t6 * &
!       (pc0 + pt0 / 0.3D1 + 0.2D1 / 0.9D1 * s1 + 0.2D1 / 0.9D1 * s2 + 0.2D1 / 0.9D1 * s3) + &
!       (t17 + (-s2 -s3) * s1+ t20 - s2 * s3 + t22) * (pc0 - pt0) / 0.6D1) / 0.12D2

      t1 = sqrt(0.3D1)
      t3 = M ** 2
      t4 = sqrt(0.4D1)
      t8 = 0.3D1 / 0.2D1 * pt0
      t9 = s1 + s2 + s3 + 0.3D1 / 0.2D1 * pc0 + t8
      t10 = t9 ** 2
      t11 = t3 ** 2
      t13 = s1 ** 2
      t15 = -s2 - s3
      t18 = s2 ** 2
      t20 = s2 * s3
      t22 = s3 ** 2
      t25 = pc0 - pt0
      t26 = t25 ** 2
      t27 = t26 ** 2
      t29 = sqrt(t27 * (t11 * t10 + 0.81D2 / 0.2D1 * t13 + 0.81D2 / 0.2D1* &
      s1 * t15 + 0.81D2 / 0.2D1 * t18 - 0.81D2 / 0.2D1 * t20 + 0.81D2 /&
      0.2D1 * t22))
      dcc = -t26 * t25 * (t3 * (s1 + s2 + s3 + 0.3D1 * pt0) * (s1 + s2 +&
      s3 + 0.9D1 / 0.2D1 * pc0 - t8) + 0.9D1 * t13 + 0.9D1 * s1 * t15 +&
      0.9D1 * t18 - 0.9D1 * t20 + 0.9D1 * t22) * t9 / t29 * t4 * t3 * t1*&
      Hcc / 0.108D3

  ! print*,'dcc3d:',pt0,pc0,M,Hcc,s1,s2,s3,dcc

end SUBROUTINE DCCD3D
!**************************************************************************

!*************************************************************************
subroutine partition3d(sige6,sige3,vsige33,vsige33t,siget6,sigec6,sigec3,siget3)
  implicit none
  
  ! declaration des varibles externes
  real(kind=8) :: sige6(6),sige3(3),vsige33(3,3),vsige33t(3,3)
  real(kind=8) :: siget6(6),sigec6(6),sigec3(3),siget3(3)
  ! declaration des variables locales
  real(kind=8) :: sige33(3,3)
  real(kind=8) :: x33(3,3),siget33(3,3),sigec33(3,3),sige33p(3,3),sige6p(6)
  integer      :: i      

  ! rangement des contraintes effectives en tableau 3*3
  call x6x33(sige6,sige33)
  ! diagonalisation contraintes effectives actuelles et valeurs propres par la methode de jacobi
  call b3d_valp33(sige33,sige3,vsige33)
  ! creation de la matrice de passage inverse    
  call transpos1(vsige33t,vsige33,3)
  ! decomposition des contraintes principales en partie positive et négative dans 
  ! la base principale (avec prise en compte des erreurs numeriques de diagonalisation)
  ! on suppose sige33p pas tout a fait diagonale par defaut et on utilise
  ! que les contraintes normales positves pour faire la partition
  call chrep3d(sige33p,sige33,vsige33)
  call x33x6(sige33p,sige6p)
  do i=1,3
    siget3(i)=0.5d0*(sige33p(i,i)+abs(sige33p(i,i)))
    sigec3(i)=0.5d0*(sige33p(i,i)-abs(sige33p(i,i)))
    siget6(i)=siget3(i)
    sigec6(i)=sigec3(i)
  end do
  do i=4,6
    siget6(i)=0.d0
    sigec6(i)=sige6p(i)-siget6(i)
  end do
  ! stockage des parties positives et negatives en base fixe
  ! cas des contraintes de traction
  call x6x33(siget6,x33)
  call chrep3d(siget33,x33,vsige33t)
  call x33x6(siget33,siget6)
  ! cas des contraintes de compression
  call x6x33(sigec6,x33)
  call chrep3d(sigec33,x33,vsige33t)
  call x33x6(sigec33,sigec6)

end subroutine partition3d
!**************************************************************************************

!**************************************************************************************
subroutine buckling3d(altc,dt3,dct3,vwplx33,vwplx33t,sigfc61)
  ! couplage des endo de traction localise et de compression
  ! pour le flambage des biellettes de compression
  ! A.Sellier 2021/05/05
  implicit none
      
  real(kind=8) :: altc,dt3(3),dct3(3),vwplx33(3,3),vwplx33t(3,3),sigfc61(6)
  real(kind=8) :: sigfc6p(6),sigfc6ct(6),dx
  integer      :: i,j,k,l

  if (altc.gt.0.) then
    do i=1,3
      call indice1(i,k,l)
      dct3(i)=1.d0-((1.d0-dt3(k))*(1.d0-dt3(l)))**altc
    end do      
    ! passage  des contraintes effectives dans la base 
    ! prin des endo actuels
    call chrep6(sigfc61,vwplx33,.false.,sigfc6p)  
    ! application du tenseur d endommagement aux contraintes 
    ! de tractions
    do i=1,6
      if (i.le.3) then
        sigfc6ct(i)=sigfc6p(i)*(1.d0-dct3(i))
      else
        call indice0(i,k,l)
        dx=max(dct3(k),dct3(l))
        sigfc6ct(i)=sigfc6p(i)*(1.d0-dx)
      end if           
    end do
    ! retour des contraintes negatives en base fixe
    call chrep6(sigfc6ct,vwplx33t,.false.,sigfc61)
  else
    ! pas de couplage endo de traction / endo de compression       
    do i=1,3
      dct3(i)=0.d0
    end do
  end if

end subroutine buckling3d
!**************************************************************************************
      
!**************************************************************************************       
subroutine damdp3d(prec3d,ekdc,epeqpc,epspc6,dcc3,sigfc61,sigft61,dcdp,ortho)
  ! endo de compression
  ! A.Sellier 2021/05/05
  implicit none 
      
  real(kind=8) :: prec3d,epspc6(6),dcc3(3),ekdc,epeqpc,sigfc61(6),sigft61(6),dcdp
  real(kind=8) :: dcpp
  real(kind=8) :: epspc33(3,3),epspc3(3),vepspc33(3,3),vepspc33t(3,3)
  real(kind=8) :: sigfc6p(6),sigfc6ct(6),dx
  integer      :: i,j,k,l
  real(kind=8) :: trepsdc 
  real(kind=8) :: dmaxi
  logical      :: ortho
  real(kind=8) :: dcc,scom
      
  dmaxi=1.d0-prec3d      
      
  !***********************************************************************
  ! variation de raideur due aux dilatations transverses
  ! induites par le cisaillement 
  !******************************************************************
  if (ortho) then

    !***********************************************************************      
    ! cas ou l endo de DP est orthotrope      
    ! directions principale de epspc6
    call x6x33(epspc6,epspc33)       
    call b3d_valp33(epspc33,epspc3,vepspc33)
    ! do i=1,3
    !   print*,'endo3d epspc3:',i,epspc3(i)
    ! end do
    ! construction matrice de passage inverse
    call transpos1(vepspc33t,vepspc33,3) 
    ! calcul des endo orthotropes de compression (dcc)      
    do i=1,3
      call indice1(i,k,l)
      trepsdc=max(epspc3(k),0.d0)+max(epspc3(l),0.d0)
       if (trepsdc.gt.epeqpc) then
         trepsdc=trepsdc-epeqpc
         dcc3(i)=min(dmaxi,(trepsdc/(trepsdc+ekdc)))
       else
         dcc3(i)=0.d0
       endif       
    end do      
    ! *****************************************************************
    ! prise en compte endo ortho de DP sur contraintes de compression
    ! *****************************************************************
    if (max(dcc3(1),dcc3(2),dcc3(3)).gt.0.) then
      ! passage  des contraintes effectives dans la base prin des endo dcc actuels
      call chrep6(sigfc61,vepspc33,.false.,sigfc6p)  
      ! application du tenseur d endommagement aux contraintes de tractions
      do i=1,6
        if (i.le.3) then
          sigfc6ct(i)=sigfc6p(i)*(1.d0-dcc3(i))
        else
          call indice0(i,k,l)
          dx=max(dcc3(k),dcc3(l))
          sigfc6ct(i)=sigfc6p(i)*(1.d0-dx)
        end if           
      end do
      ! retour des contraintes negatives en base fixe
      call chrep6(sigfc6ct,vepspc33t,.false.,sigfc61)
    end if
  else
    !***********************************************************************      
    ! cas ou l endo de compression est isotrope
    ! calcul de la dilatance post pic
    trepsdc=0.d0
    do i=1,3
      trepsdc=trepsdc+epspc6(i)
    end do
    ! *****************************************************************       
    ! calcul de l endommagement isotrope actuel
    ! *****************************************************************       
    if (trepsdc.gt.epeqpc) then
      trepsdc=trepsdc-epeqpc
      dcc=min(dmaxi,(trepsdc/(trepsdc+ekdc)))
    else
      dcc=0.d0
    endif
    ! condition de croissance de l endommagement de cisaillement
    dcdp=max(dcdp,dcc)       
    ! *****************************************************************
    ! prise en compte endo iso de DP sur contraintes 
    ! *****************************************************************
    scom=1.d0-dcdp
    do i=1,6
      sigfc61(i)=scom*sigfc61(i)
      sigft61(i)=scom*sigft61(i)          
    end do       
  end if
end subroutine damdp3d
!***********************************************************************           

!***********************************************************************           
subroutine b3d_d66(nu,sn3,d66,prog1,comp)
  ! calcul de la matrice d'endommagement 6*6 en base principale des endommagements
  ! hypothese du materiau orthotrope dans les directions principales de fissuration
  ! sn=1/(1-d Normal)
  ! sp=1/(1-d Poisson)
  
  implicit none
  
  ! declaration externe
  real(kind=8) :: d66(6,6),sn3(3)
  real(kind=8) :: nu
  logical      :: prog1,comp
  
  ! declarations locales      
  integer      :: i,j,k,l
  real(kind=8) :: s33(3,3)
  real(kind=8) :: d1,d2,d3,sdmax,smax,sdmin

  real(kind=8) :: t1,t2,t4,t6,t8,t13,t15,t16,t21,t22,t23,t24,t27,t28,t29,t31,t32,t34, &
                  t37,t39,t40,t46,t50,t51       

  
  ! limitation l'endo si prog1=.true.
  if (prog1) then
    smax=1.0d5
    do i=1,3
      sn3(i)=min(sn3(i),smax)
    end do
  end if     

  ! initialisation de la matrice d endommagement dt66
  do i=1,6
     do j=1,6
       d66(i,j)=0.d0
     end do
  end do
  
  if (comp) then
    ! endommagement simplifie  (pas de couplage directionnel)
    do i=1,3
      do j=1,3
        if (i.eq.j)then
          s33(i,j)=1.d0/sn3(i)
        else
          s33(i,j)=0.d0
        end if
      end do
    end do 
  else      
    ! endommagement complet (othotrope avec endo de E et Nu)
    d1=1.d0-1.d0/sn3(1)
    d2=1.d0-1.d0/sn3(2)
    d3=1.d0-1.d0/sn3(3)
    t1 = nu ** 2
    t2 = 2.d0 * nu
    t4 = t1* d3* d2
    t6 = nu - 1.d0
    t8 = t1 * nu
    t13 = t8 * d1
    t15 = t1 * d1
    t16 = t15 * d2
    t21 = t15 * d3
    t22 = -t8 +3.d0 * t1 -3.d0 * nu + 1.d0 + t8 * d3 * d2 - t4 + t13 * d2 - t16 + 2.d0 * t13 * d2 * d3 + t13 * d3 - t21
    t23 = 1.d0 / t22
    t24 = -1.d0 + d1
    t27 = nu * d2
    t28 = nu * d3
    t29 = nu - 1.d0 + t28
    t31 = t6 * t23
    t32 = t31 * t24
    t34 = t27 + nu - 1.d0
    t37 = nu * d1
    t39 = -1.d0 + d2
    t40 = t31 * t39
    t46 = nu - 1.d0 + t37
    t50 = -1.d0 + d3
    t51 = t31 * t50
    s33(1,1) = -(-t1 + t2 - 1.d0 + t4) * t6 * t23 * t24
    s33(1,2) = t27 * t29 * t32
    s33(1,3) = t28 * t34 * t32
    s33(2,1) = t37 * t29 * t40
    s33(2,2) = -(-t1 + t2 - 1.d0 + t21) * t6 * t23 * t39
    s33(2,3) = t28 * t46 * t40
    s33(3,1) = t37 * t34 * t51
    s33(3,2) = t27 * t46 * t51
    s33(3,3) = -(-t1 + t2 - 1.d0 + t16) * t6 * t23 * t50
  end if
  ! print*,'1-dt33 ds b3d_d66'
  ! call affiche33(s33)
  ! read*
  do i=1,3
    do j=1,3
      if (i.eq.j) then
        d66(i,j)=1.d0-s33(i,j)
      else
        d66(i,j)=-s33(i,j)
      end if
    end do
  end do
  ! calcul des termes de cisaillement 
  do i=4,6
    call indice0(i,k,l)
    !endommagement effectif = max des deux facettes fissuree      
    !   sdmax=max(sn3(k),sn3(l)) 
    !   d66(i,i)=1.d0-1.d0/sdmax       
    !  endommagement effectif = min des deux facettes fissuree    
    sdmin=min(sn3(k),sn3(l))        
    d66(i,i)=1.d0-1.d0/sdmin  
  end do
  ! affichage de la matrice d'endommagement en base principale de fissuration
  ! print*,'endo dt66 en base principale de fissuration ds b3d_d66'
  ! call affiche66(d66)
  ! read*      

end subroutine b3d_d66
!*******************************************************************************

!*******************************************************************************
SUBROUTINE EPLCC3D(pt0,pc0,M,s1,s2,s3,lcc,Vdep)
  !Ecoulement plastique plastique CamClay (A.Sellier 2021/04/22)
  implicit none

  real(kind=8) :: pt0,pc0,M,s1,s2,s3,lcc,Vdep(3)
!  real(kind=8) :: t1,t2,t5,t6,t7,t8,t9,t11,t12,t16,t18,t19,t24,t25,t32,t37
  real(kind=8) :: t1,t3,t4,t6,t7,t10,t11,t12,t13,t15,t19,t23,t27,t28,t29,t30,t32,t33,t38
  
!  t1 = M ** 2
!  t2 = pc0 ** 2
!  t5 = t2 * pc0 * t1 / 0.12D2
!  t6 = pt0 * t1
!  t7 = 0.9D1 * t6
!  t8 = s1 + s2 + s3
!  t9 = 0.2D1 * t1 * t8
!  t11 = 0.3D1 * s2
!  t12 = 0.3D1 * s3
!  t16 = 0.4D1 / 0.9D1 * t1 * t8
!  t18 = 0.2D1 / 0.3D1 * s2
!  t19 = 0.2D1 / 0.3D1 * s3
!  t24 = pt0 ** 2
!  t25 = 0.2D1 / 0.3D1 * t1 * t8
!  t32 = 0.3D1 * s1
!  t37 = 0.2D1 / 0.3D1 * s1
  
!  Vdep(1) = (t5 + t2 * (t7 + t9 + 0.6D1 * s1 - t11 - t12) / 0.36D2 + &
!       pc0 * (t6 + t16 - 0.4D1 / 0.3D1 * s1 + t18 + t19) * pt0 / 0.4D1 + &
!       (t6 + t25 + 0.2D1 * s1 - s2 - s3) * t24 / 0.12D2) * lcc

!  Vdep(2) = (t5 + t2 * (t7 + t9 - t32 + 0.6D1 * s2 - t12) / 0.36D2 + &
!     pc0 * (t6 + t16 + t37 - 0.4D1 / 0.3D1 * s2 + t19) * pt0 / 0.4D1 + &
!     t24 * (t6 + t25 - s1 + 0.2D1 * s2 - s3) / 0.12D2) * lcc

!  Vdep(3) = (t5 + t2 * (t7 + t9 - t32 - t11 + 0.6D1 * s3) / 0.36D2 + &
!       pc0 * (t6 + t16 + t37 + t18 - 0.4D1 / 0.3D1 * s3) * pt0 / 0.4D1 + &
!       (t6 + t25 - s1 - s2 + 0.2D1 * s3) * t24 / 0.12D2) * lcc

      t1 = sqrt(0.3D1)
      t3 = sqrt(0.4D1)
      t4 = t3 * t1 * lcc
      t6 = (pc0 - pt0) ** 2
      t7 = t6 ** 2
      t10 = s1 + s2 + s3 + 0.3D1 / 0.2D1 * pc0 + 0.3D1 / 0.2D1 * pt0
      t11 = t10 ** 2
      t12 = M ** 2
      t13 = t12 ** 2
      t15 = s1 ** 2
      t19 = s2 ** 2
      t23 = s3 ** 2
      t27 = sqrt((t13 * t11 + 0.81D2 / 0.2D1 * t15 + 0.81D2 / 0.2D1 * &
      (-s2 - s3) * s1 + 0.81D2 / 0.2D1 * t19 - 0.81D2 / 0.2D1 * s2 * s3 + &
      0.81D2 / 0.2D1 * t23) * t7)
      t28 = 0.1D1 / t27
      t29 = t6 * t28
      t30 = t12 * t10
      t32 = 0.9D1 / 0.2D1 * s2
      t33 = 0.9D1 / 0.2D1 * s3
      t38 = 0.9D1 / 0.2D1 * s1
      Vdep(1) = (t30 + 0.9D1 * s1 - t32 - t33) * t29 * t4 / 0.6D1
      Vdep(2) = t6 * (t30 - t38 + 0.9D1 * s2 - t33) * t28 * t4 / 0.6D1
      Vdep(3) = (t30 - t38 - t32 + 0.9D1 * s3) * t29 * t4 / 0.6D1


  if(isnan(Vdep(1))) then
    print*,'Pb dans eplcc3d'
    print*,'pt0,pc0,M,s1,s2,s3,lcc,Vdep'
    print*,pt0,pc0,M,s1,s2,s3,lcc,Vdep
  end if
  
end subroutine EPLCC3D
!*******************************************************************************

!*******************************************************************************
SUBROUTINE LCCD3D(pt0,pc0,M,s1,s2,s3,K,G,dcc,f1,lcc,precision3d)
  !Multiplicateur plastique CamClay seul actif (A.Sellier 2021/04/22)
  implicit none
      
  real(kind=8) :: pc0,pt0,M,s1,s2,s3,Hcc,f1,lcc,K,G,dcc,precision3d
!  real(kind=8) :: t1,t2,t3,t4,t5,t18,t20,t22,t24,t26,t28,t29,t30,t33,t35,t37,t38,t43,t62,denom

  real(kind=8) ::t1,t6,t7,t8,t10,t12,t15,t17,t19,t22,t23,t24,t26,t30,t31,&
t42,t46,t50,t59,t60,t62,denom

!  t1 = M ** 2
!  t2 = t1 ** 2
!  t3 = t2 * K
!  t4 = pc0 ** 2
!  t5 = t4 ** 2
!  t18 = pt0 ** 2
!  t20 = t2 * K * t18
!  t22 = s1 + s2 + s3
!  t24 = pt0 * t22 * t3
!  t26 = t22 ** 2
!  t28 = K * t26 * t2
!  t29 = 0.4D1 * t28
!  t30 = s1 ** 2
!  t33 = s2 ** 2
!  t35 = s3 ** 2
!  t37 = G * (t30 + (-s2 - s3) * s1 + t33 - s2 * s3 + t35)
!  t38 = 0.12D2 * t37
!  t43 = t28 / 0.3D1
!  t62 = t18 ** 2
!  denom=(0.9D1 * t5 * t4 * t3 + 0.54D2 * t5 * pc0 * K * ( pt0 + 0.2D1 / 0.9D1 * s1 + &
!         0.2D1 / 0.9D1 * s2 + 0.2D1 / 0.9D1 * s3)* t2 + t5 * (0.135D3 * t20 + 0.60D2 * t24 + t29 + t38) - &
!         0.48D2 *t4 * pc0 * (-0.15D2 / 0.4D1 * t20 - 0.5D1 / 0.2D1 * t24 - t43 + t37) * pt0 + 0.72D2 * t4 * t18 * (0.15D2 / 0.8D1 * t20 + & 
!         0.5D1 / 0.3D1 * t24 + t43 + t37) - 0.48D2 * pc0 * (-0.9D1 / 0.8D1 * t20 - 0.5D1 / 0.4D1 * t24 - t43 + t37) * t18 * pt0 + &
 !        0.9D1 * t62 * t18 * t3 + 0.12D2 * t62 * pt0 * t22 * t3 + t62 * (t29 + t38) - 0.144D3 * dcc)
 ! if(abs(denom).gt.precision3d)then
 !   lcc = (0.144D3* f1) / denom  
 ! else
 !   print*,'Pb dans lccd3d, denominateur nul'
  !  lcc = (0.144D3* f1) / precision3d          
  !end if

      t1 = sqrt(0.4D1)
      t6 = (s1 + s2 + s3 + 0.3D1 / 0.2D1 * pc0 + 0.3D1 / 0.2D1 * pt0) **2
      t7 = M ** 2
      t8 = t7 ** 2
      t10 = s1 ** 2
      t12 = -s2 - s3
      t15 = s2 ** 2
      t17 = s2 * s3
      t19 = s3 ** 2
      t22 = pc0 - pt0
      t23 = t22 ** 2
      t24 = t23 ** 2
      t26 = sqrt(t24 * (t8 * t6 + 0.81D2 / 0.2D1 * t10 + 0.81D2 / 0.2D1 *&
      s1 * t12 + 0.81D2 / 0.2D1 * t15 - 0.81D2 / 0.2D1 * t17 + 0.81D2/& 
      0.2D1 * t19))
      t30 = t8 * K
      t31 = pc0 ** 2
      t42 = pt0 ** 2
      t46 = s1 + s2 + s3
      t50 = t46 ** 2
      t59 = t22 ** 2
      t60 = t59 ** 2
      t62 = sqrt(0.3D1)
      lcc = 0.36D2 / (-0.36D2 * dcc * t26 * t1 + 0.108D3 * t62 * t60 *&
      (t31 * t30 / 0.12D2 + pc0 * t8 * (pt0 + 0.2D1 / 0.3D1 * s1 + 0.2D1/& 
      0.3D1 * s2 + 0.2D1 / 0.3D1 * s3) * K / 0.6D1 + t8 * K * t42 / 0.12D2 +&
      pt0 * t46 * t30 / 0.9D1 + K * t50 * t8 / 0.27D2 + (s1 * t12+&
      t10 + t15 - t17 + t19) * G)) * t26 * t1 * f1

      if(isnan(lcc)) then
         print*,'Pb dans lccd3d, denominateur nul'
         print*,pt0,pc0,M,s1,s2,s3,K,G,dcc,f1,lcc
         read*
         lcc=0.d0         
      end if
      
!     on ne prend que lcc/4 a cause de la sous estimation du module d ecrouissage non lineaire
      lcc=lcc/2.d0   

      return

end subroutine LCCD3D
!******************************************************************************* 
   
!******************************************************************************* 
SUBROUTINE LCCLDPD3D(M,K,G,b,d,pt0,pc0,s1,s2,s3,dcc,Hdp,lcc,ldp,fc1,fd2,precision3d,log_err)
  ! Multiplicateur plastique CamClay et Drucker Prager couples
  ! (A.Sellier 2021/04/22)

  implicit none

  real(kind=8) :: M,K,G,b,d,pt0,pc0,s1,s2,s3,dcc,Hdp,lcc,ldp,fc1,fd2,precision3d
  logical      :: log_err
  real(kind=8) :: BB(2,2),denom
!  real(kind=8) :: t1,t2,t3,t4,t5,t18,t21,t25,t28,t29,t30,t32,t33,t34,t35,t36,t40,t46,t50,t53,t54,t57,t59,t87,t89,t94,t96,t97,t115,t130
  real(kind=8) :: t1,t3,t5,t7,t9,t12,t14,t16,t17,t21,t22,t23,t32,t36,t38,t39,t42,t43,t44,t50,&
t54,t58,t63,t71,t72,t76,t81

  log_err=.false.

  
!  t1 = M ** 2
!  t2 = t1 ** 2
!  t3 = t2 * K
!  t4 = pc0 ** 2
!  t5 = t4 ** 2
!  t18 = pt0 ** 2
!  t21 = s1 + s2 + s3
!  t25 = t21 ** 2
!  t28 = K * t25 * t2 / 0.3D1
!  t29 = s1 ** 2
!  t30 = -s2 - s3
!  t32 = s2 ** 2
!  t33 = s2 * s3
!  t34 = s3 ** 2
!  t35 = s1 * t30 + t29 + t32 - t33 + t34
!  t36 = t35 * G
!  t40 = t18 * pt0
!  t46 = -t28 + t36
!  t50 = t4 * pc0
!  t53 = t18 ** 2
!  t54 = t53 * t3
!  t57 = t40 * t21 * t3
!  t59 = t28 + t36
!  t87 = (0.3D1 * s1 * t30 + 0.3D1 * t29 + 0.3D1 * t32 - 0.3D1 * t33 + 0.3D1 * t34)
!  if(t87.gt.0.d0) then
!    t87=sqrt(t87)
!  else
!    log_err=.true.
!    goto 10
!  end if
!  t89 = t1 * K
!  t94 = b * pt0 * t89
!  t96 = t21 * b
!  t97 = t96 * t89
!  t115 = (pc0 - pt0) ** 2
!  t130 = (pc0 + pt0) ** 2
  
!  BB(1,1) = -t5 * t4 * t3 / 0.16D2 - 0.3D1 / 0.8D1 * t5 * pc0 * K * (pt0 + 0.2D1 / 0.9D1 * s1 + &
!       0.2D1 / 0.9D1 * s2 + 0.2D1 / 0.9D1 * s3) * t2 - t5 * (0.45D2 / 0.4D1 * t18 * t3 + &
!       0.5D1 * pt0 * t21 * t3 + t28 + t36) / 0.12D2 - t50 * (0.5D1 / 0.2D1 * t40 * t3 + 0.5D1 / &
!       0.3D1 * t18 * t21 * t3 - 0.2D1 / 0.3D1 * pt0 * t46) / 0.2D1 - t4 * (0.15D2 / 0.8D1 * t54 + &
!       0.5D1 / 0.3D1 * t57 + t18 * t59) / 0.2D1 + pc0 * (-0.9D1 / 0.8D1 * t54 - 0.5D1 / 0.4D1 * t57 + &
!       t18 * t46) * pt0 / 0.3D1 - t53 * t18 * t3 / 0.16D2 - t53 * pt0 * t21 * t3 / 0.12D2 - &
!       t53 * t59 / 0.12D2 + dcc

!  BB(1,2) = -(t87 * (t50 * b * t89 / 0.2D1 + t4 * (0.3D1 / 0.2D1 * t94 + t97 / 0.3D1) - &
!       0.2D1 / 0.3D1 * pc0 * (-0.9D1 / 0.4D1 * t94 - t97) * pt0 +  &
!       t40 * b * t89 / 0.2D1 + t18 * t96 * t89 / 0.3D1) + G * t115 * t35) / t87 / 0.2D1

!  BB(2,1) = -t87 * t115 * G / 0.6D1 - K * d * t130 * (pc0 + pt0 + 0.2D1 / 0.3D1 * s1 + &
!       0.2D1 / 0.3D1 * s2 + 0.2D1 / 0.3D1 * s3) * t1 / 0.4D1

!  BB(2,2) = -K * b * d - G - Hdp

      t1 = s1 ** 2
      t3 = -s2 - s3
      t5 = s2 ** 2
      t7 = s2 * s3
      t9 = s3 ** 2
      t12 = sqrt(0.3D1 * s1 * t3 + 0.3D1 * t1 + 0.3D1 * t5 - 0.3D1 * t7+& 
      0.3D1 * t9)
      t14 = sqrt(0.4D1)
      t16 = (pc0 - pt0) ** 2
      t17 = t16 ** 2
      t21 = (s1 + s2 + s3 + 0.3D1 / 0.2D1 * pc0 + 0.3D1 / 0.2D1 * pt0)* 2
      t22 = M ** 2
      t23 = t22 ** 2
      t32 = sqrt((t23 * t21 + 0.81D2 / 0.2D1 * t1 + 0.81D2 / 0.2D1 * s1*& 
      t3 + 0.81D2 / 0.2D1 * t5 - 0.81D2 / 0.2D1 * t7 + 0.81D2 / 0.2D1 *&
      t9) * t17)
      t36 = sqrt(0.3D1)
      t38 = t23 * K
      t39 = pc0 ** 2
      t42 = 0.2D1 / 0.3D1 * s1
      t43 = 0.2D1 / 0.3D1 * s2
      t44 = 0.2D1 / 0.3D1 * s3
      t50 = pt0 ** 2
      t54 = s1 + s2 + s3
      t58 = t54 ** 2
      t63 = s1 * t3 + t1 + t5 - t7 + t9
      t71 = 0.1D1 / t32
      t72 = 0.1D1 / t12
      t76 = t22 * K
      t81 = K * b
      BB(1,1) = -t72 * t71 * t14 * (-0.2D1 * t32 * t14 * t12 * dcc + 0.6D1*&
      t12 * (t39 * t38 / 0.12D2 + pc0 * t23 * K * (pt0 + t42 + t43+& 
      t44) / 0.6D1 + t23 * K * t50 / 0.12D2 + pt0 * t54 * t38 / 0.9D1 +&
      K * t58 * t23 / 0.27D2 + G * t63) * t36 * t17) / 0.8D1
      BB(1,2) = -t72 * (t12 * (t39 * pc0 * b * t76 / 0.6D1 - t39 * t22 *&
      (pt0 - t42 - t43 - t44) * t81 / 0.6D1 - pc0 * t22 * b * K * pt0 *&
      (0.4D1 / 0.3D1 * s1 + 0.4D1 / 0.3D1 * s2 + 0.4D1 / 0.3D1 * s3 + pt0)/&
      0.6D1 + t50 * pt0 * b * t76 / 0.6D1 + t50 * t54 * b * t76 / &
      0.9D1) + G * t16 * t63) / 0.2D1
      BB(2,1) = -0.3D1 / 0.2D1 * t16 * t36 * (t12 * G + (pc0 + pt0 + t42+&
      t43 + t44) * t22 * d * K / 0.2D1) * t71 * t14
      BB(2,2) = -d * t81 - G - Hdp

  denom=BB(1,1) * BB(2,2) - BB(1,2) * BB(2,1)      


  if (abs(denom).gt.precision3d) then      
    lcc = (BB(1,2) * fd2 - BB(2,2) * fc1) / denom
    ldp = -(BB(1,1) * fd2 - BB(2,1) * fc1) / denom
  else 
    log_err=.true.
  end if
      
10 if (log_err.or.isnan(lcc).or.isnan(ldp)) then
     print*,'Pb dans lccldp3d'
     lcc=0.d0
     ldp=0.d0
     log_err=.true.
   end if

end subroutine LCCLDPD3D
!*******************************************************************************       

!*******************************************************************************       
SUBROUTINE LDPD3D(f1,K,G,b,d,Hdp,ldp)
  ! Multiplicateur plastique Drucker Prager seul actif (A.Sellier 2021/04/22)
  implicit none

  real(kind=8) :: f1,K,G,b,d,Hdp,ldp
  ! print*,f1,K,b,d,G,Hdp
  ldp = -f1 / (-K * b * d - G - Hdp)
        
end subroutine LDPD3D
!*******************************************************************************

!*******************************************************************************
subroutine LNOEUD3D(long,xe3d,NBNMAX3D,NBNB3D,dir3)

  ! calcul taille de l element a partir des coordonnees des noeuds
  ! tables de dimension fixe pour resolution des sytemes lineaires 
  ! Sellier 26/04/2021
  
  implicit none

  ! declaration 
  real(kind=8) :: dir3(3),long    
  integer      :: i,j
  real(kind=8) :: dmin,dmax,dim1
  integer      :: NBNMAX3D,NBNB3D
  real(kind=8) :: xe3d(3,NBNMAX3D)
  integer      :: err1
  
  dmin=0.d0
  dmax=0.d0
  do i=1,nbnb3d
    if (i.eq.1) then
      ! initialisation dmin dmax sur 1er neoud          
      dim1=0.d0
      do j=1,3
        dim1=dim1+xe3d(j,i)*dir3(j)
      end do
      dmin=dim1
      dmax=dim1
    else
      dim1=0.d0
      do j=1,3
        dim1=dim1+xe3d(j,i)*dir3(j)
      end do
      if (dim1.lt.dmin) then
        dmin=dim1
      else if (dim1.gt.dmax) then
        dmax=dim1 
      end if
    end if    
  end do          
  long=dmax-dmin
  if(long.eq.0.) then
    print*,'dir3',dir3,'long',long
    err1=1
  end if

end subroutine LNOEUD3D
!*******************************************************************************

!*******************************************************************************
subroutine wfiss3d(depspt3,long3,vdepspt33t,wplt06,wplt6,vwpl33,vwpl33t,wpl3,wpltx06,wpltx6,vwplx33,vwplx33t,wplx3)

  ! calcul des ouvertures de fissures      
  ! Sellier 26/04/2021
  implicit none
      
  real(kind=8) :: depspt3(3),long3(3),vdepspt33t(3,3),wplt06(6),wplt6(6)
  real(kind=8) :: vwpl33(3,3),vwpl33t(3,3),wpl3(3),wpltx06(6),wpltx6(6)
  real(kind=8) :: vwplx33(3,3),vwplx33t(3,3),wplx3(3)
      
  real(kind=8) :: dwp6(6),dw6(6)
  integer      :: i
  real(kind=8) :: wplt33(3,3)
  real(kind=8) :: wplt61(6),wpltx061(6),wpltx61(6)
  real(kind=8) :: wpltx33(3,3)
      
  ! increment des ouvertures
  do i=1,3
    dwp6(i)=depspt3(i)*long3(i)
    ! print*,depspt3(i),long3(i)
  end do
  do i=4,6
    dwp6(i)=0.d0
  end do
  ! passage des increments en base fixe 
  call chrep6(dwp6,vdepspt33t,.false.,dw6)
  ! actualisation de l ouverture actuelle (stockage en gamma)
  do i=1,6
    wplt6(i)=wplt06(i)+dw6(i)
  end do
  ! direction principale des ouvertures actuelles
  ! passage en epsilon pour diagonalise
  ! passage 33      
  call x6x33(wplt6,wplt33)      
  ! diagonalisation     
  call b3d_valp33(wplt33,wpl3,vwpl33)  
  ! construction matrice de passage inverse         
  call transpos1(vwpl33t,vwpl33,3)  
  ! on s assure que les valeurs propres sont positives
  do i=1,3
    wpl3(i)=max(wpl3(i),0.d0)
    wplt61(i)=wpl3(i)
  end do
  do i=4,6
    wplt61(i)=0.d0
  end do
  ! on repasse en matrice de def avec des eps      
  call chrep6(wplt61,vwpl33t,.false.,wplt6)      
  ! print*,' apres actualisation 1 dans majw3d'      
  ! do i=1,3
  !   print*,'ds majw3d wpl3(',i,')=',wpl3(i)
  ! end do

  ! ***** ouvertures maximales ***************************************

  ! passage des ouvertures maximale dans la base principale actuelle 
  call chrep6(wpltx06,vwpl33,.false.,wpltx061)
  ! comparaison des valeurs normales maxi
  do i=1,3
    wpltx61(i)=max(wpltx061(i),wpl3(i))        
  end do
  ! completion      
  do i=4,6
    wpltx61(i)=wpltx061(i)
  end do
  ! retour en base fixe des ouvertures maximales
  call chrep6(wpltx61,vwpl33t,.false.,wpltx6)
  ! on a les nouveaux gama      
  ! print*,' apres actualisation 2 dans majw3d'      
  ! do i=1,3
  !    print*,'ds majw3d wpl3(',i,')=',wpl3(i)
  ! end do
  ! diagonalisation des ouvertures maxi pour la base d endommagement
  ! passage 33 pour diagonalisation (apres passage en epsilon)
  ! passage en epsilon pour diagonaliser
  !    do i=1,6
  !        print*,'maj wpltx6(',i,')=',wpltx6(i)
  !    end do          
  ! on diagonalise les epsilon      
  call x6x33(wpltx6,wpltx33)      
  ! diagonalisation   
  ! print*,' apres actualisation 3 dans majw3d'      
  ! do i=1,3
  !   print*,'ds majw3d wpl3(',i,')=',wpl3(i)
  ! end do
  call b3d_valp33(wpltx33,wplx3,vwplx33)
  do i=1,3
    wplx3(i)=max(wplx3(i),0.d0)
  end do
  !    print*,' apres actualisation 4 dans majw3d'      
  !    do i=1,3
  !       print*,'ds majw3d wpl3(',i,')=',wpl3(i)
  !    end do
  ! construction matrice de passage inverse 
  call transpos1(vwplx33t,vwplx33,3)
      
  !    print*,' apres actualisation  dans majw3d w' 
  !    call affiche33( vwpl33)
  !    call affiche33( wplt33)
  !    print*,' apres actualisation  dans majw3d wmax'       
  !    call affiche33( vwplx33t) 
  !    call affiche33( wpltx33)
      
  !    do i=1,3
  !       print*,'ds majw3d wpl3(',i,')=',wpl3(i)
  !       print*,'ds majw3d wplx3(',i,')=',wplx3(i)
  !    end do

end subroutine wfiss3d
!*******************************************************************************      

!*******************************************************************************      
subroutine basefiss3d(epspt6,epspt60,depspt3,vdepspt33,vdepspt33t)
  ! base principale de devellopement des fissures      
  ! Sellier 26/04/2021
  
  implicit none
      
  real*8 depspt6(6),epspt6(6),epspt60(6),depspt3(3)
  real*8 vdepspt33(3,3),vdepspt33t(3,3)
      
  integer i
  real*8 depspt33(3,3)
      
  ! increment de plasticite      
  do i=1,6
    depspt6(i)=epspt6(i)-epspt60(i)
  end do
  ! passage 33      
  call x6x33(depspt6,depspt33)      
  ! diagonalisation  
  ! print*,'ds majw3d depspt33'
  ! call affiche33(depspt33)
  call b3d_valp33(depspt33,depspt3,vdepspt33)
  ! construction matrice de passage inverse         
  call transpos1(vdepspt33t,vdepspt33,3)

end subroutine basefiss3d
!*******************************************************************************      

end module Demmefi
