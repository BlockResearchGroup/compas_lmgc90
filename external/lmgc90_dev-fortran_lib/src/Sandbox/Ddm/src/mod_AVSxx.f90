
!==========>         LMGC 5 JUIN 2008        <=====

!==========>         mod_AVSxx.f90           <=====

!Module additionel pour lmgc 90 qui permet de faire
!une sortie au format avs des differentes donnees 
!utilisées sur un pas de temps comme les graphe des
!contacts des grains, leur vitesses, les reactions...
!L'objectif est ici de sortir ces donnees pour pouvoir
!soit en faire un post traitement avec un autre logiciel,
!soit comparer avec un resultat obtenue par un autre
!moyen de calcul.

!==========>	     MODULE AVSxx            <=====

MODULE AVSxx
!module utilise pour l'ecriture d'un fichier de donnee avs

!----------> Autres modules a utiliser <-----

  USE overall
  USE tact_behaviour
  USE RBDY2
  USE DISKx
  !USE JONCx
  USE DKDKx

!Autre module additionel permettant la decomposition de domaine
  USE SDMxx

!------------------------------------------------------------------------
! Variables locales

IMPLICIT NONE

PRIVATE


! Variables globales du module:

!pointeur vers les coordonnee des disques de modDKDKx        
REAL(kind=8),DIMENSION(:,:),POINTER :: dcoor=>null()
!variables stoquant les nombre de disques et de contactes
INTEGER     :: dnb_contacts, dnb_DISKx
!Tables issus de modDKDKx avec les coordonnee des points de contact
REAL(kind=8),DIMENSION(:,:),POINTER :: contact_coor=>null()
!Matrice resultat de la decomposition de dommaine
!Donne pour chaque contact un numero de sous domaine
INTEGER,DIMENSION(:),POINTER :: Pmboxi

!------------------------------------------------------------------------
! public functions 

 PUBLIC :: ecriture_AVS, Recup_donnee_lmgc, D_GET_COOR, D_GET_MAILLAGE, &
  D_GET_INFO, D_GET_DONNEES_GRAIN, D_GET_DONNEES_CONTACT
           
 CONTAINS
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
SUBROUTINE ecriture_AVS(arret)


!L'entree principale de cette routine est la valeur arret
!qui donne le pas auquel on va faire la sortie du fichier AVS.
!C'est ici la sous routine principale qui recupere les donnees
!issues de mod_dkdkx, RBDY2 et eventuelement de SDMxx si il y 
!a decomposition de domaine,
!puis lance la sous routine d'ecriture au format avs.

IMPLICIT NONE

!==========> definition des variables <===== 


!----------> Variables d'entree <-----

!pas auquel on va faire la sortie du fichier AVS
INTEGER     :: arret

!----------> Variables locales <-----




!PRINT*, 'je rentre dans ecriture_AVS' 


!----------> Variables de sortie <-----

!Aucune, on ecrit un fichier dans OUTBOX

!==========> actions effectuees <=====

!recuperation des coordonee des disques et leur nombre issus du module dkdkx
call get_coor_DISKx_DKDKx(dcoor)
dnb_DISKx = size(dcoor,dim=2)
!recuperation des coordonee des contactes et leur nombre issus du module dkdkx
call get_coor_ptc_DKDKx(contact_coor)
dnb_contacts = size(contact_coor,dim=2)

!Test pour voir a quel pas on fait l'ecriture
!if (mod(Nstep, arret) == 0) then
if (Nstep == arret .or. Nstep == arret+1) then
	!Test pour voir si la decomposition de domaine a ete faite ou non
	IF (check_DD()) THEN
		!Si oui on va chercher le resultat de la decomposition
		!Donne pour chaque contact un numero de sous domaine
		call Resultat_SDM(Pmboxi)
	END IF
	!Appel de la routine d'ecriture avec les differentes donnee recuperees
	call WRITE_AVS()    !!!!!!!! bidouille pour ne plus vraiment ecrir lavs
end if

!PRINT*, 'je sort de ecriture_AVS' 
END SUBROUTINE ecriture_AVS
!----------------------------------------------------------
!--------------------------------------------------------------------------------------------------
SUBROUTINE Recup_donnee_lmgc(arret)


!L'entree principale de cette routine est la valeur arret
!qui donne le pas auquel on va faire la sortie du fichier AVS.
!C'est ici la sous routine principale qui recupere les donnees
!issues de mod_dkdkx, RBDY2 et eventuelement de SDMxx si il y 
!a decomposition de domaine,
!puis lance la sous routine d'ecriture au format avs.

IMPLICIT NONE

!==========> definition des variables <===== 


!----------> Variables d'entree <-----

!pas auquel on va faire la sortie du fichier AVS
INTEGER     :: arret

!----------> Variables locales <-----




!PRINT*, 'je rentre dans ecriture_AVS' 


!----------> Variables de sortie <-----

!Aucune, on ecrit un fichier dans OUTBOX

!==========> actions effectuees <=====

!recuperation des coordonee des disques et leur nombre issus du module dkdkx
call get_coor_DISKx_DKDKx(dcoor)
dnb_DISKx = size(dcoor,dim=2)
!recuperation des coordonee des contactes et leur nombre issus du module dkdkx
call get_coor_ptc_DKDKx(contact_coor)
dnb_contacts = size(contact_coor,dim=2)

!Test pour voir a quel pas on fait l'ecriture
!if (mod(Nstep, arret) == 0) then
!if (Nstep == arret .or. Nstep == arret+1) then
	!Test pour voir si la decomposition de domaine a ete faite ou non
	IF (check_DD()) THEN
		!Si oui on va chercher le resultat de la decomposition
		!Donne pour chaque contact un numero de sous domaine
		call Resultat_SDM(Pmboxi)
	ELSE
		call faterr('recup donnee' , 'dans recup donne c est le bordel car dd pas faite')
		stop
	END IF
	!Appel de la routine d'ecriture avec les differentes donnee recuperees
	!call WRITE_AVS()    !!!!!!!! bidouille pour ne plus vraiment ecrir lavs
!end if

!PRINT*, 'je sort de ecriture_AVS' 
END SUBROUTINE Recup_donnee_lmgc
!----------------------------------------------------------



!----------------------------------------------------------
SUBROUTINE WRITE_AVS()
!Realise l'ecriture des differentes donnees qu'on lui fournis au format AVS.

!==========> Declaration des variables <=====

!----------> Variables locales <-----

   INTEGER     :: nfich,i,nb_DISKx,idddd

!----------> Pour le maillage  <-----

   INTEGER     :: ndno, ndcell, wzero

!----------> Pour les elements <-----

   REAL(kind=8),DIMENSION(3) :: Fext
   INTEGER  :: Vix,Viy
   REAL(kind=8) :: Vdx,Vdy,Fdx,Fdy 
   REAL(kind=8) :: nombre
   REAL(kind=8),DIMENSION(3) :: Vbegin

!----------> Pour les contacts <-----

!Pour chacune des routines ci dessous la variable d'entree est le numero du contact icdan  

   INTEGER          :: icdan

!pour utiliser get_loc_DKDKx(icdan,rlt,rln,status)
!pour recuperer les reaction de contact (rln)
   REAL(kind=8)     :: rlt,rln
   CHARACTER(len=5) :: status,statusBEGIN
   
!pour utiliser get_Wik_DKDKx(icdan,icdbdy,ianbdy,tik,nik,ikcdmass,ikanmass,ikGcdt,ikGcdn,ikGant,ikGann)
!Recupations des numeros de corps associe a chaque contact (icdbdy et ianbdy)

   INTEGER                   :: icdbdy,ianbdy
   REAL(kind=8)              :: ikGcdt,ikGcdn,ikGant,ikGann
   REAL(kind=8),DIMENSION(3) :: ikcdmass,ikanmass
   REAL(kind=8),DIMENSION(2) :: tik,nik

!pour utiliser get_vloc_DKDKx(icdan,vln,vlt,vlnBEGIN,vltBEGIN)
!recuperation des vitesses relatives(vln)

   REAL(kind=8) :: vln,vlnBEGIN,vlt,vltBEGIN

!----------> Pour la decomposition de domaine <-----

   CHARACTER(len=27) :: lclout
   INTEGER :: SDM , nboiteX, nboiteY

!----------> Pour recuperer le gap <-----

   !INTEGER      :: icdan
   REAL(kind=8) :: gapBegin,gap,gapTTbegin


!PRINT*, 'je rentre WRITE_AVS' 
!==========> donnee (valeur des variables) <=====

ndno = 10
ndcell = 4
wzero = 0
nombre = 1.D0


!==========> debut d'ecriture du fichier <===== 

!----------> nom du fichier <-----

lclout='                           '
nfich = 23
WRITE(lclout(1:23),'(A23)') 'OUTBOX/DONNEES.OUT.AVS.'
WRITE(lclout(24:27),'(I4.4)') Nstep
OPEN(nfich, file=trim(lclout), form='formatted', status='replace')
	WRITE(nfich,9001) dnb_DISKx, dnb_contacts, ndno , ndcell , wzero
	
!----------> ecriture du maillage <-----
	
	!coordonees des disques:	
		DO i=1,dnb_DISKx
			WRITE(nfich,9002) i, dcoor(1,i), dcoor(2,i), 0*nombre 
		!wcoor(3,i) est la rotation pas la troisieme coordonee 3D
		END DO
	
	!graphe des contacts:
	        DO idddd=1,dnb_contacts
			!Recupations des numeros de corps associe a chaque contact (ici a chaque segment)
			call get_Wik_DKDKx(idddd,icdbdy,ianbdy,tik,nik,ikcdmass,ikanmass,ikGcdt,ikGcdn,ikGant,ikGann)
			WRITE(nfich,9003) idddd, 1, 'line', icdbdy, ianbdy
		END DO
	
!----------> carracteristiques des noeuds <-----
	
	WRITE(nfich,9004) 10,1,1,1,1,1,1,1,1,1,1
	WRITE(nfich,9005) 'RAYO,'
	WRITE(nfich,9005) 'MASS,'
	WRITE(nfich,9005) 'VX  ,'
	WRITE(nfich,9005) 'VY  ,'
	WRITE(nfich,9005) 'VIX ,'
	WRITE(nfich,9005) 'VIY ,'
	WRITE(nfich,9005) 'VDX ,'
	WRITE(nfich,9005) 'VDY ,'
	WRITE(nfich,9005) 'FDX ,'
	WRITE(nfich,9005) 'FDY ,'
	
	
	DO i=1,dnb_DISKx
		!recuperation des vitesses
		!IF (smooth_method) THEN
			Vbegin = get_V_DISKx(i)
		!ELSE
		!	Vbegin = get_Vbegin_DISKx(i)
		!ENDIF
	
		!information sur les ddl aux vitesses imposees issues de RBDY2
		call get_drv_vlocy_RBDY2(i,Vix,Viy,Vdx,Vdy)

		!recuperation des effort imposes issus de RBDY2
		Fext = get_Fext(i)
		Fdx = Fext(1)
		Fdy = Fext(2)
	
		!ecriture 
		WRITE(nfich,9006) i, get_radius_DISKx(i), get_mass_DISKx(i), Vbegin(1), Vbegin(2),Vix*nombre,Viy*nombre,Vdx,Vdy,Fdx,Fdy
	END DO

	!carracteristiques des elements (ici les contacts):
	
	!Si la decomposition de domaine a ete effectuee il faut ajouter
	!une colonne donnant l'info dans le fichier avs.
	!Test pour voir si la decomposition de domaine a ete faite ou non
	IF (check_DD()) THEN
		WRITE(nfich,9007) 4, 1, 1, 1, 1 
	ELSE
		WRITE(nfich,9007) 4, 1, 1, 1 
	ENDIF

	WRITE(nfich,9005) 'FN  ,'
	WRITE(nfich,9005) 'VN  ,'
	
	!Test pour voir si la decomposition de domaine a ete faite ou non
	IF (check_DD()) THEN
		WRITE(nfich,9005) 'SDM ,'
	ENDIF
        WRITE(nfich,9005) 'GAP ,'

	DO i=1,dnb_contacts
		!recuperation des reactions de contact
		call get_loc_DKDKx(i,rlt,rln,status)
		!recuperation des vitesses relatives
		call get_vloc_DKDKx(i,vln,vlt,vlnBEGIN,vltBEGIN)
		!recuperation du gap
                call get_gap_DKDKx(i,gapBegin,gap)
		!call get_locBEGIN_DKDKx(i,vltBEGIN,vlnBEGIN,gapTTbegin,statusBEGIN)
		

		!Test pour voir si la decomposition de domaine a ete faite ou non
		IF (check_DD()) THEN
	                WRITE(nfich,9008) i, rln, vln, Pmboxi(i)*nombre , gapBegin
			!WRITE(nfich,9008) i, rln, vlnBEGIN, Pmboxi(i)*nombre , gapTTBegin
		ELSE
			WRITE(nfich,9008) i, rln, vln
		ENDIF
	END DO

!----------> formats d'ecriture <-----
9001  FORMAT(1X,I5,1X,I5,1X,I5,1X,I5,1X,I5)
9002  FORMAT(1X,I5,1X,E21.14,1X,E21.14,1X,E21.14)
9003  FORMAT(1X,I5,1X,I2,1X,A5,1X,I5,1X,I5)
9004  FORMAT(1X,I5,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4)
9005  FORMAT(1X,A5)
9006  FORMAT(1X,I5,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14)
9007  FORMAT(1X,I5,1X,I4,1X,I4,1X,I4)
9009  FORMAT(1X,A5,I1)
9008  FORMAT(1X,I5,1X,E21.14,1X,E21.14,1X,E21.14,1X,E21.14)
9010  FORMAT(1X,A5,E21.14)

 CLOSE(nfich)


!PRINT*, 'je sort de WRITE_AVS' 

END SUBROUTINE WRITE_AVS
!-------------------------------------------------------------------

!----------------------------------------------------------
FUNCTION D_GET_COOR(numero_disk)

INTEGER     :: numero_disk

REAL(Kind=8),DIMENSION(4) :: D_GET_COOR
	
!coordonees des disques:	
!DO numero_disk=1,dnb_DISKx
!WRITE(nfich,9002) numero_disk, dcoor(1,numero_disk), dcoor(2,numero_disk), 0*nombre 

D_GET_COOR(1) = real(numero_disk,8)
D_GET_COOR(2) = dcoor(1,numero_disk)
D_GET_COOR(3) = dcoor(2,numero_disk)
D_GET_COOR(4) = dcoor(3,numero_disk)

!wcoor(3,i) est la rotation pas la troisieme coordonee 3D
!END DO

END FUNCTION D_GET_COOR
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
FUNCTION D_GET_MAILLAGE(numero_contact)

INTEGER,DIMENSION(3) :: D_GET_MAILLAGE
INTEGER     :: numero_contact

INTEGER                   :: icdbdy,ianbdy
REAL(kind=8)              :: ikGcdt,ikGcdn,ikGant,ikGann
REAL(kind=8),DIMENSION(3) :: ikcdmass,ikanmass
REAL(kind=8),DIMENSION(2) :: tik,nik




!graphe des contacts:
!DO idddd=1,wnb_contacts
!Recupations des numeros de corps associe a chaque contact (ici a chaque segment)
call get_Wik_DKDKx(numero_contact,icdbdy,ianbdy,tik,nik,ikcdmass,ikanmass,ikGcdt,ikGcdn,ikGant,ikGann)
!WRITE(nfich,9003) idddd, 1, 'line', icdbdy, ianbdy
!END DO


D_GET_MAILLAGE(1) = numero_contact
D_GET_MAILLAGE(2) = icdbdy
D_GET_MAILLAGE(3) = ianbdy


END FUNCTION D_GET_MAILLAGE
!-------------------------------------------------------------------

!----------------------------------------------------------
FUNCTION D_GET_INFO()


INTEGER,DIMENSION(5) :: D_GET_INFO
INTEGER     :: ndno, ndcell, wzero
REAL(kind=8) :: nombre

ndno = 10
ndcell = 4
wzero = 0
nombre = 1.D0



!WRITE(nfich,9001) dnb_DISKx, dnb_contacts, ndno , ndcell , wzero
D_GET_INFO(1) = dnb_DISKx
D_GET_INFO(2) = dnb_contacts
D_GET_INFO(3) = ndno
D_GET_INFO(4) = ndcell
D_GET_INFO(5) = wzero


END FUNCTION D_GET_INFO
!-------------------------------------------------------------------

FUNCTION D_GET_DONNEES_GRAIN(numero_disk)

INTEGER     :: numero_disk
REAL(Kind=8),DIMENSION(11) :: D_GET_DONNEES_GRAIN
INTEGER     :: ndno, ndcell, wzero
REAL(kind=8) :: nombre

REAL(kind=8),DIMENSION(3) :: Fext
INTEGER  :: Vix,Viy
REAL(kind=8) :: Vdx,Vdy,Fdx,Fdy 
REAL(kind=8),DIMENSION(3) :: Vbegin




!DO i=1,dnb_DISKx
!recuperation des vitesses
!IF (smooth_method) THEN
Vbegin = get_V_DISKx(numero_disk)
!ELSE
!	Vbegin = get_Vbegin_DISKx(i)
!ENDIF

!information sur les ddl aux vitesses imposees issues de RBDY2
call get_drv_vlocy_RBDY2(numero_disk,Vix,Viy,Vdx,Vdy)

!recuperation des effort imposes issus de RBDY2
Fext = get_Fext(numero_disk)
Fdx = Fext(1)
Fdy = Fext(2)

!ecriture 
!WRITE(nfich,9006) i, get_radius_DISKx(i), get_mass_DISKx(i), Vbegin(1), Vbegin(2),Vix*nombre,Viy*nombre,Vdx,Vdy,Fdx,Fdy
!END DO

D_GET_DONNEES_GRAIN(1) = real(numero_disk,8)
D_GET_DONNEES_GRAIN(2) = get_radius_DISKx(numero_disk)
D_GET_DONNEES_GRAIN(3) = get_mass_DISKx(numero_disk)
D_GET_DONNEES_GRAIN(4) = Vbegin(1)
D_GET_DONNEES_GRAIN(5) = Vbegin(2)
D_GET_DONNEES_GRAIN(6) = real(Vix,8)
D_GET_DONNEES_GRAIN(7) = real(Viy,8)
D_GET_DONNEES_GRAIN(8) = Vdx
D_GET_DONNEES_GRAIN(9) = Vdy
D_GET_DONNEES_GRAIN(10) = Fdx
D_GET_DONNEES_GRAIN(11) = Fdy




END FUNCTION D_GET_DONNEES_GRAIN
!-------------------------------------------------------------------

!----------------------------------------------------------
FUNCTION D_GET_DONNEES_CONTACT(numero_contact)



INTEGER     :: numero_contact
REAL(Kind=8),DIMENSION(5) ::  D_GET_DONNEES_CONTACT


REAL(kind=8)     :: rlt,rln
CHARACTER(len=5) :: status,statusBEGIN
REAL(kind=8) :: vln,vlnBEGIN,vlt,vltBEGIN
REAL(kind=8) :: gapBegin,gap,gapTTbegin


!PRINT*, 'D_GET_DONNEES_CONTACT' , numero_contact
	
	!Test pour voir si la decomposition de domaine a ete faite ou non
	IF (check_DD()) THEN
	
		!DO i=1,dnb_contacts
		!recuperation des reactions de contact
		call get_loc_DKDKx(numero_contact,rlt,rln,status)
		!PRINT*, 'get_loc_DKDKx' , numero_contact,rlt,rln
		!recuperation des vitesses relatives
		call get_vloc_DKDKx(numero_contact,vln,vlt,vlnBEGIN,vltBEGIN)
		!PRINT*, 'get_vloc_DKDKx' , numero_contact,vln,vlt,vlnBEGIN,vltBEGIN
		!recuperation du gap
		call get_gap_DKDKx(numero_contact,gapBegin,gap)
		!PRINT*, 'get_gap_DKDKx' , numero_contact,gapBegin,gap
		!call get_locBEGIN_DKDKx(i,vltBEGIN,vlnBEGIN,gapTTbegin,statusBEGIN)

		!WRITE(nfich,9008) numero_contact, rln, vln, Pmboxi(numero_contact)*nombre , gapBegin
		D_GET_DONNEES_CONTACT(1) = real(numero_contact,8)
		!PRINT*, 'fin du 1' 
		D_GET_DONNEES_CONTACT(2) = rln
		!PRINT*, 'fin du 2' 
		D_GET_DONNEES_CONTACT(3) = vln
		!PRINT*, 'fin du 3'
		!Print*,  'Pmboxi', associated(Pmboxi) 
		D_GET_DONNEES_CONTACT(4) = Pmboxi(numero_contact)*1.D0
		!PRINT*, 'fin du 4' 
		D_GET_DONNEES_CONTACT(5) = gapBegin
		
		!PRINT*, 'fin du bouzin' 
		
	ELSE
		Write (*,*) 'Problem decomposition de domaine non effectuee'
	ENDIF



END FUNCTION D_GET_DONNEES_CONTACT
!-------------------------------------------------------------------


END MODULE AVSxx










 






















