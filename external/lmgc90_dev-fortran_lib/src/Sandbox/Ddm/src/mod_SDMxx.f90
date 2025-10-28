
!==========>  LMGC 5 JUIN 2008   <=====

!==========>          mod_SDMxx              <=====

!Module additionel pour lmgc 90 qui permet en 2D de
!creer un champ de decomposition de domaine.
!On fournit dans le chic sous le mot clef DECOMPO DOMAINE
!le nombre de decoupes sur X puis sur Y.
!A partir de la on va recupérer en sortie dans quelle case
!est chaque element, que l'on ai repartis des grains ou des contactes.


!------------------------------------------------------------------------
MODULE SDMxx
!module utilisé pour la décomposition de domaine

!------------------------------------------------------------------------
! Autres modules à utiliser

  USE overall
  USE tact_behaviour
  USE RBDY2
  USE DISKx
  !USE JONCx
  USE DKDKx
  !USE DKJCx

 
!------------------------------------------------------------------------
! Variables locales

 IMPLICIT NONE

!Type de matrice donnant pour chaque numero de sous domaine,
!le nombre d'element s'y trouvant et la liste correspondante

PRIVATE
TYPE ,public :: T_boxD
   PRIVATE                                                    
   INTEGER                               :: popul   !le nombre     
   INTEGER, DIMENSION(:), POINTER        :: which   !la liste     
END TYPE T_boxD 




!vecteur donnant pour chaque element le numero du sous domaine correspondant.

INTEGER,DIMENSION(:),ALLOCATABLE,TARGET :: mboxi

!Indice permettant de savoir si une decomposition de domaine a ete realisee ou non.
!utile pour le module avs.

LOGICAL :: DD_made=.FALSE.

!------------------------------------------------------------------------
! public functions 

 PUBLIC :: &
           !decomposition_de_domaine_DKDKx, &
           !invert_dbox, &
           Main_SDM, &
           Resultat_SDM, &
           check_DD


 CONTAINS
!--------------------------------------------------------------------------------------------------

SUBROUTINE Main_SDM(nboiteX,nboiteY)



!creer le vecteur donnant pour chaque element
!le numero du sous domaine correspondant a partir
!du nombre de decoupage du domaine complet sur X et Y.

!==========> Declaration des variables <=====

!----------> Variables d'entree <-----
!les coordonnee des contacts le nombre de boites
!sur X et Y et le nombre de contact

REAL(kind=8),DIMENSION(:,:),pointer :: contact_coor => null()
INTEGER     :: nboiteX, nboiteY, dnb_contact

!----------> Variables de sortie <-----
!matrice donnant pour chaque numero de sous domaine,
!le nombre d'element s'y trouvant et la liste correspondante

TYPE(T_boxD), DIMENSION(:,:),ALLOCATABLE  :: contact_mbox
!et surtout mboxi qui est definie globalement sur le module.

!==========> Utilisation <=====

!Repartition des contacts et non pas des grains par sous domaines,
!on recupere donc les doonees des contacts.
!print*, 'je rentre dans Main_SDM' 



call get_coor_ptc_DKDKx(contact_coor)

!print*, 'contact_coor' 

dnb_contact = size(contact_coor,dim=2)

!print*, 'dnb_contact' ,dnb_contact  
!utilisation de la sous routine decomposition de domaine.
!On dispache les contact obtenues dans les sous domaines.

call decomposition_de_domaine_DKDKx(nboiteX,nboiteY,dnb_contact,contact_coor,contact_mbox)


!print*, 'coucou2' 
!invertion de la decomposition de domaine
!numero de sous domaine par numero d'elements au lieu d'avoir
!une matrice donnant pour chaque numero de sous domaine,
!le nombre d'element s'y trouvant et la liste correspondante

call invert_dbox(nboiteX,nboiteY,dnb_contact,contact_mbox,mboxi)

!changement de l'indice a vrai pour dire que la decomposition a ete realisee

DD_made=.TRUE.

!print*, 'je sort de Main_SDM'
END SUBROUTINE Main_SDM
!----------------------------------------------------------

!----------------------------------------------------------
SUBROUTINE Resultat_SDM(Pmboxi)

!sous routine permettant d'extraire le resultat du module a
!savoir un pointeur vers mbxi, le vecteur donnant pour chaque
!element le numero du sous domaine correspondant.




	INTEGER,DIMENSION(:),POINTER :: Pmboxi
	!print*, 'je rentre dans Resultat_SDM' 
	!print*, 'allocated(mboxi)' ,allocated(mboxi)
	Pmboxi => mboxi


!print*, 'je sort de Resultat_SDM' 
END SUBROUTINE
!----------------------------------------------------------

!----------------------------------------------------------
FUNCTION check_DD()

!sous routine permettant d'extraire l'indice permettant de
!savoire si une decomposition de domaine a ete realisee ou non.
 
	LOGICAL :: check_DD
!print*, 'je rentre dans check_DD'
	check_DD = DD_made
!print*, 'je sort de check_DD' 
END FUNCTION
!----------------------------------------------------------

!----------------------------------------------------------

SUBROUTINE invert_dbox(nboiteX,nboiteY,nb_elements,dbox,dboxi) 

!invertion de la decomposition de domaine
!numero de sous domaine par numero d'elements au lieu d'avoir
!une matrice donnant pour chaque numero de sous domaine,
!le nombre d'element s'y trouvant et la liste correspondante.

IMPLICIT NONE

!==========> Declaration des variables <=====

!----------> Variables d'entrée <-----

TYPE(T_boxD), DIMENSION(:,:),ALLOCATABLE  , INTENT (in)   :: dbox
INTEGER     :: nboiteX, nboiteY
  
!----------> Variables locales <-----

    INTEGER                     :: errare, nb_elements
    INTEGER                     :: dibdy,dibox1,dibox2,iwhich
   
!----------> Variables de sorties <-----
 
INTEGER,DIMENSION(:),ALLOCATABLE :: dboxi 


!print*, 'entree invert_dbox'

!==========> Utilisation <=====

!allocation de la place memoire necessaire a la construction de dbox

    IF (ALLOCATED(dboxi)) THEN
       DEALLOCATE(dboxi)
    END IF

    ALLOCATE(dboxi(1:nb_elements),stat=errare)
    
	!print* ,'dboxi alloue'
	
    !IF (errare /=0 ) THEN
    !   WRITE(cout,9103) ' nboiteX=',nboiteX,'nboiteY=',nboiteY
    !   CALL LOGMES(cout)
    !   CALL FATERR(IAM,'error allocating dbox')
    !   STOP
    !END IF

! Remplissage (on test toutes les boites pour savoir dans 
!laquel on est (et bien sur le vecteur which de chacune des boites)

DO dibdy=1,nb_elements
	DO dibox1=1,nboiteX
		DO dibox2=1,nboiteY
			DO iwhich=1,dbox(dibox1,dibox2)%popul
     IF (dbox(dibox1,dibox2)%which(iwhich) == dibdy) THEN
     !compte avec un numero par boite, pas une ordonée et une abscisse   
     dboxi(dibdy)=dibox1+(dibox2-1)*nboiteX

     END IF
			END DO
		END DO
	END DO
END DO
!on a dboxi
!print*, 'sortie invert_dbox'

END SUBROUTINE invert_dbox
!----------------------------------------------------------








!----------------------------------------------------------
SUBROUTINE decomposition_de_domaine_DKDKx(nboiteX,nboiteY,nb_elements,dcoor,dbox) 
!(création de dbox)
!à partir d'un nombre de boites sur x et sur y, du nombre d'éléments (corps ou contacts) 
!ainsi que leurs coordonées cette sous routine crée dbox dans laquel on range d'une part le 
!nombre de corp par sous domaine (popul) et d'autre part le numéros de serie respectif de chaqun 
!de ces corps (rangé par sous domaine) dans un vecteur "which".

IMPLICIT NONE

!==========> Declaration des variables <=====

!----------> Variables d'entrée <-----

    INTEGER, INTENT (in)    :: nboiteX, nboiteY, nb_elements 
    REAL(kind=8),DIMENSION(:,:),pointer :: dcoor

!----------> Variables locales <-----

    INTEGER                     :: errare 
    INTEGER                     :: dibdy,dibox1,dibox2   
    REAL(kind=8)                :: Bleft,Bright,Bup,Bdown,dray
    REAL(kind=8)                :: dLboxh,dLboxh_1,dLboxv,dLboxv_1
    REAL(kind=8),DIMENSION(nbdime)   :: coord
    LOGICAL                     :: visible
    CHARACTER(len=103)          :: cout
    CHARACTER(len=28)           :: IAM = 'mod_DKDKx::decompo_domaine'

!---------> Variables de sorties <-----
 
TYPE(T_boxD), DIMENSION(:,:),ALLOCATABLE  , INTENT (out)   :: dbox
!print*, 'entree decomposition_de_domaine_DKDKx'
!==========> Utilisation <=====

!allocation de la place memoire necessaire a la construction de dbox

    IF (ALLOCATED(dbox)) THEN
       DO dibox1=1,nboiteX
          DO dibox2=1,nboiteY
             IF ( ASSOCIATED(dbox(dibox1,dibox2)%which) ) DEALLOCATE( dbox(dibox1,dibox2)%which )
          END DO
       END DO
       DEALLOCATE(dbox)
    END IF

!print*, 'test1'
!matrice avec une case par sous domaine
    ALLOCATE(dbox(1:nboiteX,1:nboiteY),stat=errare)
    
    IF (errare /=0 ) THEN
       WRITE(cout,9103) ' nboiteX=',nboiteX,'nboiteY=',nboiteY
       CALL LOGMES(cout)
       CALL FATERR(IAM,'error allocating dbox')
       STOP
    END IF

!print*, 'test2'
!dans chacune des case de la matrice précedente 
!on met une autre matrice avec une case par corp

    DO dibox1=1,nboiteX
       DO dibox2=1,nboiteY
          dbox(dibox1,dibox2)%popul=0
          ALLOCATE(dbox(dibox1,dibox2)%which(1:nb_elements+1),stat=errare)
          IF (errare /=0 ) THEN
             CALL FATERR(IAM,'error in allocating box(1+nboiteX,1+nboiteY)%which')
             STOP
          END IF
          dbox(dibox1,dibox2)%which=0
       END DO
    END DO
!print*, 'test3'
!Fin de l'allocation des variables

!==========> création de la boite globale <=====
!print*, dcoor
!print*, dcoor(1,1)

!On prend les coordonées d'un contact (le premier de la liste)
    Bleft    =  dcoor(1,1)
    Bright   =  dcoor(1,1)
    Bup      =  dcoor(2,1)
    Bdown    =  dcoor(2,1)

!print*, 'test4'
!on la fait grossir pour qu'elle contienne tous les contacts
    DO dibdy=1,nb_elements
       !visible=.TRUE.
       coord = dcoor(1:nbdime,dibdy)
       !visible=get_visible_DISKx(dibdy)
       !IF (.NOT.visible) CYCLE
       Bleft = MIN(coord(1),Bleft )
       Bright= MAX(coord(1),Bright)
       Bup   = MAX(coord(2),Bup   )
       Bdown = MIN(coord(2),Bdown )
    END DO

!print*, 'test5'
!On a la grosse boite et on calcul les petites (sous structure)
  
!longueur des coté des sous domaines   
   dLboxh   = 1.D0*((Bright - Bleft)/nboiteX)
   dLboxh_1 = 1.D0/dLboxh
   dLboxv   = 1.D0*((Bup - Bdown)/nboiteY)
   dLboxv_1 = 1.D0/dLboxv

!==========> Remplissage <=====

!print*, 'test6'


DO dibdy=1,nb_elements
      !visible=.TRUE.
      coord=dcoor(1:nbdime,dibdy)
      !visible=get_visible_DISKx(dibdy)
      !IF (.NOT.visible) CYCLE
      dibox1=MIN(1+floor((coord(1)-Bleft )*dLboxh_1),nboiteX)
      dibox2=MIN(1+floor((coord(2)-Bdown )*dLboxv_1),nboiteY)
 !test qu'il faudrait remettre
     IF (dibox1 < 1 .OR. dibox1 > nboiteX .OR. dibox2 < 1 .OR. dibox2 > nboiteY) THEN
         WRITE(cout,9103) ' nboiteX=',nboiteX,'nboiteY=',nboiteY
         CALL LOGMES(cout)
         WRITE(cout,9103) '    dibox1=',dibox1,   '   dibox2=',dibox2
         CALL LOGMES(cout)
         WRITE(cout,'(A13,I10,A13)') '  body DISKx ',dibdy,' out of dboxes'
         CALL FATERR(IAM,cout)
         STOP
9103      FORMAT(1X,A10,1X,I5,1X,A10,1X,I5)
     END IF

      dbox(dibox1,dibox2)%popul = dbox(dibox1,dibox2)%popul+1
      dbox(dibox1,dibox2)%which(dbox(dibox1,dibox2)%popul) = dibdy

   END DO
!print*, 'test7'
!on a dbox
!print*, 'sortie decomposition_de_domaine_DKDKx'
 END SUBROUTINE decomposition_de_domaine_DKDKx
!-----------------------------------------------------------

END MODULE SDMxx
