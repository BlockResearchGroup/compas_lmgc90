
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

MODULE wrap_AVSxx
!module utilise pour l'ecriture d'un fichier de donnee avs

!----------> Autres modules a utiliser <-----

  USE AVSxx
  USE CHIC
 
!------------------------------------------------------------------------
! Variables locales

IMPLICIT NONE

PRIVATE


!------------------------------------------------------------------------
! public functions 

 PUBLIC :: chic_command_AVSxx,more_chic_command_AVSxx
           
 CONTAINS
!--------------------------------------------------------------------------------------------------

!!!-------------------------------------------------------
  SUBROUTINE chic_command_AVSxx
    
    IMPLICIT NONE
    
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.

    CHARACTER(len=35) :: cout

    INTEGER :: value
  
    IF ( INDEX(CHZZZ,'ECRITURE AVS')==1) THEN
       
       CALL LOGCHIC('AVSxx')
       READ(CHLZZZ(NZZZ+2),*) value
       CALL ecriture_AVS(value)
       IETZZZ = 1
       RETURN      
    END IF
    

  END SUBROUTINE chic_command_AVSxx
!!!-------------------------------------------------------
  SUBROUTINE more_chic_command_AVSxx
    
    IMPLICIT NONE
  END SUBROUTINE more_chic_command_AVSxx
!!!-------------------------------------------------------

end module
