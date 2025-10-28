
!==========>  LMGC 5 JUIN 2008   <=====

!==========>          mod_SDMxx              <=====

!Module additionel pour lmgc 90 qui permet en 2D de
!creer un champ de decomposition de domaine.
!On fournit dans le chic sous le mot clef DECOMPO DOMAINE
!le nombre de decoupes sur X puis sur Y.
!A partir de la on va recupérer en sortie dans quelle case
!est chaque element, que l'on ai repartis des grains ou des contactes.


!------------------------------------------------------------------------
MODULE wrap_SDMxx
!module utilisé pour la décomposition de domaine

!------------------------------------------------------------------------
! Autres modules à utiliser

  USE SDMxx
  USE CHIC
 
!------------------------------------------------------------------------
! public functions 

 PUBLIC :: &
           !decomposition_de_domaine_DKDKx, &
           !invert_dbox, &
           chic_command_SDMxx,more_chic_command_SDMxx


 CONTAINS
!--------------------------------------------------------------------------------------------------

!!!-------------------------------------------------------
  SUBROUTINE chic_command_SDMxx
    
    IMPLICIT NONE
    
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.

    CHARACTER(len=35) :: cout

    INTEGER :: nb_boiteX,nb_boiteY
  
    IF ( INDEX(CHZZZ,'DECOMPO DOMAINE')==1) THEN
       
       CALL LOGCHIC('SDMxx')
       READ(CHLZZZ(NZZZ+1),*) nb_boiteX
       READ(CHLZZZ(NZZZ+2),*) nb_boiteY
       CALL Main_SDM(nb_boiteX,nb_boiteY)
       IETZZZ = 1
       RETURN      
    END IF
    

  END SUBROUTINE chic_command_SDMxx
!!!-------------------------------------------------------
  SUBROUTINE more_chic_command_SDMxx
    
    IMPLICIT NONE
    
  END SUBROUTINE more_chic_command_SDMxx
!!!-------------------------------------------------------

END MODULE
