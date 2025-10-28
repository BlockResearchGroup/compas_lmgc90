MODULE wrap_SDMxx
!module utilisé pour la décomposition de domaine

!------------------------------------------------------------------------
! Autres modules à utiliser

  USE SDMxx

CONTAINS

  SUBROUTINE MainSDM(nb_boiteX,nb_boiteY)

     INTEGER :: nb_boiteX,nb_boiteY 
 
     CALL Main_SDM(nb_boiteX,nb_boiteY)

  END SUBROUTINE MainSDM

END MODULE wrap_SDMxx
 

