MODULE wrap_AVSxx
!module utilisé pour la décomposition de domaine

!------------------------------------------------------------------------
! Autres modules à utiliser

  USE AVSxx

CONTAINS

  SUBROUTINE EcritureAVS(value)

     INTEGER :: value
 
     CALL ecriture_AVS(value)

  END SUBROUTINE EcritureAVS
  
  SUBROUTINE Recupdonneelmgc(value)

     INTEGER :: value
 
     CALL Recup_donnee_lmgc(value)

  END SUBROUTINE Recupdonneelmgc
  
  FUNCTION DGETCOOR(value)

     REAL(Kind=8),DIMENSION(4) :: DGETCOOR
     
     INTEGER :: value
 
     DGETCOOR = D_GET_COOR(value)

  END FUNCTION DGETCOOR
  
  
  
  
  FUNCTION DGETMAILLAGE(value)

     INTEGER,DIMENSION(3) :: DGETMAILLAGE
     
     INTEGER :: value
 
     DGETMAILLAGE = D_GET_MAILLAGE(value)

  END FUNCTION DGETMAILLAGE
  
  
  
  
  FUNCTION DGETINFO()
  
     INTEGER,DIMENSION(5) :: DGETINFO
 
     DGETINFO = D_GET_INFO()

  END FUNCTION DGETINFO
  
  
  
  
  
  FUNCTION DGETDONNEESGRAIN(value)

  REAL(Kind=8),DIMENSION(11) :: DGETDONNEESGRAIN
     
     INTEGER :: value
 
     DGETDONNEESGRAIN = D_GET_DONNEES_GRAIN(value)

  END FUNCTION DGETDONNEESGRAIN
  
  
  
  
  FUNCTION  DGETDONNEESCONTACT(value)

     REAL(Kind=8),DIMENSION(5) ::  DGETDONNEESCONTACT
     
     INTEGER :: value
 
      DGETDONNEESCONTACT = D_GET_DONNEES_CONTACT(value)

  END FUNCTION  DGETDONNEESCONTACT
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

END MODULE wrap_AVSxx
 

