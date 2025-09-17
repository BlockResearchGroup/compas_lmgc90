*$$$$ TRACES
C
C SUBROUTINES MODIFIEES PAR DD LE 28 / 11 / 94 POUR ECRIRE CCDDTR
C AJOUT DE IMPPVM       PAR DD LE 18 / 12 / 95
C SUBROUTINES MODIFIEES PAR DD LE 20 / 12 / 07 POUR FORTRAN
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPTDC ( NOMSUB , NIVMIN , NOM , TABLO , NL , NC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un tableau reel double precision
C
C     Exemple  :
C     CALL IMPTDP ( 'SUBROUT' , 2 , 'XXX' , XXXX , 10 , 6 )
C     Resultat :
C  - TD - SUBROUT -  Tableau : XXXX (transpose) 10 ligne(s) 6 colonne(s)
C
C ... E    CHARACT  NOMSUB : Nom de la routine appelante
C ....E    INTEGER  NIVMIN : Niveau mini pour l impression
C ... E    CHARACT  NOM    : Nom du tableau a imprimer
C ... E    DOU-PRE  TABLO  : Tableau a imprimer
C ... E    INTEGER  NL     : Nombre de lignes du tableau
C ... E    INTEGER  NC     : Nombre de colonnes du tableau
C ======================================================================
C
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER NL , NC
CDD      DOUBLE PRECISION TABLO(NL,NC)
      REAL*8 TABLO(NL,NC)
C
      INTEGER II , JJ
C
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -        recherche du maxi entre le nombre de lignes et de colonnes
           IF (NL.GT.NC) THEN
             WRITE(IOIMP,20) CCDDTR , NOMSUB , NOM , NL , NC
C
             DO II=1,NC
               IF (NC.GT.6) THEN
                 WRITE (IOIMP,10) II
               END IF
               WRITE (IOIMP,FORTDP) (TABLO(JJ,II),JJ=1,NL)
             END DO
           ELSE
             WRITE(IOIMP,21) CCDDTR , NOMSUB , NOM , NL , NC
C
             DO JJ=1,NL
               IF (NL.GT.6) THEN
                 WRITE (IOIMP,11) JJ
               END IF
               WRITE(IOIMP,FORTDP) (TABLO(JJ,II),II=1,NC)
             END DO
           END IF
      END IF
10    FORMAT(' colonne numero : ',I5)
11    FORMAT(' ligne numero : ',I5)
CDD20    FORMAT(/,A4,' - TD - ' , A6 , ' -  Tableau : ',A,' (transpose) ,',
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
20    FORMAT(/,A4,' - TD - ',A,' -  Tableau : ',A,' (transpose) ,',
     &       I5,' ligne(s) ',I5,' colonne(s) ')
CDD21    FORMAT(/,A4,' - TD - ' , A6 , ' -  Tableau : ',A ,
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
21    FORMAT(/,A4,' - TD - ',A,' -  Tableau : ',A,
     &       I5,' ligne(s) ',I5,' colonne(s) ')
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE JMPTDC ( NOMSUB,NIVMIN,NOM,TABLO,NL,INCL,NC,INCC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  L.M.T. STRUCTURES et SYSTEME le 20 / 11 / 98
C
C     Impression d un tableau reel double precision troue (cf BLAS)
C	Generalisation du stockage utilise dans IMPTDC
C
C	On attend
C	  NOMSUB	CHARACTER		Nom de la routine appelante
C	  NIVMIN	INTEGER		Niveau mini pour l impression
C	  NOM		CHARACTER		Nom du tableau a imprimer
C	  TABLO	REAL*8		Tableau a imprimer
C	  NL		INTEGER		Nombre de lignes du tableau
C	  INCL	INTEGER		Increment sur le numero de ligne
C	  NC		INTEGER		Nombre de colonnes du tableau
C	  INCC	INTEGER		Increment sur le numero de colonne
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   Nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *   Nom de la variable 
      CHARACTER NOM *(*) 
C *   Niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *   Entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER NL , NC , INCL , INCC
      REAL*8 TABLO ( NL*INCL , NC*INCC )
C *	Divers
      INTEGER II , JJ
C -
C -   Test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
C -     Recherche du maxi entre le nombre de lignes et de colonnes
        IF (NL.GT.NC) THEN
          WRITE(IOIMP,20) CCDDTR , NOMSUB , NOM , NL , NC
          DO II=1,NC
            IF (NC.GT.6) THEN
              WRITE (IOIMP,10) II
            ENDIF
            WRITE (IOIMP,FORTDP)
     &        (TABLO(1+(JJ-1)*INCL,1+(II-1)*INCC),JJ=1,NL)
          ENDDO
        ELSE
          WRITE(IOIMP,21) CCDDTR , NOMSUB , NOM , NL , NC
          DO JJ=1,NL
            IF (NL.GT.6) THEN
              WRITE (IOIMP,11) JJ
            ENDIF
            WRITE(IOIMP,FORTDP)
     &        (TABLO(1+(JJ-1)*INCL,1+(II-1)*INCC),II=1,NC)
          ENDDO
        ENDIF
      ENDIF
C
10    FORMAT(' colonne numero : ',I5)
11    FORMAT(' ligne numero : ',I5)
CDD20    FORMAT(/,A4,' - TD - ' , A6 , ' -  Tableau : ',A,' (transpose) ,',
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
20    FORMAT(/,A4,' - TD - ',A,' -  Tableau : ',A,' (transpose) ,',
     &       I5,' ligne(s) ',I5,' colonne(s) ')
CDD21    FORMAT(/,A4,' - TD - ' , A6 , ' -  Tableau : ',A ,
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
21    FORMAT(/,A4,' - TD - ',A,' -  Tableau : ',A,
     &       I5,' ligne(s) ',I5,' colonne(s) ')
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPTRC ( NOMSUB , NIVMIN , NOM , RTABLO , NL , NC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un tableau reel simple precision
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER NL , NC
      REAL RTABLO(NL,NC)
      INTEGER II , JJ
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -       recherche du maxi entre le nombre de lignes et de colonnes
          IF (NL.GT.NC) THEN
            WRITE(IOIMP,30) CCDDTR , NOMSUB , NOM , NL , NC
C
            DO II=1,NC
              IF (NC.GT.6) THEN
                WRITE (IOIMP,10) II
              END IF
              WRITE (IOIMP,FORTDP) (RTABLO(JJ,II),JJ=1,NL)
            END DO
          ELSE
            WRITE(IOIMP,31) CCDDTR , NOMSUB , NOM , NL , NC
C
            DO JJ=1,NL
              IF (NL.GT.6) THEN
                WRITE (IOIMP,11) JJ
              END IF
              WRITE(IOIMP,FORTDP) (RTABLO(JJ,II),II=1,NC)
            END DO
          END IF
      END IF
10    FORMAT(' colonne numero : ',I5)
11    FORMAT(' ligne numero : ',I5)
CDD30    FORMAT(/,A4,' - TR - ' , A6 , ' -  Tableau : ',A,' (transpose) ,',
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
30    FORMAT(/,A4,' - TR - ',A, ' -  Tableau : ',A,' (transpose) ,',
     &       I5,' ligne(s) ',I5,' colonne(s) ')
CDD31    FORMAT(/,A4,' - TR - ' , A6 , ' -  Tableau : ',A ,
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
31    FORMAT(/,A4,' - TR - ',A,' -  Tableau : ',A,
     &       I5,' ligne(s) ',I5,' colonne(s) ')
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPTEC ( NOMSUB , NIVMIN , NOM , ITABLO , NL , NC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un tableau entier
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER NL , NC
      INTEGER ITABLO(NL,NC)
      INTEGER II , JJ
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -       recherche du maxi entre le nombre de lignes et de colonnes
          IF (NL.GT.NC) THEN
            WRITE(IOIMP,40) CCDDTR , NOMSUB , NOM , NL , NC
C
            DO II=1,NC
              IF (NC.GT.6) THEN
                WRITE (IOIMP,10) II
              END IF
              WRITE (IOIMP,42) (ITABLO(JJ,II),JJ=1,NL)
            END DO
          ELSE
            WRITE(IOIMP,41) CCDDTR , NOMSUB , NOM , NL , NC
C
            DO JJ=1,NL
              IF (NL.GT.6) THEN
                WRITE (IOIMP,11) JJ
              END IF
              WRITE(IOIMP,42) (ITABLO(JJ,II),II=1,NC)
            END DO
          END IF
      END IF
C
10    FORMAT(' colonne numero : ',I5)
11    FORMAT(' ligne numero : ',I5)
CDD40    FORMAT(/,A4,' - TE - ' , A6 , ' -  Tableau : ',A,' (transpose) ,',
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
40    FORMAT(/,A4,' - TE - ',A,' -  Tableau : ',A,' (transpose) ,',
     &       I5,' ligne(s) ',I5,' colonne(s) ')
CDD41    FORMAT(/,A4,' - TE - ' , A6 , ' -  Tableau : ',A ,
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
41    FORMAT(/,A4,' - TE - ',A,' -  Tableau : ',A,
     &       I5,' ligne(s) ',I5,' colonne(s) ')
42    FORMAT(4(5 I6 , 3X))
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPTCC ( NOMSUB , NIVMIN , NOM , TABC , NB , NL , NC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un tableau de caracteres
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER NL , NC
      INTEGER NB , NN
      CHARACTER*(*) TABC(NL,NC)
      CHARACTER*16 FORMAT
C
      INTEGER II , JJ , I
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -     determination du format d ecriture
        NN = MAX (1,120/(NB+1))
        I  = MIN (NB,120)
CC        FORMAT='(X,..(A...,'',''))  '
CC        WRITE(FORMAT(4:5),'(I2)')NN
CC        WRITE(FORMAT(8:10),'(I5)')I
C -
C -        recherche du maxi entre le nombre de lignes et de colonnes
           IF (NL.GT.NC) THEN
             WRITE(IOIMP,220) CCDDTR , NOMSUB , NOM , NB , NL , NC
C
             DO II=1,NC
               IF (NC.GT.6) THEN
                 WRITE (IOIMP,10) II
               END IF
CC               WRITE (IOIMP,FORMAT) (TABC(JJ,II),JJ=1,NL)
               WRITE (IOIMP,*) (TABC(JJ,II),JJ=1,NL)
             END DO
           ELSE
             WRITE(IOIMP,221) CCDDTR , NOMSUB , NOM , NB , NL , NC
C
             DO JJ=1,NL
               IF (NL.GT.6) THEN
                 WRITE (IOIMP,11) JJ
               END IF
CC               WRITE(IOIMP,FORMAT) (TABC(JJ,II),II=1,NC)
               WRITE(IOIMP,*) (TABC(JJ,II),II=1,NC)
             END DO
           END IF
      END IF

3456  continue


10    FORMAT(' colonne numero : ',I5)
11    FORMAT(' ligne numero : ',I5)
CDD220   FORMAT(/,A4,' - TC - ' , A6 , ' -  Tableau : ', A ,
CDD     &         ' ( *',I5,' ) (transpose) ,',
CDD     &         I5,' ligne(s) ',I5,' colonne(s) ')
220   FORMAT(/,A4,' - TC - ',A,' -  Tableau : ',A,
     &         ' ( *',I5,' ) (transpose) ,',
     &         I5,' ligne(s) ',I5,' colonne(s) ')
CDD221   FORMAT(/,A4,' - TC - ' , A6 , ' -  Tableau : ',A ,' ( *',I5,' ) ',
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
221   FORMAT(/,A4,' - TC - ',A,' -  Tableau : ',A ,' ( *',I5,' ) ',
     &       I5,' ligne(s) ',I5,' colonne(s) ')
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPTLC ( NOMSUB , NIVMIN , NOM , TABL , NL , NC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un tableau de logiques
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER NL , NC
      LOGICAL TABL (NL,NC)
C
      INTEGER II , JJ
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -
C -        recherche du maxi entre le nombre de lignes et de colonnes
           IF (NL.GT.NC) THEN
             WRITE(IOIMP,120) CCDDTR , NOMSUB , NOM , NL , NC
C
             DO II=1,NC
               IF (NC.GT.6) THEN
                 WRITE (IOIMP,10) II
               END IF
               WRITE (IOIMP,122) (TABL(JJ,II),JJ=1,NL)
             END DO
           ELSE
             WRITE(IOIMP,121) CCDDTR , NOMSUB , NOM , NL , NC
C
             DO JJ=1,NL
               IF (NL.GT.6) THEN
                 WRITE (IOIMP,11) JJ
               END IF
               WRITE(IOIMP,122) (TABL(JJ,II),II=1,NC)
             END DO
           END IF
      END IF
C
10    FORMAT(' colonne numero : ',I5)
11    FORMAT(' ligne numero : ',I5)
CDD120   FORMAT(/,A4,' - TL - ' , A6 , ' -  Tableau : ',A,' (transpose) ',
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
120   FORMAT(/,A4,' - TL - ',A,' -  Tableau : ',A,' (transpose) ',
     &       I5,' ligne(s) ',I5,' colonne(s) ')
CDD121   FORMAT(/,A4,' - TL - ' , A6 , ' -  Tableau : ', A ,
CDD     &       I5,' ligne(s) ',I5,' colonne(s) ')
121   FORMAT(/,A4,' - TL - ',A,' -  Tableau : ',A,
     &       I5,' ligne(s) ',I5,' colonne(s) ')
122   FORMAT(40L3)
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPDC ( NOMSUB , NIVMIN , NOM , X )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un reel double precision
C                traces normales
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
CDD      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
CDDDDDDDDDDD      DOUBLE PRECISION X
      REAL*8 X
C -
C -   test d impressions
      IF ( NIVMIN .LE. IIMPI ) THEN
CDD        NOM1 = NOMSUB
        WRITE(IOIMP,821) CCDDTR , NOMSUB , NOM , X
CDD        WRITE(IOIMP,821) CCDDTR , NOM1 , NOM , X
CDD     &                      ' - D  - ' // NOM1 // ' - ' , NOM , X
      END IF
CDD821   FORMAT(/,A4,' - D  - ' , A6 , ' - ',A ,3E22.14)
821   FORMAT(/,A4,' - D  - ',A,' - ',A,3E22.14)
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPRC ( NOMSUB , NIVMIN , NOM , R )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION :   impression d un reel simple precision
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      REAL R 
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
        NOM1 = NOMSUB
        WRITE(IOIMP,FORVDP) CCDDTR ,
     &                      ' - R  - ' // NOM1 // ' - ' , NOM ,R
      ENDIF
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPEC ( NOMSUB , NIVMIN , NOM , IENTIE )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un entier
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      INTEGER IENTIE
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
        WRITE(IOIMP,9) CCDDTR , NOMSUB , NOM , IENTIE
      END IF
CDD9     FORMAT(A4,' - E  - ' , A6 , ' - ' , A , ' = ' , I8 )
9     FORMAT(A4,' - E  - ',A, ' - ',A,' = ',I8)
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPCC ( NOMSUB , NIVMIN , NOM , CARAC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d une chaine de caracteres
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      CHARACTER CARAC *(*)
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
        WRITE(IOIMP,48) CCDDTR , NOMSUB , NOM , CARAC
      END IF
CDD48    FORMAT(A4,' - C  - ' , A6 , ' - ' , A ,' = ',A)
48    FORMAT(A4,' - C  - ',A, ' - ' , A ,' = ',A)

      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPLC ( NOMSUB , NIVMIN , NOM , LOGIC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d une variable logique
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   nom de la variable 
      CHARACTER NOM *(*) 
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   entier , reel , tableau a imprimer (nombre de lignes et colonnes)
      LOGICAL LOGIC
C -
C -   test d impression
      IF ( NIVMIN .LE. IIMPI ) THEN
        IF (LOGIC) THEN
          WRITE(IOIMP,28) CCDDTR , NOMSUB , NOM
CDD28        FORMAT(A4,' - L  - ' , A6 , ' - ' , A , ' = VRAI')
28        FORMAT(A4,' - L  - ',A,' - ' , A , ' = VRAI')
        ELSE
          WRITE(IOIMP,29) CCDDTR , NOMSUB , NOM
CDD29        FORMAT(A4,' - L  - ' , A6 , ' - ' , A , ' = FAUX')
29        FORMAT(A4,' - L  - ',A,' - ' , A , ' = FAUX')
        END IF
      END IF
      RETURN
	END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE IMPMC ( NOMSUB , NIVMIN , TEXTE )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression d un message
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine appelante
      CHARACTER NOMSUB*(*)
      CHARACTER*6 NOM1
C *
C *   texte a imprimer
      CHARACTER TEXTE*(*)
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C -
C -   test d impressions
      IF ( NIVMIN .LE. IIMPI ) THEN
        WRITE(IOIMP,99) CCDDTR , NOMSUB , TEXTE
      END IF
CDD99    FORMAT(A4,' - M  - ' , A6 , ' -   MESSAGE : ',A,/)
99    FORMAT(A4,' - M  - ',A,' -   MESSAGE : ',A,/)
      RETURN
      END
C 
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ENTREC ( NOM , NIVMIN )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C    COGNARD Jean Yves   L.M.T. STRUCTURES et CMAO  le 24 / 05 / 91
C
C     pour l entree et la sortie des routines
C     traces s effectuant sur IOIMP 
C
C     FONCTION : impression des entrees des sous programmes
C
C...  E   CHARACT NOM    : Nom du sous programme
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom de la routine
      CHARACTER NOM *(*)
C *
C *   niveau nimimal pour obtenir l impression
      INTEGER NIVMIN
C *
C *   pour les ecritures des entrees et sorties se S.P.
      CHARACTER*12 FORMAT
      CHARACTER*7 TENTRE , TSORTI
C *
C *   indicateur de modification de format d ecriture
      LOGICAL MODFOR
      SAVE    MODFOR
C *
C *   indicateurs de decalages
      INTEGER IDECAL , JDECAL
      SAVE    IDECAL , JDECAL
C -
      DATA FORMAT /'(A4,T02,A,A)' /
      DATA TENTRE /'--->>  '/
      DATA TSORTI /'<<===  '/
      DATA    MODFOR /.TRUE./
      DATA JDECAL / 6 /
C -
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -     ecriture eventuelle du format en tenant compte du decalage
        IF (MODFOR) THEN
          JDECAL=JDECAL+3
          IF (JDECAL.LT.100) THEN
            IDECAL=JDECAL
          END IF
          IF (JDECAL.LT.6) THEN
            WRITE(IOIMP,FORMAT) CCDDTR,TENTRE,NOM
            CALL MESFAT ( 'ENTREC' ,' probleme de decalage')
          END IF
          WRITE(FORMAT(6:7),'(I2)') IDECAL
        ELSE
          MODFOR= .TRUE.
        END IF
C -
C -     ecriture
        WRITE(IOIMP ,FORMAT) CCDDTR,TENTRE,NOM
      END IF
C -
      RETURN
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
       ENTRY SORTIC ( NOM , NIVMIN )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     FONCTION : impression des sorties des sous programmes
C
C ======================================================================
C -
      IF ( NIVMIN .LE. IIMPI ) THEN
C -
C -     ecriture eventuelle du format en tenant compte du decalage
        IF (MODFOR) THEN
          MODFOR= .FALSE.
        ELSE
          JDECAL=JDECAL-3
          IF (JDECAL.LT.100) THEN
            IDECAL=JDECAL
          END IF
          IF (JDECAL.LT.6) THEN
            WRITE(IOIMP,FORMAT) CCDDTR,TSORTI,NOM
            CALL MESFAT ( 'SORTIC' ,' probleme de decalage')
          END IF
          WRITE(FORMAT(6:7),'(I2)') IDECAL
        END IF
C -
C -     ecriture sur le fichier
        WRITE(IOIMP,FORMAT) CCDDTR,TSORTI,NOM
      END IF
C -
      RETURN
C /
      END
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE MESFAT(IPROG,IMESS)
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C    COGNARD Jean Yves   L.M.T. STRUCTURES et CMAO  le 24 / 10 / 91
C
C     message fatal !!!!!!!!!!
C ======================================================================
C *   declaration des parametres globaux
C     """"""""""""""""""""""""""""""""""
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
C *
C *   nom du programme appelant et message
      CHARACTER*(*) IPROG , IMESS
C *
C *   declaration des parametres locaux
C *   """"""""""""""""""""""""""""""""
      REAL COEF
C
      CHARACTER*6 IDPROG
      PARAMETER ( IDPROG = 'MESFAT' )
C - 
C -   ecriture du message
      WRITE(IOIMP,9001)IMESS,IPROG
C -
9001  FORMAT(//1H ,39('[]')/1X,'ERREUR FATALE :'//1X,A//' DE ',A/
     &1X,39('[]')
     &//)
C -
C -   pour recuperer la trace sur apollo
      COEF = -1.
      COEF = SQRT ( COEF )              
C -
      STOP ' C''est fini !!!!'
C /
      RETURN
	END
C 
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE MESUTI(IPROG,IMESS)
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C    COGNARD Jean Yves   L.M.T. STRUCTURES et CMAO  le 24 / 10 / 91
C
C     message intermediaire
C ======================================================================
C - 
	INTEGER LEN
CDD	EXTERNAL LEN
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
	CHARACTER*(*) IPROG
	CHARACTER*(*) IMESS
C -   ecriture du message
      WRITE(IOIMP,9002) CCDDTR , IPROG(1:LEN(IPROG)) ,
     &                           IMESS(1:LEN(IMESS))
CDD      WRITE(IOIMP,9002) CCDDTR , IPROG , IMESS
C -
9002  FORMAT(A4,' MESSAGE DE : ', A ,/,
     &       '              ' , A ) 
C /
      RETURN
      END
C
C
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
	SUBROUTINE AFFTRA ( IDDTR1 )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C    DUREISSEIX David    L.M.T. STRUCTURES et CMAO  le 28 / 11 / 94
C    DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 20 / 12 / 2007
C
C	Affecte la variable du numero de node pour les traces en parallele
C	Remplis les commons
C ======================================================================
	IMPLICIT NONE
	include 'traces.h'
	CHARACTER*4 CCDDTR
	COMMON / CDDTRA / CCDDTR
	INTEGER IDDTR1
C ======================================================================
C -
C -	format d ecriture des reels avec 15 chiffres significatifs
	FOTDPS = '(5(1X,D23.15))                  '
	FOVDPS = '(A,A,'' = '',D23.15)              '
C -	format d ecriture des reels avec 5 chiffres significatifs
	FOTDPN = '(10(1X,D12.5))                  '
	FOVDPN = '(A,A,'' = '',D13.5))              '
C -	format d ecriture des reels par defaut
	FORTDP = FOTDPN
	FORVDP = FOVDPN
C -	pour le trace en blanc
CDD	LBLANC = .FALSE.
C -	pour la sortie standard
	IOIMP = 6
C
	CCDDTR(1:4) = '    '
	IF (IDDTR1 .GT. 0) WRITE (CCDDTR(1:3),'(I3)') IDDTR1
	RETURN
	END
C
C
CDDC [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
CDD      SUBROUTINE IMPPVM ( NOMOP , NIVMIN , INS , TYP )
CDDC [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
CDDC     FONCTION : trace d un passage par un echange de message
CDDC ======================================================================
CDD	IMPLICIT NONE
CDD	include 'traces.h'
CDD	CHARACTER*4 CCDDTR
CDD	COMMON / CDDTRA / CCDDTR
CDDC *
CDDC *   Pour les envois de message
CDD	INTEGER NBNOMX
CDD	PARAMETER ( NBNOMX = 99 )
CDD	INTEGER INSTN ( NBNOMX )
CDD	COMMON / IDDTRA / INSTN
CDDC *
CDDC *   Nom de l operation
CDD      CHARACTER*6 NOMOP
CDDC *   Niveau nimimal pour obtenir l impression
CDD      INTEGER NIVMIN
CDDC *   Instance et type de message
CDD      INTEGER INS , TYP
CDDC *	Numero de node
CDD	INTEGER IDANS
CDDC ======================================================================
CDDC -
CDDC -   test d impression
CDD      IF ( NIVMIN .LE. IIMPI ) THEN
CDD	  IF (INS .EQ. -1) THEN
CDD	    IDANS = -1
CDD	  ELSE
CDD	    CALL PLACE2 ( INSTN , NBNOMX , IDANS , INS )
CDD	  ENDIF
CDD        WRITE(IOIMP,9) CCDDTR , NOMOP , IDANS , TYP
CDD      END IF
CDD9     FORMAT ( A4 , A6 , I4 , ' type ' , I4 )
CDD      RETURN
CDD	END
