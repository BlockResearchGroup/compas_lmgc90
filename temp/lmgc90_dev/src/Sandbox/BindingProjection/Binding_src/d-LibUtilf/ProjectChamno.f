C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ProjectChamno ( 
C       Inputs
     &                          NBPT , NBNO  , NBELM , NBVAL ,
     &                          ELEM , ALPHA , XVAL  ,
C       Outputs
     &                          YVAL )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 21 / 12 / 2007
C
C	Project a field (defined per element at its nodes) onto
C	target nodes
C	YVAL <- YVAL + Proj(XVAL)
C
C     Inputs
C	NBPT			INTEGER		Nb of target points
C	NBNO			INTEGER		Nb of nodes per element
C	NBELM			INTEGER		Nb of elements
C	NBVAL			INTEGER		Nb of values per point
C	ELEM(NBPT)		INTEGER		Corresponding element
C	ALPHA(NBNO,NBPT)	REAL*8		Corresponding weights
C	XVAL(NBNO,NBVAL,NBELM)	REAL*8		Values to project
C     Outputs
C	YVAL(NBPT,NBVAL)	REAL*8		Projected values
C     Working space
C
C	Warning: YVAL must be initialiazed before calling this
C	routine, since the projected value is added to YVAL
C
C	Calls dgemm blas routine
C
C	Autres stockages possibles
C	oriente champ a plusieurs composantes en chaque noeud
C	REAL*8 XVAL ( NBAL , NBNO , NBELM ) , YVAL ( NBAL , NBPT )
C	  YVAL(1:NBVAL,I)=XVAL(1:NBVAL,1:NBNO,IELEM)*ALPHA(1:NBNO,I)
C	  NBVAL,1         NBVAL,NBNO,1               NBNO,1
C	tres oriente champ scalaire par noeud
C	REAL*8 XVAL ( NBNO , NBELM , NBVAL ) , YVAL ( NBPT , NBVAL )
C	  necessite une boucle sur les NBVAL, et un produit scalaire
C	  YVAL(1,IVAL,I)=XVAL(1:NBNO,IELEM,IVAL)*ALPHA(1:NBNO,I)
C	  1,1            NBNO,1,1                NBNO,1
C	  ou du gruyere sur cet indice
C	  YVAL(1:NBVAL,I)=ALPHA(1:NBNO,I)t*XVAL(1:NBNO,IELEM,1:NBVAL)
C	  NBVAL,1         1,NBNO           NBNO,1,NBVAL
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBPT , NBNO , NBELM , NBVAL
	INTEGER ELEM ( NBPT )
	REAL*8 ALPHA ( NBNO , NBPT )
	REAL*8 XVAL ( NBNO , NBVAL , NBELM ) , YVAL ( NBPT , NBVAL )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER I , IELEM
	REAL*8 XPUN
C ======================================================================
C -
	XPUN = 1.0D0
C
C	Loop on target points
C	""""""""""""""""""""
	DO I = 1 , NBPT
	  IELEM = ELEM ( I )

C	  YVAL(I,1:NBVAL)=ALPHA(1:NBNO,I)t*XVAL(1:NBNO,1:NBVAL,IELEM) 
C	  1,NBVAL         1,NBNO           NBNO,NBVAL,1
	  CALL dgemm ( 'T' , 'N' , 1 , NBVAL , NBNO ,
     &                 XPUN , ALPHA(1,I) , NBNO ,
     &                 XVAL(1,1,IELEM)   , NBNO , 
     &                 XPUN , YVAL(I,1)  , NBPT )

	ENDDO
C -
	RETURN
	END
