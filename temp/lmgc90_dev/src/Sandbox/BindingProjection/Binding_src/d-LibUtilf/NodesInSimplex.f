C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE NodesInSimplex ( 
C       Inputs
     &                          NBNO    , IDIM   , NBPTP   ,
     &                          XCOOREL , XCOORPT ,
C       Outputs
     &                          ALPHAP )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 19 / 12 / 2007
C     MARCON Bertrand   LMGC SYSTEMES MULTICONTACTS  le 19 / 12 / 2007
C
C	Gives weights correspondance to project a field
C	defined on a simplex to target points whose coordinates are
C	XCOORPT
C
C     Inputs
C	NBNO			INTEGER		Nb of nodes per element
C	IDIM			INTEGER		Spatial dimension
C	NBPTP			INTEGER		Nb of target points
C	XCOOREL(NBNO,IDIM)	REAL*8		Element node coordinates
C	XCOORPT(NBPTP,IDIM)	REAL*8		Target point coordinates
C     Outputs
C	ALPHAP(NBNO,NBPTP)	REAL*8		Corresponding weights
C
C	For simplex, the weights are identical to the barycentric
C	coordinates
C
C	Calls LAPACK routine dgesv for linear solve with LU
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBNO , IDIM , NBPTP
	REAL*8 XCOOREL ( NBNO , IDIM ) , XCOORPT ( NBPTP , IDIM )
	REAL*8 ALPHAP ( NBNO , NBPTP )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER I , J , K  , LENGTH
C	Left hand side
	INTEGER IDIMAX
	PARAMETER ( IDIMAX = 3 )
	REAL*8 X ( IDIMAX , IDIMAX )
C	Pivoting information for Lapack
	INTEGER IPIV ( IDIMAX )
C	Error indicator
	INTEGER KERR
C ======================================================================
C -
	IF (NBNO .NE. (IDIM + 1)) THEN
	  WRITE (*,*) 'NodesInSimplex: NOT A SIMPLEX'
	  STOP 8
	ENDIF
	CALL IMPTDC ('NodesInSimplex',3,'XCOOREL',XCOOREL,NBNO,IDIM)
CDD	CALL IMPTDC ('NodesInSimplex',3,'XCOORPT',XCOORPT,NBPTP,IDIM)
C
C	Left hand side
C	""""""""""""""
	LENGTH = IDIMAX * IDIMAX
	CALL ZDANUL ( X , LENGTH )
	DO I = 2 , NBNO
	  DO K = 1 , IDIM
	    X(K,I-1) = XCOOREL(I,K) - XCOOREL(1,K) 
	  ENDDO
	ENDDO
CDD	CALL IMPTDC ( 'NodesInSimplex' , 1 , 'X' , X , IDIMAX , IDIMAX )
C
C	Right hand side
C	""""""""""""""
	DO I = 1 , NBPTP
	  DO K = 1 , IDIM
C	    From 2nd position to preserve first barycentric coordinate
	    ALPHAP(K+1,I) = XCOORPT(I,K) - XCOOREL(1,K)
	  ENDDO
	ENDDO
C
C	Resolution
C	""""""""""
	CALL IMPTDC ( 'NodesInSimplex' , 2 , 'X' , X , IDIM , IDIM )
	CALL dgesv ( IDIM , NBPTP , X , IDIMAX , IPIV , ALPHAP(2,1) ,
     &               NBNO , KERR )
	IF (KERR .NE. 0) THEN
	  CALL IMPTDC ( 'NodesInSimplex' , 0 , 'ALPHAP' , ALPHAP ,
     &                  NBNO , NBPTP )
	  CALL IMPEC ( 'NodesInSimplex' , 0 , 'KERR' , KERR )
	  CALL MESFAT ( 'NodesInSimplex' , 'FAILED TO SOLVE' )
	ENDIF
C
C	Final information (first barycentric coordinate)
C	""""""""""""""""
	DO I = 1 , NBPTP
	  ALPHAP(1,I) = 1.0D0
	  DO J = 2 , NBNO
	    ALPHAP(1,I) = ALPHAP(1,I) - ALPHAP(J,I)
	  ENDDO
	ENDDO
C -
	RETURN
	END
