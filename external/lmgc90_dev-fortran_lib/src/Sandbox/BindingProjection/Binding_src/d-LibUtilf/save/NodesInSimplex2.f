C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE NodesInSimplex2 ( 
C       Inputs
     &                          NBNO    , IDIM   , NBPTP   ,
     &                          XCOOREL , XCOORPT ,
C       Outputs
     &                          ALPHAP  , ALPHAPC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 19 / 12 / 2007
C     MARCON Bertrand   LMGC SYSTEMES MULTICONTACTS  le 19 / 12 / 2007
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 02 / 08 / 2008
C       Case of a subspace where NBNO < IDIM+1
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
C	ALPHAPC(NBPTP)		REAL*8		For complementary space
C
C	For simplex, the weights are identical to the barycentric
C	coordinates
C
C	Calls LAPACK routine dgesv for linear solve with LU
C	Calls LAPACK routine dgesvd for svd, dgemv
C
C	THIS ROUTINE SHOULD OVERLOAD NodesInSimplex
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBNO , IDIM , NBPTP
	REAL*8 XCOOREL ( NBNO , IDIM ) , XCOORPT ( NBPTP , IDIM )
	REAL*8 ALPHAP ( NBNO , NBPTP ) , ALPHAPC ( NBPTP )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER I , J , K  , LENGTH , NBC , NBD , I2
	INTEGER IPUN
	REAL*8 XPUN , XZERO
C	Left hand side
	INTEGER IDIMAX
	PARAMETER ( IDIMAX = 3 )
	REAL*8 X ( IDIMAX , IDIMAX )
	REAL*8 S ( IDIMAX )
	REAL*8 U ( IDIMAX , IDIMAX ) , VT ( IDIMAX , IDIMAX )
	REAL*8 ALPHAP2 ( IDIMAX ) , ALPHAP3 ( IDIMAX )
C	Pivoting information for Lapack
	INTEGER IPIV ( IDIMAX )
C	Working space for Lapack
	INTEGER LWORK
	PARAMETER ( LWORK = 10 * IDIMAX )
	REAL*8 WORK ( LWORK , LWORK )
C	Error indicator
	INTEGER KERR
C ======================================================================
C -
	CALL ENTREC ( 'NodesInSimplex2' , 1 )
C
	IF (NBNO .GT. (IDIM + 1)) THEN
	  WRITE (*,*) 'NodesInSimplex2: NOT A SIMPLEX'
	  STOP 8
	ENDIF
	CALL IMPTDC ('NodesInSimplex2',3,'XCOOREL',XCOOREL,NBNO,IDIM)
	CALL IMPTDC ('NodesInSimplex2',3,'XCOORPT',XCOORPT,NBPTP,IDIM)
C
	XPUN  = 1.0D0
	XZERO = 0.0D0
	IPUN  = 1
C
	LENGTH = NBNO * NBPTP
	CALL ZDANUL ( ALPHAP , LENGTH )
	CALL ZDANUL ( ALPHAPC , NBPTP )
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
	CALL IMPTDC ( 'NodesInSimplex2' , 4 , 'X' , X , IDIM , IDIM )
C
	IF (NBNO .EQ. (IDIM + 1)) THEN
C
C	  A true simplex: direct resolution ALPHAP <- X^{-1} * ALPHAP
C	  """"""""""""""""""""""""""""""""""
C	  Right hand side
	  DO I = 1 , NBPTP
	    DO K = 1 , IDIM
C	      From 2nd position to preserve first barycentric coordinate
	      ALPHAP(K+1,I) = XCOORPT(I,K) - XCOOREL(1,K)
	    ENDDO
	  ENDDO
C
C	  Solve
	  CALL dgesv ( IDIM , NBPTP , X , IDIMAX , IPIV , ALPHAP(2,1) , NBNO ,
     &                 KERR )
	  IF (KERR .NE. 0) THEN
	    CALL IMPTDC ( 'NodesInSimplex2' , 0 , 'ALPHAP' , ALPHAP ,
     &                    NBNO , NBPTP )
	    CALL IMPEC ( 'NodesInSimplex2' , 0 , 'KERR' , KERR )
	    CALL MESFAT ( 'NodesInSimplex2' , 'FAILED TO SOLVE' )
	  ENDIF
C
	ELSEIF (NBNO .LT. (IDIM + 1)) THEN
C
C	  NBNO < IDIM + 1: a subspace case requires svd
C	  """"""""""""""""""""""""""""""""""""""""""""""
	  NBC = NBNO - 1
	  NBD = IDIM + 1
	  LENGTH = IDIMAX * IDIMAX
	  CALL ZDANUL ( U , LENGTH )
	  CALL ZDANUL ( VT , LENGTH )
	  CALL ZDANUL ( S , IDIMAX )
	  CALL dgesvd ( 'A'  , 'A'   , IDIM   , NBC , X      , IDIMAX ,
     &                  S    ,  U    , IDIMAX , VT  , IDIMAX , 
     &                  WORK , LWORK , KERR   )
C	  REM : ON PEUT AUSSI ECRASER VT DANS X
	  CALL IMPTDC ( 'NodesInSimplex2' , 3 , 'U' , U , IDIMAX , IDIMAX )
	  CALL IMPTDC ( 'NodesInSimplex2' , 3 , 'S' , S , 1 , IDIMAX )
	  CALL IMPTDC ( 'NodesInSimplex2' , 3 , 'VT' , VT , IDIMAX , IDIMAX )
	  IF (KERR .NE. 0) THEN
	    CALL IMPEC ( 'NodesInSimplex2' , 0 , 'KERR' , KERR )
	    CALL MESFAT ( 'NodesInSimplex2' , 'FAILED IN SVD' )
	  ENDIF
	  IF (S(NBC) .LT. 1.D-8) THEN
	    CALL MESFAT ( 'NodesInSimplex2' , 'NULL SINGULAR VALUE' )
	  ENDIF
C
C	  Complementary space (with maximal dimension of the element)
	  DO I = NBC + 1, IDIM
	    S ( I )  = S ( 1 )
	    VT ( I , I ) = XPUN
	  ENDDO
	  CALL IMPTDC ( 'NodesInSimplex2' , 3 , 'S' , S , 1 , IDIMAX )
	  CALL IMPTDC ( 'NodesInSimplex2' , 3 , 'VT' , VT , IDIMAX , IDIMAX )
C
C	  Loop on RHS for static storage reasons
	  DO I2 = 1 , NBPTP
CDD	    CALL IMPEC ( 'NodesInSimplex2' , 3 , 'Point' , I2 )
CDD	    write(*,*) '  XCOOR ' , (XCOORPT(I2,k),k=1,idim)
C
	    CALL ZDANUL ( ALPHAP2 , IDIMAX )
	    CALL ZDANUL ( ALPHAP3 , IDIMAX )
	    DO K = 1 , IDIM
	      ALPHAP2(K) = XCOORPT(I2,K) - XCOOREL(1,K)
	    ENDDO
	    CALL IMPTDC ( 'NodesInSimplex2',3,'ALPHAP2',ALPHAP2,1,IDIMAX )
C
C	    Solve: U*S*VT * ALPHAP2 = ALPHAP2
CDD	    CALL dgemm ( 'T'   , 'N'    , IDIM         , NBPTP , IDIM , XPUN ,
CDD     &                 U     , IDIMAX , ALPHAP(2,1) , NBD   , 
CDD     &                 XZERO , ALPHAP(2,1) , NBD )
C	    ALPHAP3 = UT * ALPHAP2
	    CALL dgemv ( 'T'     , IDIM , IDIM  , XPUN    , U    , IDIMAX , 
     &                   ALPHAP2 , IPUN , XZERO , ALPHAP3 , IPUN )
	    CALL IMPTDC ( 'NodesInSimplex2' , 3 , 'U' , U , IDIMAX , IDIM )
	    CALL IMPTDC ( 'NodesInSimplex2',3,'ALPHAP3',ALPHAP3,1,IDIMAX )
C	    ALPHAP3 = S^-1 * ALPHAP3
	    DO I = 1 , IDIM
	      ALPHAP3(I) = (1.0D0 / S(I)) * ALPHAP3(I)
	    ENDDO
CDD	    CALL dgemm ( 'T'   , 'N'    , IDIM         , NBPTP , IDIM , XPUN ,
CDD     &                 VT    , IDIMAX , ALPHAP(2,1) , NBD   , 
CDD     &                 XZERO , ALPHAP(2,1) , NBD )
C	    ALPHAP2 = V * ALPHAP3
	    CALL dgemv ( 'T'     , IDIM , IDIM  , XPUN    , VT   , IDIMAX ,
     &                   ALPHAP3 , IPUN , XZERO , ALPHAP2 , IPUN )
	    CALL IMPTDC ( 'NodesInSimplex2',3,'ALPHAP2',ALPHAP2,1,IDIMAX )
C
C	    On remet ALPHAP2(1:NBNO-1) dans ALPHAP(2:NBNO,I2)
	    DO I = 1 , NBNO - 1
	      ALPHAP(I+1,I2) = ALPHAP2(I)
	    ENDDO
C	    On teste la proximite dans le complementaire avec ALPHAP2(NBNO:IDIM)
	    I = NBNO
	    ALPHAPC(I2) = DABS(ALPHAP2(I))
	    DO I = NBNO + 1 , IDIM
	      IF (DABS(ALPHAP2(I)) .GT. ALPHAPC(I2)) THEN
	        ALPHAPC(I2) = DABS(ALPHAP2(I))
	      ENDIF
	    ENDDO
CDD	    CALL IMPTDC ( 'NodesInSimplex2',3,'ALPHAP',ALPHAP(1,I2),NBNO,1 )
CDD	    CALL IMPDC ( 'NodesInSimplex2',3,'ALPHAPC',ALPHAPC(I2) )
	  ENDDO
C
	ELSE
	  CALL MESFAT ( 'NodesInSimplex2' , 'NOT A SIMPLEX - BIS' )
	ENDIF
C
C	Final information (first barycentric coordinate)
C	""""""""""""""""
	DO I = 1 , NBPTP
	  ALPHAP(1,I) = XPUN
	  DO J = 2 , NBNO
	    ALPHAP(1,I) = ALPHAP(1,I) - ALPHAP(J,I)
	  ENDDO
	ENDDO
C
	CALL IMPTDC ( 'NodesInSimplex2',3,'ALPHAP',ALPHAP,IDIM+1,NBPTP)
CC	write (*,*) (alphap(i,4),i=1,nbno)
CC	write (*,*) alphapc(4)
CC	stop 7
C
	CALL SORTIC ( 'NodesInSimplex2' , 1 )
C -
	RETURN
	END
