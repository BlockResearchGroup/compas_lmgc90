C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ElementToSimplexes ( 
C           Inputs
     &                          NBNO    , IDIM    ,
     &                          XCOOREL , CTYPE   , OPTIO1 ,
C           Outputs
     &                          XCOORES , ALPHAS  , NODELIST )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 25 / 12 / 2007
C
C	Split an element into simplexes
C
C     Inputs
C	NBNO			INTEGER		Nb of nodes per element
C	IDIM			INTEGER		Spatial dimension
C	XCOOREL(NBNO,IDIM)	REAL*8		Element node coordinates
C	CTYPE			CHARACTER*8	Type of element
C	OPTIO1			CHARACTER*8	Option
C     Outputs
C	XCOORES(IDIM+1,IDIM,NBS1) REAL*8	Simplexes nodes coord.
C	ALPHAS(NBNO,IDIM+1,NBS1)  REAL*8	Weights at simplex nodes
C	NODELIST(NBS1,NBNO1-1)    INTEGER	Permutation of nodes
C
C	Option OPTIO1 can be
C	'CENTROID' for which the first simplex node is the element
C	           centroid
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBNO , IDIM
	REAL*8 XCOOREL ( NBNO , IDIM )
	CHARACTER*8 OPTIO1 , CTYPE
	REAL*8 XCOORES(IDIM+1,IDIM,*)
	REAL*8 ALPHAS(NBNO,IDIM+1,*)
	INTEGER NODELIST(*)
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
C	Number of sub-simplexes
	INTEGER NBS1
C	Reference coordinates of nodes
	INTEGER MAXNOD
	PARAMETER ( MAXNOD = 20 )
	REAL*8 XREFN ( MAXNOD*3 )
C	Reference and real coordinates of centroid
	REAL*8 XREFG ( 3 ) , XREALG ( 3 )
C	Shape function values
	REAL*8 XSHAPE ( MAXNOD )
C	Dummy
	INTEGER INO , I , K , IS1 , IND , LENGTH , NBNO1 , INO1
	REAL*8 XCOEF
	INTEGER IPUN
	REAL*8 XPUN , XZERO
C ======================================================================
C -
	CALL ElementToSimplexesNB ( 
C           Inputs
     &                          NBNO    , IDIM    ,
     &                          XCOOREL , CTYPE   , OPTIO1 ,
C           Outputs
     &                          NBS1 )
	IF (NBS1 .EQ. 0) THEN
	  CALL MESFAT ('ElementToSimplexes','Element Not implemented')
	ENDIF
C
C	Security test
	IF (NBNO .GT. MAXNOD) THEN
	  CALL IMPEC ('ElementToSimplexes',0,'NBNO',NBNO)
	  CALL MESFAT ('ElementToSimplexes',
     &                 'Not enough static memory for NBNO')
	ENDIF

	IPUN = 1
	XPUN = 1.0D0
	XZERO = 0.0D0
C
C	Initialization
	NBNO1 = IDIM + 1
C	ALPHAS(NBNO,IDIM+1=NBNO1,NBS1)
	LENGTH = NBNO * NBNO1 * NBS1
	CALL ZDANUL ( ALPHAS , LENGTH )
C	XCOORES(IDIM+1=NBNO1,IDIM,NBS1)
	LENGTH = NBNO1 * IDIM * NBS1
	CALL ZDANUL ( XCOORES , LENGTH )

	IF (OPTIO1(1:8) .EQ. 'CENTROID') THEN
C
C	  Reference coordinates of nodes
	  CALL NodesRefCoor (
C           Inputs
     &                        CTYPE , NBNO , IDIM , 
C           Outputs
     &                        XREFN )
CDD	  CALL IMPTDC ( 'ElementToSimplexes',2,'XREFN',XREFN,NBNO,IDIM ) 
C
C	  Reference coordinates of centroid
	  CALL ZDANUL ( XREFG , IDIM )
	  DO INO = 1 , NBNO
	    DO K = 1 , IDIM
	      IND = (K-1)*NBNO + INO
	      XREFG ( K ) = XREFG ( K ) + XREFN ( IND )
	    ENDDO
	  ENDDO
	  XCOEF = 1. / FLOAT(NBNO)
	  CALL dscal ( IDIM , XCOEF , XREFG , IPUN )
CDD	  CALL IMPTDC ( 'ElementToSimplexes',2,'XREFG',XREFG,1,IDIM ) 
C
C	  Values of shape functions at centroid
	  CALL ShapeFunctionValues (
C           Inputs
     &        CTYPE , NBNO , IDIM , 1 , XREFG ,
C           Outputs
     &        XSHAPE )
CDD	  CALL IMPTDC ( 'ElementToSimplexes',2,'XSHAPE',XSHAPE,1,NBNO ) 
C
C	  Real coordinates of centroid (isoparametric case)
C	  XREALG(1,IDIM) = XSHAPE(1,NBNO)*XCOOREL(NBNO,IDIM)
C	  XREALG(IDIM,1) = XCOOREL^T(IDIM,NBNO)*XSHAPE(NBNO,1)
	  CALL dgemv ('T'    , NBNO , IDIM  , XPUN   , XCOOREL , NBNO ,
     &                XSHAPE , IPUN , XZERO , XREALG , IPUN )
CDD	  CALL IMPTDC ( 'ElementToSimplexes',2,'XREALG',XREALG,1,IDIM ) 
C
C	  Switch depending of the element: find node permutation
C	  NODELIST(NBS1,NBNO1-1)
	  IF ((CTYPE(1:4) .EQ. 'TRI3') .AND. (IDIM .EQ. 2) .AND.
     &        (NBNO .EQ. 3)) THEN
C	    Massive element TRI3 (already a simplex...)
	    NODELIST(1) = 1
	    NODELIST(2) = 2
	    NODELIST(3) = 2
	    NODELIST(4) = 3
	    NODELIST(5) = 3
	    NODELIST(6) = 1

	  ELSEIF ((CTYPE(1:4) .EQ. 'TET4') .AND. (IDIM .EQ. 3) .AND.
     &        (NBNO .EQ. 4)) THEN
C	    Massive element TET4 (already a simplex...)
	    NODELIST( 1) = 1
	    NODELIST( 2) = 2
	    NODELIST( 3) = 3
	    NODELIST( 4) = 2
	    NODELIST( 5) = 3
	    NODELIST( 6) = 4
	    NODELIST( 7) = 3
	    NODELIST( 8) = 4
	    NODELIST( 9) = 1
	    NODELIST(10) = 4
	    NODELIST(11) = 1
	    NODELIST(12) = 2

	  ELSEIF ((CTYPE(1:4) .EQ. 'QUA4') .AND. (IDIM .EQ. 2) .AND.
     &        (NBNO .EQ. 4)) THEN
C	    Massive element QUA4
	    NODELIST(1) = 1
	    NODELIST(2) = 2
	    NODELIST(3) = 2
	    NODELIST(4) = 3
	    NODELIST(5) = 3
	    NODELIST(6) = 4
	    NODELIST(7) = 4
	    NODELIST(8) = 1

	  ELSE
	    CALL IMPCC  ('ElementToSimplexes',CTYPE)
	    CALL MESFAT ('ElementToSimplexes','ELEMENT NOT IMPLEMENTED')
	  ENDIF
C
C	  XCOORES(NBNO1,IDIM,NBS1) REAL*8	Simplexes nodes coord.
C	  ALPHAS(NBNO,NBNO1,NBS1)  REAL*8	Weights at simplex nodes
	  DO IS1 = 1 , NBS1
C	    First node is the centroid
	    INO1 = 1
	    CALL dcopy (IDIM,XREALG,IPUN,XCOORES(INO1,1,IS1),NBNO1)
	    CALL dcopy (NBNO,XSHAPE,IPUN,ALPHAS(1,INO1,IS1),IPUN)
C	    Remaining nodes are permutations of element nodes
	    DO INO1 = 2 , NBNO1
	      IND = (IS1-1)*(NBNO1-1) + (INO1-1)
	      I = NODELIST(IND)
	      CALL dcopy (IDIM , XCOOREL(I,1)        , NBNO , 
     &                           XCOORES(INO1,1,IS1) , NBNO1 )
	      ALPHAS(I,INO1,IS1) = 1.0D0
	    ENDDO
CDD	    CALL IMPEC ( 'ElementToSimplexes' , 2 , 'IS1' , IS1 )
CDD	    CALL IMPTDC ( 'ElementToSimplexes' , 2 , 'XCOORES' , 
CDD     &                    XCOORES(1,1,IS1) , NBNO1 , IDIM ) 
	  ENDDO

	ELSE
	  CALL IMPCC  ('ElementToSimplexes',OPTIO1)
	  CALL MESFAT ('ElementToSimplexes','OPTION NOT IMPLEMENTED')
	ENDIF
C -
	RETURN
	END
