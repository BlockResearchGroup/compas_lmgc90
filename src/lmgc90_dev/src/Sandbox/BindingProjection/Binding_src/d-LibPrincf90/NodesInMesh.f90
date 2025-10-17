C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE NodesInMesh ( 
C       Inputs
     &                          NBNOT , IDIM , NBELM , NBNO   , NBPT ,
     &                          XCOOR , MAIL , CTYPE , XPRECR , XCOORC ,
     &                          OPTIO , 
C       Outputs
     &                          ELEM  , ALPHA )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 18 / 12 / 2007
C     MARCON Bertand    LMGC SYSTEMES MULTICONTACTS  le 18 / 12 / 2007
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 26 / 12 / 2007
C       Fortran90 version for dynamical memory allocation
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 06 / 02 / 2009
C       Fix a bug found by A. Martin
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 12 / 07 / 2009
C       Sort element boxes to reduce potential search cost
C
C	Gives weights and element correspondances to project a field
C	defined on mesh MAIL to target points whose coordinates are
C	XCOORC
C
C     Inputs
C	NBNOT			INTEGER		Total nb of nodes
C	IDIM			INTEGER		Spatial dimension
C	NBELM			INTEGER		Nb of elements
C	NBNO			INTEGER		Nb of nodes per element
C	NBPT			INTEGER		Nb of target points
C	XCOOR(NBNOT,IDIM)	REAL*8		Node coordinates
C	MAIL(NBELM,NBNO)	INTEGER		Connectivity of the mesh
C	CTYPE			CHARACTER*8	Type of element
C	XPRECR			REAL*8		Relative precision
C	XCOORC(NBPT,IDIM)	REAL*8		Target point coordinates
C	OPTIO			CHARACTER*8	Option
C     Outputs
C	ELEM(NBPT)		INTEGER		Corresponding element
C	ALPHA(NBNO,NBPT)	REAL*8		Corresponding weights
C
C	Warning: ELEM has to be initialized to 0 before calling the
C	subroutine, otherwise, the corresponding nodes are supposed
C	to be already found.
C
C	Calls ScaLAPACK sort routine dlapst.
C	Calls Blas copy routine dcopy.
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBNOT , IDIM , NBELM , NBNO , NBPT
	INTEGER MAIL ( NBELM , NBNO )
	REAL*8 XCOOR ( NBNOT , IDIM ) , XCOORC ( NBPT , IDIM )
	REAL*8 XPRECR
	CHARACTER*8 OPTIO , CTYPE
	INTEGER ELEM ( NBPT )
	REAL*8 ALPHA ( NBNO , NBPT )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER I , J , K
	INTEGER IND , INO , JPTC , IELC , JSTART
C	Absolute precision
	REAL*8 XPRECA
C	Extreme values of box first coordinates
	REAL*8 XMIN1 , XMAX1
C	Number of potential inner target points
	INTEGER NBPTP
C	Logical test
	LOGICAL LTEST
C	Error Indicator
	INTEGER KERR
! *
! *	Working space (fortran90) with known dimensions: automatic 
! *	""""""""""""""""""""""""
!	Coordinate box around elements
	REAL*8, DIMENSION(NBELM,2*IDIM) :: XBOX
!	List of potential points for the current element
	INTEGER, DIMENSION(NBPT) :: LPOINTS
!	Sorted list of points
	INTEGER, DIMENSION(NBPT) :: LPTC
!	Sorted list of element boxes
	INTEGER, DIMENSION(NBELM) :: LELC
!	Coordinates of element nodes
	REAL*8, DIMENSION(NBNO,IDIM) :: XCOOREL
!
! *	Working space (fortran90) with unknown dimensions: deffered-shape-array
! *	""""""""""""""""""""""""
!	Coordinates of selected points (NBPTP,IDIM)
	REAL*8, DIMENSION(:,:), ALLOCATABLE :: XCOORPT
!	Corresponding weights (NBNO,NBPTP)
	REAL*8, DIMENSION(:,:), ALLOCATABLE :: ALPHAP
!	Max weight in the complementary subspace (NBPTP)
	REAL*8, DIMENSION(:), ALLOCATABLE :: ALPHAPC
C ======================================================================

	CALL ENTREC ( 'NodesInMesh.f90' , 1 )
CDD	CALL IMPTDC ( 'NodesInMesh' , 2 , 'XCOORC' , XCOORC , NBPT , IDIM )
CDD	CALL IMPTEC ( 'NodesInMesh' , 2 , 'MAIL' , MAIL , NBELM , NBNO )

C	Box around elements
C	""""""""""""""""""
	DO I = 1 , NBELM
	  J = 1
	    DO K = 1 , IDIM
	      IND = (K-1)*2 + 1
	      INO = MAIL(I,J)
	      XBOX(I,IND)   = XCOOR(INO,K)
	      XBOX(I,IND+1) = XCOOR(INO,K)
	    ENDDO
	ENDDO
C
	DO I = 1 , NBELM
	  DO J = 1 , NBNO
	    DO K = 1 , IDIM
	      IND = (K-1)*2 + 1
	      INO = MAIL(I,J)
	      XBOX(I,IND)   = MIN(XBOX(I,IND),XCOOR(INO,K))
	      XBOX(I,IND+1) = MAX(XBOX(I,IND+1),XCOOR(INO,K))
	    ENDDO
	  ENDDO
	ENDDO

CDD	CALL IMPTDC ( 'NodesInMesh' , 2 , 'XBOX' , XBOX,NBELM,2*IDIM )

C	Sort boxes along first min coordinate: gives permutation LELC
C	""""""""""""""""""""""""""""""""""""""
	IF (NBELM .EQ. 1) THEN
	  LELC ( 1 ) = 1
	ELSE
	  CALL dlapst ( 'I' , NBELM , XBOX(1,1) , LELC , KERR )
	  IF (KERR .NE. 0) THEN
	    WRITE (*,*) 'NodesInMesh: FAILED TO SORT BOXES'
	    STOP 8
	  ENDIF
	ENDIF
CDD	CALL IMPTEC ( 'NodesInMesh' , 2 , 'LELC' , LELC,1,NBELM )

C	Sort points along first coordinate: gives permutation LPTC
C	""""""""""""""""""""""""""""""""""
	IF (NBPT .EQ. 1) THEN
	  LPTC ( 1 ) = 1
	ELSE
	  CALL dlapst ( 'I' , NBPT , XCOORC(1,1) , LPTC , KERR )
	  IF (KERR .NE. 0) THEN
	    WRITE (*,*) 'NodesInMesh: FAILED TO SORT'
	    STOP 8
	  ENDIF
	ENDIF
CDD	do J = 1 , NBPT
CDD	  LPTC(J) = J
CDD	enddo

CDD	CALL IMPTEC ( 'NodesInMesh' , 2 , 'LPTC' , LPTC,1,NBPT )

C	Loop on elements
C	""""""""""""""""
	JSTART = 1
	DO I = 1 , NBELM
	  IELC = LELC ( I )
	  CALL IMPEC ( 'NodesInMesh' , 3 , 'ELEMENT' , IELC )
C
C	  Absolute precision for the element: XPRECA
C	  ''''''''''''''''''''''''''''''''''
	  XPRECA = 0.0D0
	  DO K = 1 , IDIM
	    IND = (K-1)*2 + 1
	    XPRECA = XPRECA + (XBOX(IELC,IND+1)-XBOX(IELC,IND))
	  ENDDO
	  XPRECA = XPRECR * (XPRECA / FLOAT(IDIM))
	  CALL IMPDC ( 'NodesInMesh' , 3 , 'XPRECA' , XPRECA )
	  XMIN1 = XBOX(IELC,1) - XPRECA
	  XMAX1 = XBOX(IELC,2) + XPRECA
C
C	  Find potential inner target points (sorted along x): LPOINTS
C	  ''''''''''''''''''''''''''''''''''
	  CALL IANUL ( LPOINTS , NBPT )
	  NBPTP = 0
C -
C	  Filter with first coordinates: find new starting potential point
	  DO WHILE (JSTART .LE. NBPT)
	    JPTC = LPTC ( JSTART )
	    IF (XCOORC(JPTC,1) .GT. XMIN1) EXIT
	    JSTART = JSTART + 1
	  ENDDO
C -
C	  Filter with first point coordinate
	  J = JSTART
	  DO WHILE (J .LE. NBPT)
	    JPTC = LPTC ( J )
	    IF (XCOORC(JPTC,1) .GE. XMAX1) EXIT
C -
	    LTEST = ELEM(JPTC) .EQ. 0
	    IF (LTEST) THEN
CDD No more useful      LTEST = LTEST .AND. (XCOORC(JPTC,1) .GT. XMIN1)
	      DO K = 2 , IDIM
	        IND = (K-1)*2 + 1
	        LTEST = LTEST .AND.
     &                  (XCOORC(JPTC,K).GT.(XBOX(IELC,IND)-XPRECA))
	        LTEST = LTEST .AND. 
     &                  (XCOORC(JPTC,K).LT.(XBOX(IELC,IND+1)+XPRECA))
	      ENDDO
C  A REMPLACER PAR
C LTEST =  DistToBox(IDIM,XCOORC(JPTC,1),NBPT,XBOX(IELC,1),NBELM) .GT. XPRECA
	      IF (LTEST) THEN
	        NBPTP = NBPTP + 1
	        LPOINTS(NBPTP) = J
	      ENDIF
	    ENDIF
C -
	    J = J + 1
CBUG	    JPTC = LPTC ( J )
	  ENDDO
	  CALL IMPTEC ('NodesInMesh',2,'LPOINTS',LPOINTS,1,NBPTP)

	  IF (NBPTP .GT. 0) THEN
!
!	  Dynamical allocations
!	  Coordinates of selected points
	  ALLOCATE ( XCOORPT(NBPTP,IDIM) )
!	  Corresponding weights
	  ALLOCATE ( ALPHAP(NBNO,NBPTP) )
!	  Max weight in the complementary subspace
	  ALLOCATE ( ALPHAPC(NBPTP) )
C
C	  Find weights for this element: ALPHAP
C	  ''''''''''''''''''''''''''''''
C	  Coordinates of element nodes
	  DO J = 1 , NBNO
	    INO = MAIL(IELC,J)
	    DO K = 1 , IDIM
	      IND = (K - 1)*NBNO + J
	      XCOOREL(J,K) = XCOOR(INO,K)
	    ENDDO
	  ENDDO
C	  Coordinates of potential target points
	  DO J = 1 , NBPTP
CDD	    call impec('NInMsh',2,'J',J)
	    INO = LPOINTS(J)
CDD	    call impec('NInMsh',2,'INO',INO)
	    JPTC = LPTC(INO)
CDD	    call impec('NInMsh',2,'JPTC',JPTC)
	    DO K = 1 , IDIM
CDD	      call impec('NInMsh',2,'K',K)
!		     IND = (K-1)*NBPTP+J
!	      XCOORPT(IND) = XCOORC(JPTC,K)
	      XCOORPT(J,K) = XCOORC(JPTC,K)
	    ENDDO
	  ENDDO

	  CALL NodesInElement ( 
C       Inputs
     &                          NBNO    , IDIM  , NBPTP  ,
     &                          XCOOREL , CTYPE , XPRECR , XCOORPT ,
     &                          OPTIO   , 
C       Outputs
     &                          ALPHAP  , ALPHAPC )

	CALL IMPTDC ('NodesInMesh',2,'ALPHAP',ALPHAP,NBNO,NBPTP)
	CALL IMPTDC ('NodesInMesh',2,'ALPHAPC',ALPHAPC,1,NBPTP)

	  DO J = 1 , NBPTP
	    LTEST = .TRUE.
	    DO K = 1 , NBNO
	      LTEST = LTEST .AND. (ALPHAP(K,J) .GT. -XPRECR)
	      LTEST = LTEST .AND. (ALPHAP(K,J) .LT. (1.0D0+XPRECR))
	    ENDDO
C	    On devrait pouvoir remplacer ca par
C	    LTEST = DistToSimplex(IDIM,ALPHAP(1,J)) .GT. XPRECR
C	    For complementary space, should be 0
	    IF (NBNO .LT. IDIM+1) THEN
	      LTEST = LTEST .AND. (ALPHAPC(J) .GT. -XPRECR)
	      LTEST = LTEST .AND. (ALPHAPC(J) .LT. XPRECR)
	    ENDIF
C
	    IF (LTEST) THEN
C	      Inner point: store values
	      INO = LPOINTS(J)
	      JPTC = LPTC(INO)
	      CALL dcopy ( NBNO , ALPHAP(1,J) , 1 , ALPHA(1,JPTC) , 1 )
	      ELEM(JPTC)  = IELC
	    ENDIF
	  ENDDO
!
!	  Dynamical deallocations
	  DEALLOCATE ( XCOORPT )
	  DEALLOCATE ( ALPHAP )
	  DEALLOCATE ( ALPHAPC )

	  ENDIF

	ENDDO

	CALL SORTIC ( 'NodesInMesh.f90' , 1 )
C -
	RETURN
	END
