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
C	Fortran90 version for dynamical memory allocation
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
	INTEGER IND , INO , JPTC
C	Absolute precision
	REAL* 8 XPRECA
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
C -
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

C	Sort points along first coordinate: gives permutation LPTC
C	""""""""""""""""""""""""""""""""""
	CALL dlapst ( 'I' , NBPT , XCOORC(1,1) , LPTC , KERR )
	IF (KERR .NE. 0) THEN
	  WRITE (*,*) 'NodesInMesh: FAILED TO SORT'
	  STOP 8
	ENDIF
CDD	do J = 1 , NBPT
CDD	  LPTC(J) = J
CDD	enddo
CDD	CALL IMPTEC ( 'NodesInMesh' , 2 , 'LPTC' , LPTC,1,NBPT )

C	Loop on elements
C	""""""""""""""""
	DO I = 1 , NBELM
	  CALL IMPEC ( 'NodesInMesh' , 3 , 'ELEMENT' , I )
C
C	  Absolute precision for the element: XPRECA
C	  ''''''''''''''''''''''''''''''''''
	  XPRECA = 0.0D0
	  DO K = 1 , IDIM
	    IND = (K-1)*2 + 1
	    XPRECA = XPRECA + (XBOX(I,IND+1)-XBOX(I,IND))
	  ENDDO
	  XPRECA = XPRECR * (XPRECA / FLOAT(IDIM))
	  CALL IMPDC ( 'NodesInMesh' , 3 , 'XPRECA' , XPRECA )
C
C	  Find potential inner target points (sorted along x): LPOINTS
C	  ''''''''''''''''''''''''''''''''''
CDD	  k = 3
CDD	      IND = (K-1)*2 + 1
CDD	  do j = 1,nbpt
CDD	    write(*,*) (XCOORC(LPTC(j),k),k=1,3)
CDD	  enddo
CDD	  write(*,*) XBOX(I,IND+1)+XPRECA
CDD	  write(*,*) XBOX(I,IND)-XPRECA
CDD	  stop 16
	  CALL IANUL ( LPOINTS , NBPT )
	  NBPTP = 0
	  J = 1
	  JPTC = LPTC ( J )
C	  Filter with first coordinate
	  DO WHILE ((XCOORC(JPTC,1).LT.(XBOX(I,2)+XPRECA)) .AND.
     &              (J .LE. NBPT))
C       am: on deplace la recuperation de JPTC au debut de la boucle
C           pour eviter le cas ou J est plus grand que la taille de LPTC 
	    JPTC = LPTC ( J )
	    LTEST = ELEM(JPTC) .EQ. 0
	    LTEST = LTEST .AND. (XCOORC(JPTC,1).GT.(XBOX(I,1)-XPRECA))
	    DO K = 2 , IDIM
	      IND = (K-1)*2 + 1
	      LTEST = LTEST .AND.
     &                (XCOORC(JPTC,K).GT.(XBOX(I,IND)-XPRECA))
	      LTEST = LTEST .AND. 
     &                (XCOORC(JPTC,K).LT.(XBOX(I,IND+1)+XPRECA))
	    ENDDO
C  A REMPLACER PAR
C LTEST =  DistToBox(IDIM,XCOORC(JPTC,1),NBPT,XBOX(I,1),NBELM) .GT. XPRECA
	    IF (LTEST) THEN
	      NBPTP = NBPTP + 1
	      LPOINTS(NBPTP) = J
	    ENDIF
	    J = J + 1
C       am: la ligne suivante a ete deplacee en haut car pour J=NBPT
C           au debut de la boucle, l'incrementation precedente donnerait
C           J = NBPT + 1 et la tentative recuperation de LPTC ( J )
C           donnerait une seg fault! 
C	    JPTC = LPTC ( J )
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
	    INO = MAIL(I,J)
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
	      ELEM(JPTC)  = I
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
