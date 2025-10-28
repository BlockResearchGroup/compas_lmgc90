C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ChpoToChamno ( 
C       Inputs
     &                          NBPT , NBNO , NBELM , NBVAL ,
     &                          YVAL , LPT  , MAIL  ,
C       Outputs
     &                          XVAL  )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 23 / 12 / 2007
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 27 / 12 / 2007
C       Fortran90 version to build inverse list for performance
C
C	Change field storage: from points to elements (at nodes)
C
C     Inputs
C	NBPT			INTEGER		Nb of field points
C	NBNO			INTEGER		Nb of nodes per element
C	NBELM			INTEGER		Nb of elements
C	NBVAL			INTEGER		Nb of values per point
C	YVAL(NBPT,NBVAL)	REAL*8		Field values at points
C	LPT(NBPT)		INTEGER		List of field points
C       MAIL(NBELM,NBNO)        INTEGER         Connectivity of the mesh
C     Outputs
C	XVAL(NBNO,NBVAL,NBELM)	REAL*8		Values at element nodes
C
C	Calls blas routine dcopy
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBPT , NBNO , NBELM , NBVAL
	INTEGER LPT ( NBPT )
        INTEGER MAIL ( NBELM , NBNO )
	REAL*8 XVAL ( NBNO , NBVAL , NBELM ) , YVAL ( NBPT , NBVAL )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER IEL , INO , IPT , NO1 , I , NODMAX1
	INTEGER LENGTH
!
! *     Working space (fortran90) with unknown dimensions: deffered-shape-array
! *     """"""""""""""""""""""""
!       Inverse list of nodes
        REAL*8, DIMENSION(:), ALLOCATABLE :: LPTINV
C ======================================================================
C -
C	Initialization of XVAL
C	""""""""""""""""""""""
	LENGTH = NBNO * NBVAL * NBELM
	CALL ZDANUL ( XVAL , LENGTH )
C
C	Inverse list of nodes to prevent numerous searches
C	""""""""""""""""""""""""""""""""""""""""""""""""""
	NODMAX1 = 0
	DO I = 1 , NBPT
	  NODMAX1 = MAX ( NODMAX1 , LPT(I) )
	ENDDO
!
!       Dynamical allocation
!       Inverse list of nodes
        ALLOCATE ( LPTINV(NODMAX1) )
	DO I = 1 , NBPT
	  NO1 = LPT(I)
	  LPTINV(NO1) = I
	ENDDO
C
C	Loop on elements
C	""""""""""""""""
	DO IEL = 1 , NBELM
C
C	  Loop on element nodes
C	  """"""""""""""""""""
	  DO INO = 1 , NBNO
C	    Search for MAIL(IEL,INO) in LPT -> IPT
	    NO1 = MAIL ( IEL , INO )
CDD	    CALL PLACE2 ( LPT , NBPT , IPT , NO1 )
	    IPT = LPTINV ( NO1 )
	    IF (IPT .NE. 0) THEN
C	      XVAL(INO,1:NBVAL,IEL) <- YVAL(IPT,1:NBVAL)
	      CALL dcopy ( NBVAL , YVAL(IPT,1) , NBPT ,
     &                     XVAL(INO,1,IEL) , NBNO )
	    ENDIF
	  ENDDO
	ENDDO
!
!       Dynamical deallocation
        DEALLOCATE ( LPTINV )
C -
	RETURN
	END
