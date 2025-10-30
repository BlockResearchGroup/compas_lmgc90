C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE NodesInElement ( 
C       Inputs
     &                          NBNO    , IDIM  , NBPTP  ,
     &                          XCOOREL , CTYPE , XPRECR , XCOORPT ,
     &                          OPTIO   , 
C       Outputs
     &                          ALPHAP  , ALPHAPC )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 19 / 12 / 2007
C     MARCON Bertrand   LMGC SYSTEMES MULTICONTACTS  le 19 / 12 / 2007
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 26 / 12 / 2007
C       Fortran90 version for dynamical memory allocation
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 02 / 08 / 2008
C       Case of elements in a subspace (e.g. TRI3 in 3D)
C
C	Gives weights correspondance to project a field
C	defined on an element to NBPTP target points whose coordinates are
C	XCOORPT ( NBPTP , IDIM )
C
C     Inputs
C	NBNO			INTEGER		Nb of nodes per element
C	IDIM			INTEGER		Spatial dimension
C	NBPTP			INTEGER		Nb of target points
C	XCOOREL(NBNO,IDIM)	REAL*8		Element node coordinates
C	CTYPE			CHARACTER*8	Type of element
C	XPRECR			REAL*8		Relative precision
C	XCOORPT(NBPTP,IDIM)	REAL*8		Target point coordinates
C	OPTIO			CHARACTER*8	Option
C     Outputs
C	ALPHAP(NBNO,NBPTP)	REAL*8		Corresponding weights
C	ALPHAPC(NBPTP)		REAL*8		In complementary subspace
C
C	Option OPTIO can be
C	'SIMPLEX' for crude facetting of the element (split into simplex)
C	'NEWTON' for true geometry of the element (non linear)
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBNO , IDIM , NBPTP
	REAL*8 XCOOREL ( NBNO , IDIM ) , XCOORPT ( NBPTP , IDIM )
	REAL*8 XPRECR
	CHARACTER*8 OPTIO , CTYPE
	REAL*8 ALPHAP ( NBNO , NBPTP ) , ALPHAPC ( NBPTP )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
C	Option for element splitting into simplexes
	CHARACTER*8 OPTIO1
	INTEGER IS1
C	Number of sub-simplexes
	INTEGER NBS1
C	Number of simplex nodes
	INTEGER NBNO1
C	Distance to a simplex
	REAL*8 XDIST
	REAL*8 DistToSimplex
	REAL*8 XPUN , XZER
	INTEGER IPUN , IZER
	INTEGER J, II, JJ
! *
! *	Working space (fortran90) with known dimensions: automatic 
! *	""""""""""""""""""""""""
!	Weights for the current sub-simplex
	REAL*8, DIMENSION(IDIM+1,NBPTP) :: ALPHA1
!
! *     Working space (fortran90) with unknown dimensions: deffered-shape-array
! *     """"""""""""""""""""""""
!	Weights at simplex nodes (NBNO,IDIM+1,NBS1)
	REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: ALPHAS
!	Simplexes nodes real coordinates (IDIM+1,IDIM,NBS1)
	REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: XCOORES
!	Node permutation to define sub-simplexes (NBS1,IDIM)
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: NODELIST
C ======================================================================
C
C		 print*, 'XCOOREL='
C         do II=1, NBNO
C            do JJ=1, IDIM 
C               write(*, '(D14.7,1X,A1)', advance='no') XCOOREL(II, JJ)
C            end do
C            write(*, *)
C         end do

	CALL ENTREC ( 'NodesInElement' , 1 )
C -
	XPUN = 1.0D0
	XZER = 0.0D0
	IPUN = 1
	IZER = 0
C
	CALL IMPCC ( 'NodesInElement' , 2 , 'CTYPE' , CTYPE )
	CALL IMPEC ( 'NodesInElement' , 1 , 'IDIM'  , IDIM )
C
	IF (((CTYPE(1:4) .EQ. 'TRI3') .AND. (IDIM .EQ. 2)) .OR.
     &      ((CTYPE(1:4) .EQ. 'TET4') .AND. (IDIM .EQ. 3))) THEN
C
C	  The element is a simplex
C	  """"""""""""""""""""""""
	  CALL NodesInSimplex ( 
C           Inputs
     &                          NBNO    , IDIM    , NBPTP   ,
     &                          XCOOREL , XCOORPT ,
C           Outputs
     &                          ALPHAP )
	  CALL ZDANUL ( ALPHAPC , NBPTP )


        ELSEIF (((CTYPE(1:4) .EQ. 'TRI3') .AND. (IDIM .EQ. 3)) .OR.
     &          ((CTYPE(1:4) .EQ. 'SEG2') .AND. (IDIM .EQ. 3)) .OR.
     &          ((CTYPE(1:4) .EQ. 'SEG2') .AND. (IDIM .EQ. 2))) THEN
C
C         The element is a simplex in a subspace
C         """"""""""""""""""""""""""""""""""""""
          CALL NodesInSimplex2 (
C           Inputs
     &                          NBNO    , IDIM   , NBPTP   ,
     &                          XCOOREL , XCOORPT ,
C           Outputs
     &                          ALPHAP  , ALPHAPC )
C
	ELSE
CDD	  CALL IMPEC ( 'NodesInElement' , 1 , 'IDIM'  , IDIM )
CDD	  CALL IMPCC ( 'NodesInElement' , 1 , 'CTYPE' , CTYPE )
CDD	  CALL MESFAT ('NodesInElement' , 'ELEMENT NOT IMPLEMENTED' )
C
C	  Subdivide the element into simplexes
C	  """"""""""""""""""""""""""""""""""""
C	  First simplex node is the element centroid
	  OPTIO1 = 'CENTROID'
	  CALL ElementToSimplexesNB (
C           Inputs
     &                          NBNO    , IDIM    ,
     &                          XCOOREL , CTYPE   , OPTIO1 ,
C           Outputs
     &                          NBS1 )
	  IF (NBS1 .EQ. 0) THEN
	    CALL MESFAT ( 'NodesInElement' , 'Not implemented' )
	  ENDIF
!
!         Dynamical allocations
!	  Weights at simplex nodes
	  ALLOCATE ( ALPHAS(NBNO,IDIM+1,NBS1) )
!	  Simplexes nodes real coordinates
	  ALLOCATE ( XCOORES(IDIM+1,IDIM,NBS1) )
!	  Node permutation to define sub-simplexes
	  ALLOCATE ( NODELIST(NBS1,IDIM) )
C
	  CALL ElementToSimplexes (
C           Inputs
     &                          NBNO    , IDIM   ,
     &                          XCOOREL , CTYPE  , OPTIO1 ,
C           Outputs
     &                          XCOORES , ALPHAS , NODELIST )
C
C	  Loop on simplexes
C	  """"""""""""""""
	  NBNO1 = IDIM + 1
	  DO IS1 = 1 , NBS1

	    CALL NodesInSimplex ( 
C             Inputs
     &                            NBNO1   , IDIM    , NBPTP ,
     &                            XCOORES(1,1,IS1) , XCOORPT ,
C             Outputs
     &                            ALPHA1 )
C
C		print*, 'ALPHA1='
C        do II=1, IDIM + 1
C           do JJ=1, NBPTP
C              write(*, '(D14.7,1X,A1)', advance='no') Alpha1(II, JJ)
C           end do
C           write(*, *)
C        end do

C        print*, 'ALPHAS(:, :, ', IS1, ')='
C        do II=1, NBNO
C           do JJ=1, IDIM + 1
C              write(*, '(D14.7,1X,A1)', advance='no') AlphaS(II,JJ,IS1)
C           end do
C           write(*, *)
C        end do

C	    Loop on target points
C	    ''''''''''''''''''''
	    DO J = 1 , NBPTP
	      XDIST = DistToSimplex ( IDIM , ALPHA1(1,J) )
	      IF ((XDIST .LE. XPRECR) .OR. (IS1 .EQ. 1)) THEN
C	        Point J is in the simplex IS1
C	        or first simplex (for initialization)
C               ALPHAP(1:NBNO,J) = ALPHAS(1:NBNO,1:NBNO1,IS1) *
C                                  ALPHA1(1:NBNO1,J)

C     am: remplacement appel LAPACK par un appel a MATMUL
	        CALL dgemv('N',NBNO,NBNO1, XPUN,ALPHAS(1,1,IS1),NBNO, 
     &                     ALPHA1(1,J),IPUN, XZER,ALPHAP(1,J),IPUN )
C			ALPHAP(:, J) = matmul(ALPHAS(:, :, IS1), ALPHA1(:, J))
			
	      ENDIF
	    ENDDO
		
C		print*, 'ALPHAP='
C        do II=1, NBNO
C           do JJ=1, NBPTP
C              write(*, '(D14.7,1X,A1)', advance='no') AlphaP(II, JJ)
C           end do
C           write(*, *)
C        end do
		
	  ENDDO
!
!         Dynamical deallocations
	  DEALLOCATE ( ALPHAS )
	  DEALLOCATE ( XCOORES )
	  DEALLOCATE ( NODELIST )
C
	ENDIF

	IF (OPTIO(1:7) .EQ. 'SIMPLEX') THEN
	ELSE
	  CALL IMPCC  ( 'NodesInElement' , 0 , 'OPTIO' , OPTIO )
	  CALL MESFAT ( 'NodesInElement' ,'OPTION NOT IMPLEMENTED' )
	ENDIF
C
	CALL IMPTDC ( 'NodesInElement' , 2 ,'ALPHAP',ALPHAP,NBNO,NBPTP )
C
	CALL SORTIC ( 'NodesInElement' , 1 )
C -
	RETURN
	END
