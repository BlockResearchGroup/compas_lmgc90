C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      FUNCTION DistToSimplex ( IDIM , ALPHA )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 25 / 12 / 2007
C
C	Return the relative distance to a simplex of a point given by
C	its barycentric coordinates in the simplex
C
C     Inputs
C	IDIM			INTEGER		Spatial dimension
C	ALPHA(IDIM+1)		REAL*8		Barycentric coord.
C     Returns
C	DistToSimplex		REAL*8		Relative distance
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER IDIM
	REAL*8 ALPHA ( IDIM + 1 )
	REAL*8 DistToSimplex
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	REAL*8 AMIN , AMAX , XZERO
	INTEGER I
C ======================================================================
C -
	XZERO = 0.0D0
C
C	Minimum and maximum values of barycentric coordinates
	AMIN = ALPHA ( 1 )
	AMAX = ALPHA ( 1 )
	DO I = 1 , IDIM
	  AMIN = MIN(AMIN,ALPHA(I+1))
	  AMAX = MAX(AMAX,ALPHA(I+1))
	ENDDO
C	If the point lies inside the simplex, then barycentric
C	coordinates should be between 0. and 1.
	AMIN = -1.0D0 * AMIN
	AMIN = MAX ( AMIN , XZERO )
	AMAX = AMAX - 1.0D0
	AMAX = MAX ( AMAX , XZERO )
	DistToSimplex = MAX(AMIN,AMAX)
C -
	RETURN
	END
