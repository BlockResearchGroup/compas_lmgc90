C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      FUNCTION DistToBox ( IDIM , XCOORC , INCX , XBOX , INCY )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 26 / 12 / 2007
C
C	Return the distance to a box of a point given by its
C	real coordinates
C
C     Inputs
C	IDIM			INTEGER		Spatial dimension
C	XCOORC(*)		REAL*8		Point coordinate
C	INCX			INTEGER		Pitch for XCOORC
C	XBOX(*)			REAL*8		Barycentric coord.
C	INCY			INTEGER		Pitch for XBOX
C     Returns
C	DistToBox		REAL*8		Absolute distance
C
C	The Kth coordinate of the point is XCOORC(1+(K-1)*INCX)
C	If INCX==1, XCOORC = [X1 X2 ...]
C	The minimum value of the Kth coordinate of the box is
C	XBOX(1+(K-1)*2*INCY)
C	The maximum value of the Kth coordinate of the box is
C	XBOX(2+(K-1)*2*INCY)
C	If INCY==1, XBOX = [X1min X1max Y1min Y1max ...]
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER IDIM
	REAL*8 XCOORC ( * ) , XBOX ( * )
	INTEGER INCX , INCY
	REAL*8 DistToBox
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	REAL*8 AMIN , AMAX , XZERO
	INTEGER K
C ======================================================================
C -
	XZERO = 0.0D0
C
	DistToBox = 0.0D0
	DO K = 1 , IDIM
	  AMIN = XBOX(1+(K-1)*2*INCY) - XCOORC(1+(K-1)*INCX)
	  AMIN = MAX ( AMIN , XZERO )
	  AMAX = XCOORC(1+(K-1)*INCX) - XBOX(2+(K-1)*2*INCY)
	  AMAX = MAX ( AMAX , XZERO )
	  DistToBox = MAX(DistToBox,AMIN)
	  DistToBox = MAX(DistToBox,AMAX)
	ENDDO
C -
	RETURN
	END
