C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ShapeFunctionValues ( 
C           Inputs
     &                          CTYPE , NBNO , IDIM , NBPT , XREF ,
C           Outputs
     &                          XSHAPE )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 29 / 12 / 2007
C
C	Give shape function values at particular points
C
C     Inputs
C	CTYPE			CHARACTER*8	Type of element
C	NBNO			INTEGER		Nb of nodes per element
C	IDIM			INTEGER		Spatial dimension
C	NBPT			INTEGER		Number of points
C	XREF(NBPT,IDIM)		REAL*8		Reference coord.
C     Outputs
C	XSHAPE(NBNO,NBPT)	REAL*8		Values at points
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	CHARACTER*8 CTYPE
	INTEGER NBNO , IDIM , NBPT
	REAL*8 XREF ( NBPT , IDIM )
	REAL*8 XSHAPE ( NBNO , NBPT )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER I
C ======================================================================
C -
C	  Switch depending of the element
	  IF ((CTYPE(1:4) .EQ. 'TRI3') .AND. (IDIM .EQ. 2) .AND.
     &        (NBNO .EQ. 3)) THEN
C	    Massive element TRI3
	    DO I = 1 , NBPT
	      XSHAPE(1,I) = (1.-XREF(I,1)-XREF(I,2))
	      XSHAPE(2,I) = XREF(I,1)
	      XSHAPE(3,I) = XREF(I,2)
	    ENDDO

	  ELSEIF ((CTYPE(1:4) .EQ. 'TET4') .AND. (IDIM .EQ. 3) .AND.
     &        (NBNO .EQ. 4)) THEN
C	    Massive element TET4
	    DO I = 1 , NBPT
	      XSHAPE(1,I) = (1.-XREF(I,1)-XREF(I,2)-XREF(I,3))
	      XSHAPE(2,I) = XREF(I,1)
	      XSHAPE(3,I) = XREF(I,2)
	      XSHAPE(4,I) = XREF(I,3)
	    ENDDO

	  ELSEIF ((CTYPE(1:4) .EQ. 'QUA4') .AND. (IDIM .EQ. 2) .AND.
     &        (NBNO .EQ. 4)) THEN
C	    Massive element QUA4
	    DO I = 1 , NBPT
	      XSHAPE(1,I) = 0.25D0 * (1.-XREF(I,1))*(1.-XREF(I,2))
	      XSHAPE(2,I) = 0.25D0 * (1.+XREF(I,1))*(1.-XREF(I,2))
	      XSHAPE(3,I) = 0.25D0 * (1.+XREF(I,1))*(1.+XREF(I,2))
	      XSHAPE(4,I) = 0.25D0 * (1.-XREF(I,1))*(1.+XREF(I,2))
	    ENDDO

	  ELSE
	    CALL IMPCC  ('ShapeFunctionValues',CTYPE)
	    CALL MESFAT ('ShapeFunctionValues','ELEMENT NOT IMPLEMENTED')
	  ENDIF
C -
	RETURN
	END
