C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ElementToSimplexesNB ( 
C           Inputs
     &                          NBNO    , IDIM    ,
     &                          XCOOREL , CTYPE   , OPTIO1 ,
C           Outputs
     &                          NBS1 )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 25 / 12 / 2007
C
C	Split an element into simplexes (return the nb of sub-simplexes)
C
C     Inputs
C	NBNO			INTEGER		Nb of nodes per element
C	IDIM			INTEGER		Spatial dimension
C	XCOOREL(NBNO,IDIM)	REAL*8		Element node coordinates
C	CTYPE			CHARACTER*8	Type of element
C	OPTIO1			CHARACTER*8	Option
C     Outputs
C	NBS1			INTEGER		Number of simplexes
C
C	Option OPTIO1 can be
C	'CENTROID' for which the first simplex node is the element
C	           centroid
C
C	If the element is not implemented, NBS1 = 0
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	INTEGER NBNO , IDIM
	REAL*8 XCOOREL ( NBNO , IDIM )
	CHARACTER*8 OPTIO1 , CTYPE
	INTEGER NBS1
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER INO , K , IS1 , IND , LENGTH
	INTEGER IPUN
C ======================================================================
C -
	CALL IMPCC ( 'ElementToSimplexesNB' , 1 ,  'CTYPE' , CTYPE ) 
	CALL IMPCC ( 'ElementToSimplexesNB' , 1 ,  'OPTIO1' , OPTIO1 ) 
	CALL IMPEC ( 'ElementToSimplexesNB' , 1 ,  'IDIM' , IDIM ) 
	CALL IMPEC ( 'ElementToSimplexesNB' , 1 ,  'NBNO' , NBNO ) 
C
C	Select the number of sub-simplexes
C	""""""""""""""""""""""""""""""""""
	NBS1 = 0
C	Switch depending of the element
	IF ((CTYPE(1:4) .EQ. 'TRI3') .AND. (IDIM .EQ. 2) .AND.
     &      (NBNO .EQ. 3)) THEN
C	  Massive element TRI3 (already a simplex...)
	  NBS1 = 1
	  CALL MESFAT ('ElementToSimplexes','TRI3 already simplex')
	ELSEIF ((CTYPE(1:4) .EQ. 'TET4') .AND. (IDIM .EQ. 3) .AND.
     &      (NBNO .EQ. 4)) THEN
C	  Massive element TET4 (already a simplex...)
	  NBS1 = 1
	  CALL MESFAT ('ElementToSimplexes','TET4 already simplex')
	ELSEIF ((CTYPE(1:4) .EQ. 'QUA4') .AND. (IDIM .EQ. 2) .AND.
     &      (NBNO .EQ. 4)) THEN
C	  Massive element QUA4
	  IF (OPTIO1(1:8) .EQ. 'CENTROID') THEN
	    NBS1 = 4
	  ENDIF
	ENDIF
C -
	RETURN
	END
