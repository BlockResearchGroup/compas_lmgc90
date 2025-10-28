C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE NodesRefCoor ( 
C           Inputs
     &                        CTYPE , NBNO , IDIM , 
C           Outputs
     &                        Xcorel1 )
C [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
C     DUREISSEIX David  LMGC SYSTEMES MULTICONTACTS  le 26 / 12 / 2007
C
C	Return reference (natural) coordinates of nodes for one
C	typical element
C
C     Inputs
C	CTYPE			CHARACTER*8	Type of element
C	NBNO			INTEGER		Nb of nodes per element
C	IDIM			INTEGER		Spatial dimension
C     Outputs
C	Xcorel1(NBNO,IDIM)	REAL*8		Nodes reference coord.
C ======================================================================
C *
	IMPLICIT NONE
C *
C *	Global parameter statements
C *	""""""""""""""""""""""""""""
	CHARACTER*8 CTYPE
	INTEGER NBNO , IDIM
	REAL*8 Xcorel1 ( NBNO , IDIM  )
C *
C *	Local parameter statements
C *	""""""""""""""""""""""""""
	INTEGER LENGTH
C ======================================================================
C -
C	Node ordering information
C	""""""""""""""""""""""""
C     SEG2       RAC2      RAC3
C     1-----2    1----2    1--2--3
C    -1  0  1    0    1   -1  0  1
C
C     SEG3
C     1--3--2
C    -1  0  1
C
C     TRI3
C   1 3
C     |\
C     | \
C     1--2
C   0    1
C
C     QUA4
C   1 3----4
C     |    |
C     |    |
C  -1 1----2
C    -1    1
C
C     QUA8
C   1 3--7--4
C     |     |
C     8     6
C     |     |
C  -1 1--5--2
C    -1    1
C
C     TRI6
C   1 3
C     |\
C     6 5
C     |  \
C     1-4-2
C   0     1
C
C     PRI6
C         0  1
C         4--6
C        /|  |
C     1 5 |  |
C       | 1--3
C       |/
C    -1 2
C
C     TET4
C       1 4
C         |
C         |
C         1--3
C        /   1
C     1 2
C
C     TE10
C       comme gmsh
C
C     CUB8

	LENGTH = NBNO * IDIM
	CALL ZDANUL ( Xcorel1 , LENGTH )
C
	IF (CTYPE(1:4) .EQ. 'POI1') THEN
	  Xcorel1(1,1) = 0.
	ELSEIF (CTYPE(1:4) .EQ. 'SEG2') THEN
	  Xcorel1(1,1) = -1.
	  Xcorel1(2,1) = 1.
	ELSEIF (CTYPE(1:4) .EQ. 'SEG3') THEN
	  Xcorel1(1,1) = -1.
	  Xcorel1(2,1) = 1.
	ELSEIF (CTYPE(1:4) .EQ. 'TRI3') THEN
	  Xcorel1(2,1) = 1.
	  Xcorel1(3,2) = 1.
	ELSEIF (CTYPE(1:4) .EQ. 'QUA4') THEN
	  Xcorel1(1,1) = -1.
	  Xcorel1(1,2) = -1.
	  Xcorel1(2,1) =  1.
	  Xcorel1(2,2) = -1.
	  Xcorel1(3,1) =  1.
	  Xcorel1(3,2) =  1.
	  Xcorel1(4,1) = -1.
	  Xcorel1(4,2) =  1.
C	ELSEIF (CTYPE(1:4) .EQ. 'QUA8') THEN
C	  Xcorel1 = [-1. -1.
C	              1. -1.
C	              1. 1.
C	             -1. 1.
C	              0. -1.
C	              1. 0.
C	              0. 1.
C	             -1. 0.];
C	ELSEIF (CTYPE(1:4) .EQ. 'TRI6') THEN
C	  Xcorel1 = [0.  0.
C	             1.  0.
C	             0.  1.
C	             0.5 0.
C	             0.5 0.5
C	             0.  0.5];
C	ELSEIF (CTYPE(1:4) .EQ. 'RAC2') THEN
C	  Only 2 nodes of 4 taken into account
C	  Xcorel1 = [0.
C	             1.];
C	ELSEIF (CTYPE(1:4) .EQ. 'RAC3') THEN
C	  Only 3 nodes of 6 taken into account
C	  Xcorel1 = [-1.
C	              0.
C	              1.];
C	ELSEIF (CTYPE(1:4) .EQ. 'PRI6') THEN
C	  Xcorel1 = [0. 0. -1.
C	             1. 0. -1.
C	             0. 1. -1.
C	             0. 0. 1.
C	             1. 0. 1.
C	             0. 1. 1.];
C	ELSEIF (CTYPE(1:4) .EQ. 'PR15') THEN
C	  Xcorel1 = [0.  0.  -1.
C	             1.  0.  -1.
C	             0.  1.  -1.
C	             0.  0.   1.
C	             1.  0.   1.
C	             0.  1.   1.
C	             0.5 0.  -1.
C	             0.5 0.5 -1.
C	             0.  0.5 -1.
C	             0.5 0.   1.
C	             0.5 0.5  1.
C	             0.  0.5  1.
C	             0.  0.   0.
C	             1.  0.   0.
C	             0.  1.   0.];
C	ELSEIF (CTYPE(1:4) .EQ. 'TET4') THEN
C	  Xcorel1 = [0. 0. 0.
C	             1. 0. 0.
C	       0. 1. 0.
C	       0. 0. 1.];
C	ELSEIF (CTYPE(1:4) .EQ. 'TE10') THEN
C	  cf gmsh
C	  Xcorel1 = [0.  0.  0.
C	             1.  0.  0.
C	       0.  1.  0.
C	       0.  0.  1.
C	             0.5 0.  0.
C	             0.5 0.  0.5
C	             0.  0.  0.5
C	             0.  0.5 0.
C	             0.  0.5 0.5
C	             0.5 0.5 0.];
C	ELSEIF (CTYPE(1:4) .EQ. 'CUB8') THEN
C	  Xcorel1 = [-1. -1. -1.
C	              1. -1. -1.
C	              1.  1. -1.
C	             -1.  1. -1.
C	             -1. -1.  1.
C	              1. -1.  1.
C	              1.  1.  1.
C	             -1.  1.  1.];
C	ELSEIF (CTYPE(1:4) .EQ. 'CU20') THEN
C	  Xcorel1 = [-1. -1. -1.
C	              1. -1. -1.
C	              1.  1. -1.
C	             -1.  1. -1.
C	             -1. -1.  1.
C	              1. -1.  1.
C	              1.  1.  1.
C	             -1.  1.  1.
C	              0. -1. -1.
C	              1.  0. -1.
C	              0.  1. -1.
C	             -1.  0. -1.
C	              0. -1. 1.
C	              1.  0. 1.
C	              0.  1. 1.
C	             -1.  0. 1.
C	             -1. -1.  0.
C	              1. -1.  0.
C	              1.  1.  0.
C	             -1.  1.  0.];
	ELSE
	  CALL IMPCC ( 'NodesRefCoor' , 0 , 'CTYPE' , CTYPE ) 
	  CALL MESFAT ( 'NodesRefCoor' , 'Element not implemented' ) 
	ENDIF
C -
	RETURN
	END
