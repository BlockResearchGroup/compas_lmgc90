	PROGRAMME
C	  Test des routines de projection TRI3

	IMPLICIT NONE

	include 'traces.h'

	INTEGER NBNOT , IDIM , NBELM , NBNO , NBPT
C	NBNOT			INTEGER		Total nb of nodes
	PARAMETER ( NBNOT = 6 )
C	IDIM			INTEGER		Spatial dimension
	PARAMETER ( IDIM = 2 )
C	NBELM			INTEGER		Nb of elements
	PARAMETER ( NBELM = 4 )
C	NBNO			INTEGER		Nb of nodes per element
	PARAMETER ( NBNO = 3 )
C	NBPT			INTEGER		Nb of target points
	PARAMETER ( NBPT = 6 )
	INTEGER NBVAL
C	NBVAL			INTEGER		Nb of values per point
	PARAMETER ( NBVAL = 1 )

	INTEGER MAIL ( NBELM , NBNO )
	REAL*8 XCOOR ( NBNOT , IDIM ) , XCOORC ( NBPT , IDIM )
	REAL*8 XPRECR
	CHARACTER*8 OPTIO , CTYPE
	INTEGER ELEM ( NBPT )
	REAL*8 ALPHA ( NBNO , NBPT )
	REAL*8 XBOX(NBELM,2*IDIM)
	INTEGER LPOINTS(NBPT) , LPTC(NBPT)
	REAL*8 XCOORPT(NBPT,IDIM) , ALPHAP(NBNO,NBPT) , ALPHAPC(NBPT)

	REAL*8 ALPHA1(NBNO,NBPT)

	REAL*8 XVAL(NBNO,NBVAL,NBELM)
	REAL*8 YVAL(NBPT,NBVAL)
	REAL*8 YCHPO1(NBNOT,NBVAL)
	INTEGER LPT1(NBNOT)

	INTEGER IPT , IEL , K

	CALL AFFTRA ( 0 )
	IIMPI = 4

C	Coordinates of nodes
	XCOOR(1,1) =  0.0D0
	XCOOR(1,2) =  0.0D0
	XCOOR(2,1) =  0.0D0
	XCOOR(2,2) =  0.5D0
	XCOOR(3,1) =  2.0D0
	XCOOR(3,2) =  0.5D0
	XCOOR(4,1) =  2.0D0
	XCOOR(4,2) =  0.0D0
	XCOOR(5,1) =  2.0D0
	XCOOR(5,2) =  1.0D0
	XCOOR(6,1) =  0.0D0
	XCOOR(6,2) =  1.0D0

C	Field values at points
	YCHPO1(1,1) = 0.0D0
	YCHPO1(2,1) = 0.0D0
	YCHPO1(3,1) = 2.0D0
	YCHPO1(4,1) = 2.0D0
	YCHPO1(5,1) = 2.0D0
	YCHPO1(6,1) = 0.0D0
	LPT1(1) = 1
	LPT1(2) = 2
	LPT1(3) = 3
	LPT1(4) = 4
	LPT1(5) = 5
	LPT1(6) = 6

C	Mesh
	CTYPE = 'TRI3    '
	MAIL(1,1) = 1
	MAIL(1,2) = 4
	MAIL(1,3) = 3
	MAIL(2,1) = 1
	MAIL(2,2) = 3
	MAIL(2,3) = 2
	MAIL(3,1) = 2
	MAIL(3,2) = 3
	MAIL(3,3) = 6
	MAIL(4,1) = 3
	MAIL(4,2) = 5
	MAIL(4,3) = 6

C	Target points
	IF (1 .EQ. 0) THEN
	ALPHA1(2,1) = 0.0D0
	ALPHA1(3,1) = 0.0D0
	ALPHA1(2,2) = 1.0D0
	ALPHA1(3,2) = 0.0D0
	ALPHA1(2,3) = 0.0D0
	ALPHA1(3,3) = 1.0D0
	DO IPT = 1 , NBPT
	  ALPHA1(1,IPT) = 1.0D0 - ALPHA1(2,IPT) - ALPHA1(3,IPT)
	ENDDO
	CALL IMPTDC ( 'test' , 1 , 'ALPHA1' , ALPHA1 , NBNO , NBPT )
	DO IPT = 1 , NBPT
	  DO K = 1 , IDIM
	    XCOORC(IPT,K) =  XCOOR(1,K) +
     &                       ALPHA1(2,IPT)*(XCOOR(2,K)-XCOOR(1,K)) +
     &                       ALPHA1(3,IPT)*(XCOOR(3,K)-XCOOR(1,K))
	  ENDDO
	ENDDO
	ENDIF
	XCOORC(1,1) =  2.3131745E-01
	XCOORC(1,2) =  -3.1186645E-01
	XCOORC(2,1) =  6.0307379E-02
	XCOORC(2,2) =  1.5797986E-01
	XCOORC(3,1) =  1.9396926E+00
	XCOORC(3,2) =  8.4202014E-01
	XCOORC(4,1) =  2.1107027E+00
	XCOORC(4,2) =  3.7217383E-01
	XCOORC(5,1) =  1.7686825E+00
	XCOORC(5,2) =  1.3118665E+00
	XCOORC(6,1) =  1.1070269E-01
	XCOORC(6,2) =  6.2782617E-01

C	Transforms the Chpo to Chamno
C	""""""""""""""""""""""""""""
      CALL ChpoToChamno (
C       Inputs
     &                          NBNOT , NBNO  , NBELM , NBVAL ,
     &                          YCHPO1 , LPT1 , MAIL ,
C       Outputs
     &                          XVAL  )
	DO IEL = 1 , NBELM
	  CALL IMPTDC ( 'test' , 1 , 'XVAL' , XVAL(1,1,IEL),NBNO,NBVAL )
	ENDDO

        OPTIO = 'SIMPLEX '
	XPRECR = 1.0D-6

C	Initialization of ELEM and ALPHA (mandatory)
C	""""""""""""""""""""""""""""""""
	CALL IANUL ( ELEM , NBPT )
	CALL ZDANUL ( ALPHA , NBNO*NBPT )
      CALL NodesInMesh ( 
C       Inputs
     &                          NBNOT , IDIM , NBELM , NBNO   , NBPT ,
     &                          XCOOR , MAIL , CTYPE , XPRECR , XCOORC ,
     &                          OPTIO , 
C       Outputs
     &                          ELEM  , ALPHA ,
C       Working space
     &                          XBOX  , LPOINTS , LPTC , 
     &                          XCOORPT , ALPHAP , ALPHAPC  )

	CALL IMPTDC ( 'test' , 1 , 'ALPHA ' , ALPHA  , NBNO , NBPT )
	CALL IMPTEC ( 'test' , 1 , 'ELEM' , ELEM , 1 , NBPT )

C	Initialization of YVAL (mandatory)
C	""""""""""""""""""""""""""""""""""
	CALL ZDANUL ( YVAL , NBPT*NBVAL )
      CALL ProjectChamno ( 
C       Inputs
     &                          NBPT , NBNO  , NBELM , NBVAL ,
     &                          ELEM , ALPHA , XVAL  ,
C       Outputs
     &                          YVAL )

	CALL IMPTDC ( 'test' , 1 , 'YVAL' , YVAL , NBPT , NBVAL )

	END
