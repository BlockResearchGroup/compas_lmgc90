	PROGRAMME
C	  Test des routines de projection QUA4

	IMPLICIT NONE

	include 'traces.h'

	INTEGER NBNOT , IDIM , NBELM , NBNO , NBPT
C	NBNOT			INTEGER		Total nb of nodes
	PARAMETER ( NBNOT = 4 )
C	IDIM			INTEGER		Spatial dimension
	PARAMETER ( IDIM = 2 )
C	NBELM			INTEGER		Nb of elements
	PARAMETER ( NBELM = 1 )
C	NBNO			INTEGER		Nb of nodes per element
	PARAMETER ( NBNO = 4 )
C	NBPT			INTEGER		Nb of target points
	PARAMETER ( NBPT = 4 )
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
	REAL*8 XCOORPT(NBPT,IDIM) , ALPHAP(NBNO,NBPT)

	REAL*8 ALPHA1(NBNO,NBPT)

	REAL*8 XVAL(NBNO,NBVAL,NBELM)
	REAL*8 YVAL(NBPT,NBVAL)
	REAL*8 YCHPO1(NBNOT,NBVAL)
	INTEGER LPT1(NBNOT)

	INTEGER IPT , IEL , K

	CALL AFFTRA ( 0 )
	IIMPI = 3

C	Coordinates of nodes
	XCOOR(1,1) =  0.0D0
	XCOOR(1,2) =  0.0D0
	XCOOR(2,1) =  1.0D0
	XCOOR(2,2) =  0.0D0
	XCOOR(3,1) =  1.0D0
	XCOOR(3,2) =  1.0D0
	XCOOR(4,1) =  0.0D0
	XCOOR(4,2) =  1.0D0

C	Field values at points
	YCHPO1(1,1) = 1.0D0
	YCHPO1(2,1) = 2.0D0
	YCHPO1(3,1) = 3.0D0
	YCHPO1(4,1) = 4.0D0
	LPT1(1) = 1
	LPT1(2) = 2
	LPT1(3) = 3
	LPT1(4) = 4

C	Mesh
	CTYPE = 'QUA4    '
	MAIL(1,1) = 1
	MAIL(1,2) = 2
	MAIL(1,3) = 3
	MAIL(1,4) = 4

	XCOORC(1,1) =  0.1D0
	XCOORC(1,2) =  0.1D0
	XCOORC(2,1) =  0.9D0
	XCOORC(2,2) =  0.1D0
	XCOORC(3,1) =  0.9D0
	XCOORC(3,2) =  0.9D0
	XCOORC(4,1) =  0.1D0
	XCOORC(4,2) =  0.9D0

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
     &                          XCOORPT , ALPHAP  )

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
