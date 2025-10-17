#include <fintrf.h>
C
C     ProjectChamnog.F - Gateway function for ProjectChamno.f
C     interfacing a mex file to matlab
C
C     Matlab call
C     YVAL = ProjectChamnog(ELEM,ALPHA,XVAL)
C
      SUBROUTINE MEXFUNCTION(NLHS, PLHS, NRHS, PRHS)
C-----------------------------------------------------------------------
C     (pointer) Replace integer by integer*8 on 64-bit platforms
C
      MWPOINTER PLHS(*), PRHS(*)

C-----------------------------------------------------------------------
C

      INTEGER NLHS, NRHS
C
C-----------------------------------------------------------------------
C     (pointer) Replace integer by integer*8 on 64-bit platforms
C
      MWPOINTER MXCREATEDOUBLEMATRIX, MXGETPR

C-----------------------------------------------------------------------
C

      INTEGER MXGETM, MXGETN
C
C KEEP THE ABOVE SUBROUTINE, ARGUMENT, AND FUNCTION DECLARATIONS FOR USE
C IN ALL YOUR FORTRAN MEX FILES.
C---------------------------------------------------------------------

      include 'traces.h'

C     YVAL = ProjectChamnog(ELEM,ALPHA,XVAL)
C     Pointers for input arguments
      MWPOINTER P_ELEM , P_ALPHA , P_XVAL
C     Pointers for output arguments
      MWPOINTER P_YVAL

      INTEGER NBPT , NBNO  , NBELM , NBVAL

C     Local memory space for storing data (floats)
      INTEGER IDIMAX
      PARAMETER ( IDIMAX = 65536 )
      REAL*8 X_ELEM ( IDIMAX ) , ALPHA ( IDIMAX )
      REAL*8 XVAL ( IDIMAX )
      REAL*8 YVAL ( IDIMAX )
C     Local memory space for storing data (integers)
      INTEGER ELEM ( IDIMAX )
C     Same memory storage!
C      EQUIVALENCE (X_ELEM(1),ELEM(1))

      INTEGER M , N
      INTEGER LENGTH

      CALL AFFTRA ( 0 )
      IIMPI = 0

C     Check number of input arguments
      IF (NRHS .NE. 3) THEN
        CALL MEXERRMSGTXT('ProjectChamnog requires 3 input arguments')
      ENDIF
C     Check number of output arguments
      IF (NLHS .GT. 1) THEN
        CALL MEXERRMSGTXT('ProjectChamnog requires 1 output argument')
      ENDIF

C     Dimensions of input arguments
C     ELEM(1,NBPT)
      M    = MXGETM(PRHS(1))
      NBPT = MXGETN(PRHS(1))
      IF (M .NE. 1) THEN
        CALL MEXERRMSGTXT('Bad size for ELEM')
      ENDIF
      IF (M*NBPT .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for ELEM')
      ENDIF
C     ALPHA(NBNO,NBPT)
      NBNO = MXGETM(PRHS(2))
      N    = MXGETN(PRHS(2))
      IF (N .NE. NBPT) THEN
        CALL MEXERRMSGTXT('Bad size for ALPHA')
      ENDIF
      IF (NBNO*N .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for ALPHA')
      ENDIF
C     XVAL(NBNO*NBVAL,NBELM)
      M     = MXGETM(PRHS(3))
      NBELM = MXGETN(PRHS(3))
      NBVAL = M / NBNO
      IF (M .NE. NBNO*NBVAL) THEN
        CALL MEXERRMSGTXT('Bad size for XVAL')
      ENDIF
      IF (M*NBELM .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for XVAL')
      ENDIF

C     Create matrix for output arguments
C     YVAL(NBPT,NBVAL)
      M = NBPT
      N = NBVAL
      IF (M*N .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for YVAL')
      ENDIF
      PLHS(1) = MXCREATEDOUBLEMATRIX(M,N,0)
C     Assign pointers to output arguments
      P_YVAL  = MXGETPR(PLHS(1))

C     Assign pointers to input arguments
      P_ELEM   = MXGETPR(PRHS(1))
      P_ALPHA  = MXGETPR(PRHS(2))
      P_XVAL   = MXGETPR(PRHS(3))

C     Copy input arguments to local memory space
      LENGTH = 1 * NBPT
      CALL MXCOPYPTRTOREAL8 ( P_ELEM , X_ELEM , LENGTH )
      CALL DDREAL8TOINT ( LENGTH , X_ELEM , ELEM )
      LENGTH = NBNO * NBPT
      CALL MXCOPYPTRTOREAL8 ( P_ALPHA , ALPHA , LENGTH )
      LENGTH = NBNO*NBVAL * NBELM
      CALL MXCOPYPTRTOREAL8 ( P_XVAL , XVAL , LENGTH )

C	if (1 .eq. 0) then
	CALL IMPTDC('ProjectChamnog',1,'X_ELEM',X_ELEM,1,NBPT)
	CALL IMPTEC('ProjectChamnog',1,'ELEM',ELEM,1,NBPT)
	CALL IMPTDC('ProjectChamnog',1,'ALPHA',ALPHA,NBNO,NBPT)
	CALL IMPTDC('ProjectChamnog',1,'XVAL',XVAL,NBNO*NBVAL,NBELM)
C	endif

      CALL ZDANUL ( YVAL , NBPT*NBVAL )

C     Call fortran routine
      CALL ProjectChamno ( 
C       Inputs
     &                          NBPT , NBNO  , NBELM , NBVAL ,
     &                          ELEM , ALPHA , XVAL  ,
C       Outputs
     &                          YVAL )

	CALL IMPTDC('ProjectChamnog',1,'YVAL',YVAL,NBPT,NBVAL)

C     Copy local memory space to output arguments
C     YVAL(NBPT,NBVAL)
      LENGTH = NBPT * NBVAL
      CALL MXCOPYREAL8TOPTR ( YVAL , P_YVAL , LENGTH )
C
      RETURN
      END
