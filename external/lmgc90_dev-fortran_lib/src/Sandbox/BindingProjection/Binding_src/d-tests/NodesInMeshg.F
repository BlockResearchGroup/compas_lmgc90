#include <fintrf.h>
C
C     NodesInMeshg.F - Gateway function for nodesInMesh.f
C     interfacing a mex file to matlab
C
C     Matlab call
C     [ELEM,ALPHA] = NodesInMeshg(XCOOR,MAIL,CTYPE,XPREC,XCOORC,OPTIO)
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
      MWPOINTER MXCREATESTRING , MXGETSTRING

C-----------------------------------------------------------------------
C

      INTEGER MXGETM, MXGETN
      INTEGER MXISCHAR
C
C KEEP THE ABOVE SUBROUTINE, ARGUMENT, AND FUNCTION DECLARATIONS FOR USE
C IN ALL YOUR FORTRAN MEX FILES.
C---------------------------------------------------------------------

      include 'traces.h'

C     [ELEM,ALPHA] = NodesInMeshg(XCOOR,MAIL,CTYPE,XPREC,XCOORC,OPTIO)
C     Pointers for input arguments
      MWPOINTER P_XCOOR , P_MAIL , P_XPREC , P_XCOORC
      MWPOINTER P_CTYPE , P_OPTIO
C     Pointers for output arguments
      MWPOINTER P_ELEM , P_ALPHA

      INTEGER NBNOT , IDIM , NBELM , NBNO   , NBPT

C     Local memory space for storing data (floats)
      INTEGER IDIMAX
      PARAMETER ( IDIMAX = 65536 )
      REAL*8 XCOOR ( IDIMAX ) , X_MAIL ( IDIMAX )
      REAL*8 XPREC , XCOORC ( IDIMAX )
      REAL*8 X_ELEM ( IDIMAX ) , ALPHA ( IDIMAX )
      CHARACTER*8 CTYPE , OPTIO
C     Local memory space for storing data (integers)
      INTEGER ELEM ( IDIMAX ) , MAIL ( IDIMAX )
C     Same memory storage!
C      EQUIVALENCE (X_MAIL(1),MAIL(1))
C      EQUIVALENCE (X_ELEM(1),ELEM(1))
C     Local memory space for working tabs
      REAL*8 XBOX ( IDIMAX ) , XCOORPT ( IDIMAX ) , ALPHAP ( IDIMAX )
      INTEGER LPOINTS ( IDIMAX )
      INTEGER LPTC ( IDIMAX )

      INTEGER M , N
      INTEGER LENGTH , LCTYPE , LOPTIO
      INTEGER KERR

      CALL AFFTRA ( 0 )
      IIMPI = 0

C     Check number of input arguments
      IF (NRHS .NE. 6) THEN
        CALL MEXERRMSGTXT('NodesInMeshg requires 6 input arguments')
      ENDIF
C     Check if strings
      IF (MXISCHAR(PRHS(3)) .NE. 1) THEN
        CALL MEXERRMSGTXT('NodesInMeshg: argument 3 should be a string')
      ENDIF
      IF (MXISCHAR(PRHS(6)) .NE. 1) THEN
        CALL MEXERRMSGTXT('NodesInMeshg: argument 6 should be a string')
      ENDIF
C     Check number of output arguments
      IF (NLHS .GT. 2) THEN
        CALL MEXERRMSGTXT('NodesInMeshg requires 2 output arguments')
      ENDIF

C     Dimensions of input arguments
C     XCOOR(NBNOT,IDIM)
      NBNOT = MXGETM(PRHS(1))
      IDIM  = MXGETN(PRHS(1))
      IF (NBNOT*IDIM .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for XCOOR')
      ENDIF
C     X_MAIL(NBELM,NBNO)
      NBELM = MXGETM(PRHS(2))
      NBNO  = MXGETN(PRHS(2))
      IF (NBELM*NBNO .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for MAIL')
      ENDIF
C     CTYPE
      LCTYPE = MXGETM(PRHS(3))*MXGETN(PRHS(3))
      CALL IMPEC('NodesInMeshg',1,'LCTYPE',LCTYPE)
      IF (LCTYPE .GT. 8) THEN
        CALL MEXERRMSGTXT('CTYPE is a too long string')
      ENDIF
C     XPREC(1,1)
      M = MXGETM(PRHS(4))
      N = MXGETN(PRHS(4))
      IF ((M .NE. 1) .OR. (N .NE. 1)) THEN
        CALL MEXERRMSGTXT('Bad size for XPREC')
      ENDIF
C     XCOORC(NBPT,IDIM)
      NBPT = MXGETM(PRHS(5))
      N    = MXGETN(PRHS(5))
      IF (N .NE. IDIM) THEN
        CALL MEXERRMSGTXT('Bad size for XCOORC')
      ENDIF
      IF (NBPT*IDIM .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for XCOORC')
      ENDIF
C     OPTIO
      LOPTIO = MXGETM(PRHS(6))*MXGETN(PRHS(6))
      CALL IMPEC('NodesInMeshg',1,'LOPTIO',LOPTIO)
      IF (LOPTIO .GT. 8) THEN
        CALL MEXERRMSGTXT('OPTIO is a too long string')
      ENDIF

C     Dimensions of working spaces
      IF (NBELM*2*IDIM .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for XBOX')
      ENDIF
      IF (NBPT .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for LPOINTS, LPTC')
      ENDIF
      IF (NBPT*IDIM .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for XCOORPT')
      ENDIF
      IF (NBNO*NBPT .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for ALPHAP')
      ENDIF

C     Create matrix for output arguments
C     ELEM(1,NBPT)
      M = 1
      N = NBPT
      IF (M*N .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for ELEM')
      ENDIF
      PLHS(1) = MXCREATEDOUBLEMATRIX(M,N,0)
C     ALPHA(NBNO,NBPT)
      M = NBNO
      N = NBPT
      IF (M*N .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for ALPHA')
      ENDIF
      PLHS(2) = MXCREATEDOUBLEMATRIX(M,N,0)
C     Assign pointers to output arguments
      P_ELEM  = MXGETPR(PLHS(1))
      P_ALPHA = MXGETPR(PLHS(2))

C     Assign pointers to input arguments
      P_XCOOR  = MXGETPR(PRHS(1))
      P_MAIL   = MXGETPR(PRHS(2))
      P_XPREC  = MXGETPR(PRHS(4))
      P_XCOORC = MXGETPR(PRHS(5))

C     Copy input arguments to local memory space
      LENGTH = NBNOT * IDIM
      CALL MXCOPYPTRTOREAL8 ( P_XCOOR , XCOOR , LENGTH )
      LENGTH = NBELM * NBNO
      CALL MXCOPYPTRTOREAL8 ( P_MAIL , X_MAIL , LENGTH )
      CALL DDREAL8TOINT ( LENGTH , X_MAIL , MAIL )
      LENGTH = 1
      CALL MXCOPYPTRTOREAL8 ( P_XPREC , XPREC , LENGTH )
      LENGTH = NBPT * IDIM
      CALL MXCOPYPTRTOREAL8 ( P_XCOORC , XCOORC , LENGTH )
      KERR = MXGETSTRING(PRHS(3),CTYPE,8)
      IF (KERR .NE. 0) THEN
        CALL MEXERRMSGTXT('CTYPE should be max 8 character long')
      ENDIF
      KERR = MXGETSTRING(PRHS(6),OPTIO,8)
      IF (KERR .NE. 0) THEN
        CALL MEXERRMSGTXT('OPTIO should be max 8 character long')
      ENDIF

	if (1 .eq. 0) then
	CALL IMPTDC('NodesInMeshg',1,'XCOOR',XCOOR,NBNOT,IDIM)
	CALL IMPTDC('NodesInMeshg',1,'X_MAIL',X_MAIL,NBELM,NBNO)
	CALL IMPTEC('NodesInMeshg',1,'MAIL',MAIL,NBELM,NBNO)
	CALL IMPDC('NodesInMeshg',1,'XPREC',XPREC)
	CALL IMPTDC('NodesInMeshg',1,'XCOORC',XCOORC,NBPT,IDIM)
	endif

      CALL IANUL ( ELEM , NBPT )
      CALL ZDANUL ( ALPHA , NBNO*NBPT )

C     Call fortran routine
CDD      CTYPE = 'TRI3    '
CDD      OPTIO = 'SIMPLEX '
      CALL NodesInMesh ( 
C       Inputs
     &                          NBNOT , IDIM , NBELM , NBNO  , NBPT ,
     &                          XCOOR , MAIL , CTYPE , XPREC , XCOORC ,
     &                          OPTIO , 
C       Outputs
     &                          ELEM  , ALPHA ,
C       Working space
     &                          XBOX  , LPOINTS , LPTC , 
     &                          XCOORPT , ALPHAP  )

	CALL IMPTEC('NodesInMeshg',1,'ELEM',ELEM,1,NBPT)

C     Copy local memory space to output arguments
C     ELEM(1,NBPT)
      LENGTH = 1 * NBPT
      CALL DDINTTOREAL8 ( LENGTH , ELEM , X_ELEM )
      CALL MXCOPYREAL8TOPTR ( X_ELEM , P_ELEM , LENGTH )
C     ALPHA(NBNO,NBPT)
      LENGTH = NBNO * NBPT
      CALL MXCOPYREAL8TOPTR ( ALPHA , P_ALPHA , LENGTH )

	CALL IMPTDC('NodesInMeshg',1,'X_ELEM',X_ELEM,1,NBPT)
	CALL IMPTDC('NodesInMeshg',1,'ALPHA',ALPHA,NBNO,NBPT)
C
      RETURN
      END
