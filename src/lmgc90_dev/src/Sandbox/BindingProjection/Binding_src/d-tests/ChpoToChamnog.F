#include <fintrf.h>
C
C     ChpoToChamnog.F - Gateway function for ChpoToChamno.f
C     interfacing a mex file to matlab
C
C     Matlab call
C     XVAL = ChpoToChamnog(YVAL,LPT,MAIL)
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

C     Pointers for input arguments
      MWPOINTER P_YVAL , P_LPT , P_MAIL
C     Pointers for output arguments
      MWPOINTER P_XVAL

      INTEGER NBPT , NBNO , NBELM , NBVAL

C     Local memory space for storing data (floats)
      INTEGER IDIMAX
      PARAMETER ( IDIMAX = 65536 )
      REAL*8 YVAL ( IDIMAX ) , X_LPT ( IDIMAX ) , X_MAIL ( IDIMAX )
      REAL*8 XVAL ( IDIMAX )
C     Local memory space for storing data (integers)
      INTEGER LPT ( IDIMAX ) , MAIL ( IDIMAX )
C     Same memory storage!
C      EQUIVALENCE (X_LPT(1),LPT(1))
C      EQUIVALENCE (X_MAIL(1),MAIL(1))

      INTEGER M , N
      INTEGER LENGTH

C     Check number of input arguments
      IF (NRHS .NE. 3) THEN
        CALL MEXERRMSGTXT('ChpoToChamnog requires 3 input arguments')
      ENDIF
C     Check number of output arguments
      IF (NLHS .GT. 1) THEN
        CALL MEXERRMSGTXT('ChpoToChamnog requires one output argument')
      ENDIF

C     Dimensions of input arguments
C     YVAL(NBPT,NBVAL)
      NBPT  = MXGETM(PRHS(1))
      NBVAL = MXGETN(PRHS(1))
      IF (NBPT*NBVAL .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for YVAL')
      ENDIF
C     X_LPT(1,NBPT)
      M = MXGETM(PRHS(2))
      N = MXGETN(PRHS(2))
      IF (M .NE. 1) THEN
        CALL MEXERRMSGTXT('size(LPT,1) should be 1')
      ENDIF
      IF (N .NE. NBPT) THEN
        CALL MEXERRMSGTXT('size(LPT,2) should be NBPT')
      ENDIF
      IF (M*N .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for LPT')
      ENDIF
C     X_MAIL(NBELM,NBNO)
      NBELM = MXGETM(PRHS(3))
      NBNO  = MXGETN(PRHS(3))
      IF (NBELM*NBNO .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for MAIL')
      ENDIF

C     Create matrix for output arguments
C     XVAL(NBNO*NBVAL,NBELM)
      M = NBNO * NBVAL
      N = NBELM
      IF (M*N .GT. IDIMAX) THEN
        CALL MEXERRMSGTXT('Not enough static space for XVAL')
      ENDIF
      PLHS(1) = MXCREATEDOUBLEMATRIX(M,N,0)
C     Assign pointers to output arguments
      P_XVAL = MXGETPR(PLHS(1))

C     Assign pointers to input arguments
      P_YVAL = MXGETPR(PRHS(1))
      P_LPT  = MXGETPR(PRHS(2))
      P_MAIL = MXGETPR(PRHS(3))

C     Copy input arguments to local memory space
      LENGTH = NBPT * NBVAL
      CALL MXCOPYPTRTOREAL8 ( P_YVAL , YVAL , LENGTH )
      LENGTH = 1 * NBPT
      CALL MXCOPYPTRTOREAL8 ( P_LPT , X_LPT , LENGTH )
      CALL DDREAL8TOINT ( NBPT , X_LPT , LPT )
      LENGTH = NBELM * NBNO
      CALL MXCOPYPTRTOREAL8 ( P_MAIL , X_MAIL , LENGTH )
      CALL DDREAL8TOINT ( LENGTH , X_MAIL , MAIL )

	if (1 .eq. 0) then
	write(*,10),'NBPT ' , NBPT
	write(*,10),'NBNO ' , NBNO
	write(*,10),'NBELM' , NBELM
	write(*,10),'NBVAL' , NBVAL
	write(*,20),'YVAL '
C     YVAL(NBPT,NBVAL)
	write(*,30),(YVAL(ii),ii=1,NBPT*NBVAL)
	write(*,20),'X_LPT'
C     X_LPT(1,NBPT)
	write(*,30),(X_LPT(ii),ii=1,NBPT)
	write(*,20),'LPT  '
	write(*,40),(LPT(ii),ii=1,NBPT)
C     X_MAIL(NBELM,NBNO)
	write(*,20),'X_MAI'
	write(*,30),(X_MAIL(ii),ii=1,NBELM*NBNO)
	write(*,20),'MAIL '
	write(*,40),(MAIL(ii),ii=1,NBELM*NBNO)
10	FORMAT(A5,I5)
20	FORMAT(A5)
30	FORMAT(10(1X,D12.5))
40	FORMAT(10(1X,I5))
	endif

C     Call fortran routine
      CALL ChpoToChamno (
C       Inputs
     &                          NBPT , NBNO  , NBELM , NBVAL ,
     &                          YVAL , LPT , MAIL ,
C       Outputs
     &                          XVAL  )

	if (1 .eq. 0) then
C     XVAL(NBNO*NBVAL,NBELM)
	write(*,20),'XVAL '
	write(*,30),(XVAL(ii),ii=1,NBNO*NBVAL*NBELM)
	endif

C     Copy local memory space to output arguments
      LENGTH = NBELM * NBNO
      CALL MXCOPYREAL8TOPTR ( XVAL , P_XVAL , LENGTH )
C
      RETURN
      END
