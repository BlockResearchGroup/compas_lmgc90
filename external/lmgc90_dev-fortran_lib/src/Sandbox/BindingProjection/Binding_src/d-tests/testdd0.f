C	On teste les distances
	PROGRAMME
	  INTEGER IDIM
	  PARAMETER ( IDIM = 2 )
	  REAL*8 ALPHA ( 4 )
	  REAL*8 XBOX ( 6 )
	  REAL*8 XCOOR ( 3 )

	  REAL*8 DistToSimplex , DistToBox

	  INTEGER I , J , K
	  REAL*8 X
	  INTEGER NBNO , NBPTP
	  PARAMETER ( NBNO = 4 )
	  PARAMETER ( NBPTP = 1 )
	  REAL*8 XCOOREL ( NBNO , IDIM )
	  REAL*8 XCOORPT ( NBPTP , IDIM )
	  REAL*8 ALPHAP( NBNO , NBPTP )
	  CHARACTER*8 CTYPE , OPTIO
	  REAL*8 XPRECR

	  WRITE(*,*),'IDIM',IDIM

	  ALPHA(1) = 0.1D0
	  ALPHA(2) = 0.5D0
	  ALPHA(3) = 1. - ALPHA(1) - ALPHA(2)
	  WRITE(*,*),'  ALPHA',(ALPHA(K),K=1,IDIM+1)
	  X = DistToSimplex ( IDIM , ALPHA )
	  WRITE(*,*),'    DistToSimplex',X

	  ALPHA(1) = -0.1D0
	  ALPHA(2) = 0.5D0
	  ALPHA(3) = 1. - ALPHA(1) - ALPHA(2)
	  WRITE(*,*),'  ALPHA',(ALPHA(K),K=1,IDIM+1)
	  X = DistToSimplex ( IDIM , ALPHA )
	  WRITE(*,*),'    DistToSimplex',X

	  ALPHA(1) = 0.1D0
	  ALPHA(2) = 1.05D0
	  ALPHA(3) = 1. - ALPHA(1) - ALPHA(2)
	  WRITE(*,*),'  ALPHA',(ALPHA(K),K=1,IDIM+1)
	  X = DistToSimplex ( IDIM , ALPHA )
	  WRITE(*,*),'    DistToSimplex',X

	  XBOX(1) = 1.0D0
	  XBOX(2) = 2.0D0
	  XBOX(3) = 1.0D0
	  XBOX(4) = 2.0D0
	  XCOOR(1) = 1.5D0
	  XCOOR(2) = 1.5D0
	  WRITE(*,*),'  XCOOR',(XCOOR(K),K=1,IDIM)
	  X = DistToBox ( IDIM , XCOOR , 1 , XBOX , 1 )
	  WRITE(*,*),'    DistToBox',X

	  XCOOR(1) = 2.5D0
	  XCOOR(2) = 1.5D0
	  WRITE(*,*),'  XCOOR',(XCOOR(K),K=1,IDIM)
	  X = DistToBox ( IDIM , XCOOR , 1 , XBOX , 1 )
	  WRITE(*,*),'    DistToBox',X

	  XCOOR(1) = 0.5D0
	  XCOOR(2) = 1.5D0
	  WRITE(*,*),'  XCOOR',(XCOOR(K),K=1,IDIM)
	  X = DistToBox ( IDIM , XCOOR , 1 , XBOX , 1 )
	  WRITE(*,*),'    DistToBox',X

	  XBOX(1) = 1.0D0
	  XBOX(2) = 2.0D0
	  XBOX(3) = 1.0D0
	  XBOX(4) = 2.0D0
	  XCOOR(1) = 2.5D0
	  XCOOR(2) = 5.5D0
	  WRITE(*,*),'  XCOOR',(XCOOR(K),K=1,IDIM)
	  X = DistToBox ( IDIM , XCOOR , 1 , XBOX , 1 )
	  WRITE(*,*),'  DistToBox',X


	  XCOOREL(1,1) = 0.0D0
	  XCOOREL(1,2) = 0.0D0
	  XCOOREL(2,1) = 1.0D0
	  XCOOREL(2,2) = 0.0D0
	  XCOOREL(3,1) = 1.0D0
	  XCOOREL(3,2) = 1.0D0
	  XCOOREL(4,1) = 0.0D0
	  XCOOREL(4,2) = 1.0D0
	  CTYPE = 'QUA4'
	  XPRECR = 1.D-5
	  XCOORPT(1,1) = 0.4D0
	  XCOORPT(1,2) = 0.5D0
	  OPTIO = 'SIMPLEX'
	  CALL NodesInElement ( 
C       Inputs
     &                          NBNO    , IDIM  , NBPTP  ,
     &                          XCOOREL , CTYPE , XPRECR , XCOORPT ,
     &                          OPTIO   , 
C       Outputs
     &                          ALPHAP )
	DO I = 1 , NBPTP
	  WRITE(*,*) 'PT',I,' ALPHAP',(ALPHAP(J,I),J=1,NBNO)
	ENDDO

	END
