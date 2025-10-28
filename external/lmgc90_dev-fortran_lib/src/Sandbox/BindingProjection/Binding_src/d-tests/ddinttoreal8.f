	SUBROUTINE DDINTTOREAL8 ( N , I , X )
	INTEGER N
	INTEGER I ( N )
	REAL*8 X ( N )
	REAL*8 XX
C	Beware that I and X may share the same memory space
	INTEGER J

	DO J = N , 1 , -1
          XX = FLOAT ( I(J) )
	  X ( J ) = XX
	ENDDO

	RETURN
	END
