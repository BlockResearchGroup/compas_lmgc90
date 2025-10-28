	SUBROUTINE DDREAL8TOINT ( N , X , I )
	INTEGER N
	REAL*8 X ( N )
	INTEGER I ( N )
	INTEGER II
C	Beware that X and I may share the same memory space
	INTEGER J

	DO J = 1 , N
	  II = INT ( X(J) )
	  I ( J ) = II
	ENDDO

	RETURN
	END
