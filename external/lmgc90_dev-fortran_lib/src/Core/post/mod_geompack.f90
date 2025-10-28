MODULE GEOMPACK

  !!****h* LMGC90.CORE/GEOMPACK
  !! NAME
  !!  module GEOMPACK
  !! AUTHOR
  !!  Joe Barry & John Burkardt
  !!****

  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: dtris2
  
CONTAINS

FUNCTION dvec_eq( n, a1, a2 )

!*******************************************************************************
!
!! DVEC_EQ is true if two DVECs are equal.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), two vectors to compare.
!
!    Output, logical DVEC_EQ, is TRUE if every pair of elements A1(I)
!    and A2(I) are equal, and FALSE otherwise.
!
  IMPLICIT NONE

  INTEGER n

  REAL ( kind = 8 ) a1(n)
  REAL ( kind = 8 ) a2(n)

  LOGICAL dvec_eq

  dvec_eq = ( ALL ( a1(1:n) == a2(1:n) ) )

  RETURN
END FUNCTION dvec_eq

FUNCTION dvec_gt ( n, a1, a2 )

!*******************************************************************************
!
!! DVEC_GT == ( A1 > A2 ) for DVECs.
!
!  Discussion:
!
!    The comparison is lexicographic.
!
!    A1 > A2  <=>                              A1(1) > A2(1) or
!                 ( A1(1)     == A2(1)     and A1(2) > A2(2) ) or
!                 ...
!                 ( A1(1:N-1) == A2(1:N-1) and A1(N) > A2(N)
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!    Output, logical DVEC_GT, is TRUE if and only if A1 > A2.
!
  IMPLICIT NONE

  INTEGER n

  REAL ( kind = 8 ) a1(n)
  REAL ( kind = 8 ) a2(n)

  INTEGER i

  LOGICAL dvec_gt

  dvec_gt = .FALSE.

  DO i = 1, n

    IF ( a2(i) < a1(i) ) THEN
      dvec_gt = .TRUE.
      EXIT
    ELSE IF ( a1(i) < a2(i) ) THEN
      dvec_gt = .FALSE.
      EXIT
    END IF

  END DO

  RETURN
END FUNCTION dvec_gt

FUNCTION dvec_lt ( n, a1, a2 )

!*******************************************************************************
!
!! DVEC_LT == ( A1 < A2 ) for DVECs.
!
!  Discussion:
!
!    The comparison is lexicographic.
!
!    A1 < A2  <=>                              A1(1) < A2(1) or
!                 ( A1(1)     == A2(1)     and A1(2) < A2(2) ) or
!                 ...
!                 ( A1(1:N-1) == A2(1:N-1) and A1(N) < A2(N)
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!    Output, logical DVEC_LT, is TRUE if and only if A1 < A2.
!
  IMPLICIT NONE

  INTEGER n

  REAL ( kind = 8 ) a1(n)
  REAL ( kind = 8 ) a2(n)

  INTEGER i

  LOGICAL dvec_lt

  dvec_lt = .FALSE.

  DO i = 1, n

    IF ( a1(i) < a2(i) ) THEN
      dvec_lt = .TRUE.
      EXIT
    ELSE IF ( a2(i) < a1(i) ) THEN
      dvec_lt = .FALSE.
      EXIT
    END IF

  END DO

  RETURN
END FUNCTION dvec_lt
!!!---------------------------------------------------------------------
  FUNCTION d_is_int ( r )
    !!****f*  LMGC90.CORE/GEOMPACK/d_is_int
    !! NAME
    !!  d_is_int
    !! AUTHOR
    !!  John Burkardt
    !! SYNOPSIS
    !!  d_is_int ( r )
    !! INPUTS
    !!  r (real) :  the number to be checked
    !! OUTPUT
    !!  logical D_IS_INT : TRUE if R is an integer value
    !! PURPOSE
    !!  determines if a double precision number represents an integer value.
    !!****
    IMPLICIT NONE
    
    INTEGER i
    REAL ( kind = 8 ) r
    LOGICAL d_is_int
    
    IF ( REAL ( HUGE ( i ), kind = 8 ) < r ) THEN
       d_is_int = .FALSE.
    ELSE IF ( r < - REAL ( HUGE ( i ), kind = 8 ) ) THEN
       d_is_int = .FALSE.
    ELSE IF ( r == REAL ( INT ( r ), kind = 8 ) ) THEN
       d_is_int = .TRUE.
    ELSE
       d_is_int = .FALSE.
    END IF
    
    RETURN

  END FUNCTION d_is_int

  !*******************************************************************************
  !
  !! D2VEC_PART_QUICK_A reorders a D2 vector as part of a quick sort.
  !
  !  Discussion:
  !
  !    The routine reorders the entries of A.  Using A(1:2,1) as a
  !    key, all entries of A that are less than or equal to the key will
  !    precede the key, which precedes all entries that are greater than the key.
  !
  !  Example:
  !
  !    Input:
  !
  !      N = 8
  !
  !      A = ( (2,4), (8,8), (6,2), (0,2), (10,6), (10,0), (0,6), (4,8) )
  !
  !    Output:
  !
  !      L = 2, R = 4
  !
  !      A = ( (0,2), (0,6), (2,4), (8,8), (6,2), (10,6), (10,0), (4,8) )
  !             -----------          ----------------------------------
  !             LEFT          KEY    RIGHT
  !
  !  Modified:
  !
  !    08 December 2004
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Parameters:
  !
  !    Input, integer N, the number of entries of A.
  !
  !    Input/output, real ( kind = 8 ) A(2,N).  On input, the array to be checked.
  !    On output, A has been reordered as described above.
  !
  !    Output, integer L, R, the indices of A that define the three segments.
  !    Let KEY = the input value of A(1:2,1).  Then
  !    I <= L                 A(1:2,I) < KEY;
  !         L < I < R         A(1:2,I) = KEY;
  !                 R <= I    KEY < A(1:2,I).
  !
  SUBROUTINE d2vec_part_quick_a ( n, a, l, r )
    
    IMPLICIT NONE
    
    INTEGER n
    INTEGER, PARAMETER :: dim_num = 2
    
    REAL ( kind = 8 ) a(dim_num,n)

!    LOGICAL dvec_eq
!    LOGICAL dvec_gt
!    LOGICAL dvec_lt

    INTEGER i
    REAL ( kind = 8 ) key(dim_num)
    INTEGER l
    INTEGER m
    INTEGER r
    
    IF ( n < 1 ) THEN
       WRITE ( *, '(a)' ) ' '
       WRITE ( *, '(a)' ) 'D2VEC_PART_QUICK_A - Fatal error!'
       WRITE ( *, '(a)' ) '  N < 1.'
       STOP
    ELSE IF ( n == 1 ) THEN
       l = 0
       r = 2
       RETURN
    END IF
    
    key(1:dim_num) = a(1:dim_num,1)
    m = 1
    !
    !  The elements of unknown size have indices between L+1 and R-1.
    !
    l = 1
    r = n + 1
    
    DO i = 2, n
       
       IF ( dvec_gt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) THEN
          r = r - 1
          CALL dvec_swap ( dim_num, a(1:dim_num,r), a(1:dim_num,l+1) )
       ELSE IF ( dvec_eq ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) THEN
          m = m + 1
          CALL dvec_swap ( dim_num, a(1:dim_num,m), a(1:dim_num,l+1) )
          l = l + 1
       ELSE IF ( dvec_lt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) THEN
          l = l + 1
       END IF
       
    END DO
    !
    !  Now shift small elements to the left, and KEY elements to center.
    !
    DO i = 1, l - m
       a(1:dim_num,i) = a(1:dim_num,i+m)
    END DO
    
    l = l - m
    
    DO i = 1, dim_num
       a(i,l+1:l+m) = key(i)
    END DO
    
    RETURN
  END SUBROUTINE d2vec_part_quick_a

!*******************************************************************************
!
!! D2VEC_PERMUTE permutes a D2 vector in place.
!
!  Discussion:
!
!    This routine permutes an array of real "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,    4,    5,    1,    3 )
!      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
!          (11.0, 22.0, 33.0, 44.0, 55.0 )
!
!    Output:
!
!      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
!             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of objects.
!
!    Input/output, real ( kind = 8 ) A(2,N), the array to be permuted.
!
!    Input, integer P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
  SUBROUTINE d2vec_permute ( n, a, p )

    IMPLICIT NONE

    INTEGER n

    REAL ( kind = 8 ) a(2,n)
    REAL ( kind = 8 ) a_temp(2)
    INTEGER ierror
    INTEGER iget
    INTEGER iput
    INTEGER istart
    INTEGER p(n)
    
    CALL perm_check ( n, p, ierror )
    
    IF ( ierror /= 0 ) THEN
       WRITE ( *, '(a)' ) ' '
       WRITE ( *, '(a)' ) 'D2VEC_PERMUTE - Fatal error!'
       WRITE ( *, '(a)' ) '  The input array does not represent'
       WRITE ( *, '(a)' ) '  a proper permutation.  In particular, the'
       WRITE ( *, '(a,i8)' ) '  array is missing the value ', ierror
       STOP
    END IF
    !
    !  Search for the next element of the permutation that has not been used.
    !
    DO istart = 1, n
       
       IF ( p(istart) < 0 ) THEN
          
          CYCLE
          
       ELSE IF ( p(istart) == istart ) THEN
          
          p(istart) = -p(istart)
          CYCLE
          
       ELSE
          
          a_temp(1:2) = a(1:2,istart)
          iget = istart
          !
          !  Copy the new value into the vacated entry.
          !
      DO

        iput = iget
        iget = p(iget)

        p(iput) = -p(iput)

        IF ( iget < 1 .OR. n < iget ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'D2VEC_PERMUTE - Fatal error!'
          STOP
        END IF

        IF ( iget == istart ) THEN
          a(1:2,iput) = a_temp(1:2)
          EXIT
        END IF

        a(1:2,iput) = a(1:2,iget)

      END DO

    END IF

  END DO
!
!  Restore the signs of the entries.
!
  p(1:n) = -p(1:n)

  RETURN
END SUBROUTINE d2vec_permute


SUBROUTINE d2vec_sort_heap_index_a ( n, a, indx )
!*******************************************************************************
!
!! D2VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of a D2 vector.
!
!  Discussion:
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(1:2,INDX(I)), I = 1 to N is sorted,
!
!    or explicitly, by the call
!
!      call D2VEC_PERMUTE ( N, A, INDX )
!
!    after which A(1:2,I), I = 1 to N is sorted.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(2,N), an array to be index-sorted.
!
!    Output, integer INDX(N), the sort index.  The
!    I-th element of the sorted array is A(1:2,INDX(I)).
!
  IMPLICIT NONE

  INTEGER n

  REAL ( kind = 8 ) a(2,n)
  REAL ( kind = 8 ) aval(2)
  INTEGER i
  INTEGER indx(n)
  INTEGER indxt
  INTEGER ir
  INTEGER j
  INTEGER l

  IF ( n < 1 ) THEN
    RETURN
  END IF

  DO i = 1, n
    indx(i) = i
  END DO

  IF ( n == 1 ) THEN
    RETURN
  END IF

  l = n / 2 + 1
  ir = n

  DO

    IF ( 1 < l ) THEN

      l = l - 1
      indxt = indx(l)
      aval(1:2) = a(1:2,indxt)

    ELSE

      indxt = indx(ir)
      aval(1:2) = a(1:2,indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      IF ( ir == 1 ) THEN
        indx(1) = indxt
        EXIT
      END IF

    END IF

    i = l
    j = l + l

    DO WHILE ( j <= ir )

      IF ( j < ir ) THEN
        IF (   a(1,indx(j)) <  a(1,indx(j+1)) .OR. &
             ( a(1,indx(j)) == a(1,indx(j+1)) .AND. &
               a(2,indx(j)) <  a(2,indx(j+1)) ) ) THEN
          j = j + 1
        END IF
      END IF

      IF (   aval(1) <  a(1,indx(j)) .OR. &
           ( aval(1) == a(1,indx(j)) .AND. &
             aval(2) <  a(2,indx(j)) ) ) THEN
        indx(i) = indx(j)
        i = j
        j = j + j
      ELSE
        j = ir + 1
      END IF

    END DO

    indx(i) = indxt

  END DO

  RETURN
END SUBROUTINE d2vec_sort_heap_index_a
SUBROUTINE d2vec_sort_quick_a ( n, a )

!*******************************************************************************
!
!! D2VEC_SORT_QUICK_A ascending sorts a D2 vector using quick sort.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(2,N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: level_max = 25
  INTEGER n
  INTEGER, PARAMETER :: dim_num = 2

  REAL ( kind = 8 ) a(dim_num,n)
  INTEGER base
  INTEGER l_segment
  INTEGER level
  INTEGER n_segment
  INTEGER rsave(level_max)
  INTEGER r_segment

  IF ( n < 1 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'D2VEC_SORT_QUICK_A - Fatal error!'
    WRITE ( *, '(a)' ) '  N < 1.'
    STOP
  ELSE IF ( n == 1 ) THEN
    RETURN
  END IF

  level = 1
  rsave(level) = n + 1
  base = 1
  n_segment = n

  DO
!
!  Partition the segment.
!
    CALL d2vec_part_quick_a ( n_segment, a(1,base), l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    IF ( 1 < l_segment ) THEN

      IF ( level_max < level ) THEN
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'D2VEC_SORT_QUICK_A - Fatal error!'
        WRITE ( *, '(a,i8)' ) '  Exceeding recursion maximum of ', level_max
        STOP
      END IF

      level = level + 1
      n_segment = l_segment
      rsave(level) = r_segment + base - 1
!
!  The left segment and the middle segment are sorted.
!  Must the right segment be partitioned?
!
    ELSE IF ( r_segment < n_segment ) THEN

      n_segment = n_segment + 1 - r_segment
      base = base + r_segment - 1
!
!  Otherwise, we back up a level if there is an earlier one.
!
    ELSE

      DO

        IF ( level <= 1 ) THEN
          RETURN
        END IF

        base = rsave(level)
        n_segment = rsave(level-1) - rsave(level)
        level = level - 1

        IF ( 0 < n_segment ) THEN
          EXIT
        END IF

      END DO

    END IF

  END DO

  RETURN
END SUBROUTINE d2vec_sort_quick_a

INTEGER FUNCTION diaedg ( x0, y0, x1, y1, x2, y2, x3, y3 )
!*******************************************************************************
!
!! DIAEDG chooses a diagonal edge.
!
!  Discussion:
!
!    The routine determines whether 0--2 or 1--3 is the diagonal edge
!    that should be chosen, based on the circumcircle criterion, where
!    (X0,Y0), (X1,Y1), (X2,Y2), (X3,Y3) are the vertices of a simple
!    quadrilateral in counterclockwise order.
!
!  Modified:
!
!    19 February 2001
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Reference:
!
!    Barry Joe,
!    GEOMPACK - a software package for the generation of meshes
!    using geometric algorithms,
!    Advances in Engineering Software,
!    Volume 13, pages 325-331, 1991.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X0, Y0, X1, Y1, X2, Y2, X3, Y3, the
!    coordinates of the vertices of a quadrilateral, given in
!    counter clockwise order.
!
!    Output, integer DIAEDG, chooses a diagonal:
!    +1, if diagonal edge 02 is chosen;
!    -1, if diagonal edge 13 is chosen;
!     0, if the four vertices are cocircular.
!
  IMPLICIT NONE

  REAL ( kind = 8 ) ca
  REAL ( kind = 8 ) cb
  REAL ( kind = 8 ) dx10
  REAL ( kind = 8 ) dx12
  REAL ( kind = 8 ) dx30
  REAL ( kind = 8 ) dx32
  REAL ( kind = 8 ) dy10
  REAL ( kind = 8 ) dy12
  REAL ( kind = 8 ) dy30
  REAL ( kind = 8 ) dy32
  REAL ( kind = 8 ) s
  REAL ( kind = 8 ) tol
  REAL ( kind = 8 ) tola
  REAL ( kind = 8 ) tolb
  REAL ( kind = 8 ) x0
  REAL ( kind = 8 ) x1
  REAL ( kind = 8 ) x2
  REAL ( kind = 8 ) x3
  REAL ( kind = 8 ) y0
  REAL ( kind = 8 ) y1
  REAL ( kind = 8 ) y2
  REAL ( kind = 8 ) y3

  tol = 100.0D+00 * EPSILON ( tol )

  dx10 = x1 - x0
  dy10 = y1 - y0
  dx12 = x1 - x2
  dy12 = y1 - y2
  dx30 = x3 - x0
  dy30 = y3 - y0
  dx32 = x3 - x2
  dy32 = y3 - y2

  tola = tol * MAX ( ABS ( dx10 ), ABS ( dy10 ), ABS ( dx30 ), ABS ( dy30 ) )
  tolb = tol * MAX ( ABS ( dx12 ), ABS ( dy12 ), ABS ( dx32 ), ABS ( dy32 ) )

  ca = dx10 * dx30 + dy10 * dy30
  cb = dx12 * dx32 + dy12 * dy32

  IF ( tola < ca .AND. tolb < cb ) THEN

    diaedg = -1

  ELSE IF ( ca < -tola .AND. cb < -tolb ) THEN

    diaedg = 1

  ELSE

    tola = MAX ( tola, tolb )
    s = ( dx10 * dy30 - dx30 * dy10 ) * cb + ( dx32 * dy12 - dx12 * dy32 ) * ca

    IF ( tola < s ) THEN
      diaedg = -1
    ELSE IF ( s < -tola ) THEN
      diaedg = 1
    ELSE
      diaedg = 0
    END IF

  END IF

  RETURN
END FUNCTION diaedg

SUBROUTINE dmat_transpose_print ( m, n, a, title )
!*******************************************************************************
!
!! DMAT_TRANSPOSE_PRINT prints a DMAT, transposed.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  IMPLICIT NONE

  INTEGER m
  INTEGER n

  REAL ( kind = 8 ) a(m,n)
  CHARACTER ( len = * ) title

  CALL dmat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  RETURN
END SUBROUTINE dmat_transpose_print
SUBROUTINE dmat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*******************************************************************************
!
!! DMAT_TRANSPOSE_PRINT_SOME prints some of a DMAT, transposed.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: incx = 5
  INTEGER m
  INTEGER n

  REAL ( kind = 8 ) a(m,n)
  CHARACTER ( len = 14 ) ctemp(incx)
  INTEGER i
  INTEGER i2
  INTEGER i2hi
  INTEGER i2lo
  INTEGER ihi
  INTEGER ilo
  INTEGER inc
  INTEGER j
  INTEGER j2hi
  INTEGER j2lo
  INTEGER jhi
  INTEGER jlo
  CHARACTER ( len = * ) title

  IF ( 0 < LEN_TRIM ( title ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) TRIM ( title )
  END IF

  DO i2lo = MAX ( ilo, 1 ), MIN ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = MIN ( i2hi, m )
    i2hi = MIN ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    WRITE ( *, '(a)' ) ' '

    DO i = i2lo, i2hi
      i2 = i + 1 - i2lo
      WRITE ( ctemp(i2), '(i7,7x)') i
    END DO

    WRITE ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    WRITE ( *, '(a)' ) '  Col'
    WRITE ( *, '(a)' ) ' '

    j2lo = MAX ( jlo, 1 )
    j2hi = MIN ( jhi, n )

    DO j = j2lo, j2hi

      DO i2 = 1, inc
        i = i2lo - 1 + i2
        WRITE ( ctemp(i2), '(g14.6)' ) a(i,j)
      END DO

      WRITE ( *, '(i5,1x,5a14)' ) j, ( ctemp(i), i = 1, inc )

    END DO

  END DO

  WRITE ( *, '(a)' ) ' '

  RETURN
END SUBROUTINE dmat_transpose_print_some


SUBROUTINE dmat_uniform ( m, n, a, b, seed, r )
!*******************************************************************************
!
!! DMAT_UNIFORM fills an array with scaled pseudorandom numbers.
!
!  Modified:
!
!    05 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, L E Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    P A Lewis, A S Goodman, J M Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns in the array.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer SEED, the "seed" value, which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  IMPLICIT NONE

  INTEGER m
  INTEGER n

  REAL ( kind = 8 ) a
  REAL ( kind = 8 ) b
  INTEGER i
  INTEGER j
  INTEGER k
  INTEGER seed
  REAL ( kind = 8 ) r(m,n)

  DO j = 1, n

    DO i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      IF ( seed < 0 ) THEN
        seed = seed + 2147483647
      END IF

      r(i,j) = a + ( b - a ) * REAL ( seed, kind = 8 ) * 4.656612875D-10

    END DO
  END DO

  RETURN
END SUBROUTINE dmat_uniform

SUBROUTINE dtris2 ( node_num , node_xy , indx , triangle_num, triangle_node, &
  triangle_neighbor )

!*******************************************************************************
!
!! DTRIS2 constructs a Delaunay triangulation of 2D vertices.
!
!  Discussion:
!
!    The routine constructs the Delaunay triangulation of a set of 2D vertices
!    using an incremental approach and diagonal edge swaps.  Vertices are
!    first sorted in lexicographically increasing (X,Y) order, and
!    then are inserted one at a time from outside the convex hull.
!
!  Modified:
!
!    25 August 2001
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Reference:
!
!    Barry Joe,
!    GEOMPACK - a software package for the generation of meshes
!    using geometric algorithms,
!    Advances in Engineering Software,
!    Volume 13, pages 325-331, 1991.
!
!  Parameters:
!
!    Input, integer NODE_NUM, the number of vertices.
!
!    Input/output, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates
!    of the vertices.  On output, the vertices have been sorted into
!    dictionary order.
!
!    Output, integer TRIANGLE_NUM, the number of triangles in the triangulation;
!    TRIANGLE_NUM is equal to 2*NODE_NUM - NB - 2, where NB is the number
!    of boundary vertices.
!
!    Output, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes that make up 
!    each triangle.  The elements are indices of P.  The vertices of the
!    triangles are in counter clockwise order.
!
!    Output, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the triangle neighbor
!    list.  Positive elements are indices of TIL; negative elements are used
!    for links of a counter clockwise linked list of boundary edges;
!    LINK = -(3*I + J-1) where I, J = triangle, edge index;
!    TRIANGLE_NEIGHBOR(J,I) refers to the neighbor along edge from vertex J
!    to J+1 (mod 3).
!
  IMPLICIT NONE

  INTEGER node_num

  REAL ( kind = 8 ) cmax
  INTEGER e
  INTEGER i
  INTEGER ierr
  INTEGER indx(node_num)
  INTEGER j
  INTEGER k
  INTEGER l
  INTEGER ledg
  INTEGER lr
  INTEGER ltri
  INTEGER m
  INTEGER m1
  INTEGER m2
  INTEGER n
  REAL ( kind = 8 ) node_xy(2,node_num)
  INTEGER redg
  INTEGER rtri
  INTEGER stack(node_num)
  INTEGER t
  REAL ( kind = 8 ) tol
  INTEGER top
  INTEGER triangle_neighbor(3,node_num*2)
  INTEGER triangle_num
  INTEGER triangle_node(3,node_num*2)

  tol = 100.0D+00 * EPSILON ( tol )

  ierr = 0
!
!  Sort the vertices by increasing (x,y).
!
  CALL d2vec_sort_heap_index_a ( node_num, node_xy, indx )

  CALL d2vec_permute ( node_num, node_xy, indx )
!
!  Make sure that the data points are "reasonably" distinct.
!
  m1 = 1

  DO i = 2, node_num

    m = m1
    m1 = i

    k = 0

    DO j = 1, 2

      cmax = MAX ( ABS ( node_xy(j,m) ), ABS ( node_xy(j,m1) ) )

      IF ( tol * ( cmax + 1.0D+00 ) &
           < ABS ( node_xy(j,m) - node_xy(j,m1) ) ) THEN
        k = j
        EXIT
      END IF

    END DO

    IF ( k == 0 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'DTRIS2 - Fatal error!'
      WRITE ( *, '(a,i8)' ) '  Fails for point number I = ', i
      WRITE ( *, '(a,i8)' ) '  M = ', m
      WRITE ( *, '(a,i8)' ) '  M1 = ', m1
      WRITE ( *, '(a,2g14.6)' ) '  X,Y(M)  = ', node_xy(1:2,m)
      WRITE ( *, '(a,2g14.6)' ) '  X,Y(M1) = ', node_xy(1:2,m1)
      ierr = 224
      RETURN
    END IF

  END DO
!
!  Starting from points M1 and M2, search for a third point M that
!  makes a "healthy" triangle (M1,M2,M)
!
  m1 = 1
  m2 = 2
  j = 3

  DO

    IF ( node_num < j ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'DTRIS2 - Fatal error!'
      ierr = 225
      RETURN
    END IF

    m = j

    lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), &
      node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )

    IF ( lr /= 0 ) THEN
      EXIT
    END IF

    j = j + 1

  END DO
!
!  Set up the triangle information for (M1,M2,M), and for any other
!  triangles you created because points were collinear with M1, M2.
!
  triangle_num = j - 2

  IF ( lr == -1 ) THEN

    triangle_node(1,1) = m1
    triangle_node(2,1) = m2
    triangle_node(3,1) = m
    triangle_neighbor(3,1) = -3

    DO i = 2, triangle_num

      m1 = m2
      m2 = i+1
      triangle_node(1,i) = m1
      triangle_node(2,i) = m2
      triangle_node(3,i) = m
      triangle_neighbor(1,i-1) = -3 * i
      triangle_neighbor(2,i-1) = i
      triangle_neighbor(3,i) = i - 1

    END DO

    triangle_neighbor(1,triangle_num) = -3 * triangle_num - 1
    triangle_neighbor(2,triangle_num) = -5
    ledg = 2
    ltri = triangle_num

  ELSE

    triangle_node(1,1) = m2
    triangle_node(2,1) = m1
    triangle_node(3,1) = m
    triangle_neighbor(1,1) = -4

    DO i = 2, triangle_num
      m1 = m2
      m2 = i+1
      triangle_node(1,i) = m2
      triangle_node(2,i) = m1
      triangle_node(3,i) = m
      triangle_neighbor(3,i-1) = i
      triangle_neighbor(1,i) = -3 * i - 3
      triangle_neighbor(2,i) = i - 1
    END DO

    triangle_neighbor(3,triangle_num) = -3 * triangle_num
    triangle_neighbor(2,1) = -3 * triangle_num - 2
    ledg = 2
    ltri = 1

  END IF
!
!  Insert the vertices one at a time from outside the convex hull,
!  determine visible boundary edges, and apply diagonal edge swaps until
!  Delaunay triangulation of vertices (so far) is obtained.
!
  top = 0

  DO i = j+1, node_num

    m = i
    m1 = triangle_node(ledg,ltri)

    IF ( ledg <= 2 ) THEN
      m2 = triangle_node(ledg+1,ltri)
    ELSE
      m2 = triangle_node(1,ltri)
    END IF

    lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), &
      node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )

    IF ( 0 < lr ) THEN
      rtri = ltri
      redg = ledg
      ltri = 0
    ELSE
      l = -triangle_neighbor(ledg,ltri)
      rtri = l / 3
      redg = MOD(l,3) + 1
    END IF

    CALL vbedg ( node_xy(1,m), node_xy(2,m), node_num, node_xy, triangle_num, &
      triangle_node, triangle_neighbor, ltri, ledg, rtri, redg )

    n = triangle_num + 1
    l = -triangle_neighbor(ledg,ltri)

    DO

      t = l / 3
      e = MOD ( l, 3 ) + 1
      l = -triangle_neighbor(e,t)
      m2 = triangle_node(e,t)

      IF ( e <= 2 ) THEN
        m1 = triangle_node(e+1,t)
      ELSE
        m1 = triangle_node(1,t)
      END IF

      triangle_num = triangle_num + 1
      triangle_neighbor(e,t) = triangle_num
      triangle_node(1,triangle_num) = m1
      triangle_node(2,triangle_num) = m2
      triangle_node(3,triangle_num) = m
      triangle_neighbor(1,triangle_num) = t
      triangle_neighbor(2,triangle_num) = triangle_num - 1
      triangle_neighbor(3,triangle_num) = triangle_num + 1
      top = top + 1

      IF ( node_num < top ) THEN
        ierr = 8
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'DTRIS2 - Fatal error!'
        WRITE ( *, '(a)' ) '  Stack overflow.'
        RETURN
      END IF

      stack(top) = triangle_num

      IF ( t == rtri .AND. e == redg ) THEN
        EXIT
      END IF

    END DO

    triangle_neighbor(ledg,ltri) = -3 * n - 1
    triangle_neighbor(2,n) = -3 * triangle_num - 2
    triangle_neighbor(3,triangle_num) = -l
    ltri = n
    ledg = 2

    CALL swapec ( m, top, ltri, ledg, node_num, node_xy, triangle_num, &
      triangle_node, triangle_neighbor, stack, ierr )

    IF ( ierr /= 0 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'DTRIS2 - Fatal error!'
      WRITE ( *, '(a)' ) '  Error return from SWAPEC.'
      RETURN
    END IF

  END DO
!
!  Now account for the sorting that we did.
!
  DO i = 1, 3
    DO j = 1, triangle_num
      triangle_node(i,j) = indx ( triangle_node(i,j) )
    END DO
  END DO

  CALL perm_inv ( node_num, indx )

  CALL d2vec_permute ( node_num, node_xy, indx )

  RETURN
END SUBROUTINE dtris2

SUBROUTINE dvec_print ( n, a, title )

!*******************************************************************************
!
!! DVEC_PRINT prints a DVEC.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  IMPLICIT NONE

  INTEGER n

  REAL ( kind = 8 ) a(n)
  INTEGER i
  CHARACTER ( len = * ) title

  IF ( 0 < LEN_TRIM ( title ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) TRIM ( title )
  END IF

  WRITE ( *, '(a)' ) ' '
  DO i = 1, n
    WRITE ( *, '(2x,i8,g16.8)' ) i, a(i)
  END DO

  RETURN
END SUBROUTINE dvec_print
SUBROUTINE dvec_swap ( n, a1, a2 )

!*******************************************************************************
!
!! DVEC_SWAP swaps the entries of two DVECs.
!
!  Modified:
!
!    04 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the arrays.
!
!    Input/output, real ( kind = 8 ) A1(N), A2(N), the vectors to swap.
!
  IMPLICIT NONE

  INTEGER n

  REAL ( kind = 8 ) a1(n)
  REAL ( kind = 8 ) a2(n)
  REAL ( kind = 8 ) a3(n)

  a3(1:n) = a1(1:n)
  a1(1:n) = a2(1:n)
  a2(1:n) = a3(1:n)

  RETURN
END SUBROUTINE dvec_swap

SUBROUTINE get_unit ( iunit )
!*******************************************************************************
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT, the free unit number.
!
  IMPLICIT NONE

  INTEGER i
  INTEGER ios
  INTEGER iunit
  LOGICAL lopen

  iunit = 0

  DO i = 1, 99

    IF ( i /= 5 .AND. i /= 6 .AND. i /= 9 ) THEN

      INQUIRE ( unit = i, opened = lopen, iostat = ios )

      IF ( ios == 0 ) THEN
        IF ( .NOT. lopen ) THEN
          iunit = i
          RETURN
        END IF
      END IF

    END IF

  END DO

  RETURN
END SUBROUTINE get_unit
INTEGER FUNCTION i_modp ( i, j )

!*******************************************************************************
!
!! I_MODP returns the nonnegative remainder of integer division.
!
!  Formula:
!
!    If
!      NREM = I_MODP ( I, J )
!      NMULT = ( I - NREM ) / J
!    then
!      I = J * NMULT + NREM
!    where NREM is always nonnegative.
!
!  Discussion:
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, I_MODP(A,360) is between 0 and 360, always.
!
!  Examples:
!
!        I     J     MOD  I_MODP    Factorization
!
!      107    50       7       7    107 =  2 *  50 + 7
!      107   -50       7       7    107 = -2 * -50 + 7
!     -107    50      -7      43   -107 = -3 *  50 + 43
!     -107   -50      -7      43   -107 =  3 * -50 + 43
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, the number to be divided.
!
!    Input, integer J, the number that divides I.
!
!    Output, integer I_MODP, the nonnegative remainder when I is
!    divided by J.
!
  IMPLICIT NONE

  INTEGER i
  INTEGER j

  IF ( j == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I_MODP - Fatal error!'
    WRITE ( *, '(a,i8)' ) '  I_MODP ( I, J ) called with J = ', j
    STOP
  END IF

  i_modp = MOD ( i, j )

  IF ( i_modp < 0 ) THEN
    i_modp = i_modp + ABS ( j )
  END IF

  RETURN
END FUNCTION i_modp
SUBROUTINE i_swap ( i, j )

!*******************************************************************************
!
!! I_SWAP swaps two integer values.
!
!  Modified:
!
!    30 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer I, J.  On output, the values of I and
!    J have been interchanged.
!
  IMPLICIT NONE

  INTEGER i
  INTEGER j
  INTEGER k

  k = i
  i = j
  j = k

  RETURN
END SUBROUTINE i_swap

INTEGER FUNCTION i_wrap ( ival, ilo, ihi )

!*******************************************************************************
!
!! I_WRAP forces an integer to lie between given limits by wrapping.
!
!  Example:
!
!    ILO = 4, IHI = 8
!
!    I  I_WRAP
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Modified:
!
!    19 August 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer IVAL, an integer value.
!
!    Input, integer ILO, IHI, the desired bounds for the integer value.
!
!    Output, integer I_WRAP, a "wrapped" version of IVAL.
!
  IMPLICIT NONE

  INTEGER ihi
  INTEGER ilo
  INTEGER ival
  INTEGER jhi
  INTEGER jlo
  INTEGER wide

  jlo = MIN ( ilo, ihi )
  jhi = MAX ( ilo, ihi )

  wide = jhi - jlo + 1

  IF ( wide == 1 ) THEN
    i_wrap = jlo
  ELSE
    i_wrap = jlo + i_modp ( ival - jlo, wide )
  END IF

  RETURN
END FUNCTION i_wrap
SUBROUTINE imat_transpose_print ( m, n, a, title )

!*******************************************************************************
!
!! IMAT_TRANSPOSE_PRINT prints an IMAT, transposed.
!
!  Modified:
!
!    09 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, integer A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  IMPLICIT NONE

  INTEGER m
  INTEGER n

  INTEGER a(m,n)
  CHARACTER ( len = * ) title

  CALL imat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  RETURN
END SUBROUTINE imat_transpose_print
SUBROUTINE imat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*******************************************************************************
!
!! IMAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an integer matrix.
!
!  Modified:
!
!    09 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, integer A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: incx = 10
  INTEGER m
  INTEGER n

  INTEGER a(m,n)
  CHARACTER ( len = 7 ) ctemp(incx)
  INTEGER i
  INTEGER i2
  INTEGER i2hi
  INTEGER i2lo
  INTEGER ihi
  INTEGER ilo
  INTEGER inc
  INTEGER j
  INTEGER j2hi
  INTEGER j2lo
  INTEGER jhi
  INTEGER jlo
  CHARACTER ( len = * ) title

  IF ( 0 < LEN_TRIM ( title ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) TRIM ( title )
  END IF

  DO i2lo = MAX ( ilo, 1 ), MIN ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = MIN ( i2hi, m )
    i2hi = MIN ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    WRITE ( *, '(a)' ) ' '

    DO i = i2lo, i2hi
      i2 = i + 1 - i2lo
      WRITE ( ctemp(i2), '(i7)') i
    END DO

    WRITE ( *, '(''  Row '',10a7)' ) ctemp(1:inc)
    WRITE ( *, '(a)' ) '  Col'
    WRITE ( *, '(a)' ) ' '

    j2lo = MAX ( jlo, 1 )
    j2hi = MIN ( jhi, n )

    DO j = j2lo, j2hi

      DO i2 = 1, inc

        i = i2lo - 1 + i2

        WRITE ( ctemp(i2), '(i7)' ) a(i,j)

      END DO

      WRITE ( *, '(i5,1x,10a7)' ) j, ( ctemp(i), i = 1, inc )

    END DO

  END DO

  WRITE ( *, '(a)' ) ' '

  RETURN
END SUBROUTINE imat_transpose_print_some
SUBROUTINE ivec_heap_d ( n, a )

!*******************************************************************************
!
!! IVEC_HEAP_D reorders an array of integers into an descending heap.
!
!  Discussion:
!
!    A descending heap is an array A with the property that, for every index J,
!    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer N, the size of the input array.
!
!    Input/output, integer A(N).
!    On input, an unsorted array.
!    On output, the array has been reordered into a heap.
!
  IMPLICIT NONE

  INTEGER n

  INTEGER a(n)
  INTEGER i
  INTEGER ifree
  INTEGER key
  INTEGER m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  DO i = n/2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = a(i)
    ifree = i

    DO
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      IF ( n < m ) THEN
        EXIT
      END IF
!
!  Does the second position exist?
!
      IF ( m + 1 <= n ) THEN
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
        IF ( a(m) < a(m+1) ) THEN
          m = m + 1
        END IF

      END IF
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      IF ( a(m) <= key ) THEN
        EXIT
      END IF

      a(ifree) = a(m)
      ifree = m

    END DO
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
    a(ifree) = key

  END DO

  RETURN
END SUBROUTINE ivec_heap_d
SUBROUTINE ivec_sort_heap_a ( n, a )

!*******************************************************************************
!
!! IVEC_SORT_HEAP_A ascending sorts an integer array using heap sort.
!
!  Modified:
!
!    15 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the array.
!
!    Input/output, integer A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  IMPLICIT NONE

  INTEGER n

  INTEGER a(n)
  INTEGER n1

  IF ( n <= 1 ) THEN
    RETURN
  END IF
!
!  1: Put A into descending heap form.
!
  CALL ivec_heap_d ( n, a )
!
!  2: Sort A.
!
!  The largest object in the heap is in A(1).
!  Move it to position A(N).
!
  CALL i_swap ( a(1), a(n) )
!
!  Consider the diminished heap of size N1.
!
  DO n1 = n-1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    CALL ivec_heap_d ( n1, a )
!
!  Take the largest object from A(1) and move it to A(N1).
!
    CALL i_swap ( a(1), a(n1) )

  END DO

  RETURN
END SUBROUTINE ivec_sort_heap_a
SUBROUTINE ivec_sorted_unique ( n, a, nuniq )

!*******************************************************************************
!
!! IVEC_SORTED_UNIQUE finds the unique elements in a sorted integer array.
!
!  Modified:
!
!    09 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in A.
!
!    Input/output, integer A(N).  On input, the sorted
!    integer array.  On output, the unique elements in A.
!
!    Output, integer NUNIQ, the number of unique elements in A.
!
  IMPLICIT NONE

  INTEGER n

  INTEGER a(n)
  INTEGER itest
  INTEGER nuniq

  nuniq = 0

  IF ( n <= 0 ) THEN
    RETURN
  END IF

  nuniq = 1

  DO itest = 2, n

    IF ( a(itest) /= a(nuniq) ) THEN
      nuniq = nuniq + 1
      a(nuniq) = a(itest)
    END IF

  END DO

  RETURN
END SUBROUTINE ivec_sorted_unique

INTEGER FUNCTION lrline ( xu, yu, xv1, yv1, xv2, yv2, dv )

!*******************************************************************************
!
!! LRLINE determines if a point is left of, right or, or on a directed line.
!
!  Discussion:
!
!    The directed line is parallel to, and at a signed distance DV from
!    a directed base line from (XV1,YV1) to (XV2,YV2).
!
!  Modified:
!
!    14 July 2001
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Reference:
!
!    Barry Joe,
!    GEOMPACK - a software package for the generation of meshes
!    using geometric algorithms,
!    Advances in Engineering Software,
!    Volume 13, pages 325-331, 1991.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XU, YU, the coordinates of the point whose
!    position relative to the directed line is to be determined.
!
!    Input, real ( kind = 8 ) XV1, YV1, XV2, YV2, the coordinates of two points
!    that determine the directed base line.
!
!    Input, real ( kind = 8 ) DV, the signed distance of the directed line
!    from the directed base line through the points (XV1,YV1) and (XV2,YV2).
!    DV is positive for a line to the left of the base line.
!
!    Output, integer LRLINE, the result:
!    +1, the point is to the right of the directed line;
!     0, the point is on the directed line;
!    -1, the point is to the left of the directed line.
!
  IMPLICIT NONE

  REAL ( kind = 8 ) dv
  REAL ( kind = 8 ) dx
  REAL ( kind = 8 ) dxu
  REAL ( kind = 8 ) dy
  REAL ( kind = 8 ) dyu
  REAL ( kind = 8 ) t
  REAL ( kind = 8 ) tol
  REAL ( kind = 8 ) tolabs
  REAL ( kind = 8 ) xu
  REAL ( kind = 8 ) xv1
  REAL ( kind = 8 ) xv2
  REAL ( kind = 8 ) yu
  REAL ( kind = 8 ) yv1
  REAL ( kind = 8 ) yv2

  tol = 100.0D+00 * EPSILON ( tol )

  dx = xv2 - xv1
  dy = yv2 - yv1
  dxu = xu - xv1
  dyu = yu - yv1

  tolabs = tol * MAX ( ABS ( dx ), ABS ( dy ), ABS ( dxu ), &
    ABS ( dyu ), ABS ( dv ) )

  t = dy * dxu - dx * dyu + dv * SQRT ( dx * dx + dy * dy )

  IF ( tolabs < t ) THEN
    lrline = 1
  ELSE IF ( -tolabs <= t ) THEN
    lrline = 0
  ELSE
    lrline = -1
  END IF

  RETURN
END FUNCTION lrline

SUBROUTINE perm_check ( n, p, ierror )

!*******************************************************************************
!
!! PERM_CHECK checks that a vector represents a permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 1
!    to N occurs among the N entries of the permutation.
!
!  Modified:
!
!    01 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries.
!
!    Input, integer P(N), the array to check.
!
!    Output, integer IERROR, error flag.
!    0, the array represents a permutation.
!    nonzero, the array does not represent a permutation.  The smallest
!    missing value is equal to IERROR.
!
  IMPLICIT NONE

  INTEGER n

  INTEGER ierror
  INTEGER ifind
  INTEGER iseek
  INTEGER p(n)

  ierror = 0

  DO iseek = 1, n

    ierror = iseek

    DO ifind = 1, n
      IF ( p(ifind) == iseek ) THEN
        ierror = 0
        EXIT
      END IF
    END DO

    IF ( ierror /= 0 ) THEN
      RETURN
    END IF

  END DO

  RETURN
END SUBROUTINE perm_check
SUBROUTINE perm_inv ( n, p )

!*******************************************************************************
!
!! PERM_INV inverts a permutation "in place".
!
!  Modified:
!
!    25 July 2000
!
!  Parameters:
!
!    Input, integer N, the number of objects being permuted.
!
!    Input/output, integer P(N), the permutation, in standard index form.
!    On output, P describes the inverse permutation
!
  IMPLICIT NONE

  INTEGER n

  INTEGER i
  INTEGER i0
  INTEGER i1
  INTEGER i2
  INTEGER ierror
  INTEGER is
  INTEGER p(n)

  IF ( n <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'PERM_INV - Fatal error!'
    WRITE ( *, '(a,i8)' ) '  Input value of N = ', n
    STOP
  END IF

  CALL perm_check ( n, p, ierror )

  IF ( ierror /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'PERM_INV - Fatal error!'
    WRITE ( *, '(a)' ) '  The input array does not represent'
    WRITE ( *, '(a)' ) '  a proper permutation.  In particular, the'
    WRITE ( *, '(a,i8)' ) '  array is missing the value ', ierror
    STOP
  END IF

  is = 1

  DO i = 1, n

    i1 = p(i)

    DO WHILE ( i < i1 )
      i2 = p(i1)
      p(i1) = -i2
      i1 = i2
    END DO

    is = -SIGN ( 1, p(i) )
    p(i) = SIGN ( p(i), is )

  END DO

  DO i = 1, n

    i1 = -p(i)

    IF ( 0 <= i1 ) THEN

      i0 = i

      DO

        i2 = p(i1)
        p(i1) = i0

        IF ( i2 < 0 ) THEN
          EXIT
        END IF

        i0 = i1
        i1 = i2

      END DO

    END IF

  END DO

  RETURN
END SUBROUTINE perm_inv

SUBROUTINE points_delaunay_naive_2d ( node_num, node_xy, maxtri, &
  triangle_num, triangle_node )

!*******************************************************************************
!
!! POINTS_DELAUNAY_NAIVE_2D is a naive Delaunay triangulation scheme.
!
!  Discussion:
!
!    This routine is only suitable as a demonstration code for small
!    problems.  Its running time is of order NODE_NUM**4.  Much faster
!    algorithms are available.
!
!    Given a set of nodes in the plane, a triangulation is set of
!    triples of distinct nodes, forming triangles, so that every
!    point within the convex hull of the set of nodes is either
!    one of the nodes, or lies on an edge of one or more triangles,
!    or lies within exactly one triangle.
!
!    A Delaunay triangulation is a triangulation with additional
!    properties.
!
!    NODE_NUM must be at least 3.
!
!  Modified:
!
!    08 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Joseph O'Rourke,
!    Computational Geometry,
!    Cambridge University Press,
!    Second Edition, 1998, page 187.
!
!  Parameters:
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer MAXTRI, the maximum number of triangles.
!
!    Output, integer TRIANGLE_NUM, the number of triangles in the triangulation.
!
!    Output, integer TRIANGLE_NODE(3,MAXTRI), the indices of the triangle nodes.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: dim_num = 2
  INTEGER maxtri
  INTEGER node_num

  LOGICAL flag
  INTEGER i
  INTEGER j
  INTEGER k
  INTEGER m
  REAL ( kind = 8 ) node_xy(dim_num,node_num)
  INTEGER triangle_node(3,maxtri)
  INTEGER triangle_num
  REAL ( kind = 8 ) xn
  REAL ( kind = 8 ) yn
  REAL ( kind = 8 ) z(node_num)
  REAL ( kind = 8 ) zn

  triangle_num = 0

  IF ( node_num < 3 ) THEN
    RETURN
  END IF
!
!  Compute Z = X*X + Y*Y.
!
  z(1:node_num) = node_xy(1,1:node_num)**2 + node_xy(2,1:node_num)**2
!
!  For each triple (I,J,K):
!
  DO i = 1, node_num - 2
    DO j = i+1, node_num
      DO k = i+1, node_num

        IF ( j /= k ) THEN

          xn = ( node_xy(2,j) - node_xy(2,i) ) * ( z(k) - z(i) ) &
             - ( node_xy(2,k) - node_xy(2,i) ) * ( z(j) - z(i) )

          yn = ( node_xy(1,k) - node_xy(1,i) ) * ( z(j) - z(i) ) &
             - ( node_xy(1,j) - node_xy(1,i) ) * ( z(k) - z(i) )

          zn = ( node_xy(1,j) - node_xy(1,i) ) &
             * ( node_xy(2,k) - node_xy(2,i) ) &
             - ( node_xy(1,k) - node_xy(1,i) ) &
             * ( node_xy(2,j) - node_xy(2,i) )

          flag = ( zn < 0.0D+00 )

          IF ( flag ) THEN
            DO m = 1, node_num
              flag = flag .AND. &
                ( ( node_xy(1,m) - node_xy(1,i) ) * xn &
                + ( node_xy(2,m) - node_xy(2,i) ) * yn &
                + ( z(m)   - z(i) )   * zn <= 0.0D+00 )
            END DO
          END IF

          IF ( flag ) THEN
            IF ( triangle_num < maxtri ) THEN
              triangle_num = triangle_num + 1
              triangle_node(1:3,triangle_num) = (/ i, j, k /)
            END IF
          END IF

        END IF

      END DO
    END DO
  END DO

  RETURN
END SUBROUTINE points_delaunay_naive_2d

SUBROUTINE swapec ( i, top, btri, bedg, node_num, node_xy, triangle_num, &
  triangle_node, triangle_neighbor, stack, ierr )

!*******************************************************************************
!
!! SWAPEC swaps diagonal edges until all triangles are Delaunay.
!
!  Discussion:
!
!    The routine swaps diagonal edges in a 2D triangulation, based on
!    the empty circumcircle criterion, until all triangles are Delaunay,
!    given that I is the index of the new vertex added to the triangulation.
!
!  Modified:
!
!    14 July 2001
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Reference:
!
!    Barry Joe,
!    GEOMPACK - a software package for the generation of meshes
!    using geometric algorithms,
!    Advances in Engineering Software,
!    Volume 13, pages 325-331, 1991.
!
!  Parameters:
!
!    Input, integer I, the index of the new vertex.
!
!    Input/output, integer TOP, the index of the top of the stack.
!    On output, TOP is zero.
!
!    Input/output, integer BTRI, BEDG; on input, if positive, are the
!    triangle and edge indices of a boundary edge whose updated indices
!    must be recorded.  On output, these may be updated because of swaps.
!
!    Input, integer NODE_NUM, the number of points.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of
!    the points.
!
!    Input, integer TRIANGLE_NUM, the number of triangles.
!
!    Input/output, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the triangle 
!    incidence list.  May be updated on output because of swaps.
!
!    Input/output, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the triangle
!    neighbor list; negative values are used for links of the counter-clockwise
!    linked list of boundary edges;  May be updated on output because of swaps.
!
!      LINK = -(3*I + J-1) where I, J = triangle, edge index.
!
!    Workspace, integer STACK(MAXST); on input, entries 1 through TOP
!    contain the indices of initial triangles (involving vertex I)
!    put in stack; the edges opposite I should be in interior;  entries
!    TOP+1 through MAXST are used as a stack.
!
!    Output, integer IERR is set to 8 for abnormal return.
!
  IMPLICIT NONE

  INTEGER node_num
  INTEGER triangle_num

  INTEGER a
  INTEGER b
  INTEGER bedg
  INTEGER btri
  INTEGER c
  INTEGER e
  INTEGER ee
  INTEGER em1
  INTEGER ep1
  INTEGER f
  INTEGER fm1
  INTEGER fp1
  INTEGER i
  INTEGER ierr
  INTEGER l
  REAL ( kind = 8 ) node_xy(2,node_num)
  INTEGER r
  INTEGER s
  INTEGER stack(node_num)
  INTEGER swap
  INTEGER t
  INTEGER top
  INTEGER triangle_neighbor(3,triangle_num)
  INTEGER triangle_node(3,triangle_num)
  INTEGER tt
  INTEGER u
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
!
!  Determine whether triangles in stack are Delaunay, and swap
!  diagonal edge of convex quadrilateral if not.
!
  x = node_xy(1,i)
  y = node_xy(2,i)

  DO

    IF ( top <= 0 ) THEN
      EXIT
    END IF

    t = stack(top)
    top = top - 1

    IF ( triangle_node(1,t) == i ) THEN
      e = 2
      b = triangle_node(3,t)
    ELSE IF ( triangle_node(2,t) == i ) THEN
      e = 3
      b = triangle_node(1,t)
    ELSE
      e = 1
      b = triangle_node(2,t)
    END IF

    a = triangle_node(e,t)
    u = triangle_neighbor(e,t)

    IF ( triangle_neighbor(1,u) == t ) THEN
      f = 1
      c = triangle_node(3,u)
    ELSE IF ( triangle_neighbor(2,u) == t ) THEN
      f = 2
      c = triangle_node(1,u)
    ELSE
      f = 3
      c = triangle_node(2,u)
    END IF

    swap = diaedg ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,c), &
      node_xy(2,c), node_xy(1,b), node_xy(2,b) )

    IF ( swap == 1 ) THEN

      em1 = i_wrap ( e - 1, 1, 3 )
      ep1 = i_wrap ( e + 1, 1, 3 )
      fm1 = i_wrap ( f - 1, 1, 3 )
      fp1 = i_wrap ( f + 1, 1, 3 )

      triangle_node(ep1,t) = c
      triangle_node(fp1,u) = i
      r = triangle_neighbor(ep1,t)
      s = triangle_neighbor(fp1,u)
      triangle_neighbor(ep1,t) = u
      triangle_neighbor(fp1,u) = t
      triangle_neighbor(e,t) = s
      triangle_neighbor(f,u) = r

      IF ( 0 < triangle_neighbor(fm1,u) ) THEN
        top = top + 1
        stack(top) = u
      END IF

      IF ( 0 < s ) THEN

        IF ( triangle_neighbor(1,s) == u ) THEN
          triangle_neighbor(1,s) = t
        ELSE IF ( triangle_neighbor(2,s) == u ) THEN
          triangle_neighbor(2,s) = t
        ELSE
          triangle_neighbor(3,s) = t
        END IF

        top = top + 1

        IF ( node_num < top ) THEN
          ierr = 8
          RETURN
        END IF

        stack(top) = t

      ELSE

        IF ( u == btri .AND. fp1 == bedg ) THEN
          btri = t
          bedg = e
        END IF

        l = - ( 3 * t + e - 1 )
        tt = t
        ee = em1

        DO WHILE ( 0 < triangle_neighbor(ee,tt) )

          tt = triangle_neighbor(ee,tt)

          IF ( triangle_node(1,tt) == a ) THEN
            ee = 3
          ELSE IF ( triangle_node(2,tt) == a ) THEN
            ee = 1
          ELSE
            ee = 2
          END IF

        END DO

        triangle_neighbor(ee,tt) = l

      END IF

      IF ( 0 < r ) THEN

        IF ( triangle_neighbor(1,r) == t ) THEN
          triangle_neighbor(1,r) = u
        ELSE IF ( triangle_neighbor(2,r) == t ) THEN
          triangle_neighbor(2,r) = u
        ELSE
          triangle_neighbor(3,r) = u
        END IF

      ELSE

        IF ( t == btri .AND. ep1 == bedg ) THEN
          btri = u
          bedg = f
        END IF

        l = - ( 3 * u + f - 1 )
        tt = u
        ee = fm1

        DO WHILE ( 0 < triangle_neighbor(ee,tt) )

          tt = triangle_neighbor(ee,tt)

          IF ( triangle_node(1,tt) == b ) THEN
            ee = 3
          ELSE IF ( triangle_node(2,tt) == b ) THEN
            ee = 1
          ELSE
            ee = 2
          END IF

        END DO

        triangle_neighbor(ee,tt) = l

      END IF

    END IF

  END DO

  RETURN
END SUBROUTINE swapec
SUBROUTINE timestamp ( )

!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    15 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  IMPLICIT NONE

  CHARACTER ( len = 40 ) string

  CALL timestring ( string )

  WRITE ( *, '(a)' ) TRIM ( string )

  RETURN
END SUBROUTINE timestamp
SUBROUTINE timestring ( string )

!*******************************************************************************
!
!! TIMESTRING writes the current YMDHMS date into a string.
!
!  Example:
!
!    STRING = 'May 31 2001   9:45:54.872 AM'
!
!  Modified:
!
!    15 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) STRING, contains the date information.
!    A character length of 40 should always be sufficient.
!
  IMPLICIT NONE

  CHARACTER ( len = 8 ) ampm
  INTEGER d
  CHARACTER ( len = 8 ) date
  INTEGER h
  INTEGER m
  INTEGER mm
  CHARACTER ( len = 9 ), PARAMETER, DIMENSION(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  INTEGER n
  INTEGER s
  CHARACTER ( len = * ) string
  CHARACTER ( len = 10 ) time
  INTEGER values(8)
  INTEGER y
  CHARACTER ( len = 5 ) zone

  CALL DATE_AND_TIME ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  IF ( h < 12 ) THEN
    ampm = 'AM'
  ELSE IF ( h == 12 ) THEN
    IF ( n == 0 .AND. s == 0 ) THEN
      ampm = 'Noon'
    ELSE
      ampm = 'PM'
    END IF
  ELSE
    h = h - 12
    IF ( h < 12 ) THEN
      ampm = 'PM'
    ELSE IF ( h == 12 ) THEN
      IF ( n == 0 .AND. s == 0 ) THEN
        ampm = 'Midnight'
      ELSE
        ampm = 'AM'
      END IF
    END IF
  END IF

  WRITE ( string, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    TRIM ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, TRIM ( ampm )

  RETURN
END SUBROUTINE timestring
SUBROUTINE triangle_circumcenter_2d ( t, center )

!*******************************************************************************
!
!! TRIANGLE_CIRCUMCENTER_2D computes the circumcenter of a triangle in 2D.
!
!  Discussion:
!
!    The circumcenter of a triangle is the center of the circumcircle, the
!    circle that passes through the three vertices of the triangle.
!
!    The circumcircle contains the triangle, but it is not necessarily the
!    smallest triangle to do so.
!
!    If all angles of the triangle are no greater than 90 degrees, then
!    the center of the circumscribed circle will lie inside the triangle.
!    Otherwise, the center will lie outside the circle.
!
!    The circumcenter is the intersection of the perpendicular bisectors
!    of the sides of the triangle.
!
!    In geometry, the circumcenter of a triangle is often symbolized by "O".
!
!  Modified:
!
!    09 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) CENTER(2), the circumcenter of the triangle.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: dim_num = 2

  REAL ( kind = 8 ) asq
  REAL ( kind = 8 ) bot
  REAL ( kind = 8 ) center(dim_num)
  REAL ( kind = 8 ) csq
  REAL ( kind = 8 ) t(dim_num,3)
  REAL ( kind = 8 ) top(dim_num)

  asq = ( t(1,2) - t(1,1) )**2 + ( t(2,2) - t(2,1) )**2
  csq = ( t(1,3) - t(1,1) )**2 + ( t(2,3) - t(2,1) )**2

  top(1) =  ( t(2,2) - t(2,1) ) * csq - ( t(2,3) - t(2,1) ) * asq
  top(2) =  ( t(1,2) - t(1,1) ) * csq - ( t(1,3) - t(1,1) ) * asq

  bot  =  ( t(2,2) - t(2,1) ) * ( t(1,3) - t(1,1) ) &
        - ( t(2,3) - t(2,1) ) * ( t(1,2) - t(1,1) )

  center(1:2) = t(1:2,1) + 0.5D+00 * top(1:2) / bot

  RETURN
END SUBROUTINE triangle_circumcenter_2d
SUBROUTINE triangulation_order3_plot ( file_name, node_num, node_xy, &
  triangle_num, triangle_node, node_show, triangle_show )

!*******************************************************************************
!
!! TRIANGULATION_ORDER3_PLOT plots a 3-node triangulation of a set of nodes.
!
!  Discussion:
!
!    The triangulation is most usually a Delaunay triangulation,
!    but this is not necessary.
!
!  Modified:
!
!    16 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the output file.
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer TRIANGLE_NUM, the number of triangles.
!
!    Input, integer TRIANGLE_NODE(3,TRIANGLE_NUM), lists, for each triangle,
!    the indices of the nodes that form the vertices of the triangle.
!
!    Input, integer NODE_SHOW,
!    0, do not show nodes;
!    1, show nodes;
!    2, show nodes and label them.
!
!    Input, integer TRIANGLE_SHOW,
!    0, do not show triangles;
!    1, show triangles;
!    2, show triangles and label them.
!
!  Local parameters:
!
!    Local, integer CIRCLE_SIZE, controls the size of the circles depicting
!    the nodes.  Currently set to 5.  3 is pretty small, and 1 is
!    barely visible.
!
  IMPLICIT NONE

  INTEGER node_num
  INTEGER triangle_num

  REAL ( kind = 8 ) ave_x
  REAL ( kind = 8 ) ave_y
  CHARACTER ( len = 40 ) date_time
  INTEGER, PARAMETER :: circle_size = 5
  INTEGER delta
  INTEGER e
  CHARACTER ( len = * ) file_name
  INTEGER file_unit
  INTEGER i
  INTEGER ios
  INTEGER node
  INTEGER node_show
  REAL ( kind = 8 ) node_xy(2,node_num)
  CHARACTER ( len = 40 ) string
  INTEGER triangle
  INTEGER triangle_node(3,triangle_num)
  INTEGER triangle_show
  REAL ( kind = 8 ) x_max
  REAL ( kind = 8 ) x_min
  INTEGER x_ps
  INTEGER :: x_ps_max = 576
  INTEGER :: x_ps_max_clip = 594
  INTEGER :: x_ps_min = 36
  INTEGER :: x_ps_min_clip = 18
  REAL ( kind = 8 ) x_scale
  REAL ( kind = 8 ) y_max
  REAL ( kind = 8 ) y_min
  INTEGER y_ps
  INTEGER :: y_ps_max = 666
  INTEGER :: y_ps_max_clip = 684
  INTEGER :: y_ps_min = 126
  INTEGER :: y_ps_min_clip = 108
  REAL ( kind = 8 ) y_scale

  CALL timestring ( date_time )
!
!  We need to do some figuring here, so that we can determine
!  the range of the data, and hence the height and width
!  of the piece of paper.
!
  x_max = MAXVAL ( node_xy(1,1:node_num) )
  x_min = MINVAL ( node_xy(1,1:node_num) )
  x_scale = x_max - x_min

  x_max = x_max + 0.05D+00 * x_scale
  x_min = x_min - 0.05D+00 * x_scale
  x_scale = x_max - x_min

  y_max = MAXVAL ( node_xy(2,1:node_num) )
  y_min = MINVAL ( node_xy(2,1:node_num) )
  y_scale = y_max - y_min

  y_max = y_max + 0.05D+00 * y_scale
  y_min = y_min - 0.05D+00 * y_scale
  y_scale = y_max - y_min

  IF ( x_scale < y_scale ) THEN

    delta = NINT ( REAL ( x_ps_max - x_ps_min, kind = 8 ) &
      * ( y_scale - x_scale ) / ( 2.0D+00 * y_scale ) )

    x_ps_max = x_ps_max - delta
    x_ps_min = x_ps_min + delta

    x_ps_max_clip = x_ps_max_clip - delta
    x_ps_min_clip = x_ps_min_clip + delta

    x_scale = y_scale

  ELSE IF ( y_scale < x_scale ) THEN

    delta = NINT ( REAL ( y_ps_max - y_ps_min, kind = 8 ) &
      * ( x_scale - y_scale ) / ( 2.0D+00 * x_scale ) )

    y_ps_max      = y_ps_max - delta
    y_ps_min      = y_ps_min + delta

    y_ps_max_clip = y_ps_max_clip - delta
    y_ps_min_clip = y_ps_min_clip + delta

    y_scale = x_scale

  END IF

  CALL get_unit ( file_unit )

  OPEN ( unit = file_unit, file = file_name, status = 'replace', &
    iostat = ios )

  IF ( ios /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'TRIANGULATION_ORDER3_PLOT - Fatal error!'
    WRITE ( *, '(a)' ) '  Can not open output file "', TRIM ( file_name ), '".'
    RETURN
  END IF

  WRITE ( file_unit, '(a)' ) '%!PS-Adobe-3.0 EPSF-3.0'
  WRITE ( file_unit, '(a)' ) '%%Creator: triangulation_order3_plot.f90'
  WRITE ( file_unit, '(a)' ) '%%Title: ' // TRIM ( file_name )
  WRITE ( file_unit, '(a)' ) '%%CreationDate: ' // TRIM ( date_time )
  WRITE ( file_unit, '(a)' ) '%%Pages: 1'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,i3,2x,i3)' ) '%%BoundingBox: ', &
    x_ps_min, y_ps_min, x_ps_max, y_ps_max
  WRITE ( file_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
  WRITE ( file_unit, '(a)' ) '%%LanguageLevel: 1'
  WRITE ( file_unit, '(a)' ) '%%EndComments'
  WRITE ( file_unit, '(a)' ) '%%BeginProlog'
  WRITE ( file_unit, '(a)' ) '/inch {72 mul} def'
  WRITE ( file_unit, '(a)' ) '%%EndProlog'
  WRITE ( file_unit, '(a)' ) '%%Page: 1 1'
  WRITE ( file_unit, '(a)' ) 'save'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  Set the RGB line color to very light gray.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '0.900  0.900  0.900 setrgbcolor'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  Draw a gray border around the page.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) 'newpath'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_min, y_ps_min, ' moveto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_max, y_ps_min, ' lineto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_max, y_ps_max, ' lineto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_min, y_ps_max, ' lineto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_min, y_ps_min, ' lineto'
  WRITE ( file_unit, '(a)' ) 'stroke'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  Set the RGB color to black.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '0.000  0.000  0.000 setrgbcolor'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  Set the font and its size.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '/Times-Roman findfont'
  WRITE ( file_unit, '(a)' ) '0.50 inch scalefont'
  WRITE ( file_unit, '(a)' ) 'setfont'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  Print a title.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  210  702  moveto'
  WRITE ( file_unit, '(a)' ) '%  (Triangulation)  show'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  Define a clipping polygon.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) 'newpath'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
    x_ps_min_clip, y_ps_min_clip, ' moveto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
    x_ps_max_clip, y_ps_min_clip, ' lineto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
    x_ps_max_clip, y_ps_max_clip, ' lineto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
    x_ps_min_clip, y_ps_max_clip, ' lineto'
  WRITE ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
    x_ps_min_clip, y_ps_min_clip, ' lineto'
  WRITE ( file_unit, '(a)' ) 'clip newpath'
!
!  Draw the nodes.
!
  IF ( 1 <= node_show ) THEN
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Draw filled dots at the nodes.'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Set the RGB color to blue.'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '0.000  0.150  0.750 setrgbcolor'
    WRITE ( file_unit, '(a)' ) '%'

    DO node = 1, node_num

      x_ps = INT ( &
        ( ( x_max - node_xy(1,node)         ) * REAL ( x_ps_min, kind = 8 )   &
        + (         node_xy(1,node) - x_min ) * REAL ( x_ps_max, kind = 8 ) ) &
        / ( x_max                   - x_min ) )

      y_ps = INT ( &
        ( ( y_max - node_xy(2,node)         ) * REAL ( y_ps_min, kind = 8 )   &
        + (         node_xy(2,node) - y_min ) * REAL ( y_ps_max, kind = 8 ) ) &
        / ( y_max                   - y_min ) )

      WRITE ( file_unit, '(a,i4,2x,i4,2x,i4,2x,a)' ) 'newpath ', x_ps, y_ps, &
        circle_size, '0 360 arc closepath fill'

    END DO

  END IF
!
!  Label the nodes.
!
  IF ( 2 <= node_show ) THEN

    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Label the nodes:'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Set the RGB color to darker blue.'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '0.000  0.250  0.850 setrgbcolor'
    WRITE ( file_unit, '(a)' ) '/Times-Roman findfont'
    WRITE ( file_unit, '(a)' ) '0.20 inch scalefont'
    WRITE ( file_unit, '(a)' ) 'setfont'
    WRITE ( file_unit, '(a)' ) '%'

    DO node = 1, node_num

      x_ps = INT ( &
        ( ( x_max - node_xy(1,node)         ) * REAL ( x_ps_min, kind = 8 )   &
        + (       + node_xy(1,node) - x_min ) * REAL ( x_ps_max, kind = 8 ) ) &
        / ( x_max                   - x_min ) )

      y_ps = INT ( &
        ( ( y_max - node_xy(2,node)         ) * REAL ( y_ps_min, kind = 8 )   &
        + (         node_xy(2,node) - y_min ) * REAL ( y_ps_max, kind = 8 ) ) &
        / ( y_max                   - y_min ) )

      WRITE ( string, '(i4)' ) node
      string = ADJUSTL ( string )

      WRITE ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps+5, &
        ' moveto (' // TRIM ( string ) // ') show'

    END DO

  END IF
!
!  Draw the triangles.
!
  IF ( 1 <= triangle_show ) THEN
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Set the RGB color to red.'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '0.900  0.200  0.100 setrgbcolor'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Draw the triangles.'
    WRITE ( file_unit, '(a)' ) '%'

    DO triangle = 1, triangle_num

      WRITE ( file_unit, '(a)' ) 'newpath'

      DO i = 1, 4

        e = i_wrap ( i, 1, 3 )

        node = triangle_node(e,triangle)

        x_ps = INT ( &
          ( ( x_max - node_xy(1,node)         ) * REAL ( x_ps_min, kind = 8 )   &
          + (         node_xy(1,node) - x_min ) * REAL ( x_ps_max, kind = 8 ) ) &
          / ( x_max                   - x_min ) )

        y_ps = INT ( &
          ( ( y_max - node_xy(2,node)         ) * REAL ( y_ps_min, kind = 8 )   &
          + (         node_xy(2,node) - y_min ) * REAL ( y_ps_max, kind = 8 ) ) &
          / ( y_max                   - y_min ) )

        IF ( i == 1 ) THEN
          WRITE ( file_unit, '(i3,2x,i3,2x,a)' ) x_ps, y_ps, ' moveto'
        ELSE
          WRITE ( file_unit, '(i3,2x,i3,2x,a)' ) x_ps, y_ps, ' lineto'
        END IF

      END DO

      WRITE ( file_unit, '(a)' ) 'stroke'

    END DO

  END IF
!
!  Label the triangles.
!
  IF ( 2 <= triangle_show ) THEN

    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Label the triangles:'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '%  Set the RGB color to darker red.'
    WRITE ( file_unit, '(a)' ) '%'
    WRITE ( file_unit, '(a)' ) '0.950  0.250  0.150 setrgbcolor'
    WRITE ( file_unit, '(a)' ) '/Times-Roman findfont'
    WRITE ( file_unit, '(a)' ) '0.20 inch scalefont'
    WRITE ( file_unit, '(a)' ) 'setfont'
    WRITE ( file_unit, '(a)' ) '%'

    DO triangle = 1, triangle_num

      ave_x = 0.0D+00
      ave_y = 0.0D+00

      DO i = 1, 3

        node = triangle_node(i,triangle)

        ave_x = ave_x + node_xy(1,node)
        ave_y = ave_y + node_xy(2,node)

      END DO

      ave_x = ave_x / 3.0D+00
      ave_y = ave_y / 3.0D+00

      x_ps = INT ( &
        ( ( x_max - ave_x         ) * REAL ( x_ps_min, kind = 8 )   &
        + (       + ave_x - x_min ) * REAL ( x_ps_max, kind = 8 ) ) &
        / ( x_max         - x_min ) )

      y_ps = INT ( &
        ( ( y_max - ave_y         ) * REAL ( y_ps_min, kind = 8 )   &
        + (         ave_y - y_min ) * REAL ( y_ps_max, kind = 8 ) ) &
        / ( y_max         - y_min ) )

      WRITE ( string, '(i4)' ) triangle
      string = ADJUSTL ( string )

      WRITE ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps, ' moveto (' &
        // TRIM ( string ) // ') show'

    END DO

  END IF

  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) 'restore  showpage'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%  End of page.'
  WRITE ( file_unit, '(a)' ) '%'
  WRITE ( file_unit, '(a)' ) '%%Trailer'
  WRITE ( file_unit, '(a)' ) '%%EOF'
  CLOSE ( unit = file_unit )

  RETURN
END SUBROUTINE triangulation_order3_plot

SUBROUTINE triangulation_order3_print ( node_num, triangle_num, node_xy, &
  triangle_node, triangle_neighbor )

!*******************************************************************************
!
!! TRIANGULATION_ORDER3_PRINT prints out information defining a Delaunay triangulation.
!
!  Discussion:
!
!    Triangulations created by DTRIS2 include extra information encoded
!    in the negative values of TRIANGLE_NEIGHBOR.
!
!    Because some of the nodes counted in NODE_NUM may not actually be
!    used in the triangulation, I needed to compute the true number
!    of vertices.  I added this calculation on 13 October 2001.
!
!  Modified:
!
!    26 November 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, integer TRIANGLE_NUM, the number of triangles.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes that make up the
!    triangles.
!
!    Input, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the triangle neighbors on
!    each side.  If there is no triangle neighbor on a particular side, the
!    value of TRIANGLE_NEIGHBOR should be negative.  If the triangulation
!    data was created by DTRIS2, then there is more information encoded
!    in the negative values.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: dim_num = 2
  INTEGER node_num
  INTEGER triangle_num

  INTEGER boundary_num
  INTEGER i
  INTEGER j
  INTEGER k
  INTEGER n1
  INTEGER n2
  REAL ( kind = 8 ) node_xy(dim_num,node_num)
  INTEGER s
  LOGICAL skip
  INTEGER t
  INTEGER triangle_node(3,triangle_num)
  INTEGER triangle_neighbor(3,triangle_num)
  INTEGER, ALLOCATABLE, DIMENSION ( : ) :: vertex_list
  INTEGER vertex_num

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) 'TRIANGULATION_ORDER3_PRINT'
  WRITE ( *, '(a)' ) '  Information defining an order3 triangulation.'
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a,i8)' ) '  The number of nodes is ', node_num

  CALL dmat_transpose_print ( dim_num, node_num, node_xy, '  Node coordinates' )

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a,i8)' ) '  The number of triangles is ', triangle_num
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Sets of three nodes are used as vertices of'
  WRITE ( *, '(a)' ) '  the triangles.  For each triangle, the nodes'
  WRITE ( *, '(a)' ) '  are listed in counterclockwise order.'

  CALL imat_transpose_print ( 3, triangle_num, triangle_node, &
    '  Triangle nodes:' )

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  On each side of a given triangle, there is either'
  WRITE ( *, '(a)' ) '  another triangle, or a piece of the convex hull.'
  WRITE ( *, '(a)' ) '  For each triangle, we list the indices of the three'
  WRITE ( *, '(a)' ) '  neighbors, or (if negative) the codes of the'
  WRITE ( *, '(a)' ) '  segments of the convex hull.'

  CALL imat_transpose_print ( 3, triangle_num, triangle_neighbor, &
    '  Triangle neighbors' )
!
!  Determine the number of vertices.
!
  ALLOCATE ( vertex_list(1:3*triangle_num) )

  vertex_list(1:3*triangle_num) = RESHAPE ( triangle_node(1:3,1:triangle_num), &
    (/ 3*triangle_num /) )

  CALL ivec_sort_heap_a ( 3*triangle_num, vertex_list )

  CALL ivec_sorted_unique ( 3*triangle_num, vertex_list, vertex_num )

  DEALLOCATE ( vertex_list )
!
!  Determine the number of boundary points.
!
  boundary_num = 2 * vertex_num - triangle_num - 2

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a,i8)' ) '  The number of boundary points is ', boundary_num

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  The segments that make up the convex hull can be'
  WRITE ( *, '(a)' ) '  determined from the negative entries of the triangle'
  WRITE ( *, '(a)' ) '  neighbor list.'
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '     #   Tri  Side    N1    N2'
  WRITE ( *, '(a)' ) ' '

  skip = .FALSE.

  k = 0

  DO i = 1, triangle_num

    DO j = 1, 3

      IF ( triangle_neighbor(j,i) < 0 ) THEN
        s = - triangle_neighbor(j,i)
        t = s / 3

        IF ( t < 1 .OR. triangle_num < t ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) '  Sorry, this data does not use the DTRIS2'
          WRITE ( *, '(a)' ) '  convention for convex hull segments.'
          skip = .TRUE.
          EXIT
        END IF

        s = MOD ( s, 3 ) + 1
        k = k + 1
        n1 = triangle_node(s,t)
        n2 = triangle_node(i_wrap(s+1,1,3),t)
        WRITE ( *, '(2x,i4,2x,i4,2x,i4,2x,i4,2x,i4)' ) k, t, s, n1, n2
      END IF

    END DO

    IF ( skip ) THEN
      EXIT
    END IF

  END DO

  RETURN
END SUBROUTINE triangulation_order3_print

SUBROUTINE vbedg ( x, y, node_num, node_xy, triangle_num, triangle_node, &
  triangle_neighbor, ltri, ledg, rtri, redg )

!*******************************************************************************
!
!! VBEDG determines which boundary edges are visible to a point.
!
!  Discussion:
!
!    The point (X,Y) is assumed to be outside the convex hull of the
!    region covered by the 2D triangulation.
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Reference:
!
!    Barry Joe,
!    GEOMPACK - a software package for the generation of meshes
!    using geometric algorithms,
!    Advances in Engineering Software,
!    Volume 13, pages 325-331, 1991.
!
!  Modified:
!
!    25 August 2001
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, the coordinates of a point outside the
!    convex hull of the current triangulation.
!
!    Input, integer NODE_NUM, the number of points.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the
!    vertices.
!
!    Input, integer TRIANGLE_NUM, the number of triangles.
!
!    Input, integer TRIANGLE_NODE(3,TRIANGLE_NUM), the triangle incidence list.
!
!    Input, integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the triangle neighbor
!    list; negative values are used for links of a counter clockwise linked
!    list of boundary edges;
!      LINK = -(3*I + J-1) where I, J = triangle, edge index.
!
!    Input/output, integer LTRI, LEDG.  If LTRI /= 0 then these values are
!    assumed to be already computed and are not changed, else they are updated.
!    On output, LTRI is the index of boundary triangle to the left of the
!    leftmost boundary triangle visible from (X,Y), and LEDG is the boundary
!    edge of triangle LTRI to the left of the leftmost boundary edge visible
!    from (X,Y).  1 <= LEDG <= 3.
!
!    Input/output, integer RTRI.  On input, the index of the boundary triangle
!    to begin the search at.  On output, the index of the rightmost boundary
!    triangle visible from (X,Y).
!
!    Input/output, integer REDG, the edge of triangle RTRI that is visible
!    from (X,Y).  1 <= REDG <= 3.
!
  IMPLICIT NONE

  INTEGER, PARAMETER :: dim_num = 2
  INTEGER node_num
  INTEGER triangle_num

  INTEGER a
  INTEGER b
  INTEGER e
  INTEGER l
  LOGICAL ldone
  INTEGER ledg
  INTEGER lr
  INTEGER ltri
  REAL ( kind = 8 ) node_xy(2,node_num)
  INTEGER redg
  INTEGER rtri
  INTEGER t
  INTEGER triangle_neighbor(3,triangle_num)
  INTEGER triangle_node(3,triangle_num)
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
!
!  Find the rightmost visible boundary edge using links, then possibly
!  leftmost visible boundary edge using triangle neighbor information.
!
  IF ( ltri == 0 ) THEN
    ldone = .FALSE.
    ltri = rtri
    ledg = redg
  ELSE
    ldone = .TRUE.
  END IF

  DO

    l = -triangle_neighbor(redg,rtri)
    t = l / 3
    e = MOD ( l, 3 ) + 1
    a = triangle_node(e,t)

    IF ( e <= 2 ) THEN
      b = triangle_node(e+1,t)
    ELSE
      b = triangle_node(1,t)
    END IF

    lr = lrline ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,b), &
      node_xy(2,b), 0.0D+00 )

    IF ( lr <= 0 ) THEN
      EXIT
    END IF

    rtri = t
    redg = e

  END DO

  IF ( ldone ) THEN
    RETURN
  END IF

  t = ltri
  e = ledg

  DO

    b = triangle_node(e,t)
    e = i_wrap ( e-1, 1, 3 )

    DO WHILE ( 0 < triangle_neighbor(e,t) )

      t = triangle_neighbor(e,t)

      IF ( triangle_node(1,t) == b ) THEN
        e = 3
      ELSE IF ( triangle_node(2,t) == b ) THEN
        e = 1
      ELSE
        e = 2
      END IF

    END DO

    a = triangle_node(e,t)

    lr = lrline ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,b), &
      node_xy(2,b), 0.0D+00 )

    IF ( lr <= 0 ) THEN
      EXIT
    END IF

  END DO

  ltri = t
  ledg = e

  RETURN
END SUBROUTINE vbedg


END MODULE GEOMPACK
