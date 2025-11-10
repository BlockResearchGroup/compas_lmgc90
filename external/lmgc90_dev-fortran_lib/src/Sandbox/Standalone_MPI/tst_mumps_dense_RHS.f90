PROGRAM MUMPS_EXAMPLE

 USE MPI

 implicit none

 INCLUDE "dmumps_struc.h"

  TYPE (DMUMPS_STRUC) id
  INTEGER I, nb_procs, rang, code, COMM_MYSELF,nb_procs_myself
  integer, dimension(12) :: IR,JR
  real(kind=8), dimension(12) :: A
  real(kind=8), dimension(5) :: RHS
  

  CALL MPI_INIT(code)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,nb_procs,code)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,rang,code)

  call MPI_COMM_SPLIT(MPI_COMM_WORLD,rang,rang,COMM_MYSELF,code) 
  CALL MPI_COMM_SIZE(COMM_MYSELF,nb_procs_myself,code)
  print*, "rang =", rang, ": nb_procs_myself = ", nb_procs_myself


  ! Define a communicator for the package
  id%COMM = COMM_MYSELF
  ! Ask for unsymmetric code
  id%SYM = 0
  ! Host working
  id%PAR = 1
  ! Initialize an instance of the package
  id%JOB = -1
  CALL DMUMPS(id)
  ! Define problem on the host (processor 0)
  IF ( (id%MYID .eq. 0) ) THEN

     print*, "rang = ", rang, "je lis les données"

     id%N = 5
     id%NZ = 12
     ALLOCATE( id%IRN ( id%NZ ) )
     ALLOCATE( id%JCN ( id%NZ ) )
     ALLOCATE( id%A( id%NZ ) )
     ALLOCATE( id%RHS ( id%N ) )


     IR(1) = 1 
     IR(2) = 2
     IR(3) = 4
     IR(4) = 5
     IR(5) = 2
     IR(6) = 1
     IR(7) = 5
     IR(8) = 3
     IR(9) = 2
     IR(10) = 3
     IR(11) = 1
     IR(12) = 3

     JR(1) = 2 
     JR(2) = 3
     JR(3) = 3
     JR(4) = 5
     JR(5) = 1
     JR(6) = 1
     JR(7) = 2
     JR(8) = 4
     JR(9) = 5
     JR(10) = 2
     JR(11) = 3
     JR(12) = 3

     A(1) = 3.0
     A(2) = -3.0
     A(3) = 2.0
     A(4) = 1.0
     A(5) = 3.0
     A(6) = 2.0
     A(7) = 4.0
     A(8) = 2.0
     A(9) = 6.0
     A(10) = -1.0
     A(11) = 4.0
     A(12) = 1.0

     RHS(1) = 20.0
     RHS(2) = 24.0
     RHS(3) = 9.0
     RHS(4) = 0.0
     RHS(5) = 13.0

     id%IRN(:)= IR(:)
     id%JCN(:)= JR(:)
     id%A  (:)=  A(:)
     id%RHS(:)=RHS(:)*(rang+1)

  END IF
  ! Call package for solution
  id%JOB = 6
  CALL DMUMPS(id)
  ! Solution has been assembled on the host
  IF ( id%MYID .eq. 0 ) THEN
     WRITE( 6, * ) " Solution is ",(id%RHS(I),I=1,id%N)
  END IF
  ! Deallocate user data
  IF ( id%MYID .eq. 0 )THEN
     DEALLOCATE( id%IRN )
     DEALLOCATE( id%JCN )
     DEALLOCATE( id%A )
     DEALLOCATE( id%RHS )
  END IF
  ! Destroy the instance (deallocate internal data structures)
  id%JOB = -2
  CALL DMUMPS(id)
  CALL MPI_FINALIZE(code)
  STOP

END PROGRAM MUMPS_EXAMPLE
