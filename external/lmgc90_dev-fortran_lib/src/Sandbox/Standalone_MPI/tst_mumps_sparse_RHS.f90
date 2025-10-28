PROGRAM MUMPS_EXAMPLE_SPARSE_RHS

 USE MPI

 implicit none

 INCLUDE "dmumps_struc.h"

  TYPE (DMUMPS_STRUC) id
  INTEGER i, nb_procs, rang, code, COMM_MYSELF,nb_procs_myself
  integer, dimension(12) :: IR,JR
  real(kind=8), dimension(12) :: A
  real(kind=8), dimension(4) :: IRHS, RHS
  

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
  ! Ask for symmetric code
  !id%SYM = 1
  ! Host working
  id%PAR = 1
  ! Initialize an instance of the package
  id%JOB = -1
  CALL DMUMPS(id)
  ! Define problem on the host (processor 0)
  IF ( (id%MYID .eq. 0) ) THEN

     print*, "rang = ", rang, "je lis les données"

     id%N  = 4
     id%NZ = 12

     id%NZ_RHS = 4
     id%NRHS   = 1


     ALLOCATE( id%IRN ( id%NZ ) )
     ALLOCATE( id%JCN ( id%NZ ) )
     ALLOCATE( id%A( id%NZ ) )

     ALLOCATE( id%RHS ( id%N ) )

     ALLOCATE( id%RHS_SPARSE ( id%NZ_RHS ) )
     ALLOCATE( id%IRHS_SPARSE ( id%NZ_RHS ) )
     ALLOCATE( id%IRHS_PTR ( id%NRHS + 1 ) )

!! termes hors diag / 2.d0
!     IR(1) = 1 ; JR(1) = 1 ; A(1) = 2.d0
!     IR(2) = 2 ; JR(2) = 1 ; A(2) = -1.d0/ 2.d0
!     IR(3) = 4 ; JR(3) = 1 ; A(3) = 0.d0 / 2.d0 
!     IR(4) = 2 ; JR(4) = 2 ; A(4) = 2.d0
!     IR(5) = 3 ; JR(5) = 2 ; A(5) = -1.d0/ 2.d0
!     IR(6) = 3 ; JR(6) = 3 ; A(6) = 2.d0
!     IR(7) = 4 ; JR(7) = 3 ; A(7) = -1.d0/ 2.d0
!     IR(8) = 4 ; JR(8) = 4 ; A(8) = 2.d0
!     IR(9) = 1 ; JR(9) = 2 ; A(9) = -1.d0/ 2.d0
!     IR(10) = 2; JR(10) = 3; A(10) =-1.d0/ 2.d0
!     IR(11) = 1; JR(11) = 4; A(11) =0.d0 / 2.d0
!     IR(12) = 3; JR(12) = 4; A(12) =-1.d0/ 2.d0

! termes diag sup classes en dernier
!     IR(1) = 1 ; JR(1) = 1 ; A(1) = 2.d0
!     IR(2) = 2 ; JR(2) = 1 ; A(2) = -1.d0
!     IR(3) = 4 ; JR(3) = 1 ; A(3) = 0.d0 
!     IR(4) = 2 ; JR(4) = 2 ; A(4) = 2.d0 
!     IR(5) = 3 ; JR(5) = 2 ; A(5) = -1.d0   
!     IR(6) = 3 ; JR(6) = 3 ; A(6) = 2.d0
!     IR(7) = 4 ; JR(7) = 3 ; A(7) = -1.d0
!     IR(8) = 4 ; JR(8) = 4 ; A(8) = 2.d0
!     IR(9) = 1 ; JR(9) = 2 ; A(9) = -1.d0  
!     IR(10) = 2; JR(10) = 3; A(10) = -1.d0
!     IR(11) = 1; JR(11) = 4; A(11) = 0.d0
!     IR(12) = 3; JR(12) = 4; A(12) = -1.d0

! termes diag inf classes en dernier
     IR(1) = 1 ; JR(1) = 1 ; A(1) = 2.d0
     IR(2) = 1 ; JR(2) = 2 ; A(2) = -1.d0
     IR(3) = 1 ; JR(3) = 4 ; A(3) = 0.5d0
     IR(4) = 2 ; JR(4) = 2 ; A(4) = 2.d0 
     IR(5) = 2 ; JR(5) = 3 ; A(5) = -1.d0
     IR(6) = 3 ; JR(6) = 3 ; A(6) = 2.d0
     IR(7) = 3 ; JR(7) = 4 ; A(7) = -1.d0
     IR(8) = 4 ; JR(8) = 4 ; A(8) = 2.d0
     IR(9) = 2 ; JR(9) = 1 ; A(9) = -1.d0 
     IR(10) = 3; JR(10) = 2; A(10) = -1.d0
     IR(11) = 4; JR(11) = 1; A(11) = 0.5d0
     IR(12) = 4; JR(12) = 3; A(12) = -1.d0

     id%IRN(:)= IR(:)
     id%JCN(:)= JR(:)
     id%A  (:)=  A(:)

     IRHS(1) = 1; RHS(1) = 20.0
     IRHS(2) = 2; RHS(2) = 24.0
     IRHS(3) = 3; RHS(3) = 9.0
     IRHS(4) = 4; RHS(4) = 6.0
     !IRHS(5) = 5; RHS(5) =1.d0 ! 13.0

     id%RHS_SPARSE(:)=RHS(:)*(rang+1)
     id%IRHS_SPARSE(:)=IRHS(:)
     id%IRHS_PTR(1) = 1
     id%IRHS_PTR(id%NRHS+1) = id%NZ_RHS+1
     id%RHS=0.d0

  END IF
  ! sparse RHS
  id%ICNTL(20) = 1

  ! Call package for solution
  id%JOB = 6
  CALL DMUMPS(id)
  ! Solution has been assembled on the host
  IF ( id%MYID .eq. 0 ) THEN
     WRITE( 6, * ) " Solution is ",(id%RHS(i),i=1,id%N)
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

END PROGRAM MUMPS_EXAMPLE_SPARSE_RHS
