!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program 
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
!
! This software is governed by the CeCILL license under French law and
! abiding by the rules of distribution of free software.  You can  use, 
! modify and/ or redistribute the software under the terms of the CeCILL
! license as circulated by CEA, CNRS and INRIA at the following URL
! "http://www.cecill.info". 
!
! As a counterpart to the access to the source code and  rights to copy,
! modify and redistribute granted by the license, users are provided only
! with a limited warranty  and the software's author,  the holder of the
! economic rights,  and the successive licensors  have only  limited
! liability. 
!
! In this respect, the user's attention is drawn to the risks associated
! with loading,  using,  modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean  that it is complicated to manipulate,  and  that  also
! therefore means  that it is reserved for developers  and  experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards their
! requirements in conditions enabling the security of their systems and/or 
! data to be ensured and,  more generally, to use and operate it in the 
! same conditions as regards security. 
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
! To report bugs, suggest enhancements, etc. to the Authors, contact
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================
MODULE a_DOF
                               
  !! PURPOSE
  !!  abstract class DOF. Treatment of all DOF
  !!  This module defines data type and methods and type

  use paranoid_checks

  USE overall
  USE utilities
  USE parameters

  ! generic node type -----------------------------------------------------
 
  TYPE T_nodty 
     private
     integer :: nodID
     
  END TYPE T_nodty
  
  ! driven degrees of freedom type ----------------------------------------
  
  TYPE  T_driven_dof
     
     LOGICAL          :: is_standard
     
     INTEGER          :: bdynb
     CHARACTER(len=5) :: nodty
     INTEGER          :: nodnb
     INTEGER          :: dofnb
     REAL(kind=8)     :: CT  
     REAL(kind=8)     :: AMP
     REAL(kind=8)     :: OMEGA
     REAL(kind=8)     :: PHI
     REAL(kind=8)     :: RAMPI
     REAL(kind=8)     :: RAMP
     
     TYPE(G_evolution) :: time_evolution  
     
     logical          :: is_active

  END TYPE T_driven_dof
  
  ! one can recover the nbdof value either giving a nodty
  ! or an ID
  
  !
  INTERFACE owner_of_a_driven_dof
     MODULE PROCEDURE owner_of_a_driven_dof_FE,owner_of_a_driven_dof_RIGID
  END INTERFACE
  
  INTERFACE read_a_driven_dof
     MODULE PROCEDURE read_a_driven_dof_FE, read_a_driven_dof_RIGID
  END INTERFACE
  
CONTAINS
  
!-------------------------------------------------------------------------------
 subroutine new_nodty(nodty,clin)

   IMPLICIT NONE
   TYPE(T_nodty) :: nodty
   CHARACTER(len=5) :: clin

   nodty%nodID = get_node_id_from_name(clin)

 END subroutine new_nodty
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 integer FUNCTION get_nodID(nodty)

   IMPLICIT NONE
   TYPE(T_nodty) :: nodty

   get_nodID = nodty%nodID

 END FUNCTION get_nodID
!-------------------------------------------------------------------------------
 CHARACTER(len=5) FUNCTION get_nodNAME(nodty)

   IMPLICIT NONE
   TYPE(T_nodty) :: nodty

   get_nodNAME=get_node_name_from_id(nodty%nodID)

 END FUNCTION get_nodNAME
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 LOGICAL FUNCTION is_a_nodty(clin)

   IMPLICIT NONE
   CHARACTER(len=5) :: clin
  
   is_a_nodty = .FALSE.
   if (get_node_id_from_name(clin) /= -99) is_a_nodty = .TRUE.

 END FUNCTION is_a_nodty
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 INTEGER FUNCTION nbdof_a_nodty(nodty)

   IMPLICIT NONE
   TYPE (T_nodty) :: nodty

   nbdof_a_nodty = nodty%nodID

 END FUNCTION nbdof_a_nodty
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
 CHARACTER(len=5) FUNCTION get_nodty_for_nbdof(nbdof)

   IMPLICIT NONE
   INTEGER           :: nbdof
                            !12345678901234567890123456
   CHARACTER(len=26) :: IAM='a_DOF::get_nodty_for_nbdof'
 
   get_nodty_for_nbdof = get_node_name_from_id(nbdof)


   if (get_nodty_for_nbdof == 'xxxxx') &
      CALL LOGMES('Error '//IAM//': No node type for the given nbdof')


 END FUNCTION get_nodty_for_nbdof
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE read_a_nodty(clin,vect,chnod)

   IMPLICIT NONE
   CHARACTER(len=*),intent(in) :: clin     
   REAL(kind=8),DIMENSION(:) :: vect
   CHARACTER(len=5),OPTIONAL :: chnod
   CHARACTER(len=5)          :: chaine
                            !1234567890123456789
   CHARACTER(len=19) :: IAM='a_DOF::read_a_nodty'
   !
   IF (PRESENT(chnod)) THEN
     chaine=chnod
   ELSE
     chaine=clin(2:6)
   ENDIF 
   SELECT CASE(chaine)
     CASE('NO1xx') 
       READ(clin(35:48),'(D14.7)') vect(1)
     CASE('NO2xx') 
       READ(clin(35:48),'(D14.7)') vect(1)
       READ(clin(56:69),'(D14.7)') vect(2)
     CASE('NO3xx') 
       READ(clin(35:48),'(D14.7)') vect(1)
       READ(clin(56:69),'(D14.7)') vect(2)
       READ(clin(77:90),'(D14.7)') vect(3)
     CASE('NO4xx','NO4Px') 
       READ(clin(35:48),'(D14.7)') vect(1)
       READ(clin(56:69),'(D14.7)') vect(2)
       READ(clin(77:90),'(D14.7)') vect(3)
       IF( .NOT. read_G_clin()) THEN 
         CALL FATERR(IAM,'error reading NO4xx or NO4Px')
       END IF
       READ(G_clin(35:48),'(D14.7)') vect(4)
     CASE('NO5xx')      
       READ(clin(35:48),'(D14.7)') vect(1)
       READ(clin(56:69),'(D14.7)') vect(2)
       READ(clin(77:90),'(D14.7)') vect(3)
       IF( .NOT. read_G_clin()) THEN 
         CALL FATERR(IAM,'error reading NO5xx')
       END IF
       READ(G_clin(35:48),'(D14.7)') vect(4)
       READ(G_clin(56:69),'(D14.7)') vect(5)
     CASE('NO6xx')
       READ(clin(35:48),'(D14.7)') vect(1)
       READ(clin(56:69),'(D14.7)') vect(2)
       READ(clin(77:90),'(D14.7)') vect(3) 
       IF( .NOT. read_G_clin()) THEN 
         CALL FATERR(IAM,'error reading NO6xx')
       END IF
       READ(G_clin(35:48),'(D14.7)') vect(4)
       READ(G_clin(56:69),'(D14.7)') vect(5)
       READ(G_clin(77:90),'(D14.7)') vect(6)
     CASE default
       call faterr('a_DOF::read_a_nodty','read NOpxx p>6 : sorry not implemented yet')
   END SELECT

 END SUBROUTINE read_a_nodty
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE G_read_a_nodty(vect,chnod)

   IMPLICIT NONE
   REAL(kind=8),DIMENSION(:) :: vect
   CHARACTER(len=5),OPTIONAL :: chnod
   CHARACTER(len=5)          :: chaine
   CHARACTER(len=21)         :: IAM='a_DOF::G_read_a_nodty'
   !
   IF (PRESENT(chnod)) THEN
     chaine=chnod
   ELSE
     chaine=G_clin(2:6)
   ENDIF      
   !
   SELECT CASE(chaine)
     CASE('NO1xx') 
       READ(G_clin(35:48),'(D14.7)') vect(1)
     CASE('NO2xx') 
       READ(G_clin(35:48),'(D14.7)') vect(1)
       READ(G_clin(56:69),'(D14.7)') vect(2)
     CASE('NO3xx') 
       READ(G_clin(35:48),'(D14.7)') vect(1)
       READ(G_clin(56:69),'(D14.7)') vect(2)
       READ(G_clin(77:90),'(D14.7)') vect(3)
     CASE('NO4xx','NO4Px') 
       READ(G_clin(35:48),'(D14.7)') vect(1)
       READ(G_clin(56:69),'(D14.7)') vect(2)
       READ(G_clin(77:90),'(D14.7)') vect(3)
       IF( .NOT. read_G_clin()) THEN 
         CALL FATERR(IAM,'error reading NO4xx or NO4Px')
       END IF
       READ(G_clin(35:48),'(D14.7)') vect(4)
     CASE('NO5xx') 
       READ(G_clin(35:48),'(D14.7)') vect(1)
       READ(G_clin(56:69),'(D14.7)') vect(2)
       READ(G_clin(77:90),'(D14.7)') vect(3)
       IF( .NOT. read_G_clin()) THEN 
         CALL FATERR(IAM,'error reading NO5xx')
       END IF
       READ(G_clin(35:48),'(D14.7)') vect(4)
       READ(G_clin(56:69),'(D14.7)') vect(5)
     CASE('NO6xx') 
       READ(G_clin(35:48),'(D14.7)') vect(1)
       READ(G_clin(56:69),'(D14.7)') vect(2)
       READ(G_clin(77:90),'(D14.7)') vect(3)
       IF( .NOT. read_G_clin()) THEN 
         CALL FATERR(IAM,'error reading NO6xx')
       END IF
       READ(G_clin(35:48),'(D14.7)') vect(4)
       READ(G_clin(56:69),'(D14.7)') vect(5)
       READ(G_clin(77:90),'(D14.7)') vect(6)
     CASE default
   END SELECT

 END SUBROUTINE G_read_a_nodty
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE write_a_nodty(clin,id,vect,TYPE,nfich)

   IMPLICIT NONE
   CHARACTER(len=5),intent(in) :: clin
   CHARACTER(len=3),intent(in) :: TYPE
   INTEGER                   :: id,nfich     
   REAL(kind=8),DIMENSION(:) :: vect
   !
   character(len=20)         :: IAM
         !12345678901234567890
   IAM = 'a_DOF::write_a_nodty'

   SELECT CASE(clin)
     CASE('NO1xx')
       SELECT CASE(TYPE)
         CASE('coo')
           WRITE(nfich,127) clin,id,'coo1=',vect(1)
         CASE('X  ')
           WRITE(nfich,127) clin,id,'X(1)=',vect(1)
         CASE('T  ')
           WRITE(nfich,127) clin,id,'T(1)=',vect(1)
         CASE('V  ')
           WRITE(nfich,128)         'V(1)=',vect(1)
         CASE('R/H')
           WRITE(nfich,127) clin,id,'R1/H=',vect(1)
         CASE('Z  ')
           WRITE(nfich,127) clin,id,'Z(1)=',vect(1)
         CASE default
           call faterr(IAM,'unknown type for NO1xx : '//TYPE)
       END SELECT
     CASE('NO2xx')
       SELECT CASE(TYPE)
         CASE('coo')
           WRITE(nfich,129) clin,id,'coo1=',vect(1),'coo2=',vect(2)
         CASE('X  ')
           WRITE(nfich,129) clin,id,'X(1)=',vect(1),'X(2)=',vect(2)
         CASE('V  ')
           WRITE(nfich,130)         'V(1)=',vect(1),'V(2)=',vect(2)
         CASE('R/H')
           WRITE(nfich,129) clin,id,'R1/H=',vect(1),'R2/H=',vect(2)
         CASE('Fin')
           WRITE(nfich,129) clin,id,'Fint1',vect(1),'Fint2',vect(2)
         CASE('Mac')
           WRITE(nfich,129) clin,id,'Mv1/H',vect(1),'Mv2/H',vect(2)
         CASE('Z  ')
           WRITE(nfich,129) clin,id,'Z(1)=',vect(1),'Z(2)=',vect(2)
         CASE default
           call faterr(IAM,'unknown type for NO2xx : '//TYPE)
       END SELECT
     CASE('NO3xx')
       SELECT CASE(TYPE)
         CASE('coo')
           WRITE(nfich,131) clin,id,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
         CASE('X  ')
           WRITE(nfich,131) clin,id,'X(1)=',vect(1),'X(2)=',vect(2),'X(3)=',vect(3)
         CASE('V  ')
           WRITE(nfich,132)         'V(1)=',vect(1),'V(2)=',vect(2),'V(3)=',vect(3)
         CASE('R/H')
           WRITE(nfich,131) clin,id,'R1/H=',vect(1),'R2/H=',vect(2),'R3/H=',vect(3)
         CASE('F  ')
           WRITE(nfich,131) clin,id,'R1  =',vect(1),'R2  =',vect(2),'R3  =',vect(3)
         CASE('Z  ')
           WRITE(nfich,131) clin,id,'Z(1)=',vect(1),'Z(2)=',vect(2),'Z(3)=',vect(3)
         CASE default
           call faterr(IAM,'unknown type for NO3xx : '//TYPE)
       END SELECT
     CASE('NO4xx','NO4Px')
       SELECT CASE(TYPE)
         CASE('coo')
           WRITE(nfich,131) clin,id,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
           WRITE(nfich,128)         'coo4=',vect(4)
         CASE('X  ')
           WRITE(nfich,131) clin,id,'X(1)=',vect(1),'X(2)=',vect(2),'X(3)=',vect(3)
           WRITE(nfich,128)         'X(4)=',vect(4)
         CASE('V  ')
           WRITE(nfich,132)         'V(1)=',vect(1),'V(2)=',vect(2),'V(3)=',vect(3)
           WRITE(nfich,128)         'V(4)=',vect(4)
         CASE('R/H')
           WRITE(nfich,131) clin,id,'R1/H=',vect(1),'R2/H=',vect(2),'R3/H=',vect(3)
           WRITE(nfich,128)         'R4/H=',vect(4)
         CASE('Z  ')
           WRITE(nfich,131) clin,id,'Z(1)=',vect(1),'Z(2)=',vect(2),'Z(3)=',vect(3)
           WRITE(nfich,128)         'Z(4)=',vect(4)
         CASE default
           call faterr(IAM,'unknown type for NO4xx : '//TYPE)
       END SELECT
     CASE('NO5xx')
       SELECT CASE(TYPE)
         CASE('coo')
           WRITE(nfich,131) clin,id,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
           WRITE(nfich,130)         'coo4=',vect(4),'coo5=',vect(5)
         CASE('X  ')
           WRITE(nfich,131) clin,id,'X(1)=',vect(1),'X(2)=',vect(2),'X(3)=',vect(3)
           WRITE(nfich,130)         'X(4)=',vect(4),'X(5)=',vect(5)
         CASE('V  ')
           WRITE(nfich,132)         'V(1)=',vect(1),'V(2)=',vect(2),'V(3)=',vect(3)
           WRITE(nfich,130)         'V(4)=',vect(4),'V(5)=',vect(5)
         CASE('R/H')
           WRITE(nfich,131) clin,id,'R1/H=',vect(1),'R2/H=',vect(2),'R3/H=',vect(3)
           WRITE(nfich,130)         'R4/H=',vect(4),'R5/H=',vect(5)
         CASE('Z  ')
           WRITE(nfich,131) clin,id,'Z(1)=',vect(1),'Z(2)=',vect(2),'Z(3)=',vect(3)
           WRITE(nfich,130)         'Z(4)=',vect(4),'Z(5)=',vect(5)
         CASE default
           call faterr(IAM,'unknown type for NO5xx : '//TYPE)
       END SELECT
     CASE('NO6xx')
       SELECT CASE(TYPE)
         CASE('coo')
          WRITE(nfich,131) clin,id,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
          WRITE(nfich,132)         'coo4=',vect(4),'coo5=',vect(5),'coo6=',vect(6)
        CASE('X  ')
          WRITE(nfich,131) clin,id,'X(1)=',vect(1),'X(2)=',vect(2),'X(3)=',vect(3)
          WRITE(nfich,132)         'X(4)=',vect(4),'X(5)=',vect(5),'X(6)=',vect(6)
        CASE('V  ')
          WRITE(nfich,132)         'V(1)=',vect(1),'V(2)=',vect(2),'V(3)=',vect(3)
          WRITE(nfich,132)         'V(4)=',vect(4),'V(5)=',vect(5),'V(6)=',vect(6)
        CASE('R/H')
          WRITE(nfich,131) clin,id,'R1/H=',vect(1),'R2/H=',vect(2),'R3/H=',vect(3)
          WRITE(nfich,132)         'R4/H=',vect(4),'R5/H=',vect(5),'R6/H=',vect(6)
        CASE('Z  ')
          WRITE(nfich,131) clin,id,'Z(1)=',vect(1),'Z(2)=',vect(2),'Z(3)=',vect(3)
          WRITE(nfich,132)         'Z(3)=',vect(4),'Z(5)=',vect(5),'Z(6)=',vect(6)
        CASE default
          call faterr(IAM,'unknown type for NO6xx : '//TYPE)
       END SELECT
     CASE('alpha')
          WRITE(nfich,132)         'a(1)=',vect(1),'a(2)=',vect(2),'a(3)=',vect(3)
     CASE('beta ')
          WRITE(nfich,132)         'b(1)=',vect(1),'b(2)=',vect(2),'b(3)=',vect(3)
     CASE('gamma')
          WRITE(nfich,132)         'g(1)=',vect(1),'g(2)=',vect(2),'g(3)=',vect(3)
     CASE default
  END SELECT

127  FORMAT(1X,A5,1X,I6,2X,5X,2X,5X,1(2X,A5,D14.7))
128  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,1(2X,A5,D14.7))

129  FORMAT(1X,A5,1X,I6,2X,5X,2X,5X,2(2X,A5,D14.7))
130  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2(2X,A5,D14.7))

131  FORMAT(1X,A5,1X,I6,2X,5X,2X,5X,3(2X,A5,D14.7))
132  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

 END SUBROUTINE write_a_nodty
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 LOGICAL FUNCTION is_a_driven_dof(iccdof,driven_dof)

   IMPLICIT NONE
   INTEGER               :: iccdof
   TYPE(T_driven_dof)    :: driven_dof

   is_a_driven_dof = .FALSE.
   IF (driven_dof%dofnb == iccdof) is_a_driven_dof = .TRUE.

 END FUNCTION is_a_driven_dof
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 LOGICAL FUNCTION is_a_driven_dof_of_body(ibdyty,driven_dof)

   IMPLICIT NONE
   INTEGER               :: ibdyty
   TYPE(T_driven_dof)    :: driven_dof

   is_a_driven_dof_of_body = .FALSE.
   IF (driven_dof%bdynb == ibdyty) is_a_driven_dof_of_body = .TRUE.

 END FUNCTION is_a_driven_dof_of_body
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 LOGICAL FUNCTION is_a_driven_dof_of_node(inodty,driven_dof)

   IMPLICIT NONE
   INTEGER               :: inodty
   TYPE(T_driven_dof)    :: driven_dof

   is_a_driven_dof_of_node = .FALSE.
   IF (driven_dof%nodnb == inodty) is_a_driven_dof_of_node = .TRUE.

 END FUNCTION is_a_driven_dof_of_node
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 INTEGER FUNCTION dofnb_of_a_driven_dof(driven_dof)

   IMPLICIT NONE
   TYPE(T_driven_dof)    :: driven_dof

   dofnb_of_a_driven_dof=driven_dof%dofnb

 END FUNCTION dofnb_of_a_driven_dof
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE read_a_driven_dof_FE(chnod,inodty,clin,driven_dof)

   IMPLICIT NONE
   INTEGER,intent(in)            :: inodty
   CHARACTER(len=5),intent(in)   :: chnod
   CHARACTER(len=*),intent(in)   :: clin     
   TYPE(T_driven_dof)            :: driven_dof

   driven_dof%nodty=chnod
   driven_dof%nodnb=inodty
   READ(clin( 9: 13),'(I5)   ') driven_dof%dofnb

   driven_dof%is_standard = .TRUE.
   IF (clin(15:23) == 'evolution') driven_dof%is_standard = .FALSE.

   IF (driven_dof%is_standard) THEN
     READ(clin(15: 28),'(D14.7)') driven_dof%CT
     READ(clin(30: 43),'(D14.7)') driven_dof%AMP
     READ(clin(45: 58),'(D14.7)') driven_dof%OMEGA
     READ(clin(60: 73),'(D14.7)') driven_dof%PHI
     READ(clin(75: 88),'(D14.7)') driven_dof%RAMPI
     READ(clin(90:103),'(D14.7)') driven_dof%RAMP     
   ELSE
     CALL read_G_evolution(trim(location('DATBOX/')),trim(clin(25:64)),driven_dof%time_evolution)
   ENDIF

   driven_dof%is_active=.TRUE.


 END SUBROUTINE read_a_driven_dof_FE
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE read_a_driven_dof_RIGID(ibdyty,chnod,inodty,clin,driven_dof)

   IMPLICIT NONE
   INTEGER,intent(in)          :: ibdyty,inodty
   CHARACTER(len=5),intent(in) :: chnod
   CHARACTER(len=*),intent(in) :: clin     
   TYPE(T_driven_dof)          :: driven_dof

   driven_dof%bdynb=ibdyty   
   driven_dof%nodty=chnod
   driven_dof%nodnb=inodty
   READ(clin( 9: 13),'(I5)   ') driven_dof%dofnb

   driven_dof%is_standard = .TRUE.
   NULLIFY(driven_dof%time_evolution%x,driven_dof%time_evolution%fx)

   IF (clin(15:23) == 'evolution') driven_dof%is_standard = .FALSE.

   IF (driven_dof%is_standard) THEN
     READ(clin(15: 28),'(D14.7)') driven_dof%CT
     READ(clin(30: 43),'(D14.7)') driven_dof%AMP
     READ(clin(45: 58),'(D14.7)') driven_dof%OMEGA
     READ(clin(60: 73),'(D14.7)') driven_dof%PHI
     READ(clin(75: 88),'(D14.7)') driven_dof%RAMPI
     READ(clin(90:103),'(D14.7)') driven_dof%RAMP     
   ELSE
     CALL read_G_evolution(trim(location('DATBOX/')),trim(clin(25:64)),driven_dof%time_evolution)
   ENDIF

   driven_dof%is_active=.TRUE.


 END SUBROUTINE read_a_driven_dof_RIGID
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------      
 SUBROUTINE write_a_driven_dof(nfich,clin,driven_dof)

   IMPLICIT NONE
   INTEGER                     :: nfich
   CHARACTER(len=5),OPTIONAL   :: clin
   TYPE(T_driven_dof),OPTIONAL :: driven_dof

   IF (PRESENT(clin) .AND. PRESENT(driven_dof)) THEN 

     IF (driven_dof%is_standard) THEN
       WRITE(nfich,120) clin,driven_dof%dofnb,&
                        driven_dof%CT,driven_dof%AMP,driven_dof%OMEGA,driven_dof%PHI, &
                     driven_dof%RAMPI,driven_dof%RAMP 
     ELSE
       WRITE(nfich,121) clin,driven_dof%dofnb,driven_dof%time_evolution%file           
       CALL  write_G_evolution(trim(location('OUTBOX/')),driven_dof%time_evolution)
     ENDIF
   ELSE
 
     WRITE(nfich,'(A103)')  &
     '$dofty  numbr [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)]...*...[RAMPI.....+.....RAMP.*.time]'

   END IF

120   FORMAT(1X,A5,2X,I5,6(1X,D14.7))
121   FORMAT(1X,A5,2X,I5,1X,'evolution',1X,A40)
 END SUBROUTINE write_a_driven_dof
!-------------------------------------------------------------------------------
 SUBROUTINE comp_a_driven_dof(driven_dof,Valbegin,Val)

   IMPLICIT NONE
   TYPE(T_driven_dof) :: driven_dof

   REAL(kind=8) :: cobegin,co,rapbegin,rap,Valbegin,Val

   IF (driven_dof%is_standard) THEN
     rapbegin = driven_dof%RAMPI+driven_dof%RAMP*TPSbegin
     rapbegin = dmin1(1.d0,rapbegin)
     rap      = driven_dof%RAMPI+driven_dof%RAMP*TPS
     rap      = dmin1(1.d0,rap)
     cobegin  = dcos(driven_dof%OMEGA*TPSbegin+driven_dof%PHI)
     co       = dcos(driven_dof%OMEGA*TPS     +driven_dof%PHI)

     Valbegin = (driven_dof%CT+driven_dof%AMP*cobegin)*rapbegin
     Val      = (driven_dof%CT+driven_dof%AMP*co     )*rap
   ELSE
     Valbegin=  eval_G_evolution(driven_dof%time_evolution,TPSbegin)
     Val     =  eval_G_evolution(driven_dof%time_evolution,TPS)
   ENDIF
 END SUBROUTINE comp_a_driven_dof
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE owner_of_a_driven_dof_FE(driven_dof,inod,idof)

   IMPLICIT NONE
   TYPE(T_driven_dof) :: driven_dof
   INTEGER            :: inod,idof

   inod = driven_dof%nodnb    
   idof = driven_dof%dofnb    

 END SUBROUTINE owner_of_a_driven_dof_FE
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 SUBROUTINE owner_of_a_driven_dof_RIGID(driven_dof,ibdy,inod,idof)

   IMPLICIT NONE
   TYPE(T_driven_dof) :: driven_dof
   INTEGER            :: ibdy,inod,idof

   ibdy = driven_dof%bdynb    
   inod = driven_dof%nodnb    
   idof = driven_dof%dofnb    

 END SUBROUTINE owner_of_a_driven_dof_RIGID
!-------------------------------------------------------------------------------


! rm's functions for new arch

 !> \brief Set a driven dof from input values
 subroutine set_driven_dof(driven_dof, type, i4, r8, cx)
   implicit none
   type(T_driven_dof), intent(inout) :: driven_dof !< [in,out] the driven dof to set
   character(len=5)  , intent(in)    :: type       !< [in] type of driven dof ('prede' or 'evol')
   integer(kind=4),    dimension(:), pointer :: i4 !< [in] node and dof index on which to apply the driven dof
   real(kind=8),       dimension(:), pointer :: r8 !< [in] if predefined driven dof: the value of the coefficients
   character(len=128), dimension(:), pointer :: cx !< [in] if evolution driven dof: the name of the file to use
   !
   character(len=80) :: cout
   character(len=23) :: IAM
   !      12345678901234567890123
   IAM = '[a_DOF::set_driven_dof]'

   call paranoid_check_i4_ptr(IAM,i4,2) 

   driven_dof%nodnb = i4(1)
   driven_dof%dofnb = i4(2)

   driven_dof%is_active = .true.


   nullify(driven_dof%time_evolution%x)
   nullify(driven_dof%time_evolution%fx)

   if( type == 'prede' ) then

     call paranoid_check_r8_ptr(IAM,r8,6)

     driven_dof%is_standard = .true.

     driven_dof%CT    = r8(1)
     driven_dof%AMP   = r8(2)
     driven_dof%OMEGA = r8(3)
     driven_dof%PHI   = r8(4)
     driven_dof%RAMPI = r8(5)
     driven_dof%RAMP  = r8(6)

   else if( type == 'evol' ) then

     call paranoid_check_cx_ptr(IAM,cx,1)

     driven_dof%is_standard = .false.

     call read_G_evolution(trim(location('DATBOX/')),trim(cx(1)),driven_dof%time_evolution)

   else
     write(cout,'(A,1x,A)') 'unknown driven dof type:', type  
     call faterr(IAM,cout)
   end if

 end subroutine set_driven_dof

 !> \brief compute the value of a driven dof at a given time
 subroutine comp_a_driven_dof_at_t(driven_dof,time,val)
   implicit none
   type(T_driven_dof), intent(in)  :: driven_dof !< [in] driven dof
   real(kind=8),       intent(in)  :: time       !< [in] time at which to compute imposed value
   real(kind=8),       intent(out) :: val        !< [out] the computed value
   !
   real(kind=8) :: co,rap

   if( driven_dof%is_standard ) then
     rap = driven_dof%RAMPI + driven_dof%RAMP*time
     rap = dmin1(1.d0,rap)
     co  = dcos( driven_dof%OMEGA*time + driven_dof%PHI )

     val = ( driven_dof%CT + driven_dof%AMP*co ) * rap
   else
     val = eval_G_evolution(driven_dof%time_evolution,time)
   end if

 end subroutine comp_a_driven_dof_at_t

 END MODULE a_DOF

