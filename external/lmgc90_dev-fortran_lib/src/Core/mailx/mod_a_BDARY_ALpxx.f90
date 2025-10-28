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
MODULE a_BDARY_ALpxx                                       

  !!****h* LMGC90.CORE/a_BDARY_ALpxx
  !! NAME
  !!  module a_BDARY_ALpxx
  !! PURPOSE
  !!  anonymous loading of the ALpxx parameters from data files
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****

  USE utilities 

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_ALpxx(idata)

    IMPLICIT NONE

    INTEGER,DIMENSION(:),POINTER  ::  idata

    INTEGER           :: nb_ALxxx,ial
    CHARACTER(len=35) :: IAM='mod_a_BDARY_ALpxx::read_BDARY_ALpxx'

    TYPE T_tmp_ALpxx
       TYPE(T_tmp_ALpxx), POINTER :: p    ! pointeur sur le precedent
       INTEGER :: num1,num2               ! les valeurs
       TYPE(T_tmp_ALpxx), POINTER :: n    ! pointeur sur le suivant
    END TYPE T_tmp_ALpxx

    TYPE(T_tmp_ALpxx),POINTER :: Root
    TYPE(T_tmp_ALpxx),POINTER :: Current
    TYPE(T_tmp_ALpxx),POINTER :: Previous

    NULLIFY(Root);NULLIFY(Current);NULLIFY(Previous)

    nb_ALxxx = 1

    ALLOCATE(Root)
    NULLIFY(Root%p)
    NULLIFY(Root%n)

    Current => Root
    Previous => Root

    READ(G_clin(35:39),'(I5)')   Current%num1
    READ(G_clin(47:51),'(I5)')   Current%num2
    
    DO    
       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'expected values in data file')
       END IF
       
       IF (G_clin(1:6) == '+ALpxx' ) THEN

          nb_ALxxx = nb_ALxxx + 1
          ALLOCATE(Current)
          Previous%n => Current
          Current%p => Previous
          NULLIFY(Current%n)

          READ(G_clin(35:39),'(I5)')   Current%num1
          READ(G_clin(47:51),'(I5)')   Current%num2

          IF (Current%num1 /= Previous%num2) THEN
             CALL FATERR(IAM,'non continuous line')
          END IF
 
          Previous => Current

       ELSE
          
          BACKSPACE(G_nfich)
          
          ALLOCATE(idata(0:nb_ALxxx+1))
          idata(0)=nb_ALxxx    !<-- ca peut preter a confusion /= ASpxx qui contient la taille reelle 
          idata(1) = Root%num1

          DO ial=nb_ALxxx,1,-1
             Previous => Current%p
             idata(ial+1) = Current%num2
             DEALLOCATE(Current)
             Current => Previous
          END DO

          NULLIFY(Root)
          EXIT
       END IF       
    END DO

  END SUBROUTINE read_BDARY_ALpxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_ALpxx(nfich,itacty,tacID,color,idata)

    IMPLICIT NONE

    INTEGER               ::  nfich,itacty,ial
    CHARACTER(len=5)      ::  tacID,color
    INTEGER,DIMENSION(0:) ::  idata
   
    WRITE(nfich,104) tacID,itacty,'color',color,'noda=',idata(1),'nodb=',idata(2)

    DO ial=2,idata(0)
       WRITE(nfich,105) tacID,itacty,'noda=',idata(ial),'nodb=',idata(ial+1) 
    END DO

104 FORMAT(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)
105 FORMAT('+',A5,2X,I5,2X,5x,2X,5x,2X,A5,I5,2X,A5,I5)

  END SUBROUTINE write_BDARY_ALpxx
!!!------------------------------------------------------------------------
  
END MODULE a_BDARY_ALpxx
