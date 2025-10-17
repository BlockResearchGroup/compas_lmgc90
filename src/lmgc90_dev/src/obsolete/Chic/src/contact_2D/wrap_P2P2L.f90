!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE wrap_P2P2L

  !!****h* LMGC90.CHIC/P2P2L
  !! NAME
  !!  module wrap_P2P2L
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/P2P2L
  !!****

  USE CHIC

  USE P2P2L,ONLY: &
       get_nb_P2P2L, &
       stock_rloc_P2P2L, &
       recup_rloc_P2P2L, &
       compute_box_P2P2L, &
       read_ini_Vloc_Rloc_P2P2L, &
       write_xxx_Vloc_Rloc_P2P2L, &
       coor_prediction_P2P2L, &
       compute_contact_P2P2L, &
       display_prox_tactors_P2P2L, &
       RUN_P2P2L, &
       CHECK_P2P2L, &
       get_write_Vloc_Rloc_P2P2L
  
CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_P2P2L 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_P2P2L
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc
    
    CHARACTER(len=35) :: cout

    CHECK = CHECK_P2P2L()

    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1) THEN
       CALL LOGCHIC('P2P2L')
       !!****e* P2P2L/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between PT2DL tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/P2P2L/coor_prediction_P2P2L
       !!  LMGC90.CORE/P2P2L/compute_contact_P2P2L
       !!****
       CALL coor_prediction_P2P2L
       
       CALL compute_contact_P2P2L
       IF (KHOZZZ == 1) THEN
          nb_P2P2L = get_nb_P2P2L()
          WRITE(cout,'(1X,I10,A12)') nb_P2P2L,' P2P2L found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* P2P2L/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/P2P2L/stock_rloc_P2P2L
       !!****
       CALL LOGCHIC('P2P2L')
       CALL stock_rloc_P2P2L 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* P2P2L/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/P2P2L/recup_rloc_P2P2L
       !!****
       CALL LOGCHIC('P2P2L') 
       CALL recup_rloc_P2P2L
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN   
       !!****e* P2P2L/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/write_xxx_Vloc_Rloc_P2P2L
       !!****
       CALL LOGCHIC('P2P2L')
       CALL write_xxx_Vloc_Rloc_P2P2L(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* P2P2L/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/write_xxx_Vloc_Rloc_P2P2L
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_P2P2L()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('P2P2L') 
          CALL write_xxx_Vloc_Rloc_P2P2L(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* P2P2L/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/write_xxx_Vloc_Rloc_P2P2L
       !!****
       CALL LOGCHIC('P2P2L') 
       CALL write_xxx_Vloc_Rloc_P2P2L(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* P2P2L/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/display_prox_tactors_P2P2L
       !!****
       CALL LOGCHIC('P2P2L')
       CALL display_prox_tactors_P2P2L
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* P2P2L/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/P2P2L/compute_box_P2P2L
       !!****
       CALL LOGCHIC('P2P2L')
       CALL compute_box_P2P2L
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* P2P2L/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/P2P2L/read_ini_Vloc_Rloc_P2P2L
       !!****
       CALL LOGCHIC('P2P2L') 
       CALL read_ini_Vloc_Rloc_P2P2L
       IETZZZ = 1
       RETURN 
    END IF

    IF ( (INDEX(CHZZZ,'P2P2L COMPUTE BOX')==1).OR.&
         (INDEX(CHZZZ,'P2P2L SELECT PROX TACTORS')==1))THEN
       CALL LOGCHIC('P2P2L')
       CALL LOGMESCHIC(' @ ERROR: This command is obsolote !')
       CALL LOGMESCHIC(' @ use COMPUTE BOX')
       STOP 
    END IF

  END SUBROUTINE chic_command_P2P2L
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_P2P2L.inc'
  
END MODULE wrap_P2P2L
