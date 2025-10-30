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
MODULE wrap_DKALp

  !!****h* LMGC90.CHIC/DKALp
  !! NAME
  !!  module wrap_DKALp
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DKALp
  !!****

  USE CHIC
  USE DKALp,ONLY: &
       get_nb_DKALp, &
       get_nb_rough_DKALp, &
       get_nb_recup_DKALp, &
       stock_rloc_DKALp, &
       recup_rloc_DKALp, &
       compute_box_DKALp, &
       read_ini_Vloc_Rloc_DKALp, &
       write_xxx_Vloc_Rloc_DKALp, &
       coor_prediction_DKALp, &
       creation_tab_visu_DKALp, &
       compute_contact_DKALp, &
       display_prox_tactors_DKALp, &
       RUN_DKALp, &
       CHECK_DKALp, &
       get_write_Vloc_Rloc_DKALp

CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_DKALp 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_DKALp,nb_rough_DKALp
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout

    CHECK = CHECK_DKALp()

    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* DKALp/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx and ALpxx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKALp/coor_prediction_DKALp
       !!  LMGC90.CORE/DKALp/creation_tab_visu_DKALp
       !!  LMGC90.CORE/DKALp/compute_contact_DKALp
       !!****
       CALL LOGCHIC('DKALp')
       
       CALL coor_prediction_DKALp
       
       RUN = RUN_DKALp()

       IF (RUN) THEN
          CALL creation_tab_visu_DKALp
          IF(KHOZZZ == 1)THEN
             nb_rough_DKALp = get_nb_rough_DKALp()
             WRITE(cout,'(1X,I10,A20)') nb_rough_DKALp,' DKALp roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_DKALp
       IF (KHOZZZ == 1) THEN
          nb_DKALp = get_nb_DKALp()
          WRITE(cout,'(1X,I10,A12)') nb_DKALp,' DKALp found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* DKALp/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKALp/stock_rloc_DKALp
       !!****
       CALL LOGCHIC('DKALp')
       CALL stock_rloc_DKALp 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* DKALp/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKALp/recup_rloc_DKALp
       !!****
       CALL LOGCHIC('DKALp') 
       CALL recup_rloc_DKALp
       IF (KHOZZZ == 1) THEN
          nb_DKALp = get_nb_recup_DKALp()
          WRITE(cout,'(1X,I10,A12)') nb_DKALp,' recup DKALp'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN  
       !!****e* DKALp/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts
       !! USES
       !!  LMGC90.CORE/DKALp/write_xxx_Vloc_Rloc_DKALp
       !!****
       CALL LOGCHIC('DKALp')
       CALL write_xxx_Vloc_Rloc_DKALp(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* DKALp/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts
       !! USES
       !!  LMGC90.CORE/DKALp/write_xxx_Vloc_Rloc_DKALp
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKALp()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('DKALp') 
          CALL write_xxx_Vloc_Rloc_DKALp(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* DKALp/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts
       !! USES
       !!  LMGC90.CORE/DKALp/write_xxx_Vloc_Rloc_DKALp
       !!****
       CALL LOGCHIC('DKALp') 
       CALL write_xxx_Vloc_Rloc_DKALp(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* DKALp/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKALp/display_prox_tactors_DKALp
       !!****
       CALL LOGCHIC('DKALp')
       CALL display_prox_tactors_DKALp
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* DKALp/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKALp/compute_box_DKALp
       !!****
       CALL LOGCHIC('DKALp')
       CALL compute_box_DKALp
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* DKALp/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKALp/read_ini_Vloc_Rloc_DKALp
       !!****
       CALL LOGCHIC('DKALp') 
       CALL read_ini_Vloc_Rloc_DKALp
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_DKALp
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_DKALp.inc'
  
END MODULE wrap_DKALp
