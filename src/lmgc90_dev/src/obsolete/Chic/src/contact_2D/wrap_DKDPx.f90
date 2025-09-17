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
MODULE wrap_DKDPx

  !!****h* LMGC90.CHIC/DKDPx
  !! NAME
  !!  module wrap_DKDPx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DKDPx
  !!****

  USE CHIC
  USE DKDPx,ONLY: &
       get_nb_DKDPx, &
       get_nb_rough_DKDPx, &
       get_nb_recup_DKDPx, &
       stock_rloc_DKDPx, &
       recup_rloc_DKDPx, &
       compute_box_DKDPx, &
       read_ini_Vloc_Rloc_DKDPx, &
       write_xxx_Vloc_Rloc_DKDPx, &
       coor_prediction_DKDPx, &
       creation_tab_visu_DKDPx, &
       compute_contact_DKDPx, &
       display_prox_tactors_DKDPx, &
       RUN_DKDPx, &
       CHECK_DKDPx, &
       get_write_Vloc_Rloc_DKDPx

CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_DKDPx 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_DKDPx,nb_rough_DKDPx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc
    
    CHARACTER(len=35) :: cout

    CHECK = CHECK_DKDPx()

    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* DKDPx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx and DISPx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKDPx/coor_prediction_DKDPx
       !!  LMGC90.CORE/DKDPx/creation_tab_visu_DKDPx
       !!  LMGC90.CORE/DKDPx/compute_contact_DKDPx
       !!****
       CALL LOGCHIC('DKDPx')
       
       CALL coor_prediction_DKDPx
       
       RUN = RUN_DKDPx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKDPx
          IF(KHOZZZ == 1)THEN
             nb_rough_DKDPx = get_nb_rough_DKDPx()
             WRITE(cout,'(1X,I10,A20)') nb_rough_DKDPx,' DKDPx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_DKDPx
       IF (KHOZZZ == 1) THEN
          nb_DKDPx = get_nb_DKDPx()
          WRITE(cout,'(1X,I10,A12)') nb_DKDPx,' DKDPx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* DKDPx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKDPx/stock_rloc_DKDPx
       !!****
       CALL LOGCHIC('DKDPx')
       CALL stock_rloc_DKDPx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* DKDPx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDPx/recup_rloc_DKDPx
       !!****
       CALL LOGCHIC('DKDPx') 
       CALL recup_rloc_DKDPx
       IF (KHOZZZ == 1) THEN
          nb_DKDPx = get_nb_recup_DKDPx()
          WRITE(cout,'(1X,I10,A12)') nb_DKDPx,' recup DKDPx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN
       !!****e* DKDPx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDPx contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/write_xxx_Vloc_Rloc_DKDPx
       !!****
       CALL LOGCHIC('DKDPx')
       CALL write_xxx_Vloc_Rloc_DKDPx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* DKDPx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDPx contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/write_xxx_Vloc_Rloc_DKDPx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDPx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('DKDPx') 
          CALL write_xxx_Vloc_Rloc_DKDPx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* DKDPx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDPx contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/write_xxx_Vloc_Rloc_DKDPx
       !!****
       CALL LOGCHIC('DKDPx') 
       CALL write_xxx_Vloc_Rloc_DKDPx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* DKDPx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/display_prox_tactors_DKDPx
       !!****
       CALL LOGCHIC('DKDPx')
       CALL display_prox_tactors_DKDPx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* DKDPx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKDPx/compute_box_DKDPx
       !!****
       CALL LOGCHIC('DKDPx')
       CALL compute_box_DKDPx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* DKDPx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKDPx/read_ini_Vloc_Rloc_DKDPx
       !!****
       CALL LOGCHIC('DKDPx') 
       CALL read_ini_Vloc_Rloc_DKDPx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_DKDPx
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_DKDPx.inc'
  
END MODULE wrap_DKDPx
