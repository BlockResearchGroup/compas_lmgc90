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
MODULE wrap_DKJCx

  !!****h* LMGC90.CHIC/DKJCx
  !! NAME
  !!  module wrap_DKJCx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DKJCx
  !!****

  USE CHIC

  USE DKJCx,ONLY: &
       get_nb_DKJCx, &
       get_nb_rough_DKJCx, &
       get_nb_recup_DKJCx, &
       stock_rloc_DKJCx, &
       recup_rloc_DKJCx, &
       smooth_computation_DKJCx, &
       compute_box_DKJCx, &
       read_ini_Vloc_Rloc_DKJCx, &
       write_xxx_Vloc_Rloc_DKJCx, &
       display_prox_tactors_DKJCx, &
       coor_prediction_DKJCx, &
       creation_tab_visu_DKJCx, &
       compute_contact_DKJCx, &
       RUN_DKJCx, &
       CHECK_DKJCx, &
       get_write_Vloc_Rloc_DKJCx
  
CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_DKJCx 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_DKJCx,nb_rough_DKJCx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc
    
    CHARACTER(len=35) :: cout

    CHECK = CHECK_DKJCx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* DKJCx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx and JONCx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKJCx/coor_prediction_DKJCx
       !!  LMGC90.CORE/DKJCx/creation_tab_visu_DKJCx
       !!  LMGC90.CORE/DKJCx/compute_contact_DKJCx
       !!****
       CALL LOGCHIC('DKJCx')
             
       CALL coor_prediction_DKJCx
       
       RUN = RUN_DKJCx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKJCx
          IF(KHOZZZ == 1)THEN
             nb_rough_DKJCx = get_nb_rough_DKJCx()
             WRITE(cout,'(1X,I10,A20)') nb_rough_DKJCx,' DKJCx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_DKJCx
       IF (KHOZZZ == 1) THEN
          nb_DKJCx = get_nb_DKJCx()
          WRITE(cout,'(1X,I10,A12)') nb_DKJCx,' DKJCx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* DKJCx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKJCx/stock_rloc_DKJCx
       !!****
       CALL LOGCHIC('DKJCx')
       CALL stock_rloc_DKJCx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* DKJCx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKJCx/recup_rloc_DKJCx
       !!****
       CALL LOGCHIC('DKJCx') 
       CALL recup_rloc_DKJCx
       IF (KHOZZZ == 1) THEN
          nb_DKJCx = get_nb_recup_DKJCx()
          WRITE(cout,'(1X,I10,A12)') nb_DKJCx,' recup DKJCx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SMOOTH FORCE COMPUTATION')==1) THEN
       !!****e* DKJCx/SMOOTH FORCE COMPUTATION
       !! NAME 
       !!  SMOOTH FORCE COMPUTATION
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKJCx/smooth_computation_DKJCx
       !!****
       CALL LOGCHIC('DKJCx')
       CALL smooth_computation_DKJCx
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN 
       !!****e* DKJCx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKJCx contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/write_xxx_Vloc_Rloc_DKJCx
       !!****
       CALL LOGCHIC('DKJCx')
       CALL write_xxx_Vloc_Rloc_DKJCx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* DKJCx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKJCx contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/write_xxx_Vloc_Rloc_DKJCx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKJCx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('DKJCx') 
          CALL write_xxx_Vloc_Rloc_DKJCx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* DKJCx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKJCx contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/write_xxx_Vloc_Rloc_DKJCx
       !!****
       CALL LOGCHIC('DKJCx') 
       CALL write_xxx_Vloc_Rloc_DKJCx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* DKJCx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/display_prox_tactors_DKJCx
       !!****
       CALL LOGCHIC('DKJCx')
       CALL display_prox_tactors_DKJCx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* DKJCx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKJCx/compute_box_DKJCx
       !!****
       CALL LOGCHIC('DKJCx')
       CALL compute_box_DKJCx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* DKJCx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKJCx/read_ini_Vloc_Rloc_DKJCx
       !!****
       CALL LOGCHIC('DKJCx') 
       CALL read_ini_Vloc_Rloc_DKJCx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_DKJCx
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_DKJCx.inc'

  
END MODULE wrap_DKJCx
