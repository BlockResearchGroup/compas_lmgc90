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
MODULE wrap_DKKDx

  !!****h* LMGC90.CHIC/DKKDx
  !! NAME
  !!  module wrap_DKKDx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DKKDx
  !!****

  USE CHIC

  USE DKKDx,ONLY: &
       get_nb_DKKDx, &
       get_nb_rough_DKKDx, &
       get_nb_recup_DKKDx, &
       stock_rloc_DKKDx, &
       recup_rloc_DKKDx, &
       smooth_computation_DKKDx, &
       compute_box_DKKDx, &
       read_ini_Vloc_Rloc_DKKDx, &
       write_xxx_Vloc_Rloc_DKKDx, &
       coor_prediction_DKKDx, &
       creation_tab_visu_DKKDx, &
       compute_contact_DKKDx, &
       display_prox_tactors_DKKDx, &
       RUN_DKKDx, &
       CHECK_DKKDx, &
       get_write_Vloc_Rloc_DKKDx

CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_DKKDx 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_DKKDx,nb_rough_DKKDx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc
    
    CHARACTER(len=35) :: cout

    CHECK = CHECK_DKKDx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* DKKDx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKKDx/coor_prediction_DKKDx
       !!  LMGC90.CORE/DKKDx/creation_tab_visu_DKKDx
       !!  LMGC90.CORE/DKKDx/compute_contact_DKKDx
       !!****
       CALL LOGCHIC('DKKDx')
      
       CALL coor_prediction_DKKDx
       
       RUN = RUN_DKKDx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKKDx
          IF(KHOZZZ == 1)THEN
             nb_rough_DKKDx = get_nb_rough_DKKDx()
             WRITE(cout,'(1X,I10,A20)') nb_rough_DKKDx,' DKKDx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_DKKDx
       IF (KHOZZZ == 1) THEN
          nb_DKKDx = get_nb_DKKDx()
          WRITE(cout,'(1X,I10,A12)') nb_DKKDx,' DKKDx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* DKKDx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKKDx/stock_rloc_DKKDx
       !!****
       CALL LOGCHIC('DKKDx')
       CALL stock_rloc_DKKDx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* DKKDx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKKDx/recup_rloc_DKKDx
       !!****
       CALL LOGCHIC('DKKDx') 
       CALL recup_rloc_DKKDx
       IF (KHOZZZ == 1) THEN
          nb_DKKDx = get_nb_recup_DKKDx()
          WRITE(cout,'(1X,I10,A12)') nb_DKKDx,' recup DKKDx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SMOOTH FORCE COMPUTATION')==1) THEN
       !!****e* CHIC.DKKDx/SMOOTH FORCE COMPUTATION
       !! NAME 
       !!  SMOOTH FORCE COMPUTATION
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKKDx/smooth_computation_DKKDx
       !!****
       CALL LOGCHIC('DKKDx')
       CALL smooth_computation_DKKDx
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* DKKDx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKKDx contacts
       !! USES
       !!  LMGC90.CORE/DKKDx/write_xxx_Vloc_Rloc_DKKDx
       !!****
       CALL LOGCHIC('DKKDx')
       CALL write_xxx_Vloc_Rloc_DKKDx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* DKKDx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKKDx contacts
       !! USES
       !!  LMGC90.CORE/DKKDx/write_xxx_Vloc_Rloc_DKKDx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKKDx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('DKKDx') 
          CALL write_xxx_Vloc_Rloc_DKKDx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* DKKDx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKKDx contacts
       !! USES
       !!  LMGC90.CORE/DKKDx/write_xxx_Vloc_Rloc_DKKDx
       !!****
       CALL LOGCHIC('DKKDx') 
       CALL write_xxx_Vloc_Rloc_DKKDx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* DKKDx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKKDx/display_prox_tactors_DKKDx
       !!****
       CALL LOGCHIC('DKKDx')
       CALL display_prox_tactors_DKKDx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* DKKDx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKKDx/compute_box_DKKDx
       !!****
       CALL LOGCHIC('DKKDx')
       CALL compute_box_DKKDx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* DKKDx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKKDx/read_ini_Vloc_Rloc_DKKDx
       !!****
       CALL LOGCHIC('DKKDx') 
       CALL read_ini_Vloc_Rloc_DKKDx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_DKKDx
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_DKKDx.inc'

END MODULE wrap_DKKDx
