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
MODULE wrap_CLJCx

  !!****h* LMGC90.CHIC/CLJCx
  !! NAME
  !!  module wrap_CLJCx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/CLJCx
  !!****

  USE CHIC

  USE CLJCx,ONLY: &
       get_nb_CLJCx, &
       get_nb_rough_CLJCx, &
       get_nb_recup_CLJCx, &
       stock_rloc_CLJCx, &
       recup_rloc_CLJCx, &
       compute_box_CLJCx, &
       read_ini_Vloc_Rloc_CLJCx, &
       write_xxx_Vloc_Rloc_CLJCx, &
       display_prox_tactors_CLJCx, &
       coor_prediction_CLJCx, &
       creation_tab_visu_CLJCx, &
       compute_contact_CLJCx, &
       RUN_CLJCx, &
       CHECK_CLJCx, &
       get_write_Vloc_Rloc_CLJCx
  
CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_CLJCx 
    
    IMPLICIT NONE

    INTEGER      :: nb_CLJCx,nb_rough_CLJCx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
!    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout

    CHECK = CHECK_CLJCx()

    IF (.NOT.CHECK) RETURN
    
    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* CLJCx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between CLxxx and JONCx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CLJCx/coor_prediction_CLJCx
       !!  LMGC90.CORE/CLJCx/creation_tab_visu_CLJCx
       !!  LMGC90.CORE/CLJCx/compute_contact_CLJCx
       !!****
       CALL LOGCHIC('CLJCx')
       
       CALL coor_prediction_CLJCx

       RUN = RUN_CLJCx()

       IF (RUN) THEN
          CALL creation_tab_visu_CLJCx
          IF(KHOZZZ == 1)THEN
             nb_rough_CLJCx = get_nb_rough_CLJCx()
             WRITE(cout,'(1X,I10,A20)') nb_rough_CLJCx,' CLJCx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_CLJCx
       IF (KHOZZZ == 1) THEN
          nb_CLJCx = get_nb_CLJCx()
          WRITE(cout,'(1X,I10,A12)') nb_CLJCx,' CLJCx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* CLJCx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CLJCx/stock_rloc_CLJCx
       !!****
       CALL LOGCHIC('CLJCx')
       CALL stock_rloc_CLJCx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* CLJCx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CLJCx/recup_rloc_CLJCx
       !!****
       CALL LOGCHIC('CLJCx') 
       CALL recup_rloc_CLJCx
       IF (KHOZZZ == 1) THEN
          nb_CLJCx = get_nb_recup_CLJCx()
          WRITE(cout,'(1X,I10,A12)') nb_CLJCx,' recup CLJCx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN    
        !!****e* CLJCx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CLJCx contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/write_xxx_Vloc_Rloc_CLJCx
       !!****
       CALL LOGCHIC('CLJCx')
       CALL write_xxx_Vloc_Rloc_CLJCx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* CLJCx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CLJCx contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/write_xxx_Vloc_Rloc_CLJCx
       !!****
       IF (get_write_Vloc_Rloc_CLJCx()) THEN
          CALL LOGCHIC('CLJCx') 
          CALL write_xxx_Vloc_Rloc_CLJCx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* CLJCx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CLJCx contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/write_xxx_Vloc_Rloc_CLJCx
       !!****
       CALL LOGCHIC('CLJCx') 
       CALL write_xxx_Vloc_Rloc_CLJCx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* CLJCx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/display_prox_tactors_CLJCx
       !!****
       CALL LOGCHIC('CLJCx')
       CALL display_prox_tactors_CLJCx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* CLJCx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CLJCx/compute_box_CLJCx
       !!****
       CALL LOGCHIC('CLJCx')
       CALL compute_box_CLJCx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       CALL LOGCHIC('CLJCx')
       !!****e* CLJCx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLJCx/read_ini_Vloc_Rloc_CLJCx
       !!****
       CALL read_ini_Vloc_Rloc_CLJCx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_CLJCx
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_CLJCx.inc'
  
END MODULE wrap_CLJCx
