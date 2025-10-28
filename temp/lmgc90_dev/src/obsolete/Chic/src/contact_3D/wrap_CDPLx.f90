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
MODULE wrap_CDPLx

  !!****h* LMGC90.CHIC/CDPLx
  !! NAME
  !!  module wrap_CDPLx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/CDPLx
  !!****

  USE CHIC
  USE CDPLx,ONLY:&
       coor_prediction_CDPLx,&
       CHECK_CDPLx,&
       RUN_CDPLx, &
       get_write_Vloc_Rloc_CDPLx, &
       read_ini_Vloc_Rloc_CDPLx,&
       write_xxx_Vloc_Rloc_CDPLx,&
       stock_rloc_CDPLx, &
       recup_rloc_CDPLx, &
       smooth_computation_CDPLx, &
       compute_box_CDPLx, &
       creation_tab_visu_CDPLx, &
       compute_contact_CDPLx, &
       display_prox_tactors_CDPLx,&
       get_nb_CDPLx
  
CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_CDPLx

    IMPLICIT NONE

    INTEGER      :: nb_CDPLx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout
!!! internal values for tactor selection

    INTEGER,PARAMETER :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , i_real_tactor = 4

    CHECK = CHECK_CDPLx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1) THEN
       !!****e* CDPLx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between SPHER and PLANx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CDPLx/coor_prediction_CDPLx
       !!  LMGC90.CORE/CDPLx/creation_tab_visu_CDPLx
       !!  LMGC90.CORE/CDPLx/compute_contact_CDPLx
       !!****
       CALL LOGCHIC('CDPLx')

       CALL coor_prediction_CDPLx

       RUN = RUN_CDPLx()

       IF (RUN) THEN
          CALL creation_tab_visu_CDPLx
          IF(KHOZZZ == 1)THEN
             nb_CDPLx = get_nb_CDPLx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_CDPLx,' CDPLx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_CDPLx
       IF (KHOZZZ == 1) THEN
          nb_CDPLx = get_nb_CDPLx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CDPLx,' CDPLx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* CDPLx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CDPLx/stock_rloc_CDPLx
       !!****
       CALL LOGCHIC('CDPLx')
       CALL stock_rloc_CDPLx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* CDPLx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CDPLx/recup_rloc_CDPLx
       !!****
       CALL LOGCHIC('CDPLx') 
       CALL recup_rloc_CDPLx
       IF (KHOZZZ == 1) THEN
          nb_CDPLx = get_nb_CDPLx(i_recup_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CDPLx,' recup CDPLx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SMOOTH FORCE COMPUTATION')==1) THEN
       !!****e* CDPLx/SMOOTH FORCE COMPUTATION
       !! NAME 
       !!  SMOOTH FORCE COMPUTATION
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CDPLx/smooth_computation_CDPLx
       !!****
       CALL LOGCHIC('CDPLx')
       CALL smooth_computation_CDPLx
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* CDPLx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CDPLx contacts
       !! USES
       !!  LMGC90.CORE/CDPLx/write_xxx_Vloc_Rloc_CDPLx
       !!****
       CALL LOGCHIC('CDPLx')
       CALL write_xxx_Vloc_Rloc_CDPLx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* CDPLx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CDPLx contacts
       !! USES
       !!  LMGC90.CORE/CDPLx/write_xxx_Vloc_Rloc_CDPLx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CDPLx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('CDPLx') 
          CALL write_xxx_Vloc_Rloc_CDPLx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* CDPLx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CDPLx contacts
       !! USES
       !!  LMGC90.CORE/CDPLx/write_xxx_Vloc_Rloc_CDPLx
       !!****
       CALL LOGCHIC('CDPLx') 
       CALL write_xxx_Vloc_Rloc_CDPLx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* CDPLx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CDPLx/display_prox_tactors_CDPLx
       !!****
       CALL LOGCHIC('CDPLx')
       CALL display_prox_tactors_CDPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* CDPLx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CDPLx/compute_box_CDPLx
       !!****
       CALL LOGCHIC('CDPLx')
       CALL compute_box_CDPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* CDPLx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CDPLx/read_ini_Vloc_Rloc_CDPLx
       !!****
       CALL LOGCHIC('CDPLx') 
       CALL read_ini_Vloc_Rloc_CDPLx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_CDPLx
!!!---------------------------------------------------------------------

END MODULE wrap_CDPLx
