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
MODULE wrap_PLALp

  !!****h* LMGC90.CHIC/PLALp
  !! NAME
  !!  module wrap_PLALp
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/PLALp
  !!****

  USE CHIC

  USE PLALp,ONLY: &
       get_nb_PLALp, &
       get_nb_rough_PLALp, &
       get_nb_recup_PLALp, &
       stock_rloc_PLALp, &
       recup_rloc_PLALp, &
       compute_box_PLALp, &
       read_ini_Vloc_Rloc_PLALp, &
       write_xxx_Vloc_Rloc_PLALp, &
       coor_prediction_PLALp, &
       creation_tab_visu_PLALp, &
       compute_contact_PLALp, &
       display_prox_tactors_PLALp, &
       RUN_PLALp, &
       CHECK_PLALp, &
       get_write_Vloc_Rloc_PLALp

CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_PLALp 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_PLALp,nb_rough_PLALp
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout

    CHECK = CHECK_PLALp()

    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* PLALp/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx and POLYG tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PLALp/coor_prediction_PLALp
       !!  LMGC90.CORE/PLALp/creation_tab_visu_PLALp
       !!  LMGC90.CORE/PLALp/compute_contact_PLALp
       !!****
       CALL LOGCHIC('PLALp')
         
       CALL coor_prediction_PLALp

       RUN = RUN_PLALp()

       IF (RUN) THEN
          CALL creation_tab_visu_PLALp
          IF(KHOZZZ == 1)THEN
             nb_rough_PLALp = get_nb_rough_PLALp()
             WRITE(cout,'(1X,I10,A20)') nb_rough_PLALp,' PLALp roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_PLALp
       IF (KHOZZZ == 1) THEN
          nb_PLALp = get_nb_PLALp()
          WRITE(cout,'(1X,I10,A12)') nb_PLALp,' PLALp found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* PLALp/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PLALp/stock_rloc_PLALp
       !!****
       CALL LOGCHIC('PLALp')
       CALL stock_rloc_PLALp 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* PLALp/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PLALp/recup_rloc_PLALp
       !!****
       CALL LOGCHIC('PLALp') 
       CALL recup_rloc_PLALp
       IF (KHOZZZ == 1) THEN
          nb_PLALp = get_nb_recup_PLALp()
          WRITE(cout,'(1X,I10,A12)') nb_PLALp,' recup PLALp'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN
       !!****e* PLALp/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts
       !! USES
       !!  LMGC90.CORE/PLALp/write_xxx_Vloc_Rloc_PLALp
       !!****
       CALL LOGCHIC('PLALp')
       CALL write_xxx_Vloc_Rloc_PLALp(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* PLALp/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts
       !! USES
       !!  LMGC90.CORE/PLALp/write_xxx_Vloc_Rloc_PLALp
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PLALp()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('PLALp') 
          CALL write_xxx_Vloc_Rloc_PLALp(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* PLALp/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts
       !! USES
       !!  LMGC90.CORE/PLALp/write_xxx_Vloc_Rloc_PLALp
       !!****
       CALL LOGCHIC('PLALp') 
       CALL write_xxx_Vloc_Rloc_PLALp(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* PLALp/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PLALp/display_prox_tactors_PLALp
       !!****
       CALL LOGCHIC('PLALp')
       CALL display_prox_tactors_PLALp
       IETZZZ = 1
       RETURN 
    END IF
       
    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* PLALp/COMPUTE_BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/PLALp/compute_box_PLALp
       !!****
       CALL LOGCHIC('PLALp')
       CALL compute_box_PLALp
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* PLALp/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PLALp/read_ini_Vloc_Rloc_PLALp
       !!****
       CALL LOGCHIC('PLALp') 
       CALL read_ini_Vloc_Rloc_PLALp
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_PLALp
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_PLALp.inc'
  
END MODULE wrap_PLALp
