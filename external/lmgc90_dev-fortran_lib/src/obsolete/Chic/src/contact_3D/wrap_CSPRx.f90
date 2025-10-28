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
MODULE wrap_CSPRx

  !!****h* LMGC90.CHIC/PRPRx
  !! NAME
  !!  module wrap_PRPRx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/PRPRx
  !!****

  USE CHIC
  USE CSPRx,ONLY:&
       coor_prediction_CSPRx,&
       CHECK_CSPRx,&
       RUN_CSPRx, &
       get_write_Vloc_Rloc_CSPRx, &
       read_ini_Vloc_Rloc_CSPRx,&
       write_xxx_Vloc_Rloc_CSPRx,&
       stock_rloc_CSPRx, &
       recup_rloc_CSPRx, &
       creation_tab_visu_CSPRx, &
       compute_contact_CSPRx, &
       display_prox_tactors_CSPRx,&
       get_nb_CSPRx !, &
!       set_xperiodic_data_CSPRx, &
!       set_yperiodic_data_CSPRx
  

CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_CSPRx

    IMPLICIT NONE

    INTEGER      :: iter=0,nb_CSPRx
    REAL(kind=8) :: periode  = 0.D0,SHRINK=0.5,sfactor
    LOGICAL      :: RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    INTEGER :: RESTART=0
    LOGICAL :: is_first_time = .TRUE.
    LOGICAL :: save_CSPRx_to_file=.FALSE.,load_CSPRx_from_file=.FALSE.
    CHARACTER(len=2) ::prefix
    CHARACTER(len=35) :: cout

!!! internal values for tactor selection

    INTEGER,PARAMETER :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , i_real_tactor = 4

    CHECK = CHECK_CSPRx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1) THEN
       !!****e* CSPRx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between POLYR tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CSPRx/coor_prediction_CSPRx
       !!  LMGC90.CORE/CSPRx/creation_tab_visu_CSPRx
       !!  LMGC90.CORE/CSPRx/compute_contact_CSPRx
       !!****
       CALL LOGCHIC('CSPRx')

       CALL coor_prediction_CSPRx

       RUN = RUN_CSPRx()
       
       IF (RUN) THEN
          CALL creation_tab_visu_CSPRx
          IF(KHOZZZ == 1)THEN
             nb_CSPRx = get_nb_CSPRx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_CSPRx,' CSPRx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_CSPRx
       IF (KHOZZZ == 1) THEN
          nb_CSPRx = get_nb_CSPRx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CSPRx,' CSPRx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF
    
!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* CSPRx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CSPRx/stock_rloc_CSPRx
       !!****
       CALL LOGCHIC('CSPRx')
       CALL stock_rloc_CSPRx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* CSPRx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CSPRx/recup_rloc_CSPRx
       !!****
       CALL LOGCHIC('CSPRx') 
       CALL recup_rloc_CSPRx
       IF (KHOZZZ == 1) THEN
          nb_CSPRx = get_nb_CSPRx(i_recup_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CSPRx,' recup CSPRx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* CSPRx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CSPRx contacts
       !! USES
       !!  LMGC90.CORE/CSPRx/write_xxx_Vloc_Rloc_CSPRx
       !!****
       CALL LOGCHIC('CSPRx')
       CALL write_xxx_Vloc_Rloc_CSPRx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* CSPRx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CSPRx contacts
       !! USES
       !!  LMGC90.CORE/CSPRx/write_xxx_Vloc_Rloc_CSPRx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CSPRx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('CSPRx') 
          CALL write_xxx_Vloc_Rloc_CSPRx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* CSPRx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CSPRx contacts
       !! USES
       !!  LMGC90.CORE/CSPRx/write_xxx_Vloc_Rloc_CSPRx
       !!****
       CALL LOGCHIC('CSPRx') 
       CALL write_xxx_Vloc_Rloc_CSPRx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* CSPRx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CSPRx/display_prox_tactors_CSPRx
       !!****
       CALL LOGCHIC('CSPRx')
       CALL display_prox_tactors_CSPRx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* CSPRx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CSPRx/compute_box_CSPRx
       !!****
       CALL LOGCHIC('CSPRx')

       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* CSPRx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CSPRx/read_ini_Vloc_Rloc_CSPRx
       !!****
       CALL LOGCHIC('CSPRx') 
       CALL read_ini_Vloc_Rloc_CSPRx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_CSPRx
!!!---------------------------------------------------------------------

END MODULE wrap_CSPRx
