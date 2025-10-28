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
MODULE wrap_CSASp

  !!****h* LMGC90.CHIC/CSASp
  !! NAME
  !!  module wrap_CSASp
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/CSASp
  !!****

  USE CHIC
  USE CSASp,ONLY:&
       coor_prediction_CSASx,&
       CHECK_CSASx,&
       RUN_CSASx, &
       get_write_Vloc_Rloc_CSASx, &
       read_ini_Vloc_Rloc_CSASx,&
       write_xxx_Vloc_Rloc_CSASx,&
       stock_rloc_CSASx, &
       recup_rloc_CSASx, &
       creation_tab_visu_CSASx, &
       compute_contact_CSASx, &
       display_prox_tactors_CSASx,&
       get_nb_CSASx, &
       initialize_CSASp !, &
!       set_xperiodic_data_CSASx, &
!       set_yperiodic_data_CSASx
  

CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_CSASp

    IMPLICIT NONE

    INTEGER      :: iter=0,nb_CSASx
    REAL(kind=8) :: periode  = 0.D0,SHRINK=0.5,sfactor
    LOGICAL      :: RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    INTEGER :: RESTART=0
    LOGICAL :: is_first_time = .TRUE.
    LOGICAL :: save_CSASx_to_file=.FALSE.,load_CSASx_from_file=.FALSE.
    CHARACTER(len=2) ::prefix
    CHARACTER(len=35) :: cout

!!! internal values for tactor selection

    INTEGER,PARAMETER :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , i_real_tactor = 4

    CHECK = CHECK_CSASx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1) THEN
       !!****e* CSASp/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between POLYR tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CSASp/coor_prediction_CSASx
       !!  LMGC90.CORE/CSASp/creation_tab_visu_CSASx
       !!  LMGC90.CORE/CSASp/compute_contact_CSASx
       !!****
       CALL LOGCHIC('CSASp')

       CALL coor_prediction_CSASx

       RUN = RUN_CSASx()
       
       IF (RUN) THEN
          CALL creation_tab_visu_CSASx
          IF(KHOZZZ == 1)THEN
             nb_CSASx = get_nb_CSASx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_CSASx,' CSASx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_CSASx
       IF (KHOZZZ == 1) THEN
          nb_CSASx = get_nb_CSASx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CSASx,' CSASx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF
    
!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* CSASp/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CSASp/stock_rloc_CSASx
       !!****
       CALL LOGCHIC('CSASp')
       CALL stock_rloc_CSASx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* CSASp/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CSASp/recup_rloc_CSASx
       !!****
       CALL LOGCHIC('CSASp') 
       CALL recup_rloc_CSASx
       IF (KHOZZZ == 1) THEN
          nb_CSASx = get_nb_CSASx(i_recup_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CSASx,' recup CSASp'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* CSASp/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CSASx contacts
       !! USES
       !!  LMGC90.CORE/CSASp/write_xxx_Vloc_Rloc_CSASx
       !!****
       CALL LOGCHIC('CSASp')
       CALL write_xxx_Vloc_Rloc_CSASx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* CSASp/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CSASx contacts
       !! USES
       !!  LMGC90.CORE/CSASp/write_xxx_Vloc_Rloc_CSASx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CSASx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('CSASp') 
          CALL write_xxx_Vloc_Rloc_CSASx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* CSASp/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CSASx contacts
       !! USES
       !!  LMGC90.CORE/CSASp/write_xxx_Vloc_Rloc_CSASx
       !!****
       CALL LOGCHIC('CSASp') 
       CALL write_xxx_Vloc_Rloc_CSASx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* CSASp/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CSASp/display_prox_tactors_CSASx
       !!****
       CALL LOGCHIC('CSASp')
       CALL display_prox_tactors_CSASx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* CSASp/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CSASp/read_ini_Vloc_Rloc_CSASx
       !!****
       CALL LOGCHIC('CSASp') 
       CALL read_ini_Vloc_Rloc_CSASx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* CSASp/COMPUTE BOX
       !! NAME 
       !!  COMPUTE BOX 
       !! PURPOSE
       !!  initialize the CSAS module
       !! USES
       !!  LMGC90.CORE/CSAS/initialize_CSASp
       !!****
       CALL LOGCHIC('CSASp')

       CALL initialize_CSASp 

       IETZZZ = 1
       RETURN
    END IF




  END SUBROUTINE chic_command_CSASp
!!!---------------------------------------------------------------------

END MODULE wrap_CSASp
