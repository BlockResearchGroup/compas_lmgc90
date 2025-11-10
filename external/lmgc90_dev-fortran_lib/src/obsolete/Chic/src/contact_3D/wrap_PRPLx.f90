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
MODULE wrap_PRPLx

  !!****h* LMGC90.CHIC/PRPLx
  !! NAME
  !!  module wrap_PRPLx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/PRPLx
  !!****

  USE CHIC
  USE PRPLx,ONLY:&
       coor_prediction_PRPLx,&
       CHECK_PRPLx,&
       RUN_PRPLx, &
       get_write_Vloc_Rloc_PRPLx, &
       read_ini_Vloc_Rloc_PRPLx,&
       write_xxx_Vloc_Rloc_PRPLx,&
       stock_rloc_PRPLx, &
       recup_rloc_PRPLx, &
       creation_tab_visu_PRPLx, &
       compute_contact_PRPLx, &
       display_prox_tactors_PRPLx,&
       get_nb_PRPLx
  
CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_PRPLx

    IMPLICIT NONE

    INTEGER      :: nb_PRPLx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc
!!! internal values for tactor selection

    INTEGER,PARAMETER :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , i_real_tactor = 4

    CHARACTER(len=35) :: cout

    CHECK = CHECK_PRPLx()
    
    IF (.NOT.CHECK) RETURN

    IF ( (INDEX(CHZZZ,'SELECT PROX TACTORS')==1).OR. &
         (INDEX(CHZZZ,'WCP SELECT PROX TACTORS')==1)) THEN
       !!****e* PRPLx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between POLYR and PLANx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PRPLx/coor_prediction_PRPLx
       !!  LMGC90.CORE/PRPLx/creation_tab_visu_PRPLx
       !!  LMGC90.CORE/PRPLx/compute_contact_PRPLx
       !!****
       CALL LOGCHIC('PRPLx')

       CALL coor_prediction_PRPLx

       RUN = RUN_PRPLx()

       IF (RUN) THEN
          CALL creation_tab_visu_PRPLx
          IF(KHOZZZ == 1)THEN
             nb_PRPLx = get_nb_PRPLx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_PRPLx,' PRPLx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_PRPLx
       IF (KHOZZZ == 1) THEN
          nb_PRPLx = get_nb_PRPLx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_PRPLx,' PRPLx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* PRPLx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PRPLx/stock_rloc_PRPLx
       !!****
       CALL LOGCHIC('PRPLx')
       CALL stock_rloc_PRPLx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* PRPLx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PRPLx/recup_rloc_PRPLx
       !!****
       CALL LOGCHIC('PRPLx') 
       CALL recup_rloc_PRPLx
       IF (KHOZZZ == 1) THEN
          nb_PRPLx = get_nb_PRPLx(i_recup_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_PRPLx,' recup PRPLx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'MD FORCE COMPUTATION')==1) THEN
       !!****e* PRPLx/MD FORCE COMPUTATION
       !! NAME 
       !!  MD FORCE COMPUTATION
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PRPLx/md_computation_PRPLx
       !!****
       CALL LOGCHIC('PRPLx')
       !CALL md_computation_PRPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DEM FORCE COMPUTATION')==1) THEN
       !!****e* PRPLx/DEM FORCE COMPUTATION
       !! NAME 
       !!  DEM FORCE COMPUTATION
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPLx/dem_computation_PRPLx
       !!****
       CALL LOGCHIC('PRPLx')
       !CALL dem_computation_PRPLx
       IETZZZ = 1
       RETURN 
    END IF
!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* PRPLx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PRPLx contacts
       !! USES
       !!  LMGC90.CORE/PRPLx/write_xxx_Vloc_Rloc_PRPLx
       !!****
       CALL LOGCHIC('PRPLx')
       CALL write_xxx_Vloc_Rloc_PRPLx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* PRPLx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PRPLx contacts
       !! USES
       !!  LMGC90.CORE/PRPLx/write_xxx_Vloc_Rloc_PRPLx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PRPLx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('PRPLx') 
          CALL write_xxx_Vloc_Rloc_PRPLx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* PRPLx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PRPLx contacts
       !! USES
       !!  LMGC90.CORE/PRPLx/write_xxx_Vloc_Rloc_PRPLx
       !!****
       CALL LOGCHIC('PRPLx') 
       CALL write_xxx_Vloc_Rloc_PRPLx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* PRPLx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PRPLx/display_prox_tactors_PRPLx
       !!****
       CALL LOGCHIC('PRPLx')
       CALL display_prox_tactors_PRPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* PRPLx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPLx/read_ini_Vloc_Rloc_PRPLx
       !!****
       CALL LOGCHIC('PRPLx') 
       CALL read_ini_Vloc_Rloc_PRPLx
       IETZZZ = 1
       RETURN 
    END IF

  END SUBROUTINE chic_command_PRPLx
!!!---------------------------------------------------------------------

END MODULE wrap_PRPLx
