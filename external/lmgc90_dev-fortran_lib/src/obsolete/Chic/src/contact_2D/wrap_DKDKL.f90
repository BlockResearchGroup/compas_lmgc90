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
MODULE wrap_DKDKL

  !!****h* LMGC90.CHIC/DKDKL
  !! NAME
  !!  module wrap_DKDKL
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DKDKL
  !!****

  USE CHIC

  USE DKDKL,ONLY: &
       get_nb_DKDKL, &
       get_nb_rough_DKDKL, &
       get_nb_recup_DKDKL, &
       stock_rloc_DKDKL, &
       recup_rloc_DKDKL, &
       smooth_computation_DKDKL, &
       compute_box_DKDKL, &
       read_ini_Vloc_Rloc_DKDKL, &
       write_xxx_Vloc_Rloc_DKDKL, &
       coor_prediction_DKDKL, &
       creation_tab_visu_DKDKL, &
       compute_contact_DKDKL, &
       display_prox_tactors_DKDKL, &
       RUN_DKDKL, &
       CHECK_DKDKL, &
       get_write_Vloc_Rloc_DKDKL,&
       set_periodic_data_DKDKL
CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_DKDKL 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_DKDKL,nb_rough_DKDKL
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout

    CHECK = CHECK_DKDKL()

    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* DKDKL/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx and DISKL tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKDKL/coor_prediction_DKDKL
       !!  LMGC90.CORE/DKDKL/creation_tab_visu_DKDKL
       !!  LMGC90.CORE/DKDKL/compute_contact_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
           
       CALL coor_prediction_DKDKL
       
       RUN = RUN_DKDKL()

       IF (RUN) THEN
          CALL creation_tab_visu_DKDKL
          IF(KHOZZZ == 1)THEN
             nb_rough_DKDKL = get_nb_rough_DKDKL()
             WRITE(cout,'(1X,I10,A20)') nb_rough_DKDKL,' DKDKL roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_DKDKL
       IF (KHOZZZ == 1) THEN
          nb_DKDKL = get_nb_DKDKL()
          WRITE(cout,'(1X,I10,A12)') nb_DKDKL,' DKDKL found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* DKDKL/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKDKL/stock_rloc_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
       CALL stock_rloc_DKDKL 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* DKDKL/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDKL/recup_rloc_DKDKL
       !!****
       CALL LOGCHIC('DKDKL') 
       CALL recup_rloc_DKDKL
       IF (KHOZZZ == 1) THEN
          nb_DKDKL = get_nb_recup_DKDKL()
          WRITE(cout,'(1X,I10,A12)') nb_DKDKL,' recup DKDKL'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SMOOTH FORCE COMPUTATION')==1) THEN
       !!****e* DKDKL/SMOOTH FORCE COMPUTATION
       !! NAME 
       !!  SMOOTH FORCE COMPUTATION
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDKL/smooth_computation_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
       CALL smooth_computation_DKDKL
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* DKDKL/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/write_xxx_Vloc_Rloc_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
       CALL write_xxx_Vloc_Rloc_DKDKL(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* DKDKL/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/write_xxx_Vloc_Rloc_DKDKL
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDKL()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('DKDKL') 
          CALL write_xxx_Vloc_Rloc_DKDKL(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* DKDKL/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/write_xxx_Vloc_Rloc_DKDKL
       !!****
       CALL LOGCHIC('DKDKL') 
       CALL write_xxx_Vloc_Rloc_DKDKL(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* DKDKL/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/display_prox_tactors_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
       CALL display_prox_tactors_DKDKL
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* DKDKL/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKDKL/compute_box_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
       CALL compute_box_DKDKL
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* DKDKL/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKDKL/read_ini_Vloc_Rloc_DKDKL
       !!****
       CALL LOGCHIC('DKDKL') 
       CALL read_ini_Vloc_Rloc_DKDKL
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'PERIODIC CONDITION')==1) THEN
       !!****e* DKDKL/PERIODIC CONDITION
       !! NAME
       !!  PERIODIC CONDITION
       !! SYNOPSIS
       !!  PERIODIC CONDITION
       !!  [periode]
       !! INPUTS
       !!  [periode] : periode of periodic condition
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       !! USES
       !!  LMGC90.CORE/DKDKL/set_periodic_data_DKDKL
       !!****
       CALL LOGCHIC('DKDKL')
       READ(CHLZZZ(NZZZ+1),*) periode
       PERIODIC=.TRUE.
       CALL set_periodic_data_DKDKL(periode,PERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_DKDKL
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_DKDKL.inc' 
  
END MODULE wrap_DKDKL
