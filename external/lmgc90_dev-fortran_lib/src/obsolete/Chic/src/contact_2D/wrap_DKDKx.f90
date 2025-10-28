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
MODULE wrap_DKDKx

  !!****h* LMGC90.CHIC/DKDKx
  !! NAME
  !!  module wrap_DKDKx
  !! USES
  !!  file:/home0/perales/DOC-CODE/LMGC90v2_dev/Docs/HTML/Core/src/contact_2D/mod_DKDKx_f90.html#CORE2fDKDKx
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DKDKx
  !!****

  !!  file:../../../Core/src/contact_2D/mod_DKDKx.f90#CORE2fDKDKx

  USE CHIC

  USE DKDKx,ONLY: &
       get_nb_DKDKx,&
       get_nb_rough_DKDKx,&
       get_nb_recup_DKDKx, &
       stock_rloc_DKDKx, &
       recup_rloc_DKDKx, &
       smooth_computation_DKDKx, &
       compute_box_DKDKx, &
       read_ini_Vloc_Rloc_DKDKx, &
       write_xxx_Vloc_Rloc_DKDKx, &
       set_periodic_data_DKDKx, &
       coor_prediction_DKDKx, &
       creation_tab_visu_DKDKx, &
       compute_contact_DKDKx, &
       display_prox_tactors_DKDKx, &
       RUN_DKDKx, &
       CHECK_DKDKx, &
       get_write_Vloc_Rloc_DKDKx, &
       set_vav_DKDKx

CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_DKDKx 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_DKDKx,nb_rough_DKDKx,NBN
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout

    CHECK = CHECK_DKDKx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* DKDKx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between DISKx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  file:/home0/perales/DOC-CODE/LMGC90v2_dev/Docs/HTML/Core/src/contact_2D/mod_DKDKx_f90.html#DKDKx2fcoor5fprediction5fDKDKx
       !!  LMGC90.CORE/DKDKx/creation_tab_visu_DKDKx
       !!  LMGC90.CORE/DKDKx/compute_contact_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')

       CALL coor_prediction_DKDKx

       RUN = RUN_DKDKx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKDKx
          IF(KHOZZZ == 1)THEN
             nb_rough_DKDKx = get_nb_rough_DKDKx()
             WRITE(cout,'(1X,I10,A20)') nb_rough_DKDKx,' DKDKx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_DKDKx
       IF (KHOZZZ == 1) THEN
          nb_DKDKx = get_nb_DKDKx()
          WRITE(cout,'(1X,I10,A12)') nb_DKDKx,' DKDKx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* DKDKx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKDKx/stock_rloc_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')
       CALL stock_rloc_DKDKx 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* DKDKx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDKx/recup_rloc_DKDKx
       !!****
       CALL LOGCHIC('DKDKx') 
       CALL recup_rloc_DKDKx
       IF (KHOZZZ == 1) THEN
          nb_DKDKx = get_nb_recup_DKDKx()
          WRITE(cout,'(1X,I10,A12)') nb_DKDKx,' recup DKDKx'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SMOOTH FORCE COMPUTATION')==1) THEN
       !!****e* DKDKx/SMOOTH FORCE COMPUTATION
       !! NAME 
       !!  SMOOTH FORCE COMPUTATION
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDKx/smooth_computation_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')
       CALL smooth_computation_DKDKx
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'USE VIS-A-VIS DETECTION')==1) THEN
       !!****e* PRPRx/USE VIS-A-VIS DETECTION
       !! NAME
       !!  USE VIS-A-VIS DETECTION
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/set_vav_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')
       READ(CHLZZZ(NZZZ+1),*) NBN
       CALL set_vav_DKDKx(NBN)
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* DKDKx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDKx contacts
       !! USES
       !!  LMGC90.CORE/DKDKx/write_xxx_Vloc_Rloc_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')
       CALL write_xxx_Vloc_Rloc_DKDKx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* DKDKx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDKx contacts
       !! USES
       !!  LMGC90.CORE/DKDKx/write_xxx_Vloc_Rloc_DKDKx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDKx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('DKDKx') 
          CALL write_xxx_Vloc_Rloc_DKDKx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* DKDKx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDKx contacts
       !! USES
       !!  LMGC90.CORE/DKDKx/write_xxx_Vloc_Rloc_DKDKx
       !!****
       CALL LOGCHIC('DKDKx') 
       CALL write_xxx_Vloc_Rloc_DKDKx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* DKDKx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKDKx/display_prox_tactors_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')
       CALL display_prox_tactors_DKDKx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* DKDKx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKDKx/compute_box_DKDKx
       !!****

!!  file:../../../Core/src/contact_2D/mod_DKDKx.f90#CORE2fDKDKx2fcompute5fbox5fDKDKx

       CALL LOGCHIC('DKDKx')
       CALL compute_box_DKDKx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* DKDKx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKDKx/read_ini_Vloc_Rloc_DKDKx
       !!****
       CALL LOGCHIC('DKDKx') 
       CALL read_ini_Vloc_Rloc_DKDKx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'PERIODIC CONDITION')==1) THEN
       !!****e* DKDKx/PERIODIC CONDITION
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
       !!  LMGC90.CORE/DKDKx/set_periodic_data_DKDKx
       !!****
       CALL LOGCHIC('DKDKx')
       READ(CHLZZZ(NZZZ+1),*) periode
       PERIODIC=.TRUE.
       CALL set_periodic_data_DKDKx(periode,PERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_DKDKx
!!!-------------------------------------------------------
  
  include '../../more_src/contact_2D/wrap_DKDKx.inc'


END MODULE wrap_DKDKx
