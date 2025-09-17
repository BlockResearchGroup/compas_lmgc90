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
MODULE wrap_PLPLx

  !!****h* LMGC90.CHIC/PLPLx
  !! NAME
  !!  module wrap_PLPLx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/PLPLx
  !!****

  USE CHIC

  USE PLPLx,ONLY: &
       get_nb_PLPLx, &
       get_nb_vPLPLx, &
       get_nb_rough_PLPLx, &
       get_nb_recup_PLPLx, &
       stock_rloc_PLPLx, &
       recup_rloc_PLPLx, &
       recup_rloc_byposition_PLPLx, &
       compute_box_PLPLx, &
       read_ini_Vloc_Rloc_PLPLx, &
       write_xxx_Vloc_Rloc_PLPLx, &
       set_periodic_data_PLPLx, &
       coor_prediction_PLPLx, &
       creation_tab_visu_PLPLx, &
       compute_contact_PLPLx, &
       display_prox_tactors_PLPLx, &
       RUN_PLPLx, &
       CHECK_PLPLx, &
       get_write_Vloc_Rloc_PLPLx
  
CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_PLPLx 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_PLPLx,nb_rough_PLPLx
    REAL(kind=8) :: periode  = 0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc
!
    real(kind=8) :: tol_RecupRloc=1d-10
    logical      :: new_recup=.false.
    
    CHARACTER(len=35) :: cout

    CHECK = CHECK_PLPLx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* PLPLx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between POLYG tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PLPLx/coor_prediction_PLPLx
       !!  LMGC90.CORE/PLPLx/creation_tab_visu_PLPLx
       !!  LMGC90.CORE/PLPLx/compute_contact_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
          
       CALL coor_prediction_PLPLx
       
       RUN = RUN_PLPLx()

       IF (RUN) THEN
          CALL creation_tab_visu_PLPLx
          IF(KHOZZZ == 1)THEN
             nb_rough_PLPLx = get_nb_rough_PLPLx()
             WRITE(cout,'(1X,I10,A20)') nb_rough_PLPLx,' PLPLx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_PLPLx
       IF (KHOZZZ == 1) THEN
          nb_PLPLx = get_nb_PLPLx()
          WRITE(cout,'(1X,I10,A12)') nb_PLPLx,' PLPLx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* PLPLx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PLPLx/stock_rloc_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
       CALL stock_rloc_PLPLx 
       IF (KHOZZZ == 1) THEN
          WRITE(cout,'(1X,I10,A12)') get_nb_vPLPLx(),' stored PLPLx'
          CALL LOGMESCHIC(cout)
       END IF 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* PLPLx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PLPLx/recup_rloc_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
       if (new_recup) then
         call recup_rloc_ByPosition_PLPLx(tol_RecupRloc) 
       else
         CALL recup_rloc_PLPLx
       endif
       IF (KHOZZZ == 1) THEN
          nb_PLPLx = get_nb_recup_PLPLx()
          WRITE(cout,'(1X,I10,A12)') nb_PLPLx,' recup PLPLx'
          CALL LOGMESCHIC(cout)
       END IF 
       IETZZZ = 1
       RETURN 
    END IF

    if (INDEX(CHZZZ,'SET TOL NEW RECUP Rloc')==1) then 
      call LOGCHIC('PLPLx')
      read(CHLZZZ(NZZZ+1),*) tol_RecupRloc
      new_recup=.true.
      IETZZZ = 1
      return 
    endif

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* PLPLx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PLPLx contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/write_xxx_Vloc_Rloc_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
       CALL write_xxx_Vloc_Rloc_PLPLx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* PLPLx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PLPLx contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/write_xxx_Vloc_Rloc_PLPLx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PLPLx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('PLPLx') 
          CALL write_xxx_Vloc_Rloc_PLPLx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* PLPLx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PLPLx contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/write_xxx_Vloc_Rloc_PLPLx
       !!****
       CALL LOGCHIC('PLPLx') 
       CALL write_xxx_Vloc_Rloc_PLPLx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* PLPLx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/display_prox_tactors_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
       CALL display_prox_tactors_PLPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* PLPLx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/PLPLx/compute_box_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
       CALL compute_box_PLPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       CALL LOGCHIC('PLPLx') 
       CALL read_ini_Vloc_Rloc_PLPLx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'PERIODIC CONDITION')==1) THEN
       !!****e* PLPLx/PERIODIC CONDITION
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
       !!  LMGC90.CORE/PLPLx/set_periodic_data_PLPLx
       !!****
       CALL LOGCHIC('PLPLx')
       READ(CHLZZZ(NZZZ+1),*) periode
       PERIODIC=.TRUE.
       CALL set_periodic_data_PLPLx(periode,PERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF


  END SUBROUTINE chic_command_PLPLx

!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_PLPLx.inc'
  
END MODULE wrap_PLPLx
