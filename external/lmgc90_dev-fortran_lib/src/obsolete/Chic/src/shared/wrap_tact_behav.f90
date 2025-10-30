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
MODULE wrap_TACT_BEHAVIOUR

  !!****h* LMGC90.CHIC/TACT_BEHAVIOUR
  !! NAME
  !!  module wrap_TACT_BEHAVIOUR
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!****

  USE CHIC
  USE tact_behaviour
  
  PRIVATE
  
  PUBLIC chic_command_tact_behav,more_chic_command_tact_behav
  
CONTAINS
!!!-------------------------------------------------------------------------
  SUBROUTINE chic_command_tact_behav

    IMPLICIT NONE
    real(kind=8) :: halo

    IF (INDEX(CHZZZ,'READ BEHAVIOURS')==1) THEN
       !!****e* TACT_BEHAVIOUR/READ BEHAVIOURS
       !! NAME
       !!  READ BEHAVIOURS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/read_xxx_tact_behav
       !!****
       CALL LOGCHIC('tact_behav')
       CALL read_xxx_tact_behav(1)
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COLLECT TACT_BEHAV.OUT')==1) THEN   !  some more CHIC command to be used in preprocessing
       !!****e* TACT_BEHAVIOUR/COLLECT TACT_BEHAV.OUT
       !! NAME
       !!  COLLECT TACT_BEHAV.OUT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/read_xxx_tact_behav
       !!****
       CALL LOGCHIC('tact_behav')
       CALL read_xxx_tact_behav(2)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'WRITE BEHAVIOURS')==1) THEN
       !!****e* TACT_BEHAVIOUR/WRITE BEHAVIOURS
       !! NAME
       !!  WRITE BEHAVIOURS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/write_xxx_tact_behav
       !!****
       CALL LOGCHIC('tact_behav')
       CALL write_xxx_tact_behav(2)
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'APPEND TO TACT_BEHAV.OUT')==1) THEN   !  some more CHIC command to be used in preprocessing
       !!****e* TACT_BEHAVIOUR/APPEND TO TACT_BEHAV.OUT
       !! NAME
       !!  APPEND TO TACT_BEHAV.OUT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/write_xxx_tact_behav
       !!****
       CALL LOGCHIC('tact_behav')
       CALL write_xxx_tact_behav(3)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REBUILD TACT_BEHAV.DAT')==1) THEN   !  some more CHIC command to be used in preprocessing
       !!****e* TACT_BEHAVIOUR/REBUILD TACT_BEHAV.DAT
       !! NAME
       !!  REBUILD TACT_BEHAV.DAT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/write_xxx_tact_behav
       !!****
       CALL LOGCHIC('tact_behav')
       CALL write_xxx_tact_behav(1)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CLEAN TACT_BEHAV.OUT')==1) THEN
       !!****e* TACT_BEHAVIOUR/CLEAN TACT_BEHAV.OUT
       !! NAME
       !!  CLEAN TACT_BEHAV.OUT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/clean_out_tact_behav
       !!****
       CALL LOGCHIC('overall')
       CALL clean_out_tact_behav
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SET CZM INITIAL FRICTION')==1) THEN
       !!****e* TACT_BEHAVIOUR/SET CZM INITIAL FRICTION
       !! NAME
       !!  SET CZM INITIAL FRICTION
       !! PURPOSE
       !!  unable linear friction coefficient dependence to damage
       !! USES
       !!  LMGC90.CORE/TACT_BEHAVIOUR/set_czm_initial_friction
       !!****
       CALL LOGCHIC('overall')
       CALL set_czm_initial_friction
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SET HALO')==1) THEN
       !!****e* TACT_BEHAV/SET HALO
       !! NAME
       !!  SET HALO
       !! SYNOPSIS
       !!  SET HALO
       !!  [halo]
       !! INPUTS
       !!  [halo] : 
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/TACT_BEHAV/set_halo_CLALp
       !!****
       CALL LOGCHIC('overall')
       READ(CHLZZZ(NZZZ+1),*) halo
       CALL set_halo(halo)
       IETZZZ = 1
       RETURN      
    ENDIF


  END SUBROUTINE chic_command_tact_behav

  include '../../more_src/shared/wrap_tact_behav.inc'

END MODULE wrap_TACT_BEHAVIOUR
