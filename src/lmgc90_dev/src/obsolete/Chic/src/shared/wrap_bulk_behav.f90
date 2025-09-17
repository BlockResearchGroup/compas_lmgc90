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
MODULE wrap_BULK_BEHAVIOUR

  !!****h* LMGC90.CHIC/BULK_BEHAVIOUR
  !! NAME
  !!  module wrap_BULK_BEHAVIOUR
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/BULK_BEHAVIOUR
  !!****

  USE CHIC
  USE BULK_BEHAVIOUR
  
  PRIVATE
  
  PUBLIC chic_command_bulk_behav,more_chic_command_bulk_behav
  
CONTAINS
!!!-------------------------------------------------------------------------
  SUBROUTINE chic_command_bulk_behav
 
    IMPLICIT NONE

    IF (INDEX(CHZZZ,'READ BEHAVIOURS')==1) THEN
       !!****e* BULK_BEHAVIOUR/READ BEHAVIOURS
       !! NAME
       !!  READ BEHAVIOURS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/BULK_BEHAVIOUR/read_in_bulk_behav
       !!****
       CALL LOGCHIC('bulk_behav')
       CALL read_in_bulk_behav 
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE BEHAVIOURS')==1) THEN
       !!****e* BULK_BEHAVIOUR/WRITE BEHAVIOURS
       !! NAME
       !!  WRITE BEHAVIOURS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/BULK_BEHAVIOUR/write_out_bulk_behav
       !!****
       CALL LOGCHIC('bulk_behav')
       CALL write_out_bulk_behav
       IETZZZ=1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'COLLECT BULK_BEHAV.OUT')==1) THEN   !  some more CHIC command to be used in preprocessing
       !!****e* BULK_BEHAVIOUR/COLLECT BULK BEHAV.OUT
       !! NAME
       !!  COLLECT BULK_BEHAV.OUT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/BULK_BEHAVIOUR/read_out_bulk_behav
       !!****
       CALL LOGCHIC('bulk_behav')
       CALL read_out_bulk_behav
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'CLEAN BULK_BEHAV.OUT')==1) THEN
       !!****e* BULK_BEHAVIOUR/CLEAN BULK BEHAV.OUT
       !! NAME
       !!  CLEAN BULK_BEHAV.OUT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/BULK_BEHAVIOUR/clean_out_bulk_behav
       !!****
       CALL LOGCHIC('bulk_behav')
       CALL clean_out_bulk_behav
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'APPEND TO BULK_BEHAV.OUT')==1) THEN   !  some more CHIC command to be used in preprocessing
       !!****e* BULK_BEHAVIOUR/APPEND TO BULK BEHAV.OUT
       !! NAME
       !!  APPEND TO BULK_BEHAV.OUT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/BULK_BEHAVIOUR/append_out_bulk_behav
       !!****
       CALL LOGCHIC('bulk_behav')
       CALL append_out_bulk_behav
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REBUILD BULK_BEHAV.DAT')==1) THEN   !  some more CHIC command to be used in preprocessing
       !!****e* BULK_BEHAVIOUR/REBUILD BULK BEHAV.DAT
       !! NAME
       !!  REBUILD BULK_BEHAV.DAT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/BULK_BEHAVIOUR/write_in_bulk_behav
       !!****
       CALL LOGCHIC('bulk_behav')
       CALL write_in_bulk_behav
       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_bulk_behav

  include '../../more_src/shared/inc_bulk_behav.f90'

END MODULE wrap_BULK_BEHAVIOUR
