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
MODULE wrap_POLYG

  !!****h* LMGC90.CHIPY/POLYG
  !! NAME
  !!  module wrap_POLYG
  !! PURPOSE
  !!  Wrap the public API
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/POLYG
  !!****

  USE POLYG,ONLY:&
       read_bodies_POLYG, &
       change_polyg2mailx, &
       change_polyg2mailx_fine

  PUBLIC
  
CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE LoadTactors

    IMPLICIT NONE

       !!****e* POLYG/LoadTactors
       !! NAME
       !!   wrap_polyg.readbodies
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for POLYG contactors
       !! USES
       !!  LMGC90.CORE/POLYG/read_bodies_POLYG
       !!****

       CALL read_bodies_POLYG

       RETURN      

  END SUBROUTINE

!!!---------------------------------------------------------------------

  SUBROUTINE Polyg2Mailx

    IMPLICIT NONE

       !!****e* POLYG/Polyg2Mailx
       !! NAME
       !!   Polyg2Mailx
       !! PURPOSE
       !!  transformation of POLYG in MAILx
       !! USES
       !!  LMGC90.CORE/POLYG/change_polyg2mailx
       !!****
       print*,' @ Warning: we transform all POLYG into MAILx' 
       CALL change_polyg2mailx

       STOP
  
  END SUBROUTINE

!!!---------------------------------------------------------------------

  SUBROUTINE POLYG2fineMAILx

    IMPLICIT NONE

       !!****e* POLYG/Polyg2fineMAILx
       !! NAME
       !!   Polyg2fineMAILx
       !! PURPOSE
       !!  transformation of POLYG in MAILx
       !! USES
       !!  LMGC90.CORE/POLYG/change_polyg2mailx_fine
       !!****

       print*,' @ Warning: we transform all POLYG into MAILx' 
       CALL change_polyg2mailx_fine

       STOP
  END SUBROUTINE

END MODULE wrap_POLYG
