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
MODULE wrap_PT2Dx

  !!****h* LMGC90.CHIC/PT2Dx
  !! NAME
  !!  module WRAP_PT2Dx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/PT2Dx
  !!****

  USE CHIC

  USE PT2Dx,ONLY:&
       read_bodies_PT2Dx,set_pt2dx_radius_PT2Dx

  PRIVATE
  
  PUBLIC chic_command_PT2Dx,more_chic_command_PT2Dx

CONTAINS

!!!---------------------------------------------------------------------
!!!   SUBROUTINE chic_command_PT2Dx
!!!
!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_PT2Dx

    IMPLICIT NONE
    REAL(kind=8)  :: rtmp

    IF (INDEX(CHZZZ,'READ BODIES')==1) THEN
       !!****e* PT2Dx/READ BODIES
       !! NAME
       !!   READ BODIES
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for PT2Dx contactors
       !! USES
       !!  LMGC90.CORE/PT2Dx/read_bodies_PT2Dx
       !!****
       CALL LOGCHIC('PT2Dx')
       CALL read_bodies_PT2Dx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'PT2Dx RADIUS')==1) THEN
       !!****e* POST2D/PT2Dx RADIUS
       !! NAME
       !!   PT2Dx RADIUS
       !! SYNOPSIS
       !!   PT2Dx RADIUS
       !!   [ref_radius]
       !! INPUTS
       !!  [ref_radius] : value of the reference radius which should be used for contact display
       !! PURPOSE
       !!   Set the reference value for displaying point.
       !! USES
       !!   LMGC90.CORE/POST2D/set_pt2Dx_radius_PT2Dx
       !!****
       READ(CHLZZZ(NZZZ+1),*) rtmp
       CALL LOGCHIC('PT2Dx')
       CALL set_pt2dx_radius_PT2Dx(rtmp)
       IETZZZ=1
       RETURN      
    END IF

  END SUBROUTINE chic_command_PT2Dx
!!!---------------------------------------------------------------------

  include '../../more_src/rigid_2D/wrap_PT2Dx.inc'

END MODULE wrap_PT2Dx
