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
MODULE wrap_post3D

  !!****h* LMGC90.CHIC/POST3D
  !! NAME
  !!  module WRAP_POST3D
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/POST3D
  !!****
  USE CHIC

  USE post3D,ONLY: &
       init_post3D, &
       update_post3D, &
       active_GMV, &
       put_radius_ratio, &
       load_external_surf, &
       set_xperiodic_data_post3D, &
       set_yperiodic_data_post3D, &
       set_ref_radius_post3D

CONTAINS

!!!--------------------------------------------------------------------
  SUBROUTINE chic_command_post3D

    IMPLICIT NONE
    REAL(kind=8) :: RATIO,xperiode,yperiode,rtmp
    LOGICAL      :: XPERIODIC,YPERIODIC

    IF (INDEX(CHZZZ,'INIT POST')==1) THEN
       !!****e* POST3D/INIT POST
       !! NAME
       !!   INIT POST
       !! PURPOSE
       !!  Do all the computation without displaying the value. 
       !!  Needed for a use in combination with LMGC90/POSTPRO
       !! USES
       !!  LMGC90.CORE/POST3D/init_post3D
       !!****
       CALL LOGCHIC('POST_3D')
       CALL init_post3D
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'UPDATE POST')==1) THEN
       !!****e* POST3D/UPDATE POST
       !! NAME
       !!   UPDATE POST
       !! PURPOSE
       !!  Do all the computation without displaying the value. 
       !!  Needed for a use in combination with LMGC90/POSTPRO
       !! USES
       !!  LMGC90.CORE/POST3D/update_post3D
       !!****
       CALL LOGCHIC('POST_3D') 
       CALL update_post3D
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY POSITION')==1) THEN
       !!****e* POST3D/DISPLAY POSITION
       !! NAME
       !!   DISPLAY POSITION
       !! PURPOSE
       !!  Specifies that you want to compute and display the average velocity between two displayed steps
       !! USES
       !!   LMGC90.CORE/POST3D/acti_GMV_position
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('POSIT')
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY AVERAGE VELOCITY')==1) THEN
       !!****e* POST3D/DISPLAY AVERAGE VELOCITY
       !! NAME
       !!   DISPLAY AVERAGE VELOCITY
       !! PURPOSE
       !!  Specifies that you want to compute and display the average velocity between two displayed steps
       !! USES
       !!   LMGC90.CORE/POST3D/acti_GMV_avlocy
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('VELOC')
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OXIDE')==1) THEN
       !!****e* POST3D/DISPLAY OXIDE
       !! NAME
       !!   DISPLAY OXIDE
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_oxide
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('OXIDE')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY ELECTRO')==1) THEN
       !!****e* POST3D/DISPLAY ELECTRO
       !! NAME
       !!   DISPLAY ELECTRO
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_ELECTRO
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('ELECT')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY HEAT')==1) THEN
       !!****e* POST3D/DISPLAY HEAT
       !! NAME
       !!   DISPLAY HEAT
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_heat
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('HEAT_')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY INTERNAL')==1) THEN
       !!****e* POST3D/DISPLAY INTERNAL
       !! NAME
       !!   DISPLAY INTERNAL
       !! PURPOSE
       !!  Specifies that you want to display internal variable
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_internal
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('INTER')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY STRESS')==1) THEN
       !!****e* POST3D/DISPLAY STRESS
       !! NAME
       !!   DISPLAY STRESS
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_stress
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('STRSS')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY FORCES')==1) THEN
       !!****e* POST3D/DISPLAY FORCES
       !! NAME
       !!   DISPLAY FORCES
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_forces
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('FORCE')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY FLUID')==1) THEN
       !!****e* POST3D/DISPLAY FLUID
       !! NAME
       !!   DISPLAY FLUID
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST3D/acti_GMV_fluid
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('FLUID')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY MECHANICAL GPV')==1) THEN
       !!****e* POST3D/DISPLAY POSITION
       !! NAME
       !!   DISPLAY POSITION
       !! PURPOSE
       !!  Specifies that you want to compute and display stress and strain
       !! USES
       !!   LMGC90.CORE/POST3D/acti_GMV_position
       !!****
       CALL LOGCHIC('post3D')
       CALL active_GMV('MECAx')
       IETZZZ = 1
       RETURN      
    END IF


    IF (INDEX(CHZZZ,'SET RADIUS PT3Dx')==1) THEN
       !!****e* POST3D/SET RADIUS PT3Dx
       !! NAME
       !!  SET RADIUS PT3Dx
       !! SYNOPSIS
       !!  SET RADIUS PT3Dx
       !!  [radius]
       !! PURPOSE
       !!  set the sphere radius used to display the PT3Dx
       !! USES
       !!  LMGC90.CORE/post3D/put_radius_ratio
       !!****
       CALL LOGCHIC('post3D')
       READ(CHLZZZ(NZZZ+1),*) RATIO
       CALL put_radius_ratio(RATIO)
    END IF

    IF (INDEX(CHZZZ,'LOAD EXTERNAL SURF')==1) THEN
       !!****e* POST3D/LOAD EXTERNAL SURF
       !! NAME
       !!  LOAD EXTERNAL SURF
       !! PURPOSE
       !!  load surf.txt which contains the description of boundary
       !! USES
       !!  LMGC90.CORE/post3D/load_external_surf
       !!****
       CALL LOGCHIC('post3D')
       CALL load_external_surf
       IETZZZ = 1
    END IF

    IF (INDEX(CHZZZ,'XPERIODIC CONDITION')==1) THEN
       !!****e* POST3D/XPERIODIC CONDITION
       !! NAME
       !!  XPERIODIC CONDITION
       !! SYNOPSIS
       !!  XPERIODIC CONDITION
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/POST3D/set_xperiodic_data_post3D
       !!****
       CALL LOGCHIC('post3D') 
       READ(CHLZZZ(NZZZ+1),*) xperiode
       XPERIODIC=.TRUE.
       CALL set_xperiodic_data_post3D(xperiode,XPERIODIC)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'YPERIODIC CONDITION')==1) THEN
       !!****e* POST3D/YPERIODIC CONDITION
       !! NAME
       !!  YPERIODIC CONDITION
       !! SYNOPSIS
       !!  YPERIODIC CONDITION
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/POST3D/set_yperiodic_data_post3D
       !!****
       CALL LOGCHIC('post3D') 
       READ(CHLZZZ(NZZZ+1),*) yperiode
       YPERIODIC=.TRUE.
       CALL set_yperiodic_data_post3D(yperiode,YPERIODIC)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REFERENCE RADIUS')==1) THEN
       !!****e* POST3D/REFERENCE RADIUS
       !! NAME
       !!   REFERENCE RADIUS
       !! SYNOPSIS
       !!   REFERENCE RADIUS
       !!   [ref_radius]
       !! INPUTS
       !!  [ref_radius] : value of the reference radius which should be used for contact display
       !! PURPOSE
       !!   Set the reference value for displaying the contact forces. 
       !!   It is related to the size of the smallest body usually a ratio of 0.1 works
       !! USES
       !!   LMGC90.CORE/POST3D/set_ref_radius_post3D
       !!****
       READ(CHLZZZ(NZZZ+1),*) rtmp
       CALL LOGCHIC('post3D')
       CALL set_ref_radius_post3D(rtmp)
       IETZZZ=1
       RETURN      
    END IF

  END SUBROUTINE chic_command_post3D
!!!--------------------------------------------------------------------

END MODULE wrap_post3D
