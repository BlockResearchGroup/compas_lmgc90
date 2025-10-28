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
MODULE wrap_post2D

  !!****h* LMGC90.CHIC/POST2D
  !! NAME
  !!  module wrap_post2D
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/POST2D
  !!****
  USE CHIC

  USE post2D,ONLY: &
       init_GMV_post2D, &
       update_post2D, &
       write_GMV_post2D, &
       set_ref_reac_max_post2D, &
       set_displ_ampl_post2D, &
       set_ref_radius_post2D, &
       set_periodic_data_post2D, &
       set_extra_material_post2D, &
       active_GMV, &
       put_material_ID_post2D, &
       GMV_circular_selection, &
       get_write_gmv_post2D, &
       set_pt2dx_radius_post2D

CONTAINS

!!!--------------------------------------------------------------------
  SUBROUTINE chic_command_post2D

    IMPLICIT NONE
    
    INTEGER           :: ii,l_ii,Nstep,Nstep_gmv,nb
    REAL(kind=8)      :: rtmp,periode,X,Y,R 
    REAL(kind=8)      :: XMIN,XMAX,radius
    LOGICAL           :: write_gmv,PERIODIC
    CHARACTER(len=5)  :: behav_tmp
    CHARACTER(len=80) :: cout

    IF (INDEX(CHZZZ,'UPDATE POST 2D')==1) THEN
       !!****e* POST2D/UPDATE POST 2D
       !! NAME
       !!   UPDATE POST 2D
       !! PURPOSE
       !!  Do all the computation without displaying the value. 
       !!  Needed for a use in combination with LMGC90/POSTPRO
       !! USES
       !!  LMGC90.CORE/POST2D/update_post2D
       !!****
       CALL LOGCHIC('post2D') 
       CALL update_post2D(KHOZZZ)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUTPUT GMV')==1) THEN
       !!****e* POST2D/WRITE OUTPUT GMV
       !! NAME
       !!  WRITE OUTPUT GMV
       !! PURPOSE
       !!  write gmv file
       !! USES
       !!  LMGC90.CORE/POST2D/update_post2D
       !!  LMGC90.CORE/POST2D/write_GMV_post2D
       !!****
       write_gmv= get_write_gmv_post2D()

       CALL update_post2D(KHOZZZ)
       IF(write_gmv) CALL write_GMV_post2D

       IETZZZ = 1
       RETURN
    END IF

    IF ( (INDEX(CHZZZ,'INIT GMV')==1).OR. &
         (INDEX(CHZZZ,'INIT POSTFILE')==1) .OR. &
         (INDEX(CHZZZ,'BINARY INIT GMV')==1)) THEN
       !!****e* POST2D/INIT GMV
       !! NAME
       !!   INIT GMV
       !! PURPOSE
       !!  Initialize the database
       !! USES
       !!  LMGC90.CORE/POST2D/init_GMV_post2D
       !!****
       !!****e* POST2D/BINARY INIT GMV
       !! NAME
       !!   BINARY INIT GMV
       !! PURPOSE
       !!  Initialize the database and write the GMV file using the ieee binary format
       !! USES
       !!  LMGC90.CORE/POST2D/init_GMV_post2D
       !!  LMGC90.CORE/POST2D/desacti_GMV_writeascii
       !!****
       CALL LOGCHIC('post2D')
       CALL init_GMV_post2D                                   !12345
       IF (INDEX(CHZZZ,'BINARY INIT GMV')==1) CALL active_GMV('BINAR')
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'SELECT GMV MATERIAL')==1) THEN
       !!****e* POST2D/SELECT GMV MATERIAL
       !! NAME
       !!  SELECT GMV MATERIAL
       !! SYNOPSIS
       !!  SELECT GMV MATERIAL
       !!  [nb]
       !!  [behav](1)
       !!  ...
       !!  [behav](nb)
       !! INPUTS
       !!  [nb]       : number of selected behavior
       !!  [behav](i) : list of selected behavior
       !! PURPOSE
       !!  Adds specific material for the display
       !! USES
       !!  LMGC90.CORE/POST2D/set_extra_material_post2D
       !!  LMGC90.CORE/POST2D/put_material_ID_post2D
       !!****
       CALL LOGCHIC('post2D')

       READ(CHLZZZ(NZZZ+1),*) nb

       CALL set_extra_material_post2D(nb,.TRUE.)

       DO ii = 1,nb
          READ(CHLZZZ(NZZZ+1+ii),*) behav_tmp
          CALL put_material_ID_post2D(ii,behav_tmp(1:5))
       END DO
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REFERENCE REAC MAX')==1) THEN
       !!****e* POST2D/REFERENCE REAC MAX
       !! NAME
       !!   REFERENCE REAC MAX
       !! SYNOPSIS
       !!   REFERENCE REAC MAX
       !!   [reac]
       !! INPUTS
       !!   [reac] reference value of the contact force
       !! PURPOSE
       !!   Set the reference value of the contact force. 
       !!   It allows to keep the same reac during the computation. 
       !!   Either this value is recomputed a each time step.
       !! USES
       !!   LMGC90.CORE/POST2D/set_ref_reac_max_post2D
       !!****
       READ(CHLZZZ(NZZZ+1),*) rtmp
       CALL LOGCHIC('post2D')
       WRITE(cout,'(D12.5)') rtmp
       CALL LOGMESCHIC(cout)
       CALL set_ref_reac_max_post2D(rtmp)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLACEMENT AMPLIFICATION')==1) THEN
       !!****e* POST2D/DISPLACEMENT AMPLIFICATION
       !! NAME
       !!   DISPLACEMENT AMPLIFICATION
       !! SYNOPSIS
       !!   DISPLACEMENT AMPLIFICATION
       !!   [disp_amp]
       !! INPUTS
       !!  [disp_amp] : displacement amplification factor
       !! PURPOSE
       !!   It allows to amplify the magnitude of displacement.  
       !!   Works only with deformable bodies
       !! USES
       !!   LMGC90.CORE/POST2D/set_displ_ampl_post2D
       !!****
       READ(CHLZZZ(NZZZ+1),*) rtmp
       CALL LOGCHIC('post2D')
       WRITE(cout,'(D12.5)') rtmp
       CALL LOGMESCHIC(cout)
       CALL set_displ_ampl_post2D(rtmp)
       IETZZZ=1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REFERENCE RADIUS')==1) THEN
       !!****e* POST2D/REFERENCE RADIUS
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
       !!   LMGC90.CORE/POST2D/set_ref_radius_post2D
       !!****
       READ(CHLZZZ(NZZZ+1),*) rtmp
       CALL LOGCHIC('post2D')
       WRITE(cout,'(D12.5)') rtmp
       CALL LOGMESCHIC(cout)
       CALL set_ref_radius_post2D(rtmp)
       IETZZZ=1
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
       !!   LMGC90.CORE/POST2D/set_pt2Dx_radius_post2D
       !!****
       READ(CHLZZZ(NZZZ+1),*) rtmp
       CALL LOGCHIC('post2D')
       WRITE(cout,'(D12.5)') rtmp
       CALL LOGMESCHIC(cout)
       CALL set_pt2dx_radius_post2D(rtmp)
       IETZZZ=1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY THERMAL GPV')==1) THEN
       !!****e* POST2D/DISPLAY THERMAL GPV
       !! NAME
       !!   DISPLAY THERMAL GPV
       !! PURPOSE
       !!  Specifies that you want to display thermal GPV values if any
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('THGPV')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY MECHANICAL GPV')==1) THEN
       !!****e* POST2D/DISPLAY MECHANICAL GPV
       !! NAME
       !!   DISPLAY MECHANICAL GP
       !! PURPOSE
       !!  Specifies that you want to display mechanical GPV values if any
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('MEGPV')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'USE MORE MECHANICAL GPV')==1) THEN
       !!****e* POST2D/USE MORE MECHANICAL GPV
       !! NAME
       !!   USE MORE MECHANICAL GPV
       !! PURPOSE
       !!  Specifies that you want to display mechanical GPV values if any that
       !!  comes from an external library
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       !                123456789012
       CALL active_GMV('EXVAL')
       IETZZZ = 1
       RETURN
    END IF

    IF (     (INDEX(CHZZZ,'COMPUTE STRESS')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE DISPLACEMENT')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE AVERAGE VELOCITY')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE ERROR')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE TACTOR')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE TACT POINT')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE BETA')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE OXIDE')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE WEAR')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE HEAT')==1) &
         .OR.(INDEX(CHZZZ,'COMPUTE ELECTRO')==1) )THEN

       CALL LOGCHIC('*** COMMAND NAME HAVE CHANGE ***')
       CALL LOGCHIC(' See the list of available keyword below:')
       CALL LOGCHIC('DISPLAY MECHANICAL GPV')
       CALL LOGCHIC('DISPLAY THERMAL GPV')
       CALL LOGCHIC('DISPLAY STRESS')
       CALL LOGCHIC('DISPLAY DISPLACEMENT')
       CALL LOGCHIC('DISPLAY AVERAGE VELOCITY')
       CALL LOGCHIC('DISPLAY ERROR')
       CALL LOGCHIC('DISPLAY TACTOR')
       CALL LOGCHIC('DISPLAY INTERNAL')
       CALL LOGCHIC('DISPLAY TACT POINT')
       CALL LOGCHIC('DISPLAY BETA')
       CALL LOGCHIC('DISPLAY OXIDE')
       CALL LOGCHIC('DISPLAY WEAR')
       CALL LOGCHIC('DISPLAY HEAT')
       CALL LOGCHIC('DISPLAY ELECTRO')
       STOP

    END IF

    IF (INDEX(CHZZZ,'DISPLAY STRESS')==1) THEN
       !!****e* POST2D/DISPLAY STRESS
       !! NAME
       !!   DISPLAY STRESS
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('STRSS')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY DISPLACEMENT')==1) THEN
       !!****e* POST2D/DISPLAY DISPLACEMENT
       !! NAME
       !!   DISPLAY DISPLACEMENT
       !! PURPOSE
       !!  Specifies that you want to compute and display the cumulated displacement between two displayed steps
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       !first_time_gmv_dspl = .FALSE.
       CALL active_GMV('DISPL')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY AVERAGE VELOCITY')==1) THEN
       !!****e* POST2D/DISPLAY AVERAGE VELOCITY
       !! NAME
       !!   DISPLAY AVERAGE VELOCITY
       !! PURPOSE
       !!  Specifies that you want to compute and display the average velocity between two displayed steps
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('VELOC')
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY ERROR')==1) THEN
       !!****e* POST2D/DISPLAY ERROR
       !! NAME
       !!   DISPLAY ERROR
       !! PURPOSE
       !!  Specifies that you want to compute and display the contact ERROR 
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('ERROR')
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY TACTOR')==1) THEN
       !!****e* POST2D/DISPLAY TACTOR
       !! NAME
       !!   DISPLAY TACTOR
       !! PURPOSE
       !!  Specifies that you want to compute and display the contactor shape 
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('TCTRS')
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

    IF (INDEX(CHZZZ,'DISPLAY TACT POINT')==1) THEN
       !!****e* POST2D/DISPLAY TACT POINT
       !! NAME
       !!   DISPLAY TACT POINT
       !! PURPOSE
       !!  Specifies that you want to compute and display the contact point 
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('TCTPT')
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY BETA')==1) THEN
       !!****e* POST2D/DISPLAY BETA
       !! NAME
       !!   DISPLAY BETA
       !! PURPOSE
       !!  Specifies that you want to compute and display the beta value (for CZM only) 
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('BETA_')
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OXIDE')==1) THEN
       !!****e* POST2D/DISPLAY OXIDE
       !! NAME
       !!   DISPLAY OXIDE
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('OXIDE')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY WEAR')==1) THEN
       !!****e* POST2D/DISPLAY WEAR
       !! NAME
       !!   DISPLAY WEAR
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('WEAR_')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY HEAT')==1) THEN
       !!****e* POST2D/DISPLAY HEAT
       !! NAME
       !!   DISPLAY HEAT
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/acti_GMV_HEAT
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('HEAT_')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY THERMAL BOUNDS')==1) THEN
       !!****e* POST2D/DISPLAY HEAT
       !! NAME
       !!   DISPLAY HEAT
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/acti_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('HEATB')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY ELECTRO')==1) THEN
       !!****e* POST2D/DISPLAY ELECTRO
       !! NAME
       !!   DISPLAY ELECTRO
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('ELECT')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'DISPLAY TRACERS')==1) THEN
       !!****e* POST2D/DISPLAY TRACERS
       !! NAME
       !!   DISPLAY TRACERS
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('TRCRS')
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RESTART GMV')==1) THEN
       !!****e* POST2D/RESTART GMV
       !! NAME
       !!   RESTART GMV
       !! PURPOSE
       !!  Specifies that you want to restart displaying the gmv file from the value stored in DISPLAY/DISPLAYED_GMV.
       !!  It allows you not to rewrite in some displayed file. 
       !! USES
       !!   LMGC90.CORE/POST2D/active_GMV
       !!****
       CALL LOGCHIC('post2D')
       CALL active_GMV('RSTRT')
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'PERIODIC CONDITION')==1) THEN
       !!****e* POST2D/PERIODIC CONDITION
       !! NAME
       !!  PERIODIC CONDITION
       !! SYNOPSIS
       !!  PERIODIC CONDITION
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/POST2D/set_periodic_data_post2D
       !!****
       CALL LOGCHIC('post2D') 
       READ(CHLZZZ(NZZZ+1),*) periode
       PERIODIC=.TRUE.
       CALL set_periodic_data_post2D(periode,PERIODIC)
       PERIODIC=.TRUE.
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT FREE BOUNDARY')==1) THEN
       !!****e* CHIC_cmd/INIT FREE BOUNDARY
       !! NAME
       !!   INIT FREE BOUNDARY
       !! SYNOPSIS
       !!  XMIN
       !!  XMAX
       !!  DX
       !! FUNCTION
       !!  guess !
       !!****
       
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) XMIN
       READ(CHLZZZ(NZZZ+2),*) XMAX
       READ(CHLZZZ(NZZZ+3),*) RADIUS
       
       CALL active_GMV('FSB__')
       
       IETZZZ = 1
       RETURN      
    END IF

!!!----------------------------------------------------------
!!!
!!! More chic command for postprocessing
!!!
!!!----------------------------------------------------------

    IF (INDEX(CHZZZ,'CIRCULAR SELECTION')==1) THEN
       !!****e* POST2D/CIRCULAR SELECTION
       !! NAME 
       !!  CIRCULAR SELECTION
       !! SYNOPSIS
       !!  CIRCULAR SELECTION
       !!  [X]
       !!  [Y]
       !!  [R]
       !! INPUTS
       !!  [X] : X coordinate
       !!  [Y] : Y coordinate
       !!  [R] : radius selection
       !! PURPOSE
       !!  initialize data for postreatment using a circular selection
       !! USES
       !!  LMGC90.CORE/POST2D/GMV_circular_selection
       !!****
       CALL LOGCHIC('post2D')
       READ(CHLZZZ(NZZZ+1),*) X
       READ(CHLZZZ(NZZZ+2),*) Y
       READ(CHLZZZ(NZZZ+3),*) R
       CALL GMV_circular_selection(X,Y,R)
       RETURN      
    END IF

  END SUBROUTINE chic_command_post2D
!!!--------------------------------------------------------------------

  include '../../more_src/post/wrap_post2D.inc'

END MODULE wrap_post2D
