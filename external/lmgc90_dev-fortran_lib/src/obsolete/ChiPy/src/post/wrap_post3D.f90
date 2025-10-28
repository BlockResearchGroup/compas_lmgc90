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

  !!****h* LMGC90.CHIPY/POST3D
  !! NAME
  !!  module wrap_POST3D
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/POST3D
  !!****

  USE post3D,ONLY: &
       init_post_3D, &
       update_post_3D, &
       active_GMV

!       acti_GMV_avlocy, &
!       acti_GMV_electro, &
!       acti_GMV_oxide, &
!       acti_GMV_fluid, &
!       acti_GMV_position, &
!       acti_GMV_forces, &
!       acti_GMV_stress, &
!       acti_GMV_heat

  use utilities

  IMPLICIT NONE 
  PRIVATE

  PUBLIC Init,Update,SetDisplayedField

CONTAINS

!!!--------------------------------------------------------------------
  SUBROUTINE Init

    IMPLICIT NONE


       !!****e* POST3D/Init
       !! NAME
       !!   Init
       !! PURPOSE
       !!  Do all the computation without displaying the value. 
       !!  Needed for a use in combination with LMGC90/POSTPRO
       !! USES
       !!  LMGC90.CORE/POST3D/init_post_3D
       !!****

       CALL init_post_3D

  END SUBROUTINE

  SUBROUTINE UPDATE
    IMPLICIT NONE
       !!****e* POST3D/UPDATE
       !! NAME
       !!   UPDATE POST
       !! PURPOSE
       !!  Do all the computation without displaying the value. 
       !!  Needed for a use in combination with LMGC90/POSTPRO
       !! USES
       !!  LMGC90.CORE/POST3D/update_post_3D
       !!****

       CALL update_post_3D

  END SUBROUTINE

  SUBROUTINE SetDisplayedField(cvalue)
   !!****e* POST3D/setdisplayfield
   !! NAME
   !!   setdisplayfield
   !! PURPOSE
   !!  Specifies that you want to compute and display the average velocity between two displayed steps
   !! PARAMETERS
   !!   POSITION
   !!   AVERAGE VELOCITY
   !!   OXIDE
   !!   ELECTRO
   !!   HEAT
   !!   STRESS
   !!   FORCES
   !!   FLUID
   !! USES
   !!   LMGC90.CORE/POST3D/acti_GMV_avlocy
   !!   LMGC90.CORE/POST3D/acti_GMV_fluid
   !!   LMGC90.CORE/POST3D/acti_GMV_ELECTRO
   !!   LMGC90.CORE/POST3D/acti_GMV_heat
   !!   LMGC90.CORE/POST3D/acti_GMV_stress
   !!   LMGC90.CORE/POST3D/acti_GMV_forces
   !!****

     IMPLICIT NONE 
     CHARACTER(*),optional :: cvalue
                                  !1234567890123456789012345678901234567890
     CHARACTER(len=33)     :: IAM='wrap_post3D::SelectDisplayedField'


     !print*,'==='
     !print*,cvalue


     SELECT CASE(cvalue)
      CASE('POSITION')

        CALL active_GMV('POSIT')

      CASE('AVERAGE VELOCITY')

        CALL active_GMV('VELOC')

      CASE('OXIDE')

        CALL active_GMV('OXIDE')

      CASE('ELECTRO')

         CALL active_GMV('ELECT')

      CASE('HEAT')

        CALL active_GMV('HEAT_')

      CASE('STRESS')

       CALL active_GMV('STRSS')

      CASE ('FORCES')

       CALL active_GMV('FORCE')

      CASE('FLUID')

       CALL active_GMV('FLUID')

      CASE('MECHANICAL GPV')

       CALL active_GMV('MECAx')

      CASE DEFAULT

        CALL FATERR(IAM,'Error: Unknown parameter')

      END SELECT

    END SUBROUTINE
!!!--------------------------------------------------------------------

END MODULE wrap_post3D
