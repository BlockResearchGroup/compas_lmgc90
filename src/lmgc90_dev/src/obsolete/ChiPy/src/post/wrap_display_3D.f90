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
MODULE wrap_DISPLAY_3D

  !!****h* LMGC90.CHIPY/DISPLAY_3D
  !! NAME
  !!  module wrap_DISPLAY_3D
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DISPLAY_3D
  !!****

  USE DISPLAY_3D,ONLY:&
       init_display_3D, &
!       set_display_3D_step, &
!       display_during_computation, &
!       display_before_computation, &
!       display_after_computation , &
       desactive_ascii_gmv_write , &
       get_write_gmv_display_3D, &
       active_GMV_DISPLAY3D, &
       write_out_GMV

  USE utilities

  IMPLICIT NONE
  PRIVATE
  PUBLIC Init, &
         WriteOutGMV, &
         SetParameterGMV
         !SetDisplayStep,
         !DisplayBeforeComputation,
         !DisplayDuringComputation, &
         !DisplayAfterComputation,

CONTAINS


!!!--------------------------------------------------------------------

  SUBROUTINE Init
    IMPLICIT NONE

       !!****e* DISPLAY_3D/Init
       !! NAME
       !!  Init
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DISPLAY_3D/Init_Display_3D
       !!****

       CALL init_display_3D(1)
  END SUBROUTINE

  subroutine WriteOutGMV(Ivalue)
      implicit none
      integer :: ivalue ! niveau de bavaradage
      logical :: write_gmv

       !!****e* POST2D/WriteOutGMV
       !! NAME
       !!  WriteOutGMV
       !! PURPOSE
       !!  write gmv file
       !! USES
       !!  LMGC90.CORE/POST2D/update_post2D
       !!  LMGC90.CORE/POST2D/write_GMV_post2D
       !!****
       write_gmv= get_write_gmv_display_3D()

       IF (write_gmv) CALL write_out_GMV

    END SUBROUTINE


!!!--------------------------------------------------------------------

!!!--------------------------------------------------------------------
!!$  SUBROUTINE SetDisplayStep(step)
!!$
!!$    IMPLICIT NONE
!!$
!!$    INTEGER,optional :: STEP
!!$
!!$       !!****e* DISPLAY_3D/SetDisplayStep
!!$       !! NAME
!!$       !!  SetDisplayStep
!!$       !! SYNOPSIS
!!$       !!  SetDisplayStep
!!$       !!  [Nstep]
!!$       !! INPUTS
!!$       !!  [Nstep] : display period
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/DISPLAY_3D/set_display_3D_step
!!$       !!****
!!$       CALL set_display_3D_step(STEP)
!!$
!!$  END SUBROUTINE

!!!--------------------------------------------------------------------

!!$  SUBROUTINE DisplayBeforeComputation
!!$    IMPLICIT NONE
!!$    INTEGER :: ibavard=1
!!$       !!****e* DISPLAY_3D/DisplayBeforeComputation
!!$       !! NAME
!!$       !!  DisplayBeforeComputation
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/DISPLAY_3D/display_before_computation
!!$       !!****
!!$
!!$       CALL display_before_computation(ibavard)
!!$
!!$  END SUBROUTINE
!!$
!!$!!!--------------------------------------------------------------------
!!$
!!$  SUBROUTINE DisplayDuringComputation
!!$    IMPLICIT NONE
!!$    INTEGER :: ibavard=1
!!$       !!****e* DISPLAY_3D/DisplayDuringComputation
!!$       !! NAME
!!$       !!  DisplayDuringComputation
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/DISPLAY_3D/display_during_computation
!!$       !!****
!!$
!!$       CALL display_during_computation(ibavard)
!!$  END SUBROUTINE
!!$
!!$!!!--------------------------------------------------------------------
!!$
!!$  SUBROUTINE DisplayAfterComputation
!!$    IMPLICIT NONE
!!$       !!****e* DISPLAY_3D/DisplayAfterComputation
!!$       !! NAME
!!$       !!  DisplayAfterComputation
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/DISPLAY_3D/display_after_computation
!!$       !!****
!!$
!!$       CALL display_after_computation
!!$
!!$    END SUBROUTINE

!!!--------------------------------------------------------------------

    SUBROUTINE SetParameterGMV(cvalue)
      IMPLICIT NONE
      character(*),optional :: cvalue
                                   !1234567890123456789012345678901234567890
      character(len=32)     :: IAM='wrap_display_3D::SetParameterGMV'
       !!****e* DISPLAY_3D/SetParameterGMV
       !! NAME
       !!  SetParameterGMV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DISPLAY_3D/desactive_ascii_gmv_write
       !!****

       SELECT CASE (cvalue)
       CASE ('BinaryWrite')
         CALL desactive_ascii_gmv_write
       CASE('DISPLAY TACTOR')
         CALL active_GMV_DISPLAY3D('TCTRS')
       CASE DEFAULT
         CALL FATERR(IAM,'Error: unknown parameter')
       END SELECT

    END SUBROUTINE
    
!!!--------------------------------------------------------------------

END MODULE wrap_DISPLAY_3D
